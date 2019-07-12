# test isle of wight
library(sf)
library(tmap)
tmap_mode("view")
library(osmdata)
library(dplyr)
library(deldir)
library(dodgr)

# 1) Get Roads
q = opq(getbb("isle of wight, UK")) %>%
  add_osm_feature(key = "highway")
osm_raw = osmdata_sf(q = q)

osm <- osm_raw$osm_lines
points <- osm_raw$osm_points

osm <- st_transform(osm, 27700)
points <- st_transform(points, 27700)

osm <- osm[,c("osm_id","name","ref","highway",
                  "junction","maxspeed","geometry")]

# only intrested in roads not paths
osm <- osm[osm$highway %in% c("living_street","primary","primary_link",
                              "residential","secondary", "secondary_link",
                              "tertiary",  "tertiary_link", "unclassified"),]


#Find Junctions, OSM Points are both nodes that make up lines/polygons, and objects e.g. shops
#remove points that are not nodes on the line
#node points have no tags
col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing", "crossing_ref","geometry")] #Get column names other than osm_id, and highway which is just for junction types, and crossing info which can be junction between cycle way and road
points.sub <- points
points <- points[,c("osm_id","highway")]
points.sub <- as.data.frame(points.sub)
points.sub$geometry <- NULL
points.sub <- points.sub[,col.names]
rowsum <- as.integer(rowSums(!is.na(points.sub)))
rm(points.sub, col.names)
points <- points[rowsum == 0,] #Remove points with any tags

#now check highway tag to remove things like traffic lights
points <- points[is.na(points$highway) | points$highway %in% c("mini_roundabout","motorway_junction"), ]
points <- points[,c("osm_id","geometry")]

#Look for points that intersect lines
inter <- st_intersects(points,osm)
len <- lengths(inter)
points <- points[len >= 2,] #Only keep points that intersec at least 2 lines i.e. a junction

#Remove any duplicated points
points <- points[!duplicated(points$geometry),]

# 2) Get AADT Counts
bounds <- readRDS("data/bounds.Rds")
traffic <- readRDS("data/traffic.Rds")
traffic <- traffic[,c("road","aadt","ncycles")]
traffic <- traffic[bounds,]

osm <- osm[bounds,]
osm_major <- osm[osm$highway %in% c("primary","secondary","motorway","trunk"),]
osm_minor <- osm[!osm$highway %in% c("primary","secondary","motorway","trunk"),]


voronoi <- dismo::voronoi(xy = st_coordinates(traffic.class))
voronoi <- as(voronoi, "sf")
st_crs(voronoi) <- st_crs(traffic.class)

qtm(voronoi, fill = NULL) +
  qtm(osm_major$geometry) +
  qtm(osm_major$geometry[is.na(osm_major$ref)], lines.col = "red") +
  qtm(traffic.class)


#Functions
#Function for classified roads
get.aadt.class <- function(e){
  #message(paste0("doing ",e))
  traffic.sub <- traffic.class[traffic.class$road == roadnames[e],]
  traffic.sub <- traffic.sub[!duplicated(traffic.sub$geometry),]
  osm.sub <- osm.nona[osm.nona$ref == roadnames[e],]
  
  #need at least 2 points to make voronoi polygons
  if(nrow(traffic.sub) > 1){
    #Make voronoi polygons and convert to SF
    voronoi <- dismo::voronoi(xy = st_coordinates(traffic.sub))
    voronoi <- as(voronoi, "sf")
    st_crs(voronoi) <- st_crs(traffic.sub)
  }else{
    #Make a big buffer around the point
    voronoi <- st_buffer(traffic.sub, 1000)
  }
  
  #Find Intersections of roads with vernoi polygons
  inter <- st_intersects(osm.sub,voronoi)
  #Get aadt and ncycle values
  osm.sub$aadt <- lapply(1:nrow(osm.sub),function(x){as.numeric(round(mean(traffic.sub$aadt[inter[[x]]])),0)})
  osm.sub$ncycles <- lapply(1:nrow(osm.sub),function(x){as.numeric(round(mean(traffic.sub$ncycles[inter[[x]]])),0)})
  
  #Remove Unneded Data
  osm.sub <- as.data.frame(osm.sub)
  osm.sub <- osm.sub[,c("osm_id","aadt","ncycles")]
  
  return(osm.sub)
}


#Separate Calssified and Unlassified Roads
traffic.class <- traffic[!substr(traffic$road,1,1) %in% c("U","C"),]
traffic.unclass <- traffic[substr(traffic$road,1,1) %in% c("U","C"),]
rm(traffic)

#start with the classified
roadnames <- unique(traffic.class$road)
roadnames <- roadnames[roadnames %in% osm$ref]
osm.nona <- osm[!is.na(osm$ref),] #Create a working dataset without nas
osm.nona <- osm.nona[,c("osm_id","ref")] #Dump unneeded data
res.class <- lapply(1:length(roadnames),get.aadt.class)
res.class <- do.call("rbind",res.class)
res.class <- res.class[!is.na(res.class$osm_id),]
rm(osm.nona,roadnames)


#remove any duplicates
res.class <- res.class[!duplicated(res.class$osm_id),]
res.class$aadt <- as.numeric(res.class$aadt)

#Join onto the original osm data
osm <- left_join(osm,res.class, by = c("osm_id" = "osm_id"))

# 3) Find Junctions between minor and major roads

# split out
osm_major <- osm[!is.na(osm$aadt),]
osm_minor <- osm[is.na(osm$aadt),]

minor_int <- st_intersects(points, osm_minor)
major_int <- st_intersects(points, osm_major)

minor_int = lengths(minor_int)
major_int = lengths(major_int)
both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

junc_majmi = points[both_int,]
junc_majmi = as.data.frame(st_coordinates(junc_majmi))

# Break cross roads
# Othereise flows can go round the major roads
osm_major_buff <- st_buffer(osm_major, 1)
osm_major_buff <- st_union(osm_major_buff)
osm_minor_mod <- st_difference(osm_minor, osm_major_buff)
osm_minor_mod <- st_cast(osm_minor_mod, "MULTILINESTRING")
osm_minor_mod <- st_cast(osm_minor_mod, "LINESTRING")

# Make new junction interection points
junc_majmi2 = st_buffer(points[both_int,], 1.1)
osm_minor_points = st_cast(osm_minor_mod, "POINT")
junc_majmi2 = osm_minor_points[junc_majmi2,,op = st_within]
#junc_majmi2 = st_cast(junc_majmi2, "MULTILINESTRING")
#junc_majmi2 = st_intersection(junc_majmi2, osm_minor_mod)
#junc_majmi2 = st_cast(junc_majmi2, "MULTIPOINT", do_split = TRUE, group_or_split = TRUE)
#junc_majmi2 = st_cast(junc_majmi2, "POINT", do_split = TRUE, group_or_split = TRUE)
junc_majmi2 = as.data.frame(st_coordinates(junc_majmi2))

# 4) Get mid-point of minor roads, i.e. centroid on the line
minor_cent <- as.data.frame(st_coordinates(osm_minor_mod))
minor_cent <- group_by(minor_cent, L1) %>%
  summarise(X = nth(X, n()/2),
            Y = nth(Y, n()/ 2))

#foo = st_sfc(st_multipoint(as.matrix(minor_cent[,2:3])), crs = 27700)

# 5) Make dodgr graph of minor roads
graph <- weight_streetnet(osm_minor_mod, wt_profile = "motorcar")
graph_ids <- graph[,c("from_id","from_lon","from_lat")]
graph_ids <- unique(graph_ids)

nrow(minor_cent)
nrow(junc_majmi)
junc_majmi2 <- left_join(junc_majmi2, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))
minor_cent <- left_join(minor_cent, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))
nrow(minor_cent)
nrow(junc_majmi)

agg_flow <- dodgr_flows_aggregate(graph,
                                  from = junc_majmi2$from_id,
                                  to = minor_cent$from_id,
                                  flows = matrix(rep(1, nrow(minor_cent) * nrow(junc_majmi)),
                                                 ncol = nrow(minor_cent)),
                                  contract = TRUE, quiet = FALSE)
graph_undir <- merge_directed_flows (agg_flow)
geoms <- dodgr_to_sfc (graph_undir)
gc <- dodgr_contract_graph (graph_undir)
gsf <- sf::st_sf (geoms)
gsf$flow <- gc$flow
st_crs(gsf) <- 27700
gsf$flow <- gsf$flow / max(gsf$flow)

# dens <- rep (1, nrow (junc_majmi)) # uniform densities
# disp_flow <- dodgr_flows_disperse (graph, from = junc_majmi$from_id, dens = dens)
# graph_undir <- merge_directed_flows (disp_flow)
# geoms <- dodgr_to_sfc (graph_undir)
# gc <- dodgr_contract_graph (graph_undir)
# gsf <- sf::st_sf (geoms)
# gsf$flow <- gc$flow
# st_crs(gsf) <- 27700
# gsf$flow <- gsf$flow / max(gsf$flow)

qtm(gsf, lines.lwd = 3, lines.col = "flow") + 
  qtm(osm_major, lines.lwd = 3, lines.col = "black")

foo = dodgr_to_sf(graph)
st_crs(foo) <- 27700
foo <- foo[foo$component < 20,]
foo$component2 <- as.character(foo$component)
qtm(foo, lines.lwd = 3, lines.col = "component2") + 
  qtm(osm_major, lines.lwd = 3, lines.col = "black")

