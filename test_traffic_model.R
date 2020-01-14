# test isle of wight
library(sf)
library(tmap)
tmap_mode("view")
library(osmdata)
library(dplyr)
library(deldir)
library(dodgr)
library(geodist)
library(dismo)
library(RANN)

# 1) Get Roads
# q = opq(getbb("isle of wight, UK")) %>%
#   add_osm_feature(key = "highway")
# osm_raw = osmdata_sf(q = q)
# saveRDS(osm_raw, "data/osm_raw.Rds")
osm_raw = readRDS("data/osm_raw.Rds")

# 2) Prep Input Data

osm <- osm_raw$osm_lines
points <- osm_raw$osm_points
polys <- osm_raw$osm_polygons # needed for roundabouts
polys <- polys[polys$highway %in% c("living_street","primary","primary_link",
                                    "residential","secondary", "secondary_link",
                                    "tertiary",  "tertiary_link", "unclassified"),]
polys <- st_cast(polys, "LINESTRING")
polys <- polys[,c("osm_id","name","ref","highway",
                  "junction","maxspeed","geometry")]

osm <- st_transform(osm, 27700)
points <- st_transform(points, 27700)
polys <- st_transform(polys, 27700)

osm <- osm[,c("osm_id","name","ref","highway",
                  "junction","maxspeed","geometry")]

# only intrested in roads not paths
osm <- osm[osm$highway %in% c("living_street","primary","primary_link",
                              "residential","secondary", "secondary_link",
                              "tertiary",  "tertiary_link", "unclassified"),]

osm <- rbind(osm, polys)
rm(osm_raw, polys)

# Find Junctions, OSM Points are both nodes that make up lines/polygons, and objects e.g. shops
# remove points that are not nodes on the line
# node points have no tags

col.names <- names(points)[!names(points) %in% c("osm_id","highway", "crossing", "crossing_ref","geometry")] #Get column names other than osm_id, and highway which is just for junction types, and crossing info which can be junction between cycle way and road
points.sub <- points
points <- points[,c("osm_id","highway")]
points.sub <- as.data.frame(points.sub)
points.sub$geometry <- NULL
points.sub <- points.sub[,col.names]
rowsum <- as.integer(rowSums(!is.na(points.sub)))
rm(points.sub, col.names)
points <- points[rowsum == 0,] #Remove points with any tags

# now check highway tag to remove things like traffic lights
points <- points[is.na(points$highway) | points$highway %in% c("mini_roundabout","motorway_junction"), ]
points <- points[,c("osm_id","geometry")]

# Look for points that intersect lines
inter <- st_intersects(points,osm)
len <- lengths(inter)
points <- points[len >= 2,] #Only keep points that intersec at least 2 lines i.e. a junction

# Remove any duplicated points
points <- points[!duplicated(points$geometry),]
rm(len, rowsum, inter)

# 3) Get AADT Counts
bounds <- readRDS("data/bounds.Rds")
traffic <- readRDS("data/traffic.Rds")
traffic <- traffic[,c("road","aadt","ncycles")]
traffic <- traffic[bounds,]

osm <- osm[bounds,]
osm_major <- osm[osm$highway %in% c("primary","secondary","motorway","trunk"),]
osm_minor <- osm[!osm$highway %in% c("primary","secondary","motorway","trunk"),]

# Primary Roads sometimes are missing refs, lets fix that
# See: qtm(osm_major, lines.col = "ref", lines.lwd = 3) 


# Use RANN to get the 20 nearest roads to each road
# If the reference is missining and the nearest road has the same highway type
# copy the reference. Else tye next nearest road
# Repeate the whole process twice to allow propogation along roads
osm_major_cents <- st_coordinates(st_centroid(osm_major))
nn = RANN::nn2(osm_major_cents, k = 20)

for(k in 1:2){
  for(i in 1:nrow(osm_major)){
    if(is.na(osm_major$ref[i])){
      for(j in 2:20){
        idx <- nn$nn.idx[i,j]
        if(osm_major$highway[idx] == osm_major$highway[i]){
          if(!is.na(osm_major$ref[idx])){
            osm_major$ref[i] <- osm_major$ref[idx]
            break
          }
        }
      }
    }
  }
}

# Plot to see results, seems to have worked well. 
# Except for B3327 (in the South) which has jumped to another road
qtm(osm_major, lines.col = "ref", lines.lwd = 3)
rm(nn,osm_major_cents, bounds)

# Functions
# Function for classified roads
# Distributes traffic along classified roads
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


# Separate Calssified and Unlassified Roads
traffic.class <- traffic[!substr(traffic$road,1,1) %in% c("U","C"),]
traffic.unclass <- traffic[substr(traffic$road,1,1) %in% c("U","C"),]
rm(traffic)

# Start with the classified
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
rm(res.class)

# 4) Find Junctions between minor and major roads

minor_int <- st_intersects(points, osm_minor)
major_int <- st_intersects(points, osm_major)

minor_int = lengths(minor_int)
major_int = lengths(major_int)
both_int = ifelse(minor_int > 0 & major_int > 0, TRUE, FALSE)

junc_majmi = points[both_int,]
junc_majmi = as.data.frame(st_coordinates(junc_majmi))

# Break cross roads
# Otherwise flows can go around the major roads

osm_major_buff <- st_buffer(osm_major, 1)
osm_major_buff <- st_union(osm_major_buff)
osm_minor_mod <- st_difference(osm_minor, osm_major_buff)
osm_minor_mod <- st_cast(osm_minor_mod, "MULTILINESTRING")
osm_minor_mod <- st_cast(osm_minor_mod, "LINESTRING")

# Make new junction interection points
# Which are fractionally offset form the original junction points
# So match up with the broken up road network.

junc_majmi2 = st_buffer(points[both_int,], 1.1)
osm_minor_points = st_cast(osm_minor_mod, "POINT")
junc_majmi2 = osm_minor_points[junc_majmi2,,op = st_within]
junc_majmi2 = as.data.frame(st_coordinates(junc_majmi2))

# 5) Get mid-point of minor roads, i.e. centroid on the line
minor_cent <- as.data.frame(st_coordinates(osm_minor_mod))
minor_cent <- group_by(minor_cent, L1) %>%
  summarise(X = nth(X, n()/2),
            Y = nth(Y, n()/ 2))

# 6) Make dodgr graph of minor roads
graph <- weight_streetnet(osm_minor_mod, wt_profile = "motorcar")
graph_ids <- graph[,c("from_id","from_lon","from_lat")]
graph_ids <- unique(graph_ids)

nrow(minor_cent)
nrow(junc_majmi)
junc_majmi2 <- left_join(junc_majmi2, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))
minor_cent <- left_join(minor_cent, graph_ids, by = c("X" = "from_lon", "Y" = "from_lat"))

######################
## Hack fix later: remove NAs
junc_majmi2 <- junc_majmi2[!is.na(junc_majmi2$from_id),]
minor_cent <- minor_cent[!is.na(minor_cent$from_id),]

nrow(minor_cent)
nrow(junc_majmi)
#######################

# Do routing
to = minor_cent$from_id
from = junc_majmi2$from_id
flows <- matrix (10 * runif (length (from) * length (to)),
                 nrow = length (from))

agg_flow <- dodgr_flows_aggregate(graph,
                                  from = from,
                                  to = to,
                                  flow = flows,
                                  #flows = matrix(rep(1, nrow(minor_cent) * nrow(junc_majmi)),
                                  #               ncol = nrow(minor_cent)),
                                  contract = TRUE, quiet = FALSE, tol = 0)
summary(agg_flow$flow)
graph_undir <- merge_directed_flows (agg_flow)
geoms <- dodgr_to_sfc (graph_undir)
#graph_cont <- dodgr_contract_graph (graph_undir)
gsf <- sf::st_sf (geoms)

#foo <- dodgr_to_sf(graph_undir)

#gsf$flow <- gc$graph
st_crs(gsf) <- 27700
gsf$flow <- gsf$dat.flow / max(gsf$dat.flow, na.rm = T)

# dens <- rep (1, nrow (junc_majmi)) # uniform densities
# disp_flow <- dodgr_flows_disperse (graph, from = junc_majmi$from_id, dens = dens)
# graph_undir <- merge_directed_flows (disp_flow)
# geoms <- dodgr_to_sfc (graph_undir)
# gc <- dodgr_contract_graph (graph_undir)
# gsf <- sf::st_sf (geoms)
# gsf$flow <- gc$flow
# st_crs(gsf) <- 27700
# gsf$flow <- gsf$flow / max(gsf$flow)


tm_shape(gsf) +
  tm_lines(col = "flow", lwd = 3, breaks = c(0,0.0007,0.003,0.014,0.1,1)) +
  tm_shape(osm_major) +
    tm_lines(col = "black", lwd = 3)

foo = dodgr_to_sf(graph)
st_crs(foo) <- 27700
foo <- foo[foo$component < 20,]
foo$component2 <- as.character(foo$component)
qtm(foo, lines.lwd = 3, lines.col = "component2") + 
  qtm(c, lines.lwd = 3, lines.col = "black")

