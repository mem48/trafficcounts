# rcpprouting tests

#install.packages("cppRouting")
library(cppRouting)
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

sf2graph <- function(sfdf){
  sfdf <- st_transform(sfdf, 4326)
  coords <- as.data.frame(sf::st_coordinates(sfdf))
  coords <- round(coords, 13)
  coords$ID <- stplanr::od_id_szudzik(coords$X, coords$Y, ordermatters = TRUE)
  #coords$ID2 <- seq(1, nrow(coords))
  coords$lengths <- geodist::geodist(coords[,c("X","Y")], sequential = TRUE, pad = TRUE)
  #coords$lengths[1] <- 0
  coords$speeds <- sfdf$maxspeed[coords$L1]
  # Convert to m/s
  maxspeed2mps <- function(x, na.val = 13.4112 ){
    if(is.na(x)){
      return(na.val) # 30 mph default
    }
    if(class(x) == "character"){
      # Check for mph flag
      if(grepl("mph",x,ignore.case = TRUE)){
        x <- gsub("mph","",x)
        x <- trimws(x, "both")
        x <- as.numeric(x)
        x <- x / 2.237
      } else {
        x <- trimws(x, "both")
        x <- as.numeric(x)
        x <- x / 3.6
      }
      
    }else if(class(x) == "numeric"){
      # Assume in kph
      x <- x / 3.6
    }else{
      stop("Not character or numeric object")
    }
  }
  coords$speedsms <- sapply(coords$speeds, maxspeed2mps)
  coords$time <- coords$lengths / coords$speedsms
  
  roads <- coords[seq(1, nrow(coords)-1),c("ID","L1")]
  roads2 <- coords[seq(2, nrow(coords)),c("ID","time","L1")]
  names(roads) <- c("from","L1a")
  names(roads2) <- c("to","weight","L1b")
  roads <- cbind(roads, roads2)
  rm(roads2)
  roads <- roads[roads$L1a == roads$L1b,]
  roads <- roads[,c("from","to","weight")]
  coords <- coords[,c("ID","X","Y")]
  coords <- coords[!duplicated(coords$ID),]
  
  graph <- cppRouting::makegraph(roads, coords = coords, directed = FALSE)
  return(graph)
  
}

graph <- sf2graph(osm)
#graph_simple <- cpp_simplify(graph)
#graph_contract <- cpp_simplify(graph_simple)


make_isochrone <- function(graph, from = "1", lim = c(5,10,15) * 60){
  iso <- cppRouting::get_isochrone(Graph = graph, from = from,lim = lim)
  poly<-lapply(iso[[1]],function(x){
    x <- data.frame(noeuds=x,stringsAsFactors = F)
    x <- dplyr::left_join(x,graph$coords,by=c("noeuds"="ID"))
    x <- sf::st_as_sf(x,coords=c("X","Y"),crs=4326, dim = "XY")
    y <- concaveman::concaveman(x, length_threshold = 0.01)
    y <- y[[1]][[1]]
    y <- lwgeom::st_make_valid(y)
    y <- st_cast(y, "MULTIPOLYGON")
    #y <- concaveman::concaveman(sf::st_combine(sf::st_as_sf(x,coords=c("X","Y"),crs=4326, dim = "XY")))
    #y <- sf::st_as_sf(x,coords=c("X","Y"),crs=4326, dim = "XY")
    #y <- sf::st_convex_hull(sf::st_combine(y))
    return(y)
  })
  #poly<-do.call(rbind,poly)
  #poly$time<-as.factor(names(iso[[1]]))
  poly <- sf::st_as_sfc(poly, crs = 4326)
  poly <- sf::st_as_sf(data.frame(lim = lim, geometry = poly))
  poly <- poly[seq(nrow(poly), 1), ]
  return(poly)
}

isochon <- make_isochrone(graph, from = 1273133762, c(5,10,15) * 60)
tm_shape(isochon) +
  tm_fill("lim", alpha = 0.5)

make_routes <- function(graph, from = "1273133762", to = "1354756376"){
  route <- cppRouting::get_path_pair(graph, from, to)
  line<-lapply(route,function(x){
    x <- data.frame(noeuds=x,stringsAsFactors = F)
    x <- dplyr::left_join(x,graph$coords,by=c("noeuds"="ID"))
    x <- sf::st_linestring(as.matrix(x[,c("X","Y")]))
    return(x)
  })
  line <- sf::st_as_sfc(line, crs = 4326)
  res <- sf::st_as_sf(data.frame(from = from, to = to, geometry = line))
}

route <- make_routes(graph)
qtm(route)
