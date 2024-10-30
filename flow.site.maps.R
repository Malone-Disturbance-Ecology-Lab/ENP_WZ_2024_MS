rm(list = ls())

setwd("/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS")

palette <- c("darkblue", "#fab255", "#43b284", "darkblue", "#43b284") 

# load packages #####
library(ggplot2)     # figure development
library(patchwork)   # panel figure development
library(tidyverse)   # everything else
library(maptools)
library(raster)  
library(rgdal)      # load shapefiles with readOGR 
library(sp)         # coordinates function
library(sf)         # simple features 
library(ggpubr)     # rotate axes text for more readable figures
library(ggspatial)  # scale bars and north arrows

enp.se <- read_sf("data/shapefiles/Southeastern Everglades_AOI.kml" ) %>% st_transform( 3857)

site <- c("Marl Prairie", "Ecotone", "Mangrove Scrub", "Inland", "Coastal") # label points by landcover type
site_code <- c( "TS/Ph-1", "SE-1", "TS/Ph-7", "EVER8", "HC") # label points by site name
type <- c("flux tower", "flux tower", "flux tower", "long term", "long term")
color <- c("darkblue", "#fab255", "#43b284", "darkblue", "#43b284") # distinguish points
LAT <- c(25.4379, 25.3539, 25.19676203, 25.34594, 25.25416)
LON <- c(-80.5946, -80.3810, -80.64207766, -80.47893, -80.44427)

mapsites <- data.frame(site, site_code, type, color, LAT, LON) # contain attribute information for mapping

# attach coordinates to make the dataframe spatial
coordinates(mapsites) <- ~LON+LAT 

# define crs for the SpatialPoints
crs(mapsites) <- '+proj=longlat +datum=WGS84 +no_defs' 

# convert SpatialPoints to sf
mapsites.sf <- sf::st_as_sf(mapsites, coords = c("LAT", "LON"))

# convert the points to web mercator (EPSG 3857) *all basemaps from the basemaps package come in this format
mapsites.sf <- st_transform(mapsites.sf, crs = st_crs(3857))
LAT <- c(2929627.37102133, 2919276.26349465, 2899931.82535477, 2918295.7462027, 2906994.88804114)
LON <- c(-8971749.83268757, -8947971.98945412, -8977035.02162282, -8958873.50718751, -8955015.17363661)
sites <- data.frame(LAT, LON)
mapsites.sf <- cbind(mapsites.sf, sites)
lapply(mapsites.sf, class)

# make attribute information for points into factors
mapsites.sf$site <- as.factor(mapsites.sf$site)
mapsites.sf$site_code <- as.factor(mapsites.sf$site_code)
mapsites.sf$type <- as.factor(mapsites.sf$type)
mapsites.sf$color <- as.factor(mapsites.sf$color)

# delete stuff cluttering environment
rm(sites, LAT, LON, site, color, site_code)

# import shapefiles #####
# import Taylor Slough shapefile
setwd("data/shapefiles")
TSL.shp <- readOGR(
  dsn = ".",
  layer = c("TaylorSlough_2"),
  verbose = TRUE)

TSL.shp <- spTransform(TSL.shp, crs('+proj=longlat +datum=WGS84 +no_defs'))


# convert SpatialPolygons to sf
TSL.sf <- sf::st_as_sf(TSL.shp)

# convert the Taylor polygon to web mercator (EPSG 3857) *all basemaps from the basemaps package come in this format
TSL.sf <- st_transform(TSL.sf, crs = st_crs(3857))

# bring in everglades and FL shapefiles for inset map
# Everglades polygon
ENP.shp <- readOGR(
  dsn = ".",
  layer = c("ENP"),
  verbose = TRUE)

ENP.shp <- spTransform(ENP.shp, crs('+proj=longlat +datum=WGS84 +no_defs'))
crs(ENP.shp) 

ENP.sf <- sf::st_as_sf(ENP.shp)

# roads and canals
roads.shp <- readOGR(
  dsn = ".",
  layer = c("Major_Roads"),
  verbose = TRUE)

roads.shp <- spTransform(roads.shp, crs('+proj=longlat +datum=WGS84 +no_defs'))
crs(roads.shp) 

roads.sf <- sf::st_as_sf(roads.shp)

# FL polygon - simple
FL.shp <- readOGR(
  dsn = ".",
  layer = c("Florida.simp"),
  verbose = TRUE)

FL.shp <- spTransform(FL.shp, crs('+proj=longlat +datum=WGS84 +no_defs'))
crs(FL.shp) 

FL.sf <- sf::st_as_sf(FL.shp)
# convert the FL polygon to web mercator (EPSG 3857) *all basemaps from the basemaps package come in this format
FL.sf <- st_transform(FL.sf, crs = st_crs(3857))

# FL polygon - complex
FL.line.shp <- readOGR(
  dsn = ".",
  layer = c("Florida_Boundary_Lines"),
  verbose = TRUE)

FL.line.shp <- spTransform(FL.shp, crs('+proj=longlat +datum=WGS84 +no_defs'))
crs(FL.line.shp) 

FL.line.sf <- sf::st_as_sf(FL.line.shp)
# convert the FL polygon to web mercator (EPSG 3857) *all basemaps from the basemaps package come in this format
FL.line.sf <- st_transform(FL.line.sf, crs = st_crs(3857))

FL.complex.shp <- readOGR(
  dsn = ".",
  layer = c("Florida"),
  verbose = TRUE)

FL.complex.shp <- spTransform(FL.complex.shp, crs('+proj=longlat +datum=WGS84 +no_defs'))
crs(FL.complex.shp) 

FL.complex.sf <- sf::st_as_sf(FL.complex.shp)
# convert the FL polygon to web mercator (EPSG 3857) *all basemaps from the basemaps package come in this format
FL.complex.sf <- st_transform(FL.complex.sf, crs = st_crs(3857))
FL.complex.sf <- st_transform(FL.complex.sf, crs = st_crs(3857))
# bring it all together in a site map! #####

# set limits for map
lon_lims <- c(-9065000, -8930000) # originally c(-8989000, -8930000)
lat_lims <- c(2855000, 2975000) # originally c(2892000, 2936500)

# define legend items
shapes <- c(19, 17)

# make the map
ggplot() + geom_sf(enp.se, mapping = aes(), color = "black", fill = "transparent", lwd = 0.5)

sitemap_no_base <- ggplot() +  theme_bw() + theme(panel.grid.major = element_blank()) +
  geom_sf(FL.complex.sf, mapping = aes(), color = "black", fill = "gray90", lwd = 0.25) + # FL polygon
  geom_sf(enp.se, mapping = aes(), color = "black", fill = "gray50", lwd = 0.5) + 
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  #geom_sf(roads.sf, mapping = aes(), color = "gray50", lwd = 0.25, show.legend = FALSE) +  # road polygon
  #coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  #geom_sf(canals.sf, mapping = aes(), color = "lightblue", lwd = 0.5, show.legend = FALSE) +  # canal polygon
  #coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  geom_sf(ENP.sf, mapping = aes(), color = "peru", fill = "peru", alpha = 0, lwd = 0.75, show.legend = FALSE) +  # Everglades National Park polygon
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  geom_sf(TSL.sf, mapping = aes(), color = "lightblue", fill = "lightblue", lwd = 0.25, show.legend = FALSE) +  # Taylor Slough polygon
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  geom_sf(mapsites.sf, mapping = aes(shape = type), color = palette, size = 4.5, show.legend = TRUE) +  # study sites, with site type and color defined
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  scale_shape_manual(name = "",    
                     values = shapes, 
                     labels = c("EC Tower", "Long-term Hydrologic Station"),
                     guide = guide_legend(reverse = TRUE)) +     # makes a legend for the site type
  theme(   
    legend.position = "bottom",
    legend.box.just = "right",
    legend.margin = margin(1, 2, 1, 2)) + # adjusts legend position
  labs(x = NULL, y = NULL) + 
  ggspatial::annotation_scale(location = "br", bar_cols = c("gray50", "white"), text_cex = 1.1, text_col = "gray30",
                              pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in")) + # adds scalebar
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                                    pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                                    style = north_arrow_fancy_orienteering(fill = c("white", "gray50"), text_size = 14, text_col = "gray30")) + # adds north arrow
  annotate(geom="text", x = -8971750, y = 2929627, label = "Marl Prairie", color = "gray10", hjust= 1, vjust=-1.3, size = 6) + # adds annotation to the map points so we know what the sites are / hjust=0.8, vjust = -1.2 / hjust=1.2, vjust = -0.1, hjust=-0.2, vjust = 0.15, 
  annotate(geom="text", x = -8947972, y = 2919276, label = "Ecotone", color = "gray10", hjust= 0.65, vjust=-1.3, size = 6) + # adds annotation to the map points so we know what the sites are  
  annotate(geom="text", x = -8977035, y = 2899932, label = "Scrub Mangrove", color = "gray10", hjust= 0.8, vjust=-1.3, size = 6) + # adds annotation to the map points so we know what the sites are  
  #annotate(geom="text", x = -8958874, y = 2918296, label = "Inland", color = "black", hjust= 1.1, vjust=-0.1, size = 5.5) + # adds annotation to the map points so we know what the sites are
 # annotate(geom="text", x = -8955015, y = 2906995, label = "Coastal", color = "black", hjust= 1, vjust=-1.4, size = 5.5) + # adds annotation to the map points so we know what the sites are  
  
  geom_text(TSL.sf, mapping = aes(x = -8983000, y = 2917000, label = "Taylor Slough"), color = "lightskyblue", size = 5.5, angle = 55, fontface = "italic", hjust= 0.45) +
  geom_text(ENP.sf, mapping = aes(x = -9000500, y = 2948500, label = "Everglades National Park"), color = "peru", size = 5.5, fontface = "italic")  #(x = -8995000, y = 2971000) ENP label outside the park outline


# make an inset map of FL with an annotated box over the SESE ####
st_bbox(FL.sf) 

FL_inset <-  ggplot() + theme_bw() + theme(panel.grid.major = element_blank()) +
  labs(x = NULL, y = NULL) + 
  geom_sf(FL.sf, mapping = aes(), color = "black", fill = "gray90", lwd = 0.25) + 
  annotate(geom = "rect", xmin = -9025000, xmax =-8920000, ymin = 2870000, ymax = 2950000, color = "black", fill = NA, lwd = 0.75) +
  geom_text(FL.sf, mapping = aes(x = -9550000, y = 2870000, label = "Florida"), color = "black", size = 5.5) +
  theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())


# export sitemap figure ####
setwd("/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS/figures")

png(filename = "flow.site.maps.png", 
    width = 8, height = 8,  units = "in", res = 400, bg = "transparent") # 8, 7.5

sitemap_no_base + ggpubr::rotate_x_text(45)+ inset_element(FL_inset, left = 0.01, bottom = 0, right = 0.25, top = 0.33) &
  theme(text=element_text(size=24)) 

dev.off()

