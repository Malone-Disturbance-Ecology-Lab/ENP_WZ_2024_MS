rm(list = ls())

setwd("/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS")

palette <- c("darkblue", "#fab255", "#43b284", "darkblue", "#43b284") 

# load packages #####
library(ggplot2)     # figure development
library(patchwork)   # panel figure development
library(tidyverse)   # everything else
#library(maptools)
library(terra)  
library(sp)         # coordinates function
library(sf)         # simple features 
library(ggpubr)     # rotate axes text for more readable figures
library(ggspatial)  # scale bars and north arrows

enp.se <- read_sf("data/shapefiles/Southeastern Everglades_AOI.kml" )
enp.se %>% ggplot() + geom_sf()

site <- c("Marl Prairie", "Ecotone", "Mangrove Scrub", "Inland", "Coastal") # label points by landcover type
site_code <- c( "TS/Ph1a", "SE-1", "TS/Ph7a", "EVER8", "HC") # label points by site name
type <- c("flux tower", "flux tower", "flux tower", "long term", "long term")
color <- c("darkblue", "#fab255", "#43b284", "darkblue", "#43b284") # distinguish points
LAT <- c(25.4379, 25.3539, 25.19676203, 25.34594, 25.25416)
LON <- c(-80.5946, -80.3810, -80.64207766, -80.47893, -80.44427)

mapsites <- data.frame(site, site_code, type, color, LAT, LON) # contain attribute information for mapping

# attach coordinates to make the dataframe spatial

mapsites.sf <- mapsites %>% st_as_sf(                         
               coords = c("LON", "LAT"))
st_crs(mapsites.sf) <- st_crs(enp.se)

enp.se %>% ggplot() + geom_sf() + geom_sf(data=mapsites.sf)

enp.se.shade <- st_read("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/SE-Shade.shp")

# make attribute information for points into factors
mapsites.sf$site <- as.factor(mapsites.sf$site)
mapsites.sf$site_code <- as.factor(mapsites.sf$site_code)
mapsites.sf$type <- as.factor(mapsites.sf$type)
mapsites.sf$color <- as.factor(mapsites.sf$color)

# import shapefiles #####
# import Taylor Slough shapefile
TSL.sf <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/TaylorSlough_2.shp") %>% 
   st_transform(crs =  st_crs(enp.se))

# bring in everglades and FL shapefiles for inset map
# Everglades polygon
ENP.sf <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/ENP.shp") %>% 
  st_transform(crs('+proj=longlat +datum=WGS84 +no_defs'))

# roads and canals
roads.shp <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/Major_Roads.shp") %>% 
  st_transform(crs('+proj=longlat +datum=WGS84 +no_defs'))

# FL polygon - simple

FL.shp <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/Florida.simp.shp") %>% 
  st_transform(FL.sf, crs =  st_crs(enp.se))

FL.sf <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/Florida_Boundary_Lines.shp") %>% 
  st_transform( crs =  st_crs(enp.se))

FL.complex.sf <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/Florida.shp") %>% 
  st_transform(FL.sf, crs =  st_crs(enp.se))

# set limits for map

lon_lims <- c(-82, -80) # originally c(-8989000, -8930000)
lat_lims <- c(24.5, 26) # originally c(2892000, 2936500)

# define legend items
shapes <- c(19, 17)

# make the map

sitemap_no_base <- ggplot() +  
  theme_bw() + 
  theme(panel.grid.major = element_blank()) +
  geom_sf(FL.complex.sf, mapping = aes(), color = "black", fill = "gray95", lwd = 0.25) + # FL polygon
  geom_sf(enp.se, mapping = aes(), color = "black", fill = "gray50", lwd = 0.5) + 
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  geom_sf(ENP.sf, mapping = aes(), color = "peru", fill = "peru", alpha = 0, lwd = 0.75, show.legend = FALSE) +  # Everglades National Park polygon
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  geom_sf(TSL.sf, mapping = aes(), color = "lightblue", fill = "lightblue", lwd = 0.25, show.legend = FALSE) +  # Taylor Slough polygon
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  geom_sf(mapsites.sf, mapping = aes(shape = type), color = palette, size = 3.5, show.legend = TRUE) +  # study sites, with site type and color defined
  coord_sf(xlim = lon_lims, ylim = lat_lims, expand = FALSE)  +
  scale_shape_manual(name = "",    
                     values = shapes, 
                     labels = c("EC Towers", "Long-term Hydrologic Stations"),
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
                                    style = north_arrow_fancy_orienteering(fill = c("white", "gray50"), text_size = 14, text_col = "gray30")) 


# make an inset map of FL with an annotated box over the SESE ####
st_bbox(FL.sf) 

FL_inset <-  ggplot() + theme_bw() + theme(panel.grid.major = element_blank()) +
  labs(x = NULL, y = NULL) + 
  geom_sf(FL.sf, mapping = aes(), color = "black", fill = "gray90", lwd = 0.25) +
  geom_text(FL.sf, mapping = aes(x = -85, y = 25, label = "Florida"), color = "black", size = 5.5) +
  theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())


# export sitemap figure ####
setwd("/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS/figures")

png(filename = "flow.site.maps_NoLabels.png", 
    width = 8, height = 8,  units = "in", res = 400, bg = "transparent") # 8, 7.5

sitemap_no_base + ggpubr::rotate_x_text(45)+ inset_element(FL_inset, left = 0.01, bottom = 0, right = 0.25, top = 0.33) &
  theme(text=element_text(size=24)) 

dev.off()

