#WZ simulation2:

# calculate distance from magroves for ecotone and FWM: https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/

# Import ENP and convert to a grid. 
library(sf)
library(terra)
library(tidyterra)
library(ggpubr)
setwd('/Users/sm3466/Dropbox (YSE)/Research/ENP_WZ_2024_MS')
# Import vegetation layers and simplify: ####
enp.eco <- read_sf(dsn="data/shapefiles", layer="SaltMarsh" )
enp.Prairie <- read_sf(dsn="data/shapefiles", layer="Prairie" )
enp.fwm <- read_sf(dsn="data/shapefiles", layer="FreshwaterMarsh" )

enp.msl <- read_sf(dsn="data/shapefiles", layer="MangroveShrubland" )
enp.ms <- read_sf(dsn="data/shapefiles", layer="MangroveScrub" )

enp.fwm <- rbind(enp.fwm, enp.Prairie )
enp.msc <- rbind(enp.msl, enp.ms)

rm( enp.Prairie, enp.msl, enp.ms  )
# Simplify:
enp.eco$eco <- 1
enp.fwm$fwm <- 1
enp.msc$msc <- 1

enp.eco.d <- enp.eco[enp.eco$eco == 1, ] %>% # select the central parts
  st_union() %>% # unite to a geometry object
  st_sf() %>% st_simplify() %>% st_transform(26917)

enp.fwm.d <- enp.fwm[enp.fwm$fwm == 1, ] %>% # select the central parts
  st_union() %>% # unite to a geometry object
  st_sf() %>% st_simplify(dTolerance=500) %>% st_transform(26917)

enp.msc.d <- enp.msc[enp.msc$msc == 1, ] %>% # select the central parts
  st_union() %>% # unite to a geometry object
  st_sf()%>% st_simplify() %>% st_transform(26917)

# Import ENP and make a grid ####
enp <- st_read( "data/shapefiles/Everglades_NP.shp")
enp.trans <-st_transform(enp, 26917)
enp.grid <- st_make_grid(enp.trans , cellsize = 500, what = "centers")

#only extract the points in the limits of you AOI
grid <- st_intersection(enp.grid, enp.fwm.d )   

# Calculate the distance from mangroves for FWM: #####
enp.msc.d <- st_cast(enp.msc.d, "MULTILINESTRING") # chane polygon to shape

#calculation of the distance
dist.fwm <- st_distance(enp.msc.d, grid)
plot(dist.fwm )

# To convert distance in meters to kilometers divide by 1000.
df.fwm <- data.frame(dist = as.vector(dist.fwm)/1000,
                 st_coordinates(grid))

ext <- ext(as(grid, "Spatial"))
r <- rast( ext(ext), resolution = 1000,crs = "+proj=utm +zone=27 +ellps=intl +towgs84=-73,47,-83,0,0,0,0 +units=m +no_defs")

#convert the points to a spatial object class sf
dist.fwm_sf <- st_as_sf(df.fwm, coords = c("X", "Y")) %>%
  st_set_crs(st_crs( df.fwm))

#create the distance raster
dist.fwm_raster <- rasterize(dist.fwm_sf, r, "dist", fun = mean) 

# Polygon for high risk areas ####
high.risk <- dist.fwm_raster

high.risk[high.risk > 1.5]<- NA

plot(high.risk)

high.risk[!is.na(high.risk)] <- 1
terra::global( high.risk, sum, na.rm=T)

high.risk.shp= as.polygons(high.risk)
high.risk.sf=st_as_sf(high.risk.shp)

plot(high.risk.sf)

# Raster Vegetation Layer ####
enp.msc.d$veg <- 1
enp.fwm.d$veg <- 2

veg.fwm <- rasterize( enp.fwm.d, r, "veg", fun = mean) 
veg.msc <- rasterize( enp.msc.d, r, "veg", fun = mean) 

veg <- sum(veg.fwm, veg.msc, na.rm=T)
veg <- as.factor(veg)
ggplot() +geom_spatraster(data=veg)

# Figure:
library(ggplot2)
library(tidyterra)

ggplot() + geom_sf(data=enp)

enp.se <- read_sf("data/shapefiles/Southeastern Everglades_AOI.kml" ) 

enp <- enp %>% st_transform(crs(enp.se  ))


enp.se <- enp.se %>% st_make_valid() %>% st_transform('+proj=utm +zone=27 +ellps=intl +towgs84=-73,47,-83,0,0,0,0 +units=m +no_defs' )

plot(enp)
test <-st_transform(enp.se , st_crs(veg))

ggplot() + geom_sf(data=enp.shade)
ggplot() + geom_spatraster(data=veg)

enp.shade <- enp %>% st_transform( '+proj=utm +zone=27 +ellps=intl +towgs84=-73,47,-83,0,0,0,0 +units=m +no_defs' ) %>% st_difference(enp.se)  
ggplot() + geom_sf(data=enp.shade)

veg$veg.p <- veg$veg
veg.factor.df <- data.frame(value=1:3, veg= c("Mangrove Scrub","Marsh",  "Ecotone"))
levels(veg$veg.p) <- veg.factor.df
coltab(veg$veg.p) <- data.frame(value=1:3, col= c("#43b284", "#000099", "#fab255"))

library(tidyterra)
veg <- veg %>% terra::project(enp.shade)

simulation.1 <- 
  ggplot() + tidyterra::geom_spatraster(data = veg$veg.p, na.rm = TRUE, show.legend = T) + 
  scale_fill_manual(na.value = "transparent", values= c("#43b284", "#000099", "#fab255")) + 
  labs( fill="" ) +
  geom_spatraster_contour(data = dist.fwm_raster, breaks =seq(1, 50, 1.5), na.rm = TRUE, col="lightblue4") + 
  geom_sf( data=high.risk.sf, col="black", fill = 'transparent', linewidth=0.5) +
  geom_sf( data=enp.shade, col="black", fill = 'transparent', linewidth=0.5) +
 theme(legend.position = "bottom") + theme(axis.text=element_text(size=6.5)) +theme_bw()




save( veg, simulation.1  ,  file='data/Landscape_C.RDATA')

png(file="figures/Simulation_2024.png", width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 1200,
    pointsize = 4)
simulation.1
dev.off()


veg$Diatance <- dist.fwm_raster
veg$veg.gC <- dist.fwm_raster
veg$veg.gC[veg$veg == 1] <- -483.7289*1000
veg$veg.gC[veg$veg == 2] <- -135.0244*1000
veg$veg.gC[veg$veg == 3] <- -130.7658*1000

# Update the veg class with distance
veg$veg2 <- veg
veg$veg2[veg$Diatance > 0 & veg$Diatance  <= 3] <- 3
veg$veg2[veg$veg == 1] <- 1
veg$veg2[veg$veg == 3] <- 1
veg$veg2[veg$Diatance > 3 ] <- 2

veg$veg.gC.century <- veg$veg.gC
veg$veg.gC.century[veg$veg2 == 1] <- -291.6616*1000
veg$veg.gC.century[veg$veg2 == 2] <- -19.08183*1000
veg$veg.gC.century[veg$veg2 == 3] <- -74.01228*1000

# Subset information for the southeastern saline everglades:
enp.se <- read_sf("data/shapefiles/Southeastern Everglades_AOI.kml" ) %>% st_transform( 26917)

veg.se <- terra::crop(veg, enp.se) %>% terra::mask( enp.se)


# Annual exchange of C
terra::global(veg$veg.gC, sum, na.rm=T)/ 1000000000 # in giga grams or Gg

sum( veg$veg.gC[ veg$veg == 2], na.rm=T)/ 1000000 # in meteric ton of carbon
sum( veg$veg.gC[ veg$veg == 1], na.rm=T)/ 1000000 # in meteric ton of carbon
sum( veg$veg.gC[ veg$veg == 3], na.rm=T)/ 1000000 # in meteric ton of carbon

# area (ENP is 610460.2 ha) 
veg$area <- terra:: cellSize( veg,unit="ha")
sum(veg$area[veg$veg == 1]); sum(veg$area[veg$veg == 1])/61046
sum(veg$area[veg$veg == 2]) ; sum(veg$area[veg$veg == 2])/610460.2
sum(veg$area[veg$veg == 3]); sum(veg$area[veg$veg == 3])/61046

plot(veg$veg.gC.century)

# Make a new veg class to summarize data by!!!!!!
terra::global(veg$veg.gC.century, sum, na.rm=T)/ 1000000000 # in giga grams or Gg

sum( veg$veg.gC.century[ veg$veg2 == 2], na.rm=T)/ 1000000 # in meteric ton of carbon
sum( veg$veg.gC.century[ veg$veg2 == 1], na.rm=T)/ 1000000 # in meteric ton of carbon
sum( veg$veg.gC.century[ veg$veg2 == 3], na.rm=T)/ 1000000 # in meteric ton of carbon
 # what was 3 became mangroves in the next century!

