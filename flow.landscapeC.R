#WZ simulation2:

# calculate distance from magroves for ecotone and FWM: https://dominicroye.github.io/en/2019/calculating-the-distance-to-the-sea-in-r/

# Import ENP and convert to a grid. 
library(sf)
library(terra)
library(tidyterra)

# Import vegetation layers and simplify: ####
enp.eco <- read_sf(dsn="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles", layer="SaltMarsh" )
enp.Prairie <- read_sf(dsn="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles", layer="Prairie" )
enp.fwm <- read_sf(dsn="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles", layer="FreshwaterMarsh" )

enp.msl <- read_sf(dsn="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles", layer="MangroveShrubland" )
enp.ms <- read_sf(dsn="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles", layer="MangroveScrub" )

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

ggplot() + geom_sf(data=enp.msc.d )

# Import ENP and make a grid ####
enp <- read_sf(dsn="/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles", layer="ENP")
enp.trans <- enp %>% st_transform(26917)
enp.grid <- st_make_grid(enp.trans , cellsize = 500, what = "centers")

#only extract the points in the limits of your AOI
grid <- st_intersection(enp.grid, enp.fwm.d )   

# Calculate the distance from mangroves for FWM: #####
enp.msc.d <- st_cast(enp.msc.d, "MULTILINESTRING") # chane polygon to shape

#calculation of the distance
dist.fwm <- st_distance(enp.msc.d, grid)
ggplot() + geom_sf(data=enp) +  geom_sf(data=enp.msc.d )+  geom_sf(data=grid )

# To convert distance in meters to kilometers divide by 1000.
df.fwm <- data.frame(dist = as.vector(dist.fwm)/1000,
                     st_coordinates(grid))

#ext <- ext(as(enp.trans), "Spatial"))
r <- rast( ext(enp.trans), resolution = 1000 ,crs = enp.trans)
ggplot() + geom_sf(data=enp) +  geom_sf(data=enp.msc.d )+  geom_sf(data=enp.trans)

#convert the points to a spatial object class sf
dist.fwm_sf <- st_as_sf(df.fwm, coords = c("X", "Y")) %>%
  st_set_crs(26917)

#create the distance raster
dist.fwm_raster <- rasterize(dist.fwm_sf, r, "dist", fun = mean) 
plot(dist.fwm_raster)

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


# Figure:
library(ggplot2)
library(tidyterra)

enp.se <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/Southeastern Everglades_AOI.kml" ) 

enp.se <- enp.se %>% st_make_valid() %>% st_transform(26917) %>% st_buffer(4000)
enp.shade <- enp %>% st_transform(26917) %>% st_difference(enp.se)  

veg$veg.p <- veg$veg
veg.factor.df <- data.frame(value=c(2,3,1), veg= c("Freshwater",  "Ecotone","Saline"))
levels(veg$veg.p) <- veg.factor.df
coltab(veg$veg.p) <- data.frame(value=c(2,3,1), col= c( "#000099", "#fab255","#43b284"))


veg.se <- veg %>% terra::crop( enp.shade) %>% mask(enp.shade)
plot(veg.se )

ggplot() + geom_spatraster(data = veg$veg.p, na.rm = TRUE,show.legend = T) +  
  scale_fill_manual(na.value = "transparent", values= c("#43b284", "#000099", "#fab255"),
                    labels =c("Freshwater",  "Ecotone","Saline"))+
  labs( fill="" ) +
  geom_spatraster_contour(data = dist.fwm_raster, breaks =seq(1, 50, 1.5), na.rm = TRUE, col="lightblue4")+
  geom_sf(data= enp.shade, alpha=0.5, col="transparent") + theme_bw()


simulation.1 <- ggplot() + geom_spatraster(data = veg$veg.p, na.rm = TRUE,show.legend = T) +  
  scale_fill_manual(na.value = "transparent", values= c("#43b284", "#000099", "#fab255"),
                    labels =c("Freshwater",  "Ecotone","Saline"))+
  labs( fill="" ) +
  geom_spatraster_contour(data = dist.fwm_raster, breaks =seq(1, 50, 1.5), na.rm = TRUE, col="lightblue4")+
  geom_sf(data= enp.shade, alpha=0.5, col="transparent") + theme_bw()

writeRaster( veg, 'data/landscapeC.tif',overwrite=TRUE)
writeRaster( dist.fwm_raster, 'data/landscapeC_dist.tif', overwrite=TRUE)
save(enp.shade,  file='data/Landscape_C.RDATA')

png(file="/Volumes/MaloneLab/Research/ENP/The Whitezone/WL+SAL/Figures/Simulation_2024.png", width     =4.8,
    height    = 3.5,
    units     = "in",
    res       = 1200,
    pointsize = 4)
simulation.1
dev.off()

# Subset by the se!
veg$Diatance <- dist.fwm_raster
veg$veg.gC <- dist.fwm_raster
veg$veg.gC[veg$veg == 1] <- -294*1000000 # there are 1000000 sq meters in an 1k resolution area
veg$veg.gC[veg$veg == 2] <- -47*1000000
veg$veg.gC[veg$veg == 3] <- -81*1000000

# Update the veg class with distance
veg$veg2 <- veg
veg$veg2[veg$Diatance > 0 & veg$Diatance  <= 3] <- 3
veg$veg2[veg$veg == 1] <- 1
veg$veg2[veg$veg == 3] <- 1
veg$veg2[veg$Diatance > 3 ] <- 2

veg$veg.gC.century <- veg$veg.gC
veg$veg.gC.century[veg$veg2 == 1] <- -294*1000000
veg$veg.gC.century[veg$veg2 == 2] <- -47*1000000
veg$veg.gC.century[veg$veg2 == 3] <- -81*1000000

# Subset information for the southeastern saline everglades:

enp.se <- read_sf("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/Southeastern Everglades_AOI.kml" ) 

veg.se <- terra::crop(veg, enp.se) %>% terra::mask( enp.se)


# Annual exchange of C
terra::global(veg.se$veg.gC, sum, na.rm=T)/ 1000000000  # Giga ton of carbon

FWM.Gt <- sum( veg.se$veg.gC[ veg.se$veg == 2], na.rm=T)/ 1000000000 # in meteric ton of carbon
MS.Gt <-sum( veg.se$veg.gC[ veg.se$veg == 1], na.rm=T)/ 1000000000 # in meteric ton of carbon
BE.Gt <- sum( veg.se$veg.gC[ veg.se$veg == 3], na.rm=T)/ 1000000000 # in meteric ton of carbon
FWM.Gt + MS.Gt + BE.Gt

# area (ENP is 610460.2 ha) 
veg.se$area <- terra:: cellSize( veg.se,unit="ha")
sum(veg.se$area[veg.se$veg == 1]); sum(veg.se$area[veg.se$veg == 1])/610670*100
sum(veg.se$area[veg.se$veg == 2]) ; sum(veg.se$area[veg.se$veg == 2])/610670*100
sum(veg.se$area[veg.se$veg == 3]); sum(veg.se$area[veg.se$veg == 3])/610670*100

(sum(veg.se$area[veg.se$veg == 1])/610670*100) + (sum(veg.se$area[veg.se$veg == 2])/610670*100) + (sum(veg.se$area[veg.se$veg == 3])/610670*100)
plot(veg.se$veg.gC.century)


terra::global(veg.se$veg.gC.century, sum, na.rm=T)/ 1000000000  # in giga grams or Gg
sum( veg.se$veg.gC.century[ veg.se$veg2 == 2], na.rm=T)/ 1000000000 # in meteric ton of carbon
sum( veg.se$veg.gC.century[ veg.se$veg2 == 1], na.rm=T)/ 1000000000 # in meteric ton of carbon
sum( veg.se$veg.gC.century[ veg.se$veg2 == 3], na.rm=T)/ 1000000000 # in meteric ton of carbon

1-115/131

write_sf( enp.shade,"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/ENP_WZ_2024_MS/data/shapefiles/SE-Shade.shp" )

