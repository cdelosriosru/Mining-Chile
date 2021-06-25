


# PROJECT :     	Mining-Oil-Gallup
# AUTHOR :				CDLR
# PURPOSE :				create pollution variables at the hospital level.

#--------------------------------------------------------------------
# INITIALIZE
#--------------------------------------------------------------------
#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages

#"htmlwidgets","widgetframe","leaflet"
z<-c("geobuffer","geosphere","sp","rgdal","raster","rgeos","dplyr","tidyverse","cleangeo","readstata13","geosphere","sf")
lapply(z, library, character.only = TRUE)
rm(z)


# Paths-----

user = "C:/Users/cdelo/Dropbox/" #Aquí el directorio personal, el resto es idéntico para todos.
data = paste(user,"Mining_HK_Chile/HEALTH/Datos", sep="")
dataeduc = paste(user,"Mining_HK_Chile/EDUCACION/Estimation", sep="")
datapol = paste(user,"Mining_HK_Chile/HEALTH/Datos/python_data_code/data_code3/processed/aero", sep="")
export = paste(user,"Mining_HK_Chile/HEALTH/Datos/pollute/hospitals/aero", sep="")


setwd(data)



# Read Map ------
setwd(dataeduc)
comunas_base <- readOGR(dsn="DATA/Admin_maps/Comunas", layer="comunas")
setwd(data)
salud <-  readOGR(dsn="hospitals/establecimientos_salud_todos_14052021", layer="establec_salud_todos_14_05_2021")


# ellipsoidal buffer (in degrees)----
proj<-crs(comunas_base)
salud_elips <- spTransform(salud,proj)
salud_elips <- spTransform(salud_elips,CRS=CRS("+init=epsg:9153"))
proj4string(salud_elips) <- CRS("+init=epsg:9153")

salud_elips$idsalud <- 1:nrow(salud_elips)
salud_buf_elips <-gBuffer(salud_elips, byid = TRUE, width = 1) # width is in meters


# cartesian buffer (in meters)----
proj<-crs(comunas_base)
salud_cart <- spTransform(salud,proj)
salud_cart <- spTransform(salud,CRS=CRS("+init=epsg:9155"))
proj4string(salud_cart) <- CRS("+init=epsg:9155")

salud_cart$idsalud <- 1:nrow(salud_cart)
salud_buf_cart <-gBuffer(salud_cart, byid = TRUE, width = 50000) # width is in meters


# with geosphere

salud_buf_cart_geo <- geobuffer_pts(xy = salud_cart, dist_m = 50000)
salud_buf_elips_geo <- geobuffer_pts(xy = salud_elips, dist_m = 50000)


# export to testing folder

setwd(data)

writeOGR(salud_buf_cart, dsn="test_size", layer="salud_buf_cart", driver ="ESRI Shapefile", overwrite_layer = T)
writeOGR(salud_buf_elips, dsn="test_size", layer="salud_buf_elips", driver ="ESRI Shapefile", overwrite_layer = T)
writeOGR(salud_buf_elips_geo, dsn="test_size", layer="salud_buf_elips_geo", driver ="ESRI Shapefile", overwrite_layer = T)

salud_buf_cart$area<-area(salud_buf_cart)
salud_buf_elips$area<-area(salud_buf_elips)
salud_buf_elips_geo$area<-area(salud_buf_elips_geo)

writeOGR(salud_buf_cart, dsn="test_size", layer="salud_buf_cartK", driver ="KML", overwrite_layer = T)
writeOGR(salud_buf_elips, dsn="test_size", layer="salud_buf_elipsk.kml", driver ="KML")
writeOGR(salud_buf_elips_geo, dsn=".", layer="zinc.kml", driver ="KML")


# At the end of this, we see that the cartesian and the elipsoidal with geosphere give us accurate measures of the buffers, while working with elipsoidal does not. We are going to stick with cartesians
# as it has the advantage that it keeps all the data frame. We are going to do the same for Raster?


# Now the grids-----



# Paths-----

user = "C:/Users/cdelo/Dropbox/" #Aquí el directorio personal, el resto es idéntico para todos.
data = paste(user,"Mining_HK_Chile/HEALTH/Datos", sep="")
dataeduc = paste(user,"Mining_HK_Chile/EDUCACION/Estimation", sep="")
datapol = paste(user,"Mining_HK_Chile/HEALTH/Datos/python_data_code/data_code3/processed/aero", sep="")
export = paste(user,"Mining_HK_Chile/HEALTH/Datos/pollute/hospitals/aero", sep="")


# Read Map ------
setwd(dataeduc)
#comunas_base <- readOGR(dsn="DATA/Admin_maps/Comunas", layer="comunas")

# filter out the islands
#comunas_base<-st_as_sf(comunas_base)
#comunas_land<-comunas_base %>% 
#  filter(!OBJECTID_1 %in% c("236", "235")) 
#comunas_land<-as(comunas_land,'Spatial')
#writeOGR(comunas_land, dsn="DATA/Admin_maps/Chile", layer="chile", driver ="ESRI Shapefile", overwrite_layer = T)

chile <- readOGR(dsn="DATA/Admin_maps/Chile", layer="chile")
# transform to metric system

chile_m <- spTransform(chile, CRS=CRS("+init=epsg:9155")) # si queremos nos podemos poner fancies y cortar entre 19 y 18. Por el momento lo dejamos así.

# extract extent
box<-extent(chile_m)



test<-raster(ext=box,  res=50000)
projection(test) <- CRS("+init=epsg:9155")


polygonras<-rasterize(chile_m,test)
polygonras<-rasterToPolygons(polygonras, digits=12, dissolve=FALSE)

setwd(data)
writeOGR(polygonras, dsn="test_size", layer="raster", driver ="ESRI Shapefile", overwrite_layer = T)




# bien hasta aqui

# intersect with dots?
#setwd(datapol)


#test_inter<-raster::intersect(a,pollute)
#writeRaster(test_inter, "test_rast_inter", format="GTiff")
#pollutest<-st_as_sf(pollute)
#test_otro<-raster::extract(a,pollutest,cell=TRUE)

#does not work. I will try to poligonize the raste
setwd(dataeduc)
polygonras<-rasterToPolygons(polygonras, digits=12, dissolve=FALSE)
#polygonras<-readOGR(dsn="DATA/Admin_maps/Chile", layer="polygonraster_chile")
polygonras$idpixel <- 1:nrow(polygonras)
proj4string(polygonras) <- CRS("+init=epsg:9155")
polygonras<-sp::spTransform(polygonras,CRS=CRS("+init=epsg:9184"))
cent_poly     <- as.data.frame(gCentroid(polygonras, byid = TRUE, id = polygonras@data$idpixel))
cent_poly$long_pix<-cent_poly$x
cent_poly$lati_pix<-cent_poly$y
cent_poly <- tibble::rownames_to_column(cent_poly, var = "idpixel")

















