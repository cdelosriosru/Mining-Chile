

# PROJECT :     	Mining-Oil-Gallup
# AUTHOR :				CDLR
# PURPOSE :				create pollution variables at the hospital level.

#--------------------------------------------------------------------
# INITIALIZE
#--------------------------------------------------------------------
#Clean the workspace
rm(list=ls())
cat("\014")


#Load Packages

#"htmlwidgets","widgetframe","leaflet"
z<-c("sf","sp","rgdal","raster","rgeos","dplyr","tidyverse","cleangeo","readstata13","geosphere","sf")
lapply(z, library, character.only = TRUE)
rm(z)


# Paths-----

user = "C:/Users/cdelo/Dropbox/" #Aquí el directorio personal, el resto es idéntico para todos.
data = paste(user,"Mining_HK_Chile/HEALTH/Datos", sep="")
dataeduc = paste(user,"Mining_HK_Chile/EDUCACION/Estimation", sep="")
datapol = paste(user,"Mining_HK_Chile/HEALTH/Datos/python_data_code/data_code3/processed/aero", sep="")
exportpix = paste(user,"Mining_HK_Chile/HEALTH/Datos/pollute/pixels/aero", sep="")
exportpob = paste(user,"Mining_HK_Chile/HEALTH/Datos/pollute/centro_poblado/aero", sep="")
# Parameters----

y<- c(2005:2016) # years
w <- c(1:52) # weeks
x <- c(50000 , 25000) # pixel size in meters

# Chile Map and Raster Basics------
setwd(dataeduc)
#comunas_base <- readOGR(dsn="DATA/Admin_maps/Comunas", layer="comunas")
# filter out the islands
#comunas_base<-st_as_sf(comunas_base)
#comunas_land<-comunas_base %>% 
#  filter(!OBJECTID_1 %in% c("236", "235")) 
#comunas_land<-as(comunas_land,'Spatial')
#writeOGR(comunas_land, dsn="DATA/Admin_maps/Chile", layer="chile", driver ="ESRI Shapefile", overwrite_layer = T)

chile <- readOGR(dsn="DATA/Admin_maps/Chile", layer="chile")
chile_m <- spTransform(chile, CRS=CRS("+init=epsg:9155")) ## transform to metric system; si queremos nos podemos poner fancies y cortar entre 19 y 18. Por el momento lo dejamos así.
box<-extent(chile_m) # necesario para el loop.


rm(chile) # free some space

# Centros Poblados -----
# Los datos se pueden acceder en https://ideocuc-ocuc.hub.arcgis.com/datasets/9db40d6ef39a4b82b7ff21bf9b3066fe_0/explore?location=-23.611789%2C-70.305950%2C9.00


setwd(dataeduc)

centpob <- readOGR(dsn="DATA/Admin_maps/centros_poblados_adj", layer="centros_poblados_adj") 
centpob<-spTransform(centpob, CRS=CRS("+init=epsg:9155")) # para sacar el centroide en long/lat
centpob$area<-area(centpob) 
centpob_clean<-st_as_sf(centpob) #es más fácil trabajarlo en sf format
centpob_clean$cod_comuna<-as.numeric(centpob_clean$COMUNA_201) # este es el código de comunas actualizado

# se puede filtrar de muchas maneras ya que no siempre hay ciudad para todas las comunas.
# En el caso de que haya ciudad, dejo ciudad, si hay más de una ciudad dejo aquella con el area más grande, si no hay ciudad dejo el centro poblado más grande. 

centpob_clean<-centpob_clean %>% 
  group_by(cod_comuna) %>% 
  mutate(max_ar=max(area),
         maxi= if_else(max_ar==area,1,0,NA_real_),
         hayci = if_else(CATEGORIA == "CIUDAD",1,0,NA_real_)) %>%
  mutate(keep = if_else(hayci == 1 , 1, 0, NA_real_)) %>% 
  mutate(keep = if_else(keep == 1 & maxi == 1 , 1 , 0 , NA_real_)) %>% 
  mutate(keep = if_else(keep == 0 & maxi == 1, 1 , keep, NA_real_)) %>% 
  filter(keep == 1)

centpob_clean<-as(centpob_clean, "Spatial") # ahora sí necesito sacar centroides y todo eso es más facil en sp format

centpob_clean$idcom <- 1:nrow(centpob_clean)
proj4string(centpob_clean) <- CRS("+init=epsg:9155")
centpob_clean_cent<-sp::spTransform(centpob_clean,CRS=CRS("+init=epsg:9153"))
centpob_clean_cent <- as.data.frame(gCentroid(centpob_clean_cent, byid = TRUE, id = centpob_clean_cent@data$idcom))
names(centpob_clean_cent)[names(centpob_clean_cent) == "x"] <- "long_pob"  # change the name of var to identify better
names(centpob_clean_cent)[names(centpob_clean_cent) == "y"] <- "lati_pob"  # change the name of var to identify better
centpob_clean_cent <- tibble::rownames_to_column(centpob_clean_cent, var = "idcom")

centpob_clean<-as.data.frame(centpob_clean)

centpob_clean<-merge(centpob_clean,centpob_clean_cent, all = TRUE)
names(centpob_clean)[names(centpob_clean) == "cod_comuna"] <- "cod_comuna_pix"  # el merge se hace con el cod comuna del pixel


centpob_clean<- centpob_clean %>% 
  select(cod_comuna_pix, long_pob, lati_pob) # esta es la info que enrealidad me interesa

rm(centpob_clean_cent,centpob) # free some space


# Loop that makes it all ------


for (val in 1:length(x)) { # loop for buffers
  
 # create raster, polygonize, and add centroid in long/lat
  
  base_raster<-raster(ext=box,  res=x[val])
  projection(base_raster) <- CRS("+init=epsg:9155")
  
  chile_m$cod_comuna<-as.numeric(chile_m$cod_comuna)
  polygonras<-rasterize(chile_m,base_raster, "cod_comuna")
  polygonras<-rasterToPolygons(polygonras, digits=12, dissolve=FALSE)
  
  
  
  polygonras$idpixel <- 1:nrow(polygonras)
  proj4string(polygonras) <- CRS("+init=epsg:9155")
  cent_poly<-sp::spTransform(polygonras,CRS=CRS("+init=epsg:9153"))
  cent_poly     <- as.data.frame(gCentroid(cent_poly, byid = TRUE, id = cent_poly@data$idpixel))
  names(cent_poly)[names(cent_poly) == "x"] <- "long_pix"  # change the name of var to identify better
  names(cent_poly)[names(cent_poly) == "y"] <- "lati_pix"  # change the name of var to identify better
  
  cent_poly <- tibble::rownames_to_column(cent_poly, var = "idpixel")
  


   setwd(datapol)
  
  for (v in 1:length(y)){ # loop for each year
    
    for (va in 1:length(w)) { # loop for each week
      
      #read pollution shps, intersect with polygonized raster 
      
      nameweek<-(paste("shape_week_",w[va], sep=""))
      nameyear<-(paste(y[v],"/clipped_maps", sep=""))
      pollute<-readOGR(dsn=nameyear[1], layer=nameweek[1])
  
      names(pollute)[names(pollute) == "Latitude"] <- "lati_pol"  # change the name of var to identify better
      names(pollute)[names(pollute) == "Longitude"] <- "long_pol"  # change the name of var to identify better
  
      pollute$week<-w[va]
      pollute$year<-y[v]
  
      pollute<-spTransform(pollute,CRS=CRS("+init=epsg:9155"))
      pollute$cod_comuna<-as.numeric(pollute$cod_comuna)
    
      pixel_m_pollute<-raster::intersect(pollute,polygonras) ## si matriz excede el límite toca hacerlo por chunks o depronto cambiar a sf
      pixel_m_pollute<-merge(pixel_m_pollute,cent_poly, all=TRUE)
    
      pixel_m_pollute<-as.data.frame(pixel_m_pollute)
      names(pixel_m_pollute)[names(pixel_m_pollute) == "layer"] <- "cod_comuna_pix"  # change the name of var to identify better
      pixel_m_pollute$coords.x1 <- NULL
      pixel_m_pollute$coords.x2 <- NULL
      
      
      
      # create distance to pixel centroid and collapse creating pollution measures
      
      pixcol <- pixel_m_pollute
      pixcol$distpix <- distHaversine(pixcol[, c('long_pol', 'lati_pol')],
                                      pixcol[, c('long_pix', 'lati_pix')],
      )
      
      pixcol<-pixcol %>% 
        mutate(distpix_inv=1/distpix,
               puntos = 1)
  
  
      pixcol<-pixcol%>%
        group_by(idpixel )%>%
        summarize(contaminaw = weighted.mean(distpix_inv, UVAI),
                  contamina = mean(UVAI),
                  contaminamax = max(UVAI),
                  contaminamin = min(UVAI),
                  puntos = sum(puntos),
                  week = first(week),
                  year = first(year))
      
      name<-(paste("pixcol_",w[va], sep=""))
      print(name)
      assign(name,pixcol)  
      
      # create distance to centro poblado and collapse creating pollution measures
    
      
      #aquí hago el merge con centros poblados.
      
      
      
      pixel_m_pollute<-pixel_m_pollute %>% 
        select(cod_comuna_pix, week, year, lati_pol, long_pol, UVAI) # esta es la info que en realidad me interesa
      
      
      pob_pix<-left_join(centpob_clean,pixel_m_pollute, by="cod_comuna_pix") # como no están los codcomunas limpios este me da menos obs que el total de puntos de pollute
      
      pob_pix<-left_join(pixel_m_pollute,centpob_clean, by="cod_comuna_pix") # este y el anterior tienene que ser iguales en el mundo ideal
      
      pob_pix$distpob <- distHaversine(pob_pix[, c('long_pob', 'lati_pob')],
                                       pob_pix[, c('long_pol', 'lati_pol')],
      )
      
      pob_pix<-pob_pix %>% 
        mutate(distpob_inv=1/distpob,
               puntos = 1)
      
      
      pob_pix<-pob_pix%>%
        group_by(cod_comuna_pix )%>%
        summarize(contaminaw = weighted.mean(distpob_inv, UVAI),
                  contamina = mean(UVAI),
                  contaminamax = max(UVAI),
                  contaminamin = min(UVAI),
                  puntos = sum(puntos),
                  week = first(week),
                  year = first(year))
      
      name<-(paste("pob_pix_",w[va], sep=""))
      print(name)
      assign(name,pob_pix)  
      
      
    }
  
    
    # bind every week of the pixel level measures
  
  
      all <- bind_rows(pixcol_1, pixcol_2, pixcol_3, pixcol_4, pixcol_5, pixcol_6, pixcol_7, pixcol_8, pixcol_9, pixcol_10,
                       pixcol_11, pixcol_12, pixcol_13, pixcol_14, pixcol_15, pixcol_16, pixcol_17, pixcol_18, pixcol_19, pixcol_20,
                       pixcol_21, pixcol_22, pixcol_23, pixcol_24, pixcol_25, pixcol_26, pixcol_27, pixcol_28, pixcol_29,pixcol_30,
                       pixcol_31, pixcol_32, pixcol_33, pixcol_34, pixcol_35, pixcol_36, pixcol_37, pixcol_38, pixcol_39, pixcol_40,
                       pixcol_41, pixcol_42, pixcol_43, pixcol_44, pixcol_45, pixcol_46, pixcol_47, pixcol_48, pixcol_49, pixcol_50,
                       pixcol_51, pixcol_52)
      
      rm(list=ls(pattern="pixcol_"))
      
  
      name<-(paste("all_",y[v], sep=""))
      print(name)
      assign(name,all)   # change the name of data frame
      
      
      
      
    # Bind every week at the centro poblado level 
      
    allpob <- bind_rows(pob_pix_1, pob_pix_2, pob_pix_3, pob_pix_4, pob_pix_5, pob_pix_6, pob_pix_7, pob_pix_8, pob_pix_9, pob_pix_10,
                       pob_pix_11, pob_pix_12, pob_pix_13, pob_pix_14, pob_pix_15, pob_pix_16, pob_pix_17, pob_pix_18, pob_pix_19, pob_pix_20,
                       pob_pix_21, pob_pix_22, pob_pix_23, pob_pix_24, pob_pix_25, pob_pix_26, pob_pix_27, pob_pix_28, pob_pix_29,pob_pix_30,
                       pob_pix_31, pob_pix_32, pob_pix_33, pob_pix_34, pob_pix_35, pob_pix_36, pob_pix_37, pob_pix_38, pob_pix_39, pob_pix_40,
                       pob_pix_41, pob_pix_42, pob_pix_43, pob_pix_44, pob_pix_45, pob_pix_46, pob_pix_47, pob_pix_48, pob_pix_49, pob_pix_50,
                       pob_pix_51, pob_pix_52)
      
      rm(list=ls(pattern="pob_pix_"))
      
      
      name<-(paste("allpob_",y[v], sep=""))
      print(name)
      assign(name,allpob)   # change the name of data frame
      
      
  }
   
   # bind all the years of the pixel level measures
  
  all <- bind_rows(all_2005, all_2006, all_2007, all_2008, all_2009,
                   all_2010, all_2011, all_2012, all_2013, all_2014,
                   all_2015, all_2016)
  rm(list=ls(pattern="all_"))
  
  
  # bind all the years of the centro poblado level measures
  
  allpob <- bind_rows(allpob_2005, allpob_2006, allpob_2007, allpob_2008, allpob_2009,
                   allpob_2010, allpob_2011, allpob_2012, allpob_2013, allpob_2014,
                   allpob_2015, allpob_2016)
  rm(list=ls(pattern="allpob_"))
  
  
  # edit and export pixel level measures
 
  setwd(exportpix)
  all %>% mutate_if(is.factor, as.character) -> all
  
  namebase<-(paste("all_pixel_",x[val], ".dta", sep=""))

  save.dta13(data=all,file=namebase[1]) # here comes all the data in a single dta.
  
 
  # edit and export centro poblado level measures
  
  setwd(exportpob)
  allpob %>% mutate_if(is.factor, as.character) -> allpob
  
  namebase<-(paste("all_pob_",x[val],".dta", sep=""))
  
  save.dta13(data=allpob,file=namebase[1]) # here comes all the data in a single dta.
  
  
   
}

  
  

  
