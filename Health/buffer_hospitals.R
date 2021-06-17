


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
z<-c("sp","rgdal","raster","rgeos","dplyr","tidyverse","cleangeo","readstata13","geosphere","sf")
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


# repare geometry----

proj<-crs(comunas_base)
salud <- spTransform(salud,proj)

# To make buffers you need to transform to cartesian 2D
salud_m <- spTransform(salud, CRS=CRS("+init=epsg:9155")) # si queremos nos podemos poner fancies y cortar entre 19 y 18. Por el momento lo dejamos así.


#--Give hospitals ID and get clean coordinates----

salud_m$idsalud <- 1:nrow(salud_m)
#salud_m$lati_sal<-salud_m@coords[,2]
#salud_m$long_sal<-salud_m@coords[,1]

salud_m$lati_sal<-salud_m$LATITUD
salud_m$long_sal<-salud_m$LONGITUD

#colegio<- na.omit(colegio,lat_cole, lon_cole)
#coord<-colegio[,c(2,1)]
#colegio_points<-SpatialPointsDataFrame(coords=coord,data = colegio,
#                                       proj4string = CRS(wgs.84))
#writeOGR(colegio_points, dsn=".", layer="colegio_points", driver ="ESRI Shapefile", overwrite_layer = T)
#colegio_points <- spTransform(colegio_points,CRS(epsg.2062)) # need this to have buffers in meters

#-----POLLUTION DATA----

# read one year of pollution Data.

y<- c(2005:2016) # years
w <- c(1:52) # weeks
x <- c(1000 , 1500) # buffers in meters. (if this changes you have to change the merge inside the loop)

setwd(datapol)

for (v in 1:length(y)){

  for (va in 1:length(w)) {
    
    nameweek<-(paste("shape_week_",w[va], sep=""))
    nameyear<-(paste(y[v],"/clipped_maps", sep=""))
    
    
    pollute<-readOGR(dsn=nameyear[1], layer=nameweek[1])
    pollute<-spTransform(pollute,CRS=CRS("+init=epsg:9155"))
    
    pollute$lati_pol<-pollute$Latitude
    pollute$long_pol<-pollute$Longitude
    pollute$week<-w[va]
    pollute$year<-y[v]
    
    for (val in 1:length(x)) { # loop for buffers
      
      #create buffer
      salud_m_buf <-gBuffer(salud_m, byid = TRUE, width = x[val]) # width is in meters
      salud_m_buf@data<-data.frame(salud_m_buf@data) # intersect does not work with tibble
      
      #intersect
      salud_m_pollute<-raster::intersect(pollute,salud_m_buf)
      salud_m_pollute<-as.data.frame(salud_m_pollute)
      
      salud_m_pollute$coords.x1 <- NULL
      salud_m_pollute$coords.x2 <- NULL
      
      # create distances
      salud_m_pollute$dist <- distHaversine(salud_m_pollute[, c('long_pol', 'lati_pol')],
                                            salud_m_pollute[, c('long_sal', 'lati_sal')],
      )
      
      salud_m_pollute<-salud_m_pollute %>% 
        mutate(dist_inv=1/dist)
      
      salud_m_pollute<-salud_m_pollute%>%
        group_by(idsalud )%>%
        summarize(contaminaw = weighted.mean(dist_inv, UVAI),
                  contamina = mean(UVAI),
                  week = first(week),
                  year = first(year))
      # En lo de arriba podemos poner todo lo que se nos ocurra en el summarize
      
      # This lines assign name to contamina variables using the buffer size
      
      namevar<-(paste("contaminaw_",x[val], sep=""))
      names(salud_m_pollute)[names(salud_m_pollute) == "contaminaw"] <- namevar[1]  # change the name of var to identify better
      
      namevar<-(paste("contamina_",x[val], sep=""))
      names(salud_m_pollute)[names(salud_m_pollute) == "contamina"] <- namevar[1]  # change the name of var to identify better
      
      name<-(paste("salud_m_pollute_",x[val], sep=""))
      print(name)
      assign(name,salud_m_pollute)   # change the name of data frame
      
    }
    
    mer<-merge(salud_m_pollute_1500,salud_m_pollute_1000, all=TRUE)
    
    name<-(paste("mer_",w[va], sep=""))
    print(name)
    assign(name,mer)   # change the name of data frame
    
    
  }
  
  all <- bind_rows(mer_1, mer_2, mer_3, mer_4, mer_5, mer_6, mer_7, mer_8, mer_9, mer_10,
                   mer_11, mer_12, mer_13, mer_14, mer_15, mer_16, mer_17, mer_18, mer_19, mer_20,
                   mer_21, mer_22, mer_23, mer_24, mer_25, mer_26, mer_27, mer_28, mer_29,mer_30,
                   mer_31, mer_32, mer_33, mer_34, mer_35, mer_36, mer_37, mer_38, mer_39, mer_40,
                   mer_41, mer_42, mer_43, mer_44, mer_45, mer_46, mer_47, mer_48, mer_49, mer_50,
                   mer_51, mer_52)
  rm(list=ls(pattern="mer_"))
  rm(list=ls(pattern="salud_m_pollute_"))
  
  name<-(paste("all_",y[v], sep=""))
  print(name)
  assign(name,all)   # change the name of data frame
  
}


#------Merge and Save as dta------

all <- bind_rows(all_2005, all_2006, all_2007, all_2008, all_2009,
                 all_2010, all_2011, all_2012, all_2013, all_2014,
                 all_2015, all_2016)
rm(list=ls(pattern="all_"))


setwd(export)


all %>% mutate_if(is.factor, as.character) -> all

#---add all the information of hospitals-----

salud_m<-as.data.frame(salud_m)
all<-merge(all,salud_m, by = "idsalud", all=TRUE)

all$coords.x1 <- NULL
all$coords.x2 <- NULL

# Export #

save.dta13(data=all,file="all_pollute.dta") # here comes all the data in a single dta.










