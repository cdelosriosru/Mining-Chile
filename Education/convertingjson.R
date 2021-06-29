
#Load Packages

z<-c("rgdal", "geojsonio", "DescTools", "rjson", "dplyr","tidyverse", "sf") # some classic packages that I use. 
lapply(z, library, character.only = TRUE)
rm(z)



#EXPLORACION------------------
#wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  # USO 18 

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Uso_18_Exploracion/")


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 10, StrPos(files,".json")-1)
for(i in names){
  path=paste("Uso_18_",i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}


top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("exploracion-", z, sep="")
  dEOG <- get(paste("exploracion-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Uso_18_Exploracion = do.call(rbind, splist)

# USO 19 

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Uso_19_Exploracion/")


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 10, StrPos(files,".json")-1)
for(i in names){
  path=paste("Uso_19_",i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}


top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("exploracion-", z, sep="")
  dEOG <- get(paste("exploracion-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Uso_19_Exploracion = do.call(rbind, splist)

projection<-proj4string(Uso_18_Exploracion)
Uso_19_Exploracion <- spTransform(Uso_19_Exploracion, CRS=projection)

Exploracion=rbind(Uso_19_Exploracion,Uso_18_Exploracion)

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/")

writeOGR(Exploracion, "Exploracion" ,driver="ESRI Shapefile", layer="Exploracion")





rm(list=ls())
#EXPLOTACION 1932------------------

# USO 18 

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Uso_18_Explotacion1932/")


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 10, StrPos(files,".json")-1)
for(i in names){
  path=paste("Uso_18_",i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}


top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("explotacion1932-", z, sep="")
  dEOG <- get(paste("explotacion1932-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Uso_18_Explotacion = do.call(rbind, splist)

# USO 19 

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Uso_19_Explotacion1932/")


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 10, StrPos(files,".json")-1)
for(i in names){
  path=paste("Uso_19_",i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}


top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("explotacion1932-", z, sep="")
  dEOG <- get(paste("explotacion1932-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Uso_19_Explotacion = do.call(rbind, splist)

projection<-proj4string(Uso_18_Explotacion)
Uso_19_Explotacion <- spTransform(Uso_19_Explotacion, CRS=projection)

Explotacion=rbind(Uso_19_Explotacion,Uso_18_Explotacion)

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/")

writeOGR(Explotacion, "Explotacion1932" ,driver="ESRI Shapefile", layer="Explotacion1932")







rm(list=ls())
#EXPLOTACION 1983------------------

# USO 18 

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Uso_18_Explotacion1983/")


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 10, StrPos(files,".json")-1)
for(i in names){
  path=paste("Uso_18_",i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}


top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("explotacion1983-", z, sep="")
  dEOG <- get(paste("explotacion1983-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Uso_18_Explotacion = do.call(rbind, splist)

# USO 19 

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Uso_19_Explotacion1983_n/")


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 10, StrPos(files,".json")-1)
for(i in names){
  path=paste("Uso_19_",i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}


top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("explotacion1983-", z, sep="")
  dEOG <- get(paste("explotacion1983-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Uso_19_Explotacion = do.call(rbind, splist)

projection<-proj4string(Uso_18_Explotacion)
Uso_19_Explotacion <- spTransform(Uso_19_Explotacion, CRS=projection)

Explotacion=rbind(Uso_19_Explotacion,Uso_18_Explotacion)

setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/")

writeOGR(Explotacion, "Explotacion1983" ,driver="ESRI Shapefile", layer="Explotacion1983")



#YACIMIENTOS---- 


setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/Yacimientos/")

# fixing the geometries...

files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 3, StrPos(files,".json")-1)
for(i in names){
  path=paste(i,".json",sep="")
  assign(i, rjson::fromJSON(file = path))
}
yaci <- lapply(names,get)


yaci<-lapply(yaci, function(i){

    for (feat in 1:length(i$features)){
    
      
      i$features[[feat]]$geometry$x[i$features[[feat]]$geometry$x == "NaN"] <- 0 # this also works
      i$features[[feat]]$geometry$y[i$features[[feat]]$geometry$y == "NaN"] <- 0 # this also works
      i$features[[feat]]$geometry$x <- as.numeric(i$features[[feat]]$geometry$x) # this also works
      i$features[[feat]]$geometry$y <- as.numeric(i$features[[feat]]$geometry$y) # this also works
      
    }
  i<- rjson::toJSON(i)
  ; return(i)
}
)


for (i in seq_along(yaci)) {
  filename = paste(names[i],".json",sep="")
  writeLines(yaci[[i]], filename)
}


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 3 , StrPos(files,".json")-1)
for(i in names){
  path=paste(i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}

top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("Yacimientos-", z, sep="")
  dEOG <- get(paste("Yacimientos-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Yacimientos = do.call(rbind, splist)


Yacimientos<-st_as_sf(Yacimientos)
Yacimientos<-Yacimientos %>% 
  filter(LATITUD_SU!=0)

Yacimientos<-as(Yacimientos, "Spatial")


setwd("C:/Users/cdelo/Documents/GitHub/Mining_Chile/")

writeOGR(Yacimientos, "Yacimientos" ,driver="ESRI Shapefile", layer="Yacimientos")




# Instalaciones Minera-------

setwd("C:/Users/cdelo/Documents/GitHub/Chile_Mining_Cadastre/Instalaciones/")

# fixing the geometries...

files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 3, StrPos(files,".json")-1)
for(i in names){
  path=paste(i,".json",sep="")
  assign(i, rjson::fromJSON(file = path))
}
insta <- lapply(names,get)


insta<-lapply(insta, function(i){
  
  for (feat in 1:length(i$features)){
    
    
    i$features[[feat]]$geometry$x[i$features[[feat]]$geometry$x == "NaN"] <- 0 # this also works
    i$features[[feat]]$geometry$y[i$features[[feat]]$geometry$y == "NaN"] <- 0 # this also works
    i$features[[feat]]$geometry$x <- as.numeric(i$features[[feat]]$geometry$x) # this also works
    i$features[[feat]]$geometry$y <- as.numeric(i$features[[feat]]$geometry$y) # this also works
    
  }
  i<- rjson::toJSON(i)
  ; return(i)
}
)


for (i in seq_along(insta)) {
  filename = paste(names[i],".json",sep="")
  writeLines(insta[[i]], filename)
}


files <- list.files( pattern="*.json", full.names=TRUE, recursive=FALSE)
names <- substr(files, 3 , StrPos(files,".json")-1)
for(i in names){
  path=paste(i,".json",sep="")
  assign(i, geojson_read(path, what = "sp"))
}

top<-length(names)

splist<-list()
for (j in 1:top){
  z <- j
  sEOG <- paste("Instalaciones_Mineras-", z, sep="")
  dEOG <- get(paste("Instalaciones_Mineras-", z, sep=""))
  splist[[sEOG]] <-dEOG
}

Instalaciones = do.call(rbind, splist)


Instalaciones<-st_as_sf(Instalaciones)
#Instalaciones<-Instalaciones %>% 
#  filter(LATITUD_SU!=0)

Instalaciones<-as(Instalaciones, "Spatial")

setwd("C:/Users/cdelo/Documents/GitHub/Chile_Mining_Cadastre/")

writeOGR(Instalaciones, "Instalaciones" ,driver="ESRI Shapefile", layer="Instalaciones")






# After this the files must be reprojected using ArcGIS and a shapefile of the regions of Chile