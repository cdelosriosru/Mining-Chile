# PROJECT :     	Mining and Human Capital in Chile
# AUTHOR :				CDLR
# PURPOSE :				clean the maps to use in this project. Also create data frames to first analysis

#--------------------------------------------------------------------
# INITIALIZE
#--------------------------------------------------------------------
#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages

z<-c("sp","rgdal","raster","rgeos","dplyr","tidyverse","cleangeo","readstata13","geosphere","sf","dplyr","readxl","openxlsx","haven") # some classic packages that I use. 
lapply(z, library, character.only = TRUE)
rm(z)
#--------------------------------------------------------------------
# Paths
#--------------------------------------------------------------------

setwd("C:/Users/cdelo/Dropbox/Mining_HK_Chile/EDUCACION/Estimation/")

#--------------------------------------------------------------------
# Import Reprojected Files
#--------------------------------------------------------------------

explotacion1983 <- readOGR(dsn="DATA/Catastro/Explotacion1983", layer="Explotacion1983_projected")
exploracion <- readOGR(dsn="DATA/Catastro/Exploracion", layer="Exploracion_projected")
explotacion1932 <- readOGR(dsn="DATA/Catastro/Explotacion1932", layer="Explotacion1932_projected")
comunas <- readOGR(dsn="DATA/Admin_maps/Comunas", layer="comunas_clean_CDR")
markets <- readOGR(dsn="DATA/Admin_maps/Labor_Markets", layer="markets_clean_CDR")

#Save Projection-----------

proj<-crs(exploracion)


#Clean Comunas------

comunas$area_ha_comuna <- area(comunas)/10000
comunas_st<-st_as_sf(comunas)
comunas_st<-st_make_valid(comunas_st)
st_write(comunas_st, "comunas_clean_CDR.shp") # always use this map. 


#Create Labor Markets Shape File------
lab_mark<-read.xlsx("comunas_dta.xlsx")
lab_mark<-lab_mark %>% 
  select(cod_comuna, cod_funtionalarea)
markets_st <- comunas_st %>% left_join(.,lab_mark, by="cod_comuna")
markets_st <- markets_st %>% group_by(cod_funtionalarea) %>% summarize(area_ha_lmarket=sum(area_ha_comuna)) 
st_write(markets_st, "markets_clean_CDR.shp") # always use this map for labor markets. 

#Create Data Frames of Polygons to follow------

explotacion83<- as.data.frame(explotacion1983)
explotacion32<- as.data.frame(explotacion1932)

explotacion32<-explotacion32 %>% rename(rol_nal = pdd_rld,
                                               nom_conces = pdd_nmc,
                                               rut_titular = pdd_rtd,
                                               nom_titular = pdd_nmt,
                                               dir_titular = pdd_drt,
                                               hectareas = pdd_chc,
                                               situacion = pdd_str,
                                               huso = pdad_hs,
                                               datum = pdd_dtm,
                                               fojas = pdd_fjs,
                                               ciudad_inscrip = pdd_cdn,
                                               year = pdd_nns,
                                               id_inscrip = pdd_nmn,
                                               tipo_inscrip = pdd_tpn,
                                               ubicacion = pdd_bcc
                                          )
explotacion83<-explotacion83 %>% rename(rol_nal = pdd_rld,
                                        nom_conces = pdd_nmc,
                                        rut_titular = pdd_rtd,
                                        nom_titular = pdd_nmt,
                                        dir_titular = pdd_drt,
                                        hectareas = pdd_chc,
                                        situacion = pdd_str,
                                        huso = pdad_hs,
                                        datum = pdd_dtm,
                                        fojas = pdd_fjs,
                                        ciudad_inscrip = pdd_cdn,
                                        year = pdd_nns,
                                        id_inscrip = pdd_nmn,
                                        tipo_inscrip = pdd_tpn,
                                        ubicacion = pdd_bcc
)


                              
setwd("C:/Users/cdelo/Dropbox/Mining_HK_Chile/EDUCACION/Estimation/DATA/Catastro/")

write_dta(explotacion83, "Explotacion1983/explotacion83.dta")
write_dta(explotacion32, "Explotacion1932/explotacion32.dta")






