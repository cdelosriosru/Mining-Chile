# PROJECT :     	Mining and Human Capital in Chile
# AUTHOR :				CDLR
# PURPOSE :				create data set with mining cadastre

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


#--------Comunas---------
comunas <- readOGR(dsn="DATA/Admin_maps/Comunas", layer="comunas")
comunas$area_ha_comuna<-(area(comunas)/1000000)
comunas_st<-st_as_sf(comunas)
st_is_valid(comunas_st) # so you always have to fix the st of comunas apparenttly.
comunas_st<-st_make_valid(comunas_st)
comunas_st$cod_comuna<-as.double(comunas_st$cod_comuna)

#Save Projection needed to transform-----------

proj<-crs(comunas)

# Labor Markets ------

lab_mark<-read.xlsx("comunas_dta.xlsx")

lab_mark<-lab_mark %>% 
  select(cod_comuna, cod_funtionalarea)

markets_st <- comunas_st %>% 
  left_join(.,lab_mark, by="cod_comuna")

markets_st <- markets_st %>% 
  group_by(cod_funtionalarea)%>%
  summarize(area_ha_lmarket=sum(area_ha_comuna)) 

st_write(markets_st, "DATA/Admin_maps/Labor_Markets/markets_clean.shp") # always use this map for labor markets. 

markets <- readOGR(dsn="DATA/Admin_maps/Labor_Markets", layer="markets_clean")
markets_st<-st_as_sf(markets)




#--------------------------------------------------------------------
# Import Mining Files
#--------------------------------------------------------------------

explotacion1983 <- readOGR(dsn="DATA/Catastro/Explotacion1983", layer="Explotacion1983_projected")
exploracion <- readOGR(dsn="DATA/Catastro/Exploracion", layer="Exploracion_projected")
explotacion1932 <- readOGR(dsn="DATA/Catastro/Explotacion1932", layer="Explotacion1932_projected")






#-create element list

df <- list(explotacion1983,exploracion,explotacion1932)

# Reproject Maps----

df <-lapply(df, function(x){ spTransform(x,proj)})






#Areas----

df <-lapply(df, function(x){ x$area_ha <- (area(x)/1000000);return(x)})
df <-lapply(df, function(x){ x$check_ha <- (x$area_ha-(x$pdd_chc/100));return(x)})

lapply(df, function(x){ summary(x$check_ha)}) # my measure of hectares is different from theirs...we can use both and simply see what happens

#Indicator-----
df <-lapply(df, function(x){ x$mina <- 1;return(x)})




#Create Points DF------
df <-lapply(df, function(x){ x <-SpatialPointsDataFrame(gCentroid(x, byid=TRUE), 
                                                        x@data, match.ID=FALSE)})


#Transform Points-----
df <-lapply(df, function(x){ x <-st_as_sf(x)})

#Intersect--------

dfc <-lapply(df, function(x){ x <-st_intersection(comunas_st,x)})
dfl <-lapply(df, function(x){ x <-st_intersection(markets_st,x)})




#Convert to DataFrames----


#dff <-lapply(df, function(x){ st_geometry(x) <- NULL;return(x)})
dffc <-lapply(dfc, function(x){ x<-x%>% st_drop_geometry()})
dffc <-lapply(dffc, function(x){ x <-as.data.frame(x)})

dffl <-lapply(dfl, function(x){ x<-x%>% st_drop_geometry()})
dffl <-lapply(dffl, function(x){ x <-as.data.frame(x)})



#Add Labor Markets to merge later------
lab_mark<-read.xlsx("comunas_dta.xlsx")
lab_mark<-lab_mark %>% 
  select(cod_comuna, cod_funtionalarea)
dffc<-lapply(dffc, function(x) x<-x %>% rename(cod_comuna = cod_comuna))
dffc <-lapply(dffc, function(x){ x <- x %>% left_join(.,lab_mark, by="cod_comuna")})



#rename vars---------

dffc<-lapply(dffc, function(x) x<-x %>% rename(rol_nal = pdd_rld,
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
                                                ubicacion = pdd_bcc,
                                                cod_region = codregion,
                                                area_ha_comuna = area_ha_comuna))
dffl<-lapply(dffl, function(x) x<-x %>% rename(rol_nal = pdd_rld,
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
                                               ubicacion = pdd_bcc,
                                               cod_funtionalarea = cd_fntn,
                                               area_ha_lmarket=ar_h_lm))

#Calculate whatever you want----------


dff_c_c<-lapply(dffc, function(x) x<- x %>% 
                filter(situacion == "CONSTITUIDA") %>% 
                #filter(!is.na(year)) %>% #Just filter them in STATA so you dont have to make a double loop for exploracion
                group_by(cod_comuna, cod_region , year) %>% 
                summarise(num_c_c=sum(mina),
                          area_cam_c_c=sum(area_ha),
                          area_ori_c_c=sum(hectareas,na.rm=T),
                          area_comuna=first(area_ha_comuna),
                          area_cam_c_c_sd=sd(area_ha),
                          cod_funtionalarea=first(cod_funtionalarea),
                          .groups='drop'
                          ) %>% 
                  ungroup() %>% 
                  mutate(per_cam_c_c=(area_cam_c_c/area_comuna),
                         per_ori_c_c=(area_ori_c_c/area_comuna)
                  )
                         
                )


dff_c_t<-lapply(dffc, function(x) x<- x %>% 
                  filter(situacion == "EN TRAMITE") %>%
                  #filter(!is.na(year)) %>% #this do not have it..
                  group_by(cod_comuna,  year) %>% 
                  summarise(num_c_t=sum(mina),
                            area_cam_c_t=sum(area_ha),
                            area_ori_c_t=sum(hectareas,na.rm=T),
                            area_comuna=first(area_ha_comuna),
                            area_cam_c_t_sd=sd(area_ha),
                            cod_funtionalarea=first(cod_funtionalarea),
                            .groups='drop'
                  ) %>% 
                  ungroup() %>% 
                  mutate(per_cam_c_t=(area_cam_c_t/area_comuna),
                         per_ori_c_t=(area_ori_c_t/area_comuna)
                  )
              )




dff_l_c<-lapply(dffl, function(x) x<- x %>% 
                  filter(situacion == "CONSTITUIDA") %>% 
                  #filter(!is.na(year)) %>% #Just filter them in STATA so you dont have to make a double loop for exploracion
                  group_by(cod_funtionalarea,  year) %>% 
                  summarise(num_l_c=sum(mina),
                            area_cam_l_c=sum(area_ha),
                            area_ori_l_c=sum(hectareas,na.rm=T),
                            area_mercado=first(area_ha_lmarket),
                            area_cam_l_c_sd=sd(area_ha),
                            .groups='drop'
                  ) %>% 
                  ungroup() %>% 
                  mutate(per_cam_l_c=(area_cam_l_c/area_mercado),
                         per_ori_l_c=(area_ori_l_c/area_mercado)
                        )
                )




dff_l_t<-lapply(dffl, function(x) x<- x %>% 
                  filter(situacion == "EN TRAMITE") %>% 
                  #filter(!is.na(year)) %>% #this do not have it..
                  group_by(cod_funtionalarea, year) %>% 
                  summarise(num_l_t=sum(mina),
                            area_cam_l_t=sum(area_ha),
                            area_ori_l_t=sum(hectareas,na.rm=T),
                            area_mercado=first(area_ha_lmarket),
                            area_cam_l_t_sd=sd(area_ha),
                            .groups='drop'
                  ) %>% 
                  ungroup() %>% 
                  mutate(per_cam_l_t=(area_cam_l_t/area_mercado),
                         per_ori_l_t=(area_ori_l_t/area_mercado)
                  )
)


#Export-----
#(it is easier to merge in stata)

setwd("C:/Users/cdelo/Dropbox/Mining_HK_Chile/EDUCACION/Estimation/Data/Catastro/harm2")

names <-c("explotacion1983","exploracion","explotacion1932")


for (i in seq_along(dff_c_c)) {
  filename = paste(names[i],"c_c.dta",sep="")
  write_dta(dff_c_c[[i]], filename)
}

for (i in seq_along(dff_c_t)) {
  filename = paste(names[i],"c_t.dta",sep="")
  write_dta(dff_c_t[[i]], filename)
}

for (i in seq_along(dff_l_c)) {
  filename = paste(names[i],"l_c.dta",sep="")
  write_dta(dff_l_c[[i]], filename)
}

for (i in seq_along(dff_l_t)) {
  filename = paste(names[i],"l_t.dta",sep="")
  write_dta(dff_l_t[[i]], filename)
}


#manage in Stata....

