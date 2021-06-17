##########################################################
# author: CLDR
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


#Load Packages
pkg<-list("ggpubr","ggplot2","haven","dplyr","broom","sf","viridis","grid", "rgeos", "rgdal")
lapply(pkg, require, character.only=T)
rm(pkg)


main = "C:/Users/cdelo/Dropbox/Mining_HK_Chile/EDUCACION/Estimation"
map = paste(main,"/DATA/Admin_maps/Labor_Markets", sep="")
dat_bc = paste(main,"/DATA", sep="") #poner si esta en el hard drive
dat_a = paste(main,"/dataset_code/proccessed_data/patentes", sep="")



# I like this projection better
wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # ASUM? QUE ESTA ERA LA PROYECCI?N DE LOS COLEGIOS

# --------------theme----------------

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}
#--------------IMPORTING FILES----------------------------------------

setwd(map)
labormarket<- readOGR("funareas_vars/funareas_vars.shp")
names(labormarket)[names(labormarket) == "cdigo_funa"] <- "code_funarea"

keep_t <- c("code_funarea")


labormarket <- labormarket[,(names(labormarket) %in% keep_t)]


setwd(dat_bc)
dta_bc<-read_dta("concessions_bylabormarket.dta")
setwd(dat_a)
dta_a<-read_dta("patentes_funareas.dta")
names(dta_a)[names(dta_a) == "cdigo_funarea"] <- "code_funarea"





# fortify, i.e., make ggplot2-compatible
labormarketf <- fortify(labormarket, region = "code_funarea")
labormarketf<-labormarketf %>% 
  mutate(id = as.numeric(id))


# now we join the thematic data
map_dat <- labormarketf %>% left_join(dta_bc, by = c("id" = "code_funarea"))
map_dat <- map_dat %>% left_join(dta_a, by = c("id" = "code_funarea"))


map_dat<-map_dat %>% 
  mutate(tot_per_pre=tot_per_pre*100,
         tot_numpersqm_pre=tot_numpersqm_pre*100)

# Create quintiles

no_classes <- 9
labels <- c()

quantiles <- quantile(map_dat$rpatentepc, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " - ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
map_dat$patente_quantiles <- cut(map_dat$rpatentepc, 
                                     breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)




# Mean socioeconomic stratum ----------------------------------------------

p <- ggplot() +
  # municipality polygons (watch how I 
  # use the new variable for the fill aesthetic)
  geom_polygon(data = map_dat, aes(fill = patente_quantiles, 
                                    x = long, 
                                    y = lat, 
                                    group = group)) +
  # municipality outline
  geom_path(data = map_dat, aes(x = long, 
                                 y = lat, 
                                 group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Switzerland's regional demographics", 
       subtitle = "Average age in Swiss municipalities, 2015", 
       caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "magma",
    name = "Average age",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p





p<- ggplot(map_dta) +
  # municipality polygons (watch how I 
  # use the new variable for the fill aesthetic)
  geom_sf(aes(fill = patente_quantiles)) +
  coord_sf(datum=st_crs(3857))  +
  # municipality outline
  geom_path(data = map_dta, aes(group = code_funarea), 
            color = "white", size = 0.1) +
  theme_void() +
  labs(x = NULL, 
       y = NULL, 
       title = "Switzerland's regional demographics", 
       subtitle = "Average age in Swiss municipalities, 2015", 
       caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016") +
  # now the discrete-option is used, 
  # and we use guide_legend instead of guide_colourbar
  scale_fill_viridis(
    option = "magma",
    name = "Average age",
    discrete = T,
    direction = -1,
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = 'top',
      reverse = T
    ))
p





p1<-ggplot(map_dta) +
  geom_sf(aes(fill = rpatentepc), size = 0.0001, color = "white") +  
  coord_sf(datum=st_crs(3857))  +
  scale_fill_viridis(option = "magma", direction = -1, name="")+
  theme_void()


p1
+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) 


p1

  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
#    caption = "July - January ; CoVIDA Data")


#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly2) +
  geom_sf(aes(fill = positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="March 3th")
#    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("raw_positivity_map.pdf")

p1<-ggplot(sf_poly1) +
  geom_sf(aes(fill = w_positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="November 30th")
#    caption = "July - January ; CoVIDA Data")


#ggsave("covida_jul_oct.png")


p2<-ggplot(sf_poly2) +
  geom_sf(aes(fill = w_positive), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black")) +
  #theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black")) +
  #theme(plot.caption=element_text(size=7,hjust=0.4, face="italic", color="black", vjust=2)) +
  labs(title="March 3th")
#    caption = "July - January ; CoVIDA Data")

#ggsave("covida_jul_jan.png")

ggarrange(p1, p2, ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("raw_weighted_positivity_map.pdf")



ggplot(loc_data) +
  geom_sf(aes(fill = estrato_prom), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="")+
  theme_void()
ggsave(here("views/stratum.pdf"))

# Accumulated Cases -------------------------------------------------------

loc_calc<-readRDS(here("Data/temp/calculations_locality.rds"))


loc_data<-left_join(loc_data,loc_calc) %>%
  filter(!is.na(acumm_covid_covida))


ggplot(loc_data ) +
  geom_sf(aes(fill = acumm_covid_covida), size = 0.0001, color = "black") +  
  coord_sf(crs = wgs.84)  +
  scale_fill_continuous(low='thistle2', high='darkred', 
                        guide='colorbar', name="", 
                        oob = scales::squish)+
  theme_void()+
  facet_wrap(. ~ grp) +
  theme(plot.title = element_text(hjust = 0.5))+
  
  theme(plot.title=element_text(size=10,  colour="black"),
        panel.spacing = unit(8, "lines"))  
ggsave(here("views/raw_positivity_map.pdf"))