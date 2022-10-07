######mapping catchments 
library(sf)
library(raster)
library(RColorBrewer)
library(ggsn)
library(viridis)
library(maptools)
#library(ggplot2)
library(grid)
library(tidyverse)


#read files
LargeData_Catchments <- read_csv("../Data/LargeCatchments_T1.csv")
LargeC<-LargeData_Catchments %>%
  select(c(`Watershed #`,`Watershed name`,Latitude,Longitude))

SmallData_Catchments <- read_csv("../Data/SmallCatchments_T2.csv")
SmallC<-SmallData_Catchments %>%
  select(c(`Watershed #`,`Watershed name`,Latitude,Longitude))

NewData_Catchments <- read_csv("../Data/NewCatchments_T3.csv")
NewC<-NewData_Catchments %>%
  select(c(`Watershed #`,`Watershed name`,Latitude,Longitude))

#combine datasets
all_catchments<-bind_rows(LargeC,SmallC,NewC) 

#plot

# extaraET0 <- raster("../Data/extraET0.tif")
# # regular sample of 500k cells
# 
# ET0 <- sampleRegular(extaraET0, 
#                      size = 5e5, 
#                      asRaster = TRUE) %>% 
#   as.data.frame(xy = TRUE, 
#                 na.rm = TRUE) %>% 
#   setNames(c("x", 
#              "y", 
#              "ET0_tc"))
# head(ET0)
# write.csv(ET0,"../Data/ET0.csv")
ET0 <- read_csv("../Data/ET0.csv")

ET0 <- ET0 %>%
  mutate(et0_band = cut(ET0_tc, breaks = 6))%>%
  group_by(et0_band)
#see bands
ggplot() +
  geom_bar(data = ET0, aes(et0_band))


ET0 <- ET0 %>%
  mutate(et0_band = cut(ET0_tc, breaks = c(0,850,1700,2500,3300,4200),include.lowest = TRUE,include.highest = TRUE))%>%
  group_by(et0_band)
#sum(is.na(ET0$et0_band)) verify no NAs values
#see bands
# ggplot() +
#   geom_bar(data = ET0, aes(et0_band))

#plot to paper

windows()
ggplot() +
  theme(panel.grid.major = element_line(color = "grey"))+ 
#  geom_sf(data = world, fill=NA)
  xlab("Longitude") + ylab("Latitude")+
  coord_sf(expand = FALSE) +
  scale_y_continuous(limits = c(-85, 85))+
  theme(panel.background = element_rect(fill = 'aliceblue')) +
  geom_raster(data = ET0,aes(x = x,y = y,fill = et0_band))+
  scale_fill_viridis(discrete = TRUE, name = "ET0", labels = c("[0,850]", "(850,1700]","(1700,2500]","(2500,3300]","(3300,4135]")) +
  geom_point(data= all_catchments,aes(x=Longitude, y=Latitude, colour='sites'),
             alpha=0.8, size = 3)+
  scale_colour_manual(values = c("sites" = 'orange'), name = "Sites") +
  ggspatial::annotation_north_arrow(location = "bl",height = unit(0.8, "cm"),
                                    width = unit(0.8, "cm")) +
  theme(axis.text = element_text(size =  rel(1.5)),
        axis.title = element_text(size =  rel(1.5)),
        legend.text = element_text(size =  rel(1.5)),
        legend.title = element_text(size =  rel(1.5))) +
  guides(color = guide_legend(override.aes = list(size=5)))
  
#+ annotation_scale(data = world ,plot_unit ="km",width_hint = 0.1)


