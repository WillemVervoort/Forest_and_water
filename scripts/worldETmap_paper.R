######paper map

library(readxl)
library(readr)
library(plotly)
library(ggmap)
library(sf)
library(raster)
library(rgdal)

#read files
LargeData_Catchments <- read_csv("C:/Users/nanin/Downloads/LargeData_Catchments.csv")
LargeC<-LargeData_Catchments[,1:4]
colnames(LargeC)<- c('Number',"Name",'latitude','longitude')

SmallData_Catchments <- read_csv("C:/Users/nanin/Downloads/SmallData_Catchments.csv")
SmallC<-SmallData_Catchments[,1:4]
colnames(SmallC)<- c('Number',"Name",'latitude','longitude')

NewData_Catchments <- read_csv("C:/Users/nanin/Downloads/NewData_Catchments.csv")
NewC<-NewData_Catchments[,1:4]
colnames(NewC)<- c('Number',"Name",'latitude','longitude')

#combine datasets
all_catchments<-bind_rows(LargeC,SmallC,NewC)

#plot
ETmap <- raster('./extraET0.tif')
ETmapdf <- as.data.frame(ETmap)
extaraET0 <- raster("./extraET0.tif")
# regular sample of 500k cells

ET0 <- sampleRegular(extaraET0, 
                     size = 5e5, 
                     asRaster = TRUE) %>% 
  as.data.frame(xy = TRUE, 
                na.rm = TRUE) %>% 
  setNames(c("x", 
             "y", 
             "ET0_tc"))
head(ET0)

ET0 <- ET0 %>%
  mutate(et0_band = cut(ET0_tc, breaks = 6))%>%
  group_by(et0_band)
#see bands
ggplot() +
  geom_bar(data = ET0, aes(et0_band))

#plot to paper
ggplot() +
  theme(panel.grid.major = element_line(color = "grey"))+
  xlab("Longitude") + ylab("Latitude")+
  coord_sf(expand = FALSE) +
  scale_y_continuous(limits = c(-79, 79))+
  theme(panel.background = element_rect(fill = 'aliceblue')) +
  geom_raster(data = ET0,aes(x = x,y = y,fill = et0_band))+
  scale_fill_viridis_c()+
  geom_point(data= all_catchments,aes(x=longitude, y=latitude, color="cacthment"),alpha=0.5)

#Error: Discrete value supplied to continuous scale
#something in scale_fill_viridis_c()
#source: https://uw-madison-datascience.github.io/r-raster-vector-geospatial/02-raster-plot/
#source2: https://r-spatial.org/r/2018/10/25/ggplot2-sf.html