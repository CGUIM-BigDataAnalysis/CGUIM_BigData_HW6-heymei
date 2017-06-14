
library(ggplot2)
library(rgdal)
library(rgeos)
library(maptools)
Sys.setlocale(category = "LC_ALL", locale = "cht")
area <- readShapeSpatial("~/R/CGUIM_BigData_HW6-heymei/R Final/CCTV/CCTV.shp")
area.df <- fortify(area , region = "are")

library(jsonlite)
library(RCurl)
WaterData<-fromJSON(getURL("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=190796c8-7c56-42e0-8068-39242b8ec927"))
WaterDataFrame<-WaterData$result$results
WaterDataFrame$longitude<-as.numeric(WaterDataFrame$longitude)
WaterDataFrame$latitude<-as.numeric(WaterDataFrame$latitude)
WaterDataFrame$qua_cntu<-as.numeric(WaterDataFrame$qua_cntu)
WaterDataClean<-WaterDataFrame[WaterDataFrame$qua_cntu>=0,]
head(WaterDataClean)

mydata <- data.frame(e = area$E , n = area$N)


library(ggmap)
TaipeiMap <- get_map(
  location = c(121.43,24.93,121.62,25.19), 
  zoom = 14, maptype = 'roadmap')
TaipeiMapO <- ggmap(TaipeiMap)+ 
  geom_point(data=mydata, 
             aes(x=mydata$e, y=mydata$n,size=0.0001))+ 
  scale_color_continuous(
    low = "yellow",high = "red")+ 
  guides(size=FALSE)
TaipeiMapO

