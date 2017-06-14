<<<<<<< HEAD
library(ggplot2) 
library(rgdal)#for fortify()
library(rgeos) #for fortify()
library(maptools) 
library(mapproj)
library(data.table)
tw_new <- 
  readShapeSpatial("~/Downloads/mapdata201706010300/COUNTY_MOI_1060601/COUNTY_MOI_1060525.shp") 
str(tw_new)
tw_new$COUNTYID
tw_new$COUNTYCODE
tw_new.df <- fortify(tw_new, region = "COUNTYID") 
tw_new$COUNTYNAME



mydata<-data.frame(NAME_2=tw_new$COUNTYNAME, 
                   id=tw_new$COUNTYID,
                   pre=rnorm(length(tw_new$COUNTYID)),
                   stringsAsFactors = F)
mydata$NAME_2<-iconv(mydata$NAME_2,from="big5", to = "UTF-8")
mydata <- as.data.table(mydata)

北部<-c("臺北市","新北市","基隆市","桃園市","新竹縣","新竹市","苗栗縣")
中部<-c("臺中市","南投縣","彰化縣","雲林縣","嘉義市","嘉義縣")
南部<-c("臺南市","高雄市","屏東縣")
東部<-c("宜蘭縣","花蓮縣","臺東縣")
離島<-c("連江縣","金門縣","澎湖縣")
mydata$NAME_2
mydata$分區 <- ""
mydata[NAME_2 %in% 北部]$分區 <-"北部"
mydata[NAME_2 %in% 中部]$分區 <-"中部"
mydata[NAME_2 %in% 東部]$分區 <-"東部"
mydata[NAME_2 %in% 南部]$分區 <-"南部"
mydata[NAME_2 %in% 離島]$分區 <-"離島"


head(mydata,10)
final.plot<-merge(tw_new.df,
                  mydata,by="id",all.x=T)
library(RColorBrewer) #配色用brewer.pal( 9 , "Reds" )
twcmap<-ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, 
                   fill = 分區,group = group
                   ), 
               color = "white", 
               size = 0.25) + 
  coord_map()+#維持地圖比例
  scale_fill_gradientn(
    colours = brewer.pal(9,"Reds"))+
  theme_void()+
  labs(title="Prevalence of X in Taiwan")
twcmap



=======

library(ggplot2)
library(rgdal)
<<<<<<< HEAD
library(rgeos)
library(maptools)
Sys.setlocale(category = "LC_ALL", locale = "cht")
area <- readShapeSpatial("~/R/CGUIM_BigData_HW6-heymei/R Final/CCTV/CCTV.shp")
area.df <- fortify(area , region = "are")
>>>>>>> e9ad2d790e7f44e9588f48ed99e68a1802535ee1

library(jsonlite)
library(RCurl)
WaterData<-fromJSON(getURL("http://data.taipei/opendata/datalist/apiAccess?scope=resourceAquire&rid=190796c8-7c56-42e0-8068-39242b8ec927"))
WaterDataFrame<-WaterData$result$results
WaterDataFrame$longitude<-as.numeric(WaterDataFrame$longitude)
WaterDataFrame$latitude<-as.numeric(WaterDataFrame$latitude)
WaterDataFrame$qua_cntu<-as.numeric(WaterDataFrame$qua_cntu)
WaterDataClean<-WaterDataFrame[WaterDataFrame$qua_cntu>=0,]
head(WaterDataClean)
<<<<<<< HEAD
library(ggmap)
TaipeiMap <- get_map(
  location = c(121.43,24.93,121.62,25.19), 
  zoom = 11, maptype = 'roadmap')
TaipeiMapO <- ggmap(TaipeiMap)+ 
  geom_point(data=WaterDataClean, 
             aes(x=longitude, y=latitude,
                 color=qua_cntu,size=3.5))+ 
  scale_color_continuous(
    low = "yellow",high = "red")+ 
  guides(size=FALSE)
TaipeiMapO





=======

mydata <- data.frame(e = area$E , n = area$N)

=======
area <- readShapePoly("~/Documents/GitHub/CGUIM_BigData_HW6-heymei/R Final/CCTV/CCTV.shp")
>>>>>>> origin/master
>>>>>>> e9ad2d790e7f44e9588f48ed99e68a1802535ee1

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

