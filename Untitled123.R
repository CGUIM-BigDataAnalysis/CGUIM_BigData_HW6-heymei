install.packages('maptools')
install.packages('sf')
library(sf)
library(maptools)
library(rgdal)
area <- rgdal::readOGR("~/Documents/GitHub/CGUIM_BigData_HW6-heymei/R Final/CCTV/CCTV.shp")


library(corrplot)
library(data.table)
abcde <- fread("~/Desktop/安哥測試 (3).csv")
M <- cor(abcde,method="kendall") 
cor_properties <- corrplot(M, method = 'number',order ="hclust", tl.col="black", tl.cex = 1, tl.offset = 1, tl.srt = 40)
