# # \dontrun{
# library(biomix)
# # library(sp)
# library(raster)
# setwd("~/proj/runbeetle")
# path<-getwd()
# b<-read.csv2('b.csv',header = TRUE,sep = ',',dec = '.')
#  coordinates(b)<- ~long+lat
#  proj4string(b)<- CRS("+init=epsg:4326")
# xt<-extent(b)
# xx<-Lon<- floor(xt@xmin-3)
# yy<- Lat<-floor(xt@ymin-3)
# lonFac<-floor(((xt@xmax+3)-Lon)/5)
# latFac<-floor(((xt@ymax+3)-Lat)/5)
#
# ytiles<-seq(1,latFac)
# xtiles<-seq(1,lonFac)
# for (i in ytiles){
#   for (j in xtiles){
#     #assign(r,paste0("srtm","_",yy,"_",xx))
#     getGeoData("SRTM",lat=floor(Lat), lon=floor(Lon),path=path)
#     cat(zipfilename)
#     Lon<-Lon+5
#   }
#   Lon<-floor(xt@xmin-3)
#   Lat<-Lat+5
# }
# # }
#
#
# # getSRTMfn <- function(xtent){
# #   srtmNames<-NULL
# #   xt<-extent(xtent)
# #   lon<- floor(xt@xmin-3)
# #   lat<-floor(xt@ymin-3)
# #   lonFac<-floor(((xt@xmax+3)-lon)/5)
# #   latFac<-floor(((xt@ymax+3)-lat)/5)
# #
# #   ytiles<-seq(1,latFac)
# #   xtiles<-seq(1,lonFac)
# #   for (i in ytiles){
# #     for (j in xtiles){
# #       stopifnot(lon >= -180 & lon <= 180)
# #       stopifnot(lat >= -60 & lat <= 60)
# #
# #       rs <- raster(nrows=24, ncols=72, xmn=-180, xmx=180, ymn=-60, ymx=60 )
# #       rowTile <- rowFromY(rs, lat)
# #       colTile <- colFromX(rs, lon)
# #       if (rowTile < 10) { rowTile <- paste('0', rowTile, sep='') }
# #       if (colTile < 10) { colTile <- paste('0', colTile, sep='') }
# #
# #       f <- paste('srtm_', colTile, '_', rowTile, sep="")
# #       srtmNames <- c(srtmNames, paste(f, ".ZIP", sep=""))
# #       #tiffilename <- paste(path, "/", f, ".TIF", sep="")
# #       lat<-lat+5
# #     }
# #     lon<-floor(xt@xmin-3)
# #     lon<-lon+5
# #   }
# #   return(srtmNames)
# #   }

