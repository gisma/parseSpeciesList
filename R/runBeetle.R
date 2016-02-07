#'@name runBeetle
#'@title Wrapper function to analyse the running beetle spread according to the hypothesis of bindings to wetness
#'
#'@description
#' Tibetan carabidae are spreading correlated to available humidity. Long term humidity is bound to surface  and ground water. It seems to be obvious that the estimation of spreeading speed and range is in no case a simple euclidian one. runBeetle provides a first better estimation using a cost or friction analysis assuming that the spread is following natural lines of wetness e.g. valleys, humidity gradients...
#'
#'@usage runBeetle(cost = costraster,type=alt,occurence=carabLocs)
#'@author Chris Reudenbach
#'
#'@references Rasemann, S., (2004), Geomorphometrische Struktur eines mesoskaligen alpinen Geosystems, Asgard-Verlag, St. Augustin, Bonner Geographische Abhandlungen, Heft Nr. 111, URL: \url{http://hss.ulb.uni-bonn.de/2003/0211/0211.htm}
#'@references Wood, J.D., (1996), The geomorphological characterisation of digital elevation models. PhD Thesis, University of Leicester, UK
#'@references Schmidt, J, & A. Hewitt, (2004). Fuzzy land element classification from DTMs based on geometry and terrain position. Geoderma 121.3, S.243-256. URL: \url{http://home.shirazu.ac.ir/~kompani/geomorphology/geomorphology-lec-papers-mehr88/schmidt-fuzzylandsurfaceclassifi-geoderma2004.pdf}
#'@references Breitkreutz, H.: Gipfelliste. URL: \url{http://www.tourenwelt.info/commons/download/bergliste-komplett.kmz.php}
#'@references OSM natural: keys \url{http://wiki.openstreetmap.org/wiki/Key:natural}


#'
#'
#'@param iniparam the full set of params as provided initEnvironGIS(). please look into the INI file for further settings of the makePEak() mode and special settings.
#'peak.mode
#'
#'(1) minmax: extracts local maxima altitudes from an arbitrary Digital
#'                     Elevation Model (DEM) (optionally you may filter the data)
#'
#'(2) merged approach: extract peaks from a DEM by analyzing morphometry and
#'                     landscape forms using the algorithm of Wood et others. The peak landforms are
#'                     analyzed for MAximunm heights and this location is related
#'                     to external peak data from Harrys peaklist or OSM
#'@param myenv SAGA environment variables provided by initEnvironGIS()
#'@param fname.DEM name of georeferenced DEM data in GDAL format




#'@return runBeetle basically returns a list of coordinates altitudes (and names)
#'that will be used to calculate the independence value.
#'
#'
#'
#'@export runBeetle
#'@examples
#'#### Example to use runBeetle for a common analysis of the
#'     estimated spreading distances of an specifified area
#'
#' # You need a georeferenced DEM (GDAL format) as data input.
#' # NOTE the existing projection of the data file has to be exactly the same
#' # as provided in target.proj4  variable in the ini file
#'
#'
# Rpeak(beetleLocs)

#'

runBeetle <-function(beetleLocs=input.List,getdata=TRUE,path=tempdir(),lon = 80, lat = 30){
  # check for libs install and load them
  libraries<-c("doParallel","foreach","curl","sp","rgdal","raster","mapview")
  # Install CRAN packages (if not already installed)
  inst <- libraries %in% installed.packages()
  if(length(libraries[!inst]) > 0) install.packages(libraries[!inst])
  # Load packages into session
  lapply(libraries, require, character.only=TRUE)
  # register number of cores for parallel operations
  registerDoParallel(cores=detectCores())

  # create runtime directories
  if (!file.exists(path)){dir.create(file.path(path),recursive = TRUE)
  }

  # radolan projection string
  costProj4 <-
    CRS("+proj=stere +lat_0=90 +lat_ts=90 +lon_0=10 +k=0.93301270189 +x_0=0 +y_0=0 +a=6370040 +b=6370040 +to_meter=1000 +no_defs")
  # radolan extent
  costExt <- extent(-523.4622, 376.5378, -4658.645, -3758.645)

  # set working directory
  setwd(path)

  ## put it in sp object (doesn't matter what type)
 # landuseCH<- readOGR(".","landuse")
  ### END setup
#  extent<-extent(beetleLocs)
  costRaster<-getGeoData('SRTM', lon=lon, lat=lat)



  # analysis of peaks and preprocessing of all data
      finalBeetleList[i,5]<-costDistance(locs=beetleLocs, costraster, domthres,plots=TRUE)


  ### make it a spatialObject
  # set the xy coordinates
  coordinates(finalBeetleList) <- ~xcoord+ycoord
  # set the projection
  proj4string(finalBeetleList) <- target.proj4

  ### to have somthing as a result
  # write it to a shape file
  writePointsShape(finalBeetleList,"beetleDistance.shp")

  # visualize it for your convenience
  mapview(finalBeetleList)


  print("That's it")
  return(finalBeetleList)
 }
