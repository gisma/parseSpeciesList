#' parseSpeciesList is a prototype parsers for running beetle species lists
#'
#' @description parseSpeciesList is a first prototype to parse running beetle species lists as provided by the beetle enthusiasts.
#'
#' This is a very raw and simple approach not optimized for R
#'
#' @param inputTXT a Text of the specified format
#'
#' @author
#' Chris Reudenbach

#' @examples
#' ### examples parseSpeciesList ###
#'  # we need stringr
#'  library(stringr)
#'
#'  # basic parsing
#'  inputFile <- system.file("extdata", "species.chunk",   package="parseSpeciesList")
#'  df <- gettupel(inputFile)
#'
#'  ### use case mapping
#'
#'  # we need some more libs
#'    if (!require(downloader)) {install.packages("downloader")}
#'    if (!require(maptools)) {install.packages("maptools")}
#'    if (!require(sp)) {install.packages("sp")}
#'    if (!require(devtools)) {install.packages("devtools")}
#'    library(maptools)
#'    library(downloader)
#'    library(sp)
#'    library(raster)
#'    library(devtools)
#'    if (!require(mapview)) {install_github("environmentalinformatics-marburg/mapview")}
#'    library(mapview)
#'
#'  # first we need some spatial geometries for mapping the data
#'  download("http://thematicmapping.org/downloads/TM_WORLD_BORDERS-0.3.zip",dest="worldborders.zip", mode = "wb")
#'
#'  # unzip it
#'    unzip ("worldborders.zip",exdir = "./")
#'
#'  # read it to a spatialpointdataframe
#'    sPDF <- readShapePoly('TM_WORLD_BORDERS-0.3.shp')
#'  # join the world countries to our data (iso2 seems best but stil poor)
#'    sPDF2 <- joinData2Map(
#'    df
#'      , nameMap = sPDF
#'      , nameJoinIDMap = "ISO2"
#'      , nameJoinColumnData = "loc")
#'
#'  # we have to project it
#'    proj4string(sPDF2) <- CRS("+init=epsg:4326")
#'
#'  # and finally plot it with mapview
#'    mapView(sPDF2)
#'
#' @export
#' @docType gettupel
#' @name gettupel
#' @rdname gettupel
#'




# main function parse the text and provides a first raw dataframe
gettupel <- function (pathToTxt) {
  tupel = list()
  dfcount = 0
  family <- ''
  genus <- ''
  subgenus <- ''
  species <- ''
  loctype <- ''
  loc <- ''
  df = data.frame(family,genus,subgenus,species,loctype,loc,stringsAsFactors =
                    FALSE)
  loc = list()

  loctype = list("A","E","N")
  gen = 'NA'
  subgen = 'NA'

   con  <- file(pathToTxt = NULL, open = "r")
   while (length(oneLine <-
                readLines(con, n = 1, warn = TRUE)) > 0) {
    if (charmatch("family",oneLine ,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "family"))
      fam <- trimws(tmp[[2]])
    } else if (charmatch("genus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "genus"))
      gen <- trimws(tmp[[2]])
    } else if (charmatch("subgenus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "subgenus"))
      subgen <- trimws(tmp[[2]])
    } else {
      species <- oneLine[1]
      loc <- parseLocations(species)
      if (length(loc) > 0) {
        for (i in seq(1,length(loc))) {
          sloc <- unlist(strsplit(loc[[i]], " "))
          if (length(sloc) > 0) {
            for (j in seq(2,length(sloc))) {
              #txt<- paste("<family>",fam,"</family> <genus>",gen,"</genus> <subgenus>",subgen,"</subgenus> <species>",species,"</species> <loctype>",sloc[1],"</loctype> <locations>",sloc[j], "</locations>")
              #tupel[[length(tupel)+1]] <- txt
              df[dfcount,] <-
                c(fam,gen,subgen,species,sloc[1],sloc[j])
              dfcount = dfcount + 1


            }
          }
        }
      }
    }
  }

  return(df)
  close(con)
}

# subparser for geographic location keys
parseLocations <- function (intext,x) {
  # get position in string
  x = list()
  aloc = ''
  eloc = ''
  nloc = ''
  ePos <-  str_locate_all(pattern = ' E: ',intext)[[1]][2] - 4
  aPos <-  str_locate_all(pattern = ' A: ',intext)[[1]][2] - 4
  nPos <-  str_locate_all(pattern = ' N: ',intext)[[1]][2] - 4
  names <- c("a", "e", "n")
  values <- c(aPos,ePos,nPos)
  df <- data.frame(names,values)
  df <- df[order(values),]
  df <- df[complete.cases(df),]
  if (nrow(df) > 0) {
    for (i in seq(1,nrow(df))) {
      z <- df$values[i + 1]

      if (i == nrow(df)) {
        z <- nchar(intext)
      }

      var <- trimws(substring(intext,df$values[i] + 2 ,z + 1))
      x[i] <- var #paste0(df$names[i],'loc')
      assign(paste0(df$names[i],'loc'), var)
      #paste0(df$names[i],'loc') = x[i]
      #assign(paste(paste0(df$names[i],'loc'),sep=""), x[i])
    }
  }
  c(x, c = aloc)
  c(x, c = eloc)
  c(x, c = nloc)

  return(x)
}

