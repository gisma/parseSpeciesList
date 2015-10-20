#' parseSpeciesList is a prototype parser for running beetle species lists
#'
#' @description parseSpeciesList is a first prototype to parse running beetle species lists as provided by the beetle enthusiasts.
#'
#' This is a very raw and simple approach not optimized for R
#'
#' @param inputTXT a Text of the specified format
#' @param short logical default = TRUE trys to get only the names. if FALSE it put all informations in the data frame
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
#'  ###  mapping useCase  ####
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
#' @name gettupel
#' @rdname gettupel
#'




# main function parse the text and provides a first raw dataframe
gettupel <- function (inputFile,short = TRUE) {

  dfcount = 0
  # create dataframe for the results of parsing
  family <- ''
  genus <- ''
  subgenus <- ''
  species <- ''
  loctype <- ''
  loc <- ''
  df = data.frame(family,genus,subgenus,species,loctype,loc,stringsAsFactors = FALSE)

  # list for the return of the location parsing
  loc = list()

  # casually they seem not to exist
  gen = 'NA'
  subgen = 'NA'
  # open connection to the file and read it sequentially line by line
   con  <- file(inputFile, open = "r")
   while (length(oneLine <-
                readLines(con, n = 1, warn = TRUE)) > 0) {
     # if 'family' is found split it and subslpit it (same with the rest keywords)
    if (charmatch("family",oneLine ,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "family"))
      if (short) {tmp<- strsplit(tmp, ",")
                  fam <- trimws(tmp[[2]][1][1])}
      else       {fam <- trimws(tmp[[2]])}
    } else if (charmatch("genus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "genus"))
      if (short) {tmp<- strsplit(tmp, ",")
                  gen <- trimws(tmp[[2]][1][1])}
      else       {gen <- trimws(tmp[[2]])}
    } else if (charmatch("subgenus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "subgenus"))
      if (short) {tmp<- strsplit(tmp, ",")
                  subgen <- trimws(tmp[[2]][1][1])}
      else       {subgen <- trimws(tmp[[2]])}
    } else {
      # all lines without keywords has to contain species
      species <- oneLine[1]
      # so we call a special parser for them
      loc <- parseLocations(species)
      # then we reorganise the returned lists
      if (length(loc) > 0) {
        for (i in seq(1,length(loc))) {
          sloc <- unlist(strsplit(loc[[i]], " "))
          if (length(sloc) > 0) {
            for (j in seq(2,length(sloc))) {
              # actually this is the depreceated "tagged" version
              #txt<- paste("<family>",fam,"</family> <genus>",gen,"</genus> <subgenus>",subgen,"</subgenus> <species>",species,"</species> <loctype>",sloc[1],"</loctype> <locations>",sloc[j], "</locations>")
              #tupel[[length(tupel)+1]] <- txt
              # we do what nobody would do in R we append it row by row to the data frame
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
  # initialize the vars
  x = list()
  aloc = ''
  eloc = ''
  nloc = ''
  # get position of the differnt location tags in the string
  ePos <-  str_locate_all(pattern = ' E: ',intext)[[1]][2] - 4
  aPos <-  str_locate_all(pattern = ' A: ',intext)[[1]][2] - 4
  nPos <-  str_locate_all(pattern = ' N: ',intext)[[1]][2] - 4
  # ugly workaround because of totally chotic organization of the location codes
  # and tags we have to derive a fixed order that we will be able to point on
  # the correct labels after extracting them. i did it using a dataframe.
  # finally we have a ordered list and we will submit the tags with the return
  names <- c("a", "e", "n")
  values <- c(aPos,ePos,nPos)
  df <- data.frame(names,values)
  df <- df[order(values),]
  df <- df[complete.cases(df),]
  # if there is a location tag
  if (nrow(df) > 0) {
    for (i in seq(1,nrow(df))) {
      z <- df$values[i + 1]

      if (i == nrow(df)) {
        z <- nchar(intext)
      }
      var <- trimws(substring(intext,df$values[i] + 2 ,z + 1))
      x[i] <- var #paste0(df$names[i],'loc')
      assign(paste0(df$names[i],'loc'), var)
    }
  }
  c(x, c = aloc)
  c(x, c = eloc)
  c(x, c = nloc)

  return(x)
}

