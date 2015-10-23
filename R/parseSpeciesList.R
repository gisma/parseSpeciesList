#' parseSpeciesList is a prototype function to parse Carabidae species lists
#'
#' @description parseSpeciesList is a first prototype to parse running beetle
#'   species lists as provided by the enthusiasts of the running beetle community.
#'
#'   This is a very raw and simple approach not really written in R style.
#'   Actually the parser looks line by line for keywords and special tags.
#'   Unfortunately it has to be performed line by line due to the fact that
#'   keywords for sepcies are missing and the rules are not always matching.
#'   So in a first try we use the "family", "genus" and "subgenus" as keywords. They are always placed in the beginning of a
#'   line. After "genus" or "subgenus" there is a unique line for each single species.
#'   In the species textline we will find a more or less systematic list of country
#'   codes that indicate all countries with known occurrence of this special species.
#'
#'   The resulting dataframe is a not normalized relation ( so it means a huge table with mostly redundant informations).
#'
#'   It looks like:
#'
#'   familiy;   genus;   subgenus;  species;     loctype; country\cr
#'   Carabidae; Carabus; Carabinae; irregularis; A:;      GE\cr
#'   Carabidae; Carabus; Carabinae; irregularis; N:;      CZ\cr
#'   .
#'   .
#'   .
#'
#' @param inputTXT a Text of the specified format
#' @param short logical parameter if TRUE (default) the function trys to get only the names and country codes. If FALSE the full text
#'   will put in the data frame.
#'
#' @author Chris Reudenbach, Flo Detsch

#' @examples
#'  ### examples parseSpeciesList ###
#'
#'  ### we need the stringr lib
#'  library(stringr)
#'  library(foreach)
#'
#'  ### first the basic parsing
#'  inputFile <- system.file("extdata", "species.chunk",   package="parseSpeciesList")
#'  df <- getspecies(inputFile)
#'
#'
#'  ######################################
#'  ###  now a basic mapping example  ####
#'
#'  ###  we need some more libs ;)
#'
#'    if (!require(downloader)) {install.packages("downloader")}
#'    if (!require(maptools)) {install.packages("maptools")}
#'    if (!require(sp)) {install.packages("sp")}
#'    if (!require(devtools)) {install.packages("devtools")}
#'    library(downloader)
#'    library(maptools)
#'    library(sp)
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
#' @name getspecies
#' @rdname getspecies
#'




# main function parse the text and provides a first raw dataframe
getspecies <- function (inputFile, short = TRUE) {

  lns <- readLines(inputFile)
  df <- data.frame(matrix(ncol = 6))
  names(df) <- c("family", "genus", "subgenus", "species", "loctype", "loc")

  # casually they seem not to exist
  fam <- gen <- subgen <- species <- loc <- sloc <- NA

  lst_all <- foreach(i = 1:length(lns)) %do% {

    oneLine <- lns[i]

    ## family
    if (charmatch("family",oneLine ,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "family"))
      if (short) {tmp<- strsplit(tmp, ",")
      fam <- trimws(tmp[[2]][1][1])}
      else       {fam <- trimws(tmp[[2]])}

      ## genus
    } else if (charmatch("genus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "genus"))
      if (short) {tmp<- strsplit(tmp, ",")
      gen <- trimws(tmp[[2]][1][1])}
      else       {gen <- trimws(tmp[[2]])}

      ## subgenus
    } else if (charmatch("subgenus",oneLine,nomatch = 0) > 0) {
      tmp <- unlist(strsplit(oneLine, "subgenus"))
      if (short) {tmp<- strsplit(tmp, ",")
      subgen <- trimws(tmp[[2]][1][1])}
      else       {subgen <- trimws(tmp[[2]])}

      ## everything else
    } else {
      # all lines without keywords has to contain species
      species <- oneLine
      # so we call a special parser for them
      loc <- parseCountryCode(species)

      if (length(loc) > 0) {
        for (z in 1:length(loc))
          species <- gsub(loc[z], "", species)
      }

      # then we reorganise the returned lists
      if (length(loc) > 0) {
        lst_loc <- lapply(seq(1,length(loc)), function(h) {
          sloc <- unlist(strsplit(loc[[h]], " "))
          if (length(sloc) > 0) {
            lst_sloc <- lapply(seq(2,length(sloc)), function(j) {
              data.frame(loctype = sloc[1], loc = sloc[j])
            })
            dat_sloc <- do.call("rbind", lst_sloc)
          }
        })
        dat_loc <- do.call("rbind", lst_loc)
      } else {
        dat_loc <- data.frame(loctype = NA, loc = NA)
      }
    }

    if (!exists("dat_loc"))
      dat_loc <- data.frame(loctype = NA, loc = NA)

    dat <- data.frame(family = fam,
                      genus = gen,
                      subgenus = subgen,
                      species = species,
                      dat_loc)

    rm(dat_loc)
    return(dat)
  }

  dat_all <- do.call("rbind", lst_all)
  return(dat_all)

}