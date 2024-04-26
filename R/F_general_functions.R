################################################################################
###
### DATE CREATED: 2017-02-28
###
### AUTHOR: Mark Wheldon
###
### DESCRIPTION:
###
### General functions used in lots of other places.
###
###-----------------------------------------------------------------------------
###
### SYNOPSIS:
###
################################################################################


##' Version of \code{\link{mapply}} that checks names of objects are the same and in same order
##'
##' If names are the same and in the same order, \code{\link{mapply}} is called.
##'
##' @param FUN See \code{\link{mapply}}.
##' @param ... See \code{\link{mapply}}.
##' @param MoreArgs See \code{\link{mapply}}.
##' @param SIMPLIFY See \code{\link{mapply}}.
##' @param USE.NAMES See \code{\link{mapply}}.
##' @return See \code{\link{mapply}}.
##' @author Mark Wheldon.
##' @noRd
mapplySafe <- function(FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE,
                       USE.NAMES = TRUE) {
    dots <- list(...)

    ## Check lengths
    ll <- lapply(dots, "length")
    if(!identical(length(unique(unlist(ll))), 1L)) stop("Objects are not all of equal length.")

    ## Check names
    ln <- lapply(dots, "names")
    ln.eq <- list()
    for(i in 2:length(ln)) {
        ln.eq <- c(ln.eq, list(ln[[i]] == ln[[1]]))
    }
    if(!isTRUE(all(unlist(ln.eq)))) stop("Objects do not all have the same names.")

    mapply(FUN = FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES)
}


##' Makes a character string into a valid filename (for Windows, at least).
##'
##' Replaces forbidden characters with underscore (default).
##'
##' @param x Character sting to convert.
##' @param safe Character used to replace disallowed.
##' @param disallowed Regular expression of disallowed characters. Passed to
##'     \code{gsub(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE,
##'     useBytes = FALSE)}.
##' @return 'x' converted to a valid filename.
##' @author Mark Wheldon.
##' @noRd
makeFileName <- function(x, safe = "_", disallowed = "\\./|\\*|<|\\\\|>|:|\\||\\?|\"|/") {
    gsub(disallowed, safe, x = x)
}


##' Shorten indicator names for use in file names
##'
##' Need to keep names from getting too long!
##' @param fnm file name
##' @return shortened file name
##' @author Mark Wheldon
##' @noRd
makeShortIndicatorFileName <- function(fnm) {
      fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                 ,fnm, fixed = TRUE)
      fnm <- gsub("Modern Married Over All", "Mod-MarriedOverAll"
                 ,fnm, fixed = TRUE)
      fnm <- gsub("Trad Married Over All", "Trad-MarriedOverAll"
                 ,fnm, fixed = TRUE)
      fnm <- gsub("Unmet Married Over All", "Unmet-MarriedOverAll"
                 ,fnm, fixed = TRUE)
      fnm <- gsub("Modern Unmarried Over All", "Mod-UnmarriedOverAll"
                 ,fnm, fixed = TRUE)
      fnm <- gsub("Trad Unmarried Over All", "Trad-UnmarriedOverAll"
                 ,fnm, fixed = TRUE)
      fnm <- gsub("Unmet Unmarried Over All", "Unmet-UnmarriedOverAll"
                 ,fnm, fixed = TRUE)
    return(fnm)
}


##' Make subdirectory name for special aggregates
##'
##' @param file.aggregates
##' @return
##' @author Mark Wheldon
##' @noRd
makeSpecAggSubdir <- function(file.aggregates) {
    if(is.null(file.aggregates)) {
        file.path("aggregatetrajectories", "UNPDaggregates")
    } else {
        file.path("aggregatetrajectories",
                  makeFileName(abbreviate(sub(pattern = "(.*)\\..*$", replacement = "\\1",
                                              basename(file.aggregates)),
                                          minlength = 12)
                               ))
        }
}


##' Turn filename for (un)married into one for all women.
##'
##' @param orig.name Character string; name of filename to convert.
##' @return
##' @author Mark Wheldon
##' @noRd
makeAWFileName <- function(old.name) {
    if(length(old.name) > 1) {
        aw.name <- vector(mode = "character", length = length(old.name))
        for(i in seq_along(old.name)) {
            aw.name[i] <- makeAWFileName(old.name[i])
        }
    } else {
        if(grepl("^umw", old.name)) aw.name <- gsub("^umw", "aw", old.name)
        else if(grepl("^uw", old.name)) aw.name <- gsub("^uw", "aw", old.name)
        else if(grepl("^mw", old.name)) aw.name <- gsub("^mw", "aw", old.name)
        else if(!grepl("^aw", old.name)) aw.name <- paste0("aw_", old.name)
        else aw.name <- old.name
    }
    return(aw.name)
}


##' Turn filename for (un)married into one for age ratios.
##'
##' @param orig.name Character string; name of filename to convert.
##' @return
##' @author Mark Wheldon
##' @noRd
makeAgeRatioFileName <- function(old.name) {
    if(length(old.name) > 1) {
        ar.name <- vector(mode = "character", length = length(old.name))
        for(i in seq_along(old.name)) {
            ar.name[i] <- makeAgeRatioFileName(old.name[i])
        }
    } else {
        ar.name <- paste0(old.name, "_age_ratio")
    }
    return(ar.name)
}


##' Make tidy country names
##'
##' Tidy up any errors in names of countries caused by non-ASCII
##' characters or by typos.
##'
##' @param x Character vector containing name to be tidied up.
##' @return Character vector with tidied up name.
##' @author Mark Wheldon
##' @noRd
makeCountryNames <- function(x) {
    x[grep("^Bolivia \\(Plurinational State of\\)", x)] <- "Bolivia, Plurinational State of"
    x[grep("^C.+e d'Ivoire$", x)] <- "C\u00F4te d'Ivoire"
    x[grep("^China, Hong Kong \\(SAR\\)", x)] <- "China, Hong Kong SAR"
    x[grep("^China, Hong Kong Special Administrative Region", x)] <- "China, Hong Kong SAR"
    x[grep("^China, Macao \\(SAR\\)", x)] <- "China, Macao SAR"
    x[grep("China, Macao Special Administrative Region", x)] <- "China, Macao SAR"
    x[grep("^Cura.+ao$ ", x)] <- "Cura\u00E7ao"
    x[grep("^Democratic People's Republic of Korea", x)] <- "Democratic People's Rep. of Korea"
    x[grep("^Democratic Republic of the Congo", x)] <- "Democratic Rep. of the Congo"
    x[grep("^Democratic Republic of Timor-Leste", x)] <- "Democratic Rep. of Timor-Leste"
    x[grep("^Timor-Leste", x)] <- "Democratic Republic of Timor-Leste"
    x[grep("^Iran \\(Islamic Republic of\\)", x)] <- "Iran, Islamic Republic of"
    x[grep("^Lao People's Democratic Republic", x)] <- "Lao People's Dem. Republic"
    x[grep("^R.+nion$", x)] <- "R\u00E9union"
    x[grep("^Saint Lucia", x)] <- "St. Lucia"
    x[grep("^Saint Vincent and the Grenadines", x)] <- "St. Vincent and the Grenadines"
    ## x[grep("^The former Yugoslav Republic of Macedonia", x)] <- "TFYR Macedonia"
    ## x[grep("^United Kingdom of Great Britain and Northern Ireland", x)] <- "United Kingdom"
    x[grep("^United Republic of Tanzania", x)] <- "United Rep. of Tanzania"
    x[grep("^Venezuela \\(Bolivarian Republic of\\)", x)] <- "Venezuela, Bolivarian Republic of"
    return(x)
}


##' Make tidy region names
##'
##' Tidy up naming of regions
##'
##' @param x Character vector with region names to be tidied.
##' @return Character vector with tidy region names.
##' @author Mark Wheldon
##' @noRd
makeRegNames <- function(x) {
    x[grep("^Federated States of Micronesia", x)] <- "Micronesia, Fed. States of"
    return(x)
}


##' Shorten region names for display
##'
##' Shorten long region names for use in plots and tables.
##'
##' @param x Character vector with region names to be shortened.
##' @return Character vector with short region names.
##' @author Mark Wheldon
##' @noRd
shortenRegNames_manus <- function(x) {
    x[grep("^Latin America and the Caribbean"    , x)] <- "LAC"
    x[grep("^Northern America"                   , x)] <- "N. America"
    return(x)
}


##' Check if a vector contains finite numeric values
##'
##' Returns \code{TRUE} if \code{x} is non-\code{NULL} and \code{is.numeric(x) && !is.na(x) && !is.nan(x) && !is.infinite(x)}.
##'
##' @param x
##' @return A logical vector the same length of \code{x}.
##' @author Mark Wheldon
##' @noRd
is_finite_numeric <- function(x) {
    if(!is.null(x)) is.numeric(x) && !is.na(x) && !is.nan(x) && !is.infinite(x)
    else FALSE
}


###-----------------------------------------------------------------------------
### USED TO BE IN F_readdata.R

###----------------------------
logit <- function(# Logit transform
  ### Logit transform
  x##<< between 0 and 1
  ){
  log(x/(1-x))
}

###----------------------------
invlogit <- function(# Inverse-Logit transform
  ### Inverse-Logit transform
  y){
 x <- 1/(1+exp(-y))
 return(x)
}

###----------------------------
LogitMinMax <- function(# Logit-minmax transform
  ### y = log((x-xmin)/(xmax-x))
  x, ##<< between xmin and xmax
  xmin, xmax){
   y <- log((x-xmin)/(xmax-x))
   return(y)
}

###----------------------------------------------------------------------------------
InvLogitMinMax <- function(#  Inverse Logit-minmax transform
  ### x = (ymax*exp(y) + ymin)/(1+exp(y)), ends up between ymin and ymax
  y, ymin=0, ymax = 1){
  x <- (ymax*exp(y) + ymin)/(1+exp(y))
  return(x)
}

###----------------------------
logitSafer <- function(# Logit transform
  ### Logit transform, budge values of x == 0 or x == 1
  x##<< between 0 and 1
  ){
    x[which(x >= 0 & x > 1 - 1e-6)] <-
        1 - 1e-6
    x[which(x <= 1 & x < 1e-6)] <-
        1e-6
    log(x/(1-x))
}
