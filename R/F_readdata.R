###----------------------------------------------------------------------------------
### Leontine Alkema
### F_readdata.R
###
### MODIFIED by Mark Wheldon starting 2016-02-22 for extension to all women.
###----------------------------------------------------------------------------------
### Contains:
### general math functions (MOVED TO F_general_functions.R)
### function to read in data, and make summary tables (plots)
### functions to read/summarize regional info
### function to change country names

###----------------------------------------------------------------------------------
##' Check input data for errors.
##'
##' A wrapper for \code{\link{PreprocessData}} for use as a stand-alone function. Default for \code{return.processed.data.frame = FALSE}.
##'
##' @param write.model.fun
##' @param messages
##' @param warnings
##' @param return.processed.data.frame
##' @return
##' @author
PrecheckData <- function(# Pre-check contraceptive prevalence data
  ### Pre-process contraceptive prevalence data
  data.csv = NULL, ##<< If \code{NULL}, data set included in package is used.
  ## To use alternative data set, use \code{data.csv = .../dataCPmodel.csv}, where ... refers to the file path where file is located.
  iso.select = NULL, ##<< If \code{NULL}, data of all countries/subpopulations in data set are read in, else
  ## only data of countries/subpopulations with selected ISO code is read in. # change JR, 20131104
  write.model.fun = "WriteModel"
  ) {
    packageStartupMessage("\n\n>>---------------------------------------------------------------------------->>\nChecking '", data.csv, "'\n>>---------------------------------------------------------------------------->>\n", sep = "")

    ## ## Set warnings to be saved till end
    ## oo <- options()
    ## on.exit(options(oo))
    ## options(warn = 1)

    PreprocessData(data.csv = data.csv
                  ,iso.select = iso.select
                  ,write.model.fun = write.model.fun
                  ,print.messages = TRUE
                  ,print.warnings = TRUE
                  ,return.processed.data.frame = FALSE
                   )

    ## ## WARNINGS
    ## cat("\n"); warnings()

    packageStartupMessage("\n<<----------------------------------------------------------------------------<<\nEND OF Checking '", data.csv, "'\n                NO PROBLEMS DETECTED BUT CHECK WARNINGS (IF ANY ABOVE)                \n<<----------------------------------------------------------------------------<<\n", sep = "")
}


CheckDataMissing <- function(col_name, data_frame, data_frame_name) {
        if(col_name %in% colnames(data_frame)) {
            miss.vals <- is.na(data_frame[, col_name])
            if(any(miss.vals)) {
                stop("There are missing values in '", data_frame_name, "' column '", col_name, "' at row(s) : ", paste0(which(miss.vals), collapse = ", "))
            }
        }
        return(invisible())
}

CheckDataRange <- function(col_name, data_frame, data_frame_name, range = c(0, 100)) {
        if(col_name %in% colnames(data_frame)) {
            outside.range <- data_frame[, col_name] < range[1] | data_frame[, col_name] > range[2]
            if(any(outside.range, na.rm = TRUE)) {
                stop("Values in '", data_frame_name, "' column '", col_name, "' fall outside the range '[",
                     paste(range, collapse = ", "), "]' at row(s) : ",
                     paste0(which(outside.range), collapse = ", "))
            }
        }
        return(invisible())
    }


###----------------------------------------------------------------------------------
PreprocessData <- function(# Pre-process contraceptive prevalence data
  ### Pre-process contraceptive prevalence data
  data.csv = NULL, ##<< If \code{NULL}, data set included in package is used.
  ## To use alternative data set, use \code{data.csv = .../dataCPmodel.csv}, where ... refers to the file path where file is located.
  iso.select = NULL, ##<< If \code{NULL}, data of all countries/subpopulations in data set are read in, else
  ## only data of countries/subpopulations with selected ISO code is read in. # change JR, 20131104
  write.model.fun = "WriteModel",
  print.messages = TRUE,
  print.warnings = TRUE,
  return.processed.data.frame = TRUE,
  marital.group = NULL
  ) {

    ## Read in input data
    data.raw <-
        read.csv(file = data.csv, header = TRUE, as.is = TRUE
                ,stringsAsFactors = FALSE, strip.white = TRUE
                 )

    cat(names(data.raw), "\n")
    cat("no. rows in dataset: ", nrow(data.raw), "\n")

    ## Filter the correct marital group
    if(is.null(marital.group)) {
        marital.group <- "MWRA"
        warning("'marital.group' is 'NULL', setting 'marital.group' to 'MWRA'.")
    }
    if(!("In.union" %in% colnames(data.raw))) {
        stop("'In.union' is not a column in the input data but is required.")
    } else {
        idx_iu <- !(data.raw$In.union %in% c(0, 1))
        if(sum(idx_iu) > 0) {
            stop("'In.union' must only have values '0', or '1'. The following rows are not compliant:\n",
                 paste(which(idx_iu), collapse = ", "))
        }
    }
    if (!is.null(marital.group) && any(names(data.raw) == "In.union")) {
      if (marital.group == "MWRA") {
        data.raw <- data.raw[data.raw$In.union == 1,]
      } else if (marital.group == "UWRA") {
        data.raw <- data.raw[data.raw$In.union == 0,]
      } else {
        stop("marital group must be NULL, 'MWRA' or 'UWRA'")
      }
    }

    cat("no. rows in dataset matching marital group ", marital.group, ": ", nrow(data.raw), "\n", sep = "")

    ## -------* Column Name and Value Checks

    ## Create column name regexps and cell value regexps
    data.col.names <- InternalRegExpsInputCols()
    data.cell.vals <- InternalRegExpsInputCells()

    ## Check for duplicates in column names
    dup_cols <- sapply(data.col.names$regex, function(z) sum(grepl(z, names(data.raw))) > 1)
    dup <- any(dup_cols)
    if(dup) stop(paste0("Column names in '", data.csv, "' are not unique enough. Check\n\t"
                        ,paste(names(dup_cols)[dup_cols], collapse = "\n\t")))

    ## Make sure data frame columns get named correctly
    for(i in 1:length(data.col.names$regex)) {
        names(data.raw)[grepl(pattern = data.col.names$regex[i], x = names(data.raw))] <-
            data.col.names$df.names[i]
    }

    ## Check that all required columns are present
    not.there <- !(data.col.names$df.names[data.col.names$required] %in% names(data.raw))
    if(sum(not.there) > 0) {
        miss.cols <-
            paste(data.col.names$df.names[data.col.names$required][not.there], collapse = ", ")
        stop("The following required columns are not in the input data file: ", miss.cols)
    }

    ## Absence of probing questions must be 0 or 1
    colnm_apqb <-
        grep("absence.of.probing.questions.bias.1*", colnames(data.raw), value = TRUE, ignore.case = TRUE)
    if(!isTRUE(all(data.raw[,colnm_apqb] %in% c(0,1))))
        stop("Column '", colnm_apqb, "' in the input file must contain only 0s and 1s.")

    ## Start.year and End.year must not be missing
    sapply(c("Start.year", "End.year"), CheckDataMissing,
           data_frame = data.raw,
           data_frame_name = data.csv)


    ## -------* Delete rows with no ISO and country name

    idx.keep <- !(is.na(data.raw$Country) | is.na(data.raw$ISO.code))
    if(sum(!idx.keep, na.rm = TRUE) > 0) {
        if(print.messages) message("Deleting row with no ISO and country name:\nThe following rows in '", data.csv, "' removed because 'ISO.code' or 'Country' missing: ", paste0((1:nrow(data.raw))[!idx.keep], collapse = ", "), "\n")
        }
    data.raw <- data.raw[idx.keep,]

    if (!is.null(iso.select)) {
        if (all(grepl("[[:digit:]]", iso.select))) {
            if(sum(as.numeric(data.raw$ISO.code) %in% iso.select) > 0) {
                data.raw <- data.raw[as.numeric(data.raw$ISO.code) %in% iso.select, ]
            } else {
                stop("One country model cannot be run for ISO ", iso.select, " because there are not observations for it in '", data.csv, "'.")
            }
        } else if (all(grepl("[[:alpha:]]", iso.select))) {
            stop("Three-letter country codes supplied as argument to 'iso.select' but these are not required in the input data file (only in the classifications file). Supply numeric iso codes instead")
        }

    if(print.messages) message(paste0("Note: Only data for ISO ", paste(iso.select, collapse = ", ")
             ," (", data.raw[as.numeric(data.raw$ISO.code) %in% iso.select, "Country"][1], ") is read in.\n"))
  }

    ## -------* Process CP Values

    sapply(c("Contraceptive.use.MODERN",
             "Contraceptive.use.TRADITIONAL",
             "Unmet"), CheckDataRange,
           data_frame = data.raw,
           data_frame_name = data.csv,
           range = c(0, 100))

    data.raw$rounded.up <- rep(FALSE, nrow(data.raw))

    ## -------** Round to 9 DPs

    data.raw$Contraceptive.use.MODERN <-
        round(as.numeric(data.raw$Contraceptive.use.MODERN), 9)
    data.raw$Contraceptive.use.TRADITIONAL <-
        round(as.numeric(data.raw$Contraceptive.use.TRADITIONAL), 9)
    data.raw$Contraceptive.use.ANY <-
        round(as.numeric(data.raw$Contraceptive.use.ANY), 9)

    ## -------** Only Mod or Only Trad

    ## There are two ways to handle observations which only measure CP Modern (no CP Trad, no CP Any).
    ## 1) Model the logit of CP Modern with a new component in the data model.
    ## 2) Assume the CP Mod observation is for CP Any: set CP Mod to
    ## the observed minus 0.001 (0.1%), set CP Trad to 0.001.

    ## Mark NAs
    trad.na <- is.na(data.raw$Contraceptive.use.TRADITIONAL) | is.nan(data.raw$Contraceptive.use.TRADITIONAL)
    mod.na <- is.na(data.raw$Contraceptive.use.MODERN) | is.nan(data.raw$Contraceptive.use.MODERN)
    any.na <- is.na(data.raw$Contraceptive.use.ANY) | is.nan(data.raw$Contraceptive.use.ANY)

    if(ModelFunctionModOnlyObs(write.model.fun)) {
        ## 1)
        mod.only <- any.na & trad.na & !mod.na
        if(sum(mod.only) > 0) {
            data.raw$Contraceptive.use.TRADITIONAL[mod.only] <- NA
            data.raw$Contraceptive.use.ANY[mod.only] <- NA
        }
    } else {
        ## 2)
        mod.only <- rep(FALSE, nrow(data.raw)) #need this for 'Exclusions'
        trad.round <- trad.na & !mod.na
        if(sum(trad.round) > 0) {
            data.raw$Contraceptive.use.TRADITIONAL[trad.round] <- 0.1
            data.raw$rounded.up <- trad.round | data.raw$rounded.up
            message(paste0("There are ", sum(trad.round, na.rm = TRUE), " observations with missing prevalence for CP Traditional but non-missing for modern. In the input data file, these are in rows (ignoring header):\n", paste(which(trad.round), collapse = " "), "\nThese will be ROUNDED UP to 0.1 percent."))
        }
        mod.round <- mod.na & !trad.na
        if(sum(mod.round) > 0) {
            data.raw$Contraceptive.use.MODERN[mod.round] <- 0.1
            data.raw$rounded.up <- mod.round | data.raw$rounded.up
            message(paste0("There are ", sum(mod.round, na.rm = TRUE), " observations with missing prevalence for CP Modern but non-missing for traditional. In the input data file, these are in rows (ignoring header):\n", paste(which(mod.round), collapse = " "), "\nThese will be ROUNDED UP to 0.1 percent."))
        }
    }

    ## -------** Round Small Values UP to 0.1 percent

    modern.zero <-
        !is.na(data.raw$Contraceptive.use.MODERN) &
        (round(data.raw$Contraceptive.use.MODERN, 9) < 0.1)
    if(sum(modern.zero, na.rm = TRUE) > 0) {
        data.raw$Contraceptive.use.MODERN[modern.zero] <- 0.1
        data.raw$rounded.up <- modern.zero | data.raw$rounded.up
        message(paste0("There are ", sum(modern.zero, na.rm = TRUE), " observations with modern prevalence of zero. In the input data file, these are in rows (ignoring header):\n", paste(which(modern.zero), collapse = " "), "\nThese will be ROUNDED UP to 0.1 percent."))
    }

    trad.zero <-
        !is.na(data.raw$Contraceptive.use.TRADITIONAL) &
        (round(data.raw$Contraceptive.use.TRADITIONAL, 9)  < 0.1)
    if(sum(trad.zero, na.rm = TRUE) > 0) {
        data.raw$Contraceptive.use.TRADITIONAL[trad.zero] <- 0.1
        data.raw$rounded.up <- trad.zero | data.raw$rounded.up
        message(paste0("There are ", sum(trad.zero, na.rm = TRUE), " observations with traditional prevalence of zero. In the input data file, these are in rows (ignoring header):\n", paste(which(trad.zero), collapse = " "), "\nThese will be ROUNDED UP to 0.1 percent."))
    }

    any.zero <-
        !is.na(data.raw$Contraceptive.use.ANY) &
        (round(data.raw$Contraceptive.use.ANY, 9)  < 0.1)
    if(sum(any.zero, na.rm = TRUE) > 0) {
        data.raw$Contraceptive.use.ANY[any.zero] <- 0.1
        data.raw$rounded.up <- any.zero | data.raw$rounded.up
        message(paste0("There are ", sum(any.zero, na.rm = TRUE), " observations with prevalence (any method) of zero. In the input data file, these are in rows (ignoring header):\n", paste(which(any.zero), collapse = " "), "\nThese will be ROUNDED UP to 0.1 percent."))
    }

    ## -------** CP Any ^= CP Mod but CP Trad blank

    ## If there are values for CP Any and CP Mod, not equal, but CP
    ## Trad is missing, set CP Trad to CP Any - CP Mod.
    mod.neq.any <- (is.na(data.raw$Contraceptive.use.TRADITIONAL) | is.nan(data.raw$Contraceptive.use.TRADITIONAL)) & !is.na(data.raw$Contraceptive.use.MODERN) & !is.na(data.raw$Contraceptive.use.ANY) &
        (round(data.raw$Contraceptive.use.MODERN, 9) != round(data.raw$Contraceptive.use.ANY, 9))
    if(sum(mod.neq.any, na.rm = TRUE) > 0) {
        message(paste0("There are ", sum(mod.neq.any, na.rm = TRUE), " observations with traditional prevalence missing but Modern and Any are not. In the input data file, these are in rows (ignoring header):\n", paste(which(mod.neq.any), collapse = " "), "\nTRADITIONAL prevalences for these observations HAVE BEEN SET TO 'ANY' - 'MODERN' and these observations WILL contribute to estimates of the modern/traditional breakdown."))
        data.raw[mod.neq.any,]$Contraceptive.use.TRADITIONAL <-
            data.raw[mod.neq.any,]$Contraceptive.use.ANY - data.raw[mod.neq.any,]$Contraceptive.use.MODERN
    }

    ##  -------** Blank out Modern

    ## If 'TRADITIONAL' is blank AND 'MODERN' = 'ANY' after rounding, set 'MODERN' to blank as well
    trad.blank <- (is.na(data.raw$Contraceptive.use.TRADITIONAL) | is.nan(data.raw$Contraceptive.use.TRADITIONAL)) & !is.na(data.raw$Contraceptive.use.MODERN) & !is.na(data.raw$Contraceptive.use.ANY) & (round(data.raw$Contraceptive.use.MODERN, 9) == round(data.raw$Contraceptive.use.ANY, 9))
    if(sum(trad.blank, na.rm = TRUE) > 0) {
        message(paste0("There are ", sum(trad.blank, na.rm = TRUE), " observations with traditional prevalence missing. In the input data file, these are in rows (ignoring header):\n", paste(which(trad.blank), collapse = " "), "\nMODERN prevalences for these observations HAVE ALSO BEEN SET TO MISSING and these observations will NOT contribute to estimates of the modern/traditional breakdown."))
        data.raw[trad.blank,]$Contraceptive.use.MODERN <- NA
    }

    ## -------** Clean Mod/Trad Breakdown Obs

    bdown <- !is.na(data.raw$Contraceptive.use.MODERN) & !is.na(data.raw$Contraceptive.use.TRADITIONAL)

    ## Fill in: CP Any = CP Mod + CP Trad
    ## This version requires CP Any to be non-missing for mod/trad breakdown obs
    data.raw$Contraceptive.use.ANY[bdown] <-
                 data.raw$Contraceptive.use.MODERN[bdown] + data.raw$Contraceptive.use.TRADITIONAL[bdown]

    ## -------* Process Unmet Values

    data.raw$Unmet <- round(as.numeric(data.raw$Unmet), 9)

    unmet.zero <- !is.na(data.raw$Unmet) & (data.raw$Unmet == 0)
    if(sum(unmet.zero, na.rm = TRUE) > 0) {
        data.raw$Unmet[unmet.zero] <-
            sapply(100 - data.raw$Contraceptive.use.ANY[unmet.zero],
                   function(z) min(0.1, z))
        warning(paste0("There are ", sum(unmet.zero, na.rm = TRUE), " observations with unmet need of zero. In the input data file, these are in rows (ignoring header):\n", paste(which(modern.zero), collapse = " "), "\nThese will be ROUNDED UP to 0.1 percent."))
    }

    ## -------* Replace NAs with '""' for CHARACTER columns

    char.cols <- numeric(0)
    for(j in 1:ncol(data.raw)) {
        if(is.character(data.raw[,j])) {
            char.cols <- c(char.cols, j)
            idx <- is.na(data.raw[,j])
            data.raw[idx,j] <- ""
        }
    }
    char.col.names <- paste(colnames(data.raw)[char.cols], collapse = ", ")

    ## -------* Fix Excel re-formats

    ## Move this here
    ## 1. Fix age groups that excel made into dates
    ## change JR, 20140409
    data.raw$Age..range <-
        InternalFixRange(gsub(" ", "", data.raw$Age..range))

    ## Move this here.
    ## [MCW-2016-03-11-2]: Fix end year
    data.raw$End.year <- InternalFixEndYear(data.raw$End.year)

    ## -------* Standard Error Columns

    ## From Niamh's version: gets survey-base SEs (2017-04-19)
    ##Add columns that indicate if SEs are available for modern,trad,unmet ratios
    include.modern.ses<-rep(NA,nrow(data.raw))
    include.trad.ses<-rep(NA,nrow(data.raw))
    include.unmet.ses<-rep(NA,nrow(data.raw))

    ##Indicator if modern ratios ses are available
    if(is.null(data.raw$SE.logR.modern.nouse))
    {
        include.modern.ses<-rep(0,length(include.modern.ses))
    }
    if(!is.null(data.raw$SE.logR.modern.nouse))
    {
        if(any(is.na(data.raw$SE.logR.modern.nouse)))
        {
            ## Take care of 'NaN's/'Infs' [MCW-2018-01-02]
            include.modern.ses[which(!is.finite(data.raw$SE.logR.modern.nouse))]<-0
            ## include.modern.ses[which(is.na(data.raw$SE.logR.modern.nouse))]<-0
            include.modern.ses[which(!is.na(data.raw$SE.logR.modern.nouse))]<-1
        }
        if(all(!is.na(data.raw$SE.logR.modern.nouse)))
        {
            include.modern.ses<-rep(1,length(include.modern.ses))
        }
    }

    ##Indicator if traditional ratios ses are available
    if(is.null(data.raw$SE.logR.trad.nouse))
    {
        include.trad.ses<-rep(0,length(include.trad.ses))
    }
    if(!is.null(data.raw$SE.logR.trad.nouse))
    {
        if(any(is.na(data.raw$SE.logR.trad.nouse)))
        {
            ## Take care of 'NaN's/'Infs' [MCW-2018-01-02]
            include.trad.ses[which(!is.finite(data.raw$SE.logR.trad.nouse))]<-0
            ## include.trad.ses[which(is.na(data.raw$SE.logR.trad.nouse))]<-0
            include.trad.ses[which(!is.na(data.raw$SE.logR.trad.nouse))]<-1
        }
        if(all(!is.na(data.raw$SE.logR.trad.nouse)))
        {
            include.trad.ses<-rep(1,length(include.trad.ses))
        }
    }

    ##Indicator if unmet ratios ses are available
    if(is.null(data.raw$SE.logR.unmet.noneed))
    {
        include.unmet.ses<-rep(0,length(include.unmet.ses))
    }
    if(!is.null(data.raw$SE.logR.unmet.noneed))
    {
        if(any(is.na(data.raw$SE.logR.unmet.noneed)))
        {
            ## Take care of 'NaN's/'Infs' [MCW-2018-01-02]
            include.unmet.ses[which(!is.finite(data.raw$SE.logR.unmet.noneed))]<-0
            ## include.unmet.ses[which(is.na(data.raw$SE.logR.unmet.noneed))]<-0
            include.unmet.ses[which(!is.na(data.raw$SE.logR.unmet.noneed))]<-1
        }
        if(all(!is.na(data.raw$SE.logR.unmet.noneed)))
        {
            include.unmet.ses<-rep(1,length(include.unmet.ses))
        }
    }
    data.raw<-cbind(data.raw,include.trad.ses,include.modern.ses,include.unmet.ses)

    if(ModelFunctionSurveySEs(write.model.fun) && is.null(iso.select)) {
        if(!isTRUE(all(c(sum(include.modern.ses)
                        ,sum(include.trad.ses)
                        ,sum(include.unmet.ses)) > 0))) {
            stop("'write.model.fun' requires survey-based SEs for 'modern', 'traditional', and 'unmet', but none present in the input file.")
        }
    }

    ## -------* Exclusions

    ##details<<
    ## Observations are excluded if column \code{EXCLUDE1isyes} == 1.
    ## Observations are excluded if \code{Note.on.methods} is "Data pertain to methods used since the last pregnancy."
    ## or "Data pertain to past or current use.".
    ## Observations are excluded if total use is missing, unless they are from service statistics.
    ## (one survey in Bhutan with modern only)
    if (is.null(data.raw$Note.on.methods)) data.raw$Note.on.methods <- rep(NA, nrow(data.raw))

    remove <-
        (!is.na(data.raw$EXCLUDE1isyes) & data.raw$EXCLUDE1isyes == 1) |
        (is.na(data.raw$Contraceptive.use.ANY) &
         !grepl(data.cell.vals$service.statistic, data.raw$Data.series.type) &
         !mod.only #<< keep obs if have only CP Modern.
        ) |
        (!is.na(data.raw$Note.on.methods) &
         (grepl(data.cell.vals$data.pertain.to.methods.used.since.the.last.pregnancy, data.raw$Note.on.methods) |
          grepl(data.cell.vals$data.pertain.to.past.or.current.use, data.raw$Note.on.methods)
         )
        )

    if (any(remove)) {
        data.raw <- data.raw[!remove, ]
        if(print.messages) message("Excluding certain observations. Observations are excluded from input data if: column 'EXCLUDE1isyes' == 1;
'Note.on.methods' is 'Data pertain to methods used since the last pregnancy.'; or 'Data pertain to past or current use.'; total use is missing, unless they are from service statistics.\n", paste0(sum(remove), " observations were removed. There were in rows (ignoring header): ",paste0(which(remove), collapse = ", "), "." ), "\n")
    }

    ## AFTER fixing all column headings and cells: Make sure only one
    ## observation per catalogue ID, age-group, marital status.
    if(!is.null(data.raw$Catalog.ID)) {
        if(sum(is.na(data.raw$Catalog.ID)) > 0) {
            if(print.warnings) message("Checking for duplicate rows:\nThere are missing 'Catalog.ID's in row(s) (assuming header is row 1):\n", paste0(which(is.na(data.raw$Catalog.ID)) + 1, collapse = ", "))
        }
        dup.row <-
            as.data.frame(with(data.raw, table(Catalog.ID, Age..range, Population.type)))
        dup.row <- dup.row[dup.row$Freq > 1,]
        if(nrow(dup.row) > 0) {
            if(print.warnings) message("Checking for duplicate rows:\nDuplicate Catalog ID*Age..range*Population.type combinations exist in 'data.csv'. NOTE: Nothing was removed but the duplicates have 'Catalog.ID's:\n", paste0(dup.row$Catalog.ID, collapse = ", "))
        }
    } else {
        if(print.warnings) message("No 'Catalog.ID' column in ", data.csv, "; input data not checked for duplicates.")
    }

    return(data.preprocessed = data.raw)
}

###----------------------------------------------------------------------------------
### Preprocess classification file
###
PreprocessClassification <-
    function(regioninfo.csv = NULL,
             check.missing = TRUE) {

        ## -------* Set up

        if(is.null(regioninfo.csv)) {
            regioninfo.csv <-
                file.path(find.package("FPEMglobal")
                         ,"data", "Country-and-area-classification.csv"
                          )
        }

        ## -------* Constants

        ## Create column name regexps and cell value regexps
        data.col.names <- InternalRegExpsInputCols()
        data.cell.vals <- InternalRegExpsInputCells()

        ## -------* MAIN BODY

        ## Read csv
        country.info <- read.csv(regioninfo.csv, stringsAsFactors = FALSE)

        ## Make sure columns named properly
        for(i in 1:length(data.col.names$regex)) {
            names(country.info)[grepl(pattern = data.col.names$regex[i], x = names(country.info))] <-
                data.col.names$df.names[i]
        }

        ## Remove Sark because it has no 3-letter code
        if(680 %in% as.numeric(country.info$ISO.code)) {
            country.info <-
                country.info[as.numeric(country.info$ISO.code) != 680,]
            message("'Sark' is in '", regioninfo.csv, "' but will be ignored because it has no 3-letter country code.")
        }

        ## Check for missing values
        if(check.missing) {

            ## Check for missing values
            miss.v <- which(country.info == "" | country.info == NA, arr.ind = TRUE)
            if(sum(miss.v) > 0) {
                stop("Missing values in ", "'", regioninfo.csv, "'", " at row "
                    ,miss.v[1], ", col ", miss.v[2]
                   ," ('", colnames(country.info)[miss.v[2]], "')", ".")
            }
        }

        ## -------* Return

        return(country.info)

    }


###----------------------------------------------------------------------------------
##' Read in alternative file aggregates with some checks.
##'
##' \code{\link{ReadFileAggregates}} checks some column names using \code{\link{PreprocessClassification}}.
##'
##' @param file.aggregates If NULL (default), UNDP aggregates are
##'     constructed. Alternatively, file path of alternative grouping should be
##'     given, e.g. \code{file.aggregates = "data/MDGgroupings.csv")}. Such data
##'     file needs to contain columns \code{iso.country}, \code{groupname} and
##'     \code{iso.group} (which may contain missing values). Each country can
##'     only be included once (can only be part of one grouping).
##' @return
##' @author Mark Wheldon
ReadFileAggregates <- function(file.aggregates) {

    ## -------* SET UP

    ## -------** Inputs

    agg.info <- PreprocessClassification(file.aggregates, check.missing = FALSE)

    ## -------* MAIN

    ## -------* RETURN
    }


###----------------------------------------------------------------------------------
### Read contraceptive prevalence data
###
ReadDataAll <-
    function(## To use alternative data set, use \code{data.csv =
             ## .../dataCPmodel.csv}, where ... refers to the file path where
             ## file is located.
             data.csv = NULL,
             data.preprocessed = NULL,
             ## R object produced by \code{\link{PreprocessData}}. If
             ## \code{NULL} and data.csv NULL, data set included in package is
             ## used.

             output.dir = NULL,

             ## To use alternative csv file, use \code{regioninfo.csv =
             ## .../Country-and-area-classification.csv}, where ... refers to
             ## the file path where file is located.
             regioninfo.csv = NULL,
             regioninfo.preprocessed = NULL,
             ## R object produced by \code{\link{PreprocessData}}. If
             ## \code{NULL} and regioninfo.csv is NULL, region info included in
             ## package is used.

             ##<< If \code{NULL}, data of all countries/subpopulations in data
             ## set are read in, else only data of countries/subpopulations with
             ## selected ISO code is read in.
             iso.select = NULL,

             ##<< If not \code{NULL}, data is treated as data from subpopulation
             ##of country with the ISO code iso.country.select.
             iso.country.select = NULL,

             ##<< Country name corresponding to
             ##\code{iso.country.select}. Cannot be \code{NULL} if
             ##\code{iso.country.select} is not \code{NULL}.
             name.country.select = NULL,

             ##<< Do first pass run of run with SS data?
             do.SS.run.first.pass = do.SS.run.first.pass,

             ##<< csv file with ISO 3-character and 3-digit country codes, only
             ##used to convert iso.country.select into 3-digit country code if
             ##given in 3-character country code.
             countrycodes.csv = "data/Country-names-and-codes.csv",

             write.model.fun = "WriteModel",

             ##<<If not NULL, summary results are written to this HTML file.
             html.file = NULL,

             ## Count PMA as its own source? (This is now obsolete)
             disagg.RN.PMA = TRUE,

             ## Include countries with no data?
             include.c.no.data = TRUE,

             validation.list = NULL,

             ## Ensure that each country has at least
             ## 'at.random.min.c' data points in training set.
             at.random.min.c = 1,

             ## Should obs in countries with only 1 obs be kept in training for 'at.end' validation?
             at.end.not.1.obs.c = FALSE,

             ## Needed for SE imputations
             data.global = NULL,
             do.country.specific.run = FALSE

             ){

        ## -------* Set-up

        ## -------** Function arguments

        if (!is.null(iso.country.select) & is.null(name.country.select)) # change JR, 20140409
            stop("name.country.select cannot be NULL if iso.country.select is non-NULL.")

        ## Create new html file
        if (!is.null(html.file)){
            cat("", file = html.file, append = F)
            print(paste("Summary stats written to", html.file))
        }

        if (is.null(data.csv) && is.null(data.preprocessed)){
            data.csv <- file.path(find.package("FPEMglobal"), "data", "dataCPmodel.csv")
            cat(paste("CP data read from", data.csv), "\n")
        }
        if (is.null(regioninfo.csv) && is.null(regioninfo.preprocessed)){
            regioninfo.csv <-
                file.path(find.package("FPEMglobal"), "data", "Country-and-area-classification.csv")
        }
        cat(paste("Country/region info read from", regioninfo.csv), "\n")

        ## -------** Constants

        ## Create column name regexps and cell value regexps
        data.col.names <- InternalRegExpsInputCols()
        data.cell.vals <- InternalRegExpsInputCells()

        ## -------* Read in csv files, process

        ## -------** Input Data

        if(is.null(data.preprocessed)) {
        data.unsorted <-
            PreprocessData(data.csv = data.csv, iso.select = iso.select)
        } else data.unsorted <- data.preprocessed

        ## -------** Region info

        if(is.null(regioninfo.preprocessed)) {
            country.info.temp <- PreprocessClassification(regioninfo.csv)
            } else country.info.temp <- regioninfo.preprocessed

        ## -------*** Country names

        ## [MCW-2016-12-28-1] :: Replace country names in 'data.csv' with names in the
        ## classifications file. This protects against problems caused when the same
        ## country (as ID'd by ISO.Code) has different variants of its name (e.g.,
        ## 'Bolivia' and 'Bolivia (Plurinational State of)').
        if("Country" %in% colnames(data.unsorted)) {
            country.name.col <- which(colnames(data.unsorted) == "Country")
            data.unsorted <-
                merge(data.unsorted[,-country.name.col]
                     ,country.info.temp[,c("ISO.code"
                                          ,"Country.or.area"
                                           )]
                     ,all.x = TRUE, all.y = FALSE, by = "ISO.code", sort = FALSE
                      )
            colnames(data.unsorted)[colnames(data.unsorted) == "Country.or.area"] <- "Country"
            message("\nCountry names in 'data.csv' have been replaced by those in the country classifications file, matched by ISO code.")
        }

        ## -------*** Service statistics first pass?

        if (do.SS.run.first.pass) {
            remove <- grepl(data.cell.vals$service.statistic, data.unsorted$Data.series.type)
            if (any(remove)) {
                data.unsorted <- data.unsorted[!remove, ]
                cat(paste0(sum(remove), " service statistics data observations removed.\n"))
            }
        }

        ## -------*** Check that all ISO codes are in country classifications file

        ## [MCW-2017-06-23-2] :: Check that all countries in the input data are in the classifications file.
        not.in.cinfo <-
            unique(data.unsorted$ISO.code)[!(unique(data.unsorted$ISO.code) %in% country.info.temp$ISO.code)]
        if(length(not.in.cinfo > 0)) stop("Country with ISO code ", not.in.cinfo, " is in the input data file but not in the country classifications file. Fix and re-run")

        ## -------* Make Data Frame to Contain Data

        ## -------** Proportions

        ## For some obs there is no break-down of total into modern
        ## and trad CP.  Re-order the rows such that all modern obs
        ## are first, followed by obs with total only (makes things
        ## easier when constructing the data set for BUGS). Put obs
        ## with modern only right at the end. These are identifiable
        ## by a non-missing value for modern and a missing value for
        ## tot.

        J <- nrow(data.unsorted)

        props.modern.j <- round(data.unsorted$Contraceptive.use.MODERN/100, 9)
        props.tot.j <- round(data.unsorted$Contraceptive.use.ANY/100, 9)
        props.trad.j <- round(data.unsorted$Contraceptive.use.TRADITIONAL/100, 9)

        breakdown.ord <- !is.na(props.modern.j) & !is.na(props.trad.j)
        tot.ord <- !breakdown.ord & !is.na(props.tot.j)
        last.ord <- !breakdown.ord & !tot.ord
        order <- c(seq(1, J)[breakdown.ord]
                  ,seq(1, J)[tot.ord]
                  ,seq(1, J)[last.ord])

        props.modern.j <- props.modern.j[order]

        props.tot.j <- round(data.unsorted$Contraceptive.use.ANY[order]/100, 9)

        props.trad.j <- round(data.unsorted$Contraceptive.use.TRADITIONAL[order]/100, 9)

        ## -------** Rounding

        rounded.up <- data.unsorted$rounded.up[order]

        ## -------** CP any less than 1%

        less.than.1.pc <- props.tot.j < 0.01

        ## -------** Years

        years.j <- ((data.unsorted$Start.year+data.unsorted$End.year)/2)[order]
        start.j = data.unsorted$Start.year[order] #Change NC, 20170201
        end.j = data.unsorted$End.year[order] #Change NC, 20170201

        ## -------** Sort whole data frame

        data <- data.unsorted[order,]

        ## -------** Country names

        if (is.null(iso.country.select)) { # change JR, 20140404
            name.j <- data$Country
            name.unsorted <- data.unsorted$Country
        } else {
            name.j <- data$New.population
            name.unsorted <- data.unsorted$New.population
        }

        ## -------** Age

        ##details<< Age categorization:
        ##details<< - If group starts at 13-17 and ends at 47-51: base line (0).
        ##details<< - If group start at 13-17 but ends after 51: negative bias ("neg").
        ##details<< - "See notes" is positive.
        ##details<< - Other groups get flagged with "?" (other).
        Age <- data$Age..range

        has_age_category_bias_data <- ("age.cat.bias" %in% colnames(data)) && any(data$age.cat.bias != "")

        if (has_age_category_bias_data) {
            if(!isTRUE(all(as.character(data$age.cat.bias) %in% c("+", "-", "?", 0), na.rm = TRUE))) {
                warning("'age.cat.bias' is in input data but contains illegal entries. The only entries allowed are: 'as.character(data$age.cat.bias)) %in% c('+', '-', '?', 0), na.rm = TRUE)' This will probably cause the run to fail so fix this. You can delete 'age.cat.bias' to revert to default behaviour for 15--49 based on 'Age..range' column.")
            }
            age.cat.j <- data$age.cat.bias
        } else {
            message("Age category bias derived from 'Age..range' column. NOTE: This is not yet implemented for age groups other than 15--49.")
            age.cat.j <- rep(NA,J)
            old.warn <- getOption("warn")
            options(warn=-1) # next part will give warnings, suppress those
            for (j in 1:J){
                split <- as.integer(strsplit(Age[j], split = "")[[1]]) # gives a warning
                age.cat.j[j] <-
                    ifelse(length(split)!=5 ,"?",
                    ifelse(split[1]==1 & split[2]<8 & split[2]>2 & ((split[4]==4 & split[5]>6)|(split[4]==5 & split[5]<2)) , 0,
                    ifelse(split[1]==1 & split[2]<8 & split[2]>2 & split[4]==5 & split[5]>1 , "-","?")))
            }
            options(warn = old.warn) # back to original setting
            age.cat.j <- ifelse(Age == "See notes", "+", age.cat.j)
            if (!is.null(html.file)){
                print(xtable::xtable(xtabs(~Age+age.cat.j), digits = c(0,0,0,0,0), type = "html",
                             caption = "Age"), type="html", file = html.file, append = T)
                print(xtable::xtable(table(age.cat.j), digits = c(0,0), type = "html",
                             caption = "Age"), type="html", file = html.file, append = T)
                                # print(xtabs(~Age+age.cat.j))
            }
        }

        ## -------** Data Series Type

        ## get other columns in shape so that they're meaningful
        ## then have separate function to get the input for bugs

        source <- data$Data.series.type

        ##[MCW-2016-03-24-4] check if need to disaggregate repeated national and PMA
        if (disagg.RN.PMA) {
            source.j <- ifelse(is.element(source, c("DHS", "DHS microdata")), "DHS",
                        ifelse(is.element(source, c("MICS", "MICS1", "MICS2", "MICS3", "MICS4"
                                                   ,"MICS5", "MICS microdata"
                                                    )), "MICS",
                        ifelse(is.element(source, c("National survey", "National Survey")), "NS",
                        ifelse(source %in% c("Repeated-natl", "Repeated national"
                                            ,"Repeated national survey"), "RN",
                        ifelse(source %in% c("PMA", "PMA microdata", "PMA Microdata", "PMA micro-data", "PMA Micro-data", "PMA Micro-Data", "PMA micro data", "PMA Micro data", "PMA Micro Data"), "PMA",
                        ifelse(grepl("Service statistic", source), "SS",
                               "Other"))))))
        } else {
            source.j <- ifelse(is.element(source, c("DHS", "DHS microdata")), "DHS",
                        ifelse(is.element(source, c("MICS", "MICS1", "MICS2", "MICS3", "MICS4"
                                                   ,"MICS5" # change [MCW-2016-02-22-1]: added MICS5
                                                    )), "MICS", # change JR, 20140310: added MICS and MICS4
                        ifelse(source == "National survey", "NS",
                        ifelse(grepl("Service statistic", source), "SS", # change JR, 20131120 # change JR, 20140418
                               "Other"))))
        }

        RN.rows <- source.j == "RN"
        if(any(RN.rows)) {
            warning("Data source type 'RN' ('repeated national survey') is depracated. Data source in rows (ignoring header) "
                   ,paste(which(RN.rows), collapse = ", ")
                   ," has been set to 'NS' ('national survey').\n**PLEASE MAKE SURE THIS IS WHAT YOU WANT**")
        }
        source.j[RN.rows] <- "NS"

        other.rows <- source.j == "Other"
        if(any(other.rows)) {
            message("\nThe following data sources have been classified as 'Other': "
                   ,paste(unique(source[other.rows]), collapse = ", "))
            }

        if (!is.null(html.file)){
            print(xtable::xtable(xtabs( ~ source + source.j), digits = c(0,0,0,0,0), type = "html",
                         caption = "Source"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(source.j), digits = c(0,0), type = "html",
                         caption = "Source"), type="html", file = html.file, append = T)
        }
        geo.j <- data$GEO.biases..unknown.direction.
        if (all(is.na(geo.j))) # change JR, 20131121: convert NA's to ""
            geo.j[is.na(geo.j)] <- ""
        geo.short.j <- ifelse(geo.j != "", 1, 0)
        if (!is.null(html.file)){
            print(xtable::xtable(table(geo.j), digits = c(0,0), type = "html",
                         caption = "Geo"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(geo.j!=""), digits = c(0,0), type = "html",
                         caption = "Geo"), type="html", file = html.file, append = T)
        }

        ## -------** Population Type

        poptype.j <- data$Population.type
        poptype.short.j <- ifelse(is.element(poptype.j, c("BS", "HW")), "BSHW", paste(poptype.j))
        if (!is.null(html.file)){
            print(xtable::xtable(xtabs(~ poptype.j + poptype.short.j), digits = rep(0,6), type = "html",
                         caption = "Pop type"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(poptype.short.j), digits = c(0,0), type = "html",
                         caption = "Pop type"), type="html", file = html.file, append = T)
        }

        ## -------** Biases

        ## -------*** Folk Bias

        folkbias.j <- data$Folk.method.positive.bias
        if (all(is.na(folkbias.j))) # change JR, 20131121: convert NA's to ""
            folkbias.j[is.na(folkbias.j)] <- ""

        ## -------*** Positive Bias

        posbias.j <- data$Non.pregnant.and.other.positive.biases
        if (all(is.na(posbias.j))) # change JR, 20131121: convert NA's to ""
            posbias.j[is.na(posbias.j)] <- ""

        ## -------*** Modern Bias

        mod.bias.j <- data$Modern.method.bias
        if (all(is.na(mod.bias.j))) # change JR, 20131121: convert NA's to ""
            mod.bias.j[is.na(mod.bias.j)] <- ""

        ## -------*** Probing Q's Bias

        probe.bias.j <- data$Absence.of.probing.questions.bias
        if (all(is.na(probe.bias.j))) # change JR, 20131121: convert NA's to ""
            probe.bias.j[is.na(probe.bias.j)] <- ""

        ## -------** Print

        if (!is.null(html.file)){
            print(xtable::xtable(table(folkbias.j), digits = rep(0,2), type = "html",
                         caption = "Folk"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(folkbias.j!=""), digits = rep(0,2), type = "html",
                         caption = "Folk"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(posbias.j), digits = rep(0,2), type = "html",
                         caption = "+bias"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(posbias.j!=""), digits = rep(0,2), type = "html",
                         caption = "+bias"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(mod.bias.j), digits = rep(0,2), type = "html",
                         caption = "Bias modern"), type="html", file = html.file, append = T)
            print(xtable::xtable(table(probe.bias.j), digits = rep(0,2), type = "html",
                         caption = "Bias absence of probing questions"), type="html", file = html.file, append = T)
        }

        ## -------** Unmet Need

        source.unmet.j <- ifelse(source.j=="DHS", "DHS", "Other")
        props.unmet.j <- round(data$Unmet/100, 9)
        if (!is.null(html.file)){
            print(xtable::xtable(table(source.unmet.j[!is.na(props.unmet.j)]), digits = rep(0,2), type = "html",
                         caption = "Source Unmet need"), type="html", file = html.file, append = T)
        }

        ## -------** Survey-based SEs (copied from Niamh's 2017-04-19)

        ##SEs for log ratios #Change NC
        if(!is.null(data.unsorted$SE.logR.trad.nouse))
            se.logR.trad.nouse<-data.unsorted$SE.logR.trad.nouse[order] #Change NC, 20161219
        if(is.null(data.unsorted$SE.logR.trad.nouse))
            se.logR.trad.nouse<-rep(NA,J)

        if(!is.null(data.unsorted$SE.logR.modern.nouse))
            se.logR.modern.nouse<-data.unsorted$SE.logR.modern.nouse[order] #Change NC, 20161219
        if(is.null(data.unsorted$SE.logR.modern.nouse))
            se.logR.modern.nouse<-rep(NA,J)

        if(!is.null(data.unsorted$SE.logR.unmet.noneed))
            se.logR.unmet.noneed<-data.unsorted$SE.logR.unmet.noneed[order] #Change NC, 20161219
        if(is.null(data.unsorted$SE.logR.unmet.noneed))
            se.logR.unmet.noneed<-rep(NA,J)

        ##Indicator if SE is included or not, Change NC
        include.trad.ses<-data.unsorted$include.trad.ses[order] #Change NC, 20161219
        include.modern.ses<-data.unsorted$include.modern.ses[order] #Change NC, 20161219
        include.unmet.ses<-data.unsorted$include.unmet.ses[order] #Change NC, 20161219

        ## -------** Put in Data Frame

        data <- data.frame(iso.j = data$ISO.code, name.j, # change JR, 20140404
                           years.j, props.tot.j, props.modern.j, props.trad.j,
                     start.j, end.j, #Change NC, 20170102
                           age.cat.j, source.j, poptype.j, geo.j,
                           posbias.j, folkbias.j, mod.bias.j, probe.bias.j,
                           props.unmet.j, source.unmet.j,
                           rounded.up,
                           less.than.1.pc,
                          se.logR.trad.nouse,
                          se.logR.modern.nouse,
                           se.logR.unmet.noneed,
                          include.trad.ses,
                          include.modern.ses,
                           include.unmet.ses,
                     stringsAsFactors=FALSE)

        ## -------* Validation Run?

        if(!is.null(validation.list)) {

            ## -------** Draw Training Set

            ## Move generation of 'getj.training.k' via 'GetTraining()' here from
            ## 'GetBugsData()' so that 'at.random.no.data' validation exercise can
            ## be handled.
            ##details<< If \code{!is.null(validation.list)}, \code{getj.training.k} is constructed
            ## using \code{\link{GetTraining}}
            if (validation.list$generate.new.set){
                getj.training.k <- GetTraining(data, winbugs.data = NULL,
                                               validation.list = validation.list,
                                               country.info = country.info.temp,
                                               seed = validation.list$seed
                                              ,at.random.min.c = at.random.min.c
                                               ,at.end.not.1.obs.c = at.end.not.1.obs.c)
                if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
                    save(getj.training.k, file = file.path(output.dir, "getj.training.k.orig.rda"))
                } else {
                    save(getj.training.k, file = file.path(output.dir, "getj.training.k.rda")) # change JR, 20140418
                }
            } else { # not used!
                load(file = file.path(output.dir, "getj.training.k.rda")) ## change JR, 20140418
            }
        } else {
            getj.training.k <- 1:J
        }

        ## -------** Training and Test Set data

        if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
            data.test <- data[sort(setdiff(1:J, getj.training.k)),]
            data <- data[sort(getj.training.k),]
            write(unique(as.character(data.test$name.j)), file = file.path(output.dir, "test-set-countries.txt"))
            message("\nTest set countries written to ", file.path(output.dir, "test-set-countries.txt"))
        }

        ## -------* Make Data Frame for Country Info

        ## -------** Countries with Data

        ## Takes countries in the input data file and merges on regions,
        ## sub-regions, etc. from the classifications file.
        country.info <- unique(data.frame(data$name.j, # change JR, 20140404
                                          gsub(" ", "", data$iso.j), # change JR, 20131104
                                          stringsAsFactors=FALSE)
                               )
        names(country.info) <- c("name.c", "iso.c")
        country.info <- merge(country.info
                             ,country.info.temp[,c("ISO.code", "Country..letter.code")]
                             ,by.x = "iso.c", by.y = "ISO.code"
                             ,all.x = TRUE, sort = FALSE
                              )
        names(country.info)[3] <- "code.c"

        ## Keep only training set
        idx <- country.info$iso.c %in% data$iso.j
        country.info <- country.info[idx,]

        ## name.short.c <- InternalMakeCountryNamesShort(country.info$name.c)
        ## note: when using table, the output is in alphabetical order, thus China and China, Hong Kong are swapped
        ## swap back!
        ## sum(names(table(data$Country)[name.c])!=  paste(name.c))
        ## sum((table(data$Country)[name.c])!=  table(data$Country))

        ## 'N.c' is the number of observations for this country.
        N.c <- table(data$iso.j)[country.info$iso.c]

        ## Region info. Taken from the 'regioninfo.csv' file.
        if (is.null(iso.country.select)) { # change JR, 20140404
            reg.country.info.c <-
                InternalGetRegionInfoForCountry(iso.c = country.info$iso.c,
                                                country.info = country.info.temp, #Use '.temp' because 'country.info' has had training set countries removed
                                                countrycodes.csv = countrycodes.csv
                                               ,write.model.fun = write.model.fun)
        } else {
            reg.country.info.c <-
                InternalGetRegionInfoForCountry(iso.c = iso.country.select,
                                                country.info = country.info.temp,#Use '.temp' because 'country.info' has had training set countries removed
                                                countrycodes.csv = countrycodes.csv
                                               ,write.model.fun = write.model.fun)
        }

        ## Check to see if sexual activity column included
        if("sex.ac.unm.c" %in% colnames(reg.country.info.c)) {
            reg.country.cols.select <-
                c("namereg.c", "reg.c", "namesubreg.c", "subreg.c"
                ,"ssa.c", "dev.c", "fp2020.c"
                 ,"name.sex.ac.unm.c", "sex.ac.unm.c"
                 ,"name.reg.in.sex.ac.unm.c", "reg.in.sex.ac.unm.c"
                 ,"name.subreg.in.sex.ac.unm.c", "subreg.in.sex.ac.unm.c"
                 ,"name.reg.in.sex.ac.unm.SA1sub.c", "reg.in.sex.ac.unm.SA1sub.c"
                 ,"name.subreg.in.sex.ac.unm.SA1sub.c", "subreg.in.sex.ac.unm.SA1sub.c"
                  )
        } else {
            reg.country.cols.select <-
                c("namereg.c", "reg.c", "namesubreg.c", "subreg.c"
                ,"ssa.c", "dev.c", "fp2020.c")
        }
        country.info <- data.frame(country.info, N.c = as.numeric(N.c),
                                   reg.country.info.c[, reg.country.cols.select],
                                   stringsAsFactors=FALSE)

        if (!is.null(iso.country.select)) # change JR, 20140404
            country.info <- data.frame(country.info,
                                       iso.country.select = iso.country.select,
                                       name.country.select = name.country.select, # change JR, 20140409
                                       stringsAsFactors=FALSE)
        if (!is.null(html.file)){
            print(xtable::xtable(country.info, type = "html",
                         caption = "Country info"), type="html", file = html.file, append = T)
        }
        region.info <- InternalGetRegionInfoGeneral(country.info = country.info)

        ## -------** Countries without data

        ## [MCW-2016-08-23-1] Add countries with no data. Countries with no
        ## data have separate data frames so they can be treated differently in
        ## the JAGS model.

        if(include.c.no.data && !isTRUE(validation.list$at.random.no.data) && !isTRUE(validation.list$leave.iso.out)) {
            iso.no.data <- !(country.info.temp$ISO.code %in% country.info$iso.c)
                                #Use '.temp' because 'country.info' has had
                                #training set countries removed
        } else if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
            iso.no.data <- country.info.temp$ISO.code %in% data.test$iso.j
                                #Use '.temp' because 'country.info' has had
                                #training set countries removed
            } else iso.no.data <- 0
        if(isTRUE(sum(iso.no.data) > 0)) {

            ## -------*** Deal with 'iso.country.select'

            if(!is.null(iso.country.select)) {
                iso.no.data.select <-
                    iso.country.select[iso.country.select %in% country.info.temp$ISO.code[iso.no.data]]
            }
            if(!is.null(name.country.select)) {
                name.no.data.select <-
                    name.country.select[name.country.select %in% country.info.temp$Country..letter.code[iso.no.data]]
            }

            ## -------*** Country.info.no.data

            country.info.no.data <-
                data.frame(name.c = country.info.temp[iso.no.data, "Country.or.area"]
                          ,iso.c = as.character(country.info.temp[iso.no.data, "ISO.code"]) #to match country.info
                          ,code.c = country.info.temp[iso.no.data, "Country..letter.code"] #Used in the parameter plots.
                          ,N.c = 0
                           ,stringsAsFactors = FALSE
                           )
            names(country.info.no.data) <- c("name.c", "iso.c", "code.c", "N.c")

            if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
            ## Need to fill in column 'N.c' in 'country.info.no.data'. If just
            ## estimating for countries with actually no data this is 0. In this
            ## case, need it to contain actual number of obs.
                country.info.no.data$N.c <- table(data.test$name.j)[country.info.no.data$name.c]
                }

            ## -------*** Region / Subregion Info

            if(is.null(iso.country.select)) {
                reg.country.info.c.no.data <-
                    InternalGetRegionInfoForCountry(iso.c = country.info.temp$ISO.code[iso.no.data], #make sure it's numeric
                                                    country.info = country.info.temp,#Use '.temp' because 'country.info' has had training set countries removed
                                                    countrycodes.csv = countrycodes.csv
                                                    #,no.data = TRUE #numeric codes for (sub)regions not created
                                                   ,write.model.fun = write.model.fun)
            } else {
                reg.country.info.c.no.data <-
                    InternalGetRegionInfoForCountry(iso.c = iso.no.data.select, #make sure it's numeric
                                                    country.info = country.info.temp,#Use '.temp' because 'country.info' has had training set countries removed
                                                    countrycodes.csv = countrycodes.csv
                                                    #,no.data = TRUE #numeric codes for (sub)regions not created
                                                   ,write.model.fun = write.model.fun)
            }

            ## -------*** Merge Country and Region Info

            if(is.null(iso.country.select)) {
            country.info.no.data <-
                data.frame(country.info.no.data
                          ,reg.country.info.c.no.data[,reg.country.cols.select]
                          ,stringsAsFactors = FALSE)
            } else {
                country.info.no.data <-
                    data.frame(country.info.no.data
                              ,iso.country.select = iso.no.data.select
                               ,name.country.select = name.no.data.select
                              ,reg.country.info.c.no.data[,reg.country.cols.select]
                              ,stringsAsFactors = FALSE)
            }

            ## -------*** Fix Numeric Codes for Clusters

            ## 'country.info' and 'country.info.no.data' contain character
            ## variables with the names of clusters ((i.e., (sub)regions, sexual
            ## activity groups, etc) /and/ corresponding numeric codes. Creating
            ## them separately means that different codes might be used for the
            ## same cluster. The next part makes sure 'country.info' and
            ## 'country.info.no.data' use the same numeric codes for all
            ## clusters.

            info.fixed <- InternalFixNumericClusterCodes(country.info, country.info.no.data)
            country.info <- info.fixed$country.info
            country.info.no.data <- info.fixed$country.info.no.data

            ## [MCW-2016-09-07-4] :: 'region.info' and 'region.info.no.data'
            ## should have all the regions and subregions.
            region.info.no.data <-
                InternalGetRegionInfoGeneral(country.info = rbind(country.info, country.info.no.data))
            region.info <- region.info.no.data

            ## -------*** Index for Re-Ordering

            ## [MCW-2016-08-26-9] Create an index to re-order countries as in
            ## 'regioninfo.csv' when countries with no data are estimated. This
            ## is used, for example, in making the plots.
            ## [MCW-2016-09-07-5] :: USE ISO CODES to create re-ordering index.
            all.c <- unique(c(country.info$iso.c, country.info.no.data$iso.c))
            ## Make sure only countries in either 'country.info', or
            ## 'country.info.no.data' are referred to. This has an effect if
            ## doing validation exercise 'at.random.no.data'.
            ISO.all.orig.order <- as.character(country.info.temp$ISO.code)
            ISO.all.orig.order <- ISO.all.orig.order[ISO.all.orig.order %in% all.c]
            input.order.c <- sapply(ISO.all.orig.order, function(w) which(w == all.c))

        } else { ## ends part for no countries with no data

            ## -------** if !no.data

            country.info.no.data <- NULL
            region.info.no.data <- NULL
            input.order.c <- 1:nrow(country.info)
        }

        ## -------* SE Imputations

        if(ModelFunctionSurveySEs(write.model.fun)) {
            ## Do this here so 'at.random.no.data' and 'leave.iso.out' validations can be accommodated.
            ##Info on SEs #Change NC, 20161218
            se.info.j<-ImputeSE(data=data,country.info=country.info,data.global=data.global,do.country.specific.run=do.country.specific.run)
            if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
                se.info.j.test <-
                    ImputeSE(data=data.test,country.info=country.info.no.data,data.global=NULL,do.country.specific.run=FALSE)
                ## Possible that no test-set obs will have SEs, so set
                ## them to the imputed values from the training set.
                med.max.se.names <- c("med.max.trad.se", "med.max.modern.se", "med.max.unmet.se")
                logR.se.names <- c("se.logR.trad.impute", "se.logR.modern.impute", "se.logR.unmet.impute")
                for(i in seq_along(med.max.se.names)) {
                    if(is.na(se.info.j.test[[med.max.se.names[i]]])) {
                        se.info.j.test[[med.max.se.names[i]]] <- se.info.j[[med.max.se.names[i]]]
                    }
                }
                for(i in seq_along(logR.se.names)) {
                    ## Set to the imputed value
                    na.idx <- is.na(se.info.j.test[[logR.se.names[i]]])
                    if(any(na.idx)) {
                        se.info.j.test[[logR.se.names[i]]][na.idx] <-
                            rep(se.info.j.test[[med.max.se.names[i]]], sum(na.idx))
                    }
                }
            }
        } else {
            se.info.j <- NULL
            if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
                se.info.j.test <- NULL
            }
        }

        ## -------* Special List for Validation Ex. 'at.random.no.data'

        if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
            validation.at.random.no.data <-
                list(getj.training.k.orig = getj.training.k #original version
                    ,data.test = data.test
                     ,se.info.j.test = se.info.j.test
                     )

            ## Need to set getj.training.k to all rows of the triaining set.
            getj.training.k <- 1:nrow(data)
        } else {
            validation.at.random.no.data <- NULL
        }

        ## -------* Finish

        ##value<<
        return(list(data = data,##<< Object of class \code{data}, data frame, described here in Details
                    country.info = country.info,##<< Object of class \code{country.info}
                    region.info = region.info,##<< Object of class \code{region.info}
                    country.info.no.data = country.info.no.data,
                    region.info.no.data = region.info.no.data
                     ,se.info.j = se.info.j
                                        #MCW (2017-02-07): Forgotten why I need
                                        #to keep that... plots?
                    ## [MCW-2016-08-26-10] Return the re-ordering vector.
                   ,input.order.c = input.order.c
                   ,getj.training.k = getj.training.k
                    ,validation.at.random.no.data = validation.at.random.no.data
                    ))
    }
###----------------------------------------------------------------------------------
InternalFixRange <- function(# Fix range character vector that got converted to dates in Excel
  ### Replaces month names with numbers
  x
) {
  x <- as.character(x)
  from <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  to <- as.character(1:12)
  sub.table <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  for (i in 1:nrow(sub.table))
    x <- gsub(sub.table$from[i], sub.table$to[i], x)
  return(x)
}
###----------------------------------------------------------------------------------
### Regular expressions for column names in input data files.
###
### Put them in a function in this file so that they are standardized across the
### package; any other function that needs them can call this and get the same
### regexps. (Another option is to consider them as 'internal data' and put them
### in 'R/sysdata.rda'.)
###
InternalRegExpsInputCols <- function() {
    ##
    ##  General Notes:
    ##
    ##    Regexps are in alphabetical order.
    ##
    ##    The regexps are case insensitive and any number of any kinds of
    ##    whitespace character is allowed between words (e.g., space, tab,
    ##    newline) as well as a full stop '.'. No delimiter is also
    ##    matched. Beginning and end of word markers are included. E.g., 'Age
    ##    range', 'Age.range', 'age range', '. age..range ', 'AgeRange' all
    ##    match the regexp for 'age.range', but 'xage.range' and 'age.range1' do
    ##    not.
    ##
    ##  Regexp notes:
    ##
    ##    '?i' makes search case INsensitive
    ##
    ##    [:space:]' matches tab, newline, vertical tab, form feed, carriage
    ##    return, space and possibly other locale-dependent characters.
    ##

    ## -------* Subroutines

    re.col <- function(txt) {
        txt <- unlist(strsplit(txt, "\\."))
        txt <- txt[!txt==""] #if there are any multiple spaces, remove them
        txt <- paste(txt, collapse = "[.[:space:]]*")
        paste0("^[.[:space:]]*", "(?i:", txt, ")", "[.[:space:]]*$")
    }

    ## -------* MAIN BODY

    ## -------** Input data columns

    ## Vector of strings that will be turned into regular expressions, and will
    ## be the names of the elements of the output list. These regexps are used
    ## to search in the colnames of the input csv.
    list.names.input.cols <- c("age.range"  #becomes 'Age..range' after 'make.names()'.
                              ## ,"age.range.original"
                              ,"catalog.id"
                              ,"contraceptive.use.any"
                              ,"contraceptive.use.modern"
                              ,"contraceptive.use.traditional"
                              ,"country"
                              ,"country.letter.code"
                              ,"country.or.area"
                              ,"data.series.type"
                              ,"developed.region"
                              ,"end.year"
                              ,"exclude.1.is.yes"
                              ,"female.sterilization"
                              ,"folk.method.positive.bias"
                              ,"fp2020.country"
                              ,"geo.biases.unknown.direction"
                              ,"iso.code"
                              ,"least.developed.country"
                              ,"least.developed.regions"
                              ,"less.developed.regions..excluding.least.developed.countries"
                              ,"major.area"
                              ,"major.area.code"
                              ,"modern.method.bias"
                              ,"negative.bias"
                              ,"new.population"
                              ,"non.pregnant.and.other.positive.biases"
                              ,"note.on.country"
                              ,"note.on.methods"
                              ,"note.on.population"
                              ,"population.type"
                             ,"absence.of.probing.questions.bias.1*"
                              ,"region"
                              ,"region.code"
                              ,"SE.logR.modern.nouse"
                              ,"SE.logR.trad.nouse"
                              ,"SE.logR.unmet.noneed"
                              ,"sexual.activity.among.unmarried"
                              ,"sexual.activity.group"
                              ,"source.name"
                              ,"sub.saharan..africa"
                              ,"start.year"
                              ,"total.population.of.less.than.90.000.by.mid.2015"
                              ,"unmet"
                               ## For alternative aggregates (e.g., used by
                               ## 'GetAggregates()' when 'file.aggregates'
                               ## supplied).
                              ,"iso.country"
                              ,"groupname"
                               ,"iso.group"
                               )

    ## Vector of the names of the data frame columns that are created after reading in
    ## data files. These are a legacy of the specific way in which input file
    ## columns had to be written.
    ## MUST BE IN SAME ORDER as 'list.names.input.cols'!
    df.names.input.cols <- c("Age..range"
                            ## ,"AgeRangeOriginal"
                            ,"Catalog.ID"
                            ,"Contraceptive.use.ANY"
                            ,"Contraceptive.use.MODERN"
                            ,"Contraceptive.use.TRADITIONAL"
                            ,"Country"
                            ,"Country..letter.code"
                            ,"Country.or.area"
                            ,"Data.series.type"
                            ,"Developed..region"
                            ,"End.year"
                            ,"EXCLUDE1isyes"
                            ,"Female.sterilization"
                            ,"Folk.method.positive.bias"
                            ,"FP2020.country"
                            ,"GEO.biases..unknown.direction."
                            ,"ISO.code"
                            ,"Least.developed.country"
                            ,"Least.Developed.Regions"
                            ,"Less.developed.regions..excluding.least.developed.countries"
                            ,"Major.area"
                            ,"Major.area.Code"
                            ,"Modern.method.bias"
                            ,"Negative.bias"
                            ,"New.population"
                            ,"Non.pregnant.and.other.positive.biases"
                            ,"Note.on.country"
                            ,"Note.on.methods"
                            ,"Note.on.population"
                            ,"Population.type"
                             ,"Absence.of.probing.questions.bias"
                            ,"Region"
                            ,"Region.Code"
                              ,"SE.logR.modern.nouse"
                              ,"SE.logR.trad.nouse"
                              ,"SE.logR.unmet.noneed"
                            ,"Sexual.activity.among.unmarried"
                            ,"Sexual.activity.group"
                            ,"Source.name"
                            ,"Sub.Saharan..Africa"
                            ,"Start.year"
                            ,"Total.population.of.less.than.90.000.by.mid.2015"
                            ,"Unmet"
                               ## For alternative aggregates (e.g., used by
                               ## 'GetAggregates()' when 'file.aggregates'
                               ## supplied).
                              ,"iso.country"
                              ,"groupname"
                               ,"iso.group")
    ## Check that names is same length as list
    stopifnot(all.equal(length(df.names.input.cols)
                       ,length(list.names.input.cols)
                        ))

    ## -------** Create regular expressions

    out.input.cols <- lapply(list.names.input.cols, "re.col")

    ## -------** Name the lists

    ## Names are all lower case with multiple interword spaces replaced with
    ## single spaces. 'make.names()' is then applied, followed by 'tolower()'.

    names(out.input.cols) <- tolower(make.names(list.names.input.cols))

    ## -------** Which columns are REQUIRED?

    name.required <-
        c("Catalog.ID"
         ,"ISO.code"
         ,"Country"
         ,"EXCLUDE1isyes"
         ,"Start.year"
         ,"End.year"
         ,"Age..range"
         ,"Population.type"
         ,"Contraceptive.use.ANY"
         ,"Contraceptive.use.MODERN"
         ,"Contraceptive.use.TRADITIONAL"
         ,"Note.on.country"
         ,"Note.on.population"
         ,"Note.on.methods"
         ,"GEO.biases..unknown.direction."
         ,"Non.pregnant.and.other.positive.biases"
         ,"Negative.bias"
         ,"Modern.method.bias"
         ,"Folk.method.positive.bias"
         ,"Unmet"
         ,"Data.series.type"
         ,"Source.name"
         ,"Absence.of.probing.questions.bias"
         #,"SE.logR.modern.nouse"
         #,"SE.logR.trad.nouse"
         #,"SE.logR.unmet.noneed"
          )
    required <- which(df.names.input.cols %in% name.required)

    ## -------* END

    return(list(regex = out.input.cols, df.names = df.names.input.cols,
                required = required))
}

###----------------------------------------------------------------------------------
# Regular expressions for cell values in input files.
#
# Put them in a function in this file so that they are standardized across the
# package; any other function that needs them can call this and get the same
# regexps. (Another option is to consider them as 'internal data' and put them
# in 'R/sysdata.rda'.)
#
InternalRegExpsInputCells <- function() {
    ##
    ##  General Notes:
    ##
    ##    Regexps are in alphabetical order.
    ##
    ##    The regexps are case insensitive and any number of any kinds of
    ##    whitespace character is allowed between words (e.g., space, tab,
    ##    newline) as well as a full stop '.'. ALSO no delimiter is also
    ##    matched. E.g., 'Age range', 'Age.range', 'age range', 'AgeRange' all
    ##    match the regexp for 'age.range'.
    ##
    ##  Regexp notes:
    ##
    ##    '?i' makes search case INsensitive
    ##
    ##    [:space:]' matches tab, newline, vertical tab, form feed, carriage
    ##    return, space and possibly other locale-dependent characters.
    ##

    ## -------* Functions to make regular expressions

    re.cell <- function(txt) {
        txt <- unlist(strsplit(txt, "[[:space:]]"))
        txt <- txt[!txt==""] #if there are any multiple spaces, remove them
        txt <- paste(txt, collapse = "[.[:space:]]*")
        paste0("^[.[:space:]]*", "(?i:", txt, ")", "[.[:space:]]*$")
    }

    ## -------* MAIN BODY

    ## -------** Input data cells

    list.names.input.cells <-
        c("data pertain to methods used since the last pregnancy"
         ,"data pertain to past or current use"
         ,"repeated national survey"
         ,"service statistic"
          )

    ## -------** Create regular expressions

    out.input.cells <- lapply(list.names.input.cells, "re.cell")

    ## -------** Name the lists

    ## Names are all lower case with multiple interword spaces replaced with
    ## single spaces. 'make.names()' is then applied, followed by 'tolower()'.

    names(out.input.cells) <- tolower(make.names(list.names.input.cells))

    ## -------* END

    return(out.input.cells)
}
###----------------------------------------------------------------------------------
InternalGetRegionInfoGeneral <- function(# Summarize regional info
### Summarize regional info to construct region.info
             country.info
             ){
        n.subreg <- length(unique(country.info$subreg.c))
        n.reg <- length(unique(country.info$reg.c))
        name.subreg <- rep("", n.subreg)
        name.reg <- rep("", n.reg)
                                        # make sure that name.subreg[1] corresponds to same 1 in country.info
                                        #name.subreg <- levels(as.factor(country.info$subreg.c))
                                        #name.reg <- levels(as.factor(country.info$reg.c))
        for (subreg in 1:n.subreg){
            name.subreg[subreg] <- country.info$namesubreg.c[country.info$subreg.c==subreg][1]
        }
        for (reg in 1:n.reg){
            name.reg[reg] <- country.info$namereg.c[country.info$reg.c==reg][1]
        }
        name.reg.short <- name.reg
        name.reg.short <- ifelse(name.reg=="Latin America and the Caribbean","LAC", paste(name.reg.short))
        name.reg.short <- ifelse(name.reg=="Northern America","N. Am.", paste(name.reg.short))

        ## Sexual Activity
        if(all(c("sex.ac.unm.c", "reg.in.sex.ac.unm.c") %in% colnames(country.info))) {
            n.sex.ac.unm <- length(unique(country.info$sex.ac.unm.c))
            name.sex.ac.unm <- rep("", n.sex.ac.unm)
            for(x in 1:n.sex.ac.unm) {
                name.sex.ac.unm[x] <-
                    country.info$name.sex.ac.unm.c[country.info$sex.ac.unm.c == x][1]
            }
            n.reg.in.sex.ac.unm <- length(unique(country.info$reg.in.sex.ac.unm.c))
            name.reg.in.sex.ac.unm <- rep("", n.reg.in.sex.ac.unm)
            for(x in 1:n.reg.in.sex.ac.unm) {
                name.reg.in.sex.ac.unm[x] <-
                    country.info$name.reg.in.sex.ac.unm.c[country.info$reg.in.sex.ac.unm.c == x][1]
            }
            name.reg.in.sex.ac.unm.short <- name.reg.in.sex.ac.unm
            name.reg.in.sex.ac.unm.short <-
                gsub("Latin America and the Caribbean", "LAC"
                   ,name.reg.in.sex.ac.unm.short)
            name.reg.in.sex.ac.unm.short <-
                gsub("Northern America", "N. Am.", name.reg.in.sex.ac.unm.short)

            ## SA1 subregion, India alone
            n.reg.in.sex.ac.unm.SA1sub <- length(unique(country.info$reg.in.sex.ac.unm.SA1sub.c))
            name.reg.in.sex.ac.unm.SA1sub <- rep("", n.reg.in.sex.ac.unm.SA1sub)
            for(x in 1:n.reg.in.sex.ac.unm.SA1sub) {
                name.reg.in.sex.ac.unm.SA1sub[x] <-
                    country.info$name.reg.in.sex.ac.unm.SA1sub.c[country.info$reg.in.sex.ac.unm.SA1sub.c == x][1]
            }
            name.reg.in.sex.ac.unm.SA1sub.short <- name.reg.in.sex.ac.unm.SA1sub
            name.reg.in.sex.ac.unm.SA1sub.short <-
                gsub("Latin America and the Caribbean", "LAC"
                   ,name.reg.in.sex.ac.unm.SA1sub.short)
            name.reg.in.sex.ac.unm.SA1sub.short <-
                gsub("Northern America", "N. Am.", name.reg.in.sex.ac.unm.SA1sub.short)

        }

        region.info <- list(name.subreg = as.character(name.subreg),
                            name.reg = as.character(name.reg),
                            name.reg.short = as.character(name.reg.short),
                            n.subreg = n.subreg, n.reg = n.reg
                           ,name.sex.ac.unm = as.character(name.sex.ac.unm)
                           ,name.reg.in.sex.ac.unm = as.character(name.reg.in.sex.ac.unm)
                           ,name.reg.in.sex.ac.unm.short = as.character(name.reg.in.sex.ac.unm.short)
                          ,n.reg.in.sex.ac.unm = n.reg.in.sex.ac.unm
                           ,n.sex.ac.unm = n.sex.ac.unm
                           ,name.reg.in.sex.ac.unm.SA1sub = as.character(name.reg.in.sex.ac.unm.SA1sub)
                           ,name.reg.in.sex.ac.unm.SA1sub.short = as.character(name.reg.in.sex.ac.unm.SA1sub.short)
                          ,n.reg.in.sex.ac.unm.SA1sub = n.reg.in.sex.ac.unm.SA1sub
                            )
        ##value<<
        return(region.info##<< Object of class \code{\link{region.info}}
               ## where name.subreg[1] refers to index 1 in country.info$subreg.c etc
               )
    }
###---------------------------------------------------------------------------------------
InternalGetRegionInfoForCountry <- function(# Find (sub)region info for country vector
  ### Find (sub)region info for country vector
  iso.c,
  country.info, ##<< csv with region info
  countrycodes.csv ##<< csv with country codes info
 ,no.data = FALSE
 ,write.model.fun = "WriteModel"
  ){

    regions <- country.info
                                        # change JR, 20140404
    if (!all(grepl("[[:digit:]]", iso.c))) {
        if (all(grepl("[[:alpha:]]", iso.c))) {
            iso.c <- InternalGetCountryCodes(iso.c = iso.c, countrycodes.csv = countrycodes.csv)
        } else {
            stop("Elements of iso.c must be all alphabet letters or all digits.")
        }
    }

    C <- length(iso.c)

    dev.c <-  ldev.c <- namesubreg.c <- namereg.c <- ssa.c <- fp2020.c <- rep("", C)
    for (c in 1:C){
        namesubreg.c[c] <- paste(regions$Region[which.max(regions$ISO.code==iso.c[c])])
        namereg.c[c] <- paste(regions$Major.area[which.max(regions$ISO.code==iso.c[c])])
        ssa.c[c] <- paste(regions$Sub.Saharan..Africa[which.max(regions$ISO.code==iso.c[c])])
        dev.c[c] <- ifelse(paste(regions$Least.developed.country[which.max(regions$ISO.code==iso.c[c])])=="Yes","Poor",
                    ifelse(paste(regions$Developed..region[which.max(regions$ISO.code==iso.c[c])])=="Yes", "Rich", "Med"))
        fp2020.c[c] <- paste(regions$FP2020.country[which.max(regions$ISO.code==iso.c[c])])
    }

    ## for 'no.data' don't include the numeric codes for region and
    ## subregion. These are made separately to ensure they are the same for
    ## countries with and without data.
    if(no.data) {
        reg.country.info.c <- data.frame(as.character(iso.c),
                                         namereg.c,
                                         namesubreg.c,
                                         ssa.c, dev.c,
                                         fp2020.c,
                                         stringsAsFactors=FALSE)
        names(reg.country.info.c) <- c("iso.c", "namereg.c",
                                       "namesubreg.c", "ssa.c", "dev.c"
                                      ,"fp2020.c")
    } else {
        subreg.c <- as.numeric(as.factor(namesubreg.c))
        reg.c <- as.numeric(as.factor(namereg.c))
        reg.country.info.c <- data.frame(as.character(iso.c),
                                         reg.c, namereg.c,
                                         subreg.c, namesubreg.c,
                                         ssa.c, dev.c,
                                         fp2020.c,
                                         stringsAsFactors=FALSE)
        names(reg.country.info.c) <- c("iso.c", "reg.c", "namereg.c",
                                       "subreg.c", "namesubreg.c", "ssa.c", "dev.c"
                                      ,"fp2020.c")
    }

    ##
    ## Sexual activity classifications (do separately here so it's easy to see)
    ##
    if("Sexual.activity.among.unmarried" %in% colnames(regions)) {

        name.sex.ac.unm.c <- name.reg.in.sex.ac.unm.c <- name.subreg.in.sex.ac.unm.c <-
            name.reg.in.sex.ac.unm.SA1sub.c <- name.subreg.in.sex.ac.unm.SA1sub.c <- rep(NA, C)
                                        # 'name.reg.in.sex.ac.unm.c' is a new
                                        # variable: major region nested in
                                        # sexual activity category
        for(c in 1:C) {
            value.sex.ac.c <- regions$Sexual.activity.among.unmarried[which.max(regions$ISO.code==iso.c[c])]
            value.reg.c <- regions$Major.area[which.max(regions$ISO.code==iso.c[c])]
            value.subreg.c <- regions$Region[which.max(regions$ISO.code==iso.c[c])]

            name.sex.ac.unm.c[c] <- paste(value.sex.ac.c) # 0 or 1

            ## [MCW-2017-06-16-10] :: Make new columns in ~region.country.info.c~ to
            ## implement the new model for sexual activity categories:
            ## 1. If sexual activity group == 1, use subregions instead of major areas.
            ## 2. India is its own subregion and major area.

            value.reg.SA1sub.c <- value.reg.c
            value.reg.SA1sub.c[value.sex.ac.c == 1] <- value.subreg.c
            value.reg.SA1sub.c[iso.c[c] == "356"] <- #India
                regions$Country.or.area[regions$ISO.code == iso.c[c]]

            value.subreg.SA1sub.c <- value.subreg.c
            value.subreg.SA1sub.c[iso.c[c] == "356"] <- #India
                regions$Country.or.area[regions$ISO.code == iso.c[c]]

            ## Add SA labels.
            name.reg.in.sex.ac.unm.c[c] <-
                paste0(value.reg.c
                       ,ifelse(value.sex.ac.c == 1, " (SA high)", " (SA low)")
                      )
            name.subreg.in.sex.ac.unm.c[c] <-
                paste0(value.subreg.c
                       ,ifelse(value.sex.ac.c == 1, " (SA high)", " (SA low)")
                      )
            name.reg.in.sex.ac.unm.SA1sub.c[c] <-
                paste0(value.reg.SA1sub.c
                       ,ifelse(value.sex.ac.c == 1, " (SA high)", " (SA low)")
                      )
            name.subreg.in.sex.ac.unm.SA1sub.c[c] <-
                paste0(value.subreg.SA1sub.c
                       ,ifelse(value.sex.ac.c == 1, " (SA high)", " (SA low)")
                      )
        }
        reg.country.info.c$name.sex.ac.unm.c <-
            name.sex.ac.unm.c
        reg.country.info.c$name.reg.in.sex.ac.unm.c <-
            name.reg.in.sex.ac.unm.c
        reg.country.info.c$name.subreg.in.sex.ac.unm.c <-
            name.subreg.in.sex.ac.unm.c
        reg.country.info.c$name.reg.in.sex.ac.unm.SA1sub.c <-
            name.reg.in.sex.ac.unm.SA1sub.c
        reg.country.info.c$name.subreg.in.sex.ac.unm.SA1sub.c <-
            name.subreg.in.sex.ac.unm.SA1sub.c

        if(!no.data) {
            reg.country.info.c$sex.ac.unm.c <-
                as.numeric(as.factor(name.sex.ac.unm.c))
            reg.country.info.c$reg.in.sex.ac.unm.c <-
                    as.numeric(as.factor(reg.country.info.c$name.reg.in.sex.ac.unm.c))
            reg.country.info.c$subreg.in.sex.ac.unm.c <-
                    as.numeric(as.factor(reg.country.info.c$name.subreg.in.sex.ac.unm.c))
            reg.country.info.c$reg.in.sex.ac.unm.SA1sub.c <-
                    as.numeric(as.factor(reg.country.info.c$name.reg.in.sex.ac.unm.SA1sub.c))
            reg.country.info.c$subreg.in.sex.ac.unm.SA1sub.c <-
                    as.numeric(as.factor(reg.country.info.c$name.subreg.in.sex.ac.unm.SA1sub.c))
        }
    }
    ##value<< Data frame with (iso.c, reg.c, namereg.c, subreg.c, namesubreg.c, ssa.c, dev.c)
    ## where ssa.c and dev.c are factors,
    ## and reg.c and subreg.c are just integers
    ## part of Object \code{\link{country.info}}.
    return(reg.country.info.c)
}
###----------------------------------------------------------------------------------
InternalGetCountryCodes <- function(# Get ISO 3-digit country codes from character codes and vice versa.
  ### Get vector of 3-digit country codes from character codes and vice versa.
  iso.c,
  countrycodes.csv ##<< csv file with ISO 3-character and 3-digit country codes
) {
  # change JR, 20140404
  countrycodes <- read.csv(countrycodes.csv, header = T, stringsAsFactors = F)
  countrycodes$ISO.code <- as.character(countrycodes$ISO.code)
  iso.c <- as.character(iso.c)
  if (all(grepl("[[:digit:]]", iso.c))) {
    iso.c.output <- join(data.frame(ISO.code = iso.c), countrycodes)$Country.letter.code
  } else if (all(grepl("[[:alpha:]]", iso.c))) {
    iso.c.output <- join(data.frame(Country.letter.code = iso.c), countrycodes)$ISO.code
  } else {
    stop("Elements of iso.c must be all alphabet letters or all digits.")
  }
  if (any(is.na(iso.c.output)))
    warning(paste0("The country code(s) ", paste(iso.c[is.na(iso.c.output)], collapse = ", "),
                   " cannot be found in countrycodes.csv!"))
  return(iso.c.output)
}
###----------------------------------------------------------------------------------
## [MCW-2017-02-08-1] :: Created to harmonize the numbering of model clusters across countries with data and those without.
InternalFixNumericClusterCodes <- function(country.info, country.info.no.data)
{
    ## -------* Sub Functions

    MakeNumDf <- function(name.col, num.col) {
        all.clus <- levels(factor(c(country.info[[name.col]], country.info.no.data[[name.col]])))
        num.code.df <- data.frame(all.clus, 1:length(all.clus))
        colnames(num.code.df) <- c(name.col, num.col)
        return(num.code.df)
        }

    ## -------* MAIN PART

    ## -------** Data Frames w Numeric Codes

    regions.num.code.df <- MakeNumDf("namereg.c", "reg.c")
    subregions.num.code.df <- MakeNumDf("namesubreg.c", "subreg.c")
    sex.ac.num.code.df <- MakeNumDf("name.sex.ac.unm.c", "sex.ac.unm.c")
    reg.in.sex.ac.num.code.df <- MakeNumDf("name.reg.in.sex.ac.unm.c", "reg.in.sex.ac.unm.c")
    subreg.in.sex.ac.num.code.df <- MakeNumDf("name.subreg.in.sex.ac.unm.c", "subreg.in.sex.ac.unm.c")
    reg.in.sex.ac.SA1sub.num.code.df <-
        MakeNumDf("name.reg.in.sex.ac.unm.SA1sub.c", "reg.in.sex.ac.unm.SA1sub.c")
    subreg.in.sex.ac.SA1sub.num.code.df <-
        MakeNumDf("name.subreg.in.sex.ac.unm.SA1sub.c", "subreg.in.sex.ac.unm.SA1sub.c")

    ## -------** Fix Country Infos

    ## -------*** With Data

    ## Remember the order
    country.info$order <- 1:nrow(country.info)

    ## Merge
    country.info <-
        merge(country.info[,!colnames(country.info)=="reg.c"]
             ,regions.num.code.df, sort = FALSE
              )
    country.info <-
        merge(country.info[,!colnames(country.info)=="subreg.c"]
             ,subregions.num.code.df, sort = FALSE
              )
    country.info <-
        merge(country.info[,!colnames(country.info)=="sex.ac.unm.c"]
             ,sex.ac.num.code.df, sort = FALSE
              )
    country.info <-
        merge(country.info[,!colnames(country.info)=="reg.in.sex.ac.unm.c"]
             ,reg.in.sex.ac.num.code.df, sort = FALSE
              )
    country.info <-
        merge(country.info[,!colnames(country.info)=="subreg.in.sex.ac.unm.c"]
             ,subreg.in.sex.ac.num.code.df, sort = FALSE
              )
    country.info <-
        merge(country.info[,!colnames(country.info)=="reg.in.sex.ac.unm.SA1sub.c"]
             ,reg.in.sex.ac.SA1sub.num.code.df, sort = FALSE
              )
    country.info <-
        merge(country.info[,!colnames(country.info)=="subreg.in.sex.ac.unm.SA1sub.c"]
             ,subreg.in.sex.ac.SA1sub.num.code.df, sort = FALSE
              )

    ## Re-order
    country.info <-
        country.info[order(country.info$order)
                    ,!(colnames(country.info) == "order")
                     ]

    ## -------*** No Data

    ## Remember the order
    country.info.no.data$order <- 1:nrow(country.info.no.data)

    ## Merge
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="reg.c"]
             ,regions.num.code.df, sort = FALSE
              )
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="subreg.c"]
             ,subregions.num.code.df, sort = FALSE
              )
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="sex.ac.unm.c"]
             ,sex.ac.num.code.df, sort = FALSE
              )
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="reg.in.sex.ac.unm.c"]
             ,reg.in.sex.ac.num.code.df, sort = FALSE
              )
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="subreg.in.sex.ac.unm.c"]
             ,subreg.in.sex.ac.num.code.df, sort = FALSE
              )
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="reg.in.sex.ac.unm.SA1sub.c"]
             ,reg.in.sex.ac.SA1sub.num.code.df, sort = FALSE
              )
    country.info.no.data <-
        merge(country.info.no.data[,!colnames(country.info.no.data)=="subreg.in.sex.ac.unm.SA1sub.c"]
             ,subreg.in.sex.ac.SA1sub.num.code.df, sort = FALSE
              )

    ## Re-order
    country.info.no.data <-
        country.info.no.data[order(country.info.no.data$order)
                    ,!(colnames(country.info.no.data) == "order")
                     ]

    ## -------* Finish

    return(list(country.info = country.info
               ,country.info.no.data = country.info.no.data
                ))
}
###----------------------------------------------------------------------------------
InternalMakeCountryNamesShort <- function(# Shorten country names (and make consistent)
  ### Shorten country names (and make consistent)
  name.c){
    name.c = ifelse(name.c=="Cote d Ivoire", paste("Cote d'Ivoire"), paste(name.c))
    name.c = ifelse(name.c=="Sao Tome and Principe" |name.c=="Sao Tome & Principe", paste("Sao Tome Pr"), paste(name.c))
  name.c = ifelse(name.c=="Vietnam", paste("Viet Nam"), paste(name.c))
  name.c = ifelse(name.c=="Gambia The", paste("Gambia"), paste(name.c))
  name.c = ifelse(name.c=="Brunei Darussalam", paste("Brunei"), paste(name.c))
  name.c = ifelse(name.c=="Saint Kitts and Nevis", paste("Saint Kitts & Nevis"), paste(name.c))
  name.c = ifelse(name.c=="Timor Leste", paste("Timor-Leste"), paste(name.c))
  name.c = ifelse(name.c=="Dominican Rep.", paste("Dominican Republic"), paste(name.c))
  name.c = ifelse(name.c=="Syrian Arab Republic", paste("Syria"), paste(name.c))
  name.c = ifelse(name.c=="GuineaBissau", paste("Guinea-Bissau"), paste(name.c))
  name.c = ifelse(name.c=="Libyan Arab Jamahiriya", paste("Libya"), paste(name.c))
  name.c = ifelse(name.c=="Ukraine ", paste("Ukraine"), paste(name.c))
  name.c = ifelse(name.c=="Republic of Moldova"|name.c=="Moldova, Rep. of", paste("Moldova"), paste(name.c))
  name.c = ifelse(name.c=="Federated States of Micronesia"|name.c=="Micronesia (Federated States of )"|
  name.c=="Micronesia, Federated States of"|name.c=="Micronesia, Fed. States of"|
      name.c=="Micronesia (Fed. States of)", paste("Micronesia"), paste(name.c))
    name.c = ifelse(name.c=="United Kingdom", paste("U.K."), paste(name.c))
    name.c = ifelse(name.c=="United Kingdom of Great Britain and Northern Ireland", paste("U.K."), paste(name.c))
  name.c = ifelse(name.c=="United States of America"|name.c=="United States", paste("U.S."), paste(name.c))
  name.c = ifelse(name.c=="Congo, Dem. Rep."|name.c=="Democratic Republic of the Congo"|name.c=="Congo DR", paste("DRC"), paste(name.c))
  name.c = ifelse(name.c=="The former Yugoslav Republic of Macedonia"|name.c=="TFYR Macedonia", paste("Macedonia"), paste(name.c))
  name.c = ifelse(name.c=="Bosnia and Herzegovina"|name.c=="Bosnia & Herzegovina", paste("Bosn&Herze"), paste(name.c))
  name.c = ifelse(name.c=="Trinidad and Tobago"|name.c=="Trinidad & Tobago", paste("Trinidad&T"), paste(name.c))
  name.c = ifelse(name.c=="China, Hong Kong SAR", paste("Hong Kong"), paste(name.c))
  name.c = ifelse(name.c=="China, Macao Special Administrative Region", paste("Macao"), paste(name.c))
  name.c = ifelse(name.c=="China, Hong Kong Special Administrative Region", paste("Hong Kong"), paste(name.c))
  name.c = ifelse(name.c=="United Repulic of Tanzania",   paste("Tanzania"), paste(name.c))
  name.c = ifelse(name.c=="United States Virgin Islands",   paste("US Virgin Isl."), paste(name.c))
  name.c = ifelse(name.c=="United Arab Emirates",   paste("Arab Emirates"), paste(name.c))
  name.c = ifelse(name.c=="Lao People's Democratic Republic"|name.c=="Lao People's Dem. Rep."|
        name.c =="Lao PDR",   paste("Laos"), paste(name.c))
  name.c = ifelse(name.c=="Republic of Korea"|name.c == "Republic of Korea "|
        name.c == "Korea Rep"|name.c=="Korea, Rep. of", paste("South Korea"), paste(name.c))
  name.c = ifelse(name.c=="Democratic People's Republic of Korea"|name.c=="Korea DPR"|
        name.c=="Dem. People's Republic of Korea"|name.c=="Korea, Dem. People's Rep.", paste("North Korea"), paste(name.c))
  name.c = ifelse(name.c=="Central African Republic"|name.c=="Central African Rep.", paste("CAR"), paste(name.c))
  name.c = ifelse(name.c=="Iran (Islamic Republic of)"| name.c == "Iran, Islamic Republic of", paste("Iran"), paste(name.c))
  name.c = ifelse(name.c=="United Republic of Tanzania"| name.c=="Tanzania, United Republic of",   paste("Tanzania"), paste(name.c))
  name.c = ifelse(name.c=="Venezuela (Bolivarian Republic of)",   paste("Venezuela"), paste(name.c))
  name.c = ifelse(name.c=="Bolivia (Plurinational State of)",   paste("Bolivia"), paste(name.c))
  name.c = ifelse(name.c=="Antigua and Barbuda"|name.c=="Antigua & Barbuda", paste("Antigua and B."), paste(name.c))
  name.c = ifelse(name.c=="Northern Mariana Islands", paste("N. Mariana Isl."), paste(name.c))
  name.c = ifelse(name.c=="Occupied Palestinian Territory"|name.c=="OPT",
                  #paste("Occ. Palestinian Terr."),
                  paste("OPT"),
                  paste(name.c))
  name.c = ifelse(name.c=="Saint Vincent and the Grenadines" |
      name.c=="Saint Vincent & the Grenadines" |name.c=="Saint Vincent/Grenadines"|
      name.c == "St Vincent & the Grenadines", paste("St. Vincent & Gren."), paste(name.c))
  ##value<< Vector with length of \code{name.c}, but some names replaced
  return(name.c)
}

###----------------------------------------------------------------------------------
InternalWhichReg <- function(# Find region for vector of country iso codes
  ### Find region for vector of country iso codes
  iso.j, ##<< vector of country iso codes
  iso.c, ##<< vector with unique country iso codes
  reg.c##<< vector with region integers, corresponding to iso.c
  ){
  reg.j <- rep(NA, length(iso.j))
  for (iso in unique(iso.j)){
    reg <- reg.c[which.max(iso.c == iso)]
    reg.j[iso.j==iso] <- reg
  }
  ##value<< vector of length \code{iso.j} with region codes
  return(reg.j)
}

###----------------------------------------------------------------------------------
InternalInternalWhichSubreg <- function(# Find region for vector of country iso codes
  ### Find region for vector of country iso codes
  iso.j, ##<< vector of country iso codes
  iso.c, ##<< vector with unique country iso codes
  subreg.c##<< vector with sub-region integers, corresponding to iso.c
  ){
  subreg.j <- rep(NA, length(iso.j))
  for (iso in unique(iso.j)){
    subreg <- subreg.c[which.max(iso.c == iso)]
    subreg.j[iso.j==iso] <- subreg
  }
  ##value<< vector of length \code{iso.j} with subregion codes
  return(subreg.j)
}
###----------------------------------------------------------------------------------
GetObsCounts <- function(# Find proportion of countries with 0, 1, nres observations
  ### Find proportion of countries with 0, 1, nres observations
  data.j, ##<< observations, can include NAs
  iso.j, #<< ID for country
  select.j = NULL, ##<< subset of observations to select (NULL = all)
  selected.isos  = NULL,##<< set of iso codes to select (NULL = all)
  nres = 5 ##<< nres gives max obs, e.g. nres = 4 means 4+ is the last open-ended cat
  ## set to max(nres, 5) or min(4, nres)
  ){
  if (is.null(select.j)){
    select.j <- rep(T, length(data.j))
  }
  if (is.null(selected.isos)){
    selected.isos <- unique(iso.j)
  }
  # isos for countries with observations in the period
  isos.select <- iso.j[!is.na(data.j) & select.j==TRUE & is.element(iso.j, selected.isos)]
  if (nres >5) nres = 5
  if (nres <4) nres = 4
  if (length(isos.select)==0){
    res <- c(1, rep(0, nres))
    names(res) <- c("0", "1", "2", "3", "4", "5+")[1:(1+nres)]
    return(res)
  }
  # all countries, with or without observations in that particular period
  isos.all <- iso.j[ is.element(iso.j, selected.isos)]#select.j==TRUE &
  bla <- table(table(isos.select))
  if (nres==4){
    res <- c(length(unique(isos.all)) -  length(unique(isos.select)), # countries without data
             sum(bla["1"], na.rm =T), sum(bla["2"], na.rm =T), sum(bla["3"], na.rm =T),
             sum(bla[as.numeric(names(bla))>=4], na.rm =T)
             )
    names(res) <- c("0", "1", "2", "3", "4+")
  } else {
    res <- c(length(unique(isos.all)) -  length(unique(isos.select)),
             sum(bla["1"], na.rm =T), sum(bla["2"], na.rm =T), sum(bla["3"], na.rm =T),
             sum(bla["4"], na.rm =T),
             sum(bla[as.numeric(names(bla))>=5], na.rm =T)
             )
    names(res) <- c("0", "1", "2", "3", "4", "5+")
  }
  ##value<< vector with number of countries with 0,..,nres observations
  return(res/sum(res, na.rm = T))
}
###----------------------------------------------------------------------------------
InternalPlotPropsDataAvailability <- function(# Plot proportions of data availability
  ### Internal function for plotting proportions of data availability
  ymin, ##<< which height should stuff be plotted?
  res, ##<< proportions to be plotted
  col = 2, ##<< color to visualize proportion
  include.zero ##<< should first box with zero be included?
  ){
  yvalues <- ymin + res
  istart <- ifelse(include.zero,1,2)
  for (i in istart:length(res)){
    polygon(c(i-1,i-1, i,i,i-1),
            c(ymin,yvalues[i], yvalues[i], ymin, ymin),
            col = col, border = NA)
    # get box back
    polygon(c(i-1,i-1, i,i,i-1),
            c(ymin,ymin+1, ymin+1, ymin, ymin),
            bg = NA, border = 1)
    text(i-0.5,ymin+0.55, labels = round(100*res[i]), cex =1, col = 1)
  }
}

###----------------------------------------------------------------------------------
PlotDataAvailability <- function(# Create a plot to visualize data availability.
  ### Create a plot to visualize data availability.
  data, ##<< Object of \code{data}
  country.info, ##<< Object of \code{country.info}
  nres = 5, ##<< Determines the number of columns (nres+ is the last column, max is 5).
  summarize.unmet = FALSE, ##<< Logical: total prevalence is summarized when FALSE, unmet need when TRUE.
  years = c(1990, 2000, 2010),
  use.SDG.regions = FALSE
  ){

    if (!summarize.unmet){
        data.j <- data$props.tot.j
    } else {
        data.j <- data$props.unmet.j
    }
    iso.j  <- data$iso.j

    if(!is.null(years)) {
        years <- sort(unique(c(0, years, Inf)))
    } else stop("'years' must be non-NULL.")

    ## get regional info
    isos.Lr <- list()
    if(use.SDG.regions) {
        country.info <- country.info[country.info$iso.country %in% unique(iso.j),]
        isos.Lr[["All"]] = paste(country.info$iso.country)
        for(x in unique(country.info$groupname)) {
            isos.Lr[[x]] <- paste(country.info$iso.country[country.info$groupname == x])
        }
    } else {
        isos.Lr[["All"]] = paste(country.info$iso.c)
        isos.Lr[["Developed"]] = paste(country.info$iso.c[country.info$dev.c=="Rich"])
        isos.Lr[["Africa"]] = paste(country.info$iso.c[country.info$dev.c!="Rich"&
                                                       country.info$namereg.c=="Africa"])
        isos.Lr[["Asia"]] =paste(country.info$iso.c[country.info$dev.c!="Rich"&
                                                    country.info$namereg.c=="Asia"])
        isos.Lr[["LAC"]] =paste(country.info$iso.c[country.info$dev.c!="Rich"&
                                                   country.info$namereg.c=="Latin America and the Caribbean"])
        isos.Lr[["Oceania"]] =paste(country.info$iso.c[country.info$dev.c!="Rich"&
                                                       country.info$namereg.c=="Oceania"])
    }
    regs <- names(isos.Lr)
    nc.r <- list() # number of countries in each region
    for (reg in regs){
        nc.r[[reg]] <- length(unique(isos.Lr[[reg]]))
    }

  include.zero = TRUE
  #res <- GetObsCounts(data.j = data.j, iso.j = iso.j)
  # could be that break down by region+period introduces zeroes
  #if (res[1]>0 ) include.zero = TRUE

    if(use.SDG.regions) {
        par(mfrow = c(1,1), mar = c(5,25,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    } else {
        par(mfrow = c(1,1), mar = c(5,10,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
    }
  plot(1, type = "n", ylim = c(0, -1 + length(regs) * (length(years) + 1)), bty = "n",
       xlim = c(as.numeric(!include.zero),nres+1), xaxt = "n", yaxt = "n",
       ylab = "", xlab = "Number of observations",
       main = "")
  axis(1, at = 0.5+seq(0,nres), labels = c(seq(0,nres-1), paste(nres, "+", sep = "")))
  # year is the cutoff moment (1990.0 is in second row)

  yearnow <- max(floor(data$years.j))+1 # note: 2011 should give 2012 as the end point
    labelsperiods <- character(length(years) - 1)
    labelsperiods[1] <- paste0("Before ", years[2])
    for(i in 2:(length(labelsperiods) - 1)) {
        labelsperiods[i] <- paste0(years[i], "-", years[i + 1] - 1)
    }
    labelsperiods[length(labelsperiods)] <- paste0(years[length(labelsperiods)], "-", yearnow)
  ymin <- ystart <- -1 + length(regs) * (length(years) + 1)
  namesregs <- paste(regs, " (", unlist(nc.r), ")", sep = "")
  for (reg in 1:length(regs)){
    axis(2, at = -1+ymin-seq(1,length(years)), labels = rep("",length(years)), las = 1, tick=TRUE)
    axis(2, at = -1.5+ymin-seq(1,length(years)-1), labels = labelsperiods, las = 1, tick=FALSE)
    #  axis(2, at = -1+ymin-seq(1,5), labels = c("", years[-1]), las = 1)
    axis(2, at = -1+ymin-0.5, labels = paste(namesregs[reg], "", sep = ""), las = 1)
    ymin <- ymin-2
    region <- regs[reg]
    iso.select <- isos.Lr[[region]]
    res <- GetObsCounts(data.j = data.j, iso.j = iso.j,
                        selected.isos = iso.select)
    InternalPlotPropsDataAvailability(col = 3,ymin = ymin, res = res, include.zero = include.zero)
    for (t in 2:length(years)){
      ymin <- ymin-1
      res <- GetObsCounts(data.j = data.j, iso.j = iso.j,
                          select.j = (data$years.j>=years[t-1] & data$years.j<years[t]),
                          selected.isos = iso.select)
      InternalPlotPropsDataAvailability(col = "lightblue",ymin = ymin, res = res, include.zero = include.zero)
    }
  }# end plot

  if (include.zero) segments(1, -1, 1, ystart-1, lwd = 5)
  ##value<< NULL. Plot appears in R-console.
  return(invisible())
}


## [MCW-2016-03-11-1]: CREATED THIS FUNCTION. Based on InternalFixRange()
InternalFixEndYear <- function(# Fix range character vector that got converted to dates in Excel
  ### Replaces month names with numbers
  x
  ) {
  x <- as.character(x)
  from <- c("Jun-05", "Jul-05")
  to <- c(2006, 2007)
  sub.table <- data.frame(from = from, to = to, stringsAsFactors = FALSE)
  for (i in 1:nrow(sub.table))
    x <- gsub(sub.table$from[i], sub.table$to[i], x)
    return(as.numeric(x))
    return(x)
}
###----------------------------------------------------------------------------------
##' @importFrom magrittr %>%
extractDenominators <- function(denominator_csv, in_union) {
  data <- read.csv(
    file = denominator_csv,
    header = TRUE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA")
  )

  verifyDenominators(data, in_union = in_union)

  if (!any(names(data) == "In.union")) {
    return(data)
  }

  data <-data %>% dplyr::filter(In.union == in_union) %>% dplyr::select(-In.union)

  # Remove empty columns
  data[, sapply(data, function(i) {
    !all(is.na(i))
  })]
}
###----------------------------------------------------------------------------------
verifyDenominators <- function(x, in_union) {
    if(is.data.frame(x)) {
        temp_denom <- x
        fname <- "denominator counts data frame"
    } else if(is.character(x) && file.exists(x)) {
        temp_denom <- extractDenominators(denominator_csv = x, in_union = in_union)
        return(TRUE)
    } else {
        stop("'x' must be a data frame or a valid file path.")
    }

    ## Check for required columns
    if(!all(c("ISO.code", "Country") %in% colnames(temp_denom)))
        stop("'", fname, "' must have columns 'ISO code' and 'Country'.")
    if(!("In.union" %in% colnames(temp_denom))) {
        temp_denom_ccount <- table(temp_denom$`ISO.code`)
        ccount_gt_1 <- names(temp_denom_ccount[temp_denom_ccount > 1])
        if(length(ccount_gt_1) > 0) {
            stop("Countries with ISO codes ", paste(ccount_gt_1, collapse = ", "), " occur in '",
                 fname,
                 "' more than once. \n\tIf the file is for married and unmarried, the column identifying marital status must be called 'In union' (case sensitive).")
        }
    } else {
        ## Check 'in union' has correct entries
        if(!(all(temp_denom$`In.union` %in% c(0, 1))))
            stop("'In union' in '",
                 fname,
                 "' must only contain '0' or '1'.")
        ccount_by_iu <- as.data.frame(table(temp_denom$`In.union`, temp_denom$`ISO.code`),
                                      stringsAsFactors = FALSE)
        ccount_gt_1 <- ccount_by_iu$Var2[ccount_by_iu$Freq > 1]
        if(length(ccount_gt_1 > 0))
            stop("Countries with ISO codes ",
                 paste(ccount_gt_1, collapse = ", "),
                 "occur more than twice in '",
                 fname, "'.")
    }

    ## Check that counts are all numeric
    year_cols <-
        colnames(temp_denom)[!(colnames(temp_denom) %in% c("Country.letter.code", "ISO.code", "Country", "In.union"))]
    bad_cols <- character(0)
    for(nm in year_cols) {
        if(any(is.character(temp_denom[,nm]))) bad_cols <- c(bad_cols, nm)
    }
    if(length(bad_cols > 0))
        stop("'", fname, "' has non-numeric entries in column(s) '",
             paste(bad_cols, collapse = ", "), "'. Note that the country names should be in a column called 'Country', country letter codes in a column called 'Country letter code', the ISO codes in a column called either 'ISO code' or 'LocID' and the marital group identifier in a column called 'In union'.")
    return(TRUE)
}










###----------------------------------------------------------------------------------
### The End.
