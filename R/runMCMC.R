#--------------------------------------------------------------------------
# F_runMCMC.R
# Leontine Alkema and Jin Rou New
#--------------------------------------------------------------------------
RunMCMC <- function(# Start MCMC sampling
### Start MCMC sampling for the CP model and save mcmc.meta and JAGS objects to \code{output.dir}.
                    run.name = "test", ##<< Run name, used to create a directory \code{output/run.name}
                    ## with JAGS output (after MCMC sampling), and estimates (in next steps).
                    N.ITER = ifelse(!do.country.specific.run, 80000, 40000), ##<< Number of iterations, NOT including burn-in.
                    N.STEPS = 4, ##<< For each N.ITER/N.STEPS iterations, the iterations will be saved.
                    N.THIN = ifelse(!do.country.specific.run, 30, 15), ##<< Thinning factor.
                    N.BURNIN = 20000, ##<< Burnin (excluded samples at start of chain).
                    ChainNums = seq(1,5), ##<< IDs of chains to run in series
                    ## (the IDs need to be numeric because they are used to set the seed).
                    do.country.specific.run = FALSE, ##<< Logical: execute a country-specific (as opposed to global) run? # change JR, 20131104
  do.country.specific.targets.run = FALSE, ##<< Logical: execute a country-specific (as opposed to global) run for targets? # change JR, 20150301
                    iso.select = NULL, ##<< (For country/subpopulation-specific run) Numeric or 3-character ISO country code for country/subpopulation to select
                    ## one country/subpopulation to run the model for, or if NULL, all countries/subpopulations in data are selected. Should be of length 1
                    ## if \code{do.country.specific.run} is \code{TRUE} # change JR, 20131104
                    iso.country.select = NULL, ##<< (For subpopulation-specific run) Numeric or 3-character ISO country code for country that subpopulation
                    ## belongs to. # change JR, 20140404
                    run.name.global = NULL, ##<< (For country-specific run) Run name of global run # Change JR, 20131104
                    data_global_file_path = NULL,
                    change.priors.to.zerolower = FALSE, ##<< Logical: Change priors?
                    ## FALSE: use gammas on kappa.c's, TRUE: use uniform priors.
                    include.AR = TRUE , ##<< Logical: include AR(1)'s for total, modern/total and unmet?
                    seed.MCMC = 999, ##<< seed for initializing MCMC, defaults to 1.
                    output.dir = NULL, ##<< Directory where mcmc meta and raw MCMC output will be stored
                    ##either an existing directory, or if NULL, directory \code{output/run.name} is created
                    ## in current working directory
                    data.csv = NULL, ##<< If \code{NULL}, contraceptive use data set included in package is used.
                    ## To use alternative data set, use \code{data.csv = .../dataCPmodel.csv}, where ... refers to the file path where file is located.
                    regioninfo.csv = NULL, ##<< If \code{NULL}, region info included in package is used.
                    ## To use alternative csv file, use \code{regioninfo.csv = .../Country-and-area-classification.csv}, where ... refers to the file path where file is located.
                    html.file = NULL,##<<If not NULL, summary results about the data set get written to this HTML file.
                    exclude.unmet.only = FALSE, ##<< Logical: do validation for unmet need only?  (see description on validation exercises below)
                    exclude.unmet.only.test.prop = 0.2, ## Proportion of obs used for test set
                    at.random = FALSE, ##<< Logical: do validation, leaving out obs at random?
                    at.random.min.c = 1, ## Min number of data points per country to ensure are left in training set
                    at.random.test.prop = 0.2, ## Proportion of obs used for test set
                    at.end = FALSE, ##<< Logical: do validation, leaving out obs at end?
                    at.end.not.1.obs.c = FALSE, ##<< Logical: keep all obs from countries w only one obs?
                    at.random.no.data = FALSE, ##<< Logical: [MCW-2017-02-14-1] :: Argument
                    ## 'at.random.no.data = FALSE' added to perform validation
                    ## for countries with no data.
                    at.random.no.data.strata = NULL,
                    at.random.no.data.test.prop = 0.2, ## Proportion of obs used for test set
                    leave.iso.out = FALSE,
                    leave.iso.out.iso.test = NULL,
                    year.cutoff = 2005,##<< Used only if \code{at.end} = \code{TRUE}):
                    ## All data with observation year after and IN year.cutoff is excluded.
                    seed.validation = 12345, ##<< For constructing training set in validation exercise (if applicable)
                    generate.new.set = TRUE, ##<<Logical: generate a new training set in validation exercise?
                    run.jags = TRUE, ##<< Logical: run JAGS?
                    run.on.server = TRUE, ##<< Logical: run on server (in parallel?)
                    disagg.RN.PMA = TRUE##<<[MCW-2016-03-24-1] dis-aggregate repeated national surveys and PMA?
                   ,uwra.z.priors = NULL #[MCW-2016-06-02-5] Added to allow different priors for
                                #z model. Set to an integer code and define in body of GetBusPriorSpecs().
                   ,write.model.fun = "WriteModel" #[MCW-2016-06-02-6] pass in the
                    ## WriteModel[suff]() function used. *MUST* be
                                #the name of the function as a character
                                #string.
                    ## [MCW-2016-06-14-7] Added to control priors for Omega variance
                    ## parameters. Set to an integer code and define in body of GetBusPriorSpecs().
                   ,uwra.Omega.priors = NULL
                    ## [MCW-2016-06-21-1] Added to control priors for kappa.^(c) variances. Set to
                    ## an integer code and define in body of GetBugsPriorSpecs().
                   ,uwra.kappa.c.priors = NULL
                    ##[MCW-2016-08-24-2] Added to control estimation for countries with no data
                  , include.c.no.data = FALSE
                    ## [MCW-2016-10-05-1] Sets the world-level prior means for timing parameters (Omega).
                   ,timing.world.priors = list(mean.TOneLevel = 1920, mean.Tworld = 1980)
                    ## [MCW-2017-06-05-2] :: Model the 'ever-married/all women' perturbation multiplier as < 1?
                    ## NB: for UWRA  this is the 'Sterilization Only' ("SO") multiplier.
                   ,EA.bias.negative = FALSE
                    ## [MCW-2017-06-05-3] :: Model the 'Husband/wives, both sexes' perturbation multiplier as > 1?
                    ## NB: For UWRA this is the 'With Partner' ("WP") multiplier
                   ,HW.bias.negative = FALSE
                    ## Marital group
                   ,marital.group = "MWRA"
                    ## Age group
                    ,age.group = "15-49"
                    ## Use 'sink' to write to logfile.txt?
                    ,sink.seed.logfile = TRUE
  , verbose = TRUE
                    ){

    ## -------* SET-UP

    ## -------** Check And Re-Set Arguments

    ## Check iterations, burn-in, n.steps
    N.ITER <- round(N.ITER)
    N.BURNIN <- round(N.BURNIN)
    N.STEPS <- round(N.STEPS)

    if(any(N.ITER <= 0, N.BURNIN <= 0, N.STEPS <= 0)) {
        stop("'N.ITER', 'N.BURNIN', and 'N.STEPS' must be positive integers. In addition, 'N.STEPS' must be a divisor of ('N.ITER').")
    }

    ## N.STEPS must be a divisor of (N.ITER)
    if((N.ITER) %% N.STEPS > 0) {
        warning("'N.STEPS' is not a divisor of 'N.ITER'. 'N.ITER' has been reset to '", ceiling(N.ITER / N.STEPS) * N.STEPS, "'.")
        N.ITER <- ceiling(N.ITER / N.STEPS) * N.STEPS
        }

    ##details<< All output is written to folder output.dir (you'll get a message
    ## if it already exists).  JAGS objects are written to
    ## output.dir/temp.JAGSobjects.
    if (is.null(output.dir)){
        dir.create(file.path(getwd(), "output"), showWarnings = FALSE)
        output.dir <-  file.path(getwd(), "output", run.name) # change JR, 20140414
    }
    if (file.exists(file.path(output.dir, "mcmc.meta.rda"))){
        cat(paste("The output directory", output.dir, "already contains an MCMC.meta (and probably an older MCMC run).\n"))
        cat(paste("Delete files in this directory or choose a different run.name/output directory,\n"))
        return(invisible())
    }
    dir.create(output.dir, showWarnings = FALSE)
    if (run.jags) # change JR, 20131105
        dir.create(file.path(output.dir, "temp.JAGSobjects"))#, showWarnings = FALSE)

    if(sink.seed.logfile) {
    filename <- file.path(output.dir, "logfile.txt") # change JR, 20140418
    cat(paste("Seed and data files used are written to logfile ", filename), "\n")
    fileout <- file(filename, open = "wt")
                                # sink(fileout, split = T)
    }

    ##if (is.null(seed.MCMC)) seed.MCMC <- as.numeric(Sys.Date())
    ## Note about Sys.Date, to reproduce it, use
    ## as.numeric(as.Date("2012-03-09"))
    message("\nRandom seed passed to JAGS is '", seed.MCMC, "'. The random seed for each chain is '", seed.MCMC, "' multiplied by the chain number.")

      #######Argument for country specific RUNS!!!!
  ##Matching characters in the datsets to make sure ISO codes are correct
  if (!is.null(iso.select)) { # change JR, 20131104
    if (!(all(grepl("[[:alpha:]]", iso.select)) | all(grepl("[[:digit:]]", iso.select)))) {
      stop("iso.select: Must be NULL, else numeric or character ISO country code.")
      return(invisible())
    }
  }

  if (!is.null(iso.country.select)) { # change JR, 20140404
    if (!(all(grepl("[[:alpha:]]", iso.country.select)) | all(grepl("[[:digit:]]", iso.country.select)))) {
      stop("iso.country.select: Must be NULL, else numeric or character ISO country code.")
      return(invisible())
    }
  }

  ##details<< Object \code{data.global} is loaded or created, which is NULL if this run is not country-specific.
    if (do.country.specific.run || do.country.specific.targets.run) { # change JR, 20150301
        message("\n'do.country.specific.run' or 'do.country.specific.targets.run' is TRUE.")
        if (is.null(run.name.global)) {
            run.name.global <- "Run20140520" # change JR, 2010612
            if (!file.exists(file.path("data/data.global.rda"))) {
                cat(paste0("Error: No default data.global file in data folder. Run global run first or specify run.name.global!\n"))
                return(invisible())
            } else {
                load(file.path("data/data.global.rda"))
                cat(paste0("Default global run loaded.\n"))
            }
            if (do.country.specific.targets.run) { # change JR, 20150301
                if (!file.exists(file.path("data/data.logratios.rda"))) {
                    cat(paste0("Error: No default data.logratios file in data folder. Run global run first or specify run.name.global!\n"))
                    return(invisible())
                } else {
                    load(file.path("data/data.logratios.rda"))
                    cat(paste0("Default data.logratios loaded.\n"))
                }
            }
        } else {
            if(is.null(data_global_file_path)) {
                data_global_file_path <- file.path("output", run.name.global, "data.global.rda")
            }

            cat("data.global.rda path:", data_global_file_path, "\n", file = stderr())

            if (!file.exists(data_global_file_path))
                SummariseGlobalRun(run.name = run.name.global, write.model.fun = write.model.fun)
            load(file.path("output", run.name.global, "data.global.rda"))

            cat(paste0("Global run ", run.name.global, " loaded.\n"))
            if (do.country.specific.targets.run) { # change JR, 20150301
                load(file.path("output", run.name.global, "data.logratios.rda"))
                cat(paste0("Log ratio info list of ", run.name.global, " loaded.\n"))
            } else {
                data.logratios <- NULL # change JR, 20150301
            }
        }
    } else {
        data.global <- NULL
        data.logratios <- NULL # change JR, 20150301
    }

    if (!is.null(iso.country.select)) { # change JR, 20140404
        if (!(all(grepl("[[:alpha:]]", iso.country.select)) | all(grepl("[[:digit:]]", iso.country.select)))) {
            error("iso.country.select: Must be NULL, else numeric or character ISO country code.")
            return(invisible())
        }
    }

    ## Defaults for arguments
    if(is.null(disagg.RN.PMA)) disagg.RN.PMA <- TRUE
    if(is.null(timing.world.priors)) timing.world.priors <- list(mean.TOneLevel = 1920, mean.Tworld = 1980)
    if(is.null(include.AR)) include.AR <- TRUE
    if(is.null(at.end)) at.end <- FALSE
    if(is.null(at.random)) at.rand <- FALSE
    if(is.null(at.random.no.data)) at.rand.no.data <- FALSE
    if(is.null(year.cutoff)) year.cutoff <- 2005
    if(is.null(include.c.no.data)) include.c.no.data <- FALSE
    if(is.null(leave.iso.out)) leave.iso.out <- FALSE

    ## Analyze 'write.model.fun'.
    if(verbose) message("\nUsing the function ", write.model.fun, " to write the BUGS model") #[MCW-2016-06-02-12]
    ModelFunctionCheck(write.model.fun)

    ## Checks for one country model runs.
    if(do.country.specific.run || do.country.specific.targets.run){
        if(!ModelFunctionOneCountry(write.model.fun)) {
            stop("A one country run has been requested. 'write.model.fun' is ", write.model.fun, " but this function is not capable of doing a one country run.")
            }
        if(include.c.no.data) {
            include.c.no.data <- FALSE
            warning("'include.c.no.data' was 'TRUE' but country specific or country specific targets run has been requested; this is not possible! 'include.c.no.data has been re-set to 'FALSE'.")
        }
        if(exclude.unmet.only || at.end || at.random || at.random.no.data || leave.iso.out) {
            stop("A validation was requested but these are not implemented for one country runs.")
        }
        }

    ## If 'include.c.no.data' but model doesn't estimate countries with no data, stop.
    if(include.c.no.data && !ModelFunctionInclNoData(write.model.fun)) stop("'include.c.no.data' is 'TRUE' but ", write.model.fun, " does not produce estimates for countries with no data.")

    ## If validation run, don't estimate for countries with no data.
    if((exclude.unmet.only || at.end || at.random || at.random.no.data || leave.iso.out) &&
       include.c.no.data) {
        include.c.no.data <- FALSE
        warning("Estimates for countries with no data not produced in validation runs")
    }
    ## If validation is 'at.random.no.data' need a model function that can
    ## estimate for countries with no data.
    if(at.random.no.data) {
        if(!ModelFunctionInclNoData(write.model.fun)) {
            stop("'at.random.no.data == TRUE': 'write.model.fun' must be capable of producing estimates for countries with no data even though 'include.c.no.data == FALSE'. This is because countries randomly selected to be in the test set are treated like countries with no data for the purposes of estimation. Choose a different 'write.model.fun'.")
        }
    }

    if(leave.iso.out) {
        if(!ModelFunctionLeaveISOOut(write.model.fun)) {
            stop("'leave.iso.out == TRUE': 'write.model.fun' is not compatible with this validation exercise. Choose a different 'write.model.fun'.")
            }}

    ## If 'EA' or 'HW' are to be modelled as negative biases, 'write.model.fun' must be able to do it.
    if(EA.bias.negative || HW.bias.negative) {
        if(!ModelFunctionSignedMultipliersEAHW(write.model.fun))
            stop(paste0("'write.model.fun' is NOT capable of modelling 'EA' or 'HW' biases as negative biases. Use one of these model functions:\n"
                       ,paste(ModelFunctionSignedMultipliersEAHW("", return.functions = TRUE), collapse = ", ")
                        ))
    }

    ## 'marital.group' must be MWRA or UWRA
    if(!isTRUE(marital.group %in% c("MWRA", "UWRA"))) stop("'marital.group' must be \"MWRA\" or \"UWRA\"")

    ## 'age.group' must be valid
    if(!isTRUE(age.group %in% c("15-49", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))) stop("'age.group' must be in 'c(\"15-49\", \"15-19\", \"20-24\", \"25-29\", \"30-34\", \"35-39\", \"40-44\", \"45-49\")'")

    ## -------** Constants

    ChainNums <- unique(ChainNums)

    ## -------* MAIN

    ##details<< Object \code{validation.list} is created, which is NULL if this run is not a validation exercise.
    ## If several options in validation exercise were set to TRUE (in arguments of this function),
    ## observations are left out at random (first choice),
    ## or left out at the end (second choice).
    if(isTRUE(sum(exclude.unmet.only, at.end, at.random, at.random.no.data, leave.iso.out) > 1)) {
        warning("More than one validation exercise requested but can only do one. Priority order is:\n1) 'at.random', 2) 'at.end', 3) 'exclude.unmet.only', 4) 'at.random.no.data', 5) 'leave.iso.out'")
        if(exclude.unmet.only) {
            at.end <- at.random <- at.random.no.data <- leave.iso.out <- FALSE
        } else if(at.end) {
            at.random <- at.random.no.data <- leave.iso.out <- FALSE
        } else if(at.random) {
            at.random.no.data <- leave.iso.out <- FALSE
        } else if(at.random.no.data) {
            leave.iso.out <- FALSE
        }
    }

    if (exclude.unmet.only || at.end || at.random || at.random.no.data || leave.iso.out) {
        do.validation <- TRUE
        ## Fix strata names.
        if(at.random.no.data && !is.null(at.random.no.data.strata)) {
            data.col.names <- InternalRegExpsInputCols()
            for(i in 1:length(data.col.names$regex)) {
                at.random.no.data.strata[grepl(pattern = data.col.names$regex[i]
                                              ,x = at.random.no.data.strata
                                               )] <- data.col.names$df.names[i]
            }
        }
        if(leave.iso.out) {
            if(is.null(leave.iso.out.iso.test)) stop("Must specify 'leave.iso.out.iso.test'.")
            if(!(is.character(leave.iso.out.iso.test) || is.numeric(leave.iso.out.iso.test)))
                stop("'leave.iso.out.iso.test' must be a character or numeric vector of ISO codes.")
        }
        validation.list <- list(do.validation = do.validation,# ##<< TRUE
                                exclude.unmet.only = exclude.unmet.only,# ##<< From arguments
                                exclude.unmet.only.test.prop = exclude.unmet.only.test.prop,
                                at.random = at.random,###<< From arguments
                                at.random.test.prop = at.random.test.prop,###<< From arguments
                                at.end = at.end,#  ##<< From arguments
                                year.cutoff = year.cutoff,# ##<< From arguments
                                at.random.no.data = at.random.no.data,
                                at.random.no.data.strata = at.random.no.data.strata,
                                at.random.no.data.test.prop = at.random.no.data.test.prop,
                                leave.iso.out = leave.iso.out,
                                leave.iso.out.iso.test = leave.iso.out.iso.test,
                                seed = seed.validation,# ##<< From arguments
                                generate.new.set = generate.new.set # ##<< From arguments
                                )
        ##end<<
    } else {
        validation.list <- NULL
    }

    ## -------** Read Data

    file.copy(data.csv, file.path(output.dir, "dataCPmodel_input_raw.csv"))
    data_csv_input_raw <- read.csv(file.path(output.dir, "dataCPmodel_input_raw.csv"),
                                   stringsAsFactors = FALSE)
    ## ------------------------------------------------------------
    ## Check raw data
    sapply(c("Start.year", "End.year"), CheckDataMissing,
           data_frame = data_csv_input_raw,
           data_frame_name = "dataCPmodel_input_raw.csv")
    sapply(c("Contraceptive.use.MODERN",
             "Contraceptive.use.TRADITIONAL",
             "Unmet"), CheckDataRange,
           data_frame = data_csv_input_raw,
           data_frame_name = "dataCPmodel_input_raw.csv",
           range = c(0, 100))
    ## ------------------------------------------------------------

    if (is.null(iso.country.select)) { # change JR, 20140409
        name.country.select <- NULL
    } else {
        name.country.select <- names(data.global$iso.c)[match(iso.country.select, data.global$iso.c)]
        ## [MCW-2016-08-25-2] Estimation for countries with missing data not implemented if iso.country.select non-null.
        if(isTRUE(include.c.no.data)) {
            include.c.no.data <- FALSE
            warning("'include.c.no.data' set to FALSE. Not implemented if 'iso.country.select' non-null")
        }
    }
    data.preprocessed <- PreprocessData(data.csv = data.csv,
                                        iso.select = iso.select,
                                        write.model.fun = write.model.fun,
                                        marital.group = marital.group)
    select.ss <- grepl("Service statistic", data.preprocessed$Data.series.type)
    if (!do.country.specific.run & any(select.ss))
        stop("SS data should not be used for global run!")
    do.SS.run <- do.country.specific.run & any(select.ss)
    if (do.SS.run & sum(select.ss) == 1)
        stop("Only 1 observation of SS data is available. Not possible to use SS data for projection.")
    if (do.SS.run) {
        years.all <- (data.preprocessed$Start.year+data.preprocessed$End.year)/2
        max.survey.year <- max(years.all[!select.ss])
        diff.years.ss <- years.all[select.ss] - max.survey.year
        if (all(diff.years.ss <= 0))
            stop("Service statistics data do not extend beyond survey data.")
        if (length(diff.years.ss >= 0) == 1)
            stop("Only 1 observation of service statistics data at/beyond the latest survey data year is available. More SS data required!")
    }
    ## for run with SS data, check if this is the first/second pass run
    do.SS.run.first.pass <- do.SS.run & !file.exists(file.path(output.dir, "res.country_pre.rda"))
    do.SS.run.second.pass <- do.SS.run & file.exists(file.path(output.dir, "res.country_pre.rda"))

    if (do.SS.run.second.pass) {
        cat("Starting the second pass run for model with service statistics data...\n")
        load(file.path(output.dir, "res.country_pre.rda"))
        ## get SS data point, either the closest SS obs prior to/in most recent
        ## survey year if available, else the closest one after that year
        if (any(diff.years.ss <= 0)) { # if SS data overlaps with non-SS data
            year.ss <- max(years.all[select.ss & years.all <= max.survey.year])
        } else {
            year.ss <- min(years.all[select.ss])
        }
        modern.CP.ss <- data.preprocessed$Contraceptive.use.MODERN[select.ss &
                                                                   years.all == year.ss]/100
        cat(paste0("The most recent observation year of non-SS data is ", max.survey.year, ".\n"))
        cat(paste0("The service statistics data observation in ", year.ss, " is used to calculate the relative SS bias.\n"))
        ## get bias.modern
        modern.CP.est <- res.country$CIprop.Lg.Lcat.qt[[1]][["Modern"]]["0.5", ]
        years.est <- as.numeric(names(modern.CP.est))
        modern.CP.ssyear <- approx(x = years.est, y = modern.CP.est, xout = year.ss)$y
        bias.modern <- (modern.CP.ss*modern.CP.ssyear - modern.CP.ss)/(modern.CP.ss*modern.CP.ssyear - modern.CP.ssyear)
        cat(paste0("The relative service statistics bias is ", bias.modern, ".\n"))
        data.SS <- list(bias.modern = bias.modern)
        rm(res.country)
    } else {
        if (do.SS.run.first.pass)
            cat("Starting the first pass run for model with service statistics data...\n")
        data.SS <- NULL
    }

    filename.append <- ifelse(do.SS.run.first.pass, "_pre", "")
    ## save a copy of preprocessed data file for Shiny

    write.csv(data.preprocessed, file = file.path(output.dir, paste0("dataCPmodel_input_preprocessed", filename.append, ".csv")), row.names = F)
    cat(paste0("Pre-processed data saved to ", file.path(output.dir, paste0("dataCPmodel_input_preprocessed", filename.append, ".csv")), "\n"))

    ## ------------------------------------------------------------
    ## Check preprocessed data
    sapply(c("Start.year", "End.year"), CheckDataMissing,
           data_frame = data.preprocessed,
           data_frame_name = "dataCPmodel_input_preprocessed")
    sapply(c("Contraceptive.use.MODERN",
             "Contraceptive.use.TRADITIONAL",
             "Unmet"), CheckDataRange,
           data_frame = data.preprocessed,
           data_frame_name = "dataCPmodel_input_preprocessed.csv",
           range = c(0, 100))
    ## ------------------------------------------------------------

    data.raw <- ReadDataAll(data.preprocessed = data.preprocessed,
                            regioninfo.csv = regioninfo.csv,
                            output.dir = output.dir,
                            iso.select = iso.select,
                            iso.country.select = iso.country.select,
                            name.country.select = name.country.select,
                            do.SS.run.first.pass = do.SS.run.first.pass,
                            write.model.fun = write.model.fun,
                            html.file = html.file,
                            disagg.RN.PMA = disagg.RN.PMA #[MCW-2016-03-24-2] added
                           ,include.c.no.data = include.c.no.data #[MCW-2016-09-09-3] :: Add
                           ,validation.list = validation.list
                           ,at.random.min.c = at.random.min.c
                           ,at.end.not.1.obs.c = at.end.not.1.obs.c
                           ,data.global = data.global
                            ,do.country.specific.run = do.country.specific.run
                            )

    write.csv(data.raw$data, file = file.path(output.dir, paste0("dataCPmodel_input_to_model", filename.append, ".csv")), row.names = F)
    cat(paste0("Model fitted to data saved to ", file.path(output.dir, paste0("dataCPmodel_input_to_model", filename.append, ".csv")), "\n"))

    if(ModelFunctionRateModel(write.model.fun)) {
        winbugs.data <- GetBugsData_Rate(data = data.raw$data,
                              country.info = data.raw$country.info,
                              data.global = data.global,
                              data.SS = data.SS, # change JR, 20140414
                              data.logratios = data.logratios, # change JR, 20150301
                              output.dir = output.dir,
                              validation.list  = validation.list,
                              do.country.specific.run = do.country.specific.run,
                              do.SS.run.second.pass = do.SS.run.second.pass, # change JR, 20140414
                              do.country.specific.targets.run = do.country.specific.targets.run, # change JR, 20150301
                              change.priors.to.zerolower = change.priors.to.zerolower
                              ## # Added by Mark w
                               ,country.info.no.data = data.raw$country.info.no.data
                              ,disagg.RN.PMA = disagg.RN.PMA #[MCW-2016-03-24-5]
                                ##[MCW-2016-06-02-9] added next two lines to pass
                                ##through values of new z priors and write model
                                ##script.
                               ,uwra.z.priors = uwra.z.priors
                               ,write.model.fun = write.model.fun
                                ##[MCW-2016-06-14-6] Added to pass this argument through.
                               ,uwra.Omega.priors = uwra.Omega.priors
                                ##[MCW-2016-06-21-2] Added to pass this argument through.
                               ,uwra.kappa.c.priors = uwra.kappa.c.priors
                                ## [MCW-2016-08-24-3] Pass this through
                              , include.c.no.data = include.c.no.data
                                ,timing.world.priors = timing.world.priors
                               ,getj.training.k = data.raw$getj.training.k
                               ,validation.at.random.no.data = data.raw$validation.at.random.no.data
                              ,verbose = verbose
                              )
        } else {
    winbugs.data <- GetBugsData(data = data.raw$data,
                                country.info = data.raw$country.info,
                                ## [MCW-2016-08-23-3] Pass this through
                                country.info.no.data = data.raw$country.info.no.data,
                                data.global = data.global,
                                data.SS = data.SS, # change JR, 20140414
                                output.dir = output.dir,
                                validation.list  = validation.list,
                                do.country.specific.run = do.country.specific.run,
                                do.country.specific.targets.run = do.country.specific.targets.run, # change JR, 20150301
                                do.SS.run.second.pass = do.SS.run.second.pass, # change JR, 20140414
                                change.priors.to.zerolower = change.priors.to.zerolower,
                                disagg.RN.PMA = disagg.RN.PMA #[MCW-2016-03-24-5]
                                ##[MCW-2016-06-02-9] added next two lines to pass
                                ##through values of new z priors and write model
                                ##script.
                               ,uwra.z.priors = uwra.z.priors
                               ,write.model.fun = write.model.fun
                                ##[MCW-2016-06-14-6] Added to pass this argument through.
                               ,uwra.Omega.priors = uwra.Omega.priors
                                ##[MCW-2016-06-21-2] Added to pass this argument through.
                               ,uwra.kappa.c.priors = uwra.kappa.c.priors
                                ## [MCW-2016-08-24-3] Pass this through
                              , include.c.no.data = include.c.no.data
                                ,timing.world.priors = timing.world.priors
                               ,getj.training.k = data.raw$getj.training.k
                               ,validation.at.random.no.data = data.raw$validation.at.random.no.data
                              ,verbose = verbose)
        }

    ## save names of V parameters separately
    ## this function is used to find out which parameter was assigned to which country
    ## (through indices of observations)
    if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        name.short.j <-
            InternalMakeCountryNamesShort(c(data.raw$data$name.j
                                           ,data.raw$validation.at.random.no.data$data.test$name.j
                                            )
                                          )
    } else name.short.j <- InternalMakeCountryNamesShort(data.raw$data$name.j)

    if (!do.country.specific.targets.run) {
        par.V <- InternalGetParnamesV(winbugs.data = winbugs.data,
                                      name.short.j = name.short.j,
                                      marital.group = marital.group)
    } else par.V <- NULL

    parnames.list <- GetParNames(winbugs.data = winbugs.data,
                                 validation.list  = validation.list,
                                 do.country.specific.run = do.country.specific.run
                                ,include.c.no.data = include.c.no.data #[MCW-2016-08-25-8] Added to pass this through
                                ,write.model.fun = write.model.fun
                                 ,disagg.RN.PMA = disagg.RN.PMA
                               ,do.country.specific.targets.run = do.country.specific.targets.run # change JR, 20150301
                                 ) # change JR, 20131104

   ##details<<
    ##describe<< Object mcmc.meta is saved, which is a list with
    mcmc.meta <- list(
        general = ##<< General info:
            ##describe<<
            list(N.ITER = N.ITER,##<< From arguments
                 ChainNums = ChainNums, ##<< ##<< From arguments (updated later if chains are added)
                 N.STEPS = N.STEPS, ##<< From arguments
                 N.THIN = N.THIN, ##<< From arguments
                 N.BURNIN = N.BURNIN, ##<< From arguments
                 seed.MCMC = seed.MCMC, ##<< From arguments
                 do.country.specific.run = do.country.specific.run, ##<< From arguments # change JR, 20131104
                 iso.select = iso.select, ##<< For country specific runs, which country?
                 do.SS.run.first.pass = do.SS.run.first.pass, # change JR, 20140414
                 run.name.global = run.name.global, ##<< From arguments # change JR, 20131104
                 change.priors.to.zerolower = change.priors.to.zerolower, ##<< From arguments
                 output.dir = output.dir##<< From arguments
                ,disagg.RN.PMA = disagg.RN.PMA ## [MCW-2016-04-04-5] Added so that it can be used in WriteModel()
                 ## [MCW-2016-08-25-1] Add this to indicate if estimates for countries with no data are to be produced.
                ,include.c.no.data = include.c.no.data
                 ,write.model.fun = write.model.fun
                ,EA.bias.negative = EA.bias.negative
                ,HW.bias.negative = HW.bias.negative
                ,marital.group = marital.group
                 ,age.group = age.group
           ,do.country.specific.targets.run = do.country.specific.targets.run # change JR, 20150301
                 ),
        ##end<<
        parnames.list = parnames.list, ##<< Output from \code{GetParNames}, list with all parnames in BUGS
        par.V = par.V, ##<< Output from \code{InternalInternalGetParnamesV}, BUGS and ``nice'' parameter names for data multipliers
        validation.list = validation.list, ##<< Details about the validation exercise
        include.AR = include.AR,##<< From arguments
        winbugs.data = winbugs.data, ##<< Object from \code{\link{GetBugsData}}
        data.raw = data.raw, ##<< Object from \code{\link{ReadDataAll}}
        data.global = data.global, ##<< Object from \code{\link{SummariseGlobalRun}} # change JR, 20131104
        data.SS = data.SS ##<< Object summarising information from first pass of run with SS data # change JR, 20140414
    )
    ##end<<
    save(mcmc.meta, file = file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda"))) # change JR, 20140414
    ##details<< See \code{\link{AddMCMCChain}} for adding an additional chain (with same number of iterations etc).
                                #Details<< BUGS model is stored in \code{output.dir} using \code{WriteModel} or \code{WriteCountryModel}
### if do.country.specific.run is \code{TRUE}.

    ## [MCW-2018-01-17] JAGS model function is supplied to RunMCMC() by name so just accept it.
    ## if (!do.country.specific.run) {
    ##     do.call(write.model.fun, args = list(mcmc.meta = mcmc.meta)) #[MCW-2016-06-02-13] Use the function passed in through arguments.
    ## } else if (do.country.specific.run & do.country.specific.targets.run) { # change JR, 20150301
    ##     WriteCountryModelForTargets(mcmc.meta = mcmc.meta)
    ## } else {
    ##     WriteCountryModel(mcmc.meta = mcmc.meta)
    ## }
    ##file.show(file.path(output.dir, "model.txt"))
    do.call(write.model.fun, args = list(mcmc.meta = mcmc.meta))

    # sink()
    # closeAllConnections()

    ## -------** Run MCMC

    if (run.jags) {
        if (run.on.server) {
            foreach::foreach(chainNum=ChainNums, .packages = "FPEMglobal") %dopar% {
                cat(paste("Start chain ID ", chainNum), "\n")
                ##tryCatch(
                InternalRunOneChain(chainNum = chainNum, mcmc.meta = mcmc.meta
                                    ##[MCW-2016-06-02-17] Pass this through
                                   ,write.model.fun = write.model.fun)
                ##                   , warning=function(w, output.dir = mcmc.meta$general$output.dir){
                ##                   file.w = file.path(output.dir, "warnings.txt");
                ##                   cat(paste(w), file=file.w, append=T); return()},##;  close(file.w)},
                ##                   error=function(e){##},  output.dir = mcmc.meta$general$output.dir){
                ##                   file.e=file.path(output.dir, "errors.rda");
                ##                   save(e, file = "file.e"); ## does not give anything....
                ##                   return()},
                ##                   ##cat(paste(unlist(e)), file = file.e,append=T); return()},##; close(file.e)},
                ##                   ##sink(file=file.path(mcmc.meta$general$output.dir, "errors.txt"),append=T);print("Errors:");print(e);sink();return(NULL)},
                ##                   finally=function(j){ return()}) ## does not do anything either...
            } ## end chainNums
        } else {
            for (chainNum in ChainNums){
                cat(paste("Start chain ID ", chainNum), "\n")
                                #tryCatch(
                InternalRunOneChain(chainNum = chainNum, mcmc.meta = mcmc.meta
                                   ,write.model.fun = write.model.fun #[MCW-2016-06-02-17] Pass this through.
                                    )
                ##                   , warning=function(w, output.dir = mcmc.meta$general$output.dir){
                ##                   file.w = file.path(output.dir, "warnings.txt");
                ##                   cat(paste(w), file=file.w, append=T); return()},##;  close(file.w)},
                ##                   error=function(e){##},  output.dir = mcmc.meta$general$output.dir){
                ##                   file.e=file.path(output.dir, "errors.rda");
                ##                   save(e, file = "file.e"); ## does not give anything....
                ##                   return()},
                ##                   ##cat(paste(unlist(e)), file = file.e,append=T); return()},##; close(file.e)},
                ##                   ##sink(file=file.path(mcmc.meta$general$output.dir, "errors.txt"),append=T);print("Errors:");print(e);sink();return(NULL)},
                ##                   finally=function(j){ return()}) ## does not do anything either...
            }
        }
        ##cat("Any JAGS related errors/warnings are written to files in output.dir, but can usually be ignored.. (if there is something wrong, you'll find out soon enough in the next steps :))", "\n")
        cat("All chains have finished!\n")
    }
    ##value<< NULL (mcmc.meta is saved to output.dir, and JAGS objects are saved in their own directory).
}## end function

#-----------------------------------------------------
InternalRunOneChain <- function(#Do MCMC sampling
  ###Do MCMC sampling for one chain
  #, use with tryCatch function to avoid zillion errors from JAGS...
  chainNum, ##<< Chain ID
  mcmc.meta ##<< List, described in \code{\link{RunMCMC}}
 ,write.model.fun = "WriteModel"       #[MCW-2016-06-02-16] Pass this argument through.
  ){
  # set seed before sampling the initial values
  set.seed.chain <- chainNum*mcmc.meta$general$seed.MCMC*
    min(as.numeric(mcmc.meta$data.raw$country.info$iso.c[1]), na.rm = T) # change JR, 20140617: added min # change JR, 20140310: added seed
  # in Jags version (July 21, 2012) jags.seed doesn't work, and inits need to be provided as a function
  # note that even with same Jags seed, as long as inits from R are different, any non-initialized pars will haev different starting values
  # and seed in Jags is consistent
    cat("Random seed for this chain is '", set.seed.chain, "'.\n")
    mcmc.info <- list(set.seed.chain = set.seed.chain, chainNum = chainNum)
  filename.append <- ifelse(mcmc.meta$general$do.SS.run.first.pass, "_pre", "") # change JR, 20140414
  mcmc.info.file <- file.path(mcmc.meta$general$output.dir, paste0("mcmc.info", filename.append, ".", chainNum, ".rda")) # change JR, 20140414 # change JR, 20140418
  if (file.exists(mcmc.info.file)){
    cat(paste("The output directory", mcmc.meta$general$output.dir, "already contains info on chain", chainNum))
    cat(paste("No new samples are added"))
    return(invisible())
  }
  save(mcmc.info, file = mcmc.info.file)
  cat("JAGS is called to obtain posterior samples, and there will be some info about the model and steps written to file.", "\n")
  #  cat("And lots of error messages (potentially), which I do not know how to get rid off", "\n")
  cat("Just wait for statement that MCMC run has finished", "\n")
  jags.dir <- file.path(mcmc.meta$general$output.dir, "temp.JAGSobjects/")
  #   mod<-tryCatch(R2jags::jags(data=mcmc.meta$winbugs.data,
  #               inits=fix.init,
  #               parameters.to.save=unlist(mcmc.meta$parnames.list),
  #               model.file=file.path(mcmc.meta$general$output.dir, "model.txt"),
  #               n.chains=1,
  #               n.iter=mcmc.meta$general$N.BURNIN+mcmc.meta$general$N.ITER/mcmc.meta$general$N.STEPS,
  #               n.burnin=mcmc.meta$general$N.BURNIN,
  #               n.thin=mcmc.meta$general$N.THIN,
  #               DIC=FALSE,
  #               working.directory=mcmc.meta$general$output.dir),
  #
  #               warning=function(w, output.dir = mcmc.meta$general$output.dir){
  #                 file.w = file.path(output.dir, "warnings.txt");
  #                 cat(paste(unlist(w)), file=file.w, append=T); return()},#;  close(file.w)},
  #               error=function(e){#},  output.dir = mcmc.meta$general$output.dir){
  #                 #file.e=file.path(output.dir, "errors.rda");
  #                 save(e, file = "bla.rda");#file.e);
  #                 return()},#; close(file.e)},
  #                 #cat(paste(unlist(e)), file = file.e,append=T); return()},#; close(file.e)},
  #                 #sink(file=file.path(mcmc.meta$general$output.dir, "errors.txt"),append=T);print("Errors:");print(e);sink();return(NULL)},
  #               finally=function(mod.upd, i.temp =1, chainNum.temp = chainNum, jags.dir.temp = jags.dir){
  #                 save(mod.upd,file=paste(jags.dir.temp, "/jags_mod", chainNum.temp, "update_", i.temp, ".Rdata", sep = ""));
  #                 return(mod.upd)} ) # return doesn't help much...
  set.seed(set.seed.chain) # note: seed only useful if inits function didn't change!
  # need to sample something in R first
    temp <- rnorm(1)

  mod<-R2jags::jags(data=mcmc.meta$winbugs.data,
            # inits=fix.init,
            # note this functionality does not work as of July 21, 2012
            # isntead: need a function without input arguments, that samples 1!
            inits= InternalMCMCinits(
              winbugs.data = mcmc.meta$winbugs.data,
              do.country.specific.run = mcmc.meta$general$do.country.specific.run, # change JR, 20131104
              do.country.specific.targets.run = mcmc.meta$general$do.country.specific.targets.run, # change JR, 20150301
              change.priors.to.zerolower = mcmc.meta$general$change.priors.to.zerolower,
              write.model.fun = write.model.fun #[MCW-2016-06-02-15] Pass this through.
             ,include.c.no.data = mcmc.meta$general$include.c.no.data #[MCW-2016-08-25-6] Pass this through.
              ,validation.list = mcmc.meta$validation.list
            ),
            parameters.to.save= unlist(mcmc.meta$parnames.list),
            model.file=paste0("model", filename.append, ".txt"), # change JR, 20140414
            n.chains=1,
            n.iter=mcmc.meta$general$N.BURNIN+mcmc.meta$general$N.ITER/mcmc.meta$general$N.STEPS,
            n.burnin=mcmc.meta$general$N.BURNIN,
            n.thin=mcmc.meta$general$N.THIN,
            DIC=FALSE,
            # jags.seed = set.seed.chain,
            # note this functionality does not work as of July 21, 2012
            jags.seed = set.seed.chain, # change JR, 20140617: changed from 123
            working.directory=mcmc.meta$general$output.dir)
  # in theory... to update in the future: load("jags_mod.Rdata");recompile(mod)
  # in practice.... that never worked for me!

  i = 1 # index for which update
  mod.upd <- mod
  save(mod.upd, file=file.path(mcmc.meta$general$output.dir, "temp.JAGSobjects", paste0("jags_mod", filename.append, chainNum, "update_", i, ".Rdata"))) # change JR, 20140414 # change JR, 20140418
  #load(file=file.path(mcmc.meta$general$output.dir, "temp.JAGSobjects", paste0("jags_mod", filename.append, chainNum, "update_", i, ".Rdata")) # change JR, 20140418
  cat(paste("MCMC results step", 1, " for chain ", chainNum, " written to folder temp.JAGSobjects in ", mcmc.meta$general$output.dir), "\n")

  #--- update MCMC ----------
  if (mcmc.meta$general$N.STEPS >1){
    for (i in 2:(mcmc.meta$general$N.STEPS)){
      mod.upd <-update(mod.upd, parameters.to.save=unlist(mcmc.meta$parnames.list), # change JR, 20131104
                       n.iter=mcmc.meta$general$N.ITER/mcmc.meta$general$N.STEPS,
                       n.thin=mcmc.meta$general$N.THIN)
      save(mod.upd, file = file.path(mcmc.meta$general$output.dir, "temp.JAGSobjects", paste0("jags_mod", filename.append, chainNum, "update_", i, ".Rdata"))) # change JR, 20140414 # change JR, 20140418
      #load(file = file.path(mcmc.meta$general$output.dir, "temp.JAGSobjects", paste0("jags_mod", filename.append, chainNum, "update_", i, ".Rdata"))) # change JR, 20140414 # change JR, 20140418
      cat(paste("MCMC results step", i, " for chain ", chainNum, " written to folder temp.JAGSobjects in ", mcmc.meta$general$output.dir), "\n")
    }
  }
  #cat("Ignore all errors above....")
  cat(paste("Hooraah, Chain", chainNum, "has finished!"), "\n")
  ##note<< Called from \code{\link{RunMCMC}} and \code{\link{AddMCMCChain}}.
  ## This function can give errors and warnings when JAGS output is read into R,
  ## no worries about that here, convergence will be checked later.
  ##value<< NULL
  return(invisible())
}
#----------------------------------------------------------------------------------
AddMCMCChain <- function(# Add additional MCMC chain to existing run.
  ### Add additional MCMC chain with same specs as chains from meta from \code{run.name}.
  run.name = "test", ##<< run name, usually Frunnumber, from run where chain(s) should be added
  ChainNums = 6, ##<< IDs of additional chains to add, mcmc.meta is updated with additional chain info.
  ## If one or more of the IDs were already there, a message will be printed and no chains will be added.
  do.SS.run.first.pass = FALSE, ##<< is this for the first pass run with SS data?
  output.dir = NULL ##<< Directory where mcmc meta of run name was stored.
  ##  If NULL, it's \code{output/run.name/} in the current working directory.
 ,MCMCInits = NULL
  ## [MCW-2016-11-01-8] :: Add argument 'write.model.fun'.
  ,write.model.fun = "WriteModel"
  ,run.on.server = TRUE ##<< Logical: run on server (in parallel?)
){
  ##details<< See \code{\link{RunMCMC}} for initial run.
  ## This function will crash if you specified a run for which mcmc.meta has not yet been constructed
  output.dir <- file.path(getwd(), "output", run.name) # change JR, 20140418
  filename.append <- ifelse(do.SS.run.first.pass, "_pre", "")
  load(file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda"))) # change JR, 20140418
  if (sum(is.element(ChainNums, mcmc.meta$general$ChainNums))>0){
    ChainNums <- setdiff(ChainNums, mcmc.meta$general$ChainNums)
    if (sum(ChainNums)==0){
      cat("MCMC run(s) for ChainNum(s) and run.name already exist(s)!", "\n")
      return(invisible())
    }
  }
  # add chain info to mcmc.meta

  mcmc.meta$general$ChainNums <- unique(c(mcmc.meta$general$ChainNums, ChainNums))
    save(mcmc.meta, file = file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda"))) # change JR, 20140418
    if(run.on.server) {
        foreach::foreach(chainNum=ChainNums, .packages = "FPEMglobal") %dopar% {
                cat(paste("Start chain ID ", chainNum), "\n")
    #tryCatch(
      InternalRunOneChain(chainNum = chainNum, mcmc.meta = mcmc.meta
                          ## [MCW-2016-11-01-7] :: Pass in value of 'write.model.fun'.
                         ,write.model.fun = write.model.fun
                          )
    #              , warning=function(w, output.dir = mcmc.meta$general$output.dir){
    #                file.w = file.path(output.dir, "warnings.txt");
    #                cat(paste(w), file=file.w, append=T); return()},#;  close(file.w)},
    #              error=function(e){#},  output.dir = mcmc.meta$general$output.dir){
    #                file.e=file.path(output.dir, "errors.rda");
    #                save(e, file = "file.e"); # does not give anything....
    #                return()},
    #              #cat(paste(unlist(e)), file = file.e,append=T); return()},#; close(file.e)},
    #              #sink(file=file.path(mcmc.meta$general$output.dir, "errors.txt"),append=T);print("Errors:");print(e);sink();return(NULL)},
    #              finally=function(j){ return()}) # does not do anything either...
  } # end chainNums
        } else {
  for (chainNum in ChainNums){
    #tryCatch(
      InternalRunOneChain(chainNum = chainNum, mcmc.meta = mcmc.meta
                          ## [MCW-2016-11-01-7] :: Pass in value of 'write.model.fun'.
                         ,write.model.fun = write.model.fun
                          )
    #              , warning=function(w, output.dir = mcmc.meta$general$output.dir){
    #                file.w = file.path(output.dir, "warnings.txt");
    #                cat(paste(w), file=file.w, append=T); return()},#;  close(file.w)},
    #              error=function(e){#},  output.dir = mcmc.meta$general$output.dir){
    #                file.e=file.path(output.dir, "errors.rda");
    #                save(e, file = "file.e"); # does not give anything....
    #                return()},
    #              #cat(paste(unlist(e)), file = file.e,append=T); return()},#; close(file.e)},
    #              #sink(file=file.path(mcmc.meta$general$output.dir, "errors.txt"),append=T);print("Errors:");print(e);sink();return(NULL)},
    #              finally=function(j){ return()}) # does not do anything either...
  } # end chainNums
  }
  cat("Any JAGS related errors/warnings are written to txt files in output.dir, but can usually be ignored.. (if there is something wrong, you'll find out soon enough in the next steps :))", "\n")
  cat("All additional chains have finished!")
  ##value<< NULL
}# end function
#----------------------------------------------------------------------------------
InternalMCMCinits <-
    function(# Initialize MCMC run in BUGS
### Initialize MCMC run in Bugs
             winbugs.data, ## Object of \code{\link{winbugs.data}}
             do.country.specific.run, ##<< Logical # change JR, 20131104
  do.country.specific.targets.run, ##<< Logical # change JR, 20150301
             change.priors.to.zerolower ##<< Logical
            ,write.model.fun = "WriteModel"       #[MCW-2016-06-02-14] Added to pass value of this argument through.
            ,include.c.no.data #[MCW-2016-08-25-6] Added to pass this through.
             ,validation.list = NULL
             )
{
    if(!ModelFunctionRateModel(write.model.fun)) {

        ## ####################################################################### ##
        ##                               LEVEL MODEL                               ##
        ## ####################################################################### ##

        list.varpar <- NULL

        ## [MCW-2016-08-25-6] Increase length of parameters if want estimates for countries with no data.
        if(include.c.no.data || isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
            inits.list1 <- list(
                T.c = c(msm::rtnorm(winbugs.data$C + winbugs.data$C.no.data, 1980, 80, lower=1800), NA),
                                # change JR, 2013119: changed from winbugs.data$C+1
                RT.c = c(msm::rtnorm(winbugs.data$C + winbugs.data$C.no.data, 1980, 80, lower=1800), NA),
                                # change JR, 2013119: changed from winbugs.data$C+1
                logitRomega.c = logit(runif(winbugs.data$C + winbugs.data$C.no.data, 0.02, 0.25)),
                logitomega.c = logit(runif(winbugs.data$C + winbugs.data$C.no.data, 0.02, 0.25)),
                logitRmax.c = LogitMinMax(runif(winbugs.data$C + winbugs.data$C.no.data, 0.55, 0.95), xmin = 0.5, xmax = 1),
                logitpmax.c = LogitMinMax(runif(winbugs.data$C + winbugs.data$C.no.data, 0.55, 0.95), xmin = 0.5, xmax = 1)
            )
        } else {
            inits.list1 <- list(
                T.c = c(msm::rtnorm(winbugs.data$C, 1980, 80, lower=1800), NA),
                                # change JR, 2013119: changed from winbugs.data$C+1
                RT.c = c(msm::rtnorm(winbugs.data$C, 1980, 80, lower=1800), NA),
                                # change JR, 2013119: changed from winbugs.data$C+1
                logitRomega.c = logit(runif(winbugs.data$C, 0.02, 0.25)),
                logitomega.c = logit(runif(winbugs.data$C, 0.02, 0.25)),
                logitRmax.c = LogitMinMax(runif(winbugs.data$C, 0.55, 0.95), xmin = 0.5, xmax = 1),
                logitpmax.c = LogitMinMax(runif(winbugs.data$C, 0.55, 0.95), xmin = 0.5, xmax = 1)
            )
        }

        if (!do.country.specific.run) {
            if (change.priors.to.zerolower){
                list.varpar <-
                    c(list.varpar
                     ,list(sigma.lpc = runif(1,0,5), sigma.lrc = runif(1,0,5),
                           sigma.wc = runif(1, 0,2), sigma.Rwc = runif(1, 0,2), # [MCW-2016-06-15-3] Was
                                # 'runif(10,2)' which contradicts
                                # definition in WriteModel().
                                # [MCW-2016-10-06-6] :: 'sigma.Rwc =
                                # runif(0,2)' changed to 'sigma.Rwc =
                                # runif(1, 0,2)'. Old version didn't
                                # make sense. Unknown if it had an
                                # impact on results.
                           sigma.Tc= runif(1,0,30), sigma.RTc = runif(1,0,30),
                           sigma.earlierTc = runif(1,0,70),
                           sigma.unmetc = runif(1,0,5) ))
            } else {
                list.varpar <-
                    c(list.varpar
                     ,list(
                          ## eps.ci  = matrix(rnorm(winbugs.data$C*, mean = 0, sd = ),..,..),
                          ## eta.ci  = rnorm(winbugs.data$C*, mean = 0, sd = ),
                          ## theta.ci  = rnorm(winbugs.data$C*, mean = 0, sd = ),
                          tau.lrc = 1/runif(1,0,5)^2,
                          tau.lpc = 1/runif(1,0,5)^2,
                         tau.wc = 1/runif(1,0,2)^2,
                          tau.Rwc = 1/runif(1,0,2)^2, # change JR 20131101
                          tau.Tc= 1/runif(1,0,30)^2,
                          tau.RTc = 1/runif(1,0,30)^2,
                          tau.earlierTc = 1/runif(1,0,70)^2,
                          tau.unmetc = 1/runif(1,0,5)^2 ))
            }

            ## [MCW-2016-11-09-2] :: If not fixing source-specific variances, initialize parameters.
            tau.sourcetot.init <- 1/runif(1,0.05, 1)^2
            sigma.unmet.dhs.init <- runif(1, 0.01, 1)
            sigma.unmet.other.init <- runif(1, 0.01, 1)

            ## Default for c.unmet.init
            c.unmet.init <- runif(1, -7,0)

            if(ModelFunctionCUnmetPrior(write.model.fun)) {
                ## [MCW-2016-06-02-14] Need to make sure prior for c.unmet is consistent
                ## with definition in WriteModel[suffix]().
                c.unmet.init <- runif(1, -34, -26)
            }

            ## [MCW-2016-11-01-3] :: If fixing source-specific variances, initialize 'tau.sourcetot' as 'NULL'.
            if(ModelFunctionFixSV(write.model.fun)) {
                tau.sourcetot.init <- NULL
                ## [MCW-2016-11-09-1] :: If fixing source-specific variances
                ## for unmet, initialize 'sigma.unmet.dhs' and
                ## 'sigma.unmet.other' as 'NULL'.
                sigma.unmet.dhs.init <- NULL
                sigma.unmet.other.init <- NULL
            }

            ## [MCW-2017-02-09-2] :: Use 'ModelFunctionRegInSA()' to identify models
            ## that use major region nested in sexual activity category
            ## hierarchy. This affects the number of initial values needed for
            ## 'RT.reg', etc.

            ## First set default values for pure geographic hierarchy.
            ## 'clus.lev.1' ~ major region
            ## 'clus.lev.2' ~ sub region
            size.clus.lev.1 <- winbugs.data$n.reg
            size.clus.lev.2 <- winbugs.data$n.subreg
            ## Change if using other hierarcy
            if(ModelFunctionRegInSA(write.model.fun)) {
                size.clus.lev.1 <- winbugs.data$n.sex.ac.unm
                size.clus.lev.2 <- winbugs.data$n.reg.in.sex.ac.unm
            }
            if(ModelFunctionSubRegInSA1India(write.model.fun)) {
                size.clus.lev.1 <- winbugs.data$n.sex.ac.unm
                size.clus.lev.2 <- winbugs.data$n.reg.in.sex.ac.unm.SA1sub
            }

            inits.list1 <-
                c(inits.list1
                 ,list(
                      ## "T.s[1,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                      ## "T.s[2,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                      ## "T.s[3,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                      ## "T.s[4,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                      ## # R specs: T ~ W(k,S) where T is prec. matrix, with E(T) = k*S, thus S = Vinv

                      ## [MCW-2016-11-01-4] :: Use 'tau.sourcetot.init' as initial value.
                      tau.sourcetot = tau.sourcetot.init,
                      sigma.unmetworld = runif(1,0.05,1),
                      mu.pos.m = rnorm(2, -2, 2),

                      sigma.pos = runif(1, 0.01, 2),
                      ##tau.pos.m = 1/runif(2, 0.01, 1)^2,
                      sigma.geo.m = runif(2, 0.01, 2),
                      ## note: first 3 refer to non-rich countries
                      T.world = msm::rtnorm(1, 1980, 40,  lower=1800),
                      T.subreg = msm::rtnorm(size.clus.lev.2, 1980, 60,  lower=1800),
                      T.reg = msm::rtnorm(size.clus.lev.1, 1980, 50,  lower=1800),

                      TOneLevel = msm::rtnorm(1, 1900, 50,  lower=1800),
                      RT.world = msm::rtnorm(1, 1980, 40,  lower=1800),
                      RT.subreg = msm::rtnorm(size.clus.lev.2, 1980, 60,  lower=1800),
                      RT.reg = msm::rtnorm(size.clus.lev.1, 1980, 50,  lower=1800),

                      sigma.Tsubreg = runif(1, 2, 80),
                      sigma.Treg = runif(1, 2, 80),
                      sigma.RTsubreg = runif(1, 2, 80),
                      sigma.RTreg = runif(1, 2, 80),

                      sigma.wreg = runif(1, 0.01, 2),
                      sigma.Rwreg = runif(1, 0.01, 2),
                      sigma.wsubreg = runif(1, 0.01, 2),
                      sigma.Rwsubreg = runif(1, 0.01, 2),
                      w.subreg = msm::rtnorm(size.clus.lev.2, logit(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                      Rw.subreg = msm::rtnorm(size.clus.lev.2, logit(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                      w.reg = msm::rtnorm(size.clus.lev.1, log(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                      Rw.reg = msm::rtnorm(size.clus.lev.1, log(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                      w.world = msm::rtnorm(1,logit(0.07), 1,  lower=-4.5, upper = 0),
                      Rw.world = msm::rtnorm(1,logit(0.07), 1, lower=-4.5, upper = 0),

                      lr.world  = rnorm(1, 2, 2),
                      sigma.ar.unmet  = runif(1,0.01,1),
                      ## [MCW-2016-11-09-3] :: Set 'source.unmet....' to init values.
                      sigma.unmet.dhs = sigma.unmet.dhs.init,
                      sigma.unmet.other = sigma.unmet.other.init,
                      rho.unmet  = runif(1,0,1),
                      sigma.tot  = runif(1,0.01,1),
                      sigma.rat  = runif(1,0.01,1),
                      rho.tot  = runif(1,0,1),
                      rho.rat  = runif(1,0,1),
                      a.unmet = rnorm(1,0,1),
                      b.unmet = rnorm(1,0,1),
                      ## [MCW-2016-11-01-2] :: use the new initial value for 'c.unmet'
                      c.unmet = c.unmet.init,
                      v.abs.probe.q = runif(1),v.folk = runif(1),v.mneg = runif(1),v.mpos = runif(1)
                  ))
            if(ModelFunctionSAAsymp(write.model.fun)) {
                inits.list1 <-
                    c(inits.list1
                     ,list(lp.world.not.sex = rnorm(1, -0.5, 1),
                           lp.world.sex = rnorm(1, -0.5, 1))
                      )
            } else {
                inits.list1 <-
                    c(inits.list1, list(lp.world = rnorm(1, -0.5, 1)))
            }
        }
        inits.list <- c(inits.list1, list.varpar)
        ##value<<List with initial values for model parameters

    } else {

        ## ####################################################################### ##
        ##                                RATE MODEL                               ##
        ## ####################################################################### ##

        list.varpar <- NULL
        ## Increase length of parameters if want estimates for countries with no data.
        if(include.c.no.data || isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
         inits.list1 <- list(
            RT.c = c(msm::rtnorm(winbugs.data$C + winbugs.data$C.no.data, 1980, 80, lower=1800), NA),
                                #Removed inits for T.c
            logitRomega.c = logit(runif(winbugs.data$C + winbugs.data$C.no.data, 0.02, 0.25)),
            logitomega.c = logit(runif(winbugs.data$C + winbugs.data$C.no.data, 0.02, 0.25)),
            logitRmax.c = LogitMinMax(runif(winbugs.data$C + winbugs.data$C.no.data, 0.55, 0.95), xmin = 0.5, xmax = 1),
            logitpmax.c = LogitMinMax(runif(winbugs.data$C + winbugs.data$C.no.data, 0.55, 0.95), xmin = 0.5, xmax = 1)
         )
         } else {
        inits.list1 <- list(
            RT.c = c(msm::rtnorm(winbugs.data$C, 1980, 80, lower=1800), NA), # change JR, 2013119: changed from winbugs.data$C+1
                                #Removed inits for T.c
            logitRomega.c = logit(runif(winbugs.data$C, 0.02, 0.25)),
            logitomega.c = logit(runif(winbugs.data$C, 0.02, 0.25)),
            logitRmax.c = LogitMinMax(runif(winbugs.data$C, 0.55, 0.95), xmin = 0.5, xmax = 1),
            logitpmax.c = LogitMinMax(runif(winbugs.data$C, 0.55, 0.95), xmin = 0.5, xmax = 1)
                                # ,setlevel.c = logit(runif(winbugs.data$C, 0.05,0.95)) #Change NC, 20161130
        )
        }

        ## Default for c.unmet.init
        c.unmet.init <- runif(1, -7,0)

        if(ModelFunctionCUnmetPrior(write.model.fun)) {
            ## [MCW-2016-06-02-14] Need to make sure prior for c.unmet is consistent
            ## with definition in WriteModel[suffix]().
            c.unmet.init <- runif(1, -34, -26)
        }

            ## [MCW-2017-02-09-2] :: Use 'ModelFunctionRegInSA()' to identify models
            ## that use major region nested in sexual activity category
            ## hierarchy. This affects the number of initial values needed for
            ## 'RT.reg', etc.

            ## First set default values for pure geographic hierarchy.
            ## 'clus.lev.1' ~ major region
            ## 'clus.lev.2' ~ sub region
            size.clus.lev.1 <- winbugs.data$n.reg
            size.clus.lev.2 <- winbugs.data$n.subreg
            ## Change if using other hierarcy
            if(ModelFunctionRegInSA(write.model.fun)) {
                size.clus.lev.1 <- winbugs.data$n.sex.ac.unm
                size.clus.lev.2 <- winbugs.data$n.reg.in.sex.ac.unm
            }
            if(ModelFunctionSubRegInSA1India(write.model.fun)) {
                size.clus.lev.1 <- winbugs.data$n.sex.ac.unm
                size.clus.lev.2 <- winbugs.data$n.reg.in.sex.ac.unm.SA1sub
            }


        if (!do.country.specific.run & !do.country.specific.targets.run) { # change JR, 20150301
            list.varpar <- NULL # change JR, 20150301
            inits.list1 <-
                c(inits.list1
                , list(         # "T.s[1,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                                # "T.s[2,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                                # "T.s[3,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                                # "T.s[4,,]" =  rwish(v = 2, S = solve(diag(0.1,2))),
                                # # R specs: T ~ W(k,S) where T is prec. matrix, with E(T) = k*S, thus S = Vinv
                      tau.sourcetot = 1/runif(1,0.05, 1)^2,
                      sigma.unmetworld = runif(1,0.05,1),
                      mu.pos.m = rnorm(2, -2, 2),
                      sigma.pos = runif(1, 0.01, 2),
                                # tau.pos.m = 1/runif(2, 0.01, 1)^2,
                      sigma.geo.m = runif(2, 0.01, 2),
                                # note: first 3 refer to non-rich countries
                      w.world = msm::rtnorm(1,logit(0.07), 1,  lower=-4.5, upper = 0),
                      S.world = msm::rtnorm(1,logit(0.5), 1,  lower=-4.5, upper = 4.5), #Change NC, 20161130
                      RT.world = msm::rtnorm(1, 1980, 40,  lower=1800),
                      Rw.world = msm::rtnorm(1,logit(0.07), 1, lower=-4.5, upper = 0),
                      w.reg = msm::rtnorm(size.clus.lev.1, log(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                      S.reg = msm::rtnorm(size.clus.lev.1, log(0.5), sd = 1.5,  lower=-4.5, upper = 4.5), #Change NC, 20161130
                      RT.reg = msm::rtnorm(size.clus.lev.1, 1980, 50,  lower=1800),
                      Rw.reg = msm::rtnorm(size.clus.lev.1, log(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                      sigma.RTreg = runif(1, 2, 80),
                      sigma.wreg = runif(1, 0.01, 2),
                      sigma.Sreg= runif(1, 0.01, 2),
                      sigma.Rwreg = runif(1, 0.01, 2),
                      sigma.RTsubreg = runif(1, 2, 80),
                      sigma.wsubreg = runif(1, 0.01, 2),
                      sigma.Ssubreg = runif(1, 0.01, 2),
                      sigma.Rwsubreg = runif(1, 0.01, 2),
                      lp.world = rnorm(1, -0.5, 1),
                      lr.world  = rnorm(1, 2, 2),
                      sigma.unmet.dhs = runif(1, 0.01, 1),
                      sigma.unmet.other = runif(1, 0.01, 1),
                      sigma.ar.unmet  = runif(1,0.01,1),
                      rho.unmet  = runif(1,0,1),
                      sigma.tot  = runif(1,0.01,1),
                      sigma.rat  = runif(1,0.01,1),
                      rho.tot  = runif(1,0,1),
                      rho.rat  = runif(1,0,1),
                      a.unmet = rnorm(1,0,1),
                      b.unmet = rnorm(1,0,1),
                      c.unmet = c.unmet.init, # [MCW-2017-12-29-1] change
                      v.abs.probe.q = runif(1),v.folk = runif(1),v.mneg = runif(1),v.mpos = runif(1) # [MCW-2018-01-02-5] change
                  ))

            if (!do.country.specific.run) { # change JR, 20150301
                if (change.priors.to.zerolower){
                    list.varpar <-
                        c(list.varpar
                        , list(
                              sigma.lpc = runif(1,0,5),
                              sigma.lrc = runif(1,0,5),
                              sigma.wc = runif(1, 0,2),
                              sigma.Sc = runif(1,0,3), # change NC, 20160817
                              sigma.Rwc = runif(1,0,2),## [MCW-2017-12-29-2] was 'runif(10, 2)'. Mistake?
                              sigma.RTc = runif(1,0,30),
                              sigma.unmetc = runif(1,0,5) ))
                } else {
                    list.varpar <-
                        c(list.varpar
                        , list(
                                # eps.ci  = matrix(rnorm(winbugs.data$C*, mean = 0, sd = ),..,..),
                                # eta.ci  = rnorm(winbugs.data$C*, mean = 0, sd = ),
                                # theta.ci  = rnorm(winbugs.data$C*, mean = 0, sd = ),
                              sigma.lpc = runif(1,0,5),# change NC, 20160817
                              tau.lrc = 1/runif(1,0,5)^2,
                              sigma.wc = runif(1, 0,2),# change NC, 20160817
                              sigma.Sc = runif(1,0,3), # change NC, 20160817
                              tau.Rwc = 1/runif(1,0,2)^2, # change JR 20131101
                              tau.RTc = 1/runif(1,0,30)^2,
                              tau.unmetc = 1/runif(1,0,5)^2 ))
                }
                inits.list1 <-
                    c(inits.list1
                    , list(
                          w.subreg = msm::rtnorm(size.clus.lev.2, logit(0.07), sd = 1.5,  lower=-4.5, upper = 0),
                          S.subreg = msm::rtnorm(size.clus.lev.2, logit(0.5), sd = 1.5,  lower=-4.5, upper = 4.5),
                          RT.subreg = msm::rtnorm(size.clus.lev.2, 1980, 60,  lower=1800),
                          Rw.subreg = msm::rtnorm(size.clus.lev.2, logit(0.07), sd = 1.5,  lower=-4.5, upper = 0)
                      ))
            }
        }
        inits.list <- c(inits.list1, list.varpar)
        ##value<<List with initial values for model parameters

    }

    ## ####################################################################### ##
    return(list(inits.list))
} ## end inits
#----------------------------------------------------------------------
GetBugsData <- function( # Construct winbugs.data object
  ### Construct winbugs.data object
  data, ##<< class data
  country.info, ##<< class country.info
  country.info.no.data = NULL,  ## [MCW-2016-08-23-2] Added
  data.global = NULL, ##<< class data.global
  data.SS = NULL, ##<< class data.SS
  output.dir = NULL, ##<< used in validation exercise only (to read in training set or save training set).
  verbose = TRUE, ##<< logical: print info?
  validation.list = NULL, ##<< NULL or of class validation.list (see \code{\link{RunMCMC}})
  do.country.specific.run = FALSE, ##<< logical, see \code{\link{RunMCMC}}
  do.SS.run.second.pass = FALSE, ##<< logical, see \code{\link{RunMCMC}}
  do.country.specific.targets.run = FALSE, ##<< logical, see \code{\link{RunMCMC}} # change JR, 20140301
  change.priors.to.zerolower = FALSE, ##<< logical, see \code{\link{RunMCMC}}
  names.sources = if(disagg.RN.PMA) { c("DHS", "MICS", "NS", "Other", "RN", "PMA", "SS") } else { c("DHS", "MICS", "NS", "Other", "SS") }, ##<< defines numeric IDs of data sources for contraceptive use.
                                        #, based on \code{unique(data$source.j)}
  ## [MCW-2016-03-07-1] Added "RN" for repeated nat'l survey and "PMA" for
  ## PMA. Noted that this is hardcoded; could not find call to
  ## \unique{data$source.j} (but didn't look very hard).
  names.sources.unmet = c("DHS", "Other"), ##<< defines ordering of
  ## data sources for unmet need.
  #, based on \code{unique(source.unmetj)}
  disagg.RN.PMA = TRUE #[MCW-2016-03-24-6] Added to allow disaggregation of repeated national and PMA.
 ,uwra.z.priors = NULL #[MCW-2016-06-02-7] Added to allow different
                                #priors for z model. Set to an integer code and
                                #define in body of GetBugsPriorSpecs().
 ,write.model.fun = "WriteModel" #[MCW-2016-06-02-8] pass in the 'WriteModel[suff]() fun used.
  ## [MCW-2016-06-14-5] added to pass through value of uwra.Omega.priors to GetBugsPriorSpecs().
  ,uwra.Omega.priors = NULL
  ## [MCW-2016-06-21-3] Added to control priors for kappa.^(c) variances. Set to
  ## an integer code and define in body of GetBusPriorSpecs().
 ,uwra.kappa.c.priors = NULL
  ##[MCW-2016-08-24-4] Added to control estimation for countries with no data.
  ,include.c.no.data = TRUE
 ,timing.world.priors = list(mean.TOneLevel = 1920, mean.Tworld = 1980) ## [MCW-2016-10-19-20] added.
 ,getj.training.k = NULL
 ,validation.at.random.no.data = NULL
 ,EA.bias.negative = EA.bias.negative
 ,HW.bias.negative = HW.bias.negative
  ) {
    if (do.SS.run.second.pass & is.null(data.SS))
        stop("data.SS cannot be NULL if this is the second pass of a run with SS data!")

        ## note: ordering of obs is not changed (else training wrong!)
        ##------------------------------------------------------------------------------------
        ## 0. If do.SS.run.first.pass, all obs of SS already removed from data in ReadDataAll.
        ##    If do.SS.run.second.pass, remove all observations of SS prior to and including SS obs used to estimate SS bias for the JAGS model.
        if (do.SS.run.second.pass) {
            max.survey.year <- max(data$years.j[data$source.j != "SS"])
            diff.years.ss <- data$years.j[data$source.j == "SS"] - max.survey.year
            ## get SS data point, either the closest SS obs prior to/in most recent survey year if available, else the closest one after that year
            if (any(diff.years.ss <= 0)) { # if SS data overlaps with non-SS data
                year.ss <- max(data$years.j[data$source.j == "SS" & data$years.j <= max.survey.year])
            } else {
                year.ss <- min(data$years.j[data$source.j == "SS"])
            }
            remove <- data$source.j == "SS" & data$years.j <= year.ss
            data <- data[!remove, ]
        }

    ##------------------------------------------------------------------------------------
    ## 1.Log ratios
    props.tot.j <- data$props.tot.j
    props.modern.j <- data$props.modern.j
    props.trad.j <- data$props.trad.j
    props.unmet.j <- data$props.unmet.j

    logratio.ymodern.j <- log(props.modern.j/(1-props.tot.j))
    logratio.ytrad.j <- log(props.trad.j/(1-props.tot.j))
    logit.ytot.j <- log(props.tot.j/(1-props.tot.j))

    logitratio.yunmet.j <- logitSafer(props.unmet.j/(1-props.tot.j))
    y.modern.j <- props.modern.j # change JR, 20131120 # SS obs selected for in model using getj.training.modern.k
    se.modern.j <- rep(0.025, length(y.modern.j)) # change JR, 20140806
    ##------------------------------------------------------------------------------------
    ## 2. Find all sorts of indices of the observations (year index, country, i)
    ## round years at half-year
    cat("Observation years are grouped into their respective calendar years.\n")
    round.years.j <- floor(data$years.j) + 0.5
    ## for obs j, find the country c and year index t
    gett.j <- round.years.j  # so t refers to midpoint of calendar year
    J <- length(gett.j)
    C <- length(country.info$iso.c)

    ## getc.J: which UNIQUE obs years are there in country c?
    ## to get getc.j,  watch out with as.numeric(data$name.j), gives order alphabetically!
    ## so just do a loop!
    N.unique.c <- rep(NA, C)


        ## gett.ci: the SORTED indices of the obs years for i = 1, .., N.unique.c[c] (for AR(1))
        ## Note: i does not relate to the observations anymore, just the unique years!
        gett.ci <- matrix(NA, C, max(country.info$N.c))
        for (c in 1:C) {
            select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
            N.unique.c[c] <- length(unique(gett.j[select]))
            gett.ci[c,1:N.unique.c[c]] <- sort(unique(gett.j[select]))
        }

    geti.j <- getc.j <- rep(NA, J)
    for (c in 1:C){
        select <- seq(1, J)[data$iso.j == country.info$iso.c[c]] #row j in 'data' is for which country?
        getc.j[select] <- c
        ## to get i for obs j, find year and see which index it has...
        for (jc in 1:length(select)){
            geti.j[select[jc]] <- which.max(gett.j[select][jc]==sort(unique(gett.j[select]))) #row j in 'data' corresponds to which chronologically ordered observation for that country (rows of 'data' not in chronological order within countries)
        }
    }
    ## For AR: find the indices of the countries with more than 1 observation:
    getc.z <- seq(1, C)[N.unique.c>1]
    n.countriesmorethan1obs <- length(getc.z)

    ## For unmet: find # obs and indices to skip the missing observation j's
    N.unmet <- sum(!is.na(props.unmet.j))
    getj.unmet.k <- seq(1,J)[!is.na(props.unmet.j)]

    ##------------------------------------------------------------------------------------
    ## 3. Regional information
    ## Note: reg.c and subreg.c are categorical variables/factors, so as.numeric gives their level

    n.reg <- length(unique(country.info$reg.c))
    n.subreg <- length(unique(country.info$subreg.c))
    subreg.c <- country.info$subreg.c
    reg.subreg <- rep(NA, n.subreg)       #The region the subregion is in
    for (subreg in 1:n.subreg){
        c <- which.max(country.info$subreg.c==subreg)
        reg.subreg[subreg] <- country.info$reg.c[c]
    }
    if("sex.ac.unm.c" %in% colnames(country.info)) {
        ## Create the indices (mimics 'crich', 'cnotrich')
        n.sex.ac.unm <- length(unique(country.info$sex.ac.unm.c))
        n.reg.in.sex.ac.unm <-
            length(unique(country.info$reg.in.sex.ac.unm.c))
        reg.in.sex.ac.unm.c <- country.info$reg.in.sex.ac.unm.c
        sex.ac.reg <- rep(NA, n.reg.in.sex.ac.unm)
        for(x in 1:n.reg.in.sex.ac.unm) {
            c <- which.max(country.info$reg.in.sex.ac.unm.c == x)
            sex.ac.reg[x] <- country.info$sex.ac.unm.c[c]
        }
        n.subreg.in.sex.ac.unm <-
            length(unique(country.info$subreg.in.sex.ac.unm.c))
        subreg.in.sex.ac.unm.c <- country.info$subreg.in.sex.ac.unm.c
        reg.in.sex.ac.unm.subreg <- rep(NA, n.subreg.in.sex.ac.unm) #The region the subregion is in
        for(x in 1:n.subreg.in.sex.ac.unm) {
            c <- which.max(country.info$subreg.in.sex.ac.unm.c == x)
            reg.in.sex.ac.unm.subreg[x] <- country.info$subreg.in.sex.ac.unm.c[c]
            }

        ## For subregion in SA1 / India alone
        n.reg.in.sex.ac.unm.SA1sub <-
            length(unique(country.info$reg.in.sex.ac.unm.SA1sub.c))
        reg.in.sex.ac.unm.SA1sub.c <- country.info$reg.in.sex.ac.unm.SA1sub.c
        sex.ac.unm.SA1sub.reg <- rep(NA, n.reg.in.sex.ac.unm.SA1sub)
        for(x in 1:n.reg.in.sex.ac.unm.SA1sub) {
            c <- which.max(country.info$reg.in.sex.ac.unm.SA1sub.c == x)
            sex.ac.unm.SA1sub.reg[x] <- country.info$sex.ac.unm.c[c] #same as for non 'SA1/India'
        }
        n.subreg.in.sex.ac.unm.SA1sub <-
            length(unique(country.info$subreg.in.sex.ac.unm.SA1sub.c))
        subreg.in.sex.ac.unm.SA1sub.c <- country.info$subreg.in.sex.ac.unm.SA1sub.c
        reg.in.sex.ac.unm.SA1sub.subreg <- rep(NA, n.subreg.in.sex.ac.unm.SA1sub) #The region the subregion is in
        for(x in 1:n.subreg.in.sex.ac.unm.SA1sub) {
            c <- which.max(country.info$subreg.in.sex.ac.unm.SA1sub.c == x)
            reg.in.sex.ac.unm.SA1sub.subreg[x] <- country.info$subreg.in.sex.ac.unm.SA1sub.c[c]
            }
    }

    ## Rich/Not rich indices
    crich.index <- seq(1, C)[country.info$dev.c=="Rich"] # dev.c is factor
    n.rich <- length(crich.index)
    cnotrich.index <- seq(1, C)[country.info$dev.c!="Rich"]
    n.notrich <- length(cnotrich.index)

    ## Mimic 'rich' 'norich'
    if("sex.ac.unm.c" %in% colnames(country.info)) {
        sex.ac.unm.index <- seq(1, C)[country.info$name.sex.ac.unm.c=="1"]
        n.sex.ac.unm.index <- length(sex.ac.unm.index)
        not.sex.ac.unm.index <- seq(1, C)[country.info$name.sex.ac.unm.c=="0"]
        n.not.sex.ac.unm.index <- length(not.sex.ac.unm.index)
    }
    ##----------------------------------------------------------------------------------
    ## 4. Data dummies

    ## NB: If isTRUE(validation.list$at.random.no.data) ||
    ## isTRUE(validation.list$leave.iso.out) these all need to be defined for
    ## the training obs (1, ..., J) and the test obs (J+1, ..., J+J.test).

    if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        data.test <- validation.at.random.no.data$data.test
        data.temp <- rbind(data, data.test)
        props.tot.j.temp <- c(props.tot.j, data.test$props.tot.j)
    } else {
        data.temp <- data
        props.tot.j.temp <- props.tot.j
    }

    ## Sources in 'names.sources' are:
    ## 1. DHS
    ## 2. MICS
    ## 3. NS (national survey)
    ## 4. Other (Other international survey)
    ## 5. Repeated national survey [not used]
    ## 6. PMA
    ## 7. SS (service statistics)

    if(disagg.RN.PMA) {                   #[MCW-2016-03-24-8]
        source.ind.j <- ifelse(data.temp$source.j==names.sources[1],1,
                        ifelse(data.temp$source.j==names.sources[2],2,
                        ifelse(data.temp$source.j==names.sources[3],3,
                        ifelse(data.temp$source.j==names.sources[5],5,
                        ifelse(data.temp$source.j==names.sources[6],6,  # change JR, 20131120
                               ## [MCW-2018-03-31] FIX sent by Niamh (31/3/2018) Source type '7' is not a valid source type in any of the BUGS models.
                               ## [MCW-2018-04-02] CHANGE; set 'source.ind.j' to '4' if 'data.temp$source.j' is '4'. Was being set to '7'.
                        ifelse(data.temp$source.j==names.sources[7],4,4))))))
    } else {
        source.ind.j <- ifelse(data.temp$source.j==names.sources[1],1,
                        ifelse(data.temp$source.j==names.sources[2],2,
                        ifelse(data.temp$source.j==names.sources[3],3,
                        ifelse(data.temp$source.j==names.sources[4],4,
                        ifelse(data.temp$source.j==names.sources[5],5
                              ,4     #[MCW-2016-04-26-1]
                               )))))  # change JR, 20131120
    }

    ## There should be no 'RN' values (repeated national survey) ; this source
    ## name has been deprecated. If there are any, issue an error:
    RN.rows <- data.temp$source.j == names.sources[5]
    if(any(RN.rows)) {
        stop("Data source type 'RN' ('repeated national survey') is depracated but is used for observations "
            ,paste(which(RN.rows), collapse = ", ")
            ,".")
    }
    ## NOW: Use value 5 for the obs with CP Any less than 1 percent,
    ## irrespective of the value of 'disagg.RN.PMA'. This is done here because
    ## the BUGS models use 'source.ind.j' to define source types for the the
    ## variance model. 'source.ind.j' is not used anywhere else, in particular
    ## it is not used in the plotting functions.

    if(ModelFunctionLT1pcOwnSource(write.model.fun)) {
        source.ind.j[data.temp$less.than.1.pc] <- 5
        }

    ## for unmet, only DHS versus non-DHS
    source.ind.unmet.j <- ifelse(data.temp$source.unmet.j==names.sources.unmet[1], 1, 2)

    ## ind for SA, EMAL, HW, age refers to a 0,1,2,3 etc indicator, (O = not applicable)
    ## note that within countries, only one multiplier is assigned
    ## approach for those categories is to use "" for the first category
    ## followed by a new level for each additional country
    ## use as.numeric for factors, then 1 corresponds to first level
    ## (alphabetically ordered, thus "" first)
    ## levels(as.factor(c("", "   ")))
    ## levels(as.factor(c("", "0")))


    ## PERTURBATION MULTIPLIERS

    ## _Married Women_

    ## |--------------------------------+-------+--------------------------|
    ## | Category                       | Abbr. | Stem (GetBugsData())     |
    ## |                                |       | values: <stem>.factor    |
    ## |                                |       | indicator: <stem>.ind.j  |
    ## |                                |       | no. of cats: ncat.<stem> |
    ## |--------------------------------+-------+--------------------------|
    ## | geographical region            |       | geo                      |
    ## | ever-married/all women         | EMAL  | emal                     |
    ## | Husband/wives, both sexes      | HW    | hw                       |
    ## | all sexually active women      | SA    | sa                       |
    ## | non-preg/fert/married SA women |       | posbias                  |
    ## | age group -ve bias             |       | negage                   |
    ## | age group +ve bias             |       | posage                   |
    ## | age group different            |       | age                      |
    ## |--------------------------------+-------+--------------------------|

    ## _UNmarried Women_

    ## |--------------------------+------+----------------------------------------+---------------+-----------+--------------------------+----------------------|
    ## |                          |      |                                        | Value         |           | Stem (GetBugsData())     |                      |
    ## |                          |      |                                        | in col.('s)   | Var.      | values: <stem>.factor    | Label                |
    ## |                          |      |                                        | in input      | in        | indicator: <stem>.ind.j  | in                   |
    ## | Category                 | Dir. | Col.('s) in input data                 | data          | data      | no. of cats: ncat.<stem> | [...]Multipliers.pdf |
    ## |--------------------------+------+----------------------------------------+---------------+-----------+--------------------------+----------------------|
    ## | With partner only        | +    | Population.type                        | PW            | poptype.j | hw                       | WP, trad/mod         |
    ## | Sterilization only       | +    | Population.type                        | FM            | poptype.j | emal                     | SO, trad/mod         |
    ## | Geographical region      | ?    | GEO.biases..unknown.direction.         | [non-miss]/'' | geo.j     | geo                      | geo, trad/mod        |
    ## | Higher risk of pregnancy | +    | Non.pregnant.and.other.positive.biases | +             | posbias.j | posbias                  | +, trad/mod          |
    ## | Age group with - bias    | -    | age.cat.bias                           | -             | age.cat.j | negage                   | -Age, trad/mod       |
    ## | Age group + bias         | +    | age.cat.bias                           | +             | age.cat.j | posage                   | +Age, trad/mod       |
    ## | Age group different      | ?    | age.cat.bias                           | ?             | age.cat.j | age                      | age, trad/mod        |
    ## |--------------------------+------+----------------------------------------+---------------+-----------+--------------------------+----------------------|


    ## note: list of multipliers is verified using
    ## par.V <- GetParnamesV(winbugs.data = winbugs.data, name.short.j = MakeCountryNamesShort(data$name.j))
    ## par.V$parnames.V.in.bugs
    ## par.V$parnames.V.nice
    ## data.frame(par.V$parnames.V.in.bugs,  par.V$parnames.V.nice)

    ## note: each of these vectors are augmented with a (J+1)th element containing the baseline category
    ## such that there are no errors when the baseline category is not present! # change JR, 2013111

    select <- !is.na(props.tot.j.temp) # change JR, 20131120

    sa.factor <- as.factor(c(ifelse(data.temp$poptype.j=="SA" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    sa.ind.j <- as.numeric(sa.factor)
    ncat.sa <- nlevels(sa.factor)

    emal.factor <- as.factor(c(ifelse( (data.temp$poptype.j=="EM"|data.temp$poptype.j=="AL"|data.temp$poptype.j=="FM") & select, data.temp$name.j, ""), "")) # change JR, 20131120
    emal.ind.j <- as.numeric(emal.factor)
    ncat.emal <- nlevels(emal.factor)
    ## NB: 'poptype.j == "EM"' also includes UWRA population type 'FM' (formerly
    ## married).

    hw.factor <- as.factor(c(ifelse((data.temp$poptype.j=="HW"|data.temp$poptype.j=="PW") & select, data.temp$name.j, ""), "")) # change JR, 20131120
    hw.ind.j <- as.numeric(hw.factor)
    ncat.hw <- nlevels(hw.factor)
    ## NB: 'poptype.j == "HW"' also includes UWRA population type 'PW'
    ## (partnered women).

    age.factor <- as.factor(c(ifelse(data.temp$age.cat.j=="?" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    age.ind.j <- as.numeric(age.factor)
    ncat.age <- nlevels(age.factor)

    posage.factor <- as.factor(c(ifelse(data.temp$age.cat.j=="+" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    posage.ind.j <- as.numeric(posage.factor)
    ncat.posage <- nlevels(posage.factor)

    negage.factor <- as.factor(c(ifelse(data.temp$age.cat.j=="-" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    negage.ind.j <- as.numeric(negage.factor)
    ncat.negage <- nlevels(negage.factor)

    ##<< Combine iso code/name with the explanation of the subgroup for geo and posbias
    ## (such that different multipliers are added, e.g. if different geo regions used within one country)
    geo.factor <- as.factor(c(ifelse(data.temp$geo.j!="" & select,
                                     paste(data.temp$name.j, data.temp$geo.j, sep = ": "), ""), "")) # change JR, 20131120
    geo.ind.j <- as.numeric(geo.factor)
    ncat.geo <- nlevels(geo.factor)

    posbias.factor <- as.factor(c(ifelse(data.temp$posbias.j!="" & select,
                                         paste(data.temp$name.j, data.temp$posbias.j, sep = ": "), ""), "")) # change JR, 20131120
    posbias.ind.j <- as.numeric(posbias.factor)
    ncat.posbias <- nlevels(posbias.factor)

    ## ind1 refers to a 0/1 indicator, (1 = Yes)
    ## For biases:
    abs.probe.q.ind1.j <- ifelse(data.temp$probe.bias.j == "1" & select, 1,0)
    folk.ind1.j <- ifelse(data.temp$folkbias.j != "" & select, 1,0) # change JR, 20131120
    mneg.ind1.j <- ifelse(data.temp$mod.bias.j == "-" & select, 1,0) # change JR, 20131120
    mpos.ind1.j <- ifelse(data.temp$mod.bias.j == "+" & select, 1,0) # change JR, 20131120

    ##  if (return.Vinfo){
    ##     return(list(posbias.factor = unique(posbias.factor),
    ##                 geo.factor = unique(geo.factor),
    ##                 negage.factor = unique(negage.factor),
    ##                 posage.factor = unique(posage.factor),
    ##                 age.factor = unique(age.factor),
    ##                 hw.factor = unique(hw.factor),
    ##                 sa.factor = unique(sa.factor),
    ##                 emal.factor = unique(emal.factor)
    ##                 ))
    ##   }

  ##----------------------------------------------------------------------------------
  ## 5. Countries with no data
  ## [MCW-2016-08-25-3] Create indices and such for countries with no data

    ## [MCW-2016-08-25-5] If including countries with no data, need to include any
    ## (sub)regions with no data as well. Will define 'C', 'crich.index' etc for
    ## no data countries later.

    if(include.c.no.data || isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        ## 5.1 Indices
        ## for obs j, find the country c and year index t
        C.no.data <- length(country.info.no.data$iso.c)

      ## 5.2a Regional information
      ## Note: reg.c and subreg.c are categorical variables/factors, so
        ## as.numeric gives their level

        ## Re-define 'n.reg', 'n.subreg', 'subreg.c'
        all.reg <- factor(c(country.info$namereg.c, country.info.no.data$namereg.c))
        n.reg <- length(levels(all.reg))
        reg.c <- as.numeric(all.reg)

        all.subreg <- factor(c(country.info$namesubreg.c, country.info.no.data$namesubreg.c))
        n.subreg <- length(levels(all.subreg))
        subreg.c <- as.numeric(all.subreg)

        ## Using redefinitions, redefine 'reg.subreg', 'reg.subreg'
        reg.subreg <- rep(NA, n.subreg)       #The region the subregion is in
        for (subreg in 1:n.subreg) {
            x <- which.max(subreg.c == subreg)
            reg.subreg[subreg] <- reg.c[x]
        }
        ## Re definitions for sexual activity
        if("sex.ac.unm.c" %in% colnames(country.info)) {
            n.sex.ac.unm <-
                length(unique(c(country.info$sex.ac.unm.c, country.info.no.data$sex.ac.unm.c)))

            ## regions in sexual activity categories
            all.reg.in.sex.ac.unm <-
                factor(c(country.info$name.reg.in.sex.ac.unm.c
                       ,country.info.no.data$name.reg.in.sex.ac.unm.c))
            n.reg.in.sex.ac.unm <- length(levels(all.reg.in.sex.ac.unm))
            reg.in.sex.ac.unm.c <- as.numeric(all.reg.in.sex.ac.unm)
            sex.ac.reg <- rep(NA, n.reg.in.sex.ac.unm)
            for(x in 1:n.reg.in.sex.ac.unm) {
                c <- which.max(reg.in.sex.ac.unm.c == x)
                sex.ac.reg[x] <-
                    c(country.info$sex.ac.unm.c, country.info.no.data$sex.ac.unm.c)[c]
            }
            all.subreg.in.sex.ac.unm <-
                factor(c(country.info$name.subreg.in.sex.ac.unm.c
                       ,country.info.no.data$name.subreg.in.sex.ac.unm.c))
            n.subreg.in.sex.ac.unm <- length(levels(all.subreg.in.sex.ac.unm))
            subreg.in.sex.ac.unm.c <- as.numeric(all.subreg.in.sex.ac.unm)
            reg.in.sex.ac.unm.subreg <- rep(NA, n.subreg.in.sex.ac.unm)
            for(x in 1:n.subreg.in.sex.ac.unm) {
                c <- which.max(subreg.in.sex.ac.unm.c == x)
                reg.in.sex.ac.unm.subreg[x] <-
                    c(country.info$subreg.in.sex.ac.unm.c, country.info.no.data$subreg.in.sex.ac.unm.c)[c]
            }

            ## For subregion in SA1 / India alone
            all.reg.in.sex.ac.unm.SA1sub <-
                factor(c(country.info$name.reg.in.sex.ac.unm.SA1sub.c
                        ,country.info.no.data$name.reg.in.sex.ac.unm.SA1sub.c))
            n.reg.in.sex.ac.unm.SA1sub <- length(levels(all.reg.in.sex.ac.unm.SA1sub))
            reg.in.sex.ac.unm.SA1sub.c <- as.numeric(all.reg.in.sex.ac.unm.SA1sub)
            sex.ac.unm.SA1sub.reg <- rep(NA, n.reg.in.sex.ac.unm.SA1sub)
            for(x in 1:n.reg.in.sex.ac.unm.SA1sub) {
                c <- which.max(reg.in.sex.ac.unm.SA1sub.c == x)
                sex.ac.unm.SA1sub.reg[x] <-
                    c(country.info$sex.ac.unm.c, country.info.no.data$sex.ac.unm.c)[c]
            }
            all.subreg.in.sex.ac.unm.SA1sub <-
                factor(c(country.info$name.subreg.in.sex.ac.unm.SA1sub.c
                        ,country.info.no.data$name.subreg.in.sex.ac.unm.SA1sub.c))
            n.subreg.in.sex.ac.unm.SA1sub <- length(levels(all.subreg.in.sex.ac.unm.SA1sub))
            subreg.in.sex.ac.unm.SA1sub.c <- as.numeric(all.subreg.in.sex.ac.unm.SA1sub)
            reg.in.sex.ac.unm.SA1sub.subreg <- rep(NA, n.subreg.in.sex.ac.unm.SA1sub)
            for(x in 1:n.subreg.in.sex.ac.unm.SA1sub) {
                c <- which.max(subreg.in.sex.ac.unm.SA1sub.c == x)
                reg.in.sex.ac.unm.SA1sub.subreg[x] <-
                    c(country.info$subreg.in.sex.ac.unm.SA1sub.c, country.info.no.data$subreg.in.sex.ac.unm.SA1sub.c)[c]
            }
        }

        ## 5.3 Rich/not rich indices
        ##country.info.no.data$name.c[country.info.no.data$dev.c=="Rich"]
        crich.index.no.data <- C +                                #start at C + 1
            seq(1, C.no.data)[country.info.no.data$dev.c=="Rich"] # dev.c is factor
        n.rich.no.data <- length(crich.index.no.data)
        cnotrich.index.no.data <- C +     #start at C+1
            seq(1, C.no.data)[country.info.no.data$dev.c!="Rich"]
        n.notrich.no.data <- length(cnotrich.index.no.data)

        ## 5.4 Sexually active indices
        if("sex.ac.unm.c" %in% colnames(country.info.no.data)) {
            sex.ac.unm.index.no.data <- C +
                seq(1, C.no.data)[country.info.no.data$name.sex.ac.unm.c=="1"]
            n.sex.ac.unm.index.no.data <- length(sex.ac.unm.index.no.data)
            not.sex.ac.unm.index.no.data <- C +
                seq(1, C.no.data)[country.info.no.data$name.sex.ac.unm.c=="0"]
            n.not.sex.ac.unm.index.no.data <- length(not.sex.ac.unm.index.no.data)
        }

        ##[MCW-2016-08-26-3] 'N.unique.c' extended for countries no data, which
        ##are given /one/ "datum".
        ## [MCW-2017-03-14-1] :: Commented out
        if(!isTRUE(validation.list$at.random.no.data) && !isTRUE(validation.list$leave.iso.out)) {
            N.unique.c <- c(N.unique.c, rep(1, C.no.data))
            ## [MCW-2017-03-14-1] :: Need 'N.unique.c.test' as well.
            N.unique.c.test <- rep(1, C.no.data)
            ## [MCW-2017-03-14-2] :: Need 'gett.ci.test' as well. It needs to
            ## contain 'mean.TOneLevel' or 'mean.Tworld', depending on whether the
            ## country is 'rich' or 'notrich'.
            ## gett.ci.test: the SORTED indices of the obs years for i = 1, ..,
            ## N.unique.c.test[c] (for AR(1))
            ## Note: i does not relate to the observations anymore, just the unique years!
            gett.ci.test <- matrix(NA, C.no.data, 1)
            if(ModelFunctionSATiming(write.model.fun)) {
                gett.ci.test[not.sex.ac.unm.index.no.data - C,] <- timing.world.priors$mean.TOneLevel
                gett.ci.test[sex.ac.unm.index.no.data - C,] <- timing.world.priors$mean.Tworld
                } else {
            gett.ci.test[crich.index.no.data - C,] <- timing.world.priors$mean.TOneLevel
            gett.ci.test[cnotrich.index.no.data - C,] <- timing.world.priors$mean.Tworld
            }
        }

        message(C, " countries with data\n", C.no.data, " countries with no data")
    }
    ##------------------------------------------------------------

    ##details<< list.no.validation contains
    ##describe<<
    list.no.validation <- list(
        N.unmet = N.unmet, #<<count unmet
        getj.unmet.k = getj.unmet.k, #<< indices unmet
        pmid.for.unmet = 0.4, #<< constant in model unmet
        logitratio.yunmet.j = logitratio.yunmet.j,  #<< logit(unmet/1-total) observed
        ratios.trad.modern.jn = as.matrix(cbind(logratio.ytrad.j, logratio.ymodern.j)),#<< matrix, observed logit(trad/tot, mod/tot))
        logit.ytot.j = logit.ytot.j,#<< logit(tot/none) observed
        y.modern.j = y.modern.j, #<< observations of modern from SS # change JR, 20131121
        se.modern.j = se.modern.j, #<< SE of observations of modern from SS (set to 2.5 percent) # change JR, 20131120
        getc.z = getc.z, #<< indices of countries with more than 1 observation (for AR loop)
        n.countriesmorethan1obs = n.countriesmorethan1obs, #<< count
        getc.j = getc.j, #<< country index for each obs index j
        N.unique.c = c(N.unique.c), #<< number of unique obs years per country
        gett.ci = gett.ci, #<< year index for obs i in country c
        geti.j = geti.j, #<< index of obs year in country c
        C = C, #<< no of countries
        J = J, #<< no of observations (total non-missing observations on total contraceptive use)
        crich.index = crich.index,#<< indices of developED (rich) countries
        cnotrich.index = cnotrich.index, #<< indices of developING (notrich) countries
        n.rich = n.rich, #<< no of developed regions
        n.notrich = n.notrich,#<< no of developing regions
        reg.subreg = reg.subreg, #<< region index for each subregion
        n.subreg = n.subreg,#<< no of subregions
        n.reg = n.reg, #<< no of regions
        subreg.c = subreg.c,#<< subregion index for each country
        source.ind.unmet.j = source.ind.unmet.j, #<< indicator for unmet source (1,2)
        source.ind.j = source.ind.j,#<< indicator for source (1,2,3,4,5,6)
        sa.ind.j = sa.ind.j, #<< indicator for SA women (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.sa = ncat.sa,#<< 1 + no of permutation parameters
        posbias.ind.j = posbias.ind.j, #<< indicator for pos bias (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.posbias = ncat.posbias,#<< 1 + no of permutation parameters
        age.ind.j = age.ind.j, #<< indicator for age (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.age = ncat.age,#<<1 + no of permutation parameters
        hw.ind.j = hw.ind.j, #<< indicator for HW (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.hw = ncat.hw,#<<1 + no of permutation parameters
        emal.ind.j = emal.ind.j,#<<  indicator for EM/AL (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.emal = ncat.emal,#<<1 + no of permutation parameters
        geo.ind.j = geo.ind.j, #<< indicator for geo bias (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.geo = ncat.geo,#<<1 + no of permutation parameters
        posage.ind.j = posage.ind.j,#<< indicator for pos age bias (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.posage = ncat.posage,#<<1 + no of permutation parameters
        negage.ind.j = negage.ind.j, #<< indicator for neg age bias (1 = NA, 2+ gives unique pertubation multiplier)
        ncat.negage = ncat.negage,#<<1 + no of permutation parameters
        abs.probe.q.ind1.j = abs.probe.q.ind1.j, #<< indicator for absence of probing questions (1 = yes, 0 = no)
        folk.ind1.j = folk.ind1.j,#<<  indicator for folk(1 = yes, 0 = no)
        mpos.ind1.j = mpos.ind1.j, #<<  indicator for modern[+] (1 = yes, 0 = no)
        mneg.ind1.j = mneg.ind1.j #<<  indicator for modern[-] (1 = yes, 0 = no)
    )

    ## Sexual activity among unmarried
    if("sex.ac.unm.c" %in% colnames(country.info)) {
        list.no.validation <-
            c(list.no.validation
             ,list(sex.ac.unm.index = sex.ac.unm.index
                  ,n.sex.ac.unm.index = n.sex.ac.unm.index
                  ,not.sex.ac.unm.index = not.sex.ac.unm.index
                  ,n.not.sex.ac.unm.index = n.not.sex.ac.unm.index
                  ,n.sex.ac.unm = n.sex.ac.unm
                  ,reg.in.sex.ac.unm.c = reg.in.sex.ac.unm.c
                  ,n.reg.in.sex.ac.unm = n.reg.in.sex.ac.unm
                  ,sex.ac.reg = sex.ac.reg
                  ,subreg.in.sex.ac.unm.c = subreg.in.sex.ac.unm.c
                  ,n.subreg.in.sex.ac.unm = n.subreg.in.sex.ac.unm
                  ,reg.in.sex.ac.unm.subreg = reg.in.sex.ac.unm.subreg
                  ,reg.in.sex.ac.unm.SA1sub.c = reg.in.sex.ac.unm.SA1sub.c
                  ,n.reg.in.sex.ac.unm.SA1sub = n.reg.in.sex.ac.unm.SA1sub
                  ,sex.ac.unm.SA1sub.reg = sex.ac.unm.SA1sub.reg
                  ,subreg.in.sex.ac.unm.SA1sub.c = subreg.in.sex.ac.unm.SA1sub.c
                  ,n.subreg.in.sex.ac.unm.SA1sub = n.subreg.in.sex.ac.unm.SA1sub
                  ,reg.in.sex.ac.unm.SA1sub.subreg = reg.in.sex.ac.unm.SA1sub.subreg
                   ))

    }

    ## If include.c.no.data
    if(include.c.no.data || isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        list.no.validation <-
            c(list.no.validation
             ,list(C.no.data = C.no.data
                  ,crich.index.no.data = crich.index.no.data
                  ,n.rich.no.data = n.rich.no.data
                  ,cnotrich.index.no.data = cnotrich.index.no.data
                  ,n.notrich.no.data = n.notrich.no.data
                   )
              )
        if(!isTRUE(validation.list$at.random.no.data) && !isTRUE(validation.list$leave.iso.out)) {
            list.no.validation <-
            c(list.no.validation
             ,list(N.unique.c.test = N.unique.c.test #[MCW-2017-03-14-1]
                  ,gett.ci.test = gett.ci.test      #[MCW-2017-03-14-2]
                   ))
            }

        ## Sexual activity among unmarried
        if("sex.ac.unm.c" %in% colnames(country.info.no.data)) {
            list.no.validation <-
                c(list.no.validation
                 ,list(sex.ac.unm.index.no.data = sex.ac.unm.index.no.data
                      ,n.sex.ac.unm.index.no.data = n.sex.ac.unm.index.no.data
                      ,not.sex.ac.unm.index.no.data = not.sex.ac.unm.index.no.data
                      ,n.not.sex.ac.unm.index.no.data = n.not.sex.ac.unm.index.no.data
                       ))
        }
    }

  if (do.SS.run.second.pass) { # change JR, 20140414
    list.no.validation <- c(list.no.validation,
                            list(bias.modern = data.SS$bias.modern ##<< calculated SS modern bias
                            ))
  }

    ##end<<
    if (!is.null(validation.list)){
        ## when leaving out obs at random, all tot are included in training when leaving
        ## out obs at end, some tot might be excluded (but tot in training is not zero)
        ## and test set should not include tot obs!
        ## setdiff means first set is the baseline, remove any elements that are in the 2nd set
        if (validation.list$exclude.unmet.only){
            getj.test.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)], getj.training.k)
            getj.training.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)],getj.test.unmet.k)
            n.training.unmet <- length(getj.training.unmet.k)
            n.test.unmet <- length(getj.test.unmet.k)
                                # include all total/breakdown in training!!!
            getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
            getj.training.k <- seq(1, J)[!is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
            n.training.tot <- length(getj.training.tot.k)
            n.training.breakdown <- length(getj.training.k)
            getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
            n.training.modern <- length(getj.training.modern.k) # change JR, 20131120
            list.validation <- list(
                getj.training.k  = getj.training.k,
                n.training.breakdown = n.training.breakdown,
                getj.training.tot.k  = getj.training.tot.k,
                n.training.tot = n.training.tot,
                getj.training.modern.k = getj.training.modern.k, # change JR, 20131120
                n.training.modern = n.training.modern, # change JR, 20131120
                getj.training.unmet.k = getj.training.unmet.k,
                getj.test.unmet.k = getj.test.unmet.k,
                n.training.unmet = n.training.unmet,
                n.test.unmet = n.test.unmet
            )
        } else if(validation.list$at.random.no.data || validation.list$leave.iso.out) {
            ## Training are all data not in countries put in test set
            n.training.breakdown <- length(getj.training.k)
            getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)]
            n.training.tot <- length(getj.training.tot.k)
            getj.training.modern.k <- seq(1, J)[data$source.j == "SS"]
            n.training.modern <- length(getj.training.modern.k)
            getj.training.unmet.k <- seq(1, J)[!is.na(props.unmet.j)]
            n.training.unmet <- length(getj.training.unmet.k)
            ##details<< Then \code{list.validation} is given by:
            ##describe<<
            ## ----------
            ## Need to duplicate the creation of the data vectors and indices
            ## for the data put in 'data.test'.

            props.tot.j.test <- data.test$props.tot.j
            props.modern.j.test <- data.test$props.modern.j
            props.trad.j.test <- data.test$props.trad.j
            props.unmet.j.test <- data.test$props.unmet.j

            logratio.ymodern.j.test <- log(props.modern.j.test/(1-props.tot.j.test))
            logratio.ytrad.j.test <- log(props.trad.j.test/(1-props.tot.j.test))
            logit.ytot.j.test <- log(props.tot.j.test/(1-props.tot.j.test))
            logitratio.yunmet.j.test <- logit(props.unmet.j.test/(1-props.tot.j.test))
            y.modern.j.test <- props.modern.j.test
            se.modern.j.test <- rep(0.025, length(y.modern.j.test))

            round.years.j.test <- floor(data.test$years.j) + 0.5
            ## for obs j, find the country c and year index t
            gett.j.test <- round.years.j.test  # so t refers to midpoint of
                                # calendar year
            J.test <- length(gett.j.test)

            ## getc.j.test: which UNIQUE obs years are there in country c?  to
            ## get getc.j.test, watch out with as.numeric(data.test$name.j),
            ## gives order alphabetically!  so just do a loop!
            N.unique.c.test <- rep(NA, C.no.data)
            ## gett.ci.test: the SORTED indices of the obs years for i = 1, ..,
            ## N.unique.c.test[c] (for AR(1))
            ## Note: i does not relate to the observations anymore, just the unique years!
            gett.ci.test <- matrix(NA, C.no.data, max(country.info.no.data$N.c))
            for (c in 1:C.no.data){
                select <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
                N.unique.c.test[c] <- length(unique(gett.j.test[select]))
                gett.ci.test[c,1:N.unique.c.test[c]] <- sort(unique(gett.j.test[select]))
            }

            geti.j.test <- getc.j.test <- rep(NA, J.test)
            for (c in 1:C.no.data){
                select <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
                                #row j in 'data.test' is for which country?
                getc.j.test[select] <- c + C #!!!
                ## to get i for obs j, find year and see which index it has...
                for (jc in 1:length(select)){geti.j.test[select[jc]] <-
                                                 which.max(gett.j.test[select][jc]==sort(unique(gett.j.test[select])))
                                #row j in 'data.test' corresponds to
                                #which chronologically ordered
                                #observation for that country (rows of
                                                 # 'data.test' not in chronological order
                                #within countries)
                }
            }
            ## For AR: find the indices of the countries with more than 1 observation:
            getc.z.test <- seq(1, C.no.data)[N.unique.c.test>1] + C #!!!
            n.countriesmorethan1obs.test <- length(getc.z.test)

            ## For unmet: find # obs and indices to skip the missing observation j's
            N.unmet.test <- sum(!is.na(props.unmet.j.test))
            getj.test.unmet.k <- seq(1,J.test)[!is.na(props.unmet.j.test)] + J #!!!!
            ## ----------
            ## Test are based on data.test
            n.test.unmet <- sum(!is.na(props.unmet.j.test))
            n.test.breakdown <- sum(!is.na(props.modern.j.test))
            getj.test.k <- (1:J.test) + J ##[!is.na(props.modern.j.test)] + J #!!!!

            list.validation <- list(
                getj.training.k  = getj.training.k, ##<< indices j for training obs k=1,...
                getj.test.k = getj.test.k,##<< indices (left-out if unmet left-out only)
                n.training.breakdown = n.training.breakdown,##<< count of training obs
                n.test.breakdown = n.test.breakdown,##<< count of test obs (left-out if unmet left-out only)
                getj.training.tot.k  = getj.training.tot.k,##<< indices
                n.training.tot = n.training.tot,##<< count of training obs
                getj.training.modern.k = getj.training.modern.k, # change JR, 20131120
                n.training.modern = n.training.modern, # change JR, 20131120
                getj.training.unmet.k  = getj.training.unmet.k, ##<< indices
                getj.test.unmet.k = getj.test.unmet.k,##<< indices
                n.training.unmet = n.training.unmet,##<< count of training obs
                n.test.unmet = n.test.unmet,##<< count of test obs
                ## data vectors and indices
                ## props.tot.j.test = props.tot.j.test,
                ## props.modern.j.test = props.modern.j.test,
                ## props.trad.j.test = props.trad.j.test,
                ## props.unmet.j.test = props.unmet.j.test,
                ## logratio.ymodern.j.test = logratio.ymodern.j.test,
                ## logratio.ytrad.j.test = logratio.ytrad.j.test,
                ## logit.ytot.j.test = logit.ytot.j.test,
                ## logitratio.yunmet.j.test = logitratio.yunmet.j.test,
                y.modern.j.test = y.modern.j.test,
                se.modern.j.test = se.modern.j.test,
                round.years.j.test = round.years.j.test,
                gett.j.test = gett.j.test,
                J.test = J.test,
                N.unique.c.test = N.unique.c.test,
                gett.ci.test = gett.ci.test,
                geti.j.test = geti.j.test,
                getc.j.test = getc.j.test,
                getc.z.test = getc.z.test,
                n.countriesmorethan1obs.test = n.countriesmorethan1obs.test,
                N.unmet.test = N.unmet.test
            )
        } else {# at end or at random
            getj.test.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)], getj.training.k)
            getj.training.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)],getj.test.unmet.k)
            n.training.unmet <- length(getj.training.unmet.k)
            n.test.unmet <- length(getj.test.unmet.k)
            getj.test.k <- setdiff(seq(1, J)[!is.na(logratio.ymodern.j) & !is.na(props.tot.j)], getj.training.k) # change JR, 20131120
            n.training.breakdown <- length(getj.training.k)
            n.test.breakdown <- length(getj.test.k)
            getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
            n.training.tot <- length(getj.training.tot.k)
            getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
            n.training.modern <- length(getj.training.modern.k) # change JR, 20131120
            ##details<< Then \code{list.validation} is given by:
            ##describe<<
            list.validation <- list(
                getj.training.k  = getj.training.k, ##<< indices j for training obs k=1,...
                getj.test.k = getj.test.k,##<< indices (left-out if unmet left-out only)
                n.training.breakdown = n.training.breakdown,##<< count of training obs
                n.test.breakdown = n.test.breakdown,##<< count of test obs (left-out if unmet left-out only)
                getj.training.tot.k  = getj.training.tot.k,##<< indices
                n.training.tot = n.training.tot,##<< count of training obs
                getj.training.modern.k = getj.training.modern.k, # change JR, 20131120
                n.training.modern = n.training.modern, # change JR, 20131120
                getj.training.unmet.k  = getj.training.unmet.k, ##<< indices
                getj.test.unmet.k = getj.test.unmet.k,##<< indices
                n.training.unmet = n.training.unmet,##<< count of training obs
                n.test.unmet = n.test.unmet##<< count of test obs
            )
            ##end<<
        }
    } else { #all, no validation
        getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
        getj.training.k <- seq(1, J)[!is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
        getj.test.k <- NULL
        getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
        getj.training.unmet.k <- seq(1, J)[!is.na(props.unmet.j)]
        getj.test.unmet.k <- NULL
        n.training.breakdown <- length(getj.training.k)
        n.training.tot <- length(getj.training.tot.k)
        n.training.modern <- length(getj.training.modern.k) # change JR, 20131120
        n.training.unmet <- length(getj.training.unmet.k)
        ##details<< If it is not a validation exercise, list.validation contains
        ##describe<<
        list.validation <- list(
            getj.training.k  = getj.training.k, ##<< indices
            n.training.breakdown = n.training.breakdown, ##<< count
            getj.training.tot.k  = getj.training.tot.k, ##<<indices
            n.training.tot = n.training.tot, ##<< count
            getj.training.modern.k = getj.training.modern.k, ##<< indices # change JR, 20131120
            n.training.modern = n.training.modern, ##<< count # change JR, 20131120
            getj.training.unmet.k  = getj.training.unmet.k, ##<< indices
            n.training.unmet = n.training.unmet ##<< count
        )
        ##end<<
    }

    if(do.country.specific.run || do.country.specific.targets.run) {
        ## For one country run, find name of subreg for country
        if(ModelFunctionSubRegInSA1India(write.model.fun)) {
            name.reg.for.prior.specs <-
                as.character(country.info$name.reg.in.sex.ac.unm.SA1sub)[1]
            name.subreg.for.prior.specs <-
                as.character(country.info$name.subreg.in.sex.ac.unm.SA1sub)[1]
        } else if(ModelFunctionSA(write.model.fun)) {
            name.reg.for.prior.specs <-
                as.character(country.info$name.reg.in.sex.ac.unm)[1]
            name.subreg.for.prior.specs <-
                as.character(country.info$name.subreg.in.sex.ac.unm)[1]
        } else {
            name.reg.for.prior.specs <-
                as.character(country.info$namereg.c)[1]
            name.subreg.for.prior.specs <-
                as.character(country.info$namesubreg.c)[1]
        }
    }

    priorspecs <- GetBugsPriorSpecs(change.priors.to.zerolower = change.priors.to.zerolower,
                                  do.country.specific.run = do.country.specific.run, # change JR, 20131104
                                  do.country.specific.targets.run = do.country.specific.targets.run, # change JR, 20131104
                                  name.reg = name.reg.for.prior.specs, # change JR, 20131104
                                  name.subreg = name.subreg.for.prior.specs, # change JR, 20131104
                                  iso.country.select = country.info$iso.country.select[1], # change JR, 20140404
                                  data.global = data.global, # change JR, 20131104
                                  disagg.RN.PMA = disagg.RN.PMA #[MCW-2016-04-04-1] Added.
                                        #[MCW-2016-06-02-10] added next two lines to pass through
                                        #values of new z priors and write model script.
                                 ,uwra.z.priors = uwra.z.priors
                                 ,write.model.fun = write.model.fun
                                  ## [MCW-2016-06-14-4] added to pass through value of uwra.Omega.priors.
                                 ,uwra.Omega.priors = uwra.Omega.priors
                              ##[MCW-2016-06-21-4] Added to pass this argument through.
                                 ,uwra.kappa.c.priors = uwra.kappa.c.priors
                                  ## [MCW-2016-10-05-2] :: Added to pass these through.
                                 ,mean.Tworld = timing.world.priors$mean.Tworld
                                      ,mean.TOneLevel = timing.world.priors$mean.TOneLevel,
                                  verbose = verbose
                                )
  ##value<< One combined list that includes elements from
  #  ##describe<<
  winbugs.data <- c(list.no.validation, ##<< See details.
                    list.validation, ##<< See details.
                    priorspecs, ##<< Prior specs from \code{GetBugsPriorSpecs}.
                    validation.at.random.no.data)

                                        #  ##end<<
  return(winbugs.data)
}


##--------------------------------------------------------------------------------
##
GetBugsData_Rate <- function( # Construct winbugs.data object
  ### Construct winbugs.data object
  data, ##<< class data
  country.info, ##<< class country.info
  country.info.no.data = NULL,
  data.global = NULL, ##<< class data.global
  data.SS = NULL, ##<< class data.SS
  data.logratios = NULL, ##<< class data.logratios
  output.dir = NULL, ##<< used in validation exercise only (to read in training set or save training set).
  verbose = TRUE, ##<< logical: print info?
  validation.list = NULL, ##<< NULL or of class validation.list (see \code{\link{RunMCMC}})
  do.country.specific.run = FALSE, ##<< logical, see \code{\link{RunMCMC}}
  do.SS.run.second.pass = FALSE, ##<< logical, see \code{\link{RunMCMC}}
  do.country.specific.targets.run = FALSE, ##<< logical, see \code{\link{RunMCMC}} # change JR, 20140301
  change.priors.to.zerolower = FALSE, ##<< logical, see \code{\link{RunMCMC}}
  names.sources = if(disagg.RN.PMA) { c("DHS", "MICS", "NS", "Other", "RN", "PMA", "SS") } else { c("DHS", "MICS", "NS", "Other", "SS") }, ##<< defines numeric IDs of data sources for contraceptive use.
                                        #, based on \code{unique(data$source.j)}
  ## [MCW-2016-03-07-1] Added "RN" for repeated nat'l survey and "PMA" for
  ## PMA. Noted that this is hardcoded; could not find call to
  ## \unique{data$source.j} (but didn't look very hard).
  #, based on \code{unique(data$source.j)}
  names.sources.unmet = c("DHS", "Other") ##<< defines ordering of
  ## data sources for unmet need.
   #, based on \code{unique(source.unmetj)}
### ARGS added by Mark w
  ,disagg.RN.PMA = TRUE
 ,uwra.z.priors = NULL #[MCW-2016-06-02-7] Added to allow different
                                #priors for z model. Set to an integer code and
                                #define in body of GetBugsPriorSpecs().
 ,write.model.fun = "WriteModel" #[MCW-2016-06-02-8] pass in the 'WriteModel[suff]() fun used.
  ## [MCW-2016-06-14-5] added to pass through value of uwra.Omega.priors to GetBugsPriorSpecs().
  ,uwra.Omega.priors = NULL
  ## [MCW-2016-06-21-3] Added to control priors for kappa.^(c) variances. Set to
  ## an integer code and define in body of GetBusPriorSpecs().
 ,uwra.kappa.c.priors = NULL
  ##[MCW-2016-08-24-4] Added to control estimation for countries with no data.
  ,include.c.no.data = TRUE
 ,timing.world.priors = list(mean.TOneLevel = 1920, mean.Tworld = 1980) ## [MCW-2016-10-19-20] added.
 ,getj.training.k = NULL
 ,validation.at.random.no.data = NULL
 ,EA.bias.negative = EA.bias.negative
 ,HW.bias.negative = HW.bias.negative
) {
  if (do.SS.run.second.pass & is.null(data.SS))
    stop("data.SS cannot be NULL if this is the second pass of a run with SS data!")

  if (do.country.specific.targets.run) {
    #------------------------------------------------------------------------------------
    # 1. Extract information about last available year of estimates
    logratio.y.jn <- matrix(NA, 1, 3)
    c.select <- which(data.global$iso.c == gsub(" ", "", country.info$code.c[1]))
    logratio.y.jn[1, ] <- c(data.logratios$median.log.trad.noneed.c[c.select],
                            data.logratios$median.log.mod.noneed.c[c.select],
                            data.logratios$median.log.unmet.noneed.c[c.select])
    # Tau.logratios <- solve(data.logratios$Sigma.medianmin.33)
    Tau.logratios <- solve(data.logratios$Sigma.c33[c.select, , ]) # country-specific
    #------------------------------------------------------------------------------------
    # 2. Find all sorts of indices of the observations (year index, country, i)
    # round years at half-year
    round.years.j <- floor(data.logratios$year) + 0.5 # change JR, 20150301
    # for obs j, find the country c and year index t
    gett.j <- round.years.j  # so t refers to midpoint of calendar year
    J <- length(gett.j)
    C <- length(country.info$iso.c)

    # getc.J: which UNIQUE obs years are there in country c?
    N.unique.c <- rep(1, C) # change JR, 20150301
    gett.ci <- matrix(gett.j, C, 1) # change JR, 20150301
    geti.j <- getc.j <- rep(1, J)
    # For AR: find the indices of the countries with more than 1 observation:
    getc.z <- seq(1, C)[N.unique.c>1]
    n.countriesmorethan1obs <- length(getc.z)
    #------------------------------------------------------------------------------------
    # 3. Regional information
    # Note: reg.c and subreg.c are categorical variables/factors, so as.numeric gives their level
    n.reg <- length(unique(country.info$reg.c))
    n.subreg <- length(unique(country.info$subreg.c))
    subreg.c <- country.info$subreg.c
    reg.subreg <- rep(NA, n.subreg)
    for (subreg in 1:n.subreg){
      c <- which.max(country.info$subreg.c==subreg)
      reg.subreg[subreg] <- country.info$reg.c[c]
    }
    crich.index <- seq(1, C)[country.info$dev.c=="Rich"] # dev.c is factor
    n.rich <- length(crich.index)
    cnotrich.index <- seq(1, C)[country.info$dev.c!="Rich"]
    n.notrich <- length(cnotrich.index)
    #----------------------------------------------------------------------------------
    ##details<< list.no.validation contains
    ##describe<<
    list.no.validation <- list(
      logratio.y.jn = logratio.y.jn,
      Tau.logratios = Tau.logratios,
      pmid.for.unmet = 0.4, ##<< constant in model unmet
      getc.z = getc.z, ##<< indices of countries with more than 1 observation (for AR loop)
      n.countriesmorethan1obs = n.countriesmorethan1obs, ##<< count
      getc.j = getc.j, ##<< country index for each obs index j
      N.unique.c = c(N.unique.c), ##<< number of unique obs years per country
      gett.ci = gett.ci, ##<< year index for obs i in country c
      geti.j = geti.j, ##<< index of obs year in country c
      gett.j = gett.j, ##<< obs year for observation
      C = C, ##<< no of countries
      J = J, ##<< no of observations
      crich.index = crich.index,##<< indices of developED (rich) countries
      cnotrich.index = cnotrich.index, ##<< indices of developING (notrich) countries
      n.rich = n.rich, ##<< no of developed countries
      n.notrich = n.notrich,##<< no of developing countries
      reg.subreg = reg.subreg, ##<< region index for each subregion
      n.subreg = n.subreg,##<< no of subregions
      n.reg = n.reg, ##<< no of regions
      subreg.c = subreg.c##<< subregion index for each country
    )
    list.validation <- NULL
    ##end<<
  } else {


    # note: ordering of obs is not changed (else training wrong!)
    #------------------------------------------------------------------------------------
    # 0. If do.SS.run.first.pass, all obs of SS already removed from data in ReadDataAll.
    #    If do.SS.run.second.pass, remove all observations of SS prior to and including SS obs used to estimate SS bias for the JAGS model.
    if (do.SS.run.second.pass) {
      max.survey.year <- max(data$years.j[data$source.j != "SS"])
      diff.years.ss <- data$years.j[data$source.j == "SS"] - max.survey.year
      # get SS data point, either the closest SS obs prior to/in most recent survey year if available, else the closest one after that year
      if (any(diff.years.ss <= 0)) { # if SS data overlaps with non-SS data
        year.ss <- max(data$years.j[data$source.j == "SS" & data$years.j <= max.survey.year])
      } else {
        year.ss <- min(data$years.j[data$source.j == "SS"])
      }
      remove <- data$source.j == "SS" & data$years.j <= year.ss
      data <- data[!remove, ]
    }


    #------------------------------------------------------------------------------------
    # 1. Log ratios
    props.tot.j <- data$props.tot.j
    props.modern.j <- data$props.modern.j
    props.trad.j <- data$props.trad.j
    props.unmet.j <- data$props.unmet.j

    logratio.ymodern.j <- log(props.modern.j/(1-props.tot.j))
    logratio.ytrad.j <- log(props.trad.j/(1-props.tot.j))
      logit.ytot.j <- log(props.tot.j/(1-props.tot.j))
      logitratio.yunmet.j <- logitSafer(props.unmet.j/(1-props.tot.j))
      logit.ymodonly.j <- logitSafer(props.modern.j)
      y.modern.j <- props.modern.j # change JR, 20131120 # SS obs selected for in model using getj.training.modern.k
      se.modern.j <- rep(0.025, length(y.modern.j)) # change JR, 20140806

      if(ModelFunctionSurveySEs(write.model.fun)) {
          ## [MCW 2018-11-16] Added this 'if' clause for
          ## consistency. There is only one rate model and it doesn't
          ## work without SEs.

          ##Info on SEs #Change NC, 20161218
          se.info.j<-ImputeSE(data=data,country.info=country.info,data.global=data.global,do.country.specific.run=do.country.specific.run)
          se.logR.trad.impute<-se.info.j$se.logR.trad.impute
          se.logR.modern.impute<-se.info.j$se.logR.modern.impute
          se.logR.unmet.impute<-se.info.j$se.logR.unmet.impute
      }


    #------------------------------------------------------------------------------------
    # 2. Find all sorts of indices of the observations (year index, country, i)
    # round years at half-year
    cat("Observation years are grouped into their respective calendar years.\n")
    round.years.j <- floor(data$years.j) + 0.5
    # for obs j, find the country c and year index t
    gett.j <- round.years.j  # so t refers to midpoint of calendar year
    J <- length(gett.j)
    C <- length(country.info$iso.c)

    # getc.J: which UNIQUE obs years are there in country c?
    # to get getc.j,  watch out with as.numeric(data$name.j), gives order alphabetically!
    # so just do a loop!
    N.unique.c<-N.obsperiod.c<- rep(NA, C)

    #Get estimation years incl years in between obs years  #Change NC, 20160601
    #Need to specify the years here rather than calling from the data
    #Would be ok for the global data because the range would insure the inclusion of 1990
    #Due to range being different for country specifc runs need to specify range
    date.current<-Sys.Date()
    year.current<-as.numeric(format(date.current,'%Y')) + 1.5
    totestyears<-seq(1950.5,year.current) #Change NC, 20160807
    iso.j<-data$iso.j #Change NC, 20160602


    ####Get Years for model, begin first obs year or before 1990 end last obs year or after 1990 #Change NC, 20160807
    year.begin.c<-year.end.c<-year.begin.index<-year.end.index<-year.set.index<-upper.yrindex.c<-rep(NA,C)
    year.est.c<-array(NA,c(C,length(totestyears)))
    getstart.j<-floor(data$start.j)+0.5
    getend.j<-floor(data$end.j)+0.5
    #------------------------------------------------------------------------------------
    # only for pseudo data ####
    for (j in 1:length(getend.j)) {
      if (getend.j[j] == getstart.j[j]&getend.j[j]!=year.current)
        getend.j[j] = getend.j[j] + 1
    }

    for(c in 1:C){
      select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]

      year.begin.c[c]<-min(1985.5,min(getstart.j[select],gett.j[select])) ##Need at least a few years before start point
      year.begin.index[c]<-which(totestyears==year.begin.c[c])
      year.end.c[c]<-max(1995.5,max(getend.j[select],gett.j[select])) ##need at least a few years after start point
      year.end.index[c]<-which(totestyears==year.end.c[c])
      year.set.index[c]<-which(seq(year.begin.index[c],year.end.index[c])==(which(totestyears==1990.5))) #Index for where 1990 occurs in the observation period
      N.obsperiod.c[c]<-length(seq(year.begin.index[c],year.end.index[c])) ##Length of observation period used in the model run
      year.est.c[c,1:N.obsperiod.c[c]]<-seq(year.begin.c[c],year.end.c[c]) ##The years in the observation period
      }

  ##Get indexes for location of obs years in estimation period #Change NC, 20160807
   getest.j<-rep(NA,J)

    for(c in 1:C){
      select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
      getest.j[select]<-(match(gett.j[select],totestyears[seq(year.begin.index[c],year.end.index[c])]))
    }

   #------------------------------------------------------------------------------------
   #------------------------------------------------------------------------------------
   # if (usestartendyear) {
   # use start and end years for country-specific run // 20160302 cw ####
   start.j = data$start.j
   end.j = data$end.j
   #------------------------------------------------------------------------------------
   # only for pseudo data ####
   for (j in 1:length(end.j)) {
     if (end.j[j] == start.j[j])
       end.j[j] = end.j[j] + 1
   }
   #------------------------------------------------------------------------------------
   # for extended mean // 20160413 cw ####
   year.t = seq(min(floor(start.j)), max(ceiling(end.j)))

   Gett.i <- function(years.i, year.t) {
     gett.i <- rep(NA, length(years.i))
     for (i in 1:length(years.i)) {
       gett.i[i] <- which(unique(year.t) == years.i[i])
     }
     return(gett.i)
   }

   gett.start.j <- Gett.i(years.i = floor(start.j), year.t = year.t)
   gett.end.j <- Gett.i(years.i = ceiling(end.j - 1), year.t = year.t)

   X.j <- gett.end.j - gett.start.j + 1 # number of indices
   partialtime.xj <- matrix(NA, max(X.j), J) # store parital time for each index year f for obs j
   # how to calculate the weight of each unique observation year
   GetPartialTime <- function(start, end, X) {
     partialtime.x <- rep(NA, X)
     if (X == 1) {
       partialtime.x <- end - start
     } else {
       # end is at least one calendar year after start
       partialtime.x[1] <- 1 - (start - floor(start))
       partialtime.x[X] <- end - ceiling(end - 1)
       ### note to self: end.j[j] - floor(end.j[j]) is not correct: e.g. 1995.0-1997.0
       if (X > 2)
         partialtime.x[2:(X - 1)] <- 1
     }
     return(partialtime.x)
   }
   # period (total partial time) of each observation
   period.j = c()
   for (j in 1:J) {
     partialtime.xj[1:X.j[j], j] <- GetPartialTime(start = start.j[j], end = end.j[j], X = X.j[j])
     period.j[j] = sum(partialtime.xj[,j], na.rm = T)
   }
   # the number of years in each period
   getperiod.j = apply(partialtime.xj, 2, function(x) sum(!is.na(x)))

   # for pooled data ####
   getts.jf  = getis.jf = getest.jf = matrix(NA, nrow = J, ncol = max(getperiod.j))
   for (j in 1:J){
     getts.jf[j, 1:getperiod.j[j]] = seq(floor(start.j)[j], length.out = getperiod.j[j]) + 0.5 #midyear
   }

   periodsum<-rep(NA,C)
   for(c in 1:C){
     select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
     periodsum[c]<-sum(getperiod.j[select][getperiod.j[select]>1]-1)
   }

   #select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
   gett.ci = matrix(NA, C, (max(country.info$N.c)+max(periodsum)))
   # i here stands for observation years
   for (c in 1:C){
     select.obs <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
     select = seq(1,sum(getperiod.j[select.obs])) #[data$iso.j == country.info$iso.c[c]]
     N.unique.c[c] = length(c(na.omit(unique(c(na.omit(c(getts.jf[select.obs,]))))[select])))
     gett.ci[c, 1:N.unique.c[c]] <- sort(unique(c(na.omit(c(getts.jf[select.obs,]))))[select])
   }

   getest.ci <- matrix(NA, C, max(N.unique.c))
   for (c in 1:C) {
      getest.ci[c,1:N.unique.c[c]] <- match(gett.ci[c,!is.na(gett.ci[c,])],year.est.c[c,])
   }


   getc.j = rep(NA, sum(getperiod.j))
   for (c in 1:C){
     select.obs <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
     select = seq(1,sum(getperiod.j[select.obs]))
     getc.j[select] <- c
     getseq = which(!is.na(getts.jf[select.obs,]))
     getis.jf.temp = getest.jf.temp = matrix(NA, nrow = length(select.obs), ncol = max(getperiod.j))
     for (jc in 1:length(select)){
       getis.jf.temp[getseq[jc]] = which.max(c(na.omit(c(getts.jf[select.obs,])))[select][jc]==sort(unique(c(na.omit(c(getts.jf[select.obs,])))[select])))
       getest.jf.temp[getseq[jc]] = which(year.est.c[c,] == sort(unique(c(na.omit(c(getts.jf[select.obs,])))[select]))[which.max(c(na.omit(c(getts.jf[select.obs,])))[select][jc]==sort(unique(c(na.omit(c(getts.jf[select.obs,])))[select])))])
     }
     getis.jf[select.obs,]<-getis.jf.temp
     getest.jf[select.obs,]<-getest.jf.temp
   }

   # }
   #------------------------------------------------------------------------------------
   #------------------------------------------------------------------------------------

    geti.j <- getc.j <- rep(NA, J)
    for (c in 1:C){
      select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
      getc.j[select] <- c
      # to get i for obs j, find year and see which index it has...
      for (jc in 1:length(select)){
        geti.j[select[jc]] <- which.max(gett.j[select][jc]==sort(unique(gett.j[select])))
      }
    }
    # For AR: find the indices of the countries with more than 1 observation:
    getc.z <- seq(1, C)[N.unique.c>1]
    getc.x <- seq(1, C)[N.unique.c==1]
    n.countriesmorethan1obs <- length(getc.z)
    n.countries1obs<-length(getc.x)
    # For unmet: find # obs and indices to skip the missing observation j's
    N.unmet <- sum(!is.na(props.unmet.j))
      getj.unmet.k <- seq(1,J)[!is.na(props.unmet.j)]

      ## Classify observations for data model
      if(is.null(validation.list)) {
          getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
          getj.training.k <- seq(1, J)[!is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
          getj.test.k <- NULL
          getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
          getj.training.unmet.k <- seq(1, J)[!is.na(props.unmet.j)]
          getj.test.unmet.k <- NULL
          n.training.breakdown <- length(getj.training.k)
          n.training.tot <- length(getj.training.tot.k)
          n.training.modern <- length(getj.training.modern.k) # change JR, 20131120
          n.training.unmet <- length(getj.training.unmet.k)
          if(ModelFunctionModOnlyObs(write.model.fun)) {
              getj.training.modonly.k <-
                  seq(1, J)[!is.na(props.modern.j) & is.na(props.tot.j) & is.na(props.trad.j)]
              n.training.modonly <- length(getj.training.modonly.k)
          }
      }


    #------------------------------------------------------------------------------------
    # 3. Regional information
    # Note: reg.c and subreg.c are categorical variables/factors, so as.numeric gives their level
    n.reg <- length(unique(country.info$reg.c))
    n.subreg <- length(unique(country.info$subreg.c))
    subreg.c <- country.info$subreg.c
    reg.subreg <- rep(NA, n.subreg)
    for (subreg in 1:n.subreg){
      c <- which.max(country.info$subreg.c==subreg)
      reg.subreg[subreg] <- country.info$reg.c[c]
    }
    #country.info$name.c[country.info$dev.c=="Rich"]
    crich.index <- seq(1, C)[country.info$dev.c=="Rich"] # dev.c is factor
    n.rich <- length(crich.index)
    cnotrich.index <- seq(1, C)[country.info$dev.c!="Rich"]
      n.notrich <- length(cnotrich.index)

      ## Sexual Activity Model

      if("sex.ac.unm.c" %in% colnames(country.info)) {
          ## Create the indices (mimics 'crich', 'cnotrich')
          n.sex.ac.unm <- length(unique(country.info$sex.ac.unm.c))
          n.reg.in.sex.ac.unm <-
              length(unique(country.info$reg.in.sex.ac.unm.c))
          reg.in.sex.ac.unm.c <- country.info$reg.in.sex.ac.unm.c
          sex.ac.reg <- rep(NA, n.reg.in.sex.ac.unm)
          for(x in 1:n.reg.in.sex.ac.unm) {
              c <- which.max(country.info$reg.in.sex.ac.unm.c == x)
              sex.ac.reg[x] <- country.info$sex.ac.unm.c[c]
          }
          n.subreg.in.sex.ac.unm <-
              length(unique(country.info$subreg.in.sex.ac.unm.c))
          subreg.in.sex.ac.unm.c <- country.info$subreg.in.sex.ac.unm.c
          reg.in.sex.ac.unm.subreg <- rep(NA, n.subreg.in.sex.ac.unm) #The region the subregion is in
          for(x in 1:n.subreg.in.sex.ac.unm) {
              c <- which.max(country.info$subreg.in.sex.ac.unm.c == x)
              reg.in.sex.ac.unm.subreg[x] <- country.info$subreg.in.sex.ac.unm.c[c]
          }

          ## For subregion in SA1 / India alone
          n.reg.in.sex.ac.unm.SA1sub <-
              length(unique(country.info$reg.in.sex.ac.unm.SA1sub.c))
          reg.in.sex.ac.unm.SA1sub.c <- country.info$reg.in.sex.ac.unm.SA1sub.c
          sex.ac.unm.SA1sub.reg <- rep(NA, n.reg.in.sex.ac.unm.SA1sub)
          for(x in 1:n.reg.in.sex.ac.unm.SA1sub) {
              c <- which.max(country.info$reg.in.sex.ac.unm.SA1sub.c == x)
              sex.ac.unm.SA1sub.reg[x] <- country.info$sex.ac.unm.c[c] #same as for non 'SA1/India'
          }
          n.subreg.in.sex.ac.unm.SA1sub <-
              length(unique(country.info$subreg.in.sex.ac.unm.SA1sub.c))
          subreg.in.sex.ac.unm.SA1sub.c <- country.info$subreg.in.sex.ac.unm.SA1sub.c
          reg.in.sex.ac.unm.SA1sub.subreg <- rep(NA, n.subreg.in.sex.ac.unm.SA1sub) #The region the subregion is in
          for(x in 1:n.subreg.in.sex.ac.unm.SA1sub) {
              c <- which.max(country.info$subreg.in.sex.ac.unm.SA1sub.c == x)
              reg.in.sex.ac.unm.SA1sub.subreg[x] <- country.info$subreg.in.sex.ac.unm.SA1sub.c[c]
          }
      }

      ## Rich/Not rich indices
      crich.index <- seq(1, C)[country.info$dev.c=="Rich"] # dev.c is factor
      n.rich <- length(crich.index)
      cnotrich.index <- seq(1, C)[country.info$dev.c!="Rich"]
      n.notrich <- length(cnotrich.index)

      ## Mimic 'rich' 'norich'
      if("sex.ac.unm.c" %in% colnames(country.info)) {
          sex.ac.unm.index <- seq(1, C)[country.info$name.sex.ac.unm.c=="1"]
          n.sex.ac.unm.index <- length(sex.ac.unm.index)
          not.sex.ac.unm.index <- seq(1, C)[country.info$name.sex.ac.unm.c=="0"]
          n.not.sex.ac.unm.index <- length(not.sex.ac.unm.index)
      }


    #----------------------------------------------------------------------------------
    # 4. Data dummies
      ## This part replaced with the updated version from level
      ## model. Matches this section in `GetBugsData()`.

    ## NB: If isTRUE(validation.list$at.random.no.data) ||
    ## isTRUE(validation.list$leave.iso.out) these all need to be defined for
    ## the training obs (1, ..., J) and the test obs (J+1, ..., J+J.test).

    if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        data.test <- validation.at.random.no.data$data.test
        data.temp <- rbind(data, data.test)
        props.tot.j.temp <- c(props.tot.j, data.test$props.tot.j)
    } else {
        data.temp <- data
        props.tot.j.temp <- props.tot.j
    }

    ## Sources in 'names.sources' are:
    ## 1. DHS
    ## 2. MICS
    ## 3. NS (national survey)
    ## 4. Other (Other international survey)
    ## 5. Repeated national survey [not used]
    ## 6. PMA
    ## 7. SS (service statistics)

    if(disagg.RN.PMA) {                   #[MCW-2016-03-24-8]
        source.ind.j <- ifelse(data.temp$source.j==names.sources[1],1,
                        ifelse(data.temp$source.j==names.sources[2],2,
                        ifelse(data.temp$source.j==names.sources[3],3,
                        ifelse(data.temp$source.j==names.sources[5],5,
                        ifelse(data.temp$source.j==names.sources[6],6,  # change JR, 20131120
                               ## [MCW-2018-03-31] FIX sent by Niamh (31/3/2018) Source type '7' is not a valid source type in any of the BUGS models.
                               ## [MCW-2018-04-02] CHANGE; set 'source.ind.j' to '4' if 'data.temp$source.j' is '4'. Was being set to '7'.
                        ifelse(data.temp$source.j==names.sources[7],4,4))))))
    } else {
        source.ind.j <- ifelse(data.temp$source.j==names.sources[1],1,
                        ifelse(data.temp$source.j==names.sources[2],2,
                        ifelse(data.temp$source.j==names.sources[3],3,
                        ifelse(data.temp$source.j==names.sources[4],4,
                        ifelse(data.temp$source.j==names.sources[5],5
                              ,4     #[MCW-2016-04-26-1]
                               )))))  # change JR, 20131120
    }

    ## There should be no 'RN' values (repeated national survey) ; this source
    ## name has been deprecated. If there are any, issue an error:
    RN.rows <- data.temp$source.j == names.sources[5]
    if(any(RN.rows)) {
        stop("Data source type 'RN' ('repeated national survey') is depracated but is used for observations "
            ,paste(which(RN.rows), collapse = ", ")
            ,".")
    }
    ## NOW: Use value 5 for the obs with CP Any less than 1 percent,
    ## irrespective of the value of 'disagg.RN.PMA'. This is done here because
    ## the BUGS models use 'source.ind.j' to define source types for the the
    ## variance model. 'source.ind.j' is not used anywhere else, in particular
    ## it is not used in the plotting functions.

    if(ModelFunctionLT1pcOwnSource(write.model.fun)) {
        source.ind.j[data.temp$less.than.1.pc] <- 5
        }

    ## for unmet, only DHS versus non-DHS
    source.ind.unmet.j <- ifelse(data.temp$source.unmet.j==names.sources.unmet[1], 1, 2)

    ## ind for SA, EMAL, HW, age refers to a 0,1,2,3 etc indicator, (O = not applicable)
    ## note that within countries, only one multiplier is assigned
    ## approach for those categories is to use "" for the first category
    ## followed by a new level for each additional country
    ## use as.numeric for factors, then 1 corresponds to first level
    ## (alphabetically ordered, thus "" first)
    ## levels(as.factor(c("", "   ")))
    ## levels(as.factor(c("", "0")))


    ## PERTURBATION MULTIPLIERS

    ## _Married Women_

    ## |--------------------------------+-------+--------------------------|
    ## | Category                       | Abbr. | Stem (GetBugsData())     |
    ## |                                |       | values: <stem>.factor    |
    ## |                                |       | indicator: <stem>.ind.j  |
    ## |                                |       | no. of cats: ncat.<stem> |
    ## |--------------------------------+-------+--------------------------|
    ## | geographical region            |       | geo                      |
    ## | ever-married/all women         | EMAL  | emal                     |
    ## | Husband/wives, both sexes      | HW    | hw                       |
    ## | all sexually active women      | SA    | sa                       |
    ## | non-preg/fert/married SA women |       | posbias                  |
    ## | age group -ve bias             |       | negage                   |
    ## | age group +ve bias             |       | posage                   |
    ## | age group different            |       | age                      |
    ## |--------------------------------+-------+--------------------------|

    ## _UNmarried Women_

    ## |--------------------------+------+----------------------------------------+---------------+-----------+--------------------------+----------------------|
    ## |                          |      |                                        | Value         |           | Stem (GetBugsData())     |                      |
    ## |                          |      |                                        | in col.('s)   | Var.      | values: <stem>.factor    | Label                |
    ## |                          |      |                                        | in input      | in        | indicator: <stem>.ind.j  | in                   |
    ## | Category                 | Dir. | Col.('s) in input data                 | data          | data      | no. of cats: ncat.<stem> | [...]Multipliers.pdf |
    ## |--------------------------+------+----------------------------------------+---------------+-----------+--------------------------+----------------------|
    ## | With partner only        | +    | Population.type                        | PW            | poptype.j | hw                       | WP, trad/mod         |
    ## | Sterilization only       | +    | Population.type                        | FM            | poptype.j | emal                     | SO, trad/mod         |
    ## | Geographical region      | ?    | GEO.biases..unknown.direction.         | [non-miss]/'' | geo.j     | geo                      | geo, trad/mod        |
    ## | Higher risk of pregnancy | +    | Non.pregnant.and.other.positive.biases | +             | posbias.j | posbias                  | +, trad/mod          |
    ## | Age group with - bias    | -    | age.cat.bias                           | -             | age.cat.j | negage                   | -Age, trad/mod       |
    ## | Age group + bias         | +    | age.cat.bias                           | +             | age.cat.j | posage                   | +Age, trad/mod       |
    ## | Age group different      | ?    | age.cat.bias                           | ?             | age.cat.j | age                      | age, trad/mod        |
    ## |--------------------------+------+----------------------------------------+---------------+-----------+--------------------------+----------------------|


    ## note: list of multipliers is verified using
    ## par.V <- GetParnamesV(winbugs.data = winbugs.data, name.short.j = MakeCountryNamesShort(data$name.j))
    ## par.V$parnames.V.in.bugs
    ## par.V$parnames.V.nice
    ## data.frame(par.V$parnames.V.in.bugs,  par.V$parnames.V.nice)

    ## note: each of these vectors are augmented with a (J+1)th element containing the baseline category
    ## such that there are no errors when the baseline category is not present! # change JR, 2013111

    select <- !is.na(props.tot.j.temp) # change JR, 20131120

    sa.factor <- as.factor(c(ifelse(data.temp$poptype.j=="SA" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    sa.ind.j <- as.numeric(sa.factor)
    ncat.sa <- nlevels(sa.factor)

    emal.factor <- as.factor(c(ifelse( (data.temp$poptype.j=="EM"|data.temp$poptype.j=="AL"|data.temp$poptype.j=="FM") & select, data.temp$name.j, ""), "")) # change JR, 20131120
    emal.ind.j <- as.numeric(emal.factor)
    ncat.emal <- nlevels(emal.factor)
    ## NB: 'poptype.j == "EM"' also includes UWRA population type 'FM' (formerly
    ## married).

    hw.factor <- as.factor(c(ifelse((data.temp$poptype.j=="HW"|data.temp$poptype.j=="PW") & select, data.temp$name.j, ""), "")) # change JR, 20131120
    hw.ind.j <- as.numeric(hw.factor)
    ncat.hw <- nlevels(hw.factor)
    ## NB: 'poptype.j == "HW"' also includes UWRA population type 'PW'
    ## (partnered women).

    age.factor <- as.factor(c(ifelse(data.temp$age.cat.j=="?" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    age.ind.j <- as.numeric(age.factor)
    ncat.age <- nlevels(age.factor)

    posage.factor <- as.factor(c(ifelse(data.temp$age.cat.j=="+" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    posage.ind.j <- as.numeric(posage.factor)
    ncat.posage <- nlevels(posage.factor)

    negage.factor <- as.factor(c(ifelse(data.temp$age.cat.j=="-" & select, data.temp$name.j, ""), "")) # change JR, 20131120
    negage.ind.j <- as.numeric(negage.factor)
    ncat.negage <- nlevels(negage.factor)

    ##<< Combine iso code/name with the explanation of the subgroup for geo and posbias
    ## (such that different multipliers are added, e.g. if different geo regions used within one country)
    geo.factor <- as.factor(c(ifelse(data.temp$geo.j!="" & select,
                                     paste(data.temp$name.j, data.temp$geo.j, sep = ": "), ""), "")) # change JR, 20131120
    geo.ind.j <- as.numeric(geo.factor)
    ncat.geo <- nlevels(geo.factor)

    posbias.factor <- as.factor(c(ifelse(data.temp$posbias.j!="" & select,
                                         paste(data.temp$name.j, data.temp$posbias.j, sep = ": "), ""), "")) # change JR, 20131120
    posbias.ind.j <- as.numeric(posbias.factor)
    ncat.posbias <- nlevels(posbias.factor)

    ## ind1 refers to a 0/1 indicator, (1 = Yes)
    ## For biases:
    abs.probe.q.ind1.j <- ifelse(data.temp$probe.bias.j == "1" & select, 1,0)
    folk.ind1.j <- ifelse(data.temp$folkbias.j != "" & select, 1,0) # change JR, 20131120
    mneg.ind1.j <- ifelse(data.temp$mod.bias.j == "-" & select, 1,0) # change JR, 20131120
    mpos.ind1.j <- ifelse(data.temp$mod.bias.j == "+" & select, 1,0) # change JR, 20131120

    ##  if (return.Vinfo){
    ##     return(list(posbias.factor = unique(posbias.factor),
    ##                 geo.factor = unique(geo.factor),
    ##                 negage.factor = unique(negage.factor),
    ##                 posage.factor = unique(posage.factor),
    ##                 age.factor = unique(age.factor),
    ##                 hw.factor = unique(hw.factor),
    ##                 sa.factor = unique(sa.factor),
    ##                 emal.factor = unique(emal.factor)
    ##                 ))
    ##   }

      ##----------------------------------------------------------------------------------
      ## 5. Countries with no data
      ## [MCW-2016-08-25-3] Create indices and such for countries with no data

    ## [MCW-2016-08-25-5] If including countries with no data, need to include any
    ## (sub)regions with no data as well. Will define 'C', 'crich.index' etc for
    ## no data countries later.

    if(include.c.no.data || isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        ## 5.1 Indices
        ## for obs j, find the country c and year index t
        C.no.data <- length(country.info.no.data$iso.c)

      ## 5.2a Regional information
      ## Note: reg.c and subreg.c are categorical variables/factors, so
        ## as.numeric gives their level

        ## Re-define 'n.reg', 'n.subreg', 'subreg.c'
        all.reg <- factor(c(country.info$namereg.c, country.info.no.data$namereg.c))
        n.reg <- length(levels(all.reg))
        reg.c <- as.numeric(all.reg)

        all.subreg <- factor(c(country.info$namesubreg.c, country.info.no.data$namesubreg.c))
        n.subreg <- length(levels(all.subreg))
        subreg.c <- as.numeric(all.subreg)

        ## Using redefinitions, redefine 'reg.subreg', 'reg.subreg'
        reg.subreg <- rep(NA, n.subreg)       #The region the subregion is in
        for (subreg in 1:n.subreg) {
            x <- which.max(subreg.c == subreg)
            reg.subreg[subreg] <- reg.c[x]
        }
        ## Re definitions for sexual activity
        if("sex.ac.unm.c" %in% colnames(country.info)) {
            n.sex.ac.unm <-
                length(unique(c(country.info$sex.ac.unm.c, country.info.no.data$sex.ac.unm.c)))

            ## regions in sexual activity categories
            all.reg.in.sex.ac.unm <-
                factor(c(country.info$name.reg.in.sex.ac.unm.c
                       ,country.info.no.data$name.reg.in.sex.ac.unm.c))
            n.reg.in.sex.ac.unm <- length(levels(all.reg.in.sex.ac.unm))
            reg.in.sex.ac.unm.c <- as.numeric(all.reg.in.sex.ac.unm)
            sex.ac.reg <- rep(NA, n.reg.in.sex.ac.unm)
            for(x in 1:n.reg.in.sex.ac.unm) {
                c <- which.max(reg.in.sex.ac.unm.c == x)
                sex.ac.reg[x] <-
                    c(country.info$sex.ac.unm.c, country.info.no.data$sex.ac.unm.c)[c]
            }
            all.subreg.in.sex.ac.unm <-
                factor(c(country.info$name.subreg.in.sex.ac.unm.c
                       ,country.info.no.data$name.subreg.in.sex.ac.unm.c))
            n.subreg.in.sex.ac.unm <- length(levels(all.subreg.in.sex.ac.unm))
            subreg.in.sex.ac.unm.c <- as.numeric(all.subreg.in.sex.ac.unm)
            reg.in.sex.ac.unm.subreg <- rep(NA, n.subreg.in.sex.ac.unm)
            for(x in 1:n.subreg.in.sex.ac.unm) {
                c <- which.max(subreg.in.sex.ac.unm.c == x)
                reg.in.sex.ac.unm.subreg[x] <-
                    c(country.info$subreg.in.sex.ac.unm.c, country.info.no.data$subreg.in.sex.ac.unm.c)[c]
            }

            ## For subregion in SA1 / India alone
            all.reg.in.sex.ac.unm.SA1sub <-
                factor(c(country.info$name.reg.in.sex.ac.unm.SA1sub.c
                        ,country.info.no.data$name.reg.in.sex.ac.unm.SA1sub.c))
            n.reg.in.sex.ac.unm.SA1sub <- length(levels(all.reg.in.sex.ac.unm.SA1sub))
            reg.in.sex.ac.unm.SA1sub.c <- as.numeric(all.reg.in.sex.ac.unm.SA1sub)
            sex.ac.unm.SA1sub.reg <- rep(NA, n.reg.in.sex.ac.unm.SA1sub)
            for(x in 1:n.reg.in.sex.ac.unm.SA1sub) {
                c <- which.max(reg.in.sex.ac.unm.SA1sub.c == x)
                sex.ac.unm.SA1sub.reg[x] <-
                    c(country.info$sex.ac.unm.c, country.info.no.data$sex.ac.unm.c)[c]
            }
            all.subreg.in.sex.ac.unm.SA1sub <-
                factor(c(country.info$name.subreg.in.sex.ac.unm.SA1sub.c
                        ,country.info.no.data$name.subreg.in.sex.ac.unm.SA1sub.c))
            n.subreg.in.sex.ac.unm.SA1sub <- length(levels(all.subreg.in.sex.ac.unm.SA1sub))
            subreg.in.sex.ac.unm.SA1sub.c <- as.numeric(all.subreg.in.sex.ac.unm.SA1sub)
            reg.in.sex.ac.unm.SA1sub.subreg <- rep(NA, n.subreg.in.sex.ac.unm.SA1sub)
            for(x in 1:n.subreg.in.sex.ac.unm.SA1sub) {
                c <- which.max(subreg.in.sex.ac.unm.SA1sub.c == x)
                reg.in.sex.ac.unm.SA1sub.subreg[x] <-
                    c(country.info$subreg.in.sex.ac.unm.SA1sub.c, country.info.no.data$subreg.in.sex.ac.unm.SA1sub.c)[c]
            }
        }

        ## 5.3 Rich/not rich indices
        ##country.info.no.data$name.c[country.info.no.data$dev.c=="Rich"]
        crich.index.no.data <- C +                                #start at C + 1
            seq(1, C.no.data)[country.info.no.data$dev.c=="Rich"] # dev.c is factor
        n.rich.no.data <- length(crich.index.no.data)
        cnotrich.index.no.data <- C +     #start at C+1
            seq(1, C.no.data)[country.info.no.data$dev.c!="Rich"]
        n.notrich.no.data <- length(cnotrich.index.no.data)

        ## 5.4 Sexually active indices
        if("sex.ac.unm.c" %in% colnames(country.info.no.data)) {
            sex.ac.unm.index.no.data <- C +
                seq(1, C.no.data)[country.info.no.data$name.sex.ac.unm.c=="1"]
            n.sex.ac.unm.index.no.data <- length(sex.ac.unm.index.no.data)
            not.sex.ac.unm.index.no.data <- C +
                seq(1, C.no.data)[country.info.no.data$name.sex.ac.unm.c=="0"]
            n.not.sex.ac.unm.index.no.data <- length(not.sex.ac.unm.index.no.data)
        }

        ##[MCW-2016-08-26-3] 'N.unique.c' extended for countries no data, which
        ##are given /one/ "datum".
        ## [MCW-2017-03-14-1] :: Commented out
        if(!isTRUE(validation.list$at.random.no.data) && !isTRUE(validation.list$leave.iso.out)) {
            N.unique.c <- c(N.unique.c, rep(1, C.no.data))
            ## [MCW-2017-03-14-1] :: Need 'N.unique.c.test' as well.
            N.unique.c.test <- rep(1, C.no.data)
            ## [MCW-2017-03-14-2] :: Need 'gett.ci.test' as well. It needs to
            ## contain 'mean.TOneLevel' or 'mean.Tworld', depending on whether the
            ## country is 'rich' or 'notrich'.
            ## gett.ci.test: the SORTED indices of the obs years for i = 1, ..,
            ## N.unique.c.test[c] (for AR(1))
            ## Note: i does not relate to the observations anymore, just the unique years!
            gett.ci.test <- matrix(NA, C.no.data, 1)
            if(ModelFunctionSATiming(write.model.fun)) {
                gett.ci.test[not.sex.ac.unm.index.no.data - C,] <- timing.world.priors$mean.TOneLevel
                gett.ci.test[sex.ac.unm.index.no.data - C,] <- timing.world.priors$mean.Tworld
                } else {
            gett.ci.test[crich.index.no.data - C,] <- timing.world.priors$mean.TOneLevel
            gett.ci.test[cnotrich.index.no.data - C,] <- timing.world.priors$mean.Tworld
            }
        }
        ## 5.5 For 'set level' parameters used in the rate model.
        ## I think only the 1990.5 elements are needed.
        if(!isTRUE(validation.list$at.random.no.data) && !isTRUE(validation.list$leave.iso.out)) {
            year.begin.c.test <- year.begin.index.test <- year.end.c.test <-
                year.end.index.test <- N.obsperiod.c.test <- year.set.index.test <-
                rep(NA, C.no.data)
            year.est.c.test <- array(NA, c(C.no.data, length(totestyears)))

            for(c in 1:C.no.data){

                year.begin.c.test[c]<-1985.5
                year.begin.index.test[c]<-which(totestyears==year.begin.c.test[c])
                year.end.c.test[c]<-1995.5
                year.end.index.test[c]<-which(totestyears==year.end.c.test[c])
                year.set.index.test[c]<-which(seq(year.begin.index.test[c],year.end.index.test[c])==(which(totestyears==1990.5)))
                                # Index for where 1990 occurs in the observation period
                N.obsperiod.c.test[c]<-length(seq(year.begin.index.test[c],year.end.index.test[c])) # Length of observation period used in the model run
                year.est.c.test[c,1:N.obsperiod.c.test[c]]<-seq(year.begin.c.test[c],year.end.c.test[c]) # The years in the observation period
            }
            }

        message(C, " countries with data\n", C.no.data, " countries with no data")
    }

      ##----------------------------------------------------------------------
      ## 6. Extra counters and indices for validations

      ##----------------------------------------------------------------------
      ## 6a. 'exclude.unmet.only'

      ## (Copied from 'GetBugsData()' -- level model version)

      if(isTRUE(validation.list$exclude.unmet.only)) {
          getj.test.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)], getj.training.k)
          getj.training.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)],getj.test.unmet.k)
          n.training.unmet <- length(getj.training.unmet.k)
          n.test.unmet <- length(getj.test.unmet.k)
                                # include all total/breakdown in training!!!
          getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
          getj.training.k <- seq(1, J)[!is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
          n.training.tot <- length(getj.training.tot.k)
          n.training.breakdown <- length(getj.training.k)
          getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
          n.training.modern <- length(getj.training.modern.k) # change JR, 20131120

          N.unmet.test <- n.test.unmet #UWRA JAGS code uses this exclusively

          if(ModelFunctionModOnlyObs(write.model.fun)) {
              getj.training.modonly.k <-
                  seq(1, J)[!is.na(props.modern.j) & is.na(props.tot.j) & is.na(props.trad.j)]
              n.training.modonly <- length(getj.training.modonly.k)
          }
      }

      ##----------------------------------------------------------------------
      ## 6b. Specials for 'at.random', 'at.end', 'exclude.unmet.only'

      ## (Copied from 'GetBugsData()' -- level model version)

      if(isTRUE(validation.list$at.random) || isTRUE(validation.list$at.end)) {
          getj.test.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)], getj.training.k)
          getj.training.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)],getj.test.unmet.k)
          n.training.unmet <- length(getj.training.unmet.k)
          n.test.unmet <- length(getj.test.unmet.k)
          getj.test.k <- setdiff(seq(1, J)[!is.na(logratio.ymodern.j) & !is.na(props.tot.j)], getj.training.k) # change JR, 20131120
          n.training.breakdown <- length(getj.training.k)
          n.test.breakdown <- length(getj.test.k)
          getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
          n.training.tot <- length(getj.training.tot.k)
          getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
          n.training.modern <- length(getj.training.modern.k) # change JR, 20131120

          N.unmet.test <- n.test.unmet #UWRA model JAGS code uses this exclusively

          if(ModelFunctionModOnlyObs(write.model.fun)) {
              getj.training.modonly.k <-
                  seq(1, J)[!is.na(props.modern.j) & is.na(props.tot.j) & is.na(props.trad.j)]
              n.training.modonly <- length(getj.training.modonly.k)
          }
      }

      ##----------------------------------------------------------------------
      ## 6c. 'at.random.no.data' and 'leave.iso.out' validations

      if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {

          n.training.breakdown <- length(getj.training.k)
          getj.training.tot.k <- seq(1, J)[is.na(logratio.ymodern.j) & !is.na(props.tot.j)] # change JR, 20131120
          n.training.tot <- length(getj.training.tot.k)
          getj.training.modern.k <- seq(1, J)[data$source.j == "SS"] # change JR, 20140612
          n.training.modern <- length(getj.training.modern.k) # change JR, 20131120

          props.tot.j.test <- data.test$props.tot.j
          props.modern.j.test <- data.test$props.modern.j
          props.trad.j.test <- data.test$props.trad.j
          props.unmet.j.test <- data.test$props.unmet.j

          logratio.ymodern.j.test <- log(props.modern.j.test/(1-props.tot.j.test))
          logratio.ytrad.j.test <- log(props.trad.j.test/(1-props.tot.j.test))
          logit.ytot.j.test <- log(props.tot.j.test/(1-props.tot.j.test))
          logitratio.yunmet.j.test <- logit(props.unmet.j.test/(1-props.tot.j.test))

          y.modern.j.test <- props.modern.j.test # change JR, 20131120 # SS obs selected for in model using getj.training.modern.k
          se.modern.j.test <- rep(0.025, length(y.modern.j.test)) # change JR, 20140806

          if(ModelFunctionSurveySEs(write.model.fun)) {
              ## [MCW 2018-11-16] Added this 'if' clause for
              ## consistency. There is only one rate model and it doesn't
              ## work without SEs.
              ##Info on SEs #Change NC, 20161218
              se.logR.trad.impute.test<-validation.at.random.no.data$se.info.j.test$se.logR.trad.impute
              se.logR.modern.impute.test<-validation.at.random.no.data$se.info.j.test$se.logR.modern.impute
              se.logR.unmet.impute.test<-validation.at.random.no.data$se.info.j.test$se.logR.unmet.impute
          }

          if(ModelFunctionModOnlyObs(write.model.fun)) {
              getj.training.modonly.k <-
                  seq(1, J)[!is.na(props.modern.j) & is.na(props.tot.j) & is.na(props.trad.j)]
              n.training.modonly <- length(getj.training.modonly.k)
          }

          round.years.j.test <- floor(data.test$years.j) + 0.5
          ##for obs j, find the country c and year index t
          gett.j.test <- round.years.j.test  # so t refers to midpoint of calendar year
          J.test <- length(gett.j.test)

          getj.test.unmet.k <- seq(1,J.test)[!is.na(props.unmet.j.test)] + J #<<!!!!
          n.test.unmet <- sum(!is.na(props.unmet.j.test))
          N.unmet.test <- n.test.unmet #UWRA model JAGS code uses this exclusively

          getj.test.k <- seq(1, J.test) + J #<<!!!!
          n.test.breakdown <- sum(!is.na(props.modern.j.test))

          getj.training.unmet.k <- setdiff(seq(1, J)[!is.na(props.unmet.j)],getj.test.unmet.k)
          n.training.unmet <- length(getj.training.unmet.k)

          ##getc.J: which UNIQUE obs years are there in country c?
          ##to get getc.j,  watch out with as.numeric(data$name.j), gives order alphabetically!
          ##so just do a loop!
          N.unique.c.test<-N.obsperiod.c.test<- rep(NA, C.no.data)

          iso.j.test<-data.test$iso.j #Change NC, 20160602

          ## Get Years for model, begin first obs year or before 1990 end last obs year or after 1990 #Change NC, 20160807
          year.begin.c.test<-year.end.c.test<-year.begin.index.test<-year.end.index.test<-year.set.index.test<-upper.yrindex.c.test<-rep(NA,C.no.data)
          year.est.c.test<-array(NA,c(C,length(totestyears)))
          getstart.j.test<-floor(data.test$start.j)+0.5
          getend.j.test<-floor(data.test$end.j)+0.5
          ##-----------------------------------------------------------------------------------
          ##only for pseudo data ####
          for (j in 1:length(getend.j.test)) {
              if (getend.j.test[j] == getstart.j.test[j]&getend.j.test[j]!=year.current)
                  getend.j.test[j] = getend.j.test[j] + 1
          }

          for(c in 1:C.no.data){
              select <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]

              year.begin.c.test[c]<-min(1985.5,min(getstart.j.test[select],gett.j.test[select])) ##Need at least a few years before start point
              year.begin.index.test[c]<-which(totestyears==year.begin.c.test[c])
              year.end.c.test[c]<-max(1995.5,max(getend.j.test[select],gett.j.test[select])) ##need at least a few years after start point
              year.end.index.test[c]<-which(totestyears==year.end.c.test[c])
              year.set.index.test[c]<-which(seq(year.begin.index.test[c],year.end.index.test[c])==(which(totestyears==1990.5))) #Index for where 1990 occurs in the observation period
              N.obsperiod.c.test[c]<-length(seq(year.begin.index.test[c],year.end.index.test[c])) ##Length of observation period used in the model run
              year.est.c.test[c,1:N.obsperiod.c.test[c]]<-seq(year.begin.c.test[c],year.end.c.test[c]) ##The years in the observation period
          }

          ##Get indexes for location of obs years in estimation period #Change NC, 20160807
          getest.j.test<-rep(NA,J.test)

          for(c in 1:C.no.data){
              select <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
              getest.j.test[select]<-(match(gett.j.test[select],totestyears[seq(year.begin.index.test[c],year.end.index.test[c])]))
          }

          ##-----------------------------------------------------------------------------------
          ##-----------------------------------------------------------------------------------
          ##if (usestartendyear) {
          ##use start and end years for country-specific run // 20160302 cw ####
          start.j.test = data.test$start.j
          end.j.test = data.test$end.j
          ##-----------------------------------------------------------------------------------
          ##only for pseudo data ####
          for (j in 1:length(end.j.test)) {
              if (end.j.test[j] == start.j.test[j])
                  end.j.test[j] = end.j.test[j] + 1
          }
          ##-----------------------------------------------------------------------------------
          ##for extended mean // 20160413 cw ####
          year.t.test = seq(min(floor(start.j.test)), max(ceiling(end.j.test)))

          gett.start.j.test <- Gett.i(years.i = floor(start.j.test), year.t = year.t.test)
          gett.end.j.test <- Gett.i(years.i = ceiling(end.j.test - 1), year.t = year.t.test)

          X.j.test <- gett.end.j.test - gett.start.j.test + 1 # number of indices
          partialtime.xj.test <- matrix(NA, max(X.j.test), J.test) # store parital time for each index year f for obs j
          ## period (total partial time) of each observation
          period.j.test = c()
          for (j in 1:J.test) {
              partialtime.xj.test[1:X.j.test[j], j] <- GetPartialTime(start = start.j.test[j], end = end.j.test[j], X = X.j.test[j])
              period.j.test[j] = sum(partialtime.xj.test[,j], na.rm = T)
          }
          ##the number of years in each period
          getperiod.j.test = apply(partialtime.xj.test, 2, function(x) sum(!is.na(x)))

          ##for pooled data ####
          getts.jf.test  = getis.jf.test = getest.jf.test = matrix(NA, nrow = J.test, ncol = max(getperiod.j.test))
          for (j in 1:J.test){
              getts.jf.test[j, 1:getperiod.j.test[j]] = seq(floor(start.j.test)[j], length.out = getperiod.j.test[j]) + 0.5 #midyear
          }

          periodsum.test<-rep(NA,C.no.data)
          for(c in 1:C.no.data){
              select <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
              periodsum.test[c]<-sum(getperiod.j.test[select][getperiod.j.test[select]>1]-1)
          }

          ##elect <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
          gett.ci.test = matrix(NA, C, (max(country.info.no.data$N.c)+max(periodsum.test)))
          ##i here stands for observation years
          for (c in 1:C.no.data){
              select.obs <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
              select = seq(1,sum(getperiod.j.test[select.obs])) #[data$iso.j == country.info$iso.c[c]]
              N.unique.c.test[c] = length(c(na.omit(unique(c(na.omit(c(getts.jf.test[select.obs,]))))[select])))
              gett.ci.test[c, 1:N.unique.c.test[c]] <- sort(unique(c(na.omit(c(getts.jf.test[select.obs,]))))[select])
          }

          getest.ci.test <- matrix(NA, C, max(N.unique.c.test))
          for (c in 1:C.no.data) {
              getest.ci.test[c,1:N.unique.c.test[c]] <- match(gett.ci.test[c,!is.na(gett.ci.test[c,])],year.est.c.test[c,])
          }


          getc.j.test = rep(NA, sum(getperiod.j.test))
          for (c in 1:C.no.data){
              select.obs <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
              select = seq(1,sum(getperiod.j.test[select.obs]))
              getc.j.test[select] <- c
              getseq.test = which(!is.na(getts.jf.test[select.obs,]))
              getis.jf.temp = getest.jf.temp = matrix(NA, nrow = length(select.obs), ncol = max(getperiod.j.test))
              for (jc in 1:length(select)){
                  getis.jf.temp[getseq.test[jc]] = which.max(c(na.omit(c(getts.jf.test[select.obs,])))[select][jc]==sort(unique(c(na.omit(c(getts.jf.test[select.obs,])))[select])))
                  getest.jf.temp[getseq.test[jc]] = which(year.est.c.test[c,] == sort(unique(c(na.omit(c(getts.jf.test[select.obs,])))[select]))[which.max(c(na.omit(c(getts.jf.test[select.obs,])))[select][jc]==sort(unique(c(na.omit(c(getts.jf.test[select.obs,])))[select])))])
              }
              getis.jf.test[select.obs,]<-getis.jf.temp
              getest.jf.test[select.obs,]<-getest.jf.temp
          }

          ##}
          ##-----------------------------------------------------------------------------------
          ##-----------------------------------------------------------------------------------

          geti.j.test <- getc.j.test <- rep(NA, J.test)
          for (c in 1:C.no.data){
              select <- seq(1, J.test)[data.test$iso.j == country.info.no.data$iso.c[c]]
              getc.j.test[select] <- c + C ##<<!!!
          }
          ##For AR: find the indices of the countries with more than 1 observation:
          getc.z.test <- seq(1, C.no.data)[N.unique.c.test>1] + C ##<<!!!
          getc.x.test <- seq(1, C.no.data)[N.unique.c.test==1] + C ##<<!!!
          n.countriesmorethan1obs.test <- length(getc.z.test)
          n.countries1obs.test<-length(getc.x.test)

      }


      ## ----------------------------------------------------------------------
      ## BEGIN CREATION OF OUTPUT LISTS

    #  if (return.Vinfo){
    #     return(list(posbias.factor = unique(posbias.factor),
    #                 geo.factor = unique(geo.factor),
    #                 negage.factor = unique(negage.factor),
    #                 posage.factor = unique(posage.factor),
    #                 age.factor = unique(age.factor),
    #                 hw.factor = unique(hw.factor),
    #                 sa.factor = unique(sa.factor),
    #                 emal.factor = unique(emal.factor)
    #                 ))
    #   }
    ##details<< list.no.validation contains
    ##describe<<
    list.no.validation <- list(
      N.unmet = N.unmet, ##<<count unmet
      getj.unmet.k = getj.unmet.k, ##<< indices unmet
      pmid.for.unmet = 0.4, ##<< constant in model unmet
      logitratio.yunmet.j = logitratio.yunmet.j,  ##<< logit(unmet/1-total) observed
      ratios.trad.modern.jn = as.matrix(cbind(logratio.ytrad.j, logratio.ymodern.j)),##<< matrix, observed logit(trad/tot, mod/tot))
      logit.ymodonly.j = logit.ymodonly.j,
      logit.ytot.j = logit.ytot.j,##<< logit(tot/none) observed
      se.logR.trad.impute=se.logR.trad.impute, #Change NC, 20170102
      se.logR.modern.impute=se.logR.modern.impute, #Change NC, 20170102
      se.logR.unmet.impute=se.logR.unmet.impute, #Change NC, 20170102
      med.max.trad.se=se.info.j$med.max.trad.se, #Change NC, 20170130
      med.max.modern.se=se.info.j$med.max.modern.se, #Change NC, 20170130
      med.max.unmet.se=se.info.j$med.max.unmet.se, #Change NC, 20170130
      y.modern.j = y.modern.j, ##<< observations of modern from SS # change JR, 20131121
      se.modern.j = se.modern.j, ##<< SE of observations of modern from SS (set to 2.5 percent) # change JR, 20131120
      getc.z = getc.z, ##<< indices of countries with more than 1 observation (for AR loop)
      n.countriesmorethan1obs = n.countriesmorethan1obs, ##<< count
      getc.j = getc.j, ##<< country index for each obs index j
      gett.j = gett.j,
      N.obsperiod.c=c(N.obsperiod.c), ##<<number of years in obs period per country #change NC, 20160601
      year.begin.c=year.begin.c, #Change NC, 20160807
      year.end.c=year.end.c, #Change NC, 20160807
      year.est.c=year.est.c,#Change NC, 20160807
      year.set.index=year.set.index,#Change NC, 20160807
      getc.x=getc.x, #Change NC, 20160810
      n.countries1obs=n.countries1obs, #Change NC, 20160810
      gett.ci = gett.ci, ##<< year index for obs i in country c
      geti.j = geti.j, ##<< index of obs year in country c
      getest.j=getest.j, ##<< index of obs year in vector of est years #Change NC, 20160807
      C = C, ##<< no of countries
      J = J, ##<< no of observations (total non-missing observations on total contraceptive use)
      iso.j=iso.j, ##Change NC, 20160602
      crich.index = crich.index,##<< indices of developED (rich) countries
      cnotrich.index = cnotrich.index, ##<< indices of developING (notrich) countries
      n.rich = n.rich, ##<< no of developed countries
      n.notrich = n.notrich,##<< no of developing countries
      reg.subreg = reg.subreg, ##<< region index for each subregion
      n.subreg = n.subreg,##<< no of subregions
      n.reg = n.reg, ##<< no of regions
      subreg.c = subreg.c,##<< subregion index for each country
      source.ind.unmet.j = source.ind.unmet.j, ##<< indicator for unmet source (1,2)
      source.ind.j = source.ind.j,##<< indicator for source (1,2,3,4)
      sa.ind.j = sa.ind.j, ##<< indicator for SA women (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.sa = ncat.sa,##<< 1 + no of permutation parameters
      posbias.ind.j = posbias.ind.j, ##<< indicator for pos bias (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.posbias = ncat.posbias,##<< 1 + no of permutation parameters
      age.ind.j = age.ind.j, ##<< indicator for age (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.age = ncat.age,##<<1 + no of permutation parameters
      hw.ind.j = hw.ind.j, ##<< indicator for HW (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.hw = ncat.hw,##<<1 + no of permutation parameters
      emal.ind.j = emal.ind.j,##<<  indicator for EM/AL (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.emal = ncat.emal,##<<1 + no of permutation parameters
      geo.ind.j = geo.ind.j, ##<< indicator for geo bias (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.geo = ncat.geo,##<<1 + no of permutation parameters
      posage.ind.j = posage.ind.j,##<< indicator for pos age bias (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.posage = ncat.posage,##<<1 + no of permutation parameters
      negage.ind.j = negage.ind.j, ##<< indicator for neg age bias (1 = NA, 2+ gives unique pertubation multiplier)
      ncat.negage = ncat.negage,##<<1 + no of permutation parameters
##      source.MICS.ind1.j = source.MICS.ind1.j, ##<< indicator for MICS (1 = yes, 0 = no)
      folk.ind1.j = folk.ind1.j,##<<  indicator for folk(1 = yes, 0 = no)
      mpos.ind1.j = mpos.ind1.j, ##<<  indicator for modern[+] (1 = yes, 0 = no)
      mneg.ind1.j = mneg.ind1.j ##<<  indicator for modern[-] (1 = yes, 0 = no)
        ,abs.probe.q.ind1.j = abs.probe.q.ind1.j #<< indicator for absence of probing questions (1 = yes, 0 = no)
    )

    ## Sexual activity among unmarried
    if("sex.ac.unm.c" %in% colnames(country.info)) {
        list.no.validation <-
            c(list.no.validation
             ,list(sex.ac.unm.index = sex.ac.unm.index
                  ,n.sex.ac.unm.index = n.sex.ac.unm.index
                  ,not.sex.ac.unm.index = not.sex.ac.unm.index
                  ,n.not.sex.ac.unm.index = n.not.sex.ac.unm.index
                  ,n.sex.ac.unm = n.sex.ac.unm
                  ,reg.in.sex.ac.unm.c = reg.in.sex.ac.unm.c
                  ,n.reg.in.sex.ac.unm = n.reg.in.sex.ac.unm
                  ,sex.ac.reg = sex.ac.reg
                  ,subreg.in.sex.ac.unm.c = subreg.in.sex.ac.unm.c
                  ,n.subreg.in.sex.ac.unm = n.subreg.in.sex.ac.unm
                  ,reg.in.sex.ac.unm.subreg = reg.in.sex.ac.unm.subreg
                  ,reg.in.sex.ac.unm.SA1sub.c = reg.in.sex.ac.unm.SA1sub.c
                  ,n.reg.in.sex.ac.unm.SA1sub = n.reg.in.sex.ac.unm.SA1sub
                  ,sex.ac.unm.SA1sub.reg = sex.ac.unm.SA1sub.reg
                  ,subreg.in.sex.ac.unm.SA1sub.c = subreg.in.sex.ac.unm.SA1sub.c
                  ,n.subreg.in.sex.ac.unm.SA1sub = n.subreg.in.sex.ac.unm.SA1sub
                  ,reg.in.sex.ac.unm.SA1sub.subreg = reg.in.sex.ac.unm.SA1sub.subreg
                   ))

    }

      ## If include.c.no.data
      if(include.c.no.data) {
          list.no.validation <-
              c(list.no.validation
               ,list(C.no.data = C.no.data
                    ,crich.index.no.data = crich.index.no.data
                    ,n.rich.no.data = n.rich.no.data
                    ,cnotrich.index.no.data = cnotrich.index.no.data
                    ,n.notrich.no.data = n.notrich.no.data,
                     N.obsperiod.c.test = N.obsperiod.c.test, # number of years in obs period per country #change NC, 20160601
                     year.begin.c.test = year.begin.c.test, #Change NC, 20160807
                     year.end.c.test=year.end.c.test, #Change NC, 20160807
                     year.est.c.test=year.est.c.test,#Change NC, 20160807
                     year.set.index.test=year.set.index.test,
                     N.unique.c.test = N.unique.c.test #[MCW-2017-03-14-1]
                    ,gett.ci.test = gett.ci.test      #[MCW-2017-03-14-2]
                     ))
          }

          ## Sexual activity among unmarried
          if("sex.ac.unm.c" %in% colnames(country.info.no.data)) {
              list.no.validation <-
                  c(list.no.validation
                   ,list(sex.ac.unm.index.no.data = sex.ac.unm.index.no.data
                        ,n.sex.ac.unm.index.no.data = n.sex.ac.unm.index.no.data
                        ,not.sex.ac.unm.index.no.data = not.sex.ac.unm.index.no.data
                        ,n.not.sex.ac.unm.index.no.data = n.not.sex.ac.unm.index.no.data
                         ))
          }
      }

    if (do.SS.run.second.pass) { # change JR, 20140414
      list.no.validation <- c(list.no.validation,
                              list(bias.modern = data.SS$bias.modern ##<< calculated SS modern bias
                              ))
    }
    # if (usestartendyear)
    list.no.validation = c(list.no.validation,
                           list(period.j = period.j, getperiod.j = getperiod.j,
                                partialtime.xj = partialtime.xj,
                                getts.jf = getts.jf,
                                getis.jf = getis.jf,
                                getest.jf=getest.jf,
                                getest.ci=getest.ci,
                                N.unique.c = c(N.unique.c)))

    ##end<<

  if (!is.null(validation.list)){
      ##details<< If \code{!is.null(validation.list)}, \code{getj.training.k} is constructed
      ## using \code{\link{GetTraining}}

      ## Common to all validations:
      list.validation <- list(
          getj.training.k  = getj.training.k,
          n.training.breakdown = n.training.breakdown,
          getj.training.tot.k  = getj.training.tot.k,
          n.training.tot = n.training.tot,
          getj.training.modern.k = getj.training.modern.k, # change JR, 20131120
          n.training.modern = n.training.modern, # change JR, 20131120
          getj.training.unmet.k = getj.training.unmet.k,
          n.training.unmet = n.training.unmet,
          getj.test.unmet.k = getj.test.unmet.k,
          N.unmet.test = N.unmet.test,
          n.test.unmet = n.test.unmet
      )
      if(ModelFunctionModOnlyObs(write.model.fun)) {
          list.validation <-
              c(list.validation,
                list(getj.training.modonly.k = getj.training.modonly.k,
                     n.training.modonly = n.training.modonly))
          }
      if(!validation.list$exclude.unmet.only) {
          ## Not needed for 'exclude.unmet.only'
          list.validation <-
              c(list.validation,
                list(getj.test.k = getj.test.k,
                     n.test.breakdown = n.test.breakdown
                     ))
      }
      if(validation.list$at.random.no.data || validation.list$leave.iso.out) {
          list.validation <-
              c(list.validation,
                list(logitratio.yunmet.j.test = logitratio.yunmet.j.test,
                     ratios.trad.modern.jn.test = as.matrix(cbind(logratio.ytrad.j.test, logratio.ymodern.j.test)),
                     logit.ytot.j.test = logit.ytot.j.test,
                     se.logR.trad.impute.test = se.logR.trad.impute.test,
                     se.logR.modern.impute.test = se.logR.modern.impute.test,
                     se.logR.unmet.impute.test = se.logR.unmet.impute.test,
                     med.max.trad.se.test=validation.at.random.no.data$se.info.j.test$med.max.trad.se,
                     med.max.modern.se.test=validation.at.random.no.data$se.info.j.test$med.max.modern.se,
                     med.max.unmet.se.test=validation.at.random.no.data$se.info.j.test$med.max.unmet.se,
                     y.modern.j.test = y.modern.j.test,
                     se.modern.j.test = se.modern.j.test,
                     getc.z.test = getc.z.test,
                     n.countriesmorethan1obs.test = n.countriesmorethan1obs.test,
                     getc.j.test = getc.j.test,
                     gett.j.test = gett.j.test,
                     N.obsperiod.c.test=N.obsperiod.c.test,
                     year.begin.c.test = year.begin.c.test,
                     year.end.c.test = year.end.c.test,
                     year.est.c.test = year.est.c.test,
                     year.set.index.test = year.set.index.test,
                     getc.x.test = getc.x.test,
                     n.countries1obs.test = n.countries1obs.test,
                     gett.ci.test = gett.ci.test,
                     geti.j.test = geti.j.test,
                     getest.j.test = getest.j.test,
                     C.no.data = C.no.data,
                     J.test = J.test,
                     iso.j.test = iso.j.test,
                     N.unique.c.test = N.unique.c.test, #[MCW-2017-03-14-1]
                     period.j.test = period.j.test,
                     getperiod.j.test = getperiod.j.test,
                     partialtime.xj.test = partialtime.xj.test,
                     getts.jf.test = getts.jf.test,
                     getis.jf.test = getis.jf.test,
                     getest.jf.test=getest.jf.test,
                     getest.ci.test=getest.ci.test
                     ))
      }
  } else { #all, no validation
      ##details<< If it is not a validation exercise, list.validation contains
      ##describe<<
      list.validation <- list(
          getj.training.k  = getj.training.k, ##<< indices
          n.training.breakdown = n.training.breakdown, ##<< count
          getj.training.tot.k  = getj.training.tot.k, ##<<indices
          n.training.tot = n.training.tot, ##<< count
          getj.training.modern.k = getj.training.modern.k, ##<< indices # change JR, 20131120
          n.training.modern = n.training.modern, ##<< count # change JR, 20131120
          getj.training.unmet.k  = getj.training.unmet.k, ##<< indices
          n.training.unmet = n.training.unmet ##<< count
      )
      if(ModelFunctionModOnlyObs(write.model.fun)) {
          list.validation <-
              c(list.validation,
                list(getj.training.modonly.k = getj.training.modonly.k,
                     n.training.modonly = n.training.modonly))
          }
      ##end<<
  }

    if(do.country.specific.run || do.country.specific.targets.run) {
        ## For one country run, find name of subreg for country
        if(ModelFunctionSubRegInSA1India(write.model.fun)) {
            name.reg.for.prior.specs <-
                as.character(country.info$name.reg.in.sex.ac.unm.SA1sub)[1]
            name.subreg.for.prior.specs <-
                as.character(country.info$name.subreg.in.sex.ac.unm.SA1sub)[1]
        } else if(ModelFunctionSA(write.model.fun)) {
            name.reg.for.prior.specs <-
                as.character(country.info$name.reg.in.sex.ac.unm)[1]
            name.subreg.for.prior.specs <-
                as.character(country.info$name.subreg.in.sex.ac.unm)[1]
        } else {
            name.reg.for.prior.specs <-
                as.character(country.info$namereg.c)[1]
            name.subreg.for.prior.specs <-
                as.character(country.info$namesubreg.c)[1]
        }
    }

  ## REPLACED with the version from `GetBugsData()` (level model)
  priorspecs <- GetBugsPriorSpecs(change.priors.to.zerolower = change.priors.to.zerolower,
                                  do.country.specific.run = do.country.specific.run, # change JR, 20131104
                                  do.country.specific.targets.run = do.country.specific.targets.run, # change JR, 20131104
                                  name.reg = name.reg.for.prior.specs,
                                  name.subreg = name.subreg.for.prior.specs, # change JR, 20131104
                                  iso.country.select = country.info$iso.country.select[1], # change JR, 20140404
                                  data.global = data.global, # change JR, 20131104
                                  disagg.RN.PMA = disagg.RN.PMA #[MCW-2016-04-04-1] Added.
                                #[MCW-2016-06-02-10] added next two lines to pass through
                                #values of new z priors and write model script.
                                 ,uwra.z.priors = uwra.z.priors
                                 ,write.model.fun = write.model.fun
                                  ## [MCW-2016-06-14-4] added to pass through value of uwra.Omega.priors.
                                 ,uwra.Omega.priors = uwra.Omega.priors
                                  ##[MCW-2016-06-21-4] Added to pass this argument through.
                                 ,uwra.kappa.c.priors = uwra.kappa.c.priors
                                  ## [MCW-2016-10-05-2] :: Added to pass these through.
                                 ,mean.Tworld = timing.world.priors$mean.Tworld
                                 ,mean.TOneLevel = timing.world.priors$mean.TOneLevel
                                  ,verbose = verbose
                                  )

  ##value<< One combined list that includes elements from
                                #  ##describe<<
  winbugs.data <- c(list.no.validation, ##<< See details.
                    list.validation, ##<< See details.
                    priorspecs, ##<< Prior specs from \code{GetBugsPriorSpecs}.
                    validation.at.random.no.data)
  ##end<<
  return(winbugs.data)
}

#----------------------------------------------------------------------------------
GetBugsPriorSpecs <-
    function( #Set priors parameters
## Set prior parameters for CP model. The names for the prior parameters that are not explained below
## follow from
## the names used in R for the model parameters and
## the specification of the prior distributions
## (see"Prior distributions" and the table with names used in \code{R} in the
## web appendix of Alkema et al).
             change.priors.to.zerolower = FALSE, #<< logical indicating if gammas are used for kappa.c's
             do.country.specific.run = FALSE, #<< logical indicating if run is country-specific
             ## ---------- RATE MODEL >>>>>>>>>>
             do.country.specific.targets.run = FALSE, #<< logical indicating if run is country-specific for targets # change JR, 20140301
             name.reg = NULL, #<< (For country-specific run for targets) character giving the name of the region to which country belongs to # change JR, 20150301
             ## <<<<<<<<<< RATE MODEL ----------
             name.subreg = NULL, #<< (For country-specific run) character giving the name of the subregion
             ## to which country belongs to # change JR, 20131104
             iso.country.select = NULL, #<< (For subpopulation-specific run) character giving the 3-character ISO country code of the country the
             ## subpopulation belongs to # change JR, 20140404
             data.global = NULL, #<< (For country-specific run) object from \code{\link{SummariseGlobalRun}} # change JR, 20131104
             rho.max = 1, #<< upper bound rho for AR with total/ratio
             sigma.ar.max =1, #<< upper bound sd sigma for AR with total/ratio
             rho.max.unmet = 1, #<< upper bound rho for AR with unmet
             sigma.ar.max.unmet = 1, #<< upper bound sd sigma for AR with unmet
             sigma2.sourcetot0 = 0.0225,
             R = as.matrix(cbind(c(0.1,0), c(0, 0.1))),#<< prior precision matrix for logit(trad/total, mod/total)
             nu0 = 10, #<< prior sample size for kappa.c's
             ## upto June 6, 2012
             ##   sigma2.lpc0 = 0.43,#<<
             ##   sigma2.lrc0 = 0.87, #<<
             ##   sigma2.wc0 = 0.17,#<<
             ##   sigma2.Rwc0 =  0.18,#<<
             ##   sigma2.Tc0 = 253.87,#<<
             ##   sigma2.RTc0 =  151.66,#<<
             ##   sigma2.earlierTc0 = 339.94, #<<
             ##   sigma2.unmetc0 = 0.4^2,#<<

             ## based on run20120608_noAR
             sigma2.lpc0 = 0.63,#<<
             sigma2.lrc0 = 1.97, #<<
             sigma2.wc0 =  0.35,#<<
             sigma2.Rwc0 =   0.65,#<<
             sigma2.Tc0 = 300,#<<
             sigma2.RTc0 =  197,#<<
             sigma2.earlierTc0 = 263, #<<
             sigma2.unmetc0 = 0.08,#<<

             ## [MCW-2016-09-02-8] :: Set prior mean ~mean.Tworld~ (mean of
             ## $\Omega_L$) to 1970. This now applies to countries in SA/FS
             ## categories 'A', 'B', 'D'.
             mean.Tworld = 1970,#<<

             mean.RTworld = 1980,#<<

             ## [MCW-2016-09-02-7] :: Set prior mean ~mean.TOneLevel~ (mean of
             ## $\Omega_D$) to 2070. This now applies to countries in SA/FS
             ## category 'C'.
             mean.TOneLevel = 2070,#<<

             ## ---------- RATE MODEL >>>>>>>>>>
             sigmaRTregsubreg.upper = 80,
             tau0.RT = 1/50^2,
             sigmaSregsubreg.upper = 3,
             ## <<<<<<<<<< RATE MODEL ----------

             sigmaTregsubreg.upper = 80, #<<,
             tau0.T = 1/50^2, #<< corresponding to SD of 50 years
             sigmawregsubreg.upper = 3, #<< (refers to logit scale)
             a0.unmet = -0.38, #<< From LS-fit
             b0.unmet = 0.12, #<< From LS-fit
             tau.a0 = 1, #<<
             tau.b0 = 1, #<<
             disagg.RN.PMA = TRUE ##[MCW-2016-04-04-2] Added to turn on/off disaggregation of PMA and repeated national surveys.
            ,uwra.z.priors = NULL #[MCW-2016-06-02-2] Added to allow different priors for z
                                        #model. Set to an integer code and
                                        #define in body of function (see
                                        #examples below).
            ,uwra.Omega.priors = NULL##[MCW-2016-06-14] Added to allow different priors
             ##for Omega parameters. Set to an integer code and
             ##define in body of function (see examples below).
  ## [MCW-2016-06-21-5] Added to control priors for kappa.^(c) variances. Set to
  ## an integer code (see examples below).
             ,uwra.kappa.c.priors = NULL
            ,write.model.fun = "WriteModel" ##[MCW-2016-06-02-4] pass in the 'WriteModel[suff]() function used.
            ,verbose = TRUE
             )
{
    ## For different priors for Z model.
    if(!is.null(uwra.z.priors) && !change.priors.to.zerolower) {
        if(uwra.z.priors == 1) {            #[MCW-2016-06-02-3] Change z model priors to set "1"
            a0.unmet <- -2
            tau.a0 <- 1
            b0.unmet <- -6
            tau.b0 <- 1 / (5^2)             #JAGS/BUGS uses precision, not variance
            if(verbose) message("\nuwra.z.priors == 1. Using JAGS/BUGS parameterizations:\n    z_w ~ dNorm("
                   ,a0.unmet, ", 1/", 1/tau.a0, ")\n    beta_1 ~ dNorm("
                   ,b0.unmet, ", 1/", 1/tau.b0, ")\n    c.unmet is set in 'write.model.fun'"
                    )
        }
    } else { ## if 'uwra.z.priors == NULL'
        if(verbose) message("\nUsing original (MWRA) priors for Z model") #[MCW-2016-06-02-11] For user information.
    }

    ## For different priors for Omega
    if(!is.null(uwra.Omega.priors) && !change.priors.to.zerolower) {
        if(uwra.Omega.priors == 1) {            #[MCW-2016-06-14-3] Change Omega model priors to set "1"
            sigma2.Tc0 <- 1250
            sigma2.earlierTc0 <- 2500
            if(verbose) message("\nuwra.Omega.priors == 1. Using JAGS/BUGS parameterizations:\n    1/kappa_Omega^(c) ~ dGamma(8/2, 8/2*"
                   ,sigma2.Tc0, ")\n    1/kappa_Omega^(D) ~ dGamma(2/2, 2/2*"
                   ,sigma2.earlierTc0, ")"
                    )
        }
    } else { ## if 'uwra.Omega.priors == NULL'
        if(verbose) message("\nUsing original (MWRA) priors for Omega variances")
    }

    ## For different priors for kappa.^(c)s
    if(!is.null(uwra.kappa.c.priors) && !change.priors.to.zerolower) {
        if(uwra.kappa.c.priors == 1) {
            sigma2.lpc0 <- (2 / nu0) * 6.55
            if(verbose) message("\nuwra.kappa.c.priors == 1. Using 'sigma2.lpc0 <- (2 / nu0) * 6.55'")
        }
    } else { ## if 'uwra.kappa.c.piors = NULL'
        if(verbose) message("\nUsing original (MWRA) priors for kappa.^(c)s")
    }

    ## ---------- LEVEL MODEL >>>>>>>>>>

    if(!ModelFunctionRateModel(write.model.fun)) {

        if (!do.country.specific.run) {
            ##details<< \code{prior.list1} has elements that do not relate to the priors for the kappa.c's.
            prior.list1 <- list(
                rho.max = rho.max,
                sigma.ar.max = sigma.ar.max,
                rho.max.unmet = rho.max.unmet,
                sigma.ar.max.unmet = sigma.ar.max.unmet,
                halfsigma2.sourcetot0 = 0.5*sigma2.sourcetot0,
                halfsigma2.sourcemodonly0 = 0.5*sigma2.sourcetot0,
                R = R,
                mean.RTworld = mean.RTworld,
                sigmaTregsubreg.upper = sigmaTregsubreg.upper,
                tau0.T = tau0.T,
                mean.TOneLevel = mean.TOneLevel,
                mean.Tworld = mean.Tworld,
                sigmawregsubreg.upper = sigmawregsubreg.upper,
                a0.unmet = a0.unmet,
                b0.unmet = b0.unmet,
                tau.a0 = tau.a0,
                tau.b0 = tau.b0
            )

            if (!change.priors.to.zerolower){
                halfnu0_rich =2/2
                halfnu0_poor =8/2

                ## tau.earlierTc ~ dgamma(halfnu0_rich,halfnu0_rich_sigma2.earlierTc0)
                ## tau.Tc ~ dgamma(halfnu0_poor,halfnu0_poor_sigma2.Tc0)

                ##details<<
                ## If \code{!change.priors.to.zerolower}, \code{prior.list2} contains prior gamma parameters,
                ## in the form of \code{halfnu0sigma2...}.
                prior.list2 <- list(
                    halfnu0 = nu0/2,
                    halfnu0_rich =halfnu0_rich,
                    halfnu0_poor =halfnu0_poor,
                    halfnu0_rich_sigma2.earlierTc0 = halfnu0_rich*sigma2.earlierTc0,
                    halfnu0_poor_sigma2.Tc0 =  halfnu0_poor*sigma2.Tc0,
                                #halfnu0sigma2.earlierTc0 = nu0/2*sigma2.earlierTc0,
                                #halfnu0sigma2.Tc0 =  nu0/2*sigma2.Tc0,
                    halfnu0sigma2.lpc0 = nu0/2*sigma2.lpc0,
                    halfnu0sigma2.lrc0 =  nu0/2*sigma2.lrc0,
                    halfnu0sigma2.wc0 = nu0/2*sigma2.wc0,
                    halfnu0sigma2.Rwc0 =  nu0/2*sigma2.Rwc0,
                    halfnu0sigma2.RTc0 = nu0/2*sigma2.RTc0,
                    halfsigma2.unmetc0 = 0.5*sigma2.unmetc0
                )
            } else {
                prior.list2 <- NULL
            }
        } else { # change JR, 20131104  (If *yes* country.specific.run)
            mcmc.post <- data.global$mcmc.post

            if(disagg.RN.PMA) { ## [MCW-2016-04-04-3] Added to activate disaggregation of PMA and repeated national surveys.
                T1.source.s0 <- c(mcmc.post[['T1.source.s[1]']], mcmc.post[['T1.source.s[2]']],
                                  mcmc.post[['T1.source.s[3]']], mcmc.post[['T1.source.s[4]']],
                                  ## [MCW-2016-03-07-3] Added more components for additional source types
                                  mcmc.post[['T1.source.s[5]']], mcmc.post[['T1.source.s[6]']]
                                  )
                T2.source.s0 <- c(mcmc.post[['T2.source.s[1]']], mcmc.post[['T2.source.s[2]']],
                                  mcmc.post[['T2.source.s[3]']], mcmc.post[['T2.source.s[4]']],
                                  ## [MCW-2016-03-07-3] Added more components for additional source types
                                  mcmc.post[['T2.source.s[5]']], mcmc.post[['T2.source.s[6]']]
                                  )
                T12.source.s0 <- c(mcmc.post[['T12.source.s[1]']], mcmc.post[['T12.source.s[2]']],
                                   mcmc.post[['T12.source.s[3]']], mcmc.post[['T12.source.s[4]']],
                                   ## [MCW-2016-03-07-3] Added more components for additional source types
                                   mcmc.post[['T12.source.s[5]']], mcmc.post[['T12.source.s[6]']]
                                   )
            } else {
                T1.source.s0 <- c(mcmc.post[['T1.source.s[1]']], mcmc.post[['T1.source.s[2]']],
                                  mcmc.post[['T1.source.s[3]']], mcmc.post[['T1.source.s[4]']]
                                  )
                T2.source.s0 <- c(mcmc.post[['T2.source.s[1]']], mcmc.post[['T2.source.s[2]']],
                                  mcmc.post[['T2.source.s[3]']], mcmc.post[['T2.source.s[4]']]
                                  )
                T12.source.s0 <- c(mcmc.post[['T12.source.s[1]']], mcmc.post[['T12.source.s[2]']],
                                   mcmc.post[['T12.source.s[3]']], mcmc.post[['T12.source.s[4]']]
                                   )
            }

            prior.list1 <- list(
                rho.tot0 = mcmc.post$rho.tot,
                sigma.tot0 = mcmc.post$sigma.tot,
                rho.rat0 = mcmc.post$rho.rat,
                sigma.rat0 = mcmc.post$sigma.rat,
                lp.world0 = mcmc.post$lp.world,
                lr.world0 = mcmc.post$lr.world,
                TOneLevel0 = mcmc.post$TOneLevel,
                a.unmet0 = mcmc.post$a.unmet,
                b.unmet0 = mcmc.post$b.unmet,
                c.unmet0 = mcmc.post$c.unmet,
                rho.unmet0 = mcmc.post$rho.unmet,
                sigma.ar.unmet0 = mcmc.post$sigma.ar.unmet,
                v.abs.probe.q0 = mcmc.post$v.abs.probe.q,
                v.mneg0 = mcmc.post$v.mneg,
                v.folk0 = mcmc.post$v.folk,
                v.mpos0 = mcmc.post$v.mpos,
                sigma.pos0 = mcmc.post$sigma.pos,
                mu.pos.m0 = c(mcmc.post[['mu.pos.m[1]']], mcmc.post[['mu.pos.m[2]']]),
                tau.sourcetot0 = 1/(mcmc.post$sigma.sourcetot^2),
                tau.sourcemodonly0 = 1/(mcmc.post$sigma.sourcetot^2),
                T1.source.s0 = T1.source.s0, # [MCW-2016-04-04-4] altered to pick up pre-defined objects (MCW-2016-04-04-3)
                T2.source.s0 = T2.source.s0,# [MCW-2016-04-04-4]
                T12.source.s0 = T12.source.s0,# [MCW-2016-04-04-4]
                sigma.unmet.other0 = mcmc.post$sigma.unmet.other,
                sigma.unmet.dhs0 = mcmc.post$sigma.unmet.dhs,
                sigma.unmetworld0 = mcmc.post$sigma.unmetworld,
                sigma.geo.m0 = c(mcmc.post[['sigma.geo.m[1]']], mcmc.post[['sigma.geo.m[2]']]))
                                # change JR, 20140404

            if (is.null(iso.country.select)) { # for country-specific run
                subreg.global <- which(data.global$name.subreg == name.subreg)
                prior.list1 <-
                    c(prior.list1
                     ,list(unmet.subreg0 = mcmc.post[[paste0("unmet.subreg[", subreg.global, "]")]],
                          w.subreg0 = mcmc.post[[paste0("w.subreg[", subreg.global, "]")]],
                          T.subreg0 = mcmc.post[[paste0("T.subreg[", subreg.global, "]")]],
                          Rw.subreg0 = mcmc.post[[paste0("Rw.subreg[", subreg.global, "]")]],
                          RT.subreg0 = mcmc.post[[paste0("RT.subreg[", subreg.global, "]")]]
                      ))
            } else { # for subpopulation-specific run
                country.global <- which(data.global$iso.c == iso.country.select)
                prior.list1 <-
                    c(prior.list1
                    ,list(unmet.subreg0 = mcmc.post[[paste0("unmet.intercept.c[", country.global, "]")]],
                          w.subreg0 = LogitMinMax(mcmc.post[[paste0("omega.c[", country.global, "]")]], 0.01, 0.5),
                          T.subreg0 = mcmc.post[[paste0("T.c[", country.global, "]")]],
                          Rw.subreg0 = LogitMinMax(mcmc.post[[paste0("Romega.c[", country.global, "]")]], 0.01, 0.5),
                          RT.subreg0 = mcmc.post[[paste0("RT.c[", country.global, "]")]]
                          ))
            }
            if (change.priors.to.zerolower) {
                prior.list1 <- c(prior.list1, list(
                                                  sigma.lpc0 = mcmc.post$sigma.lpc,
                                                  sigma.lrc0 = mcmc.post$sigma.lrc,
                                                  sigma.wc0 = mcmc.post$sigma.wc,
                                                  sigma.Rwc0 = mcmc.post$sigma.Rwc,
                                                  sigma.Tc0 = mcmc.post$sigma.Tc,
                                                  sigma.RTc0 = mcmc.post$sigma.RTc,
                                                  sigma.earlierTc0 = mcmc.post$sigma.earlierTc,
                                                  sigma.unmetc0 = mcmc.post$sigma.unmetc))
            } else {
                prior.list1 <- c(prior.list1, list(
                                                  tau.unmetc0 = 1/(mcmc.post$sigma.unmetc^2),
                                                  tau.earlierTc0 = 1/(mcmc.post$sigma.earlierTc^2),
                                                  tau.Tc0 = 1/(mcmc.post$sigma.Tc^2),
                                                  tau.lpc0 = 1/(mcmc.post$sigma.lpc^2),
                                                  tau.lrc0 = 1/(mcmc.post$sigma.lrc^2),
                                                  tau.wc0 = 1/(mcmc.post$sigma.wc^2),
                                                  tau.Rwc0 = 1/(mcmc.post$sigma.Rwc^2),
                                                  tau.RTc0 = 1/(mcmc.post$sigma.RTc^2)))
            }
            prior.list2 <- NULL
        }

        ## <<<<<<<<<< LEVEL MODEL ----------

    } else {

        ## ---------- RATE MODEL >>>>>>>>>>

        if (!do.country.specific.run & !do.country.specific.targets.run) { # change JR, 20150301
            ##details<< \code{prior.list1} has elements that do not relate to the priors for the kappa.c's.
            prior.list1 <- list(
                rho.max = rho.max,
                sigma.ar.max = sigma.ar.max,
                rho.max.unmet = rho.max.unmet,
                sigma.ar.max.unmet = sigma.ar.max.unmet,
                halfsigma2.sourcetot0 = 0.5*sigma2.sourcetot0,
                halfsigma2.sourcemodonly0 = 0.5*sigma2.sourcetot0,
                R = R,
                mean.RTworld = mean.RTworld,
                sigmaRTregsubreg.upper = sigmaRTregsubreg.upper,
                tau0.RT = tau0.RT,
                sigmawregsubreg.upper = sigmawregsubreg.upper,
                sigmaSregsubreg.upper = sigmaSregsubreg.upper,
                a0.unmet = a0.unmet,
                b0.unmet = b0.unmet,
                tau.a0 = tau.a0,
                tau.b0 = tau.b0)
        } else { # change JR, 20131104
            mcmc.post <- data.global$mcmc.post
            prior.list1 <- list(
                rho.tot0 = mcmc.post$rho.tot,
                sigma.tot0 = mcmc.post$sigma.tot,
                rho.rat0 = mcmc.post$rho.rat,
                sigma.rat0 = mcmc.post$sigma.rat,
                lp.world0 = mcmc.post$lp.world,
                lr.world0 = mcmc.post$lr.world,
                Shigher0 = mcmc.post$Shigher,#Change, NC 20170228
                rho.unmet0 = mcmc.post$rho.unmet,
                sigma.ar.unmet0 = mcmc.post$sigma.ar.unmet,
                a.unmet0 = mcmc.post$a.unmet,
                b.unmet0 = mcmc.post$b.unmet,
                c.unmet0 = mcmc.post$c.unmet) # change JR, 20150301
            if (!do.country.specific.targets.run) { # change JR, 20150301
                prior.list1 <-
                    c(prior.list1
                    , list(v.abs.probe.q0 = mcmc.post$v.abs.probe.q,
                           v.mneg0 = mcmc.post$v.mneg,
                           v.folk0 = mcmc.post$v.folk,
                           v.mpos0 = mcmc.post$v.mpos,
                           sigma.pos0 = mcmc.post$sigma.pos,
                           mu.pos.m0 = c(mcmc.post[['mu.pos.m[1]']], mcmc.post[['mu.pos.m[2]']]),
                           tau.sourcetot0 = 1/(mcmc.post$sigma.sourcetot^2),
                           tau.sourcemodonly0 = 1/(mcmc.post$sigma.sourcetot^2),
                           ## MCW 2017-12-22 :: There are now six data sources, with PMA disaggregated and CP TOT < 1 percent.
                           nonsample.se.trad.s0 = c(mcmc.post[['nonsample.se.trad.s[1]']], mcmc.post[['nonsample.se.trad.s[2]']],
                                                    mcmc.post[['nonsample.se.trad.s[3]']], mcmc.post[['nonsample.se.trad.s[4]']],
                                                    mcmc.post[['nonsample.se.trad.s[5]']], mcmc.post[['nonsample.se.trad.s[6]']]),
                           nonsample.se.modern.s0 = c(mcmc.post[['nonsample.se.modern.s[1]']], mcmc.post[['nonsample.se.modern.s[2]']],
                                                      mcmc.post[['nonsample.se.modern.s[3]']], mcmc.post[['nonsample.se.modern.s[4]']],
                                                      mcmc.post[['nonsample.se.modern.s[5]']], mcmc.post[['nonsample.se.modern.s[6]']]),
                           cor.trad.modern.s0 = c(mcmc.post[['cor.trad.modern.s[1]']], mcmc.post[['cor.trad.modern.s[2]']],
                                                  mcmc.post[['cor.trad.modern.s[3]']], mcmc.post[['cor.trad.modern.s[4]']],
                                                  mcmc.post[['cor.trad.modern.s[5]']], mcmc.post[['cor.trad.modern.s[6]']]
                                                  ),
                           sigma.unmet.other0 = mcmc.post$sigma.unmet.other,
                           sigma.unmet.dhs0 = mcmc.post$sigma.unmet.dhs,
                           sigma.unmetworld0 = mcmc.post$sigma.unmetworld,
                           sigma.geo.m0 = c(mcmc.post[['sigma.geo.m[1]']], mcmc.post[['sigma.geo.m[2]']])))

                if (is.null(iso.country.select)) { # for country-specific run

                    subreg.global <- which(data.global$name.subreg == name.subreg)
                    prior.list1 <-
                        c(prior.list1, list(unmet.subreg0 = mcmc.post[[paste0("unmet.subreg[", subreg.global, "]")]],
                                            w.subreg0 = mcmc.post[[paste0("w.subreg[", subreg.global, "]")]],
                                            Rw.subreg0 = mcmc.post[[paste0("Rw.subreg[", subreg.global, "]")]],
                                            RT.subreg0 = mcmc.post[[paste0("RT.subreg[", subreg.global, "]")]],
                                            S.subreg0= mcmc.post[[paste0("S.subreg[", subreg.global, "]")]] #Change NC 20160907
                                            ))
                } else { # for subpopulation-specific run
                    country.global <- which(data.global$iso.c == iso.country.select)
                    prior.list1 <-
                        c(prior.list1, list(unmet.subreg0 = mcmc.post[[paste0("unmet.intercept.c[", country.global, "]")]],
                                            w.subreg0 = LogitMinMax(mcmc.post[[paste0("omega.c[", country.global, "]")]], 0.01, 0.5),
                                            Rw.subreg0 = LogitMinMax(mcmc.post[[paste0("Romega.c[", country.global, "]")]], 0.01, 0.5),
                                            RT.subreg0 = mcmc.post[[paste0("RT.c[", country.global, "]")]],
                                            S.subreg0 = mcmc.post[[paste0("setlevel.c[", country.global, "]")]] #Change NC 20160907

                                            ))
                }
                if (change.priors.to.zerolower) {
                    prior.list1 <-
                        c(prior.list1, list(sigma.lpc0 = mcmc.post$sigma.lpc,
                                            sigma.lrc0 = mcmc.post$sigma.lrc,
                                            sigma.wc0 = mcmc.post$sigma.wc,
                                            sigma.Rwc0 = mcmc.post$sigma.Rwc,
                                            sigma.RTc0 = mcmc.post$sigma.RTc,
                                            sigma.unmetc0 = mcmc.post$sigma.unmetc,
                                            sigma.Sc0 = mcmc.post$sigma.Sc,
                                            sigma.higherSc0 = mcmc.post$sigma.higherSc #Change NC, 20170221
                                            ))
                } else {
                    prior.list1 <-
                        c(prior.list1, list(tau.unmetc0 = 1/(mcmc.post$sigma.unmetc^2),
                                            tau.lpc0 = 1/(mcmc.post$sigma.lpc^2),
                                            tau.lrc0 = 1/(mcmc.post$sigma.lrc^2),
                                            tau.wc0 = 1/(mcmc.post$sigma.wc^2),
                                            tau.Rwc0 = 1/(mcmc.post$sigma.Rwc^2),
                                            tau.RTc0 = 1/(mcmc.post$sigma.RTc^2),
                                            tau.Sc0 = 1/(mcmc.post$sigma.Sc^2),
                                            tau.higherSc0 = 1/(mcmc.post$sigma.higherSc^2) #Change NC, 20170221
                                            ))
                }
            } else { # change JR, 20150301
                reg.global <- which(data.global$name.reg == name.reg)
                prior.list1 <-
                    c(prior.list1, list(sigma.unmetworld0 = mcmc.post[["sigma.unmetworld"]],
                                        w.reg0 = mcmc.post[[paste0("w.reg[", reg.global, "]")]],
                                        Rw.reg0 = mcmc.post[[paste0("Rw.reg[", reg.global, "]")]],
                                        RT.reg0 = mcmc.post[[paste0("RT.reg[", reg.global, "]")]],
                                        S.reg0 = mcmc.post[[paste0("S.reg[", reg.global, "]")]],
                                        sigma.wsubreg0 = mcmc.post[["sigma.wsubreg"]],
                                        sigma.Rwsubreg0 = mcmc.post[["sigma.Rwsubreg"]],
                                        sigma.RTsubreg0 = mcmc.post[["sigma.RTsubreg"]],
                                        sigma.Ssubreg0 = mcmc.post[["sigma.Ssubreg"]]
                                        ))
            }
        }

        if (!do.country.specific.targets.run | (do.country.specific.targets.run &
                                                do.country.specific.targets.run)) { #copied directly from Niamh's code
            if (!change.priors.to.zerolower) {
                ##details<<
                ## If \code{!change.priors.to.zerolower}, \code{prior.list2} contains prior gamma parameters,
                ## in the form of \code{halfnu0sigma2...}.
                prior.list2 <- list(
                    halfnu0 = nu0/2,
                    halfnu0sigma2.lpc0 = nu0/2*sigma2.lpc0,
                    halfnu0sigma2.lrc0 =  nu0/2*sigma2.lrc0,
                    halfnu0sigma2.wc0 = nu0/2*sigma2.wc0,
                    halfnu0sigma2.Rwc0 =  nu0/2*sigma2.Rwc0,
                    halfnu0sigma2.RTc0 = nu0/2*sigma2.RTc0,
                    halfsigma2.unmetc0 = 0.5*sigma2.unmetc0
                )
            }
        } else {
            prior.list2 <- NULL
        }
    }
    ## <<<<<<<<<< RATE MODEL ----------

    return(c(prior.list1 , prior.list2))
### List with \code{priorlist1} and \code{priorlist2}, described above.
}
##----------------------------------------------------------------------------------
GetParNames <- function(# Get list of parnames
             ## Get list of parnames
             winbugs.data, ##<< Object from \code{\link{winbugs.data}}
             validation.list = NULL, ##<< Validation parameters are included if this is of object validation.list.
             do.country.specific.run = FALSE, ##<< Logical: do country-specific run?
             do.country.specific.targets.run = FALSE, ##<< Logical: do country-specific targets run? # change JR, 20150301
             include.c.no.data = !do.country.specific.run, #[MCW-MCW-2016-08-25-7] Added to allow for countries with no data
             write.model.fun = "WriteModel",
             disagg.RN.PMA = TRUE
             )
{

        JAGS = TRUE
  # Note: I used to have the option to run it in Bugs (and not specify the elements individually),
  # but that option is no longer needed
  # Note that for running in JAGS,
  # you get an error when simply including parameter vectors with NAs.
  # So just specify all of the NAs ones individually (does mean they allllllll show up in Winbugs...)

    if(!ModelFunctionRateModel(write.model.fun)) {

        ## ---------- LEVEL MODEL ---------->

  parnames.c <- c("omega.c", "T.c", "pmax.c", "Romega.c" ,"RT.c", "Rmax.c", "unmet.intercept.c")
  parnames.V <- c("V.geo.12i", "V.age.12i", "V.hw.12i", "V.emal.12i",
                  "V.sa.12i", "V.posbias.12i", "V.posage.12i", "V.negage.12i")
  # Note that there's a function for parnames.V for plotting
  parnames.h <- parnames.subreg <- parnames.reg <-
    T1.source.s <- T2.source.s <- T12.source.s <-
        parnames.ss <-
            parnames.pred.density.unmet <- parnames.pred.density.bdown <-
                parnames.pred.density.tot <-
                parnames.qjs <-parnames.logdens.tot <-
                    parnames.logdens.unmet <- parnames.logdens.bdown <- NULL # change JR, 20140612

  if (is.null(validation.list)){
      parnames.validation <- NULL

      ##
      ## --- For WAIC
      ## Unmet
      for (k in 1:winbugs.data$n.training.unmet){
          parnames.pred.density.unmet <- c(parnames.pred.density.unmet
                                    ,paste0("pred.logitratio.yunmet.j["
                                           ,winbugs.data$getj.training.unmet.k[k]
                                           ,"]"))
          parnames.logdens.unmet <- c(parnames.logdens.unmet
                               ,paste0("logdens.logitratio.yunmet.j["
                                       ,winbugs.data$getj.training.unmet.k[k]
                                           ,"]"))
      }
      for (k in 1:winbugs.data$n.training.unmet){
          parnames.qjs <- c(parnames.qjs
                                    ,paste0("q.unmet.j["
                                           ,winbugs.data$getj.training.unmet.k[k]
                                           ,"]"))
      }

      ## CP breakdown
      for (k in 1:winbugs.data$n.training.breakdown){
          parnames.pred.density.bdown <- c(parnames.pred.density.bdown
                                    ,paste0("pred.ratios.trad.modern.jn["
                                           ,winbugs.data$getj.training.k[k]
                                           ,",1]"))
      }
      for (k in 1:winbugs.data$n.training.breakdown){ #doing it this way matches others
          parnames.pred.density.bdown <- c(parnames.pred.density.bdown
                                    ,paste0("pred.ratios.trad.modern.jn["
                                           ,winbugs.data$getj.training.k[k]
                                           ,",2]"))
          }
      for (k in 1:winbugs.data$n.training.breakdown){
          parnames.logdens.bdown <- c(parnames.logdens.bdown
                                    ,paste0("logdens.ratios.trad.modern.jn["
                                           ,winbugs.data$getj.training.k[k]
                                           ,"]"))
      }
      for (k in 1:winbugs.data$n.training.breakdown){
          parnames.qjs <- c(parnames.qjs
                                    ,paste0("q.trad.j["
                                           ,winbugs.data$getj.training.k[k]
                                           ,"]"))
      }
      for (k in 1:winbugs.data$n.training.breakdown){
          parnames.qjs <- c(parnames.qjs
                                    ,paste0("q.modern.j["
                                           ,winbugs.data$getj.training.k[k]
                                           ,"]"))
      }

      ## CP no breakdown
      for (k in 1:winbugs.data$n.training.tot){
          parnames.pred.density.tot <- c(parnames.pred.density.tot
                                    ,paste0("pred.ytot.j["
                                           ,winbugs.data$getj.training.tot.k[k]
                                            ,"]"))
      }
      for(k in 1:winbugs.data$n.training.tot){
          parnames.logdens.tot <- c(parnames.logdens.tot
                               ,paste0("logdens.ytot.j["
                                      ,winbugs.data$getj.training.tot.k[k]
                                       ,"]"))
    }
  } else { # VALIDATIONS
      ## Spell each possibility out separately to make it clear which
      ## parameters are saved; no convoluted mazes of if/elses.
    if (validation.list$exclude.unmet.only){
      parnames.validation <- c(
        paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]"),
        paste0("q.unmet.j[", winbugs.data$getj.test.unmet.k, "]"),
        # save modern and trad as well to get the sampled total
        # (maybe more efficient to do in Bugs)
        paste0("pred.logratio.ytrad.j[", winbugs.data$getj.test.unmet.k, "]"),
        paste0("pred.logratio.ymodern.j[", winbugs.data$getj.test.unmet.k, "]")
      )
    } else if(validation.list$at.end) {
        parnames.validation <- c(
        paste0("pred.logratio.ytrad.j[",
               winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
        paste0("pred.logratio.ymodern.j[",
               winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
        paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]")
        )
    } else if(validation.list$at.random) {
        parnames.validation <- c(
            paste0("pred.logratio.ytrad.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logratio.ymodern.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]")
        )
    } else if(validation.list$at.random.no.data) {
        parnames.validation <- c(
            paste0("pred.logratio.ytrad.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logratio.ymodern.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]"),
            paste0("pred.logit.ytot.j[",
                   winbugs.data$getj.test.k[(winbugs.data$n.test.breakdown + 1):winbugs.data$J.test], "]")
        )
    } else if(validation.list$leave.iso.out) {
        parnames.validation <- c(
            paste0("pred.logratio.ytrad.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logratio.ymodern.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]")
        )
    }
      else stop("Parameters to save for this validation type have not been determined; fix 'GetParNames()'.")
       }
  parnames.theta <- "theta.ci"
  parnames.eps <- "eps.ci"
  parnames.eta <- "eta.ci"
  #if (JAGS){
  parnames.eps <- parnames.theta <- parnames.eta <- NULL

  for (c in 1:winbugs.data$C){
    for (i in 1:winbugs.data$N.unique.c[c]){
      parnames.theta <- c(parnames.theta, paste0("theta.ci[", c, ",", i, "]"))
      parnames.eps <- c(parnames.eps, paste0("eps.ci[", c, ",", i, "]"))
      parnames.eta <- c(parnames.eta, paste0("eta.ci[", c, ",", i, "]"))
    }
  }

  ## [MCW-2016-08-25-9] If include.c.no.data need to use total number of countries
  if(include.c.no.data) {
      for(c in (winbugs.data$C + 1):(winbugs.data$C + winbugs.data$C.no.data)) {
          parnames.theta <- c(parnames.theta, paste0("theta.ci[", c, ",", 1, "]"))
          parnames.eps <- c(parnames.eps, paste0("eps.ci[", c, ",", 1, "]"))
          parnames.eta <- c(parnames.eta, paste0("eta.ci[", c, ",", 1, "]"))
      }
  }
  if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
      for(c in (winbugs.data$C + 1):(winbugs.data$C + winbugs.data$C.no.data)) {
    for (i in 1:winbugs.data$N.unique.c.test[c - winbugs.data$C]){
          parnames.theta <- c(parnames.theta, paste0("theta.ci[", c, ",", i, "]"))
          parnames.eps <- c(parnames.eps, paste0("eps.ci[", c, ",", i, "]"))
          parnames.eta <- c(parnames.eta, paste0("eta.ci[", c, ",", i, "]"))
      }
      }
      }

  if (!do.country.specific.run) {
      parnames.h <- c(parnames.h,
                      "sigma.sourcetot", "sigma.unmet.dhs", "sigma.unmet.other",
                      "sigma.ar.unmet", "rho.unmet",
                      "a.unmet", "b.unmet", "c.unmet",
                      "sigma.unmetc")
      if(ModelFunctionSAAsymp(write.model.fun)) {
          parnames.h <- c(parnames.h, "lp.world.not.sex", "lp.world.sex")
      } else {
          parnames.h <- c(parnames.h, "lp.world")
      }
      if(ModelFunctionModOnlyObs(write.model.fun)) {
          parnames.h <- c(parnames.h, "sigma.sourcemodonly")
          }
      parnames.h <- c(parnames.h, "lr.world", "sigma.lrc", "sigma.lpc",
                        "rho.tot", "sigma.tot", "rho.rat", "sigma.rat",
                        "sigma.geo.m[1]", "sigma.geo.m[2]",
                        "sigma.pos",
                        "mu.pos.m[1]", "mu.pos.m[2]",
                        "v.abs.probe.q", "v.folk", "v.mneg", "v.mpos",
                        "TOneLevel", "sigma.earlierTc",
                        "sigma.wc", "sigma.Rwc", "sigma.Tc", "sigma.RTc",
                    # the following parameters are not found in WriteCountryModel at all
                    "sigma.unmetworld",
                    "w.world", "Rw.world", "T.world", "RT.world",
                    "sigma.wreg", "sigma.Rwreg", "sigma.Treg", "sigma.RTreg",
                    "sigma.wsubreg", "sigma.Rwsubreg", "sigma.Tsubreg", "sigma.RTsubreg")
      parnames.subreg <- c(parnames.subreg, "w.subreg","Rw.subreg","T.subreg","RT.subreg", "unmet.subreg")
      if(ModelFunctionSurveySEs(write.model.fun)) {
          ## T1.source.s, T2.source.s, T12.source.s already set to NULL
          if(disagg.RN.PMA) {
          parnames.h <- c(parnames.h,
                          "nonsample.se.trad.s[1]","nonsample.se.trad.s[2]","nonsample.se.trad.s[3]","nonsample.se.trad.s[4]","nonsample.se.trad.s[5]","nonsample.se.trad.s[6]",
                    "nonsample.se.modern.s[1]","nonsample.se.modern.s[2]","nonsample.se.modern.s[3]","nonsample.se.modern.s[4]","nonsample.se.modern.s[5]","nonsample.se.modern.s[6]",
                    "nonsample.se.unmet.s[1]","nonsample.se.unmet.s[2]",
                    "cor.trad.modern.s[1]","cor.trad.modern.s[2]","cor.trad.modern.s[3]","cor.trad.modern.s[4]","cor.trad.modern.s[5]","cor.trad.modern.s[6]")
                  } else {
          parnames.h <- c(parnames.h,
                          "nonsample.se.trad.s[1]","nonsample.se.trad.s[2]","nonsample.se.trad.s[3]","nonsample.se.trad.s[4]",
                    "nonsample.se.modern.s[1]","nonsample.se.modern.s[2]","nonsample.se.modern.s[3]","nonsample.se.modern.s[4]",
                    "nonsample.se.unmet.s[1]","nonsample.se.unmet.s[2]",
                    "cor.trad.modern.s[1]","cor.trad.modern.s[2]","cor.trad.modern.s[3]","cor.trad.modern.s[4]")
          }
          } else {
    T1.source.s <- c(T1.source.s, "T1.source.s")
    T2.source.s <- c(T2.source.s, "T2.source.s")
    T12.source.s <- c(T12.source.s, "T12.source.s")
    }
      parnames.reg <- c(parnames.reg, "w.reg","Rw.reg","T.reg","RT.reg")
  }

  ##value<< List with
  parnames <- list(parnames.V = parnames.V, ##<< Multipliers, e.g. V.geo.12i etc (no indices)
                   parnames.reg = parnames.reg, ##<< Regional mean parameters, e.g. w.reg (no indices)
                   parnames.subreg = parnames.subreg, ##<< Subregional mean parameters, e.g. w.subreg (no indices)
                   T1.source.s = T1.source.s, ##<< T11 of precision matrix trad/tot, modern/tot
                   T2.source.s = T2.source.s, ##<< T22 of precision matrix trad/tot, modern/tot
                   T12.source.s = T12.source.s, ##<< T12 of precision matrix trad/tot, modern/tot
                   parnames.h = parnames.h, ##<< Hyper parameters
                   parnames.c = parnames.c, ##<< Country parameters, e.g. omega.c (no indices)
                   parnames.theta = parnames.theta,##<< AR for unmet, e.g. theta.ci[193,3] (with indices)
                   parnames.eta = parnames.eta,##<< AR for ration (with indices)
                   parnames.eps = parnames.eps, ##<< AR for total (with indices)
                   parnames.ss = parnames.ss, ##<< parameters related to service statistics
                   parnames.validation = parnames.validation ##<< NULL if no validation or not\code{JAGS},
                   ##else the q.j[j]'s and pred.logratio.ytrad.j[j]'s (with indices)
                   )
        if(!do.country.specific.run) {
            parnames <- c(parnames, list(
                   parnames.pred.density.unmet = parnames.pred.density.unmet,##<< samples from predictive densities
                   parnames.pred.density.bdown = parnames.pred.density.bdown,##<< samples from predictive densities
                   parnames.pred.density.tot = parnames.pred.density.tot,##<< samples from predictive densities
                   parnames.qjs = parnames.qjs, ##<< qjs if not doing validation
                   parnames.logdens.unmet = parnames.logdens.unmet, ##<< log-densities at input data for log ratio parameters
                   parnames.logdens.bdown = parnames.logdens.bdown, ##<< log-densities at input data for log ratio parameters
                   parnames.logdens.tot = parnames.logdens.tot ##<< log-densities at input data for log ratio parameters
                   ))
            }

        ## <---------- LEVEL MODEL ----------

    } else {

        ## ========== RATE MODEL ==========>

          parnames.c <- c("omega.c", "pmax.c", "setlevel.c","Romega.c" ,"RT.c", "Rmax.c", "unmet.intercept.c")

  parnames.V <- parnames.h <- parnames.subreg <- parnames.reg <-parnames.ss <- NULL # change JR, 20140612

  if (is.null(validation.list)){
    parnames.validation <- NULL
  } else {#validation runs
    if (validation.list$exclude.unmet.only){
      #parnames.validation <- c("pred.logitratio.yunmet.j", "q.unmet.j")
      #if (JAGS){
      parnames.validation <- c(
        paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]"),
        paste0("q.unmet.j[", winbugs.data$getj.test.unmet.k, "]"),
        # save modern and trad as well to get the sampled total
        # (maybe more efficient to do in Bugs)
        paste0("pred.logratio.ytrad.j[", winbugs.data$getj.test.unmet.k, "]"),
        paste0("pred.logratio.ymodern.j[", winbugs.data$getj.test.unmet.k, "]")
      )
      #} # end JAGS
    }  else if(with(validation.list, isTRUE(at.end) || isTRUE(at.random))) { # validation for unmet and modern/trad
      #parnames.validation <- c("pred.logratio.ytrad.j", "pred.logratio.ymodern.j", "pred.logit.ytotal.j",
      #                            "pred.logitratio.yunmet.j")
      #if (JAGS){
      parnames.validation <- c(
        paste0("pred.logratio.ytrad.j[",
               winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
        #        paste0("q.trad.j[", winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
        paste0("pred.logratio.ymodern.j[",
               winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
        #        paste0("q.modern.j[", winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
        paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]")
        #        paste0("q.unmet.j[", winbugs.data$getj.test.unmet.k, "]")
      )
      #} # end JAGS
      } else if(isTRUE(validation.list$at.random.no.data)) { # end validation unmet/trad/modern
        parnames.validation <- c(
            paste0("pred.logratio.ytrad.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logratio.ymodern.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logitratio.yunmet.j[", winbugs.data$getj.test.unmet.k, "]"),
            paste0("pred.logit.ytot.j[",
                   winbugs.data$getj.test.k[(winbugs.data$n.test.breakdown + 1):winbugs.data$J.test], "]")
        )
          } else if(validation.list$leave.iso.out) {
        parnames.validation <- c(
            paste0("pred.logratio.ytrad.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]"),
            paste0("pred.logratio.ymodern.j[",
                   winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown], "]")
        )
    } else stop("Parameters to save for this validation type have not been determined; fix 'GetParNames()'.")
  } # end validation
  parnames.theta <- "theta.ci"
  parnames.eps <- "eps.ci"
  parnames.eta <- "eta.ci"
  parnames.eps <- parnames.theta <- parnames.eta <- NULL
        for (c in 1:winbugs.data$C){

            for (i in 1:winbugs.data$N.obsperiod.c[c]){ #Change NC, 20160602
                parnames.eps <- c(parnames.eps, paste0("eps.ci[", c, ",", i, "]"))
            }

            for (i in 1:winbugs.data$N.unique.c[c]){
                parnames.theta <- c(parnames.theta, paste0("theta.ci[", c, ",", i, "]"))
                parnames.eta <- c(parnames.eta, paste0("eta.ci[", c, ",", i, "]"))
            }
        }

        if(include.c.no.data) {
            for(c in (winbugs.data$C + 1):(winbugs.data$C + winbugs.data$C.no.data)) {
                parnames.theta <- c(parnames.theta, paste0("theta.ci[", c, ",", 1, "]"))
                parnames.eps <- c(parnames.eps, paste0("eps.ci[", c, ",", 1, "]"))
                parnames.eta <- c(parnames.eta, paste0("eta.ci[", c, ",", 1, "]"))
            }
        }

        if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
            for(c in (winbugs.data$C + 1):(winbugs.data$C + winbugs.data$C.no.data)) {
                for (i in 1:winbugs.data$N.unique.c.test[c - winbugs.data$C]){
                    parnames.theta <- c(parnames.theta, paste0("theta.ci[", c, ",", i, "]"))
                    parnames.eta <- c(parnames.eta, paste0("eta.ci[", c, ",", i, "]"))
                }
                for (i in 1:winbugs.data$N.obsperiod.c.test[c - winbugs.data$C]){
                    parnames.eps <- c(parnames.eps, paste0("eps.ci[", c, ",", i, "]"))
                }
            }
        }

  if (!do.country.specific.targets.run) # change JR, 20150619
    parnames.V <- c("V.geo.12i", "V.age.12i", "V.hw.12i", "V.emal.12i",
                    "V.sa.12i", "V.posbias.12i", "V.posage.12i", "V.negage.12i")
  if (!do.country.specific.run & !do.country.specific.targets.run) { # for global run # change JR, 20150301
    # Note that there's a function for parnames.V for plotting
## [MCW-2018-01-30 (1)] There are now 6 data source types
      parnames.h <- c(parnames.h,
                    "nonsample.se.trad.s[1]","nonsample.se.trad.s[2]","nonsample.se.trad.s[3]","nonsample.se.trad.s[4]","nonsample.se.trad.s[5]","nonsample.se.trad.s[6]",
                    "nonsample.se.modern.s[1]","nonsample.se.modern.s[2]","nonsample.se.modern.s[3]","nonsample.se.modern.s[4]","nonsample.se.modern.s[5]","nonsample.se.modern.s[6]",
                    "nonsample.se.unmet.s[1]","nonsample.se.unmet.s[2]",
                    "cor.trad.modern.s[1]","cor.trad.modern.s[2]","cor.trad.modern.s[3]","cor.trad.modern.s[4]","cor.trad.modern.s[5]","cor.trad.modern.s[6]",
                    "sigma.sourcetot", "sigma.unmet.dhs", "sigma.unmet.other",
                    "sigma.ar.unmet", "rho.unmet",
                    "a.unmet", "b.unmet", "c.unmet",
                    "sigma.unmetc",
                    "lp.world", "lr.world", "sigma.lrc", "sigma.lpc",
                    "rho.tot", "sigma.tot", "rho.rat", "sigma.rat",
                    "sigma.geo.m[1]", "sigma.geo.m[2]",
                    "sigma.pos",
                    "mu.pos.m[1]", "mu.pos.m[2]",
                    "v.abs.probe.q", "v.folk", "v.mneg", "v.mpos",
                    "sigma.wc", "sigma.Rwc", "sigma.Sc", "sigma.RTc",
                    # the following parameters are not found in WriteCountryModel at all
                    "sigma.unmetworld",
                    "w.world", "Rw.world", "RT.world","S.world","Shigher","sigma.higherSc",
                    "sigma.wreg", "sigma.Rwreg", "sigma.RTreg","sigma.Sreg",
                    "sigma.wsubreg", "sigma.Rwsubreg", "sigma.RTsubreg","sigma.Ssubreg")
      if(ModelFunctionModOnlyObs(write.model.fun)) {
          parnames.h <- c(parnames.h, "sigma.sourcemodonly")
          }
    parnames.reg <- c(parnames.reg, "w.reg","Rw.reg","RT.reg","S.reg")
    parnames.subreg <- c(parnames.subreg, "w.subreg","Rw.subreg","RT.subreg", "unmet.subreg","S.subreg")
  } else if (do.country.specific.run & do.country.specific.targets.run) { # for country-specific targets run # change JR, 20150301
    parnames.h <- c(parnames.h,
                    "sigma.unmetc", "sigma.lrc", "sigma.lpc",
                    "sigma.wc", "sigma.Rwc", "sigma.RTc","sigma.Sc")
    parnames.subreg <- c(parnames.subreg, "w.subreg","Rw.subreg","RT.subreg", "unmet.subreg","S.subreg")
  }

  ##value<< List with
  parnames <- list(parnames.V = parnames.V, ##<< Multipliers, e.g. V.geo.12i etc (no indices)
                   parnames.reg = parnames.reg, ##<< Regional mean parameters, e.g. w.reg (no indices)
                   parnames.subreg = parnames.subreg, ##<< Subregional mean parameters, e.g. w.subreg (no indices)
                   parnames.h = parnames.h, ##<< Hyper parameters
                   parnames.c = parnames.c, ##<< Country parameters, e.g. omega.c (no indices)
                   parnames.theta = parnames.theta,##<< AR for unmet, e.g. theta.ci[193,3] (with indices)
                   parnames.eta = parnames.eta,##<< AR for ration (with indices)
                   parnames.eps = parnames.eps, ##<< AR for total (with indices)
                   parnames.ss = parnames.ss, ##<< parameters related to service statistics
                   parnames.validation = parnames.validation##<< NULL if no validation or not\code{JAGS},
                   ##else the q.j[j]'s and pred.logratio.ytrad.j[j]'s (with indices)
  )

        ## <========== RATE MODEL ==========
    }

  return(parnames)
}

#----------------------------------------------------------------------------------
InternalGetParnamesV <- function( # Get nice parameter names for the multipliers
  ### Get nice parameter names for the multipliers
  winbugs.data, ##<< Object
  name.short.j ##<< country names, e.g. data$name.j
  ,marital.group = "MWRA"
){
  parnames.V.in.bugs <- NULL
  parnames.V.nice <- NULL
  if (winbugs.data$ncat.geo > 1) { # change JR, 20131104

    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.geo, ind.j = winbugs.data$geo.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.geo){
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.geo.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$geo.ind.j==i][1], " geo, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$geo.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.geo.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$geo.ind.j==i][1], " geo, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$geo.ind.j==i),")"))
    }
  }
  if (winbugs.data$ncat.age > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.age, ind.j = winbugs.data$age.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.age){
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.age.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$age.ind.j==i][1], " age, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$age.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.age.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$age.ind.j==i][1], " age, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$age.ind.j==i),")"))
    }
  }
  if (winbugs.data$ncat.hw > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.hw, ind.j = winbugs.data$hw.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.hw){
        if(marital.group == "MWRA") {
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.hw.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$hw.ind.j==i][1], " HW, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$hw.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.hw.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$hw.ind.j==i][1], " HW, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$hw.ind.j==i),")"))
        } else if(marital.group == "UWRA") {
            ## /W/ith /P/artner
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.hw.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$hw.ind.j==i][1], " WP, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$hw.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.hw.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$hw.ind.j==i][1], " WP, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$hw.ind.j==i),")"))
            }
    }
  }
  if (winbugs.data$ncat.emal > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.emal, ind.j = winbugs.data$emal.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.emal){
        if(marital.group == "MWRA") {
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.emal.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$emal.ind.j==i][1], " EM/AL, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$emal.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.emal.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$emal.ind.j==i][1], " EM/AL, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$emal.ind.j==i),")"))
        } else if(marital.group == "UWRA") {
            ## '/S/terylization /O/nly
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.emal.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$emal.ind.j==i][1], " SO, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$emal.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.emal.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$emal.ind.j==i][1], " SO, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$emal.ind.j==i),")"))
                     }
    }
  }
  if (winbugs.data$ncat.sa > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.sa, ind.j = winbugs.data$sa.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.sa){
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.sa.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$sa.ind.j==i][1], " SA, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$sa.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.sa.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$sa.ind.j==i][1], " SA, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$sa.ind.j==i),")"))
    }
  }
  if (winbugs.data$ncat.posbias > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.posbias, ind.j = winbugs.data$posbias.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.posbias){
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.posbias.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$posbias.ind.j==i][1], " +, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$posbias.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.posbias.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$posbias.ind.j==i][1], " +, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$posbias.ind.j==i),")"))
    }
  }
  if (winbugs.data$ncat.posage > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.posage, ind.j = winbugs.data$posage.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.posage){
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.posage.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$posage.ind.j==i][1], " +Age, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$posage.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.posage.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$posage.ind.j==i][1], " +Age, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$posage.ind.j==i),")"))
    }
  }
  if (winbugs.data$ncat.negage > 1) { # change JR, 20131104
    catindex.temp <- GetCatIndex(ncat = winbugs.data$ncat.negage, ind.j = winbugs.data$negage.ind.j,
                                 name.short.j = name.short.j) # change JR, 20131112
    for (i in 2:winbugs.data$ncat.negage){
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.negage.12i[1,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$negage.ind.j==i][1], " -Age, trad",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$negage.ind.j==i),")"))
      parnames.V.in.bugs <- c(parnames.V.in.bugs, paste0("V.negage.12i[2,", i, "]"))
      parnames.V.nice <- c(parnames.V.nice, paste0(name.short.j[winbugs.data$negage.ind.j==i][1], " -Age, mod",
                                                   catindex.temp[i], # change JR, 20131112
                                                   " (", sum(winbugs.data$negage.ind.j==i),")"))
    }
  }
  ##value<<
  return(list(parnames.V.in.bugs = parnames.V.in.bugs, ##<< vector with parnames used in Bugs
              parnames.V.nice = parnames.V.nice ##<< vector with longer description
              ## for each parname, including country, pop group, mod/trad and number of observations
  ))
}

GetCatIndex <- function(# Get category index
  ### Internal function to get category index
  ncat,
  ind.j,
  name.short.j
) {
  catindex <- NULL
  name.previous <- " "
  for (i in 2:ncat){
    name.current <- name.short.j[ind.j==i][1]
    if (name.current == name.previous) {
      cat.by.country <- cat.by.country + 1
    } else {
      cat.by.country <- 1
    }
    catindex <- c(catindex, cat.by.country)
    name.previous <- name.current
  }
  catindex <- ifelse(diff(c(catindex, 1)) == 0, "", paste0(" ", catindex))
  # add dummy first element
  catindex <- c(0, catindex)
  return(catindex)
}

###-----------------------------------------------------------------------------
### Handle write model functions

## [MCW-2017-02-07-5] :: Created this as part of managing the use of different
## model functions. This function determines if the model function causes
## non-default behaviour. Useful for debugging.
ModelFunctionCheck <- function(x) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- ModelFunctionFixSV(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses fixed source variances")

    li <- ModelFunctionCUnmetPrior(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses modified prior for 'c.unmet'")

    li <- ModelFunctionSATiming(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses sexual activity categories in the timing parameter (i.e., what used to be 'Tworld' and 'TOneLevel', originally developed for the 'rich', 'not/rich' categorization).")

    li <- ModelFunctionSA(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses sexual activity categories in the hierarchical structure")

    li <- ModelFunctionRegInSA(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses major regions nested in sexual activity among unmarried women categories")

    li <- ModelFunctionSubRegInSA1India(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses hierarchy wherein /sub/-regions are nested in sexual activity category 1, major areas + India still nested within sexual activity 0.")

    li <- ModelFunctionInclNoData(x, return.functions = TRUE)
    if(x %in% li) {
        message(x, " is capable of producing estimates for countries with no data")
    } else {
        message(x, " is NOT capable of producing estimates for countries with no data")
    }

    li <- ModelFunctionSAAsymp(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses different asymptotes for total CP depending on sexual activity category")

    li <- ModelFunctionSurveySEs(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses survey-based standard errors")

    li <- ModelFunctionSignedMultipliersEAHW(x, return.functions = TRUE)
    if(x %in% li) message(x, " does allow 'EA' and 'HW' biases to be modelled as negative biases")

    li <- ModelFunctionLT1pcOwnSource(x, return.functions = TRUE)
    if(x %in% li) message(x, " does model observations with CP Any less than 1 percent with as if from their own data source")

    li <- ModelFunctionLeaveISOOut(x, return.functions = TRUE)
    if(x %in% li) message(x, " can be used with 'leave.iso.out' validation exercise")

    li <- ModelFunctionRateModel(x, return.functions = TRUE)
    if(x %in% li) message(x, " uses the RATE model")

    li <- ModelFunctionModOnlyObs(x, return.functions = TRUE)
    if(x %in% li) message(x, " models observations with only CP Modern")
}

## [MCW-2017-02-07-7] :: Created this as part of managing the use of different
## model functions. This function determines if a model function uses fixed
## source variances. Doing so requires changes to default BUGS variable
## initializations.
ModelFunctionFixSV <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## These are the functions known to use fixed source variances
    li <- c("WriteModel_InclNoData_FixSV"
           ,"WriteModel_InclNoData_SA_FixSV"
           ,"WriteModel_InclNoData_SA_Timing_FixSV"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## [MCW-2017-02-07-9] :: Created this as part of managing the use of different
## model functions. This function identifies model functions that use
## non-default prior for 'c.unmet'. The initial value for JAGS must be
## consistent with the new prior.
ModelFunctionCUnmetPrior <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## These are the functions known to use alternative prior for 'c.unmet'
    li <- c("WriteModel_Z1"
           ,"WriteModel_UWRA_geog_InclNoData"
           ,"WriteModel_InclNoData_FixSV"
           ,"WriteModel_InclNoData_SA"
           ,"WriteModel_InclNoData_SA_FixSV"
           ,"WriteModel_InclNoData_SA_Timing"
           ,"WriteModel_InclNoData_SA_Timing_FixSV"
           ,"WriteModel_InclNoData_SA_Asymp"
            ,"WriteModel_SEs"
           ,"WriteModel_SEs_InclNoData"
           ,"WriteModel_InclNoData_SA1SubIndia"
           ,"WriteModel_InclNoData_SA1SubIndia_Rate"
           ,"WriteModel_MWRA_geog_1519_InclNoData"
            ,"WriteModel_MWRA_Geog_Rate_1519_InclNoData"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## [MCW-2017-03-17-3] :: Created this as part of managing the use of different
## model functions. This function identifies model functions that use sexual
## activity categories in the timing parameter (i.e., what used to be 'Tworld'
## and 'TOneLevel', originally developed for the 'rich', 'not/rich'
## categorization).
ModelFunctionSATiming <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_InclNoData_SA_Timing"
            ,"WriteModel_InclNoData_SA_Timing_FixSV"
            ,"WriteModel_InclNoData_SA"
           ,"WriteModel_InclNoData_SA_FixSV"
            ,"WriteModel_InclNoData_SA_Asymp"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## [MCW-2017-03-17-3] :: Created this as part of managing the use of different
## model functions. This function identifies model functions that use sexual
## activity categories in the hierarchy.
ModelFunctionSA <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## All functions in ModelFunctionRegInSA() /or/
    ## ModelFunctionSubRegInSA1India() /or/ ModelFunctionSATiming()
    ## should be listed here.
    li <- unique(c(ModelFunctionRegInSA(x, return.functions = TRUE)
                  ,ModelFunctionSubRegInSA1India(x, return.functions = TRUE)
                   ,ModelFunctionSATiming(x, return.functions = TRUE)
                   ))

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)
}

## [MCW-2017-02-09-1] :: Created this as part of managing the use of different
## model functions. This function identifies model functions that use hierarchy
## wherein major regions are nested in sexual activity categories.
ModelFunctionRegInSA <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## Cannot be in this /and/ ModelFunctionSubRegInSA1India().
    li <- c("WriteModel_InclNoData_SA"
            ,"WriteModel_InclNoData_SA_FixSV"
           ,"WriteModel_InclNoData_SA_Asymp"
            ,"WriteModel_InclNoData_SA_SEs"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## [MCW-2017-06-16-2] :: Created this as part of managing the use of different
## model functions. This function identifies model functions that use hierarchy
## wherein /sub/-regions are nested in sexual activity category 1, major areas +
## India still nested within sexual activity 0.
ModelFunctionSubRegInSA1India <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## Cannot be in this /and/ ModelFunctionSubRegInSA().
    li <- c("WriteModel_InclNoData_SA1SubIndia"
            ,"WriteModel_InclNoData_SA1SubIndia_Rate")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## [MCW-2017-03-08-14] :: Created to identify model functions that produce
## estimates for countries with no data.
ModelFunctionInclNoData <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## These are the functions known to produce estimates for countries with no
    ## data.
    li <- c("WriteModel_UWRA_geog_InclNoData"
           ,"WriteModel_InclNoData_FixSV"
            ,"WriteModel_InclNoData_SA"
           ,"WriteModel_InclNoData_SA_FixSV"
           ,"WriteModel_InclNoData_SA_Timing_FixSV"
           ,"WriteModel_InclNoData_SA_Timing"
           ,"WriteModel_InclNoData_SA_Asymp"
           ,"WriteModel_SEs_InclNoData"
           ,"WriteModel_InclNoData_SA_SEs"
           ,"WriteModel_InclNoData_SA1SubIndia"
           ,"WriteModel_InclNoData_SA1SubIndia_Rate"
           ,"WriteModel_MWRA_geog_1519_InclNoData"
           ,"WriteModel_MWRA_Geog_Rate_1519_InclNoData"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)
}

## [MCW-2017-03-22-3] :: Created to identify functions that use different
## asymptotes for total CP depending on sexual activity category.
ModelFunctionSAAsymp <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## These are the functions known to use different asymptotes for total CP
    ## depending on sexual activity category.
    li <- c("WriteModel_InclNoData_SA_Asymp")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## [MCW-2017-04-20] :: Created to identify functions that use survey-based
## standard errors.
ModelFunctionSurveySEs <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## These are the functions known to use survey-based standard error
    ## estimates.
    ## NB: All rate models also use survey-based SEs and will be
    ## appended (if not listed below).
    li <- c("WriteModel_SEs"
           ,"WriteModel_SEs_InclNoData"
           ,"WriteModel_InclNoData_SA_SEs"
           ,"WriteModel_MWRA_Geog_Rate"
           ,"WriteModel_InclNoData_SA1SubIndia_Rate"
           ,"WriteModel_MWRA_Geog_Rate_1519_InclNoData"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) {
        out <- unique(c(li, ModelFunctionRateModel(x, return.functions = TRUE)))
    } else {
        out <- isTRUE(x %in% li || ModelFunctionRateModel(x, return.functions = FALSE))
    }
    return(out)
}

## [MCW-2017-06-07-1] :: Created to identify functions that allow 'EA'
## multiplier to be < 1 and and 'HW' multiplier to be > 1.
ModelFunctionSignedMultipliersEAHW <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    ## These are the functions known to allow 'EA' and 'HW'
## biases to be negative.
    li <- c("WriteModel"
            ,"WriteModel_UWRA_geog"
            ,"WriteModel_UWRA_geog_InclNoData"
            ,"WriteModel_InclNoData_SA_Timing"
            ,"WriteModel_InclNoData_SA"
           ,"WriteModel_InclNoData_SA_SEs"
           ,"WriteModel_InclNoData_SA1SubIndia"
           ,"WriteModel_MWRA_Geog"
            ,"WriteModel_InclNoData_SA1SubIndia_Rate"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## 2017-09-08 :: Created to identify functions that trigger putting obs with CP
## any < 1% into their own category.
ModelFunctionLT1pcOwnSource <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_InclNoData_SA_Timing"
            ,"WriteModel_InclNoData_SA_Timing_FixSV"
            ,"WriteModel_InclNoData_SA"
           ,"WriteModel_InclNoData_SA_SEs"
           ,"WriteModel_InclNoData_SA1SubIndia"
           ,"WriteModel_InclNoData_SA_Asymp"
           ,"WriteModel_UWRA_geog_InclNoData"
            ,"WriteModel_InclNoData_SA1SubIndia_Rate")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## 2017-10-16 :: Created to identify functions that work with 'leave.iso.out' validation exercise.
ModelFunctionLeaveISOOut <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_InclNoData_SA1SubIndia"
           ,"WriteModel_UWRA_geog_InclNoData"
            #,"WriteModel_InclNoData_SA1SubIndia_Rate"
            )

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## 2017-12-19 :: Created to identify functions that are the rate model
ModelFunctionRateModel <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_MWRA_Geog_Rate", "WriteModel_InclNoData_SA1SubIndia_Rate"
           ,"WriteModel_MWRA_Geog_Rate_1519_InclNoData")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)

}

## 2018-01-02 :: Created to identify functions that produce predictive
## densities and WAIC estimates.
ModelFunctionPredDens <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_UWRA_geog_InclNoData", "WriteModel_InclNoData_SA1SubIndia")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)
}

## 2018-02-02 :: Created to identify functions that can do one country runs
ModelFunctionOneCountry <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_InclNoData_SA1SubIndia"
           ,"WriteModel_InclNoData_SA1SubIndia_Rate"
           ,"WriteModel_MWRA_Geog_Country"
            ,"WriteModel_MWRA_Geog_Rate"
           ,"WriteModel_MWRA_Geog_Rate_1519_InclNoData")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)
}

## 2020-01-24 :: Created to identify functions that model observations
## with only CP Mod (no CP Trad) as their own data type.
ModelFunctionModOnlyObs <- function(x, return.functions = FALSE) {

    x <- as.character(x)
    if(length(x) != 1) stop("'write.model.fun' must be of length 1")

    li <- c("WriteModel_MWRA_Geog_Rate")

    ## Return a list of the functions, or the result of the check?
    if(return.functions) out <- li
    else out <- isTRUE(x %in% li)

    return(out)
}

#-----------------------------------------------------------------
# The End!

