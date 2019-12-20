##--------------------------------------------------------------------------
# F_constructoutput.R
# Leontine Alkema, 2011
#
# Modified by Mark Wheldon, 2016.
##--------------------------------------------------------------------------
ConstructOutput <- function(# Construct output for MCMC run
  ### Construct output for MCMC run: country trajectories, and results for countries and UNDP aggregates.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored, and new objects are added
  ## (if NULL, it's output/run.name, default from \code{runMCMC}).
  WRA.csv = NULL,  ##<< If \code{NULL},
  ## estimates of the number of MWRA that are included in package are used.
  ## To use an alternative csv file, use \\
  ## \code{MWRA.csv = .../Number-of-women-married-in union_15-49.csv},
  ## where ... refers to the file path where file is located.
  do.SS.run.first.pass = FALSE, ##<< do first pass of run with SS data? # change JR, 20140414
  seedAR = 1234, ##<< Seed for sampling the AR(1) country trajectories.
  start.year = 1990.5, ##<< First year of estimation period (will be centered at half-year)
  ## If given user-input, it will use min of 1990 and user input
  end.year = 2015.5, ##<< First year of estimation period (will be centered at half-year)
  ## If given user-input, it will use max of 2015 and user input
  years.change = matrix(c(1990.5, 2000.5,
                          2000.5, 2010.5,
                          1990.5, 2010.5,
                          2000.5, 2018.5),
                        ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
  ## containing yyyy1 and column 2 containing yyyy2 for calculating change yyyy1-yyyy2
  years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                           2000.5, 2005.5, 2010.5,
                           1995.5, 2000.5, 2005.5,
                           1990.5, 1995.5, 2000.5,
                           1990.5, 2000.5, 2010.5
                           ,2000.5, 2010.5, 2018.5),
                         ncol = 3, byrow = TRUE) ##<< Matrix with 3 columns, with column 1
  ## containing yyyy1, column 2 containing yyyy2 and column 3 containing yyyy3 for
  ## calculating change (yyyy2-yyyy3) - (yyyy1-yyyy2)
  ## The years 1990, 2000 and 2010 are always included.
  ## Years outside (\code{start.year}, \code{end.year}) are ignored.
  ## Mid-point years closest to the given \code{years.change} are used for calculations.

  ## HOTFIX [MCW-2016-02-24-4] Need to be able to turn these off for datafiles
  ## with no, e.g., developing regions.
 ,make.any.aggregates = TRUE
 ,countries.to.include.in.aggregates.csv = NULL ##<< country ISO codes that should be used to form aggregates. NULL means all.
  ,verbose = TRUE
  ){

  nrepeatARsampling = 1 # Removed from set of input (July 2012), not to be changed.
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name## , "/"
                            )
  filename.append <- ifelse(do.SS.run.first.pass, "_pre", "")
  load(file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda"))) # change JR, 20140418
    load(file.path(output.dir, paste0("mcmc.array", filename.append, ".rda"))) # change JR, 20140418

    if(mcmc.meta$general$marital.group == "UWRA") In.union  <- 0
    if(mcmc.meta$general$marital.group == "MWRA") In.union  <- 1

  parnames.list <- GetParNames(winbugs.data = mcmc.meta$winbugs.data,
                               validation.list  = mcmc.meta$validation.list,
                               do.country.specific.run = mcmc.meta$general$do.country.specific.run, # change JR, 20131104
                               ## [MCW-2016-08-26-5] Pass include.c.no.data through
                               include.c.no.data =mcmc.meta$general$include.c.no.data,
                               write.model.fun = mcmc.meta$general$write.model.fun
                               )

  ##------------------------------------------------------------------------------------------
  if (!file.exists(file.path(output.dir, paste0("res.country", filename.append, ".rda")))) {
    res.country <- GetCIs(mcmc.meta  = mcmc.meta,
                          mcmc.array = mcmc.array,
                          WRA.csv = WRA.csv,
                          # thin = 1000,
                          seed = seedAR,
                          include.AR = mcmc.meta$include.AR,
                          output.dir= output.dir,
                          nrepeatARsampling = nrepeatARsampling,
                          start.year = start.year,
                          end.year = end.year,
                          years.change = years.change,
                          years.change2 = years.change2,
                          in_union = In.union,
                          verbose = verbose) # change JR, 20140317
    save(res.country, file = file.path(output.dir, paste0("res.country", filename.append, ".rda"))) # change JR, 20140418
  } else {
      warning("'", paste0("res.country", filename.append, ".rda")
            ,"' already exists. Country CIs not re-created")#[MCW-2016-04-19-1]
                                                            #Added because this
                                                            #tripped me up at
                                                            #first.
  }
  ##------------------------------------------------------------------------------------------
  if (!do.SS.run.first.pass) {
  # Validation
  if (!is.null(mcmc.meta$validation.list)){
    Ps <- GetPercentilesLeftOut(data = mcmc.meta$data.raw$data,
                                mcmc.array = mcmc.array,
                                winbugs.data = mcmc.meta$winbugs.data,
                                validation.list = mcmc.meta$validation.list)
    save(Ps, file = file.path(output.dir, "Ps_validation.rda")) # change JR, 20140418
    return(invisible()) # no aggregates etc constructed for validation run
  }
  ##------------------------------------------------------------------------------------------
  # Posteriors of model parameters of logistic curves
      ## [MCW-2016-08-26-6] Estimate for countries with no data.
      if(mcmc.meta$general$include.c.no.data) {
          country.info.for.GetPar <-
              rbind(mcmc.meta$data.raw$country.info, mcmc.meta$data.raw$country.info.no.data)
      } else {
          country.info.for.GetPar <- mcmc.meta$data.raw$country.info
      }

      par.ciq <- GetParInfo(mcmc.array = mcmc.array, parnames.list = parnames.list,
                            winbugs.data = mcmc.meta$winbugs.data
                            ## [MCW-2016-08-26-7] Use new 'country.info' as defined above.
                          , country.info = country.info.for.GetPar)
  save(par.ciq, file = file.path(output.dir, paste0("par.ciq", filename.append, ".rda"))) # change JR, 20140418
  ##------------------------------------------------------------------------------------------
  do.country.specific.run <- mcmc.meta$general$do.country.specific.run
      if (!do.country.specific.run) {
          if(make.any.aggregates) {     #HOTFIX [MCW-2016-02-24-6] (explained above)
              if (!file.exists(file.path(output.dir, paste0("res.aggregate", filename.append, ".rda")))) {
    res.aggregate <- GetAggregates(run.name = run.name,
                                   output.dir = output.dir,
                                   years.change = years.change,
                                   years.change2 = years.change2
                                  ,countries.to.include.in.aggregates.csv = countries.to.include.in.aggregates.csv,
                                   verbose = verbose) # change JR, 20140317
    save(res.aggregate, file = file.path(output.dir, "res.aggregate.rda")) # change JR, 20140418
              } else {
                  warning("'", paste0("res.aggregate", filename.append, ".rda")
            ,"' already exists. Aggregate CIs not re-created")
                  }
          } #HOTFIX [MCW-2016-02-24-6] (explained above)
      }
  cat(paste0("Results constructed and saved to ", output.dir, "\n"))
  }
  #------------------------------------------------------------------------
  ##value<< NULL, output objects from class
  ## \code{\link{Results}} for all countries (\code{res.country.rda})
  ## and UNDP aggregates (\code{res.aggregate.rda}) are written to \code{output.dir}.
  ## Additional output written to the same directory: \code{par.ciq} with CIs for the parameters
  ## of the logistic curves for total and modern/total.
  ## For validation runs, output object \code{Ps} from class \code{\link{Validation.Results}}
  ## are added.
  return(invisible(NULL))
}
##--------------------------------------------------------------------------
##' Construct country trajectories for all women
##'
##' Combines uwra and mwra results to create estimates and projections for all
##' women of reproductive age.
##'
##' .. content for \details{} ..
##' @param uwra.output.dir
##' @param mwra.output.dir
##' @param est.years
##' @param years.change
##' @param years.change2
##' @return
##' @author Mark Wheldon
ConstructOutputAllWomen <-
    function(run.name = "test",
             uwra.output.dir,
             mwra.output.dir,
             awra.output.dir = NULL,
             est.years = NULL,
             WRA.csv,          #denominator counts for WRA
             years.change = matrix(c(1990.5, 2000.5,
                          2000.5, 2010.5,
                          1990.5, 2010.5,
                          2000.5, 2010.5,
                          2000.5, 2017.5,
                          2010.5, 2017.5),
                        ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
            years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                             2000.5, 2005.5, 2010.5,
                           1995.5, 2000.5, 2005.5,
                           1990.5, 1995.5, 2000.5,
                           1990.5, 2000.5, 2010.5,
                           2000.5, 2010.5, 2017.5),
                         ncol = 3, byrow = TRUE), ##<< Matrix with 3 columns, with column 1
            make.any.aggregates = TRUE,
            countries.to.include.in.aggregates.csv = NULL,
            verbose = TRUE
             ) {
        if (is.null(awra.output.dir)) {
          awra.output.dir <- uwra.output.dir
        }

        if (!dir.exists(awra.output.dir)) {
          dir.create(awra.output.dir)
        }

        if (!dir.exists(file.path(awra.output.dir, "countrytrajectories"))) {
          dir.create(file.path(awra.output.dir, "countrytrajectories"))
        }

        ## -------* Sub functions

        ## -------** Summarize outputs (calculate 95% PIs, PPPCs)

        CP.summ.f <- function(z) {
            quantile(z, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
        }

        CP.change.summ.f <- function(z) {
            c(quantile(z, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
             ,PPPC = mean(z > 0))
        }

        ## -------** Re-structure objects for output

        awra.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("0.025", "0.1", "0.5", "0.9", "0.975")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        awra.probs.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("Probability")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        awra.change.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("0.025", "0.1", "0.5", "0.9", "0.975", "PPPC")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        ## -------* Inputs

        ## -------** Constants

        ## Don't change these without checking which subfunctions
        ## depend on them (e.g., 'InternalMakeRatios()') AND what they
        ## are called in 'GetAggregatesAllWomen().
        counts.names <-
            c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet"
             ,"TradPlusUnmet")
        ratios.names <-
            c("Met Demand", "Met Demand with Modern Methods", "Modern/Total", "Z"
             ,"Modern Married Over All", "Trad Married Over All"
             ,"Unmet Married Over All"
             ,"Modern Unmarried Over All", "Trad Unmarried Over All"
             ,"Unmet Unmarried Over All"
              )
        probs.names <- c("Met Demand with Modern Methods >= 75%")

        ## -------** Country trajectories

        ## Directories
        countrytrajectories.dir.uwra <- file.path(uwra.output.dir, "countrytrajectories")
        countrytrajectories.dir.mwra <- file.path(mwra.output.dir, "countrytrajectories")

        ## Just load the first country from uwra and mwra to check dates and countries.
        load(file.path(countrytrajectories.dir.uwra
                      ,"P.tp3s_country1.rda"
                       ))
        uwra1.country <- P.tp3s
        load(file.path(countrytrajectories.dir.mwra
                      ,"P.tp3s_country1.rda"
                       ))
        mwra1.country <- P.tp3s

        ## -------** MCMC Meta

        load(file.path(uwra.output.dir, "mcmc.meta.rda"))
        uwra.mcmc.meta <- mcmc.meta

        load(file.path(mwra.output.dir, "mcmc.meta.rda"))
        mwra.mcmc.meta <- mcmc.meta

        uwra.winbugs.data <- uwra.mcmc.meta$winbugs.data
        mwra.winbugs.data <- mwra.mcmc.meta$winbugs.data

        ## -------** Get number of iterations

        ## Which is the smaller of mcmc arrays
        n.iters <- min(dim(uwra1.country)[3], dim(mwra1.country)[3])

        ## -------** Estimation years

        ## -------*** Single years

        uwra.years <- dimnames(uwra1.country)[[1]]
        mwra.years <- dimnames(mwra1.country)[[1]]

        if(is.null(est.years)) est.years <- uwra.years

        ## Check that estimation years all match
        if(!isTRUE(all.equal(uwra.years, mwra.years, est.years))) {
            est.years <- intersect(intersect(uwra.years, mwra.years), est.years)
            warning("The estimation years for married and unmarried do not match, or one or both do not match 'est.years'. Using the intersection (unmarried <and> married <and> 'est.years'):\n\t", paste0(est.years, collapse = ", "))
        }

        ## Make est.years numeric
        est.years <- as.numeric(est.years)

        ## -------*** Changes years

        if(!(all(c(years.change, years.change2) %in% est.years)))
            stop("Estimates/projections are not available for all 'years.change' and 'years.change2'.")

        ## (from 'GetInfoChange()')
        changes.years.names <- c(apply(years.change, 1, function(years) paste0(floor(years[1]), "-", floor(years[2]))),
                                 apply(years.change2, 1, function(years)
                                     paste0("Change (", floor(years[3]), "-", floor(years[2]), ") - (",
                                            floor(years[2]), "-", floor(years[1]), ")")))

        ## -------*** Country names and ISO codes

        if(uwra.mcmc.meta$general$include.c.no.data) {
            uwra.c.info <-
                rbind(uwra.mcmc.meta$data.raw$country.info
                     ,uwra.mcmc.meta$data.raw$country.info.no.data
                      )
        } else uwra.c.info <- uwra.mcmc.meta$data.raw$country.info

        if(mwra.mcmc.meta$general$include.c.no.data) {
            mwra.c.info <-
                rbind(mwra.mcmc.meta$data.raw$country.info
                     ,mwra.mcmc.meta$data.raw$country.info.no.data
                      )
        } else mwra.c.info <- mwra.mcmc.meta$data.raw$country.info

        ## -------*** AR Parameters estimated?

        include.AR <- mwra.mcmc.meta$include.AR & uwra.mcmc.meta$include.AR

        ## -------* All Women Counts

        ## If actually need to make the all women counts
        if(!file.exists(file.path(awra.output.dir, "res.country.all.women.rda"))) {

            ## -------** Checks

            ## -------*** One country runs?

            if(!identical(uwra.mcmc.meta$general$do.country.specific.run
                         ,mwra.mcmc.meta$general$do.country.specific.run
                          )) {
                if(uwra.mcmc.meta$general$do.country.specific.run) {
                    stop("Unmarried women run is a one country run but married women run is not.")
                } else if(mwra.mcmc.meta$general$do.country.specific.run) {
                    stop("Married women run is a one country run but unmarried women run is not.")
                }
            }

            ## -------** More Inputs

            ## -------*** Load population counts of married and unmarried women (denominators)

            ## -------**** Unmarried women

            if(verbose) message("\nLoading UWRA women population counts")
            uwra.denom.counts.li <-                      # a list, top-level elements are countries
                ReadWRA(est.years = est.years
                        ,winbugs.data = uwra.mcmc.meta$winbugs.data
                        ,country.info = uwra.c.info
                        ,WRA.csv = WRA.csv
                        ,return.iso = TRUE,
                        in_union = 0
                         )
            uwra.counts.iso <- uwra.denom.counts.li[[2]]
            uwra.denom.counts.li <- uwra.denom.counts.li[[1]]

            ## NOTES at this point
            ## -----

            ## uwra.c.info ::
            ## ~ Taken from uwra.mcmc.meta$data.raw$country.info (and
            ## country.info.no.data).
            ## ~ Indicates all countries for which there are MCMC trajectories.

            ## uwra.denom.counts.li ::
            ## ~ List with denominator counts for each country for
            ## which there are MCMC trajectories.
            ## ~ `length(uwra.denom.count.li) ==
            ## nrow(uwra.c.info)`. Countries for which there are MCMC
            ## trajectories but not denomintor counts have counts set
            ## to zero in this list.
            ## ~ Element names are the names of the countries as they
            ## are listed in `uwra.c.info`, linked by ISO code to the
            ## ISO codes in the .csv.

            ## uwra.counts.iso ::
            ## ~ Data frame with ISO codes and country names of _only_
            ## the countries listed in the denominator count input
            ## .csv.
            ## ~ `nrow(uwra.c.info) <= length(uwra.denom.counts.li)`;
            ## countries for which there are MCMC trajectories but no
            ## counts are still listed in `uwra.denom.counts.li` but
            ## the counts are all zeroes.

            ## -----

            ## CHECK to determine if countries in counts file match those in the MCMC output.
            if(length(uwra.denom.counts.li) > nrow(uwra.counts.iso)) {
                idx <- !(names(uwra.denom.counts.li) %in% uwra.counts.iso$name.c)
                not.in.mcmc <- names(uwra.denom.counts.li)[idx]
                message(paste("The following countries are in the MCMC output for unmarried women but not in the population counts (denominators) file:\n    "
                             ,paste(not.in.mcmc, collapse = ", ")
                             ,".\nThey will be removed."
                             ,sep = ""))
                uwra.denom.counts.li <- uwra.denom.counts.li[!idx]
            }
            if(length(uwra.denom.counts.li) < nrow(uwra.counts.iso)) {
                idx <- !(uwra.counts.iso$name.c %in% names(uwra.denom.counts.li))
                not.in.counts <- uwra.counts.iso$name.c[idx]
                stop(paste("The following countries are in the MCMC output but there are no counts for them:\n    "
                          ,paste(not.in.counts, collapse = ", ")
                          ,".\nPlease add counts to the counts file an re-run."
                          ,sep = ""))
            }

            ## NOTES at this point
            ## -----

            ## uwra.denom.counts.li ::
            ## ~ `length(uwra.denom.counts.li) == nrow(uwra.counts.iso)` because
            ## the counts for which there are no MCMC trajectories have been
            ## removed.

            ## -----

            ## -------**** Married women

            if(verbose) message("\nLoading MWRA women population counts")
            mwra.denom.counts.li <-  ReadWRA(est.years = est.years
                                             ,winbugs.data = mwra.mcmc.meta$winbugs.data
                                             ,country.info = mwra.c.info
                                             ,WRA.csv = WRA.csv
                                             ,return.iso = TRUE,
                                             in_union = 1
                                              )
            mwra.counts.iso <- mwra.denom.counts.li[[2]]
            mwra.denom.counts.li <- mwra.denom.counts.li[[1]]

            ## CHECK to determine if countries in counts file match those in the MCMC output.
            if(length(mwra.denom.counts.li) > nrow(mwra.counts.iso)) {
                idx <- !(names(mwra.denom.counts.li) %in% mwra.counts.iso$name.c)
                not.in.mcmc <- names(mwra.denom.counts.li)[idx]
                message(paste("The following countries are in the MCMC output for married women but not in the population counts (denominators) file:\n    "
                             ,paste(not.in.mcmc, collapse = ", ")
                             ,".\nThey will be removed."
                             ,sep = ""))
                mwra.denom.counts.li <- mwra.denom.counts.li[!idx]
            }
            if(length(mwra.denom.counts.li) < nrow(mwra.counts.iso)) {
                idx <- !(mwra.counts.iso$name.c %in% names(mwra.denom.counts.li))
                not.in.counts <- mwra.counts.iso$name.c[idx]
                stop(paste("The following countries are in the MCMC output but there are no counts for them:\n    "
                          ,paste(not.in.counts, collapse = ", ")
                          ,".\nPlease add counts to the counts file an re-run."
                          ,sep = ""))
            }

            ## -------*** Get ISOs in all inputs

            ## ISOs in both UWRA and MWRA Countrytrajectories
            iso.both <- intersect(mwra.c.info$iso.c, uwra.c.info$iso.c)

            ## CHECK
            if(identical(as.double(length(iso.both)), 0)) {
                stop("The married and unmarried runs have no countries (by ISO code) in common.")
                }

            ## Intersect trajectory countries with denominator counts countries
            iso.both <-
                intersect(iso.both, intersect(uwra.counts.iso$iso.c, mwra.counts.iso$iso.c))

            ## C names
            uwra.c.name.both <-
                sapply(iso.both, function(z) {
                    uwra.c.info$name.c[as.character(uwra.c.info$iso.c) == as.character(z)]
                })
            mwra.c.name.both <-
                sapply(iso.both, function(z) {
                    mwra.c.info$name.c[as.character(mwra.c.info$iso.c) == as.character(z)]
                })

            ## Indices for 'both' into UWRA and MWRA country info data
            ## frames. Country info data frames have all countries for
            ## which there are MCMC trajectories, not just those for
            ## which there are denominator counts.
            ##
            ## Need these to load correct countrytrajectories file.
            iso.idx.mwra.in.c.info <-
                as.numeric(sapply(iso.both, function(z) which(mwra.c.info$iso.c == z)))
            iso.idx.uwra.in.c.info <-
                as.numeric(sapply(iso.both, function(z) which(uwra.c.info$iso.c == z)))

            ## They had better be the same length!
            stopifnot(identical(length(iso.idx.mwra.in.c.info), length(iso.idx.uwra.in.c.info)))

            ## Indices for 'both' into UWRA and MWRA denominator counts lists,
            ## which have had zero counts removed.
            iso.idx.mwra.denom.li <-
                as.numeric(sapply(iso.both, function(z) which(mwra.counts.iso$iso.c == z)))
            iso.idx.uwra.denom.li <-
                as.numeric(sapply(iso.both, function(z) which(uwra.counts.iso$iso.c == z)))

            ## -------*** Tidy Up

            ## Memory useage has been a problem. This might help...
            rm(list = c("uwra1.country", "mwra1.country"))

            ## -------** COUNTRIES

            ## -------*** Make Output Lists

            ## These will eventually just have the CIs
            awra.CP.counts.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(5, length(est.years), length(counts.names))))
            awra.CP.props.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(5, length(est.years), length(counts.names))))
            awra.CP.ratios.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(5, length(est.years), length(ratios.names))))
            ## Probability met demand is greater than xx%
            awra.CP.probs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(1, length(est.years), length(probs.names))))

            ## Change quantities
            awra.CP.changecounts.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(6, length(changes.years.names)
                                               , length(counts.names))))
            awra.CP.changeprops.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(6, length(changes.years.names)
                                               , length(counts.names))))
            awra.CP.changeratios.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(6, length(changes.years.names)
                                               , length(ratios.names))))

            ## -------*** Loop over countries

            for(j in seq_along(iso.both)) {

                ## -------**** Set-up

                ## -------***** Indices

                ## For countrytrajectories
                iso.both.j <- iso.both[j]
                iso.idx.uwra.in.c.info.j <- iso.idx.uwra.in.c.info[j]
                iso.idx.mwra.in.c.info.j <- iso.idx.mwra.in.c.info[j]

                ## For denominator counts
                uwra.counts.iso.idx.j <- which(uwra.counts.iso$iso.c == iso.both.j)
                mwra.counts.iso.idx.j <- which(mwra.counts.iso$iso.c == iso.both.j)

                if(verbose) message("\nMaking all women estimates for ISO ", iso.both.j
                       ,"\n  Married women name: "
                       ,mwra.c.name.both[j], "; file: P.tp3s_country", iso.idx.mwra.in.c.info.j
                       ,"\nUnmarried women name: "
                       ,uwra.c.name.both[j], "; file: P.tp3s_country", iso.idx.uwra.in.c.info.j
                        )

                ## -------**** All Women Estimates

                ## -------***** Proportions: load for country j

                ## This has to be inside this loop because each country's props are
                ## saved in a different file.

                ## Need to select the correct file to load.
                load(file.path(countrytrajectories.dir.uwra
                              ,paste0("P.tp3s_country", iso.idx.uwra.in.c.info.j, ".rda")
                               ))
                uwra.CP.props.j <- P.tp3s[as.character(est.years),,1:n.iters]

                load(file.path(countrytrajectories.dir.mwra
                              ,paste0("P.tp3s_country", iso.idx.mwra.in.c.info.j, ".rda")
                               ))
                mwra.CP.props.j <- P.tp3s[as.character(est.years),,1:n.iters]

                ## -------***** Make arrays for output

                ## Make arrays to hold this country's denominator and CP counts that
                ## have same dims as mcmc outputs. CP counts are initially filled
                ## with the population counts (denominators) but will be multiplied
                ## by proportions later so they will end up with CP counts (this is,
                ## admittedly, confusing.. don't do again).
                uwra.denom.counts.j <- uwra.CP.counts.j <-
                    array(rep(uwra.denom.counts.li[[uwra.counts.iso.idx.j]], n.iters)
                         ,dim = c(length(uwra.denom.counts.li[[uwra.counts.iso.idx.j]])
                                 ,length(counts.names)
                                 ,n.iters
                                  )
                          )
                dimnames(uwra.denom.counts.j) <- dimnames(uwra.CP.counts.j) <-
                    list(dimnames(uwra.CP.props.j)[[1]]
                        ,counts.names
                         )

                mwra.denom.counts.j <- mwra.CP.counts.j <-
                    array(rep(mwra.denom.counts.li[[mwra.counts.iso.idx.j]], n.iters)
                         ,dim = c(length(mwra.denom.counts.li[[mwra.counts.iso.idx.j]])
                                 ,length(counts.names)
                                 ,n.iters
                                  )
                          )
                dimnames(mwra.denom.counts.j) <- dimnames(mwra.CP.counts.j) <-
                    list(dimnames(mwra.CP.props.j)[[1]]
                        ,counts.names
                         )

                tot.denom.counts.j <- uwra.denom.counts.j + mwra.denom.counts.j
                                # Array; 1st dim is year, 2nd dim is 'Total',
                                # 'Modern', etc., 3rd dim is iteration. Values
                                # are duplicated across iterations. This is used
                                # later to turn trajectories of CP counts into
                                # trajectories of CP proportions, hence the
                                # repetition across iterations to make the
                                # operation 'vector-valued'.
                                #
                                # UPDATE: Shouldn't need this to have a
                                # dimension for 'n.iters' because R has
                                # automatic recycling rules. Should be able to
                                # make this object 'uwra.denom.counts.j[,1] +
                                # mwra.denom.counts.j[,1]' and rely on the
                                # recyling. Implement if memory useage becomes a
                                # problem.

                uwra.CP.counts.j[,dimnames(uwra.CP.props.j)[[2]],] <-
                    uwra.denom.counts.j[,dimnames(uwra.CP.props.j)[[2]],] * uwra.CP.props.j
                mwra.CP.counts.j[,dimnames(mwra.CP.props.j)[[2]],] <-
                    mwra.denom.counts.j[,dimnames(mwra.CP.props.j)[[2]],] * mwra.CP.props.j

                ## Change quantities
                awra.CP.changecounts.j <-
                    array(0, dim = c(length(changes.years.names), length(counts.names)
                                    ,n.iters
                                     ))
                dimnames(awra.CP.changecounts.j) <- list(changes.years.names, counts.names)

                awra.CP.changeprops.j <-
                    array(0, dim = c(length(changes.years.names), length(counts.names)
                                    ,n.iters
                                     ))
                dimnames(awra.CP.changeprops.j) <- list(changes.years.names, counts.names)

                awra.CP.changeratios.j <-
                    array(0, dim = c(length(changes.years.names), length(ratios.names)
                                    ,n.iters
                                     ))
                dimnames(awra.CP.changeratios.j) <- list(changes.years.names, ratios.names)

                ## -------***** Calculate

                ## -------****** UWRA counts

                ## Calculate Total, TotalPlusUnmet, TradPlusUnmet
                uwra.CP.counts.j[,"Total",] <-
                    uwra.CP.counts.j[,"Modern",] + uwra.CP.counts.j[,"Traditional",]
                uwra.CP.counts.j[,"TotalPlusUnmet",] <-
                    uwra.CP.counts.j[,"Total",] + uwra.CP.counts.j[,"Unmet",]
                uwra.CP.counts.j[,"TradPlusUnmet",] <-
                    uwra.CP.counts.j[,"Traditional",] + uwra.CP.counts.j[,"Unmet",]

                ## -------****** MWRA counts

                ## Calculate Total, TotalPlusUnmet, TradPlusUnmet
                mwra.CP.counts.j[,"Total",] <-
                    mwra.CP.counts.j[,"Modern",] + mwra.CP.counts.j[,"Traditional",]
                mwra.CP.counts.j[,"TotalPlusUnmet",] <-
                    mwra.CP.counts.j[,"Total",] + mwra.CP.counts.j[,"Unmet",]
                mwra.CP.counts.j[,"TradPlusUnmet",] <-
                    mwra.CP.counts.j[,"Traditional",] + mwra.CP.counts.j[,"Unmet",]

                ## -------****** All women counts

                ## Fill saved proportions (Trad, Modern, Unmet)
                awra.CP.counts.j <- uwra.CP.counts.j + mwra.CP.counts.j

                ## Calculate Total, TotalPlusUnmet, TradPlusUnmet
                awra.CP.counts.j[,"Total",] <-
                    awra.CP.counts.j[,"Modern",] + awra.CP.counts.j[,"Traditional",]
                awra.CP.counts.j[,"TotalPlusUnmet",] <-
                    awra.CP.counts.j[,"Total",] + awra.CP.counts.j[,"Unmet",]
                awra.CP.counts.j[,"TradPlusUnmet",] <-
                    awra.CP.counts.j[,"Traditional",] + awra.CP.counts.j[,"Unmet",]

                ## -------****** All women proportions (CPs)

                awra.CP.props.j <- awra.CP.counts.j / tot.denom.counts.j

                ## -------****** All women ratios

                awra.CP.ratios.j <-
                    InternalMakeRatios(ratios.names = ratios.names
                                      ,counts.ar = awra.CP.counts.j
                                      ,tot.counts.mat = tot.denom.counts.j[,"Total",]
                                      ,uwra.counts.ar = uwra.CP.counts.j
                                      ,mwra.counts.ar = mwra.CP.counts.j
                                       )

                ## -------****** All women probabilities

                awra.CP.probs.j <-
                    InternalMakeProbs(probs.names = probs.names
                                     ,counts.ar = awra.CP.counts.j)

                ## -------****** All women change quantities (counts, props, ratios)

                for(i in seq_len(nrow(years.change))) { # "2000-1990", etc.
                    for(k in seq_len(n.iters)) {
                        a <- which(est.years == years.change[i,1])
                        b <- which(est.years == years.change[i,2])
                        awra.CP.changecounts.j[i, ,k] <-
                            awra.CP.counts.j[b, ,k] - awra.CP.counts.j[a, ,k]
                        awra.CP.changeprops.j[i, ,k] <-
                            awra.CP.props.j[b, ,k] - awra.CP.props.j[a, ,k]
                        awra.CP.changeratios.j[i, ,k] <-
                            awra.CP.ratios.j[b, ,k] - awra.CP.ratios.j[a, ,k]
                    }
                }
                for(i in seq_len(nrow(years.change2))) { # "(2000-1990) - (2010-2000)", etc.
                    for(k in seq_len(n.iters)) {
                        a <- which(est.years == years.change2[i,1])
                        b <- which(est.years == years.change2[i,2])
                        c <- which(est.years == years.change2[i,3])
                        awra.CP.changecounts.j[nrow(years.change) + i, ,k] <-
                            (awra.CP.counts.j[b, ,k] - awra.CP.counts.j[a, ,k]) -
                            (awra.CP.counts.j[c, ,k] - awra.CP.counts.j[b, ,k])
                        awra.CP.changeprops.j[nrow(years.change) + i, ,k] <-
                            (awra.CP.props.j[b, ,k] - awra.CP.props.j[a, ,k]) -
                            (awra.CP.props.j[c, ,k] - awra.CP.props.j[b, ,k])
                        awra.CP.changeratios.j[nrow(years.change) + i, ,k] <-
                            (awra.CP.ratios.j[b, ,k] - awra.CP.ratios.j[a, ,k]) -
                            (awra.CP.ratios.j[c, ,k] - awra.CP.ratios.j[b, ,k])
                    }
                }

                ## -------***** Save country trajectories

                save(awra.CP.counts.j,
                     file = file.path(awra.output.dir, "countrytrajectories"
                                     ,paste0("aw_ISO_", iso.both.j, "_counts.rda")))

                ## -------***** Summarize country props, counts, and ratios

                ## Estimation years
                awra.CP.counts.CIs.li[[j]] <-
                    apply(awra.CP.counts.j, c(1, 2), "CP.summ.f")
                awra.CP.props.CIs.li[[j]] <-
                    apply( awra.CP.props.j, c(1, 2), "CP.summ.f")
                awra.CP.ratios.CIs.li[[j]] <-
                    apply(awra.CP.ratios.j, c(1, 2), "CP.summ.f")
                awra.CP.probs.li[[j]] <-
                    array(apply(awra.CP.probs.j, c(1, 2), function(z) {
                        mean(z, na.rm = TRUE)
                    })
                   ,dim = c(1, dim(awra.CP.probs.j)[1:2])
                   ,dimnames = c("Probability", dimnames(awra.CP.probs.j)[1:2])
                    )               # Note that the first dim, of extent 1, will
                                # never be of larger extent because this will
                                # always be an array of
                                # probabilities. Therefore, the array will be
                                # filled with dim 2, 'est.years', moving
                                # fastest. If additional indicators are added to
                                # dim 3, e.g., 'any method > 75%', the array
                                # should still be filled correctly.

                ## Change quantities
                awra.CP.changecounts.CIs.li[[j]] <-
                    apply(awra.CP.changecounts.j, c(1, 2), "CP.change.summ.f")
                awra.CP.changeprops.CIs.li[[j]] <-
                    apply(awra.CP.changeprops.j, c(1, 2), "CP.change.summ.f")
                awra.CP.changeratios.CIs.li[[j]] <-
                    apply(awra.CP.changeratios.j, c(1, 2), "CP.change.summ.f")

                ## -------***** Tidy up a bit

                ## Objects could end up being quite big. Maybe this will help..?!
                rm(list = c("awra.CP.counts.j", "awra.CP.props.j", "awra.CP.ratios.j"
                           ,"awra.CP.probs.j"
                           ,"awra.CP.changecounts.j", "awra.CP.changeprops.j", "awra.CP.changeratios.j"
                           ,"uwra.CP.counts.j", "mwra.CP.counts.j"
                           ,"uwra.CP.props.j", "mwra.CP.props.j"
                           ,"tot.denom.counts.j"))

            } ## END: 'for(j in 1:length(iso.both))' (the country loop)

            ## -------*** Prepare Outputs

            ## Make output lists look like 'CIprop.Lg.Lcat.qt', etc.

            awra.CP.counts.CIs.li <-
                lapply(awra.CP.counts.CIs.li, "awra.outputs.f")
            awra.CP.props.CIs.li <-
                lapply(awra.CP.props.CIs.li, "awra.outputs.f")
            awra.CP.ratios.CIs.li <-
                lapply(awra.CP.ratios.CIs.li, "awra.outputs.f")
            awra.CP.probs.li <-
                lapply(awra.CP.probs.li, "awra.probs.outputs.f")

            awra.CP.changecounts.CIs.li <-
                lapply(awra.CP.changecounts.CIs.li, "awra.change.outputs.f", transp = TRUE)
            awra.CP.changeprops.CIs.li <-
                lapply(awra.CP.changeprops.CIs.li, "awra.change.outputs.f", transp = TRUE)
            awra.CP.changeratios.CIs.li <-
                lapply(awra.CP.changeratios.CIs.li, "awra.change.outputs.f", transp = TRUE)

            ## Country names
            names(awra.CP.counts.CIs.li) <- names(awra.CP.props.CIs.li) <-
                names(awra.CP.ratios.CIs.li) <- names(awra.CP.probs.li) <-
                names(awra.CP.changecounts.CIs.li) <- names(awra.CP.changeprops.CIs.li) <-
                names(awra.CP.changeratios.CIs.li) <-
                uwra.c.name.both

            ## Output
            res.country.all.women <- list(CIcount.Lg.Lcat.qt = awra.CP.counts.CIs.li
                                         ,CIprop.Lg.Lcat.qt = awra.CP.props.CIs.li
                                         ,CIratio.Lg.Lcat.qt = awra.CP.ratios.CIs.li
                                         ,metDemGT.Lg.Lcat.pr = awra.CP.probs.li
                                         ,changecount.Lg.Lcat.Ti = awra.CP.changecounts.CIs.li
                                         ,changeprop.Lg.Lcat.Ti = awra.CP.changeprops.CIs.li
                                         ,changeratio.Lg.Lcat.Ti = awra.CP.changeratios.CIs.li
                                         ,W.Lg.t = mapply(function(x, y) { x + y }
                                                         ,mwra.denom.counts.li[iso.idx.mwra.denom.li]
                                                         ,uwra.denom.counts.li[iso.idx.uwra.denom.li]
                                                         ,SIMPLIFY = FALSE
                                                          )
                                         ,iso.g = iso.both
                                          )

            save(res.country.all.women, file = file.path(awra.output.dir, "res.country.all.women.rda"))

        } else { ## END: countries
            warning("'res.country.all.women.rda' already exists. All women country CIs not re-created.")
        }

        ## -------* AGGREGATES

        if(make.any.aggregates) {

        if(!file.exists(file.path(awra.output.dir, "res.aggregate.all.women.rda"))) {

            res.aggregate.all.women <-
                GetAggregatesAllWomen(run.name = run.name,
                                      uwra.output.dir = uwra.output.dir,
                                      mwra.output.dir = mwra.output.dir,
                                      awra.output.dir = awra.output.dir,
                                      years.change = years.change,
                                      years.change2 = years.change2,
                                      WRA.csv = WRA.csv,
                                      countries.to.include.in.aggregates.csv = countries.to.include.in.aggregates.csv,
                                      verbose = verbose
                                      )

            save(res.aggregate.all.women, file = file.path(awra.output.dir, "res.aggregate.all.women.rda"))

        } else {
            warning("'res.aggregate.all.women.rda' already exists. All women aggregate CIs not re-created.")
        }
            }

        ## -------* Return

        return(invisible(NULL))
    }
##-----------------------------------------------------------------------------
##' Make ratio indicators (Met Demand, etc.)
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param ratios.names Names of 2nd dimension of ratios array.
##' @param counts.ar Array containing CP counts of quantities needed to
##'     calculate the CP ratios.
##' @param tot.counts.mat Matrix with population counts of all women (MWRA +
##'     UWRA)
##' @param uwra.counts.ar Array of counts for unmarried women
##' @param mwra.counts.ar Array of counts for married women
##' @param mwra.uwra.ratios Calculate the UWRA MWRA ratios (e.g.,
##'     "Trad Married Over All")
##' @param ratios.ar Array in which ratios will be put.
##' @return
##' @author
InternalMakeRatios <- function(ratios.names, counts.ar, tot.counts.mat,
                               uwra.counts.ar, mwra.counts.ar) {

    ratios.ar <- array(0, dim = c(dim(counts.ar)[1]
                                 ,length(ratios.names)
                                 ,dim(counts.ar)[3]
                                  )
                      ,dimnames = list(dimnames(counts.ar)[[1]]
                                      ,ratios.names
                                      ,dimnames(counts.ar)[[3]]
                                       )
                       )

    dim2names <- dimnames(ratios.ar)[[2]]

    if("Met Demand" %in% dim2names) {
        ratios.ar[,"Met Demand",] <-
            counts.ar[,"Total", , drop = FALSE] /
            (counts.ar[,"Total", , drop = FALSE] + counts.ar[,"Unmet", , drop = FALSE])
    }

    if("Met Demand with Modern Methods" %in% dim2names) {
        ratios.ar[,"Met Demand with Modern Methods", ] <-
            counts.ar[,"Modern", , drop = FALSE] /
            (counts.ar[,"Total", , drop = FALSE] + counts.ar[,"Unmet", , drop = FALSE])
    }

    if("Modern/Total" %in% dim2names) {
        ratios.ar[,"Modern/Total", ] <-
            counts.ar[,"Modern", , drop = FALSE] / counts.ar[,"Total", , drop = FALSE]
    }

    if("Z" %in% dim2names) {
        ## Should really fix this to be safe about dimension dropping.
        ratios.ar[,"Z", ] <-
            counts.ar[,"Unmet", ] /
            (tot.counts.mat - counts.ar[,"Total", ] -
             counts.ar[,"Unmet",])
    }

    if("Modern Married Over All" %in% dim2names) {
        ratios.ar[,"Modern Married Over All", ] <-
            mwra.counts.ar[,"Modern", , drop = FALSE] / counts.ar[,"Modern", , drop = FALSE]
    }

    if("Trad Married Over All" %in% dim2names) {
        ratios.ar[,"Trad Married Over All", ] <-
            mwra.counts.ar[,"Traditional", , drop = FALSE] / counts.ar[,"Traditional", , drop = FALSE]
    }

    if("Unmet Married Over All" %in% dim2names) {
        ratios.ar[,"Unmet Married Over All", ] <-
            mwra.counts.ar[,"Unmet", , drop = FALSE] / counts.ar[,"Unmet", , drop = FALSE]
    }

    if("Modern Unmarried Over All" %in% dim2names) {
        ratios.ar[,"Modern Unmarried Over All", ] <-
            uwra.counts.ar[,"Modern", , drop = FALSE] / counts.ar[,"Modern", , drop = FALSE]
    }

    if("Trad Unmarried Over All" %in% dim2names) {
        ratios.ar[,"Trad Unmarried Over All", ] <-
            uwra.counts.ar[,"Traditional", , drop = FALSE] / counts.ar[,"Traditional", , drop = FALSE]
    }

    if("Unmet Unmarried Over All" %in% dim2names) {
        ratios.ar[,"Unmet Unmarried Over All", ] <-
            uwra.counts.ar[,"Unmet", , drop = FALSE] / counts.ar[,"Unmet", , drop = FALSE]
    }

    return(ratios.ar)
}
##--------------------------------------------------------------------------
##' Make probabilities for one country.
##'
##' This is inside the country loop.
##'
##' .. content for \details{} ..
##' @param probs.names
##' @param counts.ar
##' @return
##' @author
InternalMakeProbs <-
    function(probs.names = probs.names
            ,counts.ar = awra.CP.counts.j) {

        probs.ar <- array(0, dim = c(dim(counts.ar)[1]
                                    ,length(probs.names)
                                  ,dim(counts.ar)[3]
                                   )
                         ,dimnames = list(dimnames(counts.ar)[[1]]
                                      ,probs.names
                                      ,dimnames(counts.ar)[[3]]
                                       )
                        )

        dim2names <- dimnames(probs.ar)[[2]]

        if("Met Demand with Modern Methods >= 75%" %in% dim2names) {
            probs.ar[ , "Met Demand with Modern Methods >= 75%", ] <-
            (counts.ar[,"Modern", , drop = FALSE] /
             (counts.ar[,"Total", , drop = FALSE] +
              counts.ar[,"Unmet", , drop = FALSE])
            ) >= 0.75
        }
        return(probs.ar)
    }

##--------------------------------------------------------------------------
##' Calculate posterior quantiles of proportion of users in a subset age group.
##'
##' Creates posterior samples for, e.g., proportion of all users that
##' are aged 15--19, and calculates quantiles.
##'
##' @param age.subset.output.dir Output directory for the subset age group (e.g., 15--19).
##' @param age.total.output.dir Output directory for the total (15--49) age group.
##' @param age.ratio.output.dir Output directory to save 'res.country.age.ratio.rda'.
##' @param est.years
##' @param run.name
##' @param age.subset.WRA.csv Denominator counts.
##' @param age.total.WRA.csv Denominator counts.
##' @param years.change
##' @return Saves quantiles in 'res.country.age.ratio.rda'.
##' @author Mark Wheldon
ConstructAgeRatios <-
    function(age.subset.output.dir,
             age.total.output.dir,
             age.ratio.output.dir = age.subset.output.dir,
             est.years = NULL,
             run.name = "test",
             age.subset.WRA.csv,
             age.total.WRA.csv,
             years.change = matrix(c(1990.5, 2000.5,
                                     2000.5, 2010.5,
                                     1990.5, 2010.5,
                                     2000.5, 2010.5,
                                     2000.5, 2017.5,
                                     2010.5, 2017.5),
                                   ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
             years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                                      2000.5, 2005.5, 2010.5,
                                      1995.5, 2000.5, 2005.5,
                                      1990.5, 1995.5, 2000.5,
                                      1990.5, 2000.5, 2010.5,
                                      2000.5, 2010.5, 2017.5),
                                    ncol = 3, byrow = TRUE) ##<< Matrix with 3 columns, with column 1
             ) {

        ## -------* Sub functions

        ## -------** Summarize outputs (calculate 95% PIs, PPPCs)

        CP.summ.f <- function(z) {
            quantile(z, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
        }

        CP.change.summ.f <- function(z) {
            c(quantile(z, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
             ,PPPC = mean(z > 0))
        }

        ## -------** Re-structure objects for output

        age.ratio.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("0.025", "0.1", "0.5", "0.9", "0.975")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        age.ratio.change.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("0.025", "0.1", "0.5", "0.9", "0.975", "PPPC")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        ## -------* Inputs

        ## -------** Constants

        ## Don't change these without checking which subfunctions
        ## depend on them (e.g., 'InternalMakeRatios()') AND what they
        ## are called in 'GetAggregatesAllWomen().
        counts.names <-
            c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet"
             ,"TradPlusUnmet")

        ## -------** Country trajectories

        ## Directories
        countrytrajectories.dir.subset <- file.path(age.subset.output.dir, "countrytrajectories")
        countrytrajectories.dir.total <- file.path(age.total.output.dir, "countrytrajectories")

        ## Just load the first country from each to check dates and countries.
        load(file.path(countrytrajectories.dir.subset
                      ,"P.tp3s_country1.rda"
                       ))
        subset1.country <- P.tp3s
        load(file.path(countrytrajectories.dir.total
                      ,"P.tp3s_country1.rda"
                       ))
        total1.country <- P.tp3s

        ## -------** MCMC Meta

        load(file.path(age.subset.output.dir, "mcmc.meta.rda"))
        age.subset.mcmc.meta <- mcmc.meta

        load(file.path(age.total.output.dir, "mcmc.meta.rda"))
        age.total.mcmc.meta <- mcmc.meta

        age.subset.winbugs.data <- age.subset.mcmc.meta$winbugs.data
        age.total.winbugs.data <- age.total.mcmc.meta$winbugs.data

        ## -------** Get number of iterations

        ## Which is the smaller of mcmc arrays
        n.iters <- min(dim(subset1.country)[3], dim(total1.country)[3])

        ## -------** Marital group

        if(mcmc.meta$general$marital.group == "UWRA") {
            UWRA <- TRUE
            marr.group <- "uw"
        } else {
            UWRA <- FALSE
            marr.group <- "mw"
        }

        ## -------** Estimation years

        ## -------*** Single years

        age.subset.years <- dimnames(subset1.country)[[1]]
        age.total.years <- dimnames(total1.country)[[1]]

        if(is.null(est.years)) est.years <- age.subset.years

        ## Check that estimation years all match
        if(!isTRUE(all.equal(age.subset.years, age.total.years, est.years))) {
            est.years <- intersect(intersect(age.subset.years, age.total.years), est.years)
            warning("The estimation years for age subset and age total (15--49) do not match, or one or both do not match 'est.years'. Using the intersection (age subset <and> age total <and> 'est.years'):\n\t", paste0(est.years, collapse = ", "))
        }

        ## Make est.years numeric
        est.years <- as.numeric(est.years)

        ## -------*** Changes years

        if(!(all(c(years.change, years.change2) %in% est.years)))
            stop("Estimates/projections are not available for all 'years.change' and 'years.change2'.")

        ## (from 'GetInfoChange()')
        changes.years.names <- c(apply(years.change, 1, function(years) paste0(floor(years[1]), "-", floor(years[2]))),
                                 apply(years.change2, 1, function(years)
                                     paste0("Change (", floor(years[3]), "-", floor(years[2]), ") - (",
                                            floor(years[2]), "-", floor(years[1]), ")")))

        ## -------*** Country names and ISO codes

        if(age.subset.mcmc.meta$general$include.c.no.data) {
            age.subset.c.info <-
                rbind(age.subset.mcmc.meta$data.raw$country.info
                     ,age.subset.mcmc.meta$data.raw$country.info.no.data
                      )
        } else age.subset.c.info <- age.subset.mcmc.meta$data.raw$country.info

        if(age.total.mcmc.meta$general$include.c.no.data) {
            age.total.c.info <-
                rbind(age.total.mcmc.meta$data.raw$country.info
                     ,age.total.mcmc.meta$data.raw$country.info.no.data
                      )
        } else age.total.c.info <- age.total.mcmc.meta$data.raw$country.info

        ## -------*** AR Parameters estimated?

        include.AR <- age.total.mcmc.meta$include.AR & age.subset.mcmc.meta$include.AR

        ## If actually need to make the age ratios
        if(!file.exists(file.path(age.ratio.output.dir, "res.country.age.age.ratio.rda"))) {

            ## -------* Checks

            ## -------** One country runs?

            if(!identical(age.subset.mcmc.meta$general$do.country.specific.run
                         ,age.total.mcmc.meta$general$do.country.specific.run
                          )) {
                if(age.subset.mcmc.meta$general$do.country.specific.run) {
                    stop("Age subset women run is a one country run but total (15--49) women run is not.")
                } else if(age.total.mcmc.meta$general$do.country.specific.run) {
                    stop("Total (15--49) women run is a one country run but age subset women run is not.")
                }
            }

            ## -------* More Inputs

            ## -------** Load population counts denominators

            ## -------*** Age subset women

            if(verbose) message("\nLoading age subset population counts")
            age.subset.denom.counts.li <-                      # a list, top-level elements are countries
                ReadWRA(est.years = est.years
                       ,winbugs.data = age.subset.mcmc.meta$winbugs.data
                       ,country.info = age.subset.c.info
                       ,WRA.csv = age.subset.WRA.csv
                       ,return.iso = TRUE,
                        in_union = as.numeric(!UWRA)
                        )
            age.subset.counts.iso <- age.subset.denom.counts.li[[2]]
            age.subset.denom.counts.li <- age.subset.denom.counts.li[[1]]

            ## NOTES at this point
            ## -----

            ## age.subset.c.info ::
            ## ~ Taken from age.subset.mcmc.meta$data.raw$country.info (and
            ## country.info.no.data).
            ## ~ Indicates all countries for which there are MCMC trajectories.

            ## age.subset.denom.counts.li ::
            ## ~ List with denominator counts for each country for
            ## which there are MCMC trajectories.
            ## ~ `length(age.subset.denom.count.li) ==
            ## nrow(age.subset.c.info)`. Countries for which there are MCMC
            ## trajectories but not denomintor counts have counts set
            ## to zero in this list.
            ## ~ Element names are the names of the countries as they
            ## are listed in `age.subset.c.info`, linked by ISO code to the
            ## ISO codes in the .csv.

            ## age.subset.counts.iso ::
            ## ~ Data frame with ISO codes and country names of _only_
            ## the countries listed in the denominator count input
            ## .csv.
            ## ~ `nrow(age.subset.c.info) <= length(age.subset.denom.counts.li)`;
            ## countries for which there are MCMC trajectories but no
            ## counts are still listed in `age.subset.denom.counts.li` but
            ## the counts are all zeroes.

            ## -----

            ## CHECK to determine if countries in counts file match those in the MCMC output.
            if(length(age.subset.denom.counts.li) > nrow(age.subset.counts.iso)) {
                idx <- !(names(age.subset.denom.counts.li) %in% age.subset.counts.iso$name.c)
                not.in.mcmc <- names(age.subset.denom.counts.li)[idx]
                message(paste("The following countries are in the MCMC output for age subset but not in the population counts (denominators) file:\n    "
                             ,paste(not.in.mcmc, collapse = ", ")
                             ,".\nThey will be removed."
                             ,sep = ""))
                age.subset.denom.counts.li <- age.subset.denom.counts.li[!idx]
            }
            if(length(age.subset.denom.counts.li) < nrow(age.subset.counts.iso)) {
                idx <- !(age.subset.counts.iso$name.c %in% names(age.subset.denom.counts.li))
                not.in.counts <- age.subset.counts.iso$name.c[idx]
                stop(paste("The following countries are in the MCMC output but there are no counts for them:\n    "
                          ,paste(not.in.counts, collapse = ", ")
                          ,".\nPlease add counts to the counts file an re-run."
                          ,sep = ""))
            }

            ## NOTES at this point
            ## -----

            ## age.subset.denom.counts.li ::
            ## ~ `length(age.subset.denom.counts.li) == nrow(age.subset.counts.iso)` because
            ## the counts for which there are no MCMC trajectories have been
            ## removed.

            ## -----

            ## -------*** Age total (15--49)

            if(verbose) message("\nLoading age total (15--49) population counts")
            age.total.denom.counts.li <-  ReadWRA(est.years = est.years
                                                 ,winbugs.data = age.total.mcmc.meta$winbugs.data
                                                 ,country.info = age.total.c.info
                                                 ,WRA.csv = age.total.WRA.csv
                                                 ,return.iso = TRUE,
                                                  in_union = as.numeric(!UWRA)
                                                  )
            age.total.counts.iso <- age.total.denom.counts.li[[2]]
            age.total.denom.counts.li <- age.total.denom.counts.li[[1]]

            ## CHECK to determine if countries in counts file match those in the MCMC output.
            if(length(age.total.denom.counts.li) > nrow(age.total.counts.iso)) {
                idx <- !(names(age.total.denom.counts.li) %in% age.total.counts.iso$name.c)
                not.in.mcmc <- names(age.total.denom.counts.li)[idx]
                message(paste("The following countries are in the MCMC output for age total (15--49) but not in the population counts (denominators) file:\n    "
                             ,paste(not.in.mcmc, collapse = ", ")
                             ,".\nThey will be removed."
                             ,sep = ""))
                age.total.denom.counts.li <- age.total.denom.counts.li[!idx]
            }
            if(length(age.total.denom.counts.li) < nrow(age.total.counts.iso)) {
                idx <- !(age.total.counts.iso$name.c %in% names(age.total.denom.counts.li))
                not.in.counts <- age.total.counts.iso$name.c[idx]
                stop(paste("The following countries are in the MCMC output but there are no counts for them:\n    "
                          ,paste(not.in.counts, collapse = ", ")
                          ,".\nPlease add counts to the counts file an re-run."
                          ,sep = ""))
            }

            ## -------** Get ISOs in all inputs

            ## ISOs in both SUBSET and TOTAL Countrytrajectories
            iso.both <- intersect(age.total.c.info$iso.c, age.subset.c.info$iso.c)

            ## CHECK
            if(identical(as.double(length(iso.both)), 0)) {
                stop("The age subset and age total (15--49) runs have no countries (by ISO code) in common.")
            }

            ## Intersect trajectory countries with denominator counts countries
            iso.both <-
                intersect(iso.both, intersect(age.subset.counts.iso$iso.c, age.total.counts.iso$iso.c))

            ## C names
            age.subset.c.name.both <-
                sapply(iso.both, function(z) {
                    age.subset.c.info$name.c[as.character(age.subset.c.info$iso.c) == as.character(z)]
                })
            age.total.c.name.both <-
                sapply(iso.both, function(z) {
                    age.total.c.info$name.c[as.character(age.total.c.info$iso.c) == as.character(z)]
                })

            ## Indices for 'both' into SUBSET and TOTAL country info data
            ## frames. Country info data frames have all countries for
            ## which there are MCMC trajectories, not just those for
            ## which there are denominator counts.
            ##
            ## Need these to load correct countrytrajectories file.
            iso.idx.age.total.in.c.info <-
                as.numeric(sapply(iso.both, function(z) which(age.total.c.info$iso.c == z)))
            iso.idx.age.subset.in.c.info <-
                as.numeric(sapply(iso.both, function(z) which(age.subset.c.info$iso.c == z)))

            ## They had better be the same length!
            stopifnot(identical(length(iso.idx.age.total.in.c.info), length(iso.idx.age.subset.in.c.info)))

            ## Indices for 'both' into SUBSET and TOTAL denominator counts lists,
            ## which have had zero counts removed.
            iso.idx.age.total.denom.li <-
                as.numeric(sapply(iso.both, function(z) which(age.total.counts.iso$iso.c == z)))
            iso.idx.age.subset.denom.li <-
                as.numeric(sapply(iso.both, function(z) which(age.subset.counts.iso$iso.c == z)))

            ## -------** Tidy Up

            ## Memory useage has been a problem. This might help...
            rm(list = c("subset1.country", "total1.country"))

            ## -------* COUNTRIES

            ## -------** Make Output Lists

            ## These will eventually just have the CIs
            age.ratio.CP.ratios.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(5, length(est.years), length(counts.names))))

            age.ratio.CP.changeratios.CIs.li <-
                lapply(iso.both,
                       function(z) array(dim = c(6, length(changes.years.names)
                                               , length(counts.names))))

            ## -------** Loop over countries

            for(j in seq_along(iso.both)) {

                ## -------*** Set-up

                ## -------**** Indices

                ## For countrytrajectories
                iso.both.j <- iso.both[j]
                iso.idx.age.subset.in.c.info.j <- iso.idx.age.subset.in.c.info[j]
                iso.idx.age.total.in.c.info.j <- iso.idx.age.total.in.c.info[j]

                ## For denominator counts
                age.subset.counts.iso.idx.j <- which(age.subset.counts.iso$iso.c == iso.both.j)
                age.total.counts.iso.idx.j <- which(age.total.counts.iso$iso.c == iso.both.j)

                if(verbose) message("\nMaking age ratio estimates for ISO ", iso.both.j
                       ,"\n  Age total (15--49) women name: "
                       ,age.total.c.name.both[j], "; file: P.tp3s_country", iso.idx.age.total.in.c.info.j
                       ,"\n  Age subset women name: "
                       ,age.subset.c.name.both[j], "; file: P.tp3s_country", iso.idx.age.subset.in.c.info.j
                        )

                ## -------*** Load proportions for country j

                ## This has to be inside this loop because each country's props are
                ## saved in a different file.

                load(file.path(countrytrajectories.dir.subset
                              ,paste0("P.tp3s_country", iso.idx.age.subset.in.c.info.j, ".rda")
                               ))
                age.subset.CP.props.j <- P.tp3s[as.character(est.years),,1:n.iters]

                load(file.path(countrytrajectories.dir.total
                              ,paste0("P.tp3s_country", iso.idx.age.total.in.c.info.j, ".rda")
                               ))
                age.total.CP.props.j <- P.tp3s[as.character(est.years),,1:n.iters]

                ## -------*** Make arrays for output

                ## Make arrays to hold this country's denominator and CP counts that
                ## have same dims as mcmc outputs. CP counts are initially filled
                ## with the population counts (denominators) but will be multiplied
                ## by proportions later so they will end up with CP counts (this is,
                ## admittedly, confusing.. don't do again).
                age.subset.denom.counts.j <- age.subset.CP.counts.j <-
                    array(rep(age.subset.denom.counts.li[[age.subset.counts.iso.idx.j]], n.iters)
                         ,dim = c(length(age.subset.denom.counts.li[[age.subset.counts.iso.idx.j]])
                                 ,length(counts.names)
                                 ,n.iters
                                  )
                          )
                dimnames(age.subset.denom.counts.j) <- dimnames(age.subset.CP.counts.j) <-
                    list(dimnames(age.subset.CP.props.j)[[1]]
                        ,counts.names
                         )

                age.total.denom.counts.j <- age.total.CP.counts.j <-
                    array(rep(age.total.denom.counts.li[[age.total.counts.iso.idx.j]], n.iters)
                         ,dim = c(length(age.total.denom.counts.li[[age.total.counts.iso.idx.j]])
                                 ,length(counts.names)
                                 ,n.iters
                                  )
                          )
                dimnames(age.total.denom.counts.j) <- dimnames(age.total.CP.counts.j) <-
                    list(dimnames(age.total.CP.props.j)[[1]]
                        ,counts.names
                         )

                ## Make array for age ratios
                age.ratio.CP.ratios.j <-
                    array(0, dim = c(length(est.years), length(counts.names)
                                    ,n.iters
                                     ))
                dimnames(age.ratio.CP.ratios.j) <- list(est.years, counts.names)

                ## Make array for change in age count ratios
                age.ratio.CP.change.countratios.j <-
                    array(0, dim = c(length(changes.years.names), length(counts.names)
                                    ,n.iters
                                     ))
                dimnames(age.ratio.CP.change.countratios.j) <- list(changes.years.names, counts.names)

                ## -------*** Calculate CP counts

                age.subset.CP.counts.j[,dimnames(age.subset.CP.props.j)[[2]],] <-
                    age.subset.denom.counts.j[,dimnames(age.subset.CP.props.j)[[2]],] * age.subset.CP.props.j
                age.total.CP.counts.j[,dimnames(age.total.CP.props.j)[[2]],] <-
                    age.total.denom.counts.j[,dimnames(age.total.CP.props.j)[[2]],] * age.total.CP.props.j

                ## -------*** Calculate Total, TotalPlusUnmet, TradPlusUnmet

                ## Age subset counts

                age.subset.CP.counts.j[,"Total",] <-
                    age.subset.CP.counts.j[,"Modern",] + age.subset.CP.counts.j[,"Traditional",]
                age.subset.CP.counts.j[,"TotalPlusUnmet",] <-
                    age.subset.CP.counts.j[,"Total",] + age.subset.CP.counts.j[,"Unmet",]
                age.subset.CP.counts.j[,"TradPlusUnmet",] <-
                    age.subset.CP.counts.j[,"Traditional",] + age.subset.CP.counts.j[,"Unmet",]

                ## Age total counts

                age.total.CP.counts.j[,"Total",] <-
                    age.total.CP.counts.j[,"Modern",] + age.total.CP.counts.j[,"Traditional",]
                age.total.CP.counts.j[,"TotalPlusUnmet",] <-
                    age.total.CP.counts.j[,"Total",] + age.total.CP.counts.j[,"Unmet",]
                age.total.CP.counts.j[,"TradPlusUnmet",] <-
                    age.total.CP.counts.j[,"Traditional",] + age.total.CP.counts.j[,"Unmet",]

                ## -------*** Calculate Age Ratios

                for(x in counts.names) {
                    age.ratio.CP.ratios.j[,x,] <-
                        age.subset.CP.counts.j[, x, ] /
                        age.total.CP.counts.j[dimnames(age.subset.CP.counts.j[, x, ])[[1]], x, ]
                }

                ## -------**** Change quantities (counts, props, ratios)

                for(i in seq_len(nrow(years.change))) { # "2000-1990", etc.
                    for(k in seq_len(n.iters)) {
                        a <- which(est.years == years.change[i,1])
                        b <- which(est.years == years.change[i,2])
                        age.ratio.CP.change.countratios.j[i, ,k] <-
                            age.ratio.CP.ratios.j[b, ,k] - age.ratio.CP.ratios.j[a, ,k]
                    }
                }
                for(i in seq_len(nrow(years.change2))) { # "(2000-1990) - (2010-2000)", etc.
                    for(k in seq_len(n.iters)) {
                        a <- which(est.years == years.change2[i,1])
                        b <- which(est.years == years.change2[i,2])
                        c <- which(est.years == years.change2[i,3])
                        age.ratio.CP.change.countratios.j[nrow(years.change) + i, ,k] <-
                            (age.ratio.CP.ratios.j[b, ,k] - age.ratio.CP.ratios.j[a, ,k]) -
                            (age.ratio.CP.ratios.j[c, ,k] - age.ratio.CP.ratios.j[b, ,k])
                    }
                }

                ## -------*** Save country trajectories

                save(age.ratio.CP.ratios.j,
                     file = file.path(age.ratio.output.dir, "countrytrajectories"
                                     ,paste0(marr.group, "_ISO_", iso.both.j, "_age_ratios.rda")))

                ## -------*** Summarize country ratios

                ## Estimation years
                age.ratio.CP.ratios.CIs.li[[j]] <-
                    apply(age.ratio.CP.ratios.j, c(1, 2), "CP.summ.f")

                ## Change quantities
                age.ratio.CP.changeratios.CIs.li[[j]] <-
                    apply(age.ratio.CP.change.countratios.j, c(1, 2), "CP.change.summ.f")

            } ## END: 'for(j in 1:length(iso.both))' (the country loop)

            ## -------** Prepare Outputs

            ## Make output lists look like 'CIprop.Lg.Lcat.qt', etc.
            age.ratio.CP.ratios.CIs.li <-
                lapply(age.ratio.CP.ratios.CIs.li, "age.ratio.outputs.f")

            age.ratio.CP.changeratios.CIs.li <-
                lapply(age.ratio.CP.changeratios.CIs.li, "age.ratio.change.outputs.f",
                       transp = TRUE)

            ## Country names
            names(age.ratio.CP.ratios.CIs.li) <- names(age.ratio.CP.changeratios.CIs.li) <-
                age.subset.c.name.both

            ## Output
            res.country.age.ratio <- list(CIcountratio.Lg.Lcat.qt = age.ratio.CP.ratios.CIs.li
                                         ,changecountratio.Lg.Lcat.Ti = age.ratio.CP.changeratios.CIs.li
                                         ,iso.g = iso.both
                                          )

            save(res.country.age.ratio, file = file.path(age.ratio.output.dir, "res.country.age.ratio.rda"))

        } else { ## END: countries
            warning("'res.country.age.ratio.rda' already exists. Age subset country CIs not re-created.")
        }
    }
## ----------------------------------------------------------------------
##' Construct country age ratios for all women.
##'
##' @param age.subset.output.dir
##' @param age.total.output.dir
##' @param age.ratio.output.dir
##' @param est.years
##' @param run.name
##' @param age.subset.WRA.csv
##' @param age.total.WRA.csv
##' @param years.change
##' @param ncol
##' @param byrow
##' @return
##' @author Mark Wheldon
ConstructAgeRatiosAllWomen <-
    function(age.subset.output.dir,
             age.total.output.dir,
             age.ratio.output.dir = age.subset.output.dir,
             est.years = NULL,
             run.name  =  "test",
             years.change = matrix(c(1990.5, 2000.5,
                                     2000.5, 2010.5,
                                     1990.5, 2010.5,
                                     2000.5, 2010.5,
                                     2000.5, 2017.5,
                                     2010.5, 2017.5),
                                   ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
             years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                                      2000.5, 2005.5, 2010.5,
                                      1995.5, 2000.5, 2005.5,
                                      1990.5, 1995.5, 2000.5,
                                      1990.5, 2000.5, 2010.5,
                                      2000.5, 2010.5, 2017.5),
                                    ncol = 3, byrow = TRUE) ##<< Matrix with 3 columns, with column 1
             ) {

       ## If actually need to make the age ratios
        if(file.exists(file.path(age.ratio.output.dir, "res.country.all.women.age.ratio.rda"))) {
            warning("'res.country.all.women.age.ratio.rda' already exists. Age subset country CIs not re-created.")
            return(invisible())
        }

        ## -------* Sub functions

        ## -------** Summarize outputs (calculate 95% PIs, PPPCs)

        CP.summ.f <- function(z) {
            quantile(z, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
        }

        CP.change.summ.f <- function(z) {
            c(quantile(z, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
             ,PPPC = mean(z > 0))
        }

        ## -------** Re-structure objects for output

        age.ratio.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("0.025", "0.1", "0.5", "0.9", "0.975")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        age.ratio.change.outputs.f <- function(z, transp = FALSE) {
            out <- list()
            dimnames(z)[[1]] <- c("0.025", "0.1", "0.5", "0.9", "0.975", "PPPC")
            for(i in 1:dim(z)[3]) {
                if(transp) out <- c(out, list(t(z[,,i])))
                else out <- c(out, list(z[,,i]))
            }
            names(out) <- dimnames(z)[[3]]
            return(out)
        }

        ## -------* Inputs

        ## -------** Constants

        ## Don't change these without checking which subfunctions
        ## depend on them (e.g., 'InternalMakeRatios()') AND what they
        ## are called in 'GetAggregatesAllWomen().
        counts.names <-
            c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet"
             ,"TradPlusUnmet")

        ## -------** MCMC Meta

        load(file.path(age.subset.output.dir, "mcmc.meta.rda"))
        age.subset.mcmc.meta <- mcmc.meta

        load(file.path(age.total.output.dir, "mcmc.meta.rda"))
        age.total.mcmc.meta <- mcmc.meta

        age.subset.winbugs.data <- age.subset.mcmc.meta$winbugs.data
        age.total.winbugs.data <- age.total.mcmc.meta$winbugs.data

        ## -------** One country runs?

        if(!identical(age.subset.mcmc.meta$general$do.country.specific.run
                     ,age.total.mcmc.meta$general$do.country.specific.run
                      )) {
            if(age.subset.mcmc.meta$general$do.country.specific.run) {
                stop("Age subset women run is a one country run but total (15--49) women run is not.")
            } else if(age.total.mcmc.meta$general$do.country.specific.run) {
                stop("Total (15--49) women run is a one country run but age subset women run is not.")
            }
        }

        ## -------** Peek at country trajectories

        ## Directories
        age.subset.countrytraj.dir <- file.path(age.subset.output.dir, "countrytrajectories")
        age.total.countrytraj.dir <- file.path(age.total.output.dir, "countrytrajectories")

        ## Just load the first country from each to check dates and countries.
        load(file.path(age.subset.countrytraj.dir
                      ,dir(age.subset.countrytraj.dir,
                           pattern = "aw_ISO_[0-9]{1,3}_counts\\.rda")[1]
                       ))
        subset1.country <- awra.CP.counts.j
        load(file.path(age.total.countrytraj.dir
                      ,dir(age.total.countrytraj.dir,
                           pattern = "aw_ISO_[0-9]{1,3}_counts\\.rda")[1]
                       ))
        total1.country <- awra.CP.counts.j

        ## -------** Get number of iterations

        ## Which is the smaller of mcmc arrays
        n.iters <- min(dim(subset1.country)[3], dim(total1.country)[3])

        ## -------** Estimation years

        ## -------*** Single years

        age.subset.years <- dimnames(subset1.country)[[1]]
        age.total.years <- dimnames(total1.country)[[1]]

        if(is.null(est.years)) est.years <- age.subset.years

        ## Check that estimation years all match
        if(!isTRUE(all.equal(age.subset.years, age.total.years, est.years))) {
            est.years <- intersect(intersect(age.subset.years, age.total.years), est.years)
            warning("The estimation years for age subset and age total (15--49) do not match, or one or both do not match 'est.years'. Using the intersection (age subset <and> age total <and> 'est.years'):\n\t", paste0(est.years, collapse = ", "))
        }

        ## Make est.years numeric
        est.years <- as.numeric(est.years)

        ## -------*** Changes years

        if(!(all(c(years.change, years.change2) %in% est.years)))
            stop("Estimates/projections are not available for all 'years.change' and 'years.change2'.")

        ## (from 'GetInfoChange()')
        changes.years.names <- c(apply(years.change, 1, function(years) paste0(floor(years[1]), "-", floor(years[2]))),
                                 apply(years.change2, 1, function(years)
                                     paste0("Change (", floor(years[3]), "-", floor(years[2]), ") - (",
                                            floor(years[2]), "-", floor(years[1]), ")")))

        ## -------** Country names and ISO codes

        ## -------*** country.info from mcmc.meta

        if(age.subset.mcmc.meta$general$include.c.no.data) {
            age.subset.c.info <-
                rbind(age.subset.mcmc.meta$data.raw$country.info
                     ,age.subset.mcmc.meta$data.raw$country.info.no.data
                      )
        } else age.subset.c.info <- age.subset.mcmc.meta$data.raw$country.info

        if(age.total.mcmc.meta$general$include.c.no.data) {
            age.total.c.info <-
                rbind(age.total.mcmc.meta$data.raw$country.info
                     ,age.total.mcmc.meta$data.raw$country.info.no.data
                      )
        } else age.total.c.info <- age.total.mcmc.meta$data.raw$country.info

        ## -------*** Get ISOs in all inputs

        ## ISOs in both age subset and age total countrytrajectories
        iso.both <- intersect(age.subset.c.info$iso.c, age.total.c.info$iso.c)

        if(identical(length(iso.both), 0L)) stop("Age subset and age total (15--49) runs have no ISOs in common.")

        ## ISOs saved to directories
        age.subset.ISO.saved <-
            dir(age.subset.countrytraj.dir, pattern = "^aw_ISO_[0-9]{1,3}_counts.rda")
        age.total.ISO.saved <-
            dir(age.total.countrytraj.dir, pattern = "^aw_ISO_[0-9]{1,3}_counts.rda")
        iso.both.saved <- intersect(age.subset.ISO.saved, age.total.ISO.saved)
        if(identical(iso.both.saved, 0L)) stop("Age subset and age total (15--49) runs have not saved country trajectories for any of the same countries.")
        iso.both.saved <- sapply(strsplit(iso.both.saved, split = "_"), "[[", 3)

        iso.both <- intersect(iso.both, iso.both.saved)
        if(identical(length(iso.both), 0L)) stop("Intersection of the country runs saved by age subset and age total (15--49) runs and the countries recorded in their respective 'country.info' lists is empty.")

        ## -------*** Country names

        age.subset.c.name.both <-
            sapply(iso.both, function(z) {
                age.subset.c.info$name.c[as.character(age.subset.c.info$iso.c) == as.character(z)]
            })
        age.total.c.name.both <-
            sapply(iso.both, function(z) {
                age.total.c.info$name.c[as.character(age.total.c.info$iso.c) == as.character(z)]
            })

        ## -------* Make output lists

        ## These will eventually just have the CIs
        age.ratio.CP.ratios.CIs.li <-
            lapply(iso.both,
                   function(z) array(dim = c(5, length(est.years), length(counts.names))))

        age.ratio.CP.changeratios.CIs.li <-
            lapply(iso.both,
                   function(z) array(dim = c(6, length(changes.years.names)
                                           , length(counts.names))))

        ## -------* Calculate

        for(j in seq_along(iso.both)) {

            ## -------** Age Ratios

            load(file.path(age.subset.countrytraj.dir, paste0("aw_ISO_", iso.both[j], "_counts.rda")))
            subset.country <- awra.CP.counts.j
            dns <- dimnames(subset.country)

            load(file.path(age.total.countrytraj.dir, paste0("aw_ISO_", iso.both[j], "_counts.rda")))
            total.country <- awra.CP.counts.j

            age.ratio.CP.ratios.j <- subset.country[,,1:n.iters] /
                total.country[dns[[1]], dns[[2]], 1:n.iters]

            ## -------** Change quantities (counts, props, ratios)

            ## Make array for change in age count ratios
            age.ratio.CP.change.countratios.j <-
                array(0, dim = c(length(changes.years.names), length(counts.names)
                                ,n.iters
                                 ))
            dimnames(age.ratio.CP.change.countratios.j) <- list(changes.years.names, counts.names)

            for(i in seq_len(nrow(years.change))) { # "2000-1990", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change[i,1])
                    b <- which(est.years == years.change[i,2])
                    age.ratio.CP.change.countratios.j[i, ,k] <-
                        age.ratio.CP.ratios.j[b, ,k] - age.ratio.CP.ratios.j[a, ,k]
                }
            }
            for(i in seq_len(nrow(years.change2))) { # "(2000-1990) - (2010-2000)", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change2[i,1])
                    b <- which(est.years == years.change2[i,2])
                    c <- which(est.years == years.change2[i,3])
                    age.ratio.CP.change.countratios.j[nrow(years.change) + i, ,k] <-
                        (age.ratio.CP.ratios.j[b, ,k] - age.ratio.CP.ratios.j[a, ,k]) -
                        (age.ratio.CP.ratios.j[c, ,k] - age.ratio.CP.ratios.j[b, ,k])
                }
            }

            ## -------* Summarize country ratios

            ## Estimation years
            age.ratio.CP.ratios.CIs.li[[j]] <-
                apply(age.ratio.CP.ratios.j, c(1, 2), "CP.summ.f")

            ## Change quantities
            age.ratio.CP.changeratios.CIs.li[[j]] <-
                apply(age.ratio.CP.change.countratios.j, c(1, 2), "CP.change.summ.f")

        } ## END: 'for(j in 1:length(iso.both))' (the country loop)

        ## -------** Prepare Outputs

        ## Make output lists look like 'CIprop.Lg.Lcat.qt', etc.
        age.ratio.CP.ratios.CIs.li <-
            lapply(age.ratio.CP.ratios.CIs.li, "age.ratio.outputs.f")

        age.ratio.CP.changeratios.CIs.li <-
            lapply(age.ratio.CP.changeratios.CIs.li, "age.ratio.change.outputs.f",
                   transp = TRUE)

        ## Country names
        names(age.ratio.CP.ratios.CIs.li) <- names(age.ratio.CP.changeratios.CIs.li) <-
            age.subset.c.name.both

        ## Output
        res.country.age.ratio <- list(CIcountratio.Lg.Lcat.qt = age.ratio.CP.ratios.CIs.li
                                     ,changecountratio.Lg.Lcat.Ti = age.ratio.CP.changeratios.CIs.li
                                     ,iso.g = iso.both
                                      )

        save(res.country.age.ratio,
             file = file.path(age.ratio.output.dir, "res.country.all.women.age.ratio.rda"))

    }


##--------------------------------------------------------------------------
## The End!
