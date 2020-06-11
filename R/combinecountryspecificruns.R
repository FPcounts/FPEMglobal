#----------------------------------------------------------------------
# combinecountryspecificruns.R
# Jin Rou New, 2013
# Function to combine country-specific runs
#----------------------------------------------------------------------
CombineCountrySpecificRuns <- function(# Combine results of country-specific runs.
  ### Combine \code{mcmc.array} and results of country-specific runs for
  ### for all/some countries and generate \code{mcmc.meta} for combined data of the country-specific runs.
  run.name.global, ##<< Character specifying global run name.
  iso.all = NULL, ##<< ISO country codes of country-specific runs to combine. If \code{NULL}, runs
  ## are combined for all 194 countries.
  get.combined.results = TRUE, ##<< Get combined results?
  output.dir.country.specific = NULL, ##<< Directory where country output folders are located. If \code{NULL},
  ## defaults to \code{output}.
  output.dir = NULL ##<< Directory to save combined results to. If \code{NULL}, defaults to
  ## \code{output/(run.name.global)_all}.
) {
  if (is.null(iso.all))
    load(file.path("data", "iso.all.rda"))
  if (is.null(output.dir.country.specific))
    output.dir.country.specific <- file.path(getwd(), "output")
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", paste0(run.name.global, "_all/"))
  dir.create(output.dir, showWarnings = F)
  dir.create(file.path(output.dir, "countrytrajectories"), showWarnings = F)

  ##details<< \code{mcmc.meta} is generated and saved if no existing \code{mcmc.meta} is found.
  if (!file.exists(file.path(output.dir, "mcmc.meta.rda"))) {
    cat("Starting RunMCMC to get mcmc.meta for all data.\n")
    # first load one country-specific run's mcmc.meta to get runname.global and validation information
    load(file = file.path(output.dir.country.specific,
                          paste0(run.name.global, "_", iso.all[1]),
                          "mcmc.meta.rda"))
    RunMCMC(run.name = paste0(run.name.global, "_all"),
            iso.select = iso.all,
            run.name.global = run.name.global,
	     run.jags = FALSE,
            do.country.specific.run = mcmc.meta$general$do.country.specific.run,
            change.priors.to.zerolower = mcmc.meta$general$change.priors.to.zerolower,
            # AD-HOC
            data.csv = "data/dataCPmodel.csv", regioninfo.csv = "data/Country-and-area-classification.csv",
            output.dir = output.dir)
    closeAllConnections()
    rm(mcmc.meta)
    cat("mcmc.meta for all data saved.\n")
  }
  load(file.path(output.dir, "mcmc.meta.rda"))
  mcmc.meta.all <- mcmc.meta; rm(mcmc.meta)
  cat("mcmc.meta for all data loaded.\n")

  iso.all <- gsub(" ", "", mcmc.meta.all$data.raw$country.info$code.c)
  name.all <- mcmc.meta.all$data.raw$country.info$name.c

  if (get.combined.results) { # combine output of country-specific runs
    C <- length(iso.all)
    ##details<< Warning printed if output R object is not found for any country.
    exclude <- NULL
    for (c in 1:C) {
      if (!file.exists(file.path(output.dir.country.specific,
                                 paste0(run.name.global, "_", iso.all[c]),
                                 "res.country.rda")))
        exclude <- c(exclude, c)
    }
    for (c in 1:C) {
      if (!file.exists(file.path(output.dir.country.specific,
                                 paste0(run.name.global, "_", iso.all[c]),
                                 "par.ciq.rda")))
        exclude <- c(exclude, c)
    }
    exclude <- unique(exclude)

    if (!is.null(exclude)) {
      cat(paste0("Warning: No output available for the countries: ",
                 paste(name.all[exclude], collapse = ", "), "\n"))
      iso.all <- iso.all[-exclude]
      name.all <- name.all[-exclude]
      C <- length(iso.all)
      cat("Proceeding to combine output for all other selected countries.\n")
    }

    for (c in 1:C) {
      file.copy(from = file.path(output.dir.country.specific,
                                 paste0(run.name.global, "_", iso.all[c]),
                                 "countrytrajectories", "P.tp3s_country1.rda"),
                to = file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", c, ".rda")))
      cat(paste0("Country trajectories copied for ", c, " ", name.all[c], ".\n"))
    }

    # declare variables
    files <- c("mcmc.meta", "mcmc.array", "res.country", "par.ciq")
    files.to.load <- paste0(files, ".rda")
    cat(paste0("Combining country-specific output for country 1 (", iso.all[1], ") of ", C, " countries.\n"))
    sapply(files.to.load, LoadFile,
           output.dir = file.path(output.dir.country.specific,
                                  paste0(run.name.global, "_", iso.all[1])),
           envir = environment())
    mcmc.array.all <- NULL
    res.country.all <- res.country
    par.ciq.all <- par.ciq
    # separate Zstuff from res.country.all
    Zstuff.all <- res.country$Zstuff

    # load and combine country-specific results
    for (c in 1:C) {
      cat(paste0("Combining country-specific output for country ", c, " (", iso.all[c], ") of ", C, " countries.\n"))
      sapply(files.to.load, LoadFile,
             output.dir = file.path(output.dir.country.specific,
                                    paste0(run.name.global, "_", iso.all[c])),
             envir = environment())
      if (c == 1) {
        names.res.country <- names(res.country)
      }
      if (c != 1) {
        res.country.all <- mapply(c, res.country.all, res.country)
        Zstuff.all$Zstar.cqp <- abind::abind(Zstuff.all$Zstar.cqp, res.country$Zstuff$Zstar.cqp, along = 1)
        par.ciq.all <- abind::abind(par.ciq.all, par.ciq, along = 1)
      }
      # combine mcmc.array
      mcmc.array.names <- dimnames(mcmc.array)[[3]]
      par.V <- mcmc.meta$par.V$parnames.V.in.bugs
      # drop dummy parameters
      exclude <- (grepl(".c\\[2]", mcmc.array.names) |
                    (grepl("V", mcmc.array.names) & !is.element(mcmc.array.names, par.V)))
      mcmc.array <- mcmc.array[, , !exclude]
      mcmc.array.names <- dimnames(mcmc.array)[[3]]
      # change names of country-specific parameters
      mcmc.array.names <- gsub("\\.c\\[1", paste0(".c[", c), mcmc.array.names)
      mcmc.array.names <- gsub("\\.ci\\[1", paste0(".ci[", c), mcmc.array.names)
      # change names of perturbation parameters
      if (!is.null(par.V)) {
        order <- match(mcmc.array.names[mcmc.array.names %in% par.V], par.V)
        par.V.global <- mcmc.meta.all$par.V$parnames.V.in.bugs[match(mcmc.meta$par.V$parnames.V.nice,
                                                                     mcmc.meta.all$par.V$parnames.V.nice)]
        mcmc.array.names[mcmc.array.names %in% par.V] <- par.V.global[order]
      }
      # inpute back mcmc.array names
      dimnames(mcmc.array)[[3]] <- mcmc.array.names
      # combine mcmc.array
      mcmc.array.all <- abind::abind(mcmc.array.all, mcmc.array)
    } # end country loop
    # add back Zstuff
    res.country.all$Zstuff <- Zstuff.all
    # rename all combined files
    mcmc.array <- mcmc.array.all
    res.country <- res.country.all
    names(res.country) <- names.res.country
    par.ciq <- par.ciq.all
    # save all combined files
    save(mcmc.array, file = file.path(output.dir, "mcmc.array.rda"))
    save(res.country, file = file.path(output.dir, "res.country.rda"))
    save(par.ciq, file = file.path(output.dir, "par.ciq.rda"))
    cat(paste0("Results combined and saved to ", output.dir, "\n"))
  }
  ##value<< \code{NULL}.
  return(invisible())
}

LoadFile <- function( # Load file into specified environment.
  ### Load file into specified environment.
  filename,  ##<< File name of file to load.
  output.dir,  ##<< Directory of file to load.
  envir ##<< Environment to load file into.
) {
  if (!file.exists(file.path(output.dir, filename))) {
    cat(paste(file.path(output.dir, filename), "does not exist!\n"))
  } else {
    load(file.path(output.dir, filename), envir = envir)
  }
  ##value<< NULL
  return(invisible())
}
