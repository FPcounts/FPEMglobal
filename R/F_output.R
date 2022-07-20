#--------------------------------------------------------------
# Leontine Alkema, 2011/2012
# F_output.R
#--------------------------------------------------------------
ReadWRA <- function(# Read WRA for all countries
  ### Read WRA for all countries
  est.years, ##<< Years for which WRA are needed.
  winbugs.data, ##<< Object of class \code{\link{winbugs.data}}
  country.info,##<< Object of class \code{\link{country.info}}
  WRA.csv ##<< Denominator counts
  ##[MCW-2016-11-21-1] :: Added to allow return of a data frame that has all
  ##country names and their ISO codes. This is used in 'allWomenCIs()'.
  ,return.iso = FALSE,
  in_union,
  verbose = TRUE
){
  # read in number of women of reproductive ages in married/union
  nyears <-  length(est.years)
  W.Lc.t <- list()

  ## [MCW-2016-11-21-2] :: Create data frame for ISO codes and c names.
  if(return.iso) iso.c.name.df <-
      data.frame(iso.c = rep(NA, nrow(country.info))
                ,name.c  = rep(NA, nrow(country.info))
                 )

                                #matrix(NA, winbugs.data$C, nyears)
  message("\nReading denominator counts from ", WRA.csv)
  weight.data <- extractDenominators(denominator_csv = WRA.csv, in_union = in_union)
  names(weight.data) <- gsub("X", "", names(weight.data))
  locID.grepl <- grepl("LocID", names(weight.data))
  if(identical(as.double(sum(locID.grepl)), 1) &&
     !identical(names(weight.data)[locID.grepl], "LocID")
     ) {
      names(weight.data)[locID.grepl] <- "LocID"
      warning("Column ", which(locID.grepl), " in '", WRA.csv, "' has been used as the 'LocID' column. Its header has some other (possibly invisible) characters in it.")
  }

  names(weight.data)[names(weight.data) == "ISO.code"] <- "LocID"
  ## [MCW-2017-01-20-3] :: Allow for column names starting 'UMW_...' for unmarried women counts.
  if (any(grepl("^MW_[0-9]{4}_", colnames(weight.data)))) {
      age.range <-
          strsplit(grep("^MW_[0-9]{4}_", colnames(weight.data), value = TRUE)[1]
                   ,"_")[[1]][2]
      names.weights.t <- paste0("MW_", age.range, "_", est.years-0.5)
  } else if(any(grepl("^UMW_[0-9]{4}_", colnames(weight.data)))) {
      age.range <-
          strsplit(grep("^UMW_[0-9]{4}_", colnames(weight.data), value = TRUE)[1]
                   ,"_")[[1]][2]
      names.weights.t <- paste0("UMW_", age.range, "_", est.years-0.5)
  } else {
      names.weights.t <- paste0(est.years-0.5)
  }
  for (c in 1:nrow(country.info)){      #[MCW-2016-08-26-1] Change upper limit to whatever size of 'country.info' is.
      no.est.msg.yrs <- vector()
      W.Lc.t[[country.info$name.c[c]]] <- rep(0, nyears)
      for (t in 1:nyears){
          if (is.element(country.info$iso.c[c], weight.data$LocID) & is.element(names.weights.t[t],names(weight.data))){
              W.Lc.t[[country.info$name.c[c]]][t] <- 1/1000*weight.data[weight.data$LocID==country.info$iso.c[c], names.weights.t[t]]
              ## [MCW-2016-11-21-3] :: Enter country in ISO dataframe.
              if(return.iso) iso.c.name.df[c, c("iso.c", "name.c")] <-
                                 c(country.info$iso.c[c], country.info$name.c[c])
          } else {
              no.est.msg.yrs <- c(no.est.msg.yrs, est.years[t])
          }
      }
      if(length(no.est.msg.yrs) > 0) {
          if (verbose) {
          message(paste0("\nNo estimate for number of women for ", country.info$name.c[c], " for years\n",
                         paste(strwrap(paste(no.est.msg.yrs, collapse = ", "), initial = "  ", prefix = "  "), collapse = "\n")
                         ))
          } else {
              message(paste0("No estimate for number of women for ", country.info$name.c[c], " for some or all years between ",
                             no.est.msg.yrs[1], " and ", no.est.msg.yrs[length(no.est.msg.yrs)], "."
                         ))
          }
      }
  }
  ##details<< If WRA information on a country is missing for a year, a warning message will be printed, and the WRA will be set to 0.
  ##value<< List W.Lc.t, where elements are named by country name (but matching was done using ISO codes),
  ## and contain WRA's (*1,000 women) for est.years.
  ## [MCW-2016-11-21-4] :: Return ISO dataframe, omitting incomplete cases.
  if(return.iso) return(list(W.Lc.t, na.omit(iso.c.name.df)))
  return(W.Lc.t)
}
#---------------------------------------------------------------------
GetCIs <- function (# Construct country-specific CIs
### Construct country-specific CIs for proportions, counts and changes
### (including posterior probabilities of a positive change).
### This function is called from \code{\link{ConstructOutput}}.
                    mcmc.meta, ##<< Object of class \code{\link{mcmc.meta}}
                    mcmc.array, ##<< Object of class \code{\link{mcmc.array}}
                    include.AR = TRUE, ##<< Logical: include AR(1) trajectories?
                    thin = 1, ##<< Optional additional thinning of MCMC samples
                    output.dir,## Where to save the folder with country trajectories?
                    start.year = 1990.5, ##<< First year of estimation period (will be centered at half-year)
                    ## If given user-input, it will use min of 1990 and user input
                    end.year = 2015.5,##<< First year of estimation period (will be centered at half-year)
                    ## If given user-input, it will use max of 2015 and user input
                    years.change = matrix(c(1990.5, 2000.5,
                                            2000.5, 2010.5,
                                            1990.5, 2010.5,
                                            2000.5, 2017.5),
                                          ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
                    ## containing yyyy1 and column 2 containing yyyy2 for calculating change yyyy1-yyyy2
                    years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                                             2000.5, 2005.5, 2010.5,
                                             1995.5, 2000.5, 2005.5,
                                             1990.5, 1995.5, 2000.5,
                                             1990.5, 2000.5, 2010.5
                                            ,2000.5, 2010.5, 2017.5),
                                           ncol = 3, byrow = TRUE), ##<< Matrix with 3 columns, with column 1
                    ## containing yyyy1, column 2 containing yyyy2 and column 3 containing yyyy3 for
                    ## calculating change (yyyy2-yyyy3) - (yyyy1-yyyy2)
                    ## The years 1990, 2000 and 2010 are always included.
                    ## Years outside (\code{start.year}, \code{end.year}) are ignored.
                    ## Mid-point years closest to the given \code{years.change} are used for calculations.
                    nrepeatARsampling = 1, ##<< DO NOT CHANGE!!!
                    ## Optional additional sampling of AR(1) trajectories
                    ## for each posterior sample of model parameters.
                    WRA.csv = NULL, ##<< csv-file for WRA, see \code{\link{ConstructOutput}}
                    seed = 1234, ##<< Seed (used for sampling of AR(1) trajectories), default is 1234
                    percentiles = c(0.025, 0.1, 0.5, 0.9, 0.975),##<< Percentiles (which define q in CIs.cqt),
                    in_union,
                    verbose = TRUE
                    ){

    winbugs.data <- mcmc.meta$winbugs.data
    region.info <- mcmc.meta$data.raw$region.info
    country.info <- mcmc.meta$data.raw$country.info
    name.c <- as.character(country.info$name.c)
    do.country.specific.run <- mcmc.meta$general$do.country.specific.run # change JR, 20131104
    write.model.fun <- mcmc.meta$general$write.model.fun

    ## >>>>> RATE MODEL [MCW-2018-01-03] :: Copied from Niamh's version.
    if (is.null(mcmc.meta$general$do.country.specific.targets.run)) { # change JR, 20150301
        do.country.specific.targets.run <- FALSE # for old runs
    } else {
        do.country.specific.targets.run <- mcmc.meta$general$do.country.specific.targets.run
    }
    ## <<<<< RATE MODEL
    data.global <- mcmc.meta$data.global # change JR, 20131104

    ## [MCW-2016-08-26-4] From this point, combine countries with no data with
    ## other countries and apply rest of this function.. Note that 'region.info'
    ## already contains the union of (sub)regions across all countries, data or
    ## no.
    if(mcmc.meta$general$include.c.no.data || isTRUE(mcmc.meta$validation.list$at.random.no.data)) {
        country.info.no.data <- mcmc.meta$data.raw$country.info.no.data
        countries.all <- rbind(country.info, country.info.no.data)
        name.c <- countries.all$name.c
        C.all <- winbugs.data$C + winbugs.data$C.no.data
        if(isTRUE(mcmc.meta$validation.list$at.random.no.data)) {
            N.unique.c <- c(winbugs.data$N.unique.c, winbugs.data$N.unique.c.test)
        } else N.unique.c <- winbugs.data$N.unique.c
    } else {
        countries.all <- country.info
        C.all <- winbugs.data$C
        N.unique.c <- winbugs.data$N.unique.c
    }

    ## IMPORTANT: when using names for lists, make sure they're character
    ## or paste the word, else alph ordering/level is used (and e.g. China and China HK are swapped)

    ##details<< Country trajectories are stored in \code{output.dir/countrytrajectories}.
    ## output.dir.countrytrajectories <- file.path(mcmc.meta$general$output.dir, "countrytrajectories/")
    output.dir.countrytrajectories <- file.path(output.dir, "/countrytrajectories/")
    dir.create(output.dir.countrytrajectories, showWarnings = FALSE)

    mcmc.array.thin <- mcmc.array[seq(1, length(mcmc.array[,1,1]), thin),,,drop = FALSE]
                                # [MCW-2018-01-17 (1)] :: If only one
                                # iteration make sure extra dimension
                                # not dropped.
    if (length(mcmc.array[1,,1])==1) {# when there is only one chain the thinning changes array into matrix
        mcmc.array.thin <- array(NA, c(length(seq(1, length(mcmc.array[,1,1]), thin)),
                                       length(mcmc.array[1,,1]), length(mcmc.array[1,1,])))
        mcmc.array.thin[,1,] <- mcmc.array[seq(1, length(mcmc.array[,1,1]), thin),,]
        dimnames(mcmc.array.thin) <- list(NULL, NULL, names(mcmc.array[1,1,]))
    }
    n.s <- prod(dim(mcmc.array.thin)[1:2]) # nthinnedsim*nchains

    nrepeatARsampling <- max(nrepeatARsampling, 1)
    if (!include.AR) nrepeatARsampling = 1
    ## s refers to number of posterior samples
    ## S refers to total number of trajectories
    ## if nrepeatARsampling=2, then S=1,s+1 correspond to s=1 (parameter.s vectors are stacked).
    ##------------------------------------------------------------
    ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
    est.years.input<-seq(start.year, end.year)
    ## <<<<< RATE MODEL
    start.year <- min(floor(start.year)+0.5, 1990.5)
    end.year <- max(floor(end.year)+0.5, 2015.5)
    est.years <- seq(start.year, end.year)
    ##details<< The number of WRA are read using \code{\link{ReadWRA}}, which gives \code{W.Lg.t}.

    W.Lg.t <- ReadWRA(est.years = est.years,
                       winbugs.data = winbugs.data,
                       country.info = countries.all,
                       WRA.csv = WRA.csv,
                       return.iso = TRUE,
                      in_union,
                      verbose = verbose
                       )
    W.Lg.t.iso <- W.Lg.t[[2]]
    W.Lg.t <- W.Lg.t[[1]]

    nyears <- length(est.years)
    ## change JR, 20140317
    ## make sure 1990, 2000 and 2010 are included:
    years.change <- unique(rbind(floor(years.change)+0.5,
                                 matrix(c(1990.5, 2000.5,
                                          2000.5, 2010.5,
                                          1990.5, 2010.5),
                                        ncol= 2, byrow = TRUE)))
    years.change2 <- unique(rbind(floor(years.change2)+0.5,
                                  matrix(c(2005.5, 2010.5, 2015.5,
                                           2000.5, 2005.5, 2010.5,
                                           1995.5, 2000.5, 2005.5,
                                           1990.5, 1995.5, 2000.5,
                                           1990.5, 2000.5, 2010.5),
                                         ncol = 3, byrow = TRUE)))
    check1 <- !(c(floor(years.change)) %in% floor(est.years))
    check2 <- !(c(floor(years.change2)) %in% floor(est.years))
    if (any(check1))
        stop(paste0(c(years.change)[check1], " is not found in estimation years.\n"))
    if (any(check2))
        stop(paste0(c(years.change2)[check2], " is not found in estimation years.\n"))
    years.change.unique <- unique(c(c(years.change), c(years.change2)))
    ## needed for AR(1)'s:
    ## [MCW-2016-08-26-15] Add in elements to 'years.ci' for countries with no
    ## data.
    ##
    ## For the LEVEL MODEL, enter the mid-point of all years in
    ## data. It is approximately 1997. This must be a half year
    ## because it is used as the basis for an index into a matrix in
    ## InternalInternalGetARTrajectories().
    ##
    ## For the RATE MODEL, set it to the same year 'setlevel.c' refers
    ## to (i.e., 1990.5).
    ##
    ## This year (roughly 1997 or 1990.5) is the year the posterior estimates of
    ## the AR(1) distortion terms are attributed to. In the JAGS model
    ## a sample for each of epsilon, eta, and theta (e.g.,
    ## 'mcmc.array[,,"eps.ci[1,1]"]'), was saved for the 'first'
    ## 'observation' year for countries with no data. This is not
    ## really a posterior because it is entirely defined by the
    ## distributions in Alkema et al. (2013, Appendix p. 10). AR
    ## distortion terms for all other years in the estimation period
    ## are generated by sequentially sampling from normal
    ## distributions forwards and backwards from this year. Going
    ## backwards, the first AR term is sampled from a normal
    ## distribution with mean equal to the posterior for 'rho.s' times
    ## the datum value, s.d. the posterior 'sigma.s', etc.,
    ## recursively. Similarly going forwards.
    ##

    if(mcmc.meta$general$include.c.no.data) {
        ## Countries with no data but NOT validation
        years.ci.no.data <- matrix(NA, nrow = winbugs.data$C.no.data, ncol = ncol(winbugs.data$gett.ci))
        years.ci.no.data[,1] <- floor(mean(range(winbugs.data$gett.ci, na.rm = TRUE))) + 0.5
        years.ci <- rbind(winbugs.data$gett.ci, years.ci.no.data)
        if(ModelFunctionRateModel(write.model.fun)) {
            ## >>>>> RATE MODEL
            year.est.c.no.data <- matrix(NA, nrow = winbugs.data$C.no.data,
                                         ncol = ncol(winbugs.data$year.est.c))
            year.est.c.no.data[,1] <- 1990.5
            year.est.c <- rbind(winbugs.data$year.est.c, year.est.c.no.data)
            N.obsperiod.c <- c(winbugs.data$N.obsperiod.c, rep(1, winbugs.data$C.no.data))
            ## <<<<< RATE MODEL
        }
    } else if(!is.null(mcmc.meta$validation.list)) {
        ## Validation
        if(with(mcmc.meta$validation.list, at.random.no.data || leave.iso.out)) {
            ## 'at.random.no.data' or 'leave.iso.out'
            years.ci <-
                matrix(NA, nrow = nrow(winbugs.data$gett.ci) + nrow(winbugs.data$gett.ci.test)
                      ,ncol = max(ncol(winbugs.data$gett.ci)
                                 ,ncol(winbugs.data$gett.ci.test)
                                  )
                       )
            years.ci[1:nrow(winbugs.data$gett.ci), 1:ncol(winbugs.data$gett.ci)] <-
                winbugs.data$gett.ci
            years.ci[(nrow(winbugs.data$gett.ci) + 1):nrow(years.ci), 1:ncol(winbugs.data$gett.ci.test)] <-
                winbugs.data$gett.ci.test
            if(ModelFunctionRateModel(write.model.fun)) {
                ## >>>>> RATE MODEL
                year.est.c <- rbind(winbugs.data$year.est.c, winbugs.data$year.est.c.test)
                N.obsperiod.c <- c(winbugs.data$N.obsperiod.c, winbugs.data$N.obsperiod.c.test)
                ## <<<<< RATE MODEL
            }
        } else {
            ## Other validation
            years.ci <- winbugs.data$gett.ci
            if(ModelFunctionRateModel(write.model.fun)) {
                ## >>>>> RATE MODEL
                year.est.c <- winbugs.data$year.est.c
                N.obsperiod.c <- winbugs.data$N.obsperiod.c
                ## <<<<< RATE MODEL
            }
        }
    } else {
        ## Neither countries with no data nor validations
        years.ci <- winbugs.data$gett.ci
        if(ModelFunctionRateModel(write.model.fun)) {
            ## >>>>> RATE MODEL
            year.est.c <- winbugs.data$year.est.c
            N.obsperiod.c <- winbugs.data$N.obsperiod.c
            ## <<<<< RATE MODEL
        }
    }

    ##------------------------------------------------------------
    ## output: World level unmet parametric function for p.seq
    if (!do.country.specific.run) { # change JR, 20131104
        a.s <- c(mcmc.array.thin[, , "a.unmet"])
        b.s <- c(mcmc.array.thin[, , "b.unmet"])
        c.s <- c(mcmc.array.thin[, , "c.unmet"])
    } else {
        a.s <- rep(winbugs.data$a.unmet0, n.s)
        b.s <- rep(winbugs.data$b.unmet0, n.s)
        c.s <- rep(winbugs.data$c.unmet0, n.s)
    }
    p.seq <- seq(0, 1, 0.01)
    Zstar.cqp <- array(NA, c(C.all, length(percentiles), length(p.seq)))
    logitZstar.sp <- matrix(NA, n.s, length(p.seq))
    for (i in 1:length(p.seq)) {
        logitZstar.sp[,i] <- (a.s + b.s*(p.seq[i]- winbugs.data$pmid.for.unmet) +
                              c.s*(p.seq[i]- winbugs.data$pmid.for.unmet)^2)
    }
    Zstar.qp <- apply(invlogit(logitZstar.sp), 2, quantile, percentiles)
    ##  make a.s etc same dimension with added AR repetitions
    a.S <- rep(a.s, nrepeatARsampling)
    b.S <- rep(b.s, nrepeatARsampling)
    c.S <- rep(c.s, nrepeatARsampling)
    ##rep(seq(1,2),  2)
    ##------------------------------------------------------------
    CIprop.Lg.Lcat.qt <- CIratio.Lg.Lcat.qt <- CIstar.Lg.Lcat.qt  <-
        ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
        CIrate.Lg.Lcat.qt<-
            ## <<<<< RATE MODEL
            changeprop.Lg.Lcat.Ti <-
                CIcount.Lg.Lcat.qt <- changecount.Lg.Lcat.Ti <- changeratio.Lg.Lcat.Ti <- meanProp.Lg.Lcat <-meanRatio.Lg.Lcat <-meanStar.Lg.Lcat <- metDemGT.Lg.Lcat.pr <- list()

    Zstar.St <-  Z.St <- logitBstar.St <- B.St <- Bstar.St <-
        D.St <- Dstar.St <- p.unmet.St <- pstar.unmet.St <-
            matrix(NA, n.s*nrepeatARsampling,  nyears)
    ##------------------------------------------------------------

    ## [MCW-2017-01-23-1] :: Create a data frame to record which country (ISO)
    ## goes with which 'P.tp3s_country' file.
    iso.P.tp3s.df <-
        data.frame(iso.c = rep(NA, C.all), name.c = NA ,filename = NA)

    for (c in 1:C.all){
        if(verbose) message(paste0("(", format(c, width = nchar(C.all)), "/", C.all, ") Constructing output for ", countries.all$name.c[c], " (ISO ", countries.all$iso.c[c], ")"))
        ## [MCW-2017-01-23-2] :: Add country ISO code and name to data frame.
        iso.P.tp3s.df[c, c("iso.c", "name.c")] <- c(countries.all$iso.c[c], name.c[c])

        seed.country <- seed*as.numeric(countries.all$iso.c[c]) # change JR, 20140317
        CIprop.Lg.Lcat.qt[[name.c[c]]] <- CIratio.Lg.Lcat.qt[[name.c[c]]] <-
            CIstar.Lg.Lcat.qt[[name.c[c]]] <-
            ## >>>>> RATE MODEL [MCW-2018-01-03-12][MCW-2018-01-03-] Copied from Niamh's version
            CIrate.Lg.Lcat.qt[[name.c[c]]] <-
            ## <<<<< RATE MODEL
            list()
        pstar.st <- InternalInternalGetTrajectoriesCountryTotStar(c = c,
                                                                  start.year = start.year,
                                                                  end.year = end.year,
                                                                  mcmc.array = mcmc.array.thin ,
                                                                  write.model.fun = write.model.fun #there is a rate model version
                                                                  )
        Rstar.st <- InternalInternalGetTrajectoriesCountryRatStar(c = c, start.year = start.year, end.year = end.year,
                                                                  mcmc.array = mcmc.array.thin)
        if (!include.AR){
            p.St <- pstar.st # here no resampling!
            R.St <- Rstar.st
        } else {
            if(ModelFunctionRateModel(write.model.fun)) {

                ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
                years.i <- years.ci[c,1:N.unique.c[c]] # change JR, 20150301
                year.totCP.i<-year.est.c[c,1:N.obsperiod.c[c]]
                GetTotTraj <- InternalGetTrajectoriesCountryTot(c = c, years.i = year.totCP.i,
                                                                start.year = start.year, end.year = end.year,
                                                                mcmc.array = mcmc.array.thin,
                                                                do.country.specific.run = do.country.specific.run,
                                                                winbugs.data = winbugs.data,
                                                                pstar.st = pstar.st, #rate model pstar.st
                                                                nrepeatARsampling = nrepeatARsampling,
                                                                seed.country = seed.country*1# change JR, 20140317
                                                                ,write.model.fun = write.model.fun)
                p.str<-GetTotTraj$p.str.final
                rate.str<-GetTotTraj$rate.str.final
                rate.St<-apply(rate.str,2, cbind)
                p.St <- apply(p.str,2, cbind)
                R.str <- InternalGetTrajectoriesCountryRat(c = c, years.i = years.i,
                                                 start.year = start.year, end.year = end.year,
                                                 mcmc.array = mcmc.array.thin,
                                                 do.country.specific.run = do.country.specific.run, # change JR, 20131104
                                                 winbugs.data = winbugs.data,
                                                 Rstar.st = Rstar.st,
                                                 nrepeatARsampling = nrepeatARsampling,
                                                 seed.country = seed.country*2) # change JR, 20140317
                R.St <- apply(R.str, 2, cbind)
                ## <<<<< RATE MODEL
            } else {
                ## ====> LEVEL MODEL
                ## note: dimension s changes if nrepreatARsampling is >1!!!
                p.str <- InternalGetTrajectoriesCountryTot(c = c, years.i = years.ci[c,1:N.unique.c[c]],
                                                           start.year = start.year, end.year = end.year,
                                                           mcmc.array = mcmc.array.thin,
                                                           do.country.specific.run = do.country.specific.run, # change JR, 20131104
                                                           winbugs.data = winbugs.data, # change JR, 20131104
                                                           pstar.st = pstar.st,
                                                           nrepeatARsampling = nrepeatARsampling,
                                                           seed.country = seed.country*1# change JR, 20140317
                                                                ,write.model.fun = write.model.fun)
                p.St <- apply(p.str,2, cbind)
                R.str <- InternalGetTrajectoriesCountryRat(c = c, years.i = years.ci[c,1:N.unique.c[c]],
                                                       start.year = start.year, end.year = end.year,
                                                       mcmc.array = mcmc.array.thin,
                                                       do.country.specific.run = do.country.specific.run, # change JR, 20131104
                                                       winbugs.data = winbugs.data, # change JR, 20131104
                                                       Rstar.st = Rstar.st,
                                                       nrepeatARsampling = nrepeatARsampling,
                                                       seed.country = seed.country*2) # change JR, 20140317
                R.St <- apply(R.str,2, cbind)
                ## <==== LEVEL MODEL
            }
            ##A <- array(seq(1,20), dim = c(2,2,3))
            ##A[1:2, 1:2,1]
            ##A[1:2, 1:2,2]
            ##apply(A,2, cbind)
                                # what if repeated sampling =1
            ##A <- array(seq(1,20), dim = c(2,2,1))
            ##A[1:2, 1:2,1]
            ##apply(A,2, cbind)
        }
        CIprop.Lg.Lcat.qt[[name.c[c]]][["Total"]] <- apply(p.St, 2, quantile, percentiles)
        CIprop.Lg.Lcat.qt[[name.c[c]]][["Traditional"]] <-  apply((1-R.St)*p.St, 2, quantile, percentiles)
        CIprop.Lg.Lcat.qt[[name.c[c]]][["Modern"]] <- apply(R.St*p.St, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Total"]] <- apply(pstar.st, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Traditional"]] <-  apply((1-Rstar.st)*pstar.st, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Modern"]] <- apply(Rstar.st*pstar.st, 2, quantile, percentiles)
        CIratio.Lg.Lcat.qt[[name.c[c]]][["Modern/Total"]] <- apply(R.St, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Modern/Total"]] <- apply(Rstar.st, 2, quantile, percentiles)
        ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
        if(ModelFunctionRateModel(write.model.fun)) {
            if(include.AR) {    #Only gets made if include.AR == TRUE
                ## [MCW 2018-11-09] This does not get used (checked in Niamh's code)
                CIrate.Lg.Lcat.qt[[name.c[c]]][["TotalRate"]] <- apply(rate.St, 2, quantile, percentiles, na.rm = TRUE)
                }
        }
        ## <<<<< RATE MODEL

        ## [MCW-2016-07-12-2] :: Compute means for key CP indicators for UNPD outputs.
        meanProp.Lg.Lcat[[name.c[c]]][["Total"]] <- apply(p.St, 2, mean, na.rm = TRUE)
        meanProp.Lg.Lcat[[name.c[c]]][["Traditional"]] <-  apply((1-R.St)*p.St, 2, mean, na.rm = TRUE)
        meanProp.Lg.Lcat[[name.c[c]]][["Modern"]] <- apply(R.St*p.St, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Total"]] <- apply(pstar.st, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Traditional"]] <-  apply((1-Rstar.st)*pstar.st, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Modern"]] <- apply(Rstar.st*pstar.st, 2, mean, na.rm = TRUE)
        meanRatio.Lg.Lcat[[name.c[c]]][["Modern/Total"]] <- apply(R.St, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Modern/Total"]] <- apply(Rstar.st, 2, mean, na.rm = TRUE)
        ##------------------------------------------------------------------------
        ## Unmet
        if (!include.AR){
            theta.St <- matrix(0, n.s, nyears)
        } else {
            theta.is <- matrix(NA, N.unique.c[c], n.s)
            for (i in 1:N.unique.c[c]){
                theta.is[i,] <- c(mcmc.array.thin[, , paste0("theta.ci[", c, ",", i, "]")])
            }
            if (!do.country.specific.run) { # change JR, 20131104
                rho.unmet <- c(mcmc.array.thin[, ,"rho.unmet"])
                sigma.ar.unmet <- c(mcmc.array.thin[, ,"sigma.ar.unmet"])
            } else {
                rho.unmet <- rep(winbugs.data$rho.unmet0, n.s)
                sigma.ar.unmet <- rep(winbugs.data$sigma.ar.unmet0, n.s)
            }
            if(ModelFunctionRateModel(write.model.fun)) {

                ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
                theta.str <- InternalGetARTrajectories(rho.s = rho.unmet,
                                                       sigma.s = sigma.ar.unmet,
                                                       eps.is = theta.is,
                                                       years.i = years.i,
                                                       start.year = start.year, end.year = end.year,
                                                       nrepeatARsampling = nrepeatARsampling,
                                                       seed.country = seed.country*3) # change JR, 20140317
                theta.St <- apply(theta.str,2, cbind)
                ## <<<<< RATE MODEL

            } else {

                ## ====> LEVEL MODEL [MCW-2018-01-03] Copied from Niamh's version
                theta.str <- InternalGetARTrajectories(rho.s = rho.unmet,
                                                       sigma.s = sigma.ar.unmet,
                                                       eps.is = theta.is,
                                                       years.i = years.ci[c,1:N.unique.c[c]],
                                                       start.year = start.year, end.year = end.year,
                                                       nrepeatARsampling = nrepeatARsampling,
                                                       seed.country = seed.country*3) # change JR, 20140317
                theta.St <- apply(theta.str,2, cbind)
                ## ====< LEVEL MODEL
            }
        }
        unmet.intercept.s <- c(mcmc.array.thin[,,paste0("unmet.intercept.c[",c, "]")])
        unmet.intercept.S <- rep(unmet.intercept.s, nrepeatARsampling)
        for (t in 1:nyears) {
            Zstar.St[,t] <- invlogit(unmet.intercept.S
                                     + a.S + b.S*(p.St[,t] - winbugs.data$pmid.for.unmet) +
                                     c.S*(p.St[,t]- winbugs.data$pmid.for.unmet)^2)
            pstar.unmet.St[,t] <- Zstar.St[,t]*(1-p.St[,t])
            Bstar.St[,t] <- p.St[,t]/(pstar.unmet.St[,t] + p.St[,t])
            Z.St[,t] <- invlogit(logit(Zstar.St[,t]) + theta.St[,t])
            p.unmet.St[,t] <- Z.St[,t]*(1-p.St[,t])
            B.St[,t] <- p.St[,t]/(p.unmet.St[,t] + p.St[,t])
            ## change JR, 20140830: added demand met with modern methods
            Dstar.St[,t] <- R.St[,t]*p.St[,t]/(pstar.unmet.St[,t] + p.St[,t])
            D.St[,t] <- R.St[,t]*p.St[,t]/(p.unmet.St[,t] + p.St[,t])
        }
        for (i in 1:length(p.seq)){
            ## no ar sampling here
            Zstar.cqp[c,,i] <- quantile(invlogit(logitZstar.sp[,i]+unmet.intercept.s), percentiles)
        }

        CIprop.Lg.Lcat.qt[[name.c[c]]][["Unmet"]] <-  apply(p.unmet.St, 2, quantile, percentiles)
        CIprop.Lg.Lcat.qt[[name.c[c]]][["TotalPlusUnmet"]] <- apply(p.unmet.St + p.St, 2, quantile, percentiles)
        CIprop.Lg.Lcat.qt[[name.c[c]]][["TradPlusUnmet"]] <- apply(p.unmet.St + (1-R.St)*p.St, 2, quantile, percentiles)

        CIratio.Lg.Lcat.qt[[name.c[c]]][["Met Demand"]] <- apply(B.St, 2, quantile, percentiles)
        CIratio.Lg.Lcat.qt[[name.c[c]]][["Z"]] <- apply(Z.St, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Met Demand"]] <- apply(Bstar.St, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Z"]] <- apply(Zstar.St, 2, quantile, percentiles)

        ## change JR, 20140830: added demand met with modern methods
        CIratio.Lg.Lcat.qt[[name.c[c]]][["Met Demand with Modern Methods"]] <- apply(D.St, 2, quantile, percentiles)
        CIstar.Lg.Lcat.qt[[name.c[c]]][["Met Demand with Modern Methods"]] <- apply(Dstar.St, 2, quantile, percentiles)

        ## [MCW-2016-07-12-4] :: Compute means for CP indicators for unmet.
        meanProp.Lg.Lcat[[name.c[c]]][["Unmet"]] <-  apply(p.unmet.St, 2, mean, na.rm = TRUE)
        meanProp.Lg.Lcat[[name.c[c]]][["TotalPlusUnmet"]] <- apply(p.unmet.St + p.St, 2, mean, na.rm = TRUE)
        meanProp.Lg.Lcat[[name.c[c]]][["TradPlusUnmet"]] <- apply(p.unmet.St + (1-R.St)*p.St, 2, mean, na.rm = TRUE)

        meanRatio.Lg.Lcat[[name.c[c]]][["Met Demand"]] <- apply(B.St, 2, mean, na.rm = TRUE)
        meanRatio.Lg.Lcat[[name.c[c]]][["Z"]] <- apply(Z.St, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Met Demand"]] <- apply(Bstar.St, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Z"]] <- apply(Zstar.St, 2, mean, na.rm = TRUE)

        meanRatio.Lg.Lcat[[name.c[c]]][["Met Demand with Modern Methods"]] <- apply(D.St, 2, mean, na.rm = TRUE)
        meanStar.Lg.Lcat[[name.c[c]]][["Met Demand with Modern Methods"]] <- apply(Dstar.St, 2, mean, na.rm = TRUE)

        ## Met Demand with modern methods > 75%
        metDemGT.Lg.Lcat.pr[[name.c[c]]][["Met Demand with Modern Methods >= 75%"]] <-
            matrix(colMeans(D.St >= 0.75, na.rm = TRUE), nrow = 1)
        ##------------------------------------------------------------------------
        ## keep old name with small s
        P.tp3s <- array(NA, c(length(est.years), 3, n.s*nrepeatARsampling))
        ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
        t.input<-match(est.years.input,est.years)
        ## <<<<< RATE MODEL
        dimnames(P.tp3s) <- list(est.years, c("Traditional", "Modern", "Unmet"), NULL)
        for (t in 1:length(est.years)){
            P.tp3s[t,1,] <- (1-R.St[,t])*p.St[,t]
            P.tp3s[t,2,] <- R.St[,t]*p.St[,t]
            P.tp3s[t,3,] <- p.unmet.St[,t]
        }
        ##details<<
        ## Country trajectories in form of array \code{P.tp3s} written to \code{"output.dir/country.trajectories/"}
        ## where t refers to estimation year, p3 to (trad, modern, unmet) and s to posterior sample
        save(P.tp3s, file = file.path(output.dir.countrytrajectories, paste0("P.tp3s_country", c, ".rda"))) # change JR, 20140418

        ## [MCW-2017-01-23-3] :: Add 'P.tp3s_country' filename to data frame.
        iso.P.tp3s.df[c, "filename"] <- paste0("P.tp3s_country", c, ".rda")
        ##------------------------------------------------------------
        ## construct changes in prop and count and ratios

        P.yp3s <- P.tp3s[is.element(est.years, years.change.unique), ,]
        changeprop.Lg.Lcat.Ti[[name.c[c]]] <-
            GetInfoChange(P.yp3s = P.yp3s ,years.change = years.change
                        , years.change2 = years.change2) # change JR, 20140317
        ## get estimates of changes in counts
        ## easy and inefficient :) ...
        Psum.yp3s  <- array(NA, dim(P.yp3s))
        dimnames(Psum.yp3s) <- dimnames(P.yp3s)
        if(verbose) message(" ", rep(" ", nchar(C.all)), " ", rep(" ", nchar(C.all)), "  ", "(denominator counts for ", names(W.Lg.t)[c], ")")
        W.y <- W.Lg.t[[c]][is.element(est.years, years.change.unique)]
        names(W.y) <- est.years[is.element(est.years, years.change.unique)]
                                # NB: this not done in Niamh's version (2018-01-03)
        for (y in 1:dim(P.yp3s)[1]) {
            Psum.yp3s[y,,] <- P.yp3s[y,,]*W.y[y]
        }
        changecount.Lg.Lcat.Ti[[name.c[c]]] <-
            GetInfoChange(P.yp3s = Psum.yp3s, type.is.prop = FALSE,
                          years.change = years.change, years.change2 = years.change2) # change JR, 20140317
        ## [MCW-2017-08-14-3] :: Calculate change quantities for ratios, e.g., 'met demand'.
        changeratio.Lg.Lcat.Ti[[name.c[c]]] <-
            GetInfoChange(P.yp3s = Psum.yp3s #<-- the /counts/
                        , type.is.prop = FALSE, type.is.ratio = TRUE, W.y = W.y,
                          years.change = years.change, years.change2 = years.change2
                          ) # change JR, 20140317
    } # end country loop

    ## [MCW-2017-01-23-4] :: Save data frame.
    write.csv(x = iso.P.tp3s.df
             ,file = file.path(output.dir, "iso.Ptp3s.key.csv")
            , row.names = FALSE)

    ##print(dim(Psum.yp3s))
    ## get estimates of counts
    CIcount.Lg.Lcat.qt <- FromCIpropToCIcount(
        CIprop.Lg.Lcat.qt = CIprop.Lg.Lcat.qt, W.Lg.t = W.Lg.t)

    ## fix names for the qt matrices
    ## remove % for q dimension
    CIprop.Lg.Lcat.qt <- lapply(CIprop.Lg.Lcat.qt,  lapply, function(c.qt){
        dimnames(c.qt) <- list(percentiles, est.years)
        return(c.qt)})
    CIstar.Lg.Lcat.qt <- lapply(CIstar.Lg.Lcat.qt,  lapply, function(c.qt){
        dimnames(c.qt) <- list(percentiles, est.years)
        return(c.qt)})
    CIratio.Lg.Lcat.qt <- lapply(CIratio.Lg.Lcat.qt,  lapply, function(c.qt){
        dimnames(c.qt) <- list(percentiles, est.years)
        return(c.qt)})
    CIcount.Lg.Lcat.qt <- lapply(CIcount.Lg.Lcat.qt,  lapply, function(c.qt){
        dimnames(c.qt) <- list(percentiles, est.years)
        return(c.qt)})
    ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
    if(ModelFunctionRateModel(write.model.fun)) {
            if(include.AR) {    #Only gets made if include.AR == TRUE
        CIrate.Lg.Lcat.qt <- lapply(CIrate.Lg.Lcat.qt,  lapply, function(c.qt){
            dimnames(c.qt) <- list(percentiles, est.years[-1])
            return(c.qt)})
    }}
    ## <<<<< RATE MODEL

    ## Names for probabilities
    metDemGT.Lg.Lcat.pr <- lapply(metDemGT.Lg.Lcat.pr, lapply, function(z) {
        dimnames(z) <- list("Probability", est.years)
        return(z)})


    ##value<< List of class CIs \code{\link{CIs-class}} with
    out <- list(iso.g  = countries.all$iso.c, ##<< Included to be consistent with results for aggregates
                CIprop.Lg.Lcat.qt = CIprop.Lg.Lcat.qt, ##<< Proportions for Total, Traditional, Modern, Unmet, TotalPlusUnmet, TradPlusUnmet.
                ## Percentages are included as names in the \code{.qt}-matrix without percentage signs.
                CIratio.Lg.Lcat.qt = CIratio.Lg.Lcat.qt,##<<Ratios Met Demand, Met Demand with Modern Methods, Z (unmet/none) and modern/total (R). R and Z might not be included for aggregates.
                CIstar.Lg.Lcat.qt  = CIstar.Lg.Lcat.qt,##<< Systematic trends for Total etc.
                CIcount.Lg.Lcat.qt = CIcount.Lg.Lcat.qt,##<< Counts for same categories as under proportions.
                ## [MCW-2016-07-12-3] output means
                meanProp.Lg.Lcat = meanProp.Lg.Lcat,
                meanRatio.Lg.Lcat = meanRatio.Lg.Lcat,
                meanStar.Lg.Lcat = meanStar.Lg.Lcat,
                changeprop.Lg.Lcat.Ti = changeprop.Lg.Lcat.Ti,##<< Changes in proportions for \code{T = years.change}, \code{i} refers to percentiles and PPPC (the posterior probability of a positive change).
                changecount.Lg.Lcat.Ti = changecount.Lg.Lcat.Ti,##<< Changes in counts, same notation as for proportions.
                changeratio.Lg.Lcat.Ti = changeratio.Lg.Lcat.Ti,##<< Changes in ratios, same notation as for proportions.
                W.Lg.t = W.Lg.t,##<< Number of WRA (*1,000 women)
                Zstuff = list(
                    Zstar.qp = Zstar.qp, ##<< In list \code{Zstuff}: World trend in unmet/none for \code{p.seq}.
                    ## (Note that  \code{Zstuff}  is not added for aggregates).
                    Zstar.cqp = Zstar.cqp, ##<< In list \code{Zstuff}: Country trend in unmet/none for \code{p.seq}
                    p.seq = p.seq ##<< In list \code{Zstuff}: Sequence of total prevalence to calculate Z
                )
               ,metDemGT.Lg.Lcat.pr = metDemGT.Lg.Lcat.pr #prob of demand satisfied > xx%
                ##, output.dir.countrytrajectories = output.dir.countrytrajectories ##<< Directory with country trajectories.
                )
    ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version
    if(ModelFunctionRateModel(write.model.fun)) {
            if(include.AR) {    #Only gets made if include.AR == TRUE
        out <- c(out, list(CIrate.Lg.Lcat.qt = CIrate.Lg.Lcat.qt))
    }}
    ## <<<<< RATE MODEL
    return(out)
}
#----------------------------------------------------------------------------------
InternalGetTrajectoriesCountryTot <- function(# Get trajectories for total
### Get trajectories for total by adding AR(1) to main trend
### using posterior samples of \code{eps.ci, rho.tot} and \code{sigma.tot}.
                                              c, ##<< Country index
                                              years.i,##<< Observation years in country
                                              start.year, ##<< First year estimation period
                                              end.year, ##<< Last year estimation period
                                              mcmc.array, ##<< Object
                                              do.country.specific.run = FALSE, ##<< Logical: Do country-specific run?
                                              winbugs.data = NULL, ##<< Object
                                              pstar.st, ##<< Main trend in total for estimation period
                                              nrepeatARsampling,
                                              seed.country, ##<< Seed used for country
                                              write.model.fun
                                              ){

    eps.is <- matrix(NA, length(years.i), length(c(mcmc.array[,,1])))
    for (i in 1:length(years.i)){
        eps.is[i,] <- c(mcmc.array[, , paste0("eps.ci[", c, ",", i, "]")])
    }
    if (!do.country.specific.run) {
        rho.tot <- c(mcmc.array[, ,"rho.tot"])
        sigma.tot <- c(mcmc.array[, , "sigma.tot"])
    } else {
        n.s <- prod(dim(mcmc.array)[1:2])
        rho.tot <- rep(winbugs.data$rho.tot0, n.s)
        sigma.tot <- rep(winbugs.data$sigma.tot0, n.s)
    }

    if(!ModelFunctionRateModel(write.model.fun)) {

        ## ---------- LEVEL MODEL >>>>>>>>>>

        eps.str <- InternalGetARTrajectories(rho.s = rho.tot,
                                             sigma.s = sigma.tot,
                                             eps.is = eps.is, years.i = years.i,
                                             start.year = start.year, end.year = end.year,
                                             nrepeatARsampling = nrepeatARsampling,
                                             seed.country = seed.country) # change JR, 20140317
                                #print(dim(eps.str))
        pstar.str <- array(rep(pstar.st, nrepeatARsampling), c(dim(pstar.st), nrepeatARsampling)) # change JR, 20140317
        logitp.str <- logit(pstar.str) + eps.str
        p.str <- 1/(1+exp(-logitp.str))
        ##value<< matrix with trajectories, dimension (no of posterior samples, length estimation period)
        return(p.str)

        ## <<<<<<<<<< LEVEL MODEL ----------

    } else {

        ## ---------- RATE MODEL >>>>>>>>>>

        ##Change NC, 20160810
        ##Need to ensure that 1990 is in the estimation period because
        ##that is the year in which we fix the level
        start.year.temp <- min(1985.5,start.year)
        end.year.temp <- max(1995.5,end.year)
        years.s<-seq(start.year.temp,end.year.temp)

        ##Function Change NC, 20160807
        eps.str <- InternalGetTotCPARTrajectories(rho.s = rho.tot,
                                                  sigma.s = sigma.tot,
                                                  eps.is = eps.is, years.i = years.i,
                                                  start.year = start.year.temp, end.year = end.year.temp,
                                                  nrepeatARsampling = nrepeatARsampling,
                                                  seed.country = seed.country) # change JR, 20140317
        nyears<-length(years.s)
        year.start<-which(years.s==1990.5)
        omega.s <- c(mcmc.array[, ,paste0("omega.c[", c, "]")])
        setlevel.s <- c(mcmc.array[, , paste0("setlevel.c[", c, "]")])
        pmax.s <- c(mcmc.array[, ,paste0("pmax.c[", c, "]")])
        nsample<-length(pmax.s)

        p.str<-logitp.str<-ls.str<-rate.str<- array(NA, c(nsample, nyears))# Change NC, 20160807

        logitp.str[,year.start]<-setlevel.s
        p.str[,year.start]<-1/(1+exp(-logitp.str[,year.start]))

        for(j in 1:nsample){
            for(k in (year.start-1):1)
            {
                ls.str[j,k]<-logitp.str[j,k+1]-eps.str[j,k,nrepeatARsampling]

                if(invlogit(ls.str[j,k])<pmax.s[j])
                {
                    p.str[j,k]<-pmax.s[j]*(invlogit((logit(invlogit(ls.str[j,k])/pmax.s[j])-omega.s[j])))
                }
                else
                {
                    p.str[j,k]=invlogit(ls.str[j,k])
                }

                logitp.str[j,k]<-logit(p.str[j,k])
            }

            for(k in (year.start+1):nyears)
            {

                if(p.str[j,k-1]<pmax.s[j])
                {
                    logitp.str[j,k]<-logit(pmax.s[j]/(1+exp(-(logit(p.str[j,k-1]/pmax.s[j])+omega.s[j]))))+eps.str[j,k-1,nrepeatARsampling]
                }
                else
                {
                    logitp.str[j,k]<-logit(p.str[j,k-1])+eps.str[j,k-1,nrepeatARsampling]
                }

                p.str[j,k]<-1/(1+exp(-logitp.str[j,k]))
            }

            rate.str[j,1]<-0 #Dummy, removed later
            for(k in 2:nyears)
            {
                rate.str[j,k]<-(logitp.str[j,k]-logitp.str[j,k-1])-eps.str[j,k-1,nrepeatARsampling]
            }
        }

        select <- is.element(seq(start.year.temp, end.year.temp), seq(start.year, end.year))
        p.str.final <- p.str[,select]
        rate.str.final<-rate.str[,select][,-1]

        ##value<< matrix with trajectories, dimension (no of posterior samples, length estimation period)
        return(list(p.str.final=p.str.final,rate.str.final=rate.str.final))
    }
    ## <<<<<<<<<< RATE MODEL ----------
}
##----------------------------------------------------------------------------------------------
InternalGetTrajectoriesCountryRat <- function(# Get trajectories for modern/total
  ### Get trajectories for modern/total by adding AR(1) to main trend
  ### using posterior samples of \code{eta.ci, rho.rat} and \code{sigma.rat}.
  c, ##<< Country index
  years.i,##<< Observation years in country
  start.year, ##<< First year estimation period
  end.year, ##<< Last year estimation period
  mcmc.array, ##<< Object
  do.country.specific.run = FALSE, ##<< Logical: Do country-specific run?
  winbugs.data = NULL, ##<< Object
  Rstar.st, ##<< Main trend in modern/total for estimation period
  nrepeatARsampling,
  seed.country ##<< Seed used for country
){
  eta.is <- matrix(NA, length(years.i), length(c(mcmc.array[,,1])))
  for (i in 1:length(years.i)){
    eta.is[i,] <- c(mcmc.array[, , paste0("eta.ci[", c, ",", i, "]")])
  }
  if (!do.country.specific.run) { # change JR, 20131104
    rho.rat <- c(mcmc.array[, ,"rho.rat"])
    sigma.rat <- c(mcmc.array[, ,"sigma.rat"])
  } else {
    n.s <- prod(dim(mcmc.array)[1:2])
    rho.rat <- rep(winbugs.data$rho.rat0, n.s)
    sigma.rat <- rep(winbugs.data$sigma.rat0, n.s)
  }
  eta.str <- InternalGetARTrajectories(rho.s = rho.rat,
                                       sigma.s = sigma.rat,
                                       eps.is = eta.is,
                                       years.i = years.i, start.year = start.year, end.year = end.year,
                                       nrepeatARsampling = nrepeatARsampling,
                                       seed.country = seed.country) # change JR, 20140317
  Rstar.str <- array(rep(Rstar.st, nrepeatARsampling), c(dim(Rstar.st), nrepeatARsampling)) # change JR, 20140317
  logitR.str <- logit(Rstar.str) + eta.str
  R.str <- 1/(1+exp(-logitR.str))
  ##value<< matrix with trajectories, dimension (no of posterior samples, length estimation period)
  return(R.str)
}
#----------------------------------------------------------------------------------------------
InternalInternalGetTrajectoriesCountryRatStar <- function(# Get trajectories for main trend in modern/total
  ### Get trajectories for main trend in modern/total
  c, ##<< Country index
  years.i,##<< Observation years in country
  start.year, ##<< First year estimation period
  end.year, ##<< Last year estimation period
  mcmc.array ##<< Object
){
  # Note: mcmc.array has to be 3D
  Romega.s <- c(mcmc.array[, ,paste0("Romega.c[", c, "]")])
  RT.s <- c(mcmc.array[, ,paste0("RT.c[", c, "]")])
  Rmax.s <- c(mcmc.array[, ,paste0("Rmax.c[", c, "]")])
  Rstar.st <- Rmax.s/(1+exp(-Romega.s*(start.year - RT.s)))
  for (year in (start.year+1):end.year){
    Rstar.st <-  cbind(Rstar.st, Rmax.s/(1+exp(-Romega.s*(year - RT.s))))
  }
  ##value<< matrix with trajectories, dimension (no of posterior samples, length estimation period)
  return(Rstar.st)
}
#----------------------------------------------------------------------------------
InternalInternalGetTrajectoriesCountryTotStar <-
    function(# Get trajectories for main trend in total
### Get trajectories for main trend in total
             c, ##<< Country index
             years.i,##<< Observation years in country
             start.year, ##<< First year estimation period
             end.year, ##<< Last year estimation period
             mcmc.array ##<< Object
            ,write.model.fun
             ){
        if(ModelFunctionRateModel(write.model.fun)) {
            ## >>>>> RATE MODEL [MCW-2018-01-03] Copied from Niamh's version

            ## Need to ensure that 1990 is in the estimation period because that is the year in which we fix the level
            start.year.temp <- min(1985.5,start.year)
            end.year.temp <- max(1995.5,end.year)
            years.s<-seq(start.year.temp,end.year.temp)

            nyears<-length(years.s)
            year.start<-which(years.s==1990.5)
            omega.s <- c(mcmc.array[, ,paste0("omega.c[", c, "]")])
            setlevel.s <- c(mcmc.array[, , paste0("setlevel.c[", c, "]")])
            pmax.s <- c(mcmc.array[, ,paste0("pmax.c[", c, "]")])
            nsample<-length(pmax.s)

            pstar.st<-logitpstar.st<-lsstar.st<- array(NA, c(nsample, nyears)) # change JR, 20140317

            logitpstar.st[,year.start]<-setlevel.s
            pstar.st[,year.start]<-1/(1+exp(-logitpstar.st[,year.start]))

            for(j in 1:nsample){
                for(k in (year.start-1):1)
                {
                    lsstar.st[j,k]<-logitpstar.st[j,k+1]

                    if(invlogit(lsstar.st[j,k])<pmax.s[j])
                    {
                        pstar.st[j,k]<-pmax.s[j]*(invlogit((logit(invlogit(lsstar.st[j,k])/pmax.s[j])-omega.s[j])))
                    }
                    else
                    {
                        pstar.st[j,k]=invlogit(lsstar.st[j,k])
                    }

                    logitpstar.st[j,k]<-logit(pstar.st[j,k])
                }

                for(k in (year.start+1):nyears)
                {

                    if(pstar.st[j,k-1]<pmax.s[j])
                    {
                        logitpstar.st[j,k]<-logit(pmax.s[j]/(1+exp(-(logit(pstar.st[j,k-1]/pmax.s[j])+omega.s[j]))))
                    }
                    else
                    {
                        logitpstar.st[j,k]<-logit(pstar.st[j,k-1])
                    }

                    pstar.st[j,k]<-1/(1+exp(-logitpstar.st[j,k]))
                }
            }
            select <- is.element(seq(start.year.temp, end.year.temp), seq(start.year, end.year))
            pstar.st.final <- pstar.st[,select]

            ##value<< matrix with trajectories, dimension (no of posterior samples, length estimation period)
            return(pstar.st.final)
            ## <<<<< RATE MODEL
        } else {
            ## ====> LEVEL MODEL [MCW-2018-01-03] Copied from Niamh's version
            omega.s <- c(mcmc.array[, ,paste0("omega.c[", c, "]")])
            T.s <- c(mcmc.array[, , paste0("T.c[", c, "]")])
            pmax.s <- c(mcmc.array[, ,paste0("pmax.c[", c, "]")])
            pstar.st <- pmax.s/(1+exp(-omega.s*(start.year  - T.s)))
            for (year in (start.year+1):end.year){
                pstar.st <- cbind(pstar.st, pmax.s/(1+exp(-omega.s*(year - T.s))))
            }
            ##value<< matrix with trajectories, dimension (no of posterior samples, length estimation period)
            return(pstar.st)
        }
        ## ====< LEVEL MODEL
    }
#----------------------------------------------------------------------------------------------
InternalGetARTrajectories <- function( # Construct AR(1) trajectories
  ### Construct posterior sample of AR(1) trajectories, given samples at (unequally spaced) time points
  rho.s, ##<< Posterior sample of autoregressive parameter
  sigma.s, ##<< Posterior sample of autoregressive sd
  eps.is, ##<< Posterior sample of AR(1) for obs years i=1,..., I
  years.i, ##<< Obs years i=1,..., I
  start.year, ##<< First year where posterior sample is needed
  end.year,##<< Last year where posterior sample is needed
  nrepeatARsampling, ##<< How many AR-trajectories to sample?
  seed.country ##<< Seed used for country
  ){

  ###  Note: years.i and start/end year are centered at midpoint calendar year
  nsample <- length(rho.s)
  # Note: some obs years could be before start year or after end year
  # Don't throw them out because they inform the eps in start/end year
  # Inefficient but simple coding:
  # first construct eps from min(years.i,start year) to max(years.i, end year)
  # then select the years we need
  start.year.temp <- min(years.i,start.year)
  end.year.temp <- max(years.i,end.year)
  nyears <- end.year.temp - start.year.temp + 1
  #eps.st <- matrix(NA, nsample, nyears)
  obs.years.indices <- years.i - start.year.temp + 1
  n <- length(obs.years.indices)

  # create nrepeatARsampling trajectories for each posterior sample
  eps.str <- array(NA, c(nsample, nyears, nrepeatARsampling))

  # add posterior samples from MCMC
  for (i in 1:n){
    for (r in 1:nrepeatARsampling){
      eps.str[,obs.years.indices[i],r] <- eps.is[i,]
    }
  }

  # SAMPLING STARTS
  # create nrepeatARsampling trajectories for each posterior sample
  # speed all this up!?

  set.seed(seed.country*100) # change JR, 20140317
  if (n > 1){ # is there more than 1 obs?
    for (j in 1:(n-1)){
      if ((obs.years.indices[j+1] - obs.years.indices[j]) > 1){
        # are there are eps's missing between the two observed eps's?
        for (t in (obs.years.indices[j]+1):(obs.years.indices[j+1]-1)){
          A <- rho.s^(2*(obs.years.indices[j+1] - t + 1))
          varZ.s <- sigma.s^2/(1-rho.s^2)*(
            1 - 1/(1-A)*(rho.s^2 - 2*A + rho.s^(2*(obs.years.indices[j+1]-t)))
          )
          for (r in 1:nrepeatARsampling){
            Zhat.s <- 1/(1-A)*(
              eps.str[,t-1,r]*rho.s*(1 - rho.s^(2*(obs.years.indices[j+1]-t))) +
                eps.str[,obs.years.indices[j+1],r]*rho.s^(obs.years.indices[j+1]-t)*(1 - rho.s^2))
            eps.str[,t,r] <- rnorm(nsample, Zhat.s, sd = sqrt(varZ.s))
          }
        }
      }
    }
  }

  set.seed(seed.country*200) # change JR, 20140317
  # add samples at start
  if (obs.years.indices[1] > 1){
    for (k in seq(obs.years.indices[1]-1,1,-1)){
      for (r in 1:nrepeatARsampling){
        eps.str[,k,r] <- rnorm(nsample,rho.s*eps.str[,k+1,r], sigma.s)
      }
    }
  }

  set.seed(seed.country*300) # change JR, 20140317
  # add samples at end
  if (obs.years.indices[n] < nyears){
    for (k in (obs.years.indices[n]+1):nyears){
      for (r in 1:nrepeatARsampling){
        eps.str[,k,r] <- rnorm(nsample,rho.s*eps.str[,k-1,r], sigma.s)
      }
    }
  }

  # select the corresponding years
  # note that 3rd dimension drops out if it's 1
  #A <- array(seq(1,12), c(3,4,1))
  #A
  #array(A[,-1,], dim(A[,-1,]))
  #array(A[,-1,], c(dim(A[,-1,]),1))

  select <- is.element(seq(start.year.temp, end.year.temp), seq(start.year, end.year))
  if (nrepeatARsampling!=1){
    eps.str.final <- eps.str[,select,]
  } else {
    eps.str.final <- array(eps.str[,select,], c(dim(eps.str[,select,]),1))
  }
  #print(dim(eps.str.final))
  # note: the 3rd dimension is still lost when repeat=1
  return(eps.str.final)
  ##value<<
  ## matrix of size $s$ times $t$ for years (start.year:end.year) time $r$
  ## where r is the number of repeated AR trajectories for the same posterior sample
}

# eps <- InternalGetARTrajectories(
#   rho.s = c(0.5,0.5),
#   sigma.s= c(0.5,0.5),
#   eps.is = matrix(c(1,1), 1,2),
#   years.i = 2,
#   start.year = 0,
#   end.year = 3,
#   nrepeatARsampling=1)
# eps[1,1,1]

##------------------------------------------------------------------------------
####Function added NC, 20160810
InternalGetTotCPARTrajectories <- function( # Construct AR(1) trajectories
  ### Construct posterior sample of AR(1) trajectories, given samples at (unequally spaced) time points
  rho.s, ##<< Posterior sample of autoregressive parameter
  sigma.s, ##<< Posterior sample of autoregressive sd
  eps.is, ##<< Posterior sample of AR(1) for obs years i=1,..., I
  years.i, ##<< Obs years i=1,..., I
  start.year, ##<< First year where posterior sample is needed
  end.year,##<< Last year where posterior sample is needed
  nrepeatARsampling, ##<< How many AR-trajectories to sample?
  seed.country ##<< Seed used for country
  ){ ###  Note: years.i and start/end year are centered at midpoint calendar year
    nsample <- length(rho.s) # Note: some obs years could be before start year or after end year
                                # Don't throw them out because they inform the eps in start/end year
                                # Inefficient but simple coding:
                                # first construct eps from min(years.i,start year) to max(years.i, end year)
                                # then select the years we need
    start.year.temp <- min(years.i,start.year)
    end.year.temp <- max(years.i,end.year)
    nyears <- end.year.temp - start.year.temp + 1 #eps.st <- matrix(NA, nsample, nyears)
    obs.years.indices <- years.i - start.year.temp + 1
    n <- length(obs.years.indices) # create nrepeatARsampling trajectories for each posterior sample
    eps.str <- array(NA, c(nsample, nyears, nrepeatARsampling))
                                # add posterior samples from MCMC.
    ## (NB: 'nrepeatARsampling' is always '1' so ignore it)
    ## 'eps.str' is a matrix (essentially) with one row per trajectory
    ## and one column per year in the estimation period (e.g,. 1970 --
    ## 2030). The columns which match years at which there were
    ## observations are filled with the posterior estimates of epsilon
    ## that JAGS produced. The remaining columns are 'NA' at this
    ## point.
    for (i in 1:n){
        for (r in 1:nrepeatARsampling){
            eps.str[,obs.years.indices[i],r] <- eps.is[i,]
        }
    }

  # SAMPLING STARTS
  # create nrepeatARsampling trajectories for each posterior sample
  # speed all this up!?

    ## This part fills in the first block of 'NA' columns of
    ## 'eps.str'. These columns are just iid normal samples from N(mu,
    ## s), where mu is rho.s*[prev. column 'eps.str'] and s is
    ## 'sigma.s'.
  set.seed(seed.country*200) # change JR, 20140317
                                # add samples at start
  if (obs.years.indices[1] > 1){
      for (k in seq(obs.years.indices[1]-1,1,-1)){
          for (r in 1:nrepeatARsampling){
              eps.str[,k,r] <- rnorm(nsample,rho.s*eps.str[,k+1,r], sigma.s)
          }
      }
  }

    ## This part fills in the last block of 'NA' columns of
    ## 'eps.str', just like the above.
  set.seed(seed.country*300) # change JR, 20140317
                                # add samples at end
  if (obs.years.indices[n] < nyears){
      for (k in (obs.years.indices[n]+1):nyears){
          for (r in 1:nrepeatARsampling){
              eps.str[,k,r] <- rnorm(nsample,rho.s*eps.str[,k-1,r], sigma.s)
          }
      }
  }


  select <- is.element(seq(start.year.temp, end.year.temp), seq(start.year, end.year))
    if (nrepeatARsampling!=1){
        eps.str.final <- eps.str[,select,]
    } else {
        eps.str.final <- array(eps.str[,select,], c(dim(eps.str[,select,]),1))
    }
                                #print(dim(eps.str.final))
  # note: the 3rd dimension is still lost when repeat=1
  return(eps.str.final)
    ##value<<
  ## matrix of size $s$ times $t$ for years (start.year:end.year) time $r$
  ## where r is the number of repeated AR trajectories for the same posterior sample

}

#----------------------------------------------------------------------------------
GetParInfo <- function(# Get percentiles of parameters of logistics for total and ratio modern/total
  ### Get (2.5%, 50%, 97.5%) percentiles of parameters of logistics for total and ratio modern/total
  mcmc.array,##<< Object of class \code{\link{mcmc.array}}
  parnames.list, ##<< nn
  winbugs.data, ##<< Object of class \code{\link{winbugs.data}}
  country.info ##<< Object of class \code{\link{country.info}}
  ){
  percentiles = c(0.025,0.5,0.975)
  percentiles.names = c("2.5%", "50%", "97.5%")
  ## [MCW-2016-08-26-7] Take number of countries from 'country.info' as this now might include countries with no data.
  par.ciq <- array(NA, c(nrow(country.info), length(parnames.list$parnames.c), 3))
  for (c in 1:nrow(country.info)){
    #[MCW-2016-08-26-8] Again, take number of countries (upper limit of loop) from 'country.info' as this now might include countries with no data.
    i <- 0
      for (parname in paste0(parnames.list$parnames.c, "[", c, "]")){
          if(parname %in% dimnames(mcmc.array)[[3]]) { #Extra check [MCW-2018-01-03]
      i <- i+1
      par.ciq[c,i,] <- quantile(c(mcmc.array[,,parname]), percentiles)
    }}
  }
  dimnames(par.ciq) <- list(country.info$name.c, parnames.list$parnames.c, percentiles.names)
  par.ciq[,"RT.c",] <- par.ciq[,"RT.c",]
  ## >>>>> LEVEL MODEL [MCW-2018-01-03]
  if(isTRUE("T.c" %in% parnames.list$parnames.c)) {
      par.ciq[,"T.c",] <- par.ciq[,"T.c",]
      }
  ## <<<<< LEVEL MODEL
  ##value<< 3-dimensional array with entries (no of countries, 6 logistic parameters, percentiles)
  ## (with names for each dimension)
  return(par.ciq)
}
#----------------------------------------------------------------------
# The End!
