InternalGetAggregates <- function(#  Find aggregates for set of countries
  ### Find aggregates for set of countries
  W.Lc.t, ##<< Estimates of number of MWRA
  select.c, ##<< Selected countries to be included for the aggregate estimate
  dir.traj, ##<< Where are the country trajectories saved?
  years.change = matrix(c(1990.5, 2000.5,
                          2000.5, 2010.5,
                          1990.5, 2010.5,
                          2000.5, 2017.5),
                        ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
   years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                            2000.5, 2005.5, 2010.5,
                           1995.5, 2000.5, 2005.5,
                           1990.5, 1995.5, 2000.5,
                           1990.5, 2000.5, 2010.5
                           ,2000.5, 2010.5, 2017.5),
                         ncol = 3, byrow = TRUE) ##<< Matrix with 3 columns, with column 1
  ## containing yyyy1, column 2 containing yyyy2 and column 3 containing yyyy3 for
  ## calculating change (yyyy2-yyyy3) - (yyyy1-yyyy2)
  ## The years 1990, 2000 and 2010 are always included.
  ## Years outside (\code{start.year}, \code{end.year}) are ignored.
  ## Mid-point years closest to the given \code{years.change} are used for calculations.
  ##
  ##[MCW-2017-07-05-2] :: Passed in from from ~GetAggregates()~. Used to print out the names of countries being included in aggregates.
 ,iso.Ptp3s.key.df = NULL,
  verbose = TRUE){
  # percentiles hard-coded
  percentiles <- c(0.025, 0.1, 0.5, 0.9, 0.975)
  #as.numeric(names(res.country$CIprop.Lg.Lcat.qt[[1]][[1]][,1]))
  # find number of samples n.s:
  c <- select.c[1]
  load(file = file.path(dir.traj, paste0("P.tp3s_country", c, ".rda"))) # change JR, 20140830
  n.s <- dim(P.tp3s)[3]
  nyears <- length(W.Lc.t[[1]])
  #print(nyears)
  est.years <- as.numeric(dimnames(P.tp3s)[[1]] )#res.country$CIprop.Lg.Lcat.qt[[1]][[1]][1,]))
  #print(length(est.years))
  # change JR, 20140317
  # make sure 1990, 2000 and 2010 are included:
  years.change <- unique(rbind(floor(years.change)+0.5,
                               matrix(c(1990.5, 2000.5,
                                        2000.5, 2010.5,
                                        1990.5, 2010.5),
                                      ncol = 2, byrow = TRUE)))
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
    stop(paste0(c(years.change)[check1], " is not found in estimation years."))
  if (any(check2))
    stop(paste0(c(years.change2)[check2], " is not found in estimation years."))
  years.change.unique <- unique(c(c(years.change), c(years.change2)))

  cumsum.trad.ts  <-  cumsum.modern.ts <- cumsum.unmet.ts <- matrix(0, nyears, n.s)
  cumsumW.t <- rep(0, nyears)
    for (c in select.c){
        country_name <- iso.Ptp3s.key.df[c, "name.c"]
        country_ISO <- iso.Ptp3s.key.df[c, "iso.c"]
        country_rda_fname <- iso.Ptp3s.key.df[c, "filename"]
        if(verbose) message("\t", which(select.c %in% c), " ", country_name, " (ISO ", country_ISO, "), ", country_rda_fname)
        if(!country_name %in% names(W.Lc.t)) {
            stop("No denominator counts for country '", country_name, "'. Denominator counts are read from 'res.country.rda'.")
        }
        if (identical(as.double(sum(W.Lc.t[[country_name]])), 0)) {
            stop("Denominator counts for country '", country_name, "' are all zero. Denominator counts are read from 'res.country.rda'.")
        }

        load(file = file.path(dir.traj, country_rda_fname)) # change JR, 20140418
        for (t in 1:nyears){
            ## only increment non-missing cells
            non_miss <- !is.na(cumsum.trad.ts[t,]) & !is.na(P.tp3s[t,"Traditional",])
            if (!all(non_miss)) {
                message("For '", country_name, "', ",
                        sum(!non_miss), " out of ", n.s, " (", sum(!non_miss) / n.s * 100, "%) trajectories for CP 'Traditional' are 'NA'; these were ignored in creating aggregates.")
            }
            cumsum.trad.ts[t,non_miss] <- cumsum.trad.ts[t,non_miss] + W.Lc.t[[country_name]][t]*P.tp3s[t,"Traditional",non_miss]

            non_miss <- !is.na(cumsum.modern.ts[t,]) & !is.na(P.tp3s[t,"Modern",])
            if (!all(non_miss)) {
                message("For '", country_name, "', ",
                        sum(!non_miss), " out of ", n.s, " (", sum(!non_miss) / n.s * 100, "%) trajectories for CP 'Modern' are 'NA'; these were ignored in creating aggregates.")
            }
            cumsum.modern.ts[t,non_miss] <- cumsum.modern.ts[t,non_miss] + W.Lc.t[[country_name]][t]*P.tp3s[t,"Modern",non_miss]

            non_miss <- !is.na(cumsum.unmet.ts[t,]) & !is.na(P.tp3s[t,"Unmet",])
            if (!all(non_miss)) {
                message("For '", country_name, "', ",
                        sum(!non_miss), " out of ", n.s, " (", sum(!non_miss) / n.s * 100, "%) trajectories for 'Unmet' are 'NA'; these were ignored in creating aggregates.")
            }
            cumsum.unmet.ts[t,non_miss] <- cumsum.unmet.ts[t,non_miss] + W.Lc.t[[country_name]][t]*P.tp3s[t,"Unmet",non_miss]

            cumsumW.t[t] <- cumsumW.t[t] + W.Lc.t[[country_name]][t]
        }
    } # end country loop

  # divide by cumsumW.t (to have results same as for country results)
    CIprop.Lcat.qt <- CIratio.Lcat.qt <-
        meanCIprop.Lcat.qt <- meanCIratio.Lcat.qt <- metDemGT.Lg.Lcat.pr <- list()

  temp <- matrix(0, length(percentiles), nyears)
  colnames(temp) <- est.years
    rownames(temp) <- percentiles

    ## [MCW-2016-08-30-11] :: Create empty matrix to hold means.
    temp.means <- matrix(0, 1, nyears)
  colnames(temp.means) <- est.years
    rownames(temp.means) <- "mean"

  # note: these names are the same as in the country CIs! see F_output.R
  CIprop.Lcat.qt[["Traditional"]] <- CIprop.Lcat.qt[["TotalPlusUnmet"]]<- CIprop.Lcat.qt[["TradPlusUnmet"]] <-
    CIprop.Lcat.qt[["Modern"]] <- CIprop.Lcat.qt[["Total"]] <- CIprop.Lcat.qt[["Unmet"]] <- CIratio.Lcat.qt[["Z"]] <-
    CIratio.Lcat.qt[["Met Demand"]] <- CIratio.Lcat.qt[["Met Demand with Modern Methods"]] <- # change JR, 20140830: added demand met with modern methods
      CIratio.Lcat.qt[["Modern/Total"]]<- temp

      ## [MCW-2016-08-30-3] Initialize elements of mean objects
        meanCIprop.Lcat.qt[["Traditional"]] <- meanCIprop.Lcat.qt[["TotalPlusUnmet"]]<- meanCIprop.Lcat.qt[["TradPlusUnmet"]] <-
    meanCIprop.Lcat.qt[["Modern"]] <- meanCIprop.Lcat.qt[["Total"]] <- meanCIprop.Lcat.qt[["Unmet"]] <-
    meanCIratio.Lcat.qt[["Met Demand"]] <- meanCIratio.Lcat.qt[["Met Demand with Modern Methods"]] <- # change JR, 20140830: added demand met with modern methods
      meanCIratio.Lcat.qt[["Modern/Total"]]<-
      temp.means

    ## Met demand greater than 75%. Can use 'temp.means' to initialize.
     metDemGT.Lg.Lcat.pr[["Met Demand with Modern Methods >= 75%"]] <- temp.means

    cumsum.tot.ts <- cumsum.trad.ts + cumsum.modern.ts
    zero.counts <- numeric(0)
    for (t in 1:nyears){
        ## Don't divide by zero!
        if(!any(cumsumW.t[t] == 0)) {
    CIprop.Lcat.qt[["Traditional"]][,t] <- quantile(cumsum.trad.ts[t,]/cumsumW.t[t],  percentiles, na.rm = TRUE)
    CIprop.Lcat.qt[["TotalPlusUnmet"]][,t] <- quantile((cumsum.unmet.ts[t,]+cumsum.tot.ts[t,])/cumsumW.t[t],  percentiles, na.rm = TRUE)
    CIprop.Lcat.qt[["TradPlusUnmet"]][,t] <- quantile((cumsum.trad.ts[t,]+cumsum.unmet.ts[t,])/cumsumW.t[t],  percentiles, na.rm = TRUE)
    CIprop.Lcat.qt[["Modern"]][,t] <- quantile(cumsum.modern.ts[t,]/cumsumW.t[t],  percentiles, na.rm = TRUE)
    CIprop.Lcat.qt[["Total"]][,t] <- quantile(cumsum.tot.ts[t,]/cumsumW.t[t],  percentiles, na.rm = TRUE)
    CIprop.Lcat.qt[["Unmet"]][,t] <- quantile(cumsum.unmet.ts[t,]/cumsumW.t[t], percentiles, na.rm = TRUE)
    CIratio.Lcat.qt[["Met Demand"]][,t]<- quantile(cumsum.tot.ts[t,]/
                                (cumsum.tot.ts[t,]+cumsum.unmet.ts[t,]), percentiles, na.rm = TRUE)
    CIratio.Lcat.qt[["Met Demand with Modern Methods"]][,t]<- quantile(cumsum.modern.ts[t,]/
                                                     (cumsum.tot.ts[t,]+cumsum.unmet.ts[t,]), percentiles, na.rm = TRUE) # change JR, 20140830: added demand met with modern methods
    CIratio.Lcat.qt[["Modern/Total"]][,t]<- quantile(cumsum.modern.ts[t,]/
                                                     (cumsum.tot.ts[t,]), percentiles, na.rm = TRUE)

    CIratio.Lcat.qt[["Z"]][,t]<- quantile(cumsum.unmet.ts[t,]/
                                                     (cumsumW.t[t] - cumsum.tot.ts[t,]), percentiles, na.rm = TRUE)
    ## [MCW-2016-08-30-4] calculate means
    meanCIprop.Lcat.qt[["Traditional"]][,t] <- mean(cumsum.trad.ts[t,]/cumsumW.t[t],  na.rm = TRUE)
    meanCIprop.Lcat.qt[["TotalPlusUnmet"]][,t] <- mean((cumsum.unmet.ts[t,]+cumsum.tot.ts[t,])/cumsumW.t[t],  na.rm = TRUE)
    meanCIprop.Lcat.qt[["TradPlusUnmet"]][,t] <- mean((cumsum.trad.ts[t,]+cumsum.unmet.ts[t,])/cumsumW.t[t],  na.rm = TRUE)
    meanCIprop.Lcat.qt[["Modern"]][,t] <- mean(cumsum.modern.ts[t,]/cumsumW.t[t],  na.rm = TRUE)
    meanCIprop.Lcat.qt[["Total"]][,t] <- mean(cumsum.tot.ts[t,]/cumsumW.t[t],  na.rm = TRUE)
    meanCIprop.Lcat.qt[["Unmet"]][,t] <- mean(cumsum.unmet.ts[t,]/cumsumW.t[t], na.rm = TRUE)
    meanCIratio.Lcat.qt[["Met Demand"]][,t]<- mean(cumsum.tot.ts[t,]/
                                (cumsum.tot.ts[t,]+cumsum.unmet.ts[t,]), na.rm = TRUE)
    meanCIratio.Lcat.qt[["Met Demand with Modern Methods"]][,t]<- mean(cumsum.modern.ts[t,]/
                                                     (cumsum.tot.ts[t,]+cumsum.unmet.ts[t,]), na.rm = TRUE) # change JR, 20140830: added demand met with modern methods
    meanCIratio.Lcat.qt[["Modern/Total"]][,t]<- mean(cumsum.modern.ts[t,]/
                                                     (cumsum.tot.ts[t,]), na.rm = TRUE)

    metDemGT.Lg.Lcat.pr[["Met Demand with Modern Methods >= 75%"]][,t] <-
        mean((cumsum.modern.ts[t,]/(cumsum.tot.ts[t,]+cumsum.unmet.ts[t,])) >= 0.75, na.rm = TRUE)
        } else zero.counts <- c(zero.counts, t)
    }
    if(length(zero.counts) > 0) warning("\tDenominator counts unknown (or zero) in years :", paste(est.years[zero.counts], collapse = ", "),
                                        "\n\tDenominator counts are read from 'res.country.rda'.")

  # change JR, 20140317
  # find changes based on posterior samples
  P.yp3s  <- array(NA, c(length(years.change.unique), 3, dim(cumsum.trad.ts)[2]))
  dimnames(P.yp3s) <- list(years.change.unique, c("Traditional", "Modern", "Unmet"), NULL)
  # for the props:
zero.counts <- numeric(0)
    for (t in 1:length(years.change.unique)) {
    select <- est.years==years.change.unique[t]
        ## Don't divide by zero!
        if(!any(cumsumW.t[select] == 0)) {
    P.yp3s[t,1,] <- cumsum.trad.ts[select,]/cumsumW.t[select]
    P.yp3s[t,2,] <- cumsum.modern.ts[select,]/cumsumW.t[select]
    P.yp3s[t,3,] <- cumsum.unmet.ts[select,]/cumsumW.t[select]
        } else zero.counts <- c(zero.counts, t)
    }

    if(length(zero.counts) > 0) warning("\tDenominator counts unknown (or zero) in years :", paste(years.change.unique[zero.counts], collapse = ", "),
                                        "\n\tDenominator counts are read from 'res.country.rda'.")
  changeprop.Lcat.Ti <-  GetInfoChange(P.yp3s = P.yp3s, years.change = years.change, years.change2 = years.change2)

  # for the counts:
  for (t in 1:length(years.change.unique)) {
    select <- est.years==years.change.unique[t]
    P.yp3s[t,1,] <- cumsum.trad.ts[select,]
    P.yp3s[t,2,] <- cumsum.modern.ts[select,]
    P.yp3s[t,3,] <- cumsum.unmet.ts[select,]
  }
    changecount.Lcat.Ti <-
        GetInfoChange(P.yp3s = P.yp3s, type.is.prop = FALSE
                    , years.change = years.change, years.change2 = years.change2)
    # for ratios:
    W.y <- rep(0, length = length(years.change.unique))
    names(W.y) <- years.change.unique
    for (t in 1:length(years.change.unique)) {
      select <- est.years==years.change.unique[t]
      W.y[t] <- cumsumW.t[select]
      }
    changeratio.Lcat.Ti <- GetInfoChange(P.yp3s = P.yp3s, type.is.prop = FALSE, type.is.ratio = TRUE
                                       , years.change = years.change, years.change2 = years.change2
                                         ,W.y = W.y)
  ##value<< Object of class \code{\link{Results}} with
  return(list(
    changecount.Lcat.Ti = changecount.Lcat.Ti, ##<< Info on changes in counts
    changeprop.Lcat.Ti = changeprop.Lcat.Ti, ##<< Info on changes in proportions
    changeratio.Lcat.Ti = changeratio.Lcat.Ti, ##<< Info on changes in ratios
    W.t = cumsumW.t,##<< Number of MWRA for the aggregate
    CIprop.Lcat.qt = CIprop.Lcat.qt, ##<< CIs for props
    CIratio.Lcat.qt = CIratio.Lcat.qt ##<< CIs for counts
    ## [MCW-2016-08-30-5] Output means.
    ,meanCIprop.Lcat.qt = meanCIprop.Lcat.qt, ##<< meanCIs for props
    meanCIratio.Lcat.qt = meanCIratio.Lcat.qt, ##<< meanCIs for counts
    metDemGT.Lg.Lcat.pr = metDemGT.Lg.Lcat.pr ##<< met demand for modern methods greater than 75%
    ))
}

#--------------------------------------------------------------------
GetAggregates <- function(# Construct aggregate estimates
  ### Estimate the proportion/number of MWRA in various categories for aggregates.
  run.name,##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored,
  ## as well as folder with country trajectories
  ## If NULL, it's \code{output/run.name}, the default from \code{runMCMC}.
  file.aggregates = NULL, ##<< If NULL (default), UNDP aggregates are constructed.
  ##Alternatively, file path of alternative grouping should be given, e.g.
  ##\code{file.aggregates = "data/MDGgroupings.csv")}. Such data file needs to contain
  ## columns \code{iso.country}, \code{groupname} and \code{iso.group} (which may contain missing values).
  ## Each country can only be included once (can only be part of one grouping).
  years.change = matrix(c(1990.5, 2000.5,
                          2000.5, 2010.5,
                          1990.5, 2010.5),
                        ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
  ## containing yyyy1 and column 2 containing yyyy2 for calculating change yyyy1-yyyy2
  years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                           2000.5, 2005.5, 2010.5,
                           1995.5, 2000.5, 2005.5,
                           1990.5, 1995.5, 2000.5,
                           1990.5, 2000.5, 2010.5),
                        ncol = 3, byrow = TRUE), ##<< Matrix with 3 columns, with column 1
  ## containing yyyy1, column 2 containing yyyy2 and column 3 containing yyyy3 for
  ## calculating change (yyyy2-yyyy3) - (yyyy1-yyyy2)
  ## The years 1990, 2000 and 2010 are always included.
  ## Years outside (\code{start.year}, \code{end.year}) are ignored.
  ## Mid-point years closest to the given \code{years.change} are used for calculations.
  winbugs.data = NULL, ##<< Object of class \code{winbugs.data}, needed only for UNDP aggregates
  region.info = NULL,##<< Object of class \code{region.info}, needed only for UNDP aggregates.
  countries.to.include.in.aggregates.csv = NULL ##<< country ISO codes that should be used to form aggregates. NULL means all.
  ,verbose = TRUE){

if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name, "/")
}

    load(file.path(output.dir, "res.country.rda")) # change JR, 20140418
    iso.Ptp3s.key.df <-
        read.csv(file.path(output.dir, "iso.Ptp3s.key.csv"), stringsAsFactors = FALSE)

    ## Names of countries in 'iso.Ptp3s.key.df' may be corrupted due
    ## to non-ASCII characters in some country names. This is a
    ## problem because aggregates will be created by matching country
    ## names from 'iso.Ptp3s.key.df' to those in res.country$W.Lg.t.
    ##
    ## SO: Use the iso.g element of res.country to rename countries in
    ## 'iso.Ptp3s.key.df' for use in creating aggregates.

    res.country_names <-
        data.frame(iso.c = res.country$iso.g, name.c = names(res.country[["CIprop.Lg.Lcat.qt"]]))
    iso.Ptp3s.key.df <-
        base::merge(iso.Ptp3s.key.df[, c("iso.c", "filename")],
                    res.country_names,
                    all.x = TRUE, all.y = FALSE, sort = FALSE)[, c("iso.c", "name.c", "filename")]

  W.Lc.t <- res.country$W.Lg.t
#  if (is.null(output.dir.countrytrajectories) | is.null(W.Lc.t)){
#    print("First save country trajectories and read in number of MWRA!")
#    return()
#  }
  output.dir.countrytrajectories <- file.path(output.dir, "/countrytrajectories/")
  nyears <-  length(W.Lc.t[[1]])
    load(file.path(output.dir, "mcmc.meta.rda")) # change JR, 20140418

  ## [MCW-2016-09-02-10] :: Include countries with no data in calculation of
  ## aggregates. Should be enough to redefine 'country.info'.
  if(mcmc.meta$general$include.c.no.data) {
      country.info <-
          rbind(mcmc.meta$data.raw$country.info, mcmc.meta$data.raw$country.info.no.data)
  } else country.info <- mcmc.meta$data.raw$country.info

    ## Keep only countries in 'countries.to.include.in.aggregates.csv'
    if(!is.null(countries.to.include.in.aggregates.csv)) {
        countries.keep.df <-
            read.csv(file = countries.to.include.in.aggregates.csv, row.names = NULL, stringsAsFactors = TRUE)
        iso.Ptp3s.key.df <-
            iso.Ptp3s.key.df[iso.Ptp3s.key.df$iso.c %in% countries.keep.df$ISO.Code,]
        country.info <- country.info[country.info$iso.c %in% countries.keep.df$ISO.Code,]
    }

    ## [MCW-2017-07-05-4] :: Use ~iso.Ptp3s.key.df~ to determine which
    ## ~countrytrajectories/P.tp3s_country[x]1.rda~ files are loaded to create
    ## the aggregates.
  C <- nrow(iso.Ptp3s.key.df)
  res.aggregate <- list()
  if (is.null(file.aggregates)){# construct UNPD aggregates
    message("Overview: Constructing aggregates for UNPD regions, and dev/dev-ing countries (excl China)")

    region.info <- mcmc.meta$data.raw$region.info

    W.Lg.t <- list()

    ## 2019-02-08 :: Make 'internal' aggregates in a loop

    ## UNPD Regions
    res.aggregate <-
        c(res.aggregate,
          make_country_aggregates_internal(family = "UNPD",
                                           C = C, iso.Ptp3s.key.df = iso.Ptp3s.key.df,
                                           output.dir.countrytrajectories = output.dir.countrytrajectories,
                                           years.change = years.change, years.change2 = years.change2,
                                           W.Lc.t = W.Lc.t, verbose = verbose))

    # Add subregs
    for (subreg in 1:region.info$n.subreg){
        select.iso <- country.info$iso.c[country.info$subreg.c==subreg]
      select.c <- seq(1, C)[iso.Ptp3s.key.df$iso.c %in% select.iso]
      nameg <- region.info$name.subreg[subreg]
      if(verbose) message("(", subreg, "/", region.info$n.subreg, ") Constructing aggregates for the ", length(select.c), " countries in ", nameg, ".\n")
      ## Need to make sure India is added if it was modelled as its own subregion.
      if(nameg == "South-Central Asia" && !(356 %in% iso.Ptp3s.key.df$iso.c[select.c])) {
          select.c <- c(select.c, seq(1:C)[iso.Ptp3s.key.df$iso.c == 356])
          nameg <- "South-Central Asia (Incl. India)"
          }
      res.aggregate[[nameg]] <- InternalGetAggregates(W.Lc.t = W.Lc.t, select.c = select.c,
                                             dir.traj = output.dir.countrytrajectories,
                                             years.change = years.change,
                                             years.change2 = years.change2
                              ,iso.Ptp3s.key.df = iso.Ptp3s.key.df,
                                             verbose = verbose)
    }
    # For regs
    for (reg in 1:region.info$n.reg){
        select.iso <- country.info$iso.c[country.info$reg.c==reg]
        select.c <- seq(1, C)[iso.Ptp3s.key.df$iso.c %in% select.iso]
      nameg <- region.info$name.reg[reg]
      if(nameg %in% region.info$name.subreg) message("NOTE: ", nameg, " is also a subregion and will not be stored or plotted separately.")
      if(verbose) message("(", reg, "/", region.info$n.reg, ") Constructing aggregates for the ", length(select.c), " countries in ", nameg, ".\n")
      res.aggregate[[nameg]] <- InternalGetAggregates(W.Lc.t = W.Lc.t, select.c = select.c,
                                                      dir.traj = output.dir.countrytrajectories,
                                                      years.change = years.change,
                                                      years.change2 = years.change2
                                                     ,iso.Ptp3s.key.df = iso.Ptp3s.key.df,
                                             verbose = verbose)
    }
    # World
    select.c <- seq(1, C)
    if(verbose) message("Constructing aggregates for the ", length(select.c), " countries in the world")
    nameg <- "World"
    res.aggregate[[nameg]] <- InternalGetAggregates(W.Lc.t = W.Lc.t, select.c = select.c,
                                                    dir.traj = output.dir.countrytrajectories,
                                                    years.change = years.change,
                                                    years.change2 = years.change2
                                                   ,iso.Ptp3s.key.df = iso.Ptp3s.key.df,
                                             verbose = verbose)

    G <- length(res.aggregate)
    iso.g <- rep(NA, G)
  } else {
    # read alternative grouping (length m, need iso and groupname)
    group.data <- read.csv(file = file.aggregates, stringsAsFactors = FALSE)
    # this file needs two columns (any other columns are ignored)
    # iso and groupname
    if (is.null(group.data$iso.country) | is.null(group.data$groupname) | is.null(group.data$iso.group)){
      message("The csv file provided does not contain column(s) iso.country and/or groupname and/or iso.group!")
      message("Fix that (note that missing values in iso.group column are allowed).")
    } else {
      groupname.m <- group.data$groupname
      iso.m <- group.data$iso.country
      groupnames <- unique(groupname.m)
      G <- length(groupnames)
      iso.g  <- rep(NA, G)
      W.gt <- matrix(NA, G, length(W.Lc.t))
      for (g in 1:G){
        nameg <- paste(groupnames[g])
        if(verbose) message("Constructing aggregates for ", nameg, " (", G, " groups in total)")
        select.c <- seq(1, C)[is.element(country.info$iso.c,
                                                      iso.m[groupname.m==nameg])]
        iso.g[g] <- group.data$iso.group[select.c[1]]

        res.aggregate[[nameg]] <- InternalGetAggregates(W.Lc.t = W.Lc.t, select.c = select.c,
                                                        dir.traj = output.dir.countrytrajectories,
                                                        years.change = years.change,
                                                        years.change2 = years.change2
                              ,iso.Ptp3s.key.df = iso.Ptp3s.key.df,
                                             verbose = verbose)
      }
    }
  }

  W.Lg.t <- lapply(res.aggregate, function(l) l$W.t)
  CIprop.Lg.Lcat.qt <- lapply(res.aggregate, function(l) l$CIprop.Lcat.qt)
  CIratio.Lg.Lcat.qt <- lapply(res.aggregate, function(l) l$CIratio.Lcat.qt)
  changeprop.Lg.Lcat.Ti <- lapply(res.aggregate, function(l) l$changeprop.Lcat.Ti)
    changecount.Lg.Lcat.Ti <- lapply(res.aggregate, function(l) l$changecount.Lcat.Ti)
  changeratio.Lg.Lcat.Ti <- lapply(res.aggregate, function(l) l$changeratio.Lcat.Ti)

  ## [MCW-2016-08-30-6] :: Extract the means from objects created by
  ## InternalGetAggregates(). Elements at lowest level are matrices with one row
  ## but they need to be vectors for GetTablesRes() so drop the un-needed
  ## dimension.
  meanProp.Lg.Lcat <-
      lapply(res.aggregate, function(l) lapply(l$meanCIprop.Lcat.qt, function(z) z[,]))
  meanRatio.Lg.Lcat <-
      lapply(res.aggregate, function(l) lapply(l$meanCIratio.Lcat.qt, function(z) z[,]))

    ## Do the same for met demand greater than
    metDemGT.Lg.Lcat.pr <-
        lapply(res.aggregate, function(l) lapply(l$metDemGT.Lg.Lcat.pr, function(z) z[,]))

  # add counts for levels:
  CIcount.Lg.Lcat.qt <-
    FromCIpropToCIcount(CIprop.Lg.Lcat.qt = CIprop.Lg.Lcat.qt, W.Lg.t = W.Lg.t)

  ##value<< Object of class \code{\link{Results}},
  ## either for all subregions, regions and the world (UNDP aggregates),
  ## or for alternative groupings.
  return(list(iso.g = iso.g, ##<< Iso codes, but note that aggregate names (never missing) are used to name the results lists.
              CIprop.Lg.Lcat.qt = CIprop.Lg.Lcat.qt, ##<< Proportions for Total, Traditional, Modern, Unmet, TotalPlusUnmet, TradPlusUnmet.
              ## Percentages are included as names in the \code{.qt}-matrix without percentage signs.
              CIratio.Lg.Lcat.qt = CIratio.Lg.Lcat.qt,##<<Ratios Met Demand, Z (unmet/none) and modern/total (R). R and Z might not be included for aggregates.
              ## [MCW-2016-08-30-7] :: Output means to 'res.aggregate.rda'.
              meanProp.Lg.Lcat = meanProp.Lg.Lcat,
              meanRatio.Lg.Lcat = meanRatio.Lg.Lcat,
              CIcount.Lg.Lcat.qt = CIcount.Lg.Lcat.qt,##<< Counts for same categories as under proportions.
              changeprop.Lg.Lcat.Ti = changeprop.Lg.Lcat.Ti,##<< Changes in proportions for \code{T = years.change}, \code{i} refers to percentiles and PPPC (the posterior probability of a positive change).
              changecount.Lg.Lcat.Ti = changecount.Lg.Lcat.Ti,##<< Changes in counts, same notation as for proportions.
              changeratio.Lg.Lcat.Ti = changeratio.Lg.Lcat.Ti,##<< Changes in ratios, same notation as for proportions.
              metDemGT.Lg.Lcat.pr = metDemGT.Lg.Lcat.pr,
              W.Lg.t = W.Lg.t##<< Number of MWRA
              ))
}
#-------------------------------------------------------------------------
## THIS FUNCTION IS NEVER USED
SelectCountriesInRegion <- function(# Select countries in a particular region
  ### Select countries in a particular region
  region.name,
  country.info,
  region.info
) {
  if (region.name %in% region.info$name.subreg) {
    select.c <- country.info$namesubreg.c == region.name
  } else if (region.name %in% region.info$name.reg) {
    select.c <- country.info$namereg.c == region.name
  } else if (region.name == "World") {
    select.c <- rep(TRUE, length(country.info$name.c))
  } else if (region.name == "Developed regions") {
    select.c <- country.info$dev.c == "Rich"
  } else if (region.name == "Developing regions") {
    select.c <- country.info$dev.c != "Rich"
  } else if (region.name == "Developing (excl. China)") {
    select.c <- country.info$dev.c != "Rich" & country.info$name.c != "China"
  } else if (region.name == "Mela-Micro-Polynesia") {
    select.c <- is.element(country.info$namesubreg.c,
                           c("Melanesia" , "Micronesia", "Polynesia"))
  } else {
    stop(paste0(region.name, " does not exist!"))
  }
  return(select.c)
}
#-------------------------------------------------------------------------
##' .. title (single sentence, sentence case, full stop at end) ..
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param agg.names
##' @param counts.names
##' @param ratios.names
##' @param iso.both
##' @param output.dir
##' @param years.change
##' @param changes.years.names
##' @return
##' @author
##' @noRd
GetAggregatesAllWomen <-
    function(# Construct aggregate estimates
  ### Estimate the proportion/number of MWRA in various categories for aggregates.
  run.name = "test",##<< Run name
  uwra.output.dir = NULL, ##<< Directory where unmarried MCMC array and meta are stored,
  mwra.output.dir = NULL, ##<< Directory where married MCMC array and meta are stored,
  awra.output.dir = uwra.output.dir, ##<< Directory where all women MCMC array and meta are stored,
  WRA.csv = NULL,
  ## as well as folder with country trajectories
  ## If NULL, it's \code{output/run.name}, the default from \code{runMCMC}.
  file.aggregates = NULL, ##<< If NULL (default), UNDP aggregates are constructed.
  ##Alternatively, file path of alternative grouping should be given, e.g.
  ##\code{file.aggregates = "data/MDGgroupings.csv")}. Such data file needs to contain
  ## columns \code{iso.country}, \code{groupname} and \code{iso.group} (which may contain missing values).
  ## Each country can only be included once (can only be part of one grouping).
  years.change = matrix(c(1990.5, 2000.5,
                          2000.5, 2010.5,
                          1990.5, 2010.5),
                        ncol = 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
  ## containing yyyy1 and column 2 containing yyyy2 for calculating change yyyy1-yyyy2
  years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                           2000.5, 2005.5, 2010.5,
                           1995.5, 2000.5, 2005.5,
                           1990.5, 1995.5, 2000.5,
                           1990.5, 2000.5, 2010.5),
                        ncol = 3, byrow = TRUE), ##<< Matrix with 3 columns, with column 1
  ## containing yyyy1, column 2 containing yyyy2 and column 3 containing yyyy3 for
  ## calculating change (yyyy2-yyyy3) - (yyyy1-yyyy2)
  ## The years 1990, 2000 and 2010 are always included.
  ## Years outside (\code{start.year}, \code{end.year}) are ignored.
  ## Mid-point years closest to the given \code{years.change} are used for calculations.
  ## Compress RData files created during aggregate creation? If FALSE, files not compressed during calculation but final files are always compressed using 'resaveRdaFiles()'.
 compress.RData = FALSE,
  countries.to.include.in.aggregates.csv = NULL,
 verbose = TRUE) {

        ## -------* Sub-functions

        ## -------** Summarize outputs (calculate 95% PIs, PPPCs)

        CP.summ.f <- function(z) {
            apply(z, c(1,2), function(y) {
                quantile(y, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
                })
            }

        CP.change.summ.f <- function(z) {
            apply(z, c(1, 2), function(y) {
                c(quantile(y, probs = c(0.025, 0.1, 0.5, 0.9, 0.975), na.rm = TRUE)
                 ,PPPC = mean(y > 0))
                })
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

        ## -------* Constants

        ## Much of this IS THE SAME as it is in 'ConstructOutputAllWomen()'.

        ## -------** Directories / Files

        ## Directories
        if (is.null(uwra.output.dir)){
            uwra.output.dir <- file.path(getwd(), "output", run.name, "/")
        }
        countrytrajectories.dir.uwra <- file.path(uwra.output.dir, "countrytrajectories")
        countrytrajectories.dir.mwra <- file.path(mwra.output.dir, "countrytrajectories")

        ## Temp dir for aggregate lists
        agg.li.dir <-
            file.path(awra.output.dir, makeSpecAggSubdir(file.aggregates))
        if (!dir.exists(agg.li.dir)) dir.create(agg.li.dir, recursive = TRUE)

        ## -------** MCMC Meta

        load(file.path(uwra.output.dir, "mcmc.meta.rda"))
        uwra.mcmc.meta <- mcmc.meta

        load(file.path(mwra.output.dir, "mcmc.meta.rda"))
        mwra.mcmc.meta <- mcmc.meta

        uwra.winbugs.data <- uwra.mcmc.meta$winbugs.data
        mwra.winbugs.data <- mwra.mcmc.meta$winbugs.data

        ## -------** Get number of iterations

        ## Just load the first country from uwra and mwra to check dates and countries.
        load(file.path(countrytrajectories.dir.uwra
                      ,"P.tp3s_country1.rda"
                       ))
        uwra1.country <- P.tp3s
        load(file.path(countrytrajectories.dir.mwra
                      ,"P.tp3s_country1.rda"
                       ))
        mwra1.country <- P.tp3s

        ## Which is the smaller of mcmc arrays
        n.iters <- min(dim(uwra1.country)[3], dim(mwra1.country)[3])

        ## -------** Indicator Names

        ## Don't change these without checking which subfunctions
        ## depend on them (e.g., 'InternalMakeRatios()') AND what they
        ## are called in 'ConstructOutputAllWomen().
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

        ## -------** Estimation years

        ## -------*** Single years

        uwra.years <- dimnames(uwra1.country)[[1]]
        mwra.years <- dimnames(mwra1.country)[[1]]

        est.years <- uwra.years

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

        ## -------** Country names and ISO codes

        if(uwra.mcmc.meta$general$include.c.no.data) {
            uwra.country.info <-
                rbind(uwra.mcmc.meta$data.raw$country.info
                     ,uwra.mcmc.meta$data.raw$country.info.no.data
                      )
        } else uwra.country.info <- uwra.mcmc.meta$data.raw$country.info

        if(mwra.mcmc.meta$general$include.c.no.data) {
            mwra.country.info <-
                rbind(mwra.mcmc.meta$data.raw$country.info
                     ,mwra.mcmc.meta$data.raw$country.info.no.data
                      )
        } else mwra.country.info <- mwra.mcmc.meta$data.raw$country.info

        ## -------** Region info

        if(!is.null(uwra.mcmc.meta$data.raw$region.info.no.data)) {
            uwra.region.info <- uwra.mcmc.meta$data.raw$region.info.no.data
        } else uwra.region.info <- uwra.mcmc.meta$data.raw$region.info

        if(!is.null(mwra.mcmc.meta$data.raw$region.info.no.data)) {
            mwra.region.info <- mwra.mcmc.meta$data.raw$region.info.no.data
        } else uwra.region.info <- uwra.mcmc.meta$data.raw$region.info

        ## -------** Load population counts of married and unmarried women (denominators)

        ## From 'ConstructOutputAllWomen()'

        ## -------*** Unmarried women

        if(verbose) message("\nLoading UWRA women population counts from '", WRA.csv, "'.")
        uwra.denom.counts.li <-                      # a list, top-level elements are countries
            ReadWRA(est.years = est.years
                    ,winbugs.data = uwra.mcmc.meta$winbugs.data
                    ,country.info = uwra.country.info
                    ,WRA.csv = WRA.csv
                    ,return.iso = TRUE
                   ,in_union = 0
                    ,verbose = verbose)
        uwra.counts.iso <- uwra.denom.counts.li[[2]]
        uwra.denom.counts.li <- uwra.denom.counts.li[[1]]

        ## CHECK to determine if countries in counts file match those in the MCMC output.
        if(length(uwra.denom.counts.li) > nrow(uwra.counts.iso)) {
            idx <- !(names(uwra.denom.counts.li) %in% uwra.counts.iso$name.c)
            not.in.mcmc <- names(uwra.denom.counts.li)[idx]
            message(paste("The following countries are in the unmarried women counts file, but not in the MCMC output:\n    "
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
                      ,".\nPlease add counts to the counts file '", WRA.csv, "' and re-run."
                      ,sep = ""))
        }

        ## -------*** Married women

        if(verbose) message("\nLoading MWRA women population counts from '", WRA.csv, "'.")
        mwra.denom.counts.li <-  ReadWRA(est.years = est.years
                                         ,winbugs.data = mwra.mcmc.meta$winbugs.data
                                         ,country.info = mwra.country.info
                                         ,WRA.csv = WRA.csv
                                         ,return.iso = TRUE
                                        ,in_union = 1
                                         ,verbose = verbose
                                          )
        mwra.counts.iso <- mwra.denom.counts.li[[2]]
        mwra.denom.counts.li <- mwra.denom.counts.li[[1]]

        ## CHECK to determine if countries in counts file match those in the MCMC output.
        if(length(mwra.denom.counts.li) > nrow(mwra.counts.iso)) {
            idx <- !(names(mwra.denom.counts.li) %in% mwra.counts.iso$name.c)
            not.in.mcmc <- names(mwra.denom.counts.li)[idx]
            message(paste("The following countries are in the married women counts file, but not in the MCMC output:\n    "
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
                      ,".\nPlease add counts to the counts file '", WRA.csv, "' and re-run."
                      ,sep = ""))
        }

        ## -------** Get ISOs in all inputs

        ## ISOs in both UWRA and MWRA Countrytrajectories
        iso.both <- intersect(mwra.country.info$iso.c, uwra.country.info$iso.c)

        ## Counts
        iso.both <-
            intersect(iso.both, intersect(uwra.counts.iso$iso.c, mwra.counts.iso$iso.c))

        ## Indices for 'both' into UWRA and MWRA ISO lists. Need these
        ## to load correct countrytrajectories file.  !!! MUST DO THIS
        ## BEFORE EXCLUDING ISOs NOT IN
        ## 'countries.to.include.in.aggregates.csv' because the saved
        ## country trajectories are numbered 1:nrow(uwra.country.info).
        iso.idx.mwra <-
            as.numeric(sapply(iso.both, function(z) which(mwra.country.info$iso.c == z)))
        iso.idx.uwra <-
            as.numeric(sapply(iso.both, function(z) which(uwra.country.info$iso.c == z)))

        ## They had better be the same length!
        stopifnot(identical(length(iso.idx.mwra), length(iso.idx.uwra)))

        ## Exclude countries not in 'countries.to.include.in.aggregates.csv'
        if(!is.null(countries.to.include.in.aggregates.csv)) {
            countries.keep.df <-
                read.csv(file = countries.to.include.in.aggregates.csv, row.names = NULL, stringsAsFactors = TRUE)
            iso.both <- iso.both[as.numeric(iso.both) %in% as.numeric(countries.keep.df$ISO.Code)]
            uwra.country.info <-
                uwra.country.info[as.numeric(uwra.country.info$iso.c) %in% as.numeric(countries.keep.df$ISO.Code),]
            mwra.country.info <-
                mwra.country.info[as.numeric(mwra.country.info$iso.c) %in% as.numeric(countries.keep.df$ISO.Code),]
        }

        ## C names
        uwra.c.name.both <-
            sapply(iso.both, function(z) {
                uwra.country.info$name.c[as.character(uwra.country.info$iso.c) == as.character(z)]
            })
        mwra.c.name.both <-
            sapply(iso.both, function(z) {
                mwra.country.info$name.c[as.character(mwra.country.info$iso.c) == as.character(z)]
            })

        ## They had better be the same length!
        stopifnot(identical(length(uwra.c.name.both), length(mwra.c.name.both)))

        ## -------** Aggregate names (UNPD Aggregates)

        ## Names of all aggregate categories
        if(is.null(file.aggregates)) {
            aggregates.from.file.df <- NULL #default aggregates
            aggregates <- c(get_aggregate_names(family = "UNPD"),
                            uwra.region.info$name.subreg,
                            uwra.region.info$name.reg,
                            "World")
            names(aggregates)[names(aggregates) == ""] <-
                aggregates[names(aggregates) == ""]

            ## Remove duplicates (e.g., Northern America)
            reg.dup <-
                uwra.region.info$name.reg %in% uwra.region.info$name.subreg
            reg.dup <- uwra.region.info$name.reg[reg.dup]
            for(i in seq_along(reg.dup)) {
                message("NOTE: ", reg.dup[i], " is also a subregion and will not be stored or plotted separately.")
            }
            aggregates <- aggregates[!duplicated(aggregates)]

        } else {

            aggregates.from.file <- read.csv(file = file.aggregates, stringsAsFactors = FALSE)
            message("Aggregates read from file '", file.aggregates,
                    "'. Special aggregate 'World' not added; include it in '",
                    file.aggregates, "' if you want it.")
            aggregates <- unique(as.character(aggregates.from.file$groupname))
            names(aggregates) <- aggregates
            aggregates.from.file.df <-
                data.frame(iso = as.numeric(aggregates.from.file$iso.country),
                           groupname = as.character(aggregates.from.file$groupname),
                           stringsAsFactors = FALSE)
        }

        aggregates.names.df <-
            data.frame(agg.name = aggregates,
                     file.name = makeFileName(abbreviate(names(aggregates), minlength = 12)),
                     stringsAsFactors = FALSE,
                     display.label = names(aggregates),
                     row.names = NULL)

        ## -------** Tidy Up

        ## Memory useage has been a problem. This might help...
        rm(mcmc.meta)
        rm(list = c("uwra1.country", "mwra1.country"))

        ## -------* Output lists to hold aggregates

        ## -------** Counts

        InternalMakeIndividualAggLists("awra_CP_counts_agg_li"
                             ,output.dir = agg.li.dir
                            , n.iters = n.iters
                            ,years.names = est.years
                            ,d2.names = counts.names
                             ,aggregates.names.df = aggregates.names.df
                             ,compress.RData = compress.RData)

        ## Need these to calculate ratios (e.g., unmarried / all)
        ## They're never summarized as CIs, hence no '.CIs' in the object names.
        InternalMakeIndividualAggLists("uwra_CP_counts_agg_li"
                            ,output.dir = agg.li.dir
                            , n.iters = n.iters
                            ,years.names = est.years
                            ,d2.names = counts.names
                            ,aggregates.names.df = aggregates.names.df
                             ,compress.RData = compress.RData)

        InternalMakeIndividualAggLists("mwra_CP_counts_agg_li"
                            ,output.dir = agg.li.dir
                            , n.iters = n.iters
                            ,years.names = est.years
                            ,d2.names = counts.names
                             ,aggregates.names.df = aggregates.names.df
                             ,compress.RData = compress.RData)

        ## -------* Loop over countries

        for(j in seq_along(iso.both)) {

            ## -------** Indices

            ## For countrytrajectories
            iso.both.j <- iso.both[j]
            iso.idx.uwra.j <- iso.idx.uwra[j]
            iso.idx.mwra.j <- iso.idx.mwra[j]

            ## For denominator counts
            uwra.counts.iso.idx.j <- which(uwra.counts.iso$iso.c == iso.both.j)
            mwra.counts.iso.idx.j <- which(mwra.counts.iso$iso.c == iso.both.j)

            if(is.null(aggregates.from.file.df) || (
                iso.both.j %in% aggregates.from.file.df$iso)
               ) { #this is getting messy :(

                ## -------** Load all women counts

                load(file = file.path(awra.output.dir, "countrytrajectories"
                                     ,paste0("aw_ISO_", iso.both.j, "_counts.rda")))
                if(verbose) message("\nMaking all women aggregate counts for ISO ", iso.both.j, ", ", uwra.c.name.both[j])

                ## -------** UWRA and MWRA Proportions

                ## Need to select the correct file to load.
                load(file.path(countrytrajectories.dir.uwra
                              ,paste0("P.tp3s_country", iso.idx.uwra.j, ".rda")
                               ))
                uwra.CP.props.j <- P.tp3s[as.character(est.years),,1:n.iters]

                load(file.path(countrytrajectories.dir.mwra
                              ,paste0("P.tp3s_country", iso.idx.mwra.j, ".rda")
                               ))
                mwra.CP.props.j <- P.tp3s[as.character(est.years),,1:n.iters]

                ## Make arrays to hold this country's counts that have same dims as
                ## mcmc outputs. These are initially filled with the population
                ## counts (denominators) but will be multiplied by proportions
                ## later (this is, admittedly, confusing.. don't do again).
                uwra.CP.counts.j <-
                    array(rep(uwra.denom.counts.li[[uwra.counts.iso.idx.j]], n.iters)
                         ,dim = c(length(uwra.denom.counts.li[[uwra.counts.iso.idx.j]])
                                 ,length(counts.names)
                                 ,n.iters
                                  )
                          )
                dimnames(uwra.CP.counts.j) <-
                    list(dimnames(uwra.CP.props.j)[[1]]
                        ,counts.names
                         )

                mwra.CP.counts.j <-
                    array(rep(mwra.denom.counts.li[[mwra.counts.iso.idx.j]], n.iters)
                         ,dim = c(length(mwra.denom.counts.li[[mwra.counts.iso.idx.j]])
                                 ,length(counts.names)
                                 ,n.iters
                                  )
                          )
                dimnames(mwra.CP.counts.j) <-
                    list(dimnames(mwra.CP.props.j)[[1]]
                        ,counts.names
                         )

                tot.population.counts.j <- uwra.CP.counts.j + mwra.CP.counts.j
                                # Array; 1st dim is year, 2nd dim is 'Total',
                                # 'Modern', etc., 3rd dim is iteration. Values
                                # are duplicated across iterations. This is used
                                # later to turn trajectories of CP counts into
                                # trajectories of CP proportions, hence the
                                # repetition across iterations to make the
                                # operation 'vector-valued'.

                ## NB: Only have "Traditional" "Modern" "Unmet" in props
                uwra.CP.counts.j[,dimnames(uwra.CP.props.j)[[2]],] <-
                    uwra.CP.counts.j[,dimnames(uwra.CP.props.j)[[2]],] * uwra.CP.props.j
                mwra.CP.counts.j[,dimnames(mwra.CP.props.j)[[2]],] <-
                    mwra.CP.counts.j[,dimnames(mwra.CP.props.j)[[2]],] * mwra.CP.props.j

                ## Tidy up
                rm(list = c("uwra.CP.props.j", "mwra.CP.props.j"))

                ## Calculate Total, TotalPlusUnmet, TradPlusUnmet
                uwra.CP.counts.j[,"Total",] <-
                    uwra.CP.counts.j[,"Modern",] + uwra.CP.counts.j[,"Traditional",]
                uwra.CP.counts.j[,"TotalPlusUnmet",] <-
                    uwra.CP.counts.j[,"Total",] + uwra.CP.counts.j[,"Unmet",]
                uwra.CP.counts.j[,"TradPlusUnmet",] <-
                    uwra.CP.counts.j[,"Traditional",] + uwra.CP.counts.j[,"Unmet",]

                mwra.CP.counts.j[,"Total",] <-
                    mwra.CP.counts.j[,"Modern",] + mwra.CP.counts.j[,"Traditional",]
                mwra.CP.counts.j[,"TotalPlusUnmet",] <-
                    mwra.CP.counts.j[,"Total",] + mwra.CP.counts.j[,"Unmet",]
                mwra.CP.counts.j[,"TradPlusUnmet",] <-
                    mwra.CP.counts.j[,"Traditional",] + mwra.CP.counts.j[,"Unmet",]

                ## -------** Aggregates

                ## For this country ('iso.both.j'), loop over each aggregate
                ## (done inside Internal... function), checking to see
                ## if iso.both.j is a member of the aggregate. If it is, increment
                ## the respective counts in 'xxxx.CP.counts.agg.CIs.li', including
                ## the denominator counts.

                InternalAllWomenAggregateCounts("awra_CP_counts_agg_li"
                                               ,output.dir = agg.li.dir
                                               ,iso.both.j = iso.both.j
                                               ,CP.counts.j = awra.CP.counts.j
                                               ,uwra.denom.counts = uwra.denom.counts.li[[uwra.counts.iso.idx.j]]
                                               ,mwra.denom.counts = mwra.denom.counts.li[[mwra.counts.iso.idx.j]]
                                               ,uwra.country.info = uwra.country.info
                                               ,uwra.region.info = uwra.region.info
                                               ,verbose = verbose
                                               ,aggregates.names.df = aggregates.names.df
                                               ,reg.dup = reg.dup
                                               ,aggregates.from.file.df = aggregates.from.file.df
                                               ,compress.RData = compress.RData)

                InternalAllWomenAggregateCounts("uwra_CP_counts_agg_li"
                                               ,output.dir = agg.li.dir
                                               ,iso.both.j = iso.both.j
                                               ,CP.counts.j = uwra.CP.counts.j
                                               ,uwra.denom.counts = uwra.denom.counts.li[[uwra.counts.iso.idx.j]]
                                               ,mwra.denom.counts = NULL
                                               ,uwra.country.info = uwra.country.info
                                               ,uwra.region.info = uwra.region.info
                                               ,aggregates.names.df = aggregates.names.df
                                               ,reg.dup = reg.dup
                                               ,aggregates.from.file.df = aggregates.from.file.df
                                               ,compress.RData = compress.RData,
                                                verbose = FALSE)

                InternalAllWomenAggregateCounts("mwra_CP_counts_agg_li"
                                               ,output.dir = agg.li.dir
                                               ,iso.both.j = iso.both.j
                                               ,CP.counts.j = mwra.CP.counts.j
                                               ,uwra.denom.counts = NULL
                                               ,mwra.denom.counts = mwra.denom.counts.li[[mwra.counts.iso.idx.j]]
                                               ,uwra.country.info = uwra.country.info
                                               ,uwra.region.info = uwra.region.info
                                               ,aggregates.names.df = aggregates.names.df
                                               ,reg.dup = reg.dup
                                               ,aggregates.from.file.df = aggregates.from.file.df
                                               ,compress.RData = compress.RData,
                                                verbose = FALSE)

                ## -------** Tidy up

                rm(list = c("tot.population.counts.j"
                           ,"uwra.CP.counts.j", "mwra.CP.counts.j"))

            }

        } ## END: 'for(j in 1:length(iso.both))' (the country loop)

        ## -------* Finish Aggregates

        ## Aggregate counts were created in the country loop above. Need to get
        ## proportions and ratios. Need to do it all here because the counts are
        ## cumulated, hence not complete until the end of the country loop.

        ## Create outputs in a particular order so don't have huge objects lying
        ## around until they are needed. Ratios and proportions first, because
        ## they have to be done before counts are summarized and the full set of
        ## trajectories thrown away. Ratios before proportions because can then
        ## throw away uwra and mwra counts.

        ## -------** Counts

        message("\nSaving counts")

        awra.CP.counts.agg.CIs.li <-
            InternalMakeAggLists(NULL #returns object
                                 ,aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = est.years
                             ,d2.names = counts.names #<--
                              )
        for(agg in aggregates.names.df$display.label) {
            filename.agg <- paste0("awra_CP_counts_agg_li", "_", aggregates.names.df[aggregates.names.df$display.label == agg, "file.name"], ".RData")
            load(file = file.path(agg.li.dir, filename.agg))
            awra.CP.counts.agg.CIs.li[[agg]] <- res.aggregate.list
            file.remove(file.path(agg.li.dir, filename.agg))
        }
        res.aggregate.list <- awra.CP.counts.agg.CIs.li
        rm(awra.CP.counts.agg.CIs.li)
        save(res.aggregate.list, file = file.path(agg.li.dir, "awra_CP_counts_agg_li.RData"))

        mwra.CP.counts.agg.CIs.li <-
            InternalMakeAggLists(NULL #returns object
                                 ,aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = est.years
                             ,d2.names = counts.names #<--
                              )
        for(agg in aggregates.names.df$display.label) {
            filename.agg <- paste0("mwra_CP_counts_agg_li", "_", aggregates.names.df[aggregates.names.df$display.label == agg, "file.name"], ".RData")
            load(file = file.path(agg.li.dir, filename.agg))
            mwra.CP.counts.agg.CIs.li[[agg]] <- res.aggregate.list
            file.remove(file.path(agg.li.dir, filename.agg))
        }
        res.aggregate.list <- mwra.CP.counts.agg.CIs.li
        rm(mwra.CP.counts.agg.CIs.li)
        save(res.aggregate.list, file = file.path(agg.li.dir, "mwra_CP_counts_agg_li.RData"))

        uwra.CP.counts.agg.CIs.li <-
            InternalMakeAggLists(NULL #returns object
                                 ,aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = est.years
                             ,d2.names = counts.names #<--
                              )
        for(agg in aggregates.names.df$display.label) {
            filename.agg <- paste0("uwra_CP_counts_agg_li", "_", aggregates.names.df[aggregates.names.df$display.label == agg, "file.name"], ".RData")
            load(file = file.path(agg.li.dir, filename.agg))
            uwra.CP.counts.agg.CIs.li[[agg]] <- res.aggregate.list
            file.remove(file.path(agg.li.dir, filename.agg))
        }
        res.aggregate.list <- uwra.CP.counts.agg.CIs.li
        rm(uwra.CP.counts.agg.CIs.li)
        save(res.aggregate.list, file = file.path(agg.li.dir, "uwra_CP_counts_agg_li.RData"))

        ## -------** Ratios

        message("\nMaking all women aggregate ratios")

        ## -------*** Load

        awra.res.aggregate.list <- get(load(file.path(agg.li.dir, "awra_CP_counts_agg_li.RData")))
        mwra.res.aggregate.list <- get(load(file.path(agg.li.dir, "mwra_CP_counts_agg_li.RData")))
        uwra.res.aggregate.list <- get(load(file.path(agg.li.dir, "uwra_CP_counts_agg_li.RData")))
        rm(res.aggregate.list)

        ## -------*** Make Output Lists

        ## Yearly estimates
        awra.CP.ratios.agg.CIs.li <-
            InternalMakeAggLists(NULL #returns object
                                 ,aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = est.years
                             ,d2.names = ratios.names #<--
                              )

        ## Change quantities
        awra.CP.changeratios.agg.CIs.li <-
            InternalMakeAggLists(NULL #returns object
                                ,aggregates = names(aggregates), n.iters = n.iters
                                ,years.names = changes.years.names #<--
                                ,d2.names = ratios.names #<--
                                )

        ## -------*** Create

        for(agg in names(aggregates)) {

            ## Yearly estimates
            awra.CP.ratios.agg.CIs.li[[agg]][["CP"]] <-
                InternalMakeRatios(ratios.names = ratios.names
                                  ,counts.ar = awra.res.aggregate.list[[agg]][["CP"]]
                                  ,tot.counts.mat = matrix(awra.res.aggregate.list[[agg]][["W.Lg.t"]]
                                                          ,nrow = nrow(awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][,1,])
                                                          ,ncol = ncol(awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][,1,])
                                                           )
                                  ,uwra.counts.ar = uwra.res.aggregate.list[[agg]][["CP"]]
                                  ,mwra.counts.ar = mwra.res.aggregate.list[[agg]][["CP"]]
                                   )

            ## Change quantities
            for(i in seq_len(nrow(years.change))) { # "2000-1990", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change[i,1])
                    b <- which(est.years == years.change[i,2])
                    awra.CP.changeratios.agg.CIs.li[[agg]][["CP"]][i, ,k] <-
                        awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][b, ,k] - awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][a, ,k]
                }
            }
            for(i in seq_len(nrow(years.change2))) { # "(2000-1990) - (2010-2000)", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change2[i,1])
                    b <- which(est.years == years.change2[i,2])
                    c <- which(est.years == years.change2[i,3])
                    awra.CP.changeratios.agg.CIs.li[[agg]][["CP"]][nrow(years.change) + i, ,k] <-
                        (awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][b, ,k] - awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][a, ,k]) -
                        (awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][c, ,k] - awra.CP.ratios.agg.CIs.li[[agg]][["CP"]][b, ,k])
                }
            }
        }

        ## -------*** Summarize

        awra.CP.ratios.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.ratios.agg.CIs.li[names(aggregates)]
                  ,function(z) CP.summ.f(z$CP)
                   )
        awra.CP.changeratios.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.changeratios.agg.CIs.li[names(aggregates)]
                  ,function(z) CP.change.summ.f(z$CP)
                   )

        ## -------*** Tidy Up

        rm(mwra.res.aggregate.list)
        rm(uwra.res.aggregate.list)

        ## -------** Probabilities

        message("\nMaking all women aggregate probabilities")

        ## -------*** Make Output Lists

        ## Yearly estimates
        awra.CP.probs.agg.li <-
            InternalMakeAggLists(NULL, aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = est.years
                             ,d2.names = probs.names #<--
                             )

        ## -------*** Create

        for(agg in names(aggregates)) {

            ## Yearly estimates
            awra.CP.probs.agg.li[[agg]][["CP"]] <-
                InternalMakeProbs(probs.names = probs.names
                                 ,counts.ar = awra.res.aggregate.list[[agg]][["CP"]]
                                  )
        }

        ## -------*** Summarize

        awra.CP.probs.agg.li <- #this will ditch the "W.Lg.t" elements in each 'agg'
            lapply(awra.CP.probs.agg.li[names(aggregates)], function(y) {
                array(apply(y$CP, c(1, 2), function(z) mean(z, na.rm = TRUE))
               ,dim = c(1, dim(awra.CP.probs.agg.li[[1]]$CP)[1:2])
               ,dimnames = c("Probability", dimnames(awra.CP.probs.agg.li[[1]]$CP)[1:2])
                 )
            })           # Note that the first dim, of extent 1, will never be
                         # of larger extent because this will always be an array
                         # of probabilities. Therefore, the array will be filled
                         # with dim 2, 'est.years', moving fastest. If
                         # additional indicators are added to dim 3, e.g., 'any
                         # method > 75%', the array should still be filled
                         # correctly.

        ## -------** Proportions

        message("\nMaking all women aggregate proportions")

        ## -------*** Make Output Lists

        ## Yearly estimates
        awra.CP.props.agg.CIs.li <-
            InternalMakeAggLists(NULL, aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = est.years
                             ,d2.names = counts.names)

        ## Change quantities
        awra.CP.changeprops.agg.CIs.li <-
            InternalMakeAggLists(NULL, aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = changes.years.names #<--
                             ,d2.names = counts.names)

        ## -------*** Create

        for(agg in names(aggregates)) {

            ## Yearly estimates
            awra.CP.props.agg.CIs.li[[agg]][["CP"]] <-
                awra.res.aggregate.list[[agg]][["CP"]] / awra.res.aggregate.list[[agg]][["W.Lg.t"]]

            ## Change quantitites
            for(i in seq_len(nrow(years.change))) { # "2000-1990", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change[i,1])
                    b <- which(est.years == years.change[i,2])
                    awra.CP.changeprops.agg.CIs.li[[agg]][["CP"]][i, ,k] <-
                        awra.CP.props.agg.CIs.li[[agg]][["CP"]][b, ,k] - awra.CP.props.agg.CIs.li[[agg]][["CP"]][a, ,k]
                }
            }
            for(i in seq_len(nrow(years.change2))) { # "(2000-1990) - (2010-2000)", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change2[i,1])
                    b <- which(est.years == years.change2[i,2])
                    c <- which(est.years == years.change2[i,3])
                    awra.CP.changeprops.agg.CIs.li[[agg]][["CP"]][nrow(years.change) + i, ,k] <-
                        (awra.CP.props.agg.CIs.li[[agg]][["CP"]][b, ,k] - awra.CP.props.agg.CIs.li[[agg]][["CP"]][a, ,k]) -
                        (awra.CP.props.agg.CIs.li[[agg]][["CP"]][c, ,k] - awra.CP.props.agg.CIs.li[[agg]][["CP"]][b, ,k])
                }
            }
        }

        ## -------*** Summarize

        awra.CP.props.agg.CIs.li <- #this will ditch the "W.Lg.t" elements in each 'agg'
            lapply(awra.CP.props.agg.CIs.li[names(aggregates)]
                  ,function(z) CP.summ.f(z$CP)
                   )
        awra.CP.changeprops.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.changeprops.agg.CIs.li[names(aggregates)]
                  ,function(z) CP.change.summ.f(z$CP)
                   )

        message("\n... finishing")

        ## -------** Counts

        ## -------*** Make Output Lists

        ## Change quantities
        awra.CP.changecounts.agg.CIs.li <-
            InternalMakeAggLists(aggregates = names(aggregates), n.iters = n.iters
                             ,years.names = changes.years.names #<--
                             ,d2.names = counts.names)

        ## -------*** Compute

        for(agg in names(aggregates)) {

            ## Change quantities
            for(i in seq_len(nrow(years.change))) { # "2000-1990", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change[i,1])
                    b <- which(est.years == years.change[i,2])
                    awra.CP.changecounts.agg.CIs.li[[agg]][["CP"]][i, ,k] <-
                        awra.res.aggregate.list[[agg]][["CP"]][b, ,k] -
                        awra.res.aggregate.list[[agg]][["CP"]][a, ,k]
                }
            }
            for(i in seq_len(nrow(years.change2))) { # "(2000-1990) - (2010-2000)", etc.
                for(k in seq_len(n.iters)) {
                    a <- which(est.years == years.change2[i,1])
                    b <- which(est.years == years.change2[i,2])
                    c <- which(est.years == years.change2[i,3])
                    awra.CP.changecounts.agg.CIs.li[[agg]][["CP"]][nrow(years.change) + i, ,k] <-
                        (awra.res.aggregate.list[[agg]][["CP"]][b, ,k] -
                         awra.res.aggregate.list[[agg]][["CP"]][a, ,k]) -
                        (awra.res.aggregate.list[[agg]][["CP"]][c, ,k] -
                         awra.res.aggregate.list[[agg]][["CP"]][b, ,k])
                }
            }
        }

        ## -------*** Summarize

        ## Load aggregate counts
        awra.CP.counts.agg.CIs.li <- awra.res.aggregate.list
        rm(awra.res.aggregate.list)

        ## Put denominator counts outside the aggregates in prep for export
        W.Lg.t <-
            lapply(awra.CP.counts.agg.CIs.li, function(z) { z[["W.Lg.t"]] })

        ## CIs
        awra.CP.counts.agg.CIs.li <-
            lapply(awra.CP.counts.agg.CIs.li[names(aggregates)]
                  ,function(z) CP.summ.f(z$CP)
                   )
        awra.CP.changecounts.agg.CIs.li <-
            lapply(awra.CP.changecounts.agg.CIs.li[names(aggregates)]
                  ,function(z) CP.change.summ.f(z$CP)
                   )

        ## -------** Prepare Outputs

        ## Make output lists look like 'CIprop.Lg.Lcat.qt', etc.

        awra.CP.counts.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.counts.agg.CIs.li[names(aggregates)], "awra.outputs.f")
        awra.CP.props.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.props.agg.CIs.li[names(aggregates)], "awra.outputs.f")
        awra.CP.ratios.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.ratios.agg.CIs.li[names(aggregates)], "awra.outputs.f")
        awra.CP.probs.agg.li[names(aggregates)] <-
            lapply(awra.CP.probs.agg.li[names(aggregates)], "awra.probs.outputs.f")

        awra.CP.changecounts.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.changecounts.agg.CIs.li[names(aggregates)], "awra.change.outputs.f", transp = TRUE)
        awra.CP.changeprops.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.changeprops.agg.CIs.li[names(aggregates)], "awra.change.outputs.f", transp = TRUE)
        awra.CP.changeratios.agg.CIs.li[names(aggregates)] <-
            lapply(awra.CP.changeratios.agg.CIs.li[names(aggregates)], "awra.change.outputs.f", transp = TRUE)

        ## -------* Return

        res.aggregate.all.women <-
            list(CIcount.Lg.Lcat.qt = awra.CP.counts.agg.CIs.li[names(aggregates)]
                   ,CIprop.Lg.Lcat.qt = awra.CP.props.agg.CIs.li[names(aggregates)]
                   ,CIratio.Lg.Lcat.qt = awra.CP.ratios.agg.CIs.li[names(aggregates)]
                    ,metDemGT.Lg.Lcat.pr = awra.CP.probs.agg.li[names(aggregates)]
                   ,changecount.Lg.Lcat.Ti = awra.CP.changecounts.agg.CIs.li[names(aggregates)]
                   ,changeprop.Lg.Lcat.Ti = awra.CP.changeprops.agg.CIs.li[names(aggregates)]
                   ,changeratio.Lg.Lcat.Ti = awra.CP.changeratios.agg.CIs.li[names(aggregates)]
                ,W.Lg.t = W.Lg.t)

        return(res.aggregate.all.women)
    }
##----------------------------------------------------------------------
##' Calculate posterior quantiles of proportion of users in a subset age group for aggregates.
##'
##' Creates posterior samples for, e.g., proportion of all users that
##' are aged 15--19, and calculates quantiles.
##'
##' @note This function requires an all women run to have been done.
##'
##' @param age.subset.uwra.output.dir Output directory for the subset age group (e.g., 15--19) for unmarried women.
##' @param age.subset.mwra.output.dir Output directory for the subset age group (e.g., 15--19) for married women.
##' @param age.subset.awra.output.dir Output directory for the subset age group (e.g., 15--19) for all women. This is required.
##' @param age.total.uwra.output.dir Output directory for the total (15--49) age group for unmarried women.
##' @param age.total.mwra.output.dir Output directory for the total (15--49) age group for married women.
##' @param age.total.awra.output.dir Output directory for the total (15--49) age group for all women. This is required.
##' @param run.name
##' @param age.subset.WRA.csv Denominator counts.
##' @param age.total.WRA.csv Denominator counts.
##' @param est.years
##' @param years.change
##' @return Saves quantiles in the respective directories.
##' @author Mark Wheldon.
##' @noRd
GetAggregatesAgeRatios <-
    function(age.subset.uwra.output.dir = NULL,
             age.subset.mwra.output.dir = NULL,
             age.subset.awra.output.dir = NULL,
             age.total.uwra.output.dir = NULL,
             age.total.mwra.output.dir = NULL,
             age.total.awra.output.dir = NULL,
             run.name = "test",
             file.aggregates = NULL,
             age.subset.WRA.csv = NULL,
             age.total.WRA.csv = NULL,
             est.years = NULL,
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
            ,verbose = TRUE
            ,output_exists_messages = TRUE
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

        ## -------* Constants

        ## Don't change these without checking which subfunctions
        ## depend on them (e.g., 'InternalMakeRatios()') AND what they
        ## are called in 'GetAggregatesAllWomen().

        counts.names <-
            c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet"
             ,"TradPlusUnmet")

        if(is.null(file.aggregates)) {
            agg.rda.filename <- "res.aggregate.age.ratio.rda"
        } else {
            if(!file.exists(file.aggregates)) stop("'", file.aggregates, "' does not exist.")
            special.aggregate.name <-
                strsplit(basename(file.aggregates), "\\.")[[1]][1]
            agg.rda.filename <- paste0(special.aggregate.name, ".age.ratio.rda")
        }

        ## -------* LOOP over uwra, mwra, awra

        for(marr in c("uwra", "mwra", "awra")) {

            if(verbose) message("\nMaking aggregate age ratios for ", toupper(marr))

            ## -------** Directories

            age.subset.output.dir <- get(paste0("age.subset.", marr, ".output.dir"))
            age.total.output.dir <- get(paste0("age.total.", marr, ".output.dir"))

            ## Make sure results do not already exist
            if(file.exists(file.path(age.subset.output.dir, agg.rda.filename))) {
                if(output_exists_messages) message("'", age.subset.output.dir, "' already exists for ", toupper(marr), ". It will NOT be re-created.")
                next()
            }

            ## Aggregate trajectories are always in the AWRA directory
            age.subset.aggtraj.dir <-
                file.path(age.subset.awra.output.dir, makeSpecAggSubdir(file.aggregates))
            age.total.aggtraj.dir <-
                file.path(age.total.awra.output.dir, makeSpecAggSubdir(file.aggregates))

            ## -------** MCMC Meta

            load(file.path(age.subset.output.dir, "mcmc.meta.rda"))
            age.subset.mcmc.meta <- mcmc.meta

            load(file.path(age.total.output.dir, "mcmc.meta.rda"))
            age.total.mcmc.meta <- mcmc.meta

            age.subset.winbugs.data <- age.subset.mcmc.meta$winbugs.data
            age.total.winbugs.data <- age.total.mcmc.meta$winbugs.data

            age.subset.region.info <- age.subset.mcmc.meta$data.raw$region.info

        ## -------** Region info

        if(!is.null(age.subset.mcmc.meta$data.raw$region.info.no.data)) {
            age.subset.region.info <- age.subset.mcmc.meta$data.raw$region.info.no.data
        } else age.subset.region.info <- age.subset.mcmc.meta$data.raw$region.info

            ## -------** Aggregates

            if(is.null(file.aggregates)) {
                aggregates <- c(get_aggregate_names(family = "UNPD"),
                                age.subset.region.info$name.subreg,
                                age.subset.region.info$name.reg,
                                "World")
            } else {

                aggregates <-
                    unique(read.csv(file.aggregates, stringsAsFactors = FALSE)$groupname)
            }

            if(is.null(names(aggregates))) names(aggregates) <- aggregates
            else names(aggregates)[names(aggregates) == ""] <- aggregates[names(aggregates) == ""]

            ## Remove duplicates (e.g., Northern America)
            reg.dup <-
                age.subset.region.info$name.reg %in% age.subset.region.info$name.subreg
            reg.dup <- age.subset.region.info$name.reg[reg.dup]
            for(i in seq_along(reg.dup)) {
                if(verbose) message("NOTE: ", reg.dup[i], " is also a subregion and will not be stored or plotted separately.")
            }
            aggregates <- aggregates[!duplicated(aggregates)]

            aggregates.names.df <-
            data.frame(agg.name = aggregates,
                       file.name = makeFileName(abbreviate(names(aggregates), minlength = 12)),
                     stringsAsFactors = FALSE,
                     display.label = names(aggregates),
                     row.names = NULL)

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

            ## -------** LOAD the aggregates

            age.subset.res.aggregate.list <-
                get(load(file.path(age.subset.aggtraj.dir,
                                   paste0(marr, "_CP_counts_agg_li.RData"))))

            age.total.res.aggregate.list <-
                get(load(file.path(age.total.aggtraj.dir,
                                   paste0(marr, "_CP_counts_agg_li.RData"))))

            rm(res.aggregate.list) # remove the duplicate obj

            ## -------** Check saved aggregate count trajectories

            ## Get number of iterations: Which is the smaller of mcmc arrays
            n.iters <- min(dim(age.subset.res.aggregate.list[[1]]$CP)[3],
                           dim(age.total.res.aggregate.list[[1]]$CP)[3])

            ## -------*** Estimation years

            ## -------**** Single years

            age.subset.years <- dimnames(age.subset.res.aggregate.list[[1]]$CP)[[1]]
            age.total.years <- dimnames(age.total.res.aggregate.list[[1]]$CP)[[1]]

            if(is.null(est.years)) est.years <- age.subset.years

            ## Check that estimation years all match
            if(!isTRUE(all.equal(age.subset.years, age.total.years, est.years))) {
                est.years <- intersect(intersect(age.subset.years, age.total.years), est.years)
                warning("The estimation years for age subset and age total (15--49) do not match, or one or both do not match 'est.years'. Using the intersection (age subset <and> age total (15--49) <and> 'est.years'):\n\t", paste0(est.years, collapse = ", "))
            }

            ## Make est.years numeric
            est.years <- as.numeric(est.years)

            ## -------**** Changes years

            if(!(all(c(years.change, years.change2) %in% est.years)))
                stop("Estimates/projections are not available for all 'years.change' and 'years.change2'.")

            ## (from 'GetInfoChange()')
            changes.years.names <- c(apply(years.change, 1, function(years) paste0(floor(years[1]), "-", floor(years[2]))),
                                     apply(years.change2, 1, function(years)
                                         paste0("Change (", floor(years[3]), "-", floor(years[2]), ") - (",
                                                floor(years[2]), "-", floor(years[1]), ")")))

            ## -------*** Aggregates saved

            age.subset.agg.saved <- names(age.subset.res.aggregate.list)
            age.subset.agg.saved <- na.omit(age.subset.agg.saved)

            age.total.agg.saved <- names(age.total.res.aggregate.list)
            age.total.agg.saved <- na.omit(age.total.agg.saved)

            agg.both.agg.name <- intersect(age.subset.agg.saved, age.total.agg.saved)

            aggregates.names.df <-
                aggregates.names.df[aggregates.names.df$display.label %in% agg.both.agg.name,]

            ## -------*** CHECK

            if(identical(as.double(length(agg.both.agg.name)), 0)) {
                stop("The age subset and age total (15--49) runs have no saved aggregates trajectories in common.")
            }

            ## -------** AGGREGATES

            ## -------*** Make Output Lists

            res.aggregate.age.ratio.list <-
                InternalMakeAggLists(aggregates = agg.both.agg.name,
                                     n.iters = n.iters,
                                     years.names = age.subset.years,
                                     d2.names = counts.names)

            ## These will eventually just have the CIs
            age.ratio.CP.ratios.CIs.li <-
                lapply(agg.both.agg.name,
                       function(z) array(dim = c(5, length(est.years), length(counts.names)),
                                         dimnames = list(NULL, est.years, counts.names)))
            names(age.ratio.CP.ratios.CIs.li) <- aggregates.names.df$display.label

            age.ratio.CP.changeratios.CIs.li <-
                lapply(agg.both.agg.name, function(z) array(dim = c(6, length(changes.years.names)
                                               , length(counts.names)),
                                         dimnames = list(NULL, changes.years.names, counts.names)))
            names(age.ratio.CP.changeratios.CIs.li) <- aggregates.names.df$display.label

            ## -------*** Loop over aggregates

            for(x in agg.both.agg.name) {

                ## -------**** Load aggregates

                age.subset.CP.counts.j <-
                    age.subset.res.aggregate.list[[x]]$CP[as.character(est.years), counts.names, 1:n.iters]

                age.total.CP.counts.j <-
                    age.total.res.aggregate.list[[x]]$CP[as.character(est.years), counts.names, 1:n.iters]

                ## -------**** Calculate Age Ratios

                age.ratio.CP.ratios.j <-
                    age.subset.CP.counts.j / age.total.CP.counts.j

                ## -------**** Change quantities (counts, props, ratios)

                ## Make array for change in age count ratios
                age.ratio.CP.change.countratios.j <-
                    array(0, dim = c(length(changes.years.names), length(counts.names)
                                    ,n.iters
                                     ))
                dimnames(age.ratio.CP.change.countratios.j) <-
                    list(changes.years.names, counts.names)

                ## Calculate
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

                ## -------**** Save aggregate trajectories

                res.aggregate.age.ratio.list[[x]] <- age.ratio.CP.ratios.j

                ## -------**** Summarize ratios

                ## Estimation years
                age.ratio.CP.ratios.CIs.li[[aggregates.names.df[aggregates.names.df$display.label == x, "display.label"]]] <-
                    apply(age.ratio.CP.ratios.j, c(1, 2), "CP.summ.f")

                ## Change quantities
                age.ratio.CP.changeratios.CIs.li[[aggregates.names.df[aggregates.names.df$display.label == x, "display.label"]]] <-
                    apply(age.ratio.CP.change.countratios.j, c(1, 2), "CP.change.summ.f")

            } ## END aggregate loop

            ## -------*** Save the Full List of Aggregates

            save(res.aggregate.age.ratio.list,
                 file = file.path(age.subset.aggtraj.dir,
                                   paste0(marr, "_CP_counts_age_ratios_agg_li.RData")))

            ## -------*** Prepare Outputs

            ## Make output lists look like 'CIprop.Lg.Lcat.qt', etc.
            age.ratio.CP.ratios.CIs.li <-
                lapply(age.ratio.CP.ratios.CIs.li, "age.ratio.outputs.f")

            age.ratio.CP.changeratios.CIs.li <-
                lapply(age.ratio.CP.changeratios.CIs.li, "age.ratio.change.outputs.f",
                       transp = TRUE)

            ## Output
            res.aggregate.age.ratio <- list(CIcountratio.Lg.Lcat.qt = age.ratio.CP.ratios.CIs.li
                                           ,changecountratio.Lg.Lcat.Ti = age.ratio.CP.changeratios.CIs.li
                                            )

            save(res.aggregate.age.ratio, file = file.path(age.subset.output.dir, agg.rda.filename))
            message("\nPosterior quantiles of aggregate age ratios for ", toupper(marr), " saved to ", age.subset.output.dir)

        }
    }
#-------------------------------------------------------------------------------
##' Create lists for use in allWomenCIs().
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param filename
##' @param output.dir
##' @param aggregates
##' @param n.iters
##' @param years.names
##' @param d2.names Character vector with names of indicators to produce for
##'     counts. E.g., 'c("Total", "Modern", "Traditional", "Unmet",
##'     "TotalPlusUnmet", "TradPlusUnmet")' or 'c("Met_Demand", "Met_Dem_Mod_Meth", "ModernOverTotal",
##'     "Z", "UnmarriedOverAll", "MarriedOverAll")'.
##' @return
##' @author Mark Wheldon
##' @noRd
InternalMakeAggLists <- function(filename = NULL, output.dir = NULL, aggregates, n.iters,
                              years.names, d2.names) {

    res.aggregate.list <- list()
    length(res.aggregate.list) <- length(aggregates)
    names(res.aggregate.list) <- aggregates

    res.aggregate.list_dim_2 <- length(d2.names)

    ## Fill with arrays of zeros to be cumulated

    res.aggregate.list <- lapply(res.aggregate.list ,function(z) {
        ## Numerators
        list(CP = array(0
             ,dim =  c(length(years.names)
                      ,res.aggregate.list_dim_2
                      ,n.iters
                       )
             ,dimnames = list(years.names
                             ,d2.names
                              )
               )
             ## Denominators
             ,W.Lg.t = rep(0, length(years.names))
             )
    }
    )

    ## -------* Return

    if(is.null(filename)) { return(res.aggregate.list)
    } else {

    save(res.aggregate.list
        ,file = file.path(output.dir, filename)
         )

        return(invisible(filename))
        }
}

#-------------------------------------------------------------------------------
##' Create individual aggregate lists for use in allWomenCIs().
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param filename
##' @param output.dir
##' @param n.iters
##' @param years.names
##' @param d2.names Character vector with names of indicators to produce for
##'     counts. E.g., 'c("Total", "Modern", "Traditional", "Unmet",
##'     "TotalPlusUnmet", "TradPlusUnmet")' or 'c("Met_Demand", "Met_Dem_Mod_Meth", "ModernOverTotal",
##'     "Z", "UnmarriedOverAll", "MarriedOverAll")'.
##' @param aggregates.names.df
##' @param compress.RData
##' @return
##' @author Mark Wheldon
##' @noRd
InternalMakeIndividualAggLists <- function(filename = NULL, output.dir = NULL
                               , n.iters,
                              years.names, d2.names
                             ,aggregates.names.df = NULL
                              ,compress.RData = FALSE) {

    ## res.aggregate.list <- list()
    ## length(res.aggregate.list) <- length(aggregates)
    ## names(res.aggregate.list) <- aggregates

    res.aggregate.list_dim_2 <- length(d2.names)

    for(i in 1:nrow(aggregates.names.df)) {

        ## Arrays of zeros to be cumulated
        res.aggregate.list <-
            ## Numerators
            list(CP = array(0
                           ,dim =  c(length(years.names)
                                    ,res.aggregate.list_dim_2
                                    ,n.iters
                                     )
                           ,dimnames = list(years.names
                                           ,d2.names
                                            )
                            )
                 ## Denominators
                ,W.Lg.t = rep(0, length(years.names))
                 )

        if(is.null(filename)) {
            return(res.aggregate.list)
        } else {
            filename.agg <- paste0(filename, "_", aggregates.names.df[i,"file.name"], ".RData")
            save(res.aggregate.list
                ,file = file.path(output.dir, filename.agg)
                 ,compress = compress.RData
                 )
        }
    }
    }
#-------------------------------------------------------------------------------
##' HIDDEN. Create counts for aggregates.
##'
##' This is inside the country loop so you will have to pass in the country ISO
##' code and decide whether to add Trad, Mod, Unmet counts to the running total
##' for each aggregate.
##'
##' @param iso.both.j The ISO code of the country at this point in the loop
##' @param agg Names of aggregates to process
##' @param CP.counts.agg
##' @param CP.counts.agg.W.Lg.t
##' @param CP.counts.j
##' @param uwra.denom.counts Array of counts for unmarried women
##' @param uwra.mcmc.meta
##' @param mwra.denom.counts Array of counts for married women
##' @param mwra.mcmc.meta
##' @param verbose
##' @return
##' @author
##' @noRd
InternalAllWomenAggregateCounts <-
    function(filename, output.dir, file.aggregates = NULL
             ,iso.both.j, CP.counts.j, uwra.denom.counts,
             uwra.country.info, uwra.region.info, mwra.denom.counts,
             verbose = FALSE, aggregates.from.file.df = NULL, aggregates.names.df
             ,reg.dup, compress.RData = FALSE) {

        ## -------* Main Body

        agg.names <- as.character(aggregates.names.df$agg.name)

        if(is.null(aggregates.from.file.df)) {

            ## -------** UNPD Aggregates

            unpd_names_i <-
                which(aggregates.names.df$agg.name %in%
                      get_aggregate_names(family = "UNPD"))

            for(i in unpd_names_i) {

                agg <- aggregates.names.df[i, "agg.name"]
                agg.label <- aggregates.names.df[i, "display.label"]

                if(iso.both.j %in% get_aggregate_ISOs(name = agg, family = "UNPD")) {

                    if(verbose) message("Adding to '", agg.label, "'.")

                    load(file = file.path(output.dir,
                                          paste0(filename, "_",
                                                 as.character(aggregates.names.df[i, "file.name"]),
                                                 ".RData")))
                    res.aggregate.list$CP <-
                        res.aggregate.list$CP + CP.counts.j

                    if(!is.null(uwra.denom.counts)) {
                        res.aggregate.list$W.Lg.t <-
                            res.aggregate.list$W.Lg.t + uwra.denom.counts
                    }
                    if(!is.null(mwra.denom.counts)) {
                        res.aggregate.list$W.Lg.t <-
                            res.aggregate.list$W.Lg.t + mwra.denom.counts
                    }
                    save(res.aggregate.list, compress = compress.RData
                        ,file = file.path(output.dir,
                                          paste0(filename, "_"
                                                ,as.character(aggregates.names.df[i, "file.name"]),
                                                 ".RData")))
                }
            }

            ## -------** Aggregates Needing 'country.info'

            if(!is.null(uwra.country.info) && !is.null(uwra.region.info)) {

                ## -------*** Sub-regions

            subreg_names_i <-
                which(aggregates.names.df$agg.name %in%
                      uwra.region.info$name.subreg)

                for(i in subreg_names_i) {

                    agg <- aggregates.names.df[i, "agg.name"]
                    agg.label <- aggregates.names.df[i, "display.label"]

                    uwra.isos.in.this.subreg <-
                        uwra.country.info$iso.c[uwra.country.info$namesubreg.c == agg.label]

                    ## Need to make sure India is added if it was modelled as its own subregion.
                    if(agg.label == "South-Central Asia" && !(356 %in% uwra.isos.in.this.subreg)) {
                        uwra.isos.in.this.subreg <- c(uwra.isos.in.this.subreg, "356")
                    }

                    if(iso.both.j %in% uwra.isos.in.this.subreg) {

                        if(verbose) message("Adding to ", sQuote(agg.label))

                        load(file = file.path(output.dir, paste0(filename, "_", as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData")))

                        res.aggregate.list$CP <-
                            res.aggregate.list$CP + CP.counts.j

                        if(!is.null(uwra.denom.counts)) {
                            res.aggregate.list$W.Lg.t <-
                                res.aggregate.list$W.Lg.t + uwra.denom.counts
                        }
                        if(!is.null(mwra.denom.counts)) {
                            res.aggregate.list$W.Lg.t <-
                                res.aggregate.list$W.Lg.t + mwra.denom.counts
                        }
                        save(res.aggregate.list, compress = compress.RData
                            ,file = file.path(output.dir, paste0(filename, "_"
                                                                ,as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData"
                                                                 )))
                    }
                }

                ## -------*** Regions

                reg_names_i <-
                    which(aggregates.names.df$agg.name %in% uwra.region.info$name.reg &
                          (!(aggregates.names.df$agg.name %in% uwra.region.info$name.subreg)))

                for(i in reg_names_i) {

                    agg <- aggregates.names.df[i, "agg.name"]
                    agg.label <- aggregates.names.df[i, "display.label"]

                    uwra.isos.in.this.reg <-
                        uwra.country.info$iso.c[uwra.country.info$namereg.c == agg]

                    if(iso.both.j %in% uwra.isos.in.this.reg) {

                        if(verbose) message("Adding to ", sQuote(agg))

                        load(file = file.path(output.dir, paste0(filename, "_", as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData")))

                        res.aggregate.list$CP <-
                            res.aggregate.list$CP + CP.counts.j

                        if(!is.null(uwra.denom.counts)) {
                            res.aggregate.list$W.Lg.t <-
                                res.aggregate.list$W.Lg.t + uwra.denom.counts
                        }
                        if(!is.null(mwra.denom.counts)) {
                            res.aggregate.list$W.Lg.t <-
                                res.aggregate.list$W.Lg.t + mwra.denom.counts
                        }
                        save(res.aggregate.list, compress = compress.RData
                            ,file = file.path(output.dir, paste0(filename, "_"
                                                                ,as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData"
                                                                 )))
                    }
                }
            }

            ## -------*** World

            agg <- "World"
            agg.label <- "World"

                if(verbose) message("Adding to 'World'")

                load(file = file.path(output.dir, paste0(filename, "_", as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData")))

                res.aggregate.list$CP <-
                    res.aggregate.list$CP +  CP.counts.j

                if(!is.null(uwra.denom.counts)) {
                    res.aggregate.list$W.Lg.t <-
                        res.aggregate.list$W.Lg.t + uwra.denom.counts
                }
                if(!is.null(mwra.denom.counts)) {
                    res.aggregate.list$W.Lg.t <-
                        res.aggregate.list$W.Lg.t + mwra.denom.counts
                }
                save(res.aggregate.list, compress = compress.RData
                    ,file = file.path(output.dir, paste0(filename, "_"
                                                        ,as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData"
                                                         )))

        } else {

            ## -------** Aggregates from file

            for(agg in agg.names) {

                if(iso.both.j %in% aggregates.from.file.df[aggregates.from.file.df$groupname == agg,]$iso) {

                    if(verbose) message("Adding to '", agg, "'")

                    load(file = file.path(output.dir, paste0(filename, "_", as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData")))

                    res.aggregate.list$CP <-
                        res.aggregate.list$CP + CP.counts.j

                    if(!is.null(uwra.denom.counts)) {
                        res.aggregate.list$W.Lg.t <-
                            res.aggregate.list$W.Lg.t + uwra.denom.counts
                    }
                    if(!is.null(mwra.denom.counts)) {
                        res.aggregate.list$W.Lg.t <-
                            res.aggregate.list$W.Lg.t + mwra.denom.counts
                    }
                    save(res.aggregate.list, compress = compress.RData
                        ,file = file.path(output.dir, paste0(filename, "_"
                                                            ,as.character(aggregates.names.df[aggregates.names.df$agg.name == agg, "file.name"]), ".RData"
                                                             )))
                }
            }
        }

        ## -------* Finish and Return

        return(invisible(filename))
    }

##------------------------------------------------------------------------------
##' Make special aggregates.
##' Convenience function for making indicators for special country aggregates and (optionally) adjust for demographic consistency.
##'
##' .. content for \details{} ..
##' @param file.agg File containing definition of extra aggregate in \code{.csv} format.
##' @param name.agg  Name of the extra aggregate.
##' @param output.dir Ouptut directory.
##' @param run.name Run name.
##' @param table.dir.orig Directory in which to store unadjusted tables.
##' @param do.adjustments Logical. Do adjustments for demographic consistency?
##' @param res.country.adj R object. Output of \code{\link{AdjustMedians}}.
##' @param table.dir.adj Directory in which to store adjusted tables.
##' @param adj.method Character. One of c("modelp", "topdown", "bottomup"). Default is "modelp".
##' @return Nothing. Called for side effects, namely saving tables as \code{.csv} files.
##' @author Mark Wheldon
##' @noRd
make.aggregates <- function(file.agg, name.agg, output.dir,
                            run.name, table.dir.orig = NULL,
                            do.adjustments = TRUE,
                            res.country.adj,
                            table.dir.adj = NULL,
                            adj.method = "modelp",
                            verbose = TRUE
                            ) {
    message("\nMaking aggregates for ", name.agg, " from ", file.agg, ".")

    res.new <- GetAggregates(run.name = run.name,
                             file.aggregates = file.agg,
                             verbose = verbose)
    res.fname <- file.path(output.dir, paste0(name.agg, ".rda"))

    save(res.new, file = res.fname)

    ## Tables
    if(is.null(table.dir.orig)) table.dir.orig <- file.path(output.dir, "table", "orig")

    GetTablesRes(run.name = run.name, res = res.new, name.res = name.agg
                ,table.dir = table.dir.orig)
    GetTablesChange(run.name = run.name, res = res.new, name.res = name.agg
                   ,table.dir = table.dir.orig
                   ,change.in.changes = FALSE)


    if(do.adjustments) {

        ## Adjustments
        if(is.null(table.dir.adj)) table.dir.adj <- file.path(output.dir, "table", "adj")
        res.country.adj.agg <-
            AggregateMedians(res.country.adj
                            ,output.dir = output.dir
                            ,file.aggregates = file.agg
                             )
        GetTablesRes(run.name = run.name, name.res = name.agg
                    ,table.dir = table.dir.adj
                    ,res = res.country.adj.agg #aggregate
                    ,adjusted.medians = TRUE
                    ,adj.method = adj.method
                     )

        ## Compare
        compare.res.agg <-
            CompareAdjMedians(output.dir = output.dir
                             ,res.adj = res.country.adj.agg
                             ,name.res = name.agg
                             ,plot = TRUE
                             ,tabulate = TRUE
                             ,adj.method = adj.method
                             ,return.res = TRUE)
    }
}


make.aggregatesAllWomen <- function(file.agg, name.agg,
                                    uwra.output.dir,
                                    mwra.output.dir,
                                    WRA.csv,
                                    run.name, table.dir.orig = NULL,
                                    do.adjustments = TRUE,
                                    res.country.adj,
                                    table.dir.adj = NULL,
                                    adj.method = "modelp") {
    message("\nMaking aggregates for ", name.agg, " from ", file.agg, ".")

    res.new <- GetAggregatesAllWomen(run.name = run.name,
                                     uwra.output.dir = uwra.output.dir, ##<< Directory where MCMC array and meta are stored,
                                     mwra.output.dir = mwra.output.dir,
                                     WRA.csv = WRA.csv,
                                     file.aggregates = file.agg,
                             verbose = verbose)

    res.fname <- file.path(output.dir, paste0(name.agg, ".all.women.rda"))

    save(res.new, file = res.fname)

    ## Tables
    if(is.null(table.dir.orig)) table.dir.orig <- file.path(output.dir, "table", "orig")
    GetTablesResAllWomen(run.name = run.name, res = res.new, name.res = name.agg
                        ,table.dir = table.dir.orig)
    GetTablesChangeAllWomen(run.name = run.name, res = res.new, name.res = name.agg
                           ,table.dir = table.dir.orig)

    if(do.adjustments) {

        ## Adjustments
        if(is.null(table.dir.adj)) table.dir.adj <- file.path(output.dir, "table", "adj")
        res.country.adj.agg <-
            AggregateMedians(res.country.adj
                            ,output.dir = uwra.output.dir
                            ,file.aggregates = file.agg
                            ,all.women = TRUE
                             )
        GetTablesResAllWomen(run.name = run.name, name.res = name.agg
                            ,table.dir = table.dir.adj
                            ,res = res.country.adj.agg #aggregate
                            ,adjusted.medians = TRUE
                            ,adj.method = adj.method
                             )

        ## Compare
        compare.res.agg <-
            CompareAdjMedians(output.dir = uwra.output.dir
                             ,res.adj = res.country.adj.agg
                             ,name.res = name.agg
                             ,plot = TRUE
                             ,tabulate = TRUE
                             ,adj.method = adj.method
                             ,return.res = TRUE)
    }
}

##' Make country aggregates list
##'
##' Replaces hard-coded 'loop' over aggregates in \code{\link{GetAggregates}}.
##'
##' @param family Aggregate family.
##' @return List of aggregates, named according to aggregate display labels.
##' @seealso \code{\link{get_aggregate_names}}
##' @author Mark Wheldon
##' @noRd
make_country_aggregates_internal <-
    function(family = "UNPD", C,
             iso.Ptp3s.key.df,
             output.dir.countrytrajectories,
             years.change, years.change2,
             W.Lc.t, verbose = TRUE) {

        out_list <- list()
        agg_fam_names <- get_aggregate_names(family = family)

        for(i in seq_along(agg_fam_names)) {
            agg_name <- agg_fam_names[i]
            agg_isos <- get_aggregate_ISOs(name = agg_name, family = family)
            select.c <- seq(1:C)[iso.Ptp3s.key.df$iso.c %in% agg_isos]
            if(verbose) message("Constructing aggregates for the ", length(select.c), " ", agg_name)
            if(length(select.c) == 0) {
                warning("Skipping ", agg_name, " ; no countries with data.")
            } else {                #ONLY proceed if there are some with data.
                if(any(is.na(select.c))) {
                    warning("Some ", agg_name, " are not counted as they have no data.")
                    select.c <- select.c[!is.na(select.c)]
                }
                nameg <- names(get_aggregate_names(family = family)[i])
                out_list[[nameg]] <-
                    InternalGetAggregates(W.Lc.t = W.Lc.t # Elements are matched using 'country.info' and select.c.
                                        , select.c = select.c,
                                          dir.traj = output.dir.countrytrajectories,
                                          years.change = years.change,
                                          years.change2 = years.change2
                                         ,iso.Ptp3s.key.df = iso.Ptp3s.key.df, verbose = verbose)
            }
        }
        out_list
    }



##' Get names of aggregates within aggregate families
##'
##' Returns the names of all aggregates within a specified
##' family. Only UNPD is available. For World Bank aggregates, use the
##' 'special aggregates' interface.
##'
##' @note \emph{Internal} aggregate names are in lower case, except proper
##'     nouns. Punctuation is omitted unless part of a proper
##'     name. Similarly, abbreviations are not used unless part of a
##'     proper name (e.g., 'excluding' not 'excl.' but 'FP 2020' not
##'     'fp 2020').
##'
##' @param family Aggregate family
##' @return Named character vector, the elements of which contain the
##'     \emph{internal names} of aggregates, the names of which give
##'     the \emph{display labels}.
##' @author
##' @noRd
get_aggregate_names <-
    function(family = "UNPD") {

    family <- match.arg(family)

    if(family == "UNPD") {

            c(`Developed countries` = "developed countries",
          `Developing countries` = "developing countries",
          `Developing (excl. China)` = "developing countries excluding China",
          `Mela-Micro-Polynesia` = "Mela-Micro-Polynesia",
          `FP2020 69 Countries` = "FP 2020 countries",
          `More developed countries` = "more developed countries",
          `Less developed countries` = "less developed countries",
          `Least developed countries` = "least developed countries",
          `Other developing countries` = "other developing countries",
          `Less developed countries, excluding China` = "less developed countries excluding China",
          `Sub-Saharan Africa` = "sub-Saharan Africa")

    } else {
        stop("'family' '", family, "' is not a valid selection.")
    }
}


##' Get ISO codes of a country aggregate
##'
##' Get ISO codes of countries in a given aggregate.
##'
##' @param name Name of aggregate.
##' @param family Aggregate family.
##' @param file Not yet used.
##' @return \emph{Character} vector of ISO codes.
##' @author Mark Wheldon
##' @noRd
get_aggregate_ISOs <-
    function(name,
             family = c("UNPD"),
             file = NULL) {

        family = match.arg(family)

        fam_names <- get_aggregate_names(family = family)
        if(!(name %in% fam_names)) {
            stop("'name' must be one of ", paste(fam_names, collapse = ", "))
        }

        if(family == "UNPD") {

            if(name == "developed countries") {

                out <-
                    c("8", "20", "36", "40", "56", "60", "70", "100", "112", "124",
                      "162", "166", "191", "196", "203", "208", "233", "234", "246",
                      "248", "250", "276", "292", "300", "304", "334", "336", "348",
                      "352", "372", "376", "380", "392", "428", "438", "440", "442",
                      "470", "492", "498", "499", "528", "554", "574", "578", "616",
                      "620", "642", "643", "666", "674", "680", "688", "703", "705",
                      "724", "744", "752", "756", "804", "807", "826", "831", "832", "833", "840")

            } else if(name == "developing countries") {

                out <-
                    c("4", "12", "16", "24", "28", "31", "32", "44", "48", "50", "51",
                      "52", "64", "68", "72", "74", "76", "84", "86", "90", "92", "96",
                      "104", "108", "116", "120", "132", "136", "140", "144", "148",
                      "152", "156", "170", "174", "175", "178", "180", "184", "188",
                      "192", "204", "212", "214", "218", "222", "226", "231", "232",
                      "238", "239", "242", "254", "258", "260", "262", "266", "268",
                      "270", "275", "288", "296", "308", "312", "316", "320", "324",
                      "328", "332", "340", "344", "356", "360", "364", "368", "384",
                      "388", "398", "400", "404", "408", "410", "414", "417", "418",
                      "422", "426", "430", "434", "446", "450", "454", "458", "462",
                      "466", "474", "478", "480", "484", "496", "500", "504", "508",
                      "512", "516", "520", "524", "531", "533", "534", "535", "540",
                      "548", "558", "562", "566", "570", "580", "581", "583", "584",
                      "585", "586", "591", "598", "600", "604", "608", "612", "624",
                      "626", "630", "634", "638", "646", "652", "654", "659", "660",
                      "662", "663", "670", "678", "682", "686", "690", "694", "702",
                      "704", "706", "710", "716", "728", "729", "732", "740", "748",
                      "760", "762", "764", "768", "772", "776", "780", "784", "788",
                      "792", "795", "796", "798", "800", "818", "834", "850", "854",
                      "858", "860", "862", "876", "882", "887", "894")

            } else if(name == "developing countries excluding China") {

                out <-
                    c("4", "12", "16", "24", "28", "31", "32", "44", "48", "50", "51",
                      "52", "64", "68", "72", "74", "76", "84", "86", "90", "92", "96",
                      "104", "108", "116", "120", "132", "136", "140", "144", "148",
                      "152", # "156",  <--China is 156
                      "170", "174", "175", "178", "180", "184", "188",
                      "192", "204", "212", "214", "218", "222", "226", "231", "232",
                      "238", "239", "242", "254", "258", "260", "262", "266", "268",
                      "270", "275", "288", "296", "308", "312", "316", "320", "324",
                      "328", "332", "340", "344", "356", "360", "364", "368", "384",
                      "388", "398", "400", "404", "408", "410", "414", "417", "418",
                      "422", "426", "430", "434", "446", "450", "454", "458", "462",
                      "466", "474", "478", "480", "484", "496", "500", "504", "508",
                      "512", "516", "520", "524", "531", "533", "534", "535", "540",
                      "548", "558", "562", "566", "570", "580", "581", "583", "584",
                      "585", "586", "591", "598", "600", "604", "608", "612", "624",
                      "626", "630", "634", "638", "646", "652", "654", "659", "660",
                      "662", "663", "670", "678", "682", "686", "690", "694", "702",
                      "704", "706", "710", "716", "728", "729", "732", "740", "748",
                      "760", "762", "764", "768", "772", "776", "780", "784", "788",
                      "792", "795", "796", "798", "800", "818", "834", "850", "854",
                      "858", "860", "862", "876", "882", "887", "894")

            } else if(name == "Mela-Micro-Polynesia") {

                out <-
                    c("16", "90", "184", "242", "258", "296", "316",
                      "520", "540", "548", "570", "580", "581", "583",
                      "584", "585", "598", "612", "772", "776", "798",
                      "876", "882")

            } else if(name == "FP 2020 countries") {

                out <-
                    c("4", "50", "204", "64", "68", "854", "108", "116", "120", "140",
                      "148", "174", "178", "384", "408", "180", "262", "818", "232",
                      "231", "270", "288", "324", "624", "332", "340", "356", "360",
                      "368", "404", "417", "418", "426", "430", "450", "454", "466",
                      "478", "496", "508", "104", "524", "558", "562", "566", "586",
                      "598", "608", "646", "678", "686", "694", "90", "706", "728",
                      "144", "275", "729", "762", "626", "768", "800", "834", "860",
                      "704", "732", "887", "894", "716")

            } else if(name == "more developed countries") {

                out <-
                    c("8", "20", "36", "40", "56", "60", "70", "100", "112", "124",
                      "162", "166", "191", "196", "203", "208", "233", "234", "246",
                      "248", "250", "276", "292", "300", "304", "334", "336", "348",
                      "352", "372", "376", "380", "392", "428", "438", "440", "442",
                      "470", "492", "498", "499", "528", "554", "574", "578", "616",
                      "620", "642", "643", "666", "674", "680", "688", "703", "705",
                      "724", "744", "752", "756", "804", "807", "826", "831", "832", "833", "840")

            } else if(name == "less developed countries") {

                out <-
                    c("4", "12", "16", "24", "28", "31", "32", "44", "48", "50", "51",
                      "52", "64", "68", "72", "74", "76", "84", "86", "90", "92", "96",
                      "104", "108", "116", "120", "132", "136", "140", "144", "148",
                      "152", "156", "170", "174", "175", "178", "180", "184", "188",
                      "192", "204", "212", "214", "218", "222", "226", "231", "232",
                      "238", "239", "242", "254", "258", "260", "262", "266", "268",
                      "270", "275", "288", "296", "308", "312", "316", "320", "324",
                      "328", "332", "340", "344", "356", "360", "364", "368", "384",
                      "388", "398", "400", "404", "408", "410", "414", "417", "418",
                      "422", "426", "430", "434", "446", "450", "454", "458", "462",
                      "466", "474", "478", "480", "484", "496", "500", "504", "508",
                      "512", "516", "520", "524", "531", "533", "534", "535", "540",
                      "548", "558", "562", "566", "570", "580", "581", "583", "584",
                      "585", "586", "591", "598", "600", "604", "608", "612", "624",
                      "626", "630", "634", "638", "646", "652", "654", "659", "660",
                      "662", "663", "670", "678", "682", "686", "690", "694", "702",
                      "704", "706", "710", "716", "728", "729", "732", "740", "748",
                      "760", "762", "764", "768", "772", "776", "780", "784", "788",
                      "792", "795", "796", "798", "800", "818", "834", "850", "854",
                      "858", "860", "862", "876", "882", "887", "894")

            } else if(name == "least developed countries") {

                out <-
                    c("4", "24",	"50",	"64",	"90",	"104",	"108",	"116",	"140",	"148",
                      "174",	"180",	"204",	"231",	"232",	"262",	"270",	"296",	"324",
                      "332",	"418",	"426",	"430",	"450",	"454",	"466",	"478",	"508",
                      "524",	"548",	"562",	"624",	"626",	"646",	"678",	"686",	"694",
                      "706",	"728",	"729",	"768",	"798",	"800",	"834",	"854",	"887",
                      "894")

            } else if(name == "other developing countries") {

                out <-
                    c("12", "16", "28", "31", "32", "44", "48", "51", "52",
                      "68", "72", "74", "76", "84", "86", "92", "96", "120",
                      "132", "136", "144", "152", "156", "170", "175", "178", "184",
                      "188", "192", "212", "214", "218", "222", "226", "238", "239",
                      "242", "254", "258", "260", "266", "268", "275", "288", "308",
                      "312", "316", "320", "328", "340", "344", "356", "360", "364",
                      "368", "384", "388", "398", "400", "404", "408", "410", "414",
                      "417", "422", "434", "446", "458", "462", "474", "480", "484",
                      "496", "500", "504", "512", "516", "520", "531", "533", "534",
                      "535", "540", "558", "566", "570", "580", "581", "583", "584",
                      "585", "586", "591", "598", "600", "604", "608", "612", "630",
                      "634", "638", "652", "654", "659", "660", "662", "663", "670",
                      "682", "690", "702", "704", "710", "716", "732", "740", "748",
                      "760", "762", "764", "772", "776", "780", "784", "788", "792",
                      "795", "796", "818", "850", "858", "860", "862", "876", "882")

            } else if(name == "less developed countries excluding China") {

                out <-
                    c("4", "12", "16", "24", "28", "31", "32", "44", "48", "50", "51",
                      "52", "64", "68", "72", "74", "76", "84", "86", "90", "92", "96",
                      "104", "108", "116", "120", "132", "136", "140", "144", "148", "152", # "156",  <--China is 156
                      "170", "174", "175", "178", "180", "184", "188", "192", "204",
                      "212", "214", "218", "222", "226", "231", "232", "238", "239",
                      "242", "254", "258", "260", "262", "266", "268", "270", "275",
                      "288", "296", "308", "312", "316", "320", "324", "328", "332",
                      "340", "344", "356", "360", "364", "368", "384", "388", "398",
                      "400", "404", "408", "410", "414", "417", "418", "422", "426",
                      "430", "434", "446", "450", "454", "458", "462", "466", "474",
                      "478", "480", "484", "496", "500", "504", "508", "512", "516",
                      "520", "524", "531", "533", "534", "535", "540", "548", "558",
                      "562", "566", "570", "580", "581", "583", "584", "585", "586",
                      "591", "598", "600", "604", "608", "612", "624", "626", "630",
                      "634", "638", "646", "652", "654", "659", "660", "662", "663",
                      "670", "678", "682", "686", "690", "694", "702", "704", "706",
                      "710", "716", "728", "729", "732", "740", "748", "760", "762",
                      "764", "768", "772", "776", "780", "784", "788", "792", "795",
                      "796", "798", "800", "818", "834", "850", "854", "858", "860", "862", "876", "882", "887", "894")

            } else if(name == "sub-Saharan Africa") {

                out <-
                    c("24", "72", "86", "108", "120", "132", "140", "148", "174",
                      "175", "178", "180", "204", "226", "231", "232", "260", "262",
                      "266", "270", "288", "324", "384", "404", "426", "430", "450",
                      "454", "466", "478", "480", "508", "516", "562", "566", "624",
                      "638", "646", "654", "678", "686", "690", "694", "706", "710",
                      "716", "728", "748", "768", "800", "834", "854", "894")

            }

        } else {
            stop("'family' '", family, "' is not a valid selection.")
        }

        return(out)
    }









#-------------------------------------------------------------------------------
# The End!

