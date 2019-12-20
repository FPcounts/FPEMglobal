#----------------------------------------------------------------------
# F_posteriorprob.R
# Functions to calculate proportion/counts/ARR of an indicator from a given
# posterior probability or vice versa
# Jin Rou New, 2014
#----------------------------------------------------------------------
GetPosteriorProbFromCutoffProp <- function(# Calculate posterior probability given cut-off proportion
  ### Calculate posterior probability that 
  ### a country's proportion/count (e.g. for modern contraceptive prevalence) will be above 
  ### some cut-off.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  iso.select, ##<< 3-character ISO country code
  indicator, ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand", "Met Demand with Modern Methods"
  year, ##<< Mid-point year to calculate posterior probability for
  cutoffprop = NULL, ##<< Cut-off proportion
  cutoffwomen = NULL, ##<< Cut-off number of women # change JR, 20140311
  cutoffwomenadd = NULL, ##<< Cut-off number of additional women # change JR, 20140311
  year.current = NULL, ##<< Current year, used for reference year for number of additional women # change JR, 20140311
  sign ##<< Character input for sign of inequality, may be one of the choices: "<", "<=", ">", ">="
) {
  # change JR, 20140311
  if (indicator %in% c("Met Demand", "Met Demand with Modern Methods") & 
        is.null(cutoffprop))
    stop("cutoffprop cannot be NULL if indicator is Met Demand or Met Demand with Modern Methods.")
  cutoff <- list(cutoffprop = cutoffprop, cutoffwomen = cutoffwomen, cutoffwomenadd = cutoffwomenadd)
  if (sum(sapply(cutoff, is.null)) != 2)
    stop("One and only one of cutoffprop, cutoffwomen or cutoffwomenadd can be non-NULL.")
  if (!is.null(cutoffwomenadd) & is.null(year.current))
    stop("year.current cannot be NULL if cutoffwomenadd is specified.")
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name)
  
  load(file.path(output.dir, "mcmc.meta.rda"))
  winbugs.data <- mcmc.meta$winbugs.data
  country.info <- mcmc.meta$data.raw$country.info
  c <- which(gsub(" ", "", country.info$code.c) %in% iso.select)
  if (is.null(cutoffprop)) { # change JR, 20140311
    load(file.path(output.dir, "res.country.rda")) 
    est.years.char <- colnames(res.country$CIprop.Lg.Lcat.qt[[1]][[1]])
    women.all <- res.country$W.Lg.t[[c]][est.years.char == paste0(year)]*1000
  }
  load(file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", c, ".rda")))
  samps <- GetCountryPostSamples(props.array = P.tp3s, year = year, indicator = indicator)
  if (!is.null(cutoffprop)) { # change JR, 20140311
    eval(parse(text = paste0("postprob <- mean(samps ", sign, "cutoffprop)")))
  } else if (!is.null(cutoffwomen)) {
    eval(parse(text = paste0("postprob <- mean(samps*women.all ", sign, "cutoffwomen)")))
  } else if (!is.null(cutoffwomenadd)) {
    if (!(indicator %in% names(res.country$CIcount.Lg.Lcat.qt[[c]]))) {
      postprob <- NULL
      print(paste0("indicator ", indicator, " not found in res.country."))
    } else {
      women.current <- res.country$CIcount.Lg.Lcat.qt[[c]][[paste0(indicator)]]["0.5", est.years.char == paste0(year.current)]*1000
      eval(parse(text = paste0("postprob <- mean(samps*women.all - women.current ", sign, "cutoffwomenadd)")))
    }
  }
  return(postprob)
}

GetPropFromPosteriorProb <- function( # Calculate proportion given posterior probability
  ### Calculate what proportion is associated with 
  ### a posterior probability, e.g. if a county wants to get a modern contraceptive prevalence (CP) 
  ### that is the 95th percentile for some year, what is that CP?
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  iso.select, ##<< 3-character ISO country code
  indicator, ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand", "Met Demand with Modern Methods"
  year, ##<< Mid-point year to calculate proportion for
  postprob, ##<< Posterior probability
  year.current = NULL, ##<<< Optional: Current year, used to calculate number of additional women needed
  sign ##<< Character input for sign of inequality, may be one of the choices: "<", "<=", ">", ">="
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name)
  load(file.path(output.dir, "mcmc.meta.rda"))
  winbugs.data <- mcmc.meta$winbugs.data
  country.info <- mcmc.meta$data.raw$country.info
  
  c <- which(gsub(" ", "", country.info$code.c) %in% iso.select)
  load(file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", c, ".rda")))
  samps <- GetCountryPostSamples(props.array = P.tp3s, year = year, indicator = indicator)
  if (sign %in% c("<", "<=")) {
    prop <- quantile(samps, probs = postprob)
  } else if (sign %in% c(">=", ">")) {
    prop <- quantile(samps, probs = 1-postprob) # change JR, 20140901: Fixed bug (was 1-quantile originally!)
  }
  
  if (!(indicator %in% c("Met Demand", "Met Demand with Modern Methods"))) {
    load(file.path(output.dir, "res.country.rda"))
    est.years.char <- colnames(res.country$CIprop.Lg.Lcat.qt[[1]][[1]])
    women <- prop*res.country$W.Lg.t[[c]][est.years.char == paste0(year)]*1000
    if (is.null(year.current)) {
      women.add <- NULL
    } else {
      if (!(indicator %in% names(res.country$CIcount.Lg.Lcat.qt[[c]]))) {
        women.add <- NULL
        print(paste0("indicator ", indicator, " not found in res.country."))
      } else {
        women.current <- res.country$CIcount.Lg.Lcat.qt[[c]][[paste0(indicator)]]["0.5", est.years.char == paste0(year.current)]*1000
        women.add <- women - women.current
      }
    }
  } else {
    women <- women.add <- NULL
  }
  return(list(prop = prop, women = women, women.add = women.add))
}

GetPosteriorProbFromCutoffARR <- function(# Calculate posterior probability given cut-off ARR
  ### Calculate posterior probability that 
  ### a country's ARR for some proportion (e.g. for modern contraceptive prevalence) 
  ### in some period year1 to year2 will be above some cut-off ARR.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  iso.select, ##<< 3-character ISO country code
  indicator, ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand", "Met Demand with Modern Methods"
  year1, ##<< First year to calculate ARR from
  year2, ##<< Second year to calculate ARR to
  cutoffarr, ##<< Cut-off ARR (in percent)
  sign ##<< Character input for sign of inequality, may be one of the choices: "<", "<=", ">", ">="
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name)
  load(file.path(output.dir, "mcmc.meta.rda"))
  winbugs.data <- mcmc.meta$winbugs.data
  country.info <- mcmc.meta$data.raw$country.info
  
  c <- which(gsub(" ", "", country.info$code.c) %in% iso.select)
  load(file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", c, ".rda")))
  samps.year1 <- GetCountryPostSamples(props.array = P.tp3s, year = year1, indicator = indicator)
  samps.year2 <- GetCountryPostSamples(props.array = P.tp3s, year = year2, indicator = indicator)
  samps <- 1/(year2-year1)*log(samps.year2/samps.year1)*-100
  eval(parse(text = paste0("postprob <- mean(samps ", sign, " cutoffarr)")))
  return(postprob)
}

GetARRFromPosteriorProb <- function(# Calculate ARR given posterior probability
  ### Calculate what ARR is associated with 
  ### a posterior probability, e.g. if a county wants to get an ARR of modern 
  ### contraceptive prevalence (CP) that is the 95th percentile for some period
  ### year1 to year2, what is that ARR? 
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  iso.select, ##<< 3-character ISO country code
  indicator, ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand", "Met Demand with Modern Methods"
  year1, ##<< First year to calculate ARR from
  year2, ##<< Second year to calculate ARR to
  postprob, ##<< Posterior probability
  sign ##<< Character input for sign of inequality, may be one of the choices: "<", "<=", ">", ">="
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name)
  load(file.path(output.dir, "mcmc.meta.rda"))
  winbugs.data <- mcmc.meta$winbugs.data
  country.info <- mcmc.meta$data.raw$country.info
  
  c <- which(gsub(" ", "", country.info$code.c) %in% iso.select)
  load(file.path(output.dir, "countrytrajectories", paste0("P.tp3s_country", c, ".rda")))
  samps.year1 <- GetCountryPostSamples(props.array = P.tp3s, year = year1, indicator = indicator)
  samps.year2 <- GetCountryPostSamples(props.array = P.tp3s, year = year2, indicator = indicator)
  samps <- 1/(year2-year1)*log(samps.year2/samps.year1)*-100
  if (sign %in% c("<", "<=")) {
    prop <- quantile(samps, probs = postprob)
  } else if (sign %in% c(">=", ">")) {
    prop <- quantile(samps, probs = 1-postprob) # change JR, 20140901: Fixed bug (was 1-quantile originally!)
  }
  return(prop)
}

GetCountryPostSamples <- function( # Get posterior samples of some indicator for a country
  ### Internal function to get posterior samples of some indicator for a country.
  props.array, ##<< Array with posterior samples of proportions.
  year, ##<< Year to get posterior samples of
  indicator ##<< Any of the following choices: "Total", "Modern", "Traditional", 
  ## "Unmet", "TotalPlusUnmet", "TradPlusUnmet", "Met Demand", "Met Demand with Modern Methods"
) {
  if (!(indicator %in% c("Total", "Modern", "Traditional", 
                         "Unmet", "TotalPlusUnmet", "TradPlusUnmet", 
                         "Met Demand", "Met Demand with Modern Methods")))
    stop("indicator must be one of the following choices: Total, Modern, Traditional, Unmet, TotalPlusUnmet, TradPlusUnmet, Met Demand or Met Demand with Modern Methods.")
  
  if (indicator %in% dimnames(props.array)[[2]]) {
    samps <- props.array[paste0(year), indicator, ]
  } else if (indicator == "Total") {  
    samps <- props.array[paste0(year), "Traditional", ] + props.array[paste0(year), "Modern", ]
  } else if (indicator == "TotalPlusUnmet") {
    samps <- props.array[paste0(year), "Traditional", ] + props.array[paste0(year), "Modern", ] + props.array[paste0(year), "Unmet", ]
  } else if (indicator == "TradPlusUnmet") {
    samps <- props.array[paste0(year), "Traditional", ] + props.array[paste0(year), "Unmet", ]
  } else if (indicator == "Met Demand") {
    samps <- (props.array[paste0(year), "Traditional", ] + props.array[paste0(year), "Modern", ])/
      (props.array[paste0(year), "Traditional", ] + props.array[paste0(year), "Modern", ] + 
         props.array[paste0(year), "Unmet", ])
  } else if (indicator == "Met Demand with Modern Methods") {
    samps <- props.array[paste0(year), "Modern", ]/
      (props.array[paste0(year), "Traditional", ] + props.array[paste0(year), "Modern", ] + 
         props.array[paste0(year), "Unmet", ])
  }
  return(samps)
}
