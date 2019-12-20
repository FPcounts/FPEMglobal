#----------------------------------------------------------------------
# F_change.R
# Functions to calculate change in proportion/counts of an indicator
# Jin Rou New, 2014
#----------------------------------------------------------------------
GetChangeFromYear1ToYear2 <- function(# Get change in indicator (proportion and count)
  ### Get posterior samples of change in indicator (proportion and count) 
  ### from \code{year1} to \code{year2} for country with 3-digit ISO code \code{iso.select}.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  iso.select, ##<< 3-digit ISO country code
  indicator, ##<< Indicator
  year1, ##<< Year from
  year2 ##<< Year to
) {
  stopifnot(is.numeric(year1), is.numeric(year2))
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name)
  iso.select <- paste(iso.select)
  load(file.path(output.dir, "res.country.rda"))
  load(file.path(output.dir, "countrytrajectories", 
                 paste0("P.tp3s_country", which(res.country$iso.g == iso.select), ".rda")))
  years <- dimnames(P.tp3s)[[1]]
  P.year1.s <- GetCountryPostSamples(props.array = P.tp3s, year = year1, indicator = indicator)
  P.year2.s <- GetCountryPostSamples(props.array = P.tp3s, year = year2, indicator = indicator)
  P.s <- P.year2.s - P.year1.s
  MWRA <- GetMWRA(run.name = run.name, iso.select = iso.select, years.select = c(year1, year2))
  WP.year1.s <- P.year1.s*MWRA[paste(year1)]
  WP.year2.s <- P.year2.s*MWRA[paste(year2)]
  WP.s <- WP.year2.s - WP.year1.s 
  if (indicator %in% c("Met Demand", "Met Demand with Modern Methods")) {
    Pmet.year1.s <- GetCountryPostSamples(props.array = P.tp3s, year = year1,
                                          indicator = ifelse(indicator == "Met Demand", "Total", "Modern"))
    Pmet.year2.s <- GetCountryPostSamples(props.array = P.tp3s, year = year2, 
                                          indicator = ifelse(indicator == "Met Demand", "Total", "Modern"))
    Pdemand.year1.s <- GetCountryPostSamples(props.array = P.tp3s, year = year1, indicator = "TotalPlusUnmet")
    Pdemand.year2.s <- GetCountryPostSamples(props.array = P.tp3s, year = year2, indicator = "TotalPlusUnmet")
  } else {
    Pmet.year1.s <- Pmet.year2.s <- Pdemand.year1.s <- Pdemand.year2.s <- NULL
  }
  return(list(P.s = P.s,
              P.year1.s = P.year1.s,
              P.year2.s = P.year2.s,
              WP.s = WP.s,
              WP.year1.s = WP.year1.s,
              WP.year2.s = WP.year2.s,
              MWRA.year1 = MWRA[paste(year1)],
              MWRA.year2 = MWRA[paste(year2)],
              Pmet.year1.s = Pmet.year1.s,
              Pmet.year2.s = Pmet.year2.s,
              Pdemand.year1.s = Pdemand.year1.s, 
              Pdemand.year2.s = Pdemand.year2.s))
}
#----------------------------------------------------------------------
GetMWRA <- function(# Get number of MWRA for a certain country and year
  ### Get number of MWRA (in thousands)
  ### for \code{year} for country with 3-digit ISO code \code{iso.select} from \code{res.country}.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  iso.select, ##<< 3-digit ISO country code
  years.select = NULL ##<< Vector of year(s). If \code{NULL}, all available years of MWRA are returned.
) {
  stopifnot(is.numeric(years.select))
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name)
  load(file.path(output.dir, "res.country.rda"))
  years <- colnames(res.country$CIprop.Lg.Lcat.qt[[1]][[1]])
  MWRA <- res.country$W.Lg.t[[which(res.country$iso.g == iso.select)]][years %in% paste(years.select)]
  names(MWRA) <- years.select
  return(MWRA = MWRA) ##<< Number of MWRA (in thousands) for selected years.
}
