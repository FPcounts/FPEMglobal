#----------------------------------------------------------------------
# Jin Rou New and Leontine Alkema
# plotcomparison.R
#----------------------------------------------------------------------
# Functions to plot comparison of CIs for any objects with CIprop and CIcounts
# and country data
#----------------------------------------------------------------------
PlotComparison <- function(# Plot lots of results!
  ### Wrapper function to plot lots of results.
  run.name, ##<< Run name for first run
  run.name2, ##<< Run name for second run
  run.name3 = NULL, ##<< (Optional) Run name for third run
  run.name4 = NULL, ##<< (Optional) Run name for fourth run
  iso.compare = NULL, ##<< Vector of 3-character ISO country code of countries to plot comparison for.
  ## If NULL, all countries with estimates in \code{run.name} are plotted.
  legend = run.name, ##<< Legend for first run
  legend2 = run.name2, ##<< Legend for second run
  legend3 = run.name3, ##<< Legend for third run
  legend4 = run.name4, ##<< Legend for fourth run
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  output.dir2 = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name2}, default from \code{runMCMC}.
  output.dir3 = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name3}, default from \code{runMCMC}.
  output.dir4 = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name4}, default from \code{runMCMC}.
  fig.dir = NULL, ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
  start.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
  end.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
  plot.for.aggregates = FALSE, ##<< Plot comparison plots for aggregates?
  plot.ind.country.results = FALSE, ##<< Create zillion plots for all countries?
  ## If TRUE, plots are saved in subdirectory "country.plots" in fig.dir.
  ymin.at.0 = TRUE, ##<< Set lower bound on y-axis at 0?
  ymax.at.100 = TRUE, ##<< Set upper bound on y-axis at percent = 100%? Only applies if plot.prop is TRUE.
  UWRA = FALSE,
  all.women = FALSE,
  plot_data = TRUE
  ){

    res_rda_filename <- function(output_dir, all_women) {
        if(all_women && file.exists(file.path(output_dir, "res.country.all.women.rda")))
            fname <- "res.country.all.women.rda"
        else fname <- "res.country.rda"
        fname
    }
    res_agg_rda_filename <- function(output_dir, all_women) {
        if(all_women && file.exists(file.path(output_dir, "res.aggregate.all.women.rda")))
            fname <- "res.aggregate.all.women.rda"
        else fname <- "res.aggregate.rda"
        fname
    }

    res_rda_objectname <- function(fname) {
        paste(head(strsplit(fname, "\\.")[[1]], -1), collapse = ".")
    }

  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)
  if (plot.ind.country.results)
    fig.dir.countries <- file.path(fig.dir, "country.plots/")
  if (is.null(output.dir4))
    output.dir4 <- file.path(getwd(), "output", run.name4, "/")
  if (is.null(output.dir3))
    output.dir3 <- file.path(getwd(), "output", run.name3, "/")
  if (is.null(output.dir2))
    output.dir2 <- file.path(getwd(), "output", run.name2, "/")
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name, "/")

  fig.name.years <- ifelse(!is.null(start.year) | !is.null(end.year),
                           paste0("_",
                                  ifelse(!is.null(start.year), paste0("from", floor(start.year)), ""),
                                  ifelse(!is.null(end.year), paste0("to", floor(end.year)), "")),
                           "")

  all.run.name.fig <-
      abbreviate(c(run.name, run.name2, run.name3, run.name4), minlength = 25)
  run.name.fig <- all.run.name.fig[1]
  run.name2.fig <- all.run.name.fig[2]
  run.name3.fig <- all.run.name.fig[3]
  run.name4.fig <- all.run.name.fig[4]

  if (!is.null(run.name4)) {
      load(file = paste(output.dir4,res_rda_filename(output.dir4, all.women), sep = ""))
      res.country4 <- get(res_rda_objectname(res_rda_filename(output.dir4, all.women)))
    if (plot.for.aggregates) {
      load(file = paste(output.dir4,res_agg_rda_filename(output.dir4, all.women), sep = ""))
      res.aggregate4 <- get(res_rda_objectname(res_agg_rda_filename(output.dir4, all.women)))
    }
  }
  if (!is.null(run.name3)) {
    load(file = paste(output.dir3,res_rda_filename(output.dir3, all.women), sep = ""))
    res.country3 <- get(res_rda_objectname(res_rda_filename(output.dir3, all.women)))
    rm(res.country)
    if (plot.for.aggregates) {
      load(file = paste(output.dir3,res_agg_rda_filename(output.dir3, all.women), sep = ""))
      res.aggregate3 <- get(res_rda_objectname(res_agg_rda_filename(output.dir3, all.women)))
    }
  }
  load(file = paste(output.dir2,res_rda_filename(output.dir2, all.women), sep = ""))
  res.country2 <- get(res_rda_objectname(res_rda_filename(output.dir2, all.women)))
  if (plot.for.aggregates) {
    load(file = paste(output.dir2,res_agg_rda_filename(output.dir2, all.women), sep = ""))
    res.aggregate2 <- get(res_rda_objectname(res_agg_rda_filename(output.dir2, all.women)))
  }
  load(file = paste(output.dir,"mcmc.meta.rda", sep = ""))
    load(file = paste(output.dir,res_rda_filename(output.dir, all.women), sep = ""))
    res.country <- get(res_rda_objectname(res_rda_filename(output.dir, all.women)))
  if (plot.for.aggregates) {
      load(file = paste(output.dir,res_agg_rda_filename(output.dir, all.women), sep = ""))
      res.aggregate <- get(res_rda_objectname(res_agg_rda_filename(output.dir, all.women)))
      }

  if (is.null(iso.compare)) {
    iso.compare <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
    select.c <- NULL
  } else {
    code.c <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
    if (any(!is.element(iso.compare, code.c))) {
      stop(paste0("iso.compare: ", paste(iso.compare[!is.element(iso.compare, code.c)], collapse = ", "),
                  " not found in results of run.name"))
      return(invisible())
    }
    select.c <- match(iso.compare, code.c)
  }

  ## Trim res.country so that it has only those countries in all the others are kept
  res234.iso.g <- numeric(0)
  if(!is.null(run.name4)) res234.iso.g <- c(res234.iso.g, res.country4$iso.g)
  if(!is.null(run.name3)) res234.iso.g <- c(res234.iso.g, res.country3$iso.g)
  if(!is.null(run.name2)) res234.iso.g <- c(res234.iso.g, res.country2$iso.g)
  res234.iso.g <- unique(res234.iso.g)
  if(length(res234.iso.g) > 0) {
      iso.in.all <- res.country$iso.g %in% res234.iso.g
      res.country$CIprop.Lg.Lcat.qt <- res.country$CIprop.Lg.Lcat.qt[iso.in.all]
      res.country$CIratio.Lg.Lcat.qt <- res.country$CIratio.Lg.Lcat.qt[iso.in.all]
      res.country$CIcount.Lg.Lcat.qt <- res.country$CIcount.Lg.Lcat.qt[iso.in.all]
  }

  if (plot.for.aggregates) {
      ## Trim res.country so that it has only those aggregates in all the others are kept
      res234.reg.g <- numeric(0)
      if(!is.null(run.name4)) res234.reg.g <-
                                  c(res234.reg.g, names(res.aggregate4$CIprop.Lg.Lcat.qt))
      if(!is.null(run.name3)) res234.reg.g <-
                                  c(res234.reg.g, names(res.aggregate3$CIprop.Lg.Lcat.qt))
      if(!is.null(run.name2)) res234.reg.g <-
                                  c(res234.reg.g, names(res.aggregate2$CIprop.Lg.Lcat.qt))
      res234.reg.g <- unique(res234.reg.g)
      if(length(res234.reg.g) > 0) {
          reg.in.all <- names(res.aggregate$CIprop.Lg.Lcat.qt) %in% res234.reg.g
          res.aggregate$CIprop.Lg.Lcat.qt <- res.aggregate$CIprop.Lg.Lcat.qt[reg.in.all]
          res.aggregate$CIratio.Lg.Lcat.qt <- res.aggregate$CIratio.Lg.Lcat.qt[reg.in.all]
          res.aggregate$CIcount.Lg.Lcat.qt <- res.aggregate$CIcount.Lg.Lcat.qt[reg.in.all]
      }
  }

  # order country lists of res.country4 so that it has the same country order as res.country
  if (!is.null(run.name4)) {
      iso.order4 <- na.omit(match(res.country$iso.g, res.country4$iso.g))
    res.country4$CIprop.Lg.Lcat.qt <- res.country4$CIprop.Lg.Lcat.qt[iso.order4]
    res.country4$CIratio.Lg.Lcat.qt <- res.country4$CIratio.Lg.Lcat.qt[iso.order4]
    res.country4$CIcount.Lg.Lcat.qt <- res.country4$CIcount.Lg.Lcat.qt[iso.order4]
    if (plot.for.aggregates) {
      reg.order4 <- match(names(res.aggregate$CIprop.Lg.Lcat.qt), names(res.aggregate4$CIprop.Lg.Lcat.qt))
      res.aggregate4$CIprop.Lg.Lcat.qt <- res.aggregate4$CIprop.Lg.Lcat.qt[reg.order4]
      res.aggregate4$CIratio.Lg.Lcat.qt <- res.aggregate4$CIratio.Lg.Lcat.qt[reg.order4]
      res.aggregate4$CIcount.Lg.Lcat.qt <- res.aggregate4$CIcount.Lg.Lcat.qt[reg.order4]
    }
  } else {
    res.country4 <- res.aggregate4 <- NULL
  }
  # order country lists of res.country3 so that it has the same country order as res.country
  if (!is.null(run.name3)) {
    iso.order3 <- na.omit(match(res.country$iso.g, res.country3$iso.g))
    res.country3$CIprop.Lg.Lcat.qt <- res.country3$CIprop.Lg.Lcat.qt[iso.order3]
    res.country3$CIratio.Lg.Lcat.qt <- res.country3$CIratio.Lg.Lcat.qt[iso.order3]
    res.country3$CIcount.Lg.Lcat.qt <- res.country3$CIcount.Lg.Lcat.qt[iso.order3]
    if (plot.for.aggregates) {
      reg.order3 <- match(names(res.aggregate$CIprop.Lg.Lcat.qt), names(res.aggregate3$CIprop.Lg.Lcat.qt))
      res.aggregate3$CIprop.Lg.Lcat.qt <- res.aggregate3$CIprop.Lg.Lcat.qt[reg.order3]
      res.aggregate3$CIratio.Lg.Lcat.qt <- res.aggregate3$CIratio.Lg.Lcat.qt[reg.order3]
      res.aggregate3$CIcount.Lg.Lcat.qt <- res.aggregate3$CIcount.Lg.Lcat.qt[reg.order3]
    }
  } else {
    res.country3 <- res.aggregate3 <- NULL
  }
  # order country lists of res.country2 so that it has the same country order as res.country
  iso.order2 <- na.omit(match(res.country$iso.g, res.country2$iso.g))
  res.country2$CIprop.Lg.Lcat.qt <- res.country2$CIprop.Lg.Lcat.qt[iso.order2]
  res.country2$CIratio.Lg.Lcat.qt <- res.country2$CIratio.Lg.Lcat.qt[iso.order2]
  res.country2$CIcount.Lg.Lcat.qt <- res.country2$CIcount.Lg.Lcat.qt[iso.order2]
  if (plot.for.aggregates) {
    reg.order2 <- match(names(res.aggregate$CIprop.Lg.Lcat.qt), names(res.aggregate2$CIprop.Lg.Lcat.qt))
    res.aggregate2$CIprop.Lg.Lcat.qt <- res.aggregate2$CIprop.Lg.Lcat.qt[reg.order2]
    res.aggregate2$CIratio.Lg.Lcat.qt <- res.aggregate2$CIratio.Lg.Lcat.qt[reg.order2]
    res.aggregate2$CIcount.Lg.Lcat.qt <- res.aggregate2$CIcount.Lg.Lcat.qt[reg.order2]
  }

  #------------------------------------------------------------------------------------------
  ##details<< Plot country overview plots for proportions without details using
  ##\code{\link{PlotDataAndEstimates}}.

    ## [MCW-2019-01-25] Plot countries with no data as well if not
    ## 'diagnostic' plots. Note that 'data' is not sorted in the same order as
    ## 'country.info' (and 'data' has one row per observation anyway,
    ## 'country.info' has one row per country.)
    if(mcmc.meta$general$include.c.no.data) {
        country.info <- rbind(mcmc.meta$data.raw$country.info
                             ,mcmc.meta$data.raw$country.info.no.data
                              )
        ## Need this as well because PlotDataAndEstimates() does not take argument 'country.info'.
        mcmc.meta$data.raw$country.info <- country.info
    } else country.info <- mcmc.meta$data.raw$country.info

    if(all.women || isFALSE(plot_data)) {
        plotDE.data.raw <- NULL
        plotDE.country.info <- country.info
    }  else {
        plotDE.data.raw <- mcmc.meta$data.raw
        plotDE.country.info <- NULL
    }

    PlotDataAndEstimates(data.raw = plotDE.data.raw,
                         country.info = plotDE.country.info,
                       CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                       CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                       CI2.Lg.Lcat.qt = res.country2$CIprop.Lg.Lcat.qt,
                       CIratio2.Lg.Lcat.qt = res.country2$CIratio.Lg.Lcat.qt,
                       CI3.Lg.Lcat.qt = res.country3$CIprop.Lg.Lcat.qt,
                       CIratio3.Lg.Lcat.qt = res.country3$CIratio.Lg.Lcat.qt,
                       CI4.Lg.Lcat.qt = res.country4$CIprop.Lg.Lcat.qt,
                       CIratio4.Lg.Lcat.qt = res.country4$CIratio.Lg.Lcat.qt,
                       select.c = select.c,
                       name.dir1 = legend,
                       name.dir2 = legend2,
                       name.dir3 = legend3,
                       name.dir4 = legend4,
                       start.year = start.year,
                       end.year = end.year,
                       ymin.at.0 = ymin.at.0,
                       ymax.at.100 = ymax.at.100,
                       fig.name = file.path(fig.dir, paste0(run.name.fig, " vs ", run.name2.fig,
                                                            ifelse(is.null(run.name3), "", paste0(" vs ", run.name3.fig)),
                                                            ifelse(is.null(run.name4), "", paste0(" vs ", run.name4.fig)), "CIs_nopar", fig.name.years, ".pdf")),
                       UWRA = UWRA,
                       all.women = all.women)
  # to plot individual country results
  if (plot.ind.country.results) {
    PlotDataAndEstimates(data.raw = mcmc.meta$data.raw,
                         CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                         CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                         CI2.Lg.Lcat.qt = res.country2$CIprop.Lg.Lcat.qt,
                         CIratio2.Lg.Lcat.qt = res.country2$CIratio.Lg.Lcat.qt,
                         CI3.Lg.Lcat.qt = res.country3$CIprop.Lg.Lcat.qt,
                         CIratio3.Lg.Lcat.qt = res.country3$CIratio.Lg.Lcat.qt,
                         CI4.Lg.Lcat.qt = res.country4$CIprop.Lg.Lcat.qt,
                         CIratio4.Lg.Lcat.qt = res.country4$CIratio.Lg.Lcat.qt,
                         select.c = select.c,
                         name.dir1 = legend,
                         name.dir2 = legend2,
                         name.dir3 = legend3,
                         name.dir4 = legend4,
                         start.year = start.year,
                         end.year = end.year,
                         ymin.at.0 = ymin.at.0,
                         ymax.at.100 = ymax.at.100,
                         ind.country.overviewplot = TRUE,
                         fig.name = paste0(run.name.fig, " vs ", run.name2.fig,
                                           ifelse(is.null(run.name3), "", paste0(" vs ", run.name3.fig)),
                                           ifelse(is.null(run.name4), "", paste0(" vs ", run.name4.fig)), fig.name.years),
                       UWRA = UWRA,
                       all.women = all.women)
  }
  ##details<< Plot country overview plots for counts using
  ##\code{\link{PlotDataAndEstimates}}.
  PlotDataAndEstimates(
    CI.Lg.Lcat.qt = res.country$CIcount.Lg.Lcat.qt,
    CI2.Lg.Lcat.qt = res.country2$CIcount.Lg.Lcat.qt,
    CI3.Lg.Lcat.qt = res.country3$CIcount.Lg.Lcat.qt,
    CI4.Lg.Lcat.qt = res.country4$CIcount.Lg.Lcat.qt,
    select.c = select.c,
    name.dir1 = legend,
    name.dir2 = legend2,
    name.dir3 = legend3,
    name.dir4 = legend4,
    start.year = start.year,
    end.year = end.year,
    plot.prop = FALSE,
    ymin.at.0 = ymin.at.0,
    ymax.at.100 = ymax.at.100,
    fig.name = file.path(fig.dir, paste0(run.name.fig, " vs ", run.name2.fig,
                                         ifelse(is.null(run.name3), "", paste0(" vs ", run.name3.fig)),
                                         ifelse(is.null(run.name4), "", paste0(" vs ", run.name4.fig)), "CIscountcountry", fig.name.years, ".pdf")),
                       UWRA = UWRA,
                       all.women = all.women)


  if (plot.for.aggregates) {

    PlotDataAndEstimates(CI.Lg.Lcat.qt = res.aggregate$CIprop.Lg.Lcat.qt,
                         CIratio.Lg.Lcat.qt = res.aggregate$CIratio.Lg.Lcat.qt,
                         CI2.Lg.Lcat.qt = res.aggregate2$CIprop.Lg.Lcat.qt,
                         CIratio2.Lg.Lcat.qt = res.aggregate2$CIratio.Lg.Lcat.qt,
                         CI3.Lg.Lcat.qt = res.aggregate3$CIprop.Lg.Lcat.qt,
                         CIratio3.Lg.Lcat.qt = res.aggregate3$CIratio.Lg.Lcat.qt,
                         CI4.Lg.Lcat.qt = res.aggregate4$CIprop.Lg.Lcat.qt,
                         CIratio4.Lg.Lcat.qt = res.aggregate4$CIratio.Lg.Lcat.qt,
                         name.dir1 = legend,
                         name.dir2 = legend2,
                         name.dir3 = legend3,
                         start.year = start.year,
                         end.year = end.year,
                         ymin.at.0 = ymin.at.0,
                         ymax.at.100 = ymax.at.100,
                         fig.name = file.path(fig.dir, paste0(run.name.fig, " vs ", run.name2.fig,
                                                              ifelse(is.null(run.name3), "", paste0(" vs ", run.name3.fig)),
                                                              ifelse(is.null(run.name4), "", paste0(" vs ", run.name4.fig)), "CIsaggregate", fig.name.years, ".pdf")),
                       UWRA = UWRA,
                       all.women = all.women)
    PlotDataAndEstimates(
      CI.Lg.Lcat.qt = res.aggregate$CIcount.Lg.Lcat.qt,
      CI2.Lg.Lcat.qt = res.aggregate2$CIcount.Lg.Lcat.qt,
      CI3.Lg.Lcat.qt = res.aggregate3$CIcount.Lg.Lcat.qt,
      CI4.Lg.Lcat.qt = res.aggregate4$CIcount.Lg.Lcat.qt,
      select.c = select.c,
      name.dir1 = run.name,
      name.dir2 = run.name2,
      name.dir3 = run.name3,
      name.dir4 = run.name4,
      start.year = start.year,
      end.year = end.year,
      plot.prop = FALSE,
      ymin.at.0 = ymin.at.0,
      ymax.at.100 = ymax.at.100,
      fig.name = file.path(fig.dir, paste0(run.name.fig," vs ", run.name2.fig,
                                           ifelse(is.null(run.name3), "", paste0(" vs ", run.name3.fig)),
                                           ifelse(is.null(run.name4), "", paste0(" vs ", run.name4.fig)), "CIscountaggregate", fig.name.years, ".pdf")),
                       UWRA = UWRA,
                       all.women = all.women)
  }
  cat(paste0("Comparison plots saved to ", fig.dir, "\n"))
  ##value<< NULL
  return(invisible(NULL))
} # end function
#----------------------------------------------------------------------
# The End!
