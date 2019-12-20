#----------------------------------------------------------------------------------
# Leontine Alkema and Jin Rou New
# F_plotCIstime.R
# Functions to plot CIs only for any object with CIprop and CIcounts
# and country data
#----------------------------------------------------------------------------------
##[MCW-2016-04-26-5]
###-----------------------------------------------------------------------------
### NEW version of PlotDataAndEstiamtes
###
### Mark Wheldon, 2016-04-26
###
### - Has slightly different colour for CIs
### - Has no "Ratio (modern/any method)"
### - Add at the end a graphic for the SDG indicator "Demand satisfied (modern methods)"
### - Change title of existing graphic "Demand satisfied" to "Demand satisfied (any method)"
### - ... Probably other changes I'll do later and forget to list here.
###
## [MCW-2017-01-30-6] :: Renamed. Was called 'PlotDataAndEstimates2()'
PlotDataAndEstimates <- function (# Create overview country/aggregate plots
  ### Create overview plots of estimates of proportions/counts over time
  ### for either
  ### all countries, or a set of aggregates.
  ### Note that all input arguments with default setting NULL are optional
  ### (e.g. data are not plotted if data.raw=NULL).
  data.raw = NULL, ##<< Non-NULL only for country plots: List with \code{data},
  ## \code{country.info} and \code{region.info}.
  start.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
  end.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
  CI.Lg.Lcat.qt = NULL, ##<< Object from class \code{CI.Lg.Lcat.qt} (optional), either a proportion or a count (see next).
  plot.prop = TRUE, ##<< Are the CIs proportions or counts?
  ymin.at.0 = TRUE, ##<< Set lower bound on y-axis at 0?
  ymax.at.100 = TRUE, ##<< Set upper bound on y-axis at percent = 100%? Only applies if plot.prop is TRUE.
  add.info = TRUE, ##<< Add information related to misclassification or different populations.
  par.ciq = NULL, ##<< Add info on logistic parameters (optional, relevant for country proportion plots only)
  CIstar.Lg.Lcat.qt = NULL,##<< Systematic curves, object from class \code{CI.Lg.Lcat.qt} (optional, relevant for country proportion plots only)
  CIratio.Lg.Lcat.qt = NULL,##<< Systematic curves, object from class \code{CI.Lg.Lcat.qt} (optional, relevant for proportion plots only)
  validation = FALSE, ##<< Logical, if TRUE, plot observation from test sets in grey.
  getj.test.k = NULL, ##<< Used if \code{validation}, indices of test sets for observations on
  ## traditional and modern use.
  getj.test.unmet.k = NULL,##<< Used if \code{validation}, indices of test set for observations
  ## on unmet need.
  fig.name = NULL, ##<< Used for overview plots only:
  ## If NULL, plot appears in R, else it is saved as fig.name.
  ## Not used if individual country plots are requested using
  ## \code{ind.country.overviewplot} or
  ## \code{ind.country.indplot} below.
  select.c = NULL, ##<< For country plots only, indices of countries to plot
  ind.country.overviewplot = FALSE, ##<< Logical: for each country, overview plot
  ## saved to \code{figdir.indcountries}? (in TIFF format)
  ind.country.indplot = FALSE, ##<< Logical: for each country,
  ## individual plots saved to \code{figdir.indcountries}? (NOT USED YET)
  figdir.indcountries = NULL, ##<< Directory to store country plots, if NULL, directory \code{fig/ind.country.plots}
  ## is created in current working directory.
  categories.to.plot = NULL, ##<< Selected names of categories to plot. If NULL, all available categories are plotted.
  run.name = NULL, ##<< Run name, used only to add to directory name when plotting results for individual countries.
  CI2.Lg.Lcat.qt = NULL, ##<< Add a second set of CIs (optional)
  CIratio2.Lg.Lcat.qt = NULL, ##<< Add a second set of CIs (optional)
  CI3.Lg.Lcat.qt = NULL, ##<< Add a third set of CIs (optional)
  CIratio3.Lg.Lcat.qt = NULL, ##<< Add a third set of CIs (optional)
  CI4.Lg.Lcat.qt = NULL, ##<< Add a fourth set of CIs (optional)
  CIratio4.Lg.Lcat.qt = NULL, ##<< Add a fourth set of CIs (optional)
  name.dir1 = NULL, ##<< Used if CIs2 are added, name used for CIs
  name.dir2 = NULL, ##<< Used if CIs2 are added, name used for CIs2
  name.dir3 = NULL, ##<< Used if CIs3 are added, name used for CIs3
  name.dir4 = NULL, ##<< Used if CIs4 are added, name used for CIs4
  cex.adj.factor = 1, ## Factor to adjust size of plots by
  #  plot.blue.line = FALSE, ##<< add main trends? only if next one not null
  shiny = FALSE ##<< \code{TRUE} if plot function is used for Shiny FPET app
 ,sources.pal = c("#E41A1C", "#377EB8", "#FF7F00", "#A65628", "#4DAF4A", "#984EA3", "#F781BF", "black")[c(1,2,5,3,8,6,4,7)] ##[MCW-2016-04-05-16] Added to allow selection of colour palette for sources.
 ,cex.symbols = list(SS = 1.5, add.info = 6, no.info = 4) ##[MCW-2016-04-12-1] Added to allow control of size of plotting symbols
  ,non.std.symbol = 22 ##[MCW-2016-04-12-19] Added to allow control of plotting symbol for non-standard observations.
  ,UWRA = FALSE       #[MCW-2016-08-31-2] :: Added this argument to enable changes for unmarried runs.
 ,all.women = FALSE #[MCW-2017-01-25-8] :: Added this argument to enable changes for plotting all-women results.
  ,all.womenize.fig.name = isTRUE(all.women) #Make sure figure names have 'aw' at front if 'isTRUE(all.women)'.
  ## [MCW-2017-01-25-1] :: Argument to allow alternative way of providing data
  ## points to plot in the CI plots. This was added because PDU wanted to plot
  ## 'all women' survey data on the all women plots created by combining
  ## trajectories from married and unmarried women runs.
 ,data.to.plot = NULL
 ,data.to.plot.regioninfo  =NULL            #need this as well
 ,pdf.useDingbats = TRUE
 ,country.info = NULL          #Need this to exclude some countries. Non-null only if data.raw is NULL.
 ,fp2020.69.only = FALSE
 ,hide.CP.tot.lt.1pc = FALSE
 ,select.c.csv = NULL #MCW-2018-03-23:: CSV file with ISO codes of countries to be included in plots
 ,plot.in.order.of.input = FALSE # Plot countries in same order as input file?
  ,age.group = NULL
  )
{
    if (!is.null(data.raw)){ # for country plots only
        data <- data.raw$data
        country.info <- data.raw$country.info
        region.info <- data.raw$region.info
        if(UWRA) {
            mainlong.g <- paste0(makeCountryNames(country.info$name.c),
                                 " (",  makeRegNames(country.info$namesubreg.c),
                                 ifelse(country.info$sex.ac.unm.c == 1
                                       ,", SA Group 0", ", SA Group 1"),
                                 ")")
        } else if(all.women) {
            mainlong.g <- makeCountryNames(names(CI.Lg.Lcat.qt))
        } else {
            mainlong.g <- paste0(makeCountryNames(country.info$name.c),
                                 " (",  makeRegNames(country.info$namesubreg.c), ")")
        }

        ## for validation exercise unmet need, only plot countries where data were left out
    } else {
        mainlong.g <- names(CI.Lg.Lcat.qt)  #country names
        country.info <- country.info
    }
    name.g <- mainlong.g

    G <- length(name.g)

    ## [MCW-2017-01-25-2] :: Use input data points in the plots.
    if(!is.null(data.to.plot)) {
        data.to.plot <- ReadDataAll(data.csv = data.to.plot
                                   ,regioninfo.csv = data.to.plot.regioninfo
                                   ,do.SS.run.first.pass = FALSE
                                   ,include.c.no.data = TRUE #YES!
                                   ,disagg.RN.PMA = TRUE
                                    )
        data <- data.to.plot$data
        country.info <- data.to.plot$country.info
        if(!is.null(data.to.plot$country.info.no.data)) {
            country.info <- rbind(country.info, data.to.plot$country.info.no.data)
        }

        ## Keep only those countries in 'mainlong.g' but warn before removing.
        if(sum(data$name.j %in% mainlong.g) < length(data$name.j)) {
            warning(paste("The following country names in 'data.to.plot' are not in the MCMC output:\n    "
                         ,paste(unique(data$name.j[!data$name.j %in% mainlong.g]), collapse = ", ")
                          ))
        }
        data <- data[data$name.j %in% mainlong.g,]

        if(sum(country.info$name.c %in% mainlong.g) < length(country.info$name.c)) {
            warning(paste("The following country names in 'data.to.plot.regioninfo' are not in the MCMC output:\n    "
                         ,paste(unique(country.info$name.c[!country.info$name.c %in% mainlong.g]
                                      ,collapse = ", ")
                                )
                          ))
        }

        ## Will fail if not all countries in 'mainlong.g' are in 'country.info'
        if(sum(mainlong.g %in% country.info$name.c) < length(mainlong.g)) {
            stop(paste("All countries in the MCMC output must be in 'data.to.plot.regioninfo'. The following are missing:\n    "
                      ,paste(mainlong.g[!(mainlong.g %in% country.info$name.c)], collapse = ", ")
                       ))
        }

        ## Need these columns to make plotting symbols work
        if(is.null(data$rounded.up)) data$rounded.up <- FALSE
        if(is.null(data$poptype.j)) data$poptype.j <- "MW"
        if(is.null(data$geo.j)) data$geo.j <- ""
        if(is.null(data$age.cat.j)) data$age.cat.j <- "0"
        if(is.null(data$posbias.j)) data$posbias.j <- ""
        if(is.null(data$mod.bias.j)) data$mod.bias.j <- ""
        if(is.null(data$folkbias.j)) data$folkbias.j <- ""

        ## Order in same way as mainlong.g
        idx <-
            unlist(sapply(mainlong.g
                         ,function(z, x) { if(z %in% x) which(x == z) }
                         ,x = data$name.j
                          ))
        data <- data[idx,]

        idx <-
            unlist(sapply(mainlong.g
                         ,function(z, x) { if(z %in% x) which(x == z) }
                         ,x = country.info$name.c
                          ))
        country.info <- country.info[idx,]
    }

    if(UWRA && all.women) warning("'UWRA' and 'all.women' both true: assuming you meant 'all.women'")
    ## [MCW-2016-08-31-22] :: Add 'Unmarried' to plot titles.
    if(all.women)  {
        ## [MCW-2017-01-25-9] :: Add 'All women' to plots for all women results.
        mainlong.g <- paste0(mainlong.g, " --- All women")
    } else if(UWRA) { mainlong.g <- paste0(mainlong.g, " --- Unmarried / Not In-Union")
    } else {
        mainlong.g <- paste0(mainlong.g, " --- Married / In-Union")
    }

    fig.name.years <- ifelse(!is.null(start.year) | !is.null(end.year),
                             paste0("_",
                                    ifelse(!is.null(start.year), paste0("from", floor(start.year)), ""),
                                    ifelse(!is.null(end.year), paste0("to", floor(end.year)), "")),
                             "")
    if (is.null(start.year)){
        if (!is.null(CI.Lg.Lcat.qt)){
            est.years <- as.numeric(names(CI.Lg.Lcat.qt[[1]][[1]][1,]))
            start.year <- est.years[1]
        } else {
            start.year <- 1990.5
        }
    }
    if (is.null(end.year)){
        if (!is.null(CI.Lg.Lcat.qt)){
            ## LAchange20140610: add estyears here again (in case start year was not NULL)
            est.years <- as.numeric(names(CI.Lg.Lcat.qt[[1]][[1]][1,]))
            end.year <- est.years[length(est.years)]
        } else {
            end.year <- 2015.5
        }
    }
    xmin <- start.year #min(years.i, start.year, na.rm = T)
    xmax <- end.year

    if (ind.country.overviewplot || ind.country.indplot){
        if (is.null(figdir.indcountries)){
            figdir.indcountries <- file.path(getwd(), "fig", paste0("ind.country.plots_", run.name))
            dir.create(file.path(getwd(), "fig"), showWarnings = FALSE)
        }
        dir.create(figdir.indcountries, showWarnings = FALSE)
        if (ind.country.overviewplot & ind.country.indplot){
                                # ##details<< If \code{ind.country.indplot} and \code{ind.country.indplot} are both TRUE,
                                # ## currently, \code{ind.country.indplot} is used.
            ind.country.overviewplot <- FALSE
        }
    } else {
        if (!is.null(fig.name)) {
            if(all.women && all.womenize.fig.name) {
                ## [MCW-2018-01-24] :: Ensure filenames have "aw" in them, even
                ## if ~fig.name~ does not have "umw" or "mw" in it.
                base.fig.name <- head(strsplit(basename(fig.name), "\\.")[[1]], -1)
                aw.base.fig.name <- makeAWFileName(base.fig.name)
                aw.fig.name <- file.path(dirname(fig.name), paste0(aw.base.fig.name, ".pdf"))
            } else aw.fig.name <- fig.name
            pdf(aw.fig.name, width = 21, height = 12, useDingbats = pdf.useDingbats)
        } else stop("'fig.name' is NULL with no default.")
    }

    if (!is.null(select.c)){
        gseq <- select.c
        if(!is.null(select.c.csv)) warning("Both 'select.c' and 'select.c.csv' have been specified. 'select.c.csv' is ignored.")
    } else if(!is.null(select.c.csv)) {
        read.select.c.csv <- read.csv(select.c.csv)
        ISO.colname <- grep("^ISO|^ISO Code", colnames(read.select.c.csv), ignore.case = TRUE, value = TRUE)
        if(identical(length(ISO.colname), 0L)) stop("'select.c.csv' does not have a column called 'ISO' or 'ISO Code' (ignoring case).")
        else if(!is.numeric(read.select.c.csv[[ISO.colname]])) stop("Column ", ISO.colname, " in 'select.c.csv' is not numeric.")
        if(all.women) {
            gseq <- which(name.g %in% country.info$name.c[country.info$iso.c %in% read.select.c.csv[[ISO.colname]]])
        } else {
            gseq <- which(country.info$iso.c %in% read.select.c.csv[[ISO.colname]])
        }
        if(!plot.in.order.of.input) {
            gseq <- na.omit(gseq[order(name.g[gseq])])
            message("Plotting countries in alphabetical order.")
        }
    } else {
        gseq <- 1:G
        ## [MCW-2016-08-26-12] Re-order for plotting if countries with no data estimated.
        ## UPDATE: (2017-01-24) This only works if data.raw is supplied as an
        ## argument! It is if this function is called from PlotResults().
        if(plot.in.order.of.input && !is.null(data.raw$input.order.c)) {
            gseq <- na.omit(gseq[data.raw$input.order.c])
            message("Plotting countries in re-computed order.")
        } else {
            ## Plot in alphabetical order
            gseq <- na.omit(gseq[order(name.g[gseq])])
            message("Plotting countries in alphabetical order.")
        }

        if(fp2020.69.only) {
            if(!is.null(select.c) || !is.null(select.c.csv)) {
                warning("'fp2020.69.only' has been set along with 'select.c' or 'select.c.csv'. The set of countries plotted is the intersection of 'fp2020 69' and 'select.c' or 'select.c.csv'.")
            }
        gseq <-
            gseq[country.info[gseq, "iso.c", drop = TRUE] %in%
                 get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD")]
        }
    }

    ## Record which country is on which page of the PDF
    country.page.df <- data.frame()

    for (g in gseq){

        ## what are we going to plot?
        ## as a start, old set-up
        cats.to.plot <- categories.to.plot #passed in as an argument
        tot.CP <- CI.Lg.Lcat.qt[[g]][["Total"]]

        if(identical(length(na.omit(tot.CP)), 0L)) {
            message(names(CI.Lg.Lcat.qt)[g], " has no estimates: skipping.")
            next()
            }

        tot.CP <-
            tot.CP["0.5"
                  ,which(colnames(tot.CP) == start.year):which(colnames(tot.CP) == end.year)
                   ]

        ## change for Shiny
            if (!shiny) {
                if (!is.null(CIratio.Lg.Lcat.qt)){
                    cats <- c("Total", "Modern", "Traditional", # "Modern/Total", [MCW-2016-04-26-6] Removed
                              "Unmet", "TotalPlusUnmet", "Met Demand"
                             ,"Met Demand with Modern Methods") #[MCW-2016-04-26-7] added
                    cats.from.ratio <- c(F,F,F,F,F,T,T)          #[MCW-MCW-2016-04-26-13] modified to match 'cats'
                } else {
                    cats <- c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet")
                    cats.from.ratio <- c(F,F,F,F,F)
                }
            } else {
                if (!is.null(CIratio.Lg.Lcat.qt)){
                    cats <- c("Total", "Modern", "Traditional", "Modern/Total",
                              "TradPlusUnmet", "TotalPlusUnmet", "Met Demand with Modern Methods",
                              "Unmet", "TotalPlusUnmet", "Met Demand")
                    cats.from.ratio <- c(F,F,F,T,F,F,T,F,F,T)
                } else {
                    cats <- c("Total", "Modern", "Traditional", "TradPlusUnmet", "Unmet",
                              "TotalPlusUnmet")
                    cats.from.ratio <- c(F,F,F,F,F,F)
                }
            }

        if (is.null(cats.to.plot)) cats.to.plot <- cats
        if (is.null(CIratio.Lg.Lcat.qt))
            cats.to.plot <- cats.to.plot[!(cats.to.plot %in%
                                                       c("Modern/Total", "Met Demand with Modern Methods"
                                                       , "Met Demand"))]
        ## 2018-03-02 :: Don't plot Met demand (mod or any) if SA Low country (UWRA only)
        if(UWRA) {
            if(identical(as.double(country.info[g, "sex.ac.unm.c"]), 1) ||
               isTRUE(country.info[g, "sex.ac.unm.c"])) {
                cats.to.plot <- cats.to.plot[!(cats.to.plot %in%
                                               c("Met Demand with Modern Methods"
                                               , "Met Demand"))]
            }
        }
        cats.select <- match(cats.to.plot, cats)
        axisnamecats <- cats
        axisnamecats <- ifelse(axisnamecats=="Total", "CP (any)",
                               paste(axisnamecats))
        axisnamecats <- ifelse(axisnamecats=="Modern", "CP (modern)", paste(axisnamecats))
        axisnamecats <- ifelse(axisnamecats=="Traditional", "CP (traditional)", paste(axisnamecats))
        axisnamecats <- ifelse(axisnamecats=="Modern/Total",
                               "Ratio (modern/any method)",
                               paste(axisnamecats))
        axisnamecats <- ifelse(axisnamecats=="TradPlusUnmet",
                               "Unmet need for\nmodern methods", paste(axisnamecats))
        axisnamecats <- ifelse(axisnamecats=="TotalPlusUnmet",
                               "Total demand", paste(axisnamecats))
        axisnamecats <- ifelse(axisnamecats=="Met Demand with Modern Methods",
                               "Demand satisfied\n(modern methods)", paste(axisnamecats))
        ## [MCW-2016-08-31-24] :: Add 'among all unmarried women' to panel 'Unmet need' if 'UWRA = TRUE'.
        if(UWRA) {
            axisnamecats <- ifelse(axisnamecats=="Unmet", "Unmet need\namong all unmarried women"
                                 , paste(axisnamecats))
        } else {
            axisnamecats <- ifelse(axisnamecats=="Unmet", "Unmet need", paste(axisnamecats))
        }
        axisnamecats <- ifelse(axisnamecats=="Met Demand",
                               "Demand satisfied\n(any method)", paste(axisnamecats)) #[MCW-2016-04-26] Changed (added '\n(any method)')
        ##axisnamecats <- ifelse(axisnamecats=="", , paste(axisnamecats))
        ## cats.from.ratio refers to taking it from CI or CIratio

        ## ##details<< If \code{ind.country.overviewplot}, plots saved in \code{figdir.indcountries}
        ## if (ind.country.overviewplot)
        ##   pdf(file.path(figdir.indcountries, paste0(country.info$name.c[g], fig.name.years, ".pdf")), width = 21, height = 12)
        if (ind.country.overviewplot)
            tiff(file.path(figdir.indcountries, paste0(name.g[g], fig.name.years, ".tif")), width = 21, height = 12,
                 pointsize=6,res=300, bg="white",compression="lzw")
        ## [MCW-2017-01-25-3] :: Also now executed 'if(!null(data.to.plot))'.
        if ((!is.null(data.raw)) || (!is.null(data.to.plot))) { # for country plots only
            select <- seq(1, length(data[,1]))[is.element(data$iso.j, country.info$iso.c[g])]
            years.i <- data$years.j[select]
            include.SS <- ifelse(any(data$source.j[select]=="SS"), TRUE, FALSE)

            if(!UWRA) {

                ## Married women ----------------------------------------------

                if (add.info) {
                    pch.i <- ifelse(data$source.j[select] == "SS", 19,
                             ifelse(data$poptype.j[select]=="SA",24,
                             ifelse(data$poptype.j[select]=="EM"|data$poptype.j[select]=="AL",25,
                             ifelse(data$poptype.j[select]=="HW"|data$poptype.j[select]=="BS",22,
                                    ## Rounded values
                             ifelse(data$rounded.up[select], 21
                                # [MCW-2017-09-05-11] Rounded values no longer
                                # plotted with a different symbol. Leave the
                                # code mostly in tact, though, in case want to
                                # reverse this.
                                  , 21)))))
                } else {
                    pch.i <- ifelse(data$rounded.up[select], 21,
                                # [MCW-2017-09-05-11] Rounded values no longer
                                # plotted with a different symbol. Leave the
                                # code mostly in tact, though, in case want to
                                # reverse this.
                             ifelse(data$source.j[select] == "SS", 19,
                             ifelse(data$poptype.j[select] == "MW" & data$geo.j[select] == "" &
                                    data$age.cat.j[select] == "0" & data$posbias.j[select] == "" &
                                    data$mod.bias.j[select] == "" & data$folkbias.j[select] == "",
                                    21, # standard
                                    non.std.symbol))) #[MCW-2016-04-12-21]
                }

            } else {

                ## Unmarried women ----------------------------------------------

                if (add.info) {
                    if(hide.CP.tot.lt.1pc) {
                        pch.i <- ifelse(data$source.j[select] == "SS", 19,
                                #ifelse(data$poptype.j[select]=="SA",24,
                                 ifelse(data$poptype.j[select]=="EM"|data$poptype.j[select]=="AL"|data$poptype.j[select]=="FM",25,
                                # now 'formerly married'
                                 ifelse(data$poptype.j[select]=="HW"|data$poptype.j[select]=="BS"|data$poptype.j[select]=="PW",22,
                                # now 'with a partner'
                                 ifelse(data$less.than.1.pc[select], 21,
                                 ifelse(data$rounded.up[select], 21
                                # [MCW-2017-09-05-11] Rounded values no longer
                                # plotted with a different symbol. Leave the
                                # code mostly in tact, though, in case want to
                                # reverse this.
                                      , 21)))))
                    } else {
                        pch.i <- ifelse(data$source.j[select] == "SS", 19,
                                #ifelse(data$poptype.j[select]=="SA",24,
                                 ifelse(data$poptype.j[select]=="EM"|data$poptype.j[select]=="AL"|data$poptype.j[select]=="FM",25,
                                # now 'formerly married'
                                 ifelse(data$poptype.j[select]=="HW"|data$poptype.j[select]=="BS"|data$poptype.j[select]=="PW",22,
                                # now 'with a partner'
                                 ifelse(data$less.than.1.pc[select], 23,
                                 ifelse(data$rounded.up[select], 21
                                # [MCW-2017-09-05-11] Rounded values no longer
                                # plotted with a different symbol. Leave the
                                # code mostly in tact, though, in case want to
                                # reverse this.
                                      , 21)))))
                    }
                } else {
                    pch.i <- ifelse(data$rounded.up[select], 21,
                                # [MCW-2017-09-05-11] Rounded values no longer
                                # plotted with a different symbol. Leave the
                                # code mostly in tact, though, in case want to
                                # reverse this.
                             ifelse(data$source.j[select] == "SS", 19,
                             ifelse(data$poptype.j[select] == "UW" & data$geo.j[select] == "" &
                                    data$age.cat.j[select] == "0" & data$posbias.j[select] == "" &
                                    data$mod.bias.j[select] == "" & data$folkbias.j[select] == "",
                                    21, # standard
                                    non.std.symbol))) #[MCW-2016-04-12-21]
                }
            }
            ## [MCW-2016-04-12-2] modified to use 'cex.symbols' instead of hardcoded sizes.
            cex.i <- ifelse(data$source.j[select]=="SS", cex.symbols$SS,
                     ifelse(add.info, cex.symbols$add.info, cex.symbols$no.info))

     #[MCW-2016-04-22-3]: Legend if no RN surveys in data (only option as of 2017-09-05).
            col.i <- ifelse(data$source.j[select]=="DHS",sources.pal[1],
                     ifelse(data$source.j[select]=="MICS",sources.pal[2],
                     ifelse(data$source.j[select]=="NS",sources.pal[4],
                            ## [MCW-2016-03-10-3] added extra data sources. NB
                            ## service statistics still 'sources.pal[5]';
                            ## 'sources.pal[3]' is "Other international survey".
                             ifelse(data$source.j[select]=="PMA",sources.pal[6],
                             ifelse(data$source.j[select]=="SS",sources.pal[5],sources.pal[3])))))
            bg.i <- bg.unmet.i <- rep("white", length(select))
            ## do unmet seperately, because validation can be different

            if (!is.null(getj.test.k )){
                ## validation exercise, use grey for obs that were left out
                bg.i <- ifelse(is.element(select, getj.test.k), "darkgrey", bg.i)
            }
            if (!is.null(getj.test.unmet.k )){ # for unmet only exercise
                ## note that other props show up as if there were left out as well
                bg.i <- ifelse(is.element(select, getj.test.unmet.k), "darkgrey", bg.i)
                                #[MCW-2017-03-16-1] changed.
            }
            ## end data.raw loop
            nplots <- length(cats)+1 # +1 for legend

        } else {
            nplots <- length(cats)
            include.SS <- FALSE
        }

        ## nplots <- 8
        ## par(mfrow = c(2,ceiling(nplots/2)), mar = c(5,5,3,1), cex.main = 2, cex.axis = 2, cex.lab = 2)
        nrows <- ifelse(shiny, 3, 2)
        ncols <- max(ceiling(nplots/nrows), ifelse(plot.prop, 4, 3))
        if (shiny & length(cats.select) == 10) {
            nf <- layout(rbind(rep(1, ncols),
            (1:ncols) + 1,
            c(rep(ncols + 2, ncols - 1), ncols + 3),
            (1:ncols) + ncols + 3,
            c(rep(2*ncols + 4, ncols -1), 2*ncols + 5),
            (1:ncols) + 2*ncols + 5),
            widths = rep(1.5, ncols),
            heights = c(0.25, 1.5, 0.2, 1.5, 0.2, 1.5), TRUE)
        } else {
            nf <- layout(rbind(rep(1, ncols),
                               matrix(seq(2, nrows*ncols+1), nrows, ncols, byrow = TRUE)),
                         widths = rep(1.5, ncols),
                         heights = c(0.25, rep(1.5, nrows)), TRUE)
        }
        ## layout.show(nf)
        InternalPlotTitle(title = paste(mainlong.g[g], "   "), position = "center", cex = 3.2*cex.adj.factor)
        legend.select.comparison <- c(TRUE, !is.null(CI2.Lg.Lcat.qt[[g]]),
                                      !is.null(CI3.Lg.Lcat.qt[[g]]), !is.null(CI4.Lg.Lcat.qt[[g]]))
        if (sum(legend.select.comparison) > 1)
            legend("right", legend = c(name.dir1, name.dir2, name.dir3, name.dir4)[legend.select.comparison],
                   col = c("red","blue","green","#984EA3")[legend.select.comparison], lwd = 3, cex = 1.5*cex.adj.factor)

        ## par( mar = c(7,6,6,1), cex.main = 2, cex.axis = 2, cex.lab = 2)
        for (cat in (1:length(cats))[cats.select]) {
            if (!is.null(CI.Lg.Lcat.qt)){
                if (!is.null(CIstar.Lg.Lcat.qt[[g]][[cats[cat]]])){
                    CIstar.qt <- CIstar.Lg.Lcat.qt[[g]][[cats[cat]]]
                } else {
                    CIstar.qt <- NULL
                }
                if (!cats.from.ratio[cat]){
                    est.years <- as.numeric(names(CI.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                    CI.qt <- CI.Lg.Lcat.qt[[g]][[cats[cat]]]
                    CI.qt[, est.years < start.year | est.years > end.year] <- NA
                    if (!is.null(CI2.Lg.Lcat.qt)) {
                        est.years2 <- as.numeric(names(CI2.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                        CI2.qt <- CI2.Lg.Lcat.qt[[g]][[cats[cat]]]
                        CI2.qt[, est.years2 < start.year | est.years2 > end.year] <- NA
                    } else {
                        CI2.qt <- NULL
                    }
                    if (!is.null(CI3.Lg.Lcat.qt)) {
                        est.years3 <- as.numeric(names(CI3.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                        CI3.qt <- CI3.Lg.Lcat.qt[[g]][[cats[cat]]]
                        CI3.qt[, est.years3 < start.year | est.years3 > end.year] <- NA
                    } else {
                        CI3.qt <- NULL
                    }
                    if (!is.null(CI4.Lg.Lcat.qt)) {
                        est.years4 <- as.numeric(names(CI4.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                        CI4.qt <- CI4.Lg.Lcat.qt[[g]][[cats[cat]]]
                        CI4.qt[, est.years4 < start.year | est.years4 > end.year] <- NA
                    } else {
                        CI4.qt <- NULL
                    }
                } else {
                    est.years <- as.numeric(names(CIratio.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                    CI.qt <- CIratio.Lg.Lcat.qt[[g]][[cats[cat]]]
                    CI.qt[, est.years < start.year | est.years > end.year] <- NA
                    if (!is.null(CIratio2.Lg.Lcat.qt)) {
                        est.years2 <- as.numeric(names(CIratio2.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                        CI2.qt <- CIratio2.Lg.Lcat.qt[[g]][[cats[cat]]]
                        CI2.qt[, est.years2 < start.year | est.years2 > end.year] <- NA
                    } else {
                        CI2.qt <- NULL
                    }
                    if (!is.null(CIratio3.Lg.Lcat.qt)) {
                        est.years3 <- as.numeric(names(CIratio3.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                        CI3.qt <- CIratio3.Lg.Lcat.qt[[g]][[cats[cat]]]
                        CI3.qt[, est.years3 < start.year | est.years3 > end.year] <- NA
                    } else {
                        CI3.qt <- NULL
                    }
                    if (!is.null(CIratio4.Lg.Lcat.qt)) {
                        est.years4 <- as.numeric(names(CIratio4.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
                        CI4.qt <- CIratio4.Lg.Lcat.qt[[g]][[cats[cat]]]
                        CI4.qt[, est.years4 < start.year | est.years4 > end.year] <- NA
                    } else {
                        CI4.qt <- NULL
                    }
                }
            }
            name.cat <- cats[cat]
            if (ind.country.indplot){
                par(mfrow = c(1,1))
                pdf(file.path(figdir.indcountries,
                              paste0(name.g[g], "_total", fig.name.years, ".pdf")), width = 12, height = 12
                   ,useDingbats = pdf.useDingbats)
                main.extra <- name.g[g]
            } else {
                main.extra <- ifelse(cat==2, mainlong.g[g], "")
            }

            ## actual plotting for that category
            par(mar = c(6, ifelse(cex.adj.factor == 1, 6, 4), 5, 1),
                cex.main = 2.5*cex.adj.factor, cex.axis = 2*cex.adj.factor, cex.lab = 2*cex.adj.factor)

            ## [MCW-2017-01-25-4] :: Make vectors to plot points 'if(!is.null(data.to.plot))'.
            if ((!is.null(data.raw)) || (!is.null(data.to.plot))){
                props.j <- NULL
                trad <- modern <- FALSE
                if (name.cat=="Total") props.j <- data$props.tot.j
                if (name.cat=="Modern/Total") props.j <- data$props.modern.j/data$props.tot.j
                if (name.cat=="TradPlusUnmet") props.j <- data$props.trad.j + data$props.unmet.j
                if (name.cat=="TotalPlusUnmet") props.j <- data$props.tot.j + data$props.unmet.j
                if (name.cat=="Met Demand with Modern Methods") props.j <-
                                                                    data$props.modern.j/(data$props.tot.j + data$props.unmet.j)
                if (name.cat=="Unmet") props.j <- data$props.unmet.j
                if (name.cat=="Met Demand") props.j <- data$props.tot.j/(data$props.tot.j + data$props.unmet.j)

                if (name.cat=="Modern") {
                    props.j <- data$props.modern.j
                    modern <- TRUE
                }
                if (name.cat=="Traditional") {
                    props.j <- data$props.trad.j
                    trad <- TRUE
                }
                if (sum(!is.na(props.j[select]))>0) {
                    data.props <- props.j[select]
                    if (xmin > 1989)
                        data.props <- data.props[years.i > 1989]
                    data.props <- data.props[!is.na(data.props)]
                } else {
                    data.props <- NULL
                }
            } else {
                data.props <- NULL
            }
            ## Set y axis limits
            yall <- c(CI.qt, CI2.qt, CI3.qt, CI4.qt, data.props)
            ymin <- 0           #default case
            if((is.logical(ymin.at.0) && !isTRUE(ymin.at.0)) || ymin.at.0 == "CIs") {
                ymin <- ifelse(min(yall, na.rm = T) < 0.05, 0,
                               min(yall, na.rm = T)-0.2*diff(range(yall, na.rm = T)))
            } else if(ymin.at.0 == "data" && !is.null(data.props)) {
                ymin <- ifelse(min(data.props, na.rm = T) < 0.05, 0,
                               min(data.props, na.rm = T)-0.2*diff(range(data.props, na.rm = T)))
            }
            if(plot.prop) {
            ymax <- 1           #default case
            if((is.logical(ymax.at.100) && !isTRUE(ymax.at.100)) || ymax.at.100 == "CIs") {
                ymax <- max(yall, na.rm = T)+0.2*diff(range(yall, na.rm = T))
            } else if(ymax.at.100 == "data" && !is.null(data.props)) {
                ymax <- max(data.props, na.rm = T)+0.2*diff(range(data.props, na.rm = T))
            }
            } else {
                ymax <- max(yall, na.rm = T)+0.2*diff(range(yall, na.rm = T))
            }
            InternalPlotEmpty(ylab = ifelse(plot.prop,"", "Count"),
                              main = axisnamecats[cat],
                              xlim = c(xmin, xmax),
                              ylim = c(ymin, ymax),
                              plot.prop = plot.prop)
            ## NOTE: might want to change xlim for incl earlier data
            if (!is.null(CI2.Lg.Lcat.qt) | !is.null(CI3.Lg.Lcat.qt) | !is.null(CI4.Lg.Lcat.qt)) {
                if (!is.null(CI4.qt))
                    InternalPlotCIs(CIs.qt = CI4.qt, col.median = "#984EA3", seq.years = est.years4,
                                    col95 = "grey60", density = 15, angle = 90) #[MCW-2016-04-26-8] Changed
                if (!is.null(CI3.qt))
                    InternalPlotCIs(CIs.qt = CI3.qt, col.median = "green", seq.years = est.years3,
                                    col95 = "lightgreen", density = 15, angle = 0) #[MCW-2016-04-26-9] Changed
                if (!is.null(CI2.qt))
                    InternalPlotCIs(CIs.qt = CI2.qt, col.median = "blue", seq.years = est.years2,
                                    col95 = "lightblue", density = 15, angle = -45) #[MCW-2016-04-26-10] Changed
                InternalPlotCIs(CIs.qt = CI.qt, col.median = "red", seq.years = est.years,
                                col95 = "lightpink", density = 15, angle = 45) #[MCW-2016-04-26-11] Changed
            } else {
                if (!is.null(CI.Lg.Lcat.qt)){
                    InternalPlotCIs(CIs.qt = CI.qt, col.median = 1, #col_pxmr[1],
                                    seq.years =  est.years, col95 =  "grey90", #[MCW-2016-04-26-12] Changed
                                    CIs.star.qt = CIstar.qt)
                }
            }

            ## [MCW-2017-01-25-5] :: Plot points if 'if(!is.null(data.to.plot))'.
            if ((!is.null(data.raw)) || (!is.null(data.to.plot))){
                if (sum(!is.na(props.j[select]))>0) {
                    if (ind.country.overviewplot) {
                        cexuse.i <- cex.i*0.6
                    } else {
                        cexuse.i <- cex.i
                    }
                    InternalPlotData(props.i = props.j[select], years.i,
                                     col.i = col.i, pch.i = pch.i, cex.i = cexuse.i,
                                     add.info = add.info, data = data, select = select, bg.i = bg.i,
                                     trad = trad, modern = modern, cex.adj.factor = cex.adj.factor
                                    ,sources.pal = sources.pal
                                     )
                    if (min(years.i[!is.na(props.j[select])]) < 1990 & xmin > 1989){
                        text(xmin-1, ymax,"*Data incl. before 1990", cex = 1.7*cex.adj.factor, pos = 4)
                    }
                }
            }
            if (!is.null(par.ciq) & name.cat=="Total") {
                UWRA.sub <- UWRA || identical(age.group, "15-19")
                InternalPlotParInfoTot(par.ciq[g,,], cex.adj.factor = cex.adj.factor
                                      ,UWRA = UWRA.sub # [MCW-2016-08-31-16] :: Added to pass this through.
                                       )
                }
            if (!is.null(par.ciq) & name.cat=="Modern/Total") {
                UWRA.sub <- UWRA || identical(age.group, "15-19")
                InternalPlotParInfoRat(par.ciq[g,,], cex.adj.factor = cex.adj.factor
                                      ,UWRA = UWRA.sub # [MCW-2016-08-31-19] :: Added to pass this through.
                                       )
                }
            if (ind.country.indplot)
                dev.off()
            if (shiny & length(cats.select) == 10 & name.cat=="Modern/Total") {
                InternalPlotTitle(title = "Unmet need and demand for modern methods",
                                  position = "center", cex = 2.8*cex.adj.factor)
                InternalPlotNull()
            }
            if (shiny & length(cats.select) == 10 & name.cat=="Met Demand with Modern Methods")
                InternalPlotNull()
            if (shiny & length(cats.select) == 10 & name.cat=="Met Demand with Modern Methods") {
                InternalPlotTitle(title = "Unmet need and demand for any method",
                                  position = "center", cex = 2.8*cex.adj.factor)
                InternalPlotNull()
            }
        }

        ## [MCW-2017-01-25-6] :: Plot legend if 'if(!is.null(data.to.plot))'.
        if (!ind.country.indplot && (!is.null(data.raw) || !is.null(data.to.plot))){
            InternalPlotDataLegend(TIFF = ind.country.overviewplot,
                                   add.info = add.info, include.SS = include.SS,
                                   cex.adj.factor = cex.adj.factor
                                   ,sources.pal = sources.pal
                                  ,non.std.symbol = non.std.symbol
                                  ,UWRA = UWRA
                                   ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                                   )}
        if (ind.country.overviewplot)
            dev.off()

        country.page.df <-
            rbind(country.page.df
                 ,data.frame(internal.c.index = g, title = mainlong.g[g], name = names(CI.Lg.Lcat.qt)[g])
                  )
    } # end country loop

    if (ind.country.indplot){
        pdf(file.path(figdir.indcountries, paste0("legend", fig.name.years, ".pdf")), width = 12, height = 12
           ,useDingbats = pdf.useDingbats) # change JR, 20140418
        InternalPlotDataLegend(add.info = add.info, include.SS = include.SS, cex.adj.factor = cex.adj.factor
                               ,sources.pal = sources.pal
                              ,non.std.symbol = non.std.symbol #[MCW-2016-04-12-24] Added to pass through value of 'non.std.symbol'.
                                   ,UWRA = UWRA
                                   ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                               )
        dev.off()
    }
    if (!is.null(fig.name) & !(ind.country.overviewplot | ind.country.indplot)) {
        dev.off()
    }

    if (!shiny) {
      ## Save page / title list
      country.page.df$page <- 1:nrow(country.page.df)
      fig.name.split <- unlist(strsplit(fig.name, .Platform$file.sep))
      fig.name.dir <-
          paste(head(fig.name.split, -1), collapse = .Platform$file.sep)
      fig.name.name <-
          unlist(strsplit(tail(fig.name.split, 1), "\\."))[1]

        ## [MCW-2018-01-24] :: Ensure ~.csv~ filenames have "aw" in them, even
      ## if ~run.name~ does not have "umw" or "mw" in it.
      if(all.women && all.womenize.fig.name) {
          aw.fig.name.name <- makeAWFileName(fig.name.name)
        } else aw.fig.name.name <- fig.name.name

      write.csv(x = country.page.df
               ,file = file.path(fig.name.dir
                               ,paste0(aw.fig.name.name, "_pages.csv"))
              , row.names = FALSE)
      return()
    }
}
#----------------------------------------------------------------------
InternalPlotTitle <- function(
  title, ##<< Title to plot
  position = "center", ##<< Relative position of title
  cex = 1 ##<< Size adjustment factor
) {
  par(mar = c(0,0,0,0))
  plot(1, type = "n", xlab = "", ylab = "", bty = "n", xaxt = "n", yaxt = "n")
  legend(position, title, cex = cex, bty = "n")
}
#----------------------------------------------------------------------
InternalPlotNull <- function() {# Make a completely empty plot
  ### Make a completely empty plot
  plot(1, type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")
}
#----------------------------------------------------------------------------------
InternalPlotEmpty <- function(# Start empty plot with labeled axes
  ###F Start empty plot with labeled axes
  main = "", ylim = c(0, 1), ylab = "", plot.prop = FALSE,
  xlim = c(1990, 2015), xlab = ""
){
  plot(1, type = "n", xlim = xlim, ylim = ylim, yaxt = "n",
       main = main, xlab = xlab, ylab = ylab,
       xaxt = "n")#, yaxt = "n")#, yaxt = "n")
  abline(h=0)
  #   if (ymax > 1.1){ # counts
  #     axis(2, at = round(seq(0,ymax,length.out = 10)), labels = round(seq(0,ymax/1000,length.out = 10)), las = 2)
# #  } else {
# #    if (ymax > 1.1){ # counts
# #      axis(2, at = round(seq(0,ymax,length.out = 10)), labels = round(seq(0,ymax,length.out = 10)), las = 2)
#       } else {
#         if (ymax > 0.2) axis(2, at = seq(0,1,0.1), labels = seq(0,1,0.1)*100, las = 2)
#         if (ymax <= 0.2 & ymax >0.1) axis(2, at = seq(0,ymax,0.05), labels = seq(0,ymax,0.05)*100, las = 2)
#         if (ymax < 0.1) axis(2, at = seq(0,ymax,0.02), labels = seq(0,ymax,0.02)*100, las = 2)
#       }
#  # }
  # LAchange20140610
  axis(1, at = seq(5*floor(min(xlim)/5), 5*ceiling(max(xlim)/5),5), las = 3)
  x <- axis(2, labels = FALSE)
  if (plot.prop){
    axis(2, at = x, labels = paste(100*x,"%", sep = ""), las = 2)
  } else {
    axis(2)
  }
}
#----------------------------------------------------------------------------------
InternalPlotCIs <- function(# Add CIs to a plot
  ### Add CIs to a plot
  CIs.qt, ##<< q has to refer to (2.5, 10,50,80,97.5)th percentiles
  CIs.star.qt = NULL, ##<< Add main trend if non-NULL
  col.median = 1,
  seq.years, ##<< Estimation years
  col95 = "#0000FF30",##<< color for shaded area to represent 95% CI
  density = NULL,
  angle = 45
  ){
  # adds areas with col95 for 95% CI
  # Note: c(1,5) from CI are used!!!
  nyears <- length(seq.years)
  CI.low.t <- CIs.qt[1,]
  CI.up.t <- CIs.qt[5,]
  for (t in 2:nyears){
    polygon(c(seq.years[t-1], seq.years[t-1], seq.years[t], seq.years[t],seq.years[t-1]),
            c(CI.low.t[t-1], CI.up.t[t-1], CI.up.t[t], CI.low.t[t], CI.low.t[t-1]),
            col=col95, border = NA, density = density, angle = angle)
  }
  for (q in c(2,4)){
    lines(CIs.qt[q,] ~ seq.years, type = "l",  lwd = 2, lty = 2, col = col.median)
  }
  lines(CIs.qt[3,] ~ seq.years, type = "l",  lwd = 2, lty = 1, col = col.median)
  if (!is.null(CIs.star.qt))  lines(CIs.star.qt[3,] ~ seq.years, type = "l", col = "blue", lwd = 3)
}
#----------------------------------------------------------------------------------
InternalPlotData <- function(# Add data to a plot
  ### Add data to a plot
  props.i, ##<< i refers to observation index (selected in indices in vector of length j)
  years.i,
  lwd.i = 3, bg.i = NULL, col.i =1, pch.i = 19, cex.i = 1,
  add.info = FALSE, ##<< Logical: do you want to plot the different symbols to represent source, biases etc?
  data = NULL, ##<< used if \code{add.info}, include object from class \code{data}
  select = NULL, ##<< used if \code{add.info}, info only added for indices in select
  trad = FALSE, ##<< used if \code{add.info}; logical, are you plotting traditional data?
  modern = FALSE, ##<< used if \code{add.info}; logical, are you plotting modern data?
  cex.adj.factor = 1
 ,sources.pal = c("#E41A1C", "#377EB8", "#FF7F00", "#A65628", "#4DAF4A", "#984EA3", "#F781BF", "black")[c(1,2,5,3,8,6,4,7)]
  ,omit.trad.lt.0.001 = FALSE
  ) {

    ## Helper function
    make.text.pos <- function(x, i = 1) {
        if(is.null(x) || identical(as.double(x), 0)) return(NULL)
        else return(x[i])
        }
    ## 2018-03-02:: For trad, don't plot points 0.1% or less.
    if(trad && omit.trad.lt.0.001) {
        if (all(props.i <= 0.001, na.rm = TRUE)) return()
        else if(any(props.i <= 0.001, na.rm = TRUE)) {
        gt.0.001 <- props.i > 0.001
        props.i <- props.i[gt.0.001]
        years.i <- years.i[gt.0.001]
        bg.i <- bg.i[gt.0.001]
        col.i <- col.i[gt.0.001]
        pch.i <- pch.i[gt.0.001]
        cex.i <- cex.i[gt.0.001]
        select <- select[gt.0.001]
        }
    }
    if (sum(!is.na(props.i))!=0) {
    points(props.i ~ years.i, bg = bg.i,
           cex = cex.i*cex.adj.factor, col = col.i, lwd = lwd.i*cex.adj.factor, pch = pch.i)

    if (add.info){
      points(props.i[data$geo.j[select] !=""] ~ years.i[data$geo.j[select] !=""],
             col = sources.pal[length(sources.pal)] #[MCW-2016-04-05-21] To allow for disaggregation of PMA and repeated national sources. Picks up the last element of 'sources.pal'.
           , cex = 1.5*cex.adj.factor, lwd = 5*cex.adj.factor,
             pch = pch.i[data$geo.j[select] !=""])

      ## Annotations. Try to accommodate multiple annotations by changing position of placement.

      ## Count how many annotations per data point for this particular plot
      text.ann.pos <- matrix(0, nrow = length(select), ncol = 4)
      for(k in 1:length(select)) {
          if(isTRUE(data$age.cat.j[select[k]] != 0) &&
             isTRUE(data$age.cat.j[select[k]] != "")) text.ann.pos[k,1] <- 1
          if(isTRUE(data$posbias.j[select[k]] !="")) text.ann.pos[k,2] <- 1
          if(isTRUE(data$mod.bias.j[select[k]] !="")) text.ann.pos[k,3] <- 1
          if(isTRUE(data$folkbias.j[select[k]] !="")) text.ann.pos[k,4] <- 1
      }

      ## Do the plotting
      for(k in 1:length(select)) {

          ## Set-up positions and offsets and cexs
          if(sum(text.ann.pos[k,]) <= 1) {
              ## No re-positioning if 0 or 1 annotation(s)
              text.pos <- NULL  # NB: length(rep(NULL, 4)) == 0 !!
              mult.factor <- 1.75
              text.offset <- 0
          } else {
              text.pos <- 1:4
              mult.factor <- 1.5
              text.offset <- -0.15
          }
          ## age stuff
          if(isTRUE(data$age.cat.j[select[k]] == "?")){
              text.label = ifelse(data$age.cat.j[select[k]] == "?", "A", "")
                      text(props.i[k] ~ years.i[k], labels = text.label, col = 1, cex = mult.factor * cex.adj.factor
                          ,pos = make.text.pos(text.pos, 1), offset = text.offset)
          }
          if(isTRUE(data$age.cat.j[select[k]] == "+")){
              text.label = ifelse(data$age.cat.j[select[k]] == "+", "+", "")
              text(props.i[k] ~ years.i[k], labels = text.label, col = 1, cex = mult.factor * cex.adj.factor
                  ,pos = make.text.pos(text.pos, 1), offset = text.offset)
          }
          if(isTRUE(data$age.cat.j[select[k]] == "-")){
              text.label = ifelse( data$age.cat.j[select[k]]=="-", "-", "")
              text(props.i[k] ~ years.i[k], labels = text.label, col = 1, cex = mult.factor * cex.adj.factor
                  ,pos = make.text.pos(text.pos, 1), offset = text.offset)
          }
          ## positive bias (NB positive bias due to age range same symbol)
          if(isTRUE(data$posbias.j[select[k]]!="") && !isTRUE(data$age.cat.j[select[k]] == "+")) {
              text.label = ifelse(data$posbias.j[select[k]]!="" & data$age.cat.j[select[k]] != "+", "+","")
              text(props.i[k] ~ years.i[k], labels = text.label,col = 1, cex =mult.factor * cex.adj.factor
                  ,pos = make.text.pos(text.pos, 2), offset = text.offset)
          }
          if(modern){
              if(isTRUE(data$mod.bias.j[select[k]]=="+")){
                  text.label = ifelse(data$mod.bias.j[select[k]]=="+","S+","")
                  text(props.i[k] ~ years.i[k], labels = text.label,col = 1, cex =mult.factor * cex.adj.factor
                      ,pos = make.text.pos(text.pos, 3), offset = text.offset)
              }
              if(isTRUE(data$mod.bias.j[select[k]]=="-")){
                  text.label = ifelse(data$mod.bias.j[select[k]]=="-","S-","")
                  text(props.i[k] ~ years.i[k], labels = text.label,col = 1, cex =mult.factor * cex.adj.factor
                      ,pos = make.text.pos(text.pos, 3), offset = text.offset)
              }
          }
          if (trad){
              if(isTRUE(data$folkbias.j[select[k]]!="")){
                  text.label = ifelse(data$folkbias.j[select[k]]!="", "F", "")
                  text(props.i[k] ~ years.i[k], labels = text.label, col = 1, cex =mult.factor * cex.adj.factor
                      ,pos = make.text.pos(text.pos, 4), offset = text.offset)
              }
          }
      }
    }
  }
}
#----------------------------------------------------------------------------------
InternalPlotParInfoTot <- function(#Add information about logistic parameters to plot
  ### Add information about logistic parameters of total to plot
  par.iq, ##<< matrix with CIs from par.iq (constructed using \code{\link{GetParInfo}} for one country
  cex.adj.factor = 1
  ,UWRA = FALSE ##[MCW-2016-08-31-11] ::Added to allow functionality based on marrital
  ){
    ## Rate model does not have 'T.c'
    if("T.c" %in% dimnames(par.iq)[[2]]) {
  abline(v = par.iq["T.c", 2])
  abline(v = par.iq["T.c", c(1,3)], lty = 2)
  }
  abline(h = par.iq["pmax.c", 2])
  abline(h = par.iq["pmax.c", c(1,3)], lty = 2)
  ## [MCW-2016-08-31-12] :: Change location of legend to "topleft" if 'UWRA = TRUE'.
  if(UWRA) leg.loc <- "topleft"
  else leg.loc <- "bottomright"
    if("T.c" %in% dimnames(par.iq)[[2]]) {
  legend(leg.loc, cex = 1.8*cex.adj.factor, legend = c(
    paste0("omega", " = ", round(par.iq["omega.c", 2],2),
          " (", round(par.iq["omega.c", 3] - par.iq["omega.c", 1],2), ")"),
    paste0("T = ", round(par.iq[ "T.c", 2]),
          " (", round(par.iq["T.c", 3] - par.iq["T.c", 1]), ")"),
    paste0("p[max]", " = ", round(par.iq["pmax.c", 2],2),
          " (", round(par.iq["pmax.c", 3] - par.iq["pmax.c", 1],2), ")"))
    )
    } else {
          legend(leg.loc, cex = 1.8*cex.adj.factor, legend = c(
    paste0("omega", " = ", round(par.iq["omega.c", 2],2),
          " (", round(par.iq["omega.c", 3] - par.iq["omega.c", 1],2), ")"),
    paste0("p[max]", " = ", round(par.iq["pmax.c", 2],2),
          " (", round(par.iq["pmax.c", 3] - par.iq["pmax.c", 1],2), ")"))
    )
        }
}

#----------------------------------------------------------------------------------
InternalPlotParInfoRat <- function(#Add information about logistic parameters to plot
  ### Add information about logistic parameters of ratio modern/total to plot
  par.iq, ##<< matrix with CIs from par.ciq (constructed using \code{\link{GetParInfo}} for one country
  cex.adj.factor = 1
  ,UWRA = FALSE ##[MCW-2016-08-31-13] ::Added to allow functionality based on marrital
  ){
  abline(v = par.iq["RT.c", 2])
  abline(v = par.iq["RT.c", c(1,3)], lty = 2)
  abline(h = par.iq["Rmax.c", 2])
  abline(h = par.iq["Rmax.c", c(1,3)], lty = 2)
  ## [MCW-2016-08-31-14] :: Change location of legend to "topleft" if 'UWRA = TRUE'.
  if(UWRA) leg.loc <- "topleft"
  else leg.loc <- "bottomright"
  legend(leg.loc, cex = 1.8*cex.adj.factor, legend = c(
    paste0("R",expression(omega), " = ", round(par.iq["Romega.c", 2],2),
          " (", round(par.iq["Romega.c", 3] - par.iq["Romega.c", 1],2), ")"),
    paste0("RT = ", round(par.iq["RT.c", 2]),
          " (", round(par.iq["RT.c", 3] - par.iq["RT.c", 1]), ")"),
    paste0(expression(R[max]), " = ", round(par.iq["Rmax.c", 2],2),
          " (", round(par.iq["Rmax.c", 3] - par.iq["Rmax.c", 1],2), ")"))
         )
}

#----------------------------------------------------------------------------------
InternalPlotDataLegend <-
    function(# Plot legend with details on biases etc.
### Plot legend with details on biases etc.
             TIFF = FALSE, ##<< TIFF (if false, it's PDF)
             add.info = TRUE,
             include.SS = FALSE,
             cex.adj.factor = 1
            ,sources.pal = c("#E41A1C", "#377EB8", "#FF7F00", "#A65628", "#4DAF4A", "#984EA3", "#F781BF", "black")[c(1,2,5,3,8,6,4,7)]
            ,non.std.symbol = 22 #[MCW-2016-04-12-22] Added to pass through value from PlotDataAndEstimates().
            ,UWRA = FALSE
             ,hide.CP.tot.lt.1pc = FALSE
             )
{
    if (TIFF){
        par(mar = c(0,ifelse(cex.adj.factor == 1, 6, 4),0,1),
            cex.main = 1.5*cex.adj.factor, cex.axis = 1.5*cex.adj.factor, cex.lab = 1.5*cex.adj.factor)
    } else {
        par(mar = c(0,ifelse(cex.adj.factor == 1, 6, 4),0,1),
            cex.main = 1.5*cex.adj.factor, cex.axis = 1.5*cex.adj.factor, cex.lab = 1.5*cex.adj.factor)
    }

    ## Married Women ------------------------------------------------------------

    if(!UWRA) {

        if (add.info) {
            plot(1, type = "n", xaxt = "n", xlab = "", ylab = "", yaxt = "n")
            legend("left", legend = c("DHS", "MICS",
                                      "Other international survey",
                                      "National survey",
                                #"Rep. nat. survey" removed
                                      "PMA",
                                      ifelse(include.SS, "Service statistics", ""), # change JR, 20131122
                                      "Subpopulation", "",
                                      "+:  Higher contraceptive use", # + for age pos and pos bias because of nonpreg etc
                                      "-:  Lower contraceptive use",
                                      "A:  Other age group",
                                      "F:  Folk methods included",
                                      "S-: Sterilization included",
                                      "S+: Sterilization excluded","",
                                      "Married women",
                                      "Sexually active women", "Ever married/All women",
                                      "Both sexes and husband/wives"## ,
                                      ## "Rounded up to 0.1%" <<< REVERSED
                                      ),
                   col = c(sources.pal[1],sources.pal[2],sources.pal[3],sources.pal[4]
                          ,sources.pal[6],
                           ifelse(include.SS, sources.pal[5], "white"),sources.pal[8],"white",
                           rep("grey",6),"white", rep("grey", 4## 5
                                                      )), #[MCW-2016-06-30-15] rep("grey", 4) --> rep("grey", 5) for 'rounded.up' <<< REVERSED
                   bty = "n", cex = ifelse(TIFF, 1.6, 1.5)*cex.adj.factor,
                   lwd = 2.8*cex.adj.factor, lty = -1,
                   ## [MCW-2016-05-04-28] Make number of plot symbols match number of sources.
                   pch = c(rep(21, 5),19,21,21,-1,-1,-1,-1,-1,-1,-1,21,24,25,22
                          ## ,23
                                #[MCW-2016-06-30-16] Added for 'rounded.up' <<< REVERSED
                           ))

        } else {

            plot(1, type = "n", bty = "n", xaxt = "n", xlab = "", ylab = "", yaxt = "n")
            legend("left", legend = c("DHS", "MICS",
                                      "Other international survey",
                                      "National survey",
                                #"Rep. nat. survey" removed
                                      "PMA",
                                      ifelse(include.SS, "Service statistics", ""), # change JR, 20131122
                                      "Non-standard observation"),
                   col = c(sources.pal[1],sources.pal[2],sources.pal[3],sources.pal[4]
                          ,sources.pal[6],
                           ifelse(include.SS, sources.pal[5], "white"),"grey"),
                   bty = "o", cex = ifelse(TIFF, 1.6, 1.5)*cex.adj.factor,
                   lwd = 2.8*cex.adj.factor, lty = -1,
                   ## [MCW-2016-05-04-29] Make number of plot symbols match number of sources.
                   pch = c(rep(21, 5),19,non.std.symbol)) #[MCW-2016-04-12-19] Added to control symbol used for non-standard data sources.
        }

        ## Unmarried Women ------------------------------------------------------------

    } else {## END: '    if(!UWRA) {'

        if (add.info) {
            plot(1, type = "n", xaxt = "n", xlab = "", ylab = "", yaxt = "n")

            ## Write legend out in matrices so can see which colours go with which labels, etc.

            SS.check <- ifelse(include.SS, "Service statistics", "") # service stats
            SS.col <- ifelse(include.SS, sources.pal[5], "white") #ss colour
            leg.sources <-
                rbind(c("DHS"         , "MICS"        , "Other international survey", "National survey", "PMA"         , SS.check)
                     ,c(sources.pal[1], sources.pal[2], sources.pal[3]              , sources.pal[4]   , sources.pal[6], SS.col)
                     ,c(21            , 21            , 21                          , 21               , 21            , 21)
                      )
            leg.subpop <-
                rbind(c("Subpopulation", "")
                     ,c(sources.pal[8] , "white")
                     ,c(19, 21)
                      )
            leg.biases <-
                rbind(c("+:  Higher contraceptive use", "-:  Lower contraceptive use", "A:  Other age group", "F:  Folk methods included", "S-: Sterilization included", "S+: Sterilization excluded", "")
                     ,c("grey"                        , "grey"                       , "grey"               , "grey"                     , "grey"                      , "grey" , "grey")
                     ,c(-1                            ,-1                           , -1                   , -1                         , -1                          ,-1, -1)
                      )
            if(hide.CP.tot.lt.1pc) {
                leg.multip <-
                    rbind(c("Unmarried women", "Formerly married", "Partnered women")
                         ,c("grey"           , "grey"            , "grey"           )
                         ,c(21               , 25                , 22               )
                          )
            legend("left"
                  ,legend = c(leg.sources[1,], leg.subpop[1,], leg.biases[1,], leg.multip[1,])
                  ,col =   c(leg.sources[2,], leg.subpop[2,], leg.biases[2,], leg.multip[2,])
                  ,pch =   as.numeric(c(leg.sources[3,], leg.subpop[3,], leg.biases[3,], leg.multip[3,]))
                  ,bty = "n", cex = ifelse(TIFF, 1.6, 1.5)*cex.adj.factor,
                  ,lwd = 2.8 * cex.adj.factor
                 , lty = -1)
            } else {
                leg.multip <-
                    rbind(c("Unmarried women", "Formerly married", "Partnered women", "CP (any) \u2265 1%")
                         ,c("grey"           , "grey"            , "grey"           , "grey")
                         ,c(21               , 25                , 22               , 23)
                          )
            legend("left"
                  ,legend = c(leg.sources[1,], leg.subpop[1,], leg.biases[1,], leg.multip[1,-ncol(leg.multip)]
                              ,expression(paste("CP (any) ", ""<=`1%`))) # not allowed expression in a matrix :(
                  ,col =   c(leg.sources[2,], leg.subpop[2,], leg.biases[2,], leg.multip[2,])
                  ,pch =   as.numeric(c(leg.sources[3,], leg.subpop[3,], leg.biases[3,], leg.multip[3,]))
                  ,bty = "n", cex = ifelse(TIFF, 1.6, 1.5)*cex.adj.factor,
                  ,lwd = 2.8 * cex.adj.factor
                 , lty = -1)
            }
        } else {
            plot(1, type = "n", bty = "n", xaxt = "n", xlab = "", ylab = "", yaxt = "n")
            legend("left", legend = c("DHS", "MICS",
                                      "Other international survey",
                                      "National survey",
                                #"Rep. nat. survey" removed
                                      "PMA",
                                      ifelse(include.SS, "Service statistics", ""), # change JR, 20131122
                                      "Non-standard observation"),
                   col = c(sources.pal[1],sources.pal[2],sources.pal[3],sources.pal[4]
                          ,sources.pal[6],
                           ifelse(include.SS, sources.pal[5], "white"),"grey"),
                   bty = "o", cex = ifelse(TIFF, 1.6, 1.5)*cex.adj.factor,
                   lwd = 2.8*cex.adj.factor, lty = -1,
                   ## [MCW-2016-05-04-29] Make number of plot symbols match number of sources.
                   pch = c(rep(21, 5),19,non.std.symbol)) #[MCW-2016-04-12-19] Added to control symbol used for non-standard data sources.
        }
    }
}
#----------------------------------------------------------------------------------
BreakLongStrings <- function(# Breaks long strings with newline
  ### Breaks long strings with newline, used for plotting
  vector, ##<< Vector of strings
  max.nchar = 20 ##<< Maximum length of string on one line
) {
  vector.output <- sapply(vector, InternalBreakLongString, max.nchar = max.nchar)
  return(vector.output)
}
#----------------------------------------------------------------------------------
InternalBreakLongString <- function(# Breaks a long string with newline
  string, ##<< String
  max.nchar = 20 ##<< Maximum length of string on one line
) {
  if (nchar(string) > max.nchar) {
    niters <- floor(nchar(string)/max.nchar)
    for (iter in niters:1) {
      max.nchar.temp <- max.nchar*iter
      if (nchar(string) > max.nchar.temp) {
        indices.space <- which(strsplit(string, split = "")[[1]] == " ")
        if (length(indices.space) == 0) break()
        indices <- c(indices.space, nchar(string))
        index.replace <- indices[sum(indices <= max.nchar.temp)]
        substr(string, index.replace, index.replace) <- "\n"
      }
    }
  }
  return(string)
}
#----------------------------------------------------------------------------------
# Not used
# InternalPlotModelLegend <- function(# Plot legend with details on model estimates
#   ){
#   par( mar = c(0,0,0,0), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
#   plot(1, type = "n", xaxt = "n", xlab = "", ylab = "", yaxt = "n")
#      legend("center", legend = c("Legend for Estimates:", "Lines: Median and 80% CIs",
#      "Grey area represents 95% CI",
#      "Blue line: Excl. AR(1)"),
#      col = c("white", "white", "white", "blue"),
#       #bty = "n",
#       cex = 2.5,
#       lwd = 3, lty = 1,
#       pch = c(-1,-1,-1))
#
#   # put back default plot settings
#   par(mar = c(5,5,3,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
# }
#----------------------------------------------------------------------------------




##[MCW-2016-04-26-5]
###-----------------------------------------------------------------------------
### NEW version of PlotDataAndEstiamtes
###
### Mark Wheldon, 2016-06-13
###
### - Has slightly different colour for CIs
### - Has no "Ratio (modern/any method)"
### - Add at the end a graphic for the SDG indicator "Demand satisfied (modern methods)"
### - Change title of existing graphic "Demand satisfied" to "Demand satisfied (any method)"
### - ... Probably other changes I'll do later and forget to list here.
###
## [MCW-2017-01-30-7] :: Renamed. Was called 'PlotDataAndEstimates3()'.
PlotDataAndEstimatesDIAG <- function (# Create overview country/aggregate plots
  ### Create overview plots of estimates of proportions/counts over time
  ### for either
  ### all countries, or a set of aggregates.
  ### Note that all input arguments with default setting NULL are optional
  ### (e.g. data are not plotted if data.raw=NULL).
  data.raw = NULL, ##<< Non-NULL only for country plots: List with \code{data},
  ## \code{country.info} and \code{region.info}.
  start.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
  end.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
  CI.Lg.Lcat.qt = NULL, ##<< Object from class \code{CI.Lg.Lcat.qt} (optional), either a proportion or a count (see next).
  plot.prop = TRUE, ##<< Are the CIs proportions or counts?
  ymin.at.0 = TRUE, ##<< Set lower bound on y-axis at 0?
  ## [MCW-2016-06-13-12] Default for ymax.at.100 is FALSE for this version.
  ymax.at.100 = FALSE, ##<< Set upper bound on y-axis at percent = 100%? Only applies if plot.prop is TRUE.
  add.info = TRUE, ##<< Add information related to misclassification or different populations.
  par.ciq = NULL, ##<< Add info on logistic parameters (optional, relevant for country proportion plots only)
  CIstar.Lg.Lcat.qt = NULL,##<< Systematic curves, object from class \code{CI.Lg.Lcat.qt} (optional, relevant for country proportion plots only)
  CIratio.Lg.Lcat.qt = NULL,##<< Systematic curves, object from class \code{CI.Lg.Lcat.qt} (optional, relevant for proportion plots only)
  validation = FALSE, ##<< Logical, if TRUE, plot observation from test sets in grey.
  getj.test.k = NULL, ##<< Used if \code{validation}, indices of test sets for observations on
  ## traditional and modern use.
  getj.test.unmet.k = NULL,##<< Used if \code{validation}, indices of test set for observations
  ## on unmet need.
  fig.name = NULL, ##<< Used for overview plots only:
  ## If NULL, plot appears in R, else it is saved as fig.name.
  ## Not used if individual country plots are requested using
  ## \code{ind.country.overviewplot} or
  ## \code{ind.country.indplot} below.
  select.c = NULL, ##<< For country plots only, indices of countries to plot
  ind.country.overviewplot = FALSE, ##<< Logical: for each country, overview plot
  ## saved to \code{figdir.indcountries}? (in TIFF format)
  ind.country.indplot = FALSE, ##<< Logical: for each country,
  ## individual plots saved to \code{figdir.indcountries}? (NOT USED YET)
  figdir.indcountries = NULL, ##<< Directory to store country plots, if NULL, directory \code{fig/ind.country.plots}
  ## is created in current working directory.
  categories.to.plot = NULL, ##<< Selected names of categories to plot. If NULL, all available categories are plotted.
  run.name = NULL, ##<< Run name, used only to add to directory name when plotting results for individual countries.
  CI2.Lg.Lcat.qt = NULL, ##<< Add a second set of CIs (optional)
  CIratio2.Lg.Lcat.qt = NULL, ##<< Add a second set of CIs (optional)
  CI3.Lg.Lcat.qt = NULL, ##<< Add a third set of CIs (optional)
  CIratio3.Lg.Lcat.qt = NULL, ##<< Add a third set of CIs (optional)
  CI4.Lg.Lcat.qt = NULL, ##<< Add a fourth set of CIs (optional)
  CIratio4.Lg.Lcat.qt = NULL, ##<< Add a fourth set of CIs (optional)
  name.dir1 = NULL, ##<< Used if CIs2 are added, name used for CIs
  name.dir2 = NULL, ##<< Used if CIs2 are added, name used for CIs2
  name.dir3 = NULL, ##<< Used if CIs3 are added, name used for CIs3
  name.dir4 = NULL, ##<< Used if CIs4 are added, name used for CIs4
  cex.adj.factor = 1, ## Factor to adjust size of plots by
  #  plot.blue.line = FALSE, ##<< add main trends? only if next one not null
  shiny = FALSE ##<< \code{TRUE} if plot function is used for Shiny FPET app
,sources.pal = c("#E41A1C", "#377EB8", "#FF7F00", "#A65628", "#4DAF4A", "#984EA3", "#F781BF", "black")[c(1,2,5,3,8,6,4,7)]##[MCW-2016-04-05-16] Added to allow selection of colour palette for sources.
 ,cex.symbols = list(SS = 1.5, add.info = 6, no.info = 4) ##[MCW-2016-04-12-1] Added to allow control of size of plotting symbols
  ,non.std.symbol = 22 ##[MCW-2016-04-12-19] Added to allow control of plotting symbol for non-standard observations.
 ,UWRA = FALSE       #[MCW-2016-08-31-3] :: Added this argument to enable changes for unmarried runs.
  ,all.women = FALSE #[MCW-2017-01-25-10] :: Added this argument to enable changes for plotting all-women results.
  ,all.womenize.fig.name = isTRUE(all.women) #Make sure figure names have 'aw' at front if 'isTRUE(all.women)'.
  ## [MCW-2017-01-25-7] :: Argument to allow alternative way of providing data
  ## points to plot in the CI plots. This was added because PDU wanted to plot
  ## 'all women' survey data on the all women plots created by combining
  ## trajectories from married and unmarried women runs.
 ,data.to.plot = NULL
  ,data.to.plot.regioninfo  =NULL            #need this as well
  ,pdf.useDingbats = TRUE
 ,country.info = NULL          #For compatibility
  ,fp2020.69.only = FALSE
 ,hide.CP.tot.lt.1pc = FALSE
 ,select.c.csv = NULL #MCW-2018-03-23:: CSV file with ISO codes of countries to be included in plots
 ,age.group = NULL )
{
  if (!is.null(data.raw)){ # for country plots only
    data <- data.raw$data
    country.info <- data.raw$country.info
    region.info <- data.raw$region.info
    if(UWRA) {
        mainlong.g <- paste0(country.info$name.c, " (",  country.info$namesubreg.c,
                             ifelse(country.info$sex.ac.unm.c == 1
                                          ,", SA Low ", ", SA High "),
                             ")")
        } else if(all.women) {
            mainlong.g <- names(CI.Lg.Lcat.qt)
        } else {
            mainlong.g <- paste0(country.info$name.c, " (",  country.info$namesubreg.c, ")")
        }
    # for validation exercise unmet need, only plot countries where data were left out
  } else {
    mainlong.g <- names(CI.Lg.Lcat.qt)  #country names
  }
  name.g <- mainlong.g
  G <- length(name.g)

    ## [MCW-2017-01-25-12] :: Use input data points in the plots.
    if(!is.null(data.to.plot)) {
        data.to.plot <- ReadDataAll(data.csv = data.to.plot
                                   ,regioninfo.csv = data.to.plot.regioninfo
                                   ,do.SS.run.first.pass = FALSE
                                    ,include.c.no.data = TRUE #YES!
                                    )
        data <- data.to.plot$data
        country.info <- data.to.plot$country.info
        if(!is.null(data.to.plot$country.info.no.data)) {
            country.info <- rbind(country.info, data.to.plot$country.info.no.data)
            }

        ## Keep only those countries in 'mainlong.g' but warn before removing.
        if(sum(data$name.j %in% mainlong.g) < length(data$name.j)) {
            warning(paste("The following country names in 'data.to.plot' are not in the MCMC output:\n    "
                   ,paste(unique(data$name.j[!data$name.j %in% mainlong.g]), collapse = ", ")
                    ))
        }
        data <- data[data$name.j %in% mainlong.g,]

        if(sum(country.info$name.c %in% mainlong.g) < length(country.info$name.c)) {
            warning(paste("The following country names in 'data.to.plot.regioninfo' are not in the MCMC output:\n    "
                   ,paste(unique(country.info$name.c[!country.info$name.c %in% mainlong.g]
                        ,collapse = ", ")
                          )
                    ))
        }

        ## Will fail if not all countries in 'mainlong.g' are in 'country.info'
        if(sum(mainlong.g %in% country.info$name.c) < length(mainlong.g)) {
            stop(paste("All countries in the MCMC output must be in 'data.to.plot.regioninfo'. The following are missing:\n    "
                ,paste(mainlong.g[!(mainlong.g %in% country.info$name.c)], collapse = ", ")
                 ))
            }

        ## Need these columns to make plotting symbols work
        if(is.null(data$rounded.up)) data$rounded.up <- FALSE
        if(is.null(data$poptype.j)) data$poptype.j <- "MW"
        if(is.null(data$geo.j)) data$geo.j <- ""
        if(is.null(data$age.cat.j)) data$age.cat.j <- "0"
        if(is.null(data$posbias.j)) data$posbias.j <- ""
        if(is.null(data$mod.bias.j)) data$mod.bias.j <- ""
        if(is.null(data$folkbias.j)) data$folkbias.j <- ""

        ## Order in same way as mainlong.g
        idx <-
            unlist(sapply(mainlong.g
                         ,function(z, x) { if(z %in% x) which(x == z) }
                         ,x = data$name.j
                          ))
        data <- data[idx,]

        idx <-
            unlist(sapply(mainlong.g
                         ,function(z, x) { if(z %in% x) which(x == z) }
                        ,x = country.info$name.c
                          ))
        country.info <- country.info[idx,]
    }

  ## [MCW-2016-08-31-23] :: Add 'Unmarried' to plot titles.
  if(UWRA) mainlong.g <- paste0(mainlong.g, " --- Unmarried")

  ## [MCW-2017-01-25-13] :: Add 'All women' to plots for all women results.
  if(all.women) mainlong.g <- paste0(mainlong.g, " --- All women")

  fig.name.years <- ifelse(!is.null(start.year) | !is.null(end.year),
                           paste0("_",
                                  ifelse(!is.null(start.year), paste0("from", floor(start.year)), ""),
                                  ifelse(!is.null(end.year), paste0("to", floor(end.year)), "")),
                            "")
  if (is.null(start.year)){
    if (!is.null(CI.Lg.Lcat.qt)){
      est.years <- as.numeric(names(CI.Lg.Lcat.qt[[1]][[1]][1,]))
      start.year <- est.years[1]
    } else {
      start.year <- 1990.5
    }
  }
  if (is.null(end.year)){
    if (!is.null(CI.Lg.Lcat.qt)){
      # LAchange20140610: add estyears here again (in case start year was not NULL)
      est.years <- as.numeric(names(CI.Lg.Lcat.qt[[1]][[1]][1,]))
      end.year <- est.years[length(est.years)]
    } else {
      end.year <- 2015.5
    }
  }
  xmin <- start.year #min(years.i, start.year, na.rm = T)
  xmax <- end.year

  if (ind.country.overviewplot | ind.country.indplot){
    if (is.null(figdir.indcountries)){
      figdir.indcountries <- file.path(getwd(), "fig", paste0("ind.country.plots_", run.name))
      dir.create(file.path(getwd(), "fig"), showWarnings = FALSE)
    }
    dir.create(figdir.indcountries, showWarnings = FALSE)
    if (ind.country.overviewplot & ind.country.indplot){
      # ##details<< If \code{ind.country.indplot} and \code{ind.country.indplot} are both TRUE,
      # ## currently, \code{ind.country.indplot} is used.
      ind.country.overviewplot <- FALSE
    }
  } else {
    if (!is.null(fig.name)) {
      pdf(fig.name, width = 21, height = 12, useDingbats = pdf.useDingbats)
    }
  }

  # what are we going to plot?
  # as a start, old set-up
  # change for Shiny
  if (!shiny) {
    if (!is.null(CIratio.Lg.Lcat.qt)){
        cats <- c("Total"               #1, in CIprop
                 ,"Modern/Total"        #2, in CIratio
                 ,"Z"                  #3, in CIratio
                  ,"Modern"            #4, in CIprop
                 ,"Traditional"         #5, in CIprop
                 ,"Unmet"               #6, in CIprop
                 ,"Met Demand"           #7, in CIratio
                  ) #[MCW-2016-06-13-1] Changed.
      cats.from.ratio <- c(F,T,T,F,F,F,T)          #[MCW-MCW-2016-04-26-13] modified to match 'cats'
    } else {
      cats <- c("Total", "Modern", "Traditional", "Unmet", "TotalPlusUnmet")
      cats.from.ratio <- c(F,F,F,F,F)
    }
  } else {
  if (!is.null(CIratio.Lg.Lcat.qt)){
    cats <- c("Total", "Modern", "Traditional", "Modern/Total",
              "TradPlusUnmet", "TotalPlusUnmet", "Met Demand with Modern Methods",
              "Unmet", "TotalPlusUnmet", "Met Demand")
    cats.from.ratio <- c(F,F,F,T,F,F,T,F,F,T)
  } else {
    cats <- c("Total", "Modern", "Traditional", "TradPlusUnmet", "Unmet",
    "TotalPlusUnmet")
    cats.from.ratio <- c(F,F,F,F,F,F)
  }
  }

  if (is.null(categories.to.plot))
    categories.to.plot <- cats
  if (is.null(CIratio.Lg.Lcat.qt))
    categories.to.plot <- categories.to.plot[!(categories.to.plot %in%
                                                 c("Modern/Total", "Met Demand with Modern Methods", "Met Demand"))]
  cats.select <- match(categories.to.plot, cats)
  axisnamecats <- cats
  axisnamecats <- ifelse(axisnamecats=="Total",
                         paste("P_{c,t} (Total)"), #[MCW-2016-06-13-2] Changed label to match model parameter name.
                         paste(axisnamecats))
  axisnamecats <- ifelse(axisnamecats=="Modern", "p_{c,t,1} (Modern)"
                       , paste(axisnamecats)) #[MCW-2016-06-13-3] Changed label to match model parameter name.
  axisnamecats <- ifelse(axisnamecats=="Traditional", "p_{c,t,2} (Traditional)"
                       , paste(axisnamecats)) #[MCW-2016-06-13-4] Changed label to match model parameter name.
  axisnamecats <- ifelse(axisnamecats=="Modern/Total", "R_{c,t} (modern/any method)"
                         ,paste(axisnamecats))#[MCW-2016-06-13-5] Changed label to match model parameter name.
  axisnamecats <- ifelse(axisnamecats=="Z", "Z_{c,t}"
                         ,paste(axisnamecats))#[MCW-2016-06-13-6] Added label to match model parameter name.
  axisnamecats <- ifelse(axisnamecats=="TradPlusUnmet",
                         "Unmet need for\nmodern methods", paste(axisnamecats))
  axisnamecats <- ifelse(axisnamecats=="TotalPlusUnmet",
                         "Total demand", paste(axisnamecats))
  axisnamecats <- ifelse(axisnamecats=="Met Demand with Modern Methods",
                        "Demand satisfied\n(modern methods)", paste(axisnamecats))
  axisnamecats <- ifelse(axisnamecats=="Unmet", "p_{c,t,3} (Unmet)"
                       , paste(axisnamecats))#[MCW-2016-06-13-7] Changed label to match model parameter name.
  axisnamecats <- ifelse(axisnamecats=="Met Demand",
                         "Demand satisfied\n(any method)", paste(axisnamecats)) #[MCW-2016-04-26] Changed (added '\n(any method)')
  #axisnamecats <- ifelse(axisnamecats=="", , paste(axisnamecats))

  # cats.from.ratio refers to taking it from CI or CIratio
  if (!is.null(select.c)){
        gseq <- select.c
        if(select.c.csv) warning("Both 'select.c' and 'select.c.csv' have been specified. 'select.c.csv' is ignored.")
    } else if(!is.null(select.c.csv)) {
        read.select.c.csv <- read.csv(select.c.csv)
        ISO.colname <- grep("^ISO|^ISO Code", colnames(read.select.c.csv), ignore.case = TRUE, value = TRUE)
        if(identical(length(ISO.colname), 0L)) stop("'select.c.csv' does not have a column called 'ISO' or 'ISO Code' (ignoring case).")
        else if(!is.numeric(read.select.c.csv[[ISO.colname]])) stop("Column ", ISO.colname, " in 'select.c.csv' is not numeric.")
        gseq <- which(country.info$iso.c %in% read.select.c.csv[[ISO.colname]])
    } else {
    gseq <- 1:G

    if(fp2020.69.only) {
        gseq <-
            gseq[country.info[gseq, "iso.c", drop = TRUE] %in%
                 get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD")]
    }

    }

  for (g in gseq){

    # ##details<< If \code{ind.country.overviewplot}, plots saved in \code{figdir.indcountries}
    # if (ind.country.overviewplot)
    #   pdf(file.path(figdir.indcountries, paste0(country.info$name.c[g], fig.name.years, ".pdf")), width = 21, height = 12)
    if (ind.country.overviewplot)
      tiff(file.path(figdir.indcountries, paste0(name.g[g], fig.name.years, ".tif")), width = 21, height = 12,
           pointsize=6,res=300, bg="white",compression="lzw")

        ## [MCW-2017-01-25-15] :: Also now executed 'if(!null(data.to.plot))'.
        if ((!is.null(data.raw)) | (!is.null(data.to.plot))){ # for country plots only
      select <- seq(1, length(data[,1]))[is.element(data$iso.j, country.info$iso.c[g])]
      years.i <- data$years.j[select]
      include.SS <- ifelse(any(data$source.j[select]=="SS"), TRUE, FALSE)
      if (add.info) {
        pch.i <- ifelse(data$source.j[select] == "SS", 19,
                        ifelse(data$poptype.j[select]=="SA",24,
                               ifelse(data$poptype.j[select]=="EM"|data$poptype.j[select]=="AL",25,
                                      ifelse(data$poptype.j[select]=="HW"|data$poptype.j[select]=="BS",22
                                      ## [MCW-2016-06-30-19] Indicate rounded values
                               ,ifelse(data$rounded.up[select], 23
                                    , 21)))))
      } else {    ## [MCW-2016-06-30-20] Indicate rounded values
          pch.i <- ifelse(data$rounded.up[select], non.std.symbol,
                   ifelse(data$poptype.j[select] == "MW" & data$geo.j[select] == "" &
                                 data$age.cat.j[select] == "0" & data$posbias.j[select] == "" &
                                 data$mod.bias.j[select] == "" & data$folkbias.j[select] == "", 21, # standard
                               non.std.symbol)) #[MCW-2016-04-12-21]
      }
      ## [MCW-2016-04-12-2] modified to use 'cex.symbols' instead of hardcoded sizes.
      cex.i <- ifelse(data$source.j[select]=="SS", cex.symbols$SS,
               ifelse(add.info, cex.symbols$add.info, cex.symbols$no.info))

      col.i <- ifelse(data$source.j[select]=="DHS",sources.pal[1],
                      ifelse(data$source.j[select]=="MICS",sources.pal[2],
                      ifelse(data$source.j[select]=="NS",sources.pal[4],
                             ## [MCW-2016-03-10-3] added extra data sources. NB service statistics still 'sources.pal[5]'; 'sources.pal[3]' is "Other international survey".
                             ifelse(data$source.j[select]=="PMA",sources.pal[6],
                             ifelse(data$source.j[select]=="SS",sources.pal[5],sources.pal[3])))))
      bg.i <- bg.unmet.i <- rep("white", length(select))
      # do unmet seperately, because validation can be different
      if (!is.null(getj.test.k )){
        # validation exercise, use grey for obs that were left out
        bg.i <- ifelse(is.element(select, getj.test.k), "darkgrey", bg.i)
      }
      if (!is.null(getj.test.unmet.k )){ # for unmet only exercise
        # note that other props show up as if there were left out as well
        bg.i <- ifelse(is.element(select, getj.test.unmet.k), "darkgrey", bg.unmet.i)
      }
      # end data.raw loop
      nplots <- length(cats)+1 # +1 for legend
    } else {
      nplots <- length(cats)
      include.SS <- FALSE
    }

    # nplots <- 8
    # par(mfrow = c(2,ceiling(nplots/2)), mar = c(5,5,3,1), cex.main = 2, cex.axis = 2, cex.lab = 2)
    nrows <- ifelse(shiny, 3, 2)
    ncols <- max(ceiling(nplots/nrows), ifelse(plot.prop, 4, 3))
    if (shiny & length(cats.select) == 10) {
      nf <- layout(rbind(rep(1, ncols),
                         (1:ncols) + 1,
                         c(rep(ncols + 2, ncols - 1), ncols + 3),
                         (1:ncols) + ncols + 3,
                         c(rep(2*ncols + 4, ncols -1), 2*ncols + 5),
                         (1:ncols) + 2*ncols + 5),
                   widths = rep(1.5, ncols),
                   heights = c(0.25, 1.5, 0.2, 1.5, 0.2, 1.5), TRUE)
    } else {
      nf <- layout(rbind(rep(1, ncols),
                         matrix(seq(2, nrows*ncols+1), nrows, ncols, byrow = TRUE)),
                   widths = rep(1.5, ncols),
                   heights = c(0.25, rep(1.5, nrows)), TRUE)
    }
    # layout.show(nf)
    InternalPlotTitle(title = paste(mainlong.g[g], "   "), position = "center", cex = 3.2*cex.adj.factor)
    legend.select.comparison <- c(TRUE, !is.null(CI2.Lg.Lcat.qt[[g]]),
                                  !is.null(CI3.Lg.Lcat.qt[[g]]), !is.null(CI4.Lg.Lcat.qt[[g]]))
    if (sum(legend.select.comparison) > 1)
      legend("right", legend = c(name.dir1, name.dir2, name.dir3, name.dir4)[legend.select.comparison],
             col = c("red","blue","green","#984EA3")[legend.select.comparison], lwd = 3, cex = 1.5*cex.adj.factor)

    # par( mar = c(7,6,6,1), cex.main = 2, cex.axis = 2, cex.lab = 2)
      for (cat in (1:length(cats))[cats.select]) {
      if (!is.null(CI.Lg.Lcat.qt)){
        if (!is.null(CIstar.Lg.Lcat.qt[[g]][[cats[cat]]])){
          CIstar.qt <- CIstar.Lg.Lcat.qt[[g]][[cats[cat]]]
        } else {
          CIstar.qt <- NULL
        }
        if (!cats.from.ratio[cat]){
          est.years <- as.numeric(names(CI.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
          CI.qt <- CI.Lg.Lcat.qt[[g]][[cats[cat]]]
          CI.qt[, est.years < start.year | est.years > end.year] <- NA
          if (!is.null(CI2.Lg.Lcat.qt)) {
            est.years2 <- as.numeric(names(CI2.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
            CI2.qt <- CI2.Lg.Lcat.qt[[g]][[cats[cat]]]
            CI2.qt[, est.years2 < start.year | est.years2 > end.year] <- NA
          } else {
            CI2.qt <- NULL
          }
          if (!is.null(CI3.Lg.Lcat.qt)) {
            est.years3 <- as.numeric(names(CI3.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
            CI3.qt <- CI3.Lg.Lcat.qt[[g]][[cats[cat]]]
            CI3.qt[, est.years3 < start.year | est.years3 > end.year] <- NA
          } else {
            CI3.qt <- NULL
          }
          if (!is.null(CI4.Lg.Lcat.qt)) {
            est.years4 <- as.numeric(names(CI4.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
            CI4.qt <- CI4.Lg.Lcat.qt[[g]][[cats[cat]]]
            CI4.qt[, est.years4 < start.year | est.years4 > end.year] <- NA
          } else {
            CI4.qt <- NULL
          }
        } else {
          est.years <- as.numeric(names(CIratio.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
          CI.qt <- CIratio.Lg.Lcat.qt[[g]][[cats[cat]]]
          CI.qt[, est.years < start.year | est.years > end.year] <- NA
          if (!is.null(CIratio2.Lg.Lcat.qt)) {
            est.years2 <- as.numeric(names(CIratio2.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
            CI2.qt <- CIratio2.Lg.Lcat.qt[[g]][[cats[cat]]]
            CI2.qt[, est.years2 < start.year | est.years2 > end.year] <- NA
          } else {
            CI2.qt <- NULL
          }
          if (!is.null(CIratio3.Lg.Lcat.qt)) {
            est.years3 <- as.numeric(names(CIratio3.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
            CI3.qt <- CIratio3.Lg.Lcat.qt[[g]][[cats[cat]]]
            CI3.qt[, est.years3 < start.year | est.years3 > end.year] <- NA
          } else {
            CI3.qt <- NULL
          }
          if (!is.null(CIratio4.Lg.Lcat.qt)) {
            est.years4 <- as.numeric(names(CIratio4.Lg.Lcat.qt[[gseq[1]]][[1]][1,]))
            CI4.qt <- CIratio4.Lg.Lcat.qt[[g]][[cats[cat]]]
            CI4.qt[, est.years4 < start.year | est.years4 > end.year] <- NA
          } else {
            CI4.qt <- NULL
          }
        }
      }
      name.cat <- cats[cat]
      if (ind.country.indplot){
        par(mfrow = c(1,1))
        pdf(file.path(figdir.indcountries,
                      paste0(name.g[g], "_total", fig.name.years, ".pdf")), width = 12, height = 12
            ,useDingbats = pdf.useDingbats)
        main.extra <- name.g[g]
      } else {
        main.extra <- ifelse(cat==2, mainlong.g[g], "")
      }

      # actual plotting for that category
      par(mar = c(6, ifelse(cex.adj.factor == 1, 6, 4), 5, 1),
          cex.main = 2.5*cex.adj.factor, cex.axis = 2*cex.adj.factor, cex.lab = 2*cex.adj.factor)

      ## [MCW-2017-01-25-16] :: Make vectors to plot points 'if(!is.null(data.to.plot))'.
       if ((!is.null(data.raw)) | (!is.null(data.to.plot))){
        props.j <- NULL
        trad <- modern <- FALSE
        if (name.cat=="Total") props.j <- data$props.tot.j
        if (name.cat=="Modern/Total") props.j <- data$props.modern.j/data$props.tot.j
        if (name.cat=="TradPlusUnmet") props.j <- data$props.trad.j + data$props.unmet.j
        if (name.cat=="TotalPlusUnmet") props.j <- data$props.tot.j + data$props.unmet.j
        if (name.cat=="Met Demand with Modern Methods") props.j <-
            data$props.modern.j/(data$props.tot.j + data$props.unmet.j)
        if (name.cat=="Unmet") props.j <- data$props.unmet.j
        if (name.cat=="Met Demand") props.j <- data$props.tot.j/(data$props.tot.j + data$props.unmet.j)

        ## [MCW-2016-06-13-11] Added to plot 'observed' Z
        if(name.cat=="Z") props.j <- data$props.unmet.j/(1 - data$props.tot.j)

        if (name.cat=="Modern") {
          props.j <- data$props.modern.j
          modern <- TRUE
        }
        if (name.cat=="Traditional") {
          props.j <- data$props.trad.j
          trad <- TRUE
        }
        if (sum(!is.na(props.j[select]))>0) {
          data.props <- props.j[select]
          if (xmin > 1989)
            data.props <- data.props[years.i > 1989]
          data.props <- data.props[!is.na(data.props)]
        } else {
          data.props <- NULL
        }
      } else {
        data.props <- NULL
      }
      ## [MCW-2016-06-13-13] make axes only big enough for data points
      if(!is.null(data.props) && isTRUE(all(is.finite(data.props)))) {
          yall <- data.props
      } else yall <- c(CI.qt, CI2.qt, CI3.qt, CI4.qt, data.props)
      ymin <- ifelse(ymin.at.0, 0, ifelse(min(yall, na.rm = T) < 0.05, 0,
                                          min(yall, na.rm = T)-0.2*diff(range(yall, na.rm = T))))
      ymax <- ifelse(plot.prop & ymax.at.100, 1,
                     max(yall, na.rm = T)+0.2*diff(range(yall, na.rm = T)))
      InternalPlotEmpty(ylab = ifelse(plot.prop,"", "Count"),
                        main = axisnamecats[cat],
                        xlim = c(xmin, xmax),
                        ylim = c(ymin, ymax),
                        plot.prop = plot.prop)
      # NOTE: might want to change xlim for incl earlier data
      if (!is.null(CI2.Lg.Lcat.qt) | !is.null(CI3.Lg.Lcat.qt) | !is.null(CI4.Lg.Lcat.qt)) {
        if (!is.null(CI4.qt))
          InternalPlotCIs(CIs.qt = CI4.qt, col.median = "#984EA3", seq.years = est.years4,
                          col95 = "grey60") #[MCW-2016-04-26-8] Changed
        if (!is.null(CI3.qt))
          InternalPlotCIs(CIs.qt = CI3.qt, col.median = "green", seq.years = est.years3,
                          col95 = "grey70") #[MCW-2016-04-26-9] Changed
        if (!is.null(CI2.qt))
          InternalPlotCIs(CIs.qt = CI2.qt, col.median = "blue", seq.years = est.years2,
                          col95 = "grey80") #[MCW-2016-04-26-10] Changed
        InternalPlotCIs(CIs.qt = CI.qt, col.median = "red", seq.years = est.years,
                        col95 = "grey90") #[MCW-2016-04-26-11] Changed
      } else {
         if (!is.null(CI.Lg.Lcat.qt)){
          InternalPlotCIs(CIs.qt = CI.qt, col.median = 1, #col_pxmr[1],
                          seq.years =  est.years, col95 =  "grey90", #[MCW-2016-04-26-12] Changed
                          CIs.star.qt = CIstar.qt)
         }
      }
      ## [MCW-2017-01-25-17] :: Plot points if 'if(!is.null(data.to.plot))'.
        if (!ind.country.indplot & (!is.null(data.raw) | !is.null(data.to.plot))){
        if (sum(!is.na(props.j[select]))>0) {
          if (ind.country.overviewplot) {
            cexuse.i <- cex.i*0.6
          } else {
            cexuse.i <- cex.i
          }
          InternalPlotData(props.i = props.j[select], years.i,
                           col.i = col.i, pch.i = pch.i, cex.i = cexuse.i,
                           add.info = add.info, data = data, select = select, bg.i = bg.i,
                           trad = trad, modern = modern, cex.adj.factor = cex.adj.factor
                           ,sources.pal = sources.pal
                           )
          if (min(years.i[!is.na(props.j[select])]) < 1990 & xmin > 1989){
            text(xmin-1, ymax,"*Data incl. before 1990", cex = 1.7*cex.adj.factor, pos = 4)
          }
        }
      }
      if (!is.null(par.ciq) & name.cat=="Total") {
          UWRA.sub <- UWRA || identical(age.group, "15-19")
          InternalPlotParInfoTot(par.ciq[g,,], cex.adj.factor = cex.adj.factor
                                 ,UWRA = UWRA.sub # [MCW-2016-08-31-17] :: Added to pass this through.
                                 )
          }
      if (!is.null(par.ciq) & name.cat=="Modern/Total") {
          UWRA.sub <- UWRA || identical(age.group, "15-19")
          InternalPlotParInfoRat(par.ciq[g,,], cex.adj.factor = cex.adj.factor
                                 ,UWRA = UWRA.sub # [MCW-2016-08-31-20] :: Added to pass this through.
                                 )
      }
      if (ind.country.indplot)
        dev.off()
      if (shiny & length(cats.select) == 10 & name.cat=="Modern/Total") {
        InternalPlotTitle(title = "Unmet need and demand for modern methods",
                          position = "center", cex = 2.8*cex.adj.factor)
        InternalPlotNull()
      }
      if (shiny & length(cats.select) == 10 & name.cat=="Met Demand with Modern Methods")
        InternalPlotNull()
      if (shiny & length(cats.select) == 10 & name.cat=="Met Demand with Modern Methods") {
        InternalPlotTitle(title = "Unmet need and demand for any method",
                          position = "center", cex = 2.8*cex.adj.factor)
        InternalPlotNull()
      }
    }
    ## [MCW-2017-01-25-18] :: Plot legend if 'if(!is.null(data.to.plot))'.
        if (!ind.country.indplot & (!is.null(data.raw) | !is.null(data.to.plot))){
      InternalPlotDataLegend(TIFF = ind.country.overviewplot,
                             add.info = add.info, include.SS = include.SS,
                             cex.adj.factor = cex.adj.factor, sources.pal = sources.pal
                            ,non.std.symbol = non.std.symbol #[MCW-2016-04-12-23]
                                                             #Added to pass
                                                             #through value of
                                                             #'non.std.symbol'.
                                   ,UWRA = UWRA
                                   ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                             )}
    if (ind.country.overviewplot)
      dev.off()
  } # end country loop

  if (ind.country.indplot){
      pdf(file.path(figdir.indcountries, paste0("legend", fig.name.years, ".pdf")), width = 12, height = 12
          ,useDingbats = pdf.useDingbats) # change JR, 20140418
    InternalPlotDataLegend(add.info = add.info, include.SS = include.SS, cex.adj.factor = cex.adj.factor
                           ## [MCW-2016-04-08-2] Added.
                          ,sources.pal = sources.pal
                             ,non.std.symbol = non.std.symbol #[MCW-2016-04-12-24] Added to pass through value of 'non.std.symbol'.
                                   ,UWRA = UWRA
                                   ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                           )
    dev.off()
  }
  if (!is.null(fig.name) & !(ind.country.overviewplot | ind.country.indplot)) {
    dev.off()
  }
  return()
}
#----------------------------------------------------------------------
# The End!
