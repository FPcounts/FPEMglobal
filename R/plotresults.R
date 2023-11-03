##--------------------------------------------------------------------------
## plotresults.R
## Leontine Alkema, 2011
##--------------------------------------------------------------------------
## function called from main to plot all non-diag results...

PlotResults <- function(# Plot lots of results!
### Wrapper function to plot lots of results.
                        run.name = "test", ##<< Run name
                        output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
                        ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
                        fig.dir = NULL, ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
                        do.SS.run.first.pass = FALSE, ##<< do first pass of run with SS data? # change JR, 20140414
                        plot.ind.country.results = FALSE, ##<< Create zillion plots for all countries?
                        ## If TRUE, plots are saved in subdirectory "country.plots" in fig.dir.
                        plot.prior.post = !all.women, ##<< If TRUE, plots of priors and posteriors are generated.
                        plot.estimates = TRUE, ##<< If TRUE, plots of estimates are generated.
                        plot.parameters = !all.women, ##<< If TRUE, plots of parameters are generated.
                        start.year = NULL, ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.
                        end.year = NULL ##<< Which years are plotted? Defaults to estimation years used in \code{CI.Lg.Lcat.qt}, or 1990 otherwise.

                        ## HOTFIX [MCW-2016-02-26-1] Need to be able to turn these off for datafiles
                        ## with no, e.g., developing regions.
                       ,make.any.aggregates = TRUE
                       ,sources.pal = c("#E41A1C", "#377EB8", "#FF7F00", "#A65628", "#4DAF4A", "#984EA3", "#F781BF", "black")[c(1,2,5,3,8,6,4,7)]  ## [MCW-2016-04-05-17]: added to allow selection of colour scheme for data sources. NB: there are 7 data sources but need 8 colours because 'subpopulation' also has a colour. Colours and their sources are:
                        ## 1. DHS
                        ## 2. MICS
                        ## 3. Other international survey
                        ## 4. Service statistics (SS)
                        ## 5. Repeated national survey (RN)
                        ## 6. PMA
                        ## 7. Subpopulation
                       ,add.info = TRUE ##[MCW-2016-04-08-3]: Added to allow user to pass this through to PlotDataAndEstiamtes().
                       ,cex.symbols = list(SS = 1.5, add.info = 6, no.info = 4) ##[MCW-2016-04-12-3] Added to allow control of size of plotting symbols (passed through to PlotDataAndEstimates()).
                       ,non.std.symbol = 22 ##[MCW-2016-04-12] Added to allow control of plotting symbol for non-standard observations.
                       ,layout.style = "UNPD" ##[MCW-2017-01-30-2] :: 'layout.style' default now set to '"UNPD"', based on what was called '"new"'.
                       ,UWRA = FALSE ##[MCW-2016-08-16-1]  Added to allow modifications to plots for unmarried women.
                       ,pdf.useDingbats = FALSE
                       ,fp2020.69.only = FALSE
                       ,all.women = FALSE
                       ,all.womenize.fig.name = isTRUE(all.women) #Make sure figure names have 'aw' at front if 'isTRUE(all.women)'.
                       ,hide.CP.tot.lt.1pc = FALSE
                       ,select.c.csv = NULL #MCW-2018-03-23:: CSV file with ISO codes of countries to be included in plots
                       ,verbose = TRUE ) {

    stopifnot(layout.style %in% c("orig", "UNPD", "diagnostic"))#[MCW-2016-06-13-9]
    if(identical(layout.style, "orig")) PlotDataAndEstimates <- PlotDataAndEstimatesORIG #[MCW-2017-01-30-3] :: 'layout.style == "orig"' now uses renamed function 'PlotDataAndEstimatesORIG'.
    if(identical(layout.style, "diagnostic")) {
        PlotDataAndEstimates <- PlotDataAndEstimatesDIAG #[MCW-2017-01-30-4] :: 'layout.style == "diag"' now uses renamed function 'PlotDataAndEstimatesDIAG()'.
        ## [MCW-2016-08-29-1] Aggregate plots not working for 'diagnostic' style but don't need anyway.
        if(make.any.aggregates) {
            warning("Aggregates plots requested but not available for 'diagnostic' layout style.")
            make.any.aggregates <- FALSE
        }
    }

    if (is.null(fig.dir)){
        fig.dir <- file.path(getwd(), "fig/")
        dir.create(fig.dir, showWarnings = FALSE)
    }
    if (plot.ind.country.results){
        fig.dir.countries <- file.path(fig.dir, "country.plots/")
    }
    ## put separate?
    if (is.null(output.dir)){
        output.dir <- file.path(getwd(), "output", run.name, "/")
    }
    filename.append <- ifelse(do.SS.run.first.pass, "_pre", "")
    if (do.SS.run.first.pass & !file.exists(file.path(output.dir, paste0("par.ciq", filename.append, ".rda")))) {
        par.ciq <- NULL
    } else {
        if (file.exists(file.path(output.dir, paste0("par.ciq", filename.append, ".rda"))))
            load(file.path(output.dir, paste0("par.ciq", filename.append, ".rda"))) # change JR, 20140418
        else {
            warning("'", file.path(output.dir, paste0("par.ciq", filename.append, ".rda")),
                    "' was not found; parameter estimates cannot be plotted.")
            par.ciq <- NULL
            plot.parameters <- FALSE
        }
    }
    load(file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda"))) # change JR, 20140418
    do.country.specific.run <- mcmc.meta$general$do.country.specific.run # change JR, 20131104
    age.group <- mcmc.meta$general$age.group

    ## [MCW-2017-08-14-13] :: Load the appropriate '.rda' files if 'all.women = TRUE'.
    if(all.women) {
        load(file.path(output.dir, paste0("res.country.all.women", filename.append, ".rda")))
        res.country <- res.country.all.women
    }
    else load(file.path(output.dir, paste0("res.country", filename.append, ".rda"))) # change JR, 20140418
    if(make.any.aggregates) { #HOTFIX [MCW-2016-02-26-2] (explained above)
        if (!do.country.specific.run) { # change JR, 20131104
            if(all.women) {
                load(file.path(output.dir, "res.aggregate.all.women.rda")) # change JR, 20140418
                res.aggregate <- res.aggregate.all.women
            }
            else load(file.path(output.dir, "res.aggregate.rda")) # changedFiles JR, 20140418
        }
    }  #HOTFIX [MCW-2016-02-26-3] (explained above)

    validation <- !is.null(mcmc.meta$validation.list)

    ## >>>>> RATE MODEL [MCW-2018-01-03]
    rate.model <- ModelFunctionRateModel(mcmc.meta$general$write.model.fun)
    ## <<<<< RATE MODEL

    data <- mcmc.meta$data.raw$data
    country.info <- mcmc.meta$data.raw$country.info
    region.info <- mcmc.meta$data.raw$region.info
    winbugs.data <- mcmc.meta$winbugs.data

    ## [MCW-2016-08-26-2] Plot countries with no data as well if not
    ## 'diagnostic' plots. Note that 'data' is not sorted in the same order as
    ## 'country.info' (and 'data' has one row per observation anyway,
    ## 'country.info' has one row per country.)
    if(!identical(layout.style, "diagnostic") && mcmc.meta$general$include.c.no.data) {
        country.info <- rbind(mcmc.meta$data.raw$country.info
                             ,mcmc.meta$data.raw$country.info.no.data
                              )
        ## Need this as well because PlotDataAndEstimates() does not take argument 'country.info'.
        mcmc.meta$data.raw$country.info <- country.info
    } else country.info <- mcmc.meta$data.raw$country.info
    region.info <- mcmc.meta$data.raw$region.info
                                #This is already the union of regions for countries with and without data
    winbugs.data <- mcmc.meta$winbugs.data

    ##details<< For validation run you have to use different function
    ##\code{\link{PlotValidationResults}}. Otherwise the following plots are
    ##made:
    if (validation){
        print("Use PlotValidationResults!")
        return(invisible())
    }# end validation results

    ##details<< Plot priors and posteriors using \code{\link{PlotPriorPost}}.
    if (plot.prior.post && !identical(layout.style, "diagnostic"))
        PlotPriorPost(run.name = run.name
                     ,output.dir = output.dir, fig.dir = fig.dir #[MCW-2016-04-07-1] Added these
                     ,disagg.RN.PMA = TRUE #[MCW-2016-05-16-3] Added so that variances are plotted
                      )

    fig.run.name <- run.name
    ## [MCW-2017-08-14-18] :: For 'all.women = TRUE', Ensure '.pdf' filenames have "aw" in them, even
    ## if ~run.name~ does not have "umw" or "mw" in it.
    if(all.women && all.womenize.fig.name) {
        fig.run.name <- makeAWFileName(run.name)
    }

    ##------------------------------------------------------------------------------------------
    if (plot.estimates) {
        ##details<< Plot country overview plots for proportions with and without details using
        ##\code{\link{PlotDataAndEstimates}}.
        fig.name.years <- ifelse(!is.null(start.year) | !is.null(end.year), # change JR, 20140407
                                 paste0("_",
                                        ifelse(!is.null(start.year), paste0("from", floor(start.year)), ""),
                                        ifelse(!is.null(end.year), paste0("to", floor(end.year)), "")),
                                 "")

            if(fp2020.69.only) fp2020 <- "_fp2020_69"
            else fp2020 <- ""

        if(identical(layout.style, "diagnostic")) diag <- "_diag"
        else diag <- ""

        if(all.women) {
            plotDE.data.raw <- NULL
            plotDE.country.info <- country.info
            plotDE.par.ciq <- NULL

            if(verbose) cat("\n'CIs_nopar")

            PlotDataAndEstimates(data.raw = plotDE.data.raw,
                                 country.info = plotDE.country.info,
                                 par.ciq = plotDE.par.ciq,
                                 ## select.c=1,
                                 CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                                 CIstar.Lg.Lcat.qt = res.country$CIstar.Lg.Lcat.qt,
                                 CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                                 start.year = start.year,
                                 end.year = end.year,
                                 fig.name = file.path(fig.dir
                                                    , paste0(fig.run.name, filename.append
                                                           , "CIs", fig.name.years, fp2020, diag, ".pdf"))# change JR, 20140418
                                ,sources.pal = sources.pal
                                ,add.info = add.info #[MCW-2016-04-08-4] Control
                                 # 'add.info' and size of
                                # plotting characters.
                                ,cex.symbols = cex.symbols
                                ,non.std.symbol = non.std.symbol
                                ,UWRA = UWRA
                                ,all.women = all.women
                                ,all.womenize.fig.name = all.womenize.fig.name
                                ,fp2020.69.only = fp2020.69.only
                                ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                                ,select.c.csv = select.c.csv
                                 ,age.group = age.group
                                 )

        } else {
            plotDE.data.raw <- mcmc.meta$data.raw
                plotDE.country.info <- NULL
                plotDE.par.ciq <- par.ciq

            ## if(!rate.model) { ##[MCW-2016-10-07-2] :: Don't do plots with logistic curve parameters if rate model.

            if(verbose) cat("\n'CIs")

            PlotDataAndEstimates(data.raw = plotDE.data.raw,
                                 country.info = plotDE.country.info,
                                 par.ciq = plotDE.par.ciq,
                                 ## select.c=1,
                                 CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                                 CIstar.Lg.Lcat.qt = res.country$CIstar.Lg.Lcat.qt,
                                 CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                                 start.year = start.year,
                                 end.year = end.year,
                                 fig.name = file.path(fig.dir
                                                    , paste0(fig.run.name, filename.append
                                                           , "CIs", fig.name.years, fp2020, diag, ".pdf"))# change JR, 20140418
                                ,sources.pal = sources.pal
                                ,add.info = add.info # [MCW-2016-04-08-4] Control
                                 # 'add.info' and size of
                                # plotting characters.
                                ,cex.symbols = cex.symbols
                                ,non.std.symbol = non.std.symbol
                                ,UWRA = UWRA
                                ,all.women = all.women
                       ,all.womenize.fig.name = all.womenize.fig.name
                                ,fp2020.69.only = fp2020.69.only
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = select.c.csv
                                 ,age.group = age.group
                                 )
        ## }
        if(verbose) cat("\n'CIs_nopar")
        PlotDataAndEstimates(data.raw = plotDE.data.raw,
                             CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                             CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                             start.year = start.year,
                             end.year = end.year,
                             fig.name = file.path(fig.dir
                                                , paste0(fig.run.name, filename.append
                                                       , "CIs_nopar", fig.name.years, fp2020, diag, ".pdf"))# change JR, 20140418
                            ,sources.pal = sources.pal
                            ,add.info = add.info # [MCW-2016-04-08-5] Control
                                                 # 'add.info' and size of
                                                 # plotting characters.
                            ,cex.symbols = cex.symbols
                            ,non.std.symbol = non.std.symbol
                            ,UWRA = UWRA
                            ,all.women = all.women
                       ,all.womenize.fig.name = all.womenize.fig.name
                            ,fp2020.69.only = fp2020.69.only
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = select.c.csv
                                 ,age.group = age.group
                             )
        }

        ## to plot individual country results
        if (plot.ind.country.results && !identical(layout.style, "diagnostic")) {
            PlotDataAndEstimates(data.raw = mcmc.meta$data.raw,
                                 CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                                 CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                                 ind.country.overviewplot = TRUE,
                                 start.year = start.year,
                                 end.year = end.year,
                                 run.name = run.name
                                ,sources.pal = sources.pal
                                ,add.info = add.info # [MCW-2016-04-08-6] Control
                                                     # 'add.info' and size of
                                                     # plotting characters.
                                ,cex.symbols = cex.symbols
                                ,non.std.symbol = non.std.symbol
                                ,UWRA = UWRA
                                ,all.women = all.women
                       ,all.womenize.fig.name = all.womenize.fig.name
                                ,fp2020.69.only = fp2020.69.only
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = NULL #This is for country plots only.
                                 ,age.group = age.group
                                 )
        }

        ##details<< Plot country overview plots for counts using
        ##\code{\link{PlotDataAndEstimates}}.

        if(verbose) cat("\n'CIscountcountry")
        PlotDataAndEstimates(
            CI.Lg.Lcat.qt = res.country$CIcount.Lg.Lcat.qt,
            plot.prop = FALSE,
            start.year = start.year,
            end.year = end.year,
            fig.name = file.path(fig.dir
                               , paste0(fig.run.name, filename.append
                                      , "CIscountcountry", fig.name.years, fp2020, diag, ".pdf")) # change JR, 20140418
           ,sources.pal = sources.pal
           ,add.info = add.info #[MCW-2016-04-08-7] Control 'add.info' and size
                                #of plotting characters.
           ,cex.symbols = cex.symbols
           ,non.std.symbol = non.std.symbol
           ,UWRA = UWRA
           ,all.women = all.women
           ,all.womenize.fig.name = all.womenize.fig.name
           ,country.info = country.info
           ,fp2020.69.only = fp2020.69.only
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = select.c.csv
                                 ,age.group = age.group
        )

        if(make.any.aggregates) { #HOTFIX [MCW-2016-02-26-4] (explained above)

            ##details<< Plot UNDP aggregates overview plots for proportions and counts using
            ##\code{\link{PlotDataAndEstimates}}.
            if (!do.country.specific.run) {
                if(verbose) cat("\n'CIsaggregate")
                PlotDataAndEstimates(CI.Lg.Lcat.qt = res.aggregate$CIprop.Lg.Lcat.qt,
                                     CIratio.Lg.Lcat.qt = res.aggregate$CIratio.Lg.Lcat.qt,
                                     start.year = start.year,
                                     end.year = end.year,
                                     fig.name = file.path(fig.dir, paste0(fig.run.name, filename.append
                                                                        , "CIsaggregate", fig.name.years, diag, ".pdf")) # change JR, 20140418
                                    ,sources.pal = sources.pal
                                    ,add.info = add.info #[MCW-2016-04-08-8]
                                                         #Control 'add.info' and
                                                         #size of plotting
                                                         #characters.
                                    ,cex.symbols = cex.symbols
                                    ,non.std.symbol = non.std.symbol
                                    ,UWRA = UWRA
                                    ,all.women = all.women
                       ,all.womenize.fig.name = all.womenize.fig.name
                                    ,country.info = country.info
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = NULL #This is for country plots only.
                                 ,age.group = age.group
                                     )

                if(verbose) cat("\n'CIscountaggregate")
                PlotDataAndEstimates(
                    CI.Lg.Lcat.qt = res.aggregate$CIcount.Lg.Lcat.qt,
                    plot.prop = FALSE,
                    start.year = start.year,
                    end.year = end.year,
                    fig.name = file.path(fig.dir
                                       , paste0(fig.run.name, "CIscountaggregate"
                                              , fig.name.years, diag, ".pdf")) # change JR, 20140418
                   ,sources.pal = sources.pal
                   ,add.info = add.info #[MCW-2016-04-08-9] Control 'add.info'
                                        #and size of plotting characters.
                   ,cex.symbols = cex.symbols
                   ,non.std.symbol = non.std.symbol
                   ,UWRA = UWRA
                   ,all.women = all.women
                       ,all.womenize.fig.name = all.womenize.fig.name
                   ,country.info = country.info
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = NULL #This is for country plots only.
                                 ,age.group = age.group
                )
if(!identical(layout.style, "diagnostic")) {
                if (plot.ind.country.results){
                    PlotDataAndEstimates(CI.Lg.Lcat.qt = res.aggregate$CIprop.Lg.Lcat.qt,
                                         CIratio.Lg.Lcat.qt = res.aggregate$CIratio.Lg.Lcat.qt,
                                         ind.country.overviewplot = TRUE,
                                         start.year = start.year,
                                         end.year = end.year,
                                         run.name = run.name
                                        ,sources.pal = sources.pal #[MCW-2016-04-05-14]
                                                                   #Control
                                                                   #disaggregation
                                                                   #of PMA and
                                                                   #repeated
                                                                   #national
                                                                   #data sources
                                        ,add.info = add.info #[MCW-2016-04-08-10]
                                                             #Control 'add.info'
                                                             #and size of
                                                             #plotting
                                                             #characters.
                                        ,cex.symbols = cex.symbols
                                        ,non.std.symbol = non.std.symbol
                                        ,UWRA = UWRA
                                        ,all.women = all.women
                       ,all.womenize.fig.name = all.womenize.fig.name
                                        ,country.info = country.info
                        ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                        ,select.c.csv = NULL #This is for country plots only.
                                 ,age.group = age.group
                                         )
                }
                PlotCountryEstimatesForAggregate(
                    CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                    country.info = country.info,
                    fig.name = file.path(fig.dir
                                       , paste0(fig.run.name, "CIscountryestsinaggregates"
                                              , fig.name.years, ".pdf")) # change JR, 20140418
                   ,mcmc.meta = mcmc.meta
                    ,all.women = all.women
                )
                }
            }
        }  #HOTFIX [MCW-2016-02-26-5] (explained above)
    }
    ##------------------------------------------------------------------------------------------
    if(!identical(layout.style, "diagnostic")) {
    if(verbose) cat("\n'Logistic parameters")

    if (plot.parameters && !all.women) {
        ##details<< Plot model parameters using \code{\link{PlotLogisticParameters}}.
        if (is.null(par.ciq)) {
            stop("par.ciq is NULL. Model parameters cannot be plotted.")
        } else {
            PlotLogisticParameters(par.ciq = par.ciq, country.info = country.info,
                                   region.info = region.info,
                                   fig.name = file.path(fig.dir, paste0(fig.run.name, filename.append, "modelpar.pdf"))
                                  ,UWRA = UWRA ##[MCW-2016-08-16-2] Added to pass this argument through.
                            ,write.model.fun = mcmc.meta$general$write.model.fun
                                   ) # change JR, 20140418
        }
        ##------------------------------------------------------------------------------------------
        ##details<< Plot info about data parameters (variance by source, biases and perturbation multipliers).

            cat("\n'Source variances")

        load(file.path(output.dir, paste0("mcmc.array", filename.append, ".rda"))) # change JR, 20140418

        if (!do.country.specific.run) { # change JR, 20131105
            ## variance by source
            figname_ss <- paste0(fig.run.name, "sigmasource.pdf")
            if(hide.CP.tot.lt.1pc) {
                figname_ss <- paste0(fig.run.name, "sigmasource_no_CPTotLT1pc.pdf")
                }
            pdf(file.path(fig.dir, figname_ss), width = 7, height = 5
               ,useDingbats = pdf.useDingbats) # change JR, 20140418
            PlotSourceSigmas(mcmc.array = mcmc.array,
                            ,write.model.fun = mcmc.meta$general$write.model.fun
                            ,UWRA = UWRA
                            ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                             )
            PlotSourceSigmasUnmet(mcmc.array = mcmc.array
                             )
            dev.off()

            figname_ss <- paste0(fig.run.name, "sigmasource_ylim_0_4.pdf")
            if(hide.CP.tot.lt.1pc) {
                figname_ss <- paste0(fig.run.name, "sigmasource_no_CPTotLT1pc_ylim_0_4.pdf")
                }
            pdf(file.path(fig.dir, figname_ss), width = 7, height = 5
               ,useDingbats = pdf.useDingbats) # change JR, 20140418
            PlotSourceSigmas(mcmc.array = mcmc.array,
                            ,write.model.fun = mcmc.meta$general$write.model.fun
                             ,UWRA = UWRA, ylim = c(0, 4)
                            ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                             )
            PlotSourceSigmasUnmet(mcmc.array = mcmc.array,
                                 ylim = c(0, 1.5)
                             )
            dev.off()

            figname_ss <- paste0(fig.run.name, "sigmasource_ylim_1-5.pdf")
            if(hide.CP.tot.lt.1pc) {
                figname_ss <- paste0(fig.run.name, "sigmasource_no_CPTotLT1pc_ylim_1-5.pdf")
                }
            pdf(file.path(fig.dir, figname_ss), width = 7, height = 5
               ,useDingbats = pdf.useDingbats) # change JR, 20140418
            PlotSourceSigmas(mcmc.array = mcmc.array,
                            ,write.model.fun = mcmc.meta$general$write.model.fun
                             ,UWRA = UWRA, ylim = c(0, 1.5)
                            ,hide.CP.tot.lt.1pc = hide.CP.tot.lt.1pc
                             )
            PlotSourceSigmasUnmet(mcmc.array = mcmc.array,
                                 ylim = c(0, 1.5)
                             )
            dev.off()

            ## biases,
            pdf(file.path(fig.dir, paste0(fig.run.name, "Biases.pdf"))
               ,width = 7, height = 5, useDingbats = pdf.useDingbats
                ) # change JR, 20140418
            PlotBiases(mcmc.array = mcmc.array)
            dev.off()

            if("v.mics" %in% dimnames(mcmc.array)[[3]]) {
                parnames.biases <- c("v.mneg", "v.mpos", "v.folk", "v.mics")
            } else {
                parnames.biases <- c("v.mneg", "v.mpos", "v.folk", "v.abs.probe.q")
            }
            percentiles  <-  c(0.025,0.5, 0.975)
            biases.i3 <- NULL
            parnames.biases <-
                parnames.biases[parnames.biases %in% dimnames(mcmc.array)[[3]]]
            for (parname in (parnames.biases)) {
                biases.i3 <-
                    rbind(biases.i3, quantile(mcmc.array[,,parname],percentiles))
            }
            rownames(biases.i3) <- parnames.biases
            write.csv(biases.i3, row.names = TRUE
                    , file = file.path(fig.dir, paste0(fig.run.name, "Biases_summary.csv"))
                      )

            ## If survey-specific SEs are used, plot total measurement error
            ## variance for each source by averaging over all surveys within type
            ## (e.g., average over all DHSs).
            if(ModelFunctionSurveySEs(mcmc.meta$general$write.model.fun)) {

                pdf(file.path(fig.dir, paste0(fig.run.name, "sigmasource_total.pdf"))
                  , width = 7, height = 5, useDingbats = pdf.useDingbats)
                PlotDataModelSEs(run.name = run.name, output.dir = output.dir, mcmc.array = mcmc.array)
                dev.off()

                pdf(file.path(fig.dir, paste0(fig.run.name, "sigmasource_total_no_CPTotLT1pc.pdf"))
                  , width = 7, height = 5, useDingbats = pdf.useDingbats)
                PlotDataModelSEs(run.name = run.name, output.dir = output.dir, mcmc.array = mcmc.array,
                                 hide.CP.tot.lt.1pc = TRUE)
                dev.off()

                pdf(file.path(fig.dir, paste0(fig.run.name, "sigmasource_total_ylim_0_1-5.pdf"))
                  , width = 7, height = 5, useDingbats = pdf.useDingbats)
                PlotDataModelSEs(run.name = run.name, output.dir = output.dir, mcmc.array = mcmc.array,
                                 ylim = c(0, 1.5))
                dev.off()

                pdf(file.path(fig.dir, paste0(fig.run.name, "sigmasource_total_no_CPTotLT1pc_ylim_0_1-5.pdf"))
                  , width = 7, height = 5, useDingbats = pdf.useDingbats)
                PlotDataModelSEs(run.name = run.name, output.dir = output.dir, mcmc.array = mcmc.array,
                                 hide.CP.tot.lt.1pc = TRUE, ylim = c(0, 1.5))
                dev.off()
            }

        }

        ## (long!) list of names of unique multipliers
        par.V <- mcmc.meta$par.V
        ##InternalGetParnamesV(winbugs.data = winbugs.data,
        ##                      name.short.j = InternalMakeCountryNamesShort(mcmc.meta$data.raw$data$name.j))
        ##par.V$parnames.V.in.bugs
        ##par.V$parnames.V.nice
        ##data.frame(par.V$parnames.V.in.bugs,  par.V$parnames.V.nice)
        pdf(file.path(fig.dir, paste0(fig.run.name, filename.append, "Multipliers.pdf")))#, height = 15, width = 20) # change JR, 20140418
        PlotGeoEtc(par.V = par.V, mcmc.array = mcmc.array)
        dev.off()

        ## To check data dummies:
        ## number of observations with bias (folk/mics/M+/M-):
        ## summary.biases <-SummarizeBiases(winbugs.data) # 237 biases
        ## number of unique multipliers included (per composition):
        ## summary.multipliers <- SummarizeMultipliers(winbugs.data, data) # 214 sets
    }
    }
    cat("\nAll results plotted and saved to ", fig.dir, "\n")
    ##value<< NULL
    return(invisible(NULL))
} ## end function
##----------------------------------------------------------------------
## The End!
