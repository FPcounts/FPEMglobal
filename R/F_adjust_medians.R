################################################################################
###
### DATE CREATED: 2017-02-21
###
### AUTHOR: Mark Wheldon
###
### DESCRIPTION:
###
### Functions to adjust medians so various identities hold, e.g.,
### total cp = modern + traditional.
###
###-----------------------------------------------------------------------------
###
### SYNOPSIS:
###
################################################################################



##' Adjust country-level medians to preserve various arithmetic identities.
##'
##' \code{AdjustMedians} Adjusts country-level posterior medians to
##' ensure various arithmetic identities hold (e.g., total CP = modern +
##' traditional).
##'
##' Adjustment methods:
##'   \describe{
##' \item{topdown}{}
##' \item{bottomup}{}
##' }
##'
##' @param run.name Run name.
##'
##' @param output.dir Directory where MCMC array and meta are stored. If NULL,
##'     it's \code{output/run.name}, default from \code{runMCMC}.
##'
##' @param table.dir Directory to store tables. If NULL, folder "tables" in current
##'     working directory.
##'
##' @param res If NULL, country/UNPD aggregates are summarized, specified with next
##'     argument. Alternatively, an object of class \code{\link{Results}} with
##'     \code{CIprop.Lg.Lcat.qt} and \code{CIcount.Lg.Lcat.qt}.
##'
##' @param denom.counts File reference to csv file with denominator counts
##'     (e.g., total number of married women aged 15--49).
##'
##' @param adj.method Method of adjustment to use. See details.
##'
##' @param compare Logical. Should the adjusted indicators be compared with the
##'     unadjusted ones?
##'
##' @return TO DO.
##'
##' @author Mark Wheldon, based on \code{\link{GetTablesRes}}.
##'
##' @noRd
AdjustMedians <- function(run.name = "test",
                          name.res = "country",
                                 output.dir,
                                 res = NULL,
                          adj.method = c("mod_tot_unmet", "topdown", "bottomup", "modelp"),
                          all.women = FALSE) {

    ## -------* SET-UP

    ## -------** Fix arguments

    if (is.null(output.dir)) {
        output.dir <- file.path(getwd(), "output", run.name,"/")
    }

    adj.method <- match.arg(adj.method)

    ## -------** Inputs

    ## -------*** Results ('res')

    if(is.null(res)) {
        if(all.women) {
            if(name.res == "country") {
                load(file.path(output.dir, "res.country.all.women.rda"))
                res <- res.country.all.women
            } else if(name.res == "UNPDaggregate") {
                load(file.path(output.dir, "res.aggregate.all.women.rda"))
                res <- res.aggregate.all.women
            } else {
                stop("Not yet implemented.")
            }
        } else {
            if(name.res == "country") {
                load(file.path(output.dir, "res.country.rda"))
                res <- res.country
            } else if(name.res == "UNPDaggregate") {
                load(file.path(output.dir, "res.aggregate.rda"))
                res <- res.aggregate
            } else {                #other aggregates
                if(file.exists(file.path(output.dir, paste0(name.res, ".rda")))) {
                    res.loaded <- load(file.path(output.dir, paste0(name.res, ".rda")))
                } else stop("'", file.path(output.dir, paste0(name.res, ".rda")), "' does not exist. Check 'name.res'.")
                res <- get(res.loaded)
            }
        }
    }

    ## -------*** Meta

    load(file.path(output.dir, "mcmc.meta.rda"))

    if(mcmc.meta$general$include.c.no.data) {
        c.info <-
            rbind(mcmc.meta$data.raw$country.info
                 ,mcmc.meta$data.raw$country.info.no.data
                  )
    } else c.info <- mcmc.meta$data.raw$country.info

    ## -------*** Some constants

    est.years <- as.numeric(dimnames(res$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]])
    n.indicators <- length(res$CIprop.Lg.Lcat.qt[[1]]) # 'Modern', 'Trad', etc.
    G <- length(res$CIprop.Lg.Lcat.qt)  # Number of countries
    countries <- names(res$CIprop.Lg.Lcat.qt)

    ## -------* SUB FUNCTIONS

    ## To make output presentable to GetTablesRes().
    reform.out <- function(z, match.names, out.colnames) {
        match.names <-
            match.names[match.names %in% gsub(".Adj", "", names(z), fixed = TRUE)]
        z <- z[, paste(match.names, "Adj", sep = ".")]
        z <- as.list(z)
        z <- lapply(z, function(y) {
            y <- matrix(y, nrow = 1)
            dimnames(y) <- list("0.5", out.colnames)
            return(y)
        })
        return(z)
        }

    ## -------* MAIN

    ## -------** Extract medians

    ## Extract for counts, props and ratios.
    ## E.g., CI.Lg.medians[["CIcount.Lg.Lcat.qt"]] is the counts as a list of
    ## lists: top-level is country, second level is indicator ('Total',
    ## 'Traditional', 'Modern, 'Unmet', 'TotalPlusUnmet', 'TradPlusUnmet').
    CI.Lg.medians <-
        lapply(res[c("CIprop.Lg.Lcat.qt", "CIratio.Lg.Lcat.qt", "CIcount.Lg.Lcat.qt")]
              ,function(x) {
                  lapply(x, function(y) {
                      sapply(y, function(z) z["0.5",])
                  })
              })

    ## -------** Adjust the /counts/

    CIcount.Lg.median <- CI.Lg.medians[["CIcount.Lg.Lcat.qt"]]

    if(identical(adj.method, "topdown")) {

        ## -------*** Topdown

        CIcount.Lg.median.adj <-
            lapply(CIcount.Lg.median, function(z) {

                z <- as.data.frame(z)

                ## Adjustment factor for Total and Unmet:
                tot.demand.adjFac <- with(z, TotalPlusUnmet / (Total + Unmet))

                ## Adjust Total and Unmet
                adj.df <-
                    with(z, data.frame(TotalPlusUnmet.Adj = TotalPlusUnmet
                                      ,Total.Adj = Total * tot.demand.adjFac
                                      ,Unmet.Adj = Unmet * tot.demand.adjFac
                                       ))

                ## Adjustment factor for Modern, Trad, and Dem Satis Modern
                tot.CP.adjFac <- adj.df$Total.Adj / (z$Modern + z$Traditional)

                ## Adjust Modern and Traditional
                adj.df <-
                    within(adj.df, {
                        Modern.Adj <- z$Modern * tot.CP.adjFac
                        Traditional.Adj <- z$Traditional * tot.CP.adjFac
                    })

                ## Adjust TradPlusUnmet, Demand Satisfied Modern
                adj.df <-
                    within(adj.df, {
                        TradPlusUnmet.Adj <- Traditional.Adj + Unmet.Adj
                    })

                rownames(adj.df) <- rownames(z)

                return(adj.df)
            })

    } else if(identical(adj.method, "bottomup")) {

        ## -------*** Bottomup

        CIcount.Lg.median.adj <-
            lapply(CIcount.Lg.median, function(z) {

                z <- as.data.frame(z)

                adj.df <-
                    with(z, data.frame(Modern.Adj = Modern
                                      ,Traditional.Adj = Traditional
                                       ,Unmet.Adj = Unmet
                                      ,Total.Adj = Modern + Traditional
                                       ))
                adj.df <-
                    within(adj.df, {
                        TotalPlusUnmet.Adj <- Total.Adj + Unmet.Adj
                        TradPlusUnmet.Adj <- Traditional.Adj + Unmet.Adj
                    })

                rownames(adj.df) <- rownames(z)

                return(adj.df)
            })

    } else if(identical(adj.method, "modelp")) {

        ## -------*** Modelp

    CIcount.Lg.median <- CI.Lg.medians[["CIcount.Lg.Lcat.qt"]]

        ## 'Total' is unchanged.
        CIcount.Lg.median.adj <-
            lapply(CIcount.Lg.median, function(z) {

                z <- as.data.frame(z)

                adj.df <-
                    with(z, data.frame(Total.Adj = Total))
                rownames(adj.df) <- rownames(z)
                return(adj.df)
            })

        ## Modern/Total ratio is unchanged.
        ## Modern is re-calculated using un-adjusted 'Total' and 'Modern/Total' medians.
        CIcount.Lg.median.adj <-
            mapply(function(a, b) {
                a <- within(a, Modern.Adj <-
                                   Total.Adj * as.data.frame(b)[["Modern/Total"]]
                            )
                return(a)
            }
           ,CIcount.Lg.median.adj, CI.Lg.medians[["CIratio.Lg.Lcat.qt"]]
           ,SIMPLIFY = FALSE
            )

        ## Unmet Need is re-calculated using un-adjusted 'Total' and 'Z' medians.
        for(i in names(CIcount.Lg.median.adj)) {
            CIcount.Lg.median.adj[[i]][,"denom.count"] <- res[["W.Lg.t"]][[i]]
            }
        CIcount.Lg.median.adj <-
            mapply(function(a, b) {
                a <- within(a, Unmet.Adj <-
                                   (denom.count - Total.Adj) * as.data.frame(b)[["Z"]]
                            )
                return(a)
            }
           ,CIcount.Lg.median.adj, CI.Lg.medians[["CIratio.Lg.Lcat.qt"]]
           ,SIMPLIFY = FALSE
            )

        ## Traditional and demand indicators are re-calculated using updates so far.
        CIcount.Lg.median.adj <-
            lapply(CIcount.Lg.median.adj, function(z) {

                z <- as.data.frame(z)

                adj.df <-
                    within(z, {
                        Traditional.Adj <- (1 - (Modern.Adj / Total.Adj)) * Total.Adj
                        TotalPlusUnmet.Adj <- Total.Adj + Unmet.Adj
                        TradPlusUnmet.Adj <- Traditional.Adj + Unmet.Adj
                    })
                rownames(adj.df) <- rownames(z)
                return(adj.df)
            })

    } else if(identical(adj.method, "mod_tot_unmet")) {

        ## -------*** Mod_tot_unmet

        CIcount.Lg.median.adj <-
            lapply(CIcount.Lg.median, function(z) {

                z <- as.data.frame(z)

                adj.df <-
                    with(z, data.frame(Modern.Adj = Modern
                                      ,Traditional.Adj = Total - Modern
                                       ,Unmet.Adj = Unmet
                                      ,Total.Adj = Total
                                       ))
                adj.df <-
                    within(adj.df, {
                        TotalPlusUnmet.Adj <- Total.Adj + Unmet.Adj
                        TradPlusUnmet.Adj <- Traditional.Adj + Unmet.Adj
                    })

                rownames(adj.df) <- rownames(z)

                return(adj.df)
            })

    }


    ## -------** Adjust the _proportions_

        ## List for the proportions
        CIprop.Lg.median.adj <- list()

        ## How many indicators
        n.ind <- ncol(CIcount.Lg.median.adj[[1]])

        ## Country-by-country re calculate proportions as adjusted counts / denominators.
        for(i in 1:length(CIcount.Lg.median.adj)) {
            denom.counts <-
                matrix(res$W.Lg.t[[i]], ncol = n.ind, nrow = length(est.years))
            CIprop.Lg.median.adj <-
                c(CIprop.Lg.median.adj
                 ,list(CIcount.Lg.median.adj[[i]] / denom.counts))
        }
        names(CIprop.Lg.median.adj) <- names(CIcount.Lg.median.adj)

    ## -------** Adjust the _ratios_

    ## Re-calculate all ratios using updates so far (see below for 'Z' in 'modelp').

    CIratio.Lg.median.adj <-
        lapply(CIprop.Lg.median.adj, function(z) { # applied to _proportions_
            adj.df <- with(z, data.frame(`Modern/Total.Adj` = Modern.Adj / Total.Adj
                              ,`Met Demand.Adj` = Total.Adj / TotalPlusUnmet.Adj
                              ,Z.Adj = Unmet.Adj / (1 - TotalPlusUnmet.Adj)
                              ,`Met Demand with Modern Methods.Adj` = Modern.Adj / TotalPlusUnmet.Adj
                              ,check.names = FALSE
                               )
                           )
            rownames(adj.df) <- rownames(z)
            return(adj.df)
        })

    ## If 'modelp' then do not adjust 'Z'
    if(identical(adj.method, "modelp")) {
        ## Put back the original 'Z'
        CIratio.Lg.median.adj <-
            mapplySafe(function(x, y) {
                z <- within(x, { Z.Adj <- y[,"Z"] })
                return(z)
            }
           ,CIratio.Lg.median.adj, CI.Lg.medians[["CIratio.Lg.Lcat.qt"]]
           ,SIMPLIFY = FALSE)
    }

    ## -------** Produce Outputs

    ## Make into a form that can be passed to GetTablesRes().
    out.list <-
        list(CIprop.Lg.Lcat.median.adj = lapply(CIprop.Lg.median.adj
                                               ,"reform.out"
                                               ,names(res$CIprop.Lg.Lcat.qt[[1]])
                                               ,est.years
                                                )
            ,CIratio.Lg.Lcat.median.adj = lapply(CIratio.Lg.median.adj
                                                ,"reform.out"
                                                ,names(res$CIratio.Lg.Lcat.qt[[1]])
                                                ,est.years
                                                 )
            ,CIcount.Lg.Lcat.median.adj = lapply(CIcount.Lg.median.adj
                                                ,"reform.out"
                                                ,names(res$CIcount.Lg.Lcat.qt[[1]])
                                                 ,est.years
                                                 )
            ,iso.g = res$iso.g
             ,W.Lg.t = res$W.Lg.t
             )

    ## -------* RETURN

    return(out.list)
}

##' ConstructAdjMediansAllWomen
##'
##' Adjust country-level medians to preserve various arithmetic identities for all women results.
##'
##' \code{ConstructAdjMediansAllWomen} Adjusts country-level
##' posterior medians for all women to ensure arithmetic identities
##' such as total CP = modern + traditional hold, as well as ensuring
##' that all women = married + unmarried.
##'
##' @param uwra.adj.med Output of \code{\link{AdjustMedians}} for unmarried women. If a character string, assumed to be filepath to saved version which is passed to \code{\link{load}}.
##' @param mwra.adj.med See 'uwra.adj.med' (but for married women output).
##' @author Mark Wheldon.
##' @noRd
ConstructAdjMediansAllWomen <-
    function(uwra.adj.med,
             mwra.adj.med
             ) {

        ## -------* SET UP

        ## -------** Constants

        data.els <- c("CIprop.Lg.Lcat.median.adj",
                     "CIratio.Lg.Lcat.median.adj",
                      "CIcount.Lg.Lcat.median.adj")

        ## -------** Fix arguments

        ## -------** Inputs

        ## Adjusted medians for unmarried and married.

        ## Load if necessary

        if(is.character(uwra.adj.med)) {
            uwra.adj.med.name <- (load(uwra.adj.med))
            uwra.adj.med <- get(uwra.adj.med.name)
        }

        if(is.character(mwra.adj.med)) {
            mwra.adj.med.name <- (load(mwra.adj.med))
            mwra.adj.med <- get(mwra.adj.med.name)
        }

        ## 'mwra.med.adj' and 'uwra.med.adj' are lists with structure as follows:

        ## > names(mwra.adj.counts)
        ## [1] "CIprop.Lg.Lcat.median.adj"  "CIratio.Lg.Lcat.median.adj"
        ## [3] "CIcount.Lg.Lcat.median.adj" "iso.g"

        ## > names(mwra.adj.counts[[1]][1:10])
        ##  [1] "Afghanistan"         "Albania"             "Algeria"
        ##  [4] "Angola"              "Anguilla"            "Antigua and Barbuda"
        ##  [7] "Argentina"           "Armenia"             "Australia"
        ## [10] "Austria"

        ## > names(mwra.adj.counts[[1]][[1]])
        ## [1] "Total.Adj"          "Traditional.Adj"    "Modern.Adj"
        ## [4] "Unmet.Adj"          "TotalPlusUnmet.Adj" "TradPlusUnmet.Adj"

        ## > str(mwra.adj.counts[[1]][[1]][[1]])
        ##  num [1, 1:61] 0.0168 0.0181 0.0194 0.0208 0.0221 ...
        ##  - attr(*, "dimnames")=List of 2
        ##   ..$ : chr "0.5"
        ##   ..$ : chr [1:61] "1970.5" "1971.5" "1972.5" "1973.5" ...

        ## > names(mwra.adj.counts[[2]][[1]])
        ## [1] "Modern/Total.Adj"                   "Met Demand.Adj"
        ## [3] "Z.Adj"                              "Met Demand with Modern Methods.Adj"

        ## > names(mwra.adj.counts[[3]][[1]])
        ## [1] "Total.Adj"          "Traditional.Adj"    "Modern.Adj"
        ## [4] "Unmet.Adj"          "TotalPlusUnmet.Adj" "TradPlusUnmet.Adj"

        ## -------** Fix Bolivia and Czech Republic

        for(x in c(data.els, "W.Lg.t")) {
            names(mwra.adj.med[[x]])[names(mwra.adj.med[[x]]) == "Bolivia"] <-
                grep("^Bolivia", names(uwra.adj.med[[x]]), value = TRUE)

            names(mwra.adj.med[[x]])[names(mwra.adj.med[[x]]) == "Czech Republic"] <-
                grep("^Czech", names(uwra.adj.med[[x]]), value = TRUE)
            }

        ## -------*** Make Sure UWRA and MWRA are Conformable

        ## -------**** First (prop, ratio, count) level

        nm.mwra <- names(mwra.adj.med)
        nm.uwra <- names(uwra.adj.med)
        nm.both <- intersect(nm.mwra, nm.uwra)
        if(identical(length(nm.both), 0L)) stop("Married and unmarried women adjusted medians do not have any CI types in common.")
        if(!all(nm.mwra %in% nm.uwra) || !all(nm.uwra %in% nm.mwra)) {
            warning(paste0("Married and unmarried women adjusted medians do not have the same CI types. Only those in common will be processed (", paste0(nm.both, collapse = ", "), ")."))
            mwra.adj.med <- mwra.adj.med[nm.both]
            uwra.adj.med <- uwra.adj.med[nm.both]
        }

        ## -------**** Second (Country) Level

        for(i in c(data.els, "W.Lg.t")) {
            nm.mwra <- names(mwra.adj.med[[i]])
            nm.uwra <- names(uwra.adj.med[[i]])
            nm.both.cname <- intersect(nm.mwra, nm.uwra)

            ## save the following concordances to get iso.g concordant
            if(identical(i, "CIprop.Lg.Lcat.median.adj")) {
                nm.both.cname.in.mwra <- match(nm.both.cname, nm.mwra)
                nm.both.cname.in.uwra <- match(nm.both.cname, nm.uwra)

                ## Warn (but only once, for 'prop')
                if(!identical(nm.mwra, nm.uwra)) {
                    warning(paste0("Married and unmarried women adjusted medians do not have the same countries/aggregates. Only those in common will be processed."))
                    if(isTRUE(any(!(nm.mwra %in% nm.both.cname)))) {
                        warning("****************************************************************************\nThe following countries/areas are in married, but not unmarried, adjusted medians:\n****************************************************************************\n", paste(strwrap(paste(nm.mwra[!(nm.mwra %in% nm.both.cname)], collapse = ", "), indent = 4, exdent = 4), collapse = "\n"), ".")
                    }
                    if(isTRUE(any(!(nm.uwra %in% nm.both.cname)))) {
                        warning("The following countries/areas are in unmarried, but not married, adjusted medians:\n", paste(strwrap(paste(nm.uwra[!(nm.uwra %in% nm.both.cname)], collapse = ", "), indent = 4, exdent = 4), collapse = "\n"), ".")
                    }
                }
            }
            mwra.adj.med[[i]] <- mwra.adj.med[[i]][nm.both.cname]
            uwra.adj.med[[i]] <- uwra.adj.med[[i]][nm.both.cname]
        }
        ln2 <- sum(unlist(lapply(mwra.adj.med, "length"))
                  ,unlist(lapply(uwra.adj.med, "length")))
        if(identical(0, as.double(ln2))) stop("Married and unmarried adjusted medians have no countries or aggregates in common.")

        ## Make sure denominator counts are in same order as indicators
        for(i in c(data.els)) {
            if(!isTRUE(all.equal(names(mwra.adj.med[["W.Lg.t"]])
                        , names(mwra.adj.med[[i]])
                          ))) {
                stop("Country/Aggregate names in 'mwra.adj.med[[", i, "]]' not equal to the names in 'mwra.adj.med[[W.Lg.t]]' (the denominator counts).")
            }
            if(!isTRUE(all.equal(names(uwra.adj.med[["W.Lg.t"]])
                        , names(uwra.adj.med[[i]])
                          ))) {
                stop("Country/Aggregate names in 'uwra.adj.med[[", i, "]]' not equal to the names in 'uwra.adj.med[[W.Lg.t]]' (the denominator counts).")
            }
        }
        if(!isTRUE(all.equal(names(mwra.adj.med[["W.Lg.t"]]), names(uwra.adj.med[["W.Lg.t"]])))) {
            stop("Country/Aggregate names for denominator counts objects not same across MWRA and UWRA.")
        }

        ## Check the number of years of denominator counts in married and unmarried.
        max.no.years.denom.mwra <- max(sapply(mwra.adj.med[["W.Lg.t"]], "length"))
        max.no.years.denom.uwra <- max(sapply(uwra.adj.med[["W.Lg.t"]], "length"))
        max.no.years.denom <- max(max.no.years.denom.mwra, max.no.years.denom.uwra)

        not.max.denom.mwra <-
            !(sapply(mwra.adj.med[["W.Lg.t"]], "length") == max.no.years.denom)
        not.max.denom.uwra <-
            !(sapply(uwra.adj.med[["W.Lg.t"]], "length") == max.no.years.denom)

        if(sum(not.max.denom.mwra) > 0) {
            stop("Married  women denominator counts for some countries have different number of years available. The countries/aggregates are: ", paste(names(mwra.adj.med[["W.Lg.t"]])[not.max.denom], collapse = ", "))
        }
        if(sum(not.max.denom.uwra) > 0) {
            stop("Unmarried  women denominator counts for some countries have different number of years available. The countries/aggregates are: ", paste(names(uwra.adj.med[["W.Lg.t"]])[not.max.denom], collapse = ", "))
        }

        ## -------**** Third (Indicator) Level

        for(i in seq_along(data.els)) {
            mismatch.CP <- 0
            for(j in seq_along(mwra.adj.med[[i]])) {
                nm.mwra <- names(mwra.adj.med[[i]][[j]])
                nm.uwra <- names(uwra.adj.med[[i]][[j]])
                nm.both <- intersect(nm.mwra, nm.uwra)
                if(identical(length(nm.both), 0L)) {
                    stop(paste0("'mwra.adj.med[[", data.els[i], "]][[", j, "]]' and 'uwra.adj.med[[", data.els[i]
                               ,"]][[", j, "]]' do not have any CP indicators in common."))
                }
                if(!identical(nm.mwra, nm.uwra)) {
                    mismatch.CP <- mismatch.CP + 1
                    mwra.adj.med[[i]][[j]] <- mwra.adj.med[[i]][[j]][nm.both]
                    uwra.adj.med[[i]][[j]] <- uwra.adj.med[[i]][[j]][nm.both]
                }
            }
            if(mismatch.CP > 0) {
                warning(paste0("Some elements of 'mwra.adj.med[[", data.els[i], "]][[j]]' and 'uwra.adj.med[[", data.els[i], "]][[j]]' do not have the same CP indicators (in the same order). Only those in common will be processed (after re-ordering)."))
            }
        }

        ## -------**** Bottom (data frame) Level

        ## It's possible different estimation years were produced for
        ## married and unmarried.

        for(i in seq_along(data.els)) {
            for(j in seq_along(mwra.adj.med[[i]])) {
                for(k in seq_along(mwra.adj.med[[i]][[j]])) {
                    nm.mwra <- colnames(mwra.adj.med[[i]][[j]][[k]])
                    nm.uwra <- colnames(uwra.adj.med[[i]][[j]][[k]])
                    nm.both <- intersect(nm.mwra, nm.uwra)
                    if(identical(length(nm.both), 0L)) {
                        stop(paste0("'mwra.adj.med[[", data.els[i], "]][[", j, "]][[", k, "]]' and 'uwra.adj.med[[", data.els[i], "]][[", j, "]][[", k, "]]' do not have any estimation years in common."))
                    }
                    if(!identical(nm.mwra, nm.uwra)) {
                        warning(paste0("'mwra.adj.med[[", data.els[i], "]][[", j, "]][[", k, "]]' and 'uwra.adj.med[[", data.els[i], "]][[", j, "]][[", k, "]]' do not have the same estimation years. Only those in common will be processed."))
                        mwra.adj.med[[i]][[j]][[k]] <-
                            mwra.adj.med[[i]][[j]][[k]][, nm.both, drop = FALSE]
                        uwra.adj.med[[i]][[j]][[k]] <-
                            uwra.adj.med[[i]][[j]][[k]][, nm.both, drop = FALSE]
                    }
                }
            }
        }

        ## -------* MAIN

        ## -------** Sum MWRA and UWRA _COUNTS_

        awra.adj.med <-
            list(CIcount.Lg.Lcat.median.adj =
                     mapply(FUN = function(a, b) { #Afghanistan, etc.
                         mapply(FUN = function(c, d) { #Total.Adj, etc.
                                 c + d
                             }, a, b, SIMPLIFY = FALSE)
                     }
                    ,mwra.adj.med[["CIcount.Lg.Lcat.median.adj"]]
                    ,uwra.adj.med[["CIcount.Lg.Lcat.median.adj"]]
                    ,SIMPLIFY = FALSE
                     ))

        ## -------** Make denominator counts

        ## Add MWRA and UWRA denominator counts
        awra.denom.counts <-
            mapply(function(a, b) {
                    l <- min(length(a), length(b))
                    a[1:l] + b[1:l] #don't recycle
            }, mwra.adj.med[["W.Lg.t"]], uwra.adj.med[["W.Lg.t"]]
           ,SIMPLIFY = FALSE)

        ## Save the denominator counts
        awra.adj.med[["W.Lg.t"]] <- awra.denom.counts

        ## -------** Adjust the _proportions_

        awra.adj.med[["CIprop.Lg.Lcat.median.adj"]] <-
            mapply(function(a, b) {
                lapply(a, function(z, denom) { z / denom }
                       ,denom = b)
            }, awra.adj.med[["CIcount.Lg.Lcat.median.adj"]]
           ,awra.denom.counts, SIMPLIFY = FALSE)

        ## -------** Adjust the _ratios_

        awra.adj.med[["CIratio.Lg.Lcat.median.adj"]] <-
            lapply(awra.adj.med[["CIprop.Lg.Lcat.median.adj"]]
                   ,function(z) {
                       list(`Modern/Total.Adj` = z[["Modern.Adj"]] / z[["Total.Adj"]]
                           ,`Met Demand.Adj` = z[["Total.Adj"]] / z[["TotalPlusUnmet.Adj"]]
                           ,Z.Adj = z[["Unmet.Adj"]] / (1 - z[["TotalPlusUnmet.Adj"]])
                           ,`Met Demand with Modern Methods.Adj` =
                                z[["Modern.Adj"]] / z[["TotalPlusUnmet.Adj"]]
                            )
                   }
                   )

        ## -------** ISO

        ## Common ISOs in right order
        awra.adj.med[["iso.g"]] <- mwra.adj.med[["iso.g"]][nm.both.cname.in.mwra]

        ## -------* RETURN

        return(awra.adj.med[c("CIprop.Lg.Lcat.median.adj", "CIratio.Lg.Lcat.median.adj"
                            ,"CIcount.Lg.Lcat.median.adj", "iso.g", "W.Lg.t")])
    }


##' Create aggregates proprotions, counts, and ratios for a single grouping.
##'
##' \code{\link{InternalAggregateMedians}} is for use by
##' \code{\link{AggregateMedians}}. It creates aggregates for proprotions,
##' counts and ratios for a single grouping (e.g., major UNPD regions).
##'
##' @param W.Lc.t
##' @param select.c
##' @return
##' @author Mark Wheldon, based on \code{\link{InternalGetAggregates}}.
##' @noRd
InternalAggregateMedians <- function(counts.df, W.Lc.t
                                    ,select.iso, est.years) {

    ## -------* SET UP

    ## -------* SUB FUNCTIONS

    ## Convert the aggregate matrices into lists of required format
    mkList <- function(x) {
        y <- t(x[,-1])
        colnames(y) <- x[,1]
        y <- as.list(as.data.frame(y))
        lapply(y, function(z) {
            z <- matrix(z, nrow = 1, dimnames = list("0.5", colnames(x)[-1]))
        })
        }

    ## -------* MAIN

    ## -------** Aggregate CU counts

    ## Keep only countries in the aggregate and columns with counts in them.
    agg.counts <-
        counts.df[counts.df$iso.c %in% select.iso, c("indicator", est.years)]
    ## Get aggregate counts
    agg.counts <- stats::aggregate(. ~ indicator, data = agg.counts, FUN = "sum")

    ## -------** Aggregate denominators

    ## Select only countries in the aggregate
    denom.li <-
        W.Lc.t[unique(counts.df[counts.df$iso.c %in% as.character(select.iso)
                              , "name.c"])]

    ## Aggregate but remove
    denom.counts <- plyr::ldply(denom.li, .id = "name.c") # make into data frame
    denom.counts <- colSums(denom.counts[, -1]) # first col is '.id'
    denom.counts <-
        matrix(denom.counts, nrow = nrow(agg.counts), ncol = length(denom.counts)
              ,byrow = TRUE)   # Fill by row

    ## -------** Compute proportions

    agg.props <- agg.counts[,-1] / denom.counts
    agg.props <- cbind(indicator = agg.counts[,1], agg.props)

    ## -------** Compute ratios

    indicator <- agg.counts$indicator
    met.demand <-
        agg.counts[indicator == "Total.Adj", -1] /
        agg.counts[indicator == "TotalPlusUnmet.Adj", -1]
    met.demand.modern <-
        agg.counts[indicator == "Modern.Adj", -1] /
        agg.counts[indicator == "TotalPlusUnmet.Adj", -1]
    modern.ovr.total <-
        agg.counts[indicator == "Modern.Adj", -1] /
        agg.counts[indicator == "Total.Adj", -1]

    ## -------* RETURN

    ## Make lists in the fashion of 'GetAggregates()'
    agg.counts.list <- mkList(agg.counts)
    agg.props.list <- mkList(agg.props)

    agg.ratios.list <-
        list(as.matrix(met.demand), as.matrix(met.demand.modern)
            ,as.matrix(modern.ovr.total))

    agg.ratios.list <-          # set rownames to match style
        lapply(agg.ratios.list, function(z) {
            dimnames(z)[[1]] <- "0.5"
            return(z)
            })
    names(agg.ratios.list) <-
        c("Met Demand.Adj", "Met Demand with Modern Methods.Adj"
         ,"Modern/Total.Adj")

    W.t <- denom.counts[1,,drop = FALSE]
    colnames(W.t) <- est.years
    rownames(W.t) <- NULL

    ## output
    out <- list(CIprop.Lg.Lcat.median.adj = agg.props.list
               ,CIratio.Lg.Lcat.median.adj = agg.ratios.list
               ,CIcount.Lg.Lcat.median.adj = agg.counts.list
                ,W.t = W.t
                )

    return(out)

    }


##' Create aggregates only from country medians.
##'
##' \code{\link{AggregateMedians}} Produces regional and other aggregates from
##' medians (e.g., as produced by \code{\link{AdjustMedians}}.
##'
##' This function is based on \code{\link{GetAggregates}} and
##' \code{\link{InternalGetAggregates}} which take a set of saved MCMC
##' trajectories as input.
##'
##' The aggregation is done on the counts. The function
##' expects 'res' to be a list with first-level elements different indicator
##' types (e.g., counts, proportions, ratios). The counts are found by taking
##' the first element with name matching the regexp 'CIcount'.
##'
##' @param res Object in the form produced by
##'     \code{\link{AdjustMedians}}, which is the same as that produced
##'     by \code{\link{GetCIs}}. Aggregation is done on the counts; see details
##'     for how these are found.
##' @param file.aggregates If NULL (default), UNPD aggregates are
##'     constructed. Alternatively, file path of alternative grouping should be
##'     given, e.g. \code{file.aggregates = "data/MDGgroupings.csv")}. Such data
##'     file needs to contain columns \code{iso.country}, \code{groupname} and
##'     \code{iso.group} (which may contain missing values). Each country can
##'     only be included once (can only be part of one grouping).
##' @param output.dir Directory containing MCMC output.
##' @param all.women Logical. Is this for an 'all women' run?
##' @return
##' @author Mark Wheldon, with great swathes copied from \code{\link{GetAggregates}} and
##'     \code{\link{InternalGetAggregates}}.
##' @noRd
AggregateMedians <- function(res,
                             file.aggregates = NULL,
                             output.dir,
                             all.women = FALSE
                             ){

    ## -------* SET UP

    ## ## -------** Tables

    ## if(tabulate) {
    ##     if(is.null(table.dir)) {
    ##         if(!is.null(output.dir)) {
    ##             table.dir <- file.path(output.dir, "table", "cf_adj_orig")
    ##             if(!dir.exists(table.dir)) stopifnot(dir.create(table.dir))
    ##         } else stop("Must specify 'table.dir' or 'output.dir'")
    ##     }
    ## }

    ## -------* INPUTS

    load(file.path(output.dir, "mcmc.meta.rda"))
    if(mcmc.meta$general$include.c.no.data) {
        c.info <-
            rbind(mcmc.meta$data.raw$country.info
                 ,mcmc.meta$data.raw$country.info.no.data
                  )
    } else c.info <- mcmc.meta$data.raw$country.info


    if(!is.null(file.aggregates)) {
        group.data <- ReadFileAggregates(file.aggregates)
        if (is.null(group.data$iso.country) || is.null(group.data$groupname) || is.null(group.data$iso.group)) {
            cat("The csv file provided does not contain column(s) iso.country and/or groupname and/or iso.group!", "\n")
            cat("Fix that (note that missing values in iso.group column are allowed).", "\n")
            stop()
        }}

    ## Country outputs (incl. denominator counts)
    if(all.women) {
        load(file.path(output.dir, "res.country.all.women.rda")) #called 'res.country.all.women'
        res.orig <- res.country.all.women
    } else {
        load(file.path(output.dir, "res.country.rda")) #called 'res.country'
        res.orig <- res.country
    }

    ## -------* CONSTANTS

    ## Estimated years
    est.years <- dimnames(res[[grep("CIprop", names(res))]][[1]][[1]])[[2]]

    ## -------* SUB FUNCTIONS

    ## Convert res (or a subet) to a data frame with years as individual
    ## columns, indicators and countries transposed. Like this:
    ##
    ##   country indicator 1990 1991 1992
    ## 1  france     total    1    2    3
    ## 2  france     unmet    4    5    6
    ## 3  france    demand    7    8    9
    ## 4  sweden     total   10   11   12
    ## 5  sweden     unmet   13   14   15
    ## 6  sweden    demand   16   17   18
    makeDf <- function(x) {
        df <- plyr::ldply(lapply(x, plyr::ldply, .id = "indicator"), .id = "name.c")
        factor.cols <- which(sapply(df, "is.factor"))
        for(j in factor.cols) {
            df[,j] <- as.character(df[,j])
        }
        return(df)
    }

    ## -------* MAIN

    ## Counts as a dataframe
    median.counts.df <-
        makeDf(res[[grep("CIcount", x = names(res))[1]]])

    ## Merge on ISO codes and region information
    median.counts.df <-
        merge(median.counts.df
              ,data.frame(iso.c = res$iso.g
                         ,name.c = names(res[[grep("CIcount", x = names(res))]])
                          ,stringsAsFactors = FALSE)
             ,by = "name.c"    #country names will be same
              )
    median.counts.df <-
        merge(median.counts.df
             ,c.info[,!(colnames(c.info)=="name.c")]
             ,by = "iso.c"
              )

    ## -------** Aggregates

    ## -------*** Stuff from 'GetAggregates()'.

    W.Lc.t <- res.orig$W.Lg.t # W.L**c**.t
    C <- length(c.info$name.c)
    res.aggregate <- list()

    if (is.null(file.aggregates)){# construct UNPD aggregates

        ## -------*** UNPD Aggregates

        cat("Overview: Constructing aggregates for UNPD regions, and dev/dev-ing countries (excl China)", "\n")
        region.info <- mcmc.meta$data.raw$region.info
        G <-  (3+ # dev. dev-ing, dev-ing excl china
               1+ # world
               1+ # Oceania, deving only
               region.info$n.subreg
            +region.info$n.reg -1) # -1 because northern america is subregion
                                # and region
        iso.g <- rep(NA, G) # maybe to include later!
        G <- length(iso.g)
        W.Lg.t <- list()        # W.L**g**.t

        ## -------**** Make Aggregates

        ## Harcode some of these for now, to match GetAggregates(). Change later so that
        ## columns in 'c.info' are used.

        ## -------***** Developed regions

        cat("Constructing aggregates for the developED regions","\n")
        ## [MCW-2016-09-02-11] :: Hard-code developed regions.
        select.iso <- get_aggregate_ISOs(name = "developed countries", family = "UNPD")
        nameg <- "Developed regions"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Developing regions

        cat("Constructing aggregates for the developING countries","\n")
                select.iso <- get_aggregate_ISOs(name = "developing countries", family = "UNPD")
        nameg <- "Developing regions"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Developing excl China

        cat("Constructing aggregates for the developing regions, excl. China","\n")
        select.iso <- get_aggregate_ISOs(name = "developing countries excluding China", family = "UNPD")
        nameg <- "Developing (excl. China)"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Mela-Micro-Polynesia

        cat("Constructing aggregates for Mela-Micro-Polynesia","\n")
        select.iso <- get_aggregate_ISOs(name = "Mela-Micro-Polynesia", family = "UNPD")
        nameg <- "Mela-Micro-Polynesia"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** FP2020 69 Countries

        ## [MCW-2016-09-02-13] :: Aggregate for 69 countries.
        cat("Constructing aggregate for the 69 FP2020 countries","\n")
        select.iso <- get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD")
        nameg <- "FP2020 69 Countries"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** More Developed Regions

        ## [MCW-2017-09-27] :: Aggregate for More developed regions
        select.iso <- get_aggregate_ISOs(name = "more developed countries", family = "UNPD")
        nameg <- "More developed regions"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Less Developed Regions

        ## [MCW-2017-09-27] :: Aggregate for less developed regions
        select.iso <- get_aggregate_ISOs(name = "less developed countries", family = "UNPD")
        nameg <- "Less developed regions"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Least Developed Regions

        ## [MCW-2017-09-27] :: Aggregate for Least developed regions
        select.iso <- get_aggregate_ISOs(name = "least developed countries", family = "UNPD")
        nameg <- "Least developed regions"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Other Developing Regions

        ## [MCW-2017-09-27] :: Aggregate for other developing regions
        select.iso <- get_aggregate_ISOs(name = "other developing countries", family = "UNPD")
        nameg <- "Other developing regions"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Less Developed Regions, Excluding China

        ## [MCW-2017-09-27] :: Aggregate for less developed regions, excluding China
        select.iso <- get_aggregate_ISOs(name = "less developed countries excluding China", family = "UNPD")
        nameg <- "Less developed regions, excluding China"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Sub-Saharan Africa

        ## [MCW-2018-03-23] :: Aggregate for Sub-Saharan Africa
        select.iso <- get_aggregate_ISOs(name = "sub-Saharan Africa", family = "UNPD")
        nameg <- "Sub-Saharan Africa"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

        ## -------***** Sub regions

        for (subreg in 1:region.info$n.subreg){
            cat("Constructing aggregates for subregion", subreg, "(out of", region.info$n.subreg, "subregions)","\n")
            select.iso <- c.info$iso.c[c.info$subreg.c == subreg]
            nameg <- region.info$name.subreg[subreg]
            res.aggregate[[nameg]] <-
                InternalAggregateMedians(median.counts.df
                                        ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                        ,est.years = est.years)
        }

        ## -------***** Region

        for (reg in 1:region.info$n.reg){
            cat("Constructing aggregates for region", reg, "(out of", region.info$n.reg, "regions)","\n")
            select.iso <- c.info$iso.c[c.info$reg.c==reg]
            nameg <- region.info$name.reg[reg]
            res.aggregate[[nameg]] <-
                InternalAggregateMedians(median.counts.df
                                        ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                        ,est.years = est.years)
        }

        ## ------- **** World

        cat("Constructing aggregates for the world","\n")
        select.iso <- c.info$iso.c
        nameg <- "World"
        res.aggregate[[nameg]] <-
            InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                    ,est.years = est.years)

    } else { ## END 'is.null(file.aggregates)'

        ## -------*** Alternative Aggregates ('file.aggregates').

      groupname.m <- group.data$groupname
      iso.m <- group.data$iso.country
      groupnames <- unique(groupname.m)
      G <- length(groupnames)
      iso.g  <- rep(NA, G)
        ## W.gt <- matrix(NA, G, length(W.Lc.t))
      for (g in 1:G){
        nameg <- paste(groupnames[g])
        cat("Constructing aggregates for", nameg, "(", G, "groups in total)","\n")
        select.iso <- iso.m[groupname.m==nameg]
        ## iso.g[g] <- group.data$iso.group[select.c[1]]
        res.aggregate[[nameg]] <- InternalAggregateMedians(median.counts.df
                                    ,W.Lc.t = W.Lc.t, select.iso = select.iso
                                     ,est.years = est.years)
      }
    }

    ## -------* RETURN

    ## -------** Re-order levels of res.aggregate

    W.Lg.t <- lapply(res.aggregate, function(l) l$W.t)
    CIprop.Lg.Lcat.median.adj <- lapply(res.aggregate, function(l) l$CIprop.Lg.Lcat.median.adj)
    CIratio.Lg.Lcat.median.adj <- lapply(res.aggregate, function(l) l$CIratio.Lg.Lcat.median.adj)
    CIcount.Lg.Lcat.median.adj <- lapply(res.aggregate, function(l) l$CIcount.Lg.Lcat.median.adj)

    ##value<< Object of class \code{\link{Results}},
    ## either for all subregions, regions and the world (UNDP aggregates),
    ## or for alternative groupings.
    return(list(CIprop.Lg.Lcat.median.adj = CIprop.Lg.Lcat.median.adj,
                                #<< Proportions for Total, Traditional, Modern,
                                #Unmet, TotalPlusUnmet,
                                #TradPlusUnmet. Percentages are included as
                                #names in the \code{.qt}-matrix without
                                #percentage signs.
                CIratio.Lg.Lcat.median.adj = CIratio.Lg.Lcat.median.adj,
                                #<<Ratios Met Demand, Z (unmet/none) and
                                #modern/total (R). R and Z might not be included
                                #for aggregates.
                CIcount.Lg.Lcat.median.adj = CIcount.Lg.Lcat.median.adj,
                                #<< Counts for same categories as under proportions.
                W.Lg.t = W.Lg.t, #<< Number of MWRAiso.g = iso.g,  #<< Iso codes, but note that aggregate names
                                #(never missing) are used to name the results
                                #lists.
                iso.g = iso.g  #<< Iso codes, but note that aggregate names
                                #(never missing) are used to name the results
                                #lists.
                ))
}


##' Compare original posterior medians with adjusted medians.
##'
##' \code{CompareAdjMedians} Compares original posterior medians (medians of the
##' posterior marginal distributions for each parameter) with medians adjusted
##' by \code{\link{AdjustMedians}}.
##'
##' .. content for \details{} ..
##' @param output.dir Run name from which to extract the original (unadjusted)
##'     marginal posterior medians. Must supply either 'output.dir' or
##'     'res.orig'. 'output.dir' is ignored if 'res.orig' is given.
##' @param res.orig List as produced by \code{\link{GetCIs}}. Must contain
##'     elements named "CIprop.Lg.Lcat.qt", "CIratio.Lg.Lcat.qt", and
##'     "CIcount.Lg.Lcat.qt" which are two-level lists, first level is country,
##'     second level is indicator (e.g., 'Total', 'Modern', etc.). Must supply
##'     either 'output.dir' or 'res.orig'.
##' @param res.adj List as produced by \code{\link{AdjustMedians}}. Same
##'     form as 'res.orig'.
##' @param name.res Character. One of 'country' or 'UNDPaggregates'.
##' @param adj.method Method of adjustment to use. See
##'     \code{\link{AdjustMedians}}.
##' @param plot Logical. Make some plots comparing original and adjusted?
##' @param plot.dir Directory to save plots as .pdf files.
##' @param tabulate Logical. Make tables comparing original and adjusted? These
##'     are tabulations of the object returned if 'return.res' = TRUE.
##' @param table.dir Directory to save tables as .csv files.
##' @param return.res Logical. Should the function return a list with comparison
##'     results?
##' @return If 'return.res' is TRUE, a list with comparison results. If 'plot'
##'     is TRUE, comparison plots are saved in 'plot.dir'. If 'tabulate' is
##'     true, comparison tables are saved in 'table.dir'.
##' @author Mark Wheldon.
##' @noRd
CompareAdjMedians <- function(run.name = "test",
                              output.dir = NULL,
                              res.orig = NULL,
                              res.adj,
                              name.res = "country",
                              adj.method = NULL,
                              plot = FALSE,
                              plot.dir = NULL,
                              tabulate = FALSE,
                              table.dir = NULL,
                              return.res = FALSE,
                              all.women = FALSE,
                              all.womenize.table.name = TRUE,
                              verbose = TRUE) {

    ## -------* SET UP

    ## -------** Plotting

    if(plot) {
        if(is.null(plot.dir)) {
            if(!is.null(output.dir)) {
                plot.dir <- file.path(output.dir, "fig", "cf_adj_orig")
                if(!dir.exists(plot.dir)) stopifnot(dir.create(plot.dir))
            } else stop("Must specify 'plot.dir' or 'output.dir'")
        }
    }

    ## -------** Tables

    if(tabulate) {
        if(is.null(table.dir)) {
            if(!is.null(output.dir)) {
                table.dir <- file.path(output.dir, "table", "cf_adj_orig")
                if(!dir.exists(table.dir)) stopifnot(dir.create(table.dir))
            } else stop("Must specify 'table.dir' or 'output.dir'")
        }
    }

    ## -------** Inputs

    ## -------*** Original results

    if(all(is.null(res.orig), is.null(output.dir))) stop("Must supply either 'output.dir' or 'res.orig'")
    if(is.null(res.orig)) {
        if(all.women) {
            if(name.res == "country") {
                load(file.path(output.dir, "res.country.all.women.rda"))
                res.orig <- res.country.all.women
            } else if(name.res == "UNPDaggregate") {
                load(file.path(output.dir, "res.aggregate.all.women.rda"))
                res.orig <- res.aggregate.all.women
            } else {                #other aggregates
                if(file.exists(file.path(output.dir, paste0(name.res, ".all.women.rda")))) {
                    res.loaded <- load(file.path(output.dir, paste0(name.res, ".all.women.rda")))
                } else stop("'", file.path(output.dir, paste0(name.res, ".rda")), "' does not exist. Check 'name.res'.")
                res.orig <- get(res.loaded)
            }
        } else {
            if(name.res == "country") {
                load(file.path(output.dir, "res.country.rda"))
                res.orig <- res.country
            } else if(name.res == "UNPDaggregate") {
                load(file.path(output.dir, "res.aggregate.rda"))
                res.orig <- res.aggregate
            } else {                #other aggregates
                if(file.exists(file.path(output.dir, paste0(name.res, ".rda")))) {
                    res.loaded <- load(file.path(output.dir, paste0(name.res, ".rda")))
                } else stop("'", file.path(output.dir, paste0(name.res, ".rda")), "' does not exist. Check 'name.res'.")
                res.orig <- get(res.loaded)
            }
        }
    }

    ## Extract medians from original results
    medians.orig <-
        lapply(res.orig[c("CIprop.Lg.Lcat.qt", "CIratio.Lg.Lcat.qt", "CIcount.Lg.Lcat.qt")]
              ,function(x) {
                  lapply(x, function(y) {
                      sapply(y, function(z) z["0.5",])
                  })
              })

    ## Extract upper and lower 95CI limits from original
    posterior.95ints.orig <-
        lapply(res.orig[c("CIprop.Lg.Lcat.qt", "CIratio.Lg.Lcat.qt", "CIcount.Lg.Lcat.qt")]
              ,function(x) {
                  lapply(x, function(y) {
                      lapply(y, function(z) z[c("0.025", "0.975"),])
                  })
              })

    ## Extract upper and lower 80CI limits from original
    posterior.80ints.orig <-
        lapply(res.orig[c("CIprop.Lg.Lcat.qt", "CIratio.Lg.Lcat.qt", "CIcount.Lg.Lcat.qt")]
              ,function(x) {
                  lapply(x, function(y) {
                      lapply(y, function(z) z[c("0.1", "0.9"),])
                  })
              })

    ## -------*** Adjusted results

    ## Extract medians from adjusted results
    medians.adj <-
        lapply(res.adj[c("CIprop.Lg.Lcat.median.adj", "CIratio.Lg.Lcat.median.adj"
                       , "CIcount.Lg.Lcat.median.adj")]
              ,function(x) {
                  lapply(x, function(y) {
                      sapply(y, function(z) z[1,])
                  })
              })

    ## -------*** Check for missing values

    ## Might be some countries that have 'NaN' (e.g., Northern Mariana
    ## Islands). Remove them from adj.
    if(identical(name.res, "country")) {
    nan.idx <- rapply(medians.adj
                     ,function(z) all(!is.finite(z))
                    , classes = "matrix", how = "replace"
                      )                # A list same structure as medians.adj
    medians.adj <- mapply(function(a, b) a[!as.logical(b)]
                         ,medians.adj, nan.idx, SIMPLIFY = FALSE
                          )

    ## Remove them from orig but need to do it by ISO code.
    iso.adj <-
        lapply(nan.idx, function(z) res.adj$iso.g[!as.logical(z)])
    iso.keep <- lapply(iso.adj, function(z) res.orig$iso.g %in% z)
    medians.orig <- mapply(function(a, b) a[b]
                          ,medians.orig, iso.keep, SIMPLIFY = FALSE
                           )
    posterior.95ints.orig <- mapply(function(a, b) a[b]
                          ,posterior.95ints.orig, iso.keep, SIMPLIFY = FALSE
                           )
    posterior.80ints.orig <- mapply(function(a, b) a[b]
                          ,posterior.80ints.orig, iso.keep, SIMPLIFY = FALSE
                           )
    }

    ## -------*** Re-scale

    ## Turn proportions into percentages
    medians.orig[["CIprop.Lg.Lcat.qt"]] <-
        lapply(medians.orig[["CIprop.Lg.Lcat.qt"]], "*", 100)
    medians.adj[["CIprop.Lg.Lcat.median.adj"]] <-
        lapply(medians.adj[["CIprop.Lg.Lcat.median.adj"]], "*", 100)

    posterior.95ints.orig[["CIprop.Lg.Lcat.qt"]] <-
        lapply(posterior.95ints.orig[["CIprop.Lg.Lcat.qt"]],
               function(z) lapply(z, "*", 100))

    posterior.80ints.orig[["CIprop.Lg.Lcat.qt"]] <-
        lapply(posterior.80ints.orig[["CIprop.Lg.Lcat.qt"]],
               function(z) lapply(z, "*", 100))

    ## -------*** (DO THESE STEPS LAST!!) AFFECTS ORDERING OF LIST ELEMENTS

    ## -------**** Check for same countries/aggregates AND SAME ORDERING

    ## NOTE: DO THIS LAST TO ENSURE SAME ORDERING OF COUNTRIES AGGREGATES.

    if(!identical(as.numeric(lapply(medians.orig, "length"))
                 ,as.numeric(lapply(medians.adj, "length"))
                  )
       ) warning("Original and adjusted results have different number of countries or aggregates.")

    c.in.orig <- names(medians.orig[[1]]) %in% names(medians.adj[[1]])
    c.in.adj <- names(medians.adj[[1]]) %in% names(medians.orig[[1]])
    c.in.both <- intersect(names(medians.orig[[1]]), names(medians.adj[[1]]))

    if(isTRUE(length(c.in.both) > 0)) {

    if(verbose) message(paste0("Original and adjusted results have "
                  ,length(c.in.both)
                  ," countries or aggregates in common."))

    } else {
        stop("Original and adjusted results have no countries or aggregates in common.")
        }

    medians.orig <-
        lapply(medians.orig, function(z) z[c.in.both]) #So orig and adj have
                                #countries/aggregates
                                #in same order.
    posterior.95ints.orig <-
        lapply(posterior.95ints.orig, function(z) z[c.in.both]) #So orig and adj have
                                #countries/aggregates
                                #in same order.
    posterior.80ints.orig <-
        lapply(posterior.80ints.orig, function(z) z[c.in.both]) #So orig and adj have
                                #countries/aggregates
                                #in same order.
    medians.adj <-
        lapply(medians.adj, function(z) z[c.in.both])

    ## -------**** Check columns in matrices in layer 3 in same order

    col.names.orig <- lapply(medians.orig, "lapply", "colnames")
    col.names.adj <- lapply(medians.adj, "lapply", "colnames")
    col.names.in.both <-
        mapply(function(a, b) {
            mapply(function(c, d) {
                d <- gsub("\\.Adj", "", d)
                intersect(c, d)
            }, a, b, SIMPLIFY = FALSE)
        }, col.names.orig, col.names.adj, SIMPLIFY = FALSE)

    if(verbose) {
    message("Original and adjusted results have the following indicators in common: ")
    for(i in seq_along(col.names.in.both)) {
        message("\t", paste(col.names.in.both[[i]][[1]], collapse = ", "))
    }
    }

    ## Order Adj and Orig
    medians.adj <-
        mapply(function(y, z) {
            mapply(function(w, x) {
                x <- paste0(x, ".Adj")
                w[,x]
            }, y, z, SIMPLIFY = FALSE)
        }, medians.adj, col.names.in.both, SIMPLIFY = FALSE)

    medians.orig <-
        mapply(function(y, z) {
            mapply(function(w, x) {
                w[,x]
            }, y, z, SIMPLIFY = FALSE)
        }, medians.orig, col.names.in.both, SIMPLIFY = FALSE)

    posterior.95ints.orig <-
        mapply(function(y, z) {
            mapply(function(w, x) {
                w[x]           # 'w' is a list, not a dataframe.
            }, y, z, SIMPLIFY = FALSE)
        }, posterior.95ints.orig, col.names.in.both, SIMPLIFY = FALSE)

    posterior.80ints.orig <-
        mapply(function(y, z) {
            mapply(function(w, x) {
                w[x]           # 'w' is a list, not a dataframe.
            }, y, z, SIMPLIFY = FALSE)
        }, posterior.80ints.orig, col.names.in.both, SIMPLIFY = FALSE)

    ## -------* MAIN

    ## -------** Absolute Differences

    ## -------*** Calculate

    ## -------**** Absolute Differences

    abs.diff <-
        mapply(FUN = function(y, z) { # First layer is 'CIprop...', 'CIratio...', etc.
            mapply(FUN = function(w, x) { # Second layer is country/aggregate.
                abs(w - x)
            }
            ,y, z, SIMPLIFY = FALSE)
        }, medians.adj, medians.orig, SIMPLIFY = FALSE)

    ## Median Abs diff across years, within country
    median.abs.diff.c <-
        lapply(rapply(abs.diff, function(z) apply(z, 2, "median")
              ,classes = "matrix"
              ,how = "replace"
                )
               ,plyr::ldply, .id = "name.c")

    ## Median Abs diff across countries, within year
    median.abs.diff.t <-
        lapply(abs.diff, function(z) {
            x <- plyr::ldply(z, .id = "name.c")
            x$year <- rep(rownames(abs.diff[[1]][[1]], length(abs.diff[[1]])))
            z <- plyr::ddply(x, .variables = "year", .fun = function(y) {
                for(j in 1:length(colnames(y)[!(colnames(y) %in% c("name.c", "year"))])) {
                    k <- colnames(y)[!(colnames(y) %in% c("name.c", "year"))][j]
                    if(j == 1) w <- median(y[,k])
                    else w <- c(w, median(y[,k]))
                }
                names(w) <- colnames(y)[!(colnames(y) %in% c("name.c", "year"))]
                w
            })
        })

    ## -------**** Raw Differences

    raw.diff <-
        mapply(FUN = function(y, z) { # First layer is 'CIprop...', 'CIratio...', etc.
            mapply(FUN = function(w, x) { # Second layer is country/aggregate.
                w - x
            }
            ,y, z, SIMPLIFY = FALSE)
        }, medians.adj, medians.orig, SIMPLIFY = FALSE)

    ## Median Abs diff across years, within country
    median.raw.diff.c <-
        lapply(rapply(raw.diff, function(z) apply(z, 2, "median")
              ,classes = "matrix"
              ,how = "replace"
                )
               ,plyr::ldply, .id = "name.c")

    ## Median Abs diff across countries, within year
    median.raw.diff.t <-
        lapply(raw.diff, function(z) {
            x <- plyr::ldply(z, .id = "name.c")
            x$year <- rep(rownames(raw.diff[[1]][[1]], length(raw.diff[[1]])))
            z <- plyr::ddply(x, .variables = "year", .fun = function(y) {
                for(j in 1:length(colnames(y)[!(colnames(y) %in% c("name.c", "year"))])) {
                    k <- colnames(y)[!(colnames(y) %in% c("name.c", "year"))][j]
                    if(j == 1) w <- median(y[,k])
                    else w <- c(w, median(y[,k]))
                }
                names(w) <- colnames(y)[!(colnames(y) %in% c("name.c", "year"))]
                w
            })
        })

    ## -------** Inside CIs

    inside.95ints.check <-
        mapply(FUN = function(a, b) { # First layer is 'CIprop...', 'CIratio...', etc.
            mapply(FUN = function(c, d) { # Second layer is country/aggregate.
                med.adj <- as.list(as.data.frame(d))
                med.adj <- med.adj[paste0(names(c), ".Adj")]
                mapply(FUN = function(e, f) {
                    check <- f >= e["0.025",] & f <= e["0.975",]
                                # NB: can have 'NaN' values in 'f' if,
                                # e.g., no denomiator counts. This
                                # will cause 'NA's in 'check' and then
                                # 'all()' will fail. So set 'na.rm = TRUE'.
                        if(!all(check, na.rm = TRUE)) {
                            check
                        } else {
                        NULL
                    }
                }, c, med.adj, SIMPLIFY = FALSE)
            }, a, b, SIMPLIFY = FALSE)
        }, posterior.95ints.orig, medians.adj, SIMPLIFY = FALSE)

    for(i in names(inside.95ints.check)) {
        for(j in names(inside.95ints.check[[i]])) {
            for(k in names(inside.95ints.check[[i]][[j]])) {
                if(!is.null(inside.95ints.check[[i]][[j]][[k]])) {
                    warning(paste(strwrap(paste0("Adjusted medians outside posterior 95 percent intervals for CP element '", i, "', country '", j, "', indicator '", k, "', years ",
                                                 paste(names(inside.95ints.check[[i]][[j]][[k]])[!inside.95ints.check[[i]][[j]][[k]]],
                                                       collapse = ", "
                                                       ), ".")), collapse = "\n"))
                }
            }
        }
    }

    inside.80ints.check <-
        mapply(FUN = function(a, b) { # First layer is 'CIprop...', 'CIratio...', etc.
            mapply(FUN = function(c, d) { # Second layer is country/aggregate.
                med.adj <- as.list(as.data.frame(d))
                med.adj <- med.adj[paste0(names(c), ".Adj")]
                mapply(FUN = function(e, f) {
                    if(all(!is.na(f)) && all(is.finite(f)) && all(!is.null(f)) &&
                       all(!is.na(e)) && all(is.finite(e)) && all(!is.null(e))) {
                        check <- f >= e["0.1",] & f <= e["0.9",]
                                # NB: can have 'NaN' values in 'f' if,
                                # e.g., no denomiator counts. This
                                # will cause 'NA's in 'check' and then
                                # 'all()' will fail. So set 'na.rm = TRUE'.
                        if(!all(check, na.rm = TRUE)) {
                            check
                        }
                    } else {
                        NULL
                    }
                }, c, med.adj, SIMPLIFY = FALSE)
            }, a, b, SIMPLIFY = FALSE)
        }, posterior.80ints.orig, medians.adj, SIMPLIFY = FALSE)

    for(i in names(inside.80ints.check)) {
        for(j in names(inside.80ints.check[[i]])) {
            for(k in names(inside.80ints.check[[i]][[j]])) {
                if(!is.null(inside.80ints.check[[i]][[j]][[k]])) {
                    warning(paste(strwrap(paste0("Adjusted medians outside posterior 80 percent intervals for CP element '", i, "', country '", j, "', indicator '", k, "', years ", paste(names(inside.80ints.check[[i]][[j]][[k]])[!inside.80ints.check[[i]][[j]][[k]]], collapse = ", "), ".")), collapse = "\n"))
                }
            }
        }
    }

    ## -------** Plots

    if(plot) {

        ## -------*** Raw Differences

        for(j in 1:length(raw.diff)) {

            j.stem <- gsub("\\.[Aa]dj", "", names(raw.diff)[j])
            j.qt <- gsub("median", "qt", j.stem)

            fnm <- file.path(plot.dir
                            ,paste0(run.name
                                   ,"_", name.res, "_", c("perc", "ratio", "count")[j]
                                   ,"_raw_diff"
                                   ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                   ,".pdf"))

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
            }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm, fixed = TRUE)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            ## common scale
            comm.ylim <-
                    c(min(unlist(raw.diff[[j]])), max(unlist(raw.diff[[j]]))) * 1.05

            pdf(file = fnm, width = 19.2, height = 11.8)

            for(k in colnames(raw.diff[[j]][[1]])) {

                k.stem <- gsub("\\.[Aa]dj", "", k)

                plot.df <-
                    plyr::ldply(lapply(raw.diff[[j]]
                                      ,function(z) z[,k]
                                       ), .id = "name.c")
                plot.df <- reshape::melt(plot.df, id.vars = "name.c", variable_name = "year")
                plot.df$value <- round(plot.df$value, 4)

                outside80.df <-
                    plyr::ldply(lapply(inside.80ints.check[[j.qt]]
                                      ,function(z) {
                                          if(!is.null(z[[k.stem]])) return(!z[[k.stem]])
                                          else return(NULL)
                                        }), .id = "name.c")

                outside95.df <-
                    plyr::ldply(lapply(inside.95ints.check[[j.qt]]
                                      ,function(z) {
                                          if(!is.null(z[[k.stem]])) return(!z[[k.stem]])
                                          else return(NULL)
                                        }), .id = "name.c")

                if(nrow(outside80.df) > 0) {

                    outside80.df <- reshape::melt(outside80.df
                                              ,id.vars = "name.c")
                    colnames(outside80.df)[colnames(outside80.df) == "variable"] <- "year"
                    colnames(outside80.df)[colnames(outside80.df) == "value"] <- "outside80"
                    outside80.df$name.c <- as.character(outside80.df$name.c)
                    plot.df <- merge(plot.df, outside80.df
                                      ,by = c("name.c", "year")
                                      ,all.y = TRUE, all.x = TRUE
                                       ,sort = FALSE
                                     )
                    plot.df$outside80[is.na(plot.df$outside80)] <- FALSE
                    plot.df$outside <- NA
                    plot.df$outside[plot.df$outside80] <- "80% PI"
                    plot.df$outside.value[!is.na(plot.df$outside)] <-
                        plot.df$value[!is.na(plot.df$outside)]
                }

                if(nrow(outside95.df) > 0) {

                    outside95.df <- reshape::melt(outside95.df
                                              ,id.vars = "name.c")
                    colnames(outside95.df)[colnames(outside95.df) == "variable"] <- "year"
                    colnames(outside95.df)[colnames(outside95.df) == "value"] <- "outside95"
                    outside95.df$name.c <- as.character(outside95.df$name.c)
                    plot.df <- merge(plot.df, outside95.df
                                      ,by = c("name.c", "year")
                                      ,all.y = TRUE, all.x = TRUE
                                       ,sort = FALSE
                                       )
                    plot.df$outside95[is.na(plot.df$outside95)] <- FALSE
                    plot.df$outside[plot.df$outside95] <- "95% PI"
                }

                gp <-
                    ggplot2::ggplot(data = plot.df
                                   ,ggplot2::aes(x = name.c, y = value)
                                    ) +
                    ggplot2::geom_boxplot() +
                    ggplot2::geom_hline(yintercept = 0) +
                    ggplot2::ylim(comm.ylim) +
                    ggplot2::ylab(paste0("adjusted - orig\n(", c("perc", "ratio", "count")[j],")")) +
                    ggplot2::xlab("Name") +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1)) +
                    ggplot2::ggtitle(paste0(k.stem, " ", c("perc", "ratio", "count")[j]
                                           ,", raw difference, ", name.res, "-level"))

                if(nrow(outside80.df) > 0 || nrow(outside95.df) > 0) {
                    gp <- gp + ggplot2::geom_point(ggplot2::aes(x = name.c
                                                      ,y = outside.value
                                                  ,col = outside)
                                                  ,shape = 1
                                                   ,alpha = 0.5) +
                        ggplot2::scale_colour_manual(breaks = c("80% PI", "95% PI")
                                           ,values = c("yellow4", "red")) +
                        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
                    }

                print(gp)
            }

            dev.off()

        }

        ## -------*** Absolute Differences

            for(j in 1:length(abs.diff)) {

            j.stem <- gsub("\\.[Aa]dj", "", names(raw.diff)[j])
            j.qt <- gsub("median", "qt", j.stem)

            fnm <- file.path(plot.dir
                            ,paste0(run.name
                                   ,"_", name.res, "_", c("perc", "ratio", "count")[j]
                                   ,"_abs_diff"
                                   ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                   ,".pdf"))

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
            }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm, fixed = TRUE)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            ## common scale
            if(identical(c("perc", "ratio", "count")[j], "perc")) {
                comm.ylim <- c(0, max(unlist(abs.diff[[j]])) * 1.05)
            } else {
                comm.ylim <-
                    c(min(unlist(abs.diff[[j]])), max(unlist(abs.diff[[j]]))) * 1.05
            }

            pdf(file = fnm, width = 19.2, height = 11.8)

            for(k in colnames(abs.diff[[j]][[1]])) {

                k.stem <- gsub("\\.[Aa]dj", "", k)

                plot.df <-
                    plyr::ldply(lapply(abs.diff[[j]]
                                      ,function(z) z[,k]
                                       ), .id = "name.c")
                plot.df <- reshape::melt(plot.df, id.vars = "name.c", variable_name = "year")
                plot.df$value <- round(plot.df$value, 4)

                outside80.df <- plyr::ldply(lapply(inside.80ints.check[[j.qt]]
                                      ,function(z) {
                                          if(!is.null(z[[k.stem]])) return(!z[[k.stem]])
                                          else return(NULL)
                                        }), .id = "name.c")

                outside95.df <- plyr::ldply(lapply(inside.95ints.check[[j.qt]]
                                      ,function(z) {
                                          if(!is.null(z[[k.stem]])) return(!z[[k.stem]])
                                          else return(NULL)
                                        }), .id = "name.c")

                if(nrow(outside80.df) > 0) {

                    outside80.df <- reshape::melt(outside80.df
                                              ,id.vars = "name.c")
                    colnames(outside80.df)[colnames(outside80.df) == "variable"] <- "year"
                    colnames(outside80.df)[colnames(outside80.df) == "value"] <- "outside80"
                    outside80.df$name.c <- as.character(outside80.df$name.c)
                    plot.df <- merge(plot.df, outside80.df
                                      ,by = c("name.c", "year")
                                      ,all.y = TRUE, all.x = TRUE
                                       ,sort = FALSE
                                     )
                    plot.df$outside80[is.na(plot.df$outside80)] <- FALSE
                    plot.df$outside <- NA
                    plot.df$outside[plot.df$outside80] <- "80% PI"
                    plot.df$outside.value[!is.na(plot.df$outside)] <-
                        plot.df$value[!is.na(plot.df$outside)]
                }

                if(nrow(outside95.df) > 0) {

                    outside95.df <- reshape::melt(outside95.df
                                              ,id.vars = "name.c")
                    colnames(outside95.df)[colnames(outside95.df) == "variable"] <- "year"
                    colnames(outside95.df)[colnames(outside95.df) == "value"] <- "outside95"
                    outside95.df$name.c <- as.character(outside95.df$name.c)
                    plot.df <- merge(plot.df, outside95.df
                                      ,by = c("name.c", "year")
                                      ,all.y = TRUE, all.x = TRUE
                                       ,sort = FALSE
                                       )
                    plot.df$outside95[is.na(plot.df$outside95)] <- FALSE
                    plot.df$outside[plot.df$outside95] <- "95% PI"
                }
                gp <-
                    ggplot2::ggplot(data = plot.df
                                   ,ggplot2::aes(x = name.c, y = value)
                                    ) +
                    ggplot2::geom_boxplot() +
                    ggplot2::geom_hline(yintercept = 0) +
                    ggplot2::ylim(comm.ylim) +
                    ggplot2::ylab(paste0("|adjusted - orig|\n(", c("perc", "ratio", "count")[j],")")) +
                    ggplot2::xlab("Name") +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust=1)) +
                    ggplot2::ggtitle(paste0(k.stem, " ", c("perc", "ratio", "count")[j]
                                           ,", absolute difference, ", name.res, "-level"))

                if(nrow(outside80.df) > 0 || nrow(outside95.df) > 0) {
                    gp <- gp + ggplot2::geom_point(ggplot2::aes(x = name.c
                                                      ,y = outside.value
                                                  ,col = outside)
                                                  ,shape = 1
                                                   ,alpha = 0.5) +
                        ggplot2::scale_colour_manual(breaks = c("80% PI", "95% PI")
                                           ,values = c("yellow4", "red")) +
                        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1)))
                    }

                print(gp)
            }

            dev.off()

        }

        message(paste0("Plots saved to ", plot.dir))

    } ## END PLOTS

        ## -------** Tables

    if(tabulate) {

        ## -------*** All Results

        for(j in 1:length(abs.diff)) {

            tbl.df <- plyr::ldply(abs.diff[[j]], .id = "name.c")
            tbl.df$year <- rownames(abs.diff[[j]][[1]])
            tbl.df <-
                tbl.df[,c("name.c", "year"
                         ,colnames(tbl.df)[!(colnames(tbl.df) %in% c("name.c", "year"))]
                          )]

            fnm <- file.path(table.dir, paste0(run.name
                                              ,"_", name.res, "_", c("perc", "ratio", "count")[j]
                                              ,"_", "abs_diff", "_all"
                                              ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                              ,".csv"))

                ## Need to keep names from getting too long!
                fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Modern Married Over All", "Mod-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Trad Married Over All", "Trad-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Unmet Married Over All", "Unmet-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Modern Unmarried Over All", "Mod-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Trad Unmarried Over All", "Trad-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Unmet Unmarried Over All", "Unmet-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
            }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

        write.csv(tbl.df
                  ,file = fnm
                   ,row.names = FALSE
                  )

        }

        ## -------*** Within country medians

        for(j in 1:length(median.abs.diff.c)) {

            tbl.df <- median.abs.diff.c[[j]]

            fnm <- file.path(table.dir
                                       ,paste0(run.name
                                              ,"_", name.res, "_", c("perc", "ratio", "count")[j]
                                               ,"_", "abs_diff_med", "_by_c"
                                              ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                              ,".csv"))

                ## Need to keep names from getting too long!
                fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Modern Married Over All", "Mod-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Trad Married Over All", "Trad-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Unmet Married Over All", "Unmet-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Modern Unmarried Over All", "Mod-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Trad Unmarried Over All", "Trad-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Unmet Unmarried Over All", "Unmet-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
                }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            write.csv(tbl.df
                      ,file = fnm
                   ,row.names = FALSE
                     )
            }

        ## -------*** Within year medians

        for(j in 1:length(median.abs.diff.t)) {

            tbl.df <- median.abs.diff.t[[j]]

            fnm <- file.path(table.dir, paste0(run.name
                                              ,"_", name.res, "_", c("perc", "ratio", "count")[j]
                                              ,"_", "abs_diff_med","_by_yr"
                                              ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                              ,".csv"))

                ## Need to keep names from getting too long!
                fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Modern Married Over All", "Mod-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Trad Married Over All", "Trad-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Unmet Married Over All", "Unmet-MarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Modern Unmarried Over All", "Mod-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Trad Unmarried Over All", "Trad-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)
                fnm <- gsub("Unmet Unmarried Over All", "Unmet-UnmarriedOverAll"
                           ,fnm, fixed = TRUE)

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
                }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            write.csv(tbl.df
                      ,file = fnm
                   ,row.names = FALSE
                     )
        }

        ## -------*** Inside 95 CIs

        tbl.df <-
            data.frame(scale = integer(0), country = integer(0), indicator = integer(0)
                      ,year = integer(0), med.adj = integer(0)
                      ,l0.025.orig = integer(0), med.orig = integer(0), u0.975.orig = integer(0))

        for(i in names(inside.95ints.check)) {
            for(j in names(inside.95ints.check[[i]])) {
                for(k in names(inside.95ints.check[[i]][[j]])) {
                    obj <- inside.95ints.check[[i]][[j]][[k]]
                    if(!is.null(obj)) {
                        obj <- !obj #make it so 'TRUE' marks those where adj.med is outside 95PI.
                        yrs.outside <- names(obj)[obj & !is.na(obj)]
                        i.adj <- gsub("\\.qt", ".median.adj", x = i)
                        k.adj <- paste0(k, ".Adj")
                        tbl.df <-
                            rbind(tbl.df
                                 ,data.frame(scale = rep_len(i, sum(obj, na.rm = TRUE))
                                            ,country = rep_len(j, sum(obj, na.rm = TRUE))
                                            ,indicator = rep_len(k, sum(obj, na.rm = TRUE))
                                            ,yrs.outside = yrs.outside
                                            ,med.adj = medians.adj[[i.adj]][[j]][,k.adj][obj  & !is.na(obj)]
                                            ,l0.025.orig = posterior.95ints.orig[[i]][[j]][[k]]["0.025", obj & !is.na(obj)]
                                            ,med.orig = medians.orig[[i]][[j]][,k][obj & !is.na(obj)]
                                            ,u0.975.orig = posterior.95ints.orig[[i]][[j]][[k]]["0.975", obj & !is.na(obj)]
                                             ))
                    }
                }
            }
        }

        if(!identical(nrow(tbl.df), 0L)) {

            row.names(tbl.df) <- NULL

            fnm <- file.path(table.dir, paste0(run.name
                                              ,"_", name.res, "_", "outside_95pc_PI"
                                              ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                              ,".csv"))

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
            }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            write.csv(tbl.df
                     ,file = fnm
                     ,row.names = FALSE
                      )
        }

        ## -------*** Inside 80 CIs

        tbl.df <-
            data.frame(scale = integer(0), country = integer(0), indicator = integer(0)
                      ,year = integer(0), med.adj = integer(0)
                      ,l0.1.orig = integer(0), med.orig = integer(0), u0.9.orig = integer(0))

        for(i in names(inside.80ints.check)) {
            for(j in names(inside.80ints.check[[i]])) {
                for(k in names(inside.80ints.check[[i]][[j]])) {
                    obj <- inside.80ints.check[[i]][[j]][[k]]
                    if(!is.null(obj)) {
                        obj <- !obj #make it so 'TRUE' marks those where adj.med is outside 80PI.
                        yrs.outside <- names(obj)[obj & !is.na(obj)]
                        i.adj <- gsub("\\.qt", ".median.adj", x = i)
                        k.adj <- paste0(k, ".Adj")
                        tbl.df <-
                            rbind(tbl.df
                                 ,data.frame(scale = rep_len(i, sum(obj, na.rm = TRUE))
                                            ,country = rep_len(j, sum(obj, na.rm = TRUE))
                                            ,indicator = rep_len(k, sum(obj, na.rm = TRUE))
                                            ,yrs.outside = yrs.outside
                                            ,med.adj = medians.adj[[i.adj]][[j]][,k.adj][obj & !is.na(obj)]
                                            ,l0.1.orig = posterior.80ints.orig[[i]][[j]][[k]]["0.1", obj & !is.na(obj)]
                                            ,med.orig = medians.orig[[i]][[j]][,k][obj & !is.na(obj)]
                                            ,u0.9.orig = posterior.80ints.orig[[i]][[j]][[k]]["0.9", obj & !is.na(obj)]
                                             ))
                    }
                }
            }
        }

        if(!identical(nrow(tbl.df), 0L)) {

            row.names(tbl.df) <- NULL

            fnm <- file.path(table.dir, paste0(run.name
                                              ,"_", name.res, "_", "outside_80pc_PI"
                                              ,ifelse(!is.null(adj.method), paste0("_", adj.method), "")
                                              ,".csv"))

            if(all.women && all.womenize.table.name) {
                fnm <- file.path(dirname(fnm), makeAWFileName(basename(fnm)))
            }

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            write.csv(tbl.df
                     ,file = fnm
                     ,row.names = FALSE
                      )
        }

            message(paste0("Tables saved to ", table.dir))

    } ## END TABLES

    ## -------* RETURN

    if(return.res) {

        out.list <- list(abs.diff = abs.diff
                        ,median.abs.diff.c = median.abs.diff.c
                        ,median.abs.diff.t = median.abs.diff.t
                        ,inside.95ints.check = inside.95ints.check
                         )

        return(out.list)

    } else return(invisible())

}

##' Replace original with adjusted medians
##'
##' Replace original medians in output list with adjusted medians
##'
##' @param res.orig Ouptut list with original (unadjusted) posterior quantiles
##' @param res.adj Ouptut list with adjusted posterior medians
##'
##' @section Note: Only works for country results (not aggregates).
##'
##' @return \code{res.orig} but with medians replaced with adjusted medians.
##' @author Mark Wheldon
##' @noRd
ReplaceMediansWAdj <- function(res.orig, res.adj) {
    for(x in c("CIprop.Lg.Lcat", "CIratio.Lg.Lcat", "CIcount.Lg.Lcat")) {
        x.qt <- paste0(x, ".qt")
        x.adj <- paste0(x, ".median.adj")
        for(y in names(res.orig[[x.qt]])) {
            for(z in c("Total", "Traditional", "Modern", "Unmet",
                       "TotalPlusUnmet", "TradPlusUnmet")) {
                z.adj <- paste0(z, ".Adj")
                years <-
                    names(res.orig[[x.qt]][[y]][[z]]["0.5",])
                res.orig[[x.qt]][[y]][[z]]["0.5", years] <-
                    res.adj[[x.adj]][[y]][[z.adj]]["0.5", years]

            }
        }
    }
    return(res.orig)
}
