#----------------------------------------------------------------------
# summariseglobalrun.R
# Jin Rou New, Nov 2013
#----------------------------------------------------------------------
SummariseGlobalRun <- function(# Summarise info from global run required for country-specific runs
### Summarise info from global run required for country-specific runs
                               run.name = "test", ##<< Run name
                               output.dir = NULL ##<< Directory where MCMC array and meta are stored, and new objects are added
                               ## (if NULL, it's output/run.name, default from \code{runMCMC}).
                              ,write.model.fun = NULL
                               ) {
    if (is.null(output.dir))
        output.dir <- file.path(getwd(), "output", run.name)

    load(file.path(output.dir, "mcmc.meta.rda"))
    load(file.path(output.dir, "mcmc.array.rda"))

    if(is.null(write.model.fun)) write.model.fun <- mcmc.meta$general$write.model.fun

    ## Create parameter names lists

    parnames.list <- mcmc.meta$parnames.list
    parnames.to.save <- parnames.list$parnames.h

    ## [MCW-2018-02-02 (1)] ::
    ## Modifications to handle sexual activity models:
    ##
    ## - The number of (sub)regions is different depending on whether
    ##   geographic or sexual activity model is used. Thus the second
    ##   component of `parnames.to.save' needs to be changed accordingly.
    ##
    ## - In sexual activity models, the 'subreg' level consists of EITHER
    ##
    ##   1. Modified major areas (i.e,. 'regions') in sexual activity
    ##      groups.
    ##      - The correct number of these clusters is
    ##        `n.reg.in.sex.ac.unm'. They are named in
    ##        `name.reg.in.sex.ac.unm'.
    ##      - Note the 'reg' in the names of the counters (as opposed to
    ##        'subreg').
    ##      - The major areas are modified to allow for, e.g,. 'Africa (SA
    ##        high)' and 'Africa (SA low)'.
    ##      - `(ModelFunctionSubRegInSA(write.model.fun) &&
    ##        !ModelFunctionSubRegInSA1India(write.model.fun)) == TRUE' for
    ##        these models.
    ##
    ##   2. Modified major areas (i.e,. 'regions') nested in Sexual
    ##      Activity Group 0 and (unmodified) subregions nested within SA
    ##      Group 1.
    ##      - The correct number of these clusters is
    ##        `n.reg.in.sex.ac.unm.SA1sub'. They are named in
    ##        `name.reg.in.sex.ac.unm.SA1sub'.
    ##      - Note the 'reg' in the names of the counters (as opposed to
    ##        'subreg').
    ##      - `ModelFunctionSubRegInSA1India(write.model.fun) == TRUE' for
    ##        these models.
    ##
    ## - The 'reg' level consists of the Sexual Activity Groups, of which
    ##   there are only two. This is the value of `n.sex.ac.unm' and the
    ##   names are in `name.sex.ac.unm'.

    if(!ModelFunctionRateModel(write.model.fun)) {

        ## ---------- LEVEL MODEL ---------->

    if(ModelFunctionSubRegInSA1India(write.model.fun)) {

        ## ..... SEXUAL ACTIVITY MODELS .....>

        ## Sexual activity model with sub-regions nested in SA Group 1 and regions nested in SA Group 0 and India its own subregion
        parnames.to.save <-
            c(parnames.to.save,
              c(sapply(parnames.list$parnames.subreg, paste0, "[", 1:mcmc.meta$winbugs.data$n.reg.in.sex.ac.unm.SA1sub, "]")))
        name.subreg <- as.character(mcmc.meta$data.raw$region.info$name.reg.in.sex.ac.unm.SA1sub)

    } else if(ModelFunctionSA(write.model.fun)) {

        ## Sexual activity model with major areas (regions) in SA Group 1 and SA Group 0 and _not_ with india its own subregion
        warning("\nONE COUNTRY MODEL FOR THIS CLASS OF MODELS NOT THOROUGHLY TESTED!")
        parnames.to.save <-
            c(parnames.to.save,
              c(sapply(parnames.list$parnames.subreg, paste0, "[", 1:mcmc.meta$winbugs.data$n.reg.in.sex.ac.unm, "]")))
        name.subreg <- as.character(mcmc.meta$data.raw$region.info$name.reg.in.sex.ac.unm)

        ## <..... SEXUAL ACTIVITY MODELS .....

    } else {

        ## ..... GEOGRAPHIC MODEL ..... >

        parnames.to.save <-
            c(parnames.to.save,
              c(sapply(parnames.list$parnames.subreg, paste0, "[", 1:mcmc.meta$winbugs.data$n.subreg, "]")))
        name.subreg <- as.character(mcmc.meta$data.raw$region.info$name.subreg)

        ## <..... GEOGRAPHIC MODEL .....

    }

        parnames.to.save <- c(parnames.to.save,
                              paste0(parnames.list$T1.source.s, "[", 1:6, "]"),
                              paste0(parnames.list$T2.source.s, "[", 1:6, "]"),
                              paste0(parnames.list$T12.source.s, "[", 1:6, "]"),
                              c(sapply(c("unmet.intercept.c", "T.c", "RT.c", # change JR, 20140404
                                         "omega.c", "Romega.c",
                                         "pmax.c", "Rmax.c"), paste0, "[", 1:mcmc.meta$winbugs.data$C, "]")))
        ## Add countries with no data
        if(ModelFunctionInclNoData(write.model.fun) && is_finite_numeric(mcmc.meta$winbugs.data$C.no.data)) {
            parnames.to.save <- c(parnames.to.save,
                              c(sapply(c("unmet.intercept.c", "setlevel.c", "RT.c",
                                         "omega.c", "Romega.c",
                                         "pmax.c", "Rmax.c"), paste0, "[",
                                       (mcmc.meta$winbugs.data$C + 1):(mcmc.meta$winbugs.data$C + mcmc.meta$winbugs.data$C.no.data), "]")))
            }
        parnames.not.to.save <- c("w.world", "Rw.world", "T.world", "RT.world",
                                  "sigma.wreg", "sigma.Rwreg", "sigma.Treg", "sigma.RTreg",
                                  "sigma.wsubreg", "sigma.Rwsubreg", "sigma.Tsubreg", "sigma.RTsubreg")
        parnames.to.save <- parnames.to.save[!is.element(parnames.to.save, parnames.not.to.save)]

                                # check that all required parameters are found in the mcmc.array
        if (sum(!is.element(parnames.to.save, dimnames(mcmc.array)[[3]])) > 0) {
            cat("Warning: The following parameters cannot be found in the mcmc.array:\n")
            cat(setdiff(parnames.to.save, dimnames(mcmc.array)[[3]]))
        }
        parnames.to.save <- intersect(parnames.to.save, dimnames(mcmc.array)[[3]])

                                # get posterior median of all parameters to save
        mcmc.post <- lapply(parnames.to.save, function(parname) median(c(mcmc.array[, , parname]), na.rm = T))
        names(mcmc.post) <- parnames.to.save
        mcmc.post.sd <- lapply(parnames.to.save, function(parname) sd(c(mcmc.array[, , parname]), na.rm = T))
        names(mcmc.post.sd) <- parnames.to.save
                                # change JR, 20140409
        iso.c <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
        names(iso.c) <- mcmc.meta$data.raw$country.info$name.c
        data.global <- list(run.name.global = run.name,
                            name.subreg = name.subreg,
                            iso.c = iso.c,
                            cnotrich.index = mcmc.meta$winbugs.data$cnotrich.index, # change JR, 20140404
                            mcmc.post = mcmc.post,
                            mcmc.post.sd = mcmc.post.sd)

        ## <---------- LEVEL MODEL ----------

    } else {

        ## ========== RATE MODEL ==========>

    if(ModelFunctionSubRegInSA1India(write.model.fun)) {

        ## ..... SEXUAL ACTIVITY MODELS .....>

        ## Sexual activity model with india its own subregion
        parnames.to.save <-
            c(parnames.to.save,
              c(sapply(parnames.list$parnames.subreg, paste0, "[", 1:mcmc.meta$winbugs.data$n.reg.in.sex.ac.unm.SA1sub, "]")),
              c(sapply(parnames.list$parnames.reg, paste0, "[", 1:mcmc.meta$winbugs.data$n.sex.ac.unm, "]"))
              )
        name.subreg <- as.character(mcmc.meta$data.raw$region.info$name.reg.in.sex.ac.unm.SA1sub)
        name.reg <- as.character(mcmc.meta$data.raw$region.info$name.sex.ac.unm)

    } else if(ModelFunctionSA(write.model.fun)) {

        ## Sexual activity model _not_ with india its own subregion
        parnames.to.save <-
            c(parnames.to.save,
              c(sapply(parnames.list$parnames.subreg, paste0, "[", 1:mcmc.meta$winbugs.data$n.reg.in.sex.ac.unm, "]")),
              c(sapply(parnames.list$parnames.reg, paste0, "[", 1:mcmc.meta$winbugs.data$n.sex.ac.unm, "]"))
              )
        name.subreg <- as.character(mcmc.meta$data.raw$region.info$name.reg.in.sex.ac.unm)
        name.reg <- as.character(mcmc.meta$data.raw$region.info$name.sex.ac.unm)

        ## <..... SEXUAL ACTIVITY MODELS .....

    } else {

        ## ..... GEOGRPAHIC MODEL ..... >

        parnames.to.save <-
            c(parnames.to.save,
              c(sapply(parnames.list$parnames.subreg, paste0, "[", 1:mcmc.meta$winbugs.data$n.subreg, "]")),
              c(sapply(parnames.list$parnames.reg, paste0, "[", 1:mcmc.meta$winbugs.data$n.reg, "]"))) # change JR, 20150301
        name.subreg <- as.character(mcmc.meta$data.raw$region.info$name.subreg)
        name.reg <- as.character(mcmc.meta$data.raw$region.info$name.reg) # change JR, 20150301

        ## <..... GEOGRPAHIC MODEL .....

    }

        parnames.to.save <- c(parnames.to.save,
                              c(sapply(c("unmet.intercept.c", "setlevel.c", "RT.c",
                                         "omega.c", "Romega.c",
                                         "pmax.c", "Rmax.c"), paste0, "[", 1:mcmc.meta$winbugs.data$C, "]")))
        ## Add countries with no data
        if(ModelFunctionInclNoData(write.model.fun) && is_finite_numeric(mcmc.meta$winbugs.data$C.no.data)) {
            parnames.to.save <- c(parnames.to.save,
                              c(sapply(c("unmet.intercept.c", "setlevel.c", "RT.c",
                                         "omega.c", "Romega.c",
                                         "pmax.c", "Rmax.c"), paste0, "[",
                                       (mcmc.meta$winbugs.data$C + 1):(mcmc.meta$winbugs.data$C + mcmc.meta$winbugs.data$C.no.data), "]")))
            }
        parnames.not.to.save <- c("w.world", "Rw.world", "S.world", "RT.world",
                                  "sigma.wreg", "sigma.Rwreg", "sigma.Sreg", "sigma.RTreg"
                                # , "sigma.wsubreg", "sigma.Rwsubreg", "sigma.Tsubreg", "sigma.RTsubreg" # change JR, 20150301
                                  )
        parnames.to.save <- parnames.to.save[!is.element(parnames.to.save, parnames.not.to.save)]

                                # check that all required parameters are found in the mcmc.array
        if (sum(!is.element(parnames.to.save, dimnames(mcmc.array)[[3]])) > 0) {
            cat("Warning: The following parameters cannot be found in the mcmc.array:\n")
            cat(setdiff(parnames.to.save, dimnames(mcmc.array)[[3]]))
        }
        parnames.to.save <- intersect(parnames.to.save, dimnames(mcmc.array)[[3]])

                                # get posterior median of all parameters to save
        mcmc.post <- lapply(parnames.to.save, function(parname) median(c(mcmc.array[, , parname]), na.rm = T))
        names(mcmc.post) <- parnames.to.save
        mcmc.post.sd <- lapply(parnames.to.save, function(parname) sd(c(mcmc.array[, , parname]), na.rm = T))
        names(mcmc.post.sd) <- parnames.to.save
        iso.c <- gsub(" ", "", mcmc.meta$data.raw$country.info$code.c)
        names(iso.c) <- mcmc.meta$data.raw$country.info$name.c
        data.global <- list(run.name.global = run.name,
                            name.subreg = name.subreg,
                            name.reg = name.reg,
                            iso.c = iso.c,
                            cnotrich.index=mcmc.meta$winbugs.data$cnotrich.index,
                            med.max.trad.se=mcmc.meta$winbugs.data$med.max.trad.se,
                            med.max.modern.se=mcmc.meta$winbugs.data$med.max.modern.se,
                            med.max.unmet.se=mcmc.meta$winbugs.data$med.max.unmet.se,
                            mcmc.post=mcmc.post,
                            mcmc.post.sd = mcmc.post.sd)

        ## <========== RATE MODEL ==========

    }

    save(data.global, file = file.path(output.dir, "data.global.rda"))
    ##value<< \code{NULL}; Saves \code{data.global} to \code{output.dir}.
    return(invisible())
}
#----------------------------------------------------------------------
# The End!
