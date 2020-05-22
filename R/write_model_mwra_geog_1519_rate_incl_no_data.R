## Writes BUGS model to output.dir (stored in mcmc.meta) This is
### based on 'write_model_uwra_fix_source_vars.R', so it includes all
### the modifications made up to that point.

## Based on 'write_model_uwra_sa1subindia_rate.R'

WriteModel_MWRA_Geog_Rate_1519_InclNoData  <- function(mcmc.meta){
    filename.model <- paste0(ifelse(mcmc.meta$general$do.SS.run.first.pass, "model_pre.txt", "model.txt")) # change JR, 20140414

    ##
    ## Indicators for model run types
    ##

    global.run <- with(mcmc.meta, {
        !isTRUE(general$do.country.specific.run) &&
            !isTRUE(general$do.SS.run.first.pass) &&
             !isTRUE(validation.list$do.validation)
    })

    if(!is.null(mcmc.meta$validation.list)) {
        global.validation.run <-
            with(mcmc.meta$validation.list, {
                isTRUE(do.validation)
            })
    } else {
        global.validation.run <- FALSE
    }

    one.country.run <- with(mcmc.meta$general, {
        isTRUE(do.country.specific.run) &&
            !isTRUE(do.SS.run.first.pass)
        ## something about
        ## if (!is.null(mcmc.meta$winbugs.data$n.training.modern)) {
        ##        if (mcmc.meta$winbugs.data$n.training.modern > 0) {
        ##  ??
    })

    one.country.validation.run <- NULL #is this possible?!

    incl.no.data.countries <- with(mcmc.meta$general, {
        isTRUE(include.c.no.data)
    })

    at.random <- isTRUE(mcmc.meta$validation.list$at.random)
    at.end <- isTRUE(mcmc.meta$validation.list$at.end)
    exclude.unmet.only <- isTRUE(mcmc.meta$validation.list$exclude.unmet.only)
    at.random.no.data <- isTRUE(mcmc.meta$validation.list$at.random.no.data)
    leave.iso.out <- isTRUE(mcmc.meta$validation.list$leave.iso.out)

###------ MODEL ----------
    cat("
###--------------------------------------------------------------
### WriteModel_MWRA_Geog_Rate_1519_InclNoData
### Model for contraceptive use
### Leontine Alkema, 2011
### Mods by others since
###--------------------------------------------------------------

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    cat("
    model{

        ## ################################################################### ##
        ##                                                                     ##
        ##                TOP-LEVEL HYPERPRIOR DISTRIBUTIONS                   ##
        ##                                                                     ##
        ## ################################################################### ##

        ## TOP-LEVEL HYPERPRIORS are definitions that depend entirely on
        ## hard-coded values, incl. parameters passed in from R. Typically
        ## they are variable definitions that are probability distributions
        ## with hard-coded parameter values, rather than parameters that are other
        ## variables. All world-level variables are defined here. For one-country
        ## runs, the top-most level defined in terms of the summary of a global
        ## is defined here.

        ##=====================================================================##
        ##                            AR DISTORTIONS                           ##
        ##=====================================================================##

        ## AR(1) distortion terms for P_{c,t}, R_{c,t}, and Z_{c,t}.

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        rho.tot ~ dunif(0,rho.max)
        sigma.tot ~ dunif(0.01, sigma.ar.max)
        rho.rat ~  dunif(0,rho.max)
        sigma.rat ~ dunif(0.01, sigma.ar.max)
        rho.unmet ~ dunif(0,rho.max.unmet)
        sigma.ar.unmet ~ dunif(0.01, sigma.ar.max.unmet)

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                            One Country Run                          ##
        ##---------------------------------------------------------------------##

        rho.tot <- rho.tot0
        sigma.tot <- sigma.tot0
        rho.rat <- rho.rat0
        sigma.rat <- sigma.rat0
        rho.unmet <- rho.unmet0
        sigma.ar.unmet <- sigma.ar.unmet0

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        }
    cat("
        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        tau.tot.st <- tau.tot*(1-pow(rho.tot,2))
        tau.tot <- pow(sigma.tot, -2)

        tau.rat.st <- tau.rat*(1-pow(rho.rat,2))
        tau.rat <- pow(sigma.rat, -2)

        tau.unmet.st <- tau.unmet*(1-pow(rho.unmet,2))
        tau.unmet <- pow(sigma.ar.unmet, -2)

        ##=====================================================================##
        ##              Timing parameters, rate parameters, asymptotes         ##
        ##=====================================================================##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                               Global Run                            ##
        ##---------------------------------------------------------------------##

        Rw.world ~ dnorm(-1, 0.01)
        w.world ~ dnorm(-1, 0.01)
        tau.wreg <- pow(sigma.wreg, -2)
        tau.wsubreg <- pow(sigma.wsubreg, -2)
        tau.Rwreg <- pow(sigma.Rwreg, -2)
        tau.Rwsubreg <- pow(sigma.Rwsubreg, -2)
        sigma.wsubreg ~ dunif(0,sigmawregsubreg.upper)
        sigma.wreg ~ dunif(0,sigmawregsubreg.upper)
        sigma.Rwreg ~ dunif(0,sigmawregsubreg.upper)
        sigma.Rwsubreg ~ dunif(0,sigmawregsubreg.upper)

        RT.world ~ dnorm(mean.RTworld, tau0.RT)
        ## RATE MODEL >>>>> (these not in rate model)
        ## T.world ~ dnorm(mean.Tworld, tau0.T)
        ## TOneLevel ~ dnorm(mean.TOneLevel, tau0.T)
        ## <<<<< RATE MODEL

        tau.RTreg <- pow(sigma.RTreg, -2)
        tau.RTsubreg <- pow(sigma.RTsubreg, -2)
        ## RATE MODEL >>>>> (these not in rate model)
        ## tau.Treg <- pow(sigma.Treg, -2)
        ## tau.Tsubreg <- pow(sigma.Tsubreg, -2)
        ## <<<<< RATE MODEL

        sigma.RTreg ~ dunif(0,sigmaRTregsubreg.upper)
        sigma.RTsubreg ~ dunif(0,sigmaRTregsubreg.upper)
        ## RATE MODEL >>>>> (these not in rate model)
        ## sigma.Treg ~ dunif(0,sigmaTregsubreg.upper)
        ## sigma.Tsubreg ~ dunif(0,sigmaTregsubreg.upper)
        ## <<<<< RATE MODEL

        ## >>>>> RATE MODEL
        S.world ~ dnorm(-1, 0.01)
        tau.Sreg <- pow(sigma.Sreg, -2)
        tau.Ssubreg <- pow(sigma.Ssubreg, -2)
        sigma.Sreg ~ dunif(0,sigmaSregsubreg.upper)
        sigma.Ssubreg ~ dunif(0,sigmaSregsubreg.upper)
        ## <<<<< RATE MODEL

        ## Asymptotes
        lp.world ~ dnorm(0,0.01)
        lr.world ~ dnorm(0,0.01)

        Shigher ~ dnorm(-1,0.01)

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             One Country Run                         ##
        ##---------------------------------------------------------------------##

        lp.world <- lp.world0
        lr.world <- lr.world0

        Shigher <- Shigher0

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        }
    cat("
        ##=====================================================================##
        ##                      Country Variances (Kappas)                     ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    if (mcmc.meta$general$change.priors.to.zerolower) {
        cat("
        ##
        ## ---------- PRIORS TO ZEROLOWER ----------
        ##

        sigma.lpc ~ dunif(0,5)
        sigma.lrc ~ dunif(0,5)
        sigma.wc ~ dunif(0,2)
        sigma.Rwc ~ dunif(0,2)
        ## sigma.Tc ~ dunif(0,200)                 #>>RATE MODEL<< Not in rate model
        sigma.RTc ~ dunif(0,30)
        ## sigma.earlierTc ~ dunif(0,600)          #>>RATE MODEL<< Not in rate model
        sigma.unmetc ~ dunif(0,5)
        tau.lrc <- pow(sigma.lrc,-2)
        tau.lpc <- pow(sigma.lpc,-2)
        tau.wc <- pow(sigma.wc, -2)
        tau.Rwc <- pow(sigma.Rwc, -2)
        ## tau.Tc <- pow(sigma.Tc, -2)          #>>RATE MODEL<< Not in rate model
        tau.RTc <- pow(sigma.RTc, -2)
        ## tau.earlierTc <- pow(sigma.earlierTc, -2) #>>RATE MODEL<< Not in rate model
        tau.unmetc <- pow(sigma.unmetc,-2)

        ## RATE MODEL >>>>>
        sigma.Sc ~ dunif(0,5) #Change NC, 20160811
        sigma.higherSc ~ dunif(0,5) #Change NC, 20170221
        tau.Sc <- pow(sigma.Sc, -2) #Change NC, 20160811
        tau.higherSc <- pow(sigma.higherSc, -2) #Change NC, 20170221
        ## <<<<< RATE MODEL

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    } else {
        cat("
        ##
        ## ---------- PRIORS _NOT_ TO ZEROLOWER ----------
        ##

        ## RATE MODEL >>>>>
        tau.unmetc ~ dgamma(0.5,halfsigma2.unmetc0)
        tau.lpc <- pow(sigma.lpc,-2) #Change NC, 20160811
        tau.lrc  ~ dgamma(halfnu0,halfnu0sigma2.lrc0)
        tau.wc <- pow(sigma.wc, -2) #Change NC, 20160811
        tau.Sc <- pow(sigma.Sc, -2) #Change NC, 20160811
        tau.higherSc <- pow(sigma.higherSc, -2) #Change NC, 20170221
        tau.Rwc  ~ dgamma(halfnu0,halfnu0sigma2.Rwc0)
        tau.RTc  ~ dgamma(halfnu0,halfnu0sigma2.RTc0)

        sigma.unmetc <- 1/sqrt(tau.unmetc)
        sigma.lpc ~ dunif(0,5) #Change NC, 20160811
        sigma.lrc <- 1/sqrt(tau.lrc)
        sigma.wc ~ dunif(0,2) #Change NC, 20160811
        sigma.Sc ~ dunif(0,5)  #Change NC, 20160811
        sigma.higherSc ~ dunif(0,5) #Change NC, 20170221
        sigma.Rwc <- 1/sqrt(tau.Rwc)
        sigma.RTc <- 1/sqrt(tau.RTc)
        ## <<<<< RATE MODEL

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    }
    cat("
        ##=====================================================================##
        ##                      Unmet/No Need Parameters                       ##
        ##=====================================================================##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        a.unmet ~ dnorm(a0.unmet, tau.a0)
        b.unmet ~ dnorm(b0.unmet, tau.b0)
        c.unmet ~ dunif(-35,0)                #[MCW-2019-05-13] Set to same as MWRA GEOG 15-19

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                            One Country Run                          ##
        ##---------------------------------------------------------------------##

        a.unmet <- a.unmet0
        b.unmet <- b.unmet0
        c.unmet <- c.unmet0

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        }
        cat("
        ##=====================================================================##
        ##             Perturbations and Misclassification Biases              ##
        ##=====================================================================##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        ## Biases: Folk, MICS, sterilization included/excluded (M+/M-)
        v.abs.probe.q ~ dunif(0,1)
        v.mneg ~ dunif(0,1)
        v.folk ~ dunif(0,1)
        v.mpos ~ dunif(0,1)

        sigma.pos ~ dunif(0.01,2)
        mu.pos.m[1] ~ dnorm(-2, 0.64)
        mu.pos.m[2] ~ dnorm(-2, 0.64)

        for(m in 1:2) {
            sigma.geo.m[m] ~ dunif(0.01,2)
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                            One Country Run                          ##
        ##---------------------------------------------------------------------##

        #Biases: Folk, MICS, sterilization included/excluded (M+/M-)
        v.abs.probe.q <- v.abs.probe.q0
        v.mneg <- v.mneg0
        v.folk <- v.folk0
        v.mpos <- v.mpos0

        for (m in 1:2){
          sigma.geo.m[m] <- sigma.geo.m0[m]
        } # end m-loop

        sigma.pos <- sigma.pos0
        mu.pos.m[2] <- mu.pos.m0[2]
        mu.pos.m[1] <- mu.pos.m0[1]

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        }
    cat("
        ##=====================================================================##
        ##                            Source Variances                         ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

            sigma.unmet.other ~ dunif(0.01,2)
            sigma.unmet.dhs ~ dunif(0.01,2)

            sigma.unmetworld ~ dunif(0,5)
            tau.unmetworld <- pow(sigma.unmetworld,-2)

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        tau.sourcetot ~ dgamma(0.5, halfsigma2.sourcetot0)
        sigma.sourcetot <- 1/sqrt(tau.sourcetot)

        nonsample.se.unmet.s[1]<-sigma.unmet.dhs #Change NC, 20170112
        nonsample.se.unmet.s[2]<-sigma.unmet.other

        for(s in 1:6){ ## MCW 2017-12-22 :: There are now six data
            ## sources, with PMA disaggregated and CP
            ## TOT < 1 percent.
            nonsample.se.trad.s[s]~dunif(0.01,2) #Change NC, 20170112
            nonsample.se.modern.s[s]~dunif(0.01,2) #Change NC, 20170112
            cor.trad.modern.s[s]~dunif(-1,1) #Change NC, 20170112
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                           One Country Run                           ##
        ##---------------------------------------------------------------------##

        tau.sourcetot <- tau.sourcetot0
        sigma.sourcetot <- 1/sqrt(tau.sourcetot)

        nonsample.se.unmet.s[1]<-sigma.unmet.dhs0 #Change NC, 20170112
        nonsample.se.unmet.s[2]<-sigma.unmet.other0

        for (s in 1:6){## MCW 2017-12-22 :: There are now six data
            ## sources, with PMA disaggregated and CP
            ## TOT < 1 percent.
            nonsample.se.trad.s[s]<-nonsample.se.trad.s0[s]
            nonsample.se.modern.s[s]<-nonsample.se.modern.s0[s]
            cor.trad.modern.s[s]<-cor.trad.modern.s0[s]
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    }
        cat("
        ## ################################################################### ##
        ##                                                                     ##
        ##                           PROCESS MODEL                             ##
        ##                                                                     ##
        ## ################################################################### ##

        ## PROCESS MODEL contains all definitions that do not depend on
        ## observations or their properties, and are not top-level hyperpriors.

        ##=====================================================================##
        ##                            AR DISTORTIONS                           ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        ## Distortion terms for first obsn in each country.
        for (c in 1:C){
            theta.ci[c,1] ~ dnorm(0, tau.unmet.st)
            eps.ci[c, 1] ~ dnorm(0, tau.tot.st)
            eta.ci[c,1] ~ dnorm(0, tau.rat.st)
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(incl.no.data.countries || at.random.no.data || leave.iso.out) {
        cat("
        ## Distortion terms for first obsn in each country.
        for (c in (C + 1):(C + C.no.data)){
            theta.ci[c,1] ~ dnorm(0, tau.unmet.st)
            eps.ci[c, 1] ~ dnorm(0, tau.tot.st)
            eta.ci[c,1] ~ dnorm(0, tau.rat.st)
        }

      ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
      }
     cat("
        ##=====================================================================##
        ##                       Rates, paces, asymptotes                      ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                              All Models                             ##
        ##---------------------------------------------------------------------##

        ##
        ## ---------- DUMMIES TO MAKE IT WORK PROPERLY
        ##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        if(!incl.no.data.countries && !at.random.no.data && !leave.iso.out) {
        cat("
        ## Not estimating countries with no data and not doing
        ## at.random validation

        ## #change JR, 20131105
        pmax.c[C+1] <- 0
        omega.c[C+1] <- 0
        RT.c[C+1] <- 0
        Rmax.c[C+1] <- 0
        Romega.c[C+1] <- 0
        ## T.c[C+1] <- 0 # >> RATE MODEL << Not in rate model
        unmet.intercept.c[C+1] <- 0
        ## >>>>> RATE MODEL
        setlevel.c[C+1] <- 0 #Change NC, 20160811
        ## RATE MODEL <<<<<

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        } else {
        cat("
        ##  #change JR, 20131105
        pmax.c[C + C.no.data + 1] <- 0
        omega.c[C + C.no.data + 1] <- 0
        RT.c[C + C.no.data + 1] <- 0
        Rmax.c[C + C.no.data + 1] <- 0
        Romega.c[C + C.no.data + 1] <- 0
        ## T.c[C + C.no.data + 1] <- 0 # >> RATE MODEL << Not in rate model
        unmet.intercept.c[C + C.no.data + 1] <- 0
        ## >>>>> RATE MODEL
        setlevel.c[C + C.no.data + 1] <- 0 #Change NC, 20160811
        ## RATE MODEL <<<<<

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
      }
     cat("
        ##
        ## ---------- COUNTRY LEVEL ----------
        ##

        for(c in 1:C) {

          ## Asymptotes: 1-level
          pmax.c[c] <- (exp(logitpmax.c[c])+0.1)/(1+exp(logitpmax.c[c]))
                                # >>>>> GEOG MODEL <<<<<
                                # [MCW-2016-06-16-1] Changed lower
                                # limit to 0.1 (was 0.5)
                                # [MCW-2019-05-13] KEEP at 0.1 for 15--19
          logitpmax.c[c] ~ dnorm(lp.world, tau.lpc)
          Rmax.c[c] <- (exp(logitRmax.c[c])+0.5)/(1+exp(logitRmax.c[c]))
          logitRmax.c[c] ~ dnorm(lr.world, tau.lrc)

          ## Omegas: 3-level
          omega.c[c] <-
              (0.5*exp(logitomega.c[c])+0.01)/(1+exp(logitomega.c[c]))
          logitomega.c[c] ~
              dnorm(w.subreg[subreg.c[c]], tau.wc)
                                # >>>>> GEOG MODEL <<<<<
          Romega.c[c] <-
              (0.5*exp(logitRomega.c[c])+0.01)/(1+exp(logitRomega.c[c]))
          logitRomega.c[c] ~
              dnorm(Rw.subreg[subreg.c[c]], tau.Rwc)
                                # >>>>> GEOG MODEL <<<<<

          ## Midyear ratio modern/total: 3-level
          RT.c[c]  ~
              dnorm(RT.subreg[subreg.c[c]], tau.RTc)#T(1800,)
                              # >>>>> GEOG MODEL >>>>> RATE MODEL
                              # Truncation removed as per NC's rate model.

          ## unmet need intercept
          unmet.intercept.c[c] ~
              dnorm(unmet.subreg[subreg.c[c]], tau.unmetc)
                              # >>>>> GEOG MODEL <<<<<

          ## Set levels (i.e., total prevalence at the set year, 1990.5)
               ## RATE MODEL >>>>>
               ## setlevel: 3-level #Change NC, 20160811
               ## year.set.index is the index of 1990.5 in the
               ## observation period for country c
               s.ci[c,year.set.index[c]]<-setlevel.c[c]
               p.ci[c,year.set.index[c]]<-1/(1+exp(-s.ci[c,year.set.index[c]]))
               ## <<<<< RATE MODEL

        } ##end country loop

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(incl.no.data.countries || at.random.no.data || leave.iso.out) {
        cat("
        for(c in (C+1):(C+C.no.data)) {

          ## Asymptotes: 1-level
          pmax.c[c] <- (exp(logitpmax.c[c])+0.1)/(1+exp(logitpmax.c[c]))
                                # >>>>> SA MODEL <<<<<
                                # [MCW-2016-06-16-1] Changed lower
                                # limit to 0.1 (was 0.5)
          logitpmax.c[c] ~ dnorm(lp.world, tau.lpc)
          Rmax.c[c] <- (exp(logitRmax.c[c])+0.5)/(1+exp(logitRmax.c[c]))
          logitRmax.c[c] ~ dnorm(lr.world, tau.lrc)

          ## Omegas: 3-level
          omega.c[c] <-
              (0.5*exp(logitomega.c[c])+0.01)/(1+exp(logitomega.c[c]))
          logitomega.c[c] ~
              dnorm(w.subreg[subreg.c[c]], tau.wc)
                                # >>>>> GEOG MODEL <<<<<
          Romega.c[c] <-
              (0.5*exp(logitRomega.c[c])+0.01)/(1+exp(logitRomega.c[c]))
          logitRomega.c[c] ~
              dnorm(Rw.subreg[subreg.c[c]], tau.Rwc)
                                # >>>>> GEOG MODEL <<<<<

          ## Midyear ratio modern/total: 3-level
          RT.c[c]  ~
              dnorm(RT.subreg[subreg.c[c]], tau.RTc)#T(1800,)
                              # >>>>> GEOG MODEL >>>>> RATE MODEL
                              # Truncation removed as per NC's rate model.

          ## unmet need intercept
          unmet.intercept.c[c] ~
              dnorm(unmet.subreg[subreg.c[c]], tau.unmetc)
                              # >>>>> GEOG MODEL <<<<<

          ## Set levels (i.e., total prevalence at the set year, 1990.5)
               ## RATE MODEL >>>>>
               ## setlevel: 3-level #Change NC, 20160811
               ## year.set.index.test is the index of 1990.5 in the
               ## observation period for country c
               s.ci[c,year.set.index.test[c-C]]<-setlevel.c[c]
               p.ci[c,year.set.index.test[c-C]]<-1/(1+exp(-s.ci[c,year.set.index.test[c-C]]))
               ## <<<<< RATE MODEL

        } ##end country loop

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        }
       cat("
        ##
        ## ---------- SUBREGION LEVEL ----------
        ##

        ## Depends whether this is a global or one country run. See below.

        ##
        ## ---------- REGION LEVEL ----------
        ##

        ## Only for global run.

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        ##
        ## ---------- SUBREGION LEVEL ----------
        ##

        ## In Niamh's rate model code, many of these are under the
        ## heading 'LGC Hierarchical parameters'

        ## (sub)regions already account for any that contain only
        ## countries with no data.

          for (subreg in 1:n.subreg){
              ## >>>> GEOG MODEL <<<<
            unmet.subreg[subreg] ~ dnorm(0, tau.unmetworld)
            w.subreg[subreg] ~ dnorm(w.reg[reg.subreg[subreg]] , tau.wsubreg)#T(-4.5, 0)
            S.subreg[subreg] ~ dnorm(S.reg[reg.subreg[subreg]] , tau.Ssubreg) #Change NC, 20160811

            Rw.subreg[subreg] ~ dnorm(Rw.reg[reg.subreg[subreg]] , tau.Rwsubreg)#T(-4.5, 0)
            RT.subreg[subreg] ~ dnorm(RT.reg[reg.subreg[subreg]] , tau.RTsubreg)#T(1800,)
          }

        ##
        ## ---------- REGION LEVEL ----------
        ##

         for (reg in 1:n.reg){
              ## >>>> GEOG MODEL <<<<
           w.reg[reg] ~ dnorm(w.world, tau.wreg)#T(-4.5, 0)
           S.reg[reg] ~ dnorm(S.world, tau.Sreg) #Change NC, 20160811

           Rw.reg[reg] ~ dnorm(Rw.world, tau.Rwreg)#T(-4.5, 0)
           RT.reg[reg] ~ dnorm(RT.world, tau.RTreg)#T(1800,)
         }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                          One Country Run                            ##
        ##---------------------------------------------------------------------##

        ##
        ## ---------- SUBREGION LEVEL ----------
        ##

        ## In Niamh's rate model code, many of these are under the
        ## heading 'LGC Hierarchical parameters'

        ## (sub)regions already account for any that contain only
        ## countries with no data.


        for (x in 1:n.subreg){
            unmet.subreg[x] <- unmet.subreg0[x]
            w.subreg[x] <- w.subreg0[x]
            Rw.subreg[x] <- Rw.subreg0[x]
            RT.subreg[x] <- RT.subreg0[x]
            S.subreg[x] <- S.subreg0[x] #Change NC 20160602
        }

        ##
        ## ---------- REGION LEVEL ----------
        ##

        ## No region level parameters for one country run.

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    }
    cat("
        ##=====================================================================##
        ##          Country-level 'set level' (S_{c,1990.5}) Parameters        ##
        ##=====================================================================##

        ## The 'set level' parameters take the place of big Omega in the level
        ## model; they are modelled hierarchically.

        ## These are done separately because they are modelled differently. Unlike
        ## the RT.c, Rw.c, etc., the hierarchy has no world level for countries
        ## in SA group 0.

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)

        cat("
        ## At least one country in each sexual activity group

        ## >>>>> RATE MODEL >>>>> GEOG MODEL

        for (i in 1:n.rich){
            setlevel.c[crich.index[i]] ~ dnorm(Shigher, tau.higherSc)#T(?,)
        }
        ## setlevel total: other countries, 3-level
        for (i in 1:n.notrich){
            setlevel.c[cnotrich.index[i]] ~ dnorm(S.subreg[subreg.c[cnotrich.index[i]]], tau.Sc)#T(?,)
        }

        ## <<<<< GEOG MODEL <<<<< RATE MODEL

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)

    if(incl.no.data.countries || at.random.no.data || leave.iso.out) {

        cat("
        ## At least one country in each sexual activity group

        ## >>>>> RATE MODEL >>>>> SA MODEL

       for (i in 1:n.rich.no.data){
            setlevel.c[crich.index.no.data[i]] ~ dnorm(Shigher, tau.higherSc)#T(?,)
        }
        ## setlevel total: other countries, 3-level
        for (i in 1:n.notrich.no.data){
            setlevel.c[cnotrich.index.no.data[i]] ~ dnorm(S.subreg[subreg.c[cnotrich.index.no.data[i]]], tau.Sc)#T(?,)
        }

        ## <<<<< GEOG MODEL <<<<< RATE MODEL

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    }

    cat("
        #########################################################################
        ##                                                                     ##
        ##                             DATA  MODEL                             ##
        ##                                                                     ##
        #########################################################################

        ## DATA MODEL part contains all definitions that depend on observations
        ## e.g., definitions that loop over observations themselves, or over
        ## indices that are derived from observations or their properties, such
        ## as each unique observation, indices of observations within the time-
        ## points.

        ## gett.j gives (year - year.start+1) for obs j
        ## getc.j gives gives c for obs j
        ## ind.j refers to counter (1 if not applicable), ind1 refers to yes/no (1/0).

        ##=====================================================================##
        ##                            AR DISTORTIONS                           ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        cat("
        ##
        ## ---------- INCLUDED COUNTRIES ----------
        ##

        ## AR-loop for countries with more than 1 obs
        ## (not relevant for countries with no data)

        for (z in 1:n.countriesmorethan1obs){
            for (i in 2:N.unique.c[getc.z[z]]){
                thetahat.ci[getc.z[z],i] <- theta.ci[getc.z[z],i-1]*
                    pow(rho.unmet, gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])
                etahat.ci[getc.z[z],i] <- eta.ci[getc.z[z],i-1]*
                    pow(rho.rat, gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])
                tautheta.ci[getc.z[z],i] <- tau.unmet.st/
                    (1-pow(rho.unmet,
                           2*(gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])))
                ## taueps.ci[getc.z[z],i] <- tau.tot.st/
                ##     (1-pow(rho.tot,
                ##            2*(gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])))
                ##  <<< NOT IN RATE MODEL
                taueta.ci[getc.z[z],i] <- tau.rat.st/
                    (1-pow(rho.rat,
                           2*(gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])))
                theta.ci[getc.z[z],i] ~
                    dnorm(thetahat.ci[getc.z[z],i], tautheta.ci[getc.z[z],i])
                eta.ci[getc.z[z],i] ~
                    dnorm(etahat.ci[getc.z[z],i], taueta.ci[getc.z[z],i])
            }
        }

        ## RATE MODEL >>>>>
        ## Separate AR(1) for Tot into a different loop #Change NC 20160811
        for(c in 1:C){
            for(i in 2:N.obsperiod.c[c]){
                epshat.ci[c,i] <- eps.ci[c,i-1]*rho.tot
                eps.ci[c,i] ~ dnorm(epshat.ci[c,i], tau.tot)
            }
        }
        ## <<<<< RATE MODEL

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                            Global Model                             ##
        ##---------------------------------------------------------------------##
        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        }
    if(at.random.no.data) {
        cat("
        ##
        ## ---------- TEST-SET OBSERVATIONS ----------
        ##

        ## AR-loop for countries with more than 1 obs
        ## (not relevant for countries with no data)

        for (z in 1:n.countriesmorethan1obs.test){
            for (i in 2:N.unique.c.test[getc.z.test[z]-C]){
                thetahat.ci[getc.z.test[z],i] <- theta.ci[getc.z.test[z],i-1]*
                    pow(rho.unmet, gett.ci.test[getc.z.test[z]-C,i] - gett.ci.test[getc.z.test[z]-C,i-1])
                etahat.ci[getc.z.test[z],i] <- eta.ci[getc.z.test[z],i-1]*
                    pow(rho.rat, gett.ci.test[getc.z.test[z]-C,i] - gett.ci.test[getc.z.test[z]-C,i-1])
                tautheta.ci[getc.z.test[z],i] <- tau.unmet.st/
                    (1-pow(rho.unmet,
                           2*(gett.ci.test[getc.z.test[z]-C,i] - gett.ci.test[getc.z.test[z]-C,i-1])))
                ## taueps.ci[getc.z.test[z],i] <- tau.tot.st/
                ##     (1-pow(rho.tot,
                ##            2*(gett.ci.test[getc.z.test[z]-C,i] - gett.ci.test[getc.z.test[z]-C,i-1])))
                ## <<<<< NOT IN RATE MODEL
                taueta.ci[getc.z.test[z],i] <- tau.rat.st/
                    (1-pow(rho.rat,
                           2*(gett.ci.test[getc.z.test[z]-C,i] - gett.ci.test[getc.z.test[z]-C,i-1])))
                theta.ci[getc.z.test[z],i] ~
                    dnorm(thetahat.ci[getc.z.test[z],i], tautheta.ci[getc.z.test[z],i])
                eta.ci[getc.z.test[z],i] ~
                    dnorm(etahat.ci[getc.z.test[z],i], taueta.ci[getc.z.test[z],i])
            }
        }

        ## RATE MODEL >>>>>
        ## Separate AR(1) for Tot into a different loop #Change NC 20160811
        for(c in (C+1):(C+C.no.data)){
            for(i in 2:N.obsperiod.c.test[c-C]){
                epshat.ci[c,i] <- eps.ci[c,i-1]*rho.tot
                eps.ci[c,i] ~ dnorm(epshat.ci[c,i], tau.tot)
            }
        }
        ## <<<<< RATE MODEL

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        }
    if (mcmc.meta$include.AR) {
        cat("
        ##=====================================================================##
        ##                              EXTRA AR PART                          ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        ##
        ## ---------- DO 'include.AR' ----------
        ##

        for (c in 1:C){

            ## Move Tot into its own loop #Change NC, 20160602
            ## New Set up for Tot CP based on the Rate(CP) model #Change NC, 20160807

            for(i in 1:(year.set.index[c]-1)) {

                ls.ci[c,(year.set.index[c]-i)] <-
                    s.ci[c,((year.set.index[c]-i)+1)] - eps.ci[c,(year.set.index[c]-i)] #logit
                ils.ci[c,(year.set.index[c]-i)] <-
                    1/(1+exp(-ls.ci[c,(year.set.index[c]-i)])) #inv.logit

                ## Step function; test for x >/= 0
                I[c,(year.set.index[c]-i)] <-
                    step(ils.ci[c,(year.set.index[c]-i)]-pmax.c[c])

                ## Get p.ct directly in the backward direction
                ## Only need this bit if I=0 i.e., ils.ci<pmax.c
                zeta.ci[c,(year.set.index[c]-i)] <-
                    (1-I[c,(year.set.index[c]-i)]) * (
                        logit(min((1-0.00001),ils.ci[c,(year.set.index[c]-i)] /
                                              pmax.c[c]))-omega.c[c]
                    )

                p.ci[c,(year.set.index[c]-i)] <-
                    (1-I[c,(year.set.index[c]-i)]) * (
                        pmax.c[c] * (1/(1+exp(-zeta.ci[c,(year.set.index[c]-i)])))) +
                    I[c,(year.set.index[c]-i)]*ils.ci[c,(year.set.index[c]-i)]

                ## Get logit(p.ci)
                s.ci[c,(year.set.index[c]-i)] <- logit(p.ci[c,(year.set.index[c]-i)])

            }

            for(i in (year.set.index[c]+1):N.obsperiod.c[c]){

                ## Step function; test for x >/= 0
                I[c,i] <- step(p.ci[c,i-1]-pmax.c[c])

                ## Only need this bit if I=0 i.e., p.ci<pmax.c
                zeta.ci[c,i] <-
                    (1-I[c,i]) *
                    (logit(min((1-0.000001),p.ci[c,i-1]/pmax.c[c]))+omega.c[c])

                s.ci[c,i] <-
                    logit(I[c,i]*(p.ci[c,i-1]) +
                          (1-I[c,i])*pmax.c[c]*(1/(1+exp(-zeta.ci[c,i]))))+eps.ci[c,i-1]

                ## inv.logit to get p.ci
                p.ci[c,i] <- 1/(1+exp(-s.ci[c,i]))

            } #End TotCP loop

      for (i in 1:N.unique.c[c]){
      R.ci[c, i] <- 1 / (1 + exp(-( logit(Rstar.ci[c,i]) + eta.ci[c,i])))
      }

      for (i in 1:N.unique.c[c]) {
      logitZ.ci[c,i] <- logitZstar.ci[c,i] + theta.ci[c, i]
      neg.explogitZ.ci[c,i] <- exp(-logitZ.ci[c,i])
      }

        } #End country loop

      ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    } else {
        cat("
        ##
        ## ---------- DO NOT 'include.AR' ----------
        ##

        for (c in 1:C){

            ## Move Tot into its own loop #Change NC, 20160602
            ## New Set up for Tot CP based on the Rate(CP) model #Change NC, 20160807
            for(i in 1:(year.set.index[c]-1)){

                ls.ci[c,(year.set.index[c]-i)] <-
                    s.ci[c,((year.set.index[c]-i)+1)] #logit
                ils.ci[c,(year.set.index[c]-i)] <-
                    1/(1+exp(-ls.ci[c,(year.set.index[c]-i)])) #inv.logit

                ## Step function; test for x >/= 0
                I[c,(year.set.index[c]-i)] <-
                    step(ils.ci[c,(year.set.index[c]-i)]-pmax.c[c])

                ## Get p.ct directly in the backward direction
                zeta.ci[c,(year.set.index[c]-i)] <-
                    (1-I[c,(year.set.index[c]-i)]) *
                    (logit(min((1-0.00001),ils.ci[c,(year.set.index[c]-i)]/pmax.c[c]))-omega.c[c])

                p.ci[c,(year.set.index[c]-i)] <-
                    (1-I[c,(year.set.index[c]-i)]) *
                    (pmax.c[c]*(1/(1+exp(-zeta.ci[c,(year.set.index[c]-i)])))) +
                    I[c,(year.set.index[c]-i)]*ils.ci[c,(year.set.index[c]-i)]

                ## Get logit(P.ci)
                s.ci[c,(year.set.index[c]-i)] <- logit(p.ci[c,(year.set.index[c]-i)])

            }

            for(i in (year.set.index[c]+1):N.obsperiod.c[c]){

                ## Step function; test for x >/= 0
                I[c,i] <- step(p.ci[c,i-1]-pmax.c[c])

                zeta.ci[c,i] <-
                    (1-I[c,i]) * (logit(min((1-0.000001),p.ci[c,i-1] / pmax.c[c]))+omega.c[c])

                s.ci[c,i] <-
                    logit(I[c,i] * (p.ci[c,i-1]) + (1-I[c,i]) * pmax.c[c] * (1 / (1+exp(-zeta.ci[c,i]))))

                ## CP on the logit scale
                p.ci[c,i] <- 1 / (1+exp(-s.ci[c,i]))

            } #End TotCP loop

            for (i in 1:N.unique.c[c]){
                R.ci[c, i] <- Rstar.ci[c,i]
            }
            for (i in 1:N.unique.c[c]){
                logitZ.ci[c,i] <- logitZstar.ci[c,i]
                neg.explogitZ.ci[c,i] = exp(-logitZ.ci[c,i])

            }

        } #End country loop

      ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    } ##end extra AR part
    if(global.run){
        cat("
        ##---------------------------------------------------------------------##
        ##                            Global Model                             ##
        ##---------------------------------------------------------------------##

    ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    }
    if(at.random.no.data || leave.iso.out) { #### !!!!!!!! SHOULD THIS ALSO BE DONE FOR COUNTRIES WITH NO DATA?? (I.E,. NOT VALIDATION RUNS)
        if (mcmc.meta$include.AR) {
        cat("
        ##
        ## ---------- EXTRA for VALIDATIONS and NO DATA COUNTRIES (DO 'include.AR') ----------
        ##

        for (c in (C+1):(C+C.no.data)){

            ## Move Tot into its own loop #Change NC, 20160602
            ## New Set up for Tot CP based on the Rate(CP) model #Change NC, 20160807

            for(i in 1:(year.set.index.test[c - C]-1)) {

                ls.ci[c,(year.set.index.test[c-C]-i)] <-
                    s.ci[c,((year.set.index.test[c-C]-i)+1)] - eps.ci[c,(year.set.index.test[c-C]-i)] #logit
                ils.ci[c,(year.set.index.test[c-C]-i)] <-
                    1/(1+exp(-ls.ci[c,(year.set.index.test[c-C]-i)])) #inv.logit

                ## Step function; test for x >/= 0
                I[c,(year.set.index.test[c-C]-i)] <-
                    step(ils.ci[c,(year.set.index.test[c-C]-i)]-pmax.c[c])

                ## Get p.ct directly in the backward direction
                ## Only need this bit if I=0 i.e., ils.ci<pmax.c
                zeta.ci[c,(year.set.index.test[c-C]-i)] <-
                    (1-I[c,(year.set.index.test[c-C]-i)]) * (
                        logit(min((1-0.00001),ils.ci[c,(year.set.index.test[c-C]-i)] /
                                              pmax.c[c]))-omega.c[c]
                    )

                p.ci[c,(year.set.index.test[c-C]-i)] <-
                    (1-I[c,(year.set.index.test[c-C]-i)]) * (
                        pmax.c[c] * (1/(1+exp(-zeta.ci[c,(year.set.index.test[c-C]-i)])))) +
                    I[c,(year.set.index.test[c-C]-i)]*ils.ci[c,(year.set.index.test[c-C]-i)]

                ## Get logit(p.ci)
                s.ci[c,(year.set.index.test[c-C]-i)] <- logit(p.ci[c,(year.set.index.test[c-C]-i)])

            }

            for(i in (year.set.index.test[c-C]+1):N.obsperiod.c.test[c-C]){

                ## Step function; test for x >/= 0
                I[c,i] <- step(p.ci[c,i-1]-pmax.c[c])

                ## Only need this bit if I=0 i.e., p.ci<pmax.c
                zeta.ci[c,i] <-
                    (1-I[c,i]) *
                    (logit(min((1-0.000001),p.ci[c,i-1]/pmax.c[c]))+omega.c[c])

                s.ci[c,i] <-
                    logit(I[c,i]*(p.ci[c,i-1]) +
                          (1-I[c,i])*pmax.c[c]*(1/(1+exp(-zeta.ci[c,i]))))+eps.ci[c,i-1]

                ## inv.logit to get p.ci
                p.ci[c,i] <- 1/(1+exp(-s.ci[c,i]))

            } #End TotCP loop

      for (i in 1:N.unique.c.test[c-C]){
      R.ci[c, i] <- 1 / (1 + exp(-( logit(Rstar.ci[c,i]) + eta.ci[c,i])))
      }

      for (i in 1:N.unique.c.test[c-C]) {
      logitZ.ci[c,i] <- logitZstar.ci[c,i] + theta.ci[c, i]
      neg.explogitZ.ci[c,i] <- exp(-logitZ.ci[c,i])
      }

        } #End country loop
        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
        } else {
        cat("
        ##
        ## ---------- EXTRA for VALIDATIONS and NO DATA COUNTRIES (DO NOT 'include.AR') ----------
        ##

        for (c in (C+1):(C+C.no.data)){

            ## Move Tot into its own loop #Change NC, 20160602
            ## New Set up for Tot CP based on the Rate(CP) model #Change NC, 20160807
            for(i in 1:(year.set.index.test[c-C]-1)){

                ls.ci[c,(year.set.index.test[c-C]-i)] <-
                    s.ci[c,((year.set.index.test[c-C]-i)+1)] #logit
                ils.ci[c,(year.set.index.test[c-C]-i)] <-
                    1/(1+exp(-ls.ci[c,(year.set.index.test[c-C]-i)])) #inv.logit

                ## Step function; test for x >/= 0
                I[c,(year.set.index.test[c-C]-i)] <-
                    step(ils.ci[c,(year.set.index.test[c-C]-i)]-pmax.c[c])

                ## Get p.ct directly in the backward direction
                zeta.ci[c,(year.set.index.test[c-C]-i)] <-
                    (1-I[c,(year.set.index.test[c-C]-i)]) *
                    (logit(min((1-0.00001),ils.ci[c,(year.set.index.test[c-C]-i)]/pmax.c[c]))-omega.c[c])

                p.ci[c,(year.set.index.test[c-C]-i)] <-
                    (1-I[c,(year.set.index.test[c-C]-i)]) *
                    (pmax.c[c]*(1/(1+exp(-zeta.ci[c,(year.set.index.test[c-C]-i)])))) +
                    I[c,(year.set.index.test[c-C]-i)]*ils.ci[c,(year.set.index.test[c-C]-i)]

                ## Get logit(P.ci)
                s.ci[c,(year.set.index.test[c-C]-i)] <- logit(p.ci[c,(year.set.index.test[c-C]-i)])

            }

            for(i in (year.set.index.test[c-C]+1):N.obsperiod.c.test[c-C]){

                ## Step function; test for x >/= 0
                I[c,i] <- step(p.ci[c,i-1]-pmax.c[c])

                zeta.ci[c,i] <-
                    (1-I[c,i]) * (logit(min((1-0.000001),p.ci[c,i-1] / pmax.c[c]))+omega.c[c])

                s.ci[c,i] <-
                    logit(I[c,i] * (p.ci[c,i-1]) + (1-I[c,i]) * pmax.c[c] * (1 / (1+exp(-zeta.ci[c,i]))))

                ## CP on the logit scale
                p.ci[c,i] <- 1 / (1+exp(-s.ci[c,i]))

            } #End TotCP loop

            for (i in 1:N.unique.c.test[c-C]){
                R.ci[c, i] <- Rstar.ci[c,i]
            }
            for (i in 1:N.unique.c.test[c-C]){
                logitZ.ci[c,i] <- logitZstar.ci[c,i]
                neg.explogitZ.ci[c,i] = exp(-logitZ.ci[c,i])

            }
        } #End country loop

        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
        }
    }
    cat("
        ##=====================================================================##
        ##          CP Vector Components (P_{c,t}, R_{c,t}, Z_{c,t})           ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        ##
        ## ---------- ALL NON-EXCLUDED OBSERVATIONS ----------
        ##

        for (j in 1:J){
            ## >>>>> RATE MODEL
            for (h in 1:getperiod.j[j]) {
                trad.jh[j, h] = p.ci[getc.j[j], getest.jf[j, h]] * (1 - R.ci[getc.j[j], getis.jf[j, h]])
                modern.jh[j, h] = p.ci[getc.j[j], getest.jf[j, h]] * R.ci[getc.j[j], getis.jf[j, h]]
                unmet.jh[j, h] = (1 - p.ci[getc.j[j], getest.jf[j, h]])*
                    (1 / (1 + neg.explogitZ.ci[getc.j[j], getis.jf[j, h]]))
            }
            trad.j[j] = 1 / period.j[j] *
                inprod(trad.jh[j, 1:getperiod.j[j]], partialtime.xj[1:getperiod.j[j], j])
            modern.j[j] = 1 / period.j[j] *
                inprod(modern.jh[j, 1:getperiod.j[j]], partialtime.xj[1:getperiod.j[j], j])
            unmet.j[j] = 1 / period.j[j] *
                inprod(unmet.jh[j, 1:getperiod.j[j]], partialtime.xj[1:getperiod.j[j], j])

            ## Not in rate model:
            ## --
            ## logitZstar.j[j] <- (
            ##     unmet.intercept.c[getc.j[j]]
            ##     + a.unmet
            ##     + b.unmet * (p.ci[getc.j[j], geti.j[j]] - pmid.for.unmet)
            ##     + c.unmet*pow(p.ci[getc.j[j], geti.j[j]] - pmid.for.unmet,2))
            ## --
            ## logitZ.j defined in AR-loop, just adding theta

            sump.j[j] <- (trad.j[j]*Vtrad.j[j]
                + modern.j[j]* Vmodern.j[j]
                + (1- p.ci[getc.j[j], getest.j[j]])) #Change NC, 20160811
            ## <<<<< RATE MODEL
        }

        ## Unmet observations, get mean, skip NAs
        for(k in 1:N.unmet) {
            logitratio.yunmet.hat.j[getj.unmet.k[k]] <-
                logit(max(0.0000001,q.ij[3,getj.unmet.k[k]])/none.adj.j[getj.unmet.k[k]])
        }

        for (c in 1:C){
            for (i in 1:N.unique.c[c]){
                ## mu.ci[c, i] <- omega.c[c]*(gett.ci[c,i] - T.c[c])
                                # >> RATE MODEL <<: 'mu.ci' not a
                                # parameter in rate model
                ## pstar.ci[c, i] <- pmax.c[c]/(1+exp(-mu.ci[c, i]))
                                # >> RATE MODEL <<: 'pstar.ci' not a
                                # parameter in rate model

                Rmu.ci[c, i] <- Romega.c[c]*(gett.ci[c,i] - RT.c[c])
                Rstar.ci[c, i] <- Rmax.c[c]/(1+exp(-Rmu.ci[c, i]))
                ## R.ci defined later, depends on whether AR is added or not
            } ## end unique-obs year loop
           }

        for (c in 1:C){
            for (i in 1:N.unique.c[c]){

                ## >>>>> RATE MODEL
                ## Country unmet need trend on logit scale
                logitZstar.ci[c,i] <- (unmet.intercept.c[c]
                    + a.unmet
                    + b.unmet * (p.ci[c,getest.ci[c,i]] - pmid.for.unmet)
                    + c.unmet * pow(p.ci[c,getest.ci[c,i]] - pmid.for.unmet,2))
                ## <<<<< RATE MODEL
            }## end unique-obs year loop
        }##end country loop

        ##
        ## ---------- TRAINING SETS ----------
        ##

        for (k in 1:n.training.breakdown){
            ratios.trad.modern.jn[getj.training.k[k],1:2] ~ dmnorm(
                mu.jn[getj.training.k[k], ],T.j[getj.training.k[k],,])
        }

        ## unmet training
        for (k in 1:n.training.unmet){
            logitratio.yunmet.j[getj.training.unmet.k[k]] ~ dnorm(
                logitratio.yunmet.hat.j[getj.training.unmet.k[k]],
                ## >>>>> RATE MODEL
                ##tau.unmet.source.s[source.ind.unmet.j[getj.training.unmet.k[k]]])
                tau.unmet.j[getj.training.unmet.k[k]])
            ## <<<<< RATE MODEL
        }

        ## total training set
        for (k in 1:n.training.tot){
            ##  logit.ytothat.j[j] <- logit(1-none.adj.j[j])
            logit.ytothat.j[getj.training.tot.k[k]] <-
                logit(max(0.0000001, 1-none.adj.j[getj.training.tot.k[k]]))
            logit.ytot.j[getj.training.tot.k[k]] ~ dnorm(
                logit.ytothat.j[getj.training.tot.k[k]], tau.sourcetot)
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                            Global Model                             ##
        ##---------------------------------------------------------------------##
        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        }
    if(at.random.no.data) {
        cat("
        ##
        ## ---------- EXCLUDED OBS IF DOING VALIDATION ----------
        ##

        for (j in (J+1):(J+J.test)){
            ## >>>>> RATE MODEL
            for (h in 1:getperiod.j.test[j-J]) {
                trad.jh[j, h] = p.ci[getc.j.test[j-J], getest.jf.test[j-J, h]] * (1 - R.ci[getc.j.test[j-J], getis.jf.test[j-J, h]])
                modern.jh[j, h] = p.ci[getc.j.test[j-J], getest.jf.test[j-J, h]] * R.ci[getc.j.test[j-J], getis.jf.test[j-J, h]]
                unmet.jh[j, h] = (1 - p.ci[getc.j.test[j-J], getest.jf.test[j-J, h]])*
                    (1 / (1 + neg.explogitZ.ci[getc.j.test[j-J], getis.jf.test[j-J, h]]))
            }
            trad.j[j] = 1 / period.j.test[j-J] *
                inprod(trad.jh[j, 1:getperiod.j.test[j-J]], partialtime.xj.test[1:getperiod.j.test[j-J], j-J])
            modern.j[j] = 1 / period.j.test[j-J] *
                inprod(modern.jh[j, 1:getperiod.j.test[j-J]], partialtime.xj.test[1:getperiod.j.test[j-J], j-J])
            unmet.j[j] = 1 / period.j.test[j-J] *
                inprod(unmet.jh[j, 1:getperiod.j.test[j-J]], partialtime.xj.test[1:getperiod.j.test[j-J], j-J])

            ## Not in rate model:
            ## --
            ## logitZstar.j[j] <- (
            ##     unmet.intercept.c[getc.j.test[j-J]]
            ##     + a.unmet
            ##     + b.unmet * (p.ci[getc.j.test[j-J], geti.j.test[j-J]] - pmid.for.unmet)
            ##     + c.unmet*pow(p.ci[getc.j.test[j-J], geti.j.test[j-J]] - pmid.for.unmet,2))
            ## --
            ## logitZ.j defined in AR-loop, just adding theta

            sump.j[j] <- (trad.j[j]*Vtrad.j[j]
                + modern.j[j]* Vmodern.j[j]
                + (1- p.ci[getc.j.test[j-J], getest.j.test[j-J]])) #Change NC, 20160811
            ## <<<<< RATE MODEL
        }

        ## Extra elements needed if doing validation run

        for (c in (C+1):(C+C.no.data)){
            for (i in 1:N.unique.c.test[c-C]){
                Rmu.ci[c, i] <- Romega.c[c]*(gett.ci.test[c-C,i] - RT.c[c])
                Rstar.ci[c, i] <- Rmax.c[c]/(1+exp(-Rmu.ci[c, i]))
            } ## end unique-obs year loop
           }

        for (c in (C+1):(C+C.no.data)){
            for (i in 1:N.unique.c.test[c-C]){
                ## Country unmet need trend on logit scale
                logitZstar.ci[c,i] <- (unmet.intercept.c[c]
                    + a.unmet
                    + b.unmet * (p.ci[c,getest.ci.test[c-C,i]] - pmid.for.unmet)
                    + c.unmet * pow(p.ci[c,getest.ci.test[c-C,i]] - pmid.for.unmet,2))
            }## end unique-obs year loop
        }##end country loop

        for(k in 1:N.unmet.test) {
            logitratio.yunmet.hat.j[getj.test.unmet.k[k]] <-
                logit(max(0.0000001,q.ij[3, getj.test.unmet.k[k]])/none.adj.j[getj.test.unmet.k[k]])
        }

        ##
        ## ---------- TEST SETS ----------
        ##

        ## Extra elements needed if doing validation run

        for(k in (n.test.breakdown + 1):J.test){
            logit.ytothat.j[getj.test.k[k]] <- logit(max(0.0000001, 1-none.adj.j[getj.test.k[k]]))
        }

    ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
        }
    cat("
        ##=====================================================================##
        ##             Perturbations and Misclassification Biases              ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             All Models                              ##
        ##---------------------------------------------------------------------##

        for(j in 1:J) {

            p.perturb.ij[1,j] <-  trad.j[j]*Vtrad.j[j]/sump.j[j]
            p.perturb.ij[2,j] <-  modern.j[j]* Vmodern.j[j]/sump.j[j]
            p.perturb.ij[3,j] <- unmet.j[j]/sump.j[j]
            p.perturb.ij[4,j] <- (1- trad.j[j] - modern.j[j] - unmet.j[j])/sump.j[j]

            folkbias.j[j] <- step(folk.ind1.j[j]-0.5)*v.folk* p.perturb.ij[3,j]
            absprobeqbias.j[j] <- step(abs.probe.q.ind1.j[j]-0.5)* v.abs.probe.q * p.perturb.ij[1,j]
            modposbias.j[j] <- step(mpos.ind1.j[j]-0.5)*v.mpos* p.perturb.ij[4,j]
            modnegbias.j[j] <- step(mneg.ind1.j[j]-0.5)*v.mneg * p.perturb.ij[2,j]

            q.ij[1,j] <- p.perturb.ij[1,j] - absprobeqbias.j[j] + folkbias.j[j]
            q.ij[2,j] <- p.perturb.ij[2,j] + modposbias.j[j] - modnegbias.j[j]
            q.ij[3,j] <- p.perturb.ij[3,j] + absprobeqbias.j[j] - folkbias.j[j]
            q.ij[4,j] <- p.perturb.ij[4,j] - modposbias.j[j] + modnegbias.j[j]
            none.adj.j[j] <- max(0.0000001,q.ij[3,j]) + q.ij[4,j]

            mu.jn[j,1] <- log(max(0.0000001, q.ij[1,j])/none.adj.j[j])
            mu.jn[j,2] <- log(max(0.0000001, q.ij[2,j])/ none.adj.j[j])

            Vtrad.j[j] <- (
                V.geo.12i[1,geo.ind.j[j]]
                * V.age.12i[1,age.ind.j[j]]
                * V.hw.12i[1,hw.ind.j[j]]
                * V.emal.12i[1,emal.ind.j[j]]
                * V.sa.12i[1,sa.ind.j[j]]
                * V.posbias.12i[1,posbias.ind.j[j]]
                * V.posage.12i[1, posage.ind.j[j]]
                * V.negage.12i[1, negage.ind.j[j]] )

            Vmodern.j[j] <- (
                V.geo.12i[2,geo.ind.j[j]]
                * V.age.12i[2,age.ind.j[j]]
                * V.hw.12i[2,hw.ind.j[j]]
                * V.emal.12i[2,emal.ind.j[j]]
                * V.sa.12i[2,sa.ind.j[j]]
                * V.posbias.12i[2,posbias.ind.j[j]]
                * V.posage.12i[2, posage.ind.j[j]]
                * V.negage.12i[2, negage.ind.j[j]] )

            ## T.j[j,1:2, 1:2] <- T.s[source.ind.j[j],,] # >>RATE MODEL<< Changed..see later.

        } # end loop J observations

        ##
        ## Multipliers V in [0,inf): geo, emal, hw, age other, sa (for trad only)
        ##

        V.sa.12i[1,1] <- 1
        V.geo.12i[1,1] <- 1
        V.geo.12i[2,1] <- 1
        V.age.12i[1,1] <- 1
        V.age.12i[2,1] <- 1
        V.hw.12i[1,1] <- 1
        V.hw.12i[2,1] <- 1
        V.emal.12i[1,1] <- 1
        V.emal.12i[2,1] <- 1

        ##
        ## ---------- m LOOP ----------
        ##

        for (m in 1:2){
            ## These are now done at the end of this section
            ## ---
            ## for (i in 2:ncat.emal){
            ##     V.emal.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
            ## }
            ## for (i in 2:ncat.hw){
            ##     V.hw.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
            ## }
            ## ---
            for (i in 2:ncat.geo){
                V.geo.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
            }
            for (i in 2:ncat.age){
                V.age.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
            }
            tau.geo.m[m] <- pow(sigma.geo.m[m], -2)
        } # end m-loop

        for (i in 2:ncat.sa){
            V.sa.12i[1,i] ~ dlnorm(0, tau.geo.m[1])
        }

        ##
        ## multpliers V > 1: sa (for modern only), posbias, age+
        ## multpliers V < 1: age-
        ##

        V.sa.12i[2,1] <- 1
        V.posbias.12i[1,1] <- 1
        V.posbias.12i[2,1] <- 1
        V.posage.12i[1,1] <- 1
        V.posage.12i[2,1] <- 1
        V.negage.12i[1,1] <- 1
        V.negage.12i[2,1] <- 1

        ##
        ## m = 2:
        ##

        for (i in 2:ncat.sa){
            W.sa.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
            V.sa.12i[2,i] <- 1+W.sa.12i[2,i]
        }
        for (i in 2:ncat.posbias){
            W.posbias.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
            V.posbias.12i[2,i] <- 1+W.posbias.12i[2,i]
        }
        for (i in 2:ncat.posage){
            W.posage.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
            V.posage.12i[2,i] <-1+W.posage.12i[2,i]
        }
        for (i in 2:ncat.negage){
            W.negage.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
            V.negage.12i[2,i] <- 1/(1+W.negage.12i[2,i])
        }
        tau.pos <- pow(sigma.pos, -2)

        ##
        ## m=1
        ## note: could simplify code and throw out these V's
        ##

        for (i in 2:ncat.posbias){
            V.posbias.12i[1,i] <- 1+exp(mu.pos.m[1])
        }
        for (i in 2:ncat.posage){
            V.posage.12i[1,i] <- 1+exp(mu.pos.m[1])
        }
        for (i in 2:ncat.negage){
            V.negage.12i[1,i] <- 1/(1+exp(mu.pos.m[1]))
        }

        ##
        ## 'HW' and 'AW' Biases
        ##

        ## Done here because user can change way they are modelled
        ## with function arguments

            ## EA bias negative
        for (m in 1:2){
            for (i in 2:ncat.emal){
                V.emal.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
            }
        }

        ## HW bias NOT positive
        for(m in 1:2) {
            for (i in 2:ncat.hw){
                V.hw.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
            }
        }

    ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)

    if(at.random.no.data) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        ##
        ## ---------- Validation 'at.random.no.data' ----------
        ##

        ## Extra steps beyond 'J'

        for(j in (J+1):(J + J.test)) {

            p.perturb.ij[1,j] <-  trad.j[j]*Vtrad.j[j]/sump.j[j]
            p.perturb.ij[2,j] <-  modern.j[j]* Vmodern.j[j]/sump.j[j]
            p.perturb.ij[3,j] <- unmet.j[j]/sump.j[j]
            p.perturb.ij[4,j] <- (1- trad.j[j] - modern.j[j] - unmet.j[j])/sump.j[j]

            folkbias.j[j] <- step(folk.ind1.j[j]-0.5)*v.folk* p.perturb.ij[3,j]
            absprobeqbias.j[j] <- step(abs.probe.q.ind1.j[j]-0.5)* v.abs.probe.q * p.perturb.ij[1,j]
            modposbias.j[j] <- step(mpos.ind1.j[j]-0.5)*v.mpos* p.perturb.ij[4,j]
            modnegbias.j[j] <- step(mneg.ind1.j[j]-0.5)*v.mneg * p.perturb.ij[2,j]

            q.ij[1,j] <- p.perturb.ij[1,j] - absprobeqbias.j[j] + folkbias.j[j]
            q.ij[2,j] <- p.perturb.ij[2,j] + modposbias.j[j] - modnegbias.j[j]
            q.ij[3,j] <- p.perturb.ij[3,j] + absprobeqbias.j[j] - folkbias.j[j]
            q.ij[4,j] <- p.perturb.ij[4,j] - modposbias.j[j] + modnegbias.j[j]
            none.adj.j[j] <- max(0.0000001,q.ij[3,j]) + q.ij[4,j]

            mu.jn[j,1] <- log(max(0.0000001, q.ij[1,j])/none.adj.j[j])
            mu.jn[j,2] <- log(max(0.0000001, q.ij[2,j])/ none.adj.j[j])

            Vtrad.j[j] <- (
                V.geo.12i[1,geo.ind.j[j]]
                * V.age.12i[1,age.ind.j[j]]
                * V.hw.12i[1,hw.ind.j[j]]
                * V.emal.12i[1,emal.ind.j[j]]
                * V.sa.12i[1,sa.ind.j[j]]
                * V.posbias.12i[1,posbias.ind.j[j]]
                * V.posage.12i[1, posage.ind.j[j]]
                * V.negage.12i[1, negage.ind.j[j]] )

            Vmodern.j[j] <- (
                V.geo.12i[2,geo.ind.j[j]]
                * V.age.12i[2,age.ind.j[j]]
                * V.hw.12i[2,hw.ind.j[j]]
                * V.emal.12i[2,emal.ind.j[j]]
                * V.sa.12i[2,sa.ind.j[j]]
                * V.posbias.12i[2,posbias.ind.j[j]]
                * V.posage.12i[2, posage.ind.j[j]]
                * V.negage.12i[2, negage.ind.j[j]] )

            ## T.j[j,1:2, 1:2] <- T.s[source.ind.j[j],,] # >>RATE MODEL<< Changed..see later.
        } # end loop J+J.test observations

    ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
    }
    if(one.country.run) {
    cat("
        ##---------------------------------------------------------------------##
        ##                             One Country Run                         ##
        ##---------------------------------------------------------------------##

        # add dummy column in case ncol(V) = 1 => V becomes vector!
        V.geo.12i[1,max(geo.ind.j)+1] <- 0
        V.age.12i[1,max(age.ind.j)+1] <- 0
        V.hw.12i[1,max(hw.ind.j)+1] <- 0
        V.emal.12i[1,max(emal.ind.j)+1] <- 0
        V.sa.12i[1,max(sa.ind.j)+1] <- 0
        V.posbias.12i[1,max(posbias.ind.j)+1] <- 0
        V.posage.12i[1,max(posage.ind.j)+1] <- 0
        V.negage.12i[1,max(negage.ind.j)+1] <- 0

        V.geo.12i[2,max(geo.ind.j)+1] <- 0
        V.age.12i[2,max(age.ind.j)+1] <- 0
        V.hw.12i[2,max(hw.ind.j)+1] <- 0
        V.emal.12i[2,max(emal.ind.j)+1] <- 0
        V.sa.12i[2,max(sa.ind.j)+1] <- 0
        V.posbias.12i[2,max(posbias.ind.j)+1] <- 0
        V.posage.12i[2,max(posage.ind.j)+1] <- 0
        V.negage.12i[2,max(negage.ind.j)+1] <- 0

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        }
    cat("
        ##=====================================================================##
        ##                            Source Variances                         ##
        ##=====================================================================##

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    if(global.run || global.validation.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

        for(j in 1:J)
        {
            tau.unmet.j[j] <-
                1 / (pow(se.logR.unmet.impute[j],2) +
                     pow(nonsample.se.unmet.s[source.ind.unmet.j[j]],2)) #Change NC, 20170112
        }

        for (j in 1:J) {
            S.j[j,1,1] <-
                pow(se.logR.trad.impute[j],2) +
                pow(nonsample.se.trad.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                             ,4, source.ind.j[j])],2) #+0.00001
            S.j[j,2,2] <- pow(se.logR.modern.impute[j],2) +
                pow(nonsample.se.modern.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                               ,4, source.ind.j[j])],2) #+0.00001
            S.j[j,1,2] <- cor.trad.modern.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                                 , 4, source.ind.j[j])] *
                pow(S.j[j,1,1],0.5) * pow(S.j[j,2,2],0.5)
            S.j[j,2,1] <- cor.trad.modern.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                                 , 4, source.ind.j[j])] *
                pow(S.j[j,1,1],0.5) * pow(S.j[j,2,2],0.5) #Assume no covariance for now

            T.j[j,1:2,1:2] <- inverse(S.j[j,1:2,1:2])
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
        if(at.random.no.data) {
            cat("
        for(j in (J+1):(J+J.test))
        {
            tau.unmet.j[j] <-
                1 / (pow(se.logR.unmet.impute.test[j-J],2) +
                     pow(nonsample.se.unmet.s[source.ind.unmet.j[j]],2)) #Change NC, 20170112
        }

        for(j in (J+1):(J+J.test)) {
            S.j[j,1,1] <-
                pow(se.logR.trad.impute.test[j-J],2) +
                pow(nonsample.se.trad.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                             ,4, source.ind.j[j])],2) #+0.00001
            S.j[j,2,2] <- pow(se.logR.modern.impute.test[j-J],2) +
                pow(nonsample.se.modern.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                               ,4, source.ind.j[j])],2) #+0.00001
            S.j[j,1,2] <- cor.trad.modern.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                                 , 4, source.ind.j[j])] *
                pow(S.j[j,1,1],0.5) * pow(S.j[j,2,2],0.5)
            S.j[j,2,1] <- cor.trad.modern.s[ifelse(source.ind.j[j] == 7 #[MCW-2017-12-29-3] Changed to 7
                                                 , 4, source.ind.j[j])] *
                pow(S.j[j,1,1],0.5) * pow(S.j[j,2,2],0.5) #Assume no covariance for now

            T.j[j,1:2,1:2] <- inverse(S.j[j,1:2,1:2])
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
            }
    } else if(one.country.run) {
        cat("
        ##---------------------------------------------------------------------##
        ##                           One Country Run                           ##
        ##---------------------------------------------------------------------##

        ## [MCW-2018-03-31] FIX sent by Niamh (31/3/2018)... was ' == 5, 4'
        ## but now ' == 7, 4'. Always disaggregating PMA as its own source.

        for (j in 1:J) {
        S.j[j,1,1]<-pow(se.logR.trad.impute[j],2) +
           pow(nonsample.se.trad.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])],2) #+0.00001
        S.j[j,2,2]<-pow(se.logR.modern.impute[j],2) +
           pow(nonsample.se.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])],2) #+0.00001
        S.j[j,1,2]<- cor.trad.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])]*
               pow(S.j[j,1,1],0.5)*pow(S.j[j,2,2],0.5)
        S.j[j,2,1]<- cor.trad.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])]*
               pow(S.j[j,1,1],0.5)*pow(S.j[j,2,2],0.5) #Assume no covariance for now

        T.j[j,1:2,1:2]<-inverse(S.j[j,1:2,1:2])
        }

        for(j in 1:J)
        {
          tau.unmet.j[j]<-1/(pow(se.logR.unmet.impute[j],2)+
             pow(nonsample.se.unmet.s[source.ind.unmet.j[j]],2)) #Change NC, 20170112
        }

        ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = TRUE)
    }
  if(mcmc.meta$general$do.country.specific.run) {
  if (!is.null(mcmc.meta$winbugs.data$n.training.modern)) {
    if (mcmc.meta$winbugs.data$n.training.modern > 0) {
      if (!mcmc.meta$general$do.SS.run.first.pass) { # change JR, 20140808
                cat("
        ## RATE MODEL >>>>>

        ##=====================================================================##
        ##                            Service Statistics                       ##
        ##=====================================================================##

        ##---------------------------------------------------------------------##
        ##                             One Country Run                         ##
        ##---------------------------------------------------------------------##

        ## modern training set (for service statistics)
        for (k in 1:n.training.modern) {
            y.modern.j[getj.training.modern.k[k]] ~ dnorm(
                modern.perturb.j[getj.training.modern.k[k]],
                tau.modern.j[getj.training.modern.k[k]])

            modern.perturb.j[getj.training.modern.k[k]] <-
                (modern.j[getj.training.modern.k[k]]*bias.modern) /
                (modern.j[getj.training.modern.k[k]]*bias.modern +
                 (1 - modern.j[getj.training.modern.k[k]]))

            tau.modern.j[getj.training.modern.k[k]] <-
                pow(se.modern.fix[getj.training.modern.k[k]], -2)

            se.modern.fix[getj.training.modern.k[k]] <-
                se.modern.unif
        }
        se.modern.unif ~ dunif(0,1)

        ## <<<<< RATE MODEL

        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
            }
        }
  }
  }
    if(global.validation.run) {
    cat("
        ##=====================================================================##
        ##              Predictive Distributions for Validations               ##
        ##=====================================================================##

        ## All of the variables with names beginning with 'pred.'. These are used
        ## in 'GetPercentilesLeftOut()' in 'F_validation.R'.

        ## Also, all variables that are defined *only* for test set observations.
        ## Extra elements of variables already defined for training set obs. are
        ## included above in the relevant sections.

        ##---------------------------------------------------------------------##
        ##                             Global Run                              ##
        ##---------------------------------------------------------------------##

    ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)

    if(at.random || at.end || exclude.unmet.only) {
      cat("
        for (k in 1:N.unmet.test){
            pred.logitratio.yunmet.j[getj.test.unmet.k[k]] ~ dnorm(
                logitratio.yunmet.hat.j[getj.test.unmet.k[k]],
                tau.unmet.j[getj.test.unmet.k[k]]
                )
            q.unmet.j[getj.test.unmet.k[k]] <- q.ij[3,getj.test.unmet.k[k]]
        }
        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

          if (exclude.unmet.only){
              cat("
              for (k in 1:N.unmet.test){
              # get total too
                  pred.ratios.trad.modern.jn[getj.test.unmet.k[k],1:2] ~ dmnorm(
                      mu.jn[getj.test.unmet.k[k], ],
                      T.j[getj.test.unmet.k[k],,]
                  )
              pred.logratio.ytrad.j[getj.test.unmet.k[k]] <- pred.ratios.trad.modern.jn[getj.test.unmet.k[k],1]
              pred.logratio.ymodern.j[getj.test.unmet.k[k]] <- pred.ratios.trad.modern.jn[getj.test.unmet.k[k],2]
              }
              ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    } else {
          cat("
              for (k in 1:n.test.breakdown){
                  pred.ratios.trad.modern.jn[getj.test.k[k],1:2] ~ dmnorm(
                      mu.jn[getj.test.k[k], ],
                      T.j[getj.test.k[k],,]
                  )
              pred.logratio.ytrad.j[getj.test.k[k]] <- pred.ratios.trad.modern.jn[getj.test.k[k],1]
              pred.logratio.ymodern.j[getj.test.k[k]] <- pred.ratios.trad.modern.jn[getj.test.k[k],2]

              q.trad.j[getj.test.k[k]] <- q.ij[1,getj.test.k[k]]
              q.modern.j[getj.test.k[k]] <- q.ij[2,getj.test.k[k]]
              }

    ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    } # end if-!exclude.unmet.only loop
    } else if(at.random.no.data) {
        cat("
            for (k in 1:n.test.breakdown){
                        pred.ratios.trad.modern.jn[getj.test.k[k],1:2] ~ dmnorm(
                            mu.jn[getj.test.k[k], ],
                            T.j[getj.test.k[k],,]
                        )
                        pred.logratio.ytrad.j[getj.test.k[k]] <- pred.ratios.trad.modern.jn[getj.test.k[k],1]
                        pred.logratio.ymodern.j[getj.test.k[k]] <- pred.ratios.trad.modern.jn[getj.test.k[k],2]

                        q.trad.j[getj.test.k[k]] <- q.ij[1,getj.test.k[k]]
                        q.modern.j[getj.test.k[k]] <- q.ij[2,getj.test.k[k]]
                    }

                    ## Need to do obs with only total
                    for(k in (n.test.breakdown + 1):J.test){
                        pred.logit.ytot.j[getj.test.k[k]] ~ dnorm(
                            logit.ytothat.j[getj.test.k[k]],
                            tau.sourcetot
                        )

                        q.tot.j[getj.test.k[k]] <- none.adj.j[getj.test.k[k]]
                    }

                    ## 'pred.logitratio.yunmet.j', 'q.unmet.j' have values only for
                    ## test-set observations. They are vectors are of length 'J+J.test' and
                    ## have NAs except in rows corresponding to observations that
                    ## have been left out. In this excercise, these will all be at
                    ## the end of the vectors.
                    for (k in 1:N.unmet.test){
                        pred.logitratio.yunmet.j[getj.test.unmet.k[k]] ~ dnorm(
                            logitratio.yunmet.hat.j[getj.test.unmet.k[k]],
                            tau.unmet.j[getj.test.unmet.k[k]]
                        )

                    q.unmet.j[getj.test.unmet.k[k]] <- q.ij[3,getj.test.unmet.k[k]]
                    }    ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
    }
    }

    ## ################################################################### ##
    ## ################################################################### ##
    ##
    ##                       T   H   E      E   N   D                      ##
    ##
    ## ################################################################### ##
    ## ################################################################### ##

    cat("} # end model
",sep="", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
}
## THE END!
##--------------------------------------------------------------
