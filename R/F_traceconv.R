#----------------------------------------------------------------------
# Leontine Alkema
#----------------------------------------------------------------------
CheckConvergence <- function(# Check convergence
  ### Check convergence using raftery.diag and gelman.diag, and construct trace plots.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored, and convergence report will be saved.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  fig.dir = NULL, ##<< Directory to store trace plots. If NULL, folder "fig" in current working directory.
  plot.trace = TRUE, ##<< Construct trace plots?
  check.convergence = TRUE, ##<< Calculate and report raftery.diag and gelman.diag?
  use.all.parameters = FALSE, ##<< If FALSE, only a random sample of the country-spec parameters, AR distortion terms and
  ## perturbation multipliers is selected for traceplots and/or convergence checks.
  chains = NULL, ## If non-NULL, indices \code{\chains} are used in \code{mcmc.array}
  additionalburnin = NULL ## If non-NULL, the first \code{addtionalburnin} iterations in the \code{mcmc.array} are excluded
  ## MCW-2016-05-16-1. To allow for png format traceplots.
 ,png.traceplots = FALSE
  ,sink.convergence.log = TRUE
) {
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name, "/")
  if (is.null(fig.dir))
    fig.dir <- file.path(getwd(), "fig/")
  dir.create(fig.dir, showWarnings = FALSE)

  load(file.path(output.dir, "mcmc.meta.rda")) # change JR, 20140418
  load(file.path(output.dir, "mcmc.array.rda")) # change JR, 20140418
  #  parnames.list <- mcmc.meta$parnames.list

  if (!is.null(chains)){
    mcmc.array <- mcmc.array[,chains,]
  }
  if (!is.null(additionalburnin)){
    mcmc.array <- mcmc.array[-seq(1,additionalburnin),,]
  }

  if (plot.trace) {
          dir.create(file.path(fig.dir, "traceplots"), recursive = TRUE)
      if(png.traceplots) {              #MCW-2016-05-16-2: Allow png format traceplots.
          png(file.path(fig.dir, "traceplots"
                      ,paste0(run.name, "trace_%03d.png")))
      } else {
          pdf(file.path(fig.dir, "traceplots"
                      ,paste0(run.name, "trace.pdf"))) # change JR, 20140418
      }
  parnames.all <- GetParnamesDiagandPlotTrace(run.name = run.name,
                                              plot.trace = plot.trace,
                                              mcmc.meta = mcmc.meta,
                                              mcmc.array =  mcmc.array,
                                              use.all.parameters = use.all.parameters
                                              )
      dev.off()
  }
  #------------------------------------------------------------------------------------------
  ##details<< Convergence checks:
  ##details<< Use Gelman's R after running several chains, and
  ## raftery.diag for each chain (the median over the chains is used if there are several chains).

  if (check.convergence){
      ##details<< Output of gelman and raftery are written to \code{output.dir/convergence_check.txt}.
      if(sink.convergence.log) {
          filename <- file.path(output.dir, "convergence_check.txt") # change JR, 20140418
          fileout <- file(filename, open = "wt")
          sink(fileout, split = T)
          cat(paste("Results convergence are written to file ", filename), "\n")
      }
    n.chains <- dim(mcmc.array)[2]
    n.sim <- dim(mcmc.array)[1]*n.chains
    if (n.sim <= 600){
      cat("You need more samples to check convergence!", "\n")
      #    cat("If you constructed trace plots, you might have additional warnings", "\n")
      return()
    } else {
      InternalGetRafteryGelmanDiag(
        mcmc.array = mcmc.array,
        parnames.all = parnames.all)
    }
      if(sink.convergence.log) {
          sink()
          closeAllConnections()
      }
  } # end convergence check
  ##value<< NULL, convergence diagnostic summary is written to
    ## \code{output.dir/convergence_check.txt}.
  return(invisible(NULL))
}

#--------------------------------------------------------------------
InternalGetRafteryGelmanDiag <- function(# Get Rafter/Gelman diagnostics
  ### No output, but messages are printed.
  ### Can be used for any ''mcmc.array''.
  mcmc.array, ##<< (nsim, nchain, npar), 3rd dimension needs to be named!!
  parnames.all = NULL, ##<< included in mcmc.array. If NULL, all parnames in mcmc.array
  verbose = TRUE ##<< Print info that .. diagnostics is being calculated?
  ){
  if (!is.null(parnames.all)){
    parnames.all <- parnames.all[is.element(parnames.all, dimnames(mcmc.array)[[3]])]
  } else {
    parnames.all <- dimnames(mcmc.array)[[3]]
  }

  if (verbose) cat(paste("Start error messages based on Raftery's diagnostic"), "\n")
  n.chains <- dim(mcmc.array)[2]
  n.sim <- dim(mcmc.array)[1]*n.chains
  shortest.chain <- min(apply(mcmc.array, c(2,3), "length")[,1])
  if (shortest.chain <= 600){ #[MCW-2018-01-05]:: one chain at a time run through raftery.diag().
    cat("You need more samples to check convergence!", "\n")
#    cat("If you constructed trace plots, you might have additional warnings", "\n")
    return()
  }
  n.par <- length(c(parnames.all))
  res1.pc <- res2.pc <- matrix(NA, n.par, n.chains)
  rownames(res1.pc) <- rownames(res2.pc) <- c(parnames.all)
  for (chain in 1:n.chains){
    res1.pc[,chain] <- raftery.diag(mcmc.array[,chain,c(parnames.all)],
                                    q = 0.975, r = 0.0125)$resmatrix[,"N"]
    res2.pc[,chain] <- raftery.diag(mcmc.array[,chain,c(parnames.all)],
                                    q = 0.025, r = 0.0125)$resmatrix[,"N"]
  }
  N1 <- apply(res1.pc,1,median,na.rm = T) # change JR, 20131105
  N2 <- apply(res2.pc,1,median,na.rm = T) # change JR, 20131105
  if (length(N1[N1 > n.sim])>0 | length(N2[N2 > n.sim])>0){
    if (length(N2[N2 > n.sim])>0){
      cat(paste("(RD) Additional samples needed for:", names(N2[N2 > n.sim| is.na(N2)]), round(N2[N2 > n.sim]), "\n"))
    }
    if (length(N1[N1 > n.sim])>0){
      cat(paste("(RD) Additional samples needed for:", names(N1[N1 > n.sim | is.na(N1)]), round(N1[N1 > n.sim]), "\n"))
    }
  } else {
    if (verbose) cat(paste("End of Raftery error messages, no convergence issues reported."), "\n")
  }
  # Gelman
  if (verbose) cat(paste("Start error messages based on Gelman's R"), "\n")
  if (n.chains <3){
    cat("You need at least 3 chains for gelman.diag!", "\n")
  } else {
    # all parameters that are positive and asym distr are transformed
    # transform = T in gelman.diag gives logit/log automatically
    # but logit sometimes a problem if close to 1
    # so decide beforehand

    #       parnames.all <-  GetParnamesDiagandPlotTrace(run.name = run.name,
    #         plot.trace = FALSE,  parnames.list = parnames.list,
    #         mcmc.meta = mcmc.meta,mcmc.array =  mcmc.array)
    #
    parnames.gelman.logtr <- parnames.gelman.logittr <- parnames.gelman.nottr <- NULL
    for (parname in c(parnames.all)){
      ##details<< For gelman.diag, transformations are used only if the
      #        ## median is outside the (40h, 60th) percentile of the posterior sample, and if the
      ## samples are in (0.001, 0.999) for a logit-transform,
      ## or in (0.001+) for a log-transform.

      outsidep <- TRUE #abs(0.5 - mean(c(mcmc.array[,,parname]) <= mean(c(mcmc.array[,,parname]))))>0.1
      if (min(c(mcmc.array[,,parname]))>0.001 & outsidep ){
        if (max(c(mcmc.array[,,parname]))<0.999){
          parnames.gelman.logittr <- c(parnames.gelman.logittr, parname)
        } else {
          parnames.gelman.logtr <- c(parnames.gelman.logtr, parname)
        }
      } else {
        parnames.gelman.nottr <- c(parnames.gelman.nottr, parname)
      }
    }
    for (i in 1:3){
      parnames <- list(parnames.gelman.logtr, parnames.gelman.logittr, parnames.gelman.nottr)[[i]]
      if (length(parnames)>0){
        # only check the univariate version, not the linear combinations
        # (error when too many parameters)
        R <- rep(NA,length(parnames) )
        p <- 0
        for (parname in parnames){
          p <- p+1 # index
          if (i==1){
            mcmc.array.temp <- log(mcmc.array[,,parname])
          } else {
            if (i==2){
              mcmc.array.temp <- logit(mcmc.array[,,parname])
            } else {
              mcmc.array.temp <- mcmc.array[,,parname]
            }}
          mcmc <- mcmc.list()
          for (chain in 1:n.chains){
            mcmc[[chain]] <- as.mcmc(mcmc.array.temp[,chain])
          }
          r<- gelman.diag(mcmc, autoburnin = FALSE, transform = F)$psrf
          R[p] <-r[,"Point est."]
        }
        names(R) <- parnames
        if (length(R[R>1.1])>0){
          cat(paste("(GD) Additional samples needed for:", names(R[R>1.1]), "(R = ", round(R[R>1.05],3), ")", "\n"))
        }
      }
    }
  if (verbose) cat(paste("End of Gelman error messages (if no messages above, no convergence issues reported!)"), "\n")
  }# end nchains if for gelman
  return()
}
#----------------------------------------------------------------------------------
PlotTrace <- function(#Traceplot for one parameter
### Trace plot for one parameter and add loess smoother for each chain
                      parname, mcmc.array,##<< needs to be 3-dimensional array!
                      n.chains= NULL, n.sim= NULL, main = NULL,
                      loess.curves = dim(mcmc.array)[1] > 4) {
    if (is.null(main)) main <- parname
    if (is.null(n.sim)) n.sim <- dim(mcmc.array)[1]
    if (is.null(n.chains)) n.chains <- dim(mcmc.array)[2]
    if((parname %in% dimnames(mcmc.array)[[3]])) {
        plot(c(mcmc.array[,1,parname]), type = "l", ylab = parname,  main = main,
             ylim = c(min(mcmc.array[,,parname]),max(mcmc.array[,,parname])))
        for (chain in 1:n.chains){
            lines(c(mcmc.array[,chain,parname]), type = "l", col = chain)
        }
        if(loess.curves) {           #Protect against errors
            for (chain in 1:n.chains) {
                curve(predict(loess(c(mcmc.array[,chain,parname])~seq(1,n.sim)),x)
                    , lty = 2, lwd = 3, add = TRUE, type = "l", col = chain)
            }
        }
    }
}
#----------------------------------------------------------------------------------
GetParnamesDiagandPlotTrace <- function(# Get parnames for convergence checks and plot trace (optional)
  ### Get vector with parameter names to be used for convergence check
  ### and plot trace (optional)
  run.name = "test",
  plot.trace = TRUE, ##< Logical: plot trace?
  mcmc.meta,
  mcmc.array,
  use.all.parameters ##<< Use all parameters?
  ){
  do.country.specific.run <- mcmc.meta$general$do.country.specific.run # change JR, 20131104
  parnames.all <- NULL
  parnames.list <- mcmc.meta$parnames.list
  if (!do.country.specific.run) {
    for (parname in parnames.list$parnames.h){
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    if (plot.trace) par(mfrow = c(2,2))
    if(mcmc.meta$general$disagg.RN.PMA) {
    for (s in 1:6){
      parname = paste0("T1.source.s[", s, "]")
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    for (s in 1:6){
      parname = paste0("T2.source.s[", s, "]")
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    for (s in 1:6){
      parname <- paste0("T12.source.s[", s, "]")
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    } else {
    for (s in 1:4){
      parname = paste0("T1.source.s[", s, "]")
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    for (s in 1:4){
      parname = paste0("T2.source.s[", s, "]")
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    for (s in 1:4){
      parname <- paste0("T12.source.s[", s, "]")
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
        }
    ## [MCW-2017-07-20-1] :: Ensure correct number of regions and subregions are used.
    if(ModelFunctionRegInSA(mcmc.meta$general$write.model.fun)) {
        name.reg <- mcmc.meta$data.raw$region.info$name.sex.ac.unm
        n.reg <- mcmc.meta$data.raw$region.info$n.sex.ac.unm
        name.subreg <- mcmc.meta$data.raw$region.info$name.reg.in.sex.ac.unm
        n.subreg <- mcmc.meta$data.raw$region.info$n.reg.in.sex.ac.unm
    } else if(ModelFunctionSubRegInSA1India(mcmc.meta$general$write.model.fun)) {
        name.reg <- mcmc.meta$data.raw$region.info$name.sex.ac.unm
        n.reg <- mcmc.meta$data.raw$region.info$n.sex.ac.unm
        name.subreg <- mcmc.meta$data.raw$region.info$name.reg.in.sex.ac.unm.SA1sub
        n.subreg <- mcmc.meta$data.raw$region.info$n.reg.in.sex.ac.unm.SA1sub
    } else {
        name.reg <- mcmc.meta$data.raw$region.info$name.reg
        n.reg <- mcmc.meta$data.raw$region.info$n.reg
        name.subreg <- mcmc.meta$data.raw$region.info$name.subreg
        n.subreg <- mcmc.meta$data.raw$region.info$n.subreg
    }

    for (parname.reg in parnames.list$parnames.reg){
        if (plot.trace) par(mfrow = c(3,2), cex.axis = 1.5, cex.lab = 1.5)
      for (reg in 1:n.reg){
        for (parname in paste0(parname.reg, "[", reg, "]")){
          parnames.all <- c(parnames.all, parname)
          if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array,
                                    main = name.reg[reg])
        }
      }
    }
    for (parname.subreg in parnames.list$parnames.subreg){
      if (plot.trace) par(mfrow = c(3,2), cex.axis = 1.5, cex.lab = 1.5)
      for (subreg in 1:n.subreg){
        for (parname in paste0(parname.subreg, "[", subreg, "]")){
          parnames.all <- c(parnames.all, parname)
          if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array,
                                    main = name.subreg[subreg])
        }
      }
    }
  } # end do.country.specific.run

  ##details<< For country parameters, AR(1) distortion terms and the multipliers,
  ## a random sample of parameters is used if \code{use.all.parameters} is FALSE
  ## to construct traceplots and check convergence.
  # Country... just select 12 at random
  for (parindex in 1:length(parnames.list$parnames.c)) { # change JR, 20131105
    parname.c <- parnames.list$parnames.c[parindex]
    if (plot.trace & (mcmc.meta$winbugs.data$C > 1 | (mcmc.meta$winbugs.data$C == 1 & parindex == 1) )) # change JR, 20131105
      par(mfrow = c(2,3), cex.axis = 1.5, cex.lab = 1.5)
    for (c in sample(seq(1,mcmc.meta$winbugs.data$C), ifelse(use.all.parameters, mcmc.meta$winbugs.data$C, 12))){
      for (parname in paste0(parname.c, "[", c, "]")){
        parnames.all <- c(parnames.all, parname)
        if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array,
                                  main = mcmc.meta$data.raw$country.info$name.c[c])
      }
    }
  }
  # AR's: just select 6 from each at random
  if (mcmc.meta$include.AR){
    if (plot.trace) par(mfrow = c(2,3), cex.axis = 1.5, cex.lab = 1.5)
    parselect <- parnames.list$parnames.eps[
      sample(seq(1,length(parnames.list$parnames.eps)),
             ifelse(use.all.parameters, length(parnames.list$parnames.eps), 6)
      )]
    for (parname in parselect){
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    parselect <- parnames.list$parnames.theta[
      sample(seq(1,length(parnames.list$parnames.theta)),
             ifelse(use.all.parameters, length(parnames.list$parnames.theta), 6)
      )]
    for (parname in parselect){
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
    parselect <- parnames.list$parnames.eta[
      sample(seq(1,length(parnames.list$parnames.eta)),
             ifelse(use.all.parameters, length(parnames.list$parnames.eta), 6)
      )]
    for (parname in parselect){
      parnames.all <- c(parnames.all, parname)
      if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array)
    }
  }
  # Trace plots multipliers
  par.V <- mcmc.meta$par.V #InternalGetParnamesV(winbugs.data = mcmc.meta$winbugs.data, name.short.j = InternalMakeCountryNamesShort(mcmc.meta$data.raw$data$name.j))
  # if (plot.trace) pdf(file.path(fig.dir, paste0(run.name, "traceV.pdf")))
  # sample instead!
  #for (i in 1:length(par.V$parnames.V.in.bugs)){
  if (do.country.specific.run) { # change JR, 20131111
    # remove parameters that are not sampled
    exclude <- grepl("V.posbias.12i\\[1|V.posage.12i\\[1|V.negage.12i\\[1", par.V$parnames.V.in.bugs)
    par.V$parnames.V.in.bugs <- par.V$parnames.V.in.bugs[!exclude]
    par.V$parnames.V.nice <- par.V$parnames.V.nice[!exclude]
  }
  parselect <- sample(1:length(par.V$parnames.V.in.bugs),
                      ifelse(use.all.parameters, length(par.V$parnames.V.in.bugs), 12))
  if (plot.trace) par(mfrow = c(2,3), cex.axis = 1.5, cex.lab = 1.5)
  for (i in parselect) {
    parname <- par.V$parnames.V.in.bugs[i]
    parnames.all <- c(parnames.all, parname)
    if (plot.trace) PlotTrace(parname = parname, mcmc.array = mcmc.array,
                              main = par.V$parnames.V.nice[i])
  } # i's
  #end V's'

  # change JR, 20140630
  parname <- "bias.modern"
  if (parname %in% dimnames(mcmc.array)[[3]]) {
    parnames.all <- c(parnames.all, parname)
    if (plot.trace) {
      par(mfrow = c(2,3), cex.axis = 1.5, cex.lab = 1.5)
      PlotTrace(parname = parname, mcmc.array = mcmc.array, main = parname)
    }
  }

  ##value<< Vector with parameter names
  return(parnames.all)
}
#---------------------------------------------------------------------------------
CheckConvergenceProbs <- function(# Check if we have sufficient samples for desired accuracy of estimates
  ### Check convergence using raftery.diag and gelman.diag for country estimates
  ### Only appropriate if no repeated AR-sampling was carried out, and no chains were excluded when constructing the mcmc.array
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored, and convergence report will be saved.
  #chains = NULL, ## If non-NULL, indices \code{\chains} are used in \code{mcmc.array}
  #    additionalburnin = NULL ## If non-NULL, the first \code{addtionalburnin} iterations in the \code{mcmc.array} are excluded
  fig.dir  = NULL
  ){

  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name, "/")
  }
  if (is.null(fig.dir)){
    fig.dir <- file.path(getwd(), "fig/")
    dir.create(fig.dir, showWarnings = FALSE)
  }
  load(file.path(output.dir, "mcmc.meta.rda")) # change JR, 20140418
  load(file.path(output.dir,"res.country.rda")) # change JR, 20140418
  country.info <- mcmc.meta$data.raw$country.info
  n.chains <- length(mcmc.meta$general$ChainNums)
  dir.traj <- file.path(getwd(), "output", run.name, "countrytrajectories")
  load(file.path(dir.traj, paste0("P.tp3s_country", 1, ".rda"))) # change JR, 20140418
  # note: s refers to stacked up chains...
  # assume the number of chains from meta is used
  n.sim <- dim(P.tp3s)[[3]]/n.chains
  # chains were stacked using c(mcmc.array[,,bla])
  #temp <- array(seq(1,24), c(2,3,4))
  #temp
  #c(temp[,,4])
  #array(c(temp[,,4]),c(2, 3))
  if (n.sim <= 600){
    cat("You need more samples to check convergence!", "\n")
    return()
  }
  est.years <- as.numeric(colnames(res.country$CIprop.Lg.Lcat.qt[[1]][[1]]))
  select.years <- c(1990.5, 2000.5, 2010.5, 2015.5)
  select.t <- seq(1, length(est.years))[is.element(est.years, select.years)]

  ##details<< Output of gelman and raftery are written to \code{output.dir/countryconvergence_check.txt}.
  filename <- file.path(output.dir, "countryconvergence_check.txt") # change JR, 20140418
  fileout <- file(filename, open = "wt")
  sink(fileout, split = T)
  cat(paste("Results convergence checks for estimates in years", est.years[select.t], "are written to file ", filename), "\n")
  cat("\n")
  #mcmc.array <- array(NA, c(1, n.chains, length(country.info$name.c)*3*sum(select.t)))
  #p <-
  country.info <- mcmc.meta$data.raw$country.info
  pdf(file.path(fig.dir, paste0(run.name, "countryesttrace.pdf"))) # change JR, 20140418
  for (c in 1:length(country.info$name.c)){
    par(mfrow = c(2,2), cex.axis = 1.5, cex.lab = 1.5)
    #cat(paste("Results country:", country.info$name.c[c]), "\n")
    load(file.path(dir.traj, paste0("P.tp3s_country", c, ".rda")))
    for (t in 1:length(select.t)){
      for (p in 1:3){
        mcmc.array.temp <- array(c(P.tp3s[select.t[t],p,]),c(n.sim, n.chains,1))
        dimnames(mcmc.array.temp) <- list(NULL, NULL, paste(country.info$name.c[c], select.years[t], c("trad", "modern", "unmet")[p]))
        InternalGetRafteryGelmanDiag(mcmc.array = mcmc.array.temp, verbose = FALSE)
        PlotTrace(parname = dimnames(mcmc.array.temp)[[3]], mcmc.array = mcmc.array.temp)
      }
    }
    # examine changes 1990-2000 and 2000-2010
  }
  dev.off()
  cat(paste("End convergence checking. If no messages above, no problems diagnosed!"), "\n")
  sink()
  closeAllConnections()
  return(invisible())
}

#----------------------------------------------------------------------
# The End!
