#--------------------------------------------------------------------------
# source_priorpost.R
# LA, Jan 2012
#--------------------------------------------------------------------------
PlotPriorPost <- function(# Plot posteriors and priors for hyper parameters
  ### Plot posteriors and priors for hyper parameters
  run.name = "test", ##<<,
  output.dir = NULL, ##<<, directory where main output are stored,
  # defaults to output/run.name in current working directory
  fig.dir = NULL ##<< defaults to fig in current working directory
  ## [MCW-2016-05-16-4] Added to make sure variances get plotted for new sources
 ,disagg.RN.PMA = TRUE
){
  if (is.null(output.dir))
    output.dir <- file.path(getwd(), "output", run.name, "/")
  if (is.null(fig.dir)) {
    fig.dir <- file.path(getwd(), "fig/")
    dir.create(fig.dir, showWarnings = FALSE)
  }
  load(file.path(output.dir, "mcmc.meta.rda")) # change JR, 20140418
  load(file.path(output.dir, "mcmc.array.rda")) # change JR, 20140418

  write.model.fun <- mcmc.meta$general$write.model.fun

  rate.model <- ModelFunctionRateModel(mcmc.meta$general$write.model.fun)

  pars.in.mcmc <- dimnames(mcmc.array)[[3]]

  ## [MCW-2016-06-17-2] Look-up list for latex equiv symbols to add to the plots
  latex.symbols <-
      list(sigma.wc = expression(kappa[omega]^list({(c)}))
          ,sigma.Rwc = expression(kappa[psi]^list({(c)}))
          ,sigma.lpc = expression(kappa[P]^list({(c)}))
          ,sigma.lrc = expression(kappa[R]^list({(c)}))
          ,sigma.RTc = expression(kappa[Psi]^list({(c)}))
          ,sigma.Tc = expression(kappa[Omega]^c)
          ,sigma.wreg = expression(kappa[omega]^list({(r)}))
          ,sigma.Rwreg = expression(kappa[psi]^list({(r)}))
          ,sigma.wsubreg = expression(kappa[omega]^list({(s)}))
          ,sigma.Rwsubreg = expression(kappa[psi]^list({(s)}))
          ,sigma.RTreg = expression(kappa[Psi]^list({(r)}))
          ,sigma.Treg = expression(kappa[Omega]^list({(r)}))
          ,sigma.RTsubreg = expression(kappa[Psi]^list({(s)}))
         ,sigma.Tsubreg = expression(kappa[Omega]^list({(c)}))
          ,sigma.earlierTc = expression(kappa[Omega]^D)
          ,sigma.unmetc = expression(kappa[z]^c)
          ,sigma.sourcetot = expression(sigma[T])
          ,sigma.tot = expression(sigma[epsilon])
          ,sigma.rat = expression(sigma[eta])
          ,sigma.unmet = expression(sigma[theta])
          ,sigma.unmetworld = expression(kappa[z]^list({(r)}))
           ## [MCW-2016-11-09-1] :: Added symbol for variances of unmet need data.
           ,sigma.unmet.dhs = expression(sigma[paste(1,",",3)])
           ,sigma.unmet.other = expression(sigma[paste(2,",",3)])
          ,rho.tot = expression(rho[epsilon])
          ,rho.rat = expression(rho[eta])
          ,rho.unmet = expression(rho[theta])
          ,lp.world = expression(tilde(P)[w])
          ,lr.world = expression(tilde(R)[w])
          ,Rw.world = expression(psi[w])
          ,w.world = expression(omega[w])
          ,a.unmet = expression(z[w])
          ,b.unmet = expression(beta[1])
           ,c.unmet = expression(beta[2])
           )

  if (!mcmc.meta$general$do.country.specific.run) { # for global run
    pdf(file.path(fig.dir, paste0(run.name, "priorpost_all.pdf")), width = 10, height = 10)
    #----------------------------------------------------------------------------------
    # prior and posteriors of kappas (variances in the BHMs for logistic curves) and two more gammas
    parnames <- c("sigma.wc", "sigma.Rwc", "sigma.lpc", "sigma.lrc","sigma.RTc")
    rateforparnames <- unlist(mcmc.meta$winbugs.data[c(
      "halfnu0sigma2.wc0","halfnu0sigma2.Rwc0",
      "halfnu0sigma2.lpc0", "halfnu0sigma2.lrc0",
      "halfnu0sigma2.RTc0" )])

    par(mfrow = c(1,1))
    if (mcmc.meta$general$change.priors.to.zerolower){ # unif priors
                                # priors not implemented
        parnames <- c(parnames, "sigma.Tc", "sigma.earlierTc","sigma.unmetc")
        parnames <- parname[parname %in% pars.in.mcmc]
      for (parname in parnames) {
          PlotPostOnly(post.samp =  c(mcmc.array[,,parname]), parname = parname)
          ##[MCW-2016-06-17-1] added latex equiv symbols as legends
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
      }

    } else { # gamma priors
      p <- 0
      for (parname in parnames){
        p <- p+1
        PlotPostSDWithGammaPrior(post.samp =  c(mcmc.array[,,parname]),
                                 priorshape = mcmc.meta$winbugs.data$halfnu0,
                                 priorrate = rateforparnames[p],
                                 parname = parname)
          ##[MCW-2016-06-17-3] added latex equiv symbols as legends
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
      }
      if("sigma.unmetc" %in% pars.in.mcmc) {
      for (parname in "sigma.unmetc"){
        PlotPostSDWithGammaPrior(post.samp =  c(mcmc.array[,,parname]),
                                 priorshape = 0.5,
                                 priorrate = mcmc.meta$winbugs.data$halfsigma2.unmetc0,
                                 parname = parname)
          ##[MCW-2016-06-17-4] added latex equiv symbols as legends
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
      }
      }
      if("sigma.earlierTc" %in% pars.in.mcmc) {
      for (parname in "sigma.earlierTc"){
        PlotPostSDWithGammaPrior(post.samp =  c(mcmc.array[,,parname]),
                                 priorshape =  mcmc.meta$winbugs.data$halfnu0_rich,
                                 priorrate = mcmc.meta$winbugs.data$halfnu0_rich_sigma2.earlierTc0,
                                 parname = parname)
          ##[MCW-2016-06-17-5] added latex equiv symbols as legends
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
      }
      }
      if("sigma.Tc" %in% pars.in.mcmc) {
      for (parname in "sigma.Tc"){
        PlotPostSDWithGammaPrior(post.samp =  c(mcmc.array[,,parname]),
                                 priorshape =  mcmc.meta$winbugs.data$halfnu0_poor,
                                 priorrate = mcmc.meta$winbugs.data$halfnu0_poor_sigma2.Tc0,
                                 parname = parname)
          ##[MCW-2016-06-17-6] added latex equiv symbols as legends
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
      }
      }
    }
    #----------------------------------------------------------------------------------
    # other gammas, with shape 0.5
if("sigma.sourcetot" %in% pars.in.mcmc) {
    parnames <- c("sigma.sourcetot")
    prrates <-  unlist(mcmc.meta$winbugs.data[c("halfsigma2.sourcetot0")])
    for (p in 1:length(parnames)){
        parname <- parnames[p]
        PlotPostSDWithGammaPrior(post.samp =  c(mcmc.array[,,parname]),
                                 priorshape = 0.5, priorrate = prrates[p],
                                 parname = parname)
        ##[MCW-2016-06-17-7] added latex equiv symbols as legends
        if(!is.null(latex.symbols[[parname]])) {
            legend("topright", lty = 0, legend = latex.symbols[[parname]])
        }
    }
    }
    #--------------------------------------------------------------------------
    # wishart T.s
    # Simulate from Wishart prior
    # Note: bugs notation, thus R is on the variance scale
    # R <- cbind(c(0.1,0),c(0,0.1))
    R <- mcmc.meta$winbugs.data$R
    Vinv <- solve(R)
    k <- 3 # change JR, 20131113: from k <- 4 # change JR, 20131031: from k <- 2
    nsimu <- 1000
    sigma.1 <- rep(NA, nsimu)
    sigma.2 <- rep(NA,nsimu)
    rho <- rep(NA,nsimu)
    for (s in 1:nsimu){
      Sigma <- solve(MCMCpack::rwish(v = k, S = Vinv)) # R specs: T ~ W(k,S) with E(T) = k*S, thus S = Vinv
      sigma.1[s] <- sqrt(Sigma[1,1])
      sigma.2[s] <- sqrt(Sigma[2,2])
      rho[s] <- Sigma[1,2]/(sigma.1[s]*sigma.2[s])
    }
    # hist(sigma.1, freq = F)
    # hist(1/sigma.1^2, freq = F)
    # curve(dgamma(x, 1/2, R[1,1]/2), add = T)
    # hist(sigma.2)
    # hist(rho)

    # posterior
    I <- dim(mcmc.array)[1]*dim(mcmc.array)[2]
    # s refers to cov matrix for s = DHS, MICS, NS, Other
    ## [MCW-2016-05-16-5]: Plot additional source-type variances
    if(!disagg.RN.PMA) {
        n.sources <- 4
        data.sources.long <- c("DHS", "MICS", "NS", "Other")
    } else {
        data.sources.long <- c("DHS", "MICS", "NS", "Other", "CPLT1pc", "PMA")
        n.sources <- length(data.sources.long) #Depends on which sources actually present.
    }
    psigma1.si <- matrix(NA, n.sources, I) # [MCW-2016-05-16-6]: 4 -> n.sources
    psigma2.si <- matrix(NA, n.sources, I) # [MCW-2016-05-16-7]: 4 -> n.sources
    prho.si <- matrix(NA,n.sources, I)# [MCW-2016-05-16-8]: 4 -> n.sources
    if(any(grepl("nonsample.se.trad.s", pars.in.mcmc))) {
    for (s in 1:n.sources){
      parname <- paste0("nonsample.se.trad.s[", s, "]")
      psigma1.si[s,] <- mcmc.array[,,parname]
      parname <- paste0("nonsample.se.modern.s[", s, "]")
      psigma2.si[s,] <- mcmc.array[,,parname]
      parname <- paste0("cor.trad.modern.s[", s, "]")
      prho.si[s,] <- mcmc.array[,,parname]
    }
        } else if(any(grepl("T[0-9]+\\.source\\.s", pars.in.mcmc))) {
    for (s in 1:n.sources){# [MCW-2016-05-16-9]: 4 -> n.sources
      parname <- paste0("T1.source.s[", s, "]")
      T11 <- mcmc.array[,,parname]
      parname <- paste0("T2.source.s[", s, "]")
      T22 <- mcmc.array[,,parname]
      parname <- paste0("T12.source.s[", s, "]")
      T12 <- mcmc.array[,,parname]
      for (i in 1:I){
        Sigma <- solve(cbind(c(T11[i], T12[i]), c(T12[i], T22[i])))
        psigma1.si[s,i] <- sqrt(Sigma[1,1])
        psigma2.si[s,i] <- sqrt(Sigma[2,2])
        prho.si[s,i] <- Sigma[1,2]/(psigma1.si[s,i]*psigma1.si[s,i])
      }
    }
    }
    for (s in 1:n.sources){ # [MCW-2016-05-16-10]: 4 -> n.sources
        par(mfrow = c(2,2))
        hist(psigma1.si[s,], freq = F,  col = "grey",
                 xlab = paste0("sigma11 (", data.sources.long[s], ")"),
                 main = data.sources.long[s])
      lines(density(sigma.1, to =2), col= 2)
      ##[MCW-2016-06-17-8] added latex equiv symbols as legends
      legend("topright", lty = 0, legend = expression(sigma[list(S,1)]))

      hist(psigma2.si[s,], freq = F,  col =  "grey",
          xlab = paste0("sigma22 (", data.sources.long[s], ")"),
                 main = data.sources.long[s])
      lines(density(sigma.2, to =2), col= 2)
      ##[MCW-2016-06-17-9] added latex equiv symbols as legends
      legend("topright", lty = 0, legend = expression(sigma[list(S,2)]))

      hist(prho.si[s,], freq = F,  xlim = c(-1,1), col =  "grey",
          xlab = paste0("rho (", data.sources.long[s], ")"),
                 main = data.sources.long[s])
      lines(density(rho), col= 2)
      ##[MCW-2016-06-17-10] added latex equiv symbols as legends
      legend("topright", lty = 0, legend = expression(rho[S]))
    }
    #--------------------------------------------------------------------------
    # AR parameters
    par(mfrow = c(2,2))
    parnames <- c("rho.tot", "rho.rat")
    parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in parnames){
      PlotPostWithUnifPrior(post.samp=  c(mcmc.array[,,parname]),
                            priorlow = 0, priorup = mcmc.meta$winbugs.data$rho.max, parname = parname)
      ## [MCW-2016-06-17-16] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    parnames <- c("sigma.tot","sigma.rat")
    parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in parnames){
      PlotPostWithUnifPrior(post.samp=  c(mcmc.array[,,parname]),
                            priorlow = 0.01, priorup = mcmc.meta$winbugs.data$sigma.ar.max
                          ,parname = parname)
      ## [MCW-2016-06-17-17] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    #--------------------------------------------------------------------------
    # hierarchical variances, non-country level
    if(all(c("lp.world.not.sex", "lp.world.sex") %in% pars.in.mcmc)) {
        par(mfrow = c(3,2))
        parnames <- c("lp.world.not.sex", "lp.world.sex", "lr.world")
        parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in  parnames){
      PlotPostWithNormalPrior(post.samp=  c(mcmc.array[,,parname]),
                              priormean = 0, priorsd = 1/sqrt(0.01), parname = parname)
      ## [MCW-2016-06-16-2] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
        parnames <- c("Rw.world", "w.world")
        parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in parnames){
      PlotPostWithNormalPrior(post.samp=  c(mcmc.array[,,parname]),
                              priormean = -1, priorsd = 1/sqrt(0.01), parname = parname)
      ## [MCW-2016-06-16-3] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
        } else {
            par(mfrow = c(2,2))
            parnames <- c("lp.world", "lr.world")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in  parnames){
      PlotPostWithNormalPrior(post.samp=  c(mcmc.array[,,parname]),
                              priormean = 0, priorsd = 1/sqrt(0.01), parname = parname)
      ## [MCW-2016-06-16-2] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
            parnames <- c("Rw.world", "w.world")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in parnames){
      PlotPostWithNormalPrior(post.samp=  c(mcmc.array[,,parname]),
                              priormean = -1, priorsd = 1/sqrt(0.01), parname = parname)
      ## [MCW-2016-06-16-3] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    }

    parnames <-c("RT.world", "T.world", "TOneLevel")
    parnames <- parnames[parnames %in% pars.in.mcmc]

    means <- unlist(mcmc.meta$winbugs.data[c("mean.RTworld", "mean.Tworld", "mean.TOneLevel")])
    ## >>>>>>>>>>>>>>>> LEVEL MODEL ONLY >>>>>>>>>>>>>>>>>>>>>>
    if(!rate.model) {
    p <- 0
    for (parname in  parnames){
      p <- p+1
      PlotPostWithNormalPrior(post.samp=  c(mcmc.array[,,parname]),
                              priormean = means[p], priorsd = 1/sqrt(mcmc.meta$winbugs.data$tau0.T), parname = parname)
      ## [MCW-2016-06-16-4] added latex equiv symbols as legends.
    if(parname == "RT.world") {
        legend("topright", lty = 0, legend = c(expression(Psi[w])))
    } else if(parname == "T.world") {
        legend("topright", lty = 0, legend = c(expression(Omega[L])))
    } else {
        legend("topright", lty = 0, legend = c(expression(Omega[D])))
    }
    }
    par(mfrow = c(2,2))
    parnames <-c("sigma.RTreg", "sigma.Treg", "sigma.RTsubreg", "sigma.Tsubreg")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in  parnames){
      PlotPostWithUnifPrior(post.samp=  c(mcmc.array[,,parname]),
                            priorlow = 0, priorup = mcmc.meta$winbugs.data$sigmaTregsubreg.upper,
                            parname = parname)
      ## [MCW-2016-06-17-14] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    }
    ## <<<<<<<<<<<<<<<<<<<< LEVEL MODEL ONLY <<<<<<<<<<<<<<<<<<<
    par(mfrow = c(2,2))
    ##[MCW-2016-06-14-1] Added psi kappas
    parnames <-c("sigma.wreg", "sigma.Rwreg", "sigma.wsubreg", "sigma.Rwsubreg")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in  parnames){
      PlotPostWithUnifPrior(post.samp=  c(mcmc.array[,,parname]),
                            priorlow = 0, priorup = mcmc.meta$winbugs.data$sigmawregsubreg.upper,
                            parname = parname)
      ## [MCW-2016-06-17-15] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    #--------------------------------------------------------------------------
    # data dummies
    par(mfrow = c(2,2))
    parnames <-c("v.abs.probe.q", "v.mneg", "v.folk", "v.mpos")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in  parnames){
      PlotPostWithUnifPrior(post.samp=  c(mcmc.array[,,parname]),
                            priorlow = 0, priorup = 1,
                            parname = parname)
      ## [MCW-2016-06-17-16] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    parnames <-c("sigma.geo.m[1]", "sigma.geo.m[2]")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    p <- 0
    for (parname in  parnames){
      p <- p+1
      PlotPostWithUnifPrior(post.samp = c(mcmc.array[,,parname]),
                            priorlow = 0, priorup = 2,#c(5,5,2,2)[p],
                            parname = parname)
      ## [MCW-2016-06-17-17] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    parnames <-c("sigma.pos")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    p <- 0
    for (parname in  parnames){
      p <- p+1
      PlotPostWithUnifPrior(post.samp = c(mcmc.array[,,parname]),
                            priorlow = 0.01, priorup = 2,#c(5,5,2,2)[p],
                            parname = parname)
      ## [MCW-2016-06-17-18] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    #parnames <-c("mu.pos.m[1]", "mu.pos.m[2]")
    par(mfrow = c(1,2))
    parnames <-c("mu.pos.m[1]", "mu.pos.m[2]")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in  parnames){
      PlotPostWithNormalPrior(post.samp =c(mcmc.array[,,parname]),
                              priormean = -2, priorsd = 1/sqrt(0.64), parname = parname)
      ## [MCW-2016-06-17-19] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    #--------------------------------------------------------------------------
    # unmet
    par(mfrow = c(2,2))
    parnames <- c("sigma.unmet.dhs", "sigma.unmet.other")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    for (parname in parnames){
      PlotPostWithUnifPrior(post.samp =  c(mcmc.array[,,parname]),
                            priorlow = 0.01, priorup = 2, parname = parname)
      ## [MCW-2016-06-17-20] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    if("sigma.unmetworld" %in% pars.in.mcmc) {
    parname <- "sigma.unmetworld"
    PlotPostWithUnifPrior(post.samp = c(mcmc.array[,,parname]),
                          priorlow = 0, priorup = 5, parname = parname)
      ## [MCW-2016-06-17-21] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    if("c.unmet" %in% pars.in.mcmc) {
        parname <- "c.unmet"
        if(ModelFunctionCUnmetPrior(mcmc.meta$general$write.model.fun)) {
            priorlow  <-  -35
            priorup  <-  0
        } else {
            priorlow  <-  -10
            priorup  <-  0
        }
    PlotPostWithUnifPrior(post.samp = c(mcmc.array[,,parname]),
                          priorlow = priorlow, priorup = priorup, parname = parname)
      ## [MCW-2016-06-17-22] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }

    par(mfrow = c(2,2))
    parnames <- c("a.unmet", "b.unmet")
            parnames <- parnames[parnames %in% pars.in.mcmc]
    sds <- 1/sqrt(unlist(mcmc.meta$winbugs.data[c("tau.a0", "tau.b0")]))
    means <- unlist(mcmc.meta$winbugs.data[c("a0.unmet", "b0.unmet")])
    p <- 0
    for (parname in parnames){
      p <- p+1
      PlotPostWithNormalPrior(post.samp=  c(mcmc.array[,,parname]),
                              priormean = means[p], priorsd = sds[p], parname = parname)
      ## [MCW-2016-06-17-23] added latex equiv symbols as legends.
          if(!is.null(latex.symbols[[parname]])) {
              legend("topright", lty = 0, legend = latex.symbols[[parname]])
          }
    }
    dev.off()
  } else { # for country-specific run
    #--------------------------------------------------------------------------
    #parname <- "bias.modern"
    #if (parname %in% pars.in.mcmc) {
    #  pdf(file.path(fig.dir, paste0(run.name, "priorpost_all.pdf")), width = 10, height = 10)
    #  if (mcmc.meta$general$do.SS.run.first.pass) {
    #    PlotPostWithUnifPrior(post.samp = c(mcmc.array[,,parname]),
    #                          priorlow = 0, priorup = 30, parname = parname)
    #  } else {
    #    PlotPostWithTruncatedNormalPrior(post.samp = c(mcmc.array[,,parname]),
    #                                     priormean = mcmc.meta$winbugs.data$median.bias.modern,
    #                                     priorsd = mcmc.meta$winbugs.data$se.bias.modern,
    #                                     priorlower = 0, parname = parname)
    #  }
    #  dev.off()
    #}
  }
  #--------------------------------------------------------------------------

  #--------------------------------------------------------------------------
  # Extra
  #----------------------------------------------------------------------------------
  # Joint posteriors for the hyper parameters
  # summ <- NULL
  # for (parname in c(parnames.list$parnames.h)){#, parnames.reg, parnames.c,parnames.subreg)){
  #   summ <- rbind(summ, quantile(mcmc.array[,,parname], c(0.025, 0.5, 0.975)))
  #  }
  # summ
  #parnames.list$parnames.h
  # stack.sp <- rbind(mcmc.array[,1,], mcmc.array[,2,])
  # res <- cor(stack.sp[,parnames.list$parnames.h])
  # res
  # library(arm)
  # corrplot(res, cutpts = c(0,0.25,0.5, 0.75,0.8,0.9,1))
  # #abline(v = seq(0,100), col = "grey")
  # #abline(h = seq(0,100), col = "grey")
  #
  # parname1 <- "sigma.rat"
  # parname2 <- "rho.rat"
  # parname1 <- "mu.pos"
  # parname2 <- "sigma.pos"
  # parname1 <- "TOneLevel"
  # parname2 <- "sigma.earlierTc"
  # par(mfrow = c(1,1), mar = c(5,5,1,1))
  # plot(mcmc.array[,,parname1] ~ mcmc.array[,,parname2], xlab = parname2, ylab = parname1)


  ##value<< NULL
  return(invisible(NULL))
}
#----------------------------------------------------------------------
# The End!
