PlotSourceSigmas <- function (# Plot posteriors related to covariance matrices of (trad, modern)/total
  ### Plot posteriors of sigmas and rhos for (trad, modern)/total by source
  mcmc.array,
  col_pxmr = c("red","purple", "green", "brown"),
  percentiles = c(0.025, 0.5, 0.975),
  return.res = FALSE
 ,write.model.fun = "WriteModel"
 ,UWRA = FALSE
 ,ylim = NULL
  ,hide.CP.tot.lt.1pc = FALSE
) {
  I <- dim(mcmc.array)[1]*dim(mcmc.array)[2]
  ## s refers to cov matrix for s = DHS, MICS, NS, Other

  ## Define variable for dimension of var-covar matrix to
  ## accommodate disaggregation of PMA .
  vc.dim <- 6

  psigma1.si <- matrix(NA,vc.dim, I)
  psigma2.si <- matrix(NA,vc.dim, I)
  prho.si <- matrix(NA,vc.dim, I)
  ## [MCW-2017-04-20-9] Were survey based SEs used?
  if(any(grepl("nonsample.se.trad.s", dimnames(mcmc.array)[[3]]))) {

  for (s in 1:vc.dim){
    parname <- paste0("nonsample.se.trad.s[", s, "]")
    psigma1.si[s,] <- mcmc.array[,,parname]
    parname <- paste0("nonsample.se.modern.s[", s, "]")
    psigma2.si[s,] <- mcmc.array[,,parname]
    parname <- paste0("cor.trad.modern.s[", s, "]")
    prho.si[s,] <- mcmc.array[,,parname]
  }
      } else {
  for (s in 1:vc.dim){                  #[MCW-2016-03-09-1]: changed to
                                        #accommodate new data sources.
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
  psigma1.sq <-  t(apply(psigma1.si, 1, quantile, percentiles))
  psigma2.sq <-  t(apply(psigma2.si, 1, quantile, percentiles))
  prho.sq <-  t(apply(prho.si, 1, quantile, percentiles))
  sourcenames <- c("DHS", "MICS", "National", "Other", "CP(Tot)<1%", "PMA")
                                        #[MCW-2016-03-09-2]: changed to
                                        #accommodate new sources.
  rownames(psigma1.sq) =  rownames(psigma2.sq)=  rownames(prho.sq)= sourcenames

  ## If MWRA don't include "CP(Tot)<1%"
  if(!UWRA || hide.CP.tot.lt.1pc) {
      psigma1.sq <- psigma1.sq[-5,]
      psigma2.sq <- psigma2.sq[-5,]
      prho.sq <- prho.sq[-5,]
      sourcenames <- sourcenames[-5]
      vc.dim <- 5
  }

  par( mar = c(8,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)

  if(ModelFunctionSurveySEs(write.model.fun)) plot.main <- "Non-sampling errors only"
  else plot.main <- NULL

  if(is.null(ylim)) ylim  <- c(0, max(psigma1.sq, psigma2.sq)*1.5)

  plot(psigma1.sq[, 2] ~ seq(0.9,vc.dim - 0.1)   #[MCW-2016-03-09-5]: upper limit changed to vc.dim - 0.1
     , ylim = ylim,
       xlim = c(0.5,vc.dim + 0.5),               #[MCW-2016-03-10-1]: upper limit changed to vc.dim + 0.5.
       ylab = "sigma",pch = 19, lwd =8,
       xlab = "", xaxt = "n", col = col_pxmr[2]
      ,main = plot.main, cex.main = 0.5)
      abline(h = seq(from = 1, to = max(ylim[2], 1), by = 1), col = "grey")
  axis(1, las = 3, at = seq(1,vc.dim)        #[MCW-2016-03-09-12]: upper limit changed to 6.
     , labels = sourcenames)
  segments(seq(0.9,vc.dim - 0.1), psigma1.sq[,1], seq(0.9,vc.dim - 0.1) #[MCW-2016-03-09-6]: upper
                                                      #limits of both calls to
                                                      #seq() changed to vc.dim - 0.1.
         , psigma1.sq[,3], col = col_pxmr[2], lwd = 3)
  points(psigma2.sq[,2]~seq(1.1,vc.dim + 0.1)    #[MCW-2016-03-09-7]: upper limit changed to vc.dim + 0.1.
       , pch = 19 ,lwd =8, col = col_pxmr[3])
  segments(seq(1.1,vc.dim + 0.1) #[MCW-2016-03-09-8]: upper limit changed to vc.dim + 0.1.
         , psigma2.sq[,1], seq(1.1,vc.dim + 0.1) #[MCW-2016-03-09-9]: upper limit changed to vc.dim + 0.1.
         , psigma2.sq[,3],lwd =3, col = col_pxmr[3])
  legend("topright", legend = c("Traditional", "Modern"), col = col_pxmr[c(2,3)], pch = 19, lwd =3, cex = 1.2)

  plot(prho.sq[, 2] ~ seq(0.9,vc.dim - 0.1)      #[MCW-2016-03-09-10]: upper limit changed to vc.dim - 0.1
     , ylim = c(-1,1),
       xlim = c(0.5,vc.dim + 0.5), #[MCW-2016-03-10-2]: upper limit changed to vc.dim + 0.5.
       ylab = "rho",pch = 19, lwd =8,
       xlab = "", xaxt = "n", col = 1
       ,main = plot.main, cex.main = 0.5)
  abline(h=0, lwd = 1)
  axis(1, las = 3, at = seq(1,vc.dim)        #[MCW-2016-03-09-3]: changed to
                                        #incorporate new sources.
     ,labels = sourcenames
       )  # should correspond to source.name in GetBugsData
          # [MCW-2016-03-09-4]: added new data sources, using same codes used in
          # GetBugsData() in the 'names.sources' argument.
  segments(seq(0.9,vc.dim - 0.1), prho.sq[,1], seq(0.9,vc.dim - 0.1) #[MCW-2016-03-09-11]: upper
                                                   #limits of both calls to
                                                   #seq() changed to vc.dim - 0.1.
         , prho.sq[,3], col = 1, lwd = 3)

  out.list <- list(psigma1.sq = psigma1.sq,
                   psigma2.sq = psigma2.sq,
                    prho.sq = prho.sq)

  if (return.res) return(out.list)
}
###-----------------------------------------------------------------------------------------------
PlotSourceSigmasUnmet <- function (# Plot posteriors of unmet/none by source
  ### Plot posteriors of unmet/none by source
  mcmc.array, percentiles = c(0.025, 0.5, 0.975),
  return.res  = FALSE, ylim = NULL) {
  parname <- "sigma.unmet.dhs"
  res.dhs.q <- quantile(c(mcmc.array[,,parname]), percentiles)
  parname <- "sigma.unmet.other"
  res.other.q <- quantile(c(mcmc.array[,,parname]), percentiles)
  par(mar = c(8,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  if(is.null(ylim)) ylim  <- c(0, max(res.dhs.q, res.other.q)*1.5)
    plot(1, res.dhs.q[2], ylim = ylim,
       xlim = c(0.5,2.5),
       ylab = "sigma", pch = 19, lwd =8,
       xlab = "", xaxt = "n")
      abline(h = seq(from = 1, to = max(ylim[2], 1), by = 1), col = "grey")
  axis(1, las = 3, at = seq(1,2), labels = c("DHS", "Other"))# should correspond to source.name.unmet in GetBugsData
  points(2, res.other.q[2], lwd = 8)
  segments(1, res.dhs.q[1], 1, res.dhs.q[3], lwd = 3, col = 1)
  segments(2, res.other.q[1], 2, res.other.q[3], lwd = 3, col = 1)
  legend("topright", legend = c("Unmet"), col = 1, pch = 19, lwd =3, cex = 1.5)
  if (return.res) return(list(res.other.q = res.other.q, res.dhs.q = res.dhs.q))
}
###-------------------------------------------------------------------------------
##' Plot total data model variances (sampling + non-sampling).
##'
##' This function is only relevant if design-based standard errors are used. It
##' combines estimates of sampling variance from design standard errors with
##' estimates of non-sampling errors from model run.
##'
##' Samping and non-sampling errors are added on the variance scale. Different
##' sampling errors are available for each survey within type (e.g., each DHS
##' has a separate estimate for sampling error). These are averaged over within
##' survey type by simply combining the MCMC samples and computing quantiles.
##' @param run.name
##' @param output.dir
##' @param mcmc.array
##' @param col_pxmr
##' @param percentiles
##' @param return.res
##' @return TODO.
##' @author Mark Wheldon, based on \code{\link{PlotSourceSigmas}}.
##' @noRd
PlotDataModelSEs <- function(run.name, output.dir, mcmc.array,
                             col_pxmr = c("red","purple", "green", "brown")
                            ,percentiles = c(0.025, 0.5, 0.975)
                             ,ylim = NULL
                            ,return.res = FALSE
                             ,hide.CP.tot.lt.1pc = FALSE)  {

    ## -------* Constants

    vc.dim <- 6
    sourcenames <- c("DHS", "MICS", "NS", "Other", "CP(Tot)<1%", "PMA")

    load(file = file.path(output.dir, "mcmc.meta.rda"))

    UWRA <- identical(mcmc.meta$general$marital.group, "UWRA")

    ## -------* Non-sampling errors

    ## |--8<------ Copied from 'PlotSourceSigmas()'

    I <- dim(mcmc.array)[1]*dim(mcmc.array)[2]

    ## NB: numbering of sources:
    ## c('DHS', 'MICS', 'NS', 'Other', 'RN', 'PMA', 'SS')
    ##               'RN' deprecated ---^

    psigma1.si <- matrix(NA,vc.dim, I)
    psigma2.si <- matrix(NA,vc.dim, I)

    for (s in 1:vc.dim){
      parname <- paste0("nonsample.se.trad.s[", s, "]")
    psigma1.si[s,] <- mcmc.array[,,parname]
    parname <- paste0("nonsample.se.modern.s[", s, "]")
    psigma2.si[s,] <- mcmc.array[,,parname]
  }

    ## Copied from 'PlotSourceSigmas()' --------->8--|

    ## -------* Sampling errors: Design-based + non-sampling

    data.raw <- mcmc.meta$data.raw$data
    winbugs.data <- mcmc.meta$winbugs.data

    ## -------** Create dataframe to store output

    sigma1.survey.tot <-
        data.frame(survey.code = paste0(data.raw$source.j, 1:length(data.raw$source.j))
                   ,source.type = data.raw$source.j
                  ,matrix(NA, nrow = winbugs.data$J, ncol = 3
                         ,dimnames = list(NULL, c("2.5%", "50%", "97.5%")))
                   ,check.names = FALSE, stringsAsFactors = FALSE
                   )
    sigma2.survey.tot <- sigma1.survey.tot

    sigma1.source.type.tot <-
        data.frame(source.type = sourcenames
                  ,matrix(NA, nrow = vc.dim, ncol = 3
                         ,dimnames = list(NULL, c("2.5%", "50%", "97.5%"))
                          )
                   ,check.names = FALSE, stringsAsFactors = FALSE
                   )
    sigma2.source.type.tot <- sigma1.source.type.tot

    ## ------- ** Create quantiles

    for(s in 1:vc.dim) {

        ## Index into data.raw for this 's'
        source.idx <- data.raw$source.j == sourcenames[s]

        if(sum(source.idx) > 0) {

        ## ------- *** Trad

        ## Matrix with sampling errors for this 's', repeated, conformable with mcmc.array.
        sigma.tot.temp <-
            matrix(winbugs.data$se.logR.trad.impute[source.idx]
                   ,nrow = sum(source.idx)
                  ,ncol = dim(mcmc.array)[1] * dim(mcmc.array)[2]
                  ,byrow = FALSE)

        ## Add MCMC sample from posterior of nonsampling errors
        sigma.tot.temp <-
            sqrt(sigma.tot.temp^2 +
                 matrix(psigma1.si[s,], nrow = nrow(sigma.tot.temp)
                                    ,ncol = ncol(psigma1.si)
                                    ,byrow = TRUE
                                     )^2)

        sigma1.survey.tot[source.idx, -(1:2)] <-
            t(apply(sigma.tot.temp, 1, "quantile", percentiles))

        sigma1.source.type.tot[s, -1] <-
            quantile(as.numeric(sigma.tot.temp), percentiles)

        ## -------*** Modern

        ## Matrix with sampling errors for this 's', repeated, conformable with mcmc.array.
        sigma.tot.temp <-
            matrix(winbugs.data$se.logR.modern.impute[source.idx]
                   ,nrow = sum(source.idx)
                  ,ncol = dim(mcmc.array)[1] * dim(mcmc.array)[2]
                  ,byrow = FALSE)

        ## Add MCMC sample from posterior of nonsampling errors
        sigma.tot.temp <-
            sqrt(sigma.tot.temp^2 +
                 matrix(psigma2.si[s,], nrow = nrow(sigma.tot.temp)
                                    ,ncol = ncol(psigma2.si)
                                    ,byrow = TRUE
                                     )^2)

        sigma2.survey.tot[source.idx, -(1:2)] <-
            t(apply(sigma.tot.temp, 1, "quantile", percentiles))

        sigma2.source.type.tot[s, -1] <-
            quantile(as.numeric(sigma.tot.temp), percentiles)

        }
    }

    ## -------* Plot

    if(is.null(ylim)) ylim  <- c(0, max(sigma1.source.type.tot[,-1], sigma2.source.type.tot[,-1], na.rm = TRUE)*1.5)

    plot.main <- "Sampling + non-sampling errors"

    if(!UWRA || hide.CP.tot.lt.1pc) {
        keep <- sigma1.source.type.tot$source.type != "CP(Tot)<1%"
        sigma1.source.type.tot <- sigma1.source.type.tot[keep,]
        keep <- sigma2.source.type.tot$source.type != "CP(Tot)<1%"
        sigma2.source.type.tot <- sigma2.source.type.tot[keep,]
        vc.dim <- 5
        sourcenames <- sourcenames[!sourcenames == "CP(Tot)<1%"]
    }

  plot(sigma1.source.type.tot[,-1][, 2] ~ seq(0.9, vc.dim - 0.1),
       ylim = ylim,
       xlim = c(0.5, vc.dim + 0.5),
       ylab = "sigma",pch = 19, lwd =8,
       xlab = "", xaxt = "n", col = col_pxmr[2]
       ,main = plot.main, cex.main = 0.5)
  axis(1, las = 3, at = seq(1, vc.dim)
     , labels = sourcenames)
  segments(seq(0.9, vc.dim - 0.1), sigma1.source.type.tot[,-1][,1], seq(0.9, vc.dim - 0.1)
         , sigma1.source.type.tot[,-1][,3], col = col_pxmr[2], lwd = 3)
  points(sigma2.source.type.tot[,-1][,2]~seq(1.1, vc.dim + 0.1)
       , pch = 19 ,lwd =8, col = col_pxmr[3])
  segments(seq(1.1, vc.dim + 0.1)
         , sigma2.source.type.tot[,-1][,1], seq(1.1, vc.dim + 0.1)
         , sigma2.source.type.tot[,-1][,3],lwd =3, col = col_pxmr[3])
  legend("topleft", legend = c("Traditional", "Modern"), col = col_pxmr[c(2,3)],
         pch = 19, lwd =3, cex = 1.2)

}

###-------------------------------------------------------------------------------
PlotDataModelSEsUnmet <- function(run.name, output.dir, mcmc.array,
                            percentiles = c(0.025, 0.5, 0.975)
                             ,ylim = NULL
                            ,return.res = FALSE)  {

    ## -------* Constants

    vc.dim <- 2
    sourcenames <- c("DHS", "Other")
    I <- dim(mcmc.array)[1]*dim(mcmc.array)[2]

    load(file = file.path(output.dir, "mcmc.meta.rda"))

    ## -------* Non-sampling errors

    psigma1.si <- matrix(NA,vc.dim, I)
    psigma2.si <- matrix(NA,vc.dim, I)

    for (s in 1:vc.dim) {
        parname <- paste0("nonsample.se.unmet.s[", s, "]")
        psigma1.si[s,] <- mcmc.array[,,parname]
    }

    ## -------* Sampling errors: Design-based + non-sampling

    data.raw <- mcmc.meta$data.raw$data
    winbugs.data <- mcmc.meta$winbugs.data

    ## -------** Create dataframe to store output

    sigma1.survey.tot <-
        data.frame(survey.code = paste0(data.raw$source.unmet.j, 1:length(data.raw$source.unmet.j))
                  ,source.type = data.raw$source.unmet.j
                  ,matrix(NA, nrow = winbugs.data$J, ncol = 3
                         ,dimnames = list(NULL, c("2.5%", "50%", "97.5%")))
                  ,check.names = FALSE, stringsAsFactors = FALSE
                   )

    sigma1.source.type.tot <-
        data.frame(source.type = sourcenames
                  ,matrix(NA, nrow = vc.dim, ncol = 3
                         ,dimnames = list(NULL, c("2.5%", "50%", "97.5%"))
                          )
                  ,check.names = FALSE, stringsAsFactors = FALSE
                   )

    ## ------- ** Create quantiles

    for(s in 1:vc.dim) {

        ## Index into data.raw for this 's'
        source.idx <- data.raw$source.unmet.j == sourcenames[s]

        if(sum(source.idx) > 0) {

            ## ------- *** Trad

            ## Matrix with sampling errors for this 's', repeated, conformable with mcmc.array.
            sigma.tot.temp <-
                matrix(winbugs.data$se.logR.unmet.impute[source.idx]
                      ,nrow = sum(source.idx)
                      ,ncol = dim(mcmc.array)[1] * dim(mcmc.array)[2]
                      ,byrow = FALSE)

            ## Add MCMC sample from posterior of nonsampling errors
            sigma.tot.temp <-
                sqrt(sigma.tot.temp^2 +
                     matrix(psigma1.si[s,], nrow = nrow(sigma.tot.temp)
                           ,ncol = ncol(psigma1.si)
                           ,byrow = TRUE
                            )^2)

            sigma1.survey.tot[source.idx, -(1:2)] <-
                t(apply(sigma.tot.temp, 1, "quantile", percentiles))

            sigma1.source.type.tot[s, -1] <-
                quantile(as.numeric(sigma.tot.temp), percentiles)

        }
    }

    ## -------* Plot

    if(is.null(ylim)) ylim  <- c(0, max(sigma1.source.type.tot[,-1], na.rm = TRUE)*1.5)

    plot.main <- "Sampling + non-sampling errors"

  plot(sigma1.source.type.tot[,-1][, 2] ~ seq(0.9, vc.dim - 0.1),
       ylim = ylim,
       xlim = c(0.5, vc.dim + 0.5),
       ylab = "sigma",pch = 19, lwd =8,
       xlab = "", xaxt = "n", col = 1,
       main = plot.main, cex.main = 0.5)
  axis(1, las = 3, at = seq(1, vc.dim)
     , labels = sourcenames)
  segments(seq(0.9, vc.dim - 0.1), sigma1.source.type.tot[,-1][,1], seq(0.9, vc.dim - 0.1)
         , sigma1.source.type.tot[,-1][,3], col = 1, lwd = 3)
  legend("topright", legend = c("Unmet"), col = 1, pch = 19, lwd =3, cex = 1.5)
}

###-------------------------------------------------------------------------------
PlotBiases <- function(# Plot posteriors of bias parameters, and output the CIs
  ### Plot posteriors of bias parameters
  mcmc.array, percentiles = c(0.025,0.5, 0.975),
  return.res = FALSE){
  biases.i3 <- NULL
  ## 'v.abs.probe.q' used to be called 'v.abs.probe.q' so allow for this
  if("v.abs.probe.q" %in% dimnames(mcmc.array)[[3]]) {
      parnames.biases <- c("v.mneg", "v.mpos", "v.folk", "v.abs.probe.q")
  } else {
      parnames.biases <- c("v.mneg", "v.mpos", "v.folk", "v.abs.probe.q")
  }
  parnames.biases.nice <- c("Sterilization excluded", "Sterilization included",
                            "Folk methods included", "Absence of probing questions")
  parnames.biases <- parnames.biases[parnames.biases %in% dimnames(mcmc.array)[[3]]]
  parnames.biases.nice <-
      parnames.biases.nice[parnames.biases %in% dimnames(mcmc.array)[[3]]]
  for (parname in (parnames.biases)){
    biases.i3 <- rbind(biases.i3, quantile(mcmc.array[,,parname],percentiles))
  }
  rownames(biases.i3) <- parnames.biases.nice
  nbiases <- dim(biases.i3)[1]
  add <- 0
  par(mar = c(5,12.5,1,1))
  plot(seq(1-add, nbiases) ~ biases.i3[,2], yaxt = "n", ylab = "",
       xlab = "Bias (proportion misclassified)", ylim = c(nbiases+1, 0),
       xlim = c(min(biases.i3), max(biases.i3)), type = "p",  pch = 20)
  axis(2, at = seq(1, nbiases), labels = parnames.biases.nice, las = 2, cex = 0.5)
  segments(biases.i3[,1], seq(1-add, nbiases), biases.i3[,3], seq(1-add, nbiases))
  abline(v = 0, col = 2)
  if (return.res) return(biases.i3)
}

###-------------------------------------------------------------------------------
PlotGeoEtc <- function(# Plot posteriors of perturbation multipliers
  ### Plot posteriors of perturbation multipliers
  par.V,
  mcmc.array,
  percentiles = c(0.025,0.5, 0.975)
){
  if (is.null(par.V$parnames.V.in.bugs)) { # change JR, 20131105
    return(invisible())
  }
  dummies.i3 <- NULL
  for (parname in par.V$parnames.V.in.bugs){
    dummies.i3 <- rbind(dummies.i3, quantile(mcmc.array[,,parname], percentiles))
  }
  ndummies.tot <- dim(dummies.i3)[1]
  add <- 0
  # max 50 dummies in each plot
  nplots <- ceiling(ndummies.tot/100)
  ndummiesperplot <- ceiling(ndummies.tot/nplots)
  # make one big plot...
  nf <- layout(t(seq(1, nplots)),
    widths = rep(8, nplots), heights = 15)
  #layout.show(nf)
  par(mar = c(2,5,2,1))
  for (j in 1:nplots){
    ndummies <- min( ndummiesperplot, ndummies.tot - (j-1)* ndummiesperplot)
    seq.select <- (j-1)* ndummiesperplot + seq(1, ndummies)

    plot(seq(1-add, ndummies)~ dummies.i3[seq.select,2], yaxt = "n", ylab = "",
         xlab = "Ratio multiplier (# obs)", cex.axis = 0.8,
         xlim = c(0,  3), #max(dummies.i3)),
         type = "n",
    #, #min(dummies.i3),
         # swap y-axis
         ylim = c(ndummies-1, 1.5), pch = 20)
    axis(3, at = seq(0,3,0.5), cex.axis = 0.8)
    axis(2, at = seq(1, ndummies),
         labels = par.V$parnames.V.nice[seq.select],
         las = 2, cex = 0.001, cex.lab = 0.1, cex.axis = 0.4)
    abline(v = seq(1, ndummies), col = "grey")
    abline(v = seq(0,3,0.5), col = "grey", lwd = 2)
    abline(v = 1, col = 1, lwd= 2)
    segments(dummies.i3[seq.select,3], seq(1-add, ndummies),
             dummies.i3[seq.select,1], seq(1-add, ndummies),
             lwd = 1, col = 2)
    points(seq(1-add, ndummies)~ dummies.i3[seq.select,2], col = 2, pch = 20, lwd = 2)
  }
}
###-------------------------------------------------------------------------------------------
SummarizeBiases <- function( # Document number of bias parameters
  ### Documentation function to summarize how often biases were included
  winbugs.data ##<< Object of class \code{\link{winbugs.data}}
){
  ##value<<
  res <- list(nbias.folk = sum(winbugs.data$folk.ind1.j), ##<< folk
              nbias.MICS = sum(winbugs.data$abs.probe.q.ind1.j), ##<< MICS
              nbias.modernneg = sum(winbugs.data$mneg.ind1.j), ##<< Modern[-]
              nbias.modernpos = sum(winbugs.data$mpos.ind1.j) ##<< Modern [+]
  )
  print(paste(names(res), res))
  return(res)
}
###--------------------------------------------------------------------------------------------
SummarizeMultipliers <- function(#Summarize multipliers
  ### Documentation function to summarize how many pertubation parameters were included
  winbugs.data,##<< Object of class winbugs.data
  data ##<< Object of class data
  ){
  multpliers.ncategories.inclbaseline <-
    winbugs.data[c("ncat.age", "ncat.geo", "ncat.posbias",
                   "ncat.sa", "ncat.emal", "ncat.hw",
                   "ncat.posage", "ncat.negage")]  #data.frame(par.V$parnames.V.in.bugs,  par.V$parnames.V.nice)
  multpliers.ncategories.exclbaseline <-lapply(multpliers.ncategories.inclbaseline,"-", 1)
  print(paste(names(multpliers.ncategories.exclbaseline), multpliers.ncategories.exclbaseline))
  ##value<< List with number of multipliers used in each pertubation group
  return(multpliers.ncategories.exclbaseline)
}
###--------------------------------------------------------------------


### The End!
