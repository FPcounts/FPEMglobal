#---------------------------------------------------------------------------------------
# F_plot_post
# LA, Jan 2012
#---------------------------------------------------------------------------------------
PlotPostOnly <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample
  post.samp, parname = NULL##<<used for main
  ){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
}

PlotPostWithNormalPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priormean, priorsd, parname = NULL##<< used for main
  ){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  curve(dnorm(x, mean = priormean, sd = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithNormalPrior(post.samp = rnorm(100,0,1), priormean = -1, priorsd = 10, parname = "bla")

PlotPostWithLogNormalPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priormean, priorsd, parname = NULL##<< used for main
){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  curve(dlnorm(x, meanlog = priormean, sdlog = priorsd), col = 2, lwd = 3, add = TRUE)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithLogNormalPrior(post.samp = rlnorm(100,0,1), priormean = 0, priorsd = 1, parname = "bla")

PlotPostWithTruncatedNormalPrior <- function(# Plot histogram of posterior sample.
  ### Plot histogram of posterior sample and add prior.
  post.samp, ##<< Posterior sample from MCMC.
  priormean, ##<< Prior mean of normal distribution.
  priorsd, ##<< Prior standard deviation of normal distribution.
  priorlower = -Inf, ##<< Lower bound of truncated normal distribution
  priorupper = Inf, ##<< Upper bound of truncated normal distribution
  parname = NULL, ##<< Parameter name for x-axis label.
  title = "" ##<< Plot title.
) {
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = as.character(title), xlim = c(minx, maxx))
  lines(density(rtnorm(1000, priormean, priorsd, lower = priorlower, upper = priorupper)), col = 2, lwd = 3)
  abline(v = priormean, col = 2, lty = 2)
}
#PlotPostWithTruncatedNormalPrior(post.samp = rtnorm(100,0,1,upper=1), priormean = -1, priorsd = 10, priorupper = 0.5, parname = "test")


PlotPostWithUnifPrior <- function(# Plot histogram of posterior sample
  ### Plot histogram of posterior sample and add prior
  post.samp, priorlow, priorup, parname = NULL##<< used for main
  ){
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "",xlim = c(minx, maxx))
  h <- 1/(priorup-priorlow)
  segments(priorlow, h, priorup, h, col = 2)
}
#PlotPostWithUnifPrior(post.samp = rnorm(100,0), priorlow = -2, priorup = 10, parname = "bla")

PlotPostSDWithGammaPrior <- function(# Plot histogram of posterior sample of a SD parameter
  ### Plot histogram of posterior sample of a SD parameter, and add the prior, which is based on a Gamma for the precision
  post.samp, priorshape, priorrate, parname = NULL){
  # more precisely, variance  ~ InvGamma
  # often used: shape = halfnu0, rate = halfnu0*sigma2
  par(mar = c(5,5,1,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5, lwd = 3)
  minx <- ifelse(min(post.samp)<0, 1.1*min(post.samp), 0.9*min(post.samp))
  maxx <- ifelse(max(post.samp)<0, 0.9*max(post.samp), 1.1*max(post.samp))
  minxprior <- ifelse(min(post.samp)<0, 1.2*min(post.samp), 0.8*min(post.samp))
  maxxprior <- ifelse(max(post.samp)<0, 0.8*max(post.samp), 1.2*max(post.samp))
  hist(post.samp, xlab = parname, col = "grey", freq = FALSE, main = "", xlim = c(minx, maxx))
  # note: when using "density", use to and from if sample has large negative/positive values!
  lines(density(1/sqrt(rgamma(1000, shape = priorshape, rate = priorrate)), from = minx, to = maxx), col = 2, lwd = 3)
}
#PlotPostSDWithGammaPrior(post.samp = runif(100,0.01,1), priorshape = 0.5, priorrate = 0.5, parname = "bla")

#----------------------------------------------------------------------
# The End!
