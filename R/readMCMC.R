#--------------------------------------------------------------------------
# source_readMCMC.R
# Leontine Alkema, 2011
#--------------------------------------------------------------------------
ConstructMCMCArray <- function(# Read in JAGS objects
  ###  Read in JAGS objects and constructs \code{mcmc.array}, 
  ### which is saved to \code{output.dir}.
  ### This function can only be run after finising the mcmc sampling (after function \code{\link{RunMCMC}} has completed).
  run.name = "test", ##<< Run name
  do.SS.run.first.pass = FALSE, ##<< do first pass of run with SS data? # change JR, 20140414
  n.steps = NULL, ##<< Optional: specify no of steps to read (to use when all steps have not finished yet).
  ChainIDs = NULL, ##<< Optional: specify which chains to include
  ## (to use when you want to exclude a chain that crashed, or which has not finished yet).
  n.samplestot.max = 15000, ##<< Maximum number of posterior samples to save
  output.dir = NULL ##<< Directory where MCMC output was stored and will be stored. 
  ##If NULL, it's in \code{output/run.name/}, the default from \code{runMCMC}.
){
  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name) # change JR, 20140418 
  }
  filename.append <- ifelse(do.SS.run.first.pass, "_pre", "")
  if (!file.exists(file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda")))){
    cat(paste("No MCMC meta in output.dir, run MCMC chains first!\n"))
    return(invisible())
  }
  cat(paste("Reading in MCMC meta from", output.dir), "\n")
  load(file.path(output.dir, paste0("mcmc.meta", filename.append, ".rda"))) # change JR, 20140418
  # now combine the JAGS files into one mcmc.array
  if (is.null(ChainIDs)){
    ChainNums <- mcmc.meta$general$ChainNums
  } else {
    ChainNums <- ChainIDs
  }
  n.chains <- length(ChainNums)
  if (n.chains==1){
    cat("You need at least two chains!\n")
    return()
  }
  if (is.null(n.steps)) n.steps <- mcmc.meta$general$N.STEPS
  n.sample.max <- n.samplestot.max/n.chains
  
  jags.dir <- file.path(output.dir, "temp.JAGSobjects/")
  cat("Reading in JAGS output files from", jags.dir, "\n")
  chain  <- ifelse(length(ChainNums)==1,ChainNums,ChainNums[1])
  load(file.path(jags.dir, paste0("jags_mod", filename.append, chain, "update_1.Rdata"))) # change JR, 201401418
  n.iter.perstep <- dim(mod.upd$BUGSoutput$sims.array)[1]
  n.sim <- n.steps *n.iter.perstep
  n.par <- dim(mod.upd$BUGSoutput$sims.array)[3]
  mcmc.array <- array(NA, c(n.sim, n.chains, n.par))
  dimnames(mcmc.array) <- list(NULL, NULL, names(mod.upd$BUGSoutput$sims.array[1,1,]))
  for (chain in 1:n.chains){
   chain_saved <- ifelse(length(ChainNums)==1,1,ChainNums[chain])
   cat(paste("Reading in chain number ", chain_saved, " (step 1)", sep = ""), "\n")
   load(file.path(output.dir, "temp.JAGSobjects", paste0("jags_mod", filename.append, chain_saved, "update_1.Rdata"))) # change JR, 20140418
   mcmc.array[1:n.iter.perstep,chain, ] = mod.upd$BUGSoutput$sims.array[,1,]
   if (n.steps>1){
     for (step in 2:n.steps){
      cat(paste0("Reading in chain number ", chain_saved, " (step ", step, ")"), "\n")
      load(file.path(output.dir, "temp.JAGSobjects", paste0("jags_mod", filename.append, chain_saved, "update_", step, ".Rdata"))) # change JR, 20140418
      mcmc.array[((step-1)*n.iter.perstep+1):(step*n.iter.perstep),chain, ] = mod.upd$BUGSoutput$sims.array[,1,]
      }
    }
  }
  if (n.sim > n.sample.max){
    mcmc.array <- mcmc.array[seq(1, n.sample.max, length.out = n.sample.max), , ]  
  }
  save(mcmc.array, file = file.path(output.dir, paste0("mcmc.array", filename.append, ".rda"))) # change JR, 20140418
  cat("mcmc.array saved to", output.dir, "\n")
  ##value<< NULL; adds mcmc.array to \code{output.dir}.
  return(invisible())
}
#----------------------------------------------------------------------   
# The End!
