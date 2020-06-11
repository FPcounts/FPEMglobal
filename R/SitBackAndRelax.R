SitBackAndRelax <- function(# Function to fit the Contraceptive Use Model in one go!
  ### Function to fit the Contraceptive Use Model
  ### by (1) running MCMC chains in parallel, (2) constructing an MCMC array and checking diagnistics,
  ### (3) constructing R objects with results, (4) making plots and tables.
  ### Note that all default settings of main functions are used,
  ### the user can only specify the arguments for the data sets used.
  run.name = "test", ##<< Run name, used to create a directory \code{output/run.name}
  ## in \code{working.dir} (defined next),
  ## with JAGS output (after MCMC sampling), and estimates (in next steps).
  working.dir = getwd(), ##<< User-specified directory where output,
  ## figures and tables will be stored. Defaults to current working directory.
  ## The \code{ContraceptiveUse} package needs to be stored here as well
  ## (and will be installed during function call if needed).
  data.csv = NULL, ##<< If \code{NULL}, data set included in package is used.
  ## To use alternative data set, use \code{data.csv = .../dataCPmodel.csv}, where ... refers to the file path where file is located.
  regioninfo.csv = NULL, ##<< If \code{NULL}, region info included in package is used.
  ## To use alternative csv file, use \code{regioninfo.csv = .../Country-and-area-classification.csv}, where ... refers to the file path where file is located.
  WRA.csv = NULL,##<< Directory where CPmodel is stored, defaults to dataprovided with pakcage
  test = FALSE, ##<< If TRUE, a run with 2 chains only is done
  add.chain = NULL, ## If not NULL, a chain ID number such that that additional chain is added and all results are recomputed.
  seed.MCMC = 1  ){
  setwd(working.dir)
  if (test) {
    chains <- c(1,2)
  } else {
    chains <- NULL
  }
  if (is.null(add.chain)){
    RunMCMC(run.name = run.name, seed.MCMC = seed.MCMC,
          data.csv = data.csv,
          regioninfo.csv = regioninfo.csv)
  } else {
    AddMCMCChain(run.name = run.name, ChainNums = add.chain)
  }
  ConstructMCMCArray(run.name = run.name)
  CheckConvergence(run.name = run.name)
  ConstructOutput(run.name = run.name, WRA.csv = WRA.csv)
  CheckConvergenceProbs(run.name = run.name)
  PlotResults(run.name = run.name)
  PlotMetDemandinCountryBySubregion(run.name = run.name)
  BarChartSubregion(run.name = run.name)
  CIPropChangesSubregions(run.name = run.name)
  GetAllBarCharts(run.name = run.name)
  GetTablesRes(run.name = run.name, name.res = "UNPDaggregate")
  GetTablesRes(run.name = run.name, name.res = "Country")
  GetTablesChange(run.name = run.name, name.res = "UNPDaggregate")
  GetTablesChange(run.name = run.name, name.res = "Country")
  return()
}
#----------------------------------------------------------------------
# The End!
