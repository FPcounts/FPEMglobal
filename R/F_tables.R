#----------------------------------------------------------------------
# F_tables.R
#----------------------------------------------------------------------
GetPPCSymbols <- function(PPPC){# Gives vector with symbols for PPPC
  ### Gives vector witb symbols for PPPC:
  ### Probability that the 1990-2010 change was positive: *>0.9, **>0.95, ***>0.99.
  ### Probability that the 1990-2010 change was negative: '>0.9, ''>0.95, '''>0.99.
  #PPPC <- seq(0,1,0.01)
  PPcats <- ifelse(PPPC <0.01, "\'\'\'",
                   ifelse(PPPC <0.05, "\'\'",
                          ifelse(PPPC <0.1, "\'",
                                 ifelse(PPPC >0.99, "***",
                                        ifelse(PPPC >0.95, "**",
                                               ifelse(PPPC >0.9, "*",""))))))
  return(PPcats)
}

#--------------------------------------------------------------------------------
GetSummaryCountries<- function (# Save a csv with summary info about countries
  ### Save a csv with summary info about countries
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  table.dir = NULL ##<< Directory to store tables.
  ## If NULL, folder "tables" in current working directory.
  ){

  if (is.null(table.dir)){
    table.dir <- file.path(getwd(), "tables/")
    dir.create(table.dir, showWarnings = FALSE)
  }
  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name, "/")
  }
  load(file.path(output.dir, "mcmc.meta.rda"))
  load(file.path(output.dir, "par.ciq.rda"))

  country.info <- mcmc.meta$data.raw$country.info
  results <- data.frame(paste(mcmc.meta$data.raw$country.info$iso.c),
                        paste(mcmc.meta$data.raw$country.info$code.c),
                        mcmc.meta$data.raw$country.info$N.c,  round(par.ciq[,,2],2))
  rownames(results) <- country.info$name.c
  ##details<< File \code{results.summ.csv} is written to \code{table.dir}, with
  ## country name, UN code, iso code, no of observations, and median estimates of the logistic parameters.
  colnames(results) <- c("num. code", "letter code", "#OBS", paste(colnames(par.ciq[,,2])))
  write.csv(results, file = file.path(table.dir, paste0(run.name, "results.summ.csv"))) # change JR, 20140418
  ##value<<NULL
  return(invisible())
}

##-------------------------------------------------------------------------------------
GetTablesRes <- function(# Save csv's with CIs for proportions and counts
  ### Save csv's with CIs for proportions and counts for countries or aggregates.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  table.dir = NULL, ##<< Directory to store tables.
  ## If NULL, folder "tables" in current working directory.
  res = NULL, ##<< If NULL, country/UNPD aggregates are summarized, specified with next argument.
  ## Alternatively, an object of class \code{\link{Results}} with \code{CIprop.Lg.Lcat.qt} and \code{CIcount.Lg.Lcat.qt}.
  name.res = "Country", ##<<Name used in csv file name AND to determine whether to
  ## save CIs for countries (Country) or UNPD aggregates (UNPDaggregate) if \code{res = NULL}.
  include.posterior.means = FALSE,
  adjusted.medians = FALSE,
  adj.method = NULL,
  fp2020.69.only = FALSE
  ){

    ## Means not implemented for adjusted medians
    if(adjusted.medians && include.posterior.means) {
        include.posterior.means = FALSE
        warning("'adjusted.medians' is TRUE. 'include.posterior.means' set to 'FALSE'.")
        }

  if (is.null(table.dir)) {
    table.dir <- file.path(getwd(), "tables/")
  }

  if (!dir.exists(table.dir)) {
    dir.create(table.dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(output.dir)) {
    output.dir <- file.path(getwd(), "output", run.name, "/")
  }

    if (is.null(res)) {
        if (name.res == "Country") {
            load(file.path(output.dir, "res.country.rda")) # change JR, 20140418
            res <- res.country
        }
        if (name.res == "UNPDaggregate") {
            ## [MCW-2018-02-14]
            ## Don't do anything if this is a one country run.
            load(file = file.path(output.dir,"mcmc.meta.rda"))
            if(mcmc.meta$general$do.country.specific.run) {
                warning("'name.res' is '\"UNPDaggregate\"' but this is a one country run: no tables produced.")
                return(invisible())
            } else {
                load(file.path(output.dir, "res.aggregate.rda")) # change JR, 20140418
                res <- res.aggregate
            }
        }
    }

    ## Replace 'median.adj' with 'qt' in names of res so that the rest of the
    ## code works as-is.
    ##
    ## _Note_:    The .csv files will have 'adj' in the name because this is
    ## taken from variable names in the data frames which are unchanged. It is
    ## up to the user to specify whether or not the .csv files are saved to a
    ## different folder via the 'table.dir' argument.
    if(adjusted.medians) names(res) <- gsub("median\\.adj", "qt", names(res))

  est.years <- as.numeric(dimnames(res$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]])
  percentiles <- as.numeric(dimnames(res$CIprop.Lg.Lcat.qt[[1]][[1]])[[1]])
  nperc <- length(percentiles)
    G <- length(res$CIprop.Lg.Lcat.qt)
    for (j in 1:3) {
        CI.Lg.Lcat.qt <- res[[c("CIprop.Lg.Lcat.qt", "CIcount.Lg.Lcat.qt",
                                "CIratio.Lg.Lcat.qt")[j]]]
    for (i in 1:length(CI.Lg.Lcat.qt[[1]])) {
        CI.Lg.qt <- lapply(CI.Lg.Lcat.qt, function(l) l[[i]])
      estimates <- matrix(NA, nperc * G, length(est.years))
        for (g in 1:G) {
        indices <- seq((g - 1) * nperc + 1, g * nperc)
        estimates[indices, ] <- CI.Lg.qt[[g]]
        }

      ## [MCW-2016-09-06-1] :: If 'name.res' == "UNPDaggregate" don't include
      ## the iso codes in the 'results.all' data frame.
      if(name.res == "Country") {
          iso.col <- paste(rep(res$iso.g, each = nperc))

      } else {
          iso.col <- NA             #keep it because renaming, etc. below expects this column.
      }
      results.all <- data.frame(paste(rep(names(CI.Lg.qt),
                                          each = nperc)), iso.col,
                                paste(rep(percentiles, times = G)), estimates)

      if(include.posterior.means) {
      ## [MCW-2016-07-12-5] :: Add means to output
          ## [MCW-2016-09-06-2] :: If 'name.res' == "UNPDaggregate" don't
          ## include the iso codes in the 'results.all' data frame.
      if(name.res == "Country") {
          iso.col.means <- res$iso.g
      } else {
          iso.col.means <- NA        #keep it because renaming, etc. below expects this column.
      }
      if(j == 1) {                      # 'Prop' outputs
          mean.Lg.Lcat <- res[["meanProp.Lg.Lcat"]]
          means.i <- lapply(mean.Lg.Lcat, "[[", i)
          means.all <- data.frame(names(mean.Lg.Lcat), iso.col.means
                                 ,rep("mean", times = G)
                                  ,t(as.data.frame(means.i))
                                  )
          colnames(means.all) <- colnames(results.all)
          results.all <- rbind(results.all, means.all)
      } else if(j == 2 && name.res != "UNPDaggregate") { # 'Star' outputs (not for aggregates)
          mean.Lg.Lcat <- res[["meanStar.Lg.Lcat"]]
          means.i <- lapply(mean.Lg.Lcat, "[[", i)
          means.all <- data.frame(names(mean.Lg.Lcat), iso.col.means
                                 ,rep("mean", times = G)
                                  ,t(as.data.frame(means.i))
                                  )
          colnames(means.all) <- colnames(results.all)
          results.all <- rbind(results.all, means.all)
      } else if(j == 3) {               # 'Ratio' outputs
          mean.Lg.Lcat <- res[["meanRatio.Lg.Lcat"]]
          means.i <- lapply(mean.Lg.Lcat, "[[", i)
          means.all <- data.frame(names(mean.Lg.Lcat), iso.col.means
                                 ,rep("mean", times = G)
                                  ,t(as.data.frame(means.i))
                                  )
          colnames(means.all) <- colnames(results.all)
          results.all <- rbind(results.all, means.all)
      }
      }
        dimnames(results.all)[[2]] <- c("Name", "Iso", "Percentile",
                                      est.years)

      if(fp2020.69.only) {
          results.all <- results.all[results.all$Iso %in% get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD"), ]
          fp2020 <- "_fp2020_69"
          } else fp2020 <- ""

      if(adjusted.medians && !is.null(adj.method) && adj.method != "mod_tot_unmet") adj.m <- paste0("_", adj.method)
      else adj.m <- ""

      ## Need to keep names from getting too long!
      fnm <- gsub("\\.", "_"
                 ,paste(c(unlist(strsplit(names(CI.Lg.Lcat.qt[[1]])[i]
                                         ,split = "/")))
                       ,collapse = "Over"))
      fnm <- gsub("Met Demand with Modern Methods", "MetDemModMeth"
                 ,fnm, fixed = TRUE)

        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

      write.csv(results.all
               ,file = file.path(table.dir
                                ,paste0(run.name,
                                        "_", name.res, "_", c("perc", "count", "ratio")[j],
                                        "_", fnm, fp2020,
                                        adj.m,
                                       ".csv")), # change JR, 20140418
                row.names = FALSE)
    }
        }

        ## ------------------------------------------------------------
    ## Met demand greater than

    if(!is.null(res[["metDemGT.Lg.Lcat.pr"]])) {

        metDemGT.Lg.Lcat.pr <- res[["metDemGT.Lg.Lcat.pr"]]
        estimates <- t(sapply(metDemGT.Lg.Lcat.pr, function(l) l[[1]]))
        if(name.res == "Country") {
          iso.col.GT <- res$iso.g
      } else {
          iso.col.GT <- NA        #keep it because renaming, etc. below expects this column.
      }

        results.all <-
            data.frame(rownames(estimates), iso.col.GT, estimates, row.names = NULL
                       ,stringsAsFactors = FALSE)

        dimnames(results.all)[[2]] <- c("Name", "Iso", est.years)

      if(fp2020.69.only) {
          results.all <-
              results.all[results.all$Iso %in% get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD"), ]
          fp2020 <- "_fp2020_69"
      } else fp2020 <- ""

      write.csv(results.all
               ,file = file.path(table.dir
                                ,paste0(run.name,
                                        "_", name.res, "_", "metDemGT",
                                        "_", "modMeth75pc", fp2020,
                                        adj.m,
                                       ".csv")),
                row.names = FALSE)
        }

  cat("Result tables written to", table.dir, "\n")
  return(invisible())
}
##----------------------------------------------------------------------------------
## [MCW-2016-11-21-5] :: Created.
GetTablesResAllWomen <-
    function(# Save csv's with CIs for proportions and counts
  ### Save csv's with CIs for proportions and counts for countries or aggregates.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  table.dir = NULL, ##<< Directory to store tables.
  ## If NULL, folder "tables" in current working directory.
  res = NULL, ##<< If NULL, country/UNPD aggregates are summarized, specified with next argument.
  ## Alternatively, an object of class \code{\link{Results}} with \code{CIprop.Lg.Lcat.qt} and \code{CIcount.Lg.Lcat.qt}.
  name.res = "Country", ##<<Name used in csv file name AND to determine whether to
  ## save CIs for countries (Country) or UNPD aggregates (UNPDaggregate) if \code{res = NULL}.
  fp2020.69.only = FALSE,
  adjusted.medians = FALSE,
  adj.method = NULL,
  all.womenize.table.name = TRUE
  ){
  if (is.null(table.dir)) {
    table.dir <- file.path(getwd(), "tables/")
    dir.create(table.dir, showWarnings = FALSE)
  }
  if (is.null(output.dir)) {
    output.dir <- file.path(getwd(), "output", run.name,
                            "/")
  }
  if (is.null(res)) {
    if (name.res == "Country") {
      load(file.path(output.dir, "res.country.all.women.rda")) # change JR, 20140418
      res <- res.country.all.women
    }
    if (name.res == "UNPDaggregate") {
    ## [MCW-2018-02-14]
    ## Don't do anything if this is a one country run.
    load(file = file.path(output.dir,"mcmc.meta.rda"))
    if(mcmc.meta$general$do.country.specific.run) {
        warning("'name.res' is '\"UNPDaggregate\"' but this is a one country run: no tables produced.")
        return(invisible())
    } else {
      load(file.path(output.dir, "res.aggregate.all.women.rda")) # change JR, 20140418
      res <- res.aggregate.all.women
    }
    }
  }

    ## Replace 'median.adj' with 'qt' in names of res so that the rest of the
    ## code works as-is.
    ##
    ## _Note_:    The .csv files will have 'adj' in the name because this is
    ## taken from variable names in the data frames which are unchanged. It is
    ## up to the user to specify whether or not the .csv files are saved to a
    ## different folder via the 'table.dir' argument.
  if(adjusted.medians) names(res) <- gsub("median\\.adj", "qt", names(res))

  est.years <- as.numeric(dimnames(res$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]])
  percentiles <- dimnames(res$CIprop.Lg.Lcat.qt[[1]][[1]])[[1]]
  nperc <- length(percentiles)
  G <- length(res$CIprop.Lg.Lcat.qt)
  for (j in 1:3) {
    CI.Lg.Lcat.qt <- res[[c("CIprop.Lg.Lcat.qt", "CIcount.Lg.Lcat.qt",
                            "CIratio.Lg.Lcat.qt"
                            )[j]]]

      for (i in 1:length(CI.Lg.Lcat.qt[[1]])) {
      CI.Lg.qt <- lapply(CI.Lg.Lcat.qt, function(l) l[[i]])
      estimates <- matrix(NA, nperc * G, length(est.years))
      for (g in 1:G) {
        indices <- seq((g - 1) * nperc + 1, g * nperc)
        estimates[indices, ] <- CI.Lg.qt[[g]]
      }
      ## [MCW-2016-09-06-1] :: If 'name.res' == "UNPDaggregate" don't include
      ## the iso codes in the 'results.all' data frame.
      if(name.res == "Country") {
          iso.col <- paste(rep(res$iso.g, each = nperc))

      } else  {
          iso.col <- NA             #keep it because renaming, etc. below expects this column.
      }
      results.all <- data.frame(paste(rep(names(CI.Lg.qt),
                                          each = nperc)), iso.col,
                                paste(rep(percentiles, times = G)), estimates)

      dimnames(results.all)[[2]] <- c("Name", "Iso", "Percentile",
                                      est.years)

      if(fp2020.69.only) {
          results.all <- results.all[results.all$Iso %in% get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD"), ]
          fp2020 <- "_fp2020_69"
          } else fp2020 <- ""

      if(adjusted.medians && !is.null(adj.method) && adj.method != "mod_tot_unmet") adj.m <- paste0("_", adj.method)
      else adj.m <- ""

      ## [MCW-2017-08-14-19] :: Ensure ~.csv~ filenames have "aw" in them, even
      ## if ~run.name~ does not have "umw" or "mw" in it.
      if(all.womenize.table.name) {
          aw.run.name <- makeAWFileName(run.name)
          } else aw.run.name <- run.name

      ## Remove '\' from filenames
            fnm <- gsub("\\.", "_"
                 ,paste(c(unlist(strsplit(names(CI.Lg.Lcat.qt[[1]])[i]
                                         ,split = "/")))
                       ,collapse = "Over"))
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

        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

      write.csv(results.all
               ,file = file.path(table.dir
                                ,paste0(aw.run.name,
                                        "_", name.res, "_", c("perc", "count", "ratio")[j],
                                        "_", fnm, fp2020
                                       ,".csv")), # change JR, 20140418
                row.names = FALSE)
    }
  }

if("metDemGT.Lg.Lcat.pr" %in% names(res)) {
        ## ------------------------------------------------------------
        ## Met demand greater than
        metDemGT.Lg.Lcat.pr <- res[["metDemGT.Lg.Lcat.pr"]]
    metDemGT.Lg.pr <- lapply(metDemGT.Lg.Lcat.pr, function(l) l[[1]])
    names(metDemGT.Lg.pr) <- iconv(names(metDemGT.Lg.pr), "ASCII", "UTF-8", sub="")
        estimates <-t(as.data.frame(metDemGT.Lg.pr))
        if(name.res == "Country") {
          iso.col.GT <- res$iso.g
      } else {
          iso.col.GT <- NA        #keep it because renaming, etc. below expects this column.
      }
        results.all <-
            data.frame(names(metDemGT.Lg.pr), iso.col.GT, estimates, row.names = NULL)
        dimnames(results.all)[[2]] <- c("Name", "Iso", est.years)

      if(fp2020.69.only) {
          results.all <- results.all[results.all$Iso %in% get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD"), ]
          fp2020 <- "_fp2020_69"
      } else fp2020 <- ""

    ## [MCW-2017-08-14-19] :: Ensure ~.csv~ filenames have "aw" in them, even
    ## if ~run.name~ does not have "umw" or "mw" in it.
    if(all.womenize.table.name) {
        aw.run.name <- makeAWFileName(run.name)
    } else aw.run.name <- run.name

      write.csv(results.all
               ,file = file.path(table.dir
                                ,paste0(aw.run.name,
                                        "_", name.res, "_", "metDemGT",
                                        "_", "modMeth75pc", fp2020,
                                       ".csv")),
                row.names = FALSE)
}

    cat("Result tables written to", table.dir, "\n")
  return(invisible())
}
##----------------------------------------------------------------------------------
GetTablesChange <- function(# Save csv's with CIs for change in proportions and counts
  ### Save csv's with CIs for change in proportions and counts
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  table.dir = NULL, ##<< Directory to store tables.
  ## If NULL, folder "tables" in current working directory.
  res = NULL, ##<< If NULL, country/UNPD aggregates are summarized, specified with next argument.
  ## Alternatively, an object of class \code{\link{Results}} with \code{changeprop.Lg.Lcat.qt} and \code{changecount.Lg.Lcat.qt}.
  name.res = "Country" ##<<Name used in csv file name AND to determine whether to
  ## save CIs for countries (Country) or UNPD aggregates (UNPDaggregate) if \code{res = NULL}.
 ,fp2020.69.only = FALSE
  ,change.in.changes = TRUE
  ){

  if (is.null(table.dir)){
    table.dir <- file.path(getwd(), "tables/")
    dir.create(table.dir, showWarnings = FALSE)
  }
  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name) # change JR, 20140418
  }
  if (is.null(res)){
    if (name.res == "Country"){
      load(file.path(output.dir, "res.country.rda"))
      res <- res.country
    }
    if (name.res == "UNPDaggregate"){
    ## [MCW-2018-02-14]
    ## Don't do anything if this is a one country run.
    load(file = file.path(output.dir,"mcmc.meta.rda"))
    if(mcmc.meta$general$do.country.specific.run) {
        warning("'name.res' is '\"UNPDaggregate\"' but this is a one country run: no tables produced.")
        return(invisible())
    } else {
      load(file.path(output.dir, "res.aggregate.rda")) # change JR, 20140418
      res <- res.aggregate
      #print(res)
    }
    }
    }
    change.years.names <- dimnames(res$changeprop.Lg.Lcat.Ti[[1]][[1]])[[1]]
    if(!change.in.changes) {  # [MCW-2018-02-19] :: don't include change of changes
        re.ex <- "Change \\([0-9]{4}-[0-9]{4}\\) - \\([0-9]{4}-[0-9]{4}\\)"
        change.years.names <- change.years.names[!grepl(re.ex, change.years.names)]
        }

  infonames <- dimnames(res$changeprop.Lg.Lcat.Ti[[1]][[1]])[[2]]
  ninfo <- length(infonames)
  for (j in 1:3){
      CI.Lg.Lcat.Ti <- res[[c("changeprop.Lg.Lcat.Ti", "changecount.Lg.Lcat.Ti", "changeratio.Lg.Lcat.Ti")[j]]]
      G <- length(CI.Lg.Lcat.Ti)
      for (i in 1:length(CI.Lg.Lcat.Ti[[1]])){ # not the info i...
      CI.Lg.Ti <- lapply(CI.Lg.Lcat.Ti, function(l) l[[i]])
      estimates <- matrix(NA, length(change.years.names)*G, ninfo)
      for (g in 1:G){
        indices <- seq((g-1)*length(change.years.names) +1, g*length(change.years.names))
        estimates[indices,] <- CI.Lg.Ti[[g]][change.years.names,]
      }
      ## [MCW-2016-09-06-3] :: If 'name.res' == "UNPDaggregate" don't include
      ## the iso codes in the 'results.all' data frame.
      if(name.res == "Country") iso.col <- paste(rep(res$iso.g, each = length(change.years.names)))
      else iso.col <- NA
      results.all <- data.frame(
        paste(rep(names(CI.Lg.Ti), each = length(change.years.names))),
        iso.col,
        paste(rep(change.years.names, times = G)),
        estimates)
      dimnames(results.all)[[2]] <- c("Name", "Iso", "Change", infonames)

      if(fp2020.69.only) {
          results.all <- results.all[results.all$Iso %in% get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD"), ]
          fp2020 <- "_fp2020_69"
      } else fp2020 <- ""

      ## Remove '\' from filenames
      fnm <- gsub("\\.", "_"
                 ,paste(c(unlist(strsplit(names(CI.Lg.Lcat.Ti[[1]])[i]
                                         ,split = "/")))
                       ,collapse = "Over"))
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

        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

      ##details<< Two csv-files (named \code{name.res_changes}) are saved: one with results for proportions, and one with results for counts.
      try(write.csv(results.all
               ,file = file.path(table.dir
                                ,paste0(run.name,"_", name.res, "_changes_"
                                       ,c("perc", "count", "ratio")[j], "_"
                                      ,fnm
                                       ,fp2020,".csv")), # change JR, 20140418
                row.names = FALSE))
    } # end i (cat) loop
  } # end j (prop or count) loop
  cat("Change tables written to", table.dir, "\n")
  return(invisible())
}
##----------------------------------------------------------------------------------
## [MCW-2017-08-10-1] :: Created.
GetTablesChangeAllWomen <- function(# Save csv's with CIs for change in proportions and counts
  ### Save csv's with CIs for change in proportions and counts
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  table.dir = NULL, ##<< Directory to store tables.
  ## If NULL, folder "tables" in current working directory.
  res = NULL, ##<< If NULL, country/UNPD aggregates are summarized, specified with next argument.
  ## Alternatively, an object of class \code{\link{Results}} with \code{changeprop.Lg.Lcat.qt} and \code{changecount.Lg.Lcat.qt}.
  name.res = "Country" ##<<Name used in csv file name AND to determine whether to
  ## save CIs for countries (Country) or UNPD aggregates (UNPDaggregate) if \code{res = NULL}.
 ,fp2020.69.only = FALSE
  ,all.womenize.table.name = TRUE
  ){

  if (is.null(table.dir)){
    table.dir <- file.path(getwd(), "tables/")
    dir.create(table.dir, showWarnings = FALSE)
  }
  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name) # change JR, 20140418
  }
  if (is.null(res)){
    if (name.res == "Country"){
      load(file.path(output.dir, "res.country.all.women.rda"))
      res <- res.country.all.women
    }
    if (name.res == "UNPDaggregate"){
    ## [MCW-2018-02-14]
    ## Don't do anything if this is a one country run.
    load(file = file.path(output.dir,"mcmc.meta.rda"))
    if(mcmc.meta$general$do.country.specific.run) {
        warning("'name.res' is '\"UNPDaggregate\"' but this is a one country run: no tables produced.")
        return(invisible())
    } else {
      load(file.path(output.dir, "res.aggregate.all.women.rda")) # change JR, 20140418
      res <- res.aggregate.all.women
      #print(res)
    }
    }
    }

  change.years.names <- dimnames(res$changeprop.Lg.Lcat.Ti[[1]][[1]])[[1]]
  infonames <- dimnames(res$changeprop.Lg.Lcat.Ti[[1]][[1]])[[2]]
  ninfo <- length(infonames)
  G <- length(res$changeprop.Lg.Lcat.Ti) # same for count and prop
  for (j in 1:3){
    CI.Lg.Lcat.Ti <- res[[c("changeprop.Lg.Lcat.Ti", "changecount.Lg.Lcat.Ti", "changeratio.Lg.Lcat.Ti")[j]]]
    for (i in 1:length(CI.Lg.Lcat.Ti[[1]])){ # not the info i...
      CI.Lg.Ti <- lapply(CI.Lg.Lcat.Ti, function(l) l[[i]])
      estimates <- matrix(NA, length(change.years.names)*G, ninfo)
      for (g in 1:G){
        indices <- seq((g-1)*length(change.years.names) +1, g*length(change.years.names))
        estimates[indices,] <- CI.Lg.Ti[[g]]
      }
      ## [MCW-2016-09-06-3] :: If 'name.res' == "UNPDaggregate" don't include
      ## the iso codes in the 'results.all' data frame.
      if(name.res == "Country") iso.col <- paste(rep(res$iso.g, each = length(change.years.names)))
      else iso.col <- NA
      results.all <- data.frame(
        paste(rep(names(CI.Lg.Ti), each = length(change.years.names))),
        iso.col,
        paste(rep(change.years.names, times = G)),
        estimates)
      dimnames(results.all)[[2]] <- c("Name", "Iso", "Change", infonames)

      if(fp2020.69.only) {
          results.all <- results.all[results.all$Iso %in% get_aggregate_ISOs(name = "FP 2020 countries", family = "UNPD"), ]
          fp2020 <- "_fp2020_69"
      } else fp2020 <- ""

      ## [MCW-2017-08-14-19] :: Ensure ~.csv~ filenames have "aw" in them, even
      ## if ~run.name~ does not have "umw" or "mw" in it.
      if(all.womenize.table.name) {
          aw.run.name <- makeAWFileName(run.name)
          } else aw.run.name <- run.name

      ## Remove '\' from filenames
      fnm <- gsub("\\.", "_"
                 ,paste(c(unlist(strsplit(names(CI.Lg.Lcat.Ti[[1]])[i]
                                         ,split = "/")))
                       ,collapse = "Over"))
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

        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

      ##details<< Two csv-files (named \code{name.res_changes}) are saved: one with results for proportions, and one with results for counts.
      write.csv(results.all, file = file.path(table.dir, paste0(aw.run.name,"_", name.res, "_changes_", c("perc", "count", "ratio")[j], "_", fnm,fp2020,".csv")), # change JR, 20140418
                row.names = FALSE)
    } # end i (cat) loop
  } # end j (prop or count) loop
  cat("Change tables written to", table.dir, "\n")
  return(invisible())
}
##----------------------------------------------------------------------------------
##' Produce big results table as in Lancet article.
##'
##' Reads in the .csv files produced by \code{\link{GetTablesRes}} and
##' \code{\link{GetTablesResAllWomen}} and produces a csv file like the big one
##' in the Lancet.
##'
##' .. content for \details{} ..
##' @param run.name Run name.
##' @param output.dir Directory where MCMC array and meta are
##'     stored. If NULL, it's \code{output/run.name}, default from21
##'     \code{runMCMC}.
##' @param table.dir Directory to store tables. If NULL, folder
##'     "tables" in current working directory.
##' @param indicator.1
##' @param name.res.tbl.1
##' @param name.res.agg.tbl.1
##' @param name.change.tbl.1
##' @param name.change.agg.tbl.1
##' @param indicator.2
##' @param name.res.tbl.2
##' @param name.res.agg.tbl.2
##' @param name.change.tbl.2
##' @param name.change.agg.tbl.2
##' @param year1 First year to include in the table.
##' @param year2 Second year to include in the table.
##' @param special.aggregates 'UN' produces 'Developed', 'Developing', etc. NB: whatever you request must be in 'name.res.agg.tbl.1' and 'name.res.agg.tbl.2'. 'World Bank' is no longer available; use the 'special aggregates' mechanism (2022-06-22).
##' @param regioninfo.csv
##' @param select.c.csv
##' @param adjust.medians Logical. Use adjusted medians?
##' @param adjust.medians.table.dir Character. Path to adjusted medians tables.
##' @return
##' @author Mark C. Wheldon with most of it stolen from GetTablesRes
##'     and GetTablesChange.
##' @noRd
GetLancetBigTable <- function(run.name = "test",
                              output.dir = "test",
                              table.dir = file.path(output.dir, "table"),
                              indicator.1 = "Modern",
                              name.res.tbl.1 = paste(run.name, "Country_perc_Modern.csv", sep = "_"),
                              name.res.agg.tbl.1 = paste(run.name, "UNPDaggregate_perc_Modern.csv", sep = "_"),
                              name.change.tbl.1 = paste(run.name, "Country_changes_perc_Modern.csv", sep = "_"),
                              name.change.agg.tbl.1 = paste(run.name, "UNPDaggregate_changes_perc_Modern.csv", sep = "_"),
                              indicator.2 = "Met Demand with Modern Methods",
                              name.res.tbl.2 = paste(run.name, "Country_ratio_MetDemModMeth.csv", sep = "_"),
                              name.res.agg.tbl.2 = paste(run.name, "UNPDaggregate_ratio_MetDemModMeth.csv", sep = "_"),
                              name.change.tbl.2 = paste(run.name, "Country_changes_ratio_MetDemModMeth.csv", sep = "_"),
                              name.change.agg.tbl.2 = paste(run.name, "UNPDaggregate_changes_ratio_MetDemModMeth.csv", sep = "_"),
                              year1 = 2000,
                              year2 = 2017,
                              special.aggregates = c("UN"),
                              regioninfo.csv = NULL #Need this to get the ordering of countries and regions
                              ## change.str = "Change (2017-2010) - (2010-2000)"
                             ,select.c.csv = NULL
                             ,adjust.medians = FALSE
                              ,adjust.medians.table.dir = NULL
                              ) {


    ## -------* Checks

    load(file = file.path(output.dir,"mcmc.meta.rda"))

    ## ------- ** Don't do anything if this is a one country run.

    if(mcmc.meta$general$do.country.specific.run) {
        warning("This is a one country run: no tables produced.")
        return(invisible())
    }

    ## -------* Create table

    out.tbl <-
        InternalGetLancetBigTable(table.dir = table.dir,
                              indicator.1 = indicator.1,
                              name.res.tbl.1 = name.res.tbl.1,
                              name.res.agg.tbl.1 = name.res.agg.tbl.1,
                              name.change.tbl.1 = name.change.tbl.1,
                              name.change.agg.tbl.1 = name.change.agg.tbl.1,
                              indicator.2 = indicator.2,
                              name.res.tbl.2 = name.res.tbl.2,
                              name.res.agg.tbl.2 = name.res.agg.tbl.2,
                              name.change.tbl.2 = name.change.tbl.2,
                              name.change.agg.tbl.2 = name.change.agg.tbl.2,
                              year1 = year1,
                              year2 = year2,
                              special.aggregates,
                              regioninfo.csv = regioninfo.csv #Need this to get the ordering of countries and regions
                              ## change.str = "Change (2017-2010) - (2010-2000)"
                             ,select.c.csv = select.c.csv
                              ,adjust.medians = adjust.medians
                              ,adjust.medians.table.dir = adjust.medians.table.dir
                              )

    ## -------* Data Availability

    prev.data.iso <- unique(mcmc.meta$data.raw$data$iso.j)
    out.tbl$`Prevalence data` <- 0
    out.tbl$`Prevalence data`[out.tbl$`Region/Sub-region/ISO` %in% as.character(prev.data.iso)] <- 1

    unmet.data.iso <-
        unique(mcmc.meta$data.raw$data$iso.j[!is.na(mcmc.meta$data.raw$data$props.unmet.j)])
    out.tbl$`Unmet need data` <- 0
    out.tbl$`Unmet need data`[out.tbl$`Region/Sub-region/ISO` %in% as.character(unmet.data.iso)] <- 1

    ## -------* Latex Table

    latex.tbl <- InternalLancetBigTable2Latex(out.tbl)
    latex.R.tbl <- latex.tbl$tbl
    latex.out.tbl <- latex.tbl$xtable.tbl

    ## -------* Finish

    ## File name
    fnm <- paste0(run.name, "_BIG_T_"
                  ,indicator.1, "_", indicator.2, "_"
                 ,paste(year1, year2, sep = "_"))

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

    fnm <- gsub("region[s]*", "", fnm)
    fnm <- gsub("__", "_", fnm, fixed = TRUE)

    ## Remove '\' from filenames
    fnm <- gsub("\\.", "_"
               ,paste(c(unlist(strsplit(fnm, split = "/"))), collapse = "Over"))

    ## Adjusted?
    if(isTRUE(adjust.medians)) {
        fnm <- paste0(fnm, "_Adj")
        out.dir <- adjust.medians.table.dir
    } else {
        out.dir <- table.dir
    }

    ## Save csv
    write.csv(out.tbl
             ,file = file.path(out.dir, paste0(fnm, ".csv"))
             ,row.names = FALSE)

    ## Write latex
    latex.out.tbl.fnm.li <- list()
    for(tbl.no in 1:length(latex.out.tbl)) {
        writeLines(latex.out.tbl[[tbl.no]]
                  ,con = file.path(out.dir, paste0(fnm, "_", tbl.no, ".tex")))
        latex.out.tbl.fnm.li <-
            c(latex.out.tbl.fnm.li
            ,list(file.path(out.dir, paste0(fnm, "_", tbl.no, ".tex"))))
    }

    ## Save latex object
    names(latex.R.tbl$tbl) <-
        paste0(fnm, "_", 1:length(latex.R.tbl$tbl))
    save(latex.R.tbl
        ,file = file.path(out.dir, paste0(fnm, ".RData")))

    cat("Result tables written to", out.dir, "\n")

    return(invisible(list(csv = file.path(out.dir, paste0(fnm, ".csv"))
                         ,tex = latex.out.tbl.fnm.li
                         ,R = file.path(out.dir, paste0(fnm, ".RData"))
                          )))
}
##----------------------------------------------------------------------------------
##' Produce big results table as in Lancet article.
##'
##' Reads in the .csv files produced by \code{\link{GetTablesRes}} and
##' \code{\link{GetTablesResAllWomen}} and produces a csv file like the big one
##' in the Lancet.
##'
##' .. content for \details{} ..
##' @param run.name Run name.
##' @param output.dir Directory where MCMC array and meta are
##'     stored. If NULL, it's \code{output/run.name}, default from21
##'     \code{runMCMC}.
##' @param table.dir Directory to store tables. If NULL, folder
##'     "tables" in current working directory.
##' @param indicator.1
##' @param name.res.tbl.1
##' @param name.res.agg.tbl.1
##' @param name.change.tbl.1
##' @param name.change.agg.tbl.1
##' @param indicator.2
##' @param name.res.tbl.2
##' @param name.res.agg.tbl.2
##' @param name.change.tbl.2
##' @param name.change.agg.tbl.2
##' @param year1 First year to include in the table.
##' @param year2 Second year to include in the table.
##' @param special.aggregates 'UN' produces 'Developed', 'Developing', etc. NB:
##'     whatever you request must be in 'name.res.agg.tbl.1' and
##'     'name.res.agg.tbl.2'. World Bank is no longer available; use the 'special aggregates' mechanism (2022-06-22).
##' @param regioninfo.csv
##' @param ##change.str
##' @return
##' @author Mark C. Wheldon with most of it stolen from GetTablesRes
##'     and GetTablesChange.
##' @noRd
GetLancetBigTableAllWomen <- function(run.name = "test",
                              output.dir = NULL,
                              table.dir = file.path(output.dir, "table"),
                              indicator.1 = "Modern",
                              name.res.tbl.1 = paste(aw.run.name, "Country_perc_Modern.csv", sep = "_"),
                              name.res.agg.tbl.1 = paste(aw.run.name, "UNPDaggregate_perc_Modern.csv", sep = "_"),
                              name.change.tbl.1 = paste(aw.run.name, "Country_changes_perc_Modern.csv", sep = "_"),
                              name.change.agg.tbl.1 = paste(aw.run.name, "UNPDaggregate_changes_perc_Modern.csv", sep = "_"),
                              indicator.2 = "Met Demand with Modern Methods",
                              name.res.tbl.2 = paste(aw.run.name, "Country_ratio_MetDemModMeth.csv", sep = "_"),
                              name.res.agg.tbl.2 = paste(aw.run.name, "UNPDaggregate_ratio_MetDemModMeth.csv", sep = "_"),
                              name.change.tbl.2 = paste(aw.run.name, "Country_changes_ratio_MetDemModMeth.csv", sep = "_"),
                              name.change.agg.tbl.2 = paste(aw.run.name, "UNPDaggregate_changes_ratio_MetDemModMeth.csv", sep = "_"),
                              year1 = 2000,
                              year2 = 2017,##
                              special.aggregates = c("UN"),
                              regioninfo.csv = NULL,
                              ## change.str = "Change (2017-2010) - (2010-2000)"
                              select.c.csv = NULL,
                              all.womenize.table.name = TRUE,
                             adjust.medians = FALSE,
                              adjust.medians.table.dir = NULL
                              ) {


    ## -------* Checks

    load(file = file.path(output.dir,"mcmc.meta.rda"))

    ## ------- ** Don't do anything if this is a one country run.

    if(mcmc.meta$general$do.country.specific.run) {
        warning("This is a one country run: no tables produced.")
        return(invisible())
    }

    ## -------* Set up

    ## -------** Set the filenames for csvs correctly

    ## [MCW-2017-08-14-19] :: Ensure ~.csv~ filenames have "aw" in them, even
    ## if ~run.name~ does not have "umw" or "mw" in it.
    if(all.womenize.table.name) {
        aw.run.name <- makeAWFileName(run.name)
        } else aw.run.name <- run.name

    ## if(is.null(name.res.tbl.1)) name.res.tbl.1 <- paste(aw.run.name, "Country_perc_Modern.csv", sep = "_")
    ## if(is.null(name.res.agg.tbl.1)) name.res.agg.tbl.1 <- paste(aw.run.name, "UNPDaggregate_perc_Modern.csv", sep = "_")
    ## if(is.null(name.change.tbl.1)) name.change.tbl.1 <- paste(aw.run.name, "Country_changes_perc_Modern.csv", sep = "_")
    ## if(is.null(name.change.agg.tbl.1)) name.change.agg.tbl.1 <-  paste(aw.run.name, "UNPDaggregate_changes_perc_Modern.csv", sep = "_")
    ## if(is.null(name.res.tbl.2)) name.res.tbl.2 <- paste(aw.run.name, "Country_ratio_MetDemModMeth.csv", sep = "_")
    ## if(is.null(name.res.agg.tbl.2)) name.res.agg.tbl.2 <- paste(aw.run.name, "UNPDaggregate_ratio_MetDemModMeth.csv", sep = "_")
    ## if(is.null(name.change.tbl.2)) name.change.tbl.2  <-  paste(aw.run.name, "Country_changes_ratio_MetDemModMeth.csv", sep = "_")
    ## if(is.null(name.change.agg.tbl.2)) name.change.agg.tbl.2 <- paste(aw.run.name, "UNPDaggregate_changes_ratio_MetDemModMeth.csv", sep = "_")

    ## -------* Create table

    out.tbl <-
        InternalGetLancetBigTable(table.dir = table.dir,
                              indicator.1 = indicator.1,
                              name.res.tbl.1 = name.res.tbl.1,
                              name.res.agg.tbl.1 = name.res.agg.tbl.1,
                              name.change.tbl.1 = name.change.tbl.1,
                              name.change.agg.tbl.1 = name.change.agg.tbl.1,
                              indicator.2 = indicator.2,
                              name.res.tbl.2 = name.res.tbl.2,
                              name.res.agg.tbl.2 = name.res.agg.tbl.2,
                              name.change.tbl.2 = name.change.tbl.2,
                              name.change.agg.tbl.2 = name.change.agg.tbl.2,
                              year1 = year1,
                              year2 = year2,
                              special.aggregates = special.aggregates,
                              regioninfo.csv = regioninfo.csv #Need this to get the ordering of countries and regions
                              ## change.str = "Change (2017-2010) - (2010-2000)"
                              ,select.c.csv = select.c.csv
                             ,adjust.medians = adjust.medians
                              ,adjust.medians.table.dir = adjust.medians.table.dir
                              )

    latex.tbl <-
        InternalLancetBigTable2Latex(out.tbl,
                  bullet.cols = NULL,
             separate.tables = list(c(1, 3:6), c(1, 7:10)))
    latex.R.tbl <- latex.tbl$tbl
    latex.out.tbl <- latex.tbl$xtable.tbl

    ## -------* Finish

    fnm <- paste0(aw.run.name, "_BIG_T_"
                  ,indicator.1, "_", indicator.2, "_"
                 ,paste(year1, year2, sep = "_"))

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

        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

    ## Remove '\' from filenames
    fnm <- gsub("\\.", "_"
               ,paste(c(unlist(strsplit(fnm, split = "/"))), collapse = "Over"))

    ## Adjusted?
    if(isTRUE(adjust.medians)) {
        fnm <- paste0(fnm, "_Adj")
        out.dir <- adjust.medians.table.dir
    } else {
        out.dir <- table.dir
    }

    ## Save csv
    write.csv(out.tbl
             ,file = file.path(out.dir, paste0(fnm, ".csv"))
             ,row.names = FALSE)

    ## Write latex
    latex.out.tbl.fnm.li <- list()
    for(tbl.no in 1:length(latex.out.tbl)) {
        writeLines(latex.out.tbl[[tbl.no]]
                  ,con = file.path(out.dir, paste0(fnm, "_", tbl.no, ".tex")))
        latex.out.tbl.fnm.li <-
            c(latex.out.tbl.fnm.li
            ,list(file.path(out.dir, paste0(fnm, "_", tbl.no, ".tex"))))
    }

    ## Save latex object
    names(latex.R.tbl$tbl) <-
        paste0(fnm, "_", 1:length(latex.R.tbl$tbl))
    save(latex.R.tbl
        ,file = file.path(out.dir, paste0(fnm, ".RData")))

    cat("Result tables written to", out.dir, "\n")

    return(invisible(list(csv = file.path(out.dir, paste0(fnm, ".csv"))
                         ,tex = latex.out.tbl.fnm.li
                         ,R = file.path(out.dir, paste0(fnm, ".RData"))
                          )))

}
##----------------------------------------------------------------------------------
InternalGetLancetBigTable <-
    function(table.dir = NULL,
             indicator.1 = NULL,
             name.res.tbl.1 = NULL,
             name.res.agg.tbl.1 = NULL,
             name.change.tbl.1 = NULL,
             name.change.agg.tbl.1 = NULL,
             indicator.2 = NULL,
             name.res.tbl.2 = NULL,
             name.res.agg.tbl.2 = NULL,
             name.change.tbl.2 = NULL,
             name.change.agg.tbl.2 = NULL,
             year1 = NULL,
             year2 = NULL,
             special.aggregates = NULL,
             regioninfo.csv = NULL, #Need this to get the ordering of countries and regions
             select.c.csv = NULL,
             adjust.medians = FALSE,
             adjust.medians.table.dir = NULL
             ## change.str = "Change (2017-2010) - (2010-2000)"
             ) {

        ## -------* Functions

        reshape.tbl <- function(tbl, multiplier = 100) {
            if(all(is.na(tbl$Iso))) tbl$Iso <- tbl$Name
            tbl <- tbl[, c("Name", "Iso", "Percentile"
                          ,paste0("X", c(year1, year2) + 0.5)
                           )]

            tbl <- stats::reshape(tbl, direction = "wide", timevar = "Percentile"
                          ,idvar = c("Name", "Iso")
                           )
            tbl <- data.frame(tbl[,c("Name", "Iso")]
                              ## year 1
                             ,paste0(round(tbl[, paste0("X", year1 + 0.5, ".0.5")] * multiplier, 1)
                                    ," ("
                                    ,round(tbl[, paste0("X", year1 + 0.5, ".0.025")] * multiplier, 1)
                                    ,"--"
                                    ,round(tbl[, paste0("X", year1 + 0.5, ".0.975")] * multiplier, 1)
                                    ,")")
                              ## year 2
                             ,paste0(round(tbl[, paste0("X", year2 + 0.5, ".0.5")] * multiplier, 1)
                                    ," ("
                                    ,round(tbl[, paste0("X", year2 + 0.5, ".0.025")] * multiplier, 1)
                                    ,"--"
                                    ,round(tbl[, paste0("X", year2 + 0.5, ".0.975")] * multiplier, 1)
                                    ,")")
                              )
            return(tbl)
        }

        ## Used for 'change' quantities
        subset.tbl <- function(tbl, multiplier = 100) {

            if(all(is.na(tbl$Iso))) tbl$Iso <- tbl$Name

            ## Because the columns are not consistently named in the objects.
            col25 <- grep("^X2\\.5\\.*$|^X\\.*0\\.025$", colnames(tbl))
            col50 <- grep("^X50\\.*$|^X\\.*0\\.5$", colnames(tbl))
            col95 <- grep("^X97\\.5\\.*$|^X\\.*0\\.975$", colnames(tbl))
            colPPPC <- grep("PPPC", colnames(tbl))

            tbl <- subset(tbl, Change == paste(year1, year2, sep = "-"))
            tbl <-
                data.frame(tbl[, c(1:2)]
                          ,paste0(round(tbl[, col50] * multiplier, 1), " ", "(", round(tbl[, col25] * multiplier, 1)
                                 ,"--", round(tbl[, col95] * multiplier, 1), ")")
                           ,round(tbl[, colPPPC] * multiplier, 1)
                           )
            return(tbl)
        }

        ## 'Adj' Filename
        adj.fnm <- function(x) {
            paste0(tools::file_path_sans_ext(x), "_Adj.", tools::file_ext(x))
        }

        ## Sub 'Adj' Medians
        sub.adj.medians <- function(orig, adj) {
            out <- orig[orig$Percentile != 0.5,]
            out <- rbind(out, adj)
            out <- out[with(out, order(Name, Percentile)),] #agg has all ISOs == NA
            out
            }

        ## -------* Read in csv files

        res.tbl.1 <-
            read.csv(file.path(table.dir, name.res.tbl.1)
                    ,stringsAsFactors = FALSE)
        res.agg.tbl.1 <- read.csv(file.path(table.dir, name.res.agg.tbl.1)
                                 ,stringsAsFactors = FALSE)
        change.tbl.1 <- read.csv(file.path(table.dir, name.change.tbl.1)
                                ,stringsAsFactors = FALSE)
        change.agg.tbl.1 <- read.csv(file.path(table.dir, name.change.agg.tbl.1)
                                    ,stringsAsFactors = FALSE)
        res.tbl.2 <-
            read.csv(file.path(table.dir, name.res.tbl.2)
                    ,stringsAsFactors = FALSE)
        res.agg.tbl.2 <- read.csv(file.path(table.dir, name.res.agg.tbl.2)
                                 ,stringsAsFactors = FALSE)
        change.tbl.2 <- read.csv(file.path(table.dir, name.change.tbl.2)
                                ,stringsAsFactors = FALSE)
        change.agg.tbl.2 <- read.csv(file.path(table.dir, name.change.agg.tbl.2)
                                    ,stringsAsFactors = FALSE)

        ## Check that years requested are present in the tables
        if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.tbl.1), fixed = TRUE)))) {
            stop("'", year1, "' is not in '", name.res.tbl.1, "'.")
        }
        if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.tbl.1), fixed = TRUE)))) {
            stop("'", year2, "' is not in '", name.res.tbl.1, "'.")
        }
        if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.agg.tbl.1), fixed = TRUE)))) {
            stop("'", year1, "' is not in '", name.res.agg.tbl.1, "'.")
        }
        if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.agg.tbl.1), fixed = TRUE)))) {
            stop("'", year2, "' is not in '", name.res.agg.tbl.1, "'.")
        }
        if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.tbl.2), fixed = TRUE)))) {
            stop("'", year1, "' is not in '", name.res.tbl.2, "'.")
        }
        if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.tbl.2), fixed = TRUE)))) {
            stop("'", year2, "' is not in '", name.res.tbl.2, "'.")
        }
        if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.agg.tbl.2), fixed = TRUE)))) {
            stop("'", year1, "' is not in '", name.res.agg.tbl.2, "'.")
        }
        if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.agg.tbl.2), fixed = TRUE)))) {
            stop("'", year2, "' is not in '", name.res.agg.tbl.2, "'.")
        }

        if(sum(change.tbl.1$Change == paste(year1, year2, sep = "-"), na.rm = TRUE) < 1) {
            stop("The change '", paste(year1, year2, sep = "-"), "' is not in '", name.change.tbl.1, "'.")
        }
        if(sum(change.tbl.2$Change == paste(year1, year2, sep = "-"), na.rm = TRUE) < 1) {
            stop("The change '", paste(year1, year2, sep = "-"), "' is not in '", name.change.tbl.2, "'.")
        }
        if(sum(change.agg.tbl.1$Change == paste(year1, year2, sep = "-"), na.rm = TRUE) < 1) {
            stop("The change '", paste(year1, year2, sep = "-"), "' is not in '", name.change.agg.tbl.1, "'.")
        }
        if(sum(change.agg.tbl.2$Change == paste(year1, year2, sep = "-"), na.rm = TRUE) < 1) {
            stop("The change '", paste(year1, year2, sep = "-"), "' is not in '", name.change.agg.tbl.2, "'.")
        }

        ## -------** Adjusted Medians?

        if(isTRUE(adjust.medians)) {

            ## Read in .csv files
            res.tbl.1.adj <-
                read.csv(file.path(adjust.medians.table.dir, adj.fnm(name.res.tbl.1)),
                         stringsAsFactors = FALSE)
            res.agg.tbl.1.adj <- read.csv(file.path(adjust.medians.table.dir, adj.fnm(name.res.agg.tbl.1)),
                                          stringsAsFactors = FALSE)
            res.tbl.2.adj <-
                read.csv(file.path(adjust.medians.table.dir, adj.fnm(name.res.tbl.2)),
                         stringsAsFactors = FALSE)
            res.agg.tbl.2.adj <- read.csv(file.path(adjust.medians.table.dir, adj.fnm(name.res.agg.tbl.2)),
                                          stringsAsFactors = FALSE)

            ## Check that years requested are present in the tables
            if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.tbl.1.adj), fixed = TRUE)))) {
                stop("'", year1, "' is not in '", name.res.tbl.1.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.tbl.1.adj), fixed = TRUE)))) {
                stop("'", year2, "' is not in '", name.res.tbl.1.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.agg.tbl.1.adj), fixed = TRUE)))) {
                stop("'", year1, "' is not in '", name.res.agg.tbl.1.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.agg.tbl.1.adj), fixed = TRUE)))) {
                stop("'", year2, "' is not in '", name.res.agg.tbl.1.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.tbl.2.adj), fixed = TRUE)))) {
                stop("'", year1, "' is not in '", name.res.tbl.2.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.tbl.2.adj), fixed = TRUE)))) {
                stop("'", year2, "' is not in '", name.res.tbl.2.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year1, ".5"), colnames(res.agg.tbl.2.adj), fixed = TRUE)))) {
                stop("'", year1, "' is not in '", name.res.agg.tbl.2.adj, "'.")
            }
            if(!(any(grepl(paste0("X", year2, ".5"), colnames(res.agg.tbl.2.adj), fixed = TRUE)))) {
                stop("'", year2, "' is not in '", name.res.agg.tbl.2.adj, "'.")
            }

            ## Check that all ISOs / Names in 'orig' are in 'adj'
            if(!all(res.tbl.1$Iso %in% res.tbl.1.adj$Iso)) stop("Not all ISOs in 'res.tbl.1' are in 'res.tbl.1.adj'")
            if(!all(res.tbl.2$Iso %in% res.tbl.2.adj$Iso)) stop("Not all ISOs in 'res.tbl.2' are in 'res.tbl.2.adj'")
            if(!all(res.agg.tbl.1$Name %in% res.agg.tbl.1.adj$Name)) {
                stop("Not all Names in 'res.agg.tbl.1' are in 'res.agg.tbl.1.adj'")
                }
            if(!all(res.agg.tbl.2$Name %in% res.agg.tbl.2.adj$Name)) {
                stop("Not all Names in 'res.agg.tbl.2' are in 'res.agg.tbl.2.adj'")
                }
        }

        ## -------* Re-shape

        ## -------** Prevalence estimates

        ## -------*** Adjust medians?

        if(isTRUE(adjust.medians)) {
            res.tbl.1 <- sub.adj.medians(res.tbl.1, res.tbl.1.adj)
            res.tbl.2 <- sub.adj.medians(res.tbl.2, res.tbl.2.adj)
            res.agg.tbl.1 <- sub.adj.medians(res.agg.tbl.1, res.agg.tbl.1.adj)
            res.agg.tbl.2 <- sub.adj.medians(res.agg.tbl.2, res.agg.tbl.2.adj)
        }

        ## -------*** Merge and reshape

        res.tbl <- data.frame(reshape.tbl(res.tbl.1)
                             ,reshape.tbl(res.tbl.2)[, -(1:2)]
                              )
        colnames(res.tbl)[-(1:2)] <-
            c(paste(indicator.1, year1), paste(indicator.1, year2), paste(indicator.2, year1)
             ,paste(indicator.2, year2))

        res.agg.tbl <- data.frame(reshape.tbl(res.agg.tbl.1)
                                 ,reshape.tbl(res.agg.tbl.2)[, -(1:2)]
                                  )
        colnames(res.agg.tbl)[-(1:2)] <-
            c(paste(indicator.1, year1), paste(indicator.1, year2), paste(indicator.2, year1)
             ,paste(indicator.2, year2))

        ## -------** Change

        change.tbl <- data.frame(subset.tbl(change.tbl.1)
                                ,subset.tbl(change.tbl.2)[, -(1:2)]
                                 )
        colnames(change.tbl)[-(1:2)] <-
            c(paste0("Change (", indicator.1, ") ", year1, "--", year2)
              ,c(paste0("PPPC (", indicator.1, ") ", year1, "--", year2))
             ,c(paste0("Change (", indicator.2, ") ", year1, "--", year2))
              ,c(paste0("PPPC (", indicator.2, ") ", year1, "--", year2)))

        change.agg.tbl <- data.frame(subset.tbl(change.agg.tbl.1)
                                    ,subset.tbl(change.agg.tbl.2)[, -(1:2)]
                                     )
        colnames(change.agg.tbl)[-(1:2)] <-
            c(paste0("Change (", indicator.1, ") ", year1, "--", year2)
              ,c(paste0("PPPC (", indicator.1, ") ", year1, "--", year2))
             ,c(paste0("Change (", indicator.2, ") ", year1, "--", year2))
              ,c(paste0("PPPC (", indicator.2, ") ", year1, "--", year2)))

        ## -------** Combine and order

        if(!is.null(regioninfo.csv)) {

            ## If 'regioninfo.csv' supplied, order by region, subregion, country,
            ## nestedly.

            ## Load region info
            regsubreg <-
                read.csv(file = regioninfo.csv, stringsAsFactors = FALSE)
            regsubreg <-
                regsubreg[, c("ISO.Code", "Major.area.Code", "Region.Code", "Country.or.area"
                             ,"Region", "Major.area")]
            regsubreg.subreg <-
                regsubreg[!duplicated(regsubreg$Region),
                          c("Region.Code", "Region", "Major.area.Code", "Major.area")]
            regsubreg.reg <-
                regsubreg[!duplicated(regsubreg$Major.area), c("Major.area.Code", "Major.area")]

            ## -------*** Harmonize names

            ## Make sure names of countries and areas are the same in regioninfo
            ## and the data, otherwise the merges will mess up.

            res.tbl <-
                merge(res.tbl[, !colnames(res.tbl) == "Name"]
                     ,regsubreg[, c("Country.or.area", "ISO.Code")]
                     ,by.x = "Iso", by.y = "ISO.Code"
                     ,sort = FALSE
                      )
            colnames(res.tbl)[colnames(res.tbl) == "Country.or.area"] <- "Name"

            change.tbl <-
                merge(change.tbl[, !colnames(change.tbl) == "Name"]
                     ,regsubreg[, c("Country.or.area", "ISO.Code")]
                     ,by.x = "Iso", by.y = "ISO.Code"
                     ,sort = FALSE
                      )
            colnames(change.tbl)[colnames(change.tbl) == "Country.or.area"] <- "Name"

            ## -------*** Prevalence estimates

            ## Merge region and subregion on to country data frames
            res.tbl <-
                merge(res.tbl, regsubreg[,-!colnames(regsubreg)=="ISO.Code"]
                     ,all.x = TRUE, all.y = FALSE
                     ,by.x = "Name", by.y = "Country.or.area"
                      )

            res.agg.tbl.subreg <-
                merge(res.agg.tbl, regsubreg.subreg
                     ,all.x = TRUE, all.y = FALSE
                     ,by.x = "Name", by.y = "Region"
                      )
            res.agg.tbl.reg <-
                merge(res.agg.tbl, regsubreg.reg
                     ,all.x = TRUE, all.y = FALSE
                     ,by.x = "Name", by.y = "Major.area"
                      )

            ## Sort
            res.tbl <-
                res.tbl[with(res.tbl, order(Major.area, Region, Name)),]
            res.agg.tbl.subreg <-
                res.agg.tbl.subreg[with(res.agg.tbl.subreg, order(Major.area, Name)),]
            res.agg.tbl.reg <-
                res.agg.tbl.reg[with(res.agg.tbl.reg, order(Name)),]

            ## Clean
            res.agg.tbl.subreg <- res.agg.tbl.subreg[complete.cases(res.agg.tbl.subreg),]
            res.agg.tbl.reg <- res.agg.tbl.reg[complete.cases(res.agg.tbl.reg),]

            ## Interleave regions, countries
            res.tbl.inter <- data.frame()
            for(a in unique(as.character(res.agg.tbl.reg$Name))) {
                res.tbl.inter <-
                    rbind(res.tbl.inter
                         ,res.agg.tbl[res.agg.tbl$Name == a,]
                          )
                for(r in unique(as.character(res.agg.tbl.subreg[res.agg.tbl.subreg$Major.area == a,]$Name))) {
                    res.tbl.inter <-
                        rbind(res.tbl.inter
                             ,res.agg.tbl.subreg[res.agg.tbl.subreg$Name == r,
                                                 colnames(res.tbl.inter)]
                             ,res.tbl[with(res.tbl, Major.area == a & Region == r),
                                      colnames(res.tbl.inter)]
                              )
                }
            }

            ## -------*** Change

            ## Merge region and subregion on to country data frames
            change.tbl <-
                merge(change.tbl, regsubreg
                     ,all.x = TRUE, all.y = FALSE
                     ,by.x = "Name", by.y = "Country.or.area"
                      )

            change.agg.tbl.subreg <-
                merge(change.agg.tbl, regsubreg.subreg
                     ,all.x = TRUE, all.y = FALSE
                     ,by.x = "Name", by.y = "Region"
                      )
            change.agg.tbl.reg <-
                merge(change.agg.tbl, regsubreg.reg
                     ,all.x = TRUE, all.y = FALSE
                     ,by.x = "Name", by.y = "Major.area"
                      )

            ## Sort
            change.tbl <-
                change.tbl[with(change.tbl, order(Major.area, Region, Name)),]
            change.agg.tbl.subreg <-
                change.agg.tbl.subreg[with(change.agg.tbl.subreg, order(Major.area, Name)),]
            change.agg.tbl.reg <-
                change.agg.tbl.reg[with(change.agg.tbl.reg, order(Name)),]

            ## Clean
            change.agg.tbl.subreg <- change.agg.tbl.subreg[complete.cases(change.agg.tbl.subreg),]
            change.agg.tbl.reg <- change.agg.tbl.reg[complete.cases(change.agg.tbl.reg),]

            ## Interleave regions, countries
            change.tbl.inter <- data.frame()
            for(a in unique(as.character(change.agg.tbl.reg$Name))) {
                change.tbl.inter <-
                    rbind(change.tbl.inter
                         ,change.agg.tbl[change.agg.tbl$Name == a,]
                          )
                for(r in unique(as.character(change.agg.tbl.subreg[change.agg.tbl.subreg$Major.area == a,]$Name))) {
                    change.tbl.inter <-
                        rbind(change.tbl.inter
                             ,change.agg.tbl.subreg[change.agg.tbl.subreg$Name == r,
                                                    colnames(change.tbl.inter)]
                             ,change.tbl[with(change.tbl, Major.area == a & Region == r),
                                         colnames(change.tbl.inter)]
                              )
                }
            }

            ## -------*** Combine

            ## Beware duplicates! N. America is a region and subregion.
            res.tbl.inter <- res.tbl.inter[!duplicated(res.tbl.inter$Name),]
            change.tbl.inter <-
                change.tbl.inter[!duplicated(change.tbl.inter$Name),]
            out.tbl <-
                merge(res.tbl.inter, change.tbl.inter[, -2], by = "Name"
                     ,sort = FALSE)[,c(1:4, 7:8, 5:6, 9:10)]

            ## -------*** Special aggregates

            if(special.aggregates[1] == "UN") {
                res.spec.agg <-
                rbind(res.agg.tbl[res.agg.tbl$Name == "World",]
                     ,res.agg.tbl[res.agg.tbl$Name == "More developed countries",]
                     ,res.agg.tbl[res.agg.tbl$Name == "Less developed countries",]
                     ,res.agg.tbl[res.agg.tbl$Name == "Least developed countries",]
                     ,res.agg.tbl[res.agg.tbl$Name == "Less developed countries, excluding China",]
                     ,res.agg.tbl[res.agg.tbl$Name == "Other developing countries",]
                     ,res.agg.tbl[res.agg.tbl$Name == "FP2020 69 Countries",]
                      )
            change.spec.agg <-
                rbind(change.agg.tbl[change.agg.tbl$Name == "World",]
                     ,change.agg.tbl[change.agg.tbl$Name == "More developed countries",]
                     ,change.agg.tbl[change.agg.tbl$Name == "Less developed countries",]
                     ,change.agg.tbl[change.agg.tbl$Name == "Least developed countries",]
                     ,change.agg.tbl[change.agg.tbl$Name == "Less developed countries, excluding China",]
                     ,change.agg.tbl[change.agg.tbl$Name == "Other developing countries",]
                     ,change.agg.tbl[change.agg.tbl$Name == "FP2020 69 Countries",]
                      )
            } else stop("'special.aggregates' must be 'UN'; no other values are valid.")

            out.tbl <-
                rbind(merge(res.spec.agg, change.spec.agg[, -2], by = "Name"
                           ,sort = FALSE)[,c(1:4, 7:8, 5:6, 9:10)]
                      ,out.tbl)

        } else {
            message("'regioninfo.csv' not supplied so table is ordered alphabetically (countries, regions, subregions will be mixed together).")
            res.tbl <- rbind(res.tbl, res.agg.tbl)
            change.tbl <- rbind(change.tbl, change.agg.tbl)
            out.tbl <- merge(res.tbl, change.tbl[, -1], by = "Iso")[,c(1:4, 7:8, 5:6, 9:10)]
            out.tbl <- out.tbl[order(out.tbl$Name),]
        }

        ## -------** Fix up

        ## Exclusions

        if(!is.null(select.c.csv)) {
            read.select.c.csv <- read.csv(select.c.csv)
            ISO.colname <- grep("^ISO|^ISO Code", colnames(read.select.c.csv), ignore.case = TRUE, value = TRUE)
            if(identical(length(ISO.colname), 0L)) stop("'select.c.csv' does not have a column called 'ISO' or 'ISO Code' (ignoring case).")
            else if(!is.numeric(read.select.c.csv[[ISO.colname]])) stop("Column ", ISO.colname, " in 'select.c.csv' is not numeric.")
            incl.ISO <- read.select.c.csv[[ISO.colname]]
            out.tbl <-
                out.tbl[out.tbl$Iso %in% as.character(incl.ISO) |
                        is.na(suppressWarnings(as.numeric(out.tbl$Iso))),]
        }

        ## Factors to Char

        for(j in which(sapply(out.tbl, "class") == "factor")) {
            out.tbl[,j] <- as.character(out.tbl[,j])
            }

        ## Rename Columns

        colnames(out.tbl)[colnames(out.tbl) == "Iso"] <- "Region/Sub-region/ISO"

        ## -------* Return

        return(out.tbl)
    }
##----------------------------------------------------------------------------------
##' .. title (single sentence, sentence case, full stop at end) ..
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param tbl
##' @param before.major.area
##' @param before.subregion
##' @param before.country
##' @param major.areas
##' @param bullet.cols Columns in which 0s and 1s should be replaced with blanks
##'     and '$\bullet$'s, respectively. Rows that are major areas or subregions
##'     will have 1s replaced with '$\circ$'s.
##' @param separate.tables List of numeric vectors giving the columns to put in
##'     separate tables.
##' @return
##' @author
##' @noRd
InternalLancetBigTable2Latex <-
    function(tbl,
             before.major.area = "\\hspace{2ex}",
             before.subregion = "\\hspace{4ex}",
             before.country = "\\hspace{6ex}",
             major.areas = NULL,
             bullet.cols = c(11, 12),
             separate.tables = list(c(1, 3:6, 11, 12), c(1, 7:10, 11, 12)),
             shorten.UK = FALSE,
             special.aggregates = c("UN")
             ) {

        ## -------* Major areas

        major.areas <- c("Asia", "Africa", "Europe",
                             "Latin America and the Caribbean",
                             "Oceania", "Northern America",
                             "LAC", "N America", "N. America",
                         "FP2020 69 Countries", "FP2020 69 countries")
        if(special.aggregates[1] == "UN") {
            major.areas <-
                c(major.areas, c("Developed regions", "Developing regions",
                                 "Developing (excl. China)"))
        } else stop("'special.aggregates' must be 'UN'; no other values are valid.")

        ## -------* Line endings

        ## Can be used to add indents and/or vertical space and/or horizontal
        ## rules.

        if(!is.null(before.country)) {
            line.endings <- rep(before.country, nrow(tbl))
        }

        if(!is.null(before.subregion)) {
            non.ISO.lines <-
                (1:nrow(tbl))[grepl("[[:alpha:]]", tbl$`Region/Sub-region/ISO`)]
            before.non.ISO.lines <-
                (non.ISO.lines - 1)[non.ISO.lines - 1 > 0]
            line.endings[before.non.ISO.lines] <-
                rep(before.subregion, length(before.non.ISO.lines))
        }

        if(!is.null(before.major.area)) {
            major.area.lines <- which(tbl$Name %in% major.areas)
            before.major.area.lines <-
                (major.area.lines - 1)[major.area.lines - 1 > 0]
            line.endings[before.major.area.lines] <-
                rep(before.major.area, length(before.major.area.lines))
        }

        ## Can't have anything on the last line
        if(any(!is.null(before.country), !is.null(before.subregion), !is.null(major.areas))) {
            line.endings <- head(line.endings, -1)
        }

        ## -------* Bullets

        if(!is.null(bullet.cols)) {
            zeros <-
                tbl[, bullet.cols] == 0 | tbl[, bullet.cols] == 0L
            ones <-
                tbl[, bullet.cols] == 1 | tbl[, bullet.cols] == 1L

            tbl[, bullet.cols][zeros] <- "$\\circ$"
            tbl[, bullet.cols][ones] <- "$\\bullet$"
            tbl[unique(c(non.ISO.lines, major.area.lines)), bullet.cols] <- ""
        }

        ## -------* Names

        ## Non-ASCII characters

        if(!is.null(tbl$Name)) {
            tbl$Name[grep("R.+nion", tbl$Name)] <- "R{\\'e}union"
            tbl$Name[grep("C.+e d'Ivoire", tbl$Name)] <- "C{\\^o}te d'Ivoire"
        }

        ## UK

        if(shorten.UK) {
            tbl$Name[tbl$Name == "United Kingdom of Great Britain and Northern Ireland"] <-
                "United Kingdom"
        }

        ## -------* Make output

        xtable.tbl <- lapply(separate.tables, function(columns.to.print) {
            print(xtable::xtable(tbl[,columns.to.print])
                 ,floating = FALSE, tabular.environment = "longtable"
              ,caption.placement = "top"
              ,add.to.row = list(pos = as.list(1:length(line.endings))
                                ,command = line.endings)
              ,include.rownames = FALSE
              ,print.results = FALSE
              ,sanitize.text.function = function(z) z
               )
        })

        tbl <- list(tbl = lapply(separate.tables
                                ,function(columns.to.print) {
                                    tbl[,columns.to.print]
                                })
                   ,line.endings = line.endings)

        return(list(tbl = tbl, xtable.tbl = xtable.tbl))

    }

##----------------------------------------------------------------------
##' Make output tables for age ratios
##'
##' Creates output \file{.csv} files for age ratios, e.g., proportion
##' of users that are aged 15--19.
##'
##' @param run.name
##' @param output.dir
##' @param table.dir
##' @param res
##' @param name.res
##' @return Saves \file{.csv} files.
##' @author Mark Wheldon
##' @noRd
GetTablesAgeRatios <-
    function(run.name = "test",
             output.dir = NULL,
             table.dir =NULL,
             res = NULL,
             name.res = "Country",
             fp2020.69.only = FALSE,
             all.women = FALSE,
             all.womenize.table.name = all.women,
             adjusted.medians = FALSE) {

        if (is.null(table.dir)) {
            table.dir <- file.path(getwd(), "tables/")
            dir.create(table.dir, showWarnings = FALSE)
        }
        if (is.null(output.dir)) {
            output.dir <- file.path(getwd(), "output", run.name,
                                    "/")
        }
        if (is.null(res)) {
            if (name.res == "Country") {
                if(all.women) {
                    load(file.path(output.dir, "res.country.all.women.age.ratio.rda")) # change JR, 20140418
                } else {
                    load(file.path(output.dir, "res.country.age.ratio.rda")) # change JR, 20140418
                }
                res <- res.country.age.ratio
            } else if (name.res == "UNPDaggregate") {
                ## [MCW-2018-02-14]
                ## Don't do anything if this is a one country run.
                load(file = file.path(output.dir,"mcmc.meta.rda"))
                if(mcmc.meta$general$do.country.specific.run) {
                    warning("'name.res' is '\"UNPDaggregate\"' but this is a one country run: no tables produced.")
                    return(invisible())
                } else {
                    if(all.women) {
                        if(file.exists(file.path(output.dir, "res.aggregate.all.women.age.ratio.rda"))) {
                            ## compatibility with legacy version
                            load(file.path(output.dir, "res.aggregate.all.women.age.ratio.rda"))
                        } else {
                            load(file.path(output.dir, "res.aggregate.age.ratio.rda"))
                        }
                    } else {
                        load(file.path(output.dir, "res.aggregate.age.ratio.rda")) # change JR, 20140418
                    }
                    res <- res.aggregate.age.ratio
                }
            } else {
                ## Don't do anything if this is a one country run.
                load(file = file.path(output.dir,"mcmc.meta.rda"))
                if(mcmc.meta$general$do.country.specific.run) {
                    warning("'name.res' is '", name.res, "' but this is a one country run: no tables produced.")
                    return(invisible())
                } else {
                    load(file = file.path(output.dir, paste0(name.res, ".age.ratio.rda")))
                    res <- res.aggregate.age.ratio
                }
            }
        }


        ## Replace 'median.adj' with 'qt' in names of res so that the rest of the
        ## code works as-is.
        ##
        ## _Note_:    The .csv files will have 'adj' in the name because this is
        ## taken from variable names in the data frames which are unchanged. It is
        ## up to the user to specify whether or not the .csv files are saved to a
        ## different folder via the 'table.dir' argument.
        if(adjusted.medians) names(res) <- gsub("median\\.adj", "qt", names(res))

        est.years <- as.numeric(dimnames(res$CIcountratio.Lg.Lcat.qt[[1]][[1]])[[2]])
        percentiles <- dimnames(res$CIcountratio.Lg.Lcat.qt[[1]][[1]])[[1]]
        nperc <- length(percentiles)
        G <- length(res$CIcountratio.Lg.Lcat.qt)
        CI.Lg.Lcat.qt <- res[["CIcountratio.Lg.Lcat.qt"]]

        for (i in 1:length(CI.Lg.Lcat.qt[[1]])) {
            CI.Lg.qt <- lapply(CI.Lg.Lcat.qt, function(l) l[[i]])
            estimates <- matrix(NA, nperc * G, length(est.years))
            for (g in 1:G) {
                indices <- seq((g - 1) * nperc + 1, g * nperc)
                estimates[indices, ] <- CI.Lg.qt[[g]]
            }
            ## [MCW-2016-09-06-1] :: If 'name.res' == "UNPDaggregate" don't include
            ## the iso codes in the 'results.all' data frame.
            if(name.res == "Country") {
                iso.col <- paste(rep(res$iso.g, each = nperc))

            } else  {
                iso.col <- NA             #keep it because renaming, etc. below expects this column.
            }
            results.all <- data.frame(paste(rep(names(CI.Lg.qt),
                                                each = nperc)), iso.col,
                                      paste(rep(percentiles, times = G)), estimates)

            dimnames(results.all)[[2]] <- c("Name", "Iso", "Percentile",
                                            est.years)

            if(fp2020.69.only) {
                results.all <- results.all[results.all$Iso %in% c("4", "50", "204", "64", "68", "854", "108", "116", "120", "140",
                                                                  "148", "174", "178", "384", "408", "180", "262", "818", "232",
                                                                  "231", "270", "288", "324", "624", "332", "340", "356", "360",
                                                                  "368", "404", "417", "418", "426", "430", "450", "454", "466",
                                                                  "478", "496", "508", "104", "524", "558", "562", "566", "586",
                                                                  "598", "608", "646", "678", "686", "694", "90", "706", "728",
                                                                  "144", "275", "729", "762", "626", "768", "800", "834", "860",
                                                                  "704", "732", "887", "894", "716"), ]
                fp2020 <- "_fp2020_69"
            } else fp2020 <- ""

            if(adjusted.medians && !is.null(adj.method) && adj.method != "mod_tot_unmet") adj.m <- paste0("_", adj.method)
            else adj.m <- ""

            ## Ensure ~.csv~ filenames have "aw" in them, even
            ## if ~run.name~ does not have "umw" or "mw" in it.
            if(all.womenize.table.name) {
                aw.run.name <- makeAWFileName(run.name)
            } else aw.run.name <- run.name

            ## Remove '\' from filenames
            fnm <- gsub("\\.", "_"
                       ,paste(c(unlist(strsplit(names(CI.Lg.Lcat.qt[[1]])[i]
                                               ,split = "/")))
                             ,collapse = "Over"))

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            write.csv(results.all
                     ,file = file.path(table.dir
                                      ,paste0(aw.run.name,
                                              "_", name.res, "_", "age_ratio",
                                              "_", fnm, fp2020
                                             ,".csv")), # change JR, 20140418
                      row.names = FALSE)
        }

        cat("Result tables written to", table.dir, "\n")

    }
##----------------------------------------------------------------------
##' Make output tables for age ratios
##'
##' Creates output \file{.csv} files for age ratios, e.g., proportion
##' of users that are aged 15--19.
##' @param run.name
##' @param output.dir
##' @param table.dir
##' @param res
##' @param name.res
##' @param fp2020.69.only
##' @param adjusted.medians
##' @param adj.method
##' @noRd
GetTablesChangeAgeRatios <-
    function(run.name = "test",
             output.dir,
             table.dir,
             res = NULL,
             name.res = "Country",
             fp2020.69.only = FALSE,
             all.women = FALSE,
             all.womenize.table.name = all.women,
             adjusted.medians = FALSE,
             adj.method = NULL) {
        if (is.null(table.dir)){
            table.dir <- file.path(getwd(), "tables/")
            dir.create(table.dir, showWarnings = FALSE)
        }
        if (is.null(output.dir)){
            output.dir <- file.path(getwd(), "output", run.name) # change JR, 20140418
        }
        if (is.null(res)){
            if (name.res == "Country"){
                if(all.women) {
                    load(file.path(output.dir, "res.country.all.women.age.ratio.rda")) # change JR, 20140418
                } else {
                    load(file.path(output.dir, "res.country.age.ratio.rda")) # change JR, 20140418
                }
                res <- res.country.age.ratio
            } else if (name.res == "UNPDaggregate"){
                ## [MCW-2018-02-14]
                ## Don't do anything if this is a one country run.
                load(file = file.path(output.dir,"mcmc.meta.rda"))
                if(mcmc.meta$general$do.country.specific.run) {
                    warning("'name.res' is '\"UNPDaggregate\"' but this is a one country run: no tables produced.")
                    return(invisible())
                } else {
                    load(file.path(output.dir, "res.aggregate.age.ratio.rda")) # change JR, 20140418
                    res <- res.aggregate.age.ratio
                                #print(res)
                }
            } else {
                ## Don't do anything if this is a one country run.
                load(file = file.path(output.dir,"mcmc.meta.rda"))
                if(mcmc.meta$general$do.country.specific.run) {
                    warning("'name.res' is '", name.res, "' but this is a one country run: no tables produced.")
                    return(invisible())
                } else {
                    load(file = file.path(output.dir, paste0(name.res, ".age.ratio.rda")))
                    res <- res.aggregate.age.ratio
                }
            }
        }

        change.years.names <- dimnames(res$changecountratio.Lg.Lcat.Ti[[1]][[1]])[[1]]
        infonames <- dimnames(res$changecountratio.Lg.Lcat.Ti[[1]][[1]])[[2]]
        ninfo <- length(infonames)
        G <- length(res$changecountratio.Lg.Lcat.Ti)
        CI.Lg.Lcat.Ti <- res[[c("changecountratio.Lg.Lcat.Ti")]]
        for (i in 1:length(CI.Lg.Lcat.Ti[[1]])){ # not the info i...
            CI.Lg.Ti <- lapply(CI.Lg.Lcat.Ti, function(l) l[[i]])
            estimates <- matrix(NA, length(change.years.names)*G, ninfo)
            for (g in 1:G){
                indices <- seq((g-1)*length(change.years.names) +1, g*length(change.years.names))
                estimates[indices,] <- CI.Lg.Ti[[g]]
            }
            ## [MCW-2016-09-06-3] :: If 'name.res' == "UNPDaggregate" don't include
            ## the iso codes in the 'results.all' data frame.
            if(name.res == "Country") iso.col <- paste(rep(res$iso.g, each = length(change.years.names)))
            else iso.col <- NA
            results.all <- data.frame(
                paste(rep(names(CI.Lg.Ti), each = length(change.years.names))),
                iso.col,
                paste(rep(change.years.names, times = G)),
                estimates)
            dimnames(results.all)[[2]] <- c("Name", "Iso", "Change", infonames)

            if(fp2020.69.only) {
                results.all <- results.all[results.all$Iso %in% c("4", "50", "204", "64", "68", "854", "108", "116", "120", "140",
                                                                  "148", "174", "178", "384", "408", "180", "262", "818", "232",
                                                                  "231", "270", "288", "324", "624", "332", "340", "356", "360",
                                                                  "368", "404", "417", "418", "426", "430", "450", "454", "466",
                                                                  "478", "496", "508", "104", "524", "558", "562", "566", "586",
                                                                  "598", "608", "646", "678", "686", "694", "90", "706", "728",
                                                                  "144", "275", "729", "762", "626", "768", "800", "834", "860",
                                                                  "704", "732", "887", "894", "716"), ]
                fp2020 <- "_fp2020_69"
            } else fp2020 <- ""

            ## Ensure ~.csv~ filenames have "aw" in them, even
            ## if ~run.name~ does not have "umw" or "mw" in it.
            if(all.womenize.table.name) {
                aw.run.name <- makeAWFileName(run.name)
            } else aw.run.name <- run.name

            ## Remove '\' from filenames
            fnm <- gsub("\\.", "_"
                       ,paste(c(unlist(strsplit(names(CI.Lg.Lcat.Ti[[1]])[i]
                                               ,split = "/")))
                             ,collapse = "Over"))

            ## Reduce length of filenames
        fnm <- gsub("region[s]*", "", fnm)
        fnm <- gsub("__", "_", fnm, fixed = TRUE)

            write.csv(results.all
                     ,file = file.path(table.dir
                                      ,paste0(aw.run.name,
                                              "_", name.res, "_changes_", "age_ratio",
                                              "_", fnm, fp2020
                                             ,".csv")), # change JR, 20140418
                      row.names = FALSE)
        } # end i (cat) loop

        cat("Change tables written to", table.dir, "\n")
    }

##----------------------------------------------------------------------------------
# SpitOutSomeRelevantNumbers <- function(# Spit out some relevant numbers
#   ## Info on MCMC chains and country trajectories # and world estimates
#   run.name = "test", ##<<,
#   output.dir = NULL ##<< directory where MCMC array and meta are stored, and new objects are added
#   ){
#
#
#   if (is.null(output.dir)){
#     output.dir <- file.path(getwd(), "output", run.name, "/")
#   }
#
#   #load(file = file.path(output.dir,"par.ciq.rda")) # change JR, 20140418
#   load(file = file.path(output.dir,"res.country.rda")) # change JR, 20140418
#   #load(file = file.path(output.dir,"res.aggregate.rda")) # change JR, 20140418
#   load(file = file.path(output.dir,"mcmc.meta.rda")) # change JR, 20140418
#
# #   poschange <- function(bla.i){
# #     return(paste(round(bla.i["50%"],2),
# #                  ", 95% CI (", round(bla.i["2.5%"],2), ",",
# #                  round(bla.i["97.5%"],2), "), post. prob increase ",
# #                  round(bla.i["PPPC"],3)
# #                  , sep = ""))
# #   }
# #   negchange <- function(bla.i){
# #     return(paste(round(-bla.i["50%"],2),
# #                  ", 95% CI (", -round(bla.i["2.5%"],2), ",",
# #                  round(-bla.i["97.5%"],2), "), post. prob decrease ",
# #                  round(1-bla.i["PPPC"],3)
# #                  , sep = ""))
# #   }
# #   levelcount <- function(bla.q){
# #     #NOTE: with .q no %!
# #     return(paste(round(bla.q["0.5"]),
# #                  ", 95% CI (", round(bla.q["0.025"]), ",",
# #                  round(bla.q["0.975"]), ")", sep = ""))
# #   }
# #   levelprop <- function(bla.q){
# #     #NOTE: with .q no %!
# #     return(paste(round(bla.q["0.5"],2),
# #                  ", 95% CI (", round(bla.q["0.025"],2), ",",
# #                  round(bla.q["0.975"],2), ")", sep = ""))
# #   }
#
#   file.dir <-file.path(getwd(), "relevantnumbers.txt")
#   cat("Numbers to be written to", file.dir, "\n")
#
#   cat("Info on MCMC algorithm from meta", "\n", file= file.dir, append = F)
#   cat("Number of iterations:", mcmc.meta$general$N.ITER, "\n", file=  file.dir, append = T)
#   cat("Number of chains:", length(mcmc.meta$general$ChainNums), "\n", file=  file.dir, append = T)
#   cat("Thinning:", mcmc.meta$general$N.THIN, "\n", file=  file.dir, append = T)
#   cat("Burnin:", mcmc.meta$general$N.BURNIN, "\n", file=  file.dir, append = T)
#
#   cat("Info on samples used (from mcmc.array)", "\n", file= file.dir, append = T)
#   load(file = file.path(output.dir,"mcmc.array.rda")) # change JR, 20140418
#   cat("Number of samples:", dim(mcmc.array)[[1]], "\n", file=  file.dir, append = T)
#   cat("Number of chains:", dim(mcmc.array)[[2]], "\n", file=  file.dir, append = T)
#
#   cat("Info on number of country trajectories used (and saved)", "\n", file= file.dir, append = T)
#   load(file = file.path(res.country$output.dir.countrytrajectories, "P.tp3s_country", 1, ".rda")) # change JR, 20140418
#   cat("Number of trajectories:", dim(P.tp3s)[[3]], "\n", file=  file.dir, append = T)
#
#   cat("", "\n", file=  file.dir, append = T)
#
# #   cat("Info on world estimates", "\n", file= file.dir, append = T)
# #   cat("INCREASE in total prevalence, 1990-2010:", poschange(res.aggregate$changeprop.Lg.Lcat.Ti[["World"]][["Total"]]["1990-2000",]),
# #       "\n", file = file.dir, append = T)
# #   cat("DECREASE in % unmet need, 1990-2010:", negchange(res.aggregate$changeprop.Lg.Lcat.Ti[["World"]][["Unmet"]]["1990-2000",]),
# #       "\n", file = file.dir, append = T)
# #   cat("Total MWRA with unmet need, 2010:", levelcount(
# #     res.aggregate$CIcount.Lg.Lcat.qt[["World"]][["Unmet"]][, "2010.5"]),
# #       "\n", file = file.dir, append = T)
# # cat("", "\n", file= file.dir, append = T)
#
#
#   return(invisible())
# }
#----------------------------------------------------------------------
# The End!
