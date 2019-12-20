From.Lg.Lcat.qtTo.gq <- function(# Get CI in better format for table/plot
  ### Get CI in better format for table/plot
  res.Lg.Lcat.qt, ## start
  cat, ## category to select
  year ## year to select
  ){
  est.years <- dimnames(res.Lg.Lcat.qt[[1]][[1]])[[2]]
  res.gq <- matrix(NA, length(res.Lg.Lcat.qt), length(res.Lg.Lcat.qt[[1]][[1]][,1]))
  for (g in 1:length(res.Lg.Lcat.qt)){
    #    res.gq[g,] <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
    #    l[[cat]][, est.years = year]))
    res.gq[g,] <- res.Lg.Lcat.qt[[g]][[cat]][, est.years == year]
  }
  dimnames(res.gq) <- list(names(res.Lg.Lcat.qt), dimnames(res.Lg.Lcat.qt[[1]][[1]])[[1]])
  return(res.gq)
}

#--------------------------------------------------------------------
InternalGetInfoOnChange <- function(# Find info on a change
  ### Given change
  ### find the CI for the change, and posterior prob that that change was positive.
  change.s, ##<<
  percentiles = c(0.025, 0.1, 0.5, 0.9, 0.975) ##<< percentiles to calculate CIs for
  ){
  res <- c(quantile(change.s, percentiles, na.rm = TRUE), mean(change.s>0))
  names(res)[6] <- "PPPC"
  ##value<< vector with 2.5, 10, 50, 90, 97.5 percentiles, and post prob. postive change
  return(res)
}
# test
#   InternalGetInfoOnChange(
#    change.s = seq(2,101) - seq(1,100))

#-------------------------------------------------------------------------
GetInfoChange <- function(# Find info on a change in an indicator
  ### Find the CI for the change, and posterior prob that that change was positive.
  P.yp3s, ##<< props or counts or ratios
  type.is.prop = TRUE, ##<< \code{P.yp3s} contains props?
  type.is.ratio = FALSE,        #if both 'FALSE', assumes counts
  W.y = NULL, ##<< denominator counts for unique years in 'years.change' and 'years.change2'
  years.change = matrix(c(1990.5, 2000.5,
                          2000.5, 2010.5,
                          1990.5, 2010.5),
                        3, 2, byrow = TRUE), ##<< Matrix with 2 columns, with column 1
  ## containing yyyy1 and column 2 containing yyyy2 for calculating change yyyy1-yyyy2
  years.change2 = matrix(c(2005.5, 2010.5, 2015.5,
                           2000.5, 2005.5, 2010.5,
                           1995.5, 2000.5, 2005.5,
                           1990.5, 1995.5, 2000.5,
                           1990.5, 2000.5, 2010.5),
                         5, 3, byrow = TRUE) ##<< Matrix with 3 columns, with column 1
  ## containing yyyy1, column 2 containing yyyy2 and column 3 containing yyyy3 for
  ## calculating change (yyyy2-yyyy3) - (yyyy1-yyyy2)
){
  if (ncol(years.change) != 2)
    stop("years.change must be a matrix with 2 columns.")
  if (ncol(years.change2) != 3)
      stop("years.change2 must be a matrix with 3 columns.")
                                # names same as in CIs (e.g. in F_output, construct CIs)
  ## Elements of 'P.Lcat.ts' are: [1] "Traditional" [2] "Modern" [3] "Unmet"
  if(!(type.is.ratio)) {
  P.Lcat.ts <- list(Traditional = P.yp3s[,1,],
                    Modern = P.yp3s[,2,],
                    Total = P.yp3s[,1,] + P.yp3s[,2,],
                    Unmet = P.yp3s[,3,],
                    TotalPlusUnmet = (P.yp3s[,1,]+P.yp3s[,2,]+P.yp3s[,3,]),
                    TradPlusUnmet = (P.yp3s[,1,]+P.yp3s[,3,])
                    )
  } else {
      P.Lcat.ts <- list(MetDemand = (P.yp3s[, "Traditional", ] + P.yp3s[, "Modern", ]) /
                            (P.yp3s[, "Traditional", ] + P.yp3s[, "Modern", ] + P.yp3s[, "Unmet", ])
                       ,MetDemModMeth = P.yp3s[,"Modern",] /
                            (P.yp3s[, "Traditional", ] + P.yp3s[, "Modern", ] + P.yp3s[, "Unmet", ])
                       ,ModernOverTotal = P.yp3s[,"Modern",] /
                            (P.yp3s[, "Traditional", ] + P.yp3s[, "Modern", ])
                       ,Z = P.yp3s[, "Unmet", ] /
                            (matrix(W.y, nrow = length(W.y), ncol = ncol(P.yp3s[, 1, ])) -
                             P.yp3s[, "Traditional", ] - P.yp3s[, "Modern", ] - P.yp3s[, "Unmet", ])
                        )
      ## Need to handle cases where denominator counts are not available (e.g.,
      ## Andorra). For counts and props, entries in 'P.Lcat.ts' are all zero in
      ## this case, so mimic that behaviour for ratios.
      if(isTRUE(all(W.y == 0))) {
          P.Lcat.ts <- lapply(P.Lcat.ts, function(z) array(0, dim = dim(z)))
      }
      }

  years.all <- as.numeric(dimnames(P.yp3s)[[1]])
  nchanges <- nrow(years.change) + nrow(years.change2)
  change.Lcat.Ti <- list()
  for (catname in names(P.Lcat.ts)){ #'catname' is 'Traditional', 'Modern', etc.
      P.ts <- P.Lcat.ts[[catname]]
    # make a data frame for prop and numbers
    ## res.Ti <- as.data.frame(matrix(NA, nchanges, 6)) # percentiles and PPPC
    # change JR, 20140317
    res.Ti <- rbind(
      t(apply(years.change, 1, function(years, P.ts, years.all)
        InternalGetInfoOnChange(P.ts[years.all == years[2],] - P.ts[years.all == years[1],]), P.ts, years.all)),
      t(apply(years.change2, 1, function(years, P.ts, years.all)
        InternalGetInfoOnChange(
          (P.ts[years.all == years[3],] - P.ts[years.all == years[2],])
          -(P.ts[years.all == years[2],] - P.ts[years.all == years[1],])), P.ts, years.all)))
    dimnames(res.Ti)[[1]] <- c(apply(years.change, 1, function(years) paste0(floor(years[1]), "-", floor(years[2]))),
                               apply(years.change2, 1, function(years)
                                 paste0("Change (", floor(years[3]), "-", floor(years[2]), ") - (",
                                        floor(years[2]), "-", floor(years[1]), ")")))
    #names(res.Ti)[[1]] <- should be okay already?
    #colnames(res.Ti) <- c("Aggregatename", "Change", "Low95CI", "median", "up95CI", "PPPC")
    change.Lcat.Ti[[catname]] <- res.Ti
  } # end cats
  return(change.Lcat.Ti)
}

#--------------------------------------------------------------------------------
FromCIpropToCIcount <- function(# Get counts given proportions and number of MWRA
  ### Get counts given proportions and number of MWRA
  CIprop.Lg.Lcat.qt, ##<< props
  W.Lg.t ##<<  counts
  ){
  nperc <- dim(CIprop.Lg.Lcat.qt[[1]][[1]])[1]
  nyears <- dim(CIprop.Lg.Lcat.qt[[1]][[1]])[2]
  CIcount.Lg.Lcat.qt <- list()
  for (g in names(CIprop.Lg.Lcat.qt)){
    CIcount.Lg.Lcat.qt[[g]] <- list()
    for (catname in names(CIprop.Lg.Lcat.qt[[g]])){
      CIcount.Lg.Lcat.qt[[g]][[catname]] <-
        t(matrix(rep(W.Lg.t[[g]], nperc), nyears, nperc))*CIprop.Lg.Lcat.qt[[g]][[catname]]
    }
    # not sure why this is needed
    #names(CIcount.Lg.Lcat.qt[[g]]) <- catnames
  }
  return(CIcount.Lg.Lcat.qt)
}
#----------------------------------------------------------------------
# The End!
