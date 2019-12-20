#----------------------------------------
# F_validation
# Leontine Alkema
#----------------------------------------

# note: PlotValidationResults (at end) is the main function, that calls this function:
PlotValResults <- function(# Summarize and plot validation results
  ### Summarize and plot validation results for one type of left-out observation (e.g. unmet, total, trad or modern),
  ### Note: returns summary results
  include.j, ##<< vector with logicals, include observation or not?
  P.j,##<< Percentiles to be summarized
  ## (element of P.jp data frame constructed in \code{\link{GetPercentilesLeftOut}})
  nameplot, ##<< Main for overview figure
  error.est.j, ##<< Errors to be summarized
  country.info, ##<< Object from class country.info
  winbugs.data,
  data,
  ## (element of error.est.jp data frame constructed in \code{\link{GetPercentilesLeftOut}})
  validation.list ##<< Info about validation exercise (from mcmc.meta)
  ){
    ## specify subgroups of left-out observations:
    if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        ## 'data' , 'country.info', already the concatenation of training and test data (done in PlotValidationResults()).
        getc.j.test <-
            c(winbugs.data$getc.j
             ,winbugs.data$getc.j.test
              )
        reg.j <- country.info$reg.c[getc.j.test]
        ssa.j <- country.info$ssa.c[getc.j.test]
        mics.j <- ifelse(data$source.j=="MICS", T, F)
        dhs.j <- ifelse(data$source.j=="DHS", T, F)
        dev.j <- country.info$dev.c[getc.j.test]
        if("sex.ac.unm.c" %in% colnames(country.info)) {
            sex.ac.j <- country.info$name.sex.ac.unm.c[getc.j.test]
            }
    } else {
        reg.j <-  country.info$reg.c[winbugs.data$getc.j]
        ssa.j <- country.info$ssa.c[winbugs.data$getc.j]
        mics.j <- ifelse(data$source.j=="MICS", T, F)
        dhs.j <- ifelse(data$source.j=="DHS", T, F)
        dev.j <- country.info$dev.c[winbugs.data$getc.j]
        if("sex.ac.unm.c" %in% colnames(country.info)) {
            sex.ac.j <- country.info$name.sex.ac.unm.c[winbugs.data$getc.j]
            }
    }

  select.ji <- cbind(rep(TRUE, length(ssa.j)), ssa.j=="No", ssa.j=="Yes",
                     dhs.j, mics.j, !mics.j & !dhs.j, dev.j=="Poor", dev.j=="Rich")
    namesselect <- c("All", "OutsideSSA", "SSA", "DHS", "MICS", "Other", "Poor", "Rich")

    if("sex.ac.unm.c" %in% colnames(country.info)) {
        select.ji <- cbind(select.ji, sex.ac.j == 0, sex.ac.j == 1)
        namesselect <- c(namesselect, "LowSexlActy", "HighSexlActy")
    }
  I <- dim(select.ji)[2] # number of subgroups
  table.ix <- NULL # x refers to table columns (outputs)
  table2.ix <- NULL # x refers to table columns (outputs)
  for (i in 1:I){
  #  nobs.i[i] <- sum(!is.na(Psselect[select.ji[,i]]))
    select <- select.ji[,i]& (include.j == TRUE)
    table.ix <- rbind(table.ix, GetRow(P.c = P.j[select]))
    table2.ix <- rbind(table2.ix, c(mean(error.est.j[select], na.rm= T),
                                  median((error.est.j[select]), na.rm= T),
                                     median(abs(error.est.j[select]), na.rm= T),
                      mean(P.j[select] <0.5, na.rm = T) )) # if P.j is less than 0.5, the obs fell below median
  }
  nobs.i <- table.ix[, "# Obs"]
  rownames(table.ix) <- paste0(namesselect, " (", nobs.i, ")")
  rownames(table2.ix) <- paste0(namesselect, " (", nobs.i, ")")
  colnames(table2.ix) <- c("Mean error", "Median error", "Median abs error", "Prop below median")

  par(mfrow = c(2,4), cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)

  # 1. inside 80%
  par(mar = c(5,12,5,1))
  plot(1, type = "n", ylim = c(I, 0), main = "" , xlim = c(0.5,1),
         xlab = "Coverage of 80% PI", ylab = "", yaxt = "n")
  # for inside 80
  for (i in 1:I){
    res1 <- qbinom(p = c(0.025, 0.975), size = nobs.i[i], prob = 0.8)/nobs.i[i]
    segments(res1[1], i, res1[2], i, lwd = 10, col = "grey")
    points(i~table.ix[i, "Within 80% CI"], col = 1, pch = 19, lwd = 3)
  }
  abline(v = 0.8)
  axis(2, las = 2, labels = rownames(table.ix), at  = seq(1, I))

  par(mar = c(5,5,5,1))
  # 2. outside 80% CI
  plot(1, type = "n", ylim = c(I,0), xlim = c(0,0.3), xlab = "Proportion below/above 80% PI",
       ylab = "", yaxt = "n", main = nameplot)
  for (i in 1:I){
    res1 <- qbinom(p = c(0.025, 0.975), size = nobs.i[i], prob = 0.1)/nobs.i[i]
    segments(res1[1], i, res1[2], i, lwd = 10, col = "grey")
    points((i+0.1)~table.ix[i, "Below 80% CI"], col = 2, pch = 19, lwd = 3)
    points((i-0.1)~table.ix[i, "Above 80% CI"], col = 1, pch = 19, lwd = 3)
  }
  axis(1, las = 2, labels = NULL, at  = i)
  abline(v = 0.1)
  legend("bottomright", legend = c("Above", "Below"), col = c(1,2), lwd = 3, pch = 19, lty = -1, bty = "n")

  # 3. inside 95% CI
  plot(1, type = "n", ylim = c(I,0),
        main = "", xlim = c(0.7,1),
         xlab = "Coverage of 95% PI", ylab = "", yaxt = "n")
  for (i in 1:I){
    res1 <- qbinom(p = c(0.025, 0.975), size = nobs.i[i], prob = 0.95)/nobs.i[i]
    segments(res1[1], i, res1[2], i, lwd = 10, col = "grey")
     points(i~table.ix[i, "Within 95% CI"], col = 1, pch = 19, lwd = 3)
  }
  abline(v = 0.95)

  # 4. outside 95%
  #par(mar = c(5,5,5,12))
  plot(1, type = "n", ylim = c(I,0), xlim = c(0,0.3), xlab = "Proportion below/above 95% PI",
       ylab = "", yaxt = "n")
  for (i in 1:I){
    res1 <- qbinom(p = c(0.025, 0.975), size = nobs.i[i], prob = 0.025)/nobs.i[i]
    segments(res1[1], i, res1[2], i, lwd = 10, col = "grey")
    points((i+0.1)~table.ix[i, "Below 95% CI"],col = 2, pch = 19, lwd = 3)
    points((i-0.1)~table.ix[i, "Above 95% CI"],col = 1, pch = 19, lwd = 3)
  }
  axis(1, las = 2, labels = NULL, at  = i)
  abline(v = 0.025)
  legend("bottomright", legend = c("Above", "Below"), col = c(1,2), lwd = 3,
         pch = 19, lty = -1, bty = "n")

  hist(P.j[include.j], freq = FALSE, main = "Percentiles", xlab = "P")
    abline(h=1, col = 2)
    if(sum(ssa.j=="Yes"&include.j) > 0) {
        hist(P.j[ssa.j=="Yes"&include.j], freq = FALSE, breaks = 10, main = "Percentiles SSA", xlab = "P")
  abline(h=1, col = 2)
        } else plot(1)
  hist(error.est.j[include.j], freq = T, breaks = 20, main = "Errors", xlab = "Error")
  abline(v=0, col = 1)
  abline(v = mean(error.est.j[include.j], na.rm = T), col = 3)
  abline(v = median(error.est.j[include.j], na.rm = T), col = 4)
#  hist(st.error.j, freq = FALSE, main = "St. Errors", xlab = "St. Error")
                                #  abline(v=0, col = 2)
        if(sum(ssa.j=="Yes"&include.j) > 0) {
  hist(error.est.j[ssa.j=="Yes" & include.j], freq = T,  breaks = 20, main = "Errors SSA", xlab = "St. Error")
  abline(v=0, col = 1)
  abline(v = mean(error.est.j[ssa.j=="Yes" & include.j], na.rm = T), col = 3)
  abline(v = median(error.est.j[ssa.j=="Yes" & include.j], na.rm = T), col = 4)
        } else plot(1)
  ##value<<
  return(list(table.ix = table.ix,##<< Summary table for P.j
              table2.ix = table2.ix ##<< Summary table for error.est.j
              ))
}

#----------------------------------------------------------------------------------
GetPercentilesLeftOut <- function(
  ### Calculate where left-out obs falls in pred. posterior distribution:
  ### P.j  =(approx) P(Ynew <= yleftout) = percentile of left-out obs j in posterior predictive distribution
  ### and errors:
  ### error.est.j  = y.j - median(Ynew.j).
                                  data, #<- mcmc.meta$data.raw$data
                                  mcmc.array,
                                  winbugs.data,
                                  validation.list){
    ## If doing 'at.random.no.data' validation exercise do 'else' below.

    if(!isTRUE(validation.list$at.random.no.data) && !isTRUE(validation.list$leave.iso.out)) {

    Punmet.j <- error.unmet.j <- error.est.unmet.j <-
    Ptot.j <- Ptrad.j <- Pmodern.j <-
        error.est.trad.j <-
        error.est.modern.j <-
            error.est.tot.j <- rep(NA, winbugs.data$J)
  # if trad, mod and unmet were left out:
  if (!is.null(winbugs.data$getj.test.k)){
      for (j in winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown]){
      tradobs <- data$props.trad.j[j]
      modobs <- data$props.modern.j[j]
      totCPobs <-  tradobs + modobs
      parname.tr <- paste0("pred.logratio.ytrad.j[", j, "]")
      parname.mod <- paste0("pred.logratio.ymodern.j[", j, "]")
      a <- exp(c(mcmc.array[, , parname.tr])) + exp(c(mcmc.array[, , parname.mod]))
      totCP <- a/(a+1)
      trad <- (1-totCP)*exp(c(mcmc.array[, , parname.tr]))
      mod <- (1-totCP)*exp(c(mcmc.array[, , parname.mod]))
      Ptrad.j[j] <- mean(trad <= tradobs)
      Pmodern.j[j] <- mean(mod <= modobs)
      Ptot.j[j] <- mean(totCP <= totCPobs)
      error.est.trad.j[j] <- tradobs - median(trad)
      error.est.modern.j[j] <- modobs - median(mod)
      error.est.tot.j[j] <- totCPobs - median(totCP)
#      # from mcmc sample directly, using median of mean (expected to be slightly different)
#      parname.tr <- paste0("q.trad.j[", j, "]")
#      parname.mod <- paste0("q.modern.j[", j, "]")
#      error.trad.j[j] <- tradobs - median(c(mcmc.array[, , parname.tr]))
#      error.modern.j[j] <- modobs - median(c(mcmc.array[, , parname.mod]))
#      error.tot.j[j] <- totCPobs - median(c(mcmc.array[, , parname.tr])+c(mcmc.array[, , parname.mod]))
      if (is.element(j, winbugs.data$getj.test.unmet.k)){
          unmetobs <- data$props.unmet.j[j]
          parname <- paste0("pred.logitratio.yunmet.j[", j, "]")
          # use totCP to get sampled unmet
          unmet <- (1-totCP)*invlogit(c(mcmc.array[, , parname]))
          Punmet.j[j] <- mean(unmet <= unmetobs)
          error.est.unmet.j[j] <- unmetobs - median(unmet)
#          parname <- paste0("q.unmet.j[", j, "]")
#          error.unmet.j[j] <- unmetobs - median(c(mcmc.array[, , parname]))
      }
    } #end j-loop
  } else { # unmet only
      for (j in winbugs.data$getj.test.unmet.k){
          tradobs <- data$props.trad.j[j]
          modobs <- data$props.modern.j[j]
          totCPobs <-  tradobs + modobs
          parname.tr <- paste0("pred.logratio.ytrad.j[", j, "]")
          parname.mod <- paste0("pred.logratio.ymodern.j[", j, "]")
          a <- exp(c(mcmc.array[, , parname.tr])) + exp(c(mcmc.array[, , parname.mod]))
          totCP <- a/(a+1)

          # copied from above
          unmetobs <- data$props.unmet.j[j]
          parname <- paste0("pred.logitratio.yunmet.j[", j, "]")
          # use totCP to get sampled unmet
          unmet <- (1-totCP)*invlogit(c(mcmc.array[, , parname]))
          Punmet.j[j] <- mean(unmet <= unmetobs)
          error.est.unmet.j[j] <- unmetobs - median(unmet)
#         parname <- paste0("q.unmet.j[", j, "]")
#          error.unmet.j[j] <- unmetobs - median(c(mcmc.array[, , parname]))
      } # end j-loop
  }# end else
    } else if(isTRUE(validation.list$at.random.no.data)) { ## end '!isTRUE(validation.list$at.random.no.data)

        ## If doing 'at.random.no.data' validation exercise:

        Punmet.j <- error.unmet.j <- error.est.unmet.j <-
            Ptot.j <- Ptrad.j <- Pmodern.j <-
                error.est.trad.j <-
                    error.est.modern.j <-
                        error.est.tot.j <- rep(NA, winbugs.data$J + winbugs.data$J.test)

        ## Breakdown into mod, trad, or for tot only:
        for (j in winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown]){
            tradobs <- winbugs.data$data.test$props.trad.j[j - winbugs.data$J]
            modobs <- winbugs.data$data.test$props.modern.j[j - winbugs.data$J]
            totCPobs <-  tradobs + modobs
            parname.tr <- paste0("pred.logratio.ytrad.j[", j, "]")
            parname.mod <- paste0("pred.logratio.ymodern.j[", j, "]")
            a <- exp(c(mcmc.array[, , parname.tr])) + exp(c(mcmc.array[, , parname.mod]))
            totCP <- a/(a+1)
            trad <- (1-totCP)*exp(c(mcmc.array[, , parname.tr]))
            mod <- (1-totCP)*exp(c(mcmc.array[, , parname.mod]))
            Ptrad.j[j] <- mean(trad <= tradobs)
            Pmodern.j[j] <- mean(mod <= modobs)
            Ptot.j[j] <- mean(totCP <= totCPobs)
            error.est.trad.j[j] <- tradobs - median(trad)
            error.est.modern.j[j] <- modobs - median(mod)
            error.est.tot.j[j] <- totCPobs - median(totCP)

            ## Unmet need
            if (is.element(j, winbugs.data$getj.test.unmet.k)){
                unmetobs <- winbugs.data$data.test$props.unmet.j[j - winbugs.data$J]
                parname <- paste0("pred.logitratio.yunmet.j[", j, "]")
                unmet <- (1-totCP)*invlogit(c(mcmc.array[, , parname]))
                Punmet.j[j] <- mean(unmet <= unmetobs)
                error.est.unmet.j[j] <- unmetobs - median(unmet)
            }
        }
        ## No breakdown obs

        for(j in winbugs.data$getj.test.k[(winbugs.data$n.test.breakdown + 1):(winbugs.data$J.test)]){
            totCPobs <-  winbugs.data$data.test$props.tot.j[j - winbugs.data$J]
            parname.tot <- paste0("pred.logit.ytot.j[", j, "]")
            a <- exp(mcmc.array[, , parname.tot])
            totCP <- a/(a+1)
            Ptot.j[j] <- mean(totCP <= totCPobs)
            error.est.tot.j[j] <- totCPobs - median(totCP)
        }
    } else if (isTRUE(validation.list$leave.iso.out)) {
        ## No 'unmet' or 'tot'
        Ptot.j <- Ptrad.j <- Pmodern.j <-
            error.est.trad.j <-error.est.tot.j <-
                error.est.modern.j <-rep(NA, winbugs.data$J + winbugs.data$J.test)

        ## Breakdown into mod, trad, or for tot only:
        for (j in winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown]){
            tradobs <- winbugs.data$data.test$props.trad.j[j - winbugs.data$J]
            modobs <- winbugs.data$data.test$props.modern.j[j - winbugs.data$J]
            totCPobs <-  tradobs + modobs
            parname.tr <- paste0("pred.logratio.ytrad.j[", j, "]")
            parname.mod <- paste0("pred.logratio.ymodern.j[", j, "]")
            a <- exp(c(mcmc.array[, , parname.tr])) + exp(c(mcmc.array[, , parname.mod]))
            totCP <- a/(a+1)
            trad <- (1-totCP)*exp(c(mcmc.array[, , parname.tr]))
            mod <- (1-totCP)*exp(c(mcmc.array[, , parname.mod]))
            Ptrad.j[j] <- mean(trad <= tradobs)
            Pmodern.j[j] <- mean(mod <= modobs)
            Ptot.j[j] <- mean(totCP <= totCPobs)
            error.est.trad.j[j] <- tradobs - median(trad)
            error.est.modern.j[j] <- modobs - median(mod)
            error.est.tot.j[j] <- totCPobs - median(totCP)

            ## ## Unmet need
            ## if (is.element(j, winbugs.data$getj.test.unmet.k)){
            ##     unmetobs <- winbugs.data$data.test$props.unmet.j[j - winbugs.data$J]
            ##     parname <- paste0("pred.logitratio.yunmet.j[", j, "]")
            ##     unmet <- (1-totCP)*invlogit(c(mcmc.array[, , parname]))
            ##     Punmet.j[j] <- mean(unmet <= unmetobs)
            ##     error.est.unmet.j[j] <- unmetobs - median(unmet)
            ## }
        }
        ## ## No breakdown obs
        ## for(j in winbugs.data$getj.test.k[(winbugs.data$n.test.breakdown + 1):(winbugs.data$J.test)]){
        ##     totCPobs <-  winbugs.data$data.test$props.tot.j[j - winbugs.data$J]
        ##     parname.tot <- paste0("pred.logit.ytot.j[", j, "]")
        ##     a <- exp(mcmc.array[, , parname.tot])
        ##     totCP <- a/(a+1)
        ##     Ptot.j[j] <- mean(totCP <= totCPobs)
        ##     error.est.tot.j[j] <- totCPobs - median(totCP)
        ## }
    }

  ##value<< List with data.frames P.jp and error.est.jp
    ## with percentiles and errors respectively as explained in the description,
    ## where j refers to the observation index, and p
    ## refers to trad, modern, tot, unmet.

    if(isTRUE(validation.list$leave.iso.out)) {
        return(list(P.jp =
               data.frame(Ptrad.j,
                      Pmodern.j,
                      Ptot.j),
          error.est.jp = data.frame(error.est.trad.j,
                                   error.est.modern.j,
                                    error.est.tot.j
                                    )
          ))
        } else {

  return(list(P.jp =   # ##<<P.jp is a data frame with
    # ##describe<<
               data.frame(Ptrad.j, ##<<,
                      Pmodern.j, ##<<,
                      Ptot.j,##<<,
                      Punmet.j##<<,
                           ),
    # ##end<<

#           error.jp = data.frame(error.trad.j,  ##<<,
#                        error.modern.j,  ##<<,
#                                 error.tot.j, ##<<,
#                                 error.unmet.j ##<<,
#                        ),


          error.est.jp = data.frame(error.est.trad.j,
                                   error.est.modern.j,
                                    error.est.tot.j,
                                    error.est.unmet.j
                                    )
          ))
            }
}

#------------------------------------------------------------------------------
GetRow <- function(# Summarize proportion of left-out observations outside various PIs
  ###  Summarize proportion of left-out observations outside various PIs
  P.c ##<< Vector with percentiles (see \code{\link{GetPercentilesLeftOut}})
  ){
  out.in.CI95 <- round(OutsideAndInBounds(P.c = P.c),3)
  out.in.CI80 <- round(OutsideAndInBounds(P.c = P.c, CIlow = 0.1, CIup = 0.9),3)
  n <- sum(!is.na(P.c))
  res <- data.frame(n, out.in.CI80, out.in.CI95)
  colnames(res) <- c("# Obs",
        "Below 80% CI", "Within 80% CI", "Above 80% CI",
        "Below 95% CI", "Within 95% CI", "Above 95% CI")
  ##value<< Data frame with one row and columns
  ##c("# Obs",  "Below 80% CI", "Within 80% CI", "Above 80% CI",
  ##      "Below 95% CI", "Within 95% CI", "Above 95% CI")
  return(res)
}

#------------------------------------------------------------------------------
OutsideAndInBounds <- function(# Find proportion of left-out observations outside a specific PI
  ### Find proportion of left-out observations outside a specific PI
  P.c, ##<< Vector with percentiles (see \code{\link{GetPercentilesLeftOut}})
  CIlow = 0.025, ##<< Lower bound PI
  CIup = 0.975 ##<< Upper bound PI
  ){
  res <- c(mean(P.c < CIlow, na.rm = T),
          mean((P.c >= CIlow) & (P.c <= CIup), na.rm = T),
          mean(P.c > CIup, na.rm = T))
  ##value<<
  ## ("Below CI","In CI","Above CI")
  return(data.frame("Below CI" = res[1], "In CI" = res[2], "Above CI" = res[3]))
}

#----------------------------------------------------------------------------------
GetTraining <- function (# Construct training sets for validation exercise
  ### Construct training sets for validation exercise
  ### Choose one option from (at.random, at.end, exclude.unmet.only)
                         data, winbugs.data = NULL, ##<< winbugs.data needs to be provided when leaving out obs at the end
                         country.info,
  validation.list,
  leave1 = FALSE, ##<< Not yet implemented
  at.random.min.c = 1, at.end.not.1.obs.c = FALSE
  ,seed = 12345##<< Set seed when sampling observation to leave out
  ) {

    ##
    ## CHANGES (2017):
    ## --------------
    ##
    ## [MCW-2017-02-14-2] :: Argument 'at.random.no.data = FALSE' added to
    ## perform validation for countries with no data. Countries /with/ data are
    ## randomly selected and all of their observations are left out.
    ##

  # note: make sure training/test sets are not empty for breakdown/unmet
  # and that the training set is not empty for total
    set.seed(seed)

    at.random <- validation.list$at.random
    at.random.test.prop <- validation.list$at.random.test.prop
    at.end <- validation.list$at.end
    at.random.no.data <- validation.list$at.random.no.data
    at.random.no.data.strata <- validation.list$at.random.no.data.strata
    at.random.no.data.test.prop <- validation.list$at.random.no.data.test.prop
    year.cutoff <- validation.list$year.cutoff
    exclude.unmet.only <- validation.list$exclude.unmet.only
    exclude.unmet.only.test.prop <- validation.list$exclude.unmet.only.test.prop
    leave.iso.out <- validation.list$leave.iso.out
    leave.iso.out.iso.test <-  validation.list$leave.iso.out.iso.test

    J <- nrow(data)
    iso.c <- unique(data$iso.j)
    C <- length(iso.c)

    if (at.random){
        ## 20% at random (from break-down into modern/trad only) (ok if some
        ## countries end up without observations in training, they're still in
        ## test).
        ##
        ## Countries having all data removed has big impact for UWRA as
        ## data are fewer and countries with 1 data point are common. Ensure
        ## that each country has at least 'at.random.min.c' data points in
        ## training set.
        iso.all <- as.numeric(unique(data$iso.j))
        ## only breakdown obs
        na.modern.TF <- is.na(data$props.modern.j)
        na.modern.j <- (1:J)[na.modern.TF]
        iso.keep <-             #the ones with so few obs
            as.numeric(subset(as.data.frame(table(data[!na.modern.TF, "iso.j"])
                                           ,stringsAsFactors = FALSE)
                             ,Freq <= at.random.min.c)$Var1)
        iso.keep.j <- unique(c((1:J)[data$iso.j %in% iso.keep], na.modern.j))
        eligible.j <-
            (1:J)[!((1:J) %in% iso.keep.j | (1:J) %in% na.modern.j)]
        M <- length(eligible.j)
        M.prop <-
            ((1 - at.random.test.prop) * nrow(data) - (nrow(data) - M)) / M
                                #prop. to retain from eligible.j
        n.training.breakdown <- max(1, min(M-1, round(M.prop * M)))
        ## make sure unmet test or traning set is not empty (else loop does not work)
        unmet.empty <- TRUE
        ## at least 'at.random.min.c'
        min.not.satisfied <- TRUE
        ## Limit the number of tries
        max.tries <- 1000
        tries.so.far <- 0
        while ((unmet.empty || min.not.satisfied) && tries.so.far < max.tries) {
            tries.so.far <- tries.so.far + 1

            sample.j <- sample(eligible.j, size = n.training.breakdown)
            getj.training.k <- sort(c(iso.keep.j, sample.j))

            iso.all.intraining <- iso.all %in% data[getj.training.k,]$iso.j
            if(isTRUE(all(iso.all.intraining))) {
                sample.iso.freq <-
                    as.data.frame(table(data[getj.training.k, "iso.j"]))
            less.than.min <-
                sample.iso.freq[sample.iso.freq$Freq < at.random.min.c,]$Var1
                if(identical(length(less.than.min), 0L)) {
                    min.not.satisfied <- FALSE
                } else { min.not.satisfied <- TRUE }
            } else { min.not.satisfied <- TRUE }

            if (sum(!is.na(data$props.unmet.j[getj.training.k])) !=0 &
                sum(!is.na(data$props.unmet.j[-getj.training.k])) !=0){
                unmet.empty <- FALSE
            } else { unmet.empty <- TRUE }
        }
        if((unmet.empty || min.not.satisfied) && tries.so.far >= max.tries) {
            stop("Unable to a draw training set that satisfies the requirements in after ", max.tries, " attempts.")
        }
    }
    if (at.end){
        ## leave out obs after AND IN year.cutoff
        getj.training.k <- seq(1, length(data$years.j))[ (data$years.j <= year.cutoff)]
        if(at.end.not.1.obs.c) { #put all countries with one obs only in training
            c.tbl <- with(data, table(iso.j))
            c.tbl.1 <- c.tbl[c.tbl == 1]
            getj.training.k <-
                seq(1, length(data$years.j))[data$years.j <= year.cutoff | data$iso.j %in% names(c.tbl.1)]
            }
        ## make sure there's one obs in test/training for unmet, and one in training for total
        ## (note: test set is not constructed for total)
    if( sum(!is.na(data$props.unmet.j[getj.training.k])) == 0){
          print("Warning: training set unmet is empty")
    }
    if( sum(!is.na(data$props.unmet.j[-getj.training.k])) == 0){
          print("Warning: test set unmet is empty")
    }
    if( sum(!is.na(data$props.tot.j[getj.training.k])) == 0){
          print("Warning: training set tot is empty")
    }
  }
  if (exclude.unmet.only){
    ##details<<  If \code{exclude.unmet.only}, leave out all data in round(20%  no countries with data)
    n.unmet.c <- rep(NA, C)
    for (c in 1:C){
      n.unmet.c[c] <- sum(!is.na(data$props.unmet.j[data$iso.j == iso.c[c]]))
    }
    ncountrieswithdata <- sum(n.unmet.c !=0) # 108 countries
    nleaveout <- round(0.2*ncountrieswithdata)
    # sample countries to leave out
    indicesofcountriestoleaveout <-  sample(seq(1,C)[n.unmet.c>0], nleaveout)
    getj.training.k <- seq(1, length(data$years.j))[ is.element(data$iso.j, iso.c[-indicesofcountriestoleaveout])]
  }

    if(at.random.no.data) {
        n.training <- max(1, min(C - 1, round((1 - at.random.no.data.test.prop) * C))) #irrespective of breakdown
        unmet.empty <- TRUE
        if(is.null(at.random.no.data.strata)) {
            while(unmet.empty) {
                get.training.c <- iso.c[sample(1:C, n.training)]
                idx <- data$iso.j %in% get.training.c
                getj.training.k <- (1:J)[idx]
                unmet.train <- sum(!is.na(data$props.unmet.j[idx]))
                unmet.test <-  sum(!is.na(data$props.unmet.j[-idx]))
                if(unmet.train != 0 && unmet.test != 0) unmet.empty <- FALSE
            }
            ## if stratified
        } else {
            ## Read in the countryinfo
           if(!all(validation.list$at.random.no.data.strata %in% colnames(country.info))) {
                stop("Not all 'at.random.no.data.strata' are in 'regioninfo.csv'.")
            }
            iso.c <- intersect(iso.c, country.info$ISO.code)
            strata <-
                country.info[country.info$ISO.code %in% iso.c
                            ,c("ISO.code"
                              ,validation.list$at.random.no.data.strata
                               )
                            ,drop = FALSE]
            ## Must be at least two countries in strata to take some out.
            s.tbl <-
                as.data.frame(table(strata[,validation.list$at.random.no.data.strata])
                            ,stringsAsFactors = TRUE)
            if(identical(ncol(strata), 2L)) colnames(s.tbl)[1] <- colnames(strata)[2]
            strata <- merge(s.tbl, strata)
            strata <- strata[strata$Freq > 2,]
            tries <- 0
            while(unmet.empty) {
                if(tries > 99) stop("Have tried to find training set 100 times and have not succeeded. Please reduce, or change, the strata.")
                get.training.c <-
                    tapply(strata$ISO.code, strata[,validation.list$at.random.no.data.strata]
                          ,FUN = function(z) {
                              sample(z, size = (1 - exclude.unmet.only.test.prop) * length(z))
                          }
                          )
                idx <- data$iso.j %in% unlist(get.training.c)
                getj.training.k <- (1:J)[idx]
                unmet.train <- sum(!is.na(data$props.unmet.j[idx]))
                unmet.test <-  sum(!is.na(data$props.unmet.j[-idx]))
                if(unmet.train != 0 && unmet.test != 0) unmet.empty <- FALSE
                tries <- tries + 1
        }
        }}
    if(leave.iso.out) {
        ## 'data$iso.j' is made numeric in 'ReadDataAll()' so make sure
        ## 'leave.iso.out.iso.test' matches.
        if(is.factor(leave.iso.out.iso.test)) {
            leave.iso.out.iso.test <-
                levels(leave.iso.out.iso.test)[leave.iso.out.iso.test]
        }
        leave.iso.out.iso.test <-
            as.numeric(leave.iso.out.iso.test)
        if(!(leave.iso.out.iso.test %in% data$iso.j)) {
            stop("No country in input data has ISO Code '", leave.iso.out.iso.test, "'; please choose a different one for the test country.")
        } else {
            getj.training.k <- which(!(data$iso.j %in% leave.iso.out.iso.test))
        }
    }
  if (leave1) {
    # leave out all observations but one in set of countries with > 1 obs.
    # not yet implemented!
  }
  # save(getj.training.k, file = "getj.training.k.rda")
  # Note: reset seed after calling this function?
  ##value<< vector with indices for training set (getj.training.k)
  return(getj.training.k)
}
#----------------------------------------------------------
PlotValidationResults <- function(# Plot lots of results!
  ### Wrapper function to plot lots of results.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  fig.dir = NULL, ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
  #  plot.ind.country.results = FALSE ##<< Create zillion plots for all countries?
  ## If TRUE, plots are saved in subdirectory "country.plots" in fig.dir.
  plot.CIs  = TRUE, # create contr use overview plots for all countries?
  return.res.paper = FALSE,     #[MCW-2017-03-20-1] :: Setting to 'TRUE' creates data frame with all validation results stored.
  keep.all = FALSE,
  UWRA = FALSE                  # unmarried women run?
  ){
    if (is.null(fig.dir)){
        fig.dir <- file.path(getwd(), "fig/")
        dir.create(fig.dir, showWarnings = FALSE)
    }
                                # put separate?
    if (is.null(output.dir)){
        output.dir <- file.path(getwd(), "output", run.name, "/")
    }

    load(file = file.path(output.dir,"res.country.rda")) # change JR, 20140418
    load(file = file.path(output.dir,"mcmc.meta.rda")) # change JR, 20140418
    validation <- !is.null(mcmc.meta$validation.list)
    validation.list <- mcmc.meta$validation.list

    if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
        mcmc.meta$data.raw$data <- rbind(mcmc.meta$data.raw$data
                                        ,mcmc.meta$winbugs.data$data.test)
        mcmc.meta$data.raw$country.info <-
            rbind(mcmc.meta$data.raw$country.info
                 ,mcmc.meta$data.raw$country.info.no.data)
        C <- mcmc.meta$winbugs.data$C + mcmc.meta$winbugs.data$C.no.data
        getc.j.test <-
            c(rep(NA, mcmc.meta$winbugs.data$J)
             ,mcmc.meta$winbugs.data$getc.j.test
              )
    } else {
        C <- mcmc.meta$winbugs.data$C
        getc.j.test <- mcmc.meta$winbugs.data$getc.j
        }
    data <- mcmc.meta$data.raw$data
    country.info <- mcmc.meta$data.raw$country.info
    region.info <- mcmc.meta$data.raw$region.info
                                #This is already the union of regions for
                                #countries with and without data
    winbugs.data <- mcmc.meta$winbugs.data

    ## Test set proportion
    test.prop <-
        with(validation.list, {
            c(exclude.unmet.only.test.prop, at.random.test.prop
             ,at.random.no.data.test.prop)[c(exclude.unmet.only, at.random, at.random.no.data)]
            })

  if (!validation){
    print("Not a validation exercise!")
    return()
  }
  load(file = file.path(output.dir,"Ps_validation.rda")) # change JR, 20140418
  if (validation.list$exclude.unmet.only){
    select.unmet.c <- unique(getc.j.test[winbugs.data$getj.test.unmet.k])
  } else {
    select.unmet.c <- NULL
  }
  if (plot.CIs){
    PlotDataAndEstimates(data.raw = mcmc.meta$data.raw, #country.info = country.info,
                         validation = validation,
                         select.c = select.unmet.c,
                         getj.test.k = mcmc.meta$winbugs.data$getj.test.k,
                         getj.test.unmet.k = mcmc.meta$winbugs.data$getj.test.unmet.k,
                         CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                         CIstar.Lg.Lcat.qt = res.country$CIstar.Lg.Lcat.qt,
                         CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                         fig.name = file.path(fig.dir, paste0(run.name, "CIs.pdf")) # change JR, 20140418
                         ,UWRA = UWRA
                         )
    PlotDataAndEstimates(data.raw = mcmc.meta$data.raw, #country.info = country.info,
                         validation = validation,
                         select.c = select.unmet.c,
                         getj.test.k = mcmc.meta$winbugs.data$getj.test.k,
                         getj.test.unmet.k = mcmc.meta$winbugs.data$getj.test.unmet.k,
                         CI.Lg.Lcat.qt = res.country$CIprop.Lg.Lcat.qt,
                         CIstar.Lg.Lcat.qt = res.country$CIstar.Lg.Lcat.qt,
                         CIratio.Lg.Lcat.qt = res.country$CIratio.Lg.Lcat.qt,
                         fig.name = file.path(fig.dir, paste0(run.name, "CIs_diag.pdf")) # change JR, 20140418
                        ,UWRA = UWRA
                         ,ymin.at.0 = "data", ##<< Set lower bound on y-axis at 0?
                         ,ymax.at.100 = "data", ##<< Set upper bound on y-axis at percent = 100%? Only applies if plot.prop is TRUE.
                         )
  }
  ## open text file to write results
  validation.res.file <- file.path(fig.dir, paste0(run.name, "validationres.html")) # change JR, 20140418
  cat("", file = validation.res.file, append = F)

    ## when summarizing results in table: select only last observation year in each country
    ## Note: can be different for unmet because less observations!
    ## if(isTRUE(validation.list$at.random.no.data)) {
    ##     j.include.c <- j.include.unmet.c <- rep(0, C)
    ##     J <- length(data$props.unmet.j)
    ##     select.unmet.c <- unique(getc.j.test[winbugs.data$getj.test.unmet.k])
    ##     for (c in select.unmet.c) {
    ##         select <-
    ##             seq(1, J)[getc.j.test==c & is.element(seq(1, J), winbugs.data$getj.test.unmet.k) & !is.na(data$props.unmet.j)]
    ##         kk <- which.max(data$years.j[select])
    ##         j.include.unmet.c[c] <- seq(1, J)[select][kk]
    ##     }
    ## } else {
        j.include.c <- j.include.unmet.c <- j.include.c.tot <- rep(0, C)
        J <- length(data$props.unmet.j)
        select.unmet.c <- unique(getc.j.test[winbugs.data$getj.test.unmet.k])
        for (c in select.unmet.c){
            select <- seq(1, J)[getc.j.test==c
                                & is.element(seq(1, J), winbugs.data$getj.test.unmet.k)
                                & !is.na(data$props.unmet.j) ]
            kk <- which.max(data$years.j[select])
            j.include.unmet.c[c] <- seq(1, J)[select][kk]
        }
    ## }
    ## T/F indicator
    include.unmet.j <- is.element(seq(1, length(data$years.j)), j.include.unmet.c)

#   # which country has P.j missing?
#   length(select.unmet.c)
#   sum(include.unmet.j)
#   sum(!is.na(Ps$P.jp[include.unmet.j,3]))
#   sum(!is.na(winbugs.data$logitratio.yunmet.j[include.unmet.j]))
#
#   sum(is.na(Ps$P.jp[include.unmet.j,3]))
#   which.max(is.na(Ps$P.jp[include.unmet.j,3]))
#   getc.j.test[include.unmet.j]
#   data$name.j[include.unmet.j][40]
#  length(include.unmet.j)


  # now make the one for trad/modern
    if (!is.null(winbugs.data$getj.test.k)){
            select.c <- unique(getc.j.test[winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown]])
            for (c in select.c){
                select <- seq(1, J)[getc.j.test==c
                                    & is.element(seq(1, J), winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown])
                                    & !is.na(data$props.trad.j)]
                                # most recent when left out at end
                                # random for 20%
                if (length(select)==1){
                    kk <- select # kk different here from above!
                } else {
                    kk <- ifelse(validation.list$at.end, select[which.max(data$years.j[select])],
                                #                  which.min(data$years.j[select]) )
                                #                  which.max(data$years.j[select]) )
                                 sample(select, size = 1) )
                                # watch out: sample (x,1) gives a sample from 1:x!
                }
                j.include.c[c] <- seq(1, J)[kk]
            }
            include.j <- is.element(seq(1, length(data$years.j)), j.include.c)
            if(isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out)) {
                idx.tot.j <- seq(1, J)[is.na(data$props.trad.j)]
                idx.tot.k <- (winbugs.data$n.test.breakdown + 1):winbugs.data$J.test
                ## Need to count those with only 'tot' as well.
                select.c <-
                    unique(getc.j.test[winbugs.data$getj.test.k[idx.tot.k]])
                for (c in select.c){
                    select <- seq(1, J)[getc.j.test==c & is.element(seq(1, J)
                                                    ,winbugs.data$getj.test.k[idx.tot.k]
                                                     )]
                                # most recent when left out at end
                                # random for 20%
                    if (length(select)==1){
                        kk <- select # kk different here from above!
                    } else {
                        kk <- sample(select, size = 1)
                                # watch out: sample (x,1) gives a sample from 1:x!
                    }
                    j.include.c.tot[c] <- seq(1, J)[kk]
                }
                include.tot.j <- include.j
                include.tot.j[idx.tot.j] <-
                    is.element(seq(1, length(data$years.j)), j.include.c.tot)[idx.tot.j]
            }
        } else {
            include.tot.j <- include.j <- rep(FALSE, length(data$years.j))
  }
  # check:
  #data.frame(data$iso.j, data$years.j, data$props.modern.j, include.j)
  #data.frame(data$iso.j, data$years.j, data$props.unmet.j, include.unmet.j)

    nameplot2 <-
        ifelse(mcmc.meta$validation.list$exclude.unmet.only, "Unmet only",
        ifelse(mcmc.meta$validation.list$at.random, paste0(test.prop * 100, "%"),
        ifelse(mcmc.meta$validation.list$at.end
              ,paste("Up to (excl) year", mcmc.meta$validation.list$year.cutoff),
        ifelse(mcmc.meta$validation.list$at.random.no.data, paste0(test.prop * 100, "% (no data exercise)")
              ,NA
               ))))
    res.paper <- NULL
        CI.df <- Error.df <- data.frame()
  pdf(file.path(fig.dir, paste0(run.name, "propoutside.pdf")), width = 16, height = 10) # change JR, 20140418
    for (p in 1:4){
        if (p==4){
            includeforp.j <- include.unmet.j
        } else if (p==3 && (isTRUE(validation.list$at.random.no.data) || isTRUE(validation.list$leave.iso.out))) {
            includeforp.j <- include.tot.j
        } else {
            includeforp.j <- include.j
        }
            if(!(p == 4 && validation.list$leave.iso.out)) {
        P.j <- Ps$P.jp[,p]
        ##error.j <- Ps$error.jp[,p]
        error.est.j <- Ps$error.est.jp[,p]
        nameplot1s <- c("Traditional", "Modern", "Total", "Unmet")
        if (sum(!is.na(P.j))>0){
            nameplot1 <- nameplot1s[p]
            res <- PlotValResults(include.j = includeforp.j, P.j = P.j,
                                  data = data,
                                  country.info = country.info,
                                  winbugs.data = winbugs.data,
                                  nameplot = paste(nameplot1, nameplot2, sep = " - "),
                                  error.est.j = error.est.j
                                 ,validation.list = validation.list)
            ## print table: html and tex
            print(xtable::xtable(res$table.ix, ##digits = c(0,0,0,0,0),
                         caption = paste(nameplot1, nameplot2)),
                  file = validation.res.file, append = T, type = "html")
            print(xtable::xtable(res$table2.ix, ##digits = c(0,0,0,0,0),
                         caption = paste(nameplot1, nameplot2)),
                  type="html", file = validation.res.file, append = T)
            addpaper <-unlist( c(
                res$table.ix[1, c("# Obs")],
                100*res$table.ix[1, c("Below 95% CI","Within 95% CI","Above 95% CI")] , # first one is all obs
                100*unlist(res$table2.ix[1, c("Prop below median")]),
                res$table2.ix[1, c("Median error","Median abs error")]
            ))
            if (is.null(res.paper)){
                res.paper <- addpaper
            } else {
                res.paper <- rbind(res.paper, addpaper)
            }
            if(keep.all) {
                CI.df <-
                    rbind(CI.df
                         ,data.frame(res$table.ix, group = rownames(res$table.ix)
                                    ,indicator = nameplot1s[p]
                                    ,check.names = FALSE
                                    ,row.names = NULL
                                     )
                          )
                Error.df <-
                    rbind(Error.df
                         ,data.frame(res$table2.ix
                                    ,group = rownames(res$table2.ix)
                                    ,indicator = nameplot1s[p]
                                    ,check.names = FALSE
                                    ,row.names = NULL
                                     )
                          )
            }
            }
            }
    }
    dev.off()
  if (prod(dim(res.paper))>1) rownames(res.paper) <- nameplot1s[1:nrow(res.paper)]
  # # plot(exp(Ps$error.j$error.trad.j) ~ data$props.trad.j)
  # # plot(exp(Ps$error.j$error.trad.j[ssa.j=="Yes"]) ~ data$props.trad.j[ssa.j=="Yes"])
  # # plot(Ps$error.j$error.trad.j ~ data$props.trad.j)
  # # plot(Ps$error.j$error.trad.j ~ winbugs.data$ratios.trad.modern.jn[,1])
  # # plot(Ps$error.j$error.modern.j ~ winbugs.data$ratios.trad.modern.jn[,2])
  # # plot(Ps$error.j$error.unmet.j ~ winbugs.data$logitratio.yunmet.j)
  #
  # #---------------------------------------------------------------------------------
  # # plot two sets of estimates for comparison
  # load(file = file.path(work.dir.save, paste0(name.dir.all,"CIs.rda")))
  # CIs.all <- CIs
  # load(file.path(work.dir.save, paste0(name.dir, "CIs.rda")))
  # CIs.tr <- CIs
  #
  # pdf(file.path(work.dir.save, paste0(name.dir, "_comparison_", name.dir.all, "CIs.pdf")), width = 21, height = 12)
  #   PlotDataAndEstimates(data = data, country.info = country.info,
  #                     # start.year = start.year, end.year = end.year,
  #                            # par.ciq = par.ciq, plot.blue.line = FALSE,
  #                             validation = validation,
  #                             getj.test.k = winbugs.data$getj.test.k,
  #                             getj.test.unmet.k = winbugs.data$getj.test.unmet.k,
  #                             CIs = CIs.all, CIs2 = CIs.tr,
  #                             name.dir = name.dir.all, name.dir2 = name.dir,
  #                             overview.country.plot = F, country.plot = F)
  # dev.off()

  #---------------------------------------------------------------------------------------------
  # Compare CIs in comp.year (e.g. 2008)
  # when leaving out data at the end for countries where data was left-out
  # or for unmet
  # a. get countries where data was left out
  # Note: for unmet we use the same countries as where trad/mod were left out
  # if (!is.null(winbugs.data$getj.test.k)){
  #   select.c <- unique(getc.j.test[winbugs.data$getj.test.k[1:winbugs.data$n.test.breakdown]])
  # } else {
  #   select.c <- unique(getc.j.test[winbugs.data$getj.test.unmet.k])
  # }
  #
  # comp.year <- 2008.5
  # # b. results
  # # diff = all - training
  # # all (updated) CI outside old (training) 80% CI?
  # diff.cp <- above80.cp <- below80.cp <- matrix(NA, length(country.info$iso.c),4) # trad, mod, tot, unmet
  # for (c in select.c){
  #   for (p in 1:4){
  #     CIall.median <- ifelse(p==1, CIs.all$CIs.trad.cqt[c,3,CIs.all$est.years==comp.year],
  #                     ifelse(p==2, CIs.all$CIs.modern.cqt[c,3,CIs.all$est.years==comp.year],
  #                     ifelse(p==3, CIs.all$CIs.tot.cqt[c,3,CIs.all$est.years==comp.year],
  #                       CIs.all$CIs.unmet.cqt[c,3,CIs.all$est.years==comp.year])))
  #     CItr.q <- ifelse(rep(p,5)==1, CIs.tr$CIs.trad.cqt[c,,CIs.tr$est.years==comp.year],
  #                     ifelse(rep(p,5)==2, CIs.tr$CIs.modern.cqt[c,,CIs.tr$est.years==comp.year],
  #                     ifelse(rep(p,5)==3, CIs.tr$CIs.tot.cqt[c,,CIs.tr$est.years==comp.year],
  #                       CIs.tr$CIs.unmet.cqt[c,,CIs.tr$est.years==comp.year])))
  #     diff.cp[c,p] <- CIall.median - CItr.q[3]
  #     below80.cp[c,p] <- ifelse ((CIall.median < CItr.q[2]), 1, 0)
  #     above80.cp[c,p] <- ifelse ((CIall.median > CItr.q[4]), 1, 0)
  # }}
  #
  # # specify subgroups of left-out observations:
  # reg.c <-  country.info$reg.c
  # ssa.c <- country.info$ssa.c
  # dev.c <- country.info$dev.c
  # select.ci <- cbind(rep(TRUE, C), ssa.c=="No", ssa.c=="Yes",
  #                    dev.c=="Poor", dev.c=="Rich")
  # namesselect <- c("All", "OutsideSSA", "SSA", "Poor", "Rich")
  # I <- dim(select.ci)[2] # number of subgroups
  # # already defined
  # # nameplot2 <-ifelse(mcmc.meta$validation.list$exclude.unmet.only, "Unmet only",
  # #                    ifelse(mcmc.meta$validation.list$at.random, "20%",
  # #                     ifelse(mcmc.meta$validation.list$at.end,  paste("Up to (excl) year",
  # #                     mcmc.meta$validation.list$year.cutoff),NA)))
  # for (p in 1:4){
  #   table.ix <- NULL # x refers to table columns (outputs)
  #   nameplot1 <- c("Traditional", "Modern", "Total", "Unmet")[p]
  #   for (i in 1:I){
  #     select <- select.ci[,i]
  #     table.ix <- rbind(table.ix,
  #                     c(sum(!is.na(diff.cp[select,p])),
  #                       mean(diff.cp[select,p], na.rm = T), mean(abs(diff.cp[select,p]), na.rm = T),
  #                       mean(below80.cp[select,p], na.rm = T), mean(above80.cp[select,p], na.rm = T)))
  #   }
  #   nobs.i <- table.ix[, 1]
  #   rownames(table.ix) <- paste0(namesselect, " (", nobs.i, ")")
  #   colnames(table.ix) <- c("# Obs", "Mean diff", "Medan abs diff", "% Below 80CI", "% Above 80CI")
  #   # print table
  #   print(xtable(table.ix, #digits = c(0,0,0,0,0),
  #                 caption = paste("Change in median/CI: Results for ", comp.year, ", ", nameplot1, " (",
  #                                 nameplot2, ")")),
  #                 file = validation.res.file, append = T, type = "html")
  # }
  # # plots
  # # hist(diff.cp[,1])
  if (return.res.paper){
    propleftoutunmet <- (sum(!is.na(winbugs.data$logitratio.yunmet.j[winbugs.data$getj.test.unmet.k]))/
          sum(!is.na(winbugs.data$logitratio.yunmet.j)      ))
    propleftoutmod <-  (sum(!is.na(winbugs.data$ratios.trad.modern.jn[,2][winbugs.data$getj.test.k]))/
           sum(!is.na(winbugs.data$ratios.trad.modern.jn[,2])      ))

    return(  list(res.paper = res.paper, propleftoutunmet = propleftoutunmet,
                  propleftoutmod = propleftoutmod,
                  nunmetleftout = length(select.unmet.c ),
                  nunmetleftout2 = sum(include.unmet.j)))
                  #nleftout = length(select.c ))) # matches up for non-unmet exe, error for unmet exercise
    # select.unmet.c relevant for unmet val exericise only
  }
    if(keep.all){
        validation.results <- list(CI.df, Error.df)
        save.path <- file.path(output.dir, "validn_repts"
                             ,"validation.results.rda"
                              )
        save(validation.results
            ,file = save.path)
        message("Validation results saved to ", save.path)
        }
}# end non-validation results
##----------------------------------------------------------------------
##' Plot samples from predictive densities with data point overlaid.
##'
##' A histogram of the posterior predictive density for each observed
##' data point is produced with the observed value itself marked on
##' the plot.
##'
##' .. content for \details{} ..
##' @param run.name
##' @param output.dir
##' @return
##' @author Mark Wheldon with lots taken from other ContraceptiveUse functions
PlotPredDens <- function(run.name = "test", ##<< Run name
                         output.dir = NULL,
                         fig.dir = NULL ##<< Directory where MCMC array and meta are stored.
                         ) {

    ## -------* SET UP

    load(file = file.path(output.dir,"mcmc.meta.rda")) # change JR, 20140418
    if(!is.null(mcmc.meta$validation.list)) {
        warning("Predictive density plots only available for non-validation runs.")
        return(invisible())
    }
    if(mcmc.meta$general$do.country.specific.run) {
        warning("Predictive density plots not available for one country runs.")
        return(invisible())
    }
    if(!ModelFunctionPredDens(mcmc.meta$general$write.model.fun)) {
        warning(paste0("Predictive density plots only available for the following types of model:\n"
                      ,paste(ModelFunctionPredDens(mcmc.meta$general$write.model.fun
                                                  ,return.functions = TRUE)
                             ,collapse = ", "
                             ))
                ,collapse = ", ")
        return(invisible())
    }

    load(file = file.path(output.dir, "mcmc.array.rda"))

    if (is.null(output.dir)){
        output.dir <- file.path(getwd(), "output", run.name, "/")
    }
    if(is.null(fig.dir)) {
        fig.dir <- file.path(output.dir, "fig")
    }

    winbugs.data <- mcmc.meta$winbugs.data
    parnames.list <- mcmc.meta$parnames.list

    mod.fit.dir <- file.path(output.dir, "model_fit")
    if(!dir.exists(file.path(mod.fit.dir))) dir.create(file.path(mod.fit.dir), recursive = TRUE)

    ## -------* PLOTS and P-Values

    ## -------** Unmet

    pvals.df <- data.frame()

    pdf(width = 12, height = 12
       ,file = file.path(mod.fit.dir, "post_pred_samples_unmet.pdf"))
    n.pages <- ceiling(winbugs.data$n.training.unmet / 25)
    for(p in 1:n.pages) {
        par(mfrow = c(5, 5))
        for(k in (1 + (p - 1) * 25):min(winbugs.data$n.training.unmet, p * 25)) {
            parname <- paste0("pred.logitratio.yunmet.j["
                             ,winbugs.data$getj.training.unmet.k[k]
                             ,"]")
            j.c.name <-
                mcmc.meta$data.raw$data[winbugs.data$getj.training.unmet.k[k], "name.j"]
            j.year <-
                mcmc.meta$data.raw$data[winbugs.data$getj.training.unmet.k[k], "years.j"]
            obs <- winbugs.data$logitratio.yunmet.j[winbugs.data$getj.training.unmet.k[k]]
            pvals <- data.frame(parname = parname, Pr.lt = mean(mcmc.array[,,parname] < obs)
                               ,Pr.gt = mean(mcmc.array[,,parname] > obs))
            pvals.df <- rbind(pvals.df, pvals)
            hist(mcmc.array[,,parname], freq = FALSE
                ,xlim = c(min(obs, mcmc.array[,,parname]) - 0.2 * abs(min(obs, mcmc.array[,,parname]))
                         ,max(obs, mcmc.array[,,parname]) + 0.2 * abs(max(obs, mcmc.array[,,parname])))
                ,main = paste(parname, "\n", abbreviate(j.c.name, 15), " "
                             ,round(j.year, 1))
                ,xlab = "", sub = paste0("Pr < obs ", round(pvals$Pr.lt, 2), ". Pr > obs ", round(pvals$Pr.gt, 2)))
            lines(density(mcmc(mcmc.array[,,parname])))
            abcol <- "blue"
            if(pvals[2] < 0.05 || pvals[3] < 0.05) abcol <- "red"
            abline(v = obs, col = abcol, lwd = 2)
        }
    }
    dev.off()
    write.csv(pvals.df, file = file.path(mod.fit.dir, "post_pred_samples_unmet.csv")
             ,row.names = FALSE)

    ## -------** Mod/Trad Breakdown

    pvals.df <- data.frame()

    pdf(width = 12, height = 12
       ,file = file.path(mod.fit.dir, "post_pred_samples_bdown.pdf"))
    n.pages <- ceiling(2 * winbugs.data$n.training.breakdown / 24)
    for(p in 1:n.pages) {
        par(mfrow = c(5, 5))
        for(k in (1 + (p - 1) * 24 / 2):(min(winbugs.data$n.training.breakdown, p * 24 / 2))) {

            j.c.name <-
                mcmc.meta$data.raw$data[winbugs.data$getj.training.k[k], "name.j"]
            j.year <-
                mcmc.meta$data.raw$data[winbugs.data$getj.training.k[k], "years.j"]

            ## Trad '[k,1]'
            parname <- paste0("pred.ratios.trad.modern.jn["
                             ,winbugs.data$getj.training.k[k]
                             ,",1]")
            obs <- winbugs.data$ratios.trad.modern.jn[winbugs.data$getj.training.k[k], 1]
            pvals <- data.frame(parname = parname, Pr.lt = mean(mcmc.array[,,parname] < obs)
                               ,Pr.gt = mean(mcmc.array[,,parname] > obs))
            pvals.df <- rbind(pvals.df, pvals)
            hist(mcmc.array[,,parname], freq = FALSE
                ,xlim = c(min(obs, mcmc.array[,,parname]) - 0.2 * abs(min(obs, mcmc.array[,,parname]))
                         ,max(obs, mcmc.array[,,parname]) + 0.2 * abs(max(obs, mcmc.array[,,parname])))
                ,main = paste(parname, "\n", abbreviate(j.c.name, 15), " "
                             ,round(j.year, 1))
                ,xlab = "", sub = paste0("Pr < obs ", round(pvals$Pr.lt, 2), ". Pr > obs ", round(pvals$Pr.gt, 2)))
            lines(density(mcmc(mcmc.array[,,parname])))
            abcol <- "blue"
            if(pvals[2] < 0.05 || pvals[3] < 0.05) abcol <- "red"
            abline(v = obs, col = abcol, lwd = 2, lty = 2) #different lty to distinguish easily

            ## Modern '[k,2]'
            parname <- paste0("pred.ratios.trad.modern.jn["
                             ,winbugs.data$getj.training.k[k]
                             ,",2]")
            obs <- winbugs.data$ratios.trad.modern.jn[winbugs.data$getj.training.k[k], 2]
            pvals <- data.frame(parname = parname, Pr.lt = mean(mcmc.array[,,parname] < obs)
                               ,Pr.gt = mean(mcmc.array[,,parname] > obs))
            pvals.df <- rbind(pvals.df, pvals)
            hist(mcmc.array[,,parname], freq = FALSE
                ,xlim = c(min(obs, mcmc.array[,,parname]) - 0.2 * abs(min(obs, mcmc.array[,,parname]))
                         ,max(obs, mcmc.array[,,parname]) + 0.2 * abs(max(obs, mcmc.array[,,parname])))
                ,main = paste(parname, "\n", abbreviate(j.c.name, 15), " "
                             ,round(j.year, 1))
                ,xlab = ""
                ,sub = paste0("Pr < obs ", round(pvals$Pr.lt, 2), ". Pr > obs ", round(pvals$Pr.gt, 2)))
            lines(density(mcmc(mcmc.array[,,parname])))
            abcol <- "blue"
            if(pvals[2] < 0.05 || pvals[3] < 0.05) abcol <- "red"
            abline(v = obs, col = abcol, lwd = 2)
        }

        plot(1, type = "n", xlab = "", ylab = "", axes = F)
        legend("topright", lty = c(1, 2), col = "blue", legend = c("Mod", "Trad"))
        legend("bottomleft", lty = c(1, 2), col = "red", legend = c("Pr </> 0.05", "Pr </> 0.05"))
    }
    dev.off()
    write.csv(pvals.df, file = file.path(mod.fit.dir, "post_pred_samples_bdown.csv")
             ,row.names = FALSE)

    ## -------** No Mod/Trad Breakdown

    pvals.df <- data.frame()

    pdf(width = 12, height = 12
       ,file = file.path(mod.fit.dir, "post_pred_samples_tot.pdf"))
    n.pages <- ceiling(winbugs.data$n.training.tot / 25)
    for(p in 1:n.pages) {
        par(mfrow = c(5, 5))
        for(k in (1 + (p - 1) * 25):min(winbugs.data$n.training.tot, p * 25)) {
            j.c.name <-
                mcmc.meta$data.raw$data[winbugs.data$getj.training.tot.k[k], "name.j"]
            j.year <-
                mcmc.meta$data.raw$data[winbugs.data$getj.training.tot.k[k], "years.j"]
            parname <- paste0("pred.ytot.j["
                             ,winbugs.data$getj.training.tot.k[k]
                             ,"]")
            obs <- winbugs.data$logit.ytot.j[winbugs.data$getj.training.tot.k[k]]
            pvals <- data.frame(parname = parname, Pr.lt = mean(mcmc.array[,,parname] < obs)
                               ,Pr.gt = mean(mcmc.array[,,parname] > obs))
            pvals.df <- rbind(pvals.df, pvals)
            hist(mcmc.array[,,parname], freq = FALSE
                ,xlim = c(min(obs, mcmc.array[,,parname]) - 0.2 * abs(min(obs, mcmc.array[,,parname]))
                         ,max(obs, mcmc.array[,,parname]) + 0.2 * abs(max(obs, mcmc.array[,,parname])))
                ,main = paste(parname, "\n", abbreviate(j.c.name, 15), " "
                             ,round(j.year, 1))
                ,xlab = "", sub = paste0("Pr < obs ", round(pvals$Pr.lt, 2), ". Pr > obs ", round(pvals$Pr.gt, 2)))
            lines(density(mcmc(mcmc.array[,,parname])))
            abcol <- "blue"
            if(pvals[2] < 0.05 || pvals[3] < 0.05) abcol <- "red"
            abline(v = obs, col = abcol, lwd = 2)
        }
    }
    dev.off()
    write.csv(pvals.df, file = file.path(mod.fit.dir, "post_pred_samples_tot.csv")
             ,row.names = FALSE)

    message("Histograms and csv files saved to ", file.path(mod.fit.dir))

}
##----------------------------------------------------------------------
##' Compute information criteria (AIC, WAIC, BIC).
##'
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @param run.name
##' @param output.dir
##' @return
##' @author Mark Wheldon with lots taken from other ContraceptiveUse functions
ComputeInformationCriteria <-
    function(run.name = "test", ##<< Run name
             output.dir = NULL
             ,fig.dir = NULL) {##<< Directory where MCMC array and meta are stored.

        ## -------* SET UP

        load(file = file.path(output.dir,"mcmc.meta.rda")) # change JR, 20140418
        winbugs.data <- mcmc.meta$winbugs.data
        if(!is.null(mcmc.meta$validation.list)) {
            warning("Information criteria only available for non-validation runs.")
            return(invisible())
        }

        if(mcmc.meta$general$do.country.specific.run) {
            warning("Information criteria not available for one country runs.")
            return(invisible())
        }

    if(!ModelFunctionPredDens(mcmc.meta$general$write.model.fun)) {
        warning(paste("Predictive density plots only available for the following types of model:\n"
                      ,ModelFunctionPredDens(mcmc.meta$general$write.model.fun
                                            ,return.functions = TRUE)
                             ,collapse = ", "
                             ))
        return(invisible())
    }

        load(file = file.path(output.dir, "mcmc.array.rda"))

        if (is.null(output.dir)){
            output.dir <- file.path(getwd(), "output", run.name, "/")
        }

        winbugs.data <- mcmc.meta$winbugs.data
        parnames.list <- mcmc.meta$parnames.list

        mod.fit.dir <- file.path(output.dir, "model_fit")
        if(!dir.exists(file.path(mod.fit.dir))) dir.create(file.path(mod.fit.dir), recursive = TRUE)

        ## -------* MAIN

        ## -------** WAIC

        ## From Gelman et al. BDA Ed 3

        ## Indices of log-densities in 'mcmc.array'
        unmet.idx <- which(dimnames(mcmc.array)[[3]] %in% parnames.list$parnames.logdens.unmet)
        bdown.idx <- which(dimnames(mcmc.array)[[3]] %in% parnames.list$parnames.logdens.bdown)
        tot.idx <- which(dimnames(mcmc.array)[[3]] %in% parnames.list$parnames.logdens.tot)

        ## Pointwise predictive density
        ppd <-
            apply(mcmc.array[,,c(unmet.idx, bdown.idx, tot.idx)], 3
                , function(z) mean(exp(z)))
                                #take 'exp' because elements are log-densities
                                #takes average over posterior of theta

        ## Log pointwise predictive density
        lppd <-
            apply(mcmc.array[,,c(unmet.idx, bdown.idx, tot.idx)], 3
                , function(z) mean(z))
                                #don't exponentiate because already log-densities
                                #takes average over posterior of theta

        ## WAIC Correction version 1
        pWAIC1 <- 2 * sum(log(ppd) - lppd)

        ## WAIC Correction version 2
        pWAIC2 <- sum(apply(mcmc.array[,,c(unmet.idx, bdown.idx, tot.idx)], 3
                          , function(z) var(as.numeric(z))))
                                #'as.numeric' because z is a matrix;
                                #'var' would then return a var-covar
                                #matrix

        ## WAICs
        WAIC1 <- -2 * sum(lppd) + 2 * pWAIC1
        WAIC2 <- -2 * sum(lppd) + 2 * pWAIC2

        ## Save
        out <- data.frame(WAIC1 = WAIC1, WAIC2 = WAIC2)
        write.csv(out, file = file.path(mod.fit.dir, "WAIC.csv")
                 ,row.names = FALSE)

        cat("\n--------------------\nWAIC(1) =", WAIC1, "\nWAIC(2) =", WAIC2, "\n--------------------\n")

        return(invisible(out))

        }
##----------------------------------------------------------------------
## Randomly partition a vector into 'n.parts'.
RandomlyPartition <-
    function(x, n.parts = 10, seed = 1) {
        set.seed(seed)

        lx <- length(x)

        ## Randomly re-order
        x <- sample(x, size = lx, replace = FALSE)

        ## Now cut into 'n.parts'
        splits <- as.numeric(cut(1:lx, n.parts))
        tapply(x, splits, "[", simplify = FALSE)
        }
##----------------------------------------------------------------------
## The End!
