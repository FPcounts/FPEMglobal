# plots for paper (not CIs over time not diag and not bar charts...)

PlotMetDemandinCountryBySubregion <- function(# Plot country estimates of met demand by sub-region.
  ### Plot used for Alkema et al:
  ### Plot met demand in 2010 against 1990 for all countries by sub-region and
  ### color the plotting symbols by the PPPC.
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  fig.dir = NULL, ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
  plot.tiff  = FALSE##<< TIFF-format? If FALSE, PDF-format is used.
  ){

  if (is.null(fig.dir)){
    fig.dir <- file.path(getwd(), "fig/")
    dir.create(fig.dir, showWarnings = FALSE)
  }
  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name, "/")
  }

  load(file = file.path(output.dir,"res.country.rda")) # change JR, 20140418
  #load(file = file.path(output.dir,"res.aggregate.rda")) # change JR, 20140418
  load(file = file.path(output.dir,"mcmc.meta.rda")) # change JR, 20140418
  country.info <- mcmc.meta$data.raw$country.info

  est.years <- dimnames(res.country$CIratio.Lg.Lcat.qt[[1]][[1]])[[2]]
  percentiles <- dimnames(res.country$CIratio.Lg.Lcat.qt[[1]][[1]])[[1]] # 0.5

  probBpos.c <- unlist(lapply(res.country$changeprop.Lg.Lcat.Ti, function(l)
  l[["Met Demand"]]["1990-2010", "PPPC"]))
  x <- 100*unlist(lapply(res.country$CIratio.Lg.Lcat.qt, function(l)
    l$"Met Demand"[percentiles =="0.5", est.years == "1990.5"]))
  y <- 100*unlist(lapply(res.country$CIratio.Lg.Lcat.qt, function(l)
    l$"Met Demand"[percentiles =="0.5", est.years == "2010.5"]))
  group.c <- ifelse(country.info$dev.c=="Rich", "Developed",
                    ifelse(country.info$namereg.c == "Oceania", "Oceania",
                           paste(country.info$namesubreg.c)))

  #legendprobs <- c("Pr(increase) < 0.95","Pr(increase): 0.95-0.99", "Pr(increase) > 0.99")
  #colprobs <- c("red", "darkgrey", "green")
  #col.B <- ifelse(probBpos.c>=0.95,ifelse(probBpos.c>0.99, "green","grey"), "red")
  cutoffs <- c(0.95) #c(0.5, 0.8, 0.9, 0.95, 0.99)
  legendprobs <- c(paste("Pr(increase) =<", cutoffs[1]),
        #paste("Pr(increase): ", cutoffs[-length(cutoffs)],
        #  "-", cutoffs[-1],sep = ""),
        paste("Pr(increase) >", cutoffs[length(cutoffs)]))
  #colprobs <- heat.colors(n = length(cutoffs)+1)
  #length(cutoffs)
  colprobs <- c("red", "blue") #pink", "orange", "yellow", "green", "blue")
  pchprobs <- c(25,24)
  intervals <- c(0, cutoffs, 1)
#  probBpos.c <- runif(10,0,1)
  col.B <- pch.B <- rep(NA, length(probBpos.c))

  for (c in 1: length(probBpos.c)){
    col.B[c] <-  colprobs[sum(probBpos.c[c] >  intervals[-length(intervals)])]
    pch.B[c] <-  pchprobs[sum(probBpos.c[c] >  intervals[-length(intervals)])]
    #      probBpos.c[c] >= intervals[-length(intervals)])]
  }

  ##details<< Plot \code{MetDemand19902010} is added to \code{fig.dir}.
  fig.name <- file.path(fig.dir, paste0(run.name, "MetDemand19902010")) # change JR, 20140418
  if (plot.tiff){
    tiff(filename = paste0(fig.name,".tif"),
         width = 14, height = 10,
         units = "cm",
         pointsize = 6,res = 300, bg = "white",compression = "lzw")
  } else {
    pdf(file = paste0(fig.name,".pdf"),
        width = 14, height = 10)
  }
  groupslist <- list()
  groupslist[[2]] <- c("Central Asia", "Eastern Asia","South-Eastern Asia",
                       "Southern Asia", "Western Asia")
  groupslist[[1]] <- c( "Eastern Africa", "Middle Africa","Northern Africa",
                         "Southern Africa", "Western Africa")
  groupslist[[3]] <- c(  "Caribbean", "Central America", "South America","Oceania", #c( "Melanesia"  , "Micronesia","Polynesia"  )
                         "Developed")
  nf <- layout(
    rbind(
      cbind(c(1,rep(1,2)),
            matrix(c(seq(2, 16)),3,5, byrow = TRUE)
            ),
      c(0,17,18,18,19,19))
          ,
               widths = c(0.7, rep(1.5,5)), heights = c(rep(1.5,3),0.8), TRUE)
  #layout.show(nf)
  par( mar = c(1,1,2,2))
  plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
  text(1,1, cex = 2,las = 3,srt = 90,
         labels = "Demand satisfied 2010")
  I <- length(groupslist)
  for (i in 1:I){
    for (j in 1:length(groupslist[[i]])){
    #par(mar = c(ifelse(j==1,5,1),ifelse(i==length(groupslist),5,1),1,0))
    select <- group.c==groupslist[[i]][j]
    ys <- y[select]
    xs <- x[select]
    cols <- col.B[select]
    pchs <- pch.B[select]

      plot(ys ~xs, xlim = c(0,100), ylim = c(0,100),
            cex = 1.5, cex.lab = 1.5, #col  = col.B,
            cex.axis = 1.5, type = "n",
        xlab = ifelse(#i==I |
            j==3&i==3, "Demand satisfied 1990",""),
         ylab = ifelse(j==1, "Demand satisfied 2010",""),
         xaxt = ifelse(i==length(groupslist),
                       #| i==3 & j==4 |
           #i==2&j==5,
                       "s", "n"
           ),
         yaxt = "n", #ifelse(j==1,"s", "n"),
         cex.main = 1.5,
         main = groupslist[[i]][j])
    abline(0,1, lwd  =3)
    if (j==1)   axis(2, at = seq(0,100,20), labels = seq(0,100,20), las = 2, cex.axis = 1.5)

    points(ys ~xs, col = cols, lwd = 3, cex = 1.2, pch = pchs)
    }
  }
  plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
  plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
  #plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
  text(1,1, cex = 2,las = 1,#srt = 90,
       labels = "Demand satisfied 1990")

  par(mar = c(0,0,0,0))
   plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
   legend("center", cex = 2, legend = paste(legendprobs),
          lwd = 3,  col = colprobs, pch = pchprobs, lty = -1, bty = "n")
  #plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
#   plot(1, xaxt = "n", yaxt = "n", type = "n", xlab = "", ylab = "", bty = "n")
#   text(1,1, cex = 2,las = 3,#srt = 90,
#        labels = "Demand satisfied 1990")

  dev.off()
  cat("Met demand in country by subregion plotted.\n")
  ##value<< NULL
  return(invisible())
}

#--------------------------------------------------------------------
BarChartSubregion <- function(#Plot counts of MWRA with unmet need by subregion for 'year', broken down by country estimates.
  ### Plot used in Alkema et al for counts of MWRA with unmet need by subregion.
  ### Note: this function was used for other things before (which explains the commented code)
  run.name = "test", ##<< Run name
  output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
  ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
  fig.dir = NULL, ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
  plot.tiff  = FALSE##<< TIFF-format? If FALSE, PDF-format is used.
 ,year = 2010
 ,leave.out.less.than = 1E6
  ,x.axis.scale = 1E6
  ){
  if (is.null(fig.dir)){
    fig.dir <- file.path(getwd(), "fig/")
    dir.create(fig.dir, showWarnings = FALSE)
  }
  if (is.null(output.dir)){
    output.dir <- file.path(getwd(), "output", run.name, "/")
  }
    ## [MCW-2018-02-14]
    ## Don't do anything if this is a one country run.
    load(file = file.path(output.dir,"mcmc.meta.rda"))
    if(mcmc.meta$general$do.country.specific.run) {
        warning("Once country run: barcharts not produced.")
        return(invisible())
    }

    if(mcmc.meta$general$marital.group == "UWRA") {
      UWRA <- TRUE
  } else { UWRA  <- FALSE }

  year.5 <- round(year) + 0.5

  load(file = file.path(output.dir,"res.country.rda")) # change JR, 20140418
  load(file = file.path(output.dir,"res.aggregate.rda")) # change JR, 20140418
  country.info <- mcmc.meta$data.raw$country.info
  region.info <- mcmc.meta$data.raw$region.info
  name.g <- names(res.aggregate$CIprop.Lg.Lcat.qt)
  # combine regions in Oceania
  nameselect <-  unique(ifelse(is.element(region.info$name.subreg,
                                          c("Melanesia" , "Micronesia", "Polynesia")), "Oceania",
                               region.info$name.subreg))
  select <- is.element(name.g, nameselect)
  name.s <- name.g[select]
  n.subreg <- length(name.s)

  # just assume 5 as usual...
  #percentiles <- dimnames(res.aggregate$CIprop.Lg.Lcat.qt[[1]][[1]])[[1]]
  est.years <- dimnames(res.aggregate$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]]

  ##details<< Plot \code{totalcounts} is added to \code{fig.dir}.
  fig.name <- file.path(fig.dir, paste0(run.name, "totalcounts")) # change JR, 20140418
  if (plot.tiff){
    tiff(filename = paste0(fig.name,".tif"),
         width = 7, height = 7,
         units = "cm", pointsize = 6,res = 300)#, bg = "white")#,compression = "lzw")
  } else {
    pdf(paste0(fig.name,".pdf"), width = 7, height = 7)
  }
  par(mfrow = c(1,1), mar = c(5,10,3,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  # res is in 10^6!
  res2010.sq <- From.Lg.Lcat.qtTo.gq(res.Lg.Lcat.qt = res.aggregate$CIcount.Lg.Lcat.qt,
                                 cat = "Unmet", year = year.5)[
                                   is.element(name.g, name.s),] / (x.axis.scale / 1000)
  #res2015.sq <- From.Lg.Lcat.qtTo.gq(res.Lg.Lcat.qt = res.aggregate$CIcount.Lg.Lcat.qt,
  #                                   cat = "Unmet", year = "2015.5")[
  #                                     is.element(name.g, name.s),]/1000

  order <- order(res2010.sq[,3])
  ## Regions with less than 1 million women with unmet need are left out.
  leaveout <- sum((res2010.sq[,3] * x.axis.scale) < leave.out.less.than )
  #ymax = max(res2015.sq, res2010.sq)*1.05
  ymax = max(res2010.sq)*1.1

  plot(seq(1, n.subreg)~res2010.sq[order,3], type = "n", ylab = ""
      ,xlim = c(0, ymax )
     , xaxt = "n",
       yaxt = "n", bty = "n",
       ylim = c(leaveout+1,n.subreg),
#       xlab = "Number of women (million)"
       xlab = ifelse(UWRA, paste0("Women aged 15-49, unmarried/not in a union (", formatC(x.axis.scale, format = "f", big.mark = " ", digits = 0), "s)"), paste0("Women aged 15-49, married/in a union ", formatC(x.axis.scale, format = "f", big.mark = " ", digits = 0), "s)") ), cex.lab = 1.2
      ,main = paste0("Unmet need in ", year)
       ,sub = paste0("Subregions with less than ", formatC(leave.out.less.than, format = "f", big.mark = " ", digits = 0), " are not presented"))
  axis(2, at = seq(1, n.subreg), labels = name.s[order], las = 1, cex.axis = 1)
  #abline(v = c(0,70))
  ## for (s in seq(10,70,20)){
  ##   polygon(c(s,s,s+10, s+10,s), c(0,30,30,0,0), border = "NA",
  ##           col = adjustcolor("lightgrey", alpha.f = 0.4))
  ## }
  axis(1, at = round(seq(0,max(200, round(ymax / 100)*100) ,length.out = 11),0)
     , labels = round(seq(0,max(200, round(ymax / 100)*100),length.out = 11),0), las = 2,
       cex.axis = 1)
  palette("default")
  #mycols <- rep(adjustcolor(palette(), alpha.f = 0.4)[1],100) # #
  orderp <- list() # country order is determined by 2010
  res2010.c <- unlist(lapply(res.country$CIcount.Lg.Lcat.qt, function(l) l[["Unmet"]][3,est.years==year.5]))
  # change LA, Feb 7, 2013
  #res2015.c <- unlist(lapply(res.country$CIcount.Lg.Lcat.qt, function(l) l[["Unmet"]][3,est.years=="2015.5"]))
  for (s in seq(1, n.subreg)){
    # props of country count in region by medians
    props <- res2010.c[is.element(paste(country.info$namesubreg.c), name.s[s])]/
      sum(res2010.c[is.element(paste(country.info$namesubreg.c), name.s[s])])
    orderp[[s]] <- order(props, decreasing = T)
  }
  width <- 0.15
  res.Lt.sq <- list(res2010.sq = res2010.sq)#, res2015.sq = res2015.sq)
  res.Lt.c <- list(res2010.c = res2010.c)#, res2015.c = res2015.c)
  # change LA
  for (t in 1:1){
  #for (t in 1:2){
    res.sq <- res.Lt.sq[[t]]
    res.c <- res.Lt.c[[t]]
    year <- c(year.5, year.5 + 5)[t]
    #add <- c(0.15, -0.15)[t]
    add <- 0#c(0.15, -0.15)[t]
    for (j in seq(1, n.subreg)){
      s <- order[j] # get right region for plot index j
      # props of country count in region by medians
      # no longer used, not sure if it still work (July 2012)
#      props <- res.c[is.element(paste(country.info$namesubreg.c), name.s[s])]/
#        sum(res.c[is.element(paste(country.info$namesubreg.c), name.s[s])])
      # use country order from 2010
#      cprops <- c(0, cumsum(props[orderp[[s]]]))
#       for (i in 2:length(cprops)){
#         polygon(res.sq[s,3]*c(cprops[i-1],cprops[i],cprops[i],cprops[i-1],cprops[i-1]),
#                 add + j + c(-width,-width,width, width, -width),
#                 border = "NA", col = mycols[i])
#       }
#       # put box around
      polygon(c(0, res.sq[s,3],res.sq[s,3], 0,0),
              add + j + c(-width,-width,+width, +width,-width), border = 1,
              col = adjustcolor("red", alpha.f = 0.4)[1])
    } # end s
    points(add+seq(1, n.subreg)~res.sq[order,3], pch = 20, lwd = ifelse(plot.tiff,1,2))
    segments(res.sq[order,1], add+seq(1, n.subreg), res.sq[order,5],
           add+seq(1, n.subreg), lwd = ifelse(plot.tiff,1,2), col = 1)
  } # end t
  dev.off()
  if(UWRA) {
        cat("Counts of UWRA with unmet need by subregion broken down by country estimates plotted.\n") } else {
         cat("Counts of MWRA with unmet need by subregion broken down by country estimates plotted.\n") }
  ##value<< NULL,
  return(invisible())
}
#--------------------------------------------------------------------
##' Bar chart of proportion of modern users that are unmarried women in a single year.
##'
##' Requires all women estimates. Assumes \file{res.country.all.women.rda} and
##' \file{res.aggregate.all.women.rda} are in \code{output.dir}.
##'
##' .. content for \details{} ..
##' @param run.name
##' @param output.dir
##' @param fig.dir
##' @param plot.tiff
##' @param year Year for which results are plotted.
##' @return Called for its side effect. A plot is saved in \code{fig.dir}.
##' @author Mark Wheldon with great swathes copied from \code{\link{BarChartSubregion}}.
BarChartPropUWRA <-
    function(run.name = "test",
             output.dir = NULL,
             fig.dir = NULL,
             plot.tiff  = FALSE,
             UWRA = FALSE,
       all.womenize.fig.name = TRUE,
             year = 2017) {
        if (is.null(fig.dir)){
            fig.dir <- file.path(getwd(), "fig/")
            dir.create(fig.dir, showWarnings = FALSE)
        }
        if (is.null(output.dir)){
            output.dir <- file.path(getwd(), "output", run.name, "/")
        }
    ## [MCW-2018-02-14]
    ## Don't do anything if this is a one country run.
    load(file = file.path(output.dir,"mcmc.meta.rda"))
    if(mcmc.meta$general$do.country.specific.run) {
        warning("Once country run: barcharts not produced.")
        return(invisible())
    }

        year.5 <- round(year) + 0.5

        load(file = file.path(output.dir,"res.country.all.women.rda")) # change JR, 20140418
        load(file = file.path(output.dir,"res.aggregate.all.women.rda")) # change JR, 20140418
        country.info <- mcmc.meta$data.raw$country.info
        region.info <- mcmc.meta$data.raw$region.info
        name.g <- names(res.aggregate.all.women$CIprop.Lg.Lcat.qt)
                                # combine regions in Oceania
        nameselect <-  unique(ifelse(is.element(region.info$name.subreg,
                                                c("Melanesia" , "Micronesia", "Polynesia")), "Oceania",
                                     region.info$name.subreg))
        select <- is.element(name.g, nameselect)
        name.s <- name.g[select]
        n.subreg <- length(name.s)

        est.years <- dimnames(res.aggregate.all.women$CIratio.Lg.Lcat.qt[[1]][[1]])[[2]]

        ##details<< Plot \code{totalcounts} is added to \code{fig.dir}.


        ## Ensure filename has "aw" in it, even
        ## if ~run.name~ does not have "umw" or "mw" in it.
        if(all.womenize.fig.name) {
            aw.run.name <- makeAWFileName(run.name)
            } else aw.run.name <- run.name
        fig.name <- file.path(fig.dir, paste0(aw.run.name, "prop_modern_uwra")) # change JR, 20140418
        if (plot.tiff){
            tiff(filename = paste0(fig.name,".tif"),
                 width = 7, height = 7,
                 units = "cm", pointsize = 6,res = 300)#, bg = "white")#,compression = "lzw")
        } else {
            pdf(paste0(fig.name,".pdf"), width = 7, height = 7)
        }
        par(mfrow = c(1,1), mar = c(5,10,3,1), cex.main = 1, cex.axis = 1, cex.lab = 1)
        resYear.sq <- From.Lg.Lcat.qtTo.gq(res.Lg.Lcat.qt = res.aggregate.all.women$CIratio.Lg.Lcat.qt,
                                           cat = "Modern Unmarried Over All", year = year.5)[
            is.element(name.g, name.s),]

        order <- order(resYear.sq[,3])
  ymax = max(resYear.sq)*1.1
        plot(seq(1, n.subreg)~resYear.sq[order,3], type = "n", ylab = ""
      ,xlim = c(0, ymax )
           , xaxt = "n",
             yaxt = "n", bty = "n",
             xlab = "Women aged 15-49, unmarried/not in a union"
            ,main = paste0("Proportion of modern users that are\nunmarried and not in a union in ", year)
             )
        axis(2, at = seq(1, n.subreg), labels = name.s[order], las = 1, cex.axis = 1)
        axis(1, at = seq(from = 0, to = 1, length.out = 11), las = 2, cex.axis = 1)
        palette("default")

        width <- 0.15
        res.Lt.sq <- list(resYear.sq = resYear.sq)
        for (t in 1:1){
            res.sq <- res.Lt.sq[[t]]
            year <- c(year.5, year.5 + 5)[t]
            add <- 0
            for (j in seq(1, n.subreg)){
                s <- order[j]
                polygon(c(0, res.sq[s,3],res.sq[s,3], 0,0),
                        add + j + c(-width,-width,+width, +width,-width), border = 1,
                        col = adjustcolor("red", alpha.f = 0.4)[1])
            } # end s
            points(add+seq(1, n.subreg)~res.sq[order,3], pch = 20, lwd = ifelse(plot.tiff,1,2))
            segments(res.sq[order,1], add+seq(1, n.subreg), res.sq[order,5],
                     add+seq(1, n.subreg), lwd = ifelse(plot.tiff,1,2), col = 1)
        } # end t
        dev.off()
        message("Proportion of modern users that are unmarried and not in a union broken down by subregion plotted in ", fig.dir)
        return(invisible())
    }

##--------------------------------------------------------------------
CIPropChangesSubregions <-
    function(## Plot CIs for unmet and total for subregions in one plot.
             ## Plot used in Alkema et al:  Plot CIs for unmet and total for subregions in one plot.
             run.name = "test", ##<< Run name
             output.dir = NULL, ##<< Directory where MCMC array and meta are stored.
             ## If NULL, it's \code{output/run.name}, default from \code{runMCMC}.
             fig.dir = NULL, ##<< Directory to store overview plots. If NULL, folder "fig" in current working directory.
             plot.tiff  = FALSE##<< TIFF-format? If FALSE, PDF-format is used.
            ,start.year = 1990, end.year = 2010 #Start and end years to plot
            ,all.women = FALSE
             ,all.womenize.fig.name = isTRUE(all.women)
            ,method.to.plot = c("any", "modern", "demand-satisfied-modern")
            ,aggregates.to.plot = "UNPD"
            ,legend.spec = NULL
             ,x.axis.lim = NULL
            ,fig.name = NULL
             ){

        if (is.null(fig.dir)){
            fig.dir <- file.path(getwd(), "fig/")
            dir.create(fig.dir, showWarnings = FALSE)
        }
        if (is.null(output.dir)){
            output.dir <- file.path(getwd(), "output", run.name, "/")
        }
        load(file = file.path(output.dir,"mcmc.meta.rda")) # change JR, 20140418

    if(mcmc.meta$general$do.country.specific.run) {
        warning("This is a one country run: no plots produced.")
        return(invisible())
    }
        if(mcmc.meta$general$marital.group == "UWRA") UWRA <- TRUE
        else UWRA <- FALSE

        age.group <- mcmc.meta$general$age.group
        if(is.null(age.group)) age.group <- "15-49"

        if(all.women) {
            load(file = file.path(output.dir,"res.aggregate.all.women.rda")) # change JR, 20140418
            res.aggregate <- res.aggregate.all.women
            ## [MCW-2017-08-14-25] :: Make ~.pdf~ name sensitive to value of
            ## ~all.women~ as was done for CI plots and tables.
            if(all.womenize.fig.name) {
                fig.run.name <- makeAWFileName(run.name)
                } else fig.run.name <- run.name
        } else {
            load(file = file.path(output.dir,"res.aggregate.rda")) # change JR, 20140418
            fig.run.name <- run.name
        }
        percentiles <- dimnames(res.aggregate$CIprop.Lg.Lcat.qt[[1]][[1]])[[1]]
        est.years <- dimnames(res.aggregate$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]]

        if(is.null(x.axis.lim)) {
            if(UWRA && !method.to.plot[1] == "demand-satisfied-modern") {
                x.axis.lim <- c(0, 0.8)
            } else {
                x.axis.lim <- c(0, 1)
            }
        } else {
            if(any(x.axis.lim > 1)) stop("'x.axis.lim' should be in [0, 1].")
            }

        if(method.to.plot[1] == "any") {
            DoPlot <- function(){
                year <- as.character(end.year + 0.5)
                cat <- "Total"
                med2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                year <- as.character(start.year + 0.5)
                med1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                order <- order(med2010)

                for (s in seq(2,n.subreg,2)){
                    polygon(c(0,0,1,1,0), c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA",
                            col = adjustcolor("lightgrey", alpha.f = 0.4))
                }
                segments(rep(0, n.subreg), seq(1, n.subreg)-0.5,
                         rep(1, n.subreg),seq(1, n.subreg)-0.5)
                                #  segments(rep(-1, n.subreg), seq(1, n.subreg)+0.5,rep(1, n.subreg),seq(1, n.subreg)+0.5)

                segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                         seq(1+0.25, n.subreg+0.25), lwd= 2*ifelse(plot.tiff,1,2), col = "turquoise")
                segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = "blue",
                         seq(1+0.1, n.subreg+0.1), lwd= 2*ifelse(plot.tiff,1,2))
                segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                         seq(1+0.25, n.subreg+0.25), lwd= 1, col =1)
                segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = 1,
                         seq(1+0.1, n.subreg+0.1), lwd= 1)

                med2010t <- med2010
                med1990t <- med1990

                                # add unmet
                year <- as.character(end.year + 0.5)
                cat <- "Unmet"
                med2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                year <- as.character(start.year + 0.5)
                med1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]

                if(UWRA) {
                    no.plot <- names(med2010) %in% c("Northern Europe", "Australia/New Zealand"
                                                    ,"Northern America", "Western Europe"
                                                    ,"Sourthern Europe", "Eastern Europe"
                                                    ,"Eastern Asia", "Mela-Micro-Polynesia"
                                                    ,"Northern Africa")
                    med2010[no.plot] <- NA
                    low2010[no.plot] <- NA
                    up2010[no.plot] <- NA
                    med1990[no.plot] <- NA
                    low1990[no.plot] <- NA
                    up1990[no.plot] <- NA
                }

                segments(low1990[order], seq(1-.1, n.subreg-0.1), up1990[order],
                         seq(1-0.1, n.subreg-0.1), lwd= 2*ifelse(plot.tiff,1,2), col = "pink")
                segments(low2010[order], seq(1-0.25, n.subreg-0.25), up2010[order], col = "red",
                         seq(1-0.25, n.subreg-0.25), lwd = 2*ifelse(plot.tiff,1,2))

                segments(low1990[order], seq(1-.1, n.subreg-0.1), up1990[order],
                         seq(1-0.1, n.subreg-0.1), lwd= 1, col = 1)
                segments(low2010[order], seq(1-0.25, n.subreg-0.25), up2010[order], col = 1,
                         seq(1-0.25, n.subreg-0.25), lwd = 1)

                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order], cex = 1.2, pch = 19, col = "turquoise")
                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order],  pch = 1, cex = 1.2)
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order], cex = 1.2, pch = 19, col = "blue")
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order],  pch = 1,cex = 1.2)
                points(seq(1-.1, n.subreg-0.1)~ med1990[order], cex = 1.2, pch = 19, col = "pink")
                points(seq(1-.1, n.subreg-0.1)~ med1990[order],  pch = 1,cex = 1.2)
                points(seq(1-.25, n.subreg-0.25)~ med2010[order], cex = 1.2, pch = 19, col = "red")
                points(seq(1-.25, n.subreg-0.25)~ med2010[order],  pch = 1,cex = 1.2)
                axis(2, pos = 0, at = seq(1, n.subreg),  tick = T,
                     labels =name.s[order], las = 1, cex.axis = 1)
            }
        } else if(method.to.plot[1] == "modern") {

            DoPlot <- function(){
                year <- as.character(end.year + 0.5)
                cat <- "Modern"
                med2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                year <- as.character(start.year + 0.5)
                med1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                order <- order(med2010)
                for (s in seq(2,n.subreg,2)){
                    polygon(c(0,0,1,1,0), c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA",
                            col = adjustcolor("lightgrey", alpha.f = 0.4))
                }
                segments(rep(0, n.subreg), seq(1, n.subreg)-0.5,
                         rep(1, n.subreg),seq(1, n.subreg)-0.5)
                ##  segments(rep(-1, n.subreg), seq(1, n.subreg)+0.5,rep(1, n.subreg),seq(1, n.subreg)+0.5)

                segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                         seq(1+0.25, n.subreg+0.25), lwd= 2*ifelse(plot.tiff,1,2), col = "turquoise")
                segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = "blue",
                         seq(1+0.1, n.subreg+0.1), lwd= 2*ifelse(plot.tiff,1,2))
                segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                         seq(1+0.25, n.subreg+0.25), lwd= 1, col =1)
                segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = 1,
                         seq(1+0.1, n.subreg+0.1), lwd= 1)

                med2010t <- med2010
                med1990t <- med1990

                ## add unmet need for modern methods
                year <- as.character(end.year + 0.5)
                cat1 <- "Traditional"
                cat2 <- "Unmet"
                med2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat1]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)] +
                    unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                        l[[cat2]][percentiles=="0.5", est.years = year]))[
                        is.element(name.g, name.s)]
                low2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat1]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)] +
                    unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                        l[[cat2]][percentiles=="0.025", est.years = year]))[
                        is.element(name.g, name.s)]
                up2010 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat1]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)] +
                    unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                        l[[cat2]][percentiles=="0.975", est.years = year]))[
                        is.element(name.g, name.s)]
                year <- as.character(start.year + 0.5)
                med1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat1]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)] +
                    unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                        l[[cat2]][percentiles=="0.5", est.years = year]))[
                        is.element(name.g, name.s)]
                low1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat1]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)] +
                    unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                        l[[cat2]][percentiles=="0.025", est.years = year]))[
                        is.element(name.g, name.s)]
                up1990 <- unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                    l[[cat1]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)] +
                    unlist(lapply(res.aggregate$CIprop.Lg.Lcat.qt, function(l)
                        l[[cat2]][percentiles=="0.975", est.years = year]))[
                        is.element(name.g, name.s)]

                if(UWRA) {
                    no.plot <- names(med2010) %in% c("Northern Europe", "Australia/New Zealand"
                                                    ,"Northern America", "Western Europe"
                                                    ,"Sourthern Europe", "Eastern Europe"
                                                    ,"Eastern Asia", "Mela-Micro-Polynesia"
                                                    ,"Northern Africa")
                    med2010[no.plot] <- NA
                    low2010[no.plot] <- NA
                    up2010[no.plot] <- NA
                    med1990[no.plot] <- NA
                    low1990[no.plot] <- NA
                    up1990[no.plot] <- NA
                }
                segments(low1990[order], seq(1-.1, n.subreg-0.1), up1990[order],
                         seq(1-0.1, n.subreg-0.1), lwd= 2*ifelse(plot.tiff,1,2), col = "pink")
                segments(low2010[order], seq(1-0.25, n.subreg-0.25), up2010[order], col = "red",
                         seq(1-0.25, n.subreg-0.25), lwd = 2*ifelse(plot.tiff,1,2))

                segments(low1990[order], seq(1-.1, n.subreg-0.1), up1990[order],
                         seq(1-0.1, n.subreg-0.1), lwd= 1, col = 1)
                segments(low2010[order], seq(1-0.25, n.subreg-0.25), up2010[order], col = 1,
                         seq(1-0.25, n.subreg-0.25), lwd = 1)

                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order], cex = 1.2, pch = 19, col = "turquoise")
                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order],  pch = 1, cex = 1.2)
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order], cex = 1.2, pch = 19, col = "blue")
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order],  pch = 1,cex = 1.2)
                points(seq(1-.1, n.subreg-0.1)~ med1990[order], cex = 1.2, pch = 19, col = "pink")
                points(seq(1-.1, n.subreg-0.1)~ med1990[order],  pch = 1,cex = 1.2)
                points(seq(1-.25, n.subreg-0.25)~ med2010[order], cex = 1.2, pch = 19, col = "red")
                points(seq(1-.25, n.subreg-0.25)~ med2010[order],  pch = 1,cex = 1.2)

                axis(2, pos = 0, at = seq(1, n.subreg),  tick = T,
                     labels =name.s[order], las = 1, cex.axis = 1)
            }
        } else if (method.to.plot[1] == "demand-satisfied-modern") {

            DoPlot <- function(){
                year <- as.character(end.year + 0.5)
                cat <- "Met Demand with Modern Methods"
                med2010 <- unlist(lapply(res.aggregate$CIratio.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low2010 <- unlist(lapply(res.aggregate$CIratio.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up2010 <- unlist(lapply(res.aggregate$CIratio.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                year <- as.character(start.year + 0.5)
                med1990 <- unlist(lapply(res.aggregate$CIratio.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.5", est.years = year]))[
                    is.element(name.g, name.s)]
                low1990 <- unlist(lapply(res.aggregate$CIratio.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.025", est.years = year]))[
                    is.element(name.g, name.s)]
                up1990 <- unlist(lapply(res.aggregate$CIratio.Lg.Lcat.qt, function(l)
                    l[[cat]][percentiles=="0.975", est.years = year]))[
                    is.element(name.g, name.s)]
                order <- order(med2010)
                for (s in seq(2,n.subreg,2)){
                    polygon(c(0,0,1,1,0), c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA",
                            col = adjustcolor("lightgrey", alpha.f = 0.4))
                }
                segments(rep(0, n.subreg), seq(1, n.subreg)-0.5,
                         rep(1, n.subreg),seq(1, n.subreg)-0.5)
                ##  segments(rep(-1, n.subreg), seq(1, n.subreg)+0.5,rep(1, n.subreg),seq(1, n.subreg)+0.5)

                segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                         seq(1+0.25, n.subreg+0.25), lwd= 2*ifelse(plot.tiff,1,2), col = "turquoise")
                segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = "blue",
                         seq(1+0.1, n.subreg+0.1), lwd= 2*ifelse(plot.tiff,1,2))
                segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                         seq(1+0.25, n.subreg+0.25), lwd= 1, col =1)
                segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = 1,
                         seq(1+0.1, n.subreg+0.1), lwd= 1)

                med2010t <- med2010
                med1990t <- med1990

                if(UWRA) {
                    no.plot <- names(med2010) %in% c("Northern Europe", "Australia/New Zealand"
                                                    ,"Northern America", "Western Europe"
                                                    ,"Sourthern Europe", "Eastern Europe"
                                                    ,"Eastern Asia", "Mela-Micro-Polynesia"
                                                    ,"Northern Africa")
                    med2010[no.plot] <- NA
                    low2010[no.plot] <- NA
                    up2010[no.plot] <- NA
                    med1990[no.plot] <- NA
                    low1990[no.plot] <- NA
                    up1990[no.plot] <- NA
                }

                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order], cex = 1.2, pch = 19, col = "turquoise")
                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order],  pch = 1, cex = 1.2)
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order], cex = 1.2, pch = 19, col = "blue")
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order],  pch = 1,cex = 1.2)

                axis(2, pos = 0, at = seq(1, n.subreg),  tick = T,
                     labels =name.s[order], las = 1, cex.axis = 1)
            }
        }

        ##details<< Figure \code{CIspropsubregional} is added to \code{fig.dir}.
        if(is.null(fig.name)) {
            if(method.to.plot[1] == "any") {
                fig.name <- file.path(fig.dir, paste0(fig.run.name, "CIspropsubregional"))
            } else if(method.to.plot[1] == "modern") {
                fig.name <- file.path(fig.dir, paste0(fig.run.name, "CIspropsubregional_modern"))
            } else if(method.to.plot[1] == "demand-satisfied-modern") {
                fig.name <- file.path(fig.dir, paste0(fig.run.name, "CIspropsubregional_met_demand_modern"))
            }
            if(is.character(aggregates.to.plot)) fig.name <- paste0(fig.name, "_", aggregates.to.plot)
            if(!is.null(legend.spec)) {
                if(legend.spec$text == "only.years") fig.name <- paste0(fig.name, "_leg_years")
            }
        }

        if (plot.tiff){
            tiff(filename = paste0(fig.name,".tif"),
                 width = 8*1.5, height = 9*1.25,
                 units = "cm", pointsize = 6,res = 300, bg = "white",compression = "lzw")
        } else {
            pdf(paste0(fig.name,".pdf"), width = 8, height = 9)
        }

        ## first UNDP subregional est's
        region.info <- mcmc.meta$data.raw$region.info
        name.g <- names(res.aggregate$CIprop.Lg.Lcat.qt)
        ## combine regions in Oceania
        nameselect <-  unique(ifelse(is.element(region.info$name.subreg,
                                                c("Melanesia" , "Micronesia", "Polynesia")),
                                     "Mela-Micro-Polynesia", #"Oceania",
                                     region.info$name.subreg))
        select <- is.element(name.g, nameselect)
        name.s <- name.g[select]
        n.subreg <- length(name.s)

        n.plot <- ( # temp assignment to get axis for plot right
            length(nameselect)
            + 1.5 # world
            + 3.5 # dev etc
        )
        if(UWRA) { plotxlab <- paste0("Women aged ", age.group, " unmarried/not in a union (%)")
        } else if(all.women) { plotxlab <- paste0("Women aged ", age.group, " all women (%)")
        } else { plotxlab <- paste0("Women aged ", age.group, " married/in a union (%)") }
        if(method.to.plot[1] != "demand-satisfied-modern") {
            ylim.add <- 3
        } else { ylim.add <- 2 }
        par(mfrow = c(1,1), mar = c(5,12,3,1), cex.main = 2, cex.axis = 2, cex.lab = 2)
        plot(1, ylab = "", bty = "n", type = "n",
             xlab = plotxlab, cex.lab = 1.2,
             xlim = x.axis.lim,  xaxt= "n", yaxt = "n", ylim = c(0, n.plot + ylim.add))
        axis(1, at = seq(x.axis.lim[1], x.axis.lim[2], 0.1)
            ,labels = seq(x.axis.lim[1], x.axis.lim[2], 0.1)*100, las = 2, cex.axis = 1.2)

if(aggregates.to.plot == "UNPD") {
        DoPlot()
        center <- n.subreg+0.5
        segments(0,center, 1,center)
        i =0
        names.in.output.idx <-
            c("Developing (excl. China)", "Developing regions",  "Developed regions","World") %in%
            names(res.aggregate$CIprop.Lg.Lcat.qt)
        if(!any(names.in.output.idx)) stop("No aggregates of type ", aggregates.to.plot, "in output")

        for (nameadd in c("Developing (excl. China)", "Developing regions",  "Developed regions","World")[names.in.output.idx]){
            i = i+1
            center <- center+1
            s <- 0
            if (i==4) center = center+0.5
            if (i!=2){
                polygon(c(0,0,1,1,0), center+c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA", col = "lightgrey")
                segments(0,center-0.5, 1,center-0.5)
            }

            axis(2, pos = 0, at = center, labels = nameadd, las = 1, cex.axis = 1)
            segments(0,center-0.5, 1,center-0.5)
            segments(0,center+0.5, 1,center+0.5)

            if(method.to.plot[1] != "demand-satisfied-modern") {

                med9u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles=="0.5", est.years == as.character(start.year + 0.5)]
                CI9u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles, c("0.025", "0.975")),
                                                                                 est.years == as.character(start.year + 0.5)]

                med2010u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles=="0.5", est.years == as.character(end.year + 0.5)]

                CI2010u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles, c("0.025", "0.975")),
                                                                                    est.years == as.character(end.year + 0.5)]
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = "pink", lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 2, lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = 1, lwd = 1)
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 1, lwd = 1)
                points(center-0.1~ med9u, cex = 1.2, pch = 19, col = "pink")
                points(center-0.1~ med9u,pch = 1,cex = 1.2)
                points(center-0.25~ med2010u, cex = 1.2, pch = 19, col = 2)
                points(center-0.25~ med2010u, pch = 1,cex = 1.2)

                med9u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles=="0.5", est.years == as.character(start.year + 0.5)]
                CI9u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles, c("0.025", "0.975")), est.years == as.character(start.year + 0.5)]
                med2010u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles=="0.5", est.years == as.character(end.year + 0.5)]
                CI2010u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles, c("0.025", "0.975")),est.years == as.character(end.year + 0.5)]

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col = "turquoise", lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = "blue", lwd = 2*ifelse(plot.tiff,1,2))

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col =1, lwd = 1)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = 1, lwd = 1)
                points(center+0.25~ med9u, cex = 1.2, pch = 19, col = "turquoise")
                points(center+0.25~ med9u, pch = 1,cex = 1.2)
                points(center+0.1~ med2010u, cex = 1.2, pch = 19, col = "blue")
                points(center+0.1~ med2010u,pch = 1,cex = 1.2)

            } else {

                med9u <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][percentiles=="0.5", est.years == as.character(start.year + 0.5)]
                CI9u.q2 <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][is.element(percentiles, c("0.025", "0.975")), est.years == as.character(start.year + 0.5)]
                med2010u <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][percentiles=="0.5", est.years == as.character(end.year + 0.5)]
                CI2010u.q2 <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][is.element(percentiles, c("0.025", "0.975")),est.years == as.character(end.year + 0.5)]

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col = "turquoise", lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = "blue", lwd = 2*ifelse(plot.tiff,1,2))

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col =1, lwd = 1)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = 1, lwd = 1)
                points(center+0.25~ med9u, cex = 1.2, pch = 19, col = "turquoise")
                points(center+0.25~ med9u, pch = 1,cex = 1.2)
                points(center+0.1~ med2010u, cex = 1.2, pch = 19, col = "blue")
                points(center+0.1~ med2010u,pch = 1,cex = 1.2)

            }

        }
} else if(aggregates.to.plot == "WB") {

        DoPlot()
        center <- n.subreg+0.5
        segments(0,center, 1,center)
    i =0
    names.agg <- c("High-income countries"
                   ,"Middle-income countries"
                   ,"Upper-middle-income countries"
                   ,"Lower-middle-income countries"
                  ,"Low-income countries"
                   ,"World")

    names.in.output.idx <- names.agg  %in% names(res.aggregate$CIprop.Lg.Lcat.qt)
        if(!any(names.in.output.idx)) stop("No aggregates of type ", aggregates.to.plot, "in output")

        for (nameadd in names.agg[names.in.output.idx]){
            i = i+1
            center <- center+1
            s <- 0
            if (i==6) center = center+0.5
            if (i != 2 && i != 4){
                polygon(c(0,0,1,1,0), center+c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA", col = "lightgrey")
                segments(0,center-0.5, 1,center-0.5)
            }

            axis(2, pos = 0, at = center, labels = nameadd, las = 1, cex.axis = 1)
            segments(0,center-0.5, 1,center-0.5)
            segments(0,center+0.5, 1,center+0.5)

            if(method.to.plot[1] != "demand-satisfied-modern") {

                med9u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles=="0.5", est.years == as.character(start.year + 0.5)]
                CI9u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles, c("0.025", "0.975")),
                                                                                 est.years == as.character(start.year + 0.5)]

                med2010u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles=="0.5", est.years == as.character(end.year + 0.5)]

                CI2010u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles, c("0.025", "0.975")),
                                                                                    est.years == as.character(end.year + 0.5)]
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = "pink", lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 2, lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = 1, lwd = 1)
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 1, lwd = 1)
                points(center-0.1~ med9u, cex = 1.2, pch = 19, col = "pink")
                points(center-0.1~ med9u,pch = 1,cex = 1.2)
                points(center-0.25~ med2010u, cex = 1.2, pch = 19, col = 2)
                points(center-0.25~ med2010u, pch = 1,cex = 1.2)

                med9u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles=="0.5", est.years == as.character(start.year + 0.5)]
                CI9u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles, c("0.025", "0.975")), est.years == as.character(start.year + 0.5)]
                med2010u <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles=="0.5", est.years == as.character(end.year + 0.5)]
                CI2010u.q2 <- res.aggregate$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles, c("0.025", "0.975")),est.years == as.character(end.year + 0.5)]

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col = "turquoise", lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = "blue", lwd = 2*ifelse(plot.tiff,1,2))

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col =1, lwd = 1)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = 1, lwd = 1)
                points(center+0.25~ med9u, cex = 1.2, pch = 19, col = "turquoise")
                points(center+0.25~ med9u, pch = 1,cex = 1.2)
                points(center+0.1~ med2010u, cex = 1.2, pch = 19, col = "blue")
                points(center+0.1~ med2010u,pch = 1,cex = 1.2)

            } else {

                med9u <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][percentiles=="0.5", est.years == as.character(start.year + 0.5)]
                CI9u.q2 <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][is.element(percentiles, c("0.025", "0.975")), est.years == as.character(start.year + 0.5)]
                med2010u <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][percentiles=="0.5", est.years == as.character(end.year + 0.5)]
                CI2010u.q2 <- res.aggregate$CIratio.Lg.Lcat.qt[[nameadd]][["Met Demand with Modern Methods"]][is.element(percentiles, c("0.025", "0.975")),est.years == as.character(end.year + 0.5)]

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col = "turquoise", lwd = 2*ifelse(plot.tiff,1,2))
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = "blue", lwd = 2*ifelse(plot.tiff,1,2))

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col =1, lwd = 1)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = 1, lwd = 1)
                points(center+0.25~ med9u, cex = 1.2, pch = 19, col = "turquoise")
                points(center+0.25~ med9u, pch = 1,cex = 1.2)
                points(center+0.1~ med2010u, cex = 1.2, pch = 19, col = "blue")
                points(center+0.1~ med2010u,pch = 1,cex = 1.2)

            }
        }
    }

        abline(v = 0)
        abline(v = 1)
        segments(center+1, 0, center+1, 1)
        ##box()
        if(is.null(legend.spec)) legend.spec <- list(posn = "topright")

        if(method.to.plot[1] != "demand-satisfied-modern") {
            if(is.null(legend.spec$text)) {
                if(method.to.plot[1] == "modern") {
                    leg.legend <- c(paste0("Modern methods ", start.year), paste0("Modern methods ", end.year),
                                    paste0("Unmet need for mod. meth. ", start.year), paste0("Unmet need for mod. meth. ", end.year))
                } else if(method.to.plot[1] == "any") {
                    leg.legend <- c(paste0("Any method ", start.year), paste0("Any method ", end.year),
                                    paste0("Unmet need ", start.year), paste0("Unmet need ", end.year))
                }
            } else if(legend.spec$text == "only.years") { #[MCW-2017-09-15-1]:: Only plot years in legend. For Pop Facts.
                if(method.to.plot[1] == "modern") {
                    leg.legend <- c(paste0(start.year), paste0(end.year),
                                    paste0(start.year), paste0(end.year))
                } else if(method.to.plot[1] == "any") {
                    leg.legend <- c(paste0(start.year), paste0(end.year),
                                    paste0(start.year), paste0(end.year))
                }
            }
            x = legend(#1,0, xjust = 1, yjust = 1, #
                legend.spec$posn, plot = F,
                legend = leg.legend, bg = "white",
                col = c("turquoise","blue", "pink", 2), lwd =3, pch = 19, lty = 1, cex = 1)

            legend(x$rect$left, x$rect$top,
                   legend = leg.legend, bg = "white",
                   col = c("turquoise","blue", "pink", 2), lwd =3, pch = 19, lty = 1, cex = 1)

            ##   legend(x$rect$left, x$rect$top,
            ##          legend = rep("0919",4), bty = "n",
            ##     col = 1, lwd =1, pch = 1, lty = 1, cex = 1.2)
        } else {
            if(is.null(legend.spec$text)) {
                leg.legend <- c(paste0("Met Demand (Modern) ", start.year), paste0("Met Demand (Modern) ", end.year))
            } else if(legend.spec$text == "only.years") {
                leg.legend <- c(paste0(start.year), paste0(end.year))
            }
            x = legend(#1,0, xjust = 1, yjust = 1, #
                legend.spec$posn, plot = F,
                legend = leg.legend, bg = "white",
                col = c("turquoise","blue"), lwd =3, pch = 19, lty = 1, cex = 1)

            legend(x$rect$left, x$rect$top,
                   legend = leg.legend, bg = "white",
                   col = c("turquoise","blue"), lwd =3, pch = 19, lty = 1, cex = 1)
        }

        dev.off()
        cat("CIs for unmet and total for subregions plotted.\n")
        ##value<< NULL
        return(invisible())
    }

##-----------------------------------------------------------------------------------------------
PlotLogisticParameters <- function (# Plot overview of country parameters of the logistic trends
  ### Plot overview of country parameters of the logistic trends: scatter plots of medians and CIs
  par.ciq, ##<< Output from \code{\link{GetParInfo}}
  country.info, region.info,
  fig.name = NULL
 ,UWRA = FALSE ##[MCW-2016-08-16-3] to allow modifications to plots for unmarried women.
  ,write.model.fun = NULL
  ){

  ##details<< Note: pace parameter omega of logistic curve is expressed as
  ## the no of years needed for an increase of 60% on the 0-asymptote scale
                                        #perc80 <- (logit(0.9) - logit(0.1))

    ## - [MCW-2016-09-08-6] :: If unmarried, do time for 40% increase and change ylim.
    if(UWRA) {
        perc40 <- (logit(0.7) - logit(0.3))
        ylab.perc <- "# years needed for 40% increase"
        perc <- perc40
        ylim.omega.tot <- expression(c(0,max(yearsomega, 150)))
        ylim.omega.rat <- expression(c(0,max(yearsomega, 60)))
    } else {
        perc60 <- (logit(0.8) - logit(0.2))
        ylab.perc <- "# years needed for 60% increase"
        perc <- perc60
        ylim.omega.tot <- expression(c(0,max(yearsomega, 250)))
        ylim.omega.rat <- ylim.omega.tot
    }

    if(!is.null(write.model.fun)) {
        rate.model <- ModelFunctionRateModel(write.model.fun)
        } else rate.model <- FALSE

  yearsomega <- perc/par.ciq[,"omega.c",2]
  yearsomega.low <- perc/par.ciq[,"omega.c",3]
  yearsomega.up <- perc/par.ciq[,"omega.c",1]

  yearsRomega <- perc/par.ciq[,"Romega.c",2]
  yearsRomega.low <- perc/par.ciq[,"Romega.c",3]
  yearsRomega.up <- perc/par.ciq[,"Romega.c",1]

    if(!rate.model) {
  t0low <- par.ciq[,"T.c",1]
  t0up <- par.ciq[,"T.c",3]
  }
  Rt0low <- par.ciq[,"RT.c",1]
  Rt0up <- par.ciq[,"RT.c",3]
    Rt0.c <- par.ciq[,"RT.c" ,2]
    if(!rate.model) {
        t0.c <- par.ciq[,"T.c",2]
        }
  omega.c <- par.ciq[,"omega.c",2]
  Romega.c <-par.ciq[,"Romega.c",2]
  pmax.c <- par.ciq[,"pmax.c",2]
  Rmax.c <- par.ciq[,"Rmax.c",2]

  if (!is.null(fig.name)) {
    pdf(fig.name, width = 14, height = 8)
  }
  ##----------------------------------------------------------------------------------
  # omega against Tmid
    if(!rate.model) {
        plot.xlim <- c(min(t0.c, 1850),max(t0.c,2050))
    par(mfrow = c(1,2),  mar = c(5,5,5,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  plot(yearsomega~t0.c, type = "n", main = "Latent trend: Total prevalence",
       ylab = ylab.perc, xlab = "Midpoint increase", #[MCW-2016-09-08-7] :: Change plot y-axis label.
       xlim = plot.xlim, ylim = eval(ylim.omega.tot))
  text(perc/omega.c~t0.c, labels = country.info$code.c, col =1+as.numeric(country.info$reg.c))
  legend("topleft", legend = region.info$name.reg.short, col = seq(2, 1+region.info$n.reg), lwd = 3,
         pch = 1, lty = -1)
  plot(yearsomega~t0.c, type = "n", main = "Latent trend: Total prevalence",
       ylab = ylab.perc, xlab = "Midpoint increase",#[MCW-2016-09-08-8] :: Change plot y-axis label.
       xlim = plot.xlim, ylim = eval(ylim.omega.tot))
  segments(t0.c, yearsomega.low, t0.c, yearsomega.up, col =1+as.numeric(country.info$reg.c))
        segments(t0low, yearsomega, t0up, yearsomega, col =1+as.numeric(country.info$reg.c))
        }
  ##----------------------------------------------------------------------------------
    if(rate.model) {
        plot.xlim <- c(min(Rt0.c),max(Rt0.c))
        ylim.omega.rat <- c(min(yearsRomega), max(yearsRomega))
    } else {
        plot.xlim <- c(min(Rt0.c, 1850),max(Rt0.c,2050))
    }
  par(mfrow = c(1,2),  mar = c(5,5,5,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  plot(yearsRomega~Rt0.c, type = "n", main = "Latent trend: Ratio",
       ylab = ylab.perc, xlab = "Midpoint increase",#[MCW-2016-09-08-9] :: Change plot y-axis label.
       xlim = plot.xlim, ylim = eval(ylim.omega.rat))
  text(perc/Romega.c~Rt0.c, labels = country.info$code.c, col =1+as.numeric(country.info$reg.c))
  legend("topleft", legend = region.info$name.reg.short, col = seq(2, 1+region.info$n.reg), lwd = 3,
         pch = 1, lty = -1)
  ##----------------------------------------------------------------------------------
  plot(yearsRomega~Rt0.c, type = "n", main = "Latent trend: Ratio",
       ylab = ylab.perc, xlab = "Midpoint increase",#[MCW-2016-09-08-10] :: Change plot y-axis label.
       xlim = plot.xlim, ylim = eval(ylim.omega.rat))
  segments(Rt0.c, yearsRomega.low, Rt0.c, yearsRomega.up, col =1+as.numeric(country.info$reg.c))
  segments(Rt0low, yearsRomega, Rt0up, yearsRomega, col =1+as.numeric(country.info$reg.c))
  ##----------------------------------------------------------------------------------
  # asymptotes
    ylim.asymp <- c(0.5, 1)
    if(UWRA) ylim.asymp <- c(0, 1)

  par(mfrow = c(1,2),  mar = c(5,5,5,1), cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5)
  plot(pmax.c~Rmax.c, type = "n", main = "Asymptotes",
       ylab = "Total prevalence", xlab = "Ratio",
       xlim = c(0.5,1)
   , ylim = ylim.asymp
     )
  text(pmax.c~Rmax.c, labels = country.info$code.c, col =1+as.numeric(country.info$reg.c))
  legend("topleft", legend = region.info$name.reg.short, col = seq(2, 1+region.info$n.reg), lwd = 3,
         pch = 1, lty = -1)
  plot(pmax.c~Rmax.c, type = "n", main = "Asymptotes",
       ylab = "Total prevalence", xlab = "Ratio",
       xlim = c(0.5,1))
  segments(Rmax.c, par.ciq[,"pmax.c",1],
           Rmax.c, par.ciq[,"pmax.c",3], col = 1+as.numeric(country.info$reg.c))
  segments( par.ciq[,"Rmax.c",1],pmax.c,
            par.ciq[,"Rmax.c",3], pmax.c, col =1+as.numeric(country.info$reg.c))
  #  dev.off()
  if (!is.null(fig.name)) {
    dev.off()
  }
  ##value<< NULL
  return(invisible())
}
##----------------------------------------------------------------------
PlotCountryEstimatesForAggregate <- function (# Create overview country estimates for aggregates
  ### Create overview plots of estimates of proportions/counts over time for
  ### countries within aggregates.
  CI.Lg.Lcat.qt, ##<< Object from class \code{CI.Lg.Lcat.qt}, either a proportion or a count (see next).
  fig.name = NULL, ## If NULL, plot appears in R, else it is saved as fig.name.
  cats = NULL, ## If NULL, all cats from the CI are plotted. Alternatively, subset of those cats
  country.info  ##Object of class \code{\link{country.info}}
 ,mcmc.meta = NULL
  ,all.women = FALSE
  ){
    if(mcmc.meta$general$do.country.specific.run) {
        warning("This is a one country run: no plots produced.")
        return(invisible())
    }
  if (is.null(cats)){
    cats <- names(CI.Lg.Lcat.qt[[1]])
  } else {
    cats <- cats[is.element(cats, names(CI.Lg.Lcat.qt[[1]]))]
  }
  nplot <- ceiling((length(cats)+1)/2)
  name.short.c <- InternalMakeCountryNamesShort(country.info$name.c)
  if (!is.null(fig.name)){
    pdf(fig.name, width = 7, height = 5)
  }
    if(ModelFunctionRegInSA(mcmc.meta$general$write.model.fun)) {
        plot.subreg.c <- country.info$reg.in.sex.ac.unm.c
        plot.name.subreg.c <- country.info$name.reg.in.sex.ac.unm.c
    } else if(ModelFunctionSubRegInSA1India(mcmc.meta$general$write.model.fun)) {
        plot.subreg.c <- country.info$reg.in.sex.ac.unm.SA1sub.c
        plot.name.subreg.c <- country.info$name.reg.in.sex.ac.unm.SA1sub.c
    } else {
        plot.subreg.c <- country.info$subreg.c
        plot.name.subreg.c <- country.info$namesubreg.c
    }

  ## [MCW-2017-08-15-5] :: Select only those countries that have all women
  ## estimates available (e.g., those with denominator counts for both MWRA and
  ## UWRA).
  if(all.women) {
      in.results.idx <- country.info$name.c %in% names(CI.Lg.Lcat.qt)
      plot.subreg.c <- plot.subreg.c[in.results.idx]
      plot.name.subreg.c <- plot.name.subreg.c[in.results.idx]
      }

    for (subreg in 1:max(plot.subreg.c)){

      if(all.women) {
          select.c <-
              seq(1, length(country.info$name.c[in.results.idx]))[plot.subreg.c == subreg]
      } else {
          select.c <- seq(1, length(country.info$name.c))[plot.subreg.c == subreg]
      }
        if(identical(length(select.c), 0L)) next()

    par(mfrow = c(2,nplot), mar = c(2,2,1,1)##, cex.main = 1.5, cex.axis = 1.5, cex.lab = 1.5
        )
    for (cat in cats){
      InternalPlotCountryEstimatesForAggregate(
        CI.Lg.Lcat.qt = CI.Lg.Lcat.qt, cat = cat,
        select.c = select.c)
    }
    par( mar = c(0,0,2,0), cex.lab = 0.8)
    plot(1, type = "n", xaxt = "n", xlab = "", ylab = "", yaxt = "n",
         main = plot.name.subreg.c[select.c[1]], cex.main = 0.6)
    legend("center", legend = name.short.c[plot.subreg.c==subreg],
           col = seq(1, sum(plot.subreg.c==subreg)),
           lty = seq(1, sum(plot.subreg.c==subreg)),
           lwd = 3, cex = 0.8, bty = "n")
  } # end subregs
  if (!is.null(fig.name)){
    dev.off()
  }
  return()
}
##----------------------------------------------------------------------
InternalPlotCountryEstimatesForAggregate <- function (# Create overview country estimates for aggregates
  ### Create overview plots of estimates of proportions/counts over time for
  ### countries within aggregates.
  CI.Lg.Lcat.qt, ##<< Object from class \code{CI.Lg.Lcat.qt}, either a proportion or a count (see next).
  cat, select.c
  ){
  est.years <- as.numeric(names(CI.Lg.Lcat.qt[[1]][[1]][1,]))
  name.g <- names(CI.Lg.Lcat.qt)
  plot(1, type = "n", xlim = c(min(est.years), max(est.years)), ylim = c(0,1), xlab = "Year",
       ylab = paste(cat, "(%)"), main = paste(cat, "(%)"), cex = 0.7, cex.main = 0.7
       ,cex.lab = 0.7)
  col <- 0
  lty = 0
  for (c in select.c){
    col <- col+1
    lty = lty+1
    lines(CI.Lg.Lcat.qt[[name.g[c]]][[cat]][3,]~est.years, lwd = 2, col = col, lty = lty)
  }
}


################################################################################
###
###  DATE: 2018-09-13
###
###  AUTHOR(S): Mark Wheldon
###
###  DESC: Modified version of CIPropChangesSubregions() function to
###  make plots for PAA submission.
###
################################################################################


CIPropChangesSubregions_TwoRuns <-
    function(## The filepaths to the directories with mcmc arrays to extract results from
             res.dir.1,
             res.dir.2,
             ## Names of runs for legend
             name.run.1,
             name.run.2,
             ## The years to plot from mcmc arrays
             year.to.plot.1 = 2015,
             year.to.plot.2 = year.to.plot.1,
             UWRA = FALSE,
             age.group = "15-49",
             aggregates.to.plot = "UNPD",
             legend.spec = NULL,
             x.axis.lim = NULL,
             fig.name = NULL,
             fig.dir = NULL) {

        mcmc.meta1 <-
            get(load(file = file.path(res.dir.1, "mcmc.meta.rda")))
        mcmc.meta2 <-
            get(load(file = file.path(res.dir.2, "mcmc.meta.rda")))

        res.aggregate1 <-
            get(load(file = file.path(res.dir.1, "res.aggregate.rda")))
        res.aggregate2 <-
            get(load(file = file.path(res.dir.2, "res.aggregate.rda")))

        ## Fix differences in subregion names
        res.aggregate1 <-
            lapply(res.aggregate1, function(z) {
                z <- z
                names(z)[names(z) %in% "Australia and New Zealand"] <-
                    "Australia/New Zealand"
                return(z)
            })
        res.aggregate2 <-
            lapply(res.aggregate2, function(z) {
                z <- z
                names(z)[names(z) %in% "Australia and New Zealand"] <-
                    "Australia/New Zealand"
                return(z)
            })

        percentiles1 <- dimnames(res.aggregate1$CIprop.Lg.Lcat.qt[[1]][[1]])[[1]]
        percentiles2 <- dimnames(res.aggregate2$CIprop.Lg.Lcat.qt[[1]][[1]])[[1]]

        est.years1 <- dimnames(res.aggregate1$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]]
        est.years2 <- dimnames(res.aggregate2$CIprop.Lg.Lcat.qt[[1]][[1]])[[2]]

        x.axis.lim <- c(0, 1)

            year1 <- as.character(year.to.plot.1 + 0.5)
        year2 <- as.character(year.to.plot.2 + 0.5)

        DoPlot <- function(){

            ## Total
            cat <- "Total"

            med2010 <- unlist(lapply(res.aggregate1$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles1=="0.5", est.years1 == year1]))[
                is.element(name.g1, name.s1)]
            low2010 <- unlist(lapply(res.aggregate1$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles1=="0.025", est.years1 == year1]))[
                is.element(name.g1, name.s1)]
            up2010 <- unlist(lapply(res.aggregate1$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles1=="0.975", est.years1 == year1]))[
                is.element(name.g1, name.s1)]

            med1990 <- unlist(lapply(res.aggregate2$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles2=="0.5", est.years2 == year2]))[
                is.element(name.g2, name.s2)][names(med2010)]# Put second set of results in same order as first
            low1990 <- unlist(lapply(res.aggregate2$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles2=="0.025", est.years2 == year2]))[
                is.element(name.g2, name.s2)][names(low2010)]
            up1990 <- unlist(lapply(res.aggregate2$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles2=="0.975", est.years2 == year2]))[
                is.element(name.g2, name.s2)][names(up2010)]
            order <- order(med2010)

            for (s in seq(2,n.subreg,2)){
                polygon(c(0,0,1,1,0), c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA",
                        col = adjustcolor("lightgrey", alpha.f = 0.4))
            }
            segments(rep(0, n.subreg), seq(1, n.subreg)-0.5,
                     rep(1, n.subreg),seq(1, n.subreg)-0.5)

            segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                     seq(1+0.25, n.subreg+0.25), lwd= 2, col = "turquoise")
            segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = "blue",
                     seq(1+0.1, n.subreg+0.1), lwd= 2)
            segments(low1990[order], seq(1+.25, n.subreg+0.25), up1990[order],
                     seq(1+0.25, n.subreg+0.25), lwd= 1, col =1)
            segments(low2010[order], seq(1+0.1, n.subreg+0.1), up2010[order], col = 1,
                     seq(1+0.1, n.subreg+0.1), lwd= 1)

            med2010t <- med2010
            med1990t <- med1990

            ## add unmet
            cat <- "Unmet"
            med2010 <- unlist(lapply(res.aggregate1$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles1=="0.5", est.years1 == year1]))[
                is.element(name.g1, name.s1)]
            low2010 <- unlist(lapply(res.aggregate1$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles1=="0.025", est.years1 == year1]))[
                is.element(name.g1, name.s1)]
            up2010 <- unlist(lapply(res.aggregate1$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles1=="0.975", est.years1 == year1]))[
                is.element(name.g1, name.s1)]

            med1990 <- unlist(lapply(res.aggregate2$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles2=="0.5", est.years2 == year2]))[
                is.element(name.g2, name.s2)][names(med2010)]# Put second set of results in same order as first
            low1990 <- unlist(lapply(res.aggregate2$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles2=="0.025", est.years2 == year2]))[
                is.element(name.g2, name.s2)][names(low2010)]
            up1990 <- unlist(lapply(res.aggregate2$CIprop.Lg.Lcat.qt, function(l)
                l[[cat]][percentiles2=="0.975", est.years2 == year2]))[
                is.element(name.g2, name.s2)][names(up2010)]

                if(UWRA) {
                    no.plot <- names(med2010) %in% c("Northern Europe", "Australia/New Zealand"
                                                    ,"Northern America", "Western Europe"
                                                    ,"Sourthern Europe", "Eastern Europe"
                                                    ,"Eastern Asia", "Mela-Micro-Polynesia"
                                                    ,"Northern Africa")
                    med2010[no.plot] <- NA
                    low2010[no.plot] <- NA
                    up2010[no.plot] <- NA
                    med1990[no.plot] <- NA
                    low1990[no.plot] <- NA
                    up1990[no.plot] <- NA
                }

                segments(low1990[order], seq(1-.1, n.subreg-0.1), up1990[order],
                         seq(1-0.1, n.subreg-0.1), lwd= 2, col = "pink")
                segments(low2010[order], seq(1-0.25, n.subreg-0.25), up2010[order], col = "red",
                         seq(1-0.25, n.subreg-0.25), lwd = 2)

                segments(low1990[order], seq(1-.1, n.subreg-0.1), up1990[order],
                         seq(1-0.1, n.subreg-0.1), lwd= 1, col = 1)
                segments(low2010[order], seq(1-0.25, n.subreg-0.25), up2010[order], col = 1,
                         seq(1-0.25, n.subreg-0.25), lwd = 1)

                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order], cex = 1.2, pch = 19, col = "turquoise")
                points(seq(1+0.25, n.subreg+0.25)~ med1990t[order],  pch = 1, cex = 1.2)
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order], cex = 1.2, pch = 19, col = "blue")
                points(seq(1+.1, n.subreg+0.1)~ med2010t[order],  pch = 1,cex = 1.2)
                points(seq(1-.1, n.subreg-0.1)~ med1990[order], cex = 1.2, pch = 19, col = "pink")
                points(seq(1-.1, n.subreg-0.1)~ med1990[order],  pch = 1,cex = 1.2)
                points(seq(1-.25, n.subreg-0.25)~ med2010[order], cex = 1.2, pch = 19, col = "red")
                points(seq(1-.25, n.subreg-0.25)~ med2010[order],  pch = 1,cex = 1.2)
                axis(2, pos = 0, at = seq(1, n.subreg),  tick = T,
                     labels =name.s1[order], las = 1, cex.axis = 1)
        }
        if(is.null(fig.dir)) fig.dir <- "."
        if(is.null(fig.name)) fig.name <- "CIspropsubregional"
        fig.name <- file.path(fig.dir, paste0(fig.name, ".pdf"))

        pdf(fig.name, width = 8, height = 9)

        ## first UNDP subregional est's
        region.info1 <- mcmc.meta1$data.raw$region.info
        ## Fix differences in subregion names
        region.info1$name.subreg[region.info1$name.subreg == "Australia and New Zealand"] <-
            "Australia/New Zealand"
        name.g1 <- names(res.aggregate1$CIprop.Lg.Lcat.qt)
        ## combine regions in Oceania
        nameselect1 <-  unique(ifelse(is.element(region.info1$name.subreg,
                                                c("Melanesia" , "Micronesia", "Polynesia")),
                                     "Mela-Micro-Polynesia", #"Oceania",
                                     region.info1$name.subreg))
        select1 <- is.element(name.g1, nameselect1)
        name.s1 <- name.g1[select1]
        n.subreg1 <- length(name.s1)

        region.info2 <- mcmc.meta2$data.raw$region.info
        ## Fix differences in subregion names
        region.info2$name.subreg[region.info2$name.subreg == "Australia and New Zealand"] <-
            "Australia/New Zealand"
        name.g2 <- names(res.aggregate2$CIprop.Lg.Lcat.qt)
        ## combine regions in Oceania
        nameselect2 <-  unique(ifelse(is.element(region.info2$name.subreg,
                                                c("Melanesia" , "Micronesia", "Polynesia")),
                                     "Mela-Micro-Polynesia", #"Oceania",
                                     region.info2$name.subreg))
        select2 <- is.element(name.g2, nameselect2)
        name.s2 <- name.g2[select2]
        n.subreg2 <- length(name.s2)

        if(!identical(name.s1, name.s2)) {
            warning("Different names for subregions in the two runs. Those not in common have been removed.\nThese are in 'run 1' but not 'run 2':\n  ", paste(name.s1[!(name.s1 %in% name.s2)], collapse = ", "), ".\nThese are in 'run 2' but not 'run 1':\n  ", paste(name.s2[!(name.s2 %in% name.s1)], collapse = ", "), ".")
            name.s1 <- name.s1[name.s1 %in% name.s2]
            n.subreg1 <- length(name.s1)

            name.s2 <- name.s2[name.s2 %in% name.s1]
            n.subreg2 <- length(name.s2)
        }

        if(!identical(n.subreg1, n.subreg2)) {
            warning("Different number of subregions in the two runs: 'run 1' has ", n.subreg1, ", 'run 2' has ", n.subreg2, ".")
        } else {
            n.subreg <- n.subreg1
        }

        n.plot <- ( # temp assignment to get axis for plot right
            length(name.s1)
            + 1.5 # world
            + 3.5 # dev etc
        )
        if(UWRA) { plotxlab <- paste0("Women aged ", age.group, " unmarried/not in a union (%)")
        } else { plotxlab <- paste0("Women aged ", age.group, " married/in a union (%)") }
        ylim.add <- 3
        par(mfrow = c(1,1), mar = c(5,12,3,1), cex.main = 2, cex.axis = 2, cex.lab = 2)
        plot(1, ylab = "", bty = "n", type = "n",
             xlab = plotxlab, cex.lab = 1.2,
             xlim = x.axis.lim,  xaxt= "n", yaxt = "n", ylim = c(0, n.plot + ylim.add))
        axis(1, at = seq(x.axis.lim[1], x.axis.lim[2], 0.1)
            ,labels = seq(x.axis.lim[1], x.axis.lim[2], 0.1)*100, las = 1, cex.axis = 1.2)

if(aggregates.to.plot == "UNPD") {
        DoPlot()
        center <- n.subreg+0.5
        segments(0,center, 1,center)
        i =0
        names.in.output.idx <-
            c("Developing (excl. China)", "Developing regions",  "Developed regions","World") %in%
            name.g1
        if(!any(names.in.output.idx)) stop("No aggregates of type ", aggregates.to.plot, "in output")

        for (nameadd in c("Developing (excl. China)", "Developing regions",  "Developed regions","World")[names.in.output.idx]){
            i = i+1
            center <- center+1
            s <- 0
            if (i==4) center = center+0.5
            if (i!=2){
                polygon(c(0,0,1,1,0), center+c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA", col = "lightgrey")
                segments(0,center-0.5, 1,center-0.5)
            }

            axis(2, pos = 0, at = center, labels = nameadd, las = 1, cex.axis = 1)
            segments(0,center-0.5, 1,center-0.5)
            segments(0,center+0.5, 1,center+0.5)

                med9u <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles2=="0.5", est.years2 == as.character(year2)]
                CI9u.q2 <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles2, c("0.025", "0.975")),
                                                                                 est.years2 == as.character(year2)]

                med2010u <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles1=="0.5", est.years1 == as.character(year1)]
                CI2010u.q2 <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles1, c("0.025", "0.975")),
                                                                                     est.years1 == as.character(year1)]

                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = "pink", lwd = 2)
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 2, lwd = 2)
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = 1, lwd = 1)
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 1, lwd = 1)
                points(center-0.1~ med9u, cex = 1.2, pch = 19, col = "pink")
                points(center-0.1~ med9u,pch = 1,cex = 1.2)
                points(center-0.25~ med2010u, cex = 1.2, pch = 19, col = 2)
                points(center-0.25~ med2010u, pch = 1,cex = 1.2)

                med9u <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles2=="0.5", est.years2 == as.character(year2)]
            CI9u.q2 <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles2, c("0.025", "0.975")), est.years2 == as.character(year2)]

                med2010u <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles1=="0.5", est.years1 == as.character(year1)]
                CI2010u.q2 <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles1, c("0.025", "0.975")),est.years1 == as.character(year1)]

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col = "turquoise", lwd = 2)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = "blue", lwd = 2)

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col =1, lwd = 1)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = 1, lwd = 1)
                points(center+0.25~ med9u, cex = 1.2, pch = 19, col = "turquoise")
                points(center+0.25~ med9u, pch = 1,cex = 1.2)
                points(center+0.1~ med2010u, cex = 1.2, pch = 19, col = "blue")
                points(center+0.1~ med2010u,pch = 1,cex = 1.2)

        }
} else if(aggregates.to.plot == "WB") {

        DoPlot()
        center <- n.subreg+0.5
        segments(0,center, 1,center)
    i =0
    names.agg <- c("High-income countries"
                   ,"Middle-income countries"
                   ,"Upper-middle-income countries"
                   ,"Lower-middle-income countries"
                  ,"Low-income countries"
                   ,"World")

    names.in.output.idx <- names.agg  %in% name.g1
        if(!any(names.in.output.idx)) stop("No aggregates of type ", aggregates.to.plot, "in output")

        for (nameadd in names.agg[names.in.output.idx]){
            i = i+1
            center <- center+1
            s <- 0
            if (i==6) center = center+0.5
            if (i != 2 && i != 4){
                polygon(c(0,0,1,1,0), center+c(s-0.5, s+0.5, s+0.5, s-0.5, s-0.5), border = "NA", col = "lightgrey")
                segments(0,center-0.5, 1,center-0.5)
            }

            axis(2, pos = 0, at = center, labels = nameadd, las = 1, cex.axis = 1)
            segments(0,center-0.5, 1,center-0.5)
            segments(0,center+0.5, 1,center+0.5)

                med9u <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles2=="0.5", est.years2 == as.character(year2)]
                CI9u.q2 <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles2, c("0.025", "0.975")),
                                                                                 est.years2 == as.character(year2)]

                med2010u <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][percentiles1=="0.5", est.years1 == as.character(year1)]

                CI2010u.q2 <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Unmet"]][is.element(percentiles1, c("0.025", "0.975")),
                                                                                    est.years1 == as.character(year1)]
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = "pink", lwd = 2)
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 2, lwd = 2)
                segments(CI9u.q2[1], center-0.1, CI9u.q2[2], center-0.1, col = 1, lwd = 1)
                segments(CI2010u.q2[1], center-0.25, CI2010u.q2[2], center-0.25, col = 1, lwd = 1)
                points(center-0.1~ med9u, cex = 1.2, pch = 19, col = "pink")
                points(center-0.1~ med9u,pch = 1,cex = 1.2)
                points(center-0.25~ med2010u, cex = 1.2, pch = 19, col = 2)
                points(center-0.25~ med2010u, pch = 1,cex = 1.2)

                med9u <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles2=="0.5", est.years2 == as.character(year2)]
                CI9u.q2 <- res.aggregate2$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles2, c("0.025", "0.975")), est.years2 == as.character(year2)]
                med2010u <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][percentiles1=="0.5", est.years1 == as.character(year1)]
                CI2010u.q2 <- res.aggregate1$CIprop.Lg.Lcat.qt[[nameadd]][["Total"]][is.element(percentiles1, c("0.025", "0.975")),est.years1 == as.character(year1)]

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col = "turquoise", lwd = 2)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = "blue", lwd = 2)

                segments(CI9u.q2[1], center+0.25, CI9u.q2[2], center+0.25, col =1, lwd = 1)
                segments(CI2010u.q2[1], center+0.1, CI2010u.q2[2], center+0.1, col = 1, lwd = 1)
                points(center+0.25~ med9u, cex = 1.2, pch = 19, col = "turquoise")
                points(center+0.25~ med9u, pch = 1,cex = 1.2)
                points(center+0.1~ med2010u, cex = 1.2, pch = 19, col = "blue")
                points(center+0.1~ med2010u,pch = 1,cex = 1.2)

        }
    }

        abline(v = 0)
        abline(v = 1)
        segments(center+1, 0, center+1, 1)
        ##box()
        if(is.null(legend.spec)) legend.spec <- list(posn = "topright")

            if(is.null(legend.spec$text)) {

                leg.legend <- c(paste0("CP any method ", name.run.2, " ", as.numeric(year2) - 0.5),
                                paste0("CP any method ", name.run.1, " ", as.numeric(year1) - 0.5),
                                paste0("Unmet need ", name.run.2, " ", as.numeric(year2) - 0.5),
                                paste0("Unmet need ", name.run.1, " ", as.numeric(year1) - 0.5))

            } else if(legend.spec$text == "only.years") { #[MCW-2017-09-15-1]:: Only plot years in legend. For Pop Facts. else if(method.to.plot[1] == "any") {
                    leg.legend <- c(paste0(year1), paste0(year2),
                                    paste0(year1), paste0(year2))
            }
            x = legend(#1,0, xjust = 1, yjust = 1, #
                legend.spec$posn, plot = F,
                legend = leg.legend, bg = "white",
                col = c("turquoise","blue", "pink", 2), lwd =3, pch = 19, lty = 1, cex = 1)

            legend(x$rect$left, x$rect$top,
                   legend = leg.legend, bg = "white",
                   col = c("turquoise","blue", "pink", 2), lwd =3, pch = 19, lty = 1, cex = 0.8)

            ##   legend(x$rect$left, x$rect$top,
            ##          legend = rep("0919",4), bty = "n",
            ##     col = 1, lwd =1, pch = 1, lty = 1, cex = 1.2)

        dev.off()
        cat("CIs for unmet and total for subregions plotted.\n")
        ##value<< NULL
        return(invisible())
    }

##----------------------------------------------------------------------
## The End!
