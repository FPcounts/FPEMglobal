### Copied from 'https://github.com/FPcounts/ContraceptiveUse_MWRA_Update' 2017-12-20
### function renamed

WriteModel_MWRA_Geog_Rate <- function( # Writes BUGS model
  ### Writes BUGS model to output.dir (stored in mcmc.meta)
  mcmc.meta ##<< Object
){
  filename.model <- paste0(ifelse(mcmc.meta$general$do.SS.run.first.pass, "model_pre.txt", "model.txt")) # change JR, 20140414


  #------ MODEL ----------
cat("
#--------------------------------------------------------------
# WriteModel_MWRA_Geog_Rate
# Model for contraceptive use (country-specific)
# Leontine Alkema, 2011 & Jin Rou New, 2013 & Niamh Cahill 2016
#--------------------------------------------------------------

model{",sep="",append=FALSE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

cat("
####Variances/Precisions for AR(1) distortions
tau.tot.st <- tau.tot*(1-pow(rho.tot,2))  #Precicision for first AR term for total CP
tau.tot <- pow(sigma.tot, -2) #Precision for Total CP

tau.rat.st <- tau.rat*(1-pow(rho.rat,2)) #Prec for 1st AR term for MR
tau.rat <- pow(sigma.rat, -2) #Prec for MR

tau.unmet.st <- tau.unmet*(1-pow(rho.unmet,2)) #Prec for 1st AR for Unmet Ratio
tau.unmet <- pow(sigma.ar.unmet, -2) #Prec for Unmet

###1st AR terms
for (c in 1:C){
  theta.ci[c,1] ~ dnorm(0, tau.unmet.st)
  eps.ci[c, 1] ~ dnorm(0, tau.tot.st)
  eta.ci[c,1] ~ dnorm(0, tau.rat.st)
}


####Separate AR(1) for Tot into a different loop #Change NC 20160811

for(c in 1:C){
  for(i in 2:N.obsperiod.c[c]){
      epshat.ci[c,i] <- eps.ci[c,i-1]*rho.tot
      eps.ci[c,i] ~ dnorm(epshat.ci[c,i], tau.tot)
  }
}

# AR-loop for countries with more than 1 obs
##This loop is ran for all the countries that have more than one observation.
##For each country it will run from 2 to the number of observations for the country.
##N.unique.c is a vector containing the number of observations per country
for (z in 1:n.countriesmorethan1obs){
  for (i in 2:N.unique.c[getc.z[z]]){
      thetahat.ci[getc.z[z],i] <- theta.ci[getc.z[z],i-1]*
         pow(rho.unmet, gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])
      etahat.ci[getc.z[z],i] <- eta.ci[getc.z[z],i-1]*
         pow(rho.rat, gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])
      tautheta.ci[getc.z[z],i] <- tau.unmet.st/
         (1-pow(rho.unmet, 2*(gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])))
      taueta.ci[getc.z[z],i] <- tau.rat.st/
         (1-pow(rho.rat, 2*(gett.ci[getc.z[z],i] - gett.ci[getc.z[z],i-1])))

      ##Index e.g., Country 1 obs 2, Counrty 1 obs 3....
      theta.ci[getc.z[z],i] ~ dnorm(thetahat.ci[getc.z[z],i], tautheta.ci[getc.z[z],i])
      eta.ci[getc.z[z],i] ~ dnorm(etahat.ci[getc.z[z],i], taueta.ci[getc.z[z],i])
      }
      }# end AR stuff
      #### til here it can be moved into ar loop if par's are fixed as well

      #--------------------------------------------------------------
      for (c in 1:C){
      for (i in 1:N.unique.c[c]){

      #gett.ci gives the year for observation i in country c

      ####Growth curve for Total CP #Change NC, 20160811
      #Put LGC for Tot in it's own loop later including the AR distortions and looping over all years in the obsperiod
      #Systematic trend no longer estimated in the same way

      ####Growth curve for modern ratio
      Rmu.ci[c, i] <- Romega.c[c]*(gett.ci[c,i] - RT.c[c])
      Rstar.ci[c, i] <- Rmax.c[c]/(1+exp(-Rmu.ci[c, i]))
      # R.ci defined later, depends on whether AR is added or not

      ###Country unmet need trend on logit scale
      logitZstar.ci[c,i] <- (unmet.intercept.c[c]
      + a.unmet
      + b.unmet * (p.ci[c,getest.ci[c,i]] - pmid.for.unmet)
      + c.unmet * pow(p.ci[c,getest.ci[c,i]] - pmid.for.unmet,2))

    } # end unique-obs year loop

      # Asymptotes: 1-level
      # Asymptotes are constrained to be between 50 and 100%
      # logitpmax.c and logitRmax.c should be normal distributions with world mean parameters.
      pmax.c[c] <- (exp(logitpmax.c[c])+0.5)/(1+exp(logitpmax.c[c]))
      logitpmax.c[c] ~ dnorm(lp.world, tau.lpc) ##lp.world ~ dnorm(0,0.01), sigma.lpc ~ dunif(0,5)

      Rmax.c[c] <- (exp(logitRmax.c[c])+0.5)/(1+exp(logitRmax.c[c]))
      logitRmax.c[c] ~ dnorm(lr.world, tau.lrc) ##lr.world ~ dnorm(0,0.01), sigma.lrc ~ dunif(0,5)


      # Omegas: 3-level ###Constraints
      omega.c[c] <- (0.5*exp(logitomega.c[c])+0.01)/(1+exp(logitomega.c[c]))
      logitomega.c[c] ~ dnorm(w.subreg[subreg.c[c]], tau.wc)

      Romega.c[c] <- (0.5*exp(logitRomega.c[c])+0.01)/(1+exp(logitRomega.c[c]))
      logitRomega.c[c] ~ dnorm(Rw.subreg[subreg.c[c]], tau.Rwc)

      #setlevel: 3-level #Change NC, 20160811
      #year.set.index is the index of 1990.5 in the observation period for country c
      s.ci[c,year.set.index[c]]<-setlevel.c[c]
      p.ci[c,year.set.index[c]]<-1/(1+exp(-s.ci[c,year.set.index[c]]))
      #setlevel.c[c]~dnorm(S.subreg[subreg.c[c]],tau.Sc)

      # Midyear ratio modern/total: 3-level
      RT.c[c] ~ dnorm(RT.subreg[subreg.c[c]], tau.RTc)#T(1800,)

      # unmet
      unmet.intercept.c[c] ~ dnorm(unmet.subreg[subreg.c[c]], tau.unmetc)

      } # end country loop


# setlevel total: rich countries
for (i in 1:n.rich){
setlevel.c[crich.index[i]] ~ dnorm(Shigher, tau.higherSc)#T(?,)
}
# setlevel total: other countries, 3-level
for (i in 1:n.notrich){
setlevel.c[cnotrich.index[i]] ~ dnorm(S.subreg[subreg.c[cnotrich.index[i]]], tau.Sc)#T(?,)
}

# dummy # change JR, 20131105
pmax.c[C+1] <- 0
omega.c[C+1] <- 0
RT.c[C+1] <- 0
Rmax.c[C+1] <- 0
Romega.c[C+1] <- 0
setlevel.c[C+1] <- 0 #Change NC, 20160811
unmet.intercept.c[C+1] <- 0

###End Country specific part. Subregion/region hierarchy added later
###These priors depend on country.specifc specification.

#--------------------------------------------------------------
# Likelihood
      # # gett.j gives (year - year.start+1) for obs j
      # # getc.j gives gives c for obs j
      # # ind.j refers to counter (1 if not applicable), ind1 refers to yes/no (1/0).
      # #Change NC, 20160811
      # # getest.j gives the locations of the observations years in the entire observation period.
      # # For anything that includes p.ci use the index getest.j instead of geti.j
      # for (j in 1:J){
      # trad.j[j] <- p.ci[getc.j[j], getest.j[j]] * (1-R.ci[getc.j[j], geti.j[j]]) #Change NC, 20160811
      # modern.j[j] <- p.ci[getc.j[j], getest.j[j]] * (R.ci[getc.j[j], geti.j[j]]) #Change NC, 20160811
      #
      # unmet.j[j] <- (1-p.ci[getc.j[j], getest.j[j]])/(1+exp(-logitZ.j[j])) #Change NC, 20160811

# # getts.jf give unique obs years for each obs // 20160309 cw
# # getis.jf # index of the obs. years // 20160309 cw
# # period estimate
 for (j in 1:J) {
 for (h in 1:getperiod.j[j]) {
 trad.jh[j, h] = p.ci[getc.j[j], getest.jf[j, h]] * (1 - R.ci[getc.j[j], getis.jf[j, h]])
 modern.jh[j, h] = p.ci[getc.j[j], getest.jf[j, h]] * R.ci[getc.j[j], getis.jf[j, h]]
 unmet.jh[j, h] = (1 - p.ci[getc.j[j], getest.jf[j, h]])*(1 / (1 + neg.explogitZ.ci[getc.j[j], getis.jf[j, h]]))
 }
 trad.j[j] = 1 / period.j[j] * inprod(trad.jh[j, 1:getperiod.j[j]], partialtime.xj[1:getperiod.j[j], j])
 modern.j[j] = 1 / period.j[j] * inprod(modern.jh[j, 1:getperiod.j[j]], partialtime.xj[1:getperiod.j[j], j])
 unmet.j[j] = 1 / period.j[j] * inprod(unmet.jh[j, 1:getperiod.j[j]], partialtime.xj[1:getperiod.j[j], j])


      sump.j[j] <- (trad.j[j]*Vtrad.j[j]
      + modern.j[j]* Vmodern.j[j]
      + (1- p.ci[getc.j[j], getest.j[j]])) #Change NC, 20160811

      ###trad.j, modern.j and unmet.j defined at the end.
      p.perturb.ij[1,j] <-  trad.j[j]*Vtrad.j[j]/sump.j[j]
      p.perturb.ij[2,j] <-  modern.j[j]* Vmodern.j[j]/sump.j[j]
      p.perturb.ij[3,j] <- unmet.j[j]/sump.j[j]
      p.perturb.ij[4,j] <- (1- trad.j[j] - modern.j[j] - unmet.j[j])/sump.j[j]

      ###Biases
      ##Inclusion of folk methods
      folkbias.j[j] <- step(folk.ind1.j[j]-0.5)*v.folk* p.perturb.ij[3,j]
      ##Absence of probing
      absprobeqbias.j[j] <- step(abs.probe.q.ind1.j[j]-0.5)* v.abs.probe.q * p.perturb.ij[1,j]
      ##Sterilization
      modposbias.j[j] <- step(mpos.ind1.j[j]-0.5)*v.mpos* p.perturb.ij[4,j]
      modnegbias.j[j] <- step(mneg.ind1.j[j]-0.5)*v.mneg * p.perturb.ij[2,j]

      ####Perturbed proportions adjusted for biases (1-4)
      q.ij[1,j] <- p.perturb.ij[1,j] - absprobeqbias.j[j] + folkbias.j[j]
      q.ij[2,j] <- p.perturb.ij[2,j] + modposbias.j[j] - modnegbias.j[j]
      q.ij[3,j] <- p.perturb.ij[3,j] + absprobeqbias.j[j] - folkbias.j[j]
      q.ij[4,j] <- p.perturb.ij[4,j] - modposbias.j[j] + modnegbias.j[j]
      none.adj.j[j] <- max(0.01,q.ij[3,j]) + q.ij[4,j]

      # Modern only observations
      logit.ymodonly.hat.j[j] <- logit(q.ij[2,j])

      ####Means for the bivariate lognormal likelihood for trad and modern CP ratios
      mu.jn[j,1] <- log(max(0.01, q.ij[1,j])/none.adj.j[j])
      mu.jn[j,2] <- log(max(0.01, q.ij[2,j])/none.adj.j[j])

      Vtrad.j[j] <- (
      V.geo.12i[1,geo.ind.j[j]]
      * V.age.12i[1,age.ind.j[j]]
      * V.hw.12i[1,hw.ind.j[j]]
      * V.emal.12i[1,emal.ind.j[j]]
      * V.sa.12i[1,sa.ind.j[j]]
      * V.posbias.12i[1,posbias.ind.j[j]]
      * V.posage.12i[1, posage.ind.j[j]]
      * V.negage.12i[1, negage.ind.j[j]] )

      Vmodern.j[j] <- (
      V.geo.12i[2,geo.ind.j[j]]  ##geographical region
      * V.age.12i[2,age.ind.j[j]] #Age group different from base (bias unknown)
      * V.hw.12i[2,hw.ind.j[j]] ##Husband and wives or both
      * V.emal.12i[2,emal.ind.j[j]] ##Ever married, all women
      * V.sa.12i[2,sa.ind.j[j]] ##All sexually active
      * V.posbias.12i[2,posbias.ind.j[j]] ## Non-pregnant/fertile/married SA women
      * V.posage.12i[2, posage.ind.j[j]] ##Age group with positive bias
      * V.negage.12i[2, negage.ind.j[j]] ##Age group with negative bias
      )
      } # end loop J observations



# Unmet observations, get mean, skip NAs
for(k in 1:N.unmet) {
  logitratio.yunmet.hat.j[getj.unmet.k[k]] <-
  logit(max(0.01,q.ij[3,getj.unmet.k[k]])/none.adj.j[getj.unmet.k[k]])
}

#--------------------------------------------------------------
# Training sets (data model)
for (k in 1:n.training.breakdown){
ratios.trad.modern.jn[getj.training.k[k],1:2] ~
dmnorm(mu.jn[getj.training.k[k], ],T.j[getj.training.k[k],,])
}

# Modern only
for(k in 1:n.training.modonly) {
logit.ymodonly.j[getj.training.modonly.k[k]] ~
  dnorm(logit.ymodonly.hat.j[getj.training.modonly.k[k]], tau.sourcemodonly)
}

# unmet training
for (k in 1:n.training.unmet){
logitratio.yunmet.j[getj.training.unmet.k[k]] ~ dnorm(
logitratio.yunmet.hat.j[getj.training.unmet.k[k]],
tau.unmet.j[getj.training.unmet.k[k]])
}

# total training set
for (k in 1:n.training.tot){
##  logit.ytothat.j[j] <- logit(1-none.adj.j[j])
### temp
logit.ytothat.j[getj.training.tot.k[k]] <- logit(max(0.01, 1-none.adj.j[getj.training.tot.k[k]]))
logit.ytot.j[getj.training.tot.k[k]] ~ dnorm(
logit.ytothat.j[getj.training.tot.k[k]], tau.sourcetot)
}
#--------------------------------------------------------------
# data model parameters
# Multipliers V in [0,inf): geo, emal, hw, age other, sa (for trad only)
####All of these should get a log normal distribution....
V.sa.12i[1,1] <- 1  ##Sexually active women (trad)
V.geo.12i[1,1] <- 1 ##Geographical region (trad)
V.geo.12i[2,1] <- 1 ##Geographical region (mod)
V.age.12i[1,1] <- 1 ##Age different (trad)
V.age.12i[2,1] <- 1 ##Age different (mod)
V.hw.12i[1,1] <- 1  ##Husband/wives (trad)
V.hw.12i[2,1] <- 1  ##Husband/wives (mod)
V.emal.12i[1,1] <- 1 ##ever married/ all women (trad)
    V.emal.12i[2,1] <- 1 ##evr married/ all women (mod)

for (m in 1:2){
for (i in 2:ncat.emal){
V.emal.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
}
for (i in 2:ncat.hw){
V.hw.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
}
for (i in 2:ncat.geo){
V.geo.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
}
for (i in 2:ncat.age){
V.age.12i[m,i] ~ dlnorm(0, tau.geo.m[m])
}
tau.geo.m[m] <- pow(sigma.geo.m[m], -2)
}

for (i in 2:ncat.sa){
  V.sa.12i[1,i] ~ dlnorm(0, tau.geo.m[1])
}

V.sa.12i[2,1] <- 1
V.posbias.12i[1,1] <- 1
V.posbias.12i[2,1] <- 1
V.posage.12i[1,1] <- 1
V.posage.12i[2,1] <- 1
V.negage.12i[1,1] <- 1
V.negage.12i[2,1] <- 1

# m = 2:
for (i in 2:ncat.sa){
W.sa.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
V.sa.12i[2,i] <- 1+W.sa.12i[2,i]
}

for (i in 2:ncat.posbias){
W.posbias.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
V.posbias.12i[2,i] <- 1+W.posbias.12i[2,i]
}
for (i in 2:ncat.posage){
W.posage.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
V.posage.12i[2,i] <-1+W.posage.12i[2,i]
}
for (i in 2:ncat.negage){
W.negage.12i[2,i] ~ dlnorm(mu.pos.m[2], tau.pos)
V.negage.12i[2,i] <- 1/(1+W.negage.12i[2,i])
}
tau.pos <- pow(sigma.pos, -2)

# m=1
# note: could simplify code and throw out these V's
for (i in 2:ncat.posbias){
V.posbias.12i[1,i] <- 1+exp(mu.pos.m[1])
}
for (i in 2:ncat.posage){
V.posage.12i[1,i] <- 1+exp(mu.pos.m[1])
}
for (i in 2:ncat.negage){
V.negage.12i[1,i] <- 1/(1+exp(mu.pos.m[1]))
}

",sep="",append=TRUE,  file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

  if(mcmc.meta$general$do.country.specific.run){
  if (!is.null(mcmc.meta$winbugs.data$n.training.modern)) {
    if (mcmc.meta$winbugs.data$n.training.modern > 0) {
      if (!mcmc.meta$general$do.SS.run.first.pass) { # change JR, 20140808
        cat("
            # modern training set (for service statistics)
            for (k in 1:n.training.modern) {
            y.modern.j[getj.training.modern.k[k]] ~ dnorm(
            modern.perturb.j[getj.training.modern.k[k]], tau.modern.j[getj.training.modern.k[k]])
            modern.perturb.j[getj.training.modern.k[k]] <- (modern.j[getj.training.modern.k[k]]*bias.modern) /
            (modern.j[getj.training.modern.k[k]]*bias.modern + (1 - modern.j[getj.training.modern.k[k]]))
            tau.modern.j[getj.training.modern.k[k]] <- pow(se.modern.fix[getj.training.modern.k[k]], -2)
            se.modern.fix[getj.training.modern.k[k]]<-se.modern.unif
            }
            se.modern.unif~dunif(0,1)
            ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
      }
      }
  }
  }

if(mcmc.meta$general$do.country.specific.run){
cat("
#--------------------------------------------------------------
# AR(1) Parameters
rho.tot <- rho.tot0
sigma.tot <- sigma.tot0
rho.rat <- rho.rat0
sigma.rat <- sigma.rat0
rho.unmet <- rho.unmet0
sigma.ar.unmet <- sigma.ar.unmet0


#--------------------------------------------------------------
# LGC Hierarchical parameters
for (subreg in 1:n.subreg){
  unmet.subreg[subreg] <- unmet.subreg0[subreg]
  w.subreg[subreg] <- w.subreg0[subreg]
  Rw.subreg[subreg] <- Rw.subreg0[subreg]
  RT.subreg[subreg] <- RT.subreg0[subreg]
  S.subreg[subreg] <- S.subreg0[subreg] #Change NC 20160602
}
Shigher<-Shigher0
lp.world <- lp.world0
lr.world <- lr.world0

#--------------------------------------------------------------
# data model parameters

# add dummy column in case ncol(V) = 1 => V becomes vector!
V.geo.12i[1,max(geo.ind.j)+1] <- 0
V.age.12i[1,max(age.ind.j)+1] <- 0
V.hw.12i[1,max(hw.ind.j)+1] <- 0
V.emal.12i[1,max(emal.ind.j)+1] <- 0
V.sa.12i[1,max(sa.ind.j)+1] <- 0
V.posbias.12i[1,max(posbias.ind.j)+1] <- 0
V.posage.12i[1,max(posage.ind.j)+1] <- 0
V.negage.12i[1,max(negage.ind.j)+1] <- 0

V.geo.12i[2,max(geo.ind.j)+1] <- 0
V.age.12i[2,max(age.ind.j)+1] <- 0
V.hw.12i[2,max(hw.ind.j)+1] <- 0
V.emal.12i[2,max(emal.ind.j)+1] <- 0
V.sa.12i[2,max(sa.ind.j)+1] <- 0
V.posbias.12i[2,max(posbias.ind.j)+1] <- 0
V.posage.12i[2,max(posage.ind.j)+1] <- 0
V.negage.12i[2,max(negage.ind.j)+1] <- 0

#Biases: Folk, MICS, sterilization included/excluded (M+/M-)
v.abs.probe.q <- v.abs.probe.q0
v.mneg <- v.mneg0
v.folk <- v.folk0
v.mpos <- v.mpos0

for (m in 1:2){
  sigma.geo.m[m] <- sigma.geo.m0[m]
} # end m-loop

sigma.pos <- sigma.pos0
mu.pos.m[2] <- mu.pos.m0[2]
mu.pos.m[1] <- mu.pos.m0[1]

#--------------------------------------------------------------
# Source variances
tau.sourcetot <- tau.sourcetot0
sigma.sourcetot <- 1/sqrt(tau.sourcetot)

tau.sourcemodonly <- tau.sourcemodonly0
sigma.sourcemodonly <- 1/sqrt(tau.sourcemodonly)

## Andrew Tait (email 24/3/2018)
## Changed upper limit from 4 to 6
for (s in 1:6){
nonsample.se.trad.s[s]<-nonsample.se.trad.s0[s]
nonsample.se.modern.s[s]<-nonsample.se.modern.s0[s]
cor.trad.modern.s[s]<-cor.trad.modern.s0[s]
}

## [MCW-2018-03-31] FIX sent by Niamh (31/3/2018)... was ' == 5, 4' but now ' == 7, 4'. Always disaggregating PMA as its own source. This might actually be overly defensive because SS are classed as '4 Other' in
for (j in 1:J) {
S.j[j,1,1]<-pow(se.logR.trad.impute[j],2) + pow(nonsample.se.trad.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])],2) #+0.00001
S.j[j,2,2]<-pow(se.logR.modern.impute[j],2) + pow(nonsample.se.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])],2) #+0.00001
S.j[j,1,2]<- cor.trad.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])]*pow(S.j[j,1,1],0.5)*pow(S.j[j,2,2],0.5)
S.j[j,2,1]<- cor.trad.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])]*pow(S.j[j,1,1],0.5)*pow(S.j[j,2,2],0.5) #Assume no covariance for now

T.j[j,1:2,1:2]<-inverse(S.j[j,1:2,1:2])
}


#--------------------------------------------------------------
a.unmet <- a.unmet0
b.unmet <- b.unmet0
c.unmet <- c.unmet0

nonsample.se.unmet.s[1]<-sigma.unmet.dhs0 #Change NC, 20170112
nonsample.se.unmet.s[2]<-sigma.unmet.other0
for(j in 1:J)
{
  tau.unmet.j[j]<-1/(pow(se.logR.unmet.impute[j],2)+pow(nonsample.se.unmet.s[source.ind.unmet.j[j]],2)) #Change NC, 20170112
}


#end main model code, see validation and change priors below
",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
}

else{
  cat("
#--------------------------------------------------------------
####Variances/Precisions for AR(1) distortions
rho.tot ~ dunif(0,rho.max)  #AR coef for Total CP, rho.max=1
sigma.tot ~ dunif(0.01, sigma.ar.max)  #sigma.ar.max=1

rho.rat ~  dunif(0,rho.max) #AR coef for MR
sigma.rat ~ dunif(0.01, sigma.ar.max)

rho.unmet ~ dunif(0,rho.max.unmet) #AR coef for Unmet
sigma.ar.unmet ~ dunif(0.01, sigma.ar.max.unmet) #sigma.ar.max.unmet =1

#--------------------------------------------------------------
# LGC Hierarchical parameters

for (subreg in 1:n.subreg){
  unmet.subreg[subreg] ~ dnorm(0, tau.unmetworld)
  w.subreg[subreg] ~ dnorm(w.reg[reg.subreg[subreg]] , tau.wsubreg)#T(-4.5, 0)
  S.subreg[subreg] ~ dnorm(S.reg[reg.subreg[subreg]] , tau.Ssubreg) #Change NC, 20160811

  Rw.subreg[subreg] ~ dnorm(Rw.reg[reg.subreg[subreg]] , tau.Rwsubreg)#T(-4.5, 0)
  RT.subreg[subreg] ~ dnorm(RT.reg[reg.subreg[subreg]] , tau.RTsubreg)#T(1800,)
}

for (reg in 1:n.reg){
  w.reg[reg] ~ dnorm(w.world, tau.wreg)#T(-4.5, 0)
  S.reg[reg] ~ dnorm(S.world, tau.Sreg) #Change NC, 20160811

  Rw.reg[reg] ~ dnorm(Rw.world, tau.Rwreg)#T(-4.5, 0)
  RT.reg[reg] ~ dnorm(RT.world, tau.RTreg)#T(1800,)
  }

lp.world ~ dnorm(0,0.01)
lr.world ~ dnorm(0,0.01)

# omegas
Rw.world ~ dnorm(-1, 0.01)
w.world ~ dnorm(-1, 0.01)
tau.wreg <- pow(sigma.wreg, -2)
tau.wsubreg <- pow(sigma.wsubreg, -2)
tau.Rwreg <- pow(sigma.Rwreg, -2)
tau.Rwsubreg <- pow(sigma.Rwsubreg, -2)
sigma.wsubreg ~ dunif(0,sigmawregsubreg.upper)
sigma.wreg ~ dunif(0,sigmawregsubreg.upper)
sigma.Rwreg ~ dunif(0,sigmawregsubreg.upper)
sigma.Rwsubreg ~ dunif(0,sigmawregsubreg.upper)

##Set Level #Change NC, 20160811
S.world ~ dnorm(-1, 0.01)
Shigher ~ dnorm(-1,0.01)
tau.Sreg <- pow(sigma.Sreg, -2)
tau.Ssubreg <- pow(sigma.Ssubreg, -2)
sigma.Sreg ~ dunif(0,sigmaSregsubreg.upper)
sigma.Ssubreg ~ dunif(0,sigmaSregsubreg.upper)

# Midpoints
RT.world ~ dnorm(mean.RTworld, tau0.RT)

tau.RTreg <- pow(sigma.RTreg, -2)
tau.RTsubreg <- pow(sigma.RTsubreg, -2)

sigma.RTreg ~ dunif(0,sigmaRTregsubreg.upper)
sigma.RTsubreg ~ dunif(0,sigmaRTregsubreg.upper)

#--------------------------------------------------------------
# data model parameters
# Biases: Folk, MICS, sterilization included/excluded (M+/M-)
v.abs.probe.q ~ dunif(0,1)
v.mneg ~ dunif(0,1)
v.folk ~ dunif(0,1)
v.mpos ~ dunif(0,1)

for (m in 1:2){
  sigma.geo.m[m] ~ dunif(0.01,2)
} # end m-loop

sigma.pos ~ dunif(0.01,2)
mu.pos.m[2] ~ dnorm(-2, 0.64)# 1/1.25^2 ) # 0.01)
mu.pos.m[1] ~ dnorm(-2, 0.64)#~ dnorm(-2, 0.01)#T(-10,) # in Bugs: use I()

#--------------------------------------------------------------
# Source variances
tau.sourcetot ~ dgamma(0.5, halfsigma2.sourcetot0)
sigma.sourcetot <- 1/sqrt(tau.sourcetot)

tau.sourcemodonly ~ dgamma(0.5, halfsigma2.sourcemodonly0)
sigma.sourcemodonly <- 1/sqrt(tau.sourcemodonly)

for(s in 1:6){ ## MCW 2017-12-22 :: There are now six data sources, with PMA disaggregated and CP TOT < 1 percent.
nonsample.se.trad.s[s]~dunif(0.01,2) #Change NC, 20170112
nonsample.se.modern.s[s]~dunif(0.01,2) #Change NC, 20170112
cor.trad.modern.s[s]~dunif(-1,1) #Change NC, 20170112
}

for (j in 1:J) {
S.j[j,1,1]<-pow(se.logR.trad.impute[j],2) + pow(nonsample.se.trad.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])],2) #+0.00001
S.j[j,2,2]<-pow(se.logR.modern.impute[j],2) + pow(nonsample.se.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])],2) #+0.00001
S.j[j,1,2]<- cor.trad.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])]*pow(S.j[j,1,1],0.5)*pow(S.j[j,2,2],0.5)
S.j[j,2,1]<- cor.trad.modern.s[ifelse(source.ind.j[j] == 7, 4, source.ind.j[j])]*pow(S.j[j,1,1],0.5)*pow(S.j[j,2,2],0.5) #Assume no covariance for now

T.j[j,1:2,1:2]<-inverse(S.j[j,1:2,1:2])
}


#--------------------------------------------------------------
a.unmet ~ dnorm(a0.unmet, tau.a0)
b.unmet ~ dnorm(b0.unmet, tau.b0)
c.unmet ~ dunif(-10,0)

sigma.unmet.other ~ dunif(0.01,2)
sigma.unmet.dhs ~ dunif(0.01,2)
nonsample.se.unmet.s[1]<-sigma.unmet.dhs #Change NC, 20170112
nonsample.se.unmet.s[2]<-sigma.unmet.other
for(j in 1:J)
{
  tau.unmet.j[j]<-1/(pow(se.logR.unmet.impute[j],2)+pow(nonsample.se.unmet.s[source.ind.unmet.j[j]],2)) #Change NC, 20170112
}

sigma.unmetworld ~ dunif(0,5)
tau.unmetworld <- pow(sigma.unmetworld,-2)

#end main model code, see validation and change priors below
",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
}
#--------------------------------------------------------------
# Validation
  if (!is.null(mcmc.meta$validation.list)){
    # validation: same as above, but now with test data (and save the output!)
    cat("
        for (k in 1:n.test.unmet){
        pred.logitratio.yunmet.j[getj.test.unmet.k[k]] ~ dnorm(
        logitratio.yunmet.hat.j[getj.test.unmet.k[k]],
            ## >>>>> RATE MODEL
            ##tau.unmet.source.s[source.ind.unmet.j[getj.training.unmet.k[k]]])
            tau.unmet.j[getj.test.unmet.k[k]])
            ## <<<<< RATE MODEL
        q.unmet.j[getj.test.unmet.k[k]] <- q.ij[3,getj.test.unmet.k[k]]
        }
        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

    if (mcmc.meta$validation.list$exclude.unmet.only){
      cat("
          for (k in 1:n.test.unmet){
          # get total too
          pred.ratios.trad.modern.jn[getj.test.unmet.k[k],1:2] ~ dmnorm(
          mu.jn[getj.test.unmet.k[k], ], T.j[getj.test.unmet.k[k],,])
          pred.logratio.ytrad.j[getj.test.unmet.k[k]] <- pred.ratios.trad.modern.jn[getj.test.unmet.k[k],1]
          pred.logratio.ymodern.j[getj.test.unmet.k[k]] <- pred.ratios.trad.modern.jn[getj.test.unmet.k[k],2]
          }
          ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

    } else {
      cat("
          for (k in 1:n.test.breakdown){
          pred.ratios.trad.modern.jn[getj.test.k[k],1:2] ~ dmnorm(
          mu.jn[getj.test.k[k], ], T.j[getj.test.k[k],,])
          pred.logratio.ytrad.j[getj.test.k[k]] <- pred.ratios.trad.modern.jn[getj.test.k[k],1]
          pred.logratio.ymodern.j[getj.test.k[k]] <- pred.ratios.trad.modern.jn[getj.test.k[k],2]
          q.trad.j[getj.test.k[k]] <- q.ij[1,getj.test.k[k]]
          q.modern.j[getj.test.k[k]] <- q.ij[2,getj.test.k[k]]
          }
          ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

    } # end if-!exclude.unmet.only loop
} # end if-validation loop

#------------------------------------------------------------------------------------------
# Priors on country variances kappa
if(!mcmc.meta$general$do.country.specific.run){
  if (mcmc.meta$general$change.priors.to.zerolower){
    cat("
        sigma.lpc ~ dunif(0,5)
        sigma.lrc ~ dunif(0,5)
        sigma.wc ~ dunif(0,2)
        sigma.Rwc ~ dunif(0,2)
        sigma.Sc ~ dunif(0,5) #Change NC, 20160811
        sigma.higherSc ~ dunif(0,5) #Change NC, 20170221
        sigma.RTc ~ dunif(0,30)
        sigma.unmetc ~ dunif(0,5)
        tau.lrc <- pow(sigma.lrc,-2)
        tau.lpc <- pow(sigma.lpc,-2)
        tau.wc <- pow(sigma.wc, -2)
        tau.Rwc <- pow(sigma.Rwc, -2)
        tau.Sc <- pow(sigma.Sc, -2) #Change NC, 20160811
        tau.higherSc <- pow(sigma.higherSc, -2) #Change NC, 20170221
        tau.RTc <- pow(sigma.RTc, -2)
        tau.unmetc <- pow(sigma.unmetc,-2)

        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

  } else {
    cat("
        tau.unmetc ~ dgamma(0.5,halfsigma2.unmetc0)
        tau.lpc <- pow(sigma.lpc,-2) #Change NC, 20160811
        tau.lrc  ~ dgamma(halfnu0,halfnu0sigma2.lrc0)
        tau.wc <- pow(sigma.wc, -2) #Change NC, 20160811
        tau.Sc <- pow(sigma.Sc, -2) #Change NC, 20160811
        tau.higherSc <- pow(sigma.higherSc, -2) #Change NC, 20170221
        tau.Rwc  ~ dgamma(halfnu0,halfnu0sigma2.Rwc0)
        tau.RTc  ~ dgamma(halfnu0,halfnu0sigma2.RTc0)

        sigma.unmetc <- 1/sqrt(tau.unmetc)
        sigma.lpc ~ dunif(0,5) #Change NC, 20160811
        sigma.lrc <- 1/sqrt(tau.lrc)
        sigma.wc ~ dunif(0,2) #Change NC, 20160811
        sigma.Sc ~ dunif(0,5)  #Change NC, 20160811
        sigma.higherSc ~ dunif(0,5) #Change NC, 20170221
        sigma.Rwc <- 1/sqrt(tau.Rwc)
        sigma.RTc <- 1/sqrt(tau.RTc)
        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

  } # end change priors

}

#------------------------------------------------------------------------------------------
# Priors on country variances kappa
if(mcmc.meta$general$do.country.specific.run){
  if (mcmc.meta$general$change.priors.to.zerolower){
    cat("
        sigma.lpc <- sigma.lpc0
        sigma.lrc <- sigma.lrc0
        sigma.wc <- sigma.wc0
        sigma.Sc <- sigma.Sc0 #Change NC, 20160807
        sigma.higherSc <- sigma.higherSc0 #Change NC, 20170221
        sigma.Rwc <- sigma.Rwc0
        sigma.RTc <- sigma.RTc0
        sigma.unmetc <- sigma.unmetc0
        tau.lrc <- pow(sigma.lrc,-2)
        tau.lpc <- pow(sigma.lpc,-2)
        tau.wc <- pow(sigma.wc, -2)
        tau.Sc<-pow(sigma.Sc,-2) #Change NC, 20160807
        tau.higherSc<-pow(sigma.Sc,-2) #Change NC, 20170221
        tau.Rwc <- pow(sigma.Rwc, -2)
        tau.RTc <- pow(sigma.RTc, -2)
        tau.unmetc <- pow(sigma.unmetc,-2)
        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
  } else {
    cat("
        tau.unmetc <- tau.unmetc0
        tau.lpc <- tau.lpc0
        tau.lrc <- tau.lrc0
        tau.wc <- tau.wc0
        tau.Rwc <- tau.Rwc0
        tau.RTc <- tau.RTc0
        tau.Sc <- tau.Sc0 #Change NC, 20160807
        tau.higherSc <- tau.Sc0 #Change NC, 20170221

        sigma.unmetc <- 1/sqrt(tau.unmetc)
        sigma.lpc <- 1/sqrt(tau.lpc)
        sigma.lrc <- 1/sqrt(tau.lrc)
        sigma.wc <- 1/sqrt(tau.wc)
        sigma.Rwc <- 1/sqrt(tau.Rwc)
        sigma.RTc <- 1/sqrt(tau.RTc)
        sigma.Sc<- 1/sqrt(tau.Sc) #Change NC, 20160807
        sigma.higherSc<- 1/sqrt(tau.higherSc) #Change NC, 20170221
        ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
  } # end change priors

}

#--------------------------------------------------------------
##AR Part
if (mcmc.meta$include.AR){
  cat("
      for (c in 1:C){

      ###Move Tot into its own loop #Change NC, 20160602
      ####New Set up for Tot CP based on the Rate(CP) model #Change NC, 20160807
      for(i in 1:(year.set.index[c]-1)){

      ls.ci[c,(year.set.index[c]-i)]<-s.ci[c,((year.set.index[c]-i)+1)]-eps.ci[c,(year.set.index[c]-i)] #logit
      ils.ci[c,(year.set.index[c]-i)]<-1/(1+exp(-ls.ci[c,(year.set.index[c]-i)])) #inv.logit

      #Step function; test for x >/= 0
      I[c,(year.set.index[c]-i)]<-step(ils.ci[c,(year.set.index[c]-i)]-pmax.c[c])

      ###Get p.ct directly in the backward direction
      #Only need this bit if I=0 i.e., ils.ci<pmax.c
      zeta.ci[c,(year.set.index[c]-i)]<-(1-I[c,(year.set.index[c]-i)])*(logit(min((1-0.00001),ils.ci[c,(year.set.index[c]-i)]/pmax.c[c]))-omega.c[c])

      p.ci[c,(year.set.index[c]-i)]<-(1-I[c,(year.set.index[c]-i)])*(pmax.c[c]*(1/(1+exp(-zeta.ci[c,(year.set.index[c]-i)]))))+I[c,(year.set.index[c]-i)]*ils.ci[c,(year.set.index[c]-i)]

      ###Get logit(p.ci)
      s.ci[c,(year.set.index[c]-i)] <- logit(p.ci[c,(year.set.index[c]-i)])

      }

      for(i in (year.set.index[c]+1):N.obsperiod.c[c]){

      #Step function; test for x >/= 0
      I[c,i]<-step(p.ci[c,i-1]-pmax.c[c])

      #Only need this bit if I=0 i.e., p.ci<pmax.c
      zeta.ci[c,i]<-(1-I[c,i])*(logit(min((1-0.000001),p.ci[c,i-1]/pmax.c[c]))+omega.c[c])

      s.ci[c,i]<- logit(I[c,i]*(p.ci[c,i-1]) + (1-I[c,i])*pmax.c[c]*(1/(1+exp(-zeta.ci[c,i]))))+eps.ci[c,i-1]

      ####inv.logit to get p.ci
      p.ci[c,i]<-1/(1+exp(-s.ci[c,i]))

      } #End TotCP loop

      for (i in 1:N.unique.c[c]){
      R.ci[c, i] <- 1/(1+exp(-( logit(Rstar.ci[c,i]) + eta.ci[c,i])))
      }

      for (i in 1:N.unique.c[c]){
      logitZ.ci[c,i] <- logitZstar.ci[c,i] + theta.ci[c, i]
      neg.explogitZ.ci[c,i] = exp(-logitZ.ci[c,i])
      }
    } #End country loop
      ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
} else {
  cat("
      for (c in 1:C){

      ###Move Tot into its own loop #Change NC, 20160602
      ####New Set up for Tot CP based on the Rate(CP) model #Change NC, 20160807
      for(i in 1:(year.set.index[c]-1)){

      ls.ci[c,(year.set.index[c]-i)]<-s.ci[c,((year.set.index[c]-i)+1)] #logit
      ils.ci[c,(year.set.index[c]-i)]<-1/(1+exp(-ls.ci[c,(year.set.index[c]-i)])) #inv.logit

      #Step function; test for x >/= 0
      I[c,(year.set.index[c]-i)]<-step(ils.ci[c,(year.set.index[c]-i)]-pmax.c[c])

      ###Get p.ct directly in the backward direction
      zeta.ci[c,(year.set.index[c]-i)]<-(1-I[c,(year.set.index[c]-i)])*(logit(min((1-0.00001),ils.ci[c,(year.set.index[c]-i)]/pmax.c[c]))-omega.c[c])

      p.ci[c,(year.set.index[c]-i)]<-(1-I[c,(year.set.index[c]-i)])*(pmax.c[c]*(1/(1+exp(-zeta.ci[c,(year.set.index[c]-i)]))))+I[c,(year.set.index[c]-i)]*ils.ci[c,(year.set.index[c]-i)]

      ###Get logit(P.ci)
      s.ci[c,(year.set.index[c]-i)] <- logit(p.ci[c,(year.set.index[c]-i)])

      }

      for(i in (year.set.index[c]+1):N.obsperiod.c[c]){

      #Step function; test for x >/= 0
      I[c,i]<-step(p.ci[c,i-1]-pmax.c[c])

      zeta.ci[c,i]<-(1-I[c,i])*(logit(min((1-0.000001),p.ci[c,i-1]/pmax.c[c]))+omega.c[c])

      s.ci[c,i]<- logit(I[c,i]*(p.ci[c,i-1]) + (1-I[c,i])*pmax.c[c]*(1/(1+exp(-zeta.ci[c,i]))))

      ####CP on the logit scale
      p.ci[c,i]<-1/(1+exp(-s.ci[c,i]))

      } #End TotCP loop

      for (i in 1:N.unique.c[c]){
      R.ci[c, i] <- Rstar.ci[c,i]
      }
      for (i in 1:N.unique.c[c]){
      logitZ.ci[c,i] <- logitZstar.ci[c,i]
      neg.explogitZ.ci[c,i] = exp(-logitZ.ci[c,i])

      }

      } #End country loop
      ",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)

} # end extra AR part

#--------------------------------------------------------------
cat("} # end model",sep="",append=TRUE, file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE)
# THE END!
#--------------------------------------------------------------

#if (){
# cat("
#", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
#} else {
# cat("
# ", file = file.path(mcmc.meta$general$output.dir, filename.model), fill = TRUE, append = T)
#} # end extra part
##value<< NULL
} # end function
#----------------------------------------------------------------------
# The End!

  #--------------------------------------------------------------

