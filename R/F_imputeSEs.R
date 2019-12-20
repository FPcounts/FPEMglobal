ImputeSE<-function(
  data, ##<< class data
  country.info, ##<< class country.info
  data.global = NULL, ##<< class data.global
  do.country.specific.run = FALSE ##<< logical, see \code{\link{RunMCMC}}
){

  ###Info on SEs #Change NC, 20161218

  #Trad
  if(any(data$include.trad.ses==1))
  {
    getse.trad.j<-which(data$include.trad.ses==1)
    getnose.trad.j<-which(data$include.trad.ses==0)
    se.logR.trad.nouse<-data$se.logR.trad.nouse[!is.na(data$se.logR.trad.nouse)]
  }
  if(all(data$include.trad.ses==0))
  {
    getse.trad.j<-se.logR.trad.nouse<-NULL
    getnose.trad.j<-which(data$include.trad.ses==0)
  }

  #Modern
  if(any(data$include.modern.ses==1))
  {
    getse.modern.j<-which(data$include.modern.ses==1)
    getnose.modern.j<-which(data$include.modern.ses==0)
    se.logR.modern.nouse<-data$se.logR.modern.nouse[!is.na(data$se.logR.modern.nouse)]
  }
  if(all(data$include.modern.ses==0))
  {
    getse.modern.j<- se.logR.modern.nouse<-NULL
    getnose.modern.j<-which(data$include.modern.ses==0)
  }

  #Unmet
  if(any(data$include.unmet.ses==1))
  {
    getse.unmet.j<-which(data$include.unmet.ses==1)
    getnose.unmet.j<-which(data$include.unmet.ses==0)
    se.logR.unmet.noneed<-data$se.logR.unmet.noneed[!is.na(data$se.logR.unmet.noneed)]
  }
  if(all(data$include.unmet.ses==0))
  {
    getse.unmet.j<-se.logR.unmet.noneed<-NULL
    getnose.unmet.j<-which(data$include.unmet.ses==0)
  }


  round.years.j <- floor(data$years.j) + 0.5
  # for obs j, find the country c and year index t
  gett.j <- round.years.j  # so t refers to midpoint of calendar year
  J <- length(gett.j)
  C <- length(country.info$iso.c)


  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------
  #Combine known SEs with imputed SEs #Change NC, 20170119

  se.logR.trad.impute<-se.logR.modern.impute<-se.logR.unmet.impute<-rep(NA,J)
  se.logR.trad.impute[getse.trad.j]<-se.logR.trad.nouse
  se.logR.modern.impute[getse.modern.j]<-se.logR.modern.nouse
  se.logR.unmet.impute[getse.unmet.j]<-se.logR.unmet.noneed


  if(!do.country.specific.run)
  {
    max.se.trad.c<-max.se.modern.c<-max.se.unmet.c<-rep(NA,C)
    ####Find the max known SEs for each country that has SEs
    for(c in 1:C)
    {
      select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]

      max.se.trad.c[c]<-ifelse(sum(is.na(se.logR.trad.impute[select]))==length(select),NA,max(se.logR.trad.impute[select][which(!is.na(se.logR.trad.impute[select]))]))
      max.se.modern.c[c]<-ifelse(sum(is.na(se.logR.modern.impute[select]))==length(select),NA,max(se.logR.modern.impute[select][which(!is.na(se.logR.modern.impute[select]))]))
      max.se.unmet.c[c]<-ifelse(sum(is.na(se.logR.unmet.impute[select]))==length(select),NA,max(se.logR.unmet.impute[select][which(!is.na(se.logR.unmet.impute[select]))]))
    }#End country loop

    med.max.trad.se<-median(max.se.trad.c[which(!is.na(max.se.trad.c))])
    med.max.modern.se<-median(max.se.modern.c[which(!is.na(max.se.modern.c))])
    med.max.unmet.se<-median(max.se.unmet.c[which(!is.na(max.se.unmet.c))])
  }#end if loop

  if(do.country.specific.run)
  {
    med.max.trad.se<-data.global$med.max.trad.se
    med.max.modern.se<-data.global$med.max.modern.se
    med.max.unmet.se<-data.global$med.max.unmet.se
  }

    for(c in 1:C){
    select <- seq(1, J)[data$iso.j == country.info$iso.c[c]]
    n.obs.country<-length(select)

    se.zero.trad.index<-which(data$include.trad.ses[select]==0)
    se.one.trad.index<-which(data$include.trad.ses[select]==1)

    se.zero.modern.index<-which(data$include.modern.ses[select]==0)
    se.one.modern.index<-which(data$include.modern.ses[select]==1)

    se.zero.unmet.index<-which(data$include.unmet.ses[select]==0)
    se.one.unmet.index<-which(data$include.unmet.ses[select]==1)

    if(length(se.zero.trad.index)==n.obs.country) se.logR.trad.impute[select[se.zero.trad.index]]<-med.max.trad.se
    if(length(se.zero.modern.index)==n.obs.country) se.logR.modern.impute[select[se.zero.modern.index]]<-med.max.modern.se
    if(length(se.zero.unmet.index)==n.obs.country) se.logR.unmet.impute[select[se.zero.unmet.index]]<-med.max.unmet.se

    ##Trad
    if(length(se.zero.trad.index)!=n.obs.country&&length(se.one.trad.index)!=n.obs.country)
    {
      temp.trad<-rep(NA,length(se.zero.trad.index)+length(se.one.trad.index))

      temp.trad[se.one.trad.index]<-se.logR.trad.impute[select[se.one.trad.index]]
      temp.trad[se.zero.trad.index]<-max(se.logR.trad.impute[select[se.one.trad.index]])

      se.logR.trad.impute[select[se.zero.trad.index]]<-temp.trad[se.zero.trad.index]
    }#End Trad

    ##Modern
    if(length(se.zero.modern.index)!=n.obs.country&&length(se.one.modern.index)!=n.obs.country)
    {
      temp.modern<-rep(NA,length(se.zero.modern.index)+length(se.one.modern.index))

      temp.modern[se.one.modern.index]<-se.logR.modern.impute[select[se.one.modern.index]]
      temp.modern[se.zero.modern.index]<-max(se.logR.modern.impute[select[se.one.modern.index]])

      se.logR.modern.impute[select[se.zero.modern.index]]<-temp.modern[se.zero.modern.index]
    }#End Modern

    ##Unmet
    if(length(se.zero.unmet.index)!=n.obs.country&&length(se.one.unmet.index)!=n.obs.country)
    {
      temp.unmet<-rep(NA,length(se.zero.unmet.index)+length(se.one.unmet.index))

      temp.unmet[se.one.unmet.index]<-se.logR.unmet.impute[select[se.one.unmet.index]]
      temp.unmet[se.zero.unmet.index]<-max(se.logR.unmet.impute[select[se.one.unmet.index]])

      se.logR.unmet.impute[select[se.zero.unmet.index]]<-temp.unmet[se.zero.unmet.index]
    }#End Unmet

  }#End country loop

  return(list(se.logR.trad.impute=se.logR.trad.impute,
              se.logR.modern.impute=se.logR.modern.impute,
              se.logR.unmet.impute=se.logR.unmet.impute,
              med.max.trad.se=med.max.trad.se,
              med.max.modern.se=med.max.modern.se,
              med.max.unmet.se=med.max.unmet.se))

}

