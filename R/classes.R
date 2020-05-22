
setClass("Validation.Results" 
         ### Validation results, see \code{\link{GetPercentilesLeftOut}}.
         ###  Ignore the rest of this help file as this is not a standard R class.      
         ) 


setClass("data" 
### Data frame, see ReadDataAll (details) 
         ### for elements (does not include country.info and region.info).
         ###  Ignore the rest of this help file as this is not a standard R class.      
         ) 


setClass("mcmc.array" 
### MCMC array with dimensions nsim(s),nchains,nparameters.
         ###  Ignore the rest of this help file as this is not a standard R class.        
         ) 

setClass("country.info", 
### List with country information.
         ###  See "Slots" for information in the list, ignore the rest of this help file as this is not a standard R class.
         representation(
   name.c = "character", ##<< Country names 
   iso.c= "character", ##<< Numeric code
   code.c = "character", ##<< Letter code
   N.c = "numeric", ##<< No of obs
   namereg.c = "character", ##<< UN regional classification
   reg.c = "character", ##<< Integer to represent UN regional classification
   namesubreg.c = "character", ##<< UN subregional classification
   subreg.c = "character", ##<< Integer to represent UN subregional classification
   ssa.c = "character", ##<< (Yes, No)
   dev.c = "character" ##<< (Least developed, developing, developed), coded as 
   ## (Poor, Med, Rich)
         )
) 

setClass("region.info", 
### List with region information.
         ###  See "Slots" for information in the list, ignore the rest of this help file as this is not a standard R class.
         
  representation(
   name.subreg = "character", ##<< Subregion names
   name.reg= "character", ##<< Region names
   name.reg.short = "character", ##<< Region names using NA and LAC for North America and Latin America and the Carribean
   n.subreg = "numeric", ##<< No of subregions
   n.reg = "character" ##<< No of regions
   )) 

setClass("winbugs.data"
### Objects of winbugs.data class are contructed before the MCMC sampling, 
### Winbugs.data is a list, with combined elements from (list.no.validation, list.validation, priorspecs)        
### See \code{\link{GetBugsData}} and \code{\link{GetBugsPriorSpecs}} for details on elements in those lists
###  Ignore the rest of this help file as this is not a standard R class.
         ) 

setClass("Results"
### List with confidence intervals (CIs) for proportions of women in various categories
### as well as for counts of MWRA in different categories, and information on changes in proportions and counts.
### See Value in \code{\link{GetCIs}} and \code{\link{GetAggregates}} for description of elements.
### The same class object is used for countries as well as UNPD aggregates, or user-specified aggregates.
### All its elements with CIs are lists, which elements are named by the respective country/aggregate.
###  Ignore the rest of this help file as this is not a standard R class.    
         )

setClass("mcmc.meta"
### See \code{\link{RunMCMC}} to find out what is included in mcmc.meta. 
         ###  Ignore the rest of this help file as this is not a standard R class. 
        )

setClass("CIs"
         ### See \code{\link{GetCIs}} to find out what is included in mcmc.meta. 
         ###  Ignore the rest of this help file as this is not a standard R class. 
)
