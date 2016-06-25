#' @title Portfolio tabular reports for risk decomposition and performance analysis
#' 
#' @description 
#' 
#' 
#' 
# Not the final version 

##testing functions
source('R/tsPlotMP.R')
source('R/repExposures.R')
source('R/repReturn.R')

##sample data
load("stocks145scores6.rda")
head(stacked.df)

load("stocks145scores6GMVlong.rda")
head(wts.lo)

# GET FIVE YEAR SEGMENT
short = T
if(short)
{stacked.df$DATE = as.yearmon(stacked.df$DATE)
stacked.df = stacked.df[stacked.df$DATE >=as.yearmon("2008-01-01") &
                          stacked.df$DATE <= as.yearmon("2012-12-31"),]}
names(stacked.df)

# FIT FUNDFACMOD
industry.mod <- fitFfm(data = stacked.df, # Change fit object to mixed.mod
                       exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
                       date.var = "DATE", 
                       ret.var = "RETURN", 
                       asset.var = "TICKER", 
                       fit.method="WLS",
                       z.score = F)

repExposures(industry.mod, wts.lo)
#dev.off()
repReturn(industry.mod, wts.lo)
#dev.off()
                                                