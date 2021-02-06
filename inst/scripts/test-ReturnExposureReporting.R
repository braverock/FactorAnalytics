#' @title Portfolio return reports for risk decomposition and performance analysis testing
#' 
#' @description testing functions for portfolio return analysis reporting, repReturn, repExposures 
#' 
#' @param ffmObj fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @author Lingjie Yi


library(FactorAnalytics)
##sample data
load("stocks145scores6.rda")
stacked.df = data145
head(stacked.df)

load("wts145stocksGMVlong.rda")
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


repExposures(industry.mod, wts.lo, isPlot = TRUE)
#dev.off()
repReturn(industry.mod, wts.lo, isPlot = TRUE)
#dev.off()
                                                