# @title Portfolio return reports for risk decomposition and performance analysis testing
#
# @description testing functions for portfolio return analysis reporting, repReturn, repExposures 
# 
# @param ffmObj fit object of class \code{tsfm}, \code{sfm} or \code{ffm}.
# @author Lingjie Yi
test_that( "fitFfm satisfies testcases", {



##sample data
# load(file= system.file("tests", "stocks145scores6.rda", package= "FactorAnalytics"))
load('../../tests/stocks145scores6.rda')
  
# data("stocks145scores6.rda")
stacked.df = stocks145scores6 #data145
head(stacked.df)
# load(file= system.file("tests", "wtsStocks145GmvLo.rda", package= "FactorAnalytics"))
load('../../tests/wtsStocks145GmvLo.rda')
# load("wtsStocks145GmvLo.rda")
wts.lo<- wtsStocks145GmvLo
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
                       exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121","EP"),
                       date.var = "DATE", 
                       ret.var = "RETURN", 
                       asset.var = "TICKER", 
                       fit.method="WLS",
                       z.score = "none")

# TEST OUTCOME
expect_equal( repExposures(industry.mod, wts.lo, isPlot = FALSE, which = 1)
            , list(Style.Exposures = structure(c(8.8, 48.9, 7, 2322.4, 32.5, 4.7, 45.9, 11.3, 25.7, 50.2, 14.4, 6.9), .Dim = c(6L, 2L)
                                               , .Dimnames = list( c("ROE", "BP", "MOM121", "SIZE", "VOL121", "EP")
                                                                 , c("Mean","Volatility")))
                   , Sec.Exposures = structure(list(c(2.8, 19.5, 8.8, 0, 14.2, 2.7, 5.2, 5.4, 0, 41.4))
                                               , row.names = c("CODISC","COSTAP", "ENERGY", "FINS", "HEALTH", "INDUST", "INFOTK"
                                                               , "MATRLS","TELCOM", "UTILS"), class = "data.frame")) 
            )

expect_equal( repExposures(industry.mod, wts.lo, isPlot = FALSE, which = 2)
            , list(Style.Exposures = structure(c(8.8, 48.9, 7, 2322.4, 32.5,4.7, 45.9, 11.3, 25.7, 50.2, 14.4, 6.9), .Dim = c(6L, 2L)
                                               , .Dimnames = list(c("ROE", "BP", "MOM121", "SIZE", "VOL121", "EP")
                                                                  , c("Mean", "Volatility")))
                   , Sec.Exposures = structure(list(c(2.8, 19.5,8.8, 0, 14.2, 2.7, 5.2, 5.4, 0, 41.4))
                                               , row.names = c("CODISC","COSTAP", "ENERGY", "FINS", "HEALTH", "INDUST", "INFOTK"
                                                              , "MATRLS","TELCOM", "UTILS"), class = "data.frame"))
            )

expect_equal( repExposures(industry.mod, wts.lo, isPlot = FALSE, which = 3)
            , list(Style.Exposures = structure(c(8.8, 48.9, 7, 2322.4, 32.5, 4.7, 45.9, 11.3, 25.7, 50.2, 14.4, 6.9), .Dim = c(6L, 2L)
                                               , .Dimnames = list(c("ROE", "BP", "MOM121", "SIZE", "VOL121", "EP")
                                                                  , c("Mean","Volatility")))
                   , Sec.Exposures = structure(list(c(2.8, 19.5,8.8, 0, 14.2, 2.7, 5.2, 5.4, 0, 41.4))
                                               , row.names = c("CODISC","COSTAP", "ENERGY", "FINS", "HEALTH", "INDUST", "INFOTK"
                                                               , "MATRLS","TELCOM", "UTILS"), class = "data.frame")) 
           )

expect_equal( repReturn(industry.mod, wts.lo, isPlot = FALSE, which = 1)
            , structure(c(0.9, 0.2, 0.7, 0, -0.1, 0, -0.5, -0.3, 0.1, 0.1, 
                          0.3, 0.1, 0, 0.2, 0, 0.1, 0.1, 0, 0.5, 4.5, 2.7, 4, 0.5, 1.1, 
                          1, 9.8, 2, 0.7, 0.3, 1.9, 1.1, 0, 1.4, 0.3, 0.6, 0.6, 0, 4.3)
                        , .Dim = c(19L,2L)
                        , .Dimnames = list(c("PortRet", "ResidRet", "FacRet", "ROE", 
                                            "BP", "MOM121", "SIZE", "VOL121", "EP", "CODISC", "COSTAP", "ENERGY", 
                                            "FINS", "HEALTH", "INDUST", "INFOTK", "MATRLS", "TELCOM", "UTILS")
                                           , c("Mean", "Volatility")))
          )


expect_equal( repReturn(industry.mod, wts.lo, isPlot = FALSE, which = 2)
            , structure(c(0.9, 0.2, 0.7, 0, -0.1, 0, -0.5, -0.3, 0.1, 0.1, 
                         0.3, 0.1, 0, 0.2, 0, 0.1, 0.1, 0, 0.5, 4.5, 2.7, 4, 0.5, 1.1, 
                         1, 9.8, 2, 0.7, 0.3, 1.9, 1.1, 0, 1.4, 0.3, 0.6, 0.6, 0, 4.3)
                       , .Dim = c(19L,2L)
                       , .Dimnames = list(c("PortRet", "ResidRet", "FacRet", "ROE", 
                                            "BP", "MOM121", "SIZE", "VOL121", "EP", "CODISC", "COSTAP", "ENERGY", 
                                            "FINS", "HEALTH", "INDUST", "INFOTK", "MATRLS", "TELCOM", "UTILS")
                                          , c("Mean", "Volatility")))
)

expect_equal( repReturn(industry.mod, wts.lo, isPlot = FALSE, which = 3)
            , structure(c(0.9, 0.2, 0.7, 0, -0.1, 0, -0.5, -0.3, 0.1, 0.1, 
                          0.3, 0.1, 0, 0.2, 0, 0.1, 0.1, 0, 0.5, 4.5, 2.7, 4, 0.5, 1.1, 
                          1, 9.8, 2, 0.7, 0.3, 1.9, 1.1, 0, 1.4, 0.3, 0.6, 0.6, 0, 4.3)
                        , .Dim = c(19L,2L)
                        , .Dimnames = list(c("PortRet", "ResidRet", "FacRet", "ROE", 
                                            "BP", "MOM121", "SIZE", "VOL121", "EP", "CODISC", "COSTAP", "ENERGY", 
                                            "FINS", "HEALTH", "INDUST", "INFOTK", "MATRLS", "TELCOM", "UTILS")
                                           , c("Mean", "Volatility"))) 
            )

})                                                