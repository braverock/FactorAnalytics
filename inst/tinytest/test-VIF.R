# load data 
data(stocksCRSP)
data(factorsSPGMI)

stocks_factors <- selectCRSPandSPGMI(stocks = stocksCRSP, factors = factorsSPGMI,
                                     dateSet = c("2006-01-31", "2010-12-31"), 
                                     stockItems = c("Date", "TickerLast", 
                                                    "CapGroup", "Sector", 
                                                    "Return", "Ret13WkBill",
                                                    "mktIndexCRSP"),
                                     factorItems = c("BP", "LogMktCap", "SEV"), 
                                     capChoice = "SmallCap",
                                     Nstocks = 20)
 
 # fit a fundamental factor model with style variables BP and LogMktCap
 
fundamental_model <- fitFfm(data = stocks_factors, 
                            asset.var = "TickerLast", 
                            ret.var = "Return", 
                            date.var = "Date", 
                            exposure.vars = c("BP", "LogMktCap")
                            )
 
#test for output lengths
out <- vif(fundamental_model)
expect_equal(length(out), 2)
 
 # Fit a Fundamental Sector Factor Model with Intercept
  sector_model <- fitFfm(data = stocks_factors, 
                         asset.var = "TickerLast", 
                         ret.var = "Return", 
                         date.var = "Date", 
                         exposure.vars = "Sector",
                         addIntercept = TRUE)

#Test for errors when  less than 2 exploratory variables are used to fit model. 
expect_error(vif(sector_model),"At least 2 continous variables required to find VIF")


