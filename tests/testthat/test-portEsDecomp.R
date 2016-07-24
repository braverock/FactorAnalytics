
# Time Series Factor Model
data(managers)
require(factorAnalytics)
fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
                     factor.names=colnames(managers[,(7:9)]),
                     rf.name="US.3m.TR", data=managers)

expect_equal(is.list(portEsDecomp(fit.macro)), TRUE) 
expect_equal(is.list(portEsDecomp(fit.macro,p=0.9,type='normal')), TRUE) 

# random weights
wts = runif(6)
wts = wts/sum(wts)
expect_equal(is.list(portEsDecomp(fit.macro,wts,p=0.9,type='normal')), TRUE) 

#testing error message
expect_error(portEsDecomp(fit.macro, weights = c(0.5,0.5)), 
             "Invalid argument: incorrect number of weights") 




#Load fundamental and return data 
data("stocks145scores6")
dat = stocks145scores6
dat$DATE = as.yearmon(dat$DATE)
dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]

#Load long-only GMV weights for the return data
data("wtsStocks145GmvLo")
wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  

#fit a fundamental factor model
fit.cross <- fitFfm(data = dat, 
                    exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
                    date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
                    fit.method="WLS", z.score = TRUE)

#generating statistic
expect_equal(is.list(portEsDecomp(fit.cross, wtsStocks145GmvLo, p=0.9,type='normal')), TRUE) 
expect_equal(is.list(portEsDecomp(fit.cross, p=0.9, type='normal')), TRUE) 


#testing error message
expect_error(portEsDecomp(fit.cross, weights = c(0.5,0.5)), 
             "Invalid argument: incorrect number of weights") 



