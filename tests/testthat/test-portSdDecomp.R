
# Time Series Factor Model
data(managers)
fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
                     factor.names=colnames(managers[,(7:9)]),
                     rf.name=colnames(managers[,10]), data=managers)

expect_equal(is.list(portSdDecomp(fit.macro)), TRUE) 

# random weights
wts = runif(6)
wts = wts/sum(wts)
expect_error(portSdDecomp(fit.macro, wts), 
             "Invalid argument: names of weights vector should match with asset names") 
names(wts) <- colnames(managers)[1:6]
expect_equal(is.list(portSdDecomp(fit.macro,wts)), TRUE) 

#testing error message
expect_error(portSdDecomp(fit.macro, weights = c(0.5,0.5)), 
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
              exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121","EP"),
              date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
              fit.method="WLS", z.score = TRUE)

#generating statistic
expect_equal(is.list(portSdDecomp(fit.cross, wtsStocks145GmvLo)), TRUE) 
expect_equal(is.list(portSdDecomp(fit.cross)), TRUE) 
 

#testing error message
expect_error(portSdDecomp(fit.cross, weights = c(0.5,0.5)), 
             "Invalid argument: incorrect number of weights") 