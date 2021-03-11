test_that( "portEsDecomp satisfies testcases", {


# Time Series Factor Model
require(PerformanceAnalytics, quietly = TRUE)
data(managers)
fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
                     factor.names=colnames(managers[,(7:9)]),
                     rf.name=colnames(managers[,10]), data=managers)

expect_equal(is.list(portEsDecomp(fit.macro)), TRUE) 
expect_equal(is.list(portEsDecomp(fit.macro,p=0.9,type='normal')), TRUE) 

# random weights
wts = runif(6)
wts = wts/sum(wts)
expect_error(portEsDecomp(fit.macro, wts), 
             "Invalid argument: names of weights vector should match with asset names") 
names(wts) <- colnames(managers)[1:6]
expect_equal(is.list(portEsDecomp(fit.macro,wts)), TRUE) 

#testing error message
expect_error(portEsDecomp(fit.macro, weights = c(0.5,0.5)), 
             "Invalid argument: incorrect number of weights") 




#Load fundamental and return data 
# dat <- readRDS(file= system.file("tests", "stocks145scores6.rds", package= "FactorAnalytics"))
load('../../tests/stocks145scores6.rda')
dat <- stocks145scores6

dat$DATE = as.yearmon(dat$DATE)
dat = dat[dat$DATE >=as.yearmon("2008-01-01") & dat$DATE <= as.yearmon("2012-12-31"),]

#Load long-only GMV weights for the return data
# wtsStocks145GmvLo <- readRDS(file= system.file("tests", "wtsStocks145GmvLo.rds", package= "FactorAnalytics"))
load('../../tests/wtsStocks145GmvLo.rda')


wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  

#fit a fundamental factor model
fit.cross <- fitFfm(data = dat, 
              exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121","EP"),
              date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
              fit.method="WLS", z.score = "crossSection")


#generating statistic
expect_equal(is.list(portEsDecomp(fit.cross, wtsStocks145GmvLo, p=0.9,type='normal')), TRUE) 
expect_equal(is.list(portEsDecomp(fit.cross, p=0.9, type='normal')), TRUE) 


#testing error message
expect_error(portEsDecomp(fit.cross, weights = c(0.5,0.5)), 
             "Invalid argument: incorrect number of weights") 


})
