
#Load fundamental and return data 
data("stocks145scores6")
dat = stocks145scores6
dat$DATE = zoo::as.yearmon(dat$DATE)
dat = dat[dat$DATE >=zoo::as.yearmon("2008-01-01") & dat$DATE <= zoo::as.yearmon("2012-12-31"),]

#Load long-only GMV weights for the return data
data("wtsStocks145GmvLo")
wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  

#fit a fundamental factor model
fit <- fitFfm(data = dat, 
              exposure.vars = c("SECTOR","ROE","BP","PM12M1M","SIZE","ANNVOL1M","EP"),
              date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
              fit.method="WLS", z.score = "crossSection")

#generating statistic
expect_equal(is.numeric(repReturn(fit, isPlot = FALSE, digits = 4)), TRUE) 

expect_equal(is.numeric(repReturn(fit, wtsStocks145GmvLo, isPlot = FALSE, digits = 4)), TRUE) 

expect_equal(is.numeric(repReturn(fit, wtsStocks145GmvLo, isPlot = TRUE, scaleType = "free", 
                                     stripLeft = TRUE,digits = 4, which = 1)), TRUE) 

expect_equal(is.numeric(repReturn(fit, isPlot = TRUE, scaleType = "free", 
                                  stripLeft = TRUE,digits = 4, which = 3)), TRUE) 

expect_equal(is.numeric(repReturn(fit, wtsStocks145GmvLo, isPlot = TRUE, scaleType = "same", 
                                  stripLeft = FALSE, which = 2)), TRUE) 

expect_equal(is.numeric(repReturn(fit, wtsStocks145GmvLo, isPlot = TRUE, scaleType = "same", 
                                  stripLeft = TRUE, which = 3, layout = c(3,3))), TRUE) 

#testing error message
expect_error(repReturn(fit, weights = c(0.5,0.5), isPlot = TRUE, which = 1,
                          add.grid = FALSE, zeroLine = TRUE, color = 'Blue'), 
             "Invalid argument: incorrect number of weights") 