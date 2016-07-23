
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




