#Load the data 
data("factorDataSetDjia5Yrs")
data("wtsDjiaGmvLo")

library(data.table)

w = wtsDjiaGmvLo
z.score =1.96
#Fit a Ffm
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars="SECTOR")
time.periods = fit$time.periods

#test for output lengths
out <- fmTstats(fit, isPlot = T, col = "blue", z.alpha =z.score) 
expect_equal(length(out), 2)

out <- fmTstats(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(class(out), "list")

out <- fmTstats(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(length(out), 2)

out <- fmTstats(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(length(time.periods), nrow(out$tstats))





