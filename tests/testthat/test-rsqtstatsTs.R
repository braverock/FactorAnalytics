#Load the data 
data("factorDataSetDjia5Yrs")
data("wtsDjiaGmvLo")
w = wtsDjiaGmvLo
z.score =1.96
#Fit a Ffm
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars="SECTOR")
time.periods = fit$time.periods

#test for output lengths
out <- rsqTstatsTs(fit, isPlot = T, col = "blue", z.alpha =z.score) 
expect_equal(length(out), 3)

out <- rsqTstatsTs(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(class(out), "list")

out <- rsqTstatsTs(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(length(out), 3)

out <- rsqTstatsTs(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(out$`R-squared`, fit$r2)

out <- rsqTstatsTs(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(length(time.periods), length(out$`R-squared`))

out <- rsqTstatsTs(fit, isPlot = F, col = "blue", z.alpha =z.score) 
expect_equal(length(time.periods), nrow(out$tstats))





