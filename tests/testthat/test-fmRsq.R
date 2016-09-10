#Load the data 
data("factorDataSetDjia5Yrs")

#Fit a Ffm
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars="SECTOR")

#test for output lengths
out <- fmRsq(fit)
expect_equal(length(out), 2)

out <- fmRsq(fit, rsq = F, rsqAdj = T)
expect_equal(length(out), 2)

out <- fmRsq(fit, rsq = T, rsqAdj = T)
expect_equal(length(out), 4)

fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
               date.var="DATE", exposure.vars=c("SECTOR", "P2B", "EV2S", "MKTCAP"))

out <- fmRsq(fit1, rsq = T, rsqAdj = T)
expect_equal(length(out), 4)


out<- fmRsq(fit1, rsq = T,rsqAdj = T ,isPrint = T)
expect_equal(length(out), 4)

#tests for error msgs
expect_error(fmRsq(fit, rsq = F, rsqAdj = F), 
             "Invalid arguments: Inputs rsq and rsqAdj cannot be False") 



