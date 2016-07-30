#Load the data 
data("factorDataSetDjia5Yrs")

#Fit a Ffm
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars="SECTOR")

#test for output lengths
out <- ffmRsq(fit)
expect_equal(length(out), 2)

out <- ffmRsq(fit, rsq = F, rsqAdj = T)
expect_equal(length(out), 2)

out <- ffmRsq(fit, rsq = T, rsqAdj = T)
expect_equal(length(out), 4)

fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
               date.var="DATE", exposure.vars=c("SECTOR", "P2B", "EV2S", "MKTCAP"))

out <- ffmRsq(fit1, rsq = T, rsqAdj = T, VIF = T)
expect_equal(length(out), 6)

out <- ffmRsq(fit1, rsq = F, rsqAdj = T, VIF = T)
expect_equal(length(out), 4)

out<- ffmRsq(fit1, rsq = T,rsqAdj = T, VIF = T, isPrint = T)
expect_equal(length(out), 6)

#tests for error msgs
expect_error(ffmRsq(fit, rsq = F, rsqAdj = F,VIF = F), 
             "Invalid arguments: Inputs rsq, rsqAdj and VIF cannot be False") 

expect_error(ffmRsq(fit, rsq = T, rsqAdj = T,VIF = T),
             "At least 2 continous variables required to find VIF")

