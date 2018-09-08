#Load the data 
data("factorDataSetDjia5Yrs")

#Fit a Ffm
fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars="SECTOR")


#Test for errors when  less than 2 exploratory variables are used to fit model. 
expect_error(vif(fit),"At least 2 continous variables required to find VIF")

#Fit a Ffm
fit.1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
              date.var="DATE", exposure.vars=c("SECTOR", "P2B", "SIZE", "MKTCAP", "ENTVAL"))


#test for output lengths
out <- vif(fit.1)
expect_equal(length(out), 2)