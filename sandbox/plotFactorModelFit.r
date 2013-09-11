## plotFactorModelFit.r
##
## purpose: create various plots to summarize factor model fit
##
## factor model has the form
##        Ri(t) = bi'F(t) + ei(t)
##        Ri(t) = performance of fund i in month t
##        F(t) = k x 1 vector of factor returns in month t
##        bi = k x 1 vector of factor loadings for fund i
##
## authors: Eric Zivot
## created: Feb 18, 2009
## updated: Feb 18, 2009
##
## comments:
## requires the following libraries and functions
##    zoo
##    performanceAnalytics
##    getFundInfo()


plotFactorModelFit <- function(fit.lm,fundName="example" ,which.plot = 1) {
## inputs:
## fit.lm           lm object summarizing factor model fit. It is assumed that
##                  time series date information is included in the names component
##                  of the residuals, fitted and model components of the object.
## fundInfo         dataframe with fund information. Created with a call to the function
##                  getFundInfo2()
## which.plot       integer indicating which plot to create:
##                  1     time series plot of actual and fitted values
##                  2     time series plot of residuals with standard error bands
##                  3     time series plot of squared residuals
##                  4     time series plot of absolute residuals
##                  5     SACF and PACF of residuals
##                  6     SACF and PACF of squared residuals
##                  7     SACF and PACF of absolute residuals
##                  8     histogram of residuals with normal curve overlayed
##                  9     normal qq-plot of residuals
##                  10    CUSUM plot of recursive residuals
##                  11    CUSUM plot of OLS residuals
##                  12    CUSUM plot of recursive estimates relative to full sample estimates
##                  13    rolling estimates over 24 month window
require(zoo)
require(PerformanceAnalytics)
require(strucchange)

if (!(class(fit.lm) == "lm"))
  stop("Must pass a valid lm object")
if (!any(which.plot == 1:13))
  stop("Invalid plot choice")
  
## extract information from lm object
fundId = colnames(fit.lm$model)[1]
fundName = fundName
factorNames = colnames(fit.lm$model)[-1]
fit.formula = as.formula(paste(fundId,"~", paste(factorNames, collapse="+"), sep=" "))
residuals.z = zoo(residuals(fit.lm), as.Date(names(residuals(fit.lm))))
fitted.z = zoo(fitted(fit.lm), as.Date(names(fitted(fit.lm))))
actual.z = zoo(fit.lm$model[,1], as.Date(rownames(fit.lm$model)))
tmp.summary = summary(fit.lm)

if (which.plot == 1) {
##  time series plot of actual and fitted values
  plot(actual.z, main=fundName, ylab="Monthly performance", lwd=2, col="black")
  lines(fitted.z, lwd=2, col="blue")
  abline(h=0)
  legend(x="bottomleft", legend=c("Actual", "Fitted"), lwd=2, col=c("black","blue"))
}
if (which.plot == 2) {
## time series plot of residuals with standard error bands
  plot(residuals.z, main=fundName, ylab="Monthly performance", lwd=2, col="black")
  abline(h=0)
  abline(h=2*tmp.summary$sigma, lwd=2, lty="dotted", col="red")
  abline(h=-2*tmp.summary$sigma, lwd=2, lty="dotted", col="red")
  legend(x="bottomleft", legend=c("Residual", "+/ 2*SE"), lwd=2,
         lty=c("solid","dotted"), col=c("black","red"))
}
if (which.plot == 3) {
## time series plot of squared residuals
  plot(residuals.z^2, main=fundName, ylab="Squared residual", lwd=2, col="black")
  abline(h=0)
  legend(x="topleft", legend="Squared Residuals", lwd=2, col="black")
}
if (which.plot == 4) {
## time series plot of absolute residuals
  plot(abs(residuals.z), main=fundName, ylab="Absolute residual", lwd=2, col="black")
  abline(h=0)
  legend(x="topleft", legend="Absolute Residuals", lwd=2, col="black")
}
if (which.plot == 5) {
## SACF and PACF of residuals
  chart.ACFplus(residuals.z, main=paste("Residuals: ", fundName, sep=""))
}
if (which.plot == 6) {
## SACF and PACF of squared residuals
  chart.ACFplus(residuals.z^2, main=paste("Residuals^2: ", fundName, sep=""))
}
if (which.plot == 7) {
## SACF and PACF of absolute residuals
  chart.ACFplus(abs(residuals.z), main=paste("|Residuals|: ", fundName, sep=""))
}
if (which.plot == 8) {
## histogram of residuals with normal curve overlayed
  chart.Histogram(residuals.z, methods="add.normal", main=paste("Residuals: ", fundName, sep=""))
}
if (which.plot == 9) {
##  normal qq-plot of residuals
  chart.QQPlot(residuals.z, envelope=0.95, main=paste("Residuals: ", fundName, sep=""))
}
if (which.plot == 10) {
##  CUSUM plot of recursive residuals
  cusum.rec = efp(fit.formula, type="Rec-CUSUM", data=fit.lm$model)
  plot(cusum.rec, sub=fundName)
}
if (which.plot == 11) {
##  CUSUM plot of OLS residuals
  cusum.ols = efp(fit.formula, type="OLS-CUSUM", data=fit.lm$model)
  plot(cusum.ols, sub=fundName)
}
if (which.plot == 12) {
##  CUSUM plot of recursive estimates relative to full sample estimates
  cusum.est = efp(fit.formula, type="fluctuation", data=fit.lm$model)
  plot(cusum.est, functional=NULL, sub=fundName)
}
if (which.plot == 13) {
##  rolling regression over 24 month window
rollReg <- function(data.z, formula) {
  coef(lm(formula, data = as.data.frame(data.z)))  
}
reg.z = zoo(fit.lm$model, as.Date(rownames(fit.lm$model)))
rollReg.z = rollapply(reg.z, FUN=rollReg, fit.formula, width=24, by.column = FALSE, 
                      align="right")
plot(rollReg.z, main=paste("24-month rolling regression estimates:", fundName, sep=" "))
}

}