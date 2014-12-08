
## ----message=FALSE-------------------------------------------------------
library(factorAnalytics)
options(digits=3)


## ------------------------------------------------------------------------
data(managers)
colnames(managers)
range(index(managers))


## ------------------------------------------------------------------------
asset.names <- colnames(managers[,1:6]) 
factor.names <- colnames(managers[,7:9])
mkt.name <- "SP500 TR"
rf.name <- "US 3m TR"


## ------------------------------------------------------------------------
data(CommonFactors)
names(factors.Q)
range(index(factors.Q))


## ----tidy=TRUE-----------------------------------------------------------
args(fitTsfm)


## ------------------------------------------------------------------------
# Single Index Model using SP500
fit.singleIndex <- fitTsfm(asset.names=asset.names, factor.names="SP500 TR", 
                           rf.name="US 3m TR", data=managers)


## ------------------------------------------------------------------------
class(fit.singleIndex)
names(fit.singleIndex)


## ------------------------------------------------------------------------
fit.singleIndex # print the fitted "tsfm" object


## ------------------------------------------------------------------------
# Henriksson-Merton's market timing model
fit.mktTiming <- fitTsfm(asset.names=asset.names, rf.name="US 3m TR", 
                         mkt.name="SP500 TR", mkt.timing="HM", data=managers)
fit.mktTiming


## ------------------------------------------------------------------------
fit.ols <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                    rf.name="US 3m TR", data=managers)
fit.ols$beta
fit.ols$r2
fit.ols$resid.sd


## ------------------------------------------------------------------------
fit.robust <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                       rf.name="US 3m TR", data=managers, fit.method="Robust")
fit.robust$beta
fit.robust$r2
fit.robust$resid.sd


## ----fig.cap="HAM3 Returns: OLS (top) vs Robust (bottom)", fig.show='hold'----
par(mfrow=c(2,1))
plot(fit.ols, plot.single=TRUE, which.plot.single=1, asset.name="HAM3", loop=FALSE)
mtext("OLS", side=3)
plot(fit.robust, plot.single=TRUE, which.plot.single=1, asset.name="HAM3", loop=FALSE)
mtext("Robust", side=3)


## ----fig.cap="Residual vol: OLS (left) vs Robust (right)", fig.width=3, fig.height=2.5, out.width='.49\\linewidth', fig.show='hold'----
par(mfrow=c(1,2))
plot(fit.ols, which.plot.group=5, loop=FALSE, xlim=c(0,0.043), sub="OLS")
plot(fit.robust, which.plot.group=5, loop=FALSE, xlim=c(0,0.043), sub="Robust")


## ----fig.show='hide'-----------------------------------------------------
fit.lars <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                    data=managers, rf.name="US 3m TR", 
                    variable.selection="lars")
fit.lars


## ------------------------------------------------------------------------
fit.sub <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                   data=managers, rf.name="US 3m TR", 
                   variable.selection="subsets", nvmin=2, nvmax=2)
fit.sub


## ----fig.cap="Factor betas: fit.sub", fig.show='hold'--------------------
plot(fit.sub, which.plot.group=2, loop=FALSE)


## ----fig.cap="Factor betas: fit.lars", fig.show='hold'-------------------
plot(fit.lars, which.plot.group=2, loop=FALSE)


## ------------------------------------------------------------------------
methods(class="tsfm")


## ------------------------------------------------------------------------
# all estimated coefficients from the OLS fit using all 3 factors
coef(fit.ols)

# compare returns data with fitted and residual values for HAM1 from fit.lars
HAM1.ts <- merge(fit.lars$data[,1], fitted(fit.lars)[,1], 
                      residuals(fit.lars)[,1])
colnames(HAM1.ts) <- c("HAM1.return","HAM1.fitted","HAM1.residual")
tail(HAM1.ts)

# summary for fit.sub computing HAC standard erros
summary(fit.sub, se.type="HAC")


## ----fig.cap="Factor model return correlation (pairwise complete obs)"----
fmCov(fit.sub)
# return correlation plot; Angular Order of the Eigenvectors
plot(fit.sub, which.plot.group=7, loop=FALSE, order="AOE", method="ellipse", 
     tl.pos = "d")


## ----fig.cap="Percentage factor contribution to SD"----------------------
decomp <- fmSdDecomp(fit.sub)
names(decomp)
# get the factor model standard deviation for all assets
decomp$Sd.fm
# get the component contributions to Sd
decomp$cSd
# get the marginal factor contributions to Sd
decomp$mSd
# get the percentage component contributions to Sd
decomp$pcSd
# plot the percentage component contributions to Sd
plot(fit.sub, which.plot.group=8, loop=FALSE)


## ----fig.cap="Percentage factor contribution to VaR"---------------------
decomp1 <- fmVaRDecomp(fit.sub)
names(decomp1)
# get the factor model value-at-risk for all assets
decomp1$VaR.fm
# get the percentage component contributions to VaR
decomp1$pcVaR
# plot the percentage component contributions to VaR
plot(fit.sub, which.plot.group=10, loop=FALSE)


## ----fig.cap="Percentage factor contribution to ES"----------------------
decomp2 <- fmEsDecomp(fit.sub, method="historical")
names(decomp2)
# get the factor model expected shortfall for all assets
decomp2$ES.fm
# get the component contributions to Sd
decomp2$cES
# get the marginal factor contributions to ES
decomp2$mES
# get the percentage component contributions to ES
decomp2$pcES
# plot the percentage component contributions to ES
plot(fit.sub, which.plot.group=9, loop=FALSE)


## ----eval=FALSE----------------------------------------------------------
## ## S3 method for class "tsfm"
## plot (x, which.plot.group=NULL, max.show=6, plot.single=FALSE, asset.name,
##       which.plot.single=NULL, colorset=(1:12), legend.loc="topleft", las=1,
##       VaR.method="historical", loop=TRUE, ...)


## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.sub)
## 
## ## Make a plot selection (or 0 to exit):
## ##
## ##  1: Factor model coefficients: Alpha
## ##  2: Factor model coefficients: Betas
## ##  3: Actual and Fitted asset returns
## ##  4: R-squared
## ##  5: Residual Volatility
## ##  6: Factor Model Residual Correlation
## ##  7: Factor Model Return Correlation
## ##  8: Factor Contribution to SD
## ##  9: Factor Contribution to ES
## ## 10: Factor Contribution to VaR
## ##
## ## Selection:


## ----fig.cap="Actual and fitted factor model returns for the 1st 4 assets"----
plot(fit.sub, which.plot.group=3, max.show=4, legend.loc=NULL, loop=FALSE)


## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.sub, plot.single=TRUE, asset.name="HAM1")
## 
## ## Make a plot selection (or 0 to exit):
## ##
## ##  1: Time series plot of actual and fitted asset returns
## ##  2: Time series plot of residuals with standard error bands
## ##  3: Time series plot of squared residuals
## ##  4: Time series plot of absolute residuals
## ##  5: SACF and PACF of residuals
## ##  6: SACF and PACF of squared residuals
## ##  7: SACF and PACF of absolute residuals
## ##  8: Histogram of residuals with normal curve overlayed
## ##  9: Normal qq-plot of residuals
## ## 10: CUSUM test-Recursive residuals
## ## 11: CUSUM test-OLS residuals
## ## 12: Recursive estimates (RE) test of OLS regression coefficients
## ## 13: Rolling estimates over a 24-period observation window
## ##
## ## Selection:


## ----fig.cap="Time series plot of residuals with standard error bands: HAM1", fig.show='hold'----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which.plot.single=2, 
     loop=FALSE)


## ----fig.cap="SACF and PACF of absolute residuals: HAM1", fig.show='hold'----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which.plot.single=7, 
     loop=FALSE)


## ----fig.cap="Histogram of residuals with normal curve overlayed for HAM1", fig.show='hold'----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which.plot.single=8, 
     loop=FALSE)


