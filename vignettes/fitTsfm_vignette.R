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
mkt.name <- "SP500.TR"
rf.name <- "US.3m.TR"

## ------------------------------------------------------------------------
data(CommonFactors)
names(factors.Q)
range(index(factors.Q))

## ----tidy=TRUE-----------------------------------------------------------
args(fitTsfm)

## ------------------------------------------------------------------------
# Single Index Model using SP500
fit.singleIndex <- fitTsfm(asset.names=asset.names, factor.names="SP500.TR", 
                           rf.name="US.3m.TR", data=managers)

## ------------------------------------------------------------------------
class(fit.singleIndex)
names(fit.singleIndex)

## ------------------------------------------------------------------------
fit.singleIndex # print the fitted "tsfm" object

## ----fig.cap="Single Index model: Asset returns vs Factor Returns"-------
# plot asset returns vs factor returns for the single factor models
plot(fit.singleIndex, which=12)

## ------------------------------------------------------------------------
# Henriksson-Merton's market timing model
fit.mktTiming <-  fitTsfmMT(asset.names=asset.names, mkt.name="SP500.TR", 
                            rf.name="US.3m.TR", data=managers)
t(fit.mktTiming$beta)
fit.mktTiming$r2
fit.mktTiming$resid.sd

## ------------------------------------------------------------------------
fit.ols <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                   rf.name="US.3m.TR", data=managers)
fit.ols$beta
fit.ols$r2
fit.ols$resid.sd

## ------------------------------------------------------------------------
fit.robust <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                      rf.name="US.3m.TR", data=managers, fit.method="Robust")
fit.robust$beta
fit.robust$r2
fit.robust$resid.sd

## ----fig.cap="HAM3 Returns: LS (top) vs Robust (bottom)", fig.show='hold'----
par(mfrow=c(2,1))
plot(fit.ols, plot.single=TRUE, which=1, asset.name="HAM3")
mtext("LS", side=3)
plot(fit.robust, plot.single=TRUE, which=1, asset.name="HAM3")
mtext("Robust", side=3)

## ----fig.cap="Residual Vol: LS (left) vs Robust (right)", fig.width=3, fig.height=2.5, out.width='.49\\linewidth', fig.show='hold'----
par(mfrow=c(1,2))
plot(fit.ols, which=5, xlim=c(0,0.045), sub="LS")
plot(fit.robust, which=5, xlim=c(0,0.045), sub="Robust")

## ----fig.show='hide'-----------------------------------------------------
fit.lars <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                    data=managers, rf.name="US.3m.TR", 
                    variable.selection="lars")
fit.lars

## ------------------------------------------------------------------------
(fit.sub <- fitTsfm(asset.names=asset.names, factor.names=factor.names, 
                    data=managers, rf.name="US.3m.TR", 
                    variable.selection="subsets", nvmin=2, nvmax=2))

## ----fig.cap="Factor betas: fit.sub", fig.show='hold'--------------------
plot(fit.sub, which=2)

## ----fig.cap="Factor betas: fit.lars", fig.show='hold'-------------------
plot(fit.lars, which=2)

## ------------------------------------------------------------------------
methods(class="tsfm")

## ------------------------------------------------------------------------
# all estimated coefficients from the LS fit using all 3 factors
coef(fit.ols)

# compare returns data with fitted and residual values for HAM1 from fit.lars
HAM1.ts <- merge(fit.lars$data[,1], fitted(fit.lars)[,1], residuals(fit.lars)[,1])
colnames(HAM1.ts) <- c("HAM1.return","HAM1.fitted","HAM1.residual")
tail(HAM1.ts)

# summary for fit.sub computing HAC standard erros
summary(fit.sub, se.type="HAC")

## ----fig.cap="Factor model return correlation (pairwise complete obs)"----
fmCov(fit.sub)
# factor model return correlation plot
plot(fit.sub, which=8)

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
plot(fit.sub, which=9)

## ----fig.cap="Percentage factor contribution to VaR"---------------------
decomp1 <- fmVaRDecomp(fit.sub)
names(decomp1)
# get the factor model value-at-risk for all assets
decomp1$VaR.fm
# get the percentage component contributions to VaR
decomp1$pcVaR
# plot the percentage component contributions to VaR
plot(fit.sub, which=11)

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
plot(fit.sub, which=10)

## ----eval=FALSE----------------------------------------------------------
## ## S3 method for class "tsfm"
## plot (x, which=NULL, max.show=6, plot.single=FALSE, asset.name,
##       legend.loc="topleft", las=1, lwd=2,
##       colorset=c("royalblue","dimgray","olivedrab","firebrick","goldenrod",
##                  "mediumorchid","deepskyblue","chocolate","darkslategray"),
##       maxlag=15, VaR.method="historical", ...)

## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.sub)
## 
## # Make a plot selection (or 0 to exit):
## #
## #  1: Factor model coefficients: Alpha
## #  2: Factor model coefficients: Betas
## #  3: Actual and Fitted asset returns
## #  4: R-squared
## #  5: Residual Volatility
## #  6: Residuals scatterplot matrix, with histograms, density overlays,
## #     correlations and significance stars
## #  7: Factor Model Residual Correlation
## #  8: Factor Model Return Correlation
## #  9: Factor Contribution to SD
## # 10: Factor Contribution to ES
## # 11: Factor Contribution to VaR
## # 12: Asset returns vs factor returns (single factor model)
## #
## # Selection:

## ----fig.cap="Actual and fitted factor model returns for the 1st 4 assets", fig.show='asis', fig.width=7, fig.height=6----
# Examples of a group plot: looping disabled & no. of assets displayed = 4.
plot(fit.sub, which=3, max.show=4, legend.loc=NULL, lwd=1)

## ----fig.cap="Residual scatterplot matrix with histograms, density overlays, correlations and significance stars", warning=FALSE----
plot(fit.sub, which=6)

## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.sub, plot.single=TRUE, asset.name="HAM1")
## 
## # Make a plot selection (or 0 to exit):
## #  1: Actual and fitted asset returns
## #  2: Actual vs fitted asset returns
## #  3: Residuals vs fitted asset returns
## #  4: Sqrt. of modified residuals vs fitted
## #  5: Residuals with standard error bands
## #  6: Time series of squared residuals
## #  7: Time series of absolute residuals
## #  8: SACF and PACF of residuals
## #  9: SACF and PACF of squared residuals
## # 10: SACF and PACF of absolute residuals
## # 11: Non-parametric density of residuals with normal overlaid
## # 12: Non-parametric density of residuals with skew-t overlaid
## # 13: Histogram of residuals with non-parametric density and normal overlaid
## # 14: QQ-plot of residuals
## # 15: CUSUM test-Recursive residuals
## # 16: CUSUM test-LS residuals
## # 17: Recursive estimates (RE) test of LS regression coefficients
## # 18: Rolling regression over a 24-period observation window
## # 19: Asset returns vs factor returns (single factor model)
## #
## # Selection:

## ----fig.cap="Time series plot of residuals with standard error bands: HAM1", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which=5, ylim=c(-0.06,0.06))

## ----fig.cap="SACF and PACF of absolute residuals: HAM1", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which=10)

## ----fig.cap="QQ-plot of residuals", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which=14)
grid()

## ----fig.cap="Non-parametric density of residuals with normal overlaid for HAM1", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which=11)

## ----fig.cap="Non-parametric density of residuals with skew-t overlaid for HAM1", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.sub, plot.single=TRUE, asset.name="HAM1", which=12)

