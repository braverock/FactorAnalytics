## ----message=FALSE-------------------------------------------------------
library(factorAnalytics)
options(digits=3)

## ------------------------------------------------------------------------
# load the Rdata object
data(StockReturns)
# view class and dimensions
class(r.M)
dim(r.M)
# variable names
colnames(r.M)
# range of observations
range(rownames(r.M))

## ------------------------------------------------------------------------
class(r.W)
dim(r.W)
range(rownames(r.W))

## ------------------------------------------------------------------------
data(TreasuryYields)
head(tr.yields)
range(index(tr.yields))

## ----tidy=TRUE-----------------------------------------------------------
args(fitSfm)

## ------------------------------------------------------------------------
fit.pca <- fitSfm(r.M, k=2)

## ------------------------------------------------------------------------
class(fit.pca)
names(fit.pca)

## ------------------------------------------------------------------------
fit.pca # print the fitted "sfm" object

## ----fig.cap="Screeplot of eigenvalues: fit.pca", fig.width=6, fig.height=4----
plot(fit.pca, which=1, eig.max=0.9)

## ----fig.cap="Time series of estimated factors: fit.pca", fig.width=7, fig.height=4----
plot(fit.pca, which=2)

## ----fig.cap="Estimated factor loadings: fit.pca", fig.width=7, fig.height=5----
plot(fit.pca, which=3, a.sub=1:15)

## ----fig.cap="Top 3 largest and smallest weights in the factor mimicking portfolios", fig.width=7, fig.height=4.5, fig.show='asis'----
# Factor mimicking portfolio weights from PCA fit
t(fit.pca$mimic)
plot(fit.pca, which=12, n.top=3, cex.main=0.9)

## ----fig.cap="Correlations between assets with the top 3 largest and smallest positions in the F.1's factor mimicking portfolio", fig.width=5, fig.height=5, fig.show='asis'----
plot(fit.pca, which=13, f.sub=1, n.top=3)

## ------------------------------------------------------------------------
fit.apca <- fitSfm(r.W, k=15)

## ----fig.cap="Screeplot of eigenvalues: fit.apca",fig.width=7,fig.height=4.5----
plot(fit.apca, which=1, eig.max=0.4, las=2)

## ----fig.cap="First four factor returns: fit.apca", fig.width=7,fig.height=6----
plot(fit.apca, f.sub=1:4, which=2)

## ------------------------------------------------------------------------
# APCA with the Bai & Ng method
fit.apca.bn <- fitSfm(r.W, k="bn")
summary(fit.apca.bn$loadings)

# APCA with the Connor-Korajczyk method
fit.apca.ck <- fitSfm(r.W, k="ck", sig=0.05)
fit.apca.ck$k

## ----fig.cap="Histogram of R-squared values: fit.apca", fig.width=5.5, fig.height=3.5----
plot(fit.apca, which=4, legend.loc="topright")

## ----fig.cap="Histogram of Residual volatilities: fit.apca", fig.width=5.5, fig.height=3.5----
plot(fit.apca, which=5, legend.loc="topright")

## ------------------------------------------------------------------------
methods(class="sfm")

## ----eval=FALSE----------------------------------------------------------
## ## S3 method for class "sfm"
## summary.sfm (object, se.type="Default", n.top=3, ...)

## ------------------------------------------------------------------------
# all estimated coefficients from PCA example
coef(fit.pca)

# compare returns data with fitted and residual values for CITCRP: fit.pca
CITCRP.ts <- merge(fit.pca$data[,1], fitted(fit.pca)[,1], 
                   residuals(fit.pca)[,1])
colnames(CITCRP.ts) <- c("CITCRP.return","CITCRP.fitted","CITCRP.residual")
tail(CITCRP.ts)

# summary for fit.pca with HAC standard erros
sum.pca <- summary(fit.pca, se.type="HAC", n.top=3)
names(sum.pca)

# print the summary for the 1st asset
sum.pca$sum.list[[1]]

# print the summary for the factor mimicking portfolio weights
sum.pca$mimic.sum

## ----fig.cap="Treasury yields data for 11 different maturities", fig.width=7, fig.height=6----
plot.zoo(tr.yields, main="Treasury yields", col="royalblue")

## ----fig.cap="Treasury yield curve at 3 different dates", fig.width=7, fig.height=4----
dat <- na.omit(tr.yields) # remove NAs
time = c(1/12,.25,.5,1, 2, 3, 5, 7, 10, 20, 30)
plot(time, as.vector(dat[1,]), ylim=c(0,6), type="b", col="royalblue", lwd=2, 
     pch=19, ylab="Yield", xlab="T")
lines(time, as.vector(dat[486,]), type="b", lwd=2, col="olivedrab", pch=19)
lines(time, as.vector(dat[821,]), type="b", lwd=2, col="firebrick", pch=19)
legend("bottomright", c("07/31/01","07/02/07","10/31/08"), 
       col=c("royalblue","olivedrab","firebrick"), lwd=2, bty="n")

## ------------------------------------------------------------------------
diff.yield <- na.omit(diff(tr.yields))
dim(diff.yield)
yield.pca <- fitSfm(diff.yield, k=3, corr=TRUE)

## ----fig.cap="Screeplot of eigenvalues for the changes in Treasury yields",fig.width=7,fig.height=5----
plot(yield.pca, which=1, f.sub=1:3, eig.max=1)

## ------------------------------------------------------------------------
beta <- yield.pca$loadings
summary(beta)
summary(yield.pca)$mimic.sum

## ----fig.cap="Factor loadings on the 1st three Principal components"-----
plot(yield.pca, which=3, f.sub=1:3, a.sub=1:11)

## ----fig.cap="The loadings on the 1st three factors across maturities", fig.width=7, fig.height=4----
plot(time, beta[,1], ylim=c(-.8,.8), type="b", col="royalblue", lwd=2, pch=19, 
     ylab="Factor loading", xlab="T")
lines(time, beta[,2], type="b", lwd=2, col="olivedrab", pch=19)
lines(time, beta[,3], type="b", lwd=2, col="firebrick", pch=19)
legend("bottomright", c("F.1","F.2","F.3"), 
       col=c("royalblue","olivedrab","firebrick"), lwd=2, bty="n")

## ----fig.cap="Effect of a unit change in the first 3 factors on the yield curve: level (shift), slope (tilt) and curvature (bend)", fig.width=7, fig.height=10----
mu <- colMeans(dat)
par(mfrow=c(3,1))
for (i in 1:3) {
  plot(time, mu, ylim=c(2,5.3), type="b", col="royalblue", lwd=2, pch=19, 
       ylab="Yield", xlab="T")
  lines(time, mu+beta[,i], type="b", lwd=2, lty=2, col="olivedrab", pch=19)
  lines(time, mu-beta[,i], type="b", lwd=2, lty=2, col="firebrick", pch=19)
  legend("bottomright", bty="n",
         c("mean", paste("mean+F.",i,sep=""), paste("mean-F.",i,sep="")), 
         col=c("royalblue","olivedrab","firebrick"), lwd=2, lty=c(1,2,2))
}

## ----fig.cap="Factor model return correlation", warning=FALSE, fig.width=7, fig.height=7----
Omega <- fmCov(fit.pca)
# return correlation plot for all 15 assets
plot(fit.pca, which=8, a.sub=1:15, tl.cex=0.7)

## ----fig.cap="Percentage factor contribution to SD", fig.width=7, fig.height=6, warning=FALSE----
decomp <- fmSdDecomp(fit.pca)
names(decomp)
# get the factor model standard deviation for all assets
decomp$Sd.fm
# get the component contributions to Sd; print first 6 assets
head(decomp$cSd)
# plot the percentage component contributions to Sd for all 15 assets
plot(fit.pca, which=9, f.sub=1:2, a.sub=1:15)

## ----fig.cap="Percentage factor contribution to VaR", fig.width=7, fig.height=5----
decomp1 <- fmVaRDecomp(fit.apca, method="historical")
names(decomp1)
# factor model Value-at-Risk; print first 6 assets
head(decomp1$VaR.fm)
# marginal factor contributions to VaR from 1st 4 factors; print first 6 assets
head(decomp1$mVaR[,1:4])
# plot the 1st 4 factors % component contributions to VaR for the 1st 6 assets
plot(fit.apca, which=11, f.sub=1:4, a.sub=1:6)

## ----fig.cap="Percentage factor contribution to ES", fig.width=7, fig.height=5----
decomp2 <- fmEsDecomp(fit.apca, method="historical")
names(decomp2)
# factor model Expected Shortfall; print first 6 assets
head(decomp2$ES.fm)
# percentage component contributions to ES from 1st 4 factors; show 1st 6 assets
head(decomp2$pcES[,1:4])
# plot the 1st 4 factors % component contributions to ES for the 1st 6 assets
plot(fit.apca, which=10, f.sub=1:4, a.sub=1:6)

## ----eval=FALSE----------------------------------------------------------
## ## S3 method for class "sfm"
## plot (x, which=NULL, f.sub=1:2, a.sub=1:6, n.top=3,
##       plot.single=FALSE, asset.name,
##       colorset=c("royalblue","firebrick","olivedrab","firebrick","goldenrod",
##                  "mediumorchid","deepskyblue","chocolate","darkslategray"),
##       legend.loc="topleft", las=1, lwd=2, maxlag=15,
##       VaR.method="historical", eig.max=0.9, cum.var=TRUE, ...)

## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.pca)
## 
## # Make a plot selection (or 0 to exit):
## #
## #  1: Screeplot of eigenvalues
## #  2: Time series plot of estimated factors
## #  3: Estimated factor loadings
## #  4: Histogram of R-squared
## #  5: Histogram of residual volatility
## #  6: Scatterplot matrix of residuals, with histograms, density overlays,
## #     correlations and significance stars
## #  7: Factor model residual correlation
## #  8: Factor model return correlation
## #  9: Factor contribution to SD
## # 10: Factor contribution to ES
## # 11: Factor contribution to VaR
## # 12: Factor mimicking portfolio weights - top long and short positions in each
## #     factor
## # 13: Asset correlations - top long and short positions in each factor
## #
## # Selection:

## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.pca, plot.single=TRUE, asset.name="DATGEN")
## 
## # Make a plot selection (or 0 to exit):
## #
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
## # 18: Rolling estimates over a 24-period observation window
## #
## # Selection:

## ----fig.cap="Time series plot of residuals with standard error bands: DATGEN", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="DATGEN", which=5)

## ----fig.cap="SACF and PACF of absolute residuals: DATGEN", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="DATGEN", which=10)

## ----fig.cap="QQ-plot of residuals: DATGEN", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="DATGEN", which=14)
grid()

## ----fig.cap="Non-parametric density of residuals with normal overlaid for DATGEN", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="DATGEN", which=11)

## ----fig.cap="Non-parametric density of residuals with skew-t overlaid for DATGEN", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="DATGEN", which=12)

