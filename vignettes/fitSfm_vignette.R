
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


## ----fig.cap="Screeplot of eigenvalues from PCA fit", fig.width=6, fig.height=4----
plot(fit.pca, which.plot.group=1, factor.subset=1:2, loop=FALSE, eig.max=0.9,
     cex.names=0.9, cex.axis=0.9, cex.main=0.8)


## ----fig.cap="Estimated factors from PCA fit", fig.width=7, fig.height=4----
plot(fit.pca, which.plot.group=2, factor.subset=1:2, loop=FALSE)


## ----fig.cap="Estimated loadings on PCA factors", fig.width=7, fig.height=5----
plot(fit.pca, which.plot.group=3, asset.subset=1:15, factor.subset=1:2, 
     loop=FALSE, cex.names=0.8, cex.axis=0.8, cex.main=0.8)


## ----fig.cap="Largest and smallest weights in factor mimicking portfolios from PCA fit", fig.width=7, fig.height=4.5, fig.show='asis'----
# Factor mimicking portfolio weights from PCA fit
t(fit.pca$mimic)
plot(fit.pca, which.plot.group=11, factor.subset=1:2, n.top=3, loop=FALSE, 
     cex.names=0.8, cex.axis=0.8, cex.main=0.8)


## ------------------------------------------------------------------------
fit.apca <- fitSfm(r.W, k=15)


## ----fig.cap="Screeplot of eigenvalues from APCA fit",fig.width=7,fig.height=4.5----
plot(fit.apca, which.plot.group=1, loop=FALSE, eig.max=0.4, cex.names=0.75, 
     cex.axis=0.8, cex.main=0.8)


## ----fig.cap="First four factor returns from APCA fit", fig.width=7,fig.height=4.5----
plot(fit.apca, which.plot.group=2, loop=FALSE)


## ----fig.cap="Histogram of R-squared values for APCA fit", fig.width=6, fig.height=4----
plot(fit.apca, which.plot.group=4, loop=FALSE)


## ----fig.cap="Histogram of Residual volatilities for APCA fit", fig.width=6, fig.height=4----
plot(fit.apca, which.plot.group=5, loop=FALSE)


## ----fig.cap="Asset correlations: largest and smallest positions in the first factor portfolio of the APCA fit", fig.width=6, fig.height=6----
plot(fit.apca, which.plot.group=12, factor.subset=1, loop=FALSE, n.top=5, 
     method="ellipse")


## ------------------------------------------------------------------------
# APCA with the Bai & Ng method
fit.apca.bn <- fitSfm(r.W, k="bn")
summary(fit.apca.bn$loadings)

# APCA with the Connor-Korajczyk method
fit.apca.ck <- fitSfm(r.W, k="ck", sig=0.05)
fit.apca.ck$k


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

# summary for fit.pca computing HAC standard erros
sum.pca <- summary(fit.pca, se.type="HAC", n.top=3)
names(sum.pca)

# print the summary for the 1st asset
sum.pca$sum.list[[1]]

# print the summary for the factor mimicking portfolio weights
sum.pca$mimic.sum


## ----fig.cap="Treasury yields data for 11 different maturities", fig.width=7, fig.height=6----
plot.zoo(tr.yields, main="Treasury yields", col=4)


## ----fig.cap="Treasury yield curve at 3 different dates", fig.width=7, fig.height=4----
# remove NAs
dat <- na.omit(tr.yields)
time = c(1/12,.25,.5,1, 2, 3, 5, 7, 10, 20, 30)
plot(time, as.vector(dat[1,]), ylim=c(0,6), type="b", col=1, lwd=2, pch=19, 
     ylab="Yield", xlab="T")
lines(time, as.vector(dat[486,]), type="b", lwd=2, col=2, pch=19)
lines(time, as.vector(dat[821,]), type="b", lwd=2, col=4, pch=19)
legend("bottomright", c("07/31/01","07/02/07","10/31/08"), col=c(1,2,4), lwd=2)


## ------------------------------------------------------------------------
diff.yield <- na.omit(diff(tr.yields))
dim(diff.yield)
yield.pca <- fitSfm(diff.yield, k=3, corr=TRUE)


## ----fig.cap="Screeplot of eigenvalues from APCA fit",fig.width=7,fig.height=4.5----
plot(yield.pca, which.plot.group=1, factor.subset=1:3, eig.max=1, loop=FALSE, 
     cex.names=0.9, cex.axis=0.9, cex.main=0.9)


## ------------------------------------------------------------------------
beta <- yield.pca$loadings
summary(beta)
summary(yield.pca)$mimic.sum


## ----fig.cap="Factor loadings on the 1st three Principal components"-----
plot(yield.pca, which.plot.group=3, factor.subset=1:3, asset.subset=1:11, 
     loop=FALSE, cex.names=0.9, cex.axis=0.9, cex.main=0.9)


## ----fig.cap="The loadings on the 1st three factors across maturities", fig.width=7, fig.height=4----
plot(time, beta[,1], ylim=c(-.8,.8), type="b", col=1, lwd=2, pch=19, 
     ylab="Factor loading", xlab="T")
lines(time, beta[,2], type="b", lwd=2, col=2, pch=19)
lines(time, beta[,3], type="b", lwd=2, col=4, pch=19)
legend("bottomright", c("F.1","F.2","F.3"), col=c(1,2,4), lwd=2)


## ----fig.cap="Effect of a unit change in the first 3 factors on the yield curve", fig.width=7, fig.height=10----
mu <- colMeans(dat)
par(mfrow=c(3,1))
for (i in 1:3) {
  plot(time, mu, ylim=c(2,5.3), type="b", col=1, lwd=4, pch=19, ylab="Yield", 
     xlab="T")
  lines(time, mu+beta[,i], type="b", lwd=2, col=3, pch=19)
  lines(time, mu-beta[,i], type="b", lwd=2, col=2, pch=19)
  legend("bottomright", 
         c("mean", paste("mean+F.",i,sep=""), paste("mean-F.",i,sep="")), 
         col=c(1,3,2), lwd=c(4,2,2))
}


## ----fig.cap="Factor model return correlation (pairwise complete obs)", warning=FALSE, fig.width=7, fig.height=7----
Omega <- fmCov(fit.pca)
# return correlation plot for all 15 assets; Angular Order of Eigenvectors
plot(fit.pca, which.plot.group=7, factor.subset=1:2, asset.subset=1:15, 
     loop=FALSE, order="AOE", method="ellipse")


## ----fig.cap="Percentage factor contribution to SD", fig.width=7, fig.height=6, warning=FALSE----
decomp <- fmSdDecomp(fit.pca)
names(decomp)
# get the factor model standard deviation for all assets
decomp$Sd.fm
# get the component contributions to Sd; print first 6 assets
head(decomp$cSd)
# plot the percentage component contributions to Sd for all 15 assets
plot(fit.pca, which.plot.group=8, factor.subset=1:2, asset.subset=1:15, 
     loop=FALSE)


## ----fig.cap="Percentage factor contribution to VaR", fig.width=7, fig.height=5----
decomp1 <- fmVaRDecomp(fit.apca, method="historical")
names(decomp1)
# factor model Value-at-Risk; print first 6 assets
head(decomp1$VaR.fm)
# marginal factor contributions to VaR from 1st 4 factors; print first 6 assets
head(decomp1$mVaR[,1:4])
# plot the 1st 4 factors % component contributions to VaR for the 1st 6 assets
plot(fit.apca, which.plot.group=10, loop=FALSE, asset.subset=1:6)


## ----fig.cap="Percentage factor contribution to ES", fig.width=7, fig.height=5----
decomp2 <- fmEsDecomp(fit.apca, method="historical")
names(decomp2)
# factor model Expected Shortfall; print first 6 assets
head(decomp2$ES.fm)
# percentage component contributions to ES from 1st 4 factors; show 1st 6 assets
head(decomp2$pcES[,1:4])
# plot the 1st 4 factors % component contributions to ES for the 1st 6 assets
plot(fit.apca, which.plot.group=9, loop=FALSE, asset.subset=1:6)


## ----eval=FALSE----------------------------------------------------------
## ## S3 method for class "sfm"
## plot(x, which.plot.group=NULL, factor.subset=1:4, asset.subset=1:5, n.top=3,
##      plot.single=FALSE, asset.name, which.plot.single=NULL, colorset=(1:12),
##      legend.loc="topleft", las=1, VaR.method="historical", cum.var=TRUE,
##      eig.max=0.9, loop=TRUE, ...)


## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.pca)
## 
## ## Make a plot selection (or 0 to exit):
## ##
## ##  1: Screeplot of eigenvalues
## ##  2: Time series plot of estimated factors
## ##  3: Estimated factor loadings
## ##  4: Histogram of R-squared
## ##  5: Histogram of residual volatility
## ##  6: Factor model residual correlation
## ##  7: Factor model return correlation
## ##  8: Factor contribution to SD
## ##  9: Factor contribution to ES
## ## 10: Factor contribution to VaR
## ## 11: Factor mimicking portfolio weights - top long and short positions in each factor
## ## 12: Asset correlations - top long and short positions in each factor
## ##
## ## Selection:


## ----eval=FALSE, results='hide'------------------------------------------
## plot(fit.pca, plot.single=TRUE, asset.name="CITCRP")
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


## ----fig.cap="Time series plot of residuals with standard error bands: CITCRP", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="CITCRP", which.plot.single=2, 
     loop=FALSE)


## ----fig.cap="SACF and PACF of absolute residuals: CITCRP", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="CITCRP", which.plot.single=7, 
     loop=FALSE)


## ----fig.cap="Histogram of residuals with normal curve overlayed for CITCRP", fig.show='asis', fig.width=7, fig.height=4.5----
plot(fit.pca, plot.single=TRUE, asset.name="CITCRP", which.plot.single=8, 
     loop=FALSE)


