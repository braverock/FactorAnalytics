#' @title Plots from a fitted statistical factor model
#' 
#' @description Generic \code{plot} method for object of class \code{sfm}. 
#' Plots chosen characteristic(s) for one or more assets. 
#' 
#' @details 
#' The function can be used for group plots and individual plots. User can 
#' select the type of plot either from the menu prompt (default) or directly 
#' via argument \code{which}.
#' 
#' In case multiple plots are needed, the menu is repeated after each plot 
#' (enter 0 to exit). User can also input a numeric vector of plot options via 
#' \code{which}.
#' 
#' Group plots are the default. The selected assets in \code{a.sub} and 
#' selected factors in \code{f.sub} are plotted depending on the 
#' characteristic chosen. The default is to show the first 2 factors and first 
#' 6 assets.
#' 
#' Setting \code{plot.single=TRUE} enables individual plots. If there is more 
#' than one asset fit by \code{x}, \code{asset.name} should be specified. 
#' In case the \code{tsfm} object \code{x} contains only a single asset fit, 
#' plot.tsfm can infer \code{asset.name} without user input.  
#' 
#' @param x an object of class \code{sfm} produced by \code{fitSfm}.
#' @param which a number to indicate the type of plot. If a subset of the plots 
#' is required, specify a subset of the numbers 1:13 for group plots and 1:18 
#' for individual plots. If \code{which=NULL} (default), the following menu 
#' appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Screeplot of eigenvalues, \cr
#' 2 = Time series plot of estimated factors, \cr
#' 3 = Estimated factor loadings, \cr
#' 4 = Histogram of R-squared, \cr
#' 5 = Histogram of residual volatility,\cr
#' 6 = Factor model residuals scatterplot matrix, with histograms, density overlays, correlations and significance stars, \cr
#' 7 = Factor model residual correlation \cr
#' 8 = Factor model return correlation,\cr
#' 9 = Factor contribution to SD,\cr
#' 10 = Factor contribution to ES,\cr
#' 11 = Factor contribution to VaR, \cr
#' 12 = Factor mimicking portfolio weights - top long and short positions in each factor, \cr 
#' 13 = Asset correlations - top long and short positions in each factor \cr \cr
#' For individual asset plots:\cr
#' 1 = Actual and fitted,\cr
#' 2 = Actual vs fitted,\cr
#' 3 = Residuals vs fitted, \cr
#' 4 = Sqrt. of modified residuals vs fitted, \cr
#' 5 = Residuals with standard error bands, \cr
#' 6 = Time series of squared residuals, \cr
#' 7 = Time series of absolute residuals,\cr
#' 8 = SACF and PACF of residuals,\cr
#' 9 = SACF and PACF of squared residuals,\cr
#' 10 = SACF and PACF of absolute residuals,\cr
#' 11 = Non-parametric density of residuals with normal overlaid, \cr
#' 12 = Non-parametric density of residuals with skew-t overlaid, \cr 
#' 13 = Histogram of residuals with non-parametric density and normal overlaid,\cr
#' 14 = QQ-plot of residuals,\cr
#' 15 = CUSUM test-Recursive residuals,\cr
#' 16 = CUSUM test-LS residuals,\cr
#' 17 = Recursive estimates (RE) test of LS regression coefficients,\cr
#' 18 = Rolling regression over a 24-period observation window
#' @param f.sub numeric/character vector; subset of indexes/names of factors to 
#' include for group plots. Default is 1:2.
#' @param a.sub numeric/character vector; subset of indexes/names of assets to 
#' include for group plots. At least 2 assets must be selected. Default is 1:6.
#' @param n.top scalar; number of largest and smallest weights to display for 
#' each factor mimicking portfolio. Default is 3.
#' @param plot.single logical; If \code{TRUE} plots the characteristics of an 
#' individual asset's factor model. The type of plot is given by 
#' \code{which}. Default is \code{FALSE}.
#' @param asset.name name of the individual asset to be plotted. Is necessary 
#' if \code{x} contains multiple asset fits and \code{plot.single=TRUE}.
#' @param colorset color palette to use for all the plots. The 1st element will 
#' be used for individual time series plots or the 1st object plotted, the 2nd 
#' element for the 2nd object in the plot and so on.
#' @param legend.loc places a legend into one of nine locations on the chart: 
#' "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", 
#' "right", or "center". Default is "bottomright". Use \code{legend.loc=NULL} 
#' to suppress the legend.
#' @param las one of {0, 1, 2, 3} to set the direction of axis labels, same as 
#' in \code{plot}. Default is 1.
#' @param lwd set the line width, same as in \code{\link{plot}}. Default is 2.
#' @param maxlag optional number of lags to be calculated for ACF. Default is 15.
#' @param VaR.method a method for computing VaR; one of "modified", "gaussian",
#' "historical" or "kernel". VaR is computed using 
#' \code{\link[PerformanceAnalytics]{VaR}}. Default is "historical".
#' @param eig.max scalar in (0,1] for limiting the screeplot to factors that 
#' explain a given percent of the variance. Default is 0.9.
#' @param cum.var logical; If \code{TRUE}, the cumulative fraction of the
#' variance is printed above each bar in the screeplot of eigenvalues. Default
#' is \code{TRUE}.
#' @param ... further arguments to be passed to other plotting functions.
#' 
#' @author Eric Zivot, Sangeetha Srinivasan and Yi-An Chen
#' 
#' @seealso \code{\link{fitSfm}}, \code{\link{residuals.sfm}}, 
#' \code{\link{fitted.sfm}}, \code{\link{fmCov.sfm}} and 
#' \code{\link{summary.sfm}} for statistical factor model fitting and related 
#' S3 methods. Refer to \code{\link{fmSdDecomp}}, \code{\link{fmEsDecomp}}, 
#' \code{\link{fmVaRDecomp}} for factor model risk measures.
#' 
#' Here is a list of plotting functions used. (I=individual, G=Group)
#' I(1,5,6,7) - \code{\link[PerformanceAnalytics]{chart.TimeSeries}}, 
#' I(2,3,4) - \code{\link[graphics]{plot.default}},
#' I(3,4) - \code{\link[graphics]{panel.smooth}},
#' I(8,9,10) - \code{\link[PerformanceAnalytics]{chart.ACFplus}}, 
#' I(11,12) - \code{\link[stats]{plot.density}},
#' I(13), G(4,5) - \code{\link[PerformanceAnalytics]{chart.Histogram}},
#' I(14) - \code{\link[PerformanceAnalytics]{chart.QQPlot}}, 
#' I(15,16,17) - \code{\link[strucchange]{plot.efp}},
#' I(18) - \code{\link[zoo]{plot.zoo}},
#' G(1,12) - \code{\link[graphics]{barplot}}, 
#' G(2) - \code{\link[lattice]{xyplot}},
#' G(3,9,10,11) - \code{\link[lattice]{barchart}},
#' G(6) - \code{\link[PerformanceAnalytics]{chart.Correlation}} and
#' G(7,8,13) - \code{\link[corrplot]{corrplot.mixed}}. 
#' 
#' @examples
#' 
#' # load data from the database
#' data(StockReturns)
#' 
#' # APCA with number of factors, k=15
#' fit.apca <- fitSfm(r.W, k=15, refine=TRUE)
#' 
#' # for group plots (default), user can select plot option from menu prompt
#' # menu is repeated to get multiple types of plots based on the same fit
#' # plot(fit.apca)
#' 
#' # choose specific plot option(s) using which
#' # plot the first 4 factor betas of the first 4 assets fitted above
#' plot(fit.apca, f.sub=1:4, a.sub=1:4, which=3)
#' 
#' # plot factor model residuals scatterplot matrix, with histograms, density 
#' # overlays, correlations and significance stars
#' plot(fit.apca, which=6)
#' 
#' # for individual plots: set plot.single=TRUE and specify asset.name
#' # histogram of residuals from an individual asset's factor model fit 
#' plot(fit.apca, plot.single=TRUE, asset.name="AFL", which=13)
#' 
#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus
#' chart.Histogram chart.QQPlot chart.Correlation
#' @importFrom lattice barchart xyplot panel.barchart panel.grid
#' @importFrom corrplot corrplot.mixed
#' @importFrom strucchange efp
#' @importFrom sn dst st.mple
#' 
#' @method plot sfm
#' @export

plot.sfm <- function(x, which=NULL, f.sub=1:2, a.sub=1:6, n.top=3, 
                     plot.single=FALSE, asset.name,
                     colorset=c("royalblue","dimgray","olivedrab","firebrick",
                                "goldenrod","mediumorchid","deepskyblue",
                                "chocolate","darkslategray"), 
                     legend.loc="topleft", las=1, lwd=2, maxlag=15, 
                     VaR.method="historical", eig.max=0.9, cum.var=TRUE, ...) {
  
  which.vec <- which
  which <- which[1]
  
  if (plot.single==TRUE) {
    
    if (missing(asset.name) && length(x$asset.names)>1) {
      stop("Missing input: 'asset.name' is required if plot.single is TRUE and 
           the factor model fits multiple assets.")   
    } else if (length(x$asset.names)==1) {
      i <- x$asset.names[1]
    } else {
      i <- asset.name
    }
    # extract info from the fitSfm object
    plotData <- merge.xts(x$data[,i], fitted(x)[,i], residuals(x)[,i])
    colnames(plotData) <- c("Actual","Fitted","Residuals")
    Residuals <- na.omit(plotData[,"Residuals"])
    fitData <- merge(x$data[,i], x$factors)
    fit <- lm(as.formula(paste(i," ~ .")), data=fitData)
    resid.sd <- x$resid.sd[i]
    den <- density(Residuals)
    xval <- den$x
    den.norm <- dnorm(xval, mean=mean(Residuals), sd=resid.sd)
    den.st <- dst(xval, dp=st.mple(x=matrix(1,nrow(Residuals)), y=as.vector(Residuals))$dp)
    
    # plot selection
    repeat {
      if (is.null(which)) {
        which <- 
          menu(c("Actual and fitted asset returns",
                 "Actual vs fitted asset returns",
                 "Residuals vs fitted asset returns",
                 "Sqrt. of modified residuals vs fitted",
                 "Residuals with standard error bands",
                 "Time series of squared residuals",
                 "Time series of absolute residuals",
                 "SACF and PACF of residuals",
                 "SACF and PACF of squared residuals",
                 "SACF and PACF of absolute residuals",
                 "Non-parametric density of residuals with normal overlaid",
                 "Non-parametric density of residuals with skew-t overlaid",
                 "Histogram of residuals with non-parametric density and normal overlaid",
                 "QQ-plot of residuals",
                 "CUSUM test-Recursive residuals",
                 "CUSUM test-LS residuals",
                 "Recursive estimates (RE) test of LS regression coefficients",
                 "Rolling estimates over a 24-period observation window"),
               title="\nMake a plot selection (or 0 to exit):")
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which,
             "1L" =  {
               ##  Time series plot of actual and fitted asset returns
               chart.TimeSeries(plotData[,1:2], main=paste("Actual and fitted:",i), 
                                colorset=colorset, xlab="", ylab="Returns", lwd=lwd,
                                legend.loc=legend.loc, pch=NULL, las=las, ...)
             }, "2L" = {
               ## Actual vs fitted asset returns
               plot(x=coredata(plotData[,2]), y=coredata(plotData[,1]), 
                    xlab="Fitted", ylab="Actual", col=colorset[1], 
                    main=paste("Actual vs fitted:",i), las=las, ...)
               abline(0, 1, col="dimgray",lty="dotted", lwd=lwd)
             }, "3L" = {
               ## Residuals vs fitted asset returns
               plot(x=coredata(plotData[,2]), y=coredata(plotData[,3]), 
                    xlab="Fitted", ylab="Residuals", col=colorset[1], 
                    main=paste("Residuals vs fitted:",i), las=las, ...)
               panel.smooth(x=coredata(plotData[,2]), y=coredata(plotData[,3]), 
                            col=colorset[1], col.smooth=colorset[2], lwd=lwd)
             }, "4L" = {
               ## Square root of absolute modified residuals vs fitted asset returns 
               yval <- sqrt(abs(Residuals/sqrt(1-hatvalues(fit))))
               plot(x=coredata(na.omit(plotData[,2])), y=yval, xlab="Fitted", 
                    ylab=expression(sqrt(abs("Modified Residuals"))), col=colorset[1], 
                    main=paste("Sqrt. modified residuals vs fitted:",i), las=las, ...)
               panel.smooth(x=coredata(na.omit(plotData[,2])), y=yval, 
                            col=colorset[1], col.smooth=colorset[2], lwd=lwd)
             }, "5L" = {
               ## Time series plot of residuals with standard error bands
               chart.TimeSeries(Residuals, main=paste("Residuals:",i), 
                                colorset=colorset, xlab="", ylab="Residuals", 
                                lwd=lwd, lty="solid", las=las, ...)
               abline(h=1.96*x$resid.sd[i], lwd=lwd, lty="dotted", col=colorset[2])
               abline(h=-1.96*x$resid.sd[i], lwd=lwd, lty="dotted", col=colorset[2])
               legend(x=legend.loc, lty=c("solid","dotted"), col=c(colorset[1:2]), 
                      lwd=lwd, bty="n", legend=c("Residuals",expression("\u00b1 1.96 "*sigma)))
             }, "6L" = {
               ## Time series plot of squared residuals
               chart.TimeSeries(Residuals^2, colorset=colorset, xlab="", 
                                ylab=" Squared Residuals", lwd=lwd,
                                main=paste("Squared Residuals:",i), 
                                legend.loc=NULL, pch=NULL, las=las, ...)
             }, "7L" = {
               ## time series plot of absolute residuals
               chart.TimeSeries(abs(Residuals), colorset=colorset, xlab="", 
                                ylab="Absolute Residuals", lwd=lwd,
                                main=paste("Absolute Residuals:",i), 
                                legend.loc=NULL, pch=NULL, las=las, ...)
             }, "8L" = {
               ## SACF and PACF of residuals
               chart.ACFplus(Residuals, maxlag=maxlag,
                             main=paste("SACF & PACF - Residuals:",i), ...)
             }, "9L" = {
               ## SACF and PACF of squared residuals
               chart.ACFplus(Residuals^2, maxlag=maxlag,
                             main=paste("SACF & PACF - Squared residuals:",i), ...)
             }, "10L" = {
               ## SACF and PACF of absolute residuals
               chart.ACFplus(abs(Residuals), col=colorset[1], ...,
                             main=paste("SACF & PACF - Absolute Residuals:",i))
             }, "11L" = {
               ## Non-parametric density of residuals with normal overlaid
               ymax <- ceiling(max(0,den$y,den.norm))
               plot(den, xlab="Return residuals", lwd=lwd, col=colorset[1], 
                    ylim=c(0,ymax), main=paste("Density of residuals:",i), ...)
               rug(Residuals, col="dimgray")
               lines(xval, den.norm, col=colorset[2], lwd=lwd, lty="dashed")
               legend(x=legend.loc, lty=c("solid","dashed"), col=c(colorset[1:2]), 
                      lwd=lwd, bty="n", legend=c("KDE","Normal"))
             }, "12L" = {
               ## Non-parametric density of residuals with skew-t overlaid
               ymax <- ceiling(max(0,den$y,den.st))
               plot(den, xlab="Return residuals", lwd=lwd, col=colorset[1], 
                    ylim=c(0,ymax), main=paste("Density of residuals:",i), ...)
               rug(Residuals, col="dimgray")
               lines(xval, den.st, col=colorset[2], lty="dashed", lwd=lwd)
               legend(x=legend.loc, lty=c("solid","dashed"), col=c(colorset[1:2]), 
                      lwd=lwd, bty="n", legend=c("KDE","Skew-t"))
             }, "13L" = {
               ## Histogram of residuals with non-parametric density and normal overlaid
               methods <- c("add.density","add.normal","add.rug")
               chart.Histogram(Residuals, xlab="Return residuals", methods=methods,
                               colorset=colorset[c(1,2,3)], 
                               lwd=lwd, main=paste("Histogram of residuals:",i), ...)
               legend(x=legend.loc, col=colorset[c(2,3)], lwd=lwd, bty="n", 
                      legend=c("KDE","Normal"))
             }, "14L" = {
               ##  QQ-plot of residuals
               chart.QQPlot(Residuals, envelope=0.95, col=colorset[1:2], lwd=lwd,
                            main=paste("QQ-plot of Residuals:",i), ...)
               legend(x=legend.loc, col=colorset[2], lty="dashed", lwd=1, bty="n",
                      legend=c("0.95 C.Env."))
             }, "15L" = {
               ##  Recursive CUSUM test
               cusum.rec <- efp(formula(fit), type="Rec-CUSUM", data=fit$model)
               plot(cusum.rec, main=paste("Recursive CUSUM test:",i), las=las, 
                    col=colorset, lwd=lwd, ...)
             }, "16L" = {
               ##  LS-based CUSUM test
               cusum.ols <- efp(formula(fit), type="OLS-CUSUM", data=fit$model)
               plot(cusum.ols, main=paste("LS-based CUSUM test:",i), las=las, 
                    col=colorset, lwd=lwd, ...)
             }, "17L" = {
               ##  Recursive estimates (RE) test of LS regression coefficients  
               if (x$k > 9) {
                 stop("Plotting limit: This option is not available when K > 9.")
               }
               cusum.est <- efp(formula(fit), type="RE", data=fit$model)
               plot(cusum.est, functional=NULL, col=colorset, las=0, cex.lab=0.7,
                    main=paste("RE test (Recursive estimates test):",i), ...)
               par(las=las, cex.lab=1)
             }, "18L" = {
               ##  Rolling estimates over 24-period observation window 
               reg.z <- zoo(fit$model, as.Date(rownames(fit$model)))
               rollReg.z <- rollapply(reg.z, width=24, by.column=FALSE, align="right",
                                      FUN = function(z) coef(lm(formula(fit), data=as.data.frame(z))))
               par(las=0)
               plot(rollReg.z, las=las, cex=0.8, lwd=lwd, col=colorset[1], ...,
                    main=paste("Rolling regression (24-period obs window):",i))
               par(las=las, cex=1)
             }, 
             invisible()
      )
      # repeat menu if user didn't choose to exit from the plot options
      if (which==0 || length(which.vec)==1) {break} 
      if (length(which.vec)>1) {
        which.vec <- which.vec[-1]
        which <- which.vec[1]
        par(ask=TRUE)
      } else {which=NULL}
    } 
  } else { # start of group asset plots
    
    n <- length(x$asset.names)
    if (n<=1 || length(a.sub) < 2) {
      stop("Error: Two or more assets required for group plots.")
    }
    if (!(all(a.sub %in% x$asset.names)) && !(all(a.sub %in% 1:n))) {
      stop("Invalid argument: a.sub is not a valid subset of asset names.") 
    }
    k <- x$k
    f.names <- paste("F", 1:k, sep = ".")
    if (!(all(f.sub %in% f.names)) && !(all(f.sub %in% 1:k))) {
      stop("Invalid argument: f.sub is not a valid subset of factor names.") 
    }
    
    # plot selection
    repeat {
      if (is.null(which)) {
        which <- 
          menu(c("Screeplot of eigenvalues",
                 "Time series plot of estimated factors",
                 "Estimated factor loadings", 
                 "Histogram of R-squared", 
                 "Histogram of residual volatility", 
                 "Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars",
                 "Factor model residual correlation",
                 "Factor model return correlation",
                 "Factor contribution to SD", 
                 "Factor contribution to ES", 
                 "Factor contribution to VaR",
                 "Factor mimicking portfolio weights - top long and short positions in each factor",
                 "Asset correlations - top long and short positions in each factor"), 
               title="\nMake a plot selection (or 0 to exit):") 
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which,
             "1L" = { 
               ## Screeplot of eigenvalues
               cumv <- cumsum(x$eigen)/sum(x$eigen)
               limit <- length(cumv[cumv<eig.max]) + 1
               eig <- x$eigen[1:limit]
               scree <- barplot(eig, main="Screeplot of eigenvalues", 
                                ylab="Variance", col=colorset[1], 
                                ylim=c(0, 1.1*max(eig)), las=las, ...)
               if (cum.var) {
                 text(scree, eig, label=round(cumv[1:limit],2), pos=3, cex=0.75)
               }
             }, 
             "2L" = {
               ## Time series plot of estimated factors
               plot(
                 xyplot(x$factors[,f.sub], type=c("l","g"), xlab="", lwd=lwd,
                        col.line=colorset[1], scales=list(y=list(rot=0)), 
                        strip.left=TRUE, strip=FALSE, ...)
               )
             }, 
             "3L" = {    
               ## Estimated factor loadings
               C <- x$loadings[a.sub,f.sub,drop=FALSE]
               Y <- row(C, as.factor=T)
               X <- as.vector(as.matrix(C[,,drop=FALSE]))
               Z <- col(C, as.factor=T)
               plot(
                 barchart(Y~X|Z, main="Factor Loadings \n", xlab="", as.table=TRUE,
                          origin=0, col=colorset[1], scales=list(relation="free"), ...)
               )
               #                par(mfrow=c(ceiling(length(f.sub)/2),2))
               #                for (i in f.sub) {
               #                  main=paste("Factor loadings: ", colnames(x$loadings)[i])
               #                  barplot(x$loadings[a.sub,i], main=main, 
               #                          names.arg=rownames(x$loadings)[a.sub], 
               #                          col=colorset[1], las=2, ...)
               #                  abline(h=0, lwd=1, lty=1, col=1)
               #                }
               #                par(mfrow=c(1,1))
             }, 
             "4L" = {
               ## Histogram of R-squared
               methods <- c("add.density","add.rug")
               chart.Histogram(x$r2, xlab="R-squared", lwd=lwd,
                               methods=methods, colorset=colorset[c(1,2)], 
                               main=paste("Histogram of R-squared values"), ...)
               legend(x=legend.loc, col=colorset[2], lwd=lwd, bty="n", legend="KDE")
             }, 
             "5L" = {
               ## Histogram of Residual Volatility
               methods <- c("add.density","add.rug")
               chart.Histogram(x$resid.sd, xlab="Residual volatility",
                               methods=methods, colorset=colorset[c(1,2)], 
                               main=paste("Histogram of Residual volatilities"), ...)
               legend(x=legend.loc, col=colorset[2], lwd=lwd, bty="n", legend="KDE")
             }, 
             "6L" = {
               ## Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars
               chart.Correlation(residuals(x)[,a.sub], ...)
             }, 
             "7L" = {
               ## Factor Model Residual Correlation
               cor.resid <- cor(residuals(x)[,a.sub], use="pairwise.complete.obs")
               corrplot.mixed(cor.resid, tl.col=1, upper="ellipse", ...)
               # mtext("pairwise complete obs", line=0.5)
             }, 
             "8L" = {
               ## Factor Model Return Correlation
               cor.fm <- cov2cor(fmCov(x))[a.sub,a.sub]
               corrplot.mixed(cor.fm, tl.col=1, upper="ellipse", ...)
               # mtext("pairwise complete obs", line=0.5)
             },
             "9L" = {
               ## Factor Percentage Contribution to SD
               pcSd.fm <- fmSdDecomp(x)$pcSd[a.sub,c(f.sub,k+1)]
               plot(
                 barchart(pcSd.fm, main="Factor % Contribution to SD", xlab="",
                          auto.key=list(space="bottom",columns=3, points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "10L" = {
               ## Factor Percentage Contribution to ES
               pcES.fm <- fmEsDecomp(x, method=VaR.method)$pcES[a.sub,c(f.sub,k+1)]
               plot(
                 barchart(pcES.fm, main="Factor % Contribution to ES", xlab="",
                          auto.key=list(space="bottom",columns=3, points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "11L" = {
               ## Factor Percentage Contribution to VaR
               pcVaR.fm <- fmVaRDecomp(x, method=VaR.method)$pcVaR[a.sub,c(f.sub,k+1)]
               plot(
                 barchart(pcVaR.fm, main="Factor % Contribution to VaR", xlab="", 
                          auto.key=list(space="bottom",columns=3, points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "12L" = {
               ## Factor mimicking portfolio weights - top long and short positions in each factor
               if (length(f.sub) < 2) {
                 par(mfrow=c(1,1))
               } else {
                 par(mfrow=c(ceiling(length(f.sub)/2),2))
               }
               for (i in f.sub) {
                 main=paste("Largest & smallest weights (%): ", colnames(x$loadings)[i])
                 s <- summary(x, n.top=n.top)$mimic.sum[[i]]
                 top <- 100*stack(s[,c(2,4)])$values
                 names.arg <- stack(s[,c(1,3)])$values
                 barplot(top, main=main, names.arg=names.arg, col=colorset[1], las=2, cex.main=0.9, ...)
                 abline(h=0, lwd=1, lty=1, col=1)
               }
               par(mfrow=c(1,1))
             },
             "13L" = {
               ## Asset correlations - top long and short positions in each factor
               par(ask=TRUE)
               for (i in f.sub) {
                 main=paste("Correlations of top long/short positions in ", colnames(x$loadings)[i])
                 s <- summary(x, n.top=n.top)$mimic.sum[[i]]
                 names.arg <- stack(s[,c(1,3)])$values
                 cor.fm <- cov2cor(fmCov(x))[names.arg,names.arg]
                 corrplot.mixed(cor.fm, tl.col=1, tl.cex=0.7, upper="ellipse", ...)
               }
               par(ask=FALSE)
             },
             invisible()       
      )         
      # repeat menu if user didn't choose to exit from the plot options
      if (which==0 || length(which.vec)==1) {break} 
      if (length(which.vec)>1) {
        which.vec <- which.vec[-1]
        which <- which.vec[1]
        par(ask=TRUE)
      } else {which=NULL}   
    }
  } # end of group plots
  # revert to default par values
  par(ask=FALSE)
  par(las=0)
}
