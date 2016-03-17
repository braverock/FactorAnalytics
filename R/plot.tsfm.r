#' @title Plots from a fitted time series factor model
#' 
#' @description Generic \code{plot} method for object of class \code{tsfm}. 
#' Plots chosen characteristic(s) for one or more assets. 
#' 
#' @details 
#' 
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
#' CUSUM plots (individual asset plot options 15, 16 and 17) are applicable 
#' only for \code{fit.method="LS"}.
#' 
#' Modified residuals, rolling regression and single factor model plots 
#' (individual asset plot options 4, 18 and 19) are not applicable for 
#' \code{variable.selection="lars"}.
#' 
#' The last option for plotting asset returns vs. factor returns (individual 
#' asset plot option 19 and group plot 12) are only applicable for single factor
#' models.
#' 
#' @param x an object of class \code{tsfm} produced by \code{fitTsfm}.
#' @param which a number to indicate the type of plot. If a subset of the plots 
#' is required, specify a subset of the numbers 1:12 for group plots and 1:19 
#' for individual plots. If \code{which=NULL} (default), the following menu 
#' appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Factor model coefficients: Alpha, \cr
#' 2 = Factor model coefficients: Betas, \cr
#' 3 = Actual and fitted, \cr
#' 4 = R-squared, \cr
#' 5 = Residual volatility,\cr
#' 6 = Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars, \cr
#' 7 = Factor model residual correlation \cr
#' 8 = Factor model return correlation,\cr
#' 9 = Factor contribution to SD,\cr
#' 10 = Factor contribution to ES,\cr
#' 11 = Factor contribution to VaR, \cr
#' 12 = Asset returns vs factor returns (single factor model) \cr \cr
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
#' 18 = Rolling regression over a 24-period observation window, \cr
#' 19 = Asset returns vs factor returns (single factor model)
#' @param f.sub numeric/character vector; subset of indexes/names of factors to 
#' include for group plots. Default is 1:2.
#' @param a.sub numeric/character vector; subset of indexes/names of assets to 
#' include for group plots. At least 2 assets must be selected. Default is 1:6.
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
#' @param ... further arguments to be passed to other plotting functions.
#' 
#' @author Eric Zivot, Sangeetha Srinivasan and Yi-An Chen
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{residuals.tsfm}}, 
#' \code{\link{fitted.tsfm}}, \code{\link{fmCov.tsfm}} and 
#' \code{\link{summary.tsfm}} for time series factor model fitting and related 
#' S3 methods. Refer to \code{\link{fmSdDecomp}}, \code{\link{fmEsDecomp}}, 
#' \code{\link{fmVaRDecomp}} for factor model risk measures.
#' 
#' Here is a list of plotting functions used. (I=individual, G=Group)
#' I(1,5,6,7), G(3) - \code{\link[PerformanceAnalytics]{chart.TimeSeries}}, 
#' I(2,3,4,19), G(12) - \code{\link[graphics]{plot.default}},
#' I(3,4) - \code{\link[graphics]{panel.smooth}},
#' I(8,9,10) - \code{\link[PerformanceAnalytics]{chart.ACFplus}}, 
#' I(11,12) - \code{\link[stats]{plot.density}},
#' I(13) - \code{\link[PerformanceAnalytics]{chart.Histogram}},
#' I(14) - \code{\link[PerformanceAnalytics]{chart.QQPlot}}, 
#' I(15,16,17) - \code{\link[strucchange]{plot.efp}},
#' I(18) - \code{\link[zoo]{plot.zoo}},
#' G(1,2,4,5,9,10,11) - \code{\link[lattice]{barchart}},
#' G(6) - \code{\link[PerformanceAnalytics]{chart.Correlation}} and
#' G(7,8) - \code{\link[corrplot]{corrplot.mixed}}. 
#' 
#' @examples
#' 
#' # load data from the database
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name="US.3m.TR", data=managers)
#'     
#' # for group plots (default), user can select plot option from menu prompt
#' # menu is repeated to get multiple types of plots based on the same fit
#' # plot(fit.macro)
#'                
#' # choose specific plot option(s) using which
#' # plot the first 2 factor betas of first 4 assets fitted above
#' plot(fit.macro, f.sub=1:2, a.sub=1:4, which=2)
#' 
#' # plot factor model residuals scatterplot matrix, with histograms, density 
#' # overlays, correlations and significance stars
#' plot(fit.macro, which=6)
#' 
#' # for individual plots: set plot.single=TRUE and specify asset.name
#' # histogram of residuals from an individual asset's factor model fit 
#' plot(fit.macro, plot.single=TRUE, asset.name="HAM1", which=13)
#' 
#' @method plot tsfm
#' @export

plot.tsfm <- function(x, which=NULL, f.sub=1:2, a.sub=1:6, 
                      plot.single=FALSE, asset.name,  
                      colorset=c("royalblue","dimgray","olivedrab","firebrick",
                                 "goldenrod","mediumorchid","deepskyblue",
                                 "chocolate","darkslategray"), 
                      legend.loc="topleft", las=1, lwd=2, maxlag=15, ...) {
  
  which.vec <- which
  which <- which[1]
  
  meth <- x$fit.method # one of "LS", "DLS" or "Robust"
  if (is.null(meth)) {meth <- "Lars"}
  
  if (plot.single==TRUE) {
    
    if (missing(asset.name) && length(x$asset.names)>1) {
      stop("Missing input: 'asset.name' is required if plot.single is TRUE and 
           the factor model fits multiple assets.")   
    } else if (length(x$asset.names)==1) {
      i <- x$asset.names[1]
    } else {
      i <- asset.name
    }
    # extract info from the fitTsfm object
    plotData <- merge.xts(x$data[,i], fitted(x)[,i], residuals(x)[,i])
    colnames(plotData) <- c("Actual","Fitted","Residuals")
    Residuals <- na.omit(plotData[,"Residuals"])
    fit <- x$asset.fit[[i]]
    resid.sd <- x$resid.sd[i]
    den <- density(Residuals)
    xval <- den$x
    den.norm <- dnorm(xval, mean=mean(Residuals), sd=resid.sd)
    dp.st <- st.mple(x=matrix(1,nrow(Residuals)), y=as.vector(Residuals), opt.method="BFGS")$dp
    den.st <- dst(xval, dp=dp.st)
    dp.st <- signif(dp.st, 2)
    
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
                 "Rolling estimates over a 24-period observation window",
                 "Asset returns vs factor returns (single factor model)"),
               title="\nMake a plot selection (or 0 to exit):")
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which,
             "1L" = {
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
               if (meth=="Lars") {
                 stop("This option is not available for 'lars' fits.")
               } 
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
                                ylab=" Squared residuals", lwd=lwd,
                                main=paste("Squared residuals:",i), 
                                legend.loc=NULL, pch=NULL, las=las, ...)
             }, "7L" = {
               ## Time series plot of absolute residuals
               chart.TimeSeries(abs(Residuals), colorset=colorset, xlab="", 
                                ylab="Absolute residuals", lwd=lwd,
                                main=paste("Absolute residuals:",i), 
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
               chart.ACFplus(abs(Residuals), maxlag=maxlag,
                             main=paste("SACF & PACF - Absolute residuals:",i), ...)
             }, "11L" = {
               ## Non-parametric density of residuals with normal overlaid
               ymax <- ceiling(max(0,den$y,den.norm))
               plot(den, xlab="Return residuals", lwd=lwd, col=colorset[1], 
                    ylim=c(0,ymax), main=paste("Density of residuals:",i), ...)
               rug(Residuals, col="dimgray")
               lines(xval, den.norm, col=colorset[2], lwd=lwd, lty="dashed")
               legend(x=legend.loc, lty=c("solid","dashed"), col=c(colorset[1:2]), 
                      lwd=lwd, bty="n", legend=c("KDE","Normal"))
               mtext(text=paste("Normal (mu=",round(mean(Residuals),4),", sd=",
                                round(resid.sd,4),")",sep=""), side=3, line=0.25, cex=0.8)
             }, "12L" = {
               ## Non-parametric density of residuals with skew-t overlaid
               ymax <- ceiling(max(0,den$y,den.st))
               plot(den, xlab="Return residuals", lwd=lwd, col=colorset[1], 
                    ylim=c(0,ymax), main=paste("Density of residuals:",i), ...)
               rug(Residuals, col="dimgray")
               lines(xval, den.st, col=colorset[2], lty="dashed", lwd=lwd)
               legend(x=legend.loc, lty=c("solid","dashed"), col=c(colorset[1:2]), 
                      lwd=lwd, bty="n", legend=c("KDE","Skew-t"))
               mtext(text=paste("Skew-t (xi=",dp.st[1],", omega=",dp.st[2],", alpha=",dp.st[3],
                                ", nu=",dp.st[4],")",sep=""), side=3, line=0.25, cex=0.8)
             }, "13L" = {
               ## Histogram of residuals with non-parametric density and normal overlaid
               methods <- c("add.density","add.normal","add.rug")
               chart.Histogram(Residuals, xlab="Return residuals", 
                               methods=methods, colorset=colorset[c(1,2,3)], 
                               lwd=lwd, main=paste("Histogram of residuals:",i), ...)
               legend(x=legend.loc, col=colorset[c(2,3)], lwd=lwd, bty="n", 
                      legend=c("KDE","Normal"))
               mtext(text=paste("Normal (mu=",round(mean(Residuals),4),", sd=",
                                round(resid.sd,4),")",sep=""), side=3, line=0.25, cex=0.8)
             }, "14L" = {
               ##  QQ-plot of residuals
               chart.QQPlot(Residuals, envelope=0.95, col=colorset[1:2], lwd=lwd,
                            main=paste("QQ-plot of residuals:",i), ...)
               legend(x=legend.loc, col=colorset[2], lty="dashed", lwd=1, bty="n",
                      legend=c("0.95 C.Env."))
             }, "15L" = {
               ##  Recursive CUSUM test
               if (!meth=="LS") {
                 stop("CUSUM analysis applicable only for 'LS' fit.method.")
               }
               cusum.rec <- efp(formula(fit), type="Rec-CUSUM", data=fit$model)
               plot(cusum.rec, main=paste("Recursive CUSUM test:",i), las=las, 
                    col=colorset, lwd=lwd, ...)
             }, "16L" = {
               ##  OLS-based CUSUM test
               if (!meth=="LS") {
                 stop("CUSUM analysis applicable only for 'LS' fit.method.")
               }
               cusum.ols <- efp(formula(fit), type="OLS-CUSUM", data=fit$model)
               plot(cusum.ols, main=paste("LS-based CUSUM test:",i), las=las, 
                    col=colorset, lwd=lwd, ...)
             }, "17L" = {
               ##  Recursive estimates (RE) test of LS regression coefficients
               if (!meth=="LS") {
                 stop("CUSUM analysis applicable only for 'LS' fit.method.")
               }        
               cusum.est <- efp(formula(fit), type="RE", data=fit$model)
               plot(cusum.est, functional=NULL, col=colorset, las=0, cex.lab=0.7,
                    main=paste("RE test (Recursive estimates test):",i), ...)
               par(las=las, cex.lab=1)
             }, "18L" = {
               ##  Rolling regression over 24-period observation window 
               if (meth=="Lars") {
                 stop("This option is not available for 'lars' fits.")
               } else if (meth=="LS") {
                 reg.z <- zoo(fit$model, as.Date(rownames(fit$model)))
                 rollReg.z <- rollapply(reg.z, width=24, by.column=FALSE, align="right",
                                        FUN = function(z) coef(lm(formula(fit), data=as.data.frame(z))))
               } else if (meth=="DLS") {
                 # get decay factor
                 if (as.character(x$call["decay"])=="NULL") {
                   decay <- 0.95 # default value for the decay factor
                 } else {
                   decay <- as.numeric(as.character(x$call["decay"]))
                 }
                 reg.z <- zoo(fit$model[-length(fit$model)], as.Date(rownames(fit$model)))
                 # using exp. decaying weights for 24-period window
                 rollReg.z <- rollapply(reg.z, width=24, by.column=FALSE, align="right",
                                        FUN = function(z) coef(lm(formula(fit), data=as.data.frame(z), weights=decay^seq(23,0,-1))))
               } else if (meth=="Robust") {
                 reg.z <- zoo(fit$model, as.Date(rownames(fit$model)))
                 rollReg.z <- rollapply(reg.z, width=24, by.column=FALSE, align="right",
                                        FUN = function(z) coef(lmRob(formula(fit), data=as.data.frame(z))))
               }
               par(las=0)
               plot(rollReg.z, las=las, cex=0.8, lwd=lwd, col=colorset[1], ...,
                    main=paste("Rolling regression (24-period obs window):",i))
               par(las=las, cex=1)
             }, "19L" = {
               ## Asset returns vs factor returns (single factor model)
               if (meth=="Lars") {
                 stop("This option is not available for 'lars' fits.")
               }
               if (length(x$factor.names)>1) {
                 stop("Error: This option is only for single factor models.")
               }
               rawdata <- coredata(merge.xts(x$data[,i], x$data[,x$factor.names]))
               plot(x=rawdata[,2], y=rawdata[,1], pch=20, main="",
                    xlab=paste(x$factor.names, "Returns"), ylab=paste(i,"Returns"), ...)
               coef <- summary(fit)$coefficients
               a=coef[1,1]; b=coef[2,1]
               se.a=round(coef[1,2],2); se.b=round(coef[2,2],2)
               abline(a=a, b=b, col="red", lty=2, lwd=2)
               lgnd <- c(bquote(.(meth) ~~ hat(alpha) == .(round(a,2))~(.(se.a))), 
                         bquote(.(meth) ~~  hat(beta) == .(round(b,2))~(.(se.b))))
               legend(x=legend.loc, bty="n", legend=as.expression(lgnd), cex=1.2)
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
    k <- ncol(x$beta)
    if (!(all(f.sub %in% x$factor.names)) && !(all(f.sub %in% 1:k))) {
      stop("Invalid argument: f.sub is not a valid subset of factor names.") 
    }
    if (is.character(f.sub)) {
      f.sub <- which(f.sub %in% x$factor.names)
    }
    if (is.character(a.sub)) {
      f.sub <- which(x$asset.names==a.sub)
    }
    
    # plot selection
    repeat {
      if (is.null(which)) {
        which <- 
          menu(c("Factor model coefficients: Alpha",
                 "Factor model coefficients: Betas",
                 "Actual and Fitted asset returns", 
                 "R-squared", 
                 "Residual Volatility",
                 "Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars",
                 "Factor Model Residual Correlation",
                 "Factor Model Return Correlation",
                 "Factor Contribution to SD", 
                 "Factor Contribution to ES", 
                 "Factor Contribution to VaR",
                 "Asset returns vs factor returns (single factor model)"), 
               title="\nMake a plot selection (or 0 to exit):") 
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which,
             "1L" = { 
               ## Factor model coefficients: Alpha
               plot(
                 barchart(as.matrix(x$alpha)[a.sub,], main="Factor model Alpha (Intercept)", xlab="", col=colorset[1], ...)
               )
             }, 
             "2L" = {
               ## Factor model coefficients: Betas
               C <- x$beta[a.sub,f.sub,drop=FALSE]
               Y <- row(C, as.factor=T)
               X <- as.vector(as.matrix(C[,,drop=FALSE]))
               Z <- col(C, as.factor=T)
               plot(
                 barchart(Y~X|Z, main="Factor model Betas \n", xlab="", as.table=TRUE,
                          origin=0, col=colorset[1], scales=list(relation="free"), ...)
               )
             }, 
             "3L" = {    
               ## Actual and fitted asset returns
               if (length(a.sub) < 5) {
                 par(mfrow=c(length(a.sub),1))
               } else {
                 par(mfrow=c(ceiling(length(a.sub)/2),2))
               }
               for (i in a.sub) {
                 asset <- x$asset.names[i]
                 plotData <- merge.xts(x$data[,asset], fitted(x)[,asset])
                 colnames(plotData) <- c("Actual","Fitted")
                 main <- paste("Actual and Fitted:", asset)
                 chart.TimeSeries(plotData, colorset=colorset, lwd=lwd, main=main, xlab="", 
                                  ylab="Asset returns", legend.loc=legend.loc, pch=NULL, las=las,...)
               }
               par(mfrow=c(1,1))
             }, 
             "4L" ={
               ## R-squared
               plot(
                 barchart(x$r2[a.sub], main="R-squared values", xlab="", col=colorset[1], ...)
               )
             }, 
             "5L" = {
               ## Residual volatility
               plot(
                 barchart(x$resid.sd[a.sub], main="Residual volatility", xlab="", col=colorset[1], ...)
               )
             }, 
             "6L" = {
               ## Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars
               chart.Correlation(residuals(x)[,a.sub], ...)
             }, 
             "7L" = {
               ## Factor model residual correlation
               cor.resid <- cor(residuals(x)[,a.sub], use="pairwise.complete.obs")
               corrplot.mixed(cor.resid, tl.col=1, upper="ellipse", ...)
               # mtext("pairwise complete obs", line=0.5)
             }, 
             "8L" = {
               ## Factor model return correlation
               cor.fm <- cov2cor(fmCov(x)[a.sub,a.sub]) 
               corrplot.mixed(cor.fm, tl.col=1, upper="ellipse", ...)
               # mtext("pairwise complete obs", line=0.5)
             },
             "9L" = {
               ## Factor percentage contribution to SD
               pcSd.fm <- fmSdDecomp(x)$pcSd[a.sub,c(f.sub,k+1)]
               plot(
                 barchart(pcSd.fm, main="Factor % Contribution to SD", xlab="",
                          auto.key=list(space="bottom",columns=3,points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); panel.barchart(...)}, ...)
               )
             },
             "10L"={
               ## Factor percentage contribution to ES
               pcES.fm <- fmEsDecomp(x)$pcES[a.sub,c(f.sub,k+1)]
               plot(
                 barchart(pcES.fm, main="Factor % Contribution to ES", xlab="",
                          auto.key=list(space="bottom",columns=3,points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); panel.barchart(...)}, ...)
               )
             },
             "11L" ={
               ## Factor percentage contribution to VaR
               pcVaR.fm <- fmVaRDecomp(x)$pcVaR[a.sub,c(f.sub,k+1)]
               plot(
                 barchart(pcVaR.fm, main="Factor % Contribution to VaR", xlab="", 
                          auto.key=list(space="bottom",columns=3,points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "12L" ={
               ## Asset returns vs factor returns (single factor model)
               if (meth=="Lars") {
                 stop("This option is not available for 'lars' fits.")
               }
               if (length(x$factor.names)>1) {
                 stop("Error: This option is only for single factor models.")
               }
               if (length(a.sub) < 5) {
                 par(mfrow=c(length(a.sub),1))
               } else {
                 par(mfrow=c(ceiling(length(a.sub)/2),2))
               }
               for (i in a.sub) {
                 fit <- x$asset.fit[[i]]
                 asset <- x$asset.names[i]
                 rawdata <- coredata(merge.xts(x$data[,asset], x$data[,x$factor.names]))
                 plot(x=rawdata[,2], y=rawdata[,1], pch=20, main="",
                      xlab=paste(x$factor.names, "Returns"), ylab=paste(asset,"Returns"), ...)
                 coef <- summary(fit)$coefficients
                 a=coef[1,1]; b=coef[2,1]
                 se.a=round(coef[1,2],2); se.b=round(coef[2,2],2)
                 abline(a=a, b=b, col="red", lty=2, lwd=2)
                 lgnd <- c(bquote(.(meth) ~~ hat(alpha) == .(round(a,2))~(.(se.a))), 
                           bquote(.(meth) ~~  hat(beta) == .(round(b,2))~(.(se.b))))
                 legend(x=legend.loc, bty="n", legend=as.expression(lgnd), cex=0.9)
               }
               par(mfrow=c(1,1))
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
