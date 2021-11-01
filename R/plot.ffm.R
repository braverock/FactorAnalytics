#' @title Plots from a fitted fundamental factor model
#'
#' @description Generic \code{plot} method for object of class \code{ffm}.
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
#' In case the \code{ffm} object \code{x} contains only a single asset fit,
#' plot.ffm can infer \code{asset.name} without user input.
#'
#' @importFrom xts as.xts merge.xts
#' @importFrom zoo index
#' @importFrom sn dst rst st.mple
#' @importFrom lattice panel.barchart panel.grid barchart
#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus chart.Histogram 
#' chart.QQPlot chart.Correlation chart.Boxplot
#' 
#' @param x an object of class \code{ffm} produced by \code{fitFfm}.
#' @param which a number to indicate the type of plot. If multiple plots are
#' required, specify a subset from 1:12 for group plots and 1:13 for individual
#' plots. If \code{which=NULL} (default), the following menu appears: \cr \cr
#' For plots of a group of assets: \cr
#' 1 = Distribution of factor returns, \cr
#' 2 = Factor exposures from the last period, \cr
#' 3 = Actual and Fitted asset returns, \cr
#' 4 = Time-series of R-squared values, \cr
#' 5 = Residual variance across assets, x \cr
#' 6 = Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars, \cr
#' 7 = Factor Model Residual Correlation \cr
#' 8 = Factor Model Return Correlation,\cr
#' 9 = Factor Contribution to SD,\cr
#' 10 = Factor Contribution to ES,\cr
#' 11 = Factor Contribution to VaR, \cr
#' 12 = Time series of factor returns, \cr \cr
#' For individual asset plots:\cr
#' 1 = Actual and fitted,\cr
#' 2 = Actual vs. fitted,\cr
#' 3 = Residuals vs. fitted, \cr
#' 4 = Residuals with standard error bands, \cr
#' 5 = Time series of squared residuals, \cr
#' 6 = Time series of absolute residuals,\cr
#' 7 = SACF and PACF of residuals,\cr
#' 8 = SACF and PACF of squared residuals,\cr
#' 9 = SACF and PACF of absolute residuals,\cr
#' 10 = Non-parametric density of residuals with normal overlaid, \cr
#' 11 = Non-parametric density of residuals with skew-t overlaid, \cr
#' 12 = Histogram of residuals with non-parametric density and normal overlaid,\cr
#' 13 = QQ-plot of residuals
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
#' @seealso \code{\link{fitFfm}}, \code{\link{residuals.ffm}},
#' \code{\link{fitted.ffm}}, \code{\link{fmCov.ffm}} and
#' \code{\link{summary.ffm}} for time series factor model fitting and related
#' S3 methods. Refer to \code{\link{fmSdDecomp}}, \code{\link{fmEsDecomp}},
#' \code{\link{fmVaRDecomp}} for factor model risk measures.
#'
#' Here is a list of plotting functions used. (I=individual, G=Group)
#' I(1,5,6,7), G(3,4,12) - \code{\link[PerformanceAnalytics]{chart.TimeSeries}},
#' I(2,3,4,19), G(12) - \code{\link[graphics]{plot.default}},
#' I(3,4) - \code{\link[graphics]{panel.smooth}},
#' I(8,9,10) - \code{\link[PerformanceAnalytics]{chart.ACFplus}},
#' I(11,12) - \code{\link[stats]{plot.density}},
#' I(13) - \code{\link[PerformanceAnalytics]{chart.Histogram}},
#' I(14) - \code{\link[PerformanceAnalytics]{chart.QQPlot}},
#' I(15,16,17) - \code{\link[strucchange]{plot.efp}} (requires strucchange package),
#' I(18) - \code{\link[zoo]{plot.zoo}},
#' G(1) - \code{\link[PerformanceAnalytics]{chart.Boxplot}},
#' G(2,5,9,10,11) - \code{\link[lattice]{barchart}},
#' G(6) - \code{\link[PerformanceAnalytics]{chart.Correlation}} and
#' G(7,8) - \code{\link[corrplot]{corrplot.mixed}} (requires corrplot package).
#'
#' @examples
#'
#' # load data from the database
#' data("factorDataSetDjia5Yrs")
#' 
#' # fit a fundamental factor model
#' exposure.vars <- c("P2B", "MKTCAP")
#' fit.style.sector <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", 
#'                            ret.var="RETURN", date.var="DATE", 
#'                            exposure.vars=exposure.vars)
#'
#' # for group plots (default), user can select plot option from menu prompt
#' # menu is repeated to get multiple types of plots based on the same fit
#' # plot(fit.style.sector)
#'
#' # choose specific plot option(s) using which
#' # plot all factor exposures from the last time period for 1st 10 assets
#' plot(fit.style.sector, which=2, f.sub=1:2, a.sub=1:10)
#'
#' # plot factor model residuals scatterplot matrix, with histograms, density
#' # overlays, correlations and significance stars
#' plot(fit.style.sector, which=6)
#'
#' # for individual plots: set plot.single=TRUE and specify asset.name
#' # histogram of residuals from an individual asset's factor model fit
#' plot(fit.style.sector, plot.single=TRUE, asset.name="AA", which=12)
#'
#' @method plot ffm
#' @export

plot.ffm <- function(x, which=NULL, f.sub=1:2, a.sub=1:6,
                     plot.single=FALSE, asset.name,
                     colorset=c("royalblue","dimgray","olivedrab","firebrick",
                                "goldenrod","mediumorchid","deepskyblue",
                                "chocolate","darkslategray"),
                     legend.loc="topleft", las=1, lwd=2, maxlag=15, ...) {
  
  which.vec <- which
  which <- which[1]
  
  meth <- x$fit.method # one of "LS", "WLS", "Rob" or "W-Rob"
  
  if (plot.single==TRUE) {
    
    if (missing(asset.name) && length(x$asset.names)>1) {
      stop("Missing input: 'asset.name' is required if plot.single is TRUE and
           the factor model fits multiple assets.")
    } else if (length(x$asset.names)==1) {
      i <- x$asset.names[1]
    } else {
      i <- asset.name
    }
    # extract info from the fitFfm object
    fitted.ret <- fitted(x)[,i]
    resid.ret <- residuals(x)[,i]
    asset.ret <- subset(x = x$data, subset = get(fit.style.sector$asset.var) == i)
    asset.ret <- asset.ret[ ,c(x$date.var, x$ret.var)]
    asset.ret.xts <- xts::as.xts(asset.ret[,2], order.by=zoo::index(fitted.ret))
    plotData <- merge.xts(asset.ret.xts, fitted.ret, resid.ret)
    colnames(plotData) <- c("Actual","Fitted","Residuals")
    Residuals <- na.omit(plotData[,"Residuals"])
    resid.sd <- sqrt(x$resid.var[i])
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
                 "Actual vs. fitted asset returns",
                 "Residuals vs. fitted asset returns",
                 "Residuals with standard error bands",
                 "Time series of squared residuals",
                 "Time series of absolute residuals",
                 "SACF and PACF of residuals",
                 "SACF and PACF of squared residuals",
                 "SACF and PACF of absolute residuals",
                 "Non-parametric density of residuals with normal overlaid",
                 "Non-parametric density of residuals with skew-t overlaid",
                 "Histogram of residuals with non-parametric density and normal overlaid",
                 "QQ-plot of residuals"),
               title="\nMake a plot selection (or 0 to exit):")
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which,
             "1L" = {
               ##  Time series plot of actual and fitted asset returns
      PerformanceAnalytics::chart.TimeSeries(plotData[,1:2], 
                            main = paste("Actual and fitted asset returns:", i),
                            colorset = colorset, xlab = "", ylab = "Returns", 
                            lwd = lwd, legend.loc = legend.loc, pch = NULL, 
                            las = las, ...)
             }, "2L" = {
               ## Actual vs. fitted asset returns
               plot(x=coredata(plotData[,2]), y=coredata(plotData[,1]),
                    xlab="Fitted", ylab="Actual", col=colorset[1],
                    main=paste("Actual vs. fitted asset returns:",i), las=las, ...)
               abline(0, 1, col="dimgray",lty="dotted", lwd=lwd)
             }, "3L" = {
               ## Residuals vs. fitted asset returns
               plot(x=coredata(plotData[,2]), y=coredata(plotData[,3]),
                    xlab="Fitted", ylab="Residuals", col=colorset[1],
                    main=paste("Residuals vs. fitted:",i), las=las, ...)
               panel.smooth(x=coredata(plotData[,2]), y=coredata(plotData[,3]),
                            col=colorset[1], col.smooth=colorset[2], lwd=lwd)
             }, "4L" = {
               ## Time series plot of residuals with standard error bands
               PerformanceAnalytics::chart.TimeSeries(Residuals, main=paste("Residuals:",i),
                                colorset=colorset, xlab="", ylab="Residuals",
                                lwd=lwd, lty="solid", las=las, ...)
               abline(h=1.96*resid.sd, lwd=lwd, lty="dotted", col=colorset[2])
               abline(h=-1.96*resid.sd, lwd=lwd, lty="dotted", col=colorset[2])
               legend(x=legend.loc, lty=c("solid","dotted"), col=c(colorset[1:2]),
                      lwd=lwd, bty="n", legend=c("Residuals",expression("\u00b1 1.96 "*sigma)))
             }, "5L" = {
               ## Time series plot of squared residuals
               PerformanceAnalytics::chart.TimeSeries(Residuals^2, colorset=colorset, xlab="",
                                ylab=" Squared residuals", lwd=lwd,
                                main=paste("Squared residuals:",i),
                                legend.loc=NULL, pch=NULL, las=las, ...)
             }, "6L" = {
               ## Time series plot of absolute residuals
               PerformanceAnalytics::chart.TimeSeries(abs(Residuals), colorset=colorset, xlab="",
                                ylab="Absolute residuals", lwd=lwd,
                                main=paste("Absolute residuals:",i),
                                legend.loc=NULL, pch=NULL, las=las, ...)
             }, "7L" = {
               ## SACF and PACF of residuals
               PerformanceAnalytics::chart.ACFplus(Residuals, maxlag=maxlag,
                             main=paste("SACF & PACF - Residuals:",i), ...)
             }, "8L" = {
               ## SACF and PACF of squared residuals
               PerformanceAnalytics::chart.ACFplus(Residuals^2, maxlag=maxlag,
                             main=paste("SACF & PACF - Squared residuals:",i), ...)
             }, "9L" = {
               ## SACF and PACF of absolute residuals
               PerformanceAnalytics::chart.ACFplus(abs(Residuals), maxlag=maxlag,
                             main=paste("SACF & PACF - Absolute residuals:",i), ...)
             }, "10L" = {
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
             }, "11L" = {
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
             }, "12L" = {
               ## Histogram of residuals with non-parametric density and normal overlaid
               methods <- c("add.density","add.normal","add.rug")
               PerformanceAnalytics::chart.Histogram(Residuals, xlab="Return residuals",
                               methods=methods, colorset=colorset[c(1,2,3)],
                               lwd=lwd, main=paste("Histogram of residuals:",i), ...)
               legend(x=legend.loc, col=colorset[c(2,3)], lwd=lwd, bty="n",
                      legend=c("KDE","Normal"))
               mtext(text=paste("Normal (mu=",round(mean(Residuals),4),", sd=",
                                round(resid.sd,4),")",sep=""), side=3, line=0.25, cex=0.8)
             }, "13L" = {
               ##  QQ-plot of residuals
               PerformanceAnalytics::chart.QQPlot(Residuals, envelope=0.95, col=colorset[1:2], lwd=lwd,
                            main=paste("QQ-plot of residuals:",i), ...)
               legend(x=legend.loc, col=colorset[2], lty="dashed", lwd=1, bty="n",
                      legend=c("0.95 C.Env."))
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
    if (k==1) {
      f.sub <- 1
    }
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
          menu(c("Distribution of factor returns",
                 "Factor exposures from the last period",
                 "Actual and Fitted asset returns",
                 "Time-series of R-squared values",
                 "Residual variance across assets",
                 "Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars",
                 "Factor Model Residual Correlation",
                 "Factor Model Return Correlation",
                 "Factor Contribution to SD",
                 "Factor Contribution to ES",
                 "Factor Contribution to VaR",
                 "Time series of factor returns"),
               title="\nMake a plot selection (or 0 to exit):")
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which,
             "1L" = {
               ## Distribution of factor returns
               main <- "Distribution of factor returns"
               PerformanceAnalytics::chart.Boxplot(x$factor.returns[,f.sub], colorset="black", lwd=1, main=main, xlab="Factor returns", ylab="",
                             legend.loc=legend.loc, pch=NULL, las=las, ...)
             },
             "2L" = {
               ## Factor exposures from the last period
               main <- "Factor exposures from the last period"
               C <- x$beta[a.sub,f.sub,drop=FALSE]
               Y <- row(C, as.factor=T)
               X <- as.vector(as.matrix(C[,,drop=FALSE]))
               Z <- col(C, as.factor=T)
               plot(
                 lattice::barchart(Y~X|Z, main="Factor exposures from the last period \n", xlab="", as.table=TRUE,
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
                 fitted.ret <- fitted(x)[,asset]
                 asset.ret <- subset(x$data, asset.name==asset)[,c(x$date.var,x$ret.var)]
                 asset.ret.xts <- xts::as.xts(asset.ret[,2], order.by=zoo::index(fitted.ret))
                 plotData <- merge.xts(asset.ret.xts, fitted.ret)
                 colnames(plotData) <- c("Actual","Fitted")
                 main <- paste("Actual and Fitted:", asset)
                 PerformanceAnalytics::chart.TimeSeries(plotData, colorset=colorset, lwd=lwd, main=main, xlab="",
                                  ylab="Asset returns", legend.loc=legend.loc, pch=NULL, las=las, ...)
               }
               par(mfrow=c(1,1))
             },
             "4L" ={
               ## Time-series of R-squared values
               PerformanceAnalytics::chart.TimeSeries(x$r2, main="Time-series of R-squared values", xlab="", ylab="R-squared",
                                colorset=colorset, lwd=lwd, pch=NULL, las=las, ...)
             },
             "5L" = {
               ## Residual variance across assets
               plot(
                 lattice::barchart(x$resid.var[a.sub], main="Residual variance", xlab="", col=colorset[1], ...)
               )
             },
             "6L" = {
               ## Scatterplot matrix of residuals, with histograms, density overlays, correlations and significance stars
               PerformanceAnalytics::chart.Correlation(residuals(x)[,a.sub], ...)
             },
             "7L" = {
               ## Factor model residual correlation
               cor.resid <- cor(residuals(x)[,a.sub], use="pairwise.complete.obs")
               corrplot::corrplot.mixed(cor.resid, tl.col=1, upper="ellipse", ...)
               # mtext("pairwise complete obs", line=0.5)
             },
             "8L" = {
               ## Factor model return correlation
               cor.fm <- cov2cor(fmCov(x)[a.sub,a.sub])
               corrplot::corrplot.mixed(cor.fm, tl.col=1, upper="ellipse", ...)
               # mtext("pairwise complete obs", line=0.5)
             },
             "9L" = {
               ## Factor percentage contribution to SD
               pcSd.fm <- fmSdDecomp(x)$pcSd[a.sub,c(f.sub,k+1)]
               plot(
                 lattice::barchart(pcSd.fm, main="Factor % Contribution to SD", xlab="",
                          auto.key=list(space="bottom",columns=3,points=FALSE,rectangles=TRUE),
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); panel.barchart(...)}, ...)
               )
             },
             "10L"={
               ## Factor percentage contribution to ES
               pcES.fm <- fmEsDecomp(x)$pcES[a.sub,c(f.sub,k+1)]
               plot(
                 lattice::barchart(pcES.fm, main="Factor % Contribution to ES", xlab="",
                          auto.key=list(space="bottom",columns=3,points=FALSE,rectangles=TRUE),
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); panel.barchart(...)}, ...)
               )
             },
             "11L" ={
               ## Factor percentage contribution to VaR
               pcVaR.fm <- fmVaRDecomp(x)$pcVaR[a.sub,c(f.sub,k+1)]
               plot(
                 lattice::barchart(pcVaR.fm, main="Factor % Contribution to VaR", xlab="",
                          auto.key=list(space="bottom",columns=3,points=FALSE,rectangles=TRUE),
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1);
                            panel.barchart(...)}, ...)
               )
             },
             "12L" ={
               ## Time series of factor returns
               factor.ret <- x$factor.returns[,f.sub]
               PerformanceAnalytics::chart.TimeSeries(factor.ret, main="Time-series of factor returns", xlab="", ylab="Factor returns",
                                colorset=colorset, lwd=lwd, pch=NULL, legend.loc=legend.loc, las=las, ...)
               
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