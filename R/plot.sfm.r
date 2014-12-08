#' @title Plots from a fitted statistical factor model
#' 
#' @description Generic \code{plot} method for object of class \code{sfm}. 
#' Plots chosen characteristic(s) for one or more assets. 
#' 
#' @details 
#' If the plot type argument is not specified, a menu prompts for user input 
#' and the corresponding plot is output. And, the menu is repeated for 
#' user convenience in plotting multiple characteristics. Selecting '0' from 
#' the menu exits the current \code{plot.sfm} call. Alternately, setting
#' \code{loop=FALSE} will exit after plotting any one chosen characteristic 
#' without the need for menu selection.
#' 
#' Group plots are the default. The variables in \code{asset.subset} and factors
#' in \code{asset.subset} are plotted depending on the characteristic chosen. 
#' The default is to show the first 4 factors and first 5 assets.
#' 
#' Individual asset plots are selected by specifying \code{plot.single=TRUE}. 
#' In which case, \code{asset.name} is necessary if multiple assets 
#' were modeled in \code{x}. However, if the \code{fitSfm} object contains only 
#' one asset's factor model fit, \code{plot.sfm} can infer this automatically, 
#' without user input. 
#' 
#' @param x an object of class \code{sfm} produced by \code{fitSfm}.
#' @param which.plot.group a number to indicate the type of group plot for 
#' multiple assets. If \code{NULL} (default), the following menu appears: \cr 
#' 1 = Screeplot of eigenvalues, \cr
#' 2 = Time series plot of estimated factors, \cr
#' 3 = Estimated factor loadings, \cr
#' 4 = Histogram of R-squared, \cr
#' 5 = Histogram of residual volatility,\cr
#' 6 = Factor model residual correlation \cr
#' 7 = Factor model correlation,\cr
#' 8 = Factor contribution to SD,\cr
#' 9 = Factor contribution to ES,\cr
#' 10 = Factor contribution to VaR, \cr
#' 11 = Factor mimicking portfolio weights - top long and short positions in each factor, \cr
#' 12 = Asset correlations - top long and short positions in each factor
#' @param factor.subset vector of names/indices of factors to show for group 
#' plots. Default is 1:4.
#' @param asset.subset vector of names/indices of assets to show for group 
#' plots. Default is 1:5. 
#' @param n.top scalar; number of largest and smallest weights to display for 
#' each factor mimicking portfolio. Default is 3.
#' @param plot.single logical; If \code{TRUE} plots the characteristics of an 
#' individual asset's factor model. The type of plot is given by 
#' \code{which.plot.single}. Default is \code{FALSE}.
#' @param asset.name name of the individual asset to be plotted. Is necessary 
#' if \code{x} contains multiple asset fits and \code{plot.single=TRUE}.
#' @param which.plot.single a number to indicate the type of group plot for an 
#' individual asset. If \code{NULL} (default), the following menu appears: \cr
#'  1 = Time series plot of actual and fitted asset returns,\cr
#'  2 = Time series plot of residuals with standard error bands, \cr
#'  3 = Time series plot of squared residuals, \cr
#'  4 = Time series plot of absolute residuals,\cr
#'  5 = SACF and PACF of residuals,\cr
#'  6 = SACF and PACF of squared residuals,\cr
#'  7 = SACF and PACF of absolute residuals,\cr
#'  8 = Histogram of residuals with normal curve overlayed,\cr
#'  9 = Normal qq-plot of residuals,\cr
#'  10 = CUSUM test-Recursive residuals,\cr
#'  11 = CUSUM test-OLS residuals,\cr
#'  12 = Recursive estimates (RE) test of OLS regression coefficients,\cr
#'  13 = Rolling estimates over a 24-period observation window
#' @param colorset color palette to use for all the plots. Default is 
#' \code{c(1:12)}. The 1st element will be used for individual time series 
#' plots or the 1st series plotted, the 2nd element for the 2nd object in the 
#' plot and so on.
#' @param legend.loc places a legend into one of nine locations on the chart: 
#' "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", 
#' "right", or "center". Default is "bottomright". Use \code{legend.loc=NULL} 
#' to suppress the legend.
#' @param las one of {0, 1, 2, 3} to set the direction of axis labels, same as 
#' in \code{plot}. Default here is 1.
#' @param VaR.method a method for computing VaR; one of "modified", "gaussian",
#' "historical" or "kernel". VaR is computed using 
#' \code{\link[PerformanceAnalytics]{VaR}}. Default is "historical".
#' @param eig.max scalar in (0,1] for limiting the screeplot to factors that 
#' explain a given percent of the variance. Default is 0.9.
#' @param cum.var logical; If \code{TRUE}, the cumulative fraction of the
#' variance is printed above each bar in the screeplot of eigenvalues. Default
#' is \code{TRUE}.
#' @param loop logical to indicate if the plot menu should be repeated. Default
#' is \code{TRUE}.
#' @param ... further arguments to be passed to other plotting functions.
#' 
#' @author Eric Zivot, Sangeetha Srinivasan and Yi-An Chen
#' 
#' @seealso \code{\link{fitSfm}} and \code{\link{summary.sfm}} for details
#' about the time series factor model fit, extractor functions and summary 
#' statistics.
#' 
#' \code{\link[strucchange]{efp}} for CUSUM tests.
#' 
#' \code{\link[xts]{plot.xts}}, 
#' \code{\link[PerformanceAnalytics]{chart.TimeSeries}}, 
#' \code{\link[PerformanceAnalytics]{chart.ACFplus}}, 
#' \code{\link[PerformanceAnalytics]{chart.Histogram}},
#' \code{\link[PerformanceAnalytics]{chart.QQPlot}}, 
#' \code{\link[graphics]{barplot}}, \code{\link[lattice]{barchart}} and 
#' \code{\link[corrplot]{corrplot}} for plotting methods used.
#' 
#' \code{\link{fmSdDecomp}}, \code{\link{fmEsDecomp}}, 
#' \code{\link{fmVaRDecomp}} for factor model risk measures.
#' 
#' @examples
#' 
#' # load data from the database
#' data(StockReturns)
#' 
#' # APCA with number of factors, k=15
#' fit.apca <- fitSfm(r.W, k=15, refine=TRUE)
#' 
#' # group plot(default); select type from menu prompt
#' # menu is auto-looped to get multiple types of plots based on the same fit
#' # plot(fit.apca)
#' 
#' # plot the factor betas of 1st 4 assets fitted above
#' # loop disabled to get one type of plot without interative menu
#' plot(fit.apca, asset.subset=1:4, which.plot.group=3, loop=FALSE)
#' 
#' # plot factor model return correlation; angular order of the eigenvectors
#' plot(fit.apca, which.plot.group=7, loop=FALSE, 
#'      order="AOE", method="ellipse", tl.pos = "d")
#' 
#' # histogram of residuals from an individual asset's factor model fit 
#' plot(fit.apca, plot.single=TRUE, asset.name="AFL", which.plot.single=8, 
#'      loop=FALSE)
#' 
#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus
#' chart.Histogram chart.QQPlot
#' @importFrom lattice barchart xyplot panel.barchart panel.grid
#' @importFrom corrplot corrplot
#' @importFrom strucchange efp
#' 
#' @method plot sfm
#' @export

plot.sfm <- function(x, which.plot.group=NULL, factor.subset=1:4, 
                     asset.subset=1:5, n.top=3, plot.single=FALSE, asset.name, 
                     which.plot.single=NULL, colorset=(1:12), 
                     legend.loc="topleft", las=1, VaR.method="historical", 
                     cum.var=TRUE, eig.max=0.9, loop=TRUE, ...) {
  
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
    plotData <- merge.xts(x$data[,i], fitted(x)[,i])
    colnames(plotData) <- c("Actual","Fitted")
    Residuals <- residuals(x)[,i]
    fit <- lm(x$data[,i] ~ x$factors)
    
    # plot selection
    repeat {
      if (is.null(which.plot.single)) {
        which.plot.single <- 
          menu(c("Time series plot of actual and fitted asset returns",
                 "Time series plot of residuals with standard error bands",
                 "Time series plot of squared residuals",
                 "Time series plot of absolute residuals",
                 "SACF and PACF of residuals",
                 "SACF and PACF of squared residuals",
                 "SACF and PACF of absolute residuals",
                 "Histogram of residuals with normal curve overlayed",
                 "Normal qq-plot of residuals",
                 "CUSUM test-Recursive residuals",
                 "CUSUM test-OLS residuals",
                 "Recursive estimates (RE) test of OLS regression coefficients",
                 "Rolling estimates over a 24-period observation window"),
               title="\nMake a plot selection (or 0 to exit):")
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which.plot.single,
             "1L" =  {
               ##  time series plot of actual and fitted asset returns
               chart.TimeSeries(plotData, main=paste("Returns:",i), 
                                colorset=colorset, xlab="",
                                ylab="Actual and fitted asset returns", 
                                legend.loc=legend.loc, pch=NULL, las=las, ...)
             }, "2L" = {
               ## time series plot of residuals with standard error bands
               chart.TimeSeries(Residuals, main=paste("Residuals:",i), 
                                colorset=colorset, xlab="", ylab="Residuals", 
                                lwd=2, lty="solid", las=las, ...)
               abline(h=1.96*x$resid.sd[i], lwd=2, lty="dotted", col="red")
               abline(h=-1.96*x$resid.sd[i], lwd=2, lty="dotted", col="red")
               legend(x=legend.loc, lty=c("solid","dotted"), 
                      col=c(colorset[1],"red"), lwd=2, 
                      legend=c("Residuals",expression("\u00b1 1.96"*sigma)))
             }, "3L" = {
               ## time series plot of squared residuals
               chart.TimeSeries(Residuals^2, colorset=colorset, xlab="", 
                                ylab=" Squared Residuals",
                                main=paste("Squared Residuals:",i), 
                                legend.loc=legend.loc, pch=NULL, las=las, ...)
             }, "4L" = {
               ## time series plot of absolute residuals
               chart.TimeSeries(abs(Residuals), colorset=colorset, xlab="", 
                                ylab="Absolute Residuals",
                                main=paste("Absolute Residuals:",i), 
                                legend.loc=legend.loc, pch=NULL, las=las, ...)
             }, "5L" = {
               ## SACF and PACF of residuals
               chart.ACFplus(Residuals, col=colorset[1],
                             main=paste("SACF & PACF - Residuals:",i), ...)
             }, "6L" = {
               ## SACF and PACF of squared residuals
               chart.ACFplus(Residuals^2, col=colorset[1], ...,
                             main=paste("SACF & PACF - Squared residuals:",i))
             }, "7L" = {
               ## SACF and PACF of absolute residuals
               chart.ACFplus(abs(Residuals), col=colorset[1], ...,
                             main=paste("SACF & PACF - Absolute Residuals:",i))
             }, "8L" = {
               ## histogram of residuals with normal curve overlayed
               methods <- c("add.density","add.normal","add.rug")
               chart.Histogram(Residuals, xlab="Return residuals",
                               methods=methods, colorset=colorset, 
                               main=paste("Histogram of Residuals:",i), ...)
             }, "9L" = {
               ##  normal qq-plot of residuals
               chart.QQPlot(Residuals, envelope=0.95, col=colorset,
                            main=paste("QQ-plot of Residuals:",i), ...)
               legend(x=legend.loc, col="red", lty="dotted", lwd=1,
                      legend=c("0.95 confidence envelope"))
             }, "10L" = {
               ##  Recursive CUSUM test
               cusum.rec <- efp(formula(fit), type="Rec-CUSUM", data=fit$model)
               plot(cusum.rec, main=paste("Recursive CUSUM test:",i), las=las, 
                    col=colorset, ...)
             }, "11L" = {
               ##  OLS-based CUSUM test
               cusum.ols <- efp(formula(fit), type="OLS-CUSUM", data=fit$model)
               plot(cusum.ols, main=paste("OLS-based CUSUM test:",i), las=las, 
                    col=colorset, ...)
             }, "12L" = {
               ##  Recursive estimates (RE) test of OLS regression coefficients      
               cusum.est <- efp(formula(fit), type="RE", data=fit$model)
               plot(cusum.est, functional=NULL, col=colorset, las=0,
                    main=paste("RE test (Recursive estimates test):",i), ...)
             }, "13L" = {
               ##  Rolling estimates over 24-period observation window 
               rollReg <- function(data.z, formula) {
                 coef(lm(formula, data=as.data.frame(data.z)))  
               }
               reg.z <- zoo(fit$model, as.Date(rownames(fit$model)))
               rollReg.z <- rollapply(reg.z, FUN=rollReg, formula(fit), 
                                      width=24, by.column=FALSE, align="right")
               par(las=0)
               plot(rollReg.z, ..., las=las,
                    main=paste("Rolling estimates (24-period obs window):",i))
               par(las=las)
             }, 
             invisible()
      )
      # repeat menu if user didn't choose to exit from the plot options
      if (which.plot.single==0 || loop==FALSE) {break} 
      else {which.plot.single=NULL}
    } 
  } else { # start of group asset plots
    
    # plot selection
    repeat {
      if (is.null(which.plot.group)) {
        which.plot.group <- 
          menu(c("Screeplot of eigenvalues",
                 "Time series plot of estimated factors",
                 "Estimated factor loadings", 
                 "Histogram of R-squared", 
                 "Histogram of residual volatility", 
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
      k <- x$k
      f.names <- paste("F", 1:k, sep = ".")
      a.names <- x$asset.names
      n <- nrow(x$loadings)
      
      if (!(all(factor.subset %in% f.names) || all(factor.subset %in% 1:k))) {
        stop("Invalid argument: factor.subset is not a valid subset of factor names in the fit object.") 
      }
      if (!(all(asset.subset %in% a.names) || all(asset.subset %in% 1:n))) {
        stop("Invalid argument: factor.subset is not a valid subset of factor names in the fit object.") 
      }
      
      switch(which.plot.group,
             "1L" = { 
               ## Screeplot of eigenvalues
               cumv <- cumsum(x$eigen)/sum(x$eigen)
               limit <- length(cumv[cumv<eig.max]) + 1
               eig <- x$eigen[1:limit]
               scree <- barplot(eig, main="Screeplot of eigenvalues", 
                                ylab="Variance", col="darkblue", 
                                ylim=c(0, 1.1*max(eig)), las=las, ...)
               if (cum.var) {
                 text(scree, eig, label=round(cumv[1:limit],3), pos=3, cex=0.75)
               }
             }, 
             "2L" = {
               ## Time series plot of estimated factors
               plot(
                 xyplot(x$factors[,factor.subset], type=c("l","g"), xlab="", 
                        scales=list(y=list(rot=0)), strip.left=TRUE, strip=FALSE, ...)
               )
             }, 
             "3L" = {    
               ## Estimated factor loadings
               par(mfrow=c(ceiling(length(factor.subset)/2),2))
               for (i in factor.subset) {
                 main=paste("Beta values for ", colnames(x$loadings)[i])
                 barplot(x$loadings[asset.subset,i], main=main, 
                         names.arg=rownames(x$loadings)[asset.subset], 
                         col="darkblue", las=las, horiz=TRUE, ...)
                 abline(v=0, lwd=1, lty=1, col=1)
               }
               par(mfrow=c(1,1))
             }, 
             "4L" = {
               ## Histogram of R-squared
               methods <- c("add.density","add.rug")
               chart.Histogram(x$r2, xlab="R-squared",
                               methods=methods, colorset=colorset, 
                               main=paste("Histogram of R-squared values"), ...)
             }, 
             "5L" = {
               ## Histogram of Residual Volatility
               methods <- c("add.density","add.rug")
               chart.Histogram(x$resid.sd, xlab="Residual volatility",
                               methods=methods, colorset=colorset, 
                               main=paste("Histogram of Residual volatilities"), ...)
             }, 
             "6L" = {
               ## Factor Model Residual Correlation
               cor.resid <- cor(residuals(x)[,asset.subset], use="pairwise.complete.obs")
               corrplot(cor.resid, ...)
               # mtext("pairwise complete obs", line=0.5)
             }, 
             "7L" = {
               ## Factor Model Return Correlation
               cor.fm <- cov2cor(fmCov(x))[asset.subset,asset.subset]
               corrplot(cor.fm, ...)
               # mtext("pairwise complete obs", line=0.5)
             },
             "8L" = {
               ## Factor Percentage Contribution to SD
               pcSd.fm <- fmSdDecomp(x)$pcSd[asset.subset,factor.subset]
               plot(
                 barchart(pcSd.fm, main="Factor % Contribution to SD", xlab="",
                          auto.key=list(space="bottom",columns=3, 
                                        points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "9L" = {
               ## Factor Percentage Contribution to ES
               pcES.fm <- fmEsDecomp(x, method=VaR.method)$pcES[asset.subset,factor.subset]
               plot(
                 barchart(pcES.fm, main="Factor % Contribution to ES", xlab="",
                          auto.key=list(space="bottom",columns=3, 
                                        points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "10L" = {
               ## Factor Percentage Contribution to VaR
               pcVaR.fm <- fmVaRDecomp(x, method=VaR.method)$pcVaR[asset.subset,factor.subset]
               plot(
                 barchart(pcVaR.fm, main="Factor % Contribution to VaR", 
                          xlab="", auto.key=list(space="bottom",columns=3, 
                                                 points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "11L" = {
               ## Factor mimicking portfolio weights - top long and short positions in each factor
               par(mfrow=c(ceiling(length(factor.subset)/2),2))
               for (i in factor.subset) {
                 main=paste("Top positions (%) in ", colnames(x$loadings)[i])
                 s <- summary(x, n.top=n.top)$mimic.sum[[i]]
                 top <- 100*stack(s[,c(2,4)])$values
                 names.arg <- stack(s[,c(1,3)])$values
                 barplot(top, main=main, names.arg=names.arg, col="darkblue", 
                         las=las, horiz=TRUE, ...)
                 abline(v=0, lwd=1, lty=1, col=1)
               }
               par(mfrow=c(1,1))
             },
             "12L" = {
               ## Asset correlations - top long and short positions in each factor
               for (i in factor.subset) {
                 main=paste("Correlations of top positions in ", colnames(x$loadings)[i])
                 s <- summary(x, n.top=n.top)$mimic.sum[[i]]
                 names.arg <- stack(s[,c(1,3)])$values
                 cor.fm <- cov2cor(fmCov(x))[names.arg,names.arg]
                 corrplot(cor.fm, ...)
               }
             },
             invisible()       
      )         
      # repeat menu if user didn't choose to exit from the plot options
      if (which.plot.group==0 || loop==FALSE) {break} 
      else {which.plot.group=NULL}  
    }
  } # end of group plots
}
