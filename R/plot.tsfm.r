#' @title Plots from a fitted time series factor model
#' 
#' @description Generic \code{plot} method for object of class \code{tsfm}. 
#' Plots chosen characteristic(s) for one or more assets. 
#' 
#' @details 
#' If the plot type argument is not specified, a menu prompts for user input 
#' and the corresponding plot is output. And, the menu is repeated for 
#' user convenience in plotting multiple characteristics. Selecting '0' from 
#' the menu exits the current \code{plot.tsfm} call. Alternately, setting
#' \code{loop=FALSE} will exit after plotting any one chosen characteristic 
#' without the need for menu selection.
#' 
#' For group plots (the default), the first \code{max.show} assets are plotted.
#' For individual plots, \code{asset.name} is necessary if multiple assets 
#' were modeled in \code{x} and \code{plot.single=TRUE}. However, if the 
#' \code{fitTsfm} object contains only one asset's factor model fit, 
#' \code{plot.tsfm} can infer this automatically, without user input. 
#' 
#' CUSUM plots (individual asset plot options 10, 11 and 12) are applicable 
#' only for \code{fit.method="OLS"}.
#' 
#' Rolling estimates (individual asset plot option 13) is not applicable for 
#' \code{variable.slection="lars"}.
#' 
#' @param x an object of class \code{tsfm} produced by \code{fitTsfm}.
#' @param which.plot.group a number to indicate the type of group plot for 
#' multiple assets. If \code{NULL} (default), the following menu appears: \cr 
#' 1 = Factor model coefficients: Alpha, \cr
#' 2 = Factor model coefficients: Betas, \cr
#' 3 = Actual and Fitted asset returns, \cr
#' 4 = R-squared, \cr
#' 5 = Residual Volatility,\cr
#' 6 = Factor Model Residual Correlation \cr
#' 7 = Factor Model Correlation,\cr
#' 8 = Factor Contribution to SD,\cr
#' 9 = Factor Contribution to ES,\cr
#' 10 = Factor Contribution to VaR
#' @param max.show maximum number of assets in a given plot. Default is 6.
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
#' @param loop logical to indicate if the plot menu should be repeated. Default
#' is \code{TRUE}.
#' @param ... further arguments to be passed to other plotting functions.
#' 
#' @author Eric Zivot, Sangeetha Srinivasan and Yi-An Chen
#' 
#' @seealso \code{\link{fitTsfm}} and \code{\link{summary.tsfm}} for details
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
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:8)]),
#'                      rf.name="US.3m.TR", data=managers)
#'     
#' # group plot(default); select type from menu prompt
#' # menu is auto-looped to get multiple types of plots based on the same fit
#' # plot(fit.macro)
#'                
#' # plot the factor betas of 1st 4 assets fitted above
#' # loop disabled to get one type of plot without interative menu
#' plot(fit.macro, max.show=4, which.plot.group=2, loop=FALSE)
#' 
#' # plot factor model return correlation; angular order of the eigenvectors
#' plot(fit.macro, which.plot.group=7, loop=FALSE, 
#'      order="AOE", method="ellipse", tl.pos = "d")
#' 
#' # histogram of residuals from an individual asset's factor model fit 
#' plot(fit.macro, plot.single=TRUE, asset.name="HAM1", which.plot.single=8, 
#'      loop=FALSE)
#' 
#' @importFrom PerformanceAnalytics chart.TimeSeries chart.ACFplus
#' chart.Histogram chart.QQPlot
#' @importFrom lattice barchart panel.barchart panel.grid
#' @importFrom corrplot corrplot
#' @importFrom strucchange efp
#' @importFrom robust lmRob
#' 
#' @method plot tsfm
#' @export

plot.tsfm <- function(x, which.plot.group=NULL, max.show=6, plot.single=FALSE, 
                      asset.name, which.plot.single=NULL, colorset=(1:12), 
                      legend.loc="topleft", las=1, VaR.method="historical", 
                      loop=TRUE, ...) {
  
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
    plotData <- merge.xts(x$data[,i], fitted(x)[,i])
    colnames(plotData) <- c("Actual","Fitted")
    Residuals <- residuals(x)[,i]
    fit <- x$asset.fit[[i]]
    
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
               if (!x$fit.method=="OLS") {
                 stop("CUSUM analysis applicable only for 'OLS' fit.method.")
               }
               cusum.rec <- efp(formula(fit), type="Rec-CUSUM", data=fit$model)
               plot(cusum.rec, main=paste("Recursive CUSUM test:",i), las=las, 
                    col=colorset, ...)
             }, "11L" = {
               ##  OLS-based CUSUM test
               if (!x$fit.method=="OLS") {
                 stop("CUSUM analysis applicable only for 'OLS' fit.method.")
               }
               cusum.ols <- efp(formula(fit), type="OLS-CUSUM", data=fit$model)
               plot(cusum.ols, main=paste("OLS-based CUSUM test:",i), las=las, 
                    col=colorset, ...)
             }, "12L" = {
               ##  Recursive estimates (RE) test of OLS regression coefficients
               if (!x$fit.method=="OLS") {
                 stop("CUSUM analysis applicable only for 'OLS' fit.method.")
               }        
               cusum.est <- efp(formula(fit), type="RE", data=fit$model)
               plot(cusum.est, functional=NULL, col=colorset, las=0,
                    main=paste("RE test (Recursive estimates test):",i), ...)
             }, "13L" = {
               ##  Rolling estimates over 24-period observation window 
               if (x$fit.method=="OLS") {
                 rollReg <- function(data.z, formula) {
                   coef(lm(formula, data=as.data.frame(data.z)))  
                 }
                 reg.z <- zoo(fit$model, as.Date(rownames(fit$model)))
                 rollReg.z <- rollapply(reg.z, FUN=rollReg, formula(fit), 
                                        width=24, by.column=FALSE, align="right")
               } else if (x$fit.method=="DLS") {
                 # get decay factor
                 if (as.character(x$call["decay"])=="NULL") {
                   decay <- 0.95 # default value for the decay factor
                 } else {
                   decay <- as.numeric(as.character(x$call["decay"]))
                 }
                 # calculate exp. decaying weights for 24-period window
                 w <- decay^seq(23,0,-1)
                 w <- w/sum(w) # weights sum to unity
                 rollReg.w <- function(data.z, formula, w) {
                   coef(lm(formula, weights=w, data=as.data.frame(data.z)))  
                 }
                 reg.z <- zoo(fit$model[-length(fit$model)], 
                              as.Date(rownames(fit$model)))
                 rollReg.z <- rollapply(reg.z, FUN=rollReg.w, formula(fit), w, 
                                        width=24, by.column=FALSE, align="right")
               } else if (x$fit.method=="Robust") {
                 rollReg.Rob <- function(data.z, formula) {
                   coef(lmRob(formula=formula, data=as.data.frame(data.z)))  
                 }
                 reg.z <- zoo(fit$model, as.Date(rownames(fit$model)))
                 rollReg.z <- rollapply(reg.z, width=24, FUN=rollReg.Rob, 
                                        formula(fit), by.column=FALSE, 
                                        align="right")
               } else if (is.null(x$fit.method)) {
                 stop("Rolling estimates is not available for 'lars' fits.")
               }
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
          menu(c("Factor model coefficients: Alpha",
                 "Factor model coefficients: Betas",
                 "Actual and Fitted asset returns", 
                 "R-squared", 
                 "Residual Volatility", 
                 "Factor Model Residual Correlation",
                 "Factor Model Return Correlation",
                 "Factor Contribution to SD", 
                 "Factor Contribution to ES", 
                 "Factor Contribution to VaR"), 
               title="\nMake a plot selection (or 0 to exit):") 
      }
      
      par(las=las) # default horizontal axis labels
      
      switch(which.plot.group,
             "1L" = { 
               ## Factor model coefficients: Alpha
               barplot(coef(x)[,1], main="Factor model Alpha (Intercept)", 
                       names.arg=rownames(coef(x)), col="darkblue", las=las, 
                       horiz=TRUE, ...)
               abline(v=0, lwd=1, lty=1, col=1)
             }, 
             "2L" = {
               ## Factor model coefficients: Betas
               k <- ncol(coef(x))-1
               if (k > max.show) {
                 cat(paste("Displaying only the first", max.show,"factor betas, as the number of factors > 'max.show' =", max.show))
                 k <- max.show 
               }
               par(mfrow=c(ceiling(k/2),2))
               for (i in 2:(k+1)) {
                 main=paste(colnames(coef(x))[i], "factor Betas")
                 barplot(coef(x)[,i], main=main, names.arg=rownames(coef(x)), 
                         col="darkblue", las=las, horiz=TRUE, ...)
                 abline(v=0, lwd=1, lty=1, col=1)
               }
               par(mfrow=c(1,1))
             }, 
             "3L" = {    
               ## Actual and Fitted asset returns
               n <- length(x$asset.names)
               if (n > max.show) {
                 cat(paste("Displaying only the first", max.show, "assets, since the number of assets > 'max.show'"))
                 n <- max.show 
               }
               par(mfrow=c(ceiling(n/2),2))
               for (i in 1:n) {
                 plotData <- merge.xts(x$data[,i], fitted(x)[,i])
                 colnames(plotData) <- c("Actual","Fitted")
                 main <- paste("Factor model asset returns:", x$asset.names[i])
                 chart.TimeSeries(plotData, colorset=colorset, main=main, 
                                  xlab="", ylab="Actual and fitted values", 
                                  legend.loc=legend.loc, pch=NULL, las=las,...)
               }
               par(mfrow=c(1,1))
             }, 
             "4L" ={
               ## R-squared
               plot(
                 barchart(x$r2, main="R-squared values", xlab="", 
                          col="darkblue", ...)
               )
             }, 
             "5L" = {
               ## Residual Volatility
               plot(
                 barchart(x$resid.sd, main="Residual volatility", xlab="", 
                          col="darkblue", ...)
               )
             }, 
             "6L" = {
               ## Factor Model Residual Correlation
               cor.resid <- cor(residuals(x), use="pairwise.complete.obs")
               corrplot(cor.resid, ...)
               # mtext("pairwise complete obs", line=0.5)
             }, 
             "7L" = {
               ## Factor Model Return Correlation
               cor.fm <- cov2cor(fmCov(x)) 
               corrplot(cor.fm, ...)
               # mtext("pairwise complete obs", line=0.5)
             },
             "8L" = {
               ## Factor Percentage Contribution to SD
               pcSd.fm <- fmSdDecomp(x)$pcSd
               plot(
                 barchart(pcSd.fm, main="Factor % Contribution to SD", xlab="",
                          auto.key=list(space="bottom",columns=3, 
                                        points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "9L"={
               ## Factor Percentage Contribution to ES
               pcES.fm <- fmEsDecomp(x, method=VaR.method)$pcES
               plot(
                 barchart(pcES.fm, main="Factor % Contribution to ES", xlab="",
                          auto.key=list(space="bottom",columns=3, 
                                        points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             "10L" ={
               ## Factor Percentage Contribution to VaR
               pcVaR.fm <- fmVaRDecomp(x, method=VaR.method)$pcVaR
               plot(
                 barchart(pcVaR.fm, main="Factor % Contribution to VaR", 
                          xlab="", auto.key=list(space="bottom",columns=3, 
                                        points=FALSE,rectangles=TRUE), 
                          par.settings=list(superpose.polygon=list(col=colorset)),
                          panel=function(...){panel.grid(h=0, v=-1); 
                                              panel.barchart(...)}, ...)
               )
             },
             invisible()       
      )         
      # repeat menu if user didn't choose to exit from the plot options
      if (which.plot.group==0 || loop==FALSE) {break} 
      else {which.plot.group=NULL}  
    }
  } # end of group plots
}
