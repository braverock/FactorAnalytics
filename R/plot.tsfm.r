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
#' \code{loop=FALSE} will exit after plotting any one chosen characteristic.
#' 
#' For group plots (the default), the first \code{max.show} assets are plotted.
#' For individual plots, \code{asset.name} is necessary if multiple assets 
#' were modeled in \code{x} and \code{plot.single=TRUE}. However, if the 
#' \code{fitTsfm} object \code{x} only contains one asset's factor model fit, 
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
#' @param plot.single a logical value. \code{TRUE} plots the characteristics of
#' an individual asset's factor model. The type of plot is given by 
#' \code{which.plot.single}. Default is \code{FALSE}.
#' @param asset.name name of the individual asset to be plotted. Is necessary 
#' if multiple assets factor model fits exist in \code{x} and 
#' \code{plot.single=TRUE}.
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
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan
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
#' \code{\link[graphics]{barplot}} and 
#' \code{\link[ellipse]{plotcorr}} for plotting methods used.
#' 
#' \code{\link{factorModelSDDecomposition}}, 
#' \code{\link{factorModelEsDecomposition}},
#' \code{\link{factorModelVaRDecomposition}} for factor model risk measures.
#' 
#' @examples
#' 
#' \dontrun{
#' # load data from the database
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:8)]),
#'                      rf.name="US 3m TR", data=managers)
#' # plot the 1st 4 assets fitted above.
#' plot(fit.macro, max.show=4)
#' # plot of an individual asset, "HAM1" 
#' plot(fit.macro, plot.single=TRUE, asset.name="HAM1")
#' }
#' 
#' @method plot tsfm
#' @export

plot.tsfm <- function(x, which.plot.group=NULL, max.show=6, plot.single=FALSE, 
                      asset.name, which.plot.single=NULL, colorset=(1:12), 
                      legend.loc="bottomright", las=1, 
                      VaR.method="historical", loop=TRUE, ...) {
  
  if (plot.single==TRUE) {
    
    if (missing(asset.name) && length(x$asset.names)>1) {
      stop("Missing input: 'asset.name' is required if plot.single is TRUE and 
           multiple assets factor model fits exist in 'x'.")   
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
    par(las=las) # default horizontal axis labels
    
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
               if(!exists("lwd")) {lwd=2} 
               if(!exists("lty")) {lty="solid"} 
               chart.TimeSeries(Residuals, main=paste("Residuals:",i), lty=lty,
                                colorset=colorset, xlab="", 
                                ylab="Residuals", lwd=lwd, las=las, ...)
               abline(h=1.96*x$resid.sd[i], lwd=lwd, lty="dotted", col="red")
               abline(h=-1.96*x$resid.sd[i], lwd=lwd, lty="dotted", col="red")
               legend(x=legend.loc, lty=c(lty,"dotted"), 
                      col=c(colorset[1],"red"), lwd=lwd, 
                      legend=c("Residuals",expression("\u00b1 1.96"*sigma)))
             }, "3L" = {
               ## time series plot of squared residuals
               if (!is.null(legend.loc)) {legend.loc="topright"}
               chart.TimeSeries(Residuals^2, colorset=colorset, xlab="", 
                                ylab=" Squared Residuals",
                                main=paste("Squared Residuals:",i), 
                                legend.loc=legend.loc, pch=NULL, las=las, ...)
             }, "4L" = {
               ## time series plot of absolute residuals
               if (!is.null(legend.loc)) {legend.loc="topright"}
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
               if(!exists("methods")) {
                 methods=c("add.density","add.normal","add.rug","add.risk")
               } 
               chart.Histogram(Residuals, methods=methods,
                               main=paste("Histogram of Residuals:",i), 
                               xlab="Return residuals", colorset=colorset, ...)
             }, "9L" = {
               ##  normal qq-plot of residuals
               if(!exists("envelope")) {envelope=0.95} 
               chart.QQPlot(Residuals, envelope=envelope, col=colorset,
                            main=paste("QQ-plot of Residuals:",i), ...)
             }, "10L" = {
               ##  Recursive CUSUM test
               if (!x$fit.method=="OLS") {
                 stop("CUSUM analysis applicable only for 'OLS' fit.method.")
               }
               cusum.rec = efp(formula(fit), type="Rec-CUSUM", data=fit$model)
               plot(cusum.rec, main=paste("Recursive CUSUM test:",i), las=las, 
                    col=colorset, ...)
             }, "11L" = {
               ##  OLS-based CUSUM test
               if (!x$fit.method=="OLS") {
                 stop("CUSUM analysis applicable only for 'OLS' fit.method.")
               }
               cusum.ols = efp(formula(fit), type="OLS-CUSUM", data=fit$model)
               plot(cusum.ols, main=paste("OLS-based CUSUM test:",i), las=las, 
                    col=colorset, ...)
             }, "12L" = {
               ##  Recursive estimates (RE) test of OLS regression coefficients
               if (!x$fit.method=="OLS") {
                 stop("CUSUM analysis applicable only for 'OLS' fit.method.")
               }        
               cusum.est = efp(formula(fit), type="RE", data=fit$model)
               plot(cusum.est, functional=NULL, col=colorset, las=0,
                    main=paste("RE test (Recursive estimates test):",i), ...)
             }, "13L" = {
               ##  Rolling estimates over 24-period observation window 
               if (x$fit.method=="OLS") {
                 rollReg <- function(data.z, formula) {
                   coef(lm(formula, data=as.data.frame(data.z)))  
                 }
                 reg.z = zoo(fit$model, as.Date(rownames(fit$model)))
                 rollReg.z = rollapply(reg.z, FUN=rollReg, formula(fit), 
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
                 reg.z = zoo(fit$model[-length(fit$model)], 
                             as.Date(rownames(fit$model)))
                 rollReg.z = rollapply(reg.z, FUN=rollReg.w, formula(fit), w, 
                                       width=24, by.column=FALSE, align="right")
               } else if (x$fit.method=="Robust") {
                 rollReg.Rob <- function(data.z, formula) {
                   coef(lmRob(formula=formula, data=as.data.frame(data.z)))  
                 }
                 reg.z = zoo(fit$model, as.Date(rownames(fit$model)))
                 rollReg.z = rollapply(reg.z, width=24, FUN=rollReg.Rob, 
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
    
    # extract info from the fitTsfm object
    n <- length(x$asset.names)
    if (n > max.show) {
      cat(paste("Displaying only the first", max.show,"assets, since the 
                  number of assets > 'max.show' =", max.show))
      n <- max.show 
    }
    
    # plot selection
    repeat {
      if (is.null(which.plot.single)) {
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
               #  ylab="Intercept estimate"
               barplot(coef(x)[,1], main="Factor model Alpha", 
                       xlab="Assets", col="darkblue", las=las, ...)
               abline(h=0, lwd=1, lty=1, col=1)
               
             }, "2L" = {
               ## Factor model coefficients: Betas
               k <- ncol(coef(x))-1
               if (k > max.show) {
                 cat(paste("Displaying only the first", max.show,"factor betas, 
                           as the number of factors > 'max.show' =", max.show))
                 k <- max.show 
               }
               par(mfrow=c(k/2,2))
               for (i in 2:k+1) {
                 main=paste("Factor Betas:", colnames(coef(x))[i])
                 barplot(coef(x)[,i], main=main, col="darkblue", xlab="Assets",
                         ylab="Coefficient estimate", las=las, ...)
                 abline(h=0, lwd=1, lty=1, col=1)
               }
               par(mfrow=c(1,1))
             }, "3L" = {    
               ## Actual and Fitted asset returns
               par(mfrow=c(n,1))
               for (i in 1:n) {
                 plotData <- merge.xts(x$data[,i], fitted(x)[,i])
                 colnames(plotData) <- c("Actual","Fitted")
                 main = paste("Factor model asset returns:", x$asset.names[i])
                 chart.TimeSeries(plotData, colorset=colorset, main=main, 
                                  xlab="", ylab="Actual and fitted values", 
                                  legend.loc=legend.loc, pch=NULL, las=las,...)
               }
               par(mfrow=c(1,1))
             },
             "4L" ={
               ## R-squared
               barplot(x$r2, main="R-squared values for factor model fits", 
                       xlab="Assets", ylab="R-squared", col="darkblue", 
                       las=las, ...)
               abline(h=0, lwd=1, lty=1, col=1)
             },
             "5L" = {
               ## Residual Volatility
               barplot(x$resid.sd, xlab="Assets", ylab="Residual volatility",
                       main="Residual volatility for factor model fits", 
                       col="darkblue", las=las, ...) 
               abline(h=0, lwd=1, lty=1, col=1)
             },    
             "6L" = {
               ## Factor Model Residual Correlation
               cor.resid <- cor(residuals(x),use="pairwise.complete.obs")
               if(!exists("order")) {order="AOE"}
               corrplot::corrplot(cor.resid, order=order, ...)
             },
             "7L" = {
               ## Factor Model Return Correlation
               cov.fm<- covFm(x)    
               cor.fm = cov2cor(cov.fm) 
               if(!exists("order")) {order="AOE"}
               corrplot::corrplot(cor.fm, order=order, ...)
             },
#              "8L" = {
#                ## Factor Contribution to SD
#                factor.sd.decomp.list = list()
#                for (i in asset.names) {
#                  factor.sd.decomp.list[[i]] =
#                    factorModelSdDecomposition(x$beta[i,],
#                                               cov.factors, x$resid.variance[i])
#                }
#                # function to extract contribution to sd from list
#                getCSD = function(x) {
#                  x$cSd.fm
#                }
#                # extract contributions to SD from list
#                cr.sd = sapply(factor.sd.decomp.list, getCSD)
#                rownames(cr.sd) = c(factor.names, "residual")
#                # create stacked barchart
#                barplot(cr.sd, main="Factors' Contribution to SD",
#                        legend.text=T, args.legend=list(x="topleft"))
#                
#              },
#              "9L"={
#                ## Factor Contribution to ES
#                factor.es.decomp.list = list()
#                if (variable.selection == "lar" || variable.selection == "lasso") {
#                  
#                  for (i in asset.names) {
#                    idx = which(!is.na(plot.data[,i]))
#                    alpha = x$alpha[i]
#                    beta = as.matrix(x$beta[i,])        
#                    fitted = alpha+as.matrix(plot.data[,factor.names])%*%beta
#                    residual = plot.data[,i]-fitted
#                    tmpData = cbind(coredata(plot.data[idx,i]),
#                                    coredata(plot.data[idx,factor.names]),
#                                    (residual[idx,]/sqrt(x$resid.variance[i])) )
#                    colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
#                    factor.es.decomp.list[[i]] = 
#                      factorModelEsDecomposition(tmpData, 
#                                                 x$beta[i,],
#                                                 x$resid.variance[i], tail.prob=0.05)
#                    
#                  }
#                } else {
#                  
#                  for (i in asset.names) {
#                    # check for missing values in fund data
#                    idx = which(!is.na(plot.data[,i]))
#                    tmpData = cbind(coredata(plot.data[idx,i]),
#                                    coredata(plot.data[idx,factor.names]),
#                                    residuals(x$asset.fit[[i]])/sqrt(x$resid.variance[i]))
#                    colnames(tmpData)[c(1,dim(tmpData)[2])] = c(i, "residual")
#                    factor.es.decomp.list[[i]] = 
#                      factorModelEsDecomposition(tmpData, 
#                                                 x$beta[i,],
#                                                 x$resid.variance[i], tail.prob=0.05,
#                                                 VaR.method=VaR.method)
#                  }
#                }     
#                
#                # stacked bar charts of percent contributions to SD
#                getCETL = function(x) {
#                  x$cES.fm
#                }
#                # report as positive number
#                cr.etl = sapply(factor.es.decomp.list, getCETL)
#                rownames(cr.etl) = c(factor.names, "residual")
#                barplot(cr.etl, main="Factors' Contribution to ES",
#                        legend.text=T, args.legend=list(x="topleft")) 
#              },
#              "10L" ={
#                ## Factor Contribution to VaR
#                factor.VaR.decomp.list = list()
#                
#                if (variable.selection == "lar" || variable.selection == "lasso") {
#                  
#                  for (i in asset.names) {
#                    idx = which(!is.na(plot.data[,i]))
#                    alpha = x$alpha[i]
#                    beta = as.matrix(x$beta[i,])        
#                    fitted = alpha+as.matrix(plot.data[,factor.names])%*%beta
#                    residual = plot.data[,i]-fitted
#                    tmpData = cbind(coredata(plot.data[idx,i]),
#                                    coredata(plot.data[idx,factor.names]),
#                                    (residual[idx,]/sqrt(x$resid.variance[i])) )
#                    colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
#                    factor.VaR.decomp.list[[i]] = 
#                      factorModelVaRDecomposition(tmpData, 
#                                                  x$beta[i,],
#                                                  x$resid.variance[i], tail.prob=0.05,VaR.method=VaR.method)
#                    
#                  }
#                } else {
#                  for (i in asset.names) {
#                    # check for missing values in fund data
#                    idx = which(!is.na(plot.data[,i]))
#                    tmpData = cbind(coredata(plot.data[idx,i]),
#                                    coredata(plot.data[idx,factor.names]),
#                                    residuals(x$asset.fit[[i]])/sqrt(x$resid.variance[i]))
#                    colnames(tmpData)[c(1,dim(tmpData)[2])] = c(i, "residual")
#                    factor.VaR.decomp.list[[i]] = 
#                      factorModelVaRDecomposition(tmpData, 
#                                                  x$beta[i,],
#                                                  x$resid.variance[i], tail.prob=0.05,
#                                                  VaR.method=VaR.method)
#                  }
#                }
#                
#                # stacked bar charts of percent contributions to SD
#                getCVaR = function(x) {
#                  x$cVaR.fm
#                }
#                # report as positive number
#                cr.VaR = sapply(factor.VaR.decomp.list, getCVaR)
#                rownames(cr.VaR) = c(factor.names, "residual")
#                barplot(cr.VaR, main="Factors' Contribution to VaR",
#                        legend.text=T, args.legend=list(x="topleft"))
#              },
             invisible()       
      )         
      # repeat menu if user didn't choose to exit from the plot options
      if (which.plot.group==0 || loop==FALSE) {break} 
      else {which.plot.group=NULL}  
    }
  } # end of group plots
}


#   plot.data <- x$data[,c(asset.names,factor.names)]
#   cov.factors <- var(plot.data[,factor.names])
