#' @title Plots from a fitted time series factor model
#' 
#' @description S3 \code{plot} method for object of class \code{tsfm}. Plots 
#' selected characteristics for one or more assets. 
#' 
#' @param x an object of class \code{tsfm} produced by \code{fitTSFM}.
#' @param colorset a vector of colors for the bars or bar components. Argument 
#' is used by \code{\link[graphics]{barplot}}. Default is c(1:12).
#' @param legend.loc places a legend into one of nine locations on the chart: 
#' bottomright, bottom, bottomleft, left, topleft, top, topright, right, or 
#' center. Argument is used by 
#' \code{\link[PerformanceAnalytics]{chart.TimeSeries}}. Default is \code{NULL}.
#' @param which.plot a number or "none" to indicate which type of group plot to 
#' create for multiple assets. Default is "none"; which brings up the following 
#' menu to select a type. \cr 
#' 1 = "Fitted asset returns", \cr
#' 2 = "R-squared", \cr
#' 3 = "Residual Volatility",\cr
#' 4 = "FM Correlation",\cr
#' 5 = "Factors' Contribution to SD",\cr
#' 6 = "Factors' Contribution to ES",\cr
#' 7 = "Factors' Contribution to VaR"
#' @param max.show maximum number of assets in a plot. Default is 6.
#' @param plot.single a logical value. If \code{TRUE}, plots an individual 
#' asset's linear factor model trait selected by \code{which.plot.single}. 
#' Default is \code{FALSE}.
#' @param asset.name name of the individual asset to be plotted. Is necessary 
#' if \code{plot.single=TRUE}
#' @param which.plot.single a number or "none" to indicate which type of group 
#' plot to create for multiple assets. Default is "none"; which brings up the 
#' following menu to select a type.\cr
#'  1 = time series plot of actual and fitted factor returns,\cr
#'  2 = time series plot of residuals with standard error bands, \cr
#'  3 = time series plot of squared residuals, \cr
#'  4 = time series plot of absolute residuals,\cr
#'  5 = SACF and PACF of residuals,\cr
#'  6 = SACF and PACF of squared residuals,\cr
#'  7 = SACF and PACF of absolute residuals,\cr
#'  8 = histogram of residuals with normal curve overlayed,\cr
#'  9 = normal qq-plot of residuals,\cr
#'  10= CUSUM plot of recursive residuals,\cr
#'  11= CUSUM plot of OLS residuals,\cr
#'  12= CUSUM plot of recursive estimates relative to full sample estimates,\cr
#'  13= rolling estimates over a 24-period observation window
#' @param VaR.method a method for computing VaR; one of "modified", "gaussian",
#' "historical" or "kernel". VaR is computed using 
#' \code{\link[PerformanceAnalytics]{VaR}}. Default is "historical".
#' @param ... further arguments passed to or from other methods.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTSFM}}, \code{\link{summary.tsfm}}
#' 
#' @examples
#' 
#' \dontrun{
#' # load data from the database
#' data(managers.df)
#' fit.macro <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                      factor.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                      data=managers.df,fit.method="OLS")
#' # plot all assets and show only the first 4 assets.
#' plot(fit.macro,max.show=4)
#' # plot of an individual asset, "HAM1" 
#' plot(fit.macro, plot.single=TRUE, asset.name="HAM1")
#' }
#' 
#' @method plot tsfm
#' @export

plot.tsfm <- 
  function(x, colorset=c(1:12), legend.loc=NULL,
           which.plot=c("none","1L","2L","3L","4L","5L","6L","7L"), max.show=6,
           plot.single=FALSE, asset.name, 
           which.plot.single=c("none","1L","2L","3L","4L","5L","6L","7L","8L",
                               "9L","10L","11L","12L","13L"),
           VaR.method = "historical", ...){
    
    if (plot.single==TRUE) {
      ## inputs:
      ## fit.macro        lm object summarizing factor model fit. It is assumed that
      ##                  time series date information is included in the names component
      ##                  of the residuals, fitted and model components of the object.   
      ## asset.name         charater. The name of the single asset to be ploted.
      ## which.plot.single       integer indicating which plot to create:
      ##                  1     time series plot of actual and fitted values
      ##                  2     time series plot of residuals with standard error bands
      ##                  3     time series plot of squared residuals
      ##                  4     time series plot of absolute residuals
      ##                  5     SACF and PACF of residuals
      ##                  6     SACF and PACF of squared residuals
      ##                  7     SACF and PACF of absolute residuals
      ##                  8     histogram of residuals with normal curve overlayed
      ##                  9     normal qq-plot of residuals
      ##                  10    CUSUM plot of recursive residuals
      ##                  11    CUSUM plot of OLS residuals
      ##                  12    CUSUM plot of recursive estimates relative to full sample estimates
      ##                  13    rolling estimates over a 24-period observation window
      which.plot.single<-which.plot.single[1]
      if (missing(asset.name) == TRUE) {
        stop("Neet to specify an asset to plot if plot.single is TRUE.")
      }
      
      fit.lm = x$asset.fit[[asset.name]]
      
      if (x$variable.selection == "none") {
        
        ## extract information from lm object
        
        factorNames = colnames(fit.lm$model)[-1]
        fit.formula = as.formula(paste(asset.name,"~", paste(factorNames, collapse="+"), sep=" "))
        residuals.z = zoo(residuals(fit.lm), as.Date(names(residuals(fit.lm))))
        fitted.z = zoo(fitted(fit.lm), as.Date(names(fitted(fit.lm))))
        actual.z = zoo(fit.lm$model[,1], as.Date(rownames(fit.lm$model)))
        tmp.summary = summary(fit.lm)
        
        
        if (which.plot.single=="none")
          which.plot.single<-menu(c("time series plot of actual and fitted values",
                                    "time series plot of residuals with standard error bands",
                                    "time series plot of squared residuals",
                                    "time series plot of absolute residuals",
                                    "SACF and PACF of residuals",
                                    "SACF and PACF of squared residuals",
                                    "SACF and PACF of absolute residuals",
                                    "histogram of residuals with normal curve overlayed",
                                    "normal qq-plot of residuals",
                                    "CUSUM plot of recursive residuals",
                                    "CUSUM plot of OLS residuals",
                                    "CUSUM plot of recursive estimates relative to full sample estimates",
                                    "rolling estimates over a 24-period observation window"),
                                  title="\nMake a plot selection (or 0 to exit):\n")
        switch(which.plot.single,
               "1L" =  {
                 ##  time series plot of actual and fitted values
                 plot(actual.z, main=asset.name, ylab="Monthly performance", lwd=2, col="black")
                 lines(fitted.z, lwd=2, col="blue")
                 abline(h=0)
                 legend(x="bottomleft", legend=c("Actual", "Fitted"), lwd=2, col=c("black","blue"))
               }, 
               
               "2L" = {
                 ## time series plot of residuals with standard error bands
                 plot(residuals.z, main=asset.name, ylab="Monthly performance", lwd=2, col="black")
                 abline(h=0)
                 abline(h=2*tmp.summary$sigma, lwd=2, lty="dotted", col="red")
                 abline(h=-2*tmp.summary$sigma, lwd=2, lty="dotted", col="red")
                 legend(x="bottomleft", legend=c("Residual", "+/ 2*SE"), lwd=2,
                        lty=c("solid","dotted"), col=c("black","red"))
               },
               "3L" = {
                 ## time series plot of squared residuals
                 plot(residuals.z^2, main=asset.name, ylab="Squared residual", lwd=2, col="black")
                 abline(h=0)
                 legend(x="topleft", legend="Squared Residuals", lwd=2, col="black")
               },
               "4L" = {
                 ## time series plot of absolute residuals
                 plot(abs(residuals.z), main=asset.name, ylab="Absolute residual", lwd=2, col="black")
                 abline(h=0)
                 legend(x="topleft", legend="Absolute Residuals", lwd=2, col="black")
               },
               "5L" = {
                 ## SACF and PACF of residuals
                 chart.ACFplus(residuals.z, main=paste("Residuals: ", asset.name, sep=""))
               },
               "6L" = {
                 ## SACF and PACF of squared residuals
                 chart.ACFplus(residuals.z^2, main=paste("Residuals^2: ", asset.name, sep=""))
               },
               "7L" = {
                 ## SACF and PACF of absolute residuals
                 chart.ACFplus(abs(residuals.z), main=paste("|Residuals|: ", asset.name, sep=""))
               },
               "8L" = {
                 ## histogram of residuals with normal curve overlayed
                 chart.Histogram(residuals.z, methods="add.normal", main=paste("Residuals: ", asset.name, sep=""))
               },
               "9L" = {
                 ##  normal qq-plot of residuals
                 chart.QQPlot(residuals.z, envelope=0.95, main=paste("Residuals: ", asset.name, sep=""))
               },
               "10L"= {
                 ##  CUSUM plot of recursive residuals
                 if (as.character(x$call["fit.method"]) == "OLS") {
                   cusum.rec = efp(fit.formula, type="Rec-CUSUM", data=fit.lm$model)
                   plot(cusum.rec, sub=asset.name)
                 } else 
                   stop("CUMSUM applies only on OLS method")
               },
               "11L"= {
                 ##  CUSUM plot of OLS residuals
                 if (as.character(x$call["fit.method"]) == "OLS") {        
                   cusum.ols = efp(fit.formula, type="OLS-CUSUM", data=fit.lm$model)
                   plot(cusum.ols, sub=asset.name)
                 } else 
                   stop("CUMSUM applies only on OLS method")   
               },
               "12L"= {
                 ##  CUSUM plot of recursive estimates relative to full sample estimates
                 if (as.character(x$call["fit.method"]) == "OLS") {        
                   cusum.est = efp(fit.formula, type="fluctuation", data=fit.lm$model)
                   plot(cusum.est, functional=NULL, sub=asset.name)
                 } else 
                   stop("CUMSUM applies only on OLS method")
               },
               "13L"= {
                 ##  Rolling estimates over 24-period observation window 
                 if (as.character(x$call["fit.method"]) == "OLS") {   
                   rollReg <- function(data.z, formula) {
                     coef(lm(formula, data = as.data.frame(data.z)))  
                   }
                   reg.z = zoo(fit.lm$model, as.Date(rownames(fit.lm$model)))
                   rollReg.z = rollapply(reg.z, FUN=rollReg, fit.formula, width=24, by.column = FALSE, 
                                         align="right")
                   plot(rollReg.z, main=paste("Rolling estimates over 24-period observation window:", asset.name, sep=" "))
                 } else if (as.character(x$call["fit.method"]) == "DLS") {
                   decay.factor <- as.numeric(as.character(x$call["decay.factor"]))
                   t.length <- 24
                   w <- rep(decay.factor^(t.length-1),t.length)
                   for (k in 2:t.length) {
                     w[k] = w[k-1]/decay.factor 
                   }   
                   w <- w/sum(w)
                   rollReg.w <- function(data.z, formula,w) {
                     coef(lm(formula,weights=w, data = as.data.frame(data.z)))  
                   }
                   reg.z = zoo(fit.lm$model[-length(fit.lm$model)], as.Date(rownames(fit.lm$model)))
                   factorNames = colnames(fit.lm$model)[c(-1,-length(fit.lm$model))]
                   fit.formula = as.formula(paste(asset.name,"~", paste(factorNames, collapse="+"), sep=" "))
                   rollReg.z = rollapply(reg.z, FUN=rollReg.w, fit.formula,w, width=24, by.column = FALSE, 
                                         align="right")
                   plot(rollReg.z, main=paste("Rolling estimates over 24-period observation window:", asset.name, sep=" ")) 
                 } 
               },
               invisible()
        )
        
      } else {
        # lar or lasso
        
        factor.names  = x$factors.names
        plot.data   = x$data[,c(asset.name,factor.names)]
        alpha = x$alpha[asset.name]
        beta = as.matrix(x$beta[asset.name,])        
        fitted.z = zoo(alpha+as.matrix(plot.data[,factor.names])%*%beta,as.Date(rownames(plot.data)))
        residuals.z = plot.data[,asset.name]-fitted.z
        actual.z = zoo(plot.data[,asset.name],as.Date(rownames(plot.data)))       
        t = length(residuals.z)
        k = length(factor.names)
        
        which.plot.single<-menu(c("time series plot of actual and fitted values",
                                  "time series plot of residuals with standard error bands",
                                  "time series plot of squared residuals",
                                  "time series plot of absolute residuals",
                                  "SACF and PACF of residuals",
                                  "SACF and PACF of squared residuals",
                                  "SACF and PACF of absolute residuals",
                                  "histogram of residuals with normal curve overlayed",
                                  "normal qq-plot of residuals"),
                                title="\nMake a plot selection (or 0 to exit):\n")
        switch(which.plot.single,
               "1L" =  {
                 #       "time series plot of actual and fitted values",
                 
                 plot(actual.z[,asset.name], main=asset.name, ylab="Monthly performance", lwd=2, col="black")
                 lines(fitted.z, lwd=2, col="blue")
                 abline(h=0)
                 legend(x="bottomleft", legend=c("Actual", "Fitted"), lwd=2, col=c("black","blue"))
               },
               "2L"={    
                 #       "time series plot of residuals with standard error bands"
                 plot(residuals.z, main=asset.name, ylab="Monthly performance", lwd=2, col="black")
                 abline(h=0)
                 sigma = (sum(residuals.z^2)*(t-k)^-1)^(1/2)
                 abline(h=2*sigma, lwd=2, lty="dotted", col="red")
                 abline(h=-2*sigma, lwd=2, lty="dotted", col="red")
                 legend(x="bottomleft", legend=c("Residual", "+/ 2*SE"), lwd=2,
                        lty=c("solid","dotted"), col=c("black","red"))     
                 
               },   
               "3L"={
                 #       "time series plot of squared residuals"
                 plot(residuals.z^2, main=asset.name, ylab="Squared residual", lwd=2, col="black")
                 abline(h=0)
                 legend(x="topleft", legend="Squared Residuals", lwd=2, col="black")   
               },                
               "4L" = {
                 ## time series plot of absolute residuals
                 plot(abs(residuals.z), main=asset.name, ylab="Absolute residual", lwd=2, col="black")
                 abline(h=0)
                 legend(x="topleft", legend="Absolute Residuals", lwd=2, col="black")
               },
               "5L" = {
                 ## SACF and PACF of residuals
                 chart.ACFplus(residuals.z, main=paste("Residuals: ", asset.name, sep=""))
               },
               "6L" = {
                 ## SACF and PACF of squared residuals
                 chart.ACFplus(residuals.z^2, main=paste("Residuals^2: ", asset.name, sep=""))
               },
               "7L" = {
                 ## SACF and PACF of absolute residuals
                 chart.ACFplus(abs(residuals.z), main=paste("|Residuals|: ", asset.name, sep=""))
               },
               "8L" = {
                 ## histogram of residuals with normal curve overlayed
                 chart.Histogram(residuals.z, methods="add.normal", main=paste("Residuals: ", asset.name, sep=""))
               },
               "9L" = {
                 ##  normal qq-plot of residuals
                 chart.QQPlot(residuals.z, envelope=0.95, main=paste("Residuals: ", asset.name, sep=""))
               },          
               invisible()  )
        
      }  
      # plot group data      
    } else {    
      which.plot<-which.plot[1]
      
      if(which.plot=='none') 
        which.plot<-menu(c("Fitted asset returns",
                           "R-squared",
                           "Residual Volatility",
                           "FM Correlation",
                           "Factors' Contribution to SD",
                           "Factors' Contribution to ES",
                           "Factors' Contribution to VaR"),
                         title="Factor Analytics Plot \nMake a plot selection (or 0 to exit):\n") 
      
      
      variable.selection = x$variable.selection
      asset.names = x$assets.names
      factor.names  = x$factors.names
      plot.data   = x$data[,c(asset.names,factor.names)]
      cov.factors = var(plot.data[,factor.names])
      n <- length(asset.names)
      
      switch(which.plot,
             
             "1L" = {
               if (n > max.show) {
                 cat(paste("numbers of assets are greater than",max.show,", show only first",
                           max.show,"assets",sep=" "))
                 n <- max.show 
               }      
               par(mfrow=c(n/2,2))
               if (variable.selection == "lar" || variable.selection == "lasso") {
                 for (i in 1:n) {
                   alpha = x$alpha[i]
                   beta = as.matrix(x$beta[i,])        
                   fitted = alpha+as.matrix(plot.data[,factor.names])%*%beta  
                   dataToPlot = cbind(fitted, plot.data[,i])
                   colnames(dataToPlot) = c("Fitted","Actual")
                   main = paste("Factor Model fit for",asset.names[i],seq="")
                   chart.TimeSeries(dataToPlot,colorset = colorset, legend.loc = legend.loc,main=main)
                 }
               } else {
                 for (i in 1:n) {
                   dataToPlot = cbind(fitted(x$asset.fit[[i]]), na.omit(plot.data[,i]))
                   colnames(dataToPlot) = c("Fitted","Actual")
                   main = paste("Factor Model fit for",asset.names[i],seq="")
                   chart.TimeSeries(dataToPlot,colorset = colorset, legend.loc = legend.loc,main=main)
                 }
               }
               par(mfrow=c(1,1))
             },
             "2L" ={
               barplot(x$r2)
             },
             "3L" = {
               barplot(x$resid.sd)  
             },    
             
             "4L" = {
               cov.fm<- factorModelCovariance(x$beta,cov.factors,x$resid.variance)    
               cor.fm = cov2cor(cov.fm)
               rownames(cor.fm) = colnames(cor.fm)
               ord <- order(cor.fm[1,])
               ordered.cor.fm <- cor.fm[ord, ord]
               plotcorr(ordered.cor.fm, col=cm.colors(11)[5*ordered.cor.fm + 6])
             },
             "5L" = {
               factor.sd.decomp.list = list()
               for (i in asset.names) {
                 factor.sd.decomp.list[[i]] =
                   factorModelSdDecomposition(x$beta[i,],
                                              cov.factors, x$resid.variance[i])
               }
               # function to extract contribution to sd from list
               getCSD = function(x) {
                 x$cSd.fm
               }
               # extract contributions to SD from list
               cr.sd = sapply(factor.sd.decomp.list, getCSD)
               rownames(cr.sd) = c(factor.names, "residual")
               # create stacked barchart
               barplot(cr.sd, main="Factors' Contribution to SD",
                       legend.text=T, args.legend=list(x="topleft"))
               
             },
             "6L"={
               factor.es.decomp.list = list()
               if (variable.selection == "lar" || variable.selection == "lasso") {
                 
                 for (i in asset.names) {
                   idx = which(!is.na(plot.data[,i]))
                   alpha = x$alpha[i]
                   beta = as.matrix(x$beta[i,])        
                   fitted = alpha+as.matrix(plot.data[,factor.names])%*%beta
                   residual = plot.data[,i]-fitted
                   tmpData = cbind(coredata(plot.data[idx,i]),
                                   coredata(plot.data[idx,factor.names]),
                                   (residual[idx,]/sqrt(x$resid.variance[i])) )
                   colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
                   factor.es.decomp.list[[i]] = 
                     factorModelEsDecomposition(tmpData, 
                                                x$beta[i,],
                                                x$resid.variance[i], tail.prob=0.05)
                   
                 }
               } else {
                 
                 for (i in asset.names) {
                   # check for missing values in fund data
                   idx = which(!is.na(plot.data[,i]))
                   tmpData = cbind(coredata(plot.data[idx,i]),
                                   coredata(plot.data[idx,factor.names]),
                                   residuals(x$asset.fit[[i]])/sqrt(x$resid.variance[i]))
                   colnames(tmpData)[c(1,dim(tmpData)[2])] = c(i, "residual")
                   factor.es.decomp.list[[i]] = 
                     factorModelEsDecomposition(tmpData, 
                                                x$beta[i,],
                                                x$resid.variance[i], tail.prob=0.05,
                                                VaR.method=VaR.method)
                 }
               }     
               
               # stacked bar charts of percent contributions to SD
               getCETL = function(x) {
                 x$cES.fm
               }
               # report as positive number
               cr.etl = sapply(factor.es.decomp.list, getCETL)
               rownames(cr.etl) = c(factor.names, "residual")
               barplot(cr.etl, main="Factors' Contribution to ES",
                       legend.text=T, args.legend=list(x="topleft")) 
             },
             "7L" ={
               
               factor.VaR.decomp.list = list()
               
               if (variable.selection == "lar" || variable.selection == "lasso") {
                 
                 for (i in asset.names) {
                   idx = which(!is.na(plot.data[,i]))
                   alpha = x$alpha[i]
                   beta = as.matrix(x$beta[i,])        
                   fitted = alpha+as.matrix(plot.data[,factor.names])%*%beta
                   residual = plot.data[,i]-fitted
                   tmpData = cbind(coredata(plot.data[idx,i]),
                                   coredata(plot.data[idx,factor.names]),
                                   (residual[idx,]/sqrt(x$resid.variance[i])) )
                   colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
                   factor.VaR.decomp.list[[i]] = 
                     factorModelVaRDecomposition(tmpData, 
                                                 x$beta[i,],
                                                 x$resid.variance[i], tail.prob=0.05,VaR.method=VaR.method)
                   
                 }
               } else {
                 for (i in asset.names) {
                   # check for missing values in fund data
                   idx = which(!is.na(plot.data[,i]))
                   tmpData = cbind(coredata(plot.data[idx,i]),
                                   coredata(plot.data[idx,factor.names]),
                                   residuals(x$asset.fit[[i]])/sqrt(x$resid.variance[i]))
                   colnames(tmpData)[c(1,dim(tmpData)[2])] = c(i, "residual")
                   factor.VaR.decomp.list[[i]] = 
                     factorModelVaRDecomposition(tmpData, 
                                                 x$beta[i,],
                                                 x$resid.variance[i], tail.prob=0.05,
                                                 VaR.method=VaR.method)
                 }
               }
               
               # stacked bar charts of percent contributions to SD
               getCVaR = function(x) {
                 x$cVaR.fm
               }
               # report as positive number
               cr.VaR = sapply(factor.VaR.decomp.list, getCVaR)
               rownames(cr.VaR) = c(factor.names, "residual")
               barplot(cr.VaR, main="Factors' Contribution to VaR",
                       legend.text=T, args.legend=list(x="topleft"))
             },
             invisible()       
      )         
    }       
    
  }
