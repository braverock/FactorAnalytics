#' plot StatFactorModel object.
#' 
#' Generic function of plot method for fitStatisticFactorModel. Either plot all
#' fit models or choose a single asset to plot.
#' 
#' PCA works well. APCA is underconstruction.
#' 
#' @param x fit object created by fitStatisticalFactorModel.
#' @param variables Optional. an integer vector telling which variables are to
#' be plotted. The default is to plot all the variables, or the number of
#' variables explaining 90 percent of the variance, whichever is bigger.
#' @param cumulative a logical flag: if TRUE, the cumulative fraction of the
#' variance is printed above each bar in the plot.
#' @param style Charater. bar or lines can be chosen.
#' @param which.plot integer indicating which plot to create: "none" will
#' create a menu to choose. Defualt is none. 1 = "Screeplot of Eigenvalues", 2
#' = "Factor returns", 3 = "FM Correlation", 4 = "R square", 5 = "Variance of
#' Residuals", 6 = "Factor Contributions to SD", 7 = "Factor Contributions to
#' ES", 8 = "Factor Contributions to VaR"
#' @param hgrid Logic. Whether to plot horizontal grid or not. Defualt is
#' FALSE.
#' @param vgrid Logic. Whether to plot vertical grid or not. Defualt is FALSE.
#' @param plot.single Plot a single asset of lm class. Defualt is FALSE.
#' @param asset.name Name of the asset to be plotted.
#' @param which.plot.single integer indicating which plot to create: "none"
#' will create a menu to choose. Defualt is none. 1 = time series plot of
#' actual and fitted values 2 = time series plot of residuals with standard
#' error bands 3 = time series plot of squared residuals 4 = time series plot
#' of absolute residuals 5 = SACF and PACF of residuals 6 = SACF and PACF of
#' squared residuals 7 = SACF and PACF of absolute residuals 8 = histogram of
#' residuals with normal curve overlayed 9 = normal qq-plot of residuals 10=
#' CUSUM plot of recursive residuals 11= CUSUM plot of OLS residuals 12= CUSUM
#' plot of recursive estimates relative to full sample estimates 13= rolling
#' estimates over 24 month window
#' @param max.show  Maximum assets to plot. Default is 6.
#' @param VaR.method haracter, method for computing VaR. Valid choices are
#' one of "modified","gaussian","historical", "kernel". computation is done with the \code{VaR}
#' in the PerformanceAnalytics package. Default is "historical".
#' @param ...  other variables for barplot method.
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' \dontrun{
#' # load data for fitStatisticalFactorModel.r
#' # data from finmetric berndt.dat and folio.dat
#' 
#' data(stat.fm.data)
#' # pca
#' sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=10)
#' args(plot.StatFactorModel)
#' # plot all
#' plot(sfm.pca.fit)
#' # plot single asset
#' plot(sfm.pca.fit,plot.single=TRUE,asset.name="CITCRP")
#' }
#' @method plot StatFactorModel
#' @export
plot.StatFactorModel <-
  function(x, variables, cumulative = TRUE, style = "bar",
           which.plot = c("none","1L","2L","3L","4L","5L","6L","7L","8L"),
           hgrid = FALSE, vgrid = FALSE,plot.single=FALSE, asset.name,
           which.plot.single=c("none","1L","2L","3L","4L","5L","6L",
                               "7L","8L","9L","10L","11L","12L","13L"),
           max.show=6, VaR.method = "historical",...)
  {
    require(strucchange)
    require(ellipse)
    #
    # beginning of funciton screenplot
    #
    screeplot<-
      function(mf, variables, cumulative = TRUE, style = "bar", main = "", ...)
      {
        vars <- mf$eigen
        if(missing(variables)) {
          variables <- 1:mf$k
        }
        istyle <- charmatch(style, c("bar", "lines"), nomatch = NA)
        if(is.na(istyle) || istyle <= 1)
          style <- "bar"
        else {
          style <- "lines"
        }
        if(style == "bar") {
          loc <- barplot(vars[variables]/sum(vars),
                         names = paste("F", variables,sep = "."),
                         main = main, ylab = "Percentage of Variances", ...)
        }
        else {
          loc <- 1:length(variables)
          plot(loc, vars[variables]/sum(vars), type = "b", axes = F, main = main,
               ylab = "Percentage of Variances", xlab = "")
          axis(2)
          axis(1, at = loc, labels = paste("F", variables, sep = "."))
        }
        if(cumulative) {
          cumv <- (cumsum(vars)/sum(vars))[variables]
          text(loc, vars[variables] + par("cxy")[2], as.character(signif(
            cumv, 3)))
        }
        invisible(loc)
      }
    #
    # end of screenplot
    #
    
    if (plot.single==TRUE) {
      ## inputs:
      ## x               lm object summarizing factor model fit. It is assumed that
      ##                  time series date information is included in the names component
      ##                  of the residuals, fitted and model components of the object.
      ## asset.name           charater. The name of the single asset to be ploted. 
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
      ##                  13    rolling estimates over 24 month window
      which.plot.single<-which.plot.single[1]
      
      
      
      
      if (which.plot.single=="none")
        
        
        # pca method
        
        if ( dim(x$asset.ret)[1] > dim(x$asset.ret)[2] ) {
          
          
          fit.lm = x$asset.fit[[asset.name]]
          
          
          ## exact information from lm object
          
          factorNames = colnames(fit.lm$model)[-1]
          fit.formula = as.formula(paste(asset.name,"~", paste(factorNames, collapse="+"), sep=" "))
          #Date = try(as.Date(names(residuals(fit.lm))))
          #Date = try(as.yearmon(names(residuals(fit.lm)),"%b %Y"))  
          residuals.z = zoo(residuals(fit.lm), as.Date(names(residuals(fit.lm)))) 
          fitted.z = zoo(fitted(fit.lm), as.Date(names(fitted(fit.lm))))
          actual.z = zoo(fit.lm$model[,1], as.Date(rownames(fit.lm$model)))
          tmp.summary = summary(fit.lm)
          
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
                                    "rolling estimates over 24 month window"),
                                  title="\nMake a plot selection (or 0 to exit):\n")
          
          switch(which.plot.single,
                 "1L" =  {
                   ##  time series plot of actual and fitted values
                   plot(actual.z, main=asset.name, ylab="Monthly performance", lwd=2, col="black")
                   lines(fitted.z, lwd=2, col="red")
                   abline(h=0)
                   legend(x="bottomleft", legend=c("Actual", "Fitted"), lwd=2, col=c("black","red"))
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
                   
                   cusum.rec = efp(fit.formula, type="Rec-CUSUM", data=fit.lm$model)
                   plot(cusum.rec, sub=asset.name)
                   
                 },
                 "11L"= {
                   ##  CUSUM plot of OLS residuals
                   
                   cusum.ols = efp(fit.formula, type="OLS-CUSUM", data=fit.lm$model)
                   
                 },
                 "12L"= {
                   ##  CUSUM plot of recursive estimates relative to full sample estimates
                   
                   cusum.est = efp(fit.formula, type="fluctuation", data=fit.lm$model)
                   plot(cusum.est, functional=NULL, sub=asset.name)
                   
                 },
                 "13L"= {
                   ##  rolling regression over 24 month window
                   
                   rollReg <- function(data.z, formula) {
                     coef(lm(formula, data = as.data.frame(data.z)))  
                   }
                   reg.z = zoo(fit.lm$model, as.Date(rownames(fit.lm$model)))
                   rollReg.z = rollapply(reg.z, FUN=rollReg, fit.formula, width=24, by.column = FALSE, 
                                         align="right")
                   plot(rollReg.z, main=paste("24-month rolling regression estimates:", asset.name, sep=" "))
                   
                 },
                 invisible()
          )
        } else {  #apca method
          
          dates <- names(x$data[,asset.name]) 
          actual.z <- zoo(x$asset.ret[,asset.name],as.Date(dates))
          residuals.z <- zoo(x$residuals,as.Date(dates))
          fitted.z <- actual.z - residuals.z
          t <- length(dates)
          k <- x$k
          
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
                   lines(fitted.z[,asset.name], lwd=2, col="red")
                   abline(h=0)
                   legend(x="bottomleft", legend=c("Actual", "Fitted"), lwd=2, col=c("black","red"))
                 },
                 "2L"={    
                   #       "time series plot of residuals with standard error bands"
                   plot(residuals.z[,asset.name], main=asset.name, ylab="Monthly performance", lwd=2, col="black")
                   abline(h=0)
                   sigma = (sum(residuals.z[,asset.name]^2)*(t-k)^-1)^(1/2)
                   abline(h=2*sigma, lwd=2, lty="dotted", col="red")
                   abline(h=-2*sigma, lwd=2, lty="dotted", col="red")
                   legend(x="bottomleft", legend=c("Residual", "+/ 2*SE"), lwd=2,
                          lty=c("solid","dotted"), col=c("black","red"))     
                   
                 },   
                 "3L"={
                   #       "time series plot of squared residuals"
                   plot(residuals.z[,asset.name]^2, main=asset.name, ylab="Squared residual", lwd=2, col="black")
                   abline(h=0)
                   legend(x="topleft", legend="Squared Residuals", lwd=2, col="black")   
                 },                
                 "4L" = {
                   ## time series plot of absolute residuals
                   plot(abs(residuals.z[,asset.name]), main=asset.name, ylab="Absolute residual", lwd=2, col="black")
                   abline(h=0)
                   legend(x="topleft", legend="Absolute Residuals", lwd=2, col="black")
                 },
                 "5L" = {
                   ## SACF and PACF of residuals
                   chart.ACFplus(residuals.z[,asset.name], main=paste("Residuals: ", asset.name, sep=""))
                 },
                 "6L" = {
                   ## SACF and PACF of squared residuals
                   chart.ACFplus(residuals.z[,asset.name]^2, main=paste("Residuals^2: ", asset.name, sep=""))
                 },
                 "7L" = {
                   ## SACF and PACF of absolute residuals
                   chart.ACFplus(abs(residuals.z[,asset.name]), main=paste("|Residuals|: ", asset.name, sep=""))
                 },
                 "8L" = {
                   ## histogram of residuals with normal curve overlayed
                   chart.Histogram(residuals.z[,asset.name], methods="add.normal", main=paste("Residuals: ", asset.name, sep=""))
                 },
                 "9L" = {
                   ##  normal qq-plot of residuals
                   chart.QQPlot(residuals.z[,asset.name], envelope=0.95, main=paste("Residuals: ", asset.name, sep=""))
                 },          
                 invisible()  )
        }  
      
      
    } else {    
      which.plot<-which.plot[1]
      
      
      ##
      ## 2. Plot selected choices.
      ##
      
      
      if(which.plot=='none')   
        which.plot <- menu(c("Screeplot of Eigenvalues",
                             "Factor Returns",
                             "FM Correlation",
                             "R square",
                             "Variance of Residuals",
                             "Factor Contributions to SD",
                             "Factor Contributions to ES",
                             "Factor Contributions to VaR"), title = 
                             "\nMake a plot selection (or 0 to exit):\n")
      
      switch(which.plot,
             "1L" =    {
               ## 1. screeplot.
                 if(missing(variables)) {
                 vars <- x$eigen
                 variables <- 1:x$k
               }
               screeplot(x, variables, cumulative,
                         style, "Screeplot of Eigenvalues")
             },
             "2L" = {
               ##
               ##             2. factor returns
               ##
               if(missing(variables)) {
                 f.ret <- x$factors
               }
               plot.zoo(f.ret)
               
             } ,
             "3L" = {
               cov.fm<- factorModelCovariance(t(x$loadings),var(x$factors),
                                              x$resid.variance)    
               cor.fm = cov2cor(cov.fm)
               rownames(cor.fm) = colnames(cor.fm)
               ord <- order(cor.fm[1,])
               ordered.cor.fm <- cor.fm[ord, ord]
               plotcorr(ordered.cor.fm[(1:max.show),(1:max.show)], col=cm.colors(11)[5*ordered.cor.fm + 6])  
             },
             "4L" ={
               barplot(x$r2[1:max.show])
             },
             "5L" = {
               barplot(x$resid.variance[1:max.show])  
             },
             "6L" = {
               cov.factors = var(x$factors)
               names = colnames(x$asset.ret)
               factor.sd.decomp.list = list()
               for (i in names) {
                 factor.sd.decomp.list[[i]] =
                   factorModelSdDecomposition(x$loadings[,i],
                                              cov.factors, x$resid.variance[i])
               }
               # function to extract component contribution to sd from list
               getCSD = function(x) {
                 x$cr.fm
               }
               # extract contributions to SD from list
               cr.sd = sapply(factor.sd.decomp.list, getCSD)
               rownames(cr.sd) = c(colnames(x$factors), "residual")
               # create stacked barchart
               barplot(cr.sd[,(1:max.show)], main="Factor Contributions to SD",...)
#                        legend.text=T, args.legend=list(x="topright"))
             } ,
             "7L" ={
               factor.es.decomp.list = list()
               names = colnames(x$asset.ret)
               for (i in names) {
                 # check for missing values in fund data
                 idx = which(!is.na(x$asset.ret[,i]))
                 tmpData = cbind(x$asset.ret[idx,i], x$factors,
                                 x$residuals[,i]/sqrt(x$resid.variance[i]))
                 colnames(tmpData)[c(1,length(tmpData[1,]))] = c(i, "residual")
                 factor.es.decomp.list[[i]] = 
                   factorModelEsDecomposition(tmpData, 
                                              x$loadings[,i],
                                              x$resid.variance[i], tail.prob=0.05,VaR.method=VaR.method)
               }
               
               
               # stacked bar charts of component contributions to ES 
               getCETL = function(x) {
                 x$cES
               }
               # report as positive number
               cr.etl = sapply(factor.es.decomp.list, getCETL)
               rownames(cr.etl) = c(colnames(x$factors), "residual")
               barplot(cr.etl[,(1:max.show)], main="Factor Contributions to ES",...)
#                        legend.text=T, args.legend=list(x="topright") )
             },
             "8L" =  {
               factor.VaR.decomp.list = list()
               names = colnames(x$asset.ret)
               for (i in names) {
                 # check for missing values in fund data
                 idx = which(!is.na(x$asset.ret[,i]))
                 tmpData = cbind(x$asset.ret[idx,i], x$factors,
                                 x$residuals[,i]/sqrt(x$resid.variance[i]))
                 colnames(tmpData)[c(1,length(tmpData[1,]))] = c(i, "residual")
                 factor.VaR.decomp.list[[i]] = 
                   factorModelVaRDecomposition(tmpData, 
                                               x$loadings[,i],
                                               x$resid.variance[i], tail.prob=0.05,VaR.method=VaR.method)
               }
               
               
               # stacked bar charts of component contributions to VaR
               getCVaR = function(x) {
                 x$cVaR.fm
               }
               # report as positive number
               cr.var = sapply(factor.VaR.decomp.list, getCVaR)
               rownames(cr.var) = c(colnames(x$factors), "residual")
               barplot(cr.var[,(1:max.show)], main="Factor Contributions to VaR",...)
#                        legend.text=T, args.legend=list(x="topright"))
             }, invisible()
             
      )
      
    }
    
  }
