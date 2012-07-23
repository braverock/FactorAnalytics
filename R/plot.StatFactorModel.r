plot.StatFactorModel <-
function(x, variables, cumulative = TRUE, style = "bar",
         which.plot = c("none","1L","2L","3L","4L","5L","6L","7L","8L"),
         hgrid = FALSE, vgrid = FALSE,plot.single=FALSE, fundId, fundName="TBA",
         which.plot.single=c("none","1L","2L","3L","4L","5L","6L",
                             "7L","8L","9L","10L","11L","12L","13L"), ...)
{
  require(strucchange)
  #
  # beginning of funciton screenplot
  #
  screeplot<-
  function(mf, variables, cumulative = TRUE, style = "bar", main = "", ...)
  {
    vars <- mf$eigen
    n90 <- which(cumsum(vars)/sum(vars) > 0.9)[1]
    if(missing(variables)) {
      variables <- 1:max(mf$k, min(10, n90))
    }
    istyle <- charmatch(style, c("bar", "lines"), nomatch = NA)
    if(is.na(istyle) || istyle <= 1)
      style <- "bar"
    else {
      style <- "lines"
    }
    if(style == "bar") {
      loc <- barplot(vars[variables], names = paste("F", variables,
                                                    sep = "."), main = main, ylab = "Variances", ...)
    }
    else {
      loc <- 1:length(variables)
      plot(loc, vars[variables], type = "b", axes = F, main = main,
           ylab = "Variances", xlab = "")
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
    ## fundId           charater. The name of the single asset to be ploted.            
    ## fundName         TBA
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
    fit.lm = x$asset.fit[[fundId]]
    
    if (!(class(fit.lm) == "lm"))
      stop("Must pass a valid lm object")
    
    ## extract information from lm object
    
    factorNames = colnames(fit.lm$model)[-1]
    fit.formula = as.formula(paste(fundId,"~", paste(factorNames, collapse="+"), sep=" "))
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
                                "rolling estimates over 24 month window"),
                              title="\nMake a plot selection (or 0 to exit):\n")
    switch(which.plot.single,
           "1L" =  {
             ##  time series plot of actual and fitted values
             plot(actual.z, main=fundName, ylab="Monthly performance", lwd=2, col="black")
             lines(fitted.z, lwd=2, col="blue")
             abline(h=0)
             legend(x="bottomleft", legend=c("Actual", "Fitted"), lwd=2, col=c("black","blue"))
           }, 
           
           "2L" = {
             ## time series plot of residuals with standard error bands
             plot(residuals.z, main=fundName, ylab="Monthly performance", lwd=2, col="black")
             abline(h=0)
             abline(h=2*tmp.summary$sigma, lwd=2, lty="dotted", col="red")
             abline(h=-2*tmp.summary$sigma, lwd=2, lty="dotted", col="red")
             legend(x="bottomleft", legend=c("Residual", "+/ 2*SE"), lwd=2,
                    lty=c("solid","dotted"), col=c("black","red"))
           },
           "3L" = {
             ## time series plot of squared residuals
             plot(residuals.z^2, main=fundName, ylab="Squared residual", lwd=2, col="black")
             abline(h=0)
             legend(x="topleft", legend="Squared Residuals", lwd=2, col="black")
           },
           "4L" = {
             ## time series plot of absolute residuals
             plot(abs(residuals.z), main=fundName, ylab="Absolute residual", lwd=2, col="black")
             abline(h=0)
             legend(x="topleft", legend="Absolute Residuals", lwd=2, col="black")
           },
           "5L" = {
             ## SACF and PACF of residuals
             chart.ACFplus(residuals.z, main=paste("Residuals: ", fundName, sep=""))
           },
           "6L" = {
             ## SACF and PACF of squared residuals
             chart.ACFplus(residuals.z^2, main=paste("Residuals^2: ", fundName, sep=""))
           },
           "7L" = {
             ## SACF and PACF of absolute residuals
             chart.ACFplus(abs(residuals.z), main=paste("|Residuals|: ", fundName, sep=""))
           },
           "8L" = {
             ## histogram of residuals with normal curve overlayed
             chart.Histogram(residuals.z, methods="add.normal", main=paste("Residuals: ", fundName, sep=""))
           },
           "9L" = {
             ##  normal qq-plot of residuals
             chart.QQPlot(residuals.z, envelope=0.95, main=paste("Residuals: ", fundName, sep=""))
           },
           "10L"= {
             ##  CUSUM plot of recursive residuals
            
               cusum.rec = efp(fit.formula, type="Rec-CUSUM", data=fit.lm$model)
               plot(cusum.rec, sub=fundName)
             
           },
           "11L"= {
             ##  CUSUM plot of OLS residuals
                    
               cusum.ols = efp(fit.formula, type="OLS-CUSUM", data=fit.lm$model)
              
           },
           "12L"= {
             ##  CUSUM plot of recursive estimates relative to full sample estimates
                   
               cusum.est = efp(fit.formula, type="fluctuation", data=fit.lm$model)
               plot(cusum.est, functional=NULL, sub=fundName)
             
           },
           "13L"= {
             ##  rolling regression over 24 month window
            
               rollReg <- function(data.z, formula) {
                 coef(lm(formula, data = as.data.frame(data.z)))  
               }
               reg.z = zoo(fit.lm$model, as.Date(rownames(fit.lm$model)))
               rollReg.z = rollapply(reg.z, FUN=rollReg, fit.formula, width=24, by.column = FALSE, 
                                     align="right")
               plot(rollReg.z, main=paste("24-month rolling regression estimates:", fundName, sep=" "))
            
           },
           invisible()
    )
    
    
    
  } else {    
    which.plot<-which.plot[1]

  
  ##
  ## 2. Plot selected choices.
  ##
 
    
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
  ##
  ##             1. screeplot.
  ##
  if(missing(variables)) {
    vars <- x$eigen
    n90 <- which(cumsum(vars)/
      sum(vars) > 0.9)[1]
    variables <- 1:max(x$k, min(10, n90))
  }
  screeplot(x, variables, cumulative,
            style, "Screeplot")
            },
    "2L" = {
  ##
  ##             2. factor returns
  ##
  if(missing(variables)) {
    f.ret <- x$factors
        }
    plot.ts(f.ret)
  
} ,
   "3L" = {
     cov.fm<- factorModelCovariance(t(x$loadings),var(x$factors),x$residVars.vec)    
     cor.fm = cov2cor(cov.fm)
     rownames(cor.fm) = colnames(cor.fm)
     ord <- order(cor.fm[1,])
     ordered.cor.fm <- cor.fm[ord, ord]
     plotcorr(ordered.cor.fm, col=cm.colors(11)[5*ordered.cor.fm + 6])  
   },
   "4L" ={
     barplot(x$r2)
      },
    "5L" = {
     barplot(x$residVars.vec)  
     },
    "6L" = {
      cov.factors = var(x$factors)
      names = colnames(x$asset.ret)
      factor.sd.decomp.list = list()
      for (i in names) {
        factor.sd.decomp.list[[i]] =
          factorModelSdDecomposition(x$loadings[,i],
                                     cov.factors, x$residVars.vec[i])
      }
      # function to extract contribution to sd from list
      getCSD = function(x) {
        x$cr.fm
      }
      # extract contributions to SD from list
      cr.sd = sapply(factor.sd.decomp.list, getCSD)
      rownames(cr.sd) = c(colnames(x$factors), "residual")
      # create stacked barchart
      barplot(cr.sd, main="Factor Contributions to SD",
              legend.text=T, args.legend=list(x="topleft"),
              col=c(1:50) )
    } ,
    "7L" ={
      factor.es.decomp.list = list()
      names = colnames(x$asset.ret)
      for (i in names) {
        # check for missing values in fund data
        idx = which(!is.na(x$asset.ret[,i]))
        tmpData = cbind(x$asset.ret[idx,i], x$factors,
                        x$residuals[,i]/sqrt(x$residVars.vec[i]))
        colnames(tmpData)[c(1,length(tmpData[1,]))] = c(i, "residual")
        factor.es.decomp.list[[i]] = 
          factorModelEsDecomposition(tmpData, 
                                     x$loadings[,i],
                                     x$residVars.vec[i], tail.prob=0.05)
      }
         
             
             # stacked bar charts of percent contributions to ES 
             getCETL = function(x) {
               x$cES
             }
             # report as positive number
             cr.etl = sapply(factor.es.decomp.list, getCETL)
             rownames(cr.etl) = c(colnames(x$factors), "residual")
             barplot(cr.etl, main="Factor Contributions to ES",
                     legend.text=T, args.legend=list(x="topleft"),
                     col=c(1:50) )
    },
      "8L" =  {
             factor.VaR.decomp.list = list()
             names = colnames(x$asset.ret)
             for (i in names) {
               # check for missing values in fund data
               idx = which(!is.na(x$asset.ret[,i]))
               tmpData = cbind(x$asset.ret[idx,i], x$factors,
                               x$residuals[,i]/sqrt(x$residVars.vec[i]))
               colnames(tmpData)[c(1,length(tmpData[1,]))] = c(i, "residual")
               factor.VaR.decomp.list[[i]] = 
                 factorModelVaRDecomposition(tmpData, 
                                            x$loadings[,i],
                                            x$residVars.vec[i], tail.prob=0.05)
             }
             
                               
             # stacked bar charts of percent contributions to VaR
             getCVaR = function(x) {
               x$cVaR.fm
             }
             # report as positive number
             cr.var = sapply(factor.VaR.decomp.list, getCVaR)
             rownames(cr.var) = c(colnames(x$factors), "residual")
             barplot(cr.var, main="Factor Contributions to VaR",
                     legend.text=T, args.legend=list(x="topleft"),
                     col=c(1:50) )
      }
                 
     )
  invisible()
}
}