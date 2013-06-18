#' plot MacrofactorModel object.
#' 
#' Generic function of plot method for fitMacroeconomicFactorModel. Either plot
#' all fit models or choose a single asset to plot.
#' 
#' 
#' @param fit.macro fit object created by fitMacroeconomicFactorModel.
#' @param colorset Defualt colorset is c(1:12).
#' @param legend.loc plot legend or not. Defualt is \code{NULL}.
#' @param which.plot integer indicating which plot to create: "none" will
#' create a menu to choose. Defualt is none. 1 = "Fitted factor returns", 2 =
#' "R square", 3 = "Variance of Residuals", 4 = "FM Correlation", 5 = "Factor
#' Contributions to SD", 6 = "Factor Contributions to ES", 7 = "Factor
#' Contributions to VaR"
#' @param max.show Maximum assets to plot. Default is 6.
#' @param plot.single Plot a single asset of lm class. Defualt is FALSE.
#' @param fundName Name of the asset to be plotted.
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
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' \dontrun{
#' # load data from the database
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' # fit the factor model with OLS
#' fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
#'                                  variable.selection="all subsets")
#' # plot of all assets and show only first 4 assets.
#' plot(fit.macro,max.show=4)
#' # single plot of HAM1 asset 
#' plot(fit.macro, plot.single=TRUE, fundName="HAM1")
#' }
#' 
 plot.MacroFactorModel <- 
  function(fit.macro,colorset=c(1:12),legend.loc=NULL,
           which.plot=c("none","1L","2L","3L","4L","5L","6L","7L"),max.show=6,
           plot.single=FALSE, fundName,which.plot.single=c("none","1L","2L","3L","4L","5L","6L",
                                                                  "7L","8L","9L","10L","11L","12L","13L")) {
      require(zoo)
      require(PerformanceAnalytics)
      require(strucchange)
    
    if (plot.single==TRUE) {
      ## inputs:
      ## fit.macro        lm object summarizing factor model fit. It is assumed that
      ##                  time series date information is included in the names component
      ##                  of the residuals, fitted and model components of the object.   
      ## fundName         charater. The name of the single asset to be ploted.
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
      fit.lm = fit.macro$asset.fit[[fundName]]
      
      if (!(class(fit.lm) == "lm"))
        stop("Must pass a valid lm object")
      
      ## extract information from lm object
        
      factorNames = colnames(fit.lm$model)[-1]
      fit.formula = as.formula(paste(fundName,"~", paste(factorNames, collapse="+"), sep=" "))
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
   if (as.character(fit.macro$call["fit.method"]) == "OLS") {
        cusum.rec = efp(fit.formula, type="Rec-CUSUM", data=fit.lm$model)
        plot(cusum.rec, sub=fundName)
   } else 
     stop("CUMSUM applies only on OLS method")
      },
             "11L"= {
        ##  CUSUM plot of OLS residuals
               if (as.character(fit.macro$call["fit.method"]) == "OLS") {        
        cusum.ols = efp(fit.formula, type="OLS-CUSUM", data=fit.lm$model)
        plot(cusum.ols, sub=fundName)
               } else 
                 stop("CUMSUM applies only on OLS method")   
      },
             "12L"= {
        ##  CUSUM plot of recursive estimates relative to full sample estimates
               if (as.character(fit.macro$call["fit.method"]) == "OLS") {        
        cusum.est = efp(fit.formula, type="fluctuation", data=fit.lm$model)
        plot(cusum.est, functional=NULL, sub=fundName)
               } else 
                 stop("CUMSUM applies only on OLS method")
      },
             "13L"= {
        ##  rolling regression over 24 month window
    if (as.character(fit.macro$call["fit.method"]) == "OLS") {   
          rollReg <- function(data.z, formula) {
          coef(lm(formula, data = as.data.frame(data.z)))  
        }
        reg.z = zoo(fit.lm$model, as.Date(rownames(fit.lm$model)))
        rollReg.z = rollapply(reg.z, FUN=rollReg, fit.formula, width=24, by.column = FALSE, 
                              align="right")
        plot(rollReg.z, main=paste("24-month rolling regression estimates:", fundName, sep=" "))
    } else if (as.character(fit.macro$call["fit.method"]) == "DLS") {
      decay.factor <- as.numeric(as.character(fit.macro$call["decay.factor"]))
      t.length <- 24
      w <- rep(decay.factor^(t.length-1),t.length)
      for (k in 2:t.length) {
        w[k] = w[k-1]/decay.factor 
      }   
      w <- w/sum(w)
      rollReg <- function(data.z, formula,w) {
        coef(lm(formula,weight=w, data = as.data.frame(data.z)))  
      }
      reg.z = zoo(fit.lm$model[-length(fit.lm$model)], as.Date(rownames(fit.lm$model)))
      factorNames = colnames(fit.lm$model)[c(-1,-length(fit.lm$model))]
      fit.formula = as.formula(paste(fundName,"~", paste(factorNames, collapse="+"), sep=" "))
      rollReg.z = rollapply(reg.z, FUN=rollReg, fit.formula,w, width=24, by.column = FALSE, 
                            align="right")
      plot(rollReg.z, main=paste("24-month rolling regression estimates:", fundName, sep=" ")) 
    } 
        },
             invisible()
             )
      
      
      
    } else {    
    which.plot<-which.plot[1]
    
    if(which.plot=='none') 
      which.plot<-menu(c("Fitted factor returns",
                         "R square",
                         "Variance of Residuals",
                         "FM Correlation",
                         "Factor Contributions to SD",
                        "Factor Contributions to ES",
                         "Factor Contributions to VaR"),
                  title="Factor Analytics Plot \nMake a plot selection (or 0 to exit):\n") 
    
    variable.selection = fit.macro$variable.selection
    manager.names = colnames(fit.macro$ret.assets)
    factor.names  = colnames(fit.macro$factors)
    managers.df   = cbind(fit.macro$ret.assets,fit.macro$factors)
    cov.factors = var(fit.macro$factors)
    n <- length(manager.names)
    
    switch(which.plot,
           
           "1L" = {
     if (n >= max.show) {
      cat(paste("numbers of assets are greater than",max.show,", show only first",
                max.show,"assets",sep=" "))
    n <- max.show 
     }      
    par(mfrow=c(n/2,2))
    if (variable.selection == "lar" || variable.selection == "lasso") {
     for (i in 1:n) {
     alpha = fit.macro$alpha.vec[i]
     beta = as.matrix(fit.macro$beta.mat[i,])        
     fitted = alpha+as.matrix(fit.macro$factors)%*%beta  
     dataToPlot = cbind(fitted, na.omit(fit.macro$ret.assets[,i]))
     colnames(dataToPlot) = c("Fitted","Actual")
     main = paste("Factor Model fit for",manager.names[i],seq="")
     chart.TimeSeries(dataToPlot,colorset = colorset, legend.loc = legend.loc,main=main)
    }
     } else {
    for (i in 1:n) {
    dataToPlot = cbind(fitted(fit.macro$asset.fit[[i]]), na.omit(fit.macro$ret.assets[,i]))
    colnames(dataToPlot) = c("Fitted","Actual")
    main = paste("Factor Model fit for",manager.names[i],seq="")
    chart.TimeSeries(dataToPlot,colorset = colorset, legend.loc = legend.loc,main=main)
    }
    }
    par(mfrow=c(1,1))
    },
      "2L" ={
      barplot(fit.macro$r2.vec)
     },
      "3L" = {
      barplot(fit.macro$residVars.vec)  
      },    
           
     "4L" = {
      cov.fm<- factorModelCovariance(fit.macro$beta.mat,var(fit.macro$factors),fit.macro$residVars.vec)    
      cor.fm = cov2cor(cov.fm)
      rownames(cor.fm) = colnames(cor.fm)
      ord <- order(cor.fm[1,])
      ordered.cor.fm <- cor.fm[ord, ord]
      plotcorr(ordered.cor.fm, col=cm.colors(11)[5*ordered.cor.fm + 6])
           },
    "5L" = {
       factor.sd.decomp.list = list()
       for (i in manager.names) {
         factor.sd.decomp.list[[i]] =
           factorModelSdDecomposition(fit.macro$beta.mat[i,],
                                      cov.factors, fit.macro$residVars.vec[i])
         }
            # function to extract contribution to sd from list
       getCSD = function(x) {
         x$cr.fm
        }
      # extract contributions to SD from list
       cr.sd = sapply(factor.sd.decomp.list, getCSD)
       rownames(cr.sd) = c(factor.names, "residual")
      # create stacked barchart
       barplot(cr.sd, main="Factor Contributions to SD",
                 legend.text=T, args.legend=list(x="topleft"),
                 col=c(1:50) )
      
    },
     "6L"={
        factor.es.decomp.list = list()
       if (variable.selection == "lar" || variable.selection == "lasso") {
        
         for (i in manager.names) {
           idx = which(!is.na(managers.df[,i]))
           alpha = fit.macro$alpha.vec[i]
           beta = as.matrix(fit.macro$beta.mat[i,])        
           fitted = alpha+as.matrix(fit.macro$factors)%*%beta
           residual = fit.macro$ret.assets[,i]-fitted
           tmpData = cbind(managers.df[idx,i], managers.df[idx,factor.names],
                           (residual[idx,]/sqrt(fit.macro$residVars.vec[i])) )
           colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
           factor.es.decomp.list[[i]] = 
          factorModelEsDecomposition(tmpData, 
                                        fit.macro$beta.mat[i,],
                                        fit.macro$residVars.vec[i], tail.prob=0.05)
           
         }
       } else {
            
       for (i in manager.names) {
         # check for missing values in fund data
         idx = which(!is.na(managers.df[,i]))
         tmpData = cbind(managers.df[idx,i], managers.df[idx,factor.names],
                         residuals(fit.macro$asset.fit[[i]])/sqrt(fit.macro$residVars.vec[i]))
         colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
         factor.es.decomp.list[[i]] = 
           factorModelEsDecomposition(tmpData, 
                                      fit.macro$beta.mat[i,],
                                      fit.macro$residVars.vec[i], tail.prob=0.05)
       }
       }     
       
       # stacked bar charts of percent contributions to SD
       getCETL = function(x) {
         x$cES
       }
       # report as positive number
       cr.etl = sapply(factor.es.decomp.list, getCETL)
       rownames(cr.etl) = c(factor.names, "residual")
       barplot(cr.etl, main="Factor Contributions to ES",
               legend.text=T, args.legend=list(x="topleft"),
               col=c(1:50) ) 
     },
    "7L" ={
      
      factor.VaR.decomp.list = list()
      
      if (variable.selection == "lar" || variable.selection == "lasso") {
        
        for (i in manager.names) {
          idx = which(!is.na(managers.df[,i]))
          alpha = fit.macro$alpha.vec[i]
          beta = as.matrix(fit.macro$beta.mat[i,])        
          fitted = alpha+as.matrix(fit.macro$factors)%*%beta
          residual = fit.macro$ret.assets[,i]-fitted
          tmpData = cbind(managers.df[idx,i], managers.df[idx,factor.names],
                          (residual[idx,]/sqrt(fit.macro$residVars.vec[i])) )
          colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
          factor.VaR.decomp.list[[i]] = 
            factorModelVaRDecomposition(tmpData, 
                                       fit.macro$beta.mat[i,],
                                       fit.macro$residVars.vec[i], tail.prob=0.05)
          
        }
      } else {
      for (i in manager.names) {
        # check for missing values in fund data
        idx = which(!is.na(managers.df[,i]))
        tmpData = cbind(managers.df[idx,i], managers.df[idx,factor.names],
                        residuals(fit.macro$asset.fit[[i]])/sqrt(fit.macro$residVars.vec[i]))
        colnames(tmpData)[c(1,length(tmpData))] = c(i, "residual")
        factor.VaR.decomp.list[[i]] = 
          factorModelVaRDecomposition(tmpData, 
                                     fit.macro$beta.mat[i,],
                                     fit.macro$residVars.vec[i], tail.prob=0.05,
                                      VaR.method="HS")
      }
      }
      
      # stacked bar charts of percent contributions to SD
      getCVaR = function(x) {
        x$cVaR.fm
      }
      # report as positive number
      cr.VaR = sapply(factor.VaR.decomp.list, getCVaR)
      rownames(cr.VaR) = c(factor.names, "residual")
      barplot(cr.VaR, main="Factor Contributions to VaR",
              legend.text=T, args.legend=list(x="topleft"),
              col=c(1:50) )
    },
    invisible()       
    )         
    }       
 
}
