plot.MacroFactorModel <- 
  function(fit.macro,colorset=c(1:12),legend.loc=NULL,which=c("none","1L","2L","3L",
                                                              "4L","5L")) {
    which<-which[1]
    
    if(which=='none') 
      which<-menu(c("Fitted factor returns","FM Correlation",
                    "Factor Contributions to SD",
                    "Factor Contributions to ES",
                    "Factor Contributions to VaR"),
                  title="Factor Analytics Plot") 
    
    variable.selection = fit.macro$variable.selection
    manager.names = colnames(fit.macro$ret.assets)
    factor.names  = colnames(fit.macro$factors)
    managers.df   = cbind(ret.assets,factors)
    cov.factors = var(fit.macro$factors)
    n <- length(manager.names)
    
    
    switch(which,"1L" = {
          
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
           
     "2L" = {
      cov.fm<- factorModelCovariance(fit.macro$beta.mat,var(fit.macro$factors),fit.macro$residVars.vec)    
      cor.fm = cov2cor(cov.fm)
      rownames(cor.fm) = colnames(cor.fm)
      ord <- order(cor.fm[1,])
      ordered.cor.fm <- cor.fm[ord, ord]
      plotcorr(ordered.cor.fm, col=cm.colors(11)[5*ordered.cor.fm + 6])
           },
    "3L" = {
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
     "4L"={
       
       factor.es.decomp.list = list()
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
    "5L" ={
      
      factor.VaR.decomp.list = list()
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
    }       
    )         
           
 
}