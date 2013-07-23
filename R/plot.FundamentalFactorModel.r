# plot.FundamentalFactorModel.r
# Yi-An Chen
# 7/16/2012



#' plot FundamentalFactorModel object.
#' 
#' Generic function of plot method for fitFundamentalFactorModel.
#' 
#' 
#' @param fit.fund fit object created by fitFundamentalFactorModel.
#' @param which.plot integer indicating which plot to create: "none" will
#' create a menu to choose. Defualt is none. 1 = "factor returns", 2 = "R
#' square", 3 = "Variance of Residuals", 4 = "FM Correlation",
#' @param max.show Maximum assets to plot. Default is 12.
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' \dontrun{
#' # BARRA type factor model
#' # there are 447 assets  
#' data(stock)
#' # BARRA type factor model
#' data(stock)
#' # there are 447 assets  
#' exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' fit.fund <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
#'                                        datevar = "DATE", returnsvar = "RETURN",
#'                                        assetvar = "TICKER", wls = TRUE, 
#'                                        regression = "classic", 
#'                                        covariance = "classic", full.resid.cov = TRUE, 
#'                                        robust.scale = TRUE)
#' 
#' plot(fit.fund)
#' }
#' 
plot.FundamentalFactorModel <- 
function(fit.fund,which.plot=c("none","1L","2L","3L","4L"),max.show=10) 
  {
require(ellipse)
require(PerformanceAnalytics)  
 
    which.plot<-which.plot[1]
    
    if(which.plot=='none') 
      which.plot<-menu(c("Factor returns",
                         "Residual plots",
                         "Variance of Residuals",
                         "Factor Model Correlation",
                         "Factor Contributions to SD",
                         "Factor Contributions to ES",
                         "Factor Contributions to VaR"),
                       title="Factor Analytics Plot \nMake a plot selection (or 0 to exit):\n") 
    
    n <- length(fit.fund$asset.names)
    if (n >= max.show) {
      cat(paste("numbers of assets are greater than",max.show,", show only first",
                max.show,"assets",sep=" "))
      n <- max.show 
    }
    switch(which.plot,
           
           "1L" = {
            factor.names <- colnames(fit.fund$factors)
#             nn <- length(factor.names)
            par(mfrow=c(n/2,2))
            for (i in factor.names[1:n]) {
            plot(fit.fund$factors[,i],main=paste(i," Factor Returns",sep="") )
            }
            par(mfrow=c(1,1))
           }, 
          "2L" ={
            par(mfrow=c(n/2,2))
            names <- colnames(fit.fund$residuals[,1:n])
            for (i in names) {
            plot(fit.fund$residuals[,i],main=paste(i," Residuals", sep=""))
            }
            par(mfrow=c(1,1))
           },
           "3L" = {
             barplot(fit.fund$resid.variance[c(1:n)])  
           },    
           
           "4L" = {
             cor.fm = cov2cor(fit.fund$returns.cov$cov)
             rownames(cor.fm) = colnames(cor.fm)
             ord <- order(cor.fm[1,])
             ordered.cor.fm <- cor.fm[ord, ord]
             plotcorr(ordered.cor.fm[c(1:n),c(1:n)], col=cm.colors(11)[5*ordered.cor.fm + 6])
           },
           "5L" = {
             cov.factors = var(fit.fund$factors)
             names = fit.fund$asset.names
             factor.sd.decomp.list = list()
             for (i in names) {
               factor.sd.decomp.list[[i]] =
                 factorModelSdDecomposition(fit.fund$beta[i,],
                                            cov.factors, fit.fund$resid.variance[i])
             }
             # function to efit.stattract contribution to sd from list
             getCSD = function(x) {
               x$cr.fm
             }
             # extract contributions to SD from list
             cr.sd = sapply(factor.sd.decomp.list, getCSD)
             rownames(cr.sd) = c(colnames(fit.fund$factors), "residual")
             # create stacked barchart
             barplot(cr.sd[,(1:max.show)], main="Factor Contributions to SD",
                     legend.text=T, args.legend=list(x="topleft"),
                     col=c(1:50) )
           } ,
           "6L" = {
           factor.es.decomp.list = list()
           names = fit.fund$asset.names
           for (i in names) {
             # check for missing values in fund data
#             idx = which(!is.na(fit.fund$data[,i]))
             idx <- fit.fund$data[,fit.fund$assetvar]  == i  
             asset.ret <- fit.fund$data[idx,fit.fund$returnsvar]
             tmpData = cbind(asset.ret, fit.fund$factors,
                             fit.fund$residuals[,i]/sqrt(fit.fund$resid.variance[i]) )
             colnames(tmpData)[c(1,length(tmpData[1,]))] = c(i, "residual")
             factor.es.decomp.list[[i]] = 
               factorModelEsDecomposition(tmpData, 
                                          fit.fund$beta[i,],
                                          fit.fund$resid.variance[i], tail.prob=0.05)
           }
          
           # stacked bar charts of percent contributions to ES 
           getCETL = function(x) {
             x$cES
           }
           # report as positive number
           cr.etl = sapply(factor.es.decomp.list, getCETL)
           rownames(cr.etl) = c(colnames(fit.fund$factors), "residual")
           barplot(cr.etl[,(1:max.show)], main="Factor Contributions to ES",
                   legend.text=T, args.legend=list(x="topleft"),
                   col=c(1:50) )
           },
           "7L" =  {
             factor.VaR.decomp.list = list()
             names = fit.fund$asset.names
             for (i in names) {
               # check for missing values in fund data
               #             idx = which(!is.na(fit.fund$data[,i]))
               idx <- fit.fund$data[,fit.fund$assetvar]  == i  
               asset.ret <- fit.fund$data[idx,fit.fund$returnsvar]
               tmpData = cbind(asset.ret, fit.fund$factors,
                               fit.fund$residuals[,i]/sqrt(fit.fund$resid.variance[i]) )
               colnames(tmpData)[c(1,length(tmpData[1,]))] = c(i, "residual")
               factor.VaR.decomp.list[[i]] = 
                 factorModelVaRDecomposition(tmpData, 
                                            fit.fund$beta[i,],
                                            fit.fund$resid.variance[i], tail.prob=0.05)
             }
             
             
             # stacked bar charts of percent contributions to VaR
             getCVaR = function(x) {
               x$cVaR.fm
             }
             # report as positive number
             cr.var = sapply(factor.VaR.decomp.list, getCVaR)
             rownames(cr.var) = c(colnames(fit.fund$factors), "residual")
             barplot(cr.var[,(1:max.show)], main="Factor Contributions to VaR",
                     legend.text=T, args.legend=list(x="topleft"),
                     col=c(1:50) )
           },
           invisible()       
    )         
 
  
  
} 
  
