# plot.FundamentalFactorModel.r
# Yi-An Chen
# 7/16/2012



#' plot FundamentalFactorModel object.
#' 
#' Generic function of plot method for fitFundamentalFactorModel.
#' 
#' 
#' @param fund.fit fit object created by fitFundamentalFactorModel.
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
#' assets = unique(fulldata[,"PERMNO"])
#' timedates = as.Date(unique(fulldata[,"DATE"]))
#' exposures <- exposures.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' fund.fit <- fitFundamentalFactorModel(fulldata=fulldata, timedates=timedates, exposures=exposures,covariance="classic", assets=assets,full.resid.cov=TRUE,
#'                                       regression="classic",wls=TRUE)
#' 
#' plot(fund.fit)
#' }
#' 
plot.FundamentalFactorModel <- 
function(fund.fit,which.plot=c("none","1L","2L","3L","4L"),max.show=12) 
  {
require(ellipse)
  
 
    which.plot<-which.plot[1]
    
    if(which.plot=='none') 
      which.plot<-menu(c("Factor returns",
                         "Residual plots",
                         "Variance of Residuals",
                         "Factor Model Correlation"),
                       title="Factor Analytics Plot \nMake a plot selection (or 0 to exit):\n") 
    
    
    n <- length(fund.fit$asset)
    if (n >= max.show) {
      cat(paste("numbers of assets are greater than",max.show,", show only first",
                max.show,"assets",sep=" "))
      n <- max.show 
    }
    switch(which.plot,
           
           "1L" = {
            plot(fund.fit$factor.rets,main="Factor Returns") 
             
           }, 
          "2L" ={
            plot(fund.fit$resids[,c(1:n)],main="Residuals")
           },
           "3L" = {
             barplot(fund.fit$resid.vars[c(1:n)])  
           },    
           
           "4L" = {
             cor.fm = cov2cor(fund.fit$cov.returns$cov)
             rownames(cor.fm) = colnames(cor.fm)
             ord <- order(cor.fm[1,])
             ordered.cor.fm <- cor.fm[ord, ord]
             plotcorr(ordered.cor.fm[c(1:n),c(1:n)], col=cm.colors(11)[5*ordered.cor.fm + 6])
           },
           
           invisible()       
    )         
 
  
  
} 
  
