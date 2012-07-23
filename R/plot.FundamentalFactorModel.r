# plot.FundamentalFactorModel.r
# Yi-An Chen
# 7/16/2012

plot.FundamentalFactorModel <- 
function(fund.fit,which.plot=c("none","1L","2L","3L","4L","5L","6L","7L"),max.show=12,
         plot.single=FALSE, fundId, fundName="TBA",
         which.plot.single=c("none","1L","2L","3L","4L","5L","6L","7L","8L",
                             "9L","10L","11L","12L","13L") ) {
require(ellipse)
  
  if (plot.single==TRUE) {
  
  } else {    
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
  
  
} 
  