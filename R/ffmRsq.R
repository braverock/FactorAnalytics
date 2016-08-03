#' @title  Factor Model R-squared and VIF Values
#'
#' @description Calcluate and plot the Factor Model R-squared, Adjusted R-squared and Variance Inflation Factors for a portfolio of assets
#'
#' @importFrom zoo as.yearmon
#' @importFrom graphics barplot
#' @importFrom stats lm
#' @importFrom xts xts merge.xts
#' @importFrom lattice panel.abline xyplot panel.xyplot strip.custom
#' 
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param rsq      logical; if \code{TRUE}, Factor Model R-squared values are computed for the portfolio. Default is \code{TRUE}.
#' @param rsqAdj   logical; if \code{TRUE}, Adjusted R-squared values are computed for the portfolio. Default is \code{FALSE}.
#' @param VIF      logical; if \code{TRUE}, Variance Inflation factor is calculated. Default is \code{FALSE}.
#'                 At least 2 continous variables are required in \code{exposure.vars} of fitted model to find VIF.
#' @param plt.type a number to indicate the type of plot for plotting Factor Model R-squared/Adj. R-squared values.
#'                 1 indicates barplot, 2 indicates time series xy plot. Default is 2.
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2.
#' @param isPrint  logical. if \code{TRUE}, the time series of the computed factor model values is printed along with their mean values.
#'                 Else, only the mean values are printed. Default is \code{TRUE}.
#' @param isPlot   logical. if \code{TRUE}, the time series of the output is plotted. Default is \code{TRUE}.
#' @param lwd      line width relative to the default. Default is 2.
#' @param title    logical. if \code{TRUE}, the plots will have the main tiltle. default is \code{TRUE}.
#' 
#' @param ...      potentially further arguments passed.
#' @author Avinash Acharya and Doug Martin
#'
#' @return \code{ffmRsq} returns the sample mean values and plots the time series of corresponding R squared values
#'                         and the Variance Inflation factors depending on the values of \code{rsq}, \code{rsqAdj} and \code{VIF}.
#'                         The time series of the output values are also printed if \code{isPrint} is \code{TRUE} 
#'
#' @examples
#'
#' #Load the data
#'  data("factorDataSetDjia5Yrs")
#'
#' #Fit a Ffm
#' require(factorAnalytics)
#'  fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN",
#'               date.var="DATE", exposure.vars="SECTOR")
#'
#' #Calcuate and plot the portfolio R-squared values
#'  ffmRsq(fit)
#'  
#'  fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN",
#'               date.var="DATE", exposure.vars=c("SECTOR", "P2B", "EV2S", "MKTCAP"))
#'
#' #Plot and print the time series of Adj R-squared and VIF values
#'  ffmRsq(fit1, VIF=TRUE, rsqAdj=TRUE, isPrint=TRUE, plt.type = 2)
#' @export


ffmRsq <- function(ffmObj, rsq=T, rsqAdj=F, VIF=F, plt.type= 2, digits=2, isPrint=T, isPlot =T, lwd =2, title = TRUE, ...)
{
  # set defaults and check input validity
  if (!inherits(ffmObj, "ffm"))
  {
    stop("Invalid argument: Object should be of class'ffm'.")
  }
  
  if (!(rsq) && !(rsqAdj) && !(VIF))
  {
    stop("Invalid arguments: Inputs rsq, rsqAdj and VIF cannot be False.")
  }
  
  n.assets <- length(ffmObj$asset.names)
  r2<- ffmObj$r2
  out<- list()
  ret<- list()
  plt.r2<- FALSE
  
  if(rsq)
  {
    if(isPlot && plt.type == 1)
    {
      barplot(r2,las=2,col=5,
              names.arg= as.yearmon(names(r2)),
              cex.names=0.5,
              main=" ")
      if(title){title("Factor Model R-squared Values")}
    }
    else if (isPlot && plt.type == 2)
    {
      r2.xts = xts(r2, order.by = as.yearmon(names(r2)))
      
      if(rsqAdj) plt.r2 = TRUE else{
        if(title) title.Rsq = "Factor Model R-squared Values" else title.Rsq = " " 
        panel =  function(...){
          panel.abline(h=0,lty = 3)
          panel.xyplot(...)}
        plt = xyplot(r2.xts,col = "blue",lwd =lwd, panel =panel, scale = list(y = list(cex=1,rot =0),x = list(cex =1)),
                     main = title.Rsq, type = "h", ylim = c(0,(max(r2.xts)+0.05)), strip.left = strip.custom(var.name = "R-Squared", style = 1, strip.names = T,strip.levels=F ))
        print(plt) 
        
      }
    }
    r2.mean<- round(mean(r2),digits = digits)
    names(r2.mean) <- "Mean R-Squared"
    out<- r2.mean
    r2<- round(r2,digits = digits)
    ret<- list("R-squared" = r2)
  }
  if(rsqAdj)
  {
    K <- length(ffmObj$factor.name)
    p <- K-1
    adj.r2 <- 1 - ((n.assets - 1)*(1- r2) / (n.assets - p - 1))
    adj.r2.xts = xts(adj.r2, order.by = as.yearmon(names(r2)))
    if(isPlot && plt.type == 1)
    {
      barplot(adj.r2,las=2,col=5,
              names.arg= as.yearmon(names(r2)),
              cex.names=0.5,
              main=" ")
      if(title){title(" Factor Model Adjusted R-squared Values")}
    }
    else if (isPlot && plt.type == 2 && !plt.r2)
    {
      if(title) title.AdjRsq = "Factor Model Adjusted R-squared Values" else title.AdjRsq = " " 
      panel =  function(...){
        panel.abline(h=0,lty = 3)
        panel.xyplot(...)}
      plt = xyplot(adj.r2.xts,col = "blue", lwd =lwd, main = title.AdjRsq, type = "h",panel = panel,
                   scales = list(y = list(cex = 1,relation="same"),x = list(cex =1)),
                   strip.left = strip.custom(var.name = "Adj R-Squared", style = 1, strip.names = T,strip.levels=F ))
      print(plt) 
    }
    adj.r2.mean<- round(mean(adj.r2),digits = digits)
    names(adj.r2.mean) <- "Mean Adj R-Squared"
    out<- adj.r2.mean
    adj.r2 <- round(adj.r2,digits = digits)
    ret<- list("Adj.r-Squared" = adj.r2 )
  }
  if(rsqAdj && rsq)
  {
    if (isPlot && plt.type == 2)
    { 
      if(title) title.comb = "Factor Model R-squared Values" else title.comb = " " 
      r2.combined = merge.xts("R-squared" = r2.xts,"Adj R-squared" =  adj.r2.xts)
      tsPlotMP(0.01*r2.combined,stripLeft = TRUE, scaleType = "same",
               color = "blue", yname = "", lwd = lwd, main = title.comb, type = "h", cex = 1.2)
      
    }
    out<- c(r2.mean, adj.r2.mean)
    ret<- list("R-Squared"= r2, "Adj.R-Squared" = adj.r2)
  }
  if(VIF)
  {
    exposure.vars= ffmObj$exposure.vars
    which.numeric <- sapply(ffmObj$data[,exposure.vars,drop=FALSE], is.numeric)
    exposures.num <- exposure.vars[which.numeric]
    if(length(exposures.num) < 2)
    {
      stop(" At least 2 continous variables required to find VIF")
    }
    
    object = ffmObj$data[exposures.num]  
    object <- as.matrix(object)
    ncols <- dim(object)[2]
    time.periods = length(ffmObj$time.periods)
    vifs = matrix(0, nrow = time.periods, ncol = ncols)
    for(i in 1:60)
    {
      vifs[i,1:ncols] = sapply(seq(ncols), function(x)
        1/(1 - summary(lm(object[((i-1)*n.assets+1) : (i*n.assets), x] ~ 
                            object[((i-1)*n.assets+1) :(i*n.assets), -x]))$r.squared))
    }
    colnames(vifs) <- dimnames(object)[[2]]
    vifs.xts = xts(vifs, order.by = ffmObj$time.periods)
    vifs.mean = round(colMeans(vifs.xts),digits = digits)
    if(isPlot)
    {
      if(title) title.vif = "Factor Model VIF Values" else title.vif = " " 
      #Assuming the number of continous variables in exposure.vars is less than 6,layout=c(1,ncols) is defined.
      tsPlotMP(0.01*vifs.xts,stripLeft = TRUE, layout = c(1,ncols), scaleType = "same",
               color = "blue", yname = "", lwd = lwd, main =title.vif, type = "h")
    }
    vifs.xts = round(vifs.xts,digits = digits)
    out<- append(out, list("Mean.VIF" = vifs.mean))
    ret<- append(ret, list("VIF" = vifs.xts))
  }
  
  if(isPrint)
  {
    print(c(out, ret))
  }else{
    print(out)
    invisible(c(out, ret))
  }
}

