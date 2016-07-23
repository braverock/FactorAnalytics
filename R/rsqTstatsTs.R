#' @title  t-stats and Plots for a fitted Fundamental Factor Model
#' 
#' @description
#'  Calculate and plot the time series of the R-squared values, t-statistic vales and the
#'  number of risk indices with significant t-stats for a fundamentally fit object.
#' @importFrom xts xts
#' @importFrom zoo plot.zoo
#' @importFrom zoo as.yearmon
#' @importFrom graphics barplot
#' @importFrom lattice panel.abline xyplot panel.xyplot
#'  
#' @param ffmObj  an object of class \code{ffm} produced by \code{fitFfm}
#' @param isPlot  logical. If \code{TRUE} the barplots are plotted.
#' @param col     specification for the default plotting color. Default is cyan
#' @param z.alpha critical value corresponding to the confidence interval. Default is 1.96 i.e 95\% C.I
#' @param layout  numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in the xyplot of t-statistics. Default is c(2,3).
#' @param type    character. Type of the xyplot of t-statistics; \code{"l"} for lines, \code{"p"} for points, \code{"h"} for histogram like (or high-density) vertical lines
#'                 and \code{"b"} for both. Deafault is \code{"h"}.

#' @param ...     potentially further arguments passed.
#' 
#' @author Doug Martin, Avinash Acharya
#' 
#' @return \code{rsqTstatsTs} plots the R-squared, t-stats and significant t-stats values  if \code{isPlot} is \code{TRUE} and returns a list with following components:
#' \item{R-squared}{ length-T vector of R-squared values.}
#' \item{tstats}{ an xts object of t-stats values.}
#' \item{z.alpha}{ critical value corresponding to the confidence interval.}
#' @examples 
#'  
#'  data("factorDataSetDjia5Yrs")
#'  
#' #Fit a Ffm
#'  require(factorAnalytics)
#'  fit <- fitFfm(data = factorDataSetDjia5Yrs,exposure.vars = c("MARKETCAP","ENTVALUE","P2B","EV2S"),
#'                date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", fit.method="WLS")
#'                
#'  #Find r2, tstats  with C.I = 95% and plot the results.
#'  stats = rsqTstatsTs(fit, isPlot = TRUE, col = "blue", z.alpha =1.96)  
#'    
#'               
#' 
#' @export

rsqTstatsTs<- function(ffmObj, isPlot = TRUE, col = "cyan", z.alpha = 1.96, layout =c(2,3),type ="h", ... )
{
  
  # CREATE TIME SERIES OF T-STATS
  time.periods = length(ffmObj$time.periods)
  n.exposures =  length(ffmObj$exposure.vars)
  tstats = lapply(seq(time.periods), function(a) summary(ffmObj)$sum.list[[a]]$coefficients[,3])
  tstats = matrix(unlist(tstats), byrow = TRUE, nrow = time.periods, ncol = n.exposures+1 )
  secNames = c("INTCEPT",ffmObj$exposure.vars)
  colnames(tstats)=secNames
  tstatsTs = xts(tstats,order.by=as.yearmon(names(ffmObj$r2)))
  
  # COUNT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
  sigTstats = as.matrix(rowSums(ifelse(abs(tstats) > z.alpha,1,0)))
  sigTstatsTs = xts(sigTstats,order.by=as.yearmon(names(ffmObj$r2)))
  
  if(isPlot)
  { #Plot monthly R-squared values
    barplot(ffmObj$r2,las=2,col=col,
            names.arg= as.yearmon(names(ffmObj$r2)),
            cex.names=0.5,
            main="R-squared Values for Risk Indices Factor Model")
    
    hlines1 = rep(z.alpha, n.exposures+1)
    hlines2 = rep(-z.alpha, n.exposures+1)
    panel =  function(...){
      panel.abline(h=hlines1,lty = 3, col = "red")
      panel.abline(h=hlines2,lty = 3, col = "red")
      panel.xyplot(...)
    }
    
    # PLOT T-STATS WITH XYPLOT
    plt<- xyplot(tstatsTs, panel = panel, type = type, scales = list(y = list(cex = 1), x = list(cex = 1)),
            layout = layout, main = "t-statistic values", col = col, strip.left = T, strip = F)
    print(plt)
    # PLOT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
    barplot(sigTstatsTs,col = col, main = "Number of Risk Indices with significant t-stats")
    
  }
  list("R-squared" =ffmObj$r2, "tstats" =tstatsTs, "z.alpha" =z.alpha)
}
