#' @title  t-stats and Plots for a fitted Fundamental Factor Model
#' 
#' @description
#'  Calculate and plot the time series of the R-squared values, t-statistic vales and the
#'  number of risk indices with significant t-stats for a fundamentally fit object.
#' 
#' @param ffmObj  an object of class \code{ffm} produced by \code{fitFfm}
#' @param isPlot  logical. If \code{TRUE} the barplots are plotted.
#' @param col     A specification for the default plotting color. Default is cyan
#' @param z.alpha critical value corresponding to the confidence interval. Default is 1.96 i.e 95\% C.I
#' @param ...     potentially further arguments passed.
#' 
#' @author Doug Martin, Avinash Acharya
#' 
#' @return \code{rsqTstatsTs}  returns a list with following components:
#' \item{R-squared}{ length-T vector of R-squared values.}
#' \item{tstats}{ an xts object of t-stats values.}
#' \item{z.alpha}{ critical value corresponding to the confidence interval.}
#'  It plots the barplots for R-squared, t-stats and significant t-stats if \code{isPlot} is \code{TRUE}.
#' @examples 
#'  data("factorDataSetDjia5Yrs")
#' #Fit a Ffm
#'  fit <- fitFfm(data = factorDataSetDjia5Yrs,exposure.vars = c("MARKETCAP","ENTVALUE","P2B","EV2S"),
#'                date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", fit.method="WLS")
#'  #Find R2, tstats  with C.I = 95% and plot the results.
#'  stats = rsqTstatsTs(fit, isPlot = T, col = "blue", z.alpha =1.96)  
#'    
#'               
#' 
#' @export

rsqTstatsTs<- function(ffmObj, isPlot = TRUE, col = "cyan", z.alpha = 1.96, ... )
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
    my.panel <- function(x, ...) 
    {
      lines(x, ...)
      panel.number <- parent.frame()$panel.number
      abline(h = hlines1[panel.number], col = "red", lty = "dashed", lwd = 1.5)
      abline(h = hlines2[panel.number], col = "red", lty = "dashed", lwd = 1.5)
    }
    # PLOT T-STATS WITH PLOT.ZOO
    plot.zoo(tstatsTs,type = "h", lwd = 1.7, panel = my.panel, main = "t-statistic values")
    # PLOT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
    barplot(sigTstatsTs,col = col, main = "Number of Risk Indices with significant t-stats")
    
  }
  list("R-squared" =ffmObj$r2, "tstats" =tstatsTs, "z.alpha" =z.alpha)
}
