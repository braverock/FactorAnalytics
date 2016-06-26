#' @title  Plots from a fitted fundamental factor model
#' 
#' @description
#'  Plot time series of the R-squared values, t-statistic vales and the
#'  number of risk indices with significant t-stats for a fundamentally fit object. By default, all the three plots are plotted.
#' 
#' @param ffmObj  an object of class \code{ffm} produced by \code{fitFfm}
#' @param col     A specification for the default plotting color. Default is cyan
#' @param z.score critical value corresponding to the confidence interval. Default is 1.96 i.e 95\% C.I
#' @param ...     potentially further arguments passed.
#' 
#' @author Doug Martin, Avinash Acharya
#' 
#' 
#' @examples 
#'
#' #Load the data 
#' data("factorDataSetDjia5Yrs")
#'  
#' #Fit a Ffm
#' fit <- fitFfm(data=stocks145scores6, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars="SECTOR")
#'               riskindices.mod <- fitFfm(data = stacked.df,exposure.vars = c("MARKETCAP","ENTVALUE","P2B","EV2S"),
#'               date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", fit.method="WLS")
#'    
#'  rsqTstatsTs(fit)  
#'    
#'               
#' 
#' @export

# Not the final version

rsqTstatsTs<- function(ffmObj, col = "cyan", z.score = 1.96, ... )
  {
    barplot(ffmObj$r2,las=2,col=col,
    names.arg= as.yearmon(names(ffmObj$r2)),
    cex.names=0.5,
    main="R-squared Values for Risk Indices Factor Model")

    # CREATE TIME SERIES OF T-STATS
    time.periods = length(ffmObj$time.periods)
    n.exposures =  length(ffmObj$exposure.vars)
    tstats = matrix(0,nrow = time.periods, ncol = n.exposures+1)

    for(i in 1:time.periods)
    {
      tstats[i,] = summary(ffmObj)$sum.list[[i]]$coefficients[,3]
    }
    secNames = c("INTCEPT",ffmObj$exposure.vars)
    dimnames(tstats)[[2]]=secNames
    tstatsTs = xts(tstats,order.by=as.yearmon(names(ffmObj$r2)))

    # MAKING T-STATS PLOTS WITH PLOT.ZOO
    hlines1 = rep(z.score, n.exposures+1)
    hlines2 = rep(-z.score, n.exposures+1)
    my.panel <- function(x, ...) {
    lines(x, ...)
    panel.number <- parent.frame()$panel.number
    abline(h = hlines1[panel.number], col = "red", lty = "dashed", lwd = 1.5)
    abline(h = hlines2[panel.number], col = "red", lty = "dashed", lwd = 1.5)
    }
    plot.zoo(tstatsTs,type = "h", lwd = 1.7, panel = my.panel, main = "t-statistic values")
    
    # COUNT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
    sigTstats = rep(0,time.periods)
    for(i in 1:time.periods)
    {sigTstats[i] = sum(ifelse(abs(tstats[i,]) > z.score,1,0))}

    # PLOT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
    sigTstatsTs = xts(sigTstats,order.by=as.yearmon(names(ffmObj$r2)))
    barplot(sigTstatsTs,col = col, main = "Number of Risk Indices with significant t-stats")
  }
