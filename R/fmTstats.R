#' @title  t-stats and Plots for a fitted Fundamental Factor Model
#' 
#' @description
#'  Calculate and plot the time series of the t-statistic values and the
#'  number of risk indices with significant t-stats for a fundamentally fit object.
#' @importFrom xts xts 
#' @importFrom zoo plot.zoo coredata as.yearmon
#' @importFrom lattice panel.abline xyplot panel.xyplot barchart
#' @importFrom grDevices dev.off
#' @importFrom stats vcov
#' @importFrom data.table melt
#' @importFrom HH likert
#'  
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param isPlot   logical. If \code{FALSE} no plots are displayed.
#' @param isPrint  logical. if \code{TRUE}, the time series of the computed factor model values is printed. default is \code{FALSE}, 
#' @param whichPlot string indicating the plot(s) to be plotted. Choose from ("all", "tStats", "significantTstatsV", "significantTstatsH", "significantTstatsLikert" ).
#'                   Three variants of significantTstats stand for vertical, horizontal and likert barplots. Default is \code{all} plotting t-stats and significant t-stats with vertical bars.
#' @param color  length 2 vector specifying the plotting color for t-stats plot and for barplot 
#'                 respectively. default is \code{c("black", "cyan")}
#' @param lwd      line width relative to the default. default is 2.
#' @param digits   an integer indicating the number of decimal places to be used for rounding. default is 2.
#' @param z.alpha  critical value corresponding to the confidence interval. default is 1.96 i.e 95\% C.I
#' @param layout   numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in the xyplot of t-statistics. default is c(2,3).
#' @param type     character. Type of the xyplot of t-statistics; \code{"l"} for lines, \code{"p"} for points, \code{"h"} for histogram like (or high-density) vertical lines
#'                 and \code{"b"} for both. Deafault is \code{"h"}.
#' @param scale    character. It determines how axis limits are calculated for each panel. Possible values are "same" , "free" (default) and "sliced".
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param title    logical. if \code{TRUE}, the plots will have the main tiltle. default is \code{TRUE}.

#' @param ...     potentially further arguments passed.
#' 
#' @author Avinash Acharya and Doug Martin
#' 
#' @return \code{fmTstats} plots the t-stats and significant t-stats values  if \code{isPlot} is \code{TRUE} and returns a list with following components:
#' \item{tstats}{ an xts object of t-stats values.}
#' \item{z.alpha}{ critical value corresponding to the confidence interval.}
#' @examples 
#'  
#'  data("factorDataSetDjia5Yrs")
#'
#'#Fit a Ffm with style factors only
#'  require(FactorAnalytics)
#'  fit <- fitFfm(data = factorDataSetDjia5Yrs,exposure.vars = c("MKTCAP","ENTVAL","P2B","EV2S"),
#'              date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", fit.method="WLS",z.score = "crossSection")
#'
#'#Compute time series of t-stats and number of significant t-stats 
#'  stats = fmTstats(fit, isPlot = TRUE, lwd = 2, color = c("blue", "blue"), z.alpha =1.96)
#'
#' fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                date.var="DATE", exposure.vars=c("SECTOR","MKTCAP","ENTVAL","P2B"), addIntercept=TRUE)
#' #Compute time series of t-stats and number of significant t-stats 
#'  stats = fmTstats(fit1, isPlot = TRUE, z.alpha =1.96) 
#'                
#' # Fit a SECTOR+COUNTRY+Style model with Intercept
#' # Create a COUNTRY column with just 3 countries
#' 
#'  factorDataSetDjia5Yrs$COUNTRY = rep(rep(c(rep("US", 1 ),rep("GERMANY", 1 )), 11), 60)
#'  exposure.vars= c("SECTOR", "COUNTRY","P2B", "MKTCAP")
#'  
#'  fit.MICM <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                    date.var="DATE", exposure.vars=exposure.vars, addIntercept=TRUE)
#'  stats = fmTstats(fit.MICM, isPlot = TRUE, z.alpha =1.96)
#' @rdname fmTstats
#' @export

fmTstats <- function(ffmObj, ...){
  # check input object validity
  if (!inherits(ffmObj, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  UseMethod("fmTstats")
}

#' @rdname fmTstats
#' @method fmTstats ffm
#' @export
#' 
fmTstats.ffm<- function(ffmObj, isPlot = TRUE, isPrint = FALSE,whichPlot = "all", color = c("black", "cyan"),lwd =2, digits =2, z.alpha = 1.96, layout =c(2,3),type ="h", scale = "free", stripText.cex =1,axis.cex=1, title = TRUE, ... )
{
  
  # CREATE TIME SERIES OF T-STATS
  time.periods = length(ffmObj$time.periods)
  exposure.vars = ffmObj$exposure.vars
  n.exposures =  length(exposure.vars)
  which.numeric <- sapply(ffmObj$data[,exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- exposure.vars[which.numeric]
  exposures.char <- exposure.vars[!which.numeric]
  n.expo.num <- length(exposures.num)
  n.expo.char <- length(exposures.char)
  if ( n.expo.char > 0 && grepl("Market|Alpha", ffmObj$factor.names[1]))
  { #Covaraince matrix for g coefficients.
    cov.g = lapply(seq(time.periods), function(a) vcov((ffmObj$factor.fit)[[a]]))
    restriction.mat = ffmObj$restriction.mat
    #Number of factors in g except for the style factors
    fac.num = ncol(restriction.mat)
    # covarinace of f coefficeints: cov(f) = R*cov.g*t(R)
    cov.factors = lapply(seq(time.periods), function(x) restriction.mat %*% cov.g[[x]][1:fac.num, 1:fac.num]
                         %*% t(restriction.mat))
    
    std.errors = lapply(seq(time.periods), function(x) sqrt(diag(cov.factors[[x]])))
    std.errors = matrix(unlist(std.errors), byrow = TRUE, nrow = time.periods)
    fac.names.indcty = lapply(seq(n.expo.char), function(x)
      paste(levels(ffmObj$data[,exposures.char[x]]),sep=""))
    colnames(std.errors) <- c("Market",unlist(fac.names.indcty))
    if(n.expo.num > 0)
    {
      #std.errs of stly factors 
      stdErr.sty = lapply(seq(time.periods), function(a) summary(ffmObj)$sum.list[[a]]$
                            coefficients[((fac.num+1):(fac.num+n.expo.num)),2])
      stdErr.sty = matrix(unlist(stdErr.sty), byrow = TRUE, nrow = time.periods)
      colnames(stdErr.sty) = exposures.num
      #Should be in same order as that of factor.retunrs
      std.errors = cbind(std.errors,stdErr.sty)
      std.errors = std.errors[, colnames(ffmObj$factor.returns)]
    }
    #tstatsTs = ffmObj$factor.returns/std.errors
    tstats = coredata(ffmObj$factor.returns/std.errors)
    
  }else
  { tstats = lapply(seq(time.periods), function(a) summary(ffmObj)$sum.list[[a]]$coefficients[,3])
  secNames = names(tstats[[1]])
  tstats = matrix(unlist(tstats), byrow = TRUE, nrow = time.periods)
  colnames(tstats)=secNames
  #tstatsTs = xts(tstats,order.by=as.yearmon(names(ffmObj$r2)))
  }
  
  tstatsTs = xts(tstats,order.by=as.yearmon(names(ffmObj$r2)))
  # COUNT NUMBER OF RISK INDICES WITH SIGNIFICANT T-STATS EACH MONTH
  #(Modified code using xts obj in ifelse to bypass bug in xts package v0.10)
  sigTstats = as.matrix(rowSums(ifelse(abs(tstats) > z.alpha,1,0)))
  sigTstatsTs = xts(sigTstats,order.by=as.yearmon(names(ffmObj$r2)))
  
  pos.sigTstatsTs = as.matrix(colSums(ifelse((tstats) > z.alpha,1,0)))
  #pos.sigTstatsTs = xts(pos.sigTstats,order.by=as.yearmon(names(ffmObj$r2)))
  neg.sigTstatsTs = as.matrix(colSums(ifelse((tstats) < -z.alpha,1,0)))
  Toal.sigTstats = as.matrix(colSums(ifelse(abs(tstats) > z.alpha,1,0)))
  
  combined.sigTstats = cbind(  neg.sigTstatsTs,pos.sigTstatsTs, Toal.sigTstats)
  colnames(combined.sigTstats) = c( "Negative", "Positive", "Total")
  sum.significant = apply(combined.sigTstats, 2, FUN = sum)[[3]]
  percent.sigTstats = as.data.frame((100/sum.significant)*combined.sigTstats[,-3])
  #percent.sigTstats = rbind(percent.sigTstats,"TOTAL" = colSums(percent.sigTstats))
  percent.sigTstats$var = rownames(combined.sigTstats)
  

  
  if(isPlot)
  {
    panel =  function(...){
      panel.abline(h=z.alpha,lty = 3, col = "red")
      panel.abline(h=-z.alpha,lty = 3, col = "red")
      panel.xyplot(...)
    }
    if(whichPlot == "significantTstatsLikert")
    {
      
        plt = likert(var ~ ., percent.sigTstats,
                     scales=list(y=list(cex=stripText.cex), x=list(cex=axis.cex)),
                     positive.order=TRUE, 
                     between=list(y=0),
                     strip=FALSE, strip.left=FALSE,
                     #par.strip.text=list(cex=stripText.cex, lines=3),
                     main="significant t-stats",rightAxis=FALSE,
                     ylab=NULL,  xlab='Total significance %')
        print(plt)
    }
    if(whichPlot == "significantTstatsH")
    {
        combined.sigTstatsH = combined.sigTstats[,c(3,1,2)]
        mydata = as.data.table(t(combined.sigTstatsH))
        mydata$id <- c("Total", "Negative","Positive")
        mydata$id  = factor(mydata$id , levels = c("Total", "Negative","Positive"))
        dat <- melt(mydata, id.vars = "id")
        my.settings <- list(
          superpose.polygon=list(col=c("grey", "red","black"), border="transparent"),
          strip.border=list(col="black") 
        )
        plt = barchart(~value|variable,group = (id),data=dat,par.settings = my.settings,layout = layout,
                       main="Significant t-stats", ylab="Type", xlab="Total significance %",
                       auto.key=list(space="right",points=FALSE, rectangles=TRUE,
                                     title="Significant type", cex.title=1),
                       scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black", font=2, cex = stripText.cex))
        print(plt)
    }
    if(whichPlot == "all" | whichPlot == "significantTstatsV")
    {
        combined.sigTstatsV = combined.sigTstats[,c(3,1,2)]
        mydata = as.data.table(t(combined.sigTstatsV))
        mydata$id <- c("Total", "Negative","Positive")
        mydata$id  = factor(mydata$id , levels = c("Positive", "Negative","Total"))
        dat <- melt(mydata,id.vars = "id")
        my.settings <- list(
          superpose.polygon=list(col=c("black", "red","grey"), border="transparent"),
          strip.border=list(col="black") 
        )
        
        plt = barchart(value~(id)|variable,group = (id),data=dat,origin=0,stack =TRUE,
                       main="Significant t-stats", xlab="Type", ylab="Total significance %",
                       par.settings = my.settings,layout = layout,
                       auto.key=list(space="right",points=FALSE, rectangles=TRUE,
                                         title="Significant type", cex.title=1),
                       scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black", font=2, cex = stripText.cex))
        print(plt)
    }


    if(whichPlot == "all" | whichPlot == "tStats")
    {
      #par(mfrow= c(3,1))
      # PLOT T-STATS WITH XYPLOT
      if(title) title.tstats = "t statistic values " else title.tstats = " " 
      
      plt <- xyplot(tstatsTs, panel = panel, type = type, scales = list(y = list(cex = axis.cex, relation = scale), x = list(cex = axis.cex)),
                    layout = layout, main = title.tstats , col = color[1], lwd = lwd, strip.left = T, strip = F,par.strip.text=list(col="black", cex = stripText.cex))
      print(plt)
      #par(mfrow= c(1,1))
    }
  }
  out = list("tstats" =round(tstatsTs, digits), "z.alpha" =z.alpha)
  if(isPrint){print(out)}else invisible(out)
  
}
