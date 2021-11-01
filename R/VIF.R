#' @title  Factor Model Variance Inflaction Factor Values
#'
#' @description Calculate and plot the Factor Model Variance Inflaction Factor Values for a fitted model.
#'                  A VIF for a single explanatory variable (style factor) is obtained using the time series of R-squared values obtained from 
#'                  the regression of that variable against all other explanatory variables.
#'                  So, at least 2 explanatory variables are required in \code{exposure.vars} of fitted model to find the VIF.
#'
#' @importFrom stats lm
#' @importFrom xts xts
#'
#' @param ffmObj   an object of class \code{ffm} produced by \code{fitFfm}
#' @param digits   an integer indicating the number of decimal places to be used for rounding. Default is 2.
#' @param isPrint  logical. if \code{TRUE}, the time series of the computed factor model values is printed along with their mean values.
#'                 Else, only the mean values are printed. Default is \code{TRUE}.
#' @param isPlot   logical. if \code{TRUE}, the time series of the output is plotted. Default is \code{TRUE}.
#' @param lwd      line width relative to the default. Default is 2.
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param title    logical. This argument is mainly used for the documentation purpose when you need a plot without any title.
#'                 If \code{TRUE}, the plots will have the main tiltle. default is \code{TRUE}.
#' 
#' @param ...      potentially further arguments passed.
#' @author Avinash Acharya
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
#' require(FactorAnalytics)
#'  fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN",
#'               date.var="DATE", exposure.vars=c("SECTOR", "P2B", "EV2S", "MKTCAP"))
#'
#' #Plot and print the time series of VIF values
#'  vif(fit,isPrint=TRUE)
#' @export

vif <- function(ffmObj, digits=2, isPrint=T, isPlot =T, lwd =2,stripText.cex =1,axis.cex=1, title = TRUE, ...)
{ 
  # check input object validity
  if (!inherits(ffmObj, c("tsfm", "sfm", "ffm"))) 
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
    
  n.assets <- length(ffmObj$asset.names)
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
  for(i in 1:time.periods)
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
    tsPlotMP(0.01*vifs.xts,stripLeft = TRUE, layout = c(1,ncols), scaleType = "same",stripText.cex = stripText.cex,
             axis.cex = axis.cex,color = "blue", yname = "", lwd = lwd, main =title.vif, type = "h")
  }
  vifs.xts = round(vifs.xts,digits = digits)
  out<-  list("Mean.VIF" = vifs.mean)
  ret<-  list("VIF" = vifs.xts)

  if(isPrint)
  {
    print(c(out, ret))
  }else{
    print(out)
    invisible(c(out, ret))
  }
}