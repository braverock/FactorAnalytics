#' @title Plot actual against fitted values of up and down market time series factor model
#' 
#' @description Generic \code{plot} method for object of class \code{tsfmUpDn}.
#' 
#' @details 
#' This method plots actual values against fitted value of up and down market time series
#' factor model. The black dots are actual values and the red lines are fitted values.
#' 
#' For other types of plots, use the list objects \code{Up} and \code{Dn} of class \code{tsfmUpDn}. 
#' The \code{plot.tsfm} can be applied.
#' 
#' @param object an object of class \code{tsfmUpDn} produced by \code{fitTsfmUpDn}.
#' @param mkt.name The name of market returns used in \code{fitTsfmUpDn}. It is necessary
#' to provide the name of market returns. 
#' @param assets.name The name of the asset used in \code{fitTsfmUpDn}. It supports one
#' asset name at once.   
#' @param ... Other arguments can be used in \code{plot}. 
#' @author Yi-An Chen
#' 
#' @seealso \code{\link{fitTsfmUpDn}} 
#' 
#' @examples
#' 
#' # load data from the database
#' data(managers)
#' # example: Up and down market factor model with OLS fit
#' fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="OLS",control=NULL)
#'  # plot the fitted model of the first asset
#'  plot(fitUpDn,mkt.name="SP500.TR",assets.name="HAM1")
#' 
#' 
#' @method plot tsfmUpDn
#' @export


plot.tsfmUpDn <- function(object,mkt.name,assets.name,...) {
  
  if (is.null(mkt.name)){
    stop("Missing argument: mkt.name has to be specified for plot method.")
  } 
  
  if (is.null(assets.name)){
    stop("Missing argument: assets.name has to be specified.")
  } 
  
  if (!(mkt.name %in% object$Up$factor.names)) {
    stop("mkt.name has to the same mkt.name used in fitTsfmUpDn().")
  } 
 
  # extract info from the fitTsfm object
  plotDataUp <- merge.xts(object$Up$data[,c(assets.name,mkt.name)], fitted(object$Up)[,assets.name])
  colnames(plotDataUp) <- c("ActualUp","MktUp","FittedUp")
  plotDataDn <-merge.xts(object$Dn$data[,c(assets.name,mkt.name)], fitted(object$Dn)[,assets.name])
  colnames(plotDataDn) <- c("ActualDn","MktDn","FittedDn")
  
  plot(rbind(coredata(plotDataUp$MktUp),coredata(plotDataDn$MktDn)),
       rbind(coredata(plotDataUp$ActualUp),coredata(plotDataDn$ActualDn)),
       main = paste("Actual vs Fitted values of the Asset ",assets.name,sep=""),
       xlab=mkt.name,ylab=assets.name,...)
  abline(v=0)
  lines(coredata(plotDataUp$MktUp),coredata(plotDataUp$FittedUp),col="blue")
  lines(coredata(plotDataDn$MktDn),coredata(plotDataDn$FittedDn),col="blue")
  abline(h=0)
 
  
}