#' @title Summarizing a fitted up and down market time series factor model
#' 
#' @description \code{summary} method for object of class \code{tsfmUpDn}. 
#' Returned object is of class {summary.tsfmUpDn}. This function provides a \code{summary}
#' method to an object returned by a wrapper function \code{fitTsfmUpDn}.
#' 
#' @details Since \code{fitTsfmUpDn} fits both up market and down market,
#' \code{summary.tsfmUpDn} applies \code{summary.tsfm} for both markets fitted 
#' objects and combines the coefficients interested together. 
#' 
#'   
#' @param object an object of class \code{tsfmUpDn} returned by \code{fitTsfmUpDn}.
#' @param ... futher arguments passed to or from \code{summary.tsfm} methods.
#' @param uD.list an object of class \code{summary.tsfmUpDn}.
#' @param digits number of significants digits to use when printing. 
#' Default is 3.
#' 
#' @return Returns an object of class \code{summary.tsfmUpDn}. This object contains 
#' a list object of \code{Up} and \code{Dn} for up market and down market respectively.
#'    
#' The print method for class \code{summary.tsfmUpDn} outputs the call, 
#' coefficients (with standard errors and t-statistics), r-squared and 
#' residual volatilty (under the homoskedasticity assumption) for all assets in up and 
#' down market. 
#' 
#' Object of class \code{summary.tsfmUpDn} is a list of 2 containing:
#' \item{Up}{A list of the up market fitted object. It is a class of \code{summary.tsfm}}
#' \item{Dn}{A list of the down market fitted object. It is a class of \code{summary.tsfm}} 
#' 
#' @author Yi-An Chen and Sangeetha Srinivasan.
#' 
#' @seealso \code{\link{fitTsfmUpDn}}, \code{\link[stats]{summary.tsfm}}
#' 
#' @examples
#' # load data from the database
#' data(managers)
#' 
#' # example: Up and down market factor model with OLS fit
#' fitUpDn <- fitTsfmUpDn(asset.names=colnames(managers[,(1:6)]),mkt.name="SP500.TR",
#'                        data=managers, fit.method="OLS",control=NULL)
#'  
#'  summary(fitUpDn)
#' 
#' @importFrom lmtest coeftest.default
#' @importFrom sandwich vcovHC.default vcovHAC.default
#' 
#' @method summary tsfmUpDn
#' @export

summary.tsfmUpDn <- function(object,...) {
  # check input object validity
  if (!inherits(object, "tsfmUpDn")) {
    stop("Invalid 'tsfmUpDn' object")
  }
  uD.list <- lapply(object,summary,...)
  class(uD.list) <- "summary.tsfmUpDn" 
  return(uD.list)
}  
 #' @rdname summary.tsfmUpDn
 #' @method print summary.tsfmUpDn
 #' @export
 
 
 print.summary.tsfmUpDn <- function(uD.list, digits=3, ...) {
   if(!is.null(cl <- uD.list$Up$call)) {
     cat("\nCall:\n")
     dput(cl)
   }
   cat("\nFactor Model Coefficients:\n", sep="")
   n <- length(uD.list$Up$sum.list)
   for (i in 1:n) {
     options(digits = digits)  
     table.coef.Up <- (uD.list$Up$sum.list)[[i]]$coefficients
     rownames(table.coef.Up) <- sapply(rownames(table.coef.Up),function(x) paste(x,"_Up",sep="") )
     table.coef.Dn <- (uD.list$Dn$sum.list)[[i]]$coefficients
     rownames(table.coef.Dn) <- sapply(rownames(table.coef.Dn),function(x) paste(x,"_Dn",sep="") )
     table.coef <- rbind(table.coef.Up,table.coef.Dn)
     if (dim(table.coef)[2] > 1) {
       cat("\nAsset", i, ": ", names(uD.list$Up$sum.list[i]), "\n(", uD.list$Up$se.type, 
           " Standard Errors & T-stats)\n\n", sep="")  
     } else {
       cat("\nAsset", i, ": ", names(uD.list$Up$sum.list[i]), "\n\n", sep="")  
     }
     r2Up <- uD.list$Up$sum.list[[i]]$r.squared
     sigmaUp <- uD.list$Up$sum.list[[i]]$sigma
     r2Dn <- uD.list$Dn$sum.list[[i]]$r.squared
     sigmaDn <- uD.list$Dn$sum.list[[i]]$sigma
     printCoefmat(table.coef, digits=digits,...)
     cat("\n R-squared_Up: ", r2Up,", Residual Volatility_Up: ", sigmaUp,"\n",
         "R-squared_Dn: ", r2Dn,", Residual Volatility_Dn: ", sigmaDn)
   }
}