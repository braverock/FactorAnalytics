#' summary method for StatFactorModel object.
#' 
#' Generic function of summary method for \code{fitStatisticalFactorModel}. 
#' 
#' 
#' @param object An Object created by \code{fitStatisticalFactorModel}.
#' @param digits Integer indicates the number of decimal places. Default is 3.
#' @param ... other option used in \code{print} method.
#' @author Yi-An Chen.
#' @method summary StatFactorModel
#' @export
#' @examples
#' 
#' # load data from the database
#' data(stat.fm.data)
#' # fit the factor model with OLS
#' fit <- fitStatisticalFactorModel(sfm.dat,k=2)
#' summary(fit)
#' 
#' 
summary.StatFactorModel <- function(object,digits=3,...){
  if(!is.null(cl <- object$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Betas\n")
  n <- length(object$assets.names)
  for (i in 1:n) {
    options(digits = digits)  
    cat("\n", object$assets.names[i], "\n")  
    table.macro <- t(summary(object$asset.fit[[i]])$coefficients)
    colnames(table.macro)[1] <- "alpha"
    print(table.macro,digits = digits,...)
    cat("\nR-square =", object$r2[i] ,",residual variance ="
        , object$resid.variance[i],"\n")
  }

}

