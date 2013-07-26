#' summary method for StatFactorModel object.
#' 
#' Generic function of summary method for fitStatisticalFactorModel. 
#' 
#' 
#' @param fit.stat fit.stat object created by fitStatisticalFactorModel.
#' @param digits Integer indicating the number of decimal places. Default is 3.
#' @param ... other option used in \code{summary.lm}
#' @author Yi-An Chen.
#' @examples
#' 
#' # load data from the database
#' data(managers.df)
#' # fit the factor model with OLS
#' fit <- fitStatisticalFactorModel(fitStatisticalFactorModel(sfm.dat,k=2,
#'                                   ckeckData.method="data.frame"))
#' summary(fit)
#' 
#' @export
#' 
summary.StatFactorModel <- function(fit.stat,digits=3){
  if(!is.null(cl <- fit.stat$call)) {
    cat("\nCall:\n")
    dput(cl)
  }
  cat("\nFactor Betas\n")
  n <- length(fit.stat$assets.names)
  for (i in 1:n) {
    options(digits = digits)  
    cat("\n", fit.stat$assets.names[i], "\n")  
    table.macro <- t(summary(fit.stat$asset.fit[[i]])$coefficients)
    colnames(table.macro)[1] <- "alpha"
    print(table.macro,digits = digits)
    cat("\nR-square =", fit.stat$r2[i] ,",residual variance ="
        , fit.stat$resid.variance[i],"\n")
  }
}

