#' @title Documentation for \code{tsfm} class object
#' 
#' @description This help file describes the object of class \code{tsfm} 
#' returned by \code{fitTSFM} and the associated method functions 
#' available.
#' 
#' At present, the \code{tsfm} object supports the following S3 generic methods:
#' \describe{
#' \item{plot.tsfm}{Plots the fitted time series factor model for assets.}
#' \item{predict.tsfm}{Produces predicted values based on the factor model.}
#' \item{print.tsfm}{Prints the call, factor model dimension, 
#' regression coefficients, r-squared and residual volatilities from the 
#' fitted object.}
#' \item{summary.tsfm}{Produces summary statistics for each fitted model}
#' }
#' 
#' And, the following extractor functions:
#' \describe{
#' \item{coef.tsfm} {matrix of coefficients from the fitted factor models for 
#' each of the assets}
#' \item{fitted.tsfm} {data object of fitted values from the fitted factor 
#' models for each of the assets}
#' \item{residuals.tsfm} {data object of residuals from the fitted factor 
#' models for each of the assets}
#' }
#' 
#' @author Sangeetha Srinivasan
#' 
#' @seealso \code\link{fitTSFM}
#' 
#' @examples
#' \dontrun{
#' data <- managers.df
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=colnames(data[,7:9]), market.name="SP500.TR",
#'                data=data, fit.method="OLS", variable.selection="none", 
#'                add.up.market=TRUE, add.market.sqd=TRUE)
#' print(fit)
#' }
#' 