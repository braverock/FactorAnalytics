#' @title Documentation for \code{tsfm} class object
#' 
#' @description This help file describes the object of class \code{tsfm} 
#' returned by \code{fitTSFM} and the associated method functions 
#' available.
#' 
#' At present, the \code{tsfm} object supports the following S3 generic methods:
#' \describe{
#' \item{plot.tsfm}{Plots the chosen characteristic of a fitted time series 
#' factor model for an individual or group of assets.}
#' \item{predict.tsfm}{Produces predicted values based on the factor model.}
#' \item{print.tsfm}{Prints the call, factor model dimension, 
#' regression coefficients, r-squared and residual volatilities from the 
#' fitted object.}
#' \item{summary.tsfm}{Produces summary statistics for each fitted model}
#' }
#' 
#' And, the following extractor functions:
#' \describe{
#' \item{coef.tsfm}{an N x (K+1) matrix of all coefficients}
#' \item{cov.tsfm}{an N x N covariance matrix of asset returns}
#' \item{fitted.tsfm}{an N x T data object of fitted values}
#' \item{residuals.tsfm}{an N x T data object of residuals}
#' }
#' 
#' @author Sangeetha Srinivasan
#' 
#' @seealso \code{\link{fitTSFM}}
#' 
#' @examples
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=colnames(managers.df[,7:9]), 
#'                market.name="SP500.TR",
#'                data=data, fit.method="OLS", variable.selection="none", 
#'                add.up.market=TRUE, add.market.sqd=TRUE)
#' print(fit)
#' 