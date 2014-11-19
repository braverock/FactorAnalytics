#' @title Density, distribution function, quantile function and random 
#' generation for the Cornish-Fisher distribution.
#'
#' @details CDF(q) = Pr(sqrt(n)*(x_bar-mu)/sigma < q)
#' \itemize{
#' \item \code{dCornishFisher} Computes Cornish-Fisher density from two term 
#' Edgeworth expansion given mean, standard deviation, skewness and excess 
#' kurtosis.
#' \item \code{pCornishFisher} Computes Cornish-Fisher CDF from two term 
#' Edgeworth expansion given mean, standard deviation, skewness and excess 
#' kurtosis.
#' \item \code{qCornishFisher} Computes Cornish-Fisher quantiles from two term 
#' Edgeworth expansion given mean, standard deviation, skewness and excess 
#' kurtosis.
#' \item \code{rCornishFisher} simulate observations based on Cornish-Fisher 
#' quantile expansion given mean, standard deviation, skewness and excess 
#' kurtosis.}
#'
#' @param n scalar; number of simulated values in random simulation, sample 
#' length in density, distribution and quantile functions.
#' @param sigma scalar standard deviation.
#' @param skew scalar; skewness.
#' @param ekurt scalar; excess kurtosis.
#' @param seed scalar; set seed. Default is \code{NULL}.
#' @param x,q vector of standardized quantiles.
#' @param p vector of probabilities.
#' 
#' @return 
#' \code{dCornishFisher} gives the density, \code{pCornishFisher} gives the 
#' distribution function, \code{qCornishFisher} gives the quantile function, 
#' and \code{rCornishFisher} generates \code{n} random simulations.
#' 
#' @author Eric Zivot and Yi-An Chen.
#' 
#' @references 
#' DasGupta, A. (2008). Asymptotic theory of statistics and probability. 
#' Springer.
#' Severini, T. A., (2000). Likelihood Methods in Statistics. Oxford University 
#' Press.
#'  
#' @examples
#' \dontrun{
#' # generate 1000 observation from Cornish-Fisher distribution
#' rc <- rCornishFisher(1000,1,0,5)
#' hist(rc, breaks=100, freq=FALSE, 
#'      main="simulation of Cornish Fisher Distribution", xlim=c(-10,10))
#' lines(seq(-10,10,0.1), dnorm(seq(-10,10,0.1), mean=0, sd=1), col=2)
#' # compare with standard normal curve
#'
#' # exponential example from A.dasGupta p.188
#' # x is iid exp(1) distribution, sample size = 5
#' # then x_bar is Gamma(shape=5, scale=1/5) distribution
#' q <- c(0,0.4,1,2)
#' # exact cdf
#' pgamma(q/sqrt(5)+1, shape=5, scale=1/5)
#' # use CLT
#' pnorm(q)
#' # use edgeworth expansion
#' pCornishFisher(q, n=5, skew=2, ekurt=6)
#' }
#'
#' @rdname CornishFisher
#' @export

dCornishFisher <- function(x, n,skew, ekurt) {
  density <- dnorm(x) + 
    1/sqrt(n)*(skew/6*(x^3 - 3*x))*dnorm(x) +
    1/n*( (skew)^2/72*(x^6 - 15*x^4 + 45*x^2 - 15) + ekurt/24*(x^4 - 6*x^2 + 3) )*dnorm(x)
  return(density)
}

#' @rdname CornishFisher
#' @export

pCornishFisher <- function(q, n, skew, ekurt) {
  zq <- q 
  CDF <- pnorm(zq) + 
    1/sqrt(n)*(skew/6 * (1-zq^2))*dnorm(zq) + 
    1/n*( (ekurt)/24*(3*zq-zq^3) + (skew)^2/72*(10*zq^3-15*zq-zq^5) )*dnorm(zq)
  return(CDF)
}

#' @rdname CornishFisher
#' @export

qCornishFisher <- function(p,n,skew, ekurt) {
  zq <- qnorm(p)
  q.cf <- zq  + 
    1/sqrt(n)*(((zq^2 - 1) * skew)/6) + 
    1/n*( (((zq^3 - 3*zq) * ekurt)/24) - ((((2*zq^3) - 5*zq) * skew^2)/36) )
  return(q.cf) 
}

#' @rdname CornishFisher
#' @export

rCornishFisher <- function(n, sigma, skew, ekurt, seed=NULL) {
  
  ## inputs:
  ## n          scalar, number of simulated values
  ## sigma      scalar, standard deviation
  ## skew       scalar, skewness
  ## ekurt      scalar, excess kurtosis
  ## outputs:
  ## n simulated values from Cornish-Fisher distribution
  
  if (!is.null(seed)) set.seed(seed)
  zc <- rnorm(n)
  z.cf <- zc + 
    (((zc^2 - 1) * skew)/6) + 
    (((zc^3 - 3*zc)*ekurt)/24) - 
    ((((2*zc^3) - 5*zc)*skew^2)/36)
  return(sigma*z.cf)
}
