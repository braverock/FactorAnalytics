#' Functions for Cornish-Fisher density, CDF, random number simulation and
#' quantile.
#'
#'@name  CornishFisher
#'@aliases CornishFisher
#'@aliases rCornishFisher 
#'@aliases dCornishFisher
#'@aliases qCornishFisher
#'@aliases pCornishFisher
#'
#'
#'@description 
#' \itemize{
#' \item \code{rCornishFisher} simulate observations based on
#' Cornish-Fisher quantile expansion given mean, standard
#' deviation, skewness and excess kurtosis.
#' \item \code{dCornishFisher} Computes Cornish-Fisher density
#' from two term Edgeworth expansion given mean, standard
#' deviation, skewness and excess kurtosis.
#' \item \code{pCornishFisher} Computes Cornish-Fisher CDF from
#' two term Edgeworth expansion given mean, standard
#' deviation, skewness and excess kurtosis.
#' \item \code{qCornishFisher} Computes Cornish-Fisher quantiles
#' from two term Edgeworth expansion given mean, standard
#' deviation, skewness and excess kurtosis.
#'}
#'
#'@param n Scalar, number of simulated values in rCornishFisher. Sample length in
#' density,distribution,quantile function.
#' @param sigma Scalar, standard deviation.
#' @param skew Scalar, skewness.
#' @param ekurt Scalar, excess kurtosis.
#' @param seed Set seed here. Default is \code{NULL}.
#' @param x,q Vector of standardized quantiles. See detail.
#' @param p Vector of probabilities.
#' 
#' @return n Simulated values from Cornish-Fisher distribution.
#' @author Eric Zivot and Yi-An Chen.
#' @references 
#' \enumerate{
#' \item A.DasGupta, "Asymptotic Theory of Statistics and
#' Probability", Springer Science+Business Media,LLC 2008
#' \item   Thomas A.Severini, "Likelihood Methods in Statistics",
#'  Oxford University Press, 2000 
#'  }
#'  @export
#'  
#'  @details CDF(q) = Pr(sqrt(n)*(x_bar-mu)/sigma < q)
#'  
#'  @examples
#'  \dontrun{
#'  # generate 1000 observation from Cornish-Fisher distribution
#' rc <- rCornishFisher(1000,1,0,5)
#'hist(rc,breaks=100,freq=FALSE,main="simulation of Cornish Fisher Distribution",
#'     xlim=c(-10,10))
#'lines(seq(-10,10,0.1),dnorm(seq(-10,10,0.1),mean=0,sd=1),col=2)
#' # compare with standard normal curve
#'
#' # example from A.dasGupta p.188 exponential example
#' # x is iid exp(1) distribution, sample size = 5
#' # then x_bar is Gamma(shape=5,scale=1/5) distribution
#' q <- c(0,0.4,1,2)
#' # exact cdf
#' pgamma(q/sqrt(5)+1,shape=5,scale=1/5)
#' # use CLT
#' pnorm(q)
#' # use edgeworth expansion
#' pCornishFisher(q,n=5,skew=2,ekurt=6)
#' }



rCornishFisher <-
function(n, sigma, skew, ekurt, seed=NULL) {
## inputs:
## n          scalar, number of simulated values
## sigma      scalar, standard deviation
## skew       scalar, skewness
## ekurt      scalar, excess kurtosis
## outputs:
## n simulated values from Cornish-Fisher distribution
if (!is.null(seed)) set.seed(seed)
zc = rnorm(n)
z.cf = zc  + (((zc^2 - 1) * skew)/6) + (((zc^3 - 3 * zc) *
      ekurt)/24) - ((((2 * zc^3) - 5 * zc) * skew^2)/36)
ans = sigma*z.cf
ans
}
