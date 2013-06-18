#' Compute Implied Factor Returns Using Covariance Matrix Approach
#' 
#' Compute risk factor conditional mean returns for a one group of risk factors
#' given specified returns for another group of risk factors based on the
#' assumption that all risk factor returns are multivariately normally
#' distributed.
#' 
#' Let \code{y} denote the \code{m x 1} vector of factor scenarios and \code{x}
#' denote the \code{(n-m) x 1} vector of other factors. Assume that \code{(y',
#' x')'} has a multivariate normal distribution with mean \code{(mu.y',
#' mu.x')'} and covariance matrix partitioned as \code{(cov.yy, cov.yx, cov.xy,
#' cov.xx)}. Then the implied factor scenarios are computed as \code{E[x|y] =
#' mu.x + cov.xy*cov.xx^-1 * (y - mu.y)}
#' 
#' @param factor.scenarios \code{m x 1} vector of factor mean returns of
#' scenario. m is a subset of the n, where n is risk factors and \code{n > m}.
#' @param mu.factors \code{n x 1} vector of factor mean returns.
#' @param cov.factors \code{n x n} factor covariance matrix.
#' @return \code{(n - m) x 1} vector of implied factor returns
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' # get data 
#' data(managers.df)
#' factors    = managers.df[,(7:9)]
#' # make up a factor mean returns scenario for factor SP500.TR 
#' factor.scenarios <- 0.001 
#' names(factor.scenarios) <- "SP500.TR"
#' mu.factors <- mean(factors)
#' cov.factors <- var(factors)
#' # implied factor returns
#' impliedFactorReturns(factor.scenarios,mu.factors,cov.factors)
#' 
impliedFactorReturns <-
function(factor.scenarios, mu.factors, cov.factors) {
## inputs:
## factor.scenarios     m x 1 vector of factor mean returns of scenario. m is a subset of the n, where n is 
##                      risk factors and n > m.                   
## mu.factors           n x 1 vector of factor mean returns
## cov.factors          n x n factor covariance matrix
## outputs:
## (n - m) x 1 vector of implied factor returns
##  details
## Let y denote the m x 1 vector of factor scenarios and x denote the (n-m) x 1
## vector of other factors. Assume that (y', x')' has a multivariate normal
## distribution with mean (mu.y', mu.x')' and covariance matrix
##
##                 | cov.yy, cov.yx |
##                 | cov.xy, cov.xx |
##
## Then the implied factor scenarios are computed as
##
##                 E[x|y] = mu.x + cov.xy*cov.xx^-1 * (y - mu.y)
##
  factor.names = colnames(cov.factors)
  scenario.names = names(factor.scenarios)
  non.scenario.names = setdiff(factor.names, scenario.names)
  # m x m matrix
  cov.scenarios = cov.factors[scenario.names, scenario.names]
  # (n-m) x m matrix
  cov.non.scenarios.scenarios = cov.factors[non.scenario.names, scenario.names]
  # compute (n-m) x 1 vector of implied factor returns from conditional distribution
  mu.non.scenarios = mu.factors[non.scenario.names] + cov.non.scenarios.scenarios %*% solve(cov.scenarios) %*% (factor.scenarios - mu.factors[scenario.names])
  mu.non.scenarios = as.numeric(mu.non.scenarios)
  names(mu.non.scenarios) = non.scenario.names
  return(mu.non.scenarios)
}

