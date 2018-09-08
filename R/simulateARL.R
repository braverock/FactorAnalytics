#' @title Simulation for thresholds of the Lindley's recursion
#'
#' @description This function simulates thresholds of Lindley's recursion
#' used in the function \code{cusumActMgr}.
#'
#' @details Simulate the thresholds used in \code{cusumActMgr}
#' 
#' @importFrom xts is.xts first
#' @importFrom stats rnorm
#'
#' @param mu A numeric value that determines the information ratio we want to
#' simulate thresholds for. No default value is set.
#' @param Threshold A numeric value that determines the threshold for the Lindley's
#' recursion to be updated in the recursion. No default value is set.
#' @param k A numeric value that determines the level of cut-off. Default is 3.
#' @param delta A numeric value representing the simulation accuracy.
#' In other words, we will simulate until k * sigma / mu < precision.
#' @param EW_constant A numeric value representing the ratio between the former sigma
#' and the new sigma. The default is set to 0.9
#' @param Fixed_Sigma The logical value representing if the sigma should be constant of not.
#' 0 represents fixed sigma, 1 represents weighted sigma. The default is set to 1.
#'
#' @return \code{simulateARL} returns a \code{vector} of the following:
#' \item{ARL}{The average return length}
#' \item{s}{The standard deviation of the ARLs}
#'
#' @author Thomas Philips, Chindhanai Uthaisaad.
#'
#' @references
#' Philips, T. K., Yashchin, E. and Stein, D. M. (2003). "Using Statistical
#' Process Control to Monitor Active Managers", Journal of Portfolio Management,
#' Fall 2003, pp. 86-94.
#'
#' @examples
#' Lower_Threshold = 1.00
#' Upper_Threshold = 11.0
#' #Monthly mu's for monthly sigma=1, annualized IR = +0.5, 0
#' mu = c(0.5, 0) / sqrt(12)
#' Seq_M = 30:40
#' Thresholds = Lower_Threshold + Seq_M * (Upper_Threshold - Lower_Threshold) / 100
#' Threshold_upper = sapply(Thresholds, FUN = simulateARL, mu = mu[1], delta = 0.05)
#' Threshold_lower = sapply(Thresholds, FUN = simulateARL, mu = mu[2], delta = 0.05)
#'
#' @export

############################< MAIN CODE >###############################
# This function simulates the threshold for Lindley's recursion

simulateARL = function(mu, Threshold, delta, k = 3, EW_constant = 0.9, Fixed_Sigma = 1){
  N_Events          = 0
  Sum               = 0
  SumSq             = 0
  #Set it high so that it doesn't exit the loop right away
  ThreeSigmaOverMu  = delta + 1

  while(ThreeSigmaOverMu > delta){
    L         = 0
    N         = 0
    r_n_1     = 0
    sigma_n   = 1
    sigma_n_1 = 1

    while (L < Threshold) {
      r_n = rnorm(n = 1, mean = mu, sd = 1)
      if ((N > 1) && (Fixed_Sigma == 1)) {
        sigma_n = sqrt( EW_constant * sigma_n_1^2 +
                        (1 - EW_constant) * 0.5 * (r_n - r_n_1)^2 )
      } else {
        sigma_n = 1
      }

      IR_hat    = r_n / sigma_n_1
      sigma_n_1 = sigma_n
      r_n_1     = r_n
      L         = max(0, L - IR_hat + mean(mu))
      N         <- N + 1
    }

    N_Events <- N_Events + 1
    Sum      <- Sum + N
    SumSq    <- SumSq + N^2
    ARL <- Sum / N_Events

    Sigma = ifelse(N_Events < 10, 100000, sqrt( (SumSq - Sum^2 / N_Events) / (N_Events * (N_Events - 1))) )
    ThreeSigmaOverMu = k * Sigma / ARL
  }

  #Std. deviation of the ARLs
  s = Sigma * sqrt(N_Events)
  return(c(ARL, s))
}

