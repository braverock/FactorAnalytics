#' @title Using Statistical Process Control to Monitor Active Management
#'
#' @description Monitor the risk adjusted performance (Information Ratio) of an actively managed portfolio and
#' raise an alarm when sufficient evidence has accrued to indicate that its current Information Ratio is 0 or worse.
#' The monitorng is performed using an optimal changepoint detection scheme (the CUSUM algorithm)
#' An object of class \code{cusumActMgr} is returned.
#'
#' @details
#' Assessing the performance of the active managers is hard because active returns (i.e. portfolio return - benchmark return) are noisy.
#' In addition, the risk of these active returns must be taken account of, and this is commonly (though not universally) done by measuring
#' the standard deviation of active returns. Empirical studies of active managers across a wide range of asset classes suggest that an
#' Annualized Information Ratio (IR = Active Return / Std. Dev.(Active Return)) of 0.5 over a period of 5 years or more is exceptional.
#' In addition, public markets are very efficient, and known inefficiencies disappear as they get arbitraged away, though other
#' inefficiencies sometimes appear in their place. Consequently, the majority of active managers deliver active returns and IR close to 0, and even
#' those with a positive IR are at constant risk of having their added value dissipate. Investors, therefore, must continually estimate
#' the current performance of their active portfolios and determine when sufficient evidence has accrued to suggest that their active return
#' and IR have fallen to 0 (or turned negative). Put differently, investors need to reapidly detect changes, particularly negative changes,
#' in the performance of their portfolios.
#'
#' There is a rich literature on changepoint detection, and of the many available algorithms to detect changepoints, the CUSUM (an acronym for CUmulative SUM)
#' stands out on account of its simplicity, its robustness to the actual distribution of active returns, and the optimal trade-off between detection time and
#' the rate of false alarms that it offers. It is closely retlated to Wald's Sequential Probability Ratio Test (SPRT) but is much simpler to implement,
#' and requires minimal inputs from the user. In this application, it seeks to determine when the IR of a portfolio has changed from a good level (default = 0.5 )
#' to a bad level (default = 0). An alarm is raised when the CUSUM scheme crosses a threshold, which is chosen to make the average time between false alarms
#' 84 months (7 years). By way of comparison, the time taken to detect a transition from good performance to bad is 41 months (or 3 1/2 years). This is much faster
#' than the traditional t-test, which would take 16 years to obtain a t-statistic of 2. The threshold can be recalibrated to meet a user's needs.
#'
#'
#' @param portfolioName a character representing the name of the fund.
#' It is a required argument with no default
#' @param benchmarkName a character representing the name of the benchamark
#' It is a required argument with no default
#' @param data an xts object containing the columns \code{portfolioName} and
#' \code{benchmarkName} of monthly returns. This argument is required with no default.
#' @param upperIR a numeric value representing the information ratio of a
#' good performance. The default is set to 0.5
#' @param lowerIR a numeric value representing the information ratio of a
#' bad performance. The default is set to 0
#' @param lambda_in the exponential weighting constant when the data
#'  seems consistent with the current estimate of volatility.
#' The default is set to 0.1
#' @param lambda_out the exponential weighting constant when the data
#' seems inconsistent with the current estimate of volatility.
#' The default is set to 0.2
#' @param huberize the numeric value, greater than 1, of standard deviations
#' at which we huberize. The default is set to 4.
#' @param filterStd the logical value representing the filter of the estimated
#' standard deviations. The default is set to \code{FALSE}.
#'
#'
#' @return \code{cusumActMgr} returns a \code{list} containing the following xts objects:
#' \item{Logarithmic_Excess_Returns}{Logarithmic excess returns of
#' the fund relative to the benchmark}
#' \item{Annual_Moving_Average}{The vector of annual moving average returns}
#' \item{Tracking_Error}{The monthly tracking error of the logarithmic excess returns}
#' \item{Information_Ratios}{The vector of monthly information ratios}
#' \item{Lindley's_Recursion}{The vector  Lindley's recursion with a reset after the detection threshold (6.81) is passed.}
#' \item{Annualized_Cusum_IR}{The vector annualized CUSUM of the information ratios}
#' \item{Annualized Cusum Excess Return}{The vector annualized CUSUM of the excess returns}
#' \item{Means}{The xts matrix of estimated means of the fund in the first columns,
#' the benchmark in the second column, and the excess logarithmic returns in the third column}
#' \item{Standard_deviations}{The xts matrix of estimated standard deviations of the fund in the first columns,
#' the benchmark in the second column, and the excess logarithmic returns in the third column.
#' It will not be filtered unless \code{filterStd = TRUE} is specified.}
#' \item{Protractor}{The xts matrix of the rays from IR = -3 in the first column
#' to IR = 3 in the seventh column used in the CUSUM IR as a protractor.}
#' \item{Excess_Volatility}{The annualized Standard deviations}
#'
#' @author Chindhanai Uthaisaad.
#'
#' @references
#' Philips, T. K., Yashchin, E. and Stein, D. M. (2003). "Using Statistical
#' Process Control to Monitor Active Managers", Journal of Portfolio Management,
#' Fall 2003, pp. 86-94.
#'
#' @examples
#' data("cusumData")
#' results = cusumActMgr(portfolioName = "Parvest", benchmarkName = "RUS2500", data = cusumData)
#' @export

############################< MAIN CODE >###############################

cusumActMgr <- function(portfolioName, benchmarkName, data, upperIR = 0.5,
                        lowerIR = 0, lambda_in = 0.10, lambda_out = 0.20,
                        huberize = 4, filterStd = FALSE) {

  # record the call as an element to be returned
  this.call <- match.call()

  #Check the arguments validity
  if (missing(data) || !is.xts(data)) {
    stop("Invalid args: data must be an xts object")
  }

  if(missing(portfolioName) || !is.character(portfolioName)){
    stop("Invalid args: the portfolio returns must be a charactor string")
  }

  if(missing(benchmarkName) || !is.character(benchmarkName)){
    stop("Invalid args: the benchmark returns must be a charactor string")
  }

  if(huberize < 1){
    stop("Invalid args: the huberizing parameter should be greater than 1")
  }

  if(lambda_in < 0 || lambda_in >1 || lambda_out < 0 || lambda_out > 1){
    stop("Invalid args: the lambdas must be between 0 and 1")
  }

  if(!is.logical(filterStd)){
    stop("Invalid args: filterStd must be a logical value")
  }

  #Obtain the return and benchmark
  portfolioReturns = data[, portfolioName]
  benchmarkReturns = data[, benchmarkName]
  n = length(portfolioReturns)

  if (n < 2) {
    stop("Invalid args: the portfolio returns and benchmark returns must be of length at least 2")
  }

  if (n != length(benchmarkReturns)) {
    stop("Invalid args: the portfolio returns and benchmark returns must be of the same length")
  }

  #Initialize logarithmic excess returns, IR, LLR and TE
  priorMonth = as.yearmon(first(index(portfolioReturns))) - 1 / 12
  index_TE = c(priorMonth, index(portfolioReturns))
  LR = xts(rep(0, n + 1), order.by = index_TE)

  #Compute the Logarithmic Excess Returns
  logExcessReturns = log((1 + portfolioReturns) / (1 + benchmarkReturns))

  ######< Mean and Filtered Std of the fund and benchmark >#######
  Means =  uStds = fStds = matrix(0, ncol = 3, nrow = n+1)
  Means[1,1] = ifelse(n >= 11, mean(portfolioReturns[1:11]), mean(portfolioReturns))
  Means[1,2] = ifelse(n >= 11, mean(benchmarkReturns[1:11]), mean(benchmarkReturns))
  Means[1,3] = ifelse(n >= 11, mean(logExcessReturns[1:11]), mean(logExcessReturns))
  uStds[1,1] = fStds[1,1] = ifelse(n >= 6, 1.25 * median(abs(portfolioReturns[1:6])), 1.25 * median(abs(portfolioReturns)))
  uStds[1,2] = fStds[1,2] = ifelse(n >= 6, 1.25 * median(abs(benchmarkReturns[1:6])), 1.25 * median(abs(benchmarkReturns)))
  uStds[1,3] = fStds[1,3] = ifelse(n >= 6, 1.25 * median(abs(logExcessReturns[1:6])), 1.25 * median(abs(logExcessReturns)))


  #Update the means and unfiltered standard deviations for the fund and benchmark
  for(i in 1:n){
    Means[i+1,1] = muEst(coredata(portfolioReturns[i]), Means[i,1], uStds[i,1], huberize, lambda_in)
    uStds[i+1,1] = sigmaEst(coredata(portfolioReturns[i]), Means[i+1,1], uStds[i,1], huberize, lambda_in, lambda_out)
    Means[i+1,2] = muEst(coredata(benchmarkReturns[i]), Means[i,2], uStds[i,2], huberize, lambda_in)
    uStds[i+1,2] = sigmaEst(coredata(benchmarkReturns[i]), Means[i+1,2], uStds[i,2], huberize, lambda_in, lambda_out)
    Means[i+1,3] = muEst(coredata(logExcessReturns[i]), Means[i,3], uStds[i,3], huberize, lambda_in)
    uStds[i+1,3] = sigmaEst(coredata(logExcessReturns[i]), Means[i+1,3], uStds[i,3], huberize, lambda_in, lambda_out)
  }

  Stds = uStds

  if (filterStd){
    #Filtering the standard deviations
    for (i in 1:n){
      fStds[i+1,1] = ifelse(uStds[i+1,1] > uStds[i,1], uStds[i+1,1], 0.5 * (uStds[i,1] + uStds[i+1,1]))
      fStds[i+1,2] = ifelse(uStds[i+1,2] > uStds[i,2], uStds[i+1,2], 0.5 * (uStds[i,2] + uStds[i+1,2]))
      fStds[i+1,3] = ifelse(uStds[i+1,3] > uStds[i,3], uStds[i+1,3], 0.5 * (uStds[i,3] + uStds[i+1,3]))
    }
    Stds = fStds
  }

  Means = xts(Means, order.by = index_TE)
  Stds = xts(Stds, order.by = index_TE)
  colnames(Means) = colnames(Stds) = c("Fund", "Benchmark", "Excess")

  #Excess volatility
  Vol = matrix(0, ncol = 3, nrow = n+1)
  Vol[1,1] = sqrt(12) * sd(coredata(portfolioReturns))
  Vol[1,2] = sqrt(12) * sd(coredata(benchmarkReturns))
  Vol[2:(n+1),] = Stds[2:(n+1),] * sqrt(12)

  Vol[,3] = Vol[,1] - Vol[,2]
  colnames(Vol) = c("FundVol", "BenchmarkVol", "ExcessVol")
  Vol = xts(Vol, order.by = index_TE)

  #Average level of the upper and lower IR inputs
  avgIRLevel = 0.5 * (upperIR + lowerIR) / sqrt(12)

  #####Begin looping through the new returns#####
  IR = coredata(logExcessReturns) / coredata(Stds[-(n+1), 3])
  IR = xts(IR, order.by = index(portfolioReturns))

  for (i in 1:length(portfolioReturns)) {
    #Lindley's recursion (Log-likelihood ratios)
    LR[i+1] = ifelse(coredata(LR[i]) - coredata(IR[i]) + avgIRLevel < 0, 0,
                     ifelse(coredata(LR[i]) > 6.81, max(0, avgIRLevel - IR[i]),
                            coredata(LR[i]) - coredata(IR[i]) + avgIRLevel))
  }

  #Yearly moving average returns
  AMA = xts(rep(0, n), order.by = index(portfolioReturns))
  for(i in 1:n){
    AMA[i] = ifelse(i < 12, mean(logExcessReturns[1:i]), mean(logExcessReturns[(i-11):i]))
  }

  #CUSUM IR
  cusumIR = xts(cumsum(coredata(IR)), order.by = index(IR))

  #Information obtained from annualized IR
  annualizedIR = sqrt(12) * coredata(cusumIR)
  lowerLimIR = min(annualizedIR)
  upperLimIR = max(annualizedIR)
  spreadIR = upperLimIR - lowerLimIR
  avgIR = spreadIR/n
  upperPosIR = which.max(annualizedIR)
  lowerPosIR = which.min(annualizedIR)
  medIR = lowerLimIR + 0.5 * spreadIR
  peakIR = spreadIR / (upperPosIR - lowerPosIR)
  maxIR = 0.5 * ceiling(abs(peakIR) + abs(avgIR))
  protractor_widthIR = ceiling(0.9 * spreadIR / (2 * maxIR))
  protractor_heightIR = abs(protractor_widthIR*maxIR)
  AIR = c("lowerLimIR" = lowerLimIR,
          "upperLimIR" = upperLimIR,
          "spreadIR" = spreadIR,
          "avgIR" = avgIR,
          "upperPosIR" = upperPosIR,
          "lowerPosIR" = lowerPosIR,
          "medIR" = medIR,
          "peakIR" = peakIR,
          "maxIR" = maxIR,
          "protractor_widthIR" = protractor_widthIR,
          "protractor_heightIR" = protractor_heightIR)
  annualizedIR = xts(annualizedIR, order.by = index(IR))

  #Slopes -3 to 3
  RaysIR = matrix(0, ncol = 7, nrow = n+1)
  RaysIR[1,1] = medIR + protractor_heightIR
  for (j in 2:7) {
    RaysIR[1,j] = RaysIR[1,1] - (j - 1) * protractor_heightIR / 3
  }

  for (i in 2:(n+1)) {
    for (j in 1:4) {
      RaysIR[i,j] = max(RaysIR[i-1,j] - ((RaysIR[1,j] - medIR) / protractor_widthIR), medIR)
    }
    for(j in 5:7){
      RaysIR[i,j] = min(RaysIR[i-1,j] - ((RaysIR[1,j] - medIR) / protractor_widthIR), medIR)
    }
  }

  RaysIR = xts(RaysIR, order.by = index_TE)
  colnames(RaysIR) = c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")

  #CUSUM Excess Returns
  cusumER = xts(100 * cumsum(coredata(logExcessReturns)), order.by = index(logExcessReturns))

  #Information obtained from annualized IR
  annualizedER = 12 * coredata(cusumER)
  lowerLimER = min(annualizedER)
  upperLimER = max(annualizedER)
  spreadER = upperLimER - lowerLimER
  avgER = spreadER / n
  upperPosER = which.max(annualizedER)
  lowerPosER = which.min(annualizedER)
  medER = lowerLimER + 0.5 * spreadER
  peakER = spreadIR / (upperPosER - lowerPosER)
  maxER = 0.5 * ceiling(abs(peakER) + abs(avgER))
  protractor_widthER = ceiling(0.9 * spreadER / (2 * maxER))
  protractor_heightER = abs(protractor_widthER * maxER)
  AER = c("lowerLimER" = lowerLimER,
          "upperLimER" = upperLimER,
          "spreadER" = spreadER,
          "avgER" = avgER,
          "upperPosER" = upperPosER,
          "lowerPosER" = lowerPosER,
          "medER" = medER,
          "peakER" = peakER,
          "maxER" = maxER,
          "protractor_widthER" = protractor_widthER,
          "protractor_heightER" = protractor_heightER)
  annualizedER = xts(annualizedER, order.by = index(IR))

  #Slopes -3 to 3
  RaysER = matrix(0, ncol = 7, nrow = n+1)
  RaysER[1,1] = medER + protractor_heightER
  for (j in 2:7) {
    RaysER[1,j] = RaysER[1,1] - (j - 1) * protractor_heightER / 3
  }

  for (i in 2:(n+1)) {
    for (j in 1:4) {
      RaysER[i,j] = max(RaysER[i-1,j] - ((RaysER[1,j] - medER) / protractor_widthER), medER)
    }
    for (j in 5:7) {
      RaysER[i,j] = min(RaysER[i-1,j] - ((RaysER[1,j] - medER) / protractor_widthER), medER)
    }
  }

  RaysER = xts(RaysER, order.by = index_TE)
  colnames(RaysER) = c("Ray-3", "Ray-2", "Ray-1", "Ray0", "Ray+1", "Ray+2", "Ray+3")


  #Return the updated likelihood ratios exceeding the threshold
  result = list("Logarithmic_Excess_Returns" = logExcessReturns,
              "Annual_Moving_Average" = AMA,
              "Tracking_Error" = Stds[,3],
              "Information_Ratios" = IR,
              "Lindley's_Recursion" = LR,
              "Annualized_Cusum_IR" = annualizedIR,
              "Annualized_Cusum_ER" = annualizedER,
              "Means" = Means,
              "Protractor_IR" = RaysIR,
              "Protractor_ER" = RaysER,
              "Standard_Deviations" = Stds,
              "Excess_Volatility" = Vol,
              "AIR" = AIR,
              "AER" = AER)

  class(result) = "cusumActMgr"
  return(result)
}

# This function is a simple weighted average of the current
# return and the last time period excess return. The returned value
# would be the current excess return used in the computation

# r = a return in the current period
# mu0 = a mean return from the last time period
# sigma0 = an estimated volatility from the last time period.
#          If it is not available (as for example in the first period,
#          set it to 0, in which case, the mean)
# hub_level = the number of standard deviations at which we huberize
#             (default: hub_level =4)
# lambda = the exponential weighting constant (default: lambda = 0.1)
muEst = function(r, mu0, sigma0, hub_level = 4, lambda = 0.1){
  #Huberization
  if (abs(r - mu0) > hub_level * sigma0 ) {
    r = mu0 + sign(r - mu0) * hub_level * sigma0
  }
  return(lambda * r + (1 - lambda) * mu0)
}


# This function is a simple weighted average of the current
# volatility and the last time period volatility. The returned value
# would be the current volatility used in the computation

# r = the logarithmic excess return in the current period
# mu0 = the mean excess return from the last time period
#       (this is often set to 0 in finance applications as the mean
#       is small on account of market efficiency)
# sigma0 = the estimated volatility from the last time period.
#          If it is not available (as for example in the first
#          period, set it to 0 or some initial estimate
# hub_level = the number of standard deviations at which we
#             huberize (default: hub_level =4)
# lambda_in = the exponential weighting constant when the data
#             seems consistent with the current estimate of
#             volatility (default: lambda = 0.1)
# lambda_out = the exponential weighting constant when the data
#              seems inconsistent with the current estimate of
#              volatility (default: lambda = 0.2)

sigmaEst = function(r, mu0, sigma0, hub_level = 4, lambda_in = 0.1,
                    lambda_out = 0.2){

  lambda = ifelse((sigma0 < hub_level * abs(r - mu0)) && (abs(r - mu0) < hub_level * sigma0), lambda_in, lambda_out)
  return(sqrt(lambda * (r - mu0) ^ 2 + (1 - lambda) * sigma0 ^ 2))
}

