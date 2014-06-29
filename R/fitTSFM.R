#' @title Fit a time series factor model using time series regression
#' 
#' @description Fits a time series (or, macroeconomic) factor model for single 
#' or multiple asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-OLS, discounted least 
#' squares-DLS (or) robust regression. Several variable selection options  
#' including Stepwise, Subsets, Lars are available as well. An object of class 
#' \code{tsfm} is returned.
#' 
#' @details 
#' Estimation method "OLS" corresponds to ordinary least squares, "DLS" is 
#' discounted least squares, which is weighted least squares estimation with 
#' exponentially declining weights that sum to unity, and, "Robust" is robust 
#' regression (uses \code{\link[robust]{lmRob}}). 
#' 
#' If \code{variable.selection="none"}, all chosen factors are used in the 
#' factor model. Whereas, "stepwise" performs traditional forward/backward 
#' stepwise OLS regression (using \code{\link[stats]{step}}), that starts from 
#' the initial set of factors and adds factors only if the regression fit, as 
#' measured by the Bayesian Information Criterion (BIC) or Akaike Information 
#' Criterion (AIC), improves. And, "all subsets" enables subsets selection 
#' using \code{\link[leaps]{regsubsets}} that chooses the n-best performing 
#' subsets of any given size (specified as \code{num.factor.subsets} here). 
#' "lars" and "lasso" correspond to variants of least angle regression using 
#' \code{\link[lars]{lars}}. If "lars" or "lasso" are chosen, \code{fit.method} 
#' will be ignored.
#' 
#' Note: If  \code{variable.selection}="lars" or "lasso", \code{fit.method} 
#' will be ignored. And, "Robust" \code{fit.method} is not truly available with 
#' \code{variable.selection="all subsets"}; instead, results are produced for 
#' \code{variable.selection="none"} with "Robust" to include all factors.
#' 
#' If \code{add.up.market = TRUE}, max(0, Rm-Rf) is added as a factor in the 
#' regression, following Henriksson & Merton (1981), to account for market 
#' timing (price movement of the general stock market relative to fixed income 
#' securities). The coefficient can be interpreted as the number of free put 
#' options.
#' 
#' Finally, for both the "lars" and "lasso" methods, the "Cp" statistic 
#' (defined in page 17 of Efron et al. (2002)) is calculated using 
#' \code{\link[lars]{summary.lars}} . While, "cv" computes the K-fold 
#' cross-validated mean squared prediction error using 
#' \code{\link[lars]{cv.lars}}.
#' 
#' @param asset.names  vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param market.name name of the column for market excess returns (Rm-Rf). 
#' Is required only if \code{add.up.market} or \code{add.up.market.squared} 
#' are \code{TRUE}. 
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object  
#' containing column(s) named \code{asset.names}, \code{factor.names} and 
#' optionally, \code{market.name}.
#' @param fit.method the estimation method, one of "OLS", "DLS" or "Robust". 
#' See details. 
#' @param variable.selection the variable selection method, one of "none", 
#' "stepwise","all subsets","lars" or "lasso". See details.
#' @param subsets.method one of "exhaustive", "forward", "backward" or "seqrep" 
#' (sequential replacement) to specify the type of subset search/selection. 
#' Required if "all subsets" variable selection is chosen. 
#' @param nvmax the maximum size of subsets to examine; an option for 
#' "all subsets" variable selection. Default is 8. 
#' @param force.in vector containing the names of factors that should always 
#' be included in the model; an option for "all subsets" variable selection. 
#' Default is NULL.
#' @param num.factors.subset number of factors required in the factor model; 
#' an option for "all subsets" variable selection. Default is 1. 
#' Note: nvmax >= num.factors.subset >= length(force.in).
#' @param add.up.market logical; If \code{TRUE}, adds max(0, Rm-Rf) as a 
#' regressor and \code{market.name} is also required. Default is \code{FALSE}. 
#' See Details. 
#' @param add.market.sqd logical; If \code{TRUE}, adds (Rm-Rf)^2 as a 
#' regressor and \code{market.name} is also required. Default is \code{FALSE}.
#' @param decay a scalar in (0, 1] to specify the decay factor for 
#' \code{fit.method="DLS"}. Default is 0.95.
#' @param lars.criterion an option to assess model selection for the "lars" or 
#' "lasso" variable.selection methods; one of "Cp" or "cv". See details. 
#' Default is "Cp".
#' @param ... optional arguments passed to the \code{step} function for 
#' variable.selection method "stepwise", such as direction, steps and 
#' the penalty factor k. Note that argument k is available only for "OLS" 
#' and "DLS" fits. Scope argument is not available presently. Also plan to
#' include other controls passed to \code{lmRob} soon.
#' 
#' @return fitTSFM returns an object of class 
#' \code{tsfm}. The returned object is a list
#' containing the following components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="OLS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}, or class \code{lars} if 
#' \code{variable.selection="lars" or "lasso"}.}
#' \item{alpha}{N x 1 vector of estimated alphas.}
#' \item{beta}{N x K matrix of estimated betas.}
#' \item{r2}{N x 1 vector of R-squared values.}
#' \item{resid.sd}{N x 1 vector of residual standard deviations.}
#' \item{call}{the matched function call.}
#' \item{data}{data as input.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{factor.names as input.}
#' \item{fit.method}{fit.method as input.}
#' \item{variable.selection}{variable.selection as input.}
#' Where N is the number of assets and K is the number of factors.
#' 
#' @family Factor Models
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan.
#' 
#' @references 
#' \enumerate{
#' \item Christopherson, Carino and Ferson (2009). Portfolio Performance 
#' Measurement and Benchmarking, McGraw Hill.
#' \item Efron, Hastie, Johnstone and Tibshirani (2002) "Least Angle
#' Regression" (with discussion) Annals of Statistics. Also refer to 
#' \url{http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf}. 
#' \item Hastie, Tibshirani and Friedman (2008) Elements of Statistical 
#' Learning 2nd edition, Springer, NY.
#' \item Henriksson and Merton (1981). On market timing and investment 
#' performance. II. Statistical procedures for evaluating forecasting skills, 
#' Journal of Business, Vol 54, No 4.
#' }
#' 
#' @seealso The following generic method functions: \code{\link{plot.tsfm}}, 
#' \code{\link{predict.tsfm}}, \code{\link{print.tsfm}} and 
#' \code{\link{summary.tsfm}}. 
#' 
#' And, the following extractor functions: \code{\link{coef.tsfm}}, 
#' \code{\link{cov.tsfm}}, \code{\link{fitted.tsfm}} and 
#' \code{\link{residuals.tsfm}}.
#' 
#' \code{\link{paFM}} for Performance Attribution. 
#' 
#' @examples
#' # load data from the database
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers.df, 
#'                fit.method="OLS", variable.selection="none")
#' # summary of HAM1 
#' summary(fit$asset.fit$HAM1)
#' # plot actual vs. fitted over time for HAM1
#' # using chart.TimeSeries() function from PerformanceAnalytics package
#' dataToPlot <- cbind(fitted(fit$asset.fit$HAM1), na.omit(managers.df$HAM1))
#' colnames(dataToPlot) <- c("Fitted","Actual")
#' chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
#'                  colorset=c("black","blue"), legend.loc="bottomleft")
#'
#'  
#'  @export

fitTSFM <- function(asset.names, factor.names, market.name, data=data, 
                    fit.method = c("OLS","DLS","Robust"),
                    variable.selection = c("none","stepwise","all subsets",
                                           "lars","lasso"),
                    subsets.method = c("exhaustive", "backward", "forward", 
                                       "seqrep"),
                    nvmax=8, force.in=NULL, num.factors.subset=1, 
                    add.up.market=FALSE, add.market.sqd=FALSE,
                    decay=0.95, lars.criterion="Cp", ...){
  
  # get all the arguments specified by their full names
  call <- match.call()
  if (!exists("direction")) {direction <- "backward"}
  if (!exists("steps")) {steps <- 1000}
  if (!exists("k")) {k <- 2}
  if (!exists("market.name") && (add.up.market==TRUE | add.market.sqd==TRUE)) {
    stop("Missing input: 'market.name' to include factors 'up.market' or 
         'market.sqd'")
  }
  
  # convert data into an xts object and hereafter work with xts objects
  data.xts <- checkData(data)
  
  # extract columns to be used in the time series regression
  dat.xts <- merge(data.xts[,asset.names], data.xts[,factor.names])
  if (add.up.market == TRUE | add.market.sqd == TRUE ) {
    dat.xts <- merge(dat.xts, data.xts[,market.name])
  }
  
  # Selects regression procedure based on specified variable.selection method.
  # Each method returns a list of fitted factor models for each asset.
  if (variable.selection == "none") {
    reg.list <- NoVariableSelection(dat.xts, asset.names, factor.names, 
                                    market.name, fit.method, add.up.market, 
                                    add.market.sqd, decay)
  } else if (variable.selection == "stepwise"){
    reg.list <- SelectStepwise(dat.xts, asset.names, factor.names, 
                               market.name, fit.method,
                               add.up.market, add.market.sqd, decay,
                               direction, steps, k)
  } else if (variable.selection == "all subsets"){
    reg.list <- SelectAllSubsets(dat.xts, asset.names, factor.names, 
                                 market.name, fit.method, subsets.method, 
                                 nvmax, force.in, num.factors.subset, 
                                 add.up.market, add.market.sqd, decay)
  } else if (variable.selection == "lars" | variable.selection == "lasso"){
    result.lars <- SelectLars(dat.xts, asset.names, factor.names, market.name, 
                              variable.selection, add.up.market, add.market.sqd, 
                              decay, lars.criterion)
    result.lars <- c(result.lars, call, data, asset.names, factor.names, 
                     fit.method, variable.selection)
    return(result.lars)
  } 
  else {
    stop("Invalid argument: variable.selection must be either 'none',
         'stepwise','all subsets','lars' or 'lasso'")
  }
  
  # extract the fitted factor models, coefficients, r2 values and residual vol 
  # from returned factor model fits above
  coef.mat <- t(sapply(reg.list, coef))
  alpha <- coef.mat[, 1]
  beta <- coef.mat[, -1]
  r2 <- sapply(reg.list, function(x) summary(x)$r.squared)
  resid.sd <- sapply(reg.list, function(x) summary(x)$sigma)
  # create list of return values.
  result <- list(asset.fit=reg.list, alpha=alpha, beta=beta, r2=r2, 
                 resid.sd=resid.sd, call=call, data=data, 
                 asset.names=asset.names, factor.names=factor.names, 
                 fit.method=fit.method, variable.selection=variable.selection)
  class(result) <- "tsfm"
  return(result)
}


### method variable.selection = "none"
#
NoVariableSelection <- function (dat.xts, asset.names, factor.names, 
                                 market.name, fit.method, add.up.market, 
                                 add.market.sqd, decay){
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    # optionally add factors: up.market=max(0,Rm-Rf), market.sqd=(Rm-Rf)^2
    reg.xts <- MarketFactors(dat.xts, reg.xts, market.name, 
                             add.up.market, add.market.sqd)
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
    # fit based on time series regression method chosen
    if (fit.method == "OLS") {
      reg.list[[i]] <- lm(fm.formula, data=reg.xts)
    } else if (fit.method == "DLS") {
      w <- WeightsDLS(nrow(reg.xts), decay)
      reg.list[[i]] <- lm(fm.formula, data=reg.xts, weights=w)
    } else if (fit.method == "Robust") {
      reg.list[[i]] <- lmRob(fm.formula, data=reg.xts)
    } else {
      stop("Invalid argument: fit.method must be 'OLS', 'DLS' or 'Robust'")
    }
  } 
  reg.list  
}


### method variable.selection = "stepwise"
#
SelectStepwise <- function(dat.xts, asset.names, factor.names, 
                           market.name, fit.method, add.up.market, 
                           add.market.sqd, decay, direction, steps, k){
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
    # optionally add factors: up.market=max(0,Rm-Rf), market.sqd=(Rm-Rf)^2
    if(fit.method=="Robust" && (add.up.market==TRUE | add.market.sqd==TRUE)) {
      stop("This function does not support add.up.market/add.market.sqd when 
           variable.selection = 'stepwise' && fit.method = 'Robust'. Please 
           choose a different combination of options.")
    } else {
      reg.xts <- MarketFactors(dat.xts, reg.xts, market.name, 
                               add.up.market, add.market.sqd)
    }
    
    # fit based on time series regression method chosen
    if (fit.method == "OLS") {
      reg.list[[i]] <- step(lm(fm.formula, data=reg.xts), direction=direction, 
                            steps=steps, k=k, trace=0)
    } else if (fit.method == "DLS") {
      w <- WeightsDLS(nrow(reg.xts), decay)
      reg.list[[i]] <- step(lm(fm.formula, data=reg.xts, weights=w), 
                            direction=direction, steps=steps, k=k, trace=0)
    } else if (fit.method == "Robust") {
      reg.list[[i]] <- step.lmRob(lmRob(fm.formula, data=reg.xts), trace=FALSE,
                                  direction=direction, steps=steps)
    } else {
      stop("Invalid argument: fit.method must be 'OLS', 'DLS' or 'Robust'")
    }
  }
  reg.list
}


### method variable.selection = "all subsets"
#
SelectAllSubsets <- function(dat.xts, asset.names, factor.names, 
                             market.name, fit.method, subsets.method, 
                             nvmax, force.in, num.factors.subset, 
                             add.up.market, add.market.sqd, decay){
  # Check argument validity
  if (nvmax < num.factors.subset) {
    stop("Invaid Argument: nvmax should be >= num.factors.subset")
  }
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
    # branch out based on time series regression method chosen
    if (fit.method == "Robust") {
      warning("'Robust' fit.method is not available with 'all subsets' 
              variable.selection. Instead, results are shown for 
              variable.selection='none' with fit.method='Robust' to include 
              all factors.")
      reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
      reg.xts <- MarketFactors(dat.xts, reg.xts, market.name, 
                               add.up.market, add.market.sqd)
      asset.fit <- lmRob(fm.formula, data=reg.xts)    
    } 
    else if (fit.method == "OLS" | fit.method == "DLS") {
      # use regsubsets to find the best model with a subset of factors of size 
      # num.factors.subset
      
      if (num.factors.subset == length(force.in)) {
        reg.xts <- na.omit(dat.xts[, c(i, force.in)])
      } else if (num.factors.subset > length(force.in)) {
        reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
        if (fit.method != "DLS") {decay <- 1}
        # do weighted least squares if "DLS"
        w <- WeightsDLS(nrow(reg.xts), decay)
        fm.subsets <- regsubsets(fm.formula, data=reg.xts, nvmax=nvmax,
                                 force.in=force.in, method=subsets.method, 
                                 weights=w)
        sum.sub <- summary(fm.subsets)
        reg.xts <- na.omit(dat.xts[,c(i,names(which(sum.sub$which[
          as.character(num.factors.subset),-1]==TRUE)))])
      } else {
        stop("Invalid Argument: num.factors.subset should be >= 
             length(force.in)")
      }
      
      # optionally add factors: up.market=max(0,Rm-Rf), market.sqd=(Rm-Rf)^2
      reg.xts <- MarketFactors(dat.xts, reg.xts, market.name, 
                               add.up.market, add.market.sqd)
      # fit linear regression model for the factors chosen
      reg.list[[i]] <- lm(fm.formula, data=reg.xts, weights=w)
    }
    else {
      stop("Invalid argument: fit.method must be 'OLS', 'DLS' or 'Robust'")
    }
  }
  reg.list
}


### method variable.selection = "lars" or "lasso"
#
SelectLars <- function(dat.xts, asset.names, factor.names, market.name, 
                       variable.selection, add.up.market, add.market.sqd, 
                       decay, lars.criterion) {
  # initialize list object to hold the fitted objects and, vectors and matrices
  # for the other results
  asset.fit <- list()
  alpha <- rep(NA, length(asset.names))
  beta <- matrix(NA, length(asset.names), length(factor.names))
  r2 <- rep(NA, length(asset.names))
  resid.sd <- rep(NA, length(asset.names))
  
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    # optionally add factors: up.market=max(0,Rm-Rf), market.sqd=(Rm-Rf)^2
    reg.xts <- MarketFactors(dat.xts, reg.xts, market.name, 
                             add.up.market, add.market.sqd)
    # convert to matrix
    reg.mat <- as.matrix(na.omit(reg.xts))
    # fit lar or lasso regression model
    lars.fit <- lars(reg.mat[,factor.names], reg.mat[,i], 
                     type=variable.selection, trace = FALSE)
    lars.sum <- summary(lars.fit)
    
    # get the step that minimizes the "Cp" statistic or the "cv" mean-sqd 
    # prediction error
    if (lars.criterion == "Cp") {
      s <- which.min(lars.sum$Cp)
    } else if (lars.criterion == "cv") {
      lars.cv <- cv.lars(reg.mat[,factor.names], reg.mat[,i], trace=FALSE,
                         type=variable.selection, mode="step", plot.it=FALSE)
      s <- which.min(lars.cv$cv)
    } else {
      stop("Invalid argument: lars.criterion must be Cp' or 'cv'")
    }
    
    # get factor model coefficients & fitted values at the step obtained above
    coef.lars <- predict(lars.fit, s=s, type="coef", mode="step")
    fitted.lars <- predict(lars.fit, reg.xts[,factor.names], s=s, type="fit", 
                           mode="step")
    # extract and assign the results
    asset.fit[[i]] = lars.fit
    alpha[i] <- (fitted.lars$fit - 
                   reg.xts[,factor.names]%*%coef.lars$coefficients)[1]
    beta.names <- names(coef.lars$coefficients)
    beta[i,beta.names] <- coef.lars$coefficients
    r2[i] <-  lars.fit$R2[s]
    resid.sd[i] <- lars.sum$Rss[s]/(nrow(reg.xts)-s)
    
  }
  results.lars <- list(asset.fit, alpha, beta, r2, resid.sd)
}


### Format and add optional factors "up.market" and "market.sqd"
#
MarketFactors <- function(dat.xts, reg.xts, market.name, 
                          add.up.market, add.market.sqd){
  if(add.up.market == TRUE) {
    # up.market = max(0,Rm-Rf)
    up.market <- apply(dat.xts[,market.name],1,max,0)
    reg.xts <- merge(reg.xts,up.market)
    colnames(reg.xts)[dim(reg.xts)[2]] <- "up.market"
  }
  if(add.market.sqd == TRUE) {
    # market.sqd = (Rm-Rf)^2
    market.sqd <- dat.xts[,market.name]^2
    reg.xts <- merge(reg.xts,market.sqd)
    colnames(reg.xts)[dim(reg.xts)[2]] <- "market.sqd"
  }
  reg.xts
}


### calculate weights for "DLS"
#
WeightsDLS <- function(t,d){
  # more weight given to more recent observations 
  w <- d^seq((t-1),0,-1)    
  # ensure that the weights sum to unity
  w/sum(w)
}