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
#' "lar" and "lasso" correspond to variants of least angle regression using 
#' \code{\link[lars]{lars}}. 
#' 
#' Note: If \code{variable.selection="lar" or "lasso"}, \code{fit.method} 
#' will be ignored.
#' 
#' If \code{add.up.market=TRUE}, \code{max(0, Rm-Rf)} is added as a factor in 
#' the regression, following Henriksson & Merton (1981), to account for market 
#' timing (price movement of the general stock market relative to fixed income 
#' securities). The coefficient can be interpreted as the number of free put 
#' options. Similarly, if \code{add.market.sqd=TRUE}, \code{(Rm-Rf)^2} is added 
#' as a factor in the regression, following Treynor-Mazuy (1966), to account 
#' for market timing with respect to volatility.
#' 
#' Finally, for both the "lar" and "lasso" methods, the "Cp" statistic 
#' (defined in page 17 of Efron et al. (2002)) is calculated using 
#' \code{\link[lars]{summary.lars}} . While, "cv" computes the K-fold 
#' cross-validated mean squared prediction error using 
#' \code{\link[lars]{cv.lars}}.
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param market.name name of the column for market excess returns (Rm-Rf). 
#' Is required only if \code{add.up.market} or \code{add.market.sqd} 
#' are \code{TRUE}.
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object  
#' containing column(s) named in \code{asset.names}, \code{factor.names} and 
#' optionally, \code{market.name}.
#' @param fit.method the estimation method, one of "OLS", "DLS" or "Robust". 
#' See details. 
#' @param variable.selection the variable selection method, one of "none", 
#' "stepwise","all subsets","lar" or "lasso". See details.
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
#' @param add.up.market logical, adds max(0, Rm-Rf) as a factor. If 
#' \code{TRUE}, \code{market.name} is required. Default is \code{TRUE}. 
#' See Details. 
#' @param add.market.sqd logical, adds (Rm-Rf)^2 as a factor. If \code{TRUE},
#' \code{market.name} is required. Default is \code{TRUE}.
#' @param decay a scalar in (0, 1] to specify the decay factor for 
#' \code{fit.method="DLS"}. Default is 0.95.
#' @param lars.criterion an option to assess model selection for the "lar" or 
#' "lasso" variable.selection methods; one of "Cp" or "cv". See details. 
#' Default is "Cp".
#' @param ... optional arguments passed to the \code{step} function for 
#' variable.selection method "stepwise", such as direction, steps and 
#' the penalty factor k. Note that argument k is available only for "OLS" 
#' and "DLS" fits. Scope argument is not available presently. Also plan to
#' include other controls passed to \code{lmRob} soon.
#' 
#' @return fitTSFM returns an object of class \code{tsfm}. 
#' 
#' The generic functions \code{summary}, \code{predict} and \code{plot} are 
#' used to obtain and print a summary, predicted asset returns for new factor 
#' data and plot selected characteristics for one or more assets. The generic 
#' accessor functions \code{coefficients}, \code{fitted} and \code{residuals} 
#' extract various useful features of the fit object. \code{coef.tsfm} extracts 
#' coefficients from the fitted factor model and returns an N x (K+1) matrix of 
#' all coefficients, \code{fitted.tsfm} gives an N x T data object of fitted 
#' values and \code{residuals.tsfm} gives an N x T data object of residuals.
#' 
#' An object of class \code{tsfm} is a list containing the following 
#' components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="OLS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}, or class \code{lars} if 
#' \code{variable.selection="lar" or "lasso"}.}
#' \item{alpha}{N x 1 vector of estimated alphas.}
#' \item{beta}{N x K matrix of estimated betas.}
#' \item{r2}{N x 1 vector of R-squared values.}
#' \item{resid.sd}{N x 1 vector of residual standard deviations.}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the assets and factors.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{factor.names as input.}
#' \item{fit.method}{fit.method as input.}
#' \item{variable.selection}{variable.selection as input.}
#' Where N is the number of assets, K is the number of factors and T is the 
#' number of time periods.
#' 
#' @author Eric Zivot, Yi-An Chen and Sangeetha Srinivasan.
#' 
#' @references 
#' \enumerate{
#' \item Christopherson, Jon A., David R. Carino, and Wayne E. Ferson. 
#' Portfolio performance measurement and benchmarking. McGraw Hill 
#' Professional, 2009.
#' \item Efron, Bradley, Trevor Hastie, Iain Johnstone, and Robert Tibshirani. 
#' "Least angle regression." The Annals of statistics 32, no. 2 (2004): 407-499. 
#' \item Hastie, Trevor, Robert Tibshirani, Jerome Friedman, T. Hastie, J. 
#' Friedman, and R. Tibshirani. The elements of statistical learning. Vol. 2, 
#' no. 1. New York: Springer, 2009.
#' \item Henriksson, Roy D., and Robert C. Merton. "On market timing and 
#' investment performance. II. Statistical procedures for evaluating 
#' forecasting skills." Journal of business (1981): 513-533.
#' \item Treynor, Jack, and Kay Mazuy. "Can mutual funds outguess the market." 
#' Harvard business review 44, no. 4 (1966): 131-136.
#' }
#' 
#' @seealso The \code{tsfm} methods for generic functions: 
#' \code{\link{plot.tsfm}}, \code{\link{predict.tsfm}}, 
#' \code{\link{print.tsfm}} and \code{\link{summary.tsfm}}. 
#' 
#' And, the following extractor functions: \code{\link[stats]{coef}}, 
#' \code{\link{covFM}}, \code{\link[stats]{fitted}} and 
#' \code{\link[stats]{residuals}}.
#' 
#' \code{\link{paFM}} for Performance Attribution. 
#' 
#' @examples
#' # load data from the database
#' data(managers.df)
#' fit <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
#'                factor.names=c("EDHEC.LS.EQ","SP500.TR"), data=managers.df, 
#'                add.up.market=FALSE, add.market.sqd=FALSE, 
#'                fit.method="OLS", variable.selection="none")
#' # summary of HAM1 
#' summary(fit$asset.fit$HAM1)
#' # fitted values all 6 asset returns
#' fitted(fit)
#' # plot actual vs. fitted over time for HAM1
#' # using chart.TimeSeries() function from PerformanceAnalytics package
#' dataToPlot <- cbind(fitted(fit$asset.fit$HAM1), na.omit(managers.df$HAM1))
#' colnames(dataToPlot) <- c("Fitted","Actual")
#' chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
#'                  colorset=c("black","blue"), legend.loc="bottomleft")
#'
#'  @export

fitTSFM <- function(asset.names, factor.names, market.name, data=data, 
                    fit.method = c("OLS","DLS","Robust"),
                    variable.selection = c("none","stepwise","all subsets",
                                           "lar","lasso"),
                    subsets.method = c("exhaustive", "backward", "forward", 
                                       "seqrep"),
                    nvmax=8, force.in=NULL, num.factors.subset=1, 
                    add.up.market=TRUE, add.market.sqd=TRUE,
                    decay=0.95, lars.criterion="Cp", ...){
  
  # get all the arguments specified by their full names
  call <- match.call()

  fit.method = fit.method[1] # default is OLS
  variable.selection = variable.selection[1] # default is "none"
  subsets.method = subsets.method[1] # default is "exhaustive"
  
  if (!exists("direction")) {direction <- "backward"}
  if (!exists("steps")) {steps <- 1000}
  if (!exists("k")) {k <- 2}
  if ((missing(market.name)|is.null(market.name)) && 
        (add.up.market==TRUE | add.market.sqd==TRUE)) {
    stop("Missing input: 'market.name' is required to include factors 
         'up.market' or 'market.sqd'")
  }
  
  # convert data into an xts object and hereafter work with xts objects
  data.xts <- checkData(data)
  
  # extract columns to be used in the time series regression
  dat.xts <- merge(data.xts[,asset.names], data.xts[,factor.names])
  ### When merging xts objects, the spaces in names get converted to periods
  
  # opt add market-timing factors: up.market=max(0,Rm-Rf), market.sqd=(Rm-Rf)^2
  if(add.up.market == TRUE) {
    up.market <- data.xts[,market.name]
    up.market [up.market < 0] <- 0
    dat.xts <- merge.xts(dat.xts,up.market)
    colnames(dat.xts)[dim(dat.xts)[2]] <- "up.market"
    factor.names <- c(factor.names, "up.market")
  }
  if(add.market.sqd == TRUE) {
    market.sqd <- data.xts[,market.name]^2   
    dat.xts <- merge(dat.xts, market.sqd)
    colnames(dat.xts)[dim(dat.xts)[2]] <- "market.sqd"
    factor.names <- c(factor.names, "market.sqd")
  }
  
  # spaces get converted to periods in colnames of xts object after merge
  asset.names <- gsub(" ",".", asset.names, fixed=TRUE)
  factor.names <- gsub(" ",".", factor.names, fixed=TRUE)
  
  # Selects regression procedure based on specified variable.selection method.
  # Each method returns a list of fitted factor models for each asset.
  if (variable.selection == "none") {
    reg.list <- NoVariableSelection(dat.xts, asset.names, factor.names, 
                                    fit.method, add.up.market, add.market.sqd, 
                                    decay)
  } else if (variable.selection == "stepwise"){
    reg.list <- SelectStepwise(dat.xts, asset.names, factor.names, 
                               fit.method, add.up.market, add.market.sqd, 
                               decay, direction, steps, k)
  } else if (variable.selection == "all subsets"){
    reg.list <- SelectAllSubsets(dat.xts, asset.names, factor.names, 
                                 fit.method, subsets.method, 
                                 nvmax, force.in, num.factors.subset, 
                                 add.up.market, add.market.sqd, decay)
  } else if (variable.selection == "lar" | variable.selection == "lasso"){
    result.lars <- SelectLars(dat.xts, asset.names, factor.names, 
                              variable.selection, add.up.market, add.market.sqd, 
                              decay, lars.criterion)
    input <- list(call=call, data=dat.xts, 
                  asset.names=asset.names, factor.names=factor.names, 
                  fit.method=fit.method, variable.selection=variable.selection)
    result <- c(result.lars, input)
    class(result) <- "tsfm"
    return(result)
  } 
  else {
    stop("Invalid argument: variable.selection must be either 'none',
         'stepwise','all subsets','lar' or 'lasso'")
  }
  
  # extract the fitted factor models, coefficients, r2 values and residual vol 
  # from returned factor model fits above
  coef.mat <- makePaddedDataFrame(lapply(reg.list, coef))
  alpha <- coef.mat[, 1, drop=FALSE]
  # to make class of alpha numeric instead of matrix
  # aplha <- coef.mat[,1]
  beta <- coef.mat[, -1, drop=FALSE]
  r2 <- sapply(reg.list, function(x) summary(x)$r.squared)
  resid.sd <- sapply(reg.list, function(x) summary(x)$sigma)
  # create list of return values.
  result <- list(asset.fit=reg.list, alpha=alpha, beta=beta, r2=r2, 
                 resid.sd=resid.sd, call=call, data=dat.xts, 
                 asset.names=asset.names, factor.names=factor.names, 
                 fit.method=fit.method, variable.selection=variable.selection)
  class(result) <- "tsfm"
  return(result)
}


### method variable.selection = "none"
#
NoVariableSelection <- function(dat.xts, asset.names, factor.names, fit.method, 
                                add.up.market, add.market.sqd, decay){
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    
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
SelectStepwise <- function(dat.xts, asset.names, factor.names, fit.method, 
                           add.up.market, add.market.sqd, decay, 
                           direction, steps, k){
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
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
SelectAllSubsets <- function(dat.xts, asset.names, factor.names, fit.method, 
                             subsets.method, nvmax, force.in, 
                             num.factors.subset, add.up.market, add.market.sqd, 
                             decay){
  # Check argument validity
  if (nvmax < num.factors.subset) {
    stop("Invaid Argument: nvmax should be >= num.factors.subset")
  }
  
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    
    # choose best subset of factors depending on specified number of factors
    if (num.factors.subset == length(force.in)) {
      reg.xts <- na.omit(dat.xts[, c(i, force.in)])
    } else if (num.factors.subset > length(force.in)) {
      reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
      
      # formula to pass to lm or lmRob
      fm.formula <- as.formula(paste(i," ~ ."))
      
      if (fit.method != "DLS") {decay <- 1}
      # do weighted least squares if "DLS"
      w <- WeightsDLS(nrow(reg.xts), decay)
      
      # use regsubsets to find the best model with a subset of factors of size 
      # num.factors.subset
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


### method variable.selection = "lar" or "lasso"
#
SelectLars <- function(dat.xts, asset.names, factor.names, variable.selection, 
                       add.up.market, add.market.sqd, decay, lars.criterion) {
  # initialize list object to hold the fitted objects and, vectors and matrices
  # for the other results
  asset.fit <- list()
  alpha <- rep(NA, length(asset.names))
  beta <- matrix(NA, length(asset.names), length(factor.names))
  r2 <- rep(NA, length(asset.names))
  resid.sd <- rep(NA, length(asset.names))
  names(alpha)=names(r2)=names(resid.sd)=rownames(beta)=asset.names
  colnames(beta) <- factor.names
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names){
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    
    # convert to matrix
    reg.mat <- as.matrix(na.omit(reg.xts))
    # fit lar or lasso regression model
    lars.fit <- lars(reg.mat[,-1], reg.mat[,i], 
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
    fitted.lars <- predict(lars.fit, reg.xts[,-1], s=s, type="fit", 
                           mode="step")
    # extract and assign the results
    asset.fit[[i]] = lars.fit
    alpha[i] <- (fitted.lars$fit - 
                   reg.xts[,-1]%*%coef.lars$coefficients)[1]
    beta.names <- names(coef.lars$coefficients)
    beta[i, beta.names] <- coef.lars$coefficients
    r2[i] <-  lars.fit$R2[s]
    resid.sd[i] <- lars.sum$Rss[s]/(nrow(reg.xts)-s)
    
  }
  results.lars <- list(asset.fit=asset.fit, alpha=alpha, beta=beta, r2=r2, 
                       resid.sd=resid.sd)
}


### calculate weights for "DLS"
#
WeightsDLS <- function(t,d){
  # more weight given to more recent observations 
  w <- d^seq((t-1),0,-1)    
  # ensure that the weights sum to unity
  w/sum(w)
}

### make a data frame (padded with NAs) from columns of unequal length
#
makePaddedDataFrame <- function(l){
  DF <- do.call(rbind, lapply(lapply(l, unlist), "[", 
                              unique(unlist(c(sapply(l,names))))))
  DF <- as.data.frame(DF)
  names(DF) <- unique(unlist(c(sapply(l,names))))
  # as.matrix(DF) # if matrix output needed
  DF
}

#' @param object a fit object of class \code{tsfm} which is returned by 
#' \code{fitTSFM}

#' @rdname fitTSFM
#' @method coef tsfm
#' @export

coef.tsfm <- function(object,...){
  coef.mat <- t(sapply(object$asset.fit, coef))
  return(coef.mat)
}

#' @rdname fitTSFM
#' @method fitted tsfm
#' @export

fitted.tsfm <- function(object,...){
  # get fitted values from each linear factor model fit 
  # and convert them into xts/zoo objects
  fitted.list = sapply(object$asset.fit, function(x) checkData(fitted(x)))
  # this is a list of xts objects, indexed by the asset name
  # merge the objects in the list into one xts object
  fitted.xts <- do.call(merge, fitted.list)
  return(fitted.xts)
}


#' @rdname fitTSFM
#' @method residuals tsfm
#' @export

residuals.tsfm <- function(object ,...) {
  # get residuals from each linear factor model fit 
  # and convert them into xts/zoo objects
  residuals.list = sapply(object$asset.fit, function(x) checkData(residuals(x)))
  # this is a list of xts objects, indexed by the asset name
  # merge the objects in the list into one xts object
  residuals.xts <- do.call(merge, residuals.list)
  return(residuals.xts)
}

#' @rdname fitTSFM
#' @method covFM tsfm
#' @export

covFM.tsfm <- function(object) {
  
  # check input object validity
  if (!inherits(object, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', 'sfm' or 'ffm'.")
  }
  
  # get parameters and factors from factor model
  beta <- as.matrix(object$beta)
  beta[is.na(beta)] <- 0
  sig2.e = object$resid.sd^2
  factor <- as.matrix(object$data[, colnames(object$beta)])
  
  # factor covariance matrix 
  factor.cov = var(factor, use="na.or.complete")
  
  # residual covariance matrix D
  if (length(sig2.e) > 1) {
    D.e = diag(sig2.e)
  } else {
    D.e =  as.vector(sig2.e)
  }
  
  cov.fm = beta %*% factor.cov %*% t(beta) + D.e
  
  #   if (any(diag(chol(cov.fm)) == 0)) {
  #     warning("Covariance matrix is not positive definite!")
  #   }
  
  return(cov.fm)
}
