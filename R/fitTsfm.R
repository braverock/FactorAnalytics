#' @title Fit a time series factor model using time series regression
#' 
#' @description Fits a time series (a.k.a. macroeconomic) factor model for one 
#' or more asset returns or excess returns using time series regression. 
#' Users can choose between ordinary least squares-OLS, discounted least 
#' squares-DLS (or) robust regression. Several variable selection options  
#' including Stepwise, Subsets, Lars are available as well. An object of class 
#' \code{"tsfm"} is returned.
#' 
#' @details 
#' Typically, factor models are fit using excess returns. \code{rf.name} gives 
#' the option to supply a risk free rate variable to subtract from each asset 
#' return and factor to compute excess returns. 
#' 
#' Estimation method "OLS" corresponds to ordinary least squares using 
#' \code{\link[stats]{lm}}, "DLS" is discounted least squares (weighted least 
#' squares with exponentially declining weights that sum to unity), and, 
#' "Robust" is robust regression (using \code{\link[robust]{lmRob}}). 
#' 
#' If \code{variable.selection="none"}, uses all the factors and performs no 
#' variable selection. Whereas, "stepwise" performs traditional stepwise 
#' LS or Robust regression (using \code{\link[stats]{step}} or 
#' \code{\link[robust]{step.lmRob}}), that starts from the initial set of 
#' factors and adds/subtracts factors only if the regression fit, as measured 
#' by the Bayesian Information Criterion (BIC) or Akaike Information Criterion 
#' (AIC), improves. And, "subsets" enables subsets selection using 
#' \code{\link[leaps]{regsubsets}}; chooses the best performing subset of any 
#' given size or within a range of subset sizes. Different methods such as 
#' exhaustive search (default), forward or backward stepwise, or sequential 
#' replacement can be employed.See \code{\link{fitTsfm.control}} for more 
#' details on the control arguments.
#'  
#' \code{variable.selection="lars"} corresponds to least angle regression 
#' using \code{\link[lars]{lars}} with variants "lasso" (default), "lar", 
#' "stepwise" or "forward.stagewise". Note: If \code{variable.selection="lars"}, 
#' \code{fit.method} will be ignored.
#' 
#' Arguments \code{mkt.name} and \code{mkt.timing} allow for market-timing 
#' factors to be added to any of the above methods. Market timing accounts for 
#' the price movement of the general stock market relative to fixed income 
#' securities. Specifying \code{mkt.timing="HM"}, includes 
#' $down.market = max(0, R_f-R_m)$ as a factor, following Henriksson & Merton 
#' (1981). The coefficient of this down-market factor can be interpreted as the 
#' number of "free" put options on the market provided by the manager's 
#' market-timings kills. Similarly, to account for market timing with respect 
#' to volatility, one can specify \code{mkt.timing="TM"}. Following 
#' Treynor & Mazuy (1966), $market.sqd = (R_m-R_f)^2$ is added as a factor. 
#' For example, as a test for market timing, either of these factors can be 
#' added to the single index regression model.
#' 
#' \subsection{Data Processing}{
#' 
#' Note about NAs: Before model fitting, incomplete cases are removed for 
#' every asset (return data combined with respective factors' return data) 
#' using \code{\link[stats]{na.omit}}. Otherwise, all observations in 
#' \code{data} are included.
#' 
#' Note about \code{asset.names} and \code{factor.names}: Spaces in column 
#' names of \code{data} will be converted to periods as \code{fitTsfm} works 
#' with \code{xts} objects internally and colnames won't be left as they are.
#' }
#' 
#' @param asset.names vector containing names of assets, whose returns or 
#' excess returns are the dependent variable.
#' @param factor.names vector containing names of the macroeconomic factors.
#' @param mkt.name name of the column for market excess returns (Rm-Rf); this 
#' is necessary to add market timing factors. Default is NULL.
#' @param rf.name name of the column of risk free rate variable to calculate 
#' excess returns for all assets (in \code{asset.names}) and factors (in 
#' \code{factor.names}). Default is NULL, and no action is taken.
#' @param data vector, matrix, data.frame, xts, timeSeries or zoo object  
#' containing column(s) named in \code{asset.names}, \code{factor.names} and 
#' optionally, \code{mkt.name} and \code{rf.name}.
#' @param fit.method the estimation method, one of "OLS", "DLS" or "Robust". 
#' See details. Default is "OLS". 
#' @param variable.selection the variable selection method, one of "none", 
#' "stepwise","subsets","lars". See details. Default is "none".
#' @param mkt.timing one of "HM" or "TM". See Details. Default is NULL.
#' \code{mkt.name} is required if any of these options are to be implemented.
#' @param control list of control parameters. The default is constructed by 
#' the function \code{\link{fitTsfm.control}}. See the documentation for 
#' \code{\link{fitTsfm.control}} for details.
#' @param ... arguments passed to \code{\link{fitTsfm.control}}
#' 
#' @return fitTsfm returns an object of class \code{"tsfm"} for which 
#' \code{print}, \code{plot}, \code{predict} and \code{summary} methods exist. 
#' 
#' The generic accessor functions \code{coef}, \code{fitted} and 
#' \code{residuals} extract various useful features of the fit object. 
#' Additionally, \code{fmCov} computes the covariance matrix for asset returns 
#' based on the fitted factor model
#' 
#' An object of class \code{"tsfm"} is a list containing the following 
#' components:
#' \item{asset.fit}{list of fitted objects for each asset. Each object is of 
#' class \code{lm} if \code{fit.method="OLS" or "DLS"}, class \code{lmRob} if 
#' the \code{fit.method="Robust"}, or class \code{lars} if 
#' \code{variable.selection="lars"}.}
#' \item{alpha}{length-N vector of estimated alphas.}
#' \item{beta}{N x K matrix of estimated betas.}
#' \item{r2}{length-N vector of R-squared values.}
#' \item{resid.sd}{length-N vector of residual standard deviations.}
#' \item{fitted}{xts data object of fitted values; iff 
#' \code{variable.selection="lars"}}
#' \item{call}{the matched function call.}
#' \item{data}{xts data object containing the assets and factors.}
#' \item{asset.names}{asset.names as input.}
#' \item{factor.names}{factor.names as input.}
#' \item{fit.method}{fit.method as input.}
#' \item{variable.selection}{variable.selection as input.}
#' Where N is the number of assets, K is the number of factors and T is the 
#' number of time periods.
#' 
#' @author Eric Zivot, Sangeetha Srinivasan and Yi-An Chen.
#' 
#' @references 
#' Christopherson, J. A., Carino, D. R., & Ferson, W. E. (2009). Portfolio 
#' performance measurement and benchmarking. McGraw Hill Professional.
#' 
#' Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004). Least angle 
#' regression. The Annals of statistics, 32(2), 407-499. 
#' 
#' Hastie, T., Tibshirani, R., Friedman, J., Hastie, T., Friedman, J., & 
#' Tibshirani, R. (2009). The elements of statistical learning (Vol. 2, No. 1). 
#' New York: Springer.
#' 
#' Henriksson, R. D., & Merton, R. C. (1981). On market timing and investment 
#' performance. II. Statistical procedures for evaluating forecasting skills. 
#' Journal of business, 513-533.
#' 
#' Treynor, J., & Mazuy, K. (1966). Can mutual funds outguess the market. 
#' Harvard business review, 44(4), 131-136.
#' 
#' @seealso The \code{tsfm} methods for generic functions: 
#' \code{\link{plot.tsfm}}, \code{\link{predict.tsfm}}, 
#' \code{\link{print.tsfm}} and \code{\link{summary.tsfm}}. 
#' 
#' And, the following extractor functions: \code{\link[stats]{coef}}, 
#' \code{\link[stats]{fitted}}, \code{\link[stats]{residuals}},
#' \code{\link{fmCov}}, \code{\link{fmSdDecomp}}, \code{\link{fmVaRDecomp}} 
#' and \code{\link{fmEsDecomp}}.
#' 
#' \code{\link{paFm}} for Performance Attribution. 
#' 
#' @examples
#' # load data from the database
#' data(managers)
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                factor.names=colnames(managers[,(7:9)]), data=managers)
#' summary(fit)
#' fitted(fit)
#' # plot actual returns vs. fitted factor model returns for HAM1
#' plot(fit, plot.single=TRUE, asset.name="HAM1", which.plot.single=1, 
#'      loop=FALSE)
#' # group plot; type selected from menu prompt; auto-looped for multiple plots
#' # plot(fit)
#' 
#' # example: Market-timing factors with robust fit
#' fit <- fitTsfm(asset.names=colnames(managers[,(1:6)]), factor.names=NULL, 
#'                mkt.name="SP500 TR", mkt.timing="HM", data=managers, 
#'                fit.method="Robust")
#' 
#' # example using "subsets" variable selection
#' fit.sub <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                    factor.names=colnames(managers[,(7:9)]), 
#'                    data=managers, variable.selection="subsets", 
#'                    method="exhaustive", nvmin=2) 
#' 
#' # example using "lars" variable selection and subtracting risk-free rate
#' fit.lar <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                    factor.names=colnames(managers[,(7:9)]), 
#'                    rf.name="US 3m TR", data=managers, 
#'                    variable.selection="lars", lars.criterion="cv") 
#' 
#' @importFrom PerformanceAnalytics checkData
#' @importFrom robust lmRob step.lmRob
#' @importFrom leaps regsubsets
#' @importFrom lars lars cv.lars
#' 
#' @export

fitTsfm <- function(asset.names, factor.names, mkt.name=NULL, rf.name=NULL, 
                    data=data, fit.method=c("OLS","DLS","Robust"), 
                    variable.selection=c("none","stepwise","subsets","lars"), 
                    mkt.timing=NULL, control=fitTsfm.control(...), ...) {
  
  # record the call as an element to be returned
  call <- match.call()
  
  # set defaults and check input vailidity
  fit.method = fit.method[1]
  if (!(fit.method %in% c("OLS","DLS","Robust"))) {
    stop("Invalid argument: fit.method must be 'OLS', 'DLS' or 'Robust'")
  }
  variable.selection = variable.selection[1]
  if (!(variable.selection %in% c("none","stepwise","subsets","lars"))) {
    stop("Invalid argument: variable.selection must be either 'none',
         'stepwise','subsets' or 'lars'")
  }
  if (missing(factor.names) && !is.null(mkt.name)) {
    factor.names <- NULL
  }
  
  if (xor(is.null(mkt.name), is.null(mkt.timing))) {
    stop("Missing argument: Both mkt.name and mkt.timing are necessary to add 
         market timing factors")
  }
  
  # extract arguments to pass to different fit and variable selection functions
  decay <- control$decay
  nvmin <- control$nvmin
  lars.criterion <- control$lars.criterion
  m1 <- match(c("weights","model","x","y","qr"), 
              names(control), 0L)
  lm.args <- control[m1, drop=TRUE]
  m2 <-  match(c("weights","model","x","y","nrep"), 
               names(control), 0L)
  lmRob.args <- control[m2, drop=TRUE]
  m3 <-  match(c("scope","scale","direction","trace","steps","k"), 
               names(control), 0L)
  step.args <- control[m3, drop=TRUE]
  m4 <-  match(c("weights","nvmax","force.in","force.out","method",
                 "really.big"), names(control), 0L)
  regsubsets.args <- control[m4, drop=TRUE]
  m5 <-  match(c("type","normalize","eps","max.steps","trace"), 
               names(control), 0L)
  lars.args <- control[m5, drop=TRUE]
  m6 <-  match(c("K","type","normalize","eps","max.steps","trace"), 
               names(control), 0L)
  cv.lars.args <- control[m6, drop=TRUE]
  
  # convert data into an xts object and hereafter work with xts objects
  data.xts <- checkData(data)
  # convert index to 'Date' format for uniformity 
  time(data.xts) <- as.Date(time(data.xts))
  
  # extract columns to be used in the time series regression
  dat.xts <- merge(data.xts[,asset.names], data.xts[,factor.names])
  ### After merging xts objects, the spaces in names get converted to periods
  
  # convert all asset and factor returns to excess return form if specified
  if (!is.null(rf.name)) {
    dat.xts <- "[<-"(dat.xts,,vapply(dat.xts, function(x) x-data.xts[,rf.name], 
                                     FUN.VALUE = numeric(nrow(dat.xts))))
  } else {
    warning("Excess returns were not computed.")
  }
  
  # opt add mkt-timing factors: down.market=max(0,Rf-Rm), market.sqd=(Rm-Rf)^2
  if("HM" %in% mkt.timing) {
    down.market <- data.xts[,mkt.name]
    down.market [down.market > 0] <- 0
    dat.xts <- merge.xts(dat.xts,down.market)
    colnames(dat.xts)[dim(dat.xts)[2]] <- "down.market"
    factor.names <- c(factor.names, "down.market")
  }
  if("TM" %in% mkt.timing) {
    market.sqd <- data.xts[,mkt.name]^2   
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
                                    fit.method, lm.args, lmRob.args, decay)
  } else if (variable.selection == "stepwise") {
    reg.list <- SelectStepwise(dat.xts, asset.names, factor.names, fit.method, 
                               lm.args, lmRob.args, step.args, decay)
  } else if (variable.selection == "subsets") {
    reg.list <- SelectAllSubsets(dat.xts, asset.names, factor.names, fit.method, 
                                 lm.args, lmRob.args, regsubsets.args, 
                                 nvmin, decay)
  } else if (variable.selection == "lars") {
    result.lars <- SelectLars(dat.xts, asset.names, factor.names, lars.args, 
                              cv.lars.args, lars.criterion)
    input <- list(call=call, data=dat.xts, asset.names=asset.names, 
                  factor.names=factor.names, fit.method=NULL, 
                  variable.selection=variable.selection)
    result <- c(result.lars, input)
    class(result) <- "tsfm"
    return(result)
  } 
  
  # extract coefficients from fitted factor models returned above
  coef.mat <- makePaddedDataFrame(lapply(reg.list, coef))
  alpha <- coef.mat[, 1, drop=FALSE]
  # to get alpha of class numeric, do: aplha <- coef.mat[,1]
  beta <- coef.mat[, -1, drop=FALSE]
  # reorder and expand columns of beta to match factor.names
  tmp <- matrix(NA, length(asset.names), length(factor.names))
  colnames(tmp) <- factor.names
  rownames(tmp) <- asset.names
  beta <- merge(beta, tmp, all.x=TRUE, sort=FALSE)[,factor.names, drop=FALSE]
  rownames(beta) <- asset.names
  # extract r2 and residual sd
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
                                lm.args, lmRob.args, decay){
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names) {
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
    # fit based on time series regression method chosen
    if (fit.method == "OLS") {
      reg.list[[i]] <- do.call(lm, c(list(fm.formula,data=reg.xts),lm.args))
    } else if (fit.method == "DLS") {
      lm.args$weights <- WeightsDLS(nrow(reg.xts), decay)
      reg.list[[i]] <- do.call(lm, c(list(fm.formula,data=reg.xts),lm.args))
    } else if (fit.method == "Robust") {
      reg.list[[i]] <- do.call(lmRob, c(list(fm.formula,data=reg.xts), 
                                        lmRob.args))
    } 
  } 
  reg.list  
}


### method variable.selection = "stepwise"
#
SelectStepwise <- function(dat.xts, asset.names, factor.names, fit.method, 
                           lm.args, lmRob.args, step.args, decay) {
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names) {
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
    # fit based on time series regression method chosen
    if (fit.method == "OLS") {
      lm.fit <- do.call(lm, c(list(fm.formula,data=reg.xts),lm.args))
      reg.list[[i]] <- do.call(step, c(list(lm.fit),step.args))
    } else if (fit.method == "DLS") {
      lm.args$weights <- WeightsDLS(nrow(reg.xts), decay)
      lm.fit <- do.call(lm, c(list(fm.formula,data=reg.xts),lm.args))
      reg.list[[i]] <- do.call(step, c(list(lm.fit),step.args))
    } else if (fit.method == "Robust") {
      lmRob.fit <- do.call(lmRob, c(list(fm.formula,data=reg.xts), lmRob.args))
      reg.list[[i]] <- do.call(step.lmRob, c(list(lmRob.fit), step.args))
    } 
  }
  reg.list
}


### method variable.selection = "subsets"
#
SelectAllSubsets <- function(dat.xts, asset.names, factor.names, fit.method, 
                             lm.args, lmRob.args, regsubsets.args, nvmin, 
                             decay) {
  
  # initialize list object to hold the fitted objects
  reg.list <- list()
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names) {
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    
    # formula to pass to lm or lmRob
    fm.formula <- as.formula(paste(i," ~ ."))
    
    if (fit.method=="DLS" && !"weights" %in% names(regsubsets.args)) {
      regsubsets.args$weights <- WeightsDLS(nrow(reg.xts), decay)
    }
    
    # choose best subset of factors depending on specified subset size
    fm.subsets <- do.call(regsubsets, c(list(fm.formula,data=reg.xts), 
                                        regsubsets.args))
    sum.sub <- summary(fm.subsets)
    
    # choose best model of a given subset size nvmax=nvmin (or) 
    # best model amongst subset sizes in [nvmin, nvmax]
    nvmax <- length(sum.sub$bic)
    best.size <- which.min(sum.sub$bic[nvmin:nvmax]) + nvmin -1
    names.sub <- names(which(sum.sub$which[best.size,-1]==TRUE))
    bic <- min(sum.sub$bic[nvmin:nvmax])
    
    # completely remove NA cases for chosen subset
    reg.xts <- na.omit(dat.xts[,c(i,names.sub)])
    
    # fit based on time series regression method chosen
    if (fit.method == "OLS") {
      reg.list[[i]] <- do.call(lm, c(list(fm.formula,data=reg.xts),lm.args))
    } else if (fit.method == "DLS") {
      lm.args$weights <- WeightsDLS(nrow(reg.xts), decay)
      reg.list[[i]] <- do.call(lm, c(list(fm.formula,data=reg.xts),lm.args))
    } else if (fit.method == "Robust") {
      reg.list[[i]] <- do.call(lmRob, c(list(fm.formula,data=reg.xts), 
                                        lmRob.args))
    } 
  }
  reg.list
}


### method variable.selection = "lars"
#
SelectLars <- function(dat.xts, asset.names, factor.names, lars.args, 
                       cv.lars.args, lars.criterion) {
  # initialize list object to hold the fitted objects and, vectors and matrices
  # for the other results
  asset.fit <- list()
  fitted.list <- list()
  alpha <- rep(NA, length(asset.names))
  beta <- matrix(NA, length(asset.names), length(factor.names))
  r2 <- rep(NA, length(asset.names))
  resid.sd <- rep(NA, length(asset.names))
  names(alpha)=names(r2)=names(resid.sd)=rownames(beta)=asset.names
  colnames(beta) <- factor.names
  
  # loop through and estimate model for each asset to allow unequal histories
  for (i in asset.names) {
    # completely remove NA cases
    reg.xts <- na.omit(dat.xts[, c(i, factor.names)])
    # convert to matrix
    reg.mat <- as.matrix(reg.xts)
    # fit lars regression model
    lars.fit <- do.call(lars, c(list(x=reg.mat[,factor.names],y=reg.mat[,i]),
                                lars.args))
    lars.sum <- summary(lars.fit)
    lars.cv <- do.call(cv.lars, c(list(x=reg.mat[,factor.names],y=reg.mat[,i], 
                                       mode="step"),cv.lars.args))
    # including plot.it=FALSE to cv.lars strangely gives an error: "Argument s 
    # out of range". And, specifying index=seq(nrow(lars.fit$beta)-1) resolves 
    # the issue, but care needs to be taken for small N
    
    # get the step that minimizes the "Cp" statistic or 
    # the K-fold "cv" mean-squared prediction error
    if (lars.criterion=="Cp") {
      s <- which.min(lars.sum$Cp)-1 # 2nd row is "step 1"
    } else {
      s <- which.min(lars.cv$cv)-1
    }
    # get factor model coefficients & fitted values at the step obtained above
    coef.lars <- predict(lars.fit, s=s, type="coef", mode="step")
    fitted.lars <- predict(lars.fit, reg.mat[,factor.names], s=s, type="fit", 
                           mode="step")
    fitted.list[[i]] <- xts(fitted.lars$fit, index(reg.xts))
    # extract and assign the results
    asset.fit[[i]] = lars.fit
    alpha[i] <- (fitted.lars$fit - 
                   reg.xts[,factor.names]%*%coef.lars$coefficients)[1]
    beta.names <- names(coef.lars$coefficients)
    beta[i, beta.names] <- coef.lars$coefficients
    r2[i] <-  lars.fit$R2[s+1]
    resid.sd[i] <- sqrt(lars.sum$Rss[s+1]/(nrow(reg.xts)-lars.sum$Df[s+1]))
    
  }
  if (length(asset.names)>1) {
    fitted.xts <- do.call(merge, fitted.list) 
  } else {
    fitted.xts <- fitted.list[[1]]
  }
  results.lars <- list(asset.fit=asset.fit, alpha=alpha, beta=beta, r2=r2, 
                       resid.sd=resid.sd, fitted=fitted.xts)
  # As a special case for variable.selection="lars", fitted values are also 
  # returned by fitTsfm. Else, step s from the best fit is needed to get 
  # fitted values & residuals.
}


### calculate exponentially decaying weights for fit.method="DLS"
## t = number of observations; d = decay factor
#
WeightsDLS <- function(t,d) {
  # more weight given to more recent observations 
  w <- d^seq((t-1),0,-1)    
  # ensure that the weights sum to unity
  w/sum(w)
}

### make a data frame (padded with NAs) from unequal vectors with named rows
## l = list of unequal vectors
#
makePaddedDataFrame <- function(l) {
  DF <- do.call(rbind, lapply(lapply(l, unlist), "[", 
                              unique(unlist(c(sapply(l,names))))))
  DF <- as.data.frame(DF)
  names(DF) <- unique(unlist(c(sapply(l,names))))
  # as.matrix(DF) # if matrix output needed
  DF
}

#' @param object a fit object of class \code{tsfm} which is returned by 
#' \code{fitTsfm}

#' @rdname fitTsfm
#' @method coef tsfm
#' @export

coef.tsfm <- function(object, ...) {
  # cbind alpha and beta; works for all fit and var selection methods
  coef.mat <- cbind(object$alpha, object$beta)
  # name for alpha/intercept column
  colnames(coef.mat)[1] <- "(Intercept)"
  return(coef.mat)
}

#' @rdname fitTsfm
#' @method fitted tsfm
#' @export

fitted.tsfm <- function(object, ...) {  
  if (object$variable.selection=="lars") {
    # generic method 'fitted' does not exist for "lars" fit objects
    # so, use fitted values returned by 'fitTsfm'
    fitted.xts <- object$fitted
  } else {
    if (length(object$asset.names)>1) {
      # get fitted values from each linear factor model fit 
      # and convert them into xts/zoo objects
      fitted.list = sapply(object$asset.fit, 
                           function(x) checkData(fitted(x,...)))
      # this is a list of xts objects, indexed by the asset name
      # merge the objects in the list into one xts object
      fitted.xts <- do.call(merge, fitted.list) 
    } else {
      fitted.xts <- checkData(fitted(object$asset.fit[[1]],...))
      colnames(fitted.xts) <- object$asset.names
    }
  }
  time(fitted.xts) <- as.Date(time(fitted.xts))
  return(fitted.xts)
}


#' @rdname fitTsfm
#' @method residuals tsfm
#' @export

residuals.tsfm <- function(object, ...) {
  if (object$variable.selection=="lars") {
    # generic method 'residuals' does not exist for "lars" fit objects
    # so, calculate them from the actual and fitted values
    residuals.xts <- object$data[,object$asset.names] - object$fitted
  } else {
    if (length(object$asset.names)>1) {
      # get residuals from each linear factor model fit 
      # and convert them into xts/zoo objects
      residuals.list = sapply(object$asset.fit, 
                              function(x) checkData(residuals(x,...)))
      # this is a list of xts objects, indexed by the asset name
      # merge the objects in the list into one xts object
      residuals.xts <- do.call(merge, residuals.list) 
    } else {
      residuals.xts <- checkData(residuals(object$asset.fit[[1]],...))
      colnames(residuals.xts) <- object$asset.names
    }
  }
  time(residuals.xts) <- as.Date(time(residuals.xts))
  return(residuals.xts)
}
