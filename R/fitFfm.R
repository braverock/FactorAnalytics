#' @title Fit a fundamental factor model using cross-sectional regression
#'
#' @description Fit a fundamental (cross-sectional) factor model using ordinary
#' least squares or robust regression. Fundamental factor models use observable
#' asset specific characteristics (or) fundamentals, like industry
#' classification, market capitalization, style classification (value, growth)
#' etc. to calculate the common risk factors. An object of class \code{"ffm"}
#' is returned.
#'
#' @details
#' Estimation method "LS" corresponds to ordinary least squares using 
#' \code{\link[stats]{lm}} and "Rob" is robust regression using 
#' \code{\link[robust]{lmRob}}. "WLS" is weighted least squares using estimates 
#' of the residual variances from LS regression as weights (feasible GLS). 
#' Similarly, "W-Rob" is weighted robust regression.
#' 
#' Standardizing style factor exposures: The exposures can be standardized into
#' z-scores using regular or robust (see \code{rob.stats}) measures of location 
#' and scale. Further, \code{weight.var}, a variable such as market-cap, can be 
#' used to compute the weighted mean exposure, and an equal-weighted standard 
#' deviation of the exposures about the weighted mean. This may help avoid an 
#' ill-conditioned covariance matrix. Default option equally weights exposures 
#' of different assets each period. 
#' 
#' If \code{rob.stats=TRUE}, \code{\link[robust]{covRob}} is used to compute a 
#' robust estimate of the factor covariance/correlation matrix, and, 
#' \code{\link[robustbase]{scaleTau2}} is used to compute robust tau-estimates 
#' of univariate scale for residuals during "WLS" or "W-Rob" regressions. When 
#' standardizing style exposures, the \code{\link[stats]{median}} and 
#' \code{\link[stats]{mad}} are used for location and scale respectively.
#' 
#' At this time, the regression can contain only one dummy exposure (one of 
#' industry, sector, country etc.) or intercept term, otherwise the exposure 
#' matrix will become singular. We plan to expand the function to allow 
#' specifying more than one dummy variable, and, dummy variable(s) in 
#' combination with an intercept term in the future. (Ex: Country + Sector + 
#' Intercept)
#' 
#' The original function was designed by Doug Martin and initially implemented
#' in S-PLUS by a number of University of Washington Ph.D. students:
#' Christopher Green, Eric Aldrich, and Yindeng Jiang. Guy Yollin ported the
#' function to R and Yi-An Chen modified that code. Sangeetha Srinivasan
#' re-factored, tested, corrected and expanded the functionalities and S3 
#' methods.
#'
#' @importFrom stats lm as.formula coef contr.treatment fitted mad median model.matrix
#'             na.exclude na.fail na.omit var 
#' @importFrom robustbase scaleTau2 covOGK
#' @importFrom PerformanceAnalytics checkData
#' @importFrom robust covRob covClassic lmRob
#'
#' @param data data.frame of the balanced panel data containing the variables 
#' \code{asset.var}, \code{ret.var}, \code{exposure.vars}, \code{date.var} and 
#' optionally, \code{weight.var}.
#' @param asset.var character; name of the variable for asset names.
#' @param ret.var character; name of the variable for asset returns.
#' @param date.var character; name of the variable containing the dates 
#' coercible to class \code{Date}.
#' @param exposure.vars vector; names of the variables containing the 
#' fundamental factor exposures.
#' @param weight.var character; name of the variable containing the weights 
#' used when standarizing style factor exposures. Default is \code{NULL}. See 
#' Details.
#' @param fit.method method for estimating factor returns; one of "LS", "WLS" 
#' "Rob" or "W-Rob". See details. Default is "LS".
#' @param rob.stats logical; If \code{TRUE}, robust estimates of covariance, 
#' correlation, location and univariate scale are computed as appropriate (see 
#' Details). Default is \code{FALSE}.
#' @param full.resid.cov logical; If \code{TRUE}, a full residual covariance 
#' matrix is estimated. Otherwise, a diagonal residual covariance matrix is 
#' estimated. Default is \code{FALSE}.
#' @param z.score logical; If \code{TRUE}, style exposures will be converted to 
#' z-scores; weights given by \code{weight.var}. Default is \code{FALSE}.
#' @param addIntercept logical; If \code{TRUE}, intercept is added in the exposure matrix. Deafault is \code{FALSE},
#' @param lagExposures logical; If \code{TRUE}, the style exposures in the exposure matrix are lagged by one time period. Deafault is \code{FALSE},
#' @param ... potentially further arguments passed.
#' 
#' @return \code{fitFfm} returns an object of class \code{"ffm"} for which 
#' \code{print}, \code{plot}, \code{predict} and \code{summary} methods exist. 
#' 
#' The generic accessor functions \code{coef}, \code{fitted} and 
#' \code{residuals} extract various useful features of the fit object. 
#' Additionally, \code{fmCov} computes the covariance matrix for asset returns 
#' based on the fitted factor model.
#' 
#' An object of class \code{"ffm"} is a list containing the following 
#' components:
#' \item{factor.fit}{list of fitted objects that estimate factor returns in each 
#' time period. Each fitted object is of class \code{lm} if 
#' \code{fit.method="LS" or "WLS"}, or, class \code{lmRob} if 
#' \code{fit.method="Rob" or "W-Rob"}.}
#' \item{beta}{N x K matrix of factor exposures for the last time period.}
#' \item{factor.returns}{xts object of K-factor returns (including intercept).}
#' \item{residuals}{xts object of residuals for N-assets.}
#' \item{r2}{length-T vector of R-squared values.}
#' \item{factor.cov}{K x K covariance matrix of the factor returns.}
#' \item{resid.cov}{N x N covariance matrix of residuals.}
#' \item{return.cov}{N x N return covariance estimated by the factor model, 
#' using the factor exposures from the last time period.}
#' \item{restriction.mat}{The restriction matrix used in the computation of f=Rg.}
#' \item{resid.var}{length-N vector of residual variances.}
#' \item{call}{the matched function call.}
#' \item{data}{data frame object as input.}
#' \item{date.var}{date.var as input}
#' \item{ret.var}{ret.var as input}
#' \item{asset.var}{asset.var as input.}
#' \item{exposure.vars}{exposure.vars as input.}
#' \item{weight.var}{weight.var as input.}
#' \item{fit.method}{fit.method as input.}
#' \item{asset.names}{length-N vector of asset names.}
#' \item{factor.names}{length-K vector of factor.names.}
#' \item{time.periods}{length-T vector of dates.}
#' Where N is the number of assets, K is the number of factors (including the 
#' intercept or dummy variables) and T is the number of unique time periods.
#'
#' @author Sangeetha Srinivasan, Guy Yollin,  Yi-An Chen and Avinash Acharya
#'
#' @references
#' Menchero, J. (2010). The Characteristics of Factor Portfolios. Journal of
#' Performance Measurement, 15(1), 52-62.
#'
#' Grinold, R. C., & Kahn, R. N. (2000). Active portfolio management (Second
#' Ed.). New York: McGraw-Hill.
#'
#' 
#' And, the following extractor functions: \code{\link[stats]{coef}}, 
#' \code{\link[stats]{fitted}}, \code{\link[stats]{residuals}},
#' \code{\link{fmCov}}, \code{\link{fmSdDecomp}}, \code{\link{fmVaRDecomp}} 
#' and \code{\link{fmEsDecomp}}.
#' 
#' \code{\link{paFm}} for Performance Attribution.
#'
#' @examples
#'
#' 
#' # Load fundamental and return data
#'  data("factorDataSetDjia5Yrs")
#' 
#' # fit a fundamental factor model
#' exposure.vars <- c("P2B", "MKTCAP")
#' fit <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'               date.var="DATE", exposure.vars=exposure.vars)
#' names(fit)
#' 
#' # fit a Industry Factor Model with Intercept
#' exposure.vars <- c("SECTOR","P2B")
#' fit1 <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                date.var="DATE", exposure.vars=exposure.vars, addIntercept=TRUE)
#'                
#' # Fit a SECTOR+COUNTRY+Style model with Intercept
#' # Create a COUNTRY column with just 3 countries
#' 
#'  factorDataSetDjia5Yrs$COUNTRY = rep(rep(c(rep("US", 1 ),rep("INDIA", 1),
#'                                        rep("GERMANY", 1 )), 10), 60)
#'  exposure.vars= c("SECTOR", "COUNTRY","P2B", "MKTCAP")
#'  
#'  fit.MICM <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                    date.var="DATE", exposure.vars=exposure.vars, addIntercept=TRUE)

#' @export




fitFfm <- function(data, asset.var, ret.var, date.var, exposure.vars, 
                   weight.var=NULL, fit.method=c("LS","WLS","Rob","W-Rob"), 
                   rob.stats=FALSE, full.resid.cov=FALSE, z.score=FALSE,addIntercept = FALSE,lagExposures=FALSE, ...) {
  
  # record the call as an element to be returned
  this.call <- match.call()
  
  # set defaults and check input validity
  if (missing(data) || !is.data.frame(data)) {
    stop("Invalid args: data must be a data.frame")
  }
  fit.method = fit.method[1]
  if (!(fit.method %in% c("LS","WLS","Rob","W-Rob"))) {
    stop("Invalid args: fit.method must be 'LS', 'WLS', 'Rob' or 'W-Rob'")
  }
  if (missing(asset.var) || !is.character(asset.var)) {
    stop("Invalid args: asset.var must be a character string")
  }
  if (missing(date.var) || !is.character(date.var)) {
    stop("Invalid args: date.var must be a character string")
  }
  if (missing(ret.var) || !is.character(ret.var)) {
    stop("Invalid args: ret.var must be a character string")
  }
  if (missing(exposure.vars) || !is.character(exposure.vars)) {
    stop("Invalid args: exposure.vars must be a character vector")
  }
  if (ret.var %in% exposure.vars) {
    stop("Invalid args: ret.var can not also be an exposure")
  }
  if (!is.null(weight.var) && !is.character(weight.var)) {
    stop("Invalid args: weight.var must be a character string")
  }
  if (!is.logical(rob.stats) || length(rob.stats) != 1) {
    stop("Invalid args: control parameter 'rob.stats' must be logical")
  }
  if (!is.logical(full.resid.cov) || length(full.resid.cov) != 1) {
    stop("Invalid args: control parameter 'full.resid.cov' must be logical")
  }
  if (!is.logical(z.score) || length(z.score) != 1) {
    stop("Invalid args: control parameter 'z.score' must be logical")
  }
  
  # initialize to avoid R CMD check's NOTE: no visible binding for global var
  DATE=NULL 
  W=NULL
  model.MSCI=FALSE
  model.styleOnly = FALSE
  restriction.mat = NULL
  # ensure dates are in required format
  data[[date.var]] <- as.Date(data[[date.var]])
  # extract unique time periods from data
  time.periods <- unique(data[[date.var]])
  TP <- length(time.periods)
  if (TP < 2) {
    stop("Invalid args: at least 2 unique time periods are required to fit the 
         factor model")
  }
  
  # order data.frame by date.var
  data <- data[order(data[,date.var]),]
  
  # extract asset names from data
  asset.names <- unique(data[[asset.var]])
  N <- length(asset.names)
  
  # check number & type of exposure; convert character exposures to dummy vars
  which.numeric <- sapply(data[,exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- exposure.vars[which.numeric]
  exposures.char <- exposure.vars[!which.numeric]
  if (length(exposures.char) > 1)
  {
    model.MSCI = TRUE
  }
  if (length(exposures.char) == 0)
  {
    model.styleOnly = TRUE
  }
  if(lagExposures)
  {
    data <- data[order(data[,date.var]),]
    #Remove data corresponding to the first time period
    data.lagged <- data[-(1:N),]
    #Get the style exposures except for the last time period
    dataExpoLagged <- data[1:((TP-1)*N), exposures.num] 
    #Replace style expo with lagged expo
    data.lagged[,exposures.num] <- dataExpoLagged
    data <- data.lagged
    time.periods <- unique(data[[date.var]])
    TP <- length(time.periods)
  }
  
  
  # convert numeric exposures to z-scores
  if (z.score) {
    if (!is.null(weight.var)) {
      # weight exposures within each period using weight.var
      w <- unlist(by(data=data, INDICES=data[[date.var]], 
                     function(x) x[[weight.var]]/sum(x[[weight.var]])))
    } else {
      w <- rep(1, nrow(data))
    }
    # calculate z-scores looping through all numeric exposures
    for (i in exposures.num) {
      std.expo.num <- by(data=data, INDICES=data[[date.var]], FUN=zScore,
                         i=i, w=w, rob.stats=rob.stats)
      data[[i]] <- unlist(std.expo.num)
    }
  }
  if(!model.MSCI)
  {
    # determine factor model formula to be passed to lm or lmRob
    fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse="+"))
    if (length(exposures.char))
    {
      fm.formula <- paste(fm.formula, "- 1")
      data[, exposures.char] <- as.factor(data[,exposures.char])
      contrasts.list <- lapply(seq(length(exposures.char)), function(i) 
        function(n) contr.treatment(n, contrasts=FALSE))
      names(contrasts.list) <- exposures.char
    }
    else
    {
      if (!addIntercept && model.styleOnly) {
        fm.formula <- paste(fm.formula, "- 1")}
      contrasts.list <- NULL
    }
    # convert the pasted expression into a formula object
    fm.formula <- as.formula(fm.formula)
  }
  # estimate factor returns using LS or Robust regression
  # returns a list of the fitted lm or lmRob objects for each time period
  if ((addIntercept == FALSE || model.styleOnly ==TRUE) && model.MSCI == FALSE)
  {
    if (grepl("LS",fit.method)) 
    {
      reg.list <- by(data=data, INDICES=data[[date.var]], FUN=lm, 
                     formula=fm.formula, contrasts=contrasts.list, 
                     na.action=na.fail)
    } 
    else if (grepl("Rob",fit.method))
    {
      reg.list <- by(data=data, INDICES=data[[date.var]], FUN=lmRob, 
                     formula=fm.formula, contrasts=contrasts.list, 
                     mxr=200, mxf=200, mxs=200, na.action=na.fail)
    }
    # compute residual variance for all assets for weighted regression
    if (grepl("W",fit.method)) {
      if (rob.stats) {
        resid.var <- apply(sapply(reg.list, residuals), 1, scaleTau2)^2
      } else {
        resid.var <- apply(sapply(reg.list, residuals), 1, var)
      }
      # add column of weights to data replicating resid.var for each period
      data <- cbind(data, W=1/resid.var)
    }
    
    # estimate factor returns using WLS or weighted-Robust regression
    # returns a list of the fitted lm or lmRob objects for each time period
    if (fit.method=="WLS") {
      reg.list <- by(data=data, INDICES=data[[date.var]], 
                     FUN=function(x) {
                       lm(data=x, formula=fm.formula, contrasts=contrasts.list, 
                          na.action=na.fail, weights=W)
                     })
    } else if (fit.method=="W-Rob") {
      reg.list <- by(data=data, INDICES=data[[date.var]], 
                     FUN=function(x) {
                       lmRob(data=x, formula=fm.formula, contrasts=contrasts.list, 
                             na.action=na.fail, weights=W, 
                             mxr=200, mxf=200, mxs=200)
                     })
    }
    
    ## Compute or Extract objects to be returned
    
    # number of factors including Market and dummy variables
    if (length(exposures.char)) {
      factor.names <- c(exposures.num, 
                        paste(levels(data[,exposures.char]),sep=""))
    } else {
      if(addIntercept) factor.names <- c("Alpha", exposures.num)
      else factor.names <- exposures.num
    }
    K <- length(factor.names)
    # exposure matrix B or beta for the last time period - N x K
    beta <- model.matrix(fm.formula, data=subset(data, DATE==time.periods[TP]))
    rownames(beta) <- asset.names
    colnames(beta) = gsub("COUNTRY|SECTOR|GICS.", "", colnames(beta))
    #Remove SECTOR/COUNTRY from the coef names.
    if (length(exposures.char) >0 )
    { 
      reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients) = gsub("COUNTRY|SECTOR|GICS.", "",names(reg.list[[x]]$coefficients) ) ;reg.list[[x]]})
    }else if(model.styleOnly && addIntercept)
    {
      reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients)[1] = "Alpha";reg.list[[x]]})
    }
    
    names(reg.list) = as.character(unique(data[[date.var]]))
    
    # time series of factor returns = estimated coefficients in each period
    factor.returns <- sapply(reg.list, function(x) {
      temp <- coef(x) 
      temp[match(factor.names, names(temp))]})
    # simplify factor.names for dummy variables
    if (length (exposures.char)) {
      factor.names <- c(exposures.num, levels(data[,exposures.char]))
    }
    rownames(factor.returns) <- factor.names
    factor.returns <- checkData(t(factor.returns)) # T x K
    
    # time series of residuals
    residuals <- sapply(reg.list, residuals) #  NxT
    row.names(residuals) <- asset.names
    residuals<- checkData(t(residuals)) #TxN
    
    
    # r-squared values for each time period
    r2 <- sapply(reg.list, function(x) summary(x)$r.squared)
    
    # factor and residual covariances
    if (rob.stats) {
      if (kappa(na.exclude(coredata(factor.returns))) < 1e+10) {
        factor.cov <- covRob(coredata(factor.returns), estim="pairwiseGK", 
                             distance=FALSE, na.action=na.omit)$cov
      } else {
        cat("Covariance matrix of factor returns is singular.\n")
        factor.cov <- covRob(coredata(factor.returns), distance=FALSE, 
                             na.action=na.omit)$cov
      }
      resid.var <- apply(coredata(residuals), 2, scaleTau2, na.rm=T)^2
      if (full.resid.cov) {
        resid.cov <- covOGK(coredata(residuals), sigmamu=scaleTau2, n.iter=1)$cov
      } else {
        resid.cov <- diag(resid.var)
      }
    } else {
      factor.cov <- covClassic(coredata(factor.returns), distance=FALSE, 
                               na.action=na.omit)$cov
      resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
      if (full.resid.cov) {
        resid.cov <- covClassic(coredata(residuals), distance=FALSE, 
                                na.action=na.omit)$cov
      } else {
        resid.cov <- diag(resid.var)
      }
    }
    
    
    # return covariance estimated by the factor model
    return.cov <-  beta %*% factor.cov %*% t(beta) + resid.cov
    
    
    if(addIntercept) colnames(beta)[1] = "Alpha"
    beta = beta[, colnames(factor.returns)]
    
  }
  #If Market+Sector/Country is required
  else if (addIntercept == TRUE && model.MSCI == FALSE && model.styleOnly ==FALSE)
  {
    formula.expochar = as.formula(paste(ret.var, "~", exposures.char, "-1"))
    factor.names <- c("Market",
                      paste(levels(data[,exposures.char]),sep=" "), exposures.num)
    beta <- model.matrix(fm.formula, data=data)
    rownames(beta) <- rep(asset.names, length(time.periods))
    
    beta.expochar <- model.matrix(formula.expochar, data=data)
    rownames(beta.expochar) <- rep(asset.names, length(time.periods))
    
    beta.star <- cbind("Market" = rep(1, nrow(beta.expochar)), beta.expochar)
    if(length(exposures.num) > 0){
      beta.style<- matrix(beta[,exposures.num], ncol = length(exposures.num))
      colnames(beta.style) = exposures.num}
    
    asset.names <- unique(data[[asset.var]])
    N <- length(asset.names)
    
    #Define Retrun matrix 
    ret.matrix = matrix(data[[ret.var]],nrow = N)
    #Initial regression to get residual variances. 
    reg.list <- by(data=data, INDICES=data[[date.var]], FUN=lm, 
                   formula=fm.formula, na.action=na.fail)
    resid.var <- apply(sapply(reg.list, residuals), 1, var)
    #V Inverse Matrix of resid variances
    resid.var.inv = diag(resid.var^-1) 
    # V^0.5 Inverse matrix
    resid.sd.inv = sqrt(resid.var.inv) 
    #K is with the Market
    K <- dim(beta.star)[2]
    #Define Restriction matrix 
    R_matrix = rbind(diag(K-1), c(0,rep(-1,K-2)))
    # Modify return matrix to get r~ as in Menchero Equation A6
    ret_star = resid.sd.inv %*% ret.matrix 
    #Y matrix as in Eq.A6
    row.names(ret_star) = asset.names
    colnames(ret_star) = as.character(time.periods)
    #ret_star = checkData(t(ret_star))
    
    reg.list<- list()
    for(i in 1:length(time.periods))
    {
      B.mod = ( beta.star[((i-1)*N+1) : (i*N), ]) %*% R_matrix  #Y = X*R
      #Find g as in Eq.A7
      if(length(exposures.num) > 0)
      {
        B.style = beta.style[((i-1)*N+1) : (i*N), ]
        reg.list[[i]] = lm(ret_star[,i] ~ B.mod + B.style -1)
      }
      else
        reg.list[[i]] = lm(ret_star[,i] ~ B.mod-1)
    }
    
    reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients) =  paste("g", seq(1:length(reg.list[[x]]$coefficients)), sep = "");reg.list[[x]]})
    names(reg.list) = as.character(unique(data[[date.var]]))
    
    g = sapply(reg.list, function(x) coef(x))
    
    factor.returns  = R_matrix %*% g[1:(K-1), ]
    if(length(exposures.num) > 0)
      factor.returns = rbind(factor.returns, g[K:nrow(g), ])
    rownames(factor.returns) <- factor.names
    colnames(factor.returns) = as.character(unique(data[[date.var]]))
    residuals = sapply(reg.list, residuals)
    colnames(residuals) = as.character(unique(data[[date.var]]))
    # Create a T x N xts object of residuals
    residuals <- checkData(t(residuals))
    r2<- sapply(reg.list, function(x) summary(x)$r.squared)
    names(r2) = as.character(unique(data[[date.var]]))
    factor.returns <- checkData(t(factor.returns)) # T x K
    #Fac Covarinace
    factor.cov <- covClassic(coredata(factor.returns), distance=FALSE, 
                             na.action=na.omit)$cov
    #Residual Variance
    resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
    names(resid.var) <- asset.names
    resid.cov <- diag(resid.var)
    #Returns covariance
    if(length(exposures.num) > 0){
      beta.combine = cbind(beta.star, beta.style)
    }else 
      beta.combine = beta.star
    return.cov <-  beta.combine[((TP-1)*N+1):(TP*N), 1:ncol(beta.combine)] %*% factor.cov %*% t( beta.combine[((TP-1)*N+1):(TP*N), 1:ncol(beta.combine)]) + resid.cov
    #Exposure matrix 
    beta = beta.combine[((TP-1)*N+1):(TP*N), 1:ncol(beta.combine)]
    colnames(beta) = gsub("COUNTRY|SECTOR|GICS.", "", colnames(beta))
    factor.names<- c("Market", exposures.num,
                     paste(levels(data[,exposures.char]),sep=" "))
    #Re-order the columns mkt-style-sector/country
    factor.returns = factor.returns[, factor.names]
    beta = beta[, factor.names]
    factor.cov = factor.cov[factor.names, factor.names]
    
    #Restriction matrix
    restriction.mat = R_matrix
  }
  else if(model.MSCI)
  {
    
    # determine factor model formula to be passed to lm
    fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse="+"))
    if (length(exposures.char)) {
      fm.formula <- paste(fm.formula, "- 1")
      for(i in exposures.char)
      {
        data[, i] <- as.factor(data[,i])
        if (grepl("SECTOR",i)) 
          formula.ind = as.formula(paste(ret.var, "~", i, "-1"))
        else formula.cty = as.formula(paste(ret.var, "~", i, "-1"))
      }
    }
    
    # convert the pasted expression into a formula object
    fm.formula <- as.formula(fm.formula)
    beta <- model.matrix(fm.formula, data=data)
    beta.ind <- model.matrix(formula.ind, data=data)
    beta.cty <- model.matrix(formula.cty, data=data)
    beta.mic <- cbind("Market" = rep(1, nrow(beta.ind)), beta.ind, beta.cty)
    if(length(exposures.num) > 0)
      beta.style<- beta[,exposures.num]
    
    fac.names.indcty = lapply(seq(exposures.char), function(x)
      paste(levels(data[,exposures.char[x]]),sep=""))
    if(grepl("SECTOR", exposures.char[1])){
      factor.names <- c("Market",unlist(fac.names.indcty),
                        exposures.num)
    }else{
      factor.names <- c("Market", unlist((fac.names.indcty)[2]),unlist((fac.names.indcty)[1]),
                        exposures.num)
    }
    rownames(beta.mic) <- rep(asset.names, TP)
    asset.names <- unique(data[[asset.var]])
    N <- length(asset.names)
    #Define Retrun matrix 
    returns = matrix(data[[ret.var]],nrow = N)
    K <- length(factor.names)
    K1<- dim(beta.ind)[2]
    K2<- dim(beta.cty)[2]
    #Define Restriction matrix 
    rMic<-  rbind( cbind(diag(K1), matrix(0, nrow = K1, ncol = K2-1)), 
                   c(c(0,rep(-1, K1-1)), rep(0, K2-1)),
                   cbind(matrix(0, ncol = K1, nrow = K2-1), diag(K2-1)),
                   c(rep(0, K1), rep(-1, K2-1)))
    
    row.names(returns) = asset.names
    colnames(returns) = as.character(time.periods)
    
    reg.list<- list()
    
    for(i in 1:length(time.periods))
    {
      B.mod = ( beta.mic[((i-1)*N+1) : (i*N), ]) %*% rMic  #Y = X*R
      #Find g as in Eq.A7
      if(length(exposures.num) > 0)
      {
        B.style = beta.style[((i-1)*N+1) : (i*N), ]
        reg.list[[i]] = lm(returns[,i] ~ B.mod + B.style -1)
      }
      else
        reg.list[[i]] = lm(returns[,i] ~ B.mod-1)
    }
    reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients) =  paste("g", seq(1:length(reg.list[[x]]$coefficients)), sep = "");reg.list[[x]]})
    names(reg.list) = as.character(unique(data[[date.var]]))
    g = sapply(reg.list, function(x) coef(x))
    factor.returns  = rMic %*% g[1:(K1+K2-1), ]
    if(length(exposures.num) > 0)
      factor.returns = rbind(factor.returns, g[(K1+K2):nrow(g), ])
    rownames(factor.returns) <- factor.names
    colnames(factor.returns) = as.character(unique(data[[date.var]]))
    if(length(exposures.num) > 0){
      residuals = returns - B.mod %*% g[1:(K1+K2-1), ] - B.style %*% g[(K1+K2):nrow(g), ] #NxT
    }else residuals = returns - B.mod %*% g[1:(K1+K2-1), ]
    colnames(residuals) = as.character(unique(data[[date.var]]))
    # Create a T x N xts object of residuals
    residuals <- checkData(t(residuals))
    #all.equal(x,residuals)
    r2<- sapply(reg.list, function(x) summary(x)$r.squared)
    #r2 <- as.numeric(sapply(X = summary(reg.list), FUN = "[","r.squared"))
    names(r2) = as.character(unique(data[[date.var]]))
    factor.returns <- checkData(t(factor.returns)) # T x K
    #Fac Covarinace
    factor.cov <- covClassic(coredata(factor.returns), distance=FALSE, 
                             na.action=na.omit)$cov
    #Residual Variance
    resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
    names(resid.var) <- asset.names
    resid.cov <- diag(resid.var)
    #Returns covariance
    if(length(exposures.num) > 0){
      beta.combine = cbind(beta.mic, beta.style)
    }else 
      beta.combine = beta.mic
    return.cov <-  beta.combine[((TP-1)*N+1):(TP*N), 1:K] %*% factor.cov %*% t( beta.combine[((TP-1)*N+1):(TP*N), 1:K]) + resid.cov
    #Exposure matrix 
    beta = beta.combine[((TP-1)*N+1):(TP*N), 1:K]
    colnames(beta) = gsub("COUNTRY|SECTOR|GICS.", "", colnames(beta))
    
    #Re-order the columns in the order mkt-style-sector-country
    if(length(exposures.num)>0)
      factor.returns <- factor.returns[,c(1,(K1+2+K2):K, 2:(K1+1), (K1+2):(K1+K2+1))]
    factor.names <- colnames(factor.returns)
    beta = beta[, factor.names]
    #factor.cov = factor.cov[factor.names, factor.names]
    #Restriction matrix
    restriction.mat = rMic
  }
  
  
  # create list of return values.
  result <- list(factor.fit=reg.list, beta=beta, factor.returns=factor.returns, 
                 residuals=residuals, r2=r2, factor.cov=factor.cov, 
                 resid.cov=resid.cov, return.cov=return.cov, restriction.mat=restriction.mat,
                 resid.var=resid.var, call=this.call, 
                 data=data, date.var=date.var, ret.var=ret.var, 
                 asset.var=asset.var, exposure.vars=exposure.vars, 
                 weight.var=weight.var, fit.method=fit.method, 
                 asset.names=asset.names, factor.names=factor.names,
                 time.periods=time.periods)
  
  class(result) <- "ffm"
  return(result)
  }


### function to calculate z-scores for numeric exposure i using weights w
## x is a data.frame object, i is a character string and w has same length as x 
# rob.stats is a logical argument to compute robust location and scale

zScore <- function (x, i, w, rob.stats) {
  if (rob.stats) {
    x_bar <- median(w*x[[i]])
    (x[[i]] - x_bar)/mad(x[[i]], center=x_bar)
  } else {
    x_bar <- mean(w*x[[i]]) 
    n <- length(x[[i]])
    # use equal weighted squared deviation about the weighted mean
    (x[[i]] - x_bar)/sqrt(sum((x[[i]]-x_bar)^2)/(n-1))
  }
}


#' @param object a fit object of class \code{ffm} which is returned by 
#' \code{fitFfm}

#' @rdname fitFfm
#' @method coef ffm
#' @export

coef.ffm <- function(object, ...) {
  # these are the last period factor exposures
  # already computed through fitFfm
  return(object$beta)
}

#' @rdname fitFfm
#' @method fitted ffm
#' @export

fitted.ffm <- function(object, ...) {  
  # get fitted values for all assets in each time period
  # transpose and convert into xts/zoo objects
  fitted.xts <- checkData(t(sapply(object$factor.fit, fitted)))
  names(fitted.xts) <- object$asset.names
  return(fitted.xts)
}

#' @rdname fitFfm
#' @method residuals ffm
#' @export

residuals.ffm <- function(object, ...) {
  return(object$residuals)
}



