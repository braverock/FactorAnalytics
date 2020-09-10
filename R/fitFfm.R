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
#' The weights to be used in "WLS" or "W-Rob" can be set using 
#' \code{resid.scaleType} argument which computes the residual variances in one of the following ways - 
#' sample variace, EWMA, Robust EWMA and GARCH(1,1). The inverse of these residual variances
#'  are used as the weights. For EWMA model, lambda = 0.9 is used as default and for GARCH(1,1) 
#'  omega = 0.09, alpha = 0.1, and beta = 0.81 are used as default as mentioned in Martin & Ding (2017).
#'  These default parameters can be changed using the arguments \code{lambda}, \code{GARCH.params} for EWMA and GARCH respectively.
#'  To compute GARCH parameters via MLE, set \code{GARCH.MLE} to \code{TRUE}. 
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
#' When \code{resid.scaleType} is EWMA or GARCH, the residual covariance is equal to the 
#' diagonal matrix of the estimated residual variances in last time period.
#' 
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
#' @importFrom PerformanceAnalytics checkData skewness kurtosis
#' @importFrom robust covRob covClassic lmRob
#' @importFrom rugarch ugarchspec ugarchfit
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
#' @param z.score method for exposure standardization; one of "none", "crossSection", or "timeSeries".
#' Default is \code{"none"}.
#' @param addIntercept logical; If \code{TRUE}, intercept is added in the exposure matrix. Default is \code{FALSE},
#' @param lagExposures logical; If \code{TRUE}, the style exposures in the exposure matrix are lagged by one time period. Default is \code{TRUE},
#' @param resid.scaleType character; Only valid when fit.method is set to WLS or W-Rob. The weights used in 
#' the weighted regression are estimated  using sample variance, classic EWMA, robust EWMA or GARCH model. Valid values are \code{stdDev}, \code{EWMA}, \code{robEWMA}, or \code{GARCH}.
#' Default is \code{stdDev} where the inverse of residual sample variances are used as the weights.
#' @param lambda lambda value to be used for the EWMA estimation of residual variances. Default is 0.9
#' @param GARCH.params list containing GARCH parameters omega, alpha, and beta. Default values are 0.09, 0.1, 0.81 respectively.
#' Valid only when \code{GARCH.MLE} is set to \code{FALSE}.
#' @param analysis method used in the analysis of fundamental law of active management; one of "none", "ISM", 
#' or "NEW". Default is "none".
#' @param stdReturn logical; If \code{TRUE}, the returns will be standardized using GARCH(1,1) volatilities. Default is \code{FALSE}
#' @param targetedVol numeric; the targeted portfolio volatility in the analysis. Default is 0.06.
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
#' \item{g.cov}{ covariance matrix of the g coefficients for a Sector plus market and Sector plus Country plus global market models .}
#' \item{resid.cov}{N x N covariance matrix of residuals.}
#' \item{return.cov}{N x N return covariance estimated by the factor model, 
#' using the factor exposures from the last time period.}
#' \item{restriction.mat}{The restriction matrix used in the computation of f=Rg.}
#' \item{resid.var}{N x T matrix of estimated residual variances. It will be a length-N vector of sample residual variances when \code{resid.scaleType} is set to \code{stdDev} }
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
#' \item{activeWeights}{active weights obtaining from the fundamental law of active management}
#' \item{activeReturns}{active returns corresponding to the active weights}
#' \item{IR}{the vector of Granold-K, asymptotic IR, and finite-sample IR.}
#' Where N is the number of assets, K is the number of factors (including the 
#' intercept or dummy variables) and T is the number of unique time periods.
#'
#' @author Sangeetha Srinivasan, Guy Yollin,  Yi-An Chen, Avinash Acharya and Chindhanai Uthaisaad
#'
#' @references
#' Menchero, J. (2010). The Characteristics of Factor Portfolios. Journal of
#' Performance Measurement, 15(1), 52-62.
#'
#' Grinold, R. C., & Kahn, R. N. (2000). Active portfolio management (Second
#' Ed.). New York: McGraw-Hill.
#' 
#' Ding, Z. and Martin, R. D. (2016). "The Fundamental Law of Active Management Redux", SSRN 2730434.
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
#'  factorDataSetDjia5Yrs$COUNTRY = rep(rep(c(rep("US", 1 ),rep("GERMANY", 1 )), 11), 60)
#'  exposure.vars= c("SECTOR", "COUNTRY","P2B", "MKTCAP")
#'  
#'  fit.MICM <- fitFfm(data=factorDataSetDjia5Yrs, asset.var="TICKER", ret.var="RETURN", 
#'                    date.var="DATE", exposure.vars=exposure.vars, addIntercept=TRUE)
#' 
#' @export


fitFfm <- function(data, asset.var, ret.var, date.var, exposure.vars, 
                   weight.var=NULL, fit.method=c("LS","WLS","Rob","W-Rob"), 
                   rob.stats=FALSE, full.resid.cov=FALSE, z.score = c("none", "crossSection", "timeSeries"), 
                   addIntercept = FALSE, lagExposures=TRUE, resid.scaleType = "stdDev",
                   lambda = 0.9, GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81), 
                   GARCH.MLE = FALSE, stdReturn = FALSE, analysis = c("none", "ISM", "NEW"), 
                   targetedVol = 0.06, ...) {
  
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
  if (!(resid.scaleType %in% c("stdDev","EWMA","robEWMA", "GARCH"))) {
    stop("Invalid args: resid.scaleType must be 'stdDev','EWMA','robEWMA', or 'GARCH'")
  }
  if ((resid.scaleType != "stdDev") && !(fit.method %in% c("WLS","W-Rob"))) {
    stop("Invalid args: resid.scaleType must be used with WLS or W-Rob")
  }
  if (!is.list(GARCH.params)) {
    stop("Invalid args: parameter 'GARCH.params' must be a list")
  }
  if (!is.logical(stdReturn)) {
    stop("Invalid args: stdReturn must be either 'TRUE' or 'FALSE' ") 
  }
  z.score = z.score[1]
  if (!(z.score %in% c("none", "crossSection", "timeSeries")) || length(z.score) != 1) {
    stop("Invalid args: control parameter 'z.score' must be either crossSection or timeSeries")
  }
  analysis = analysis[1]
  if (!(analysis %in% c("none", "ISM", "NEW")) || length(z.score) != 1) {
    stop("Invalid args: control parameter 'analysis' must be either ISM or NEW")
  }
  
  # initialize to avoid R CMD check's NOTE: no visible binding for global var
  DATE=NULL 
  W=NULL
  model.MSCI=FALSE
  model.styleOnly = FALSE
  restriction.mat = NULL
  g.cov = NULL
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
  rawReturns <- matrix(data[[ret.var]], nrow = N)
  
  # Standardize the returns if stdReturn = TRUE
  if (stdReturn) {
    sdReturns <- apply(rawReturns, 1, sd)
    sigmaGarch <- rawReturns
    for (i in 1:N) {
      ts <- rawReturns[i, ] ^ 2
      var_past_2 <- 0
      sigmaGarch[i, ] <- sapply(ts, function(x) var_past_2 <<- (1 - 0.10 - 0.81) * sdReturns[i] ^ 2 + 0.10 * x + 0.81 * var_past_2)
    }
    sigmaGarch <- sqrt(sigmaGarch)
    data[[ret.var]] <- as.vector(rawReturns / sigmaGarch)
  }
  stdReturns <- matrix(data[[ret.var]], nrow = N)
  
  # check number & type of exposure; convert character exposures to dummy vars
  which.numeric <- sapply(data[,exposure.vars,drop=FALSE], is.numeric)
  exposures.num <- exposure.vars[which.numeric]
  exposures.char <- exposure.vars[!which.numeric]
  if ((length(exposures.char) >1) && !addIntercept) {
    stop("Invalid args: two categorical factor model without Market(Interecept) is currenlty not handled")
  }
  
  if (length(exposures.char) > 2)
  {
	  stop("Invalid args: currently supports up to two categorical variables")
	    
  }
  
  if (length(exposures.char) > 1)
  { #Model has both Sector and Country along wit Intercept
    model.MSCI = TRUE
  }
  if (length(exposures.char) == 0)
  {
    model.styleOnly = TRUE
  }
  
  if(lagExposures)
  {
    data <- data[order(data[,date.var]),]
    #Get the style exposures except for the last time period
    dataExpoLagged <- data[1:((TP-1)*N), exposures.num]
    #Remove data corresponding to the first time period
    data.lagged <- data[-(1:N),]
    #Replace style expo with lagged expo
    data.lagged[,exposures.num] <- dataExpoLagged
    data <- data.lagged
    #Update the time period length
    time.periods <- unique(data[[date.var]])
    TP <- length(time.periods)
  }
  
  # Convert numeric exposures to z-scores
  if (!grepl(z.score, "none")) {
    if (!is.null(weight.var)) {
      # Weight exposures within each period using weight.var
      w <- unlist(by(data=data, INDICES=data[[date.var]], 
                     function(x) x[[weight.var]]/sum(x[[weight.var]])))
    } else {
      w <- rep(1, nrow(data))
    }
    # Calculate z-scores looping through all numeric exposures
    if (grepl(z.score, "crossSection")) {
      for (i in exposures.num) {
        std.expo.num <- by(data = data, INDICES = data[[date.var]], FUN = zScore,
                           i = i, w = w, rob.stats = rob.stats, z.score = z.score, 
                           asset.names = asset.names)
        data[[i]] <- unlist(std.expo.num)
      }
    } else {
      for (i in exposures.num) {
        data[[i]] <- zScore(x = data, i = i, w = w, rob.stats = rob.stats, 
                            z.score = z.score, asset.names = asset.names)
      }
    }
  }
  
  if(!model.MSCI)
  {
    # determine factor model formula to be passed to lm or lmRob
    fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse="+"))
    if (length(exposures.char))
    {
      #Remove Intercept as it introduces rank deficiency in the exposure matrix.
      #Implemetation with Intercept is handled later, using a Restriction matrix to remove the rank deficiency. 
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
  if (addIntercept == TRUE && model.MSCI == FALSE && model.styleOnly ==FALSE)
  {
    #formula to extract beta of Sec or Country
    formula.expochar = as.formula(paste(ret.var, "~", exposures.char, "-1"))
    factor.names <- c("Market",
                      paste(levels(data[,exposures.char]),sep=" "), exposures.num)
    beta.expochar <- model.matrix(formula.expochar, data=data)
    rownames(beta.expochar) <- rep(asset.names, length(time.periods))
    
    #Beta for the whole model (generally without intercept)
    beta <- model.matrix(fm.formula, data=data)
    rownames(beta) <- rep(asset.names, length(time.periods))
    #Define beta.star as Beta of the whole model with Intercept/Market represtend by col of ones
    beta.star <- cbind("Market" = rep(1, nrow(beta.expochar)), beta.expochar)
    if(length(exposures.num) > 0){
      beta.style<- matrix(beta[,exposures.num], ncol = length(exposures.num))
      colnames(beta.style) = exposures.num
      #Define Beta for Style factors
      B.style =beta.style[((TP-1)*N+1) : (TP*N), ]}
    #Number of factors
    K <- dim(beta.star)[2]
    #Define Restriction matrix 
    R_matrix = rbind(diag(K-1), c(0,rep(-1,K-2)))
    #Define B.Mod = X*R
    B.mod = (beta.star[1:N, ]) %*% R_matrix 
    #Formula for Markt+Sec/Country Model
    fmSI.formula = as.formula(paste(ret.var, "~", "B.mod+", paste(exposures.num, collapse="+"),"-1" ))                 
  }
  
  if(model.MSCI == FALSE)
  {
    #Perform regression using fm.formula without any restriction matrix, if WLS/WRob is the 
    #fit.method when addIntercept =TRUE. Else use fmSI.formula. 
    if (!(grepl("W",fit.method)) && addIntercept==TRUE && !model.styleOnly){
      fm.formula = fmSI.formula
      contrasts.list = NULL}
    
    # estimate factor returns using LS or Robust regression
    # returns a list of the fitted lm or lmRob objects for each time period
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
      #Compute cross-sectional weights using EWMA or GARCH
      if((resid.scaleType != "stdDev"))
      { #Extract Residuals
        res = sapply(reg.list, residuals)
        
        if(grepl("EWMA", resid.scaleType)){
          w<-matrix(0,N,TP)
          for(i in 1:N)
          {
            var_tminus1 = as.numeric(resid.var[i])
            for(j in 2:TP)
            {
              #ifelse conditon is used to check if robust EWMA weights has to be calculated.
              #The rejection threshold a=2.5 is used as mentioned in eq 6.6 of Martin (2005)
              w[i,j] = var_tminus1 + ((1-lambda)*(res[i,j]^2-var_tminus1)) * ifelse(resid.scaleType == "robEWMA", ifelse(abs(res[i,j] <= 2.5 * sqrt(var_tminus1)), 1, 0), 1)
              var_tminus1 = w[i,j]
            }
          }
          w[,1] = resid.var
        }
        #GARCH(1,1)
        else if(resid.scaleType == "GARCH") {
          #Compute parameters using MLE
          if(GARCH.MLE){
          garch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                                  mean.model=list(armaOrder=c(0,0), include.mean = FALSE),  
                                  distribution.model="norm")
          garch.weights = sapply(X = 1:nrow(res), FUN = function(X){(ugarchfit(garch.spec,res[X,]))@fit$var})
          w = t(garch.weights)
          }
          
        else {
          # use fixed parameters
          # default values of omega, Alpha and beta are based on Martin and Ding (2017)
          alpha = ifelse(!exists("alpha", where = GARCH.params), 0.1, GARCH.params$alpha)
          beta =  ifelse(!exists("beta", where = GARCH.params), 0.81, GARCH.params$beta )
          w<-matrix(0,N,TP)
          for(i in 1:N)
          {
            #Use sample variance as the initial variance
            w[,1] = resid.var
            var_tminus1 = as.numeric(resid.var[i])
            for(j in 2:TP)
            {
              w[i,j] = resid.var[i] * (1 - alpha - beta) + alpha * res[i,j-1]^2 + beta * var_tminus1
              var_tminus1 = w[i,j]
            }
          }

        }
          }
      data<- cbind(data, W = 1/as.numeric(w))
      }
      else
      {
        data <- cbind(data, W=1/resid.var)
      }
    }
    
    # estimate factor returns using WLS or weighted-Robust regression
    # returns a list of the fitted lm or lmRob objects for each time period
    if (fit.method=="WLS") {
      if(addIntercept  && !model.styleOnly){
        fm.formula = fmSI.formula
        contrasts.list = NULL}
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
  }
  
  ## Compute or Extract objects to be returned
  if ((addIntercept == FALSE || model.styleOnly ==TRUE) && model.MSCI == FALSE)
  {
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
    beta <- model.matrix(fm.formula, data=subset(data, data[[date.var]]==time.periods[TP]))
    rownames(beta) <- asset.names
    #Shorten the Sector/Country names
    colnames(beta) = gsub(paste(exposures.char,collapse="|"), "", colnames(beta))
    #colnames(beta) = gsub(paste(exposures.char), "", colnames(beta))
    #Remove SECTOR/COUNTRY to shorten the coef names.
    if (length(exposures.char) >0 )
    { 
      reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients) = gsub(paste(exposures.char,collapse="|"), "",names(reg.list[[x]]$coefficients) ) ;reg.list[[x]]})
      names(reg.list) = as.character(unique(data[[date.var]]))
    }else if(model.styleOnly && addIntercept)
    {
      reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients)[1] = "Alpha";reg.list[[x]]})
      names(reg.list) = as.character(unique(data[[date.var]]))
    }
    
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
      resid.var <- apply(coredata(residuals), 2, scaleTau2)^2
      if (full.resid.cov) {
        resid.cov <- covOGK(coredata(residuals), sigmamu=scaleTau2, n.iter=1)$cov
      } else {
        #if resid.scaleType is not stdDev, use the most recent residual var as the diagonal cov-var of residuals
        if((resid.scaleType != "stdDev")){
          row.names(w) = asset.names
          resid.cov <- diag(w[,ncol(w)])
          # update resid.var with the timeseries of estimated resid variances
          resid.var = as.xts(t(w), order.by = as.yearmon(time.periods))
        }
        else
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
        #if resid.scaleType is not stdDev, use the most recent residual var as the diagonal cov-var of residuals
        if((resid.scaleType != "stdDev")){
          row.names(w) = asset.names
          resid.cov <- diag(w[,ncol(w)])
          # update resid.var with the timeseries of estimated resid variances
          resid.var = as.xts(t(w), order.by = as.yearmon(time.periods))
        }
        else
          resid.cov <- diag(resid.var)
      }
    }
    # return covariance estimated by the factor model 
    #(here beta corresponds to the exposure of last time period,TP)
    return.cov <-  beta %*% factor.cov %*% t(beta) + resid.cov
    
    if(addIntercept) colnames(beta)[1] = "Alpha"
    beta = beta[, colnames(factor.returns)]
    
  }
  #If Market+Sector/Country is required
  else if (addIntercept == TRUE && model.MSCI == FALSE && model.styleOnly ==FALSE)
  {
    #Rename regression coefs
    reg.list= lapply(seq(1:TP), function(x){ names(reg.list[[x]]$coefficients) =  paste("g", seq(1:length(reg.list[[x]]$coefficients)), sep = "");reg.list[[x]]})
    names(reg.list) = as.character(unique(data[[date.var]]))
    #Extract g coef
    g = sapply(reg.list, function(x) coef(x))
    #factor returns = restriction matrix * g coefficients
    factor.returns  = R_matrix %*% g[1:(K-1), ]
    if(length(exposures.num) > 0)
      factor.returns = rbind(factor.returns, g[K:nrow(g), ])
    rownames(factor.returns) = factor.names
    colnames(factor.returns) = as.character(unique(data[[date.var]]))
    #Extract resid
    residuals = sapply(reg.list, residuals)
    colnames(residuals) = as.character(unique(data[[date.var]]))
    row.names(residuals) = asset.names
    # Create a T x N xts object of residuals
    residuals <- checkData(t(residuals))
    r2<- sapply(reg.list, function(x) summary(x)$r.squared)
    names(r2) = as.character(unique(data[[date.var]]))
    factor.returns <- checkData(t(factor.returns)) # T x K
    #Rearrange g,factor return to Mkt- Style Factor - Sec/Country order
    if(length(exposures.num) > 0) {g = g[c(1,K:nrow(g),2:(K-1)),]}
    factor.names<- c("Market", exposures.num,
                     paste(levels(data[,exposures.char]),sep=" "))
    factor.returns = factor.returns[, factor.names]
    #Fac Covarinace
    factor.cov <-covClassic(coredata(factor.returns), distance=FALSE, 
                            na.action=na.omit)$cov
    g.cov <- cov(t(g))
    #Residual Variance
    resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
    names(resid.var) <- asset.names
    # if resid.scaleType is not stdDev, use the most recent residual var as the diagonal cov-var of residuals
    if((resid.scaleType != "stdDev")){
      row.names(w) = asset.names
      resid.cov <- diag(w[,ncol(w)])
      # update resid.var with the timeseries of estimated resid variances
      resid.var = as.xts(t(w), order.by = as.yearmon(time.periods))
    }
    else
      resid.cov <- diag(resid.var)
    #Returns covariance
    if(length(exposures.num) > 0){
      beta.combine = cbind(beta.star, beta.style)
      beta.stms = cbind(B.mod[,1], B.style, B.mod[,-1])
    }else
    {
      beta.combine = beta.star
      beta.stms = B.mod
    }
    
    colnames(beta.combine) = gsub(paste(exposures.char,collapse="|"), "", colnames(beta.combine))
    beta.combine = beta.combine[, factor.names]
    return.cov <-  beta.stms %*% g.cov %*% t(beta.stms) + resid.cov
    #Exposure matrix for the last time period
    beta = beta.combine[((TP-1)*N+1):(TP*N), 1:ncol(beta.combine)]
    
    #Restriction matrix
    restriction.mat = R_matrix
  }
  else if(model.MSCI)
  {
    
    # determine factor model formula to be passed to lm
    fm.formula <- paste(ret.var, "~", paste(exposure.vars, collapse="+"))
    if (length(exposures.char)) {
      fm.formula <- paste(fm.formula, "- 1")
	  
	  formulaL = list()
      for(i in exposures.char)
      {
        data[, i] <- as.factor(data[,i])
#        if (grepl("SECTOR",i)) 
#          formula.ind = as.formula(paste(ret.var, "~", i, "-1"))
#        else 
		formulaL[[i]] = as.formula(paste(ret.var, "~", i, "-1"))
      }
    }
    
    # convert the pasted expression into a formula object
    fm.formula <- as.formula(fm.formula)
    #Extract model beta, expo.char beta and expo.num betas
    beta <- model.matrix(fm.formula, data=data)
    beta1 <- model.matrix(formulaL[[1]], data=data)
    beta2 <- model.matrix(formulaL[[2]], data=data)
    beta.mic <- cbind("Market" = rep(1, nrow(beta1)), beta1, beta2)
    if(length(exposures.num) > 0)
      beta.style<- beta[,exposures.num,drop=FALSE]
    
    fac.names = lapply(seq(exposures.char), function(x)
      paste(levels(data[,exposures.char[x]]),sep=""))


#    if(grepl("SECTOR", exposures.char[1])){
      factor.names <- c("Market",unlist(fac.names),
                        exposures.num)
#    }else{
#      factor.names <- c("Market", unlist((fac.names.indcty)[2]),unlist((fac.names.indcty)[1]),
#                        exposures.num)
#    }
    rownames(beta.mic) <- rep(asset.names, TP)
    asset.names <- unique(data[[asset.var]])
    N <- length(asset.names)
    #Define Retrun matrix 
    returns = matrix(data[[ret.var]],nrow = N)
    K <- length(factor.names)
    K1<- dim(beta1)[2]
    K2<- dim(beta2)[2]
    #Define Restriction matrix 
    rMic<-  rbind( cbind(diag(K1), matrix(0, nrow = K1, ncol = K2-1)), 
                   c(c(0,rep(-1, K1-1)), rep(0, K2-1)),
                   cbind(matrix(0, ncol = K1, nrow = K2-1), diag(K2-1)),
                   c(rep(0, K1), rep(-1, K2-1)))
    
    row.names(returns) = asset.names
    colnames(returns) = as.character(time.periods)
    
    reg.list<- list()
    
    B.mod = (beta.mic[1:N, ]) %*% rMic  #Y = X*R
    if(length(exposures.num) > 0){
    B.style = beta.style[((TP-1)*N+1) : (TP*N),,drop=FALSE]

}
    fmMSCI.formula = as.formula(paste(ret.var, "~", "B.mod+", paste(exposures.num, collapse="+"),"-1" ))                 
    reg.list <- by(data=data, INDICES=data[[date.var]], 
                   FUN=function(x) {lm(data=x, formula=fmMSCI.formula, 
                                       na.action=na.fail)})
    #Find weights for WLS regression
    if (grepl("W",fit.method)) {
		
		
		# check the data to make sure at least 2 assets for each factor combination
		freq = eval(parse(text=paste0('data.table(data)[,list(nAsset=length(unique(get(asset.var)))),by=list(',paste(exposures.char,collapse=","),')]')))		
		
		if(any(freq$nAsset==1)){
			warning('Invalid data: The above factor combinations contain only 1 asset, and weighted fitting may fail. \n Consider removing these assets, change the factor, or choose non-weighted fitting.')
			print(freq[nAsset==1])}
		
		
		
      if (rob.stats) {
        resid.var <- apply(sapply(reg.list, residuals), 1, scaleTau2)^2
      } else {
        resid.var <- apply(sapply(reg.list, residuals), 1, var)
      }
      #Compute cross-sectional weights using EWMA or GARCH
      if((resid.scaleType != "stdDev"))
      { #Extract Residuals
        res = sapply(reg.list, residuals)
        
        if(grepl("EWMA", resid.scaleType)){
          w<-matrix(0,N,TP)
          for(i in 1:N)
          {
            var_tminus1 = as.numeric(resid.var[i])
            for(j in 2:TP)
            {
              #ifelse conditon is used to check if robust EWMA weights has to be calculated.
              #The rejection threshold a=2.5 is used as mentioned in eq 6.6 of Martin (2005)
              w[i,j] = var_tminus1 + ((1-lambda)*(res[i,j]^2-var_tminus1)) * ifelse(resid.scaleType == "robEWMA", ifelse(abs(res[i,j] <= 2.5 * sqrt(var_tminus1)), 1, 0), 1)
              var_tminus1 = w[i,j]
            }
          }
          w[,1] = resid.var
        }
        #GARCH(1,1)
        else if(resid.scaleType == "GARCH") {
          #Compute parameters using MLE
          if(GARCH.MLE){
            garch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), 
                                    mean.model=list(armaOrder=c(0,0), include.mean = FALSE),  
                                    distribution.model="norm")
            garch.weights = sapply(X = 1:nrow(res), FUN = function(X){(ugarchfit(garch.spec,res[X,]))@fit$var})
            w = t(garch.weights)
          }
          
          else {
            # use fixed parameters
            # default values of omega, Alpha and beta are based on Martin and Ding (2017)
            alpha = ifelse(!exists("alpha", where = GARCH.params), 0.1, GARCH.params$alpha)
            beta =  ifelse(!exists("beta", where = GARCH.params), 0.81, GARCH.params$beta )
            w<-matrix(0,N,TP)
            for(i in 1:N)
            {
              #Use sample variance as the initial variance
              w[,1] = resid.var
              var_tminus1 = as.numeric(resid.var[i])
              for(j in 2:TP)
              {
                w[i,j] = resid.var[i] * (1 - alpha - beta) + alpha * res[i,j-1]^2 + beta * var_tminus1
                var_tminus1 = w[i,j]
              }
            }
            
          }
        }
        data<- cbind(data, W = 1/as.numeric(w))
      }
      else
      {
        data <- cbind(data, W=1/resid.var)
      }
      
      reg.list <- by(data=data, INDICES=data[[date.var]], 
                     FUN=function(x) {lm(data=x, formula=fmMSCI.formula, weights = W,
                                         na.action=na.fail)})
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
    #Re-order the columns in the order mkt-style-sector-country
    if(length(exposures.num)>0)
      factor.returns <- factor.returns[,c(1,(K1+2+K2):K, 2:(K1+1), (K1+2):(K1+K2+1))]
    factor.names <- colnames(factor.returns)
    #Fac Covarinace
    factor.cov <- covClassic(coredata(factor.returns), distance=FALSE, 
                             na.action=na.omit)$cov
    #Residual Variance
    resid.var <- apply(coredata(residuals), 2, var, na.rm=T)
    names(resid.var) <- asset.names
    #if resid.scaleType is not stdDev, use the most recent residual var as the diagonal cov-var of residuals
    if((resid.scaleType != "stdDev")){
      row.names(w) = asset.names
      resid.cov <- diag(w[,ncol(w)])
      # update resid.var with the timeseries of estimated resid variances
      resid.var = as.xts(t(w), order.by = as.yearmon(time.periods))
    }
    else
      resid.cov <- diag(resid.var)
    #Returns covariance
    if(length(exposures.num) > 0){
      beta.combine = cbind(beta.mic, beta.style)
    }else 
      beta.combine = beta.mic
    colnames(beta.combine) = gsub(paste(exposures.char,collapse="|"), "", colnames(beta.combine))
    beta.combine = beta.combine[, factor.names]
    return.cov <-  beta.combine[((TP-1)*N+1):(TP*N), 1:K] %*% factor.cov %*% t( beta.combine[((TP-1)*N+1):(TP*N), 1:K]) + resid.cov
    #Exposure matrix 
    beta = beta.combine[((TP-1)*N+1):(TP*N), 1:K]
    #colnames(beta) = gsub("COUNTRY|SECTOR", "", colnames(beta))
    #factor.cov = factor.cov[factor.names, factor.names]
    #Restriction matrix
    restriction.mat = rMic
  }
  
  # Initialization
  EX <- length(exposures.num)
  
  # FLAM
  if (EX == 1) {
    
    # Standardized exposure matrix (lagged)
    stdExposures <- matrix(data[[exposures.num]], nrow = N)
    
    # ISM model
    if (grepl(analysis, "ISM")) {
      
      # Information coefficient in each time period is the correlation between the raw returns 
      # and the cross-sectional standardized returns
      IC <- c()
      for (t in 1:TP) {
        IC[t] <- cor(rawReturns[, t + 1], stdExposures[, t])
      }
      meanIC <- mean(IC)
      sigmaIC <- sd(IC)
      IR_GK <- meanIC * sqrt(N)
      IR_inf <- meanIC / sigmaIC
      IR_N <- meanIC / sqrt((1 - meanIC^2 - sigmaIC^2) / N + sigmaIC ^ 2)
      
      # Compute the conditional mean forecast and the conditional forecast error covariance used in 
      # the optimization (TBD)
      condAlpha <- meanIC * stdExposures[, TP]
      condOmega <- sigmaIC ^ 2 * (stdExposures[, TP] %*% t(stdExposures[, TP])) + diag(resid.var)
      
      # Compute optimal active weights using formula
      sigma_A <- targetedVol
      kappa <- (t(condAlpha) %*% solve(condOmega) %*% rep(1, N)) / (rep(1, N) %*% solve(condOmega) %*% rep(1, N))
      activeWeights <- sigma_A * (solve(condOmega) %*% as.matrix(condAlpha)) / c(sqrt(t(as.matrix(condAlpha)) %*% solve(condOmega) %*% as.matrix(condAlpha)))
      #activeWeights <- activeWeights - mean(activeWeights)
      activeReturns <- t(activeWeights) %*% rawReturns[, TP + 1]
      
      
    } else if (grepl(analysis, "NEW")) {
      
      # Information coefficient is the correlation between the standardized returns 
      # and the standardized exposures
      IC <- c()
      for (t in 1:TP) {
        IC[t] <- cor(stdReturns[, t + 1], stdExposures[, t])
      }
      meanIC <- mean(IC)
      sigmaIC <- sd(IC)
      IR_GK <- meanIC * sqrt(N)
      IR_inf <- meanIC / sigmaIC
      IR_N <- meanIC / sqrt((1 - meanIC^2 - sigmaIC^2) / N + sigmaIC ^ 2)
      
      # Compute the conditional mean forecast and the conditional forecast error covariance 
      # at the end of the period
      condAlpha <- meanIC * as.vector(diag(sigmaGarch[, TP + 1]) %*% stdExposures[, TP])
      condOmega <- diag(sigmaGarch[, TP + 1]) %*% (sigmaIC^2 * (stdExposures[, TP] %*% t(stdExposures[, TP])) + diag((1 - meanIC^2 - sigmaIC^2), nrow = N, ncol = N)) %*% diag(sigmaGarch[, TP + 1])
  
      
      # Compute optimal active weights using formula
      sigma_A <- targetedVol
      kappa <- (t(condAlpha) %*% solve(condOmega) %*% rep(1, N)) / (rep(1, N) %*% solve(condOmega) %*% rep(1, N))
      activeWeights <- sigma_A * (solve(condOmega) %*% as.matrix(condAlpha)) / c(sqrt(t(as.matrix(condAlpha)) %*% solve(condOmega) %*% as.matrix(condAlpha)))
      #activeWeights <- activeWeights - mean(activeWeights)
      activeReturns <- t(activeWeights) %*% stdReturns[, TP + 1]
      IR = c(IR_GK, IR_inf, IR_N)
      
    } else {
      activeReturns <- activeWeights <- IR <- NULL
    }
    
  } else {
    
    # Multi-factor model (To be implemented)
    activeReturns <- activeWeights <- IR <- NULL
    
  }


  # create list of return values.
  result <- list(factor.fit=reg.list, beta=beta, factor.returns=factor.returns, 
                 residuals=residuals, r2=r2, factor.cov=factor.cov, g.cov = g.cov,
                 resid.cov=resid.cov, return.cov=return.cov, restriction.mat=restriction.mat,
                 resid.var=resid.var, call=this.call, time.periods=time.periods,
                 data=data, date.var=date.var, ret.var=ret.var, 
                 asset.var=asset.var, exposure.vars=exposure.vars, 
                 weight.var=weight.var, fit.method=fit.method, 
                 asset.names=asset.names, factor.names=factor.names, 
                 activeWeights = activeWeights, activeReturns = activeReturns,
                 IR = IR)
  
  class(result) <- "ffm"
  return(result)
  }


### function to calculate z-scores for numeric exposure i using weights w
## x is a data.frame object, i is a character string and w has same length as x 
# rob.stats is a logical argument to compute robust location and scale

zScore <- function(x, i, w, rob.stats, z.score, asset.names) {
  if (grepl(z.score, "crossSection")) {
    if (rob.stats) {
      x_bar <- median(w * x[[i]])
      (x[[i]] - x_bar)/mad(x[[i]], center = x_bar)
    } else {
      x_bar <- mean(w * x[[i]]) 
      n <- length(x[[i]])
      # use equal weighted squared deviation about the weighted mean
      (x[[i]] - x_bar)/sqrt(sum((x[[i]] - x_bar) ^ 2)/(n - 1))
    }
  } else {
    N <- length(asset.names)
    exposures <- matrix(w * x[[i]], nrow = N)
    sigmaEWMA <- stdExpo <- exposures
    meanExp <- apply(exposures, 1, mean)
    sigmaExp <- apply(exposures, 1, sd)
    
    for (j in 1:N) {
      ts <- (exposures[j, ] - meanExp[j])^2
      var_past_2 <- sigmaExp[j] ^ 2
      sigmaEWMA[j, ] <- sapply(ts, function(x) var_past_2 <<- 0.10 * x + 0.90 * var_past_2)
      if (any(sigmaEWMA[j, ] == 0)) {
        sigmaEWMA[j, ] <- 1
      }
    }
    as.vector((exposures -  meanExp) / sqrt(sigmaEWMA))
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
