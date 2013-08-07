#' fit fundamental factor model by classic OLS or Robust regression technique
#' 
#' fit fundamental factor model or cross-sectional time series factor model by
#' classic OLS or Robust regression technique.  Fundamental factor models use
#' observable asset specific characteristics (fundamentals) like industry
#' classification, market capitalization, style classification (value, growth)
#' etc. to determine the common risk factors. The function creates the class
#' "FundamentalFactorModel".
#' 
#' @details
#' If style factor exposure is standardized to regression-weighted mean zero, this makes
#' style factors orthogonal to the Word factor (intercept term), which in turn facilitted 
#' interpretation of the style factor returns. See Menchero 2010.    
#' 
#' The original function was designed by Doug Martin and originally implemented
#' in S-PLUS by a number of UW Ph.D. students:Christopher Green, Eric Aldrich,
#' and Yindeng Jiang. Guy Yullen re-implemented the function in R and requires
#' the following additional R libraries: zoo time series library, robust
#' Insightful robust library ported to R and robustbase Basic robust statistics
#' package for R
#'  
#' 
#' @param data data.frame, data must have \emph{assetvar}, \emph{returnvar}, \emph{datevar}
#' , and exposure.names. Generally, data is panel data setup, so it needs firm variabales 
#' and time variables.  
#' @param exposure.names a character vector of exposure names for the factor model
#' @param wls logical flag, TRUE for weighted least squares, FALSE for ordinary
#' least squares
#' @param regression A character string, "robust" for regression via lmRob,
#' "classic" for regression with lm()
#' @param covariance A character string, "robust" for covariance matrix
#' computed with covRob(), "classic" for covariance matrix with covClassic() in 
#' robust package. 
#' @param full.resid.cov logical flag, TRUE for full residual covariance matrix
#' calculation, FALSE for diagonal residual covarinace matrix
#' @param robust.scale logical flag, TRUE for exposure scaling via robust scale
#' and location, FALSE for scaling via mean and sd
#' @param returnsvar A character string giving the name of the return variable
#' in the data.
#' @param datevar A character string gives the name of the date variable in
#' the data.
#' @param assetvar A character string gives the name of the asset variable in
#' the data.
#' @param standardized.factor.exposure logical flag. Factor exposure will be standarized 
#' to regression weighted mean 0 and standardized deviation to 1 if \code(TRUE). 
#' Default is \code(FALSE). See Detail. 
#' @param weight.var. A character strping gives the name of the weight used for standarizing factor exposures. 
#' @return an S3 object containing
#' \itemize{
#' \item returns.cov A "list" object contains covariance information for
#' asset returns, includes covariance, mean and eigenvalus. Beta of taken as latest
#' date input. 
#' \item factor.cov An object of class "cov" or "covRob" which
#' contains the covariance matrix of the factor returns (including intercept).
#' \item resids.cov An object of class "cov" or "covRob" which contains
#' the covariance matrix of the residuals, if "full.resid.cov" is TRUE.  NULL
#' if "full.resid.cov" is FALSE.
#' \item resid.variance A vector of variances estimated from the OLS
#' residuals for each asset. If "wls" is TRUE, these are the weights used in
#' the weighted least squares regressions.  If "cov = robust" these values are
#' computed with "scale.tau".  Otherwise they are computed with "var".
#' \item factor.returns A "xts" object containing the times series of
#' estimated factor returns and intercepts.
#' \item residuals A "xts" object containing the time series of residuals
#' for each asset.
#' \item tstats A "xts" object containing the time series of t-statistics
#' for each exposure.
#' \item call function call
#' \item exposure.names A character string giving the name of the exposure variable in
#' the data.
#' }
#' @author Guy Yullen and Yi-An Chen
#' @references
#' \itemize{
#' \item "The Characteristics of Factor Portfolios", Fall 2010, MENCHERO Jose, 
#' Journal of Performance Measurement. 
#' }
#' 
#' @export
#' @examples
#' 
#' # BARRA type factor model
#' data(Stock.df)
#' # there are 447 assets  
#' exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
#' test.fit <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
#'                                        datevar = "DATE", returnsvar = "RETURN",
#'                                        assetvar = "TICKER", wls = TRUE, 
#'                                        regression = "classic", 
#'                                        covariance = "classic", full.resid.cov = TRUE, 
#'                                        robust.scale = TRUE)
#' 
#' names(test.fit) 
#' test.fit$returns.cov 
#' test.fit$resids.cov  
#' names(test.fit$cov.factor)  
#' test.fit$factor.cov$cov  
#' test.fit$factor  
#' test.fit$resid.variance  
#' test.fit$resids 
#' test.fit$tstats 
#' test.fit$call
#' 
#' # BARRA type Industry Factor Model
#' exposure.names <- c("GICS.SECTOR")  
#' # the rest keep the same
#' test.fit2 <- fitFundamentalFactorModel(data=data,exposure.names=exposure.names,
#'                                        datevar = "DATE", returnsvar = "RETURN",
#'                                        assetvar = "TICKER", wls = TRUE, 
#'                                        regression = "classic", 
#'                                        covariance = "classic", full.resid.cov = TRUE, 
#'                                        robust.scale = TRUE)
#' 
#' names(test.fit2) 
#' test.fit2$cov.returns 
#' test.fit2$cov.resids  
#' names(test.fit2$cov.factor)  
#' test.fit2$cov.factor$cov  
#' test.fit2$factor  
#' test.fit2$resid.variance  
#' test.fit2$resids 
#' test.fit2$tstats 
#' test.fit2$call
#' 
#' 
#' 
#' 



fitFundamentalFactorModel <-
  function(data,exposure.names, datevar, returnsvar, assetvar,
           wls = TRUE, regression = "classic", 
           covariance = "classic", full.resid.cov = FALSE, robust.scale = FALSE,
           standardized.factor.exposure = FALSE, weight.var) {
    
    require(xts)
    require(robust)
    
    
    assets = unique(data[,assetvar])
    timedates = as.Date(unique(data[,datevar]))    
    
    
    if (length(timedates) < 2) 
      stop("At least two time points, t and t-1, are needed for fitting the factor model.")
    if (!is(exposure.names, "vector") || !is.character(exposure.names)) 
      stop("exposure argument invalid---must be character vector.")
    if (!is(assets, "vector") || !is.character(assets)) 
      stop("assets argument invalid---must be character vector.")
    
    wls <- as.logical(wls)
    full.resid.cov <- as.logical(full.resid.cov)
    robust.scale <- as.logical(robust.scale)
    standardized.factor.exposure <- as.logical(standardized.factor.exposure)
    
    if (!match(regression, c("robust", "classic"), FALSE)) 
      stop("regression must one of 'robust', 'classic'.")
    if (!match(covariance, c("robust", "classic"), FALSE)) 
      stop("covariance must one of 'robust', 'classic'.")
    this.call <- match.call()
    
    if (match(returnsvar, exposure.names, FALSE)) 
      stop(paste(returnsvar, "cannot be used as an exposure."))
    
    
    numTimePoints <- length(timedates)
    numExposures <- length(exposure.names)
    numAssets <- length(assets)
    
    
    
    
    # check if exposure.names are numeric, if not, create exposures. factors by dummy variables
    which.numeric <- sapply(data[, exposure.names, drop = FALSE],is.numeric)
    exposures.numeric <- exposure.names[which.numeric]
    # industry factor model
    exposures.factor <- exposure.names[!which.numeric]
    if (length(exposures.factor) > 1) {
      stop("Only one nonnumeric variable can be used at this time.")
    }
    
    if (standardized.factor.exposure == TRUE) {
      weight = by(data = data, INDICES = as.numeric(data[[datevar]]), 
                  function(x) x[[weight.var]]/sum(x[[weight.var]]))
      data[[weight.var]] <- unlist(weight)
        
      for (i in exposures.numeric) {
    standardized.exposure <- by(data = data, INDICES = as.numeric(data[[datevar]]),
                                function(x) ((x[[i]] - mean(x[[weight.var]]*x[[i]]) )*1/sd(x[[weight.var]]*x[[i]]) )) 
    data[[i]] <- unlist(standardized.exposure)
    }
    }
    
    
    
    
    regression.formula <- paste("~", paste(exposure.names, collapse = "+"))
    #  "~ BOOK2MARKET"
    if (length(exposures.factor)) {
      regression.formula <- paste(regression.formula, "- 1")
      data[, exposures.factor] <- as.factor(data[,exposures.factor])
      exposuresToRecode <- names(data[, exposure.names, drop = FALSE])[!which.numeric]
      contrasts.list <- lapply(seq(length(exposuresToRecode)), 
                               function(i) function(n, m) contr.treatment(n, contrasts = FALSE))
      names(contrasts.list) <- exposuresToRecode
    }    else {
      contrasts.list <- NULL
    }
    # turn characters into formula 
    regression.formula <- eval(parse(text = paste(returnsvar,regression.formula)))
    # RETURN ~ BOOK2MARKET 
    
    ols.robust <- function(xdf, modelterms, conlist) {
      if (length(exposures.factor)) {
        zz <- xdf[[exposures.factor]]
        xdf[[exposures.factor]] <- if (is.ordered(zz)) 
          ordered(zz, levels = sort(unique.default(zz)))
        else factor(zz)
      }
      model <- lmRob(modelterms, data = xdf, contrasts = conlist, 
                     control = lmRob.control(mxr = 200, mxf = 200, mxs = 200))
      sdest <- sqrt(diag(model$cov))
      names(sdest) <- names(model$coef)
      coefnames <- names(model$coef)
      alphaord <- order(coefnames)
      model$coef <- model$coef[alphaord]
      sdest <- sdest[alphaord]
      c(length(model$coef), model$coef, model$coef/sdest, model$resid)
    }
    ols.classic <- function(xdf, modelterms, conlist) {
      model <- try(lm(formula = modelterms, data = xdf, contrasts = conlist, 
                      singular.ok = FALSE))
      if (is(model, "Error")) {
        mess <- geterrmessage()
        nn <- regexpr("computed fit is singular", mess)
        if (nn > 0) {
          cat("At time:", substring(mess, nn), "\n")
          model <- lm(formula = modelterms, data = xdf, 
                      contrasts = conlist, singular.ok = TRUE)
        }  else stop(mess)
      }
      tstat <- rep(NA, length(model$coef))
      tstat[!is.na(model$coef)] <- summary(model, cor = FALSE)$coef[,3]
      alphaord <- order(names(model$coef))
      c(length(model$coef), model$coef[alphaord], tstat[alphaord], 
        model$resid)
    }
    wls.robust <- function(xdf, modelterms, conlist, w) {
      assign("w", w, pos = 1)
      if (length(exposures.factor)) {
        zz <- xdf[[exposures.factor]]
        xdf[[exposures.factor]] <- if (is.ordered(zz)) 
          ordered(zz, levels = sort(unique.default(zz)))
        else factor(zz)
      }
      model <- lmRob(modelterms, data = xdf, weights = w, contrasts = conlist, 
                     control = lmRob.control(mxr = 200, mxf = 200, mxs = 200))
      sdest <- sqrt(diag(model$cov))
      names(sdest) <- names(model$coef)
      coefnames <- names(model$coef)
      alphaord <- order(coefnames)
      model$coef <- model$coef[alphaord]
      sdest <- sdest[alphaord]
      c(length(model$coef), model$coef, model$coef/sdest, model$resid)
    }
    wls.classic <- function(xdf, modelterms, conlist, w) {
      assign("w", w, pos = 1)
      model <- try(lm(formula = modelterms, data = xdf, contrasts = conlist, 
                      weights = w, singular.ok = FALSE))
      if (is(model, "Error")) {
        mess <- geterrmessage()
        nn <- regexpr("computed fit is singular", mess)
        if (nn > 0) {
          cat("At time:", substring(mess, nn), "\n")
          model <- lm(formula = modelterms, data = xdf, 
                      contrasts = conlist, weights = w)
        }
        else stop(mess)
      }
      tstat <- rep(NA, length(model$coef))
      tstat[!is.na(model$coef)] <- summary(model, cor = FALSE)$coef[, 
                                                                    3]
      alphaord <- order(names(model$coef))
      c(length(model$coef), model$coef[alphaord], tstat[alphaord], 
        model$resid)
    }
    # FE.hat has T  elements
    # every element t contains 
    # 1. number of factors (intercept incl.) 
    # 2. estimated factors at time t 
    # 3. t value of estimated factors 
    # 4. residuals  at time t       
    if (!wls) {
      if (regression == "robust") {
        # ols.robust    
        FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                     FUN = ols.robust, modelterms = regression.formula, 
                     conlist = contrasts.list)
      } else {
        # ols.classic
        FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                     FUN = ols.classic, modelterms = regression.formula, 
                     conlist = contrasts.list)
      }
    } else {
      if (regression == "robust") {
        # wls.robust
        resids <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                     FUN = function(xdf, modelterms, conlist) {
                       lmRob(modelterms, data = xdf, contrasts = conlist, 
                             control = lmRob.control(mxr = 200, mxf = 200, 
                                                     mxs = 200))$resid
                     }, modelterms = regression.formula, conlist = contrasts.list)
        resids <- apply(resids, 1, unlist)
        weights <- if (covariance == "robust") 
          apply(resids, 1, scaleTau2)^2
        else apply(resids, 1, var)
        FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                     FUN = wls.robust, modelterms = regression.formula, 
                     conlist = contrasts.list, w = weights)
      } else { 
        # wls.classic
        resids <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                     FUN = function(xdf, modelterms, conlist) {
                       lm(formula = modelterms, data = xdf, contrasts = conlist, 
                          singular.ok = TRUE)$resid
                     },
                     modelterms = regression.formula, conlist = contrasts.list)
        resids <- apply(resids, 1, unlist)
        weights <- if (covariance == "robust") 
          apply(resids, 1, scaleTau2)^2
        else apply(resids, 1, var)
        FE.hat <- by(data = data, INDICES = as.numeric(data[[datevar]]), 
                     FUN = wls.classic, modelterms = regression.formula, 
                     conlist = contrasts.list, w = weights)
      }
    }
    # if there is industry dummy variables
    if (length(exposures.factor)) {
      numCoefs <- length(exposures.numeric) + length(levels(data[, 
                                                                 exposures.factor]))
      ncols <- 1 + 2 * numCoefs + numAssets
      fnames <- c(exposures.numeric, paste(exposures.factor, 
                                           levels(data[, exposures.factor]), sep = ""))
      cnames <- c("numCoefs", fnames, paste("t", fnames, sep = "."), 
                  assets)
    } else {
      numCoefs <- 1 + length(exposures.numeric)
      ncols <- 1 + 2 * numCoefs + numAssets
      cnames <- c("numCoefs", "(Intercept)", exposures.numeric, 
                  paste("t", c("(Intercept)", exposures.numeric), sep = "."), 
                  assets)
    }
    
    # create matrix for fit
    FE.hat.mat <- matrix(NA, ncol = ncols, nrow = numTimePoints, 
                         dimnames = list(as.character(as.Date(as.numeric(names(FE.hat)), origin = "1970-01-01")), 
                                         cnames))
    # give each element t names 
    for (i in 1:length(FE.hat)) {
      names(FE.hat[[i]])[1] <- "numCoefs"
      nc <- FE.hat[[i]][1]
      names(FE.hat[[i]])[(2 + nc):(1 + 2 * nc)] <- paste("t", 
                                                         names(FE.hat[[i]])[2:(1 + nc)], sep = ".")
      if (length(FE.hat[[i]]) != (1 + 2 * nc + numAssets)) 
        stop(paste("bad count in row", i, "of FE.hat"))
      names(FE.hat[[i]])[(2 + 2 * nc):(1 + 2 * nc + numAssets)] <- assets
      idx <- match(names(FE.hat[[i]]), colnames(FE.hat.mat))
      FE.hat.mat[i, idx] <- FE.hat[[i]]
    }
    # give back the names of timedates
    timedates <- as.Date(as.numeric(dimnames(FE.hat)[[1]]), origin = "1970-01-01")
    coefs.names <- colnames(FE.hat.mat)[2:(1 + numCoefs)]
    # estimated factors returns ordered by time
    f.hat <- xts(x = FE.hat.mat[, 2:(1 + numCoefs)], order.by = timedates)
    # check for outlier
    gomat <- apply(coredata(f.hat), 2, function(x) abs(x - median(x, 
                                                                  na.rm = TRUE)) > 4 * mad(x, na.rm = TRUE))
    if (any(gomat, na.rm = TRUE) ) {
      cat("\n\n*** Possible outliers found in the factor returns:\n\n")
      for (i in which(apply(gomat, 1, any, na.rm = TRUE))) print(f.hat[i, 
                                                                       gomat[i, ], drop = FALSE])
    }
    tstats <- xts(x = FE.hat.mat[, (2 + nc):(1 + 2 * nc)], order.by = timedates)
    # residuals for every asset ordered by time
    resids <- xts(x = FE.hat.mat[, (2 + 2 * numCoefs):(1 + 2 * 
                                                         numCoefs + numAssets)], order.by = timedates)
    
    if (covariance == "robust") {
      if (kappa(na.exclude(coredata(f.hat))) < 1e+10) {
        Cov.factors <- covRob(coredata(f.hat), estim = "pairwiseGK", 
                              distance = FALSE, na.action = na.omit)
      } else {
        cat("Covariance matrix of factor returns is singular.\n")
        Cov.factors <- covRob(coredata(f.hat), distance = FALSE, 
                              na.action = na.omit)
      }
      resid.vars <- apply(coredata(resids), 2, scaleTau2, na.rm = T)^2
      D.hat <- if (full.resid.cov) 
        covOGK(coredata(resids), sigmamu = scaleTau2, n.iter = 1)
      else 
        diag(resid.vars)
    }   else {
      Cov.factors <- covClassic(coredata(f.hat), distance = FALSE,na.action = na.omit)
      resid.vars <- apply(coredata(resids), 2, var, na.rm = TRUE)
      D.hat <- if (full.resid.cov) {
        covClassic(coredata(resids), distance = FALSE, na.action = na.omit)
      }  else { diag(resid.vars) }
    }
    # create betas from origial database  
    B.final <- matrix(0, nrow = numAssets, ncol = numCoefs)
    colnames <-  coefs.names
    B.final[, match("(Intercept)", colnames, 0)] <- 1
    numeric.columns <- match(exposures.numeric, colnames, 0)
    # only take the latest beta to compute FM covariance
    # should we let user choose which beta to use ?
    B.final[, numeric.columns] <- as.matrix(data[ (as.numeric(data[[datevar]]) == 
                                                     timedates[numTimePoints]), exposures.numeric])
    rownames(B.final) = assets
    colnames(B.final) = colnames(f.hat)
    if (length(exposures.factor)) {
      B.final[, grep(exposures.factor, x = colnames)][cbind(seq(numAssets), 
                                                            as.numeric(data[data[[datevar]] == timedates[numTimePoints], 
                                                                            exposures.factor]))] <- 1
    }
    cov.returns <- B.final %*% Cov.factors$cov %*% t(B.final) + 
      if (full.resid.cov) { D.hat$cov
      }  else { D.hat  }
    mean.cov.returns = tapply(data[[returnsvar]],data[[assetvar]], mean)
    Cov.returns <- list(cov = cov.returns, mean=mean.cov.returns, eigenvalues = eigen(cov.returns, 
                                                                                      only.values = TRUE, symmetric = TRUE)$values)
    
    # report residual covaraince if full.resid.cov is true. 
    if (full.resid.cov) {
      Cov.resids <- D.hat
    }
    else {
      Cov.resids <- NULL
    }
    # 
    # # r-square for each asset = 1 - SSE/SST
    #    SSE <-  apply(fit.fund$residuals^2,2,sum) 
    #    SST <- tapply(data[,returnsvar],data[,assetvar],function(x) sum((x-mean(x))^2))
    #   r2 <- 1- SSE/SST                                  
    
    # change names for intercept
    colnames(f.hat)[1] <- "Intercept"
    
    output <- list(returns.cov = Cov.returns, 
                   factor.cov = Cov.factors, 
                   resids.cov = Cov.resids, 
                   resid.variance = resid.vars, 
                   factor.returns = f.hat, 
                   residuals = resids, 
                   tstats = tstats,                   
                   call = this.call,
                   data = data,
                   asset.names = assets,
                   beta = B.final,
                   datevar = datevar,
                   returnsvar = returnsvar,
                   assetvar = assetvar,
                   exposure.names = exposure.names)
    class(output) <- "FundamentalFactorModel"
    return(output)
  }