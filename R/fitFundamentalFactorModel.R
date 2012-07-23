fitFundamentalFactorModel <-
function (fulldata, timedates, exposures, assets, wls = FALSE, regression = "classic", 
          covariance = "classic", full.resid.cov = TRUE, robust.scale = FALSE, 
          datevar = "DATE", assetvar = "PERMNO", returnsvar = "RETURN", 
          tickersvar = "TICKER.x") {
  
require(zoo)
require(robust)
## input
##  
## fulldata               : data.frame. data stacked by dates
## timedates              : a vector of Dates specifying the date range for the model
##                          fitting
## exposures              : a character vector of exposure names for the factor model
## assets                 : a list of PERMNOs to be used for the factor model
## Optional parameters:
## wls                    : logical flag, TRUE for weighted least squares, FALSE for
##                          ordinary least squares
## regression             : character string, "robust" for regression via lmRob, "classic"
##                          for regression via lm
## covariance             : character string, "robust" for covariance matrix computed via
##                          covRob, "classic" for covariance matrix via ccov
## full.resid.cov         : logical flag, TRUE for full residual covariance matrix
##                          calculation, FALSE for diagonal residual covarinace matrix
## robust.scale           : logical flag, TRUE for exposure scaling via robust scale and
##                          location, FALSE for scaling via mean and sd
## datevar                : character string giving the name of the date variable in the data.
## assetvar               : character string giving the name of the asset variable in the data.
## returnsvar             : character string giving the name of the return variable in the data.               
## tickersvar             : character string giving the name of the ticker variable in the data.
  
## output
## 
## cov.returns            : covariance information for asset returns, includes
##                          covariance, mean, eigenvalus
## cov.factor.rets        : covariance information for factor returns, includes
##                          covariance and mean
## cov.resids             : covariance information for model residuals, includes
##                          covariance and mean
## resid.vars             : list of information regarding model residuals variance
## factor.rets            : zoo time series object of factor returns
## resids                 : zoo time series object of model residuals
## tstats                 : zoo time series object of model t-statistics
## returns.data            : data.frame of return data including RETURN, DATE,PERMNO
## exposure.data          : data.frame of exposure data including DATE, PERMNO
## assets                 : character vector of PERMNOs used in the model
## tickers                : character vector of tickers used in the model
## call                   : function call
  
   # if (dim(dataArray)[1] < 2) 
   #      stop("At least two time points, t and t-1, are needed for fitting the factor model.")
    if (length(timedates) < 2) 
        stop("At least two time points, t and t-1, are needed for fitting the factor model.")
    if (!is(exposures, "vector") || !is.character(exposures)) 
        stop("exposure argument invalid---must be character vector.")
    if (!is(assets, "vector") || !is.character(assets)) 
        stop("assets argument invalid---must be character vector.")
   
    wls <- as.logical(wls)
    full.resid.cov <- as.logical(full.resid.cov)
    robust.scale <- as.logical(robust.scale)
      
    if (!match(regression, c("robust", "classic"), FALSE)) 
        stop("regression must one of 'robust', 'classic'.")
    if (!match(covariance, c("robust", "classic"), FALSE)) 
        stop("covariance must one of 'robust', 'classic'.")
    this.call <- match.call()
    
    if (match(returnsvar, exposures, FALSE)) 
        stop(paste(returnsvar, "cannot be used as an exposure."))
    
    
    numTimePoints <- length(timedates)
    numExposures <- length(exposures)
    numAssets <- length(assets)
    tickers   <- fulldata[1:numAssets,tickersvar]
    # dim(fulldata)
    # [1] 42912   117  
    # dimnames(fulldata)
    # PERMNO"      "DATE"        "RETURN"      "TICKER.x"    "BOOK2MARKET" "TICKER.y"
        # check if exposures are numeric, if not, create exposures. factors by dummy variables
    which.numeric <- sapply(fulldata[, exposures, drop = FALSE],is.numeric)
    exposures.numeric <- exposures[which.numeric]
    # industry factor model
    exposures.factor <- exposures[!which.numeric]
    if (length(exposures.factor) > 1) {
       stop("Only one nonnumeric variable can be used at this time.")
    }
       
    regression.formula <- paste("~", paste(exposures, collapse = "+"))
    #  "~ BOOK2MARKET"
    if (length(exposures.factor)) {
        regression.formula <- paste(regression.formula, "- 1")
        fulldata[, exposures.factor] <- as.factor(fulldata[, 
            exposures.factor])
        exposuresToRecode <- names(fulldata[, exposures, drop = FALSE])[!which.numeric]
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
            FE.hat <- by(data = fulldata, INDICES = as.numeric(fulldata[[datevar]]), 
                FUN = ols.robust, modelterms = regression.formula, 
                conlist = contrasts.list)
        } else {
            # ols.classic
            FE.hat <- by(data = fulldata, INDICES = as.numeric(fulldata[[datevar]]), 
                FUN = ols.classic, modelterms = regression.formula, 
                conlist = contrasts.list)
        }
    } else {
        if (regression == "robust") {
            # wls.robust
            E.hat <- by(data = fulldata, INDICES = as.numeric(fulldata[[datevar]]), 
                        FUN = function(xdf, modelterms, conlist) {
                              lmRob(modelterms, data = xdf, contrasts = conlist, 
                              control = lmRob.control(mxr = 200, mxf = 200, 
                              mxs = 200))$resid
                              }, modelterms = regression.formula, conlist = contrasts.list)
            E.hat <- apply(E.hat, 1, unlist)
            weights <- if (covariance == "robust") 
                            apply(E.hat, 1, scaleTau2)^2
                       else apply(E.hat, 1, var)
            FE.hat <- by(data = fulldata, INDICES = as.numeric(fulldata[[datevar]]), 
                         FUN = wls.robust, modelterms = regression.formula, 
                               conlist = contrasts.list, w = weights)
        } else { 
            # wls.classic
            E.hat <- by(data = fulldata, INDICES = as.numeric(fulldata[[datevar]]), 
                        FUN = function(xdf, modelterms, conlist) {
                        lm(formula = modelterms, data = xdf, contrasts = conlist, 
                           singular.ok = TRUE)$resid
                           },
                         modelterms = regression.formula, conlist = contrasts.list)
            E.hat <- apply(E.hat, 1, unlist)
            weights <- if (covariance == "robust") 
                           apply(E.hat, 1, scaleTau2)^2
                      else apply(E.hat, 1, var)
            FE.hat <- by(data = fulldata, INDICES = as.numeric(fulldata[[datevar]]), 
                FUN = wls.classic, modelterms = regression.formula, 
                conlist = contrasts.list, w = weights)
        }
    }
    # if there is industry dummy variables
    if (length(exposures.factor)) {
        numCoefs <- length(exposures.numeric) + length(levels(fulldata[, 
            exposures.factor]))
        ncols <- 1 + 2 * numCoefs + numAssets
        fnames <- c(exposures.numeric, paste(exposures.factor, 
            levels(fulldata[, exposures.factor]), sep = ""))
        cnames <- c("numCoefs", fnames, paste("t", fnames, sep = "."), 
            assets)
    } else {
        numCoefs <- 1 + length(exposures.numeric)
        ncols <- 1 + 2 * numCoefs + numAssets
        cnames <- c("numCoefs", "(Intercept)", exposures.numeric, 
                    paste("t", c("(Intercept)", exposures.numeric), sep = "."), 
                    assets)
    }
    FE.hat.mat <- matrix(NA, ncol = ncols, nrow = numTimePoints, 
                         dimnames = list(as.character(as.Date(as.numeric(names(FE.hat)), origin = "1970-01-01")), 
                         cnames))
    # give each element t names and PERMNO
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
    # estimated factors ordered by time
    f.hat <- zoo(x = FE.hat.mat[, 2:(1 + numCoefs)], order.by = timedates)
    # check for outlier
    gomat <- apply(coredata(f.hat), 2, function(x) abs(x - median(x, 
                                      na.rm = TRUE)) > 4 * mad(x, na.rm = TRUE))
    if (any(gomat, na.rm = TRUE) ) {
        cat("\n\n*** Possible outliers found in the factor returns:\n\n")
        for (i in which(apply(gomat, 1, any, na.rm = TRUE))) print(f.hat[i, 
            gomat[i, ], drop = FALSE])
    }
    tstats <- zoo(x = FE.hat.mat[, (2 + nc):(1 + 2 * nc)], order.by = timedates)
    # residuals for every asset ordered by time
    E.hat <- zoo(x = FE.hat.mat[, (2 + 2 * numCoefs):(1 + 2 * 
        numCoefs + numAssets)], order.by = timedates)
    colnames(E.hat) <- tickers
    if (covariance == "robust") {
        if (kappa(na.exclude(coredata(f.hat))) < 1e+10) {
            Cov.facrets <- covRob(coredata(f.hat), estim = "pairwiseGK", 
                            distance = FALSE, na.action = na.omit)
        } else {
            cat("Covariance matrix of factor returns is singular.\n")
            Cov.facrets <- covRob(coredata(f.hat), distance = FALSE, 
                                  na.action = na.omit)
        }
        resid.vars <- apply(coredata(E.hat), 2, scaleTau2, na.rm = T)^2
        D.hat <- if (full.resid.cov) 
            covOGK(coredata(E.hat), sigmamu = scaleTau2, n.iter = 1)
        else 
          diag(resid.vars)
    }   else {
        Cov.facrets <- ccov(coredata(f.hat), distance = FALSE,na.action = na.omit)
        resid.vars <- apply(coredata(E.hat), 2, var, na.rm = TRUE)
        D.hat <- if (full.resid.cov) 
            ccov(coredata(E.hat), distance = FALSE, na.action = na.omit)
        else 
          diag(resid.vars)
    }
    # create betas from origial database  
    B.final <- matrix(0, nrow = numAssets, ncol = numCoefs)
    colnames <-  coefs.names
    B.final[, match("(Intercept)", colnames, 0)] <- 1
    numeric.columns <- match(exposures.numeric, colnames, 0)
    B.final[, numeric.columns] <- as.matrix(fulldata[as.numeric(fulldata[[datevar]]) == 
        timedates[numTimePoints], exposures.numeric])
    if (length(exposures.factor)) 
        B.final[, grep(exposures.factor, x = colnames)][cbind(seq(numAssets), 
            as.numeric(fulldata[fulldata[[datevar]] == timedates[numTimePoints], 
                exposures.factor]))] <- 1
    cov.returns <- B.final %*% Cov.facrets$cov %*% t(B.final) + 
        if (full.resid.cov) 
            D.hat$cov
        else D.hat
    dimnames(cov.returns) <- list(tickers, tickers)
    mean.cov.returns = tapply(fulldata[[returnsvar]],fulldata[[assetvar]], mean)
    dimnames(mean.cov.returns) = list(tickers)
    Cov.returns <- list(cov = cov.returns, mean=mean.cov.returns, eigenvalues = eigen(cov.returns, 
        only.values = TRUE, symmetric = TRUE)$values)
    if (full.resid.cov) {
        Cov.resids <- D.hat
        dimnames(Cov.resids$cov) <- list(tickers, tickers)
        }
    else {
      Cov.resids <- NULL
    }
    output <- list(cov.returns = Cov.returns, 
                   cov.factor.rets = Cov.facrets, 
                   cov.resids = Cov.resids, 
                   resid.vars = resid.vars, 
                   factor.rets = f.hat, 
                   resids = E.hat, 
                   tstats = tstats, 
                   returns.data = fulldata[,c(datevar, assetvar, returnsvar)], 
                                           exposure.data = fulldata[,c(datevar, assetvar, exposures)], 
                   assets = assets, 
                   tickers = tickers, 
                   call = this.call)
    class(output) <- "FundamentalFactorModel"
    return(output)
}

