
#' @title Specifies the elements of a fundamental factor model
#'
#' @description Factor models have a few parameters that describe how the fitting is
#' done.  This function summarizes them and returns a spec object for
#' cross-sectional regressions.  It also preps the data. An object of class \code{"ffmSpec"}
#' is returned.
#' #' @param data data.frame of the balanced panel data containing the variables
#' \code{asset.var}, \code{ret.var}, \code{exposure.vars}, \code{date.var} and
#' optionally, \code{weight.var}.
#' @param asset.var character; name of the variable  for asset names.
#' @param ret.var character; name of the variable for asset returns.
#' @param date.var character; name of the variable containing the dates
#' coercible to class \code{Date}.
#' @param exposure.vars vector; names of the variables containing the
#' fundamental factor exposures.
#' @param weight.var character; name of the variable containing the weights
#' used when standarizing style factor exposures. Default is \code{NULL}. See
#' Details.
#' @param addIntercept logical; If \code{TRUE}, intercept is added in
#'  the exposure matrix. Default is \code{FALSE},
#' @param rob.stats logical; If \code{TRUE}, robust estimates of covariance,
#' correlation, location and univariate scale are computed as appropriate (see
#' Details). Default is \code{FALSE}.
#' @import data.table
#' @export
#'
specFfm <- function(data, asset.var, ret.var, date.var, exposure.vars, weight.var = NULL,
                    addIntercept = FALSE, rob.stats = FALSE){
  
  # set defaults and check input validity
  if (missing(data) || !is.data.frame(data)) {
    stop("Invalid args: data must be a data.frame")
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
    stop("Invalid args: ret.var cannot also be an exposure")
  }
  
  if (!is.null(weight.var) && !is.character(weight.var)) {
    stop("Invalid args: weight.var must be a character string")
  }
  if (!is.logical(rob.stats) || length(rob.stats) != 1) {
    stop("Invalid args: control parameter 'rob.stats' must be logical")
  }
  obj <- list()
  class(obj) <- "ffmSpec"
  # prep the data
  obj$dataDT <- (as.data.table(data))[, c(date.var,asset.var,ret.var,exposure.vars), with = FALSE]
  obj$dataDT[ , eval(date.var) := as.Date(get(date.var))]
  # mido important change of order
  setkeyv(obj$dataDT,c(asset.var, date.var))
  
  obj$dataDT[, idx := 1:.N, by = eval(asset.var)] # this is needed for path dependent calculations
  
  # specify the variables
  obj$asset.var <- asset.var
  obj$ret.var <- ret.var
  obj$dataDT[, RawReturn := get(ret.var)] # this is the raw return
  obj$yVar <- "RawReturn" # this will serve as the name of the regressand column
  obj$standardizedReturns <- FALSE
  obj$residualizedReturns <- FALSE
  
  obj$date.var <- date.var
  obj$exposure.vars <- exposure.vars
  obj$weight.var <- weight.var
  # treat the exposures
  obj$which.numeric <- sapply(obj$dataDT[,exposure.vars, with = F], is.numeric)
  obj$exposures.num <- exposure.vars[  obj$which.numeric]
  obj$exposures.char <- exposure.vars[!  obj$which.numeric]
  # specify the type of model
  if (length(  obj$exposures.char) > 1)
  { #Model has both Sector and Country along wit Intercept
    # however it is better to  check a different condition
    obj$model.MSCI = TRUE
  } else {
    obj$model.MSCI = FALSE
  }
  if (length(  obj$exposures.char) == 0)
  {
    obj$model.styleOnly = TRUE
  } else {
    obj$model.styleOnly = FALSE
  }
  obj$rob.stats <- rob.stats
  obj$addIntercept <- addIntercept
  obj$lagged <- FALSE
  
  return(obj)
}


#' @title lagExposures allows the user to lag exposures by one time period
#'
#' @description Function lag the style exposures in the exposure matrix
#'  by one time period.
#' @param specObj an ffm specification object of of class \code{"ffmSpec"}
#' @return specObj an ffm spec Object that has been lagged
#' @import data.table
#' @export
#'
lagExposures <- function(specObj){
  a_ <- eval(specObj$asset.var) # name of the asset column or id
  specObj$dataDT <- copy(specObj$dataDT) # hard_copy
  # need to protect against only categorical variables #mido
  # for (e_ in specObj$exposures.num){
  for (e_ in specObj$exposure.vars){
    specObj$dataDT[, eval(e_) := shift(get(e_), fill = NA, type = "lag") , by = a_]
  }
  specObj$lagged <- TRUE
  
  specObj$dataDT <- specObj$dataDT[!is.na(get(e_))]
  setkeyv(specObj$dataDT,c(a_, specObj$date.var))
  
  specObj$dataDT[, idx := 1:.N, by = eval(specObj$asset.var)] # this is needed for path dependent calculations
  
  return(specObj)
}



#' @title standardizeExposures
#'
#' @description
#' function to calculate z-scores for numeric exposure using weights weight.var
#' @param specObj is a ffmSpec object,
#' @param Std.Type method for exposure standardization; one of "none",
#' "CrossSection", or "TimeSeries".
#' Default is \code{"none"}.
#' @param lambda lambda value to be used for the EWMA estimation of residual variances.
#' Default is 0.9
#' @return the ffM spec object with exposures z-scored
#' @import data.table
#' @export
#'
standardizeExposures <- function(specObj, Std.Type = c("None", "CrossSection", "TimeSeries"),
                                 lambda = 0.9){
  
  weight.var <- specObj$weight.var
  dataDT <- copy(specObj$dataDT) # hard_copy
  # we did have a copy but do we really need a full  copy, reference should be oka here
  if (class(specObj) != "ffmSpec") {
    stop("specObj must be class ffmSpec")
  }
  Std.Type = toupper(Std.Type[1])
  Std.Type <- match.arg(arg = Std.Type, choices = toupper(c("NONE", "CROSSSECTION", "TIMESERIES")), several.ok = F )
  
  d_ <- eval(specObj$date.var) # name of the date var
  # Convert numeric exposures to z-scores
  if (!grepl(Std.Type, "NONE")) {
    if (!is.null(weight.var)) {
      dataDT[, w := get(weight.var)] # adding the weight variable to the data table
      # Weight exposures within each period using weight.var
      dataDT[ , w := w/sum(w, na.rm = TRUE), by = d_]
      
    } else {
      dataDT[, w := 1] # adding the weight variable to the data table
      
    }
    
    # Calculate z-scores looping through all numeric exposures
    if (grepl(Std.Type, "CROSSSECTION")) {
      for (e_ in specObj$exposures.num) {
        if (specObj$rob.stats) {
          dataDT[, eval(e_) := (w * get(e_) - median(w * get(e_), na.rm = TRUE))/
                   mad(w * get(e_), center = median(w * get(e_), na.rm = TRUE)), by = d_]
          
        } else {
          dataDT[, eval(e_) := (w * get(e_) - mean(w * get(e_), na.rm = TRUE))/
                   sqrt(sum((w * get(e_) - mean(w * get(e_), na.rm = T))^2, na.rm = T)/(.N - 1) ),
                 by = d_]
          # sd(get(e_) , na.rm = T)
          
        }
      }
    } else {
      # for each exposure...quartion : do we need to weight it here?
      #startIdx <- ifelse(specObj$lagged,2,1)
      for (e_ in specObj$exposures.num) {
        # for each asset compute the difference between its exposure at time t - 1 and
        #the Xsection mean of exposures and square it
        dataDT[, ts := (get(e_) - mean(get(e_), na.rm = TRUE))^2, by = d_]
        for ( i in 1:NROW(dataDT))
          set(dataDT,i, "s", ifelse(dataDT$idx[i] <= 1,
                                    dataDT$ts[i],
                                    (1 - lambda) * dataDT$ts[i] + lambda * dataDT$s[i - 1] ))
        # ???? # need timeSeries mean?
        dataDT[, eval(e_) := (get(e_) - mean(get(e_), na.rm = TRUE))/sqrt(s), by = d_]
      }
      dataDT[, ts := NULL]
      dataDT[, s := NULL]
      
    }
  }
  
  specObj$dataDT <- dataDT
  
  return(specObj)
}


#'
#'
#' @title  residualizeReturns
#'
#' @description  #' function to Residualize the returns via regressions
#'
#' @param specObj  specObj is a ffmSpec object,
#' @param benchmark we might need market returns
#' @param rfRate risk free rate
#' @param isBenchExcess toggle to select whether to calculate excess returns or not
#' @import data.table
#' @export
residualizeReturns <- function(specObj, benchmark, rfRate, isBenchExcess = F ){
  
  dataDT <- copy(specObj$dataDT) # hard_copy
  currKey <- key(dataDT)
  d_ <- eval(specObj$date.var)
  
  setkeyv(dataDT, d_) # for merging with bench and ref
  
  a_ <- eval(specObj$asset.var) # name of the asset column or id
  r_ <- specObj$yVar # name of the variable column for returns.. sometimes get sometimes eval
  # we need this variable to be created.. in case returns are not standardized
  #dataDT[, rawReturn := get(r_)]
  
  # the benchmark is required to be in  an xts so that we know where the date is
  if (is.xts(benchmark)){
    specObj$benchmark.var <- colnames(benchmark) # do this before converting to data.table
    if (is.null(specObj$benchmark.var)) stop("benchmark data must have column names.")
    benchmark <- as.data.table(benchmark)
    setnames(benchmark, old = "index", d_) # this way we are able to merge
    benchmark[[d_]] <- as.Date(benchmark[[d_]])
    setkeyv(benchmark, d_)
    dataDT <- merge(dataDT, benchmark, all.x = TRUE) # left join
    
  } else {
    stop("Invalid args: benchmark must be an xts.")
  }
  
  if (is.xts(rfRate)){
    specObj$rfRate.var <- colnames(rfRate)
    if (is.null(specObj$rfRate.var)) stop("risk free vector must have a column name.")
    rfRate <- as.data.table(rfRate)
    setnames(rfRate, old = "index", d_) # this way we are able to merge
    rfRate[[d_]] <- as.Date(rfRate[[d_]])
    setkeyv(rfRate, d_)
    dataDT <- merge(dataDT, rfRate, all.x = TRUE) # left join
    
  } else {
    stop("Invalid args: rfRate must be an xts.")
  }
  
  setkeyv(dataDT, currKey)
  dataDT[, ExcessReturn := get(r_) - get(specObj$rfRate.var)]
  
  if (!isBenchExcess) {
    for (b_ in specObj$benchmark.var){
      dataDT[, eval(b_) := get(b_) - get(specObj$rfRate.var)]
    }
    
  }
  
  residuals.DT <- dataDT[, .(resid = .(residuals(lm(ExcessReturn ~0+ get(specObj$benchmark.var))))) , by = a_]
  dataDT[, ResidualizedReturn := unlist(residuals.DT$resid)]
  
  specObj$yVar <- "ResidualizedReturn"
  specObj$residualizedReturns <- TRUE
  specObj$dataDT <- copy(dataDT)
  
  return(specObj)
}


#' @title standardizeReturns
#'
#' @description Standardize the returns using GARCH(1,1) volatilities.
#' @param specObj  is a ffmSpec object
#' @param GARCH.params fixed Garch(1,1) parameters
#' @return an ffmSpec Object with the standardized returns added
#' @import data.table
#' @export
standardizeReturns <- function(specObj, GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81)){
  
  dataDT <- copy(specObj$dataDT) # hard_copy
  a_ <- eval(specObj$asset.var) # name of the asset column or id
  r_ <- specObj$yVar # name of the variable column for returns.. sometimes get sometimes eval
  # we need this variable to be created.. in case returns are not standardized
  
  alpha <- GARCH.params$alpha
  beta <- GARCH.params$beta
  dataDT[, sdReturns := .(sd(get(r_), na.rm = TRUE)), by = a_]
  
  # for each asset calculate squared returns
  dataDT[, ts := get(r_)^2]
  
  for ( i in 1: NROW(dataDT))
    set(dataDT,i, "sigmaGarch",
        ifelse(dataDT$idx[i] == 1,
               (1 - alpha - beta) * dataDT$sdReturns[i]^2 +  alpha * dataDT$ts[i],
               (1 - alpha - beta) * dataDT$sdReturns[i]^2 + alpha * dataDT$ts[i] +
                 beta * dataDT$sigmaGarch[i-1]))
  
  
  dataDT[, sigmaGarch := sqrt(sigmaGarch)]
  
  #dataDT[, stdReturns:=get(r_)]
  
  specObj$standardizedReturns <- TRUE
  
  # dataDT[, preStdReturns := get(r_)]
  # if we standardize then we do regressions with the std returns?
  # dataDT[, eval(r_) := get(r_)/sigmaGarch]
  dataDT[, StandardizedReturns := get(r_)/sigmaGarch]
  specObj$yVar <- "StandardizedReturns"
  # dataDT[, stdReturns := get(r_)]
  dataDT[, sdReturns := NULL]
  dataDT[, ts := NULL]
  # dataDT[, sigmaGarch := NULL]
  specObj$dataDT <- copy(dataDT)
  
  return(specObj)
}



#'
#' @title fitFfmDT
#' @description This function fits a fundamental factor model
#' @param ffMSpecObj a specFFm object
#' @param fit.method method for estimating factor returns; one of "LS", "WLS"
#' "ROB" or "W-ROB". See details. Default is "LS".
#' @param resid.scaleType one of 4 choices "StdDev","EWMA","RobustEWMA", "GARCH"
#' @param lambda the ewma parameter
#' @param GARCH.params control structure for GARCH e.g. list(omega = 0.09, alpha = 0.1, beta = 0.81)
#' @param GARCH.MLE if GARCH is used we can use maximum likelihood
#' @return \code{fitFfm} returns a list with two object of class \code{"data.table"}
#' The first reg.listDT is object of class \code{"data.table"} is a list containing the following
#' components:
#' \item{DATE}{length-T vector of dates.}
#' \item{id}{length-N vector of asset id's for each date.}
#' \item{reg.list}{list of fitted objects that estimate factor returns in each
#' time period. Each fitted object is of class \code{lm} if
#' \code{fit.method="LS" or "WLS"}, or, class \code{lmrobdetMM} if
#' \code{fit.method="Rob" or "W-Rob"}.}
#' The second betasDT is object of class \code{"data.table"} is a list containing the following
#' components:
#' \item{DATE}{length-T vector of dates.}
#' \item{R_matrix}{The K+1 by K restriction matrix where K is the number of categorical variables for each date.}
#'
#' @importFrom RobStatTM lmrobdetMM
#' @import data.table
#' @export
#'
fitFfmDT <- function(ffMSpecObj,
                     fit.method=c("LS","WLS","Rob","W-Rob"),
                     resid.scaleType = c("StdDev","EWMA","RobustEWMA", "GARCH"),
                     lambda = 0.9, GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81),
                     GARCH.MLE = FALSE, lmrobdet.control.para.list = lmrobdet.control(), ...){
  
  
  fit.method = toupper(fit.method[1])
  fit.method <- match.arg(arg = fit.method, choices = toupper(c("LS","WLS","ROB","W-ROB")), several.ok = F )
  
  resid.scaleType <- toupper(resid.scaleType[1])
  resid.scaleType <- match.arg(arg = resid.scaleType, choices = c("STDDEV","EWMA","ROBUSTEWMA", "GARCH"))
  
  # if ((resid.scaleType != "STDDEV") && !(fit.method %in% c("WLS","W-Rob"))) {
  #   stop("Invalid args: resid.scaleType ", resid.scaleType, " must be used with WLS or W-Rob")
  # }
  
  a_ <- eval(ffMSpecObj$asset.var) # data table requires variable names to be evaluated
  d_ <- eval(ffMSpecObj$date.var)
  
  # SET UP of FORMULAS ----
  # determine factor model formula to be passed to lm or lmrobdetMM
  fm.formula <- paste(ffMSpecObj$yVar, "~", paste(ffMSpecObj$exposure.vars, collapse="+"))
  
  if (!ffMSpecObj$model.MSCI){
    
    
    if (length(ffMSpecObj$exposures.char)){
      #Remove Intercept as it introduces rank deficiency in the exposure matrix.
      #Implemetation with Intercept is handled later, using a Restriction matrix
      # to remove the rank deficiency.
      fm.formula <- paste(fm.formula, "- 1")
      ffMSpecObj$dataDT[, eval(ffMSpecObj$exposures.char) :=  as.factor(get(ffMSpecObj$exposures.char))]
      #formula to extract beta of Sec or Country
      formula.expochar = as.formula(paste(ffMSpecObj$yVar, "~", ffMSpecObj$exposures.char, "-1"))
      
      factor.names <- c("Market", paste(levels(ffMSpecObj$dataDT[[ffMSpecObj$exposures.char]]),sep=" "),
                        ffMSpecObj$exposures.num)
      
    } else if (ffMSpecObj$addIntercept == FALSE){
      fm.formula <- paste(fm.formula, "- 1")
    }
    # convert the pasted expression into a formula object
    fm.formula <- as.formula(fm.formula)
    
    sdcols <- c(key(ffMSpecObj$dataDT), ffMSpecObj$yVar, ffMSpecObj$exposure.vars )
    #Beta  is for the whole model (generally without intercept)
    #clean up NA's
    ffMSpecObj$dataDT <- ffMSpecObj$dataDT[complete.cases(ffMSpecObj$dataDT[, .SD, .SDcols = sdcols])]
    betasDT <- ffMSpecObj$dataDT[, .(toRegress = .(.SD),
                                     beta = .(model.matrix(fm.formula, .SD))),
                                 .SDcols = sdcols, by = d_]
    idxNA <- sapply(betasDT$toRegress, FUN = anyNA) # this could exist due to LAGGING of exposures
    
    if (length(ffMSpecObj$exposures.char)){
      beta.expochar <- ffMSpecObj$dataDT[, .(beta.expochar = .(model.matrix(formula.expochar, .SD))), .SDcols = sdcols, by = d_]
      #Define beta.star as Beta of the whole model with Intercept/Market represtend by col of ones
      beta.expochar[, beta.star := .(.(cbind("Market" = rep(1, nrow(beta.expochar[[1]])),
                                             beta.expochar[[1]]))), by = d_]
      #Number of factors
      beta.expochar[, K := .(dim(beta.star[[1]])[2]), by = d_]
      setkeyv(betasDT, d_)
      setkeyv(beta.expochar, d_)
      
      betasDT <- betasDT[beta.expochar]
    }
    
    
    
    if (ffMSpecObj$addIntercept == TRUE && ffMSpecObj$model.styleOnly ==FALSE) {
      # we need to create a restriction matrix
      #Define Restriction matrix
      betasDT[, R_matrix := .(.(rbind(diag(K-1), c(0,rep(-1,K-2))))), by = d_]
      
      #Define B.Mod = X*R
      betasDT[, B.mod := .(.(beta.star[[1]] %*% R_matrix[[1]])), by = d_]
      
      betasDT[, toRegress := .(.(cbind(B.mod[[1]],toRegress[[1]] ))), by = d_]
      
      setkeyv(betasDT, d_)
      if(length(ffMSpecObj$exposures.num) > 0){
        sdcols <- ffMSpecObj$exposures.num
        #Define Beta for Style factors
        tempDT <- ffMSpecObj$dataDT[, .(B.style = .(as.matrix(x = .SD))),
                                    .SDcols = sdcols, by = d_]
        setkeyv(tempDT, ffMSpecObj$date.var)
        
        betasDT <- betasDT[tempDT]
        betasDT[, beta.mod.style := .(.(cbind(B.mod[[1]],B.style[[1]]))), by = d_]
        
      }
      
      #Formula for Markt+Sec/Country Model
      K <- length(levels(ffMSpecObj$dataDT[[ffMSpecObj$exposures.char]]))
      B.mod = paste0("V", 1:K) # variable names
      fmSI.formula = as.formula(paste(ffMSpecObj$yVar, "~",
                                      paste(c(B.mod,ffMSpecObj$exposures.num),collapse = "+")
                                      ,"-1" ))
      fm.formula = fmSI.formula
    }
  } else {
    
    # MSCI..
    if (length(ffMSpecObj$exposures.char)) {
      fm.formula <- paste(fm.formula, "- 1")
      ffMSpecObj$dataDT[ ,  (ffMSpecObj$exposures.char) := lapply(.SD, as.factor), .SDcols = ffMSpecObj$exposures.char]
      
      formulaL = list()
      
      for(i in ffMSpecObj$exposures.char)
      {
        formulaL[[i]] = as.formula(paste(ffMSpecObj$yVar, "~", i, "-1"))
      }
    }
    
    # convert the pasted expression into a formula object
    fm.formula <- as.formula(fm.formula)
    #Extract model beta, expo.char beta and expo.num betas
    sdcols <- c(key(ffMSpecObj$dataDT), ffMSpecObj$yVar, ffMSpecObj$exposure.vars )
    #Beta  is for the whole model (generally without intercept)
    #clean up NA's
    ffMSpecObj$dataDT <- ffMSpecObj$dataDT[complete.cases(ffMSpecObj$dataDT[, .SD, .SDcols = sdcols])]
    betasDT <- ffMSpecObj$dataDT[, .(toRegress = .(.SD),
                                     beta = .(model.matrix(fm.formula, .SD)),
                                     beta1 = .(model.matrix(formulaL[[1]], .SD)),
                                     beta2 = .(model.matrix(formulaL[[2]], .SD))),
                                 .SDcols = sdcols, by = d_]
    betasDT[ , beta.mic := .(.(cbind("Market" = rep(1, nrow(beta1[[1]])), beta1[[1]], beta2[[1]]))), by = d_]
    # for now we are skipping over adding a style... lines 692/693
    
    
    betasDT[, K1 := .(dim(beta1[[1]])[2]), by = d_]
    
    betasDT[, K2 := .(dim(beta2[[1]])[2]), by = d_]
    
    # we need to create a restriction matrix
    #Define Restriction matrix
    
    betasDT[, R_matrix := .(.(rbind( cbind(diag(K1), matrix(0, nrow = K1, ncol = K2-1)),
                                     c(c(0,rep(-1, K1-1)), rep(0, K2-1)),
                                     cbind(matrix(0, ncol = K1, nrow = K2-1), diag(K2-1)),
                                     c(rep(0, K1), rep(-1, K2-1))))), by = d_]
    
    
    #Define B.Mod = X*R
    betasDT[, B.mod := .(.(beta.mic[[1]] %*% R_matrix[[1]])), by = d_]
    
    betasDT[, toRegress := .(.(cbind(B.mod[[1]],toRegress[[1]] ))), by = d_]
    
    setkeyv(betasDT, d_)
    # if(length(ffMSpecObj$exposures.num) > 0){
    #   sdcols <- ffMSpecObj$exposures.num
    #   #Define Beta for Style factors
    #   tempDT <- ffMSpecObj$dataDT[, .(B.style = .(as.matrix(x = .SD))),
    #                               .SDcols = sdcols, by = d_]
    #   setkeyv(tempDT, ffMSpecObj$date.var)
    #
    #   betasDT <- betasDT[tempDT]
    #   betasDT[, beta.mod.style := .(.(cbind(B.mod[[1]],B.style[[1]]))), by = d_]
    #
    # }
    idxNA <- sapply(betasDT$toRegress, FUN = anyNA) # this could exist due to LAGGING of exposures
    
    #Formula for Markt+Sec/Country Model
    # we need to get a count of all factors levels + the intercept
    K = (betasDT[1,]$K1[[1]]) + (betasDT[1,]$K2[[1]])+1 - 2 # this is a hack for now, we subtract 2 to account for the reference level
    B.mod = paste0("V", 1:K) # variable names
    fmMSCI.formula = as.formula(paste(ffMSpecObj$yVar, "~", paste(c(B.mod,ffMSpecObj$exposures.num),collapse = "+"),"-1" ))
    
    
    fm.formula = fmMSCI.formula
    
    
    
    
    
    
  }
  
  # Perform Regressions ----
  
  # estimate factor returns using LS or Robust regression ----
  # returns a list of the fitted lm or lmrobdetMM objects for each time period
  if (grepl("LS",fit.method)) {
    
    reg.listDT <- betasDT[which(!idxNA), .(id = .(toRegress[[1]][[a_]]),
                                           reg.list = .(lm(formula = fm.formula, data = toRegress[[1]],
                                                           na.action=na.omit))), by = d_]
    
  }else if (grepl("ROB",fit.method)) {
    
    
    reg.listDT <- betasDT[which(!idxNA), .(id = .(toRegress[[1]][[a_]]),
                                           reg.list = .(lmrobdetMM(formula = fm.formula,
                                                                   data = toRegress[[1]],
                                                                   na.action = na.omit,
                                                                   control =  lmrobdet.control.para.list))), by = d_]
  }
  # second pass weighted regressions ----
  if (grepl("W",fit.method)) {
    
    SecondStepRegression <- rbindlist(betasDT$toRegress)
    # compute residual variance for all assets for weighted regression
    # the weights will be 1/w
    SecondStepRegression <- calcAssetWeightsForRegression(specObj = ffMSpecObj, fitResults = reg.listDT,
                                                          SecondStepRegression = SecondStepRegression, resid.scaleType = resid.scaleType,
                                                          lambda = lambda, GARCH.params = GARCH.params, GARCH.MLE = GARCH.MLE)
    # estimate factor returns using WLS or weighted-Robust regression
    # returns a list of the fitted lm or lmrobdetMM objects for each time period
    # w <- SecondStepRegression[, c(d_, a_, "W"), with = F] # needed for the residual variances
    # w$W <- 1/w$W
    if (fit.method=="WLS") {
      reg.listDT <- SecondStepRegression[ complete.cases(SecondStepRegression[,ffMSpecObj$exposure.vars, with = F]) ,
                                          .(reg.list = .(lm(formula = fm.formula, data = .SD, weights = W, na.action = na.omit)))
                                          , by = d_]
      
    } else if (fit.method=="W-Rob") {
      
      
      reg.listDT <-
        SecondStepRegression[ complete.cases(SecondStepRegression[,ffMSpecObj$exposure.vars, with = F]) ,
                              .(reg.list = .(lmrobdetMM(
                                formula = fm.formula,
                                data = .SD,
                                weights = W,
                                na.action = na.omit,
                                control =  lmrobdet.control.para.list)))
                              , by = d_]
      
    }
    assetInfo <- SecondStepRegression[complete.cases(SecondStepRegression[,ffMSpecObj$exposure.vars, with = F]),
                                      .(id = .(get(a_)), w = .(1/W)), by = d_]
    setkeyv(assetInfo, d_)
    setkeyv(reg.listDT, d_)
    reg.listDT <- reg.listDT[assetInfo]
  }
  
  
  return(list(reg.listDT = reg.listDT, betasDT = betasDT,
              resid.scaleType = resid.scaleType, fit.method = fit.method)
  )
  
  
}




#' @title extractRegressionStats
#' @description function to compute or Extract objects to be returned
#' @param specObj fitFM object that has been already fit
#' @param fitResults output from fitFMDT
#' @param full.resid.cov an option to calculate the full residual covariance or not
#' @return a structure of class ffm holding all the information
#' @importFrom robust covRob
#' @import data.table
#' @export
#'
extractRegressionStats <- function(specObj, fitResults, full.resid.cov=FALSE){
  
  restriction.mat = NULL
  g.cov = NULL
  
  a_ <- eval(specObj$asset.var) # data table requires variable names to be evaluated
  d_ <- eval(specObj$date.var) # name of the date var
  asset.names <- unique(specObj$data[[specObj$asset.var]])
  reg.listDT <- copy(fitResults$reg.listDT)
  betasDT <- copy(fitResults$betasDT)
  resid.scaleType <- fitResults$resid.scaleType # we send this because what we do in the
  # fit is linked to how we extract results
  fit.method <- fitResults$fit.method
  
  # r-squared values for each time period ----
  r2 <- reg.listDT[, .(r2 = .(summary(reg.list[[1]])$r.squared)), by = d_]
  r2 <- unlist(r2$r2)
  names(r2) <- reg.listDT[[d_]]
  
  # residuals ----
  reg.listDT[, residuals := .(.(data.frame(date = get(d_), id = id,
                                           residuals = residuals(reg.list[[1]])))), by = d_]
  # now we have to extract the asset level residuals series and get their time series variance or
  # robust stats
  # residuals1 <- as.data.table(reg.listDT[get(d_) == max(get(d_)),]$residuals[[1]])
  # we have a problem here in case of a jagged matrix
  residuals1 <- data.table::rbindlist(l = reg.listDT$residuals, use.names = F)
  setnames(residuals1, c("date", "id", "residuals") )
  # find the residuals for the assets that exist as of last period
  a_last <- reg.listDT[get(d_) == max(get(d_)),]$id[[1]]
  # this is needed so that the matrices conform
  residuals1 <- residuals1[ id %in% a_last]
  residuals1 <- data.table::dcast(data = residuals1 , formula = date ~ id, value.var = "residuals")
  residuals1 <- as.xts.data.table(residuals1)
  
  # Resdiuals ----
  #if resid.scaleType is not stdDev, use the most recent residual var as the diagonal cov-var of residuals
  if (grepl("W",fit.method)){
    reg.listDT[, w := .(.(data.frame(date = get(d_)[[1]], id = reg.listDT$id[[1]],
                                     w = w[[1]]))), by = d_]
    w <- data.table::rbindlist(l = reg.listDT$w)
    w <- data.table::dcast(data = w , formula = date ~ id, value.var = "w")
    w <- as.xts.data.table(w)
    
    resid.cov  <- diag(as.numeric(w[last(index(w)),])) # use the last estimate
    # update resid.var with the timeseries of estimated resid variances
    resid.var = w
    
    
  }
  #Residual Variance ----
  residuals1 <- residuals1[, which(!is.na(xts::last(residuals1)))]
  resid.var <- apply(coredata(residuals1), 2, var, na.rm=T)
  # resid.var <- resid.var[which(!is.na(xts::last(residuals1)))]
  # if we have an unbalanced panel...then there would be some NA's so we have to clean them up
  # we just need the last period
  
  
  # residual covariances----
  if (specObj$rob.stats) {
    
    resid.var <- apply(coredata(residuals1), 2, scaleTau2)^2
    if (full.resid.cov) {
      resid.cov <- covOGK(coredata(residuals1), sigmamu=scaleTau2, n.iter=1)$cov
    } else {
      resid.cov <- diag(resid.var)
    }
    
  } else {
    
    if (full.resid.cov) {
      resid.cov <- cov(coredata(residuals1), use = "pairwise.complete.obs")
    } else {
      resid.cov <- diag(resid.var)
    }
  }
  
  
  if (specObj$addIntercept == FALSE || specObj$model.styleOnly ==TRUE) {
    # number of factors including Market and dummy variables
    if (length(specObj$exposures.char)) {
      factor.names <- c(specObj$exposures.num,
                        paste(levels(specObj$dataDT[,specObj$exposures.char, with = F][[1]]),sep=""))
    } else {
      if(specObj$addIntercept) {
        factor.names <- c("Alpha", specObj$exposures.num)
      }
      else{
        factor.names <- specObj$exposures.num
      }
      
    }
    
    
    # coefficients ----
    
    reg.listDT[, factor.returns := .(.(data.frame(date = get(d_)[[1]], factor.names = .(factor.names),
                                                  factor.returns = coefficients(reg.list[[1]])))), by = d_]
    
    # now we have to extract the asset level residuals series and get their time series variance or
    # robust stats
    factor.returns <- data.table::rbindlist(l = reg.listDT$factor.returns)
    colnames(factor.returns)[2] <- "factor"
    factor.returns <- data.table::dcast(data = factor.returns , formula = date ~ factor, value.var = "factor.returns")
    setcolorder(factor.returns,  c( "date", factor.names))
    factor.returns <- as.xts.data.table(factor.returns)
    
    #Exposure matrix for the last time period
    beta <- betasDT[ get(d_) == max(get(d_)), ]$beta[[1]]
    rownames(beta) <- reg.listDT[ get(d_) == max(get(d_)), ]$id[[1]]
    if (specObj$addIntercept == TRUE) colnames(beta)[1] <- "Alpha"
    
    
  } else if ( specObj$addIntercept && specObj$model.styleOnly == FALSE && !specObj$model.MSCI) {
    if (length(specObj$exposures.char)) {
      if(specObj$addIntercept) {
        factor.names <- c("Market", specObj$exposures.num,
                          paste(levels(specObj$dataDT[,specObj$exposures.char, with = F][[1]]),sep=""))
      } else {
        factor.names <- c(specObj$exposures.num,
                          paste(levels(specObj$dataDT[,specObj$exposures.char, with = F][[1]]),sep=""))
      }
      
    } else {
      if(specObj$addIntercept) {
        factor.names <- c("Alpha", specObj$exposures.num)
      } else {
        factor.names <- specObj$exposures.num
      }
      
    }
    
    
    # coefficients ----
    
    g <- reg.listDT[, .(g = .(coefficients(reg.list[[1]]))), by = d_]
    setkeyv(g, d_)
    #factor returns = restriction matrix * g coefficients
    factor.returns <- betasDT[, c(d_ ,"R_matrix"), with = F][g]
    
    g <- g[, .(.(data.frame(date = get(d_)[[1]], t(g[[1]])))), by = d_]
    g <- rbindlist(g$V1)
    g <- as.xts.data.table(g)
    g.cov <- cov(g)
    K <- length(levels(specObj$dataDT[[specObj$exposures.char]]))
    # the first matrix contains the categorical variables that had the restriction matrix applied to
    # the second is the style varaiibles
    if (length(specObj$exposures.num)) {
      factor.returns <- factor.returns[, .(factor.returns1 = .(R_matrix[[1]] %*% g[[1]][1:K]),
                                           factor.returns2 = .(g[[1]][(K+1): length(g[[1]])])), by = d_]
      
      factor.returns[, factor.returns := .(.(matrix(c(factor.returns1[[1]],
                                                      factor.returns2[[1]]), nrow = 1,
                                                    dimnames = list(date = eval(d_)[[1]],
                                                                    factors = c("Market",
                                                                                levels(specObj$dataDT[[specObj$exposures.char]]),
                                                                                names(factor.returns2[[1]])
                                                                    ))))), by = d_]
      
    } else {
      factor.returns <- factor.returns[, .(factor.returns1 = .(R_matrix[[1]] %*% g[[1]][1:K])), by = d_]
      factor.returns[, factor.returns := .(.(matrix(c(factor.returns1[[1]]), nrow = 1,
                                                    dimnames = list(date = eval(d_)[[1]],
                                                                    factors = c("Market",
                                                                                levels(specObj$dataDT[[specObj$exposures.char]]))
                                                    )))), by = d_]
    }
    
    factor.returns[ , factor.returns := .(.(data.frame(date = get(d_)[[1]], factor.returns[[1]]))), by = d_]
    factor.returns <- rbindlist(factor.returns$factor.returns)
    factor.returns <- as.xts.data.table(factor.returns)
    
    
    
    #Restriction matrix
    restriction.mat <- betasDT[ get(d_) == max(get(d_)), R_matrix[[1]]]
    
    #Returns covariance
    if(length(specObj$exposures.num) > 0){
      #Exposure matrix for the last time period
      beta.star <- as.matrix(betasDT[ get(d_) == max(get(d_)), beta.star[[1]]])
      B.style <- as.matrix(betasDT[ get(d_) == max(get(d_)), B.style[[1]]])
      
      beta <- cbind(beta.star[,1], B.style, beta.star[,-1])
      colnames(beta) <- factor.names
      beta.stms = as.matrix(betasDT[ get(d_) == max(get(d_)),cbind(B.mod, B.style)])
    } else    {
      #Exposure matrix for the last time period
      beta <- as.matrix(betasDT[ get(d_) == max(get(d_)), beta.star[[1]]])
      rownames(beta) <- asset.names
      beta.stms = as.matrix(betasDT[ get(d_) == max(get(d_)), B.mod[[1]] ])
    }
    # return covariance estimated by the factor model
    
  } else {
    # msci
    lvl <- unlist(sapply(specObj$dataDT[, .SD, .SDcols = specObj$exposures.char], levels))
    
    factor.names <- c("Market", specObj$exposures.num, paste(lvl,sep=""))
    
    g <- reg.listDT[, .(g = .(coefficients(reg.list[[1]]))), by = d_]
    setkeyv(g, d_)
    #factor returns = restriction matrix * g coefficients
    factor.returns <- betasDT[, c(d_ ,"R_matrix"), with = F][g]
    
    g <- g[, .(.(data.frame(date = get(d_)[[1]], t(g[[1]])))), by = d_]
    g <- rbindlist(g$V1)
    g <- as.xts.data.table(g)
    g.cov <- cov(g)
    K <- length(factor.names) - length(specObj$exposures.char)
    
    
    factor.returns <- factor.returns[, .(factor.returns1 = .(R_matrix[[1]] %*% g[[1]][1:K])), by = d_]
    factor.returns[, factor.returns := .(.(matrix(c(factor.returns1[[1]]), nrow = 1,
                                                  dimnames = list(date = eval(d_)[[1]],
                                                                  factors = factor.names)))), by = d_]
    
    
    factor.returns[ , factor.returns := .(.(data.frame(date = get(d_)[[1]], factor.returns[[1]]))), by = d_]
    factor.returns <- rbindlist(factor.returns$factor.returns)
    factor.returns <- as.xts.data.table(factor.returns)
    
    
    #Exposure matrix for the last time period
    beta <- as.matrix(betasDT[ get(d_) == max(get(d_)), beta.mic[[1]]])
    rownames(beta) <- asset.names
    beta.stms = as.matrix(betasDT[ get(d_) == max(get(d_)), beta.mic[[1]] ])
  }
  
  # factor covariances ----
  if (specObj$rob.stats) {
    if (kappa(na.exclude(coredata(factor.returns))) < 1e+10) {
      factor.cov <- covRob(coredata(factor.returns), estim="pairwiseGK",
                           distance=FALSE, na.action = na.omit)$cov
    } else {
      cat("Covariance matrix of factor returns is singular.\n")
      factor.cov <- covRob(coredata(factor.returns), distance=FALSE,
                           na.action = na.omit)$cov
    }
  } else {
    factor.cov <- cov(coredata(factor.returns), use = "pairwise.complete.obs")
    
  }
  
  # return Covariance ----
  if (specObj$addIntercept == FALSE || specObj$model.styleOnly ==TRUE || specObj$model.MSCI) {
    # return covariance estimated by the factor model
    #(here beta corresponds to the exposure of last time period,TP)
    return.cov <-  beta %*% factor.cov %*% t(beta) + resid.cov
    dimnames(return.cov) <- list(names(resid.var) ,names( resid.var))
  } else if ( specObj$addIntercept && specObj$model.styleOnly == FALSE) {
    # return covariance estimated by the factor model
    return.cov <-  beta.stms %*% g.cov %*% t(beta.stms) + resid.cov
    
  }
  
  if (!identical(colnames(beta) , colnames(factor.returns))){
    # we need to clean up.. easier to do it on the beta rather than the 
    # factor returns ... (factor.cov) follows factor.returns
    colnames(beta) <- sub(pattern = specObj$exposures.char,
                          colnames(beta),replacement = "")
    # the names of the beta matrix have a prefix when we have the flag
    # add intercept F and have an exposure variable that is a character.
    # now that we have cleaned it up we can rearrange the columns
    beta = beta[, match(colnames(factor.returns), colnames(beta))]
  }
  
  # create list of return values.
  result <- list(beta=beta, factor.returns=factor.returns,
                 residuals=residuals1, r2=r2, factor.cov=factor.cov, g.cov = g.cov,
                 resid.cov=resid.cov, return.cov=return.cov, restriction.mat=restriction.mat,
                 resid.var=resid.var,
                 factor.names=factor.names)
  
  class(result) <- "ffm"
  return(result)
  
  
  
  
}





#' @title calcFLAM
#' @description function to calculate fundamental law of active management
#' @param analysis method used in the analysis of fundamental law of active management; one of "none", "ISM",
#' or "NEW". Default is "none".
#' @param targetedVol numeric; the targeted portfolio volatility in the analysis. Default is 0.06.
calcFLAM <- function(specObj, modelStats, fitResults, analysis = c("ISM", "NEW"),
                     targetedVol = 0.06, ...){
  # only works for SFM
  analysis <- match.arg(toupper(analysis[1]), choices = c("ISM", "NEW"), several.ok = F)
  
  # check if returns are lagged.. or I guess exposures are lagged then proceed.
  d_ <- eval(specObj$date.var)
  a_ <- eval(specObj$asset.var)
  r_ <- specObj$yVar # get(r_)
  # r_ is standardized in NEW and not in ISM
  # IC ----
  IC <- NULL
  # this is equation (25) and (26) for single factor models and for multi factor models equations (34) & (35)
  for (e_ in specObj$exposures.num) {
    
    # we should use pearson?
    ICtemp <- specObj$dataDT[, (IC_ = .(cor(get(e_), get(r_), use = "pair"))) , by = d_]
    
    setnames(ICtemp,c(d_, paste0("IC_", e_)))
    setkeyv(ICtemp, d_)
    if (is.null(IC)) {
      IC <- ICtemp # the first exposure
    } else {
      IC <- IC[ICtemp] # else merge the data
    }
  }
  IC <- as.xts.data.table(IC)
  # number of assets.... since they can change from month to month we will calculate mean # of assets
  N <- mean(specObj$dataDT[, .N, by = d_]$N, na.rm = TRUE)
  
  meanIC <- colMeans(IC)
  sigmaIC <- apply(IC, MARGIN = 2, sd)
  
  IR_GK <- meanIC * sqrt(N)
  IR_inf <- meanIC / sigmaIC
  IR_N <- meanIC / sqrt((1 - meanIC^2 - sigmaIC^2) / N + sigmaIC ^ 2)
  
  temp <- (specObj$dataDT[get(d_) == max(get(d_)),c(a_,e_), with = F])
  stdExposures <- as.numeric(temp[[e_]])
  names(stdExposures) <- temp[[a_]]
  
  resid.var <- modelStats$resid.var
  f_rets <- modelStats$factor.returns
  if (analysis == "ISM") {
    mu <- mean(f_rets)
    sig <- sd(f_rets)
  } else {
    mu <- meanIC
    sig <- sigmaIC
  }
  if (analysis == "ISM"){
    
    condAlpha <- mu * stdExposures
    condOmega <-  sig^2 * (stdExposures %*% t(stdExposures)) + diag(resid.var)
  } else {
    sigmaGarch <- specObj$dataDT[ get(d_) == max(get(d_)), sigmaGarch]
    
    condAlpha <- mu * diag(sigmaGarch) %*% stdExposures
    names(condAlpha) <- names(stdExposures)
    condOmega <- diag(sigmaGarch) %*%
      (sig^2 * stdExposures %*% t(stdExposures) +
         (1 - mu^2 - sig^2)*diag(rep(1, N))) %*% diag(sigmaGarch)
    #
  }
  
  kappa <- (t(condAlpha) %*% solve(condOmega) %*% rep(1, N)) / (rep(1, N) %*% solve(condOmega) %*% rep(1, N))
  K <- as.numeric(kappa) * as.matrix(rep(1, N))
  # activeWeights <- te.target * (solve(condOmega) %*% as.matrix(condAlpha)) /
  #   c(sqrt(t(as.matrix(condAlpha)) %*% solve(condOmega) %*% as.matrix(condAlpha)))
  #
  
  activeWeights <- targetedVol * (solve(condOmega) %*% (as.matrix(condAlpha) - K)) /
    c(sqrt(t(as.matrix(condAlpha)) %*% solve(condOmega) %*% (as.matrix(condAlpha) - K)))
  rownames(activeWeights) <- names(stdExposures)
  
  return(list(meanIC = meanIC, sigmaIC = sigmaIC, IR_GK = IR_GK, IR_inf = IR_inf,
              IR_N = IR_N, IC = IC, N= N, activeWeights = activeWeights))
  
  
}

# private functions ----
#' @importFrom robustbase scaleTau2 covOGK
#' @import data.table
#Calculate Weights For Second Weighted Regression (private function)
calcAssetWeightsForRegression <- function(specObj, fitResults , SecondStepRegression,
                                          resid.scaleType = "STDDEV",  lambda = 0.9,
                                          GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81),
                                          GARCH.MLE = FALSE){
  
  resid.scaleType = toupper(resid.scaleType[1])
  resid.scaleType <- match.arg(arg = resid.scaleType, choices = toupper(c("STDDEV","EWMA","ROBUSTEWMA", "GARCH")), several.ok = F )
  
  a_ <- eval(specObj$asset.var) # data table requires variable names to be evaluated
  d_ <- eval(specObj$date.var) # name of the date var
  
  fitResults[, residuals := .(.(data.frame(date = get(d_)[[1]], id = fitResults$id[[1]],
                                           residuals = residuals(reg.list[[1]])))), by = d_]
  # now we have to extract the asset level residuals series and get their time series variance or
  # robust stats
  resid.DT <- data.table::rbindlist(l = fitResults$residuals)
  setkey(resid.DT, id, date)
  
  resid.DT[, idx := 1:.N, by = id] # this is needed for path dependent calculations
  
  if (specObj$rob.stats) {
    resid.DT[, resid.var := scaleTau2(residuals)^2, by = id]
  } else {
    resid.DT[, resid.var := var(residuals), by = id]
  }
  #Compute cross-sectional weights using EWMA or GARCH
  if((resid.scaleType != "STDDEV")){
    
    if(resid.scaleType == "EWMA"){
      
      #Use sample variance as the initial variance
      for ( i in 1:NROW(resid.DT))
        set(resid.DT,i, "w", ifelse(resid.DT$idx[i] == 1,
                                    resid.DT$resid.var[i],
                                    (1 - lambda) * resid.DT$residuals[i]^2 + lambda * resid.DT$w[i - 1]))
    } else if (resid.scaleType == "ROBUSTEWMA"){
      #Use sample variance as the initial variance
      for ( i in 1:NROW(resid.DT))
        #ifelse conditon is used to check if robust EWMA weights has to be calculated.
        #The rejection threshold a=2.5 is used as mentioned in eq 6.6 of Martin (2005)
        set(resid.DT,i, "w", ifelse(resid.DT$idx[i] == 1, resid.DT$var[i],
                                    ifelse(abs(resid.DT$residuals[i]) <= 2.5*sqrt(resid.DT$w[i-1]),
                                           lambda * resid.DT$w[i - 1] + (1 - lambda) * resid.DT$residuals[i]^2,
                                           resid.DT$w[i - 1])))
      
    } else if(resid.scaleType == "GARCH") {
      
      #Compute parameters using MLE
      if(GARCH.MLE){
        garch.spec = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                                mean.model=list(armaOrder=c(0,0), include.mean = FALSE),
                                distribution.model="norm")
        resid.DT[, w := ugarchfit(garch.spec, data = .SD)@fit$var, .SDcols = c("residuals"), by = id]
        
        
      } else {
        # use fixed parameters
        # default values of omega, Alpha and beta are based on Martin and Ding (2017)
        alpha = GARCH.params$alpha
        beta =  GARCH.params$beta
        #Use sample variance as the initial variance
        for ( i in 1:NROW(resid.DT))
          set(resid.DT,i, "w", ifelse(resid.DT$idx[i] == 1,
                                      resid.DT$resid.var[i],
                                      (1 - alpha - beta)*resid.DT$resid.var[i] +
                                        alpha * resid.DT$residuals[i-1]^2 + beta * resid.DT$w[i - 1]))
        
        
        
      }
      
    }
    
    W = resid.DT[, .(W = 1/w), by = c("id", "date")] # id is the asset id
    setnames(W,old =  c("id","date"), c(a_, d_)) # we need the original name of the asset id
    
    # when the weighing scheme is not std deviation we need to merge bak by date and id
    # since the weights are time varying rather than jst 1/sample variance
    setkeyv(W, c(a_, d_)) # so that we can merger it back with the regression data set and
    setkeyv(SecondStepRegression, c(a_, d_))
    
  } else {
    W = resid.DT[, .(W = 1/unique(resid.var)), by = id] # id is the asset id
    setnames(W,old =  "id", a_) # we need the original name of the asset id
    setkeyv(W, a_) # so that we can merger it back with the regression data set and
    # run weighted regressions
    setkeyv(SecondStepRegression, a_)
    
  }
  
  
  SecondStepRegression <- SecondStepRegression[W]
  setkeyv(SecondStepRegression, c(d_, a_))
  return(SecondStepRegression)
}




# S3 methods ----
# function to convert to current class # mido to change to retroFit


#' Function to convert to current class # mido to change to retroFit
#'
#' @param SpecObj an object as the output from specFfm function
#' @param FitObj an object as the output from fitFfmDT function
#' @param RegStatsObj an object as the output from extractRegressionStats function
#' @method convert ffmSpec
#' @export
convert.ffmSpec <- function(SpecObj, FitObj, RegStatsObj, ...) {
  
  asset.names <- names(RegStatsObj$residuals) # unique(SpecObj$dataDT[[SpecObj$asset.var]])
  time.periods <- unique(SpecObj$dataDT[[SpecObj$date.var]])
  temp <- FitObj$reg.listDT[ , summary(reg.list[[1]])$r.squared,
                             by  = eval(SpecObj$date.var)]
  r2 <- temp$V1
  names(r2) <- temp[[SpecObj$date.var]]
  factor.names <- RegStatsObj$factor.names
  
  ffmObj <- list()
  ffmObj$asset.names <- asset.names
  ffmObj$r2 <- r2
  ffmObj$factor.names <- factor.names
  # SpecObj
  ffmObj$asset.var <- SpecObj$asset.var
  ffmObj$date.var <- SpecObj$date.var
  ffmObj$ret.var <- SpecObj$ret.var
  ffmObj$exposure.vars <- SpecObj$exposure.vars
  ffmObj$exposures.num <- SpecObj$exposures.num
  ffmObj$exposures.char <- SpecObj$exposures.char
  ffmObj$data <- copy(SpecObj$dataDT)
  setkeyv(ffmObj$data, c(SpecObj$date.var, SpecObj$asset.var))  # to match the order
  # expected in reporting functions
  ffmObj$data = data.frame(ffmObj$data)
  
  # fit
  ffmObj$time.periods <- time.periods
  ffmObj$factor.fit <- FitObj$reg.listDT$reg.list
  names(ffmObj$factor.fit) <- time.periods
  
  # regStats
  ffmObj$beta <- RegStatsObj$beta
  ffmObj$factor.returns <- RegStatsObj$factor.returns
  ffmObj$restriction.mat <- RegStatsObj$restriction.mat
  ffmObj$factor.cov <- RegStatsObj$factor.cov
  ffmObj$resid.var <- RegStatsObj$resid.var
  ffmObj$residuals <- RegStatsObj$residuals
  ffmObj$g.cov <- RegStatsObj$g.cov
  
  # clean up
  
  class(ffmObj) <- "ffm"
  
  return(ffmObj)
  
}

#' @title convert
#' @description function to convert the new ffm spec object to ffm object to make it
#' easier in plotting and reporting
#' @export
#'
convert <- function(SpecObj, FitObj, RegStatsObj, ...) {
  UseMethod("convert")
}



#' @method print ffmSpec
#' @export
print.ffmSpec <- function(SpecObj, ...){
  a_ <- SpecObj$asset.var
  r_ <- SpecObj$ret.var
  d_ <- SpecObj$date.var
  cat(sprintf("A fundamental factor model specification object.\n "))
  cat(sprintf("The data table is %i rows by %i columns.\n", dim(SpecObj$dataDT)[1],dim(SpecObj$dataDT)[2]))
  cat(sprintf("The asset identifier is: %s . There are %i unique assets.\n", a_, length(unique(SpecObj$dataDT[[a_]]))))
  cat(sprintf("The return variable is in this column: %s \n", r_))
  
  if (SpecObj$standardizedReturns & !SpecObj$residualizedReturns)
    cat(sprintf("Returns have been standardized but not residualized\n"))
  if (!SpecObj$standardizedReturns & SpecObj$residualizedReturns)
    cat(sprintf("Returns have been residualized but not standardized\n "))
  if (SpecObj$standardizedReturns & SpecObj$residualizedReturns)
    cat(sprintf("Returns have been residualized and standardized\n "))
  cat(sprintf("The return variable that is fit in the model is: %s.\n", SpecObj$yVar))
  
  cat(sprintf("The date variable is in this columns: %s.  The data spans from %s to %s.\n", d_,
              SpecObj$dataDT[[d_]][1],  SpecObj$dataDT[[d_]][nrow(SpecObj$dataDT)]))
  
}
