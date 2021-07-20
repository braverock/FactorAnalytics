#' @title roll.fitFfmDT
#'
#' @description roll.fitFfmDT rolls the fundamental factor model
#' 
#' @param ffMSpecObj a specFFm object
#' @param windowSize the size of the fit window
#' @param refitEvery the frequency of fitting
#' @param refitWindow choice of expanding or rolling 
#' @param stdExposuresControl for exposure standardization; (give the Std.Type and lambda)
#' @param stdReturnControl choices to standardize the returns using GARCH controls
#' @param fitControl list of options for fitting the ffm
#' @param full.resid.cov True or False toggle
#' @param analysis choice of "ISM" or "NEW"
#' @export
roll.fitFfmDT <- function(ffMSpecObj, windowSize = 60, refitEvery = 1,  
                          refitWindow = c("Expanding", "Rolling"),
                          stdExposuresControl = list(Std.Type = "timeSeries", lambda = 0.9),
                          stdReturnControl = list(GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81)),
                          fitControl = list(fit.method=c("LS","WLS","Rob","W-Rob"),
                                            resid.scaleType = c("STDDEV","EWMA","ROBEWMA", "GARCH"),
                                            lambda = 0.9, GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81),
                                            GARCH.MLE = FALSE),
                          full.resid.cov = TRUE, analysis = c("ISM", "NEW")
                          ){
  
  refitWindow = toupper(refitWindow[1])
  refitWindow <- match.arg(arg = refitWindow, choices = toupper(c("EXPANDING", "ROLLING")), several.ok = F )
  
  
  d_ <- eval(ffMSpecObj$date.var) # name of the date var
  
  T_ <- length(unique(ffMSpecObj$dataDT[[d_]]))
  Tindx <- 1:T_ 
  uniqueDates <- unique(ffMSpecObj$dataDT[[d_]])
  ffMSpecObj$dataDT[ , rollIdx := idx] # this is for rolling so that it does not get confused with idx 
  
  start = windowSize # this is the starting window
  S <- seq(start , T_, by = refitEvery)
  m = length(S)
  
  
  if(S[m]<T_){
    S = c(S,T_)
    m = length(S)
    
  }
  
  if(refitWindow == "expanding"){
    rollind = lapply(1:m, FUN = function(i) 1:S[i])
  } else{
    # rollind = lapply(1:m, FUN = function(i) max(1, (S[i]-(windowSize-1))):S[i])
    rollind = lapply(1:m, FUN = function(i) (1+(i-1)*refitEvery):S[i])
  }
  names(rollind) <- uniqueDates[sapply(rollind, last)]
  tmp = lapply(as.list(1:m), FUN = function(i) {
    rollingObject <- specFfm(data = ffMSpecObj$dataDT[ rollIdx %in% Tindx[rollind[[i]]]],
                             asset.var = ffMSpecObj$asset.var, ret.var = ffMSpecObj$ret.var, 
                             date.var = ffMSpecObj$date.var, exposure.vars = ffMSpecObj$exposure.vars, 
            weight.var = ffMSpecObj$weight.var, addIntercept = ffMSpecObj$addIntercept, rob.stats = ffMSpecObj$rob.stats)
    # we should add weight var to the spec object
    
    # lagging is done once prior to roll
    
    if (!is.null(stdExposuresControl)) { # apply StandardizeExposures
      rollingObject = standardizeExposures(obj = rollingObject, Std.Type = stdExposuresControl$Std.Type,
                                      lambda = stdExposuresControl$lambda)  
    }
    if (!is.null(stdReturnControl)){# standardize returns
      rollingObject = standardizeReturns(specObj = rollingObject, GARCH.params = stdReturnControl$GARCH.params)
    } 
    
    rollingFit <- fitFfmDT (rollingObject,fit.method = fitControl$fit.method, 
                            resid.scaleType = fitControl$resid.scaleType, lambda = fitControl$lambda, 
                            GARCH.params = fitControl$GARCH.params, GARCH.MLE = fitControl$GARCH.MLE)
    regStats = extractRegressionStats(obj = rollingObject, fitResults = rollingFit, full.resid.cov = full.resid.cov)
    ans <- calcFLAM(specObj = rollingObject, modelStats = regStats, fitResults = results, analysis = analysis[1])
    print(i)
    rebalDate <- uniqueDates[last(Tindx[rollind[[i]]])] # names ?
    rebalExposures <- rollingObject$dataDT[ get(d_) == rebalDate, c(rollingObject$asset.var, rollingObject$exposure.vars), with = FALSE]
    sigmaI <- regStats$resid.var
    # names(rebalExposures) <- names(sigmaI)
    N_ <- length(sigmaI)
    return(list(date = rebalDate, activeWeights = ans$activeWeights, exposures = rebalExposures,
                rebalStats = regStats, flamInfo = ans))
  })
  names(tmp) <- names(rollind)
  return(tmp)
}



