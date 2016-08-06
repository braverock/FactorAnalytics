#' @title Decompose portfolio risk into individual factor contributions and provide tabular report
#' 
#' @description Compute the factor contributions to standard deviation (SD), Value-at-Risk (VaR), 
#' Expected Tail Loss or Expected Shortfall (ES) of the return of individual asset within a portfolio 
#' return of a portfolio based on Euler's theorem, given the fitted factor model.
#' 
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param p confidence level for calculation. Default is 0.95.
#' @param weights a vector of weights of the assets in the portfolio, names of 
#' the vector should match with asset names. Default is NULL, in which case an 
#' equal weights will be used.
#' @param riskMeasure one of 'Sd' (standard deviation), 'VaR' (Value-at-Risk) or 'ES' (Expected Tail 
#' Loss or Expected Shortfall for calculating risk decompositon. Default is 'Sd'
#' @param decompType one of 'RM' (risk measure), 'FMCR' (factor marginal contribution to risk), 
#' 'FCR' 'factor contribution to risk' or 'FPCR' (factor percent contribution to risk). Default is 'RM'
#' @param digits digits of number in the resulting table. Default is NULL, in which case digtis = 3 will be
#' used for decompType = ('RM', 'FMCR', 'FCR'), digits = 1 will be used for decompType = 'FPCR'
#' @param nrowPrint a numerical value deciding number of assets/portfolio in result vector/table to print  
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR & Es. 
#' Default is "np".
#' @param invert a logical variable to choose if change VaR/ES to positive number, default
#' is False 
#' @param use an optional character string giving a method for computing factor
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and 
#' optional arguments passed to \code{\link[stats]{cov}}
#'
#' @return A table containing 
#' \item{decompType = 'RM'}{length-(N + 1) vector of factor model risk measure of portfolio return 
#' as well assets return.}
#' \item{decompType = 'FMCR'}{(N + 1) * (K + 1) matrix of marginal contributions to risk of portfolio 
#' return as well assets return, with first row of values for the portfolio and the remaining rows for 
#' the assets in the portfolio, with  (K + 1) columns containing values for the K risk factors and the
#' residual respectively}
#' \item{decompType = 'FCR'}{(N + 1) * (K + 2) matrix of component contributions to risk of portfolio 
#' return as well assets return, with first row of values for the portfolio and the remaining rows for 
#' the assets in the portfolio, with  first column containing portfolio and asset risk values and remaining
#' (K + 1) columns containing values for the K risk factors and the residual respectively}
#' \item{decompType = 'FPCR'}{(N + 1) * (K + 1) matrix of percentage component contributions to risk 
#' of portfolio return as well assets return, with first row of values for the portfolio and the remaining rows for 
#' the assets in the portfolio, with  (K + 1) columns containing values for the K risk factors and the
#' residual respectively}
#' Where, K is the number of factors, N is the number of assets.
#' 
#' @author Douglas Martin, Lingjie Yi
#' 
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portSdDecomp}} for factor model Sd decomposition.
#' \code{\link{portVaRDecomp}} for factor model VaR decomposition.
#' \code{\link{portEsDecomp}} for factor model ES decomposition.
#' 
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name=colnames(managers)[10], 
#'                      data=managers)
#' report <- repRisk(fit.macro, riskMeasure = "ES", decompType = 'FPCR', 
#'                   nrowPrint = 10)
#' report 
#' 
#' # random weights 
#' wts = runif(6)
#' wts = wts/sum(wts)
#' names(wts) <- colnames(managers)[1:6]
#' repRisk(fit.macro, weights = wts, riskMeasure = "VaR", decompType = 'FMCR', 
#'         nrowPrint = 10, digits = 4)
#' 
#' # Fundamental Factor Model
#' data("stocks145scores6")
#' dat = stocks145scores6
#' dat$DATE = as.yearmon(dat$DATE)
#' dat = dat[dat$DATE >=as.yearmon("2008-01-01") & 
#'           dat$DATE <= as.yearmon("2012-12-31"),]
#'
#' # Load long-only GMV weights for the return data
#' data("wtsStocks145GmvLo")
#' wtsStocks145GmvLo = round(wtsStocks145GmvLo,5)  
#'                                                      
#' # fit a fundamental factor model
#' fit.cross <- fitFfm(data = dat, 
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE",
#'               "VOL121","EP"), date.var = "DATE", ret.var = "RETURN", 
#'               asset.var = "TICKER", fit.method="WLS", z.score = TRUE)
#' repRisk(fit.cross, riskMeasure = "Sd", decompType = 'FCR', nrowPrint = 10,
#'         digits = 4) 
#' # get the factor contributions of risk 
#' repRisk(fit.cross, wtsStocks145GmvLo, riskMeasure = "Sd", decompType = 'FPCR', 
#'         nrowPrint = 10) 
#' repRisk(fit.cross, wtsStocks145GmvLo, riskMeasure = "ES", decompType = 'FCR', 
#'         nrowPrint = 10)                
#'  
#' @export    


repRisk <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm',  or 'ffm'.")
  }
  UseMethod("repRisk")
}

#' @rdname repRisk
#' @method repRisk tsfm
#' @importFrom utils head
#' @export

repRisk.tsfm <- function(object, weights = NULL, riskMeasure = c("Sd", "VaR", "ES"), 
                         decompType = c("RM", 'FMCR', 'FCR', 'FPCR'), digits = NULL, invert = FALSE,
                         nrowPrint = 20, p=0.95, type=c("np","normal"), use="pairwise.complete.obs", ...) {
  
  # set default for type
  type = type[1]
  riskMeasure = riskMeasure[1]
  decompType = decompType[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  if (!prod(riskMeasure %in% c("Sd", "VaR", "ES"))) {
    stop("Invalid args: riskMeasure must be 'Sd', 'VaR' or 'ES' ")
  }
  
  if (!prod(decompType %in% c("RM", 'FMCR', 'FCR', 'FPCR'))) {
    stop("Invalid args: decompType must be 'RM', 'FMCR', 'FCR' or 'FPCR' ")
  }
  
  if(length(which(riskMeasure == "Sd"))){
    port.Sd = portSdDecomp(object, weights = weights, use = use, ... )
    asset.Sd = fmSdDecomp(object, use = use, ... )
    
    if(decompType == "RM"){
      port = port.Sd$portSd
      asset = asset.Sd$Sd.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FMCR"){
      port = port.Sd$mSd
      asset = asset.Sd$mSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FCR"){
      portRM = port.Sd$portSd
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
    }
    
    else if(decompType == "FPCR"){
      port = port.Sd$pcSd
      asset = asset.Sd$pcSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
  }
  
  else if(length(which(riskMeasure == "VaR"))){
    port.VaR = portVaRDecomp(object, weights = weights, p = p, type = type, use = use, invert = invert, ... )
    asset.VaR = fmVaRDecomp(object, p = p, type = type, use = use, invert = invert, ... )
    
    if(decompType == "RM"){
      port = port.VaR$portVaR
      asset = asset.VaR$VaR.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FMCR"){
      port = port.VaR$mVaR
      asset = asset.VaR$mVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FCR"){
      portRM = port.VaR$portVaR
      assetRM = asset.VaR$VaR.fm
      resultRM = c(portRM, assetRM)
      
      port = port.VaR$cVaR
      asset = asset.VaR$cVaR
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
    }
    
    else if(decompType == "FPCR"){
      port = port.VaR$pcVaR
      asset = asset.VaR$pcVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
  }

  else if(length(which(riskMeasure == "ES"))){
    port.Es = portEsDecomp(object, weights = weights, p = p, type = type, use = use, invert = invert, ... )
    asset.Es = fmEsDecomp(object, p = p, type = type, use = use, invert = invert, ... )
    
    if(decompType == "RM"){
      port = port.Es$portES
      asset = asset.Es$ES.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FMCR"){
      port = port.Es$mES
      asset = asset.Es$mES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FCR"){
      portRM = port.Es$portES
      assetRM = asset.Es$ES.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Es$cES
      asset = asset.Es$cES
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
    }
    
    else if(decompType == "FPCR"){
      port = port.Es$pcES
      asset = asset.Es$pcES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
  }
  
  if(is.null(digits)){
    if(decompType == 'FPCR'){
      digits = 1
    }else{
      digits = 3
    }
  }
 
  
  
  result = head(result, nrowPrint)
  result = round(result, digits)
  
  return(result)
}

#' @rdname repRisk
#' @method repRisk ffm
#' @importFrom utils head
#' @export

repRisk.ffm <- function(object, weights = NULL, riskMeasure = c("Sd", "VaR", "ES"),
                        decompType = c("RM", 'FMCR', 'FCR', 'FPCR'), digits = NULL, invert = FALSE,
                        nrowPrint = 20, p=0.95, type=c("np","normal"), ...) {
  
  # set default for type
  type = type[1]
  riskMeasure = riskMeasure[1]
  decompType = decompType[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  if (!prod(riskMeasure %in% c("Sd", "VaR", "ES"))) {
    stop("Invalid args: riskMeasure must be 'Sd', 'VaR' or 'ES' ")
  }
  
  if (!prod(decompType %in% c("RM", 'FMCR', 'FCR', 'FPCR'))) {
    stop("Invalid args: decompType must be 'RM', 'FMCR', 'FCR' or 'FPCR' ")
  }
  
  if(length(which(riskMeasure == "Sd"))){
    port.Sd = portSdDecomp(object, weights = weights, ... )
    asset.Sd = fmSdDecomp(object, ... )
    
    if(decompType == "RM"){
      port = port.Sd$portSd
      asset = asset.Sd$Sd.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FMCR"){
      port = port.Sd$mSd
      asset = asset.Sd$mSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FCR"){
      portRM = port.Sd$portSd
      assetRM = asset.Sd$Sd.fm
      resultRM = c(portRM, assetRM)

      port = port.Sd$cSd
      asset = asset.Sd$cSd
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
    }
    
    else if(decompType == "FPCR"){
      port = port.Sd$pcSd
      asset = asset.Sd$pcSd
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
  }
  
  else if(length(which(riskMeasure == "VaR"))){
    port.VaR = portVaRDecomp(object, weights = weights, p = p, type = type, invert = invert, ... )
    asset.VaR = fmVaRDecomp(object, p = p, type = type, invert = invert, ... )
    
    if(decompType == "RM"){
      port = port.VaR$portVaR
      asset = asset.VaR$VaR.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FMCR"){
      port = port.VaR$mVaR
      asset = asset.VaR$mVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FCR"){
      portRM = port.VaR$portVaR
      assetRM = asset.VaR$VaR.fm
      resultRM = c(portRM, assetRM)
      
      port = port.VaR$cVaR
      asset = asset.VaR$cVaR
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
    }
    
    else if(decompType == "FPCR"){
      port = port.VaR$pcVaR
      asset = asset.VaR$pcVaR
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
  }
  
  else if(length(which(riskMeasure == "ES"))){
    port.Es = portEsDecomp(object, weights = weights, p = p, type = type, invert = invert, ... )
    asset.Es = fmEsDecomp(object, p = p, type = type, invert = invert, ... )
    
    if(decompType == "RM"){
      port = port.Es$portES
      asset = asset.Es$ES.fm
      result = c(port, asset)
      names(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FMCR"){
      port = port.Es$mES
      asset = asset.Es$mES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
    else if(decompType == "FCR"){
      portRM = port.Es$portES
      assetRM = asset.Es$ES.fm
      resultRM = c(portRM, assetRM)
      
      port = port.Es$cES
      asset = asset.Es$cES
      result = cbind(resultRM,rbind(port, asset))
      rownames(result)[1] = 'Portfolio'
      colnames(result)[1] = 'RM'
    }
    
    else if(decompType == "FPCR"){
      port = port.Es$pcES
      asset = asset.Es$pcES
      result = rbind(port, asset)
      rownames(result)[1] = 'Portfolio'
    }
    
  }
  
  if(is.null(digits)){
    if(decompType == 'FPCR'){
      digits = 1
    }else{
      digits = 3
    }
  }
  
  result = head(result, nrowPrint)
  result = round(result, digits)
  
  return(result)

}
