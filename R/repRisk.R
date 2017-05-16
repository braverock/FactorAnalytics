#' @title Decompose portfolio risk into individual factor contributions and provide tabular report
#' 
#' @description Compute the factor contributions to standard deviation (SD), Value-at-Risk (VaR), 
#' Expected Tail Loss or Expected Shortfall (ES) of the return of individual asset within a portfolio 
#' return of a portfolio based on Euler's theorem, given the fitted factor model.
#' 
#' @importFrom lattice barchart
#' @importFrom reshape2 melt
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param p tail probability for calculation. Default is 0.05.
#' @param weights a vector of weights of the assets in the portfolio, names of 
#' the vector should match with asset names. Default is NULL, in which case an 
#' equal weights will be used.
#' @param risk one of 'Sd' (standard deviation), 'VaR' (Value-at-Risk) or 'ES' (Expected Tail 
#' Loss or Expected Shortfall for calculating risk decompositon. Default is 'Sd'
#' @param decomp one of 'FMCR' (factor marginal contribution to risk), 
#' 'FCR' 'factor contribution to risk' or 'FPCR' (factor percent contribution to risk).
#' @param digits digits of number in the resulting table. Default is NULL, in which case digtis = 3 will be
#' used for decomp = ( 'FMCR', 'FCR'), digits = 1 will be used for decomp = 'FPCR'. Used only when 
#' isPrint = 'TRUE'
#' @param nrowPrint a numerical value deciding number of assets/portfolio in result vector/table to print
#' or plot  
#' @param type one of "np" (non-parametric) or "normal" for calculating VaR & Es. 
#' Default is "np".
#' @param sliceby one of 'factor' (slice or condition by factor) or 'asset' (slice or condition by asset)
#' Used only when isPlot = 'TRUE'  
#' @param invert a logical variable to change VaR/ES to positive number, default
#' is False and will return positive values.
#' @param layout layout is a numeric vector of length 2 or 3 giving the number of columns, rows, and pages (optional) in a multipanel display.
#' @param stripText.cex a number indicating the amount by which strip text in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param axis.cex a number indicating the amount by which axis in the plot(s) should be scaled relative to the default. 1=default, 1.5 is 50\% larger, 0.5 is 50\% smaller, etc.
#' @param portfolio.only logical variable to choose if to calculate portfolio only decomposition, in which case multiple risk measures are 
#' allowed.
#' @param isPlot logical variable to generate plot or not.
#' @param isPrint logical variable to print numeric output or not.
#' @param use an optional character string giving a method for computing factor
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and 
#' optional arguments passed to \code{\link[stats]{cov}}
#'
#' @return A table containing 
#' \item{decomp = 'FMCR'}{(N + 1) * (K + 1) matrix of marginal contributions to risk of portfolio 
#' return as well assets return, with first row of values for the portfolio and the remaining rows for 
#' the assets in the portfolio, with  (K + 1) columns containing values for the K risk factors and the
#' residual respectively}
#' \item{decomp = 'FCR'}{(N + 1) * (K + 2) matrix of component contributions to risk of portfolio 
#' return as well assets return, with first row of values for the portfolio and the remaining rows for 
#' the assets in the portfolio, with  first column containing portfolio and asset risk values and remaining
#' (K + 1) columns containing values for the K risk factors and the residual respectively}
#' \item{decomp = 'FPCR'}{(N + 1) * (K + 1) matrix of percentage component contributions to risk 
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
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- factorAnalytics::fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name=colnames(managers[,10]), data=managers)
#' report <- repRisk(fit.macro, risk = "ES", decomp = 'FPCR', 
#'                   nrowPrint = 10)
#' report 
#' 
#' # plot
#' repRisk(fit.macro, risk = "ES", decomp = 'FPCR', isPrint = FALSE, 
#'         isPlot = TRUE)
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
#'               exposure.vars = c("SECTOR","ROE","BP","MOM121","SIZE","VOL121",
#'               "EP"),date.var = "DATE", ret.var = "RETURN", asset.var = "TICKER", 
#'               fit.method="WLS", z.score = TRUE)
#' repRisk(fit.cross, risk = "Sd", decomp = 'FCR', nrowPrint = 10,
#'         digits = 4) 
#' # get the factor contributions of risk 
#' repRisk(fit.cross, wtsStocks145GmvLo, risk = "Sd", decomp = 'FPCR', 
#'         nrowPrint = 10)          
#' # portfolio only decomposition
#' repRisk(fit.cross, wtsStocks145GmvLo, risk = c("VaR", "ES"), decomp = 'FPCR', 
#'         portfolio.only = TRUE)       
#' # plot
#' repRisk(fit.cross, wtsStocks145GmvLo, risk = "Sd", decomp = 'FPCR', 
#'         isPrint = FALSE, nrowPrint = 15, isPlot = TRUE, layout = c(4,2))  
#' @export    


repRisk <- function(object, ...)
{
  # check input object validity
  if(inherits(object, "list"))
  {
    for(i in 1: length(object))
    {
      if (!inherits(object[[i]], c("tsfm", "ffm")))
        stop("Invalid argument: Object should be of class 'tsfm'  or 'ffm'.")
    }
    UseMethod("repRisk", object[[1]])
  }
  
  else
  {
    if (!inherits(object, c("tsfm", "ffm")))
      stop("Invalid argument: Object should be of class 'tsfm'  or 'ffm'.")
    UseMethod("repRisk")
  }
  
  
}
# #' @rdname repRisk
# #' @method repRisk list
# #' @importFrom utils head
# #' @export
# 
# repRisk.list <- function(object, weights = NULL, risk = c("Sd", "VaR", "ES"), 
#                          decomp = c('FPCR','FCR','FMCR' ), digits = NULL, invert = FALSE,
#                          nrowPrint = 20, p=0.05, type=c("np","normal"), use="pairwise.complete.obs", 
#                          sliceby = c('factor', 'asset'), isPrint = TRUE, isPlot = FALSE, layout =NULL,
#                          portfolio.only = FALSE, ...) {
# 
#   riskReport = function(object,weight, risk, 
#                         decomp, digits,invert,
#                         nrowPrint, p, type, use, 
#                         sliceby, isPrint, isPlot, layout,
#                         portfolio.only)
#   {
#     #return(object$r2)
#     if (!inherits(object, c("tsfm", "ffm")))
#       stop("Invalid argument: Object should be of class 'tsfm'  or 'ffm'.")
#   UseMethod("repRisk")
#   }  
#   output.list<- lapply(X = 1:length(object), FUN = function(X){ riskReport(object[[X]],weight, risk, 
#                                                                            decomp, digits,invert,
#                                                                            nrowPrint, p, type, use, 
#                                                                            sliceby, isPrint, isPlot, layout,
#                                                                            portfolio.only)})
# 
# }
#   
#   

#' @rdname repRisk
#' @method repRisk tsfm
#' @importFrom utils head
#' @export

repRisk.tsfm <- function(object, weights = NULL, risk = c("Sd", "VaR", "ES"), 
                         decomp = c('FPCR','FCR','FMCR' ), digits = NULL, invert = FALSE,
                         nrowPrint = 20, p=0.05, type=c("np","normal"), use="pairwise.complete.obs", 
                         sliceby = c('factor', 'asset'), isPrint = TRUE, isPlot = FALSE, layout =NULL,
                         stripText.cex =1,axis.cex=1,portfolio.only = FALSE, ...) {
  
  #   if(inherits(object, "list"))
  #   {
  #     output.list<- lapply(X = 1:length(object), FUN = function(X){ riskReport(object[[X]])})
  #   }
  #   riskReport = function(object)
  #   {
  #     return(object$r2)
  #   }
  
  # set default for type
  type = type[1]
  sliceby = sliceby[1]
  
  if(!portfolio.only){
    risk = risk[1]
  }
  decomp = decomp[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  if (!prod(risk %in% c("Sd", "VaR", "ES"))) {
    stop("Invalid args: risk must be 'Sd', 'VaR' or 'ES' ")
  }
  
  if (!prod(decomp %in% c('FPCR','FCR','FMCR' ))) {
    stop("Invalid args: decomp must be  'FMCR', 'FCR' or 'FPCR' ")
  }
  
  if(!portfolio.only){
    if(length(which(risk == "Sd"))){
      port.Sd = riskDecomp(object, weights = weights,risk = "Sd", ... )
      asset.Sd = riskDecomp(object,risk = "Sd", portDecomp =FALSE, ... )
      
      if(decomp == "FMCR"){
        port = port.Sd$mSd
        asset = asset.Sd$mSd
        result = rbind(port, asset)
        rownames(result)[1] = 'Portfolio'
      } else if(decomp == "FCR"){
        portRM = port.Sd$portSd
        assetRM = asset.Sd$Sd.fm
        resultRM = c(portRM, assetRM)
        
        port = port.Sd$cSd
        asset = asset.Sd$cSd
        result = cbind(resultRM,rbind(port, asset))
        rownames(result)[1] = 'Portfolio'
        colnames(result)[1] = 'RM'
      } else if(decomp == "FPCR"){
        port = port.Sd$pcSd
        asset = asset.Sd$pcSd
        result = rbind(port, asset)
        rownames(result)[1] = 'Portfolio'
        result = cbind(rowSums(result), result)
        colnames(result)[1] = 'Total'
      }
      
    } else if(length(which(risk == "VaR"))){
      port.VaR = riskDecomp(object, risk = "VaR", weights = weights, p = p, type = type, invert = invert, ... )
      asset.VaR = riskDecomp(object, p = p, type = type, invert = invert, risk = "VaR", portDecomp =FALSE, ... )
      
      if(decomp == "FMCR"){
        port = port.VaR$mVaR
        asset = asset.VaR$mVaR
        result = rbind(port, asset)
        rownames(result)[1] = 'Portfolio'
      } else if(decomp == "FCR"){
        portRM = port.VaR$portVaR
        assetRM = asset.VaR$VaR.fm
        resultRM = c(portRM, assetRM)
        
        port = port.VaR$cVaR
        asset = asset.VaR$cVaR
        result = cbind(resultRM,rbind(port, asset))
        rownames(result)[1] = 'Portfolio'
        colnames(result)[1] = 'RM'
      } else if(decomp == "FPCR"){
        port = port.VaR$pcVaR
        asset = asset.VaR$pcVaR
        result = rbind(port, asset)
        rownames(result)[1] = 'Portfolio'
        result = cbind(rowSums(result), result)
        colnames(result)[1] = 'Total'
      }
      
    } else if(length(which(risk == "ES"))){
      port.Es = riskDecomp(object, risk = "ES", weights = weights, p = p, type = type, invert = invert, ... )
      asset.Es = riskDecomp(object, p = p, type = type, invert = invert,risk = "ES", portDecomp =FALSE, ... )
      
      if(decomp == "FMCR"){
        port = port.Es$mES
        asset = asset.Es$mES
        result = rbind(port, asset)
        rownames(result)[1] = 'Portfolio'
      } else if(decomp == "FCR"){
        portRM = port.Es$portES
        assetRM = asset.Es$ES.fm
        resultRM = c(portRM, assetRM)
        
        port = port.Es$cES
        asset = asset.Es$cES
        result = cbind(resultRM,rbind(port, asset))
        rownames(result)[1] = 'Portfolio'
        colnames(result)[1] = 'RM'
      } else if(decomp == "FPCR"){
        port = port.Es$pcES
        asset = asset.Es$pcES
        result = rbind(port, asset)
        rownames(result)[1] = 'Portfolio'
        result = cbind(rowSums(result), result)
        colnames(result)[1] = 'Total'
      }
      
    }
    
    if(isPlot){
      if(decomp == "FCR"){
        result = result[,-1]
      }else if(decomp == "FPCR"){
        result = result[,-1]
      }
      
      if(sliceby == 'factor'){
        result = head(result, nrowPrint)
        
        if(is.null(layout)){
          n = ncol(result)
          l = 3
          while(n %% l == 1){
            l = l+1
          }
          layout = c(l,1)
        }
        
        print(barchart(result[rev(rownames(result)),], groups = FALSE, main = paste(decomp,"of", risk),layout = layout,
                       scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black", cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
        
      }else if(sliceby == 'asset'){
        result = head(result, nrowPrint)
        result = t(result)
        
        if(is.null(layout)){
          n = ncol(result)
          l = 3
          while(n %% l == 1){
            l = l+1
          }
          layout = c(l,1)
        }
        
        print(barchart(result[rev(rownames(result)),], groups = FALSE, main = paste(decomp,"of", risk),layout = layout, 
                       scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black", cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
      }
    }
    
    if(isPrint){
      if(is.null(digits)){
        if(decomp == 'FPCR'){
          digits = 1
        }else{
          digits = 3
        }
      }
      result = head(result, nrowPrint)
      result = round(result, digits)
      
      output = list(decomp = result)
      names(output) = paste(risk,decomp,sep = '')
      
      return(output)
    }
  } else{
    port.Sd = riskDecomp(object, risk = "Sd", weights = weights, ... )
    port.VaR = riskDecomp(object, risk = "VaR", weights = weights, p = p, type = type, invert = invert, ... )
    port.Es = riskDecomp(object, risk = "ES", weights = weights, p = p, type = type, invert = invert, ... )
    
    if(decomp == "FMCR"){
      Sd = port.Sd$mSd
      VaR = port.VaR$mVaR
      Es = port.Es$mES
      result = rbind(Sd, VaR, Es)
      rownames(result) = c('Sd','VaR','ES')
      result = result[risk,]
    } else if(decomp == "FCR"){
      SdRM = port.Sd$portSd
      VaRRM = port.VaR$portVaR
      EsRM = port.Es$portES
      resultRM = c(SdRM, VaRRM, EsRM)
      names(resultRM) = c('Sd','VaR','ES')
      
      Sd = port.Sd$cSd
      VaR = port.VaR$cVaR
      Es = port.Es$cES
      result = rbind(Sd, VaR, Es)
      rownames(result) = c('Sd','VaR','ES')
      result = cbind(resultRM,result)
      colnames(result)[1] = 'RM'
      result = result[risk,]
    } else if(decomp == "FPCR"){
      Sd = port.Sd$pcSd
      VaR = port.VaR$pcVaR
      Es = port.Es$pcES
      result = rbind(Sd, VaR, Es)
      rownames(result) = c('Sd','VaR','ES')
      result = cbind(rowSums(result), result)
      colnames(result)[1] = 'Total'
      result = result[risk,]
    }
    
    if(isPrint){
      if(is.null(digits)){
        if(decomp == 'FPCR'){
          digits = 1
        }else{
          digits = 3
        }
      }
      result = round(result, digits)
      
      if(type=="normal"){
        Type = 'Parametric Normal'
      }else{
        Type = 'Non-Parametric'
      }
      output = list(decomp = result)
      names(output) = paste('Portfolio',decomp, Type, sep = ' ')
      
      return(output)
    }
    
  }
}

#' @rdname repRisk
#' @method repRisk ffm
#' @importFrom utils head
#' @export

repRisk.ffm <- function(object, weights = NULL, risk = c("Sd", "VaR", "ES"),
                        decomp = c('FMCR', 'FCR', 'FPCR'), digits = NULL, invert = FALSE,
                        nrowPrint = 20, p=0.05, type=c("np","normal"), 
                        sliceby = c('factor', 'asset'), isPrint = TRUE, isPlot = FALSE, layout =NULL,
                        stripText.cex =1,axis.cex=1,portfolio.only = FALSE, ...) {
  riskReport = function(object,X,mul.port)
  {
    
   if(mul.port) weights = weights[[X]]
    # set default for type
    type = type[1]
    sliceby = sliceby[1]
    
    if(!portfolio.only){
      risk = risk[1]
    }
    decomp = decomp[1]
    
    if (!(type %in% c("np","normal"))) {
      stop("Invalid args: type must be 'np' or 'normal' ")
    }
    
    if (!prod(risk %in% c("Sd", "VaR", "ES"))) {
      stop("Invalid args: risk must be 'Sd', 'VaR' or 'ES' ")
    }
    
    if (!prod(decomp %in% c( 'FMCR', 'FCR', 'FPCR'))) {
      stop("Invalid args: decomp must be 'FMCR', 'FCR' or 'FPCR' ")
    }
    
    if(!portfolio.only){
      if(length(which(risk == "Sd"))){
        port.Sd = riskDecomp(object,risk = "Sd",weights = weights, ... )
        asset.Sd = riskDecomp(object,risk = "Sd", portDecomp =FALSE, ... )
        
        if(decomp == "FMCR"){
          port = port.Sd$mSd
          asset = asset.Sd$mSd
          result = rbind(port, asset)
          rownames(result)[1] = 'Portfolio'
        } else if(decomp == "FCR"){
          portRM = port.Sd$portSd
          assetRM = asset.Sd$Sd.fm
          resultRM = c(portRM, assetRM)
          
          port = port.Sd$cSd
          asset = asset.Sd$cSd
          result = cbind(resultRM,rbind(port, asset))
          rownames(result)[1] = 'Portfolio'
          colnames(result)[1] = 'RM'
        } else if(decomp == "FPCR"){
          port = port.Sd$pcSd
          asset = asset.Sd$pcSd
          result = rbind(port, asset)
          rownames(result)[1] = 'Portfolio'
          result = cbind(rowSums(result), result)
          colnames(result)[1] = 'Total'
        }
        
      } else if(length(which(risk == "VaR"))){
        port.VaR = riskDecomp(object, risk = "VaR", weights = weights, p = p, type = type, invert = invert, ... )
        asset.VaR = riskDecomp(object,risk = "VaR", portDecomp =FALSE,  p = p, type = type, invert = invert, ... )
        
        if(decomp == "FMCR"){
          port = port.VaR$mVaR
          asset = asset.VaR$mVaR
          result = rbind(port, asset)
          rownames(result)[1] = 'Portfolio'
        } else if(decomp == "FCR"){
          portRM = port.VaR$portVaR
          assetRM = asset.VaR$VaR.fm
          resultRM = c(portRM, assetRM)
          
          port = port.VaR$cVaR
          asset = asset.VaR$cVaR
          result = cbind(resultRM,rbind(port, asset))
          rownames(result)[1] = 'Portfolio'
          colnames(result)[1] = 'RM'
        } else if(decomp == "FPCR"){
          port = port.VaR$pcVaR
          asset = asset.VaR$pcVaR
          result = rbind(port, asset)
          rownames(result)[1] = 'Portfolio'
          result = cbind(rowSums(result), result)
          colnames(result)[1] = 'Total'
        }
        
      } else if(length(which(risk == "ES"))){
        port.Es = riskDecomp(object, risk = "ES", weights = weights, p = p, type = type, invert = invert, ... )
        asset.Es = riskDecomp(object,risk = "ES", portDecomp =FALSE, p = p, type = type, invert = invert, ... )
        
        if(decomp == "FMCR"){
          port = port.Es$mES
          asset = asset.Es$mES
          result = rbind(port, asset)
          rownames(result)[1] = 'Portfolio'
        } else if(decomp == "FCR"){
          portRM = port.Es$portES
          assetRM = asset.Es$ES.fm
          resultRM = c(portRM, assetRM)
          
          port = port.Es$cES
          asset = asset.Es$cES
          result = cbind(resultRM,rbind(port, asset))
          rownames(result)[1] = 'Portfolio'
          colnames(result)[1] = 'RM'
        } else if(decomp == "FPCR"){
          port = port.Es$pcES
          asset = asset.Es$pcES
          result = rbind(port, asset)
          rownames(result)[1] = 'Portfolio'
          result = cbind(rowSums(result), result)
          colnames(result)[1] = 'Total'
        }
        
      }
      
      if(isPlot){
        if(decomp == "FCR"){
          result = result[,-1]
        }else if(decomp == "FPCR"){
          result = result[,-1]
        }
        
        if(sliceby == 'factor'){
          result = head(result, nrowPrint)
          
          if(is.null(layout)){
            n = ncol(result)
            l = 3
            while(n %% l == 1){
              l = l+1
            }
            layout = c(l,1)
          }
          
          print(barchart(result[rev(rownames(result)),], groups = FALSE, main = paste(decomp,"of", risk, switch(mul.port, "1" = paste("for port", X), "")),layout = layout,
                         scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black", cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
          
        }else if(sliceby == 'asset'){
          result = head(result, nrowPrint)
          result = t(result)
          
          if(is.null(layout)){
            n = ncol(result)
            l = 3
            while(n %% l == 1){
              l = l+1
            }
            layout = c(l,1)
          }
          
          print(barchart(result[rev(rownames(result)),], groups = FALSE, main = paste(decomp,"of", risk, switch(mul.port, "1" = paste("for port", X), "")),layout = layout, 
                         scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black", cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
        }
      }
      
      if(isPrint){
        if(is.null(digits)){
          if(decomp == 'FPCR'){
            digits = 1
          }else{
            digits = 3
          }
        }
        result = head(result, nrowPrint)
        result = round(result, digits)
        
        output = list(decomp = result)
        names(output) = paste(risk,decomp,sep = '')
        
        return(output)
      }
    } else{
      port.Sd = riskDecomp(object, risk = "Sd", weights = weights, ... )
      port.VaR = riskDecomp(object, risk ="VaR", weights = weights, p = p, type = type, invert = invert, ... )
      port.Es = riskDecomp(object,risk ="ES", weights = weights, p = p, type = type, invert = invert, ... )
      
      if(decomp == "FMCR"){
        Sd = port.Sd$mSd
        VaR = port.VaR$mVaR
        Es = port.Es$mES
        result = rbind(Sd, VaR, Es)
        rownames(result) = c('Sd','VaR','ES')
        result = result[risk,]
      } else if(decomp == "FCR"){
        SdRM = port.Sd$portSd
        VaRRM = port.VaR$portVaR
        EsRM = port.Es$portES
        resultRM = c(SdRM, VaRRM, EsRM)
        names(resultRM) = c('Sd','VaR','ES')
        
        Sd = port.Sd$cSd
        VaR = port.VaR$cVaR
        Es = port.Es$cES
        result = rbind(Sd, VaR, Es)
        rownames(result) = c('Sd','VaR','ES')
        result = cbind(resultRM,result)
        colnames(result)[1] = 'RM'
        result = result[risk,]
      } else if(decomp == "FPCR"){
        Sd = port.Sd$pcSd
        VaR = port.VaR$pcVaR
        Es = port.Es$pcES
        result = rbind(Sd, VaR, Es)
        rownames(result) = c('Sd','VaR','ES')
        result = cbind(rowSums(result), result)
        colnames(result)[1] = 'Total'
        result = result[risk,]
      }
      
      if(isPrint){
        if(is.null(digits)){
          if(decomp == 'FPCR'){
            digits = 1
          }else{
            digits = 3
          }
        }
        result = round(result, digits)
        
        if(type=="normal"){
          Type = 'Parametric Normal'
        }else{
          Type = 'Non-Parametric'
        }
        output = list(decomp = result)
        names(output) = paste('Portfolio',decomp, Type, sep = ' ')
        
      }
      if(isPlot & !mul.port){
#         result = rev(result)
#         result.mat = matrix(result, ncol =1)
#         rownames(result.mat) = names(result)
#         print(barchart(result.mat, groups = FALSE, main = list(paste(decomp,"of", risk, switch(mul.port, "1" = paste("for port", X), "")), cex = axis.cex),layout = layout,
#                        scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),strip=F,ylab = '', xlab = '', as.table = TRUE))
#       
        # single portfolio with multiple risks
        if(class(result) == "matrix")
        {
          result = output[[1]]
          result.mat = result[,-1]
          newdata = melt((result.mat), id.vars = as.factor(rownames(result.mat)))
          print(barchart(value~Var1|Var2, data = newdata,stack = TRUE, origin =0,main = list(paste("Portfolio", decomp, "Comparison" ), cex = axis.cex),layout = layout,
                         scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black",font=2, cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
          
          print(barchart(value~Var2|Var1, data = newdata,stack = TRUE, origin =0, main = list(paste("Portfolio", decomp, "Comparison V2" ), cex = axis.cex),layout = layout,
                         scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex,rot=90)),par.strip.text=list(col="black",font=2, cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
          
        }
        else 
        {
          result = result[-1]
          result.mat = matrix(result, ncol =1)
          rownames(result.mat) = names(result)
          colnames(result.mat) = risk
        print(barchart(result.mat,stack = TRUE,groups = FALSE, main = list(paste("Portfolio", risk, "Decomposition- ",decomp ), cex = axis.cex),layout = layout,
                         horizontal = FALSE, scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black",font=2, cex = stripText.cex),ylab = '', xlab = '', as.table = T))
          
        }
        
      }
      return(output)
    }
    
  }
  if(inherits(object, "list"))
  {
    #check for the lenght of weights
    if(length(weights) != length(object)) stop("Error: Number of portfolios and weights do not match")
    output.list<- lapply(X = 1:length(object), FUN = function(X){riskReport(object[[X]],X,mul.port = TRUE)})
    if(isPlot && portfolio.only)
      {
        if(length(risk)>1){
#         output1 = lapply(X = 1:length(output.list),function(X) output.list[[X]][[1]][i,])
#         result.mat=  matrix(unlist(output1), ncol = length(output1))
#         colnames(result.mat) = unlist(lapply(X=1:length(object), function(X) paste("Portfolio",X)))
#         rownames(result.mat) = names(output1[[1]])
          result = unlist(output.list, recursive = FALSE, use.names = FALSE)
          result.mat = matrix(unlist(result), ncol = length(result))
          rownames(result.mat) = rep(unlist(dimnames(result[[1]])[2]), 1, each = length(risk))
          colnames(result.mat) = unlist(lapply(X=1:length(object), function(X) paste("P",X)))
          risk.type = rep(risk, 0.5*nrow(result.mat))
          #result.mat = cbind(result.mat, Risk = factor(risk.type))
          result.mat = result.mat[-c(1:length(risk)),]
          newdata = melt(t(result.mat), id.vars = as.factor(colnames(result.mat)))
          newdata$risk = factor(rep(risk, ))
          print(barchart(value~Var1|Var2*risk, data = newdata,stack = TRUE, origin =0, main = list(paste("Portfolio Risk Comparison- ", decomp), cex = axis.cex),layout = layout,
                         scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black",font=2, cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
        }
        else{
          result.mat=  matrix(unlist(output.list), ncol = length(output.list))
          colnames(result.mat) = unlist(lapply(X=1:length(object), function(X) paste("Portfolio",X)))
          rownames(result.mat) = names(output.list[[1]][[1]])
        #Remove Total
        result.mat = result.mat[-1,]
        newdata = melt(t(result.mat), id.vars = as.factor(colnames(result.mat)))
        print(barchart(value~Var1|Var2, data = newdata,stack = TRUE, origin =0, main = list(paste("Portfolio Risk Comparison- ",risk,decomp), cex = axis.cex),layout = layout,
                       scales=list(y=list(cex=axis.cex), x=list(cex=axis.cex)),par.strip.text=list(col="black",font=2, cex = stripText.cex),ylab = '', xlab = '', as.table = TRUE))
#         barchart(value~Var1|Var2, data = newdata, stack = TRUE, origin =0, scales = list(y = list(cex = axis.cex)),par.settings = my.settings,
#                  par.strip.text=list(col="black", cex = stripText.cex), group = Var1,auto.key=list(space="right",points=FALSE, rectangles=TRUE,title="", cex.title=stripText.cex))
       }}
  } 
  else
    output.list<- riskReport(object,1, mul.port = FALSE)
  return(output.list)
  
}
