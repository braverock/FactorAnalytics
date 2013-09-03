# performance attribution
# Yi-An Chen
# July 30, 2012



#' Compute BARRA-type performance attribution
#' 
#' Decompose total returns or active returns into returns attributed to factors
#' and specific returns. Class of FM.attribution is generated and generic
#' function \code{plot()} and \code{summary()},\code{print()} can be used.
#' 
#' total returns can be decomposed into returns attributed to factors and
#' specific returns. \eqn{R_t = \sum_j b_{jt} * f_{jt} +
#' u_t},t=1..T,\eqn{b_{jt}} is exposure to factor j and \eqn{f_{jt}} is factor
#' j. The returns attributed to factor j is \eqn{b_{jt} * f_{jt}} and portfolio
#' specific returns is \eqn{u_t}
#' 
#' @param fit Class of "TimeSeriesFactorModel", "FundamentalFactorModel" or
#' "statFactorModel".
#' @param benchmark a xts, vector or data.frame provides benchmark time series
#' returns.
#' @param ...  Other controled variables for fit methods.
#' @return an object of class \code{FM.attribution} containing
#' \itemize{
#'   \item{cum.ret.attr.f} N X J matrix of cumulative return attributed to
#' factors.
#'   \item{cum.spec.ret} 1 x N vector of cumulative specific returns.
#'   \item{attr.list} list of time series of attributed returns for every
#' portfolio.
#' }
#' @author Yi-An Chen.
#' @references Grinold,R and Kahn R, \emph{Active Portfolio Management},
#' McGraw-Hill.
#' @export
#' @examples
#' 
#' 
#' data(managers.df)
#' fit.ts <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                      factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                       data=managers.df,fit.method="OLS")
#' # withoud benchmark
#' fm.attr <- factorModelPerformanceAttribution(fit.ts)
#' 
#' 
#' 
factorModelPerformanceAttribution <- 
  function(fit,benchmark=NULL,...) {
   
    # input
    # fit      :   Class of MacroFactorModel, FundamentalFactorModel and statFactorModel
    # benchmark: benchmark returns, default is NULL. If benchmark is provided, active returns 
    #            is used.
    # ...      : controlled variables for fitMacroeconomicsFactorModel and fitStatisticalFactorModel 
    # output
    # class of "FMattribution" 
    #    
    # plot.FMattribution     
    # summary.FMattribution    
    # print.FMattribution    
    require(xts)
   
    if (class(fit) !="TimeSeriesFactorModel" & class(fit) !="FundamentalFactorModel" 
        & class(fit) != "StatFactorModel")
    {
      stop("Class has to be either 'TimeSeriesFactorModel', 'FundamentalFactorModel' or
           'StatFactorModel'.")
    }
    
  # TimeSeriesFactorModel chunk  
    
  if (class(fit) == "TimeSeriesFactorModel")  {
    
  # if benchmark is provided
  
#     if (!is.null(benchmark)) {
#     ret.assets =  fit$data[] - benchmark
#     fit = fitTimeSeriesFactorModel(ret.assets=ret.assets,...)
#     }

    # return attributed to factors
    cum.attr.ret <- fit$beta
    cum.spec.ret <- fit$alpha
    factorName = colnames(fit$beta)
    fundName = rownames(fit$beta)
    
    attr.list <- list()
    
    for (k in fundName) {
      fit.lm = fit$asset.fit[[k]]
      
      ## extract information from lm object
      data <- checkData(fit$data)
      date <- index(na.omit(data[,k])) 
      actual.xts = xts(fit.lm$model[1], as.Date(date))
      
      
      # attributed returns
      # active portfolio management p.512 17A.9 
      
      cum.ret <-   Return.cumulative(actual.xts)
      # setup initial value
      attr.ret.xts.all <- xts(, as.Date(date))
      for ( i in factorName ) {
        
        if (is.na(fit$beta[k,i])) {
          cum.attr.ret[k,i] <- NA
          attr.ret.xts.all <- merge(attr.ret.xts.all,xts(rep(NA,length(date)),as.Date(date)))  
        } else {
          attr.ret.xts <- actual.xts - xts(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]),
                                           as.Date(date))  
          cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.xts-attr.ret.xts)  
          attr.ret.xts.all <- merge(attr.ret.xts.all,attr.ret.xts)
        }
      }
    
      
      # specific returns    
      spec.ret.xts <- actual.xts - xts(as.matrix(fit.lm$model[,-1])%*%as.matrix(fit.lm$coef[-1]),
                                       as.Date(date))
      cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.xts-spec.ret.xts)
      attr.list[[k]] <- merge(attr.ret.xts.all,spec.ret.xts)
      colnames(attr.list[[k]]) <- c(factorName,"specific.returns")
    }

    
  }    
    
if (class(fit) =="FundamentalFactorModel" ) {
  # if benchmark is provided
  
  if (!is.null(benchmark)) {
   stop("use fitFundamentalFactorModel instead")
  }
  # return attributed to factors
  factor.returns <- fit$factor.returns[,-1]
  factor.names <- colnames(fit$beta)
  dates <- index(factor.returns)
  ticker <- fit$asset.names


    
  #cumulative return attributed to factors
  if (factor.names[1] == "(Intercept)") {
  cum.attr.ret <- matrix(,nrow=length(ticker),ncol=length(factor.names),
                         dimnames=list(ticker,factor.names))[,-1] # discard intercept
  } else {
    cum.attr.ret <- matrix(,nrow=length(ticker),ncol=length(factor.names),
                           dimnames=list(ticker,factor.names))
  }
  cum.spec.ret <- rep(0,length(ticker))
  names(cum.spec.ret) <- ticker
 
  # make list of every asstes and every list contains return attributed to factors 
  # and specific returns
   
  attr.list <- list() 
  for (k in ticker) {
    idx <- which(fit$data[,fit$assetvar]== k)
    returns <- fit$data[idx,fit$returnsvar]
    num.f.names <- intersect(fit$exposure.names,factor.names) 
    # check if there is industry factors
    if (length(setdiff(fit$exposure.names,factor.names))>0 ){
    ind.f <-  matrix(rep(fit$beta[k,][-(1:length(num.f.names))],length(idx)),nrow=length(idx),byrow=TRUE)
    colnames(ind.f) <- colnames(fit$beta)[-(1:length(num.f.names))]
    exposure <- cbind(fit$data[idx,num.f.names],ind.f) 
    } else {exposure <- fit$data[idx,num.f.names] }
    
    attr.factor <- exposure * coredata(factor.returns)
    specific.returns <- returns - apply(attr.factor,1,sum)
    attr <- cbind(attr.factor,specific.returns)
    attr.list[[k]] <- xts(attr,as.Date(dates))
    cum.attr.ret[k,] <- apply(attr.factor,2,Return.cumulative)
    cum.spec.ret[k] <- Return.cumulative(specific.returns)
  }
  
  

}
    
    if (class(fit) == "StatFactorModel") {
      
      # if benchmark is provided
      
      if (!is.null(benchmark)) {
        x =  fit$asset.ret - benchmark
        fit = fitStatisticalFactorModel(data=x,...)
      }
      # return attributed to factors
      cum.attr.ret <- t(fit$loadings)
      cum.spec.ret <- fit$r2
      factorName = rownames(fit$loadings)
      fundName = colnames(fit$loadings)
      
      # create list for attribution
      attr.list <- list()
      # pca method
          
      if ( dim(fit$asset.ret)[1] > dim(fit$asset.ret)[2] ) {
        
      
      for (k in fundName) {
        fit.lm = fit$asset.fit[[k]]
        
        ## extract information from lm object
        date <- index(fit$data[,k])
        # probably needs more general Date setting
        actual.xts = xts(fit.lm$model[1], as.Date(date))
        
        
        # attributed returns
        # active portfolio management p.512 17A.9 
        
        cum.ret <-   Return.cumulative(actual.xts)
        # setup initial value
        attr.ret.xts.all <- xts(, as.Date(date))
        for ( i in factorName ) {
                
            attr.ret.xts <- actual.xts - xts(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]),
                                         as.Date(date))  
            cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.xts-attr.ret.xts)  
            attr.ret.xts.all <- merge(attr.ret.xts.all,attr.ret.xts)
        
          
        }
        
        # specific returns    
        spec.ret.xts <- actual.xts - xts(as.matrix(fit.lm$model[,-1])%*%as.matrix(fit.lm$coef[-1]),
                                     as.Date(date))
        cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.xts-spec.ret.xts)
        attr.list[[k]] <- merge(attr.ret.xts.all,spec.ret.xts)
        colnames(attr.list[[k]]) <- c(factorName,"specific.returns")
      }
      } else {
      # apca method
#         fit$loadings # f X K
#         fit$factors  # T X f
        
        dates <- index(fit$factors)
        for ( k in fundName) {
          attr.ret.xts.all <- xts(, as.Date(dates))
          actual.xts <- xts(fit$asset.ret[,k],as.Date(dates))
          cum.ret <-   Return.cumulative(actual.xts)
          for (i in factorName) {
       attr.ret.xts <- xts(fit$factors[,i] * fit$loadings[i,k], as.Date(dates) )
       attr.ret.xts.all <- merge(attr.ret.xts.all,attr.ret.xts)
       cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.xts-attr.ret.xts)
        }
         spec.ret.xts <- actual.xts - xts(fit$factors%*%fit$loadings[,k],as.Date(dates))
          cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.xts-spec.ret.xts)
        attr.list[[k]] <- merge(attr.ret.xts.all,spec.ret.xts)
          colnames(attr.list[[k]]) <- c(factorName,"specific.returns")  
        }
                
        
        } 
          
    }
    
    
    
    ans = list(cum.ret.attr.f=cum.attr.ret,
               cum.spec.ret=cum.spec.ret,
               attr.list=attr.list)
class(ans) = "FM.attribution"      
return(ans)
    }
