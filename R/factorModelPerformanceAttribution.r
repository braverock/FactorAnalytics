# performance attribution
# Yi-An Chen
# July 30, 2012

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
    require(zoo)
   
    if (class(fit) !="MacroFactorModel" & class(fit) !="FundamentalFactorModel" 
        & class(fit) != "StatFactorModel")
    {
      stop("Class has to be MacroFactorModel.")
    }
    
  # MacroFactorModel chunk  
    
  if (class(fit) == "MacroFactorModel")  {
    
  # if benchmark is provided
  
    if (!is.null(benchmark)) {
    ret.assets =  fit$ret.assets - benchmark
    fit = fitMacroeconomicFactorModel(ret.assets=ret.assets,...)
    }
# return attributed to factors
    cum.attr.ret <- fit$beta.mat
    cum.spec.ret <- fit$alpha.vec
    factorName = colnames(fit$beta.mat)
    fundName = rownames(fit$beta.mat)
    
    attr.list <- list()
    
    for (k in fundName) {
    fit.lm = fit$asset.fit[[k]]
   
    ## extract information from lm object
    date <- names(fitted(fit.lm))
   
    actual.z = zoo(fit.lm$model[1], as.Date(date))
 

# attributed returns
# active portfolio management p.512 17A.9 
    
  cum.ret <-   Return.cumulative(actual.z)
  # setup initial value
  attr.ret.z.all <- zoo(, as.Date(date))
  for ( i in factorName ) {
  
    if (fit$beta.mat[k,i]==0) {
    cum.attr.ret[k,i] <- 0
  attr.ret.z.all <- merge(attr.ret.z.all,zoo(rep(0,length(date)),as.Date(date)))  
  } else {
  attr.ret.z <- actual.z - zoo(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]),
                               as.Date(date))  
  cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.z-attr.ret.z)  
  attr.ret.z.all <- merge(attr.ret.z.all,attr.ret.z)
  }
  
  }
    
  # specific returns    
    spec.ret.z <- actual.z - zoo(as.matrix(fit.lm$model[,-1])%*%as.matrix(fit.lm$coef[-1]),
                                 as.Date(date))
    cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.z-spec.ret.z)
  attr.list[[k]] <- merge(attr.ret.z.all,spec.ret.z)
   colnames(attr.list[[k]]) <- c(factorName,"specific.returns")
    }

    
  }    
    
if (class(fit) =="FundamentalFactorModel" ) {
  # if benchmark is provided
  
  if (!is.null(benchmark)) {
   stop("use fitFundamentalFactorModel instead")
  }
  # return attributed to factors
  factor.returns <- fit$factor.rets[,-1]
  factor.names <- names(fit$factor.rets[,-1])
  dates <- as.character(unique(fit$exposure.data[,"DATE"]))
  exposure <- fund.fit$exposure.data
  ticker <- names(fit$resids)
 
  N <- length(ticker)
  J <- length(factor.names)
  t <- length(dates)
  # array arranges in N X J X t 
  # N is assets names, J is factors, t is time
  attr.ret <- array(,dim=c(N,J,t),dimnames=list(ticker,factor.names,dates))
  for (i in dates) {
    idx = which(exposure[,"DATE"]==i)
    for (j in factor.names) {
  attr.ret[,j,i] <- exposure[idx,j]*coredata(factor.returns[as.Date(i)])[,j]
  }
  }
  
  # specific returns
  # zoo class 
  intercept <- fit$factor.rets[,1]
  resids <- fit$resids
  spec.ret.z <- resids + intercept 
   
  #cumulative return attributed to factors
  cum.attr.ret <- matrix(,nrow=length(ticker),ncol=length(factor.names),
                         dimnames=list(ticker,factor.names))
  cum.spec.ret <- rep(0,length(ticker))
  names(cum.spec.ret) <- ticker
  
  # arrange returns data
  actual <- fund.fit$returns.data
  # N <- length(assets.names)
  # t <- length(dates)
  # array arranges in N X t 
  # N is assets names, J is factors, t is time
  actual.ret <- array(,dim=c(N,t),dimnames=list(ticker,dates))
  for (i in dates) {
    idx = which(actual[,"DATE"]==i)
    actual.ret[,i] <- actual[idx,"RETURN"]  
  }  
  
  # make returns as zoo 
  actual.z.all <- zoo(,as.Date(dates))  
  for (k in ticker) {
    actual.z <- zoo(actual.ret[k,],as.Date(dates))
    actual.z.all <- merge(actual.z.all,actual.z)
  }
  colnames(actual.z.all) <- ticker
  
  
  
  # make list of every asstes and every list contains return attributed to factors 
  # and specific returns 
  attr.list <- list()
    for (k in ticker){
  attr.ret.z.all <- zoo(,as.Date(dates))
  # cumulative returns
  cum.ret <-   Return.cumulative(actual.z.all[,k])
    for (j in factor.names) {
   attr.ret.z <- zoo(attr.ret[k,j,],as.Date(dates) )
   attr.ret.z.all <- merge(attr.ret.z.all,attr.ret.z)
   cum.attr.ret[k,j] <- cum.ret - Return.cumulative(actual.z.all[,k]-attr.ret.z)
   }
  attr.list[[k]] <- merge(attr.ret.z.all,spec.ret.z[,k])
   colnames(attr.list[[k]]) <- c(factor.names,"specific.returns") 
   cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.z.all[,k]-spec.ret.z[,k])
  
    }
  
}
    
    if (class(fit) == "StatFactorModel") {
      
      # if benchmark is provided
      
      if (!is.null(benchmark)) {
        x =  fit$asset.ret - benchmark
        fit = fitStatisticalFactorModel(x=x,...)
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
        date <- names(fitted(fit.lm))
        # probably needs more general Date setting
        actual.z = zoo(fit.lm$model[1], as.Date(date))
        
        
        # attributed returns
        # active portfolio management p.512 17A.9 
        
        cum.ret <-   Return.cumulative(actual.z)
        # setup initial value
        attr.ret.z.all <- zoo(, as.Date(date))
        for ( i in factorName ) {
                
            attr.ret.z <- actual.z - zoo(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]),
                                         as.Date(date))  
            cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.z-attr.ret.z)  
            attr.ret.z.all <- merge(attr.ret.z.all,attr.ret.z)
        
          
        }
        
        # specific returns    
        spec.ret.z <- actual.z - zoo(as.matrix(fit.lm$model[,-1])%*%as.matrix(fit.lm$coef[-1]),
                                     as.Date(date))
        cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.z-spec.ret.z)
        attr.list[[k]] <- merge(attr.ret.z.all,spec.ret.z)
        colnames(attr.list[[k]]) <- c(factorName,"specific.returns")
      }
      } else {
      # apca method
#         fit$loadings # f X K
#         fit$factors  # T X f
        
        dates <- rownames(fit$factors)
        for ( k in fundName) {
          attr.ret.z.all <- zoo(, as.Date(date))
          actual.z <- zoo(fit$asset.ret[,k],as.Date(dates))
          cum.ret <-   Return.cumulative(actual.z)
          for (i in factorName) {
       attr.ret.z <- zoo(fit$factors[,i] * fit$loadings[i,k], as.Date(dates) )
       attr.ret.z.all <- merge(attr.ret.z.all,attr.ret.z)
       cum.attr.ret[k,i] <- cum.ret - Return.cumulative(actual.z-attr.ret.z)
        }
         spec.ret.z <- actual.z - zoo(fit$factors%*%fit$loadings[,k],as.Date(dates))
          cum.spec.ret[k] <- cum.ret - Return.cumulative(actual.z-spec.ret.z)
        attr.list[[k]] <- merge(attr.ret.z.all,spec.ret.z)
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