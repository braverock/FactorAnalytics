#' @title Decompose Risk into individual factor contributions
#' 
#' @description Compute the factor contributions to Sd, VaR and ES of returns based on Euler's theorem, given 
#' the fitted factor model. 
#' 
#' @importFrom xts as.xts  
#' @importFrom zoo as.Date index 
#' @importFrom graphics abline legend lines mtext panel.smooth rug
#' @importFrom stats cor cov2cor density dnorm formula hatvalues lag pnorm printCoefmat 
#' rnorm t.test time quantile residuals cov resid qnorm
#' @importFrom utils stack
#' 
#' @param object fit object of class \code{tsfm}, or \code{ffm}.
#' @param risk one of "Sd" (Standard Deviation) or "VaR" (Value at Risk) or "ES" (Expected Shortfall)
#' @param weights a vector of weights of the assets in the portfolio, names of 
#' the vector should match with asset names. Default is NULL, in which case an 
#' equal weights will be used.
#' @param portDecomp logical. If \code{True} the decomposition of risk is done for the portfolio based on the weights.
#' Else, the decomposition of risk is done for each asset. \code{Default} is \code{TRUE}
#' @param factor.cov optional user specified factor covariance matrix with 
#' named columns; defaults to the sample covariance matrix.
#' @param p tail probability for calculation. Default is 0.05.
#' @param type one of "np" (non-parametric) or "normal" for calculating Es. 
#' Default is "np".
#' @param invert a logical variable to choose if change ES to positive number, default
#' is False 
#' @param use an optional character string giving a method for computing factor
#' covariances in the presence of missing values. This must be (an 
#' abbreviation of) one of the strings "everything", "all.obs", 
#' "complete.obs", "na.or.complete", or "pairwise.complete.obs". Default is 
#' "pairwise.complete.obs".
#' @param ... other optional arguments passed to \code{\link[stats]{quantile}} and 
#' optional arguments passed to \code{\link[stats]{cov}}
#' 
#' @return A list containing 
#' \item{portES}{factor model ES of portfolio returns.}
#' \item{mES}{length-(K + 1) vector of marginal contributions to Es.}
#' \item{cES}{length-(K + 1) vector of component contributions to Es.}
#' \item{pcES}{length-(K + 1) vector of percentage component contributions to Es.}
#' Where, K is the number of factors. 
#' 
#' @author Eric Zivot, Yi-An Chen, Sangeetha Srinivasan, Lingjie Yi and Avinash Acharya
#' 
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the different factor model fitting functions.
#' 
#' \code{\link{portSdDecomp}} for factor model Sd decomposition.
#' \code{\link{portVaRDecomp}} for factor model VaR decomposition.
#' 
#' @examples
#' # Time Series Factor Model
#' data(managers)
#' fit.macro <- factorAnalytics::fitTsfm(asset.names=colnames(managers[,(1:6)]),
#'                      factor.names=colnames(managers[,(7:9)]),
#'                      rf.name=colnames(managers[,10]), data=managers)
#' decompSd <- riskDecomp(fit.macro,risk = "Sd")
#' decompVaR <- riskDecomp(fit.macro,invert = TRUE, risk = "VaR")
#' decompES <- riskDecomp(fit.macro,invert = TRUE, risk = "ES")
#' # get the component contribution
#' 
#' # random weights 
#' wts = runif(6)
#' wts = wts/sum(wts)
#' names(wts) <- colnames(managers)[1:6]
#' portSd.decomp <- riskDecomp(fit.macro, wts, portDecomp = TRUE, risk = "Sd")
#' portVaR.decomp <- riskDecomp(fit.macro, wts, portDecomp = TRUE, risk = "VaR")
#' portES.decomp <- riskDecomp(fit.macro, wts, portDecomp = TRUE, risk = "ES")
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
#'               
#' decompES = riskDecomp(fit.cross, risk = "ES") 
#' #get the factor contributions of risk 
#' portES.decomp = riskDecomp(fit.cross, weights = wtsStocks145GmvLo, risk = "ES", portDecomp = TRUE)  
#' @export

riskDecomp <- function(object, ...){
  # check input object validity
  if (!inherits(object, c("tsfm", "ffm"))) {
    stop("Invalid argument: Object should be of class 'tsfm', or 'ffm'.")
  }
  UseMethod("riskDecomp")
}


#' @rdname riskDecomp
#' @method riskDecomp tsfm
#' @importFrom zoo index 
#' @export

riskDecomp.tsfm <- function(object, risk, weights = NULL, portDecomp = TRUE, p=0.05, type=c("np","normal"), 
                                factor.cov, invert = FALSE, use="pairwise.complete.obs", ...) {

 
  
  # Check risk Type
  if (missing(risk) || !(risk %in% c("Sd","VaR","ES"))) {
    stop("Invalid or Missing arg: risk must be 'Sd' or 'VaR' or 'ES' ")
  }
  
  # set default for type
  type = type[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  # get beta.star: 1 x (K+1)
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- object$asset.names
  
  if(portDecomp)
  {
    # check if there is weight input
    if(is.null(weights)){
      weights = rep(1/n.assets, n.assets)
    }else{
      # check if number of weight parameter matches 
      if(n.assets != length(weights)){
        stop("Invalid argument: incorrect number of weights")
      }
      if(!is.null(names(weights))){
        weights = weights[asset.names]
      }else{
        stop("Invalid argument: names of weights vector should match with asset names")
      }
    } 
    
    # get portfolio beta.star: 1 x (K+1)
    beta.star <- as.matrix(cbind(weights %*% as.matrix(beta), sqrt(sum(weights^2 * object$resid.sd^2))))  
    resid.xts <- as.xts(t(t(residuals(object))/object$resid.sd) %*% weights)
  }
  else
  {
    beta.star <- as.matrix(cbind(beta, object$resid.sd))
    resid.xts <- as.xts(t(t(residuals(object))/object$resid.sd))
  }
  
  colnames(beta.star)[dim(beta.star)[2]] <- "Resid" 
  # factor returns and Resid data
  factors.xts <- object$data[,object$factor.names]
  zoo::index(resid.xts) <- as.Date(zoo::index(resid.xts))
  
  if (type=="normal" || risk == "Sd") {
    # get cov(F): K x K
    #REPLACED DIRECT FACTOR.COV CALCLUATION WITH CHECK FOR MISSING FACTOR.COV
    #     factor <- as.matrix(object$data[, object$factor.names])
    #     factor.cov = cov(factor, use=use, ...)
    if (missing(factor.cov)) {
      factor.cov = cov(as.matrix(factors.xts), use=use, ...)
    } else {
      if (!identical(dim(factor.cov), as.integer(c(ncol(factor), ncol(factor))))) {
        stop("Dimensions of user specified factor covariance matrix are not
             compatible with the number of factors in the fitTsfm object")
      }
      }
    # get cov(F.star): (K+1) x (K+1)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+1)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),"Resid")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"Resid")
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    names(MU) <- c(colnames(factor.cov),"Resid")
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
    }
  
  # initialize lists and matrices
  out<- list()
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  idx.exceed <- list()
  
  switch(risk,
         Sd = 
         {
           # compute factor model sd
           Sd.fm <- sqrt(rowSums(beta.star %*% factor.star.cov * beta.star))
           # compute marginal, component and percentage contributions to sd
           # each of these have dimensions: Nx K+1 (N+1 for a portfolio)
           mSd <- drop((t(factor.star.cov %*% t(beta.star)))/Sd.fm) 
           cSd <- drop(mSd * beta.star) 
           pcSd <- drop(100* cSd/Sd.fm) 
           if(portDecomp) {out <- list(portSd=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)}
           else {out <- list(Sd.fm=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)}
           
         },
         {
            if(portDecomp)
            {
              Risk.fm <- rep(NA, 1)
              mRisk <- rep(NA, 1+K)
              cRisk <- rep(NA, 1+K)
              pcRisk <- rep(NA, 1+K)
              n.exceed <- rep(NA, 1)
              names(mRisk)=names(cRisk)=names(pcRisk) <- colnames(beta.star)
              
              # return data for portfolio
              match = colnames(object$data) %in% asset.names
              R.xts <- object$data[,match]
              R.xts <- R.xts * weights
              R.xts = as.xts(rowSums(R.xts), order.by = zoo::index(R.xts))
              names(R.xts) = 'RETURN'
              
              if (type=="np") { 
                # get F.star data object
                factor.star <- merge(factors.xts, resid.xts)
                colnames(factor.star)[dim(factor.star)[2]] <- "Resid"
                switch(risk, 
                       VaR =
                         {
                           Risk.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                           # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
                           # the constant 2.575 corresponds to a triangular kernel 
                           eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
                           
                           # compute marginal VaR as expected value of factor returns, such that the
                           # asset return was incident in the triangular kernel region peaked at the 
                           # VaR value and bandwidth = epsilon.
                           k.weight <- as.vector(1 - abs(R.xts - Risk.fm) / eps)
                           k.weight[k.weight<0] <- 0
                           mRisk <- colMeans(factor.star*k.weight, na.rm =TRUE)
                           # index of VaR exceedances
                           idx.exceed <- which(R.xts <= Risk.fm)
                           # number of VaR exceedances
                           n.exceed <- length(idx.exceed)
                           
                         },
                       ES = 
                       {
                         VaR.fm <- rep(NA, 1)
                         # get VaR for asset i
                         VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                         # index of VaR exceedances
                         idx.exceed <- which(R.xts <= VaR.fm)
                         # compute ES as expected value of asset return, such that the given asset 
                         # return is less than or equal to its value-at-risk (VaR)
                         Risk.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
                         
                         # compute marginal ES as expected value of factor returns, when the asset's 
                         # return is less than or equal to its value-at-risk (VaR)
                         
                         mRisk <- colMeans(factor.star[idx.exceed,], na.rm =TRUE)
                       }
                )
                
                
              } else if (type=="normal") {
                switch(risk, 
                       VaR = 
                       {
                         # get VaR for asset i
                         Risk.fm <- drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))*qnorm(p))
                         # compute marginal VaR
                         mRisk <- drop(MU + SIGB * qnorm(p)/sd(R.xts, na.rm=TRUE))
                         # index of VaR exceedances
                         idx.exceed <- which(R.xts <= Risk.fm)
                         # number of VaR exceedances
                         n.exceed <- length(idx.exceed)
                         
                       },
                       ES = 
                       {
                         # compute ES
                         Risk.fm <- -drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))
                                        *dnorm(qnorm(p))/(p))
                         # compute marginal ES
                         mRisk <- -drop(MU + SIGB/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
                       }
                )
              }
              
              # correction factor to ensure that sum(cRisk) = asset Risk
              cf <- as.numeric( Risk.fm / sum(mRisk*beta.star), na.rm=TRUE) 
              
              # compute marginal, component and percentage contributions to Risk
              # each of these have dimensions: N x (K+1)
              mRisk <- drop(cf * mRisk)
              cRisk <- drop(mRisk * beta.star)
              pcRisk <- drop(100* cRisk / Risk.fm)
              
              #Since all the Var and ES calulations result in negative values by default, Invert = False will make
              #the values positive.
              if(!invert){
                Risk.fm <- -Risk.fm
                mRisk<- -mRisk
                cRisk<- -cRisk
                
              }
              switch(risk,
                     VaR = {out <- list(portVaR=Risk.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                                        mVaR=mRisk, cVaR=cRisk, pcVaR=pcRisk)},
                     ES =  {out <- list(portES=Risk.fm, mES=mRisk, cES=cRisk, pcES=pcRisk)})
            }
            else
            {
          
              Risk.fm <- rep(NA, N)
              mRisk <- matrix(NA, N, K+1)
              cRisk <- matrix(NA, N, K+1)
              pcRisk <- matrix(NA, N, K+1)
              n.exceed <- rep(NA, N)
              names(n.exceed) = names(Risk.fm) = object$asset.names
              rownames(mRisk)=rownames(cRisk)=rownames(pcRisk)=object$asset.names
              colnames(mRisk)=colnames(cRisk)=colnames(pcRisk)=c(object$factor.names,"Resid")
              
              for (i in object$asset.names) {
                # return data for asset i
                R.xts <- object$data[,i]
                
                if (type=="np") {
                  # get F.star data object
                  factor.star <- merge(factors.xts, resid.xts[,i])
                  colnames(factor.star)[dim(factor.star)[2]] <- "Resid"
                  
                  switch(risk, 
                         VaR = 
                         {
                           # get VaR for asset i
                           Risk.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                           # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
                           # the constant 2.575 corresponds to a triangular kernel 
                           eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
                           
                           # compute marginal VaR as expected value of factor returns, such that the
                           # asset return was incident in the triangular kernel region peaked at the 
                           # VaR value and bandwidth = epsilon.
                           k.weight <- as.vector(1 - abs(R.xts - Risk.fm[i]) / eps)
                           k.weight[k.weight<0] <- 0
                           mRisk[i,] <- colMeans(factor.star*k.weight, na.rm =TRUE)
                           # index of VaR exceedances
                           idx.exceed[[i]] <- which(R.xts <= Risk.fm[i])
                           # number of VaR exceedances
                           n.exceed[i] <- length(idx.exceed[[i]])
                         },
                         ES = 
                         {
                           VaR.fm <- rep(NA, N)
                           names(VaR.fm) = object$asset.names
                           # get VaR for asset i
                           VaR.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                           # index of VaR exceedances
                           idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
                           # compute ES as expected value of asset return, such that the given asset 
                           # return is less than or equal to its value-at-risk (VaR)
                           Risk.fm[i] <- mean(R.xts[idx.exceed[[i]]], na.rm =TRUE)
                           # compute marginal ES as expected value of factor returns, when the asset's 
                           # return is less than or equal to its value-at-risk (VaR)
                           mRisk[i,] <- colMeans(factor.star[idx.exceed[[i]],], na.rm =TRUE)
                         }
                  )
                  
                  
                } else if (type=="normal") {
                  switch(risk, 
                         VaR = 
                         {
                           # get VaR for asset i
                           Risk.fm[i] <- beta.star[i,] %*% MU + 
                             sqrt(beta.star[i,,drop=F] %*% factor.star.cov %*% t(beta.star[i,,drop=F]))*qnorm(p)
                           # compute marginal VaR
                           mRisk[i,] <- t(MU) + SIGB[i,] * qnorm(p)/sd(R.xts, na.rm=TRUE)
                           # index of VaR exceedances
                           idx.exceed[[i]] <- which(R.xts <= Risk.fm[i])
                           # number of VaR exceedances
                           n.exceed[i] <- length(idx.exceed[[i]])
                         },
                         ES = 
                         {
                           # extract vector of factor model loadings for asset i
                           beta.i <- beta.star[i,,drop=F]
                           # compute ES
                           Risk.fm[i] <- -(beta.star[i,] %*% MU + sqrt(beta.i %*% factor.star.cov %*% t(beta.i))*dnorm(qnorm(p))/(p)) 
                           # compute marginal ES
                           mRisk[i,] <- -(t(MU) + SIGB[i,]/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p)) 
                         }
                  )
          
                }
                
                # correction factor to ensure that sum(cES) = asset ES
                cf <- as.numeric( Risk.fm[i] / sum(mRisk[i,]*beta.star[i,], na.rm=TRUE) )
                # compute marginal, component and percentage contributions to ES
                # each of these have dimensions: N x (K+1)
                mRisk[i,] <- cf * mRisk[i,]
                cRisk[i,] <- mRisk[i,] * beta.star[i,]
                pcRisk[i,] <- 100* cRisk[i,] / Risk.fm[i]
              }
              
              #Since all the Var and ES calulations result in negative values by default, Invert = False will make
              #the values positive.
              if(!invert){
                Risk.fm <- -Risk.fm
                mRisk<- -mRisk
                cRisk<- -cRisk
                
              }
            
                switch(risk,
                       VaR = {out <- list(VaR.fm=Risk.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                                                      mVaR=mRisk, cVaR=cRisk, pcVaR=pcRisk)},
                       ES =  {out <- list(ES.fm=Risk.fm, mES=mRisk, cES=cRisk, pcES=pcRisk)})
         }}
  )
            
            return(out)
          }
          
          
          
#' @rdname riskDecomp
#' @method riskDecomp ffm
#' @importFrom zoo index 
#' @export

riskDecomp.ffm <- function(object, risk, weights = NULL, portDecomp =TRUE, factor.cov, p=0.05, type=c("np","normal"), 
                             invert = FALSE, ...){
 
  # Check risk Type
  if (missing(risk) || !(risk %in% c("Sd","VaR","ES"))) {
    stop("Invalid or Missing arg: risk must be 'Sd' or 'VaR' or 'ES' ")
  }
  
  # set default for type
  type = type[1]
  
  if (!(type %in% c("np","normal"))) {
    stop("Invalid args: type must be 'np' or 'normal' ")
  }
  
  # get beta.star: 1 x (K+1)
  beta <- object$beta
  beta[is.na(beta)] <- 0
  n.assets = nrow(beta)
  asset.names <- unique(object$data[[object$asset.var]])
  
  if(portDecomp)
  {
    # check if there is weight input
    if(is.null(weights)){
      weights = rep(1/n.assets, n.assets)
    }else{
      # check if number of weight parameter matches 
      if(n.assets != length(weights)){
        stop("Invalid argument: incorrect number of weights")
      }
      if(!is.null(names(weights))){
        weights = weights[asset.names]
      }else{
        stop("Invalid argument: names of weights vector should match with asset names")
      }
    } 
    
    # get portfolio beta.star: 1 x (K+1)
    beta.star <- as.matrix(cbind(weights %*% beta, sqrt(sum(weights^2 * object$resid.var))))
    resid.xts <- as.xts( t(t(residuals(object))/sqrt(object$resid.var)) %*% weights)
  }
  else
  {
    beta.star <- as.matrix(cbind(beta, sqrt(object$resid.var)))
    resid.xts <- as.xts(t(t(residuals(object))/sqrt(object$resid.var)))
  }
  
  colnames(beta.star)[dim(beta.star)[2]] <- "Resid" 
  # factor returns and residuals data
  factors.xts <- object$factor.returns
  zoo::index(resid.xts) <- as.Date(zoo::index(resid.xts))
  
  if (type=="normal" || risk == "Sd") {
    # get cov(F): K x K
    if (missing(factor.cov)) {
      factor.cov <- object$factor.cov
    } else {
      if (!identical(dim(factor.cov), as.integer(c(ncol(factor), ncol(factor))))) {
        stop("Dimensions of user specified factor covariance matrix are not
             compatible with the number of factors in the fitTsfm object")
      }
      }
    # get cov(F.star): (K+1) x (K+1)
    K <- ncol(object$beta)
    factor.star.cov <- diag(K+1)
    factor.star.cov[1:K, 1:K] <- factor.cov
    colnames(factor.star.cov) <- c(colnames(factor.cov),"Resid")
    rownames(factor.star.cov) <- c(colnames(factor.cov),"Resid")
    
    # factor expected returns
    MU <- c(colMeans(factors.xts, na.rm=TRUE), 0)
    names(MU) <- c(colnames(factor.cov),"Resid")
    
    # SIGMA*Beta to compute normal mVaR
    SIGB <-  beta.star %*% factor.star.cov
    }
  
  # initialize lists and matrices
  out<- list()
  N <- length(object$asset.names)
  K <- length(object$factor.names)
  idx.exceed <- list()
  switch(risk,
         Sd = 
         {
           # compute factor model sd
           Sd.fm <- sqrt(rowSums(beta.star %*% factor.star.cov * beta.star))
           # compute marginal, component and percentage contributions to sd
           # each of these have dimensions: Nx K+1 (N+1 for a portfolio)
           mSd <- drop((t(factor.star.cov %*% t(beta.star)))/Sd.fm) 
           cSd <- drop(mSd * beta.star) 
           pcSd <- drop(100* cSd/Sd.fm) 
           if(portDecomp) {out <- list(portSd=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)}
           else {out <- list(Sd.fm=Sd.fm, mSd=mSd, cSd=cSd, pcSd=pcSd)}
           
         },
         {
            if(portDecomp)
            {
              Risk.fm <- rep(NA, 1)
              mRisk <- rep(NA, 1+K)
              cRisk <- rep(NA, 1+K)
              pcRisk <- rep(NA, 1+K)
              n.exceed <- rep(NA, 1)
              names(mRisk)=names(cRisk)=names(pcRisk) <- colnames(beta.star)
              
              dat = object$data
              # return data for portfolio
              R.xts = tapply(dat[,object$ret.var], list(dat[,object$date.var], dat[,object$asset.var]), FUN = I)
              R.xts <- R.xts * weights
              R.xts = as.xts(rowSums(R.xts), order.by = object$time.periods)
              names(R.xts) = 'RETURN'
              
              if (type=="np") { 
                index(factors.xts) <- index(resid.xts)
                # get F.star data object
                factor.star <- merge(factors.xts, resid.xts)
                colnames(factor.star)[dim(factor.star)[2]] <- "Resid"
                switch(risk, 
                       VaR =
                       {
                         Risk.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                         # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
                         # the constant 2.575 corresponds to a triangular kernel 
                         eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
                         
                         # compute marginal VaR as expected value of factor returns, such that the
                         # asset return was incident in the triangular kernel region peaked at the 
                         # VaR value and bandwidth = epsilon.
                         k.weight <- as.vector(1 - abs(R.xts - Risk.fm) / eps)
                         k.weight[k.weight<0] <- 0
                         mRisk <- colMeans(factor.star*k.weight, na.rm =TRUE)
                         # index of VaR exceedances
                         idx.exceed <- which(R.xts <= Risk.fm)
                         # number of VaR exceedances
                         n.exceed <- length(idx.exceed)
                         
                       },
                       ES = 
                       {
                         VaR.fm <- rep(NA, 1)
                         # get VaR for asset i
                         VaR.fm <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                         # index of VaR exceedances
                         idx.exceed <- which(R.xts <= VaR.fm)
                         # compute ES as expected value of asset return, such that the given asset 
                         # return is less than or equal to its value-at-risk (VaR)
                         Risk.fm <- mean(R.xts[idx.exceed], na.rm =TRUE)
                         
                         # compute marginal ES as expected value of factor returns, when the asset's 
                         # return is less than or equal to its value-at-risk (VaR)
                         
                         mRisk <- colMeans(factor.star[idx.exceed,], na.rm =TRUE)
                       }
                )
                
                
              } else if (type=="normal") {
                switch(risk, 
                       VaR = 
                       {
                         # get VaR for asset i
                         Risk.fm <- drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))*qnorm(p))
                         # compute marginal VaR
                         mRisk <- drop(MU + SIGB * qnorm(p)/sd(R.xts, na.rm=TRUE))
                         # index of VaR exceedances
                         idx.exceed <- which(R.xts <= Risk.fm)
                         # number of VaR exceedances
                         n.exceed <- length(idx.exceed)
                         
                       },
                       ES = 
                       {
                         # compute ES
                         Risk.fm <- -drop(beta.star %*% MU + sqrt(beta.star %*% factor.star.cov %*% t(beta.star))
                                          *dnorm(qnorm(p))/(p))
                         # compute marginal ES
                         mRisk <- -drop(MU + SIGB/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p))
                       }
                )
              }
              
              # correction factor to ensure that sum(cRisk) = asset Risk
              cf <- as.numeric( Risk.fm / sum(mRisk*beta.star), na.rm=TRUE) 
              
              # compute marginal, component and percentage contributions to Risk
              # each of these have dimensions: N x (K+1)
              mRisk <- drop(cf * mRisk)
              cRisk <- drop(mRisk * beta.star)
              pcRisk <- drop(100* cRisk / Risk.fm)
              
              #Since all the Var and ES calulations result in negative values by default, Invert = False will make
              #the values positive.
              if(!invert){
                Risk.fm <- -Risk.fm
                mRisk<- -mRisk
                cRisk<- -cRisk
                
              }
              switch(risk,
                     VaR = {out <- list(portVaR=Risk.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                                        mVaR=mRisk, cVaR=cRisk, pcVaR=pcRisk)},
                     ES =  {out <- list(portES=Risk.fm, mES=mRisk, cES=cRisk, pcES=pcRisk)})
            }
            else
            {
              
              Risk.fm <- rep(NA, N)
              mRisk <- matrix(NA, N, K+1)
              cRisk <- matrix(NA, N, K+1)
              pcRisk <- matrix(NA, N, K+1)
              n.exceed <- rep(NA, N)
              names(n.exceed) = names(Risk.fm) = object$asset.names
              rownames(mRisk)=rownames(cRisk)=rownames(pcRisk)=object$asset.names
              colnames(mRisk)=colnames(cRisk)=colnames(pcRisk)=c(object$factor.names,"Resid")
              
              for (i in object$asset.names) {
                # return data for asset i
                subrows <- which(object$data[[object$asset.var]]==i)
                R.xts <- as.xts(object$data[subrows,object$ret.var], 
                                as.Date(object$data[subrows,object$date.var]))
                
                if (type=="np") {
                  time(factors.xts) <- time(resid.xts[,i])
                  # get F.star data object
                  factor.star <- merge(factors.xts, resid.xts[,i])
                  colnames(factor.star)[dim(factor.star)[2]] <- "Resid"
                  
                  switch(risk, 
                         VaR = 
                         {
                           # get VaR for asset i
                           Risk.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                           # epsilon is apprx. using Silverman's rule of thumb (bandwidth selection)
                           # the constant 2.575 corresponds to a triangular kernel 
                           eps <- 2.575*sd(R.xts, na.rm =TRUE) * (nrow(R.xts)^(-1/5))
                           
                           # compute marginal VaR as expected value of factor returns, such that the
                           # asset return was incident in the triangular kernel region peaked at the 
                           # VaR value and bandwidth = epsilon.
                           k.weight <- as.vector(1 - abs(R.xts - Risk.fm[i]) / eps)
                           k.weight[k.weight<0] <- 0
                           mRisk[i,] <- colMeans(factor.star*k.weight, na.rm =TRUE)
                           # index of VaR exceedances
                           idx.exceed[[i]] <- which(R.xts <= Risk.fm[i])
                           # number of VaR exceedances
                           n.exceed[i] <- length(idx.exceed[[i]])
                         },
                         ES = 
                         {
                           VaR.fm <- rep(NA, N)
                           names(VaR.fm) = object$asset.names
                           # get VaR for asset i
                           VaR.fm[i] <- quantile(R.xts, probs=p, na.rm=TRUE, ...)
                           # index of VaR exceedances
                           idx.exceed[[i]] <- which(R.xts <= VaR.fm[i])
                           # compute ES as expected value of asset return, such that the given asset 
                           # return is less than or equal to its value-at-risk (VaR)
                           Risk.fm[i] <- mean(R.xts[idx.exceed[[i]]], na.rm =TRUE)
                           # compute marginal ES as expected value of factor returns, when the asset's 
                           # return is less than or equal to its value-at-risk (VaR)
                           mRisk[i,] <- colMeans(factor.star[idx.exceed[[i]],], na.rm =TRUE)
                         }
                  )
                  
                  
                } else if (type=="normal") {
                  switch(risk, 
                         VaR = 
                         {
                           # get VaR for asset i
                           Risk.fm[i] <- beta.star[i,] %*% MU + 
                             sqrt(beta.star[i,,drop=F] %*% factor.star.cov %*% t(beta.star[i,,drop=F]))*qnorm(p)
                           # compute marginal VaR
                           mRisk[i,] <- t(MU) + SIGB[i,] * qnorm(p)/sd(R.xts, na.rm=TRUE)
                           # index of VaR exceedances
                           idx.exceed[[i]] <- which(R.xts <= Risk.fm[i])
                           # number of VaR exceedances
                           n.exceed[i] <- length(idx.exceed[[i]])
                         },
                         ES = 
                         {
                           # extract vector of factor model loadings for asset i
                           beta.i <- beta.star[i,,drop=F]
                           # compute ES
                           Risk.fm[i] <- -(beta.star[i,] %*% MU + sqrt(beta.i %*% factor.star.cov %*% t(beta.i))*dnorm(qnorm(p))/(p)) 
                           # compute marginal ES
                           mRisk[i,] <- -(t(MU) + SIGB[i,]/sd(R.xts, na.rm=TRUE) * dnorm(qnorm(p))/(p)) 
                         }
                  )
                  
                }
                
                # correction factor to ensure that sum(cES) = asset ES
                cf <- as.numeric( Risk.fm[i] / sum(mRisk[i,]*beta.star[i,], na.rm=TRUE) )
                # compute marginal, component and percentage contributions to ES
                # each of these have dimensions: N x (K+1)
                mRisk[i,] <- cf * mRisk[i,]
                cRisk[i,] <- mRisk[i,] * beta.star[i,]
                pcRisk[i,] <- 100* cRisk[i,] / Risk.fm[i]
              }
              
              #Since all the Var and ES calulations result in negative values by default, Invert = False will make
              #the values positive.
              if(!invert){
                Risk.fm <- -Risk.fm
                mRisk<- -mRisk
                cRisk<- -cRisk
                
              }
            
        switch(risk,
               VaR = {out <- list(VaR.fm=Risk.fm, n.exceed=n.exceed, idx.exceed=idx.exceed, 
                                  mVaR=mRisk, cVaR=cRisk, pcVaR=pcRisk)},
               ES =  {out <- list(ES.fm=Risk.fm, mES=mRisk, cES=cRisk, pcES=pcRisk)})
            }
         })
  
  return(out)
}