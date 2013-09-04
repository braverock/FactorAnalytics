#' Fit statistical factor model using principle components
#' 
#' Fit statistical factor model using principle components. This function is
#' mainly adapted from S+FinMetric function mfactor.
#' 
#' 
#' @param data a vector, matrix, data.frame, xts, timeSeries or zoo object with asset returns 
#' and factors retunrs names
#' @param k numbers of factors if it is scalar or method of choosing optimal
#' number of factors. "bn" represents Bai and Ng (2002) method and "ck"
#' represents Connor and korajczyk (1993) method. Default is k = 1.
#' @param refine \code{TRUE} By default, the APCA fit will use the
#' Connor-Korajczyk refinement.
#' @param check check if some variables has identical values. Default is FALSE.
#' @param max.k scalar, select the number that maximum number of factors to be
#' considered.
#' @param sig significant level when ck method uses.
#' @param na.rm if allow missing values. Default is FALSE.
#' 
#' 
#' @return
#' \itemize{
#' \item{factors}{T x K the estimated factors.}
#' \item{loadings}{K x N the asset specific factor loadings beta_i.
#' estimated from regress the asset returns on factors.}
#' \item{alpha}{1 x N the estimated intercepts alpha_i}
#' \item{ret.cov}{N x N asset returns sample variance covariance matrix.}
#' \item{r2}{regression r square value from regress the asset returns on
#' factors.}
#' \item{k}{the number of the facotrs.}
#' \item{eigen}{eigenvalues from the sample covariance matrix.}
#' \item{residuals}{T x N matrix of residuals from regression.}
#' \item{asset.ret}{asset returns}
#' \item{asset.fit}{List of regression lm class of individual returns on
#' factors.}
#' \item{resid.variance}{vector of residual variances.}
#' \item{mimic}{N x K matrix of factor mimicking portfolio returns.}
#' }
#' @author Eric Zivot and Yi-An Chen
#' @examples
#' 
#' # load data for fitStatisticalFactorModel.r
#' # data from finmetric berndt.dat and folio.dat
#' 
#' data(stat.fm.data)
#' ##
#' # sfm.dat is for pca
#' # sfm.apca.dat is for apca
#' class(sfm.dat)
#' class(sfm.apca.dat)
#' 
#' # pca
#' args(fitStatisticalFactorModel)
#' sfm.pca.fit <- fitStatisticalFactorModel(sfm.dat,k=2)
#' class(sfm.pca.fit)
#' names(sfm.pca.fit)
#' sfm.pca.fit$factors
#' sfm.pca.fit$loadings
#' sfm.pca.fit$r2
#' sfm.pca.fit$residuals
#' sfm.pca.fit$resid.variance
#' sfm.pca.fit$mimic
#' # apca
#' sfm.apca.fit <- fitStatisticalFactorModel(sfm.apca.dat,k=1)
#' names(sfm.apca.fit)
#' sfm.apca.res <- sfm.apca.fit$residuals
#' sfm.apca.mimic <- sfm.apca.fit$mimic
#' # apca with bai and Ng method
#' sfm.apca.fit.bn <- fitStatisticalFactorModel(sfm.apca.dat,k="bn")
#' class(sfm.apca.fit.bn)
#' names(sfm.apca.fit.bn)
#' sfm.apca.fit.bn$mimic
#' 
#' # apca with ck method
#' sfm.apca.fit.ck <- fitStatisticalFactorModel(sfm.apca.dat,k="ck")
#' class(sfm.apca.fit.ck)
#' names(sfm.apca.fit.ck)
#' sfm.apca.fit.ck$mimic
#' 
#' @export
#' 
fitStatisticalFactorModel <-
function(data, k = 1, refine = TRUE, check = FALSE, max.k = NULL, sig = 0.05, na.rm = FALSE){
	
# load package
require(MASS)  
require(PerformanceAnalytics)



  
 # function of test
 mfactor.test <- function(data, method = "bn", refine = TRUE, check = FALSE, max.k = NULL, sig = 0.05){
  
    if(is.null(max.k)) {
		max.k <- min(10, nrow(data) - 1)
	} 	else if (max.k >= nrow(data)) {
		stop("max.k must be less than the number of observations.")
	}
	if(check) {
		if(mfactor.check(data)) {
			warning("Some variables have identical observations.")
			return(list(factors = NA, loadings = NA, k = NA))
		}
	}
	method <- casefold(method)
	if(method == "bn") {
		ans <- mfactor.bn(data, max.k, refine = refine)
	}
	else if(method == "ck") {
		ans <- mfactor.ck(data, max.k, refine = refine, sig = sig)
	}
	else {
		stop("Invalid choice for optional argument method.")
	}
 return(ans)
    
}

 
# function of ck
mfactor.ck <- function(data, max.k, sig = 0.05, refine = TRUE) {
  
  n <- ncol(data)
  m <- nrow(data)
	idx <- 2 * (1:(m/2))
	#
	f <- mfactor.apca(data, k = 1, refine = refine, check = FALSE)
	f1 <- cbind(1, f$factors)
	B <- backsolve(chol(crossprod(f1)), diag(2))
	eps <- data - f1 %*% crossprod(t(B)) %*% crossprod(f1, data)
	s <- eps^2/(1 - 2/m - 1/n)
	#	
	for(i in 2:max.k) {
		f.old <- f
		s.old <- s
		f <- mfactor.apca(data, k = i, refine = refine, check = FALSE)
		f1 <- cbind(1, f$factors)
		B <- backsolve(chol(crossprod(f1)), diag(i + 1))
		eps <- data - f1 %*% crossprod(t(B)) %*% crossprod(f1, data)
		s <- eps^2/(1 - (i + 1)/m - i/n)
		delta <- rowMeans(s.old[idx - 1,  , drop = FALSE]) - rowMeans(
			s[idx,  , drop = FALSE])
		if(t.test(delta, alternative = "greater")$p.value > sig) {
			return(f.old)
		}
	}
	return(f)
}

# funciton of check  
  mfactor.check <- function(data) {
  temp <- apply(data, 2, range)
  if(any(abs(temp[2,  ] - temp[1,  ]) < .Machine$single.eps)) {
		TRUE
	}
	else {
		FALSE
	}
}

  # function of bn
  mfactor.bn <- function(data, max.k, refine = TRUE) {
  
  # Parameters:
	#         data : T x N return matrix
	#     max.k : maxinum number of factors to be considered
		# Returns:
	#      k : the optimum number of factors
	n <- ncol(data)
	m <- nrow(data)
	s <- vector("list", max.k) 
	for(i in 1:max.k) {
		f <- cbind(1, mfactor.apca(data, k = i, refine = refine, check = 
			FALSE)$factors)
		B <- backsolve(chol(crossprod(f)), diag(i + 1))
		eps <- data - f %*% crossprod(t(B)) %*% crossprod(f, data)
		sigma <- colSums(eps^2)/(m - i - 1)
		s[[i]] <- mean(sigma)
	}
	s <- unlist(s)
	idx <- 1:max.k
	Cp1 <- s[idx] + (idx * s[max.k] * (n + m))/(n * m) * log((n * m)/
		(n + m))
	Cp2 <- s[idx] + (idx * s[max.k] * (n + m))/(n * m) * log(min(n, m))
	if(order(Cp1)[1] != order(Cp2)[1]) {
		warning("Cp1 and Cp2 did not yield same result. The smaller one is used."	)
	}
	k <- min(order(Cp1)[1], order(Cp2)[1])
	f <- mfactor.apca(data, k = k, refine = refine, check = FALSE)
 return(f)  
 }

  
  # function of pca
  mfactor.pca <- function(data, k, check = FALSE, ret.cov = NULL) {
  
  if(check) {
		if(mfactor.check(data)) {
			warning("Some variables have identical observations.")
			return(list(factors = NA, loadings = NA, k = NA))
		}
	}
	n <- ncol(data)
	m <- nrow(data)
	if(is.null(dimnames(data))) {
		dimnames(data) <- list(1:m, paste("V", 1:n, sep = "."))
	}
	data.names <- dimnames(data)[[2]]
  # demean
	xc <- t(t(data) - colMeans(data))
	if(is.null(ret.cov)) {
		ret.cov <- crossprod(xc)/m
	}
	eigen.tmp <- eigen(ret.cov, symmetric = TRUE)
  # compute loadings beta
	B <- t(eigen.tmp$vectors[, 1:k, drop = FALSE])
  # compute estimated factors
	f <- data %*% eigen.tmp$vectors[, 1:k, drop = FALSE]
	tmp <- data - f %*% B
	alpha <- colMeans(tmp)
  # compute residuals
	resid <- t(t(tmp) - alpha)
	r2 <- (1 - colSums(resid^2)/colSums(xc^2))
	ret.cov <- t(B) %*% var(f) %*% B
	diag(ret.cov) <- diag(ret.cov) + colSums(tmp^2)/(m - k - 1)
	dimnames(B) <- list(paste("F", 1:k, sep = "."), data.names)
	dimnames(f) <- list(dimnames(data)[[1]], paste("F", 1:k, sep = "."))
	dimnames(ret.cov) <- list(data.names, data.names)
	names(alpha) <- data.names
  
#   if (ckeckData.method == "xts" | ckeckData.method == "zoo" ) {
    f <- xts(f,index(data.xts))
    resid <- xts(resid,index(data.xts))
#     }
  
  
  # create lm list for plot
  reg.list = list()
#   if (ckeckData.method == "xts" | ckeckData.method == "zoo" ) {
    for (i in data.names) {
      reg.xts = merge(data.xts[,i],f)
      colnames(reg.xts)[1] <- i
      fm.formula = as.formula(paste(i,"~", ".", sep=" "))
      fm.fit = lm(fm.formula, data=reg.xts)
      reg.list[[i]] = fm.fit
    }
#       } else {
#     for (i in data.names) {
#     reg.df = as.data.frame(cbind(data[,i],coredata(f)))
#     colnames(reg.df)[1] <- i
#     fm.formula = as.formula(paste(i,"~", ".", sep=" "))
#     fm.fit = lm(fm.formula, data=reg.df)
#     reg.list[[i]] = fm.fit
#     }
#   }
  
	ans <-  list(factors = f, loadings = B, k = k, alpha = alpha, ret.cov = ret.cov,
	            	r2 = r2, eigen = eigen.tmp$values, residuals=resid, asset.ret = data,
               asset.fit=reg.list)
 
  return(ans)
 
}

  # funciont of apca
  mfactor.apca <- function(data, k, refine = TRUE, check = FALSE, ret.cov = NULL) {
  
  if(check) {
		if(mfactor.check(data)) {
			warning("Some variables have identical observations.")
			return(list(factors = NA, loadings = NA, k = NA))
		}
	}
	n <- ncol(data)
	m <- nrow(data)
	if(is.null(dimnames(data))) {
		dimnames(data) <- list(1:m, paste("V", 1:n, sep = "."))
	}
	data.names <- dimnames(data)[[2]]
	xc <- t(t(data) - colMeans(data))
	if(is.null(ret.cov)) {
		ret.cov <- crossprod(t(xc))/n
	}
	eig.tmp <- eigen(ret.cov, symmetric = TRUE)
	f <- eig.tmp$vectors[, 1:k, drop = FALSE]
	f1 <- cbind(1, f)
	B <- backsolve(chol(crossprod(f1)), diag(k + 1))
	B <- crossprod(t(B)) %*% crossprod(f1, data)
	sigma <- colSums((data - f1 %*% B)^2)/(m - k - 1)
	if(refine) {
		xs <- t(xc)/sqrt(sigma)
		ret.cov <- crossprod(xs)/n
		eig.tmp <- eigen(ret.cov, symmetric = TRUE)
		f <- eig.tmp$vectors[, 1:k, drop = FALSE]
		f1 <- cbind(1, f)
		B <- backsolve(chol(crossprod(f1)), diag(k + 1))
		B <- crossprod(t(B)) %*% crossprod(f1, data)
		sigma <- colSums((data - f1 %*% B)^2)/(m - k - 1)
	}
	alpha <- B[1,  ]
	B <- B[-1,  , drop = FALSE]
	ret.cov <- t(B) %*% var(f) %*% B
	diag(ret.cov) <- diag(ret.cov) + sigma
	dimnames(B) <- list(paste("F", 1:k, sep = "."), data.names)
	dimnames(f) <- list(dimnames(data)[[1]], paste("F", 1:k, sep = "."))
	names(alpha) <- data.names
	resid <- t(t(data) - alpha) - f %*% B
	r2 <- (1 - colSums(resid^2)/colSums(xc^2))
  
#   if (ckeckData.method == "xts" | ckeckData.method == "zoo" ) {
    f <- xts(f,index(data.xts))
    resid <- xts(resid,index(data.xts))
#   }
  
  # create lm list for plot
  reg.list = list()
#   if (ckeckData.method == "xts" | ckeckData.method == "zoo" ) {
    for (i in data.names) {
      reg.xts = merge(data.xts[,i],f)
      colnames(reg.xts)[1] <- i
      fm.formula = as.formula(paste(i,"~", ".", sep=" "))
      fm.fit = lm(fm.formula, data=reg.xts)
      reg.list[[i]] = fm.fit
    }
#   } else {
#     for (i in data.names) {
#       reg.df = as.data.frame(cbind(data[,i],coredata(f)))
#       colnames(reg.df)[1] <- i
#       fm.formula = as.formula(paste(i,"~", ".", sep=" "))
#       fm.fit = lm(fm.formula, data=reg.df)
#       reg.list[[i]] = fm.fit
#     }
#   }
  
  
  ans <- 	list(factors = f, loadings = B, k = k, alpha = alpha, ret.cov = ret.cov,
		           r2 = r2, eigen = eig.tmp$values, residuals=resid,asset.ret = data,
               asset.fit=reg.list)
 return(ans)
}

# check data 
data.xts <- checkData(data,method="xts") 


  call <- match.call()  
  pos <- rownames(coredata(data.xts))
	data.m <- as.matrix(coredata(data.xts))
	if(any(is.na(data.m))) {
		if(na.rm) {
			data.m <- na.omit(data.m)
		}		else {
			stop("Missing values are not allowed if na.rm=F.")
		}
	}
	# use PCA if T > N
	if(ncol(data.m) < nrow(data.m)) {
		if(is.character(k)) {
			stop("k must be the number of factors for PCA.")
		}
		if(k >= ncol(data.m)) {
			stop("Number of factors must be smaller than number of variables."
				)
		}
		ans <- mfactor.pca(data.m, k, check = check)
	}	else if(is.character(k)) {
		ans <- mfactor.test(data.m, k, refine = refine, check = 
			check, max.k = max.k, sig = sig)
	}	else { # use aPCA if T <= N
		if(k >= ncol(data.m)) {
			stop("Number of factors must be smaller than number of variables."
				)
		}
		ans <- mfactor.apca(data.m, k, refine = refine, check = 
			check)
	}
  
  # mimic function
  f <- ans$factors
	
	if(is.data.frame(f)) {
		f <- as.matrix(f)
	}

	if(nrow(data.m) < ncol(data.m)) {
		mimic <- ginv(data.m) %*% f
	}	else {
		mimic <- qr.solve(data.m, f)
	}
	
  mimic <- t(t(mimic)/colSums(mimic))
	dimnames(mimic)[[1]] <- dimnames(data.m)[[2]]
  
  ans$mimic <- mimic
  ans$resid.variance <- apply(ans$residuals,2,var)
  ans$call <- call
  ans$data <- data
  ans$assets.names  <- colnames(data.m)
class(ans) <- "StatFactorModel"
  return(ans)
}

