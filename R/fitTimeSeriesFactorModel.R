#' Fit time series factor model by time series regression techniques.
#' 
#' @description Fit time series factor model by time series regression techniques. It
#' creates the class of "TimeSeriesFactorModel".
#' 
#' @details add.up.market.returns adds a max(0,Rm-Rf) term in the regression as suggested by 
#' Merton-Henriksson Model (1981) to measure market timing. The coefficient can be interpreted as 
#' number of free put options.
#' 
#' If \code{Robust} is chosen, there is no subsets but all factors will be
#' used.  Cp is defined in
#' http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf. p17.
#' 
#' @param assets.names  names of assets returns.
#' @param factors.names names of factors returns.
#' @param num.factor.subset scalar. Number of factors selected by all subsets.
#' @param data a vector, matrix, data.frame, xts, timeSeries or zoo object with \code{assets.names} 
#' and \code{factors.names} or \code{excess.market.returns.name} if necassary. 
#' @param fit.method "OLS" is ordinary least squares method, "DLS" is
#' discounted least squares method. Discounted least squares (DLS) estimation
#' is weighted least squares estimation with exponentially declining weights
#' that sum to unity. "Robust"
#' @param variable.selection "none" will not activate variables sellection. Default is "none".
#' "stepwise" is traditional forward/backward #' stepwise OLS regression, starting from the initial set of factors, that adds
#' factors only if the regression fit as measured by the Bayesian Information
#' Criteria (BIC) or Akaike Information Criteria (AIC) can be done using the R
#' function step() from the stats package. If "Robust" is chosen, the
#' function step.lmRob in Robust package will be used. "all subsets" is
#' Traditional all subsets regression can be done using the R function
#' regsubsets() from the package leaps. "lar" , "lasso" is based on package
#' "lars", linear angle regression. If "lar" or "lasso" is chose. fit.method will be ignored. 
#' @param decay.factor for DLS. Default is 0.95.
#' @param nvmax control option for all subsets. maximum size of subsets to
#' examine
#' @param force.in control option for all subsets. The factors that should be
#' in all models.
#' @param subsets.method control option for all subsets. se exhaustive search,
#' forward selection, backward selection or sequential replacement to search.
#' @param lars.criteria either choose minimum "cp": unbiased estimator of the
#' true rist or "cv" 10 folds cross-validation. Default is "Cp". See detail.
#' @param add.up.market.returns Logical. If \code{TRUE}, max(0,Rm-Rf) will be added as a regressor.
#'  Default is \code{FALSE}. \code{excess.market.returns.nam} is required if \code{TRUE}. See Detail. 
#' @param add.quadratic.term Logical. If \code{TRUE}, (Rm-Rf)^2 will be added as a regressor. 
#' \code{excess.market.returns.name} is required if \code{TRUE}. Default is \code{FALSE}.
#' @param excess.market.returns.name colnames 
#' market returns minus risk free rate. (Rm-Rf).  
#' @return an S3 object containing
#' \itemize{
#'   \item{asset.fit} {Fit objects for each asset. This is the class "lm" for
#' each object.}
#'   \item{alpha} {N x 1 Vector of estimated alphas.}
#'   \item{beta} {N x K Matrix of estimated betas.}
#'   \item{r2} {N x 1 Vector of R-square values.}
#'   \item{resid.variance} {N x 1 Vector of residual variances.}
#'   \item{call} {function call.}
#' }
#' 
#' 
#' interpreted as number 
#' @author Eric Zivot and Yi-An Chen.
#' @references 
#' \enumerate{
#' \item  Efron, Hastie, Johnstone and Tibshirani (2002) "Least Angle
#' Regression" (with discussion) Annals of Statistics; see also
#' http://www-stat.stanford.edu/~hastie/Papers/LARS/LeastAngle_2002.pdf.  
#' \item Hastie, Tibshirani and Friedman (2008) Elements of Statistical Learning 2nd
#' edition, Springer, NY.
#' \item Christopherson, Carino and Ferson (2009). Portfolio Performance Measurement 
#' and Benchmarking, McGraw Hill.
#' }
#' @examples
#'  \dontrun{
#' # load data from the database
#' data(managers.df)
#' fit <- fitTimeseriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
#'                                 factors.names=c("EDHEC.LS.EQ","SP500.TR"),
#'                                 data=managers.df,fit.method="OLS")
#' # summary of HAM1 
#' summary(fit$asset.fit$HAM1)
#' # plot actual vs. fitted over time for HAM1
#' # use chart.TimeSeries() function from PerformanceAnalytics package
#' dataToPlot = cbind(fitted(fit$asset.fit$HAM1), na.omit(managers.df$HAM1))
#' colnames(dataToPlot) = c("Fitted","Actual")
#' chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
#'                  colorset=c("black","blue"), legend.loc="bottomleft")
#'  }
#'  @export
fitTimeSeriesFactorModel <-
function(assets.names, factors.names, data=data, num.factor.subset = 1, 
          fit.method=c("OLS","DLS","Robust"),
         variable.selection="none",
          decay.factor = 0.95,nvmax=8,force.in=NULL,
          subsets.method = c("exhaustive", "backward", "forward", "seqrep"),
          lars.criteria = "Cp",add.up.market.returns = FALSE,add.quadratic.term = FALSE,
         excess.market.returns.name ) {
  
  require(PerformanceAnalytics)
  require(leaps)
  require(lars)
  require(robust)
  require(MASS)
  this.call <- match.call()
  
  # convert data into xts and hereafter compute in xts
  data.xts <- checkData(data) 
  reg.xts <- merge(data.xts[,assets.names],data.xts[,factors.names])
  if (add.up.market.returns == TRUE || add.quadratic.term == TRUE ) {
  reg.xts <- merge(reg.xts,data.xts[,excess.market.returns.name])
  }
  # initialize list object to hold regression objects
reg.list = list()


# initialize matrices and vectors to hold estimated betas,
# residual variances, and R-square values from
# fitted factor models

Alphas = ResidVars = R2values = rep(NA, length(assets.names))
names(Alphas) = names(ResidVars) = names(R2values) = assets.names
Betas = matrix(NA, length(assets.names), length(factors.names))
colnames(Betas) = factors.names
rownames(Betas) = assets.names

if(add.up.market.returns == TRUE ) {
  Betas <- cbind(Betas,rep(NA,length(assets.names)))
  colnames(Betas)[dim(Betas)[2]] <- "up.beta" 
}
  
if(add.quadratic.term == TRUE ) {
    Betas <- cbind(Betas,rep(NA,length(assets.names)))
    colnames(Betas)[dim(Betas)[2]] <- "quadratic.term" 
}
  
#
### plain vanila method
#   
if (variable.selection == "none") {
  if (fit.method == "OLS") {
          for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])  
        if(add.up.market.returns == TRUE) {
        up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
        reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        fm.formula = as.formula(paste(i,"~", ".", sep=" "))
        fm.fit = lm(fm.formula, data=reg.df)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas.names = names(coef(fm.fit)[-1])
        Betas[i,Betas.names] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }
  } else if (fit.method == "DLS") {
    for (i in assets.names) {
      reg.df = na.omit(reg.xts[, c(i, factors.names)])
      if(add.up.market.returns == TRUE) {
        up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
        reg.df = merge(reg.df,up.beta)
      }
      if(add.quadratic.term == TRUE) {
        quadratic.term <- reg.xts[,excess.market.returns.name]^2
        reg.df = merge(reg.df,quadratic.term)
        colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
      }
      t.length <- nrow(reg.df)
      w <- rep(decay.factor^(t.length-1),t.length)
      for (k in 2:t.length) {
        w[k] = w[k-1]/decay.factor 
      }   
      # sum weigth to unitary  
      w <- w/sum(w) 
      fm.formula = as.formula(paste(i,"~", ".", sep=""))                              
      fm.fit = lm(fm.formula, data=reg.df,weights=w)
      fm.summary = summary(fm.fit)
      reg.list[[i]] = fm.fit
      Alphas[i] = coef(fm.fit)[1]
      Betas.names = names(coef(fm.fit)[-1])
      Betas[i,Betas.names] = coef(fm.fit)[-1]
      ResidVars[i] = fm.summary$sigma^2
      R2values[i] =  fm.summary$r.squared
    } 
  } else if (fit.method=="Robust") {
    for (i in assets.names) {
      reg.df = na.omit(reg.xts[, c(i, factors.names)])
      if(add.up.market.returns == TRUE) {
        up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
        reg.df = merge(reg.df,up.beta)
      }
      if(add.quadratic.term == TRUE) {
        quadratic.term <- reg.xts[,excess.market.returns.name]^2
        reg.df = merge(reg.df,quadratic.term)
        colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
      }
      fm.formula = as.formula(paste(i,"~", ".", sep=" "))
      fm.fit = lmRob(fm.formula, data=reg.df)
      fm.summary = summary(fm.fit)
      reg.list[[i]] = fm.fit
      Alphas[i] = coef(fm.fit)[1]
      Betas[i, ] = coef(fm.fit)[-1]
      ResidVars[i] = fm.summary$sigma^2
      R2values[i] =  fm.summary$r.squared
    }
    
  }  else {
    stop("invalid method")
  }
  
#
### subset methods
#  
} 
  else if (variable.selection == "all subsets") {
# estimate multiple factor model using loop b/c of unequal histories for the hedge funds

if (fit.method == "OLS") {

if (num.factor.subset == length(force.in)) {
  for (i in assets.names) {
 reg.df = na.omit(reg.xts[, c(i, force.in)])
 if(add.up.market.returns == TRUE) {
   up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
   reg.df = merge(reg.df,up.beta)
 }
 if(add.quadratic.term == TRUE) {
   quadratic.term <- reg.xts[,excess.market.returns.name]^2
   reg.df = merge(reg.df,quadratic.term)
   colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
 }
 fm.formula = as.formula(paste(i,"~", ".", sep=" "))
 fm.fit = lm(fm.formula, data=reg.df)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
  }
}  else if (num.factor.subset > length(force.in)) {
    
for (i in assets.names) {
 reg.df = na.omit(reg.xts[, c(i, factors.names)])
 fm.formula = as.formula(paste(i,"~", ".", sep=" "))
 fm.subsets <- regsubsets(fm.formula,data=reg.df,nvmax=nvmax,force.in=force.in,
                          method=subsets.method)
 sum.sub <- summary(fm.subsets)
 reg.df <- na.omit(reg.xts[,c(i,names(which(sum.sub$which[as.character(num.factor.subset),-1]==TRUE))  )])
 if(add.up.market.returns == TRUE) {
   up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
   reg.df = merge(reg.df,up.beta)
 }
 if(add.quadratic.term == TRUE) {
   quadratic.term <- reg.xts[,excess.market.returns.name]^2
   reg.df = merge(reg.df,quadratic.term)
   colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
 }
 fm.fit = lm(fm.formula, data=reg.df)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
  }
} else {
  stop("ERROR! number of force.in should less or equal to num.factor.subset")
}
  



} 
else if (fit.method == "DLS"){
  

  if (num.factor.subset == length(force.in)) {  
  # define weight matrix 
for (i in assets.names) {
  reg.df = na.omit(reg.xts[, c(i, force.in)])
  if(add.up.market.returns == TRUE) {
    up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
    reg.df = merge(reg.df,up.beta)
  }
  if(add.quadratic.term == TRUE) {
    quadratic.term <- reg.xts[,excess.market.returns.name]^2
    reg.df = merge(reg.df,quadratic.term)
    colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
  }
 t.length <- nrow(reg.df)
 w <- rep(decay.factor^(t.length-1),t.length)
   for (k in 2:t.length) {
    w[k] = w[k-1]/decay.factor 
  }   
# sum weigth to unitary  
 w <- w/sum(w) 
 fm.formula = as.formula(paste(i,"~", ".", sep=""))                              
 fm.fit = lm(fm.formula, data=reg.df,weights=w)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
 } 
} else if  (num.factor.subset > length(force.in)) {
  for (i in assets.names) {
  reg.df = na.omit(reg.xts[, c(i, factors.names)])
  t.length <- nrow(reg.df)
  w <- rep(decay.factor^(t.length-1),t.length)
  for (k in 2:t.length) {
  w[k] = w[k-1]/decay.factor 
  }   
  w <- w/sum(w) 
 fm.formula = as.formula(paste(i,"~", ".", sep=""))                              
 fm.subsets <- regsubsets(fm.formula,data=reg.df,nvmax=nvmax,force.in=force.in,
                          method=subsets.method,weights=w) # w is called from global envio
 sum.sub <- summary(fm.subsets)
 reg.df <- na.omit(reg.xts[,c(i,names(which(sum.sub$which[as.character(num.factor.subset),-1]==TRUE))  )])
  if(add.up.market.returns == TRUE) {
    up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
    reg.df = merge(reg.df,up.beta)
  }
  if(add.quadratic.term == TRUE) {
    quadratic.term <- reg.xts[,excess.market.returns.name]^2
    reg.df = merge(reg.df,quadratic.term)
    colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
  }
 fm.fit = lm(fm.formula, data=reg.df,weights=w)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
 }
} else {
  stop("ERROR! number of force.in should less or equal to num.factor.subset")
}


} 
else if (fit.method=="Robust") {
  for (i in assets.names) {
    reg.df = na.omit(reg.xts[, c(i, factors.names)])
    if(add.up.market.returns == TRUE) {
      up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
      reg.df = merge(reg.df,up.beta)
    }
    if(add.quadratic.term == TRUE) {
      quadratic.term <- reg.xts[,excess.market.returns.name]^2
      reg.df = merge(reg.df,quadratic.term)
      colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
    }
    fm.formula = as.formula(paste(i,"~", ".", sep=" "))
    fm.fit = lmRob(fm.formula, data=reg.df)
    fm.summary = summary(fm.fit)
    reg.list[[i]] = fm.fit
    Alphas[i] = coef(fm.fit)[1]
    Betas[i, ] = coef(fm.fit)[-1]
    ResidVars[i] = fm.summary$sigma^2
    R2values[i] =  fm.summary$r.squared
   }

}  else {
  stop("invalid method")
}


} 
  else if (variable.selection == "stepwise") {

  if (fit.method == "OLS") {
# loop over all assets and estimate time series regression
for (i in assets.names) {
 reg.df = na.omit(reg.xts[, c(i, factors.names)])
 if(add.up.market.returns == TRUE) {
   up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
   reg.df = merge(reg.df,up.beta)
 }
 if(add.quadratic.term == TRUE) {
   quadratic.term <- reg.xts[,excess.market.returns.name]^2
   reg.df = merge(reg.df,quadratic.term)
   colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
 }
 fm.formula = as.formula(paste(i,"~", ".", sep=" "))
 fm.fit = step(lm(fm.formula, data=reg.df),trace=0)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
  }


}  
  else if (fit.method == "DLS"){
  # define weight matrix 
for (i in assets.names) {
  reg.df = na.omit(reg.xts[, c(i, factors.names)])
  if(add.up.market.returns == TRUE) {
    up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
    reg.df = merge(reg.df,up.beta)
  }
  if(add.quadratic.term == TRUE) {
    quadratic.term <- reg.xts[,excess.market.returns.name]^2
    reg.df = merge(reg.df,quadratic.term)
    colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
  }
  t.length <- nrow(reg.df)
  w <- rep(decay.factor^(t.length-1),t.length)
  for (k in 2:t.length) {
    w[k] = w[k-1]/decay.factor 
  }   
# sum weigth to unitary  
 w <- w/sum(w) 
 fm.formula = as.formula(paste(i,"~", ".", sep=""))                              
 fm.fit = step(lm(fm.formula, data=reg.df,weights=w),trace=0)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
 } 

} 
  else if (fit.method =="Robust") {  
    for (i in assets.names) {
   assign("reg.df" , na.omit(reg.xts[, c(i, factors.names)]),envir = .GlobalEnv )
#       reg.df = na.omit(reg.xts[, c(i, factors.names)],envir = .GlobalEnv)
 if(add.up.market.returns == TRUE) {
   stop("This function does not support add.up.market.returns and stepwise variable.selection
        together Please choose either one.")
   up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
   reg.df = merge(reg.df,up.beta)
 }
 if(add.quadratic.term == TRUE) {
   stop("This function does not support add.up.market.returns and stepwise variable.selection
        together. Please choose either one.")
   quadratic.term <- reg.xts[,excess.market.returns.name]^2
   reg.df = merge(reg.df,quadratic.term)
   colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
 }
 fm.formula = as.formula(paste(i,"~", ".", sep=" "))
 lmRob.obj <- lmRob(fm.formula, data=reg.df)
 fm.fit = step.lmRob(lmRob.obj,trace=FALSE)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
  }

}
  
} else if (variable.selection == "lar" | variable.selection == "lasso") {
  # use min Cp as criteria to choose predictors
  
  for (i in assets.names) {
 reg.df = na.omit(reg.xts[, c(i, factors.names)])
 if(add.up.market.returns == TRUE) {
   up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
   reg.df = merge(reg.df,up.beta)
 }
 if(add.quadratic.term == TRUE) {
   quadratic.term <- reg.xts[,excess.market.returns.name]^2
   reg.df = merge(reg.df,quadratic.term)
   colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
 }
 reg.df = as.matrix(na.omit(reg.df))
 lars.fit = lars(reg.df[,factors.names],reg.df[,i],type=variable.selection,trace=FALSE)
 sum.lars <- summary(lars.fit)
 if (lars.criteria == "cp") {
 s<- which.min(sum.lars$Cp)
 } else {
 lars.cv <- cv.lars(reg.df[,factors.names],reg.df[,i],trace=FALSE,
                    type=variable.selection,mode="step",plot.it=FALSE)
 s<- which.min(lars.cv$cv)
   }
 coef.lars <- predict(lars.fit,s=s,type="coef",mode="step")
 reg.list[[i]] = lars.fit
 fitted <- predict(lars.fit,reg.df[,factors.names],s=s,type="fit",mode="step")
 Alphas[i] = (fitted$fit - reg.df[,factors.names]%*%coef.lars$coefficients)[1]
 Betas.names = names(coef.lars$coefficients)
 Betas[i,Betas.names] = coef.lars$coefficients
 ResidVars[i] = sum.lars$Rss[s]/(nrow(reg.df)-s)
 R2values[i] =  lars.fit$R2[s]
  } 
 
  }  else  {
  stop("wrong method")
}
  

  
  
  
  # return results
# add option to return list
ans = list (asset.fit = reg.list,
            alpha = Alphas,
            beta  = Betas,
            r2    = R2values,
            resid.variance = ResidVars,
            call      = this.call,
            data = data,
            factors.names = factors.names,
            variable.selection = variable.selection,
            assets.names = assets.names)
class(ans) = "TimeSeriesFactorModel"
return(ans)
}

