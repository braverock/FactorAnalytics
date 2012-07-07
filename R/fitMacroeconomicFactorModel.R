fitMacroeconomicFactorModel <-
function(ret.assets, factors, factor.set = 2, 
                                        fit.method=c("OLS","DLS","Robust"),
                                        variable.selection=c("stepwise", "all subsets", "lar", "lasso"),
                                        decay.factor = 0.95,nvmax=8,force.in=NULL,
                                        subsets.method = c("exhaustive", "backward", "forward", "seqrep"),
                                        lars.criteria = c("Cp","cv")
                                        ) {
## Inputs:
# ret.assets           numerical returns data, univariate or multivariate. data has to be saved as class "data.frame" 
#                      so that lm function can be used and must have colnames.
# factors              numerical factors data.  data has to be saved as class "data.frame" 
#                      so that lm function can be used and must have colnames.
# factor.set           numerical, the numbers of the factors to be included in the model when all subsets 
#                      is chosen.  
# fit.method           chose between "OLS" ordinanary least sqaures, "DLS" discounted least squares using exponential
#                      declining weights, and "Robust" coerced by function lmob in package "robustbase"
# variable.selection   "all" includes all the explanary variables in the model.
#                      "stepwise" is traditional forward/backward stepwise regression, starting from the initial 
#                      set of factors, that adds factors only if the regression fit as measured by the Bayesian 
#                      Information Criteria (BIC) or Akaike Information Criteria (AIC) can be done using the R 
#                      function step() from the stats package. "all subsets" is Traditional all subsets regression 
#                      can be done using the R function regsubsets() from the package leaps. 
#                      only \code(OLS) or \code(DLS) can choose this option.   
#                      "lar" , "lasso" is based on package "lars", linear angle regression.
# decay.factor         for DLS. Default is 0.95.
# nvmax                control option for all subsets. maximum size of subsets to examine
# force.in             control option for all subsets. The factors that should be in all models.
# subsets.method       control option for all subsets. se exhaustive search, forward selection, backward selection 
#                      or sequential replacement to search.
# lars.criteria        either choose minimum "Cp" which is unbiased estimator of the true rist or "cv"
#                      10 folds cross-validation. See detail.    
## Outputs:
## output: list with following components:
## asset.fit           fit objects for each asset. This is the class "lm" for each assets.
## alpha.vec          vector of estimated alphas.
## beta.mat                matrix of estimated betas.
## r2.vec              vector of R-square values
## residVars.vec       vector of residual variances
## call               function call.  

this.call <- match.call()
  
if (is.data.frame(ret.assets) & is.data.frame(factors) ) {
  manager.names = colnames(ret.assets)
  factor.names  = colnames(factors)
  managers.df   = cbind(ret.assets,factors)
} else {
  stop("ret.assets and beta.mat must be in class data.frame")
}
                                          



# initialize list object to hold regression objects
reg.list = list()


# initialize matrices and vectors to hold estimated betas,
# residual variances, and R-square values from
# fitted factor models

Alphas = ResidVars = R2values = rep(0, length(manager.names))
names(Alphas) = names(ResidVars) = names(R2values) = manager.names
Betas = matrix(0, length(manager.names), length(factor.names))
colnames(Betas) = factor.names
rownames(Betas) = manager.names






if (variable.selection == "all subsets") {
# estimate multiple factor model using loop b/c of unequal histories for the hedge funds



if (fit.method == "OLS") {

if (factor.set == length(force.in)) {
  for (i in manager.names) {
 reg.df = na.omit(managers.df[, c(i, force.in)])
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
}  else if (factor.set > length(force.in)) {
    
for (i in manager.names) {
 reg.df = na.omit(managers.df[, c(i, factor.names)])
 fm.formula = as.formula(paste(i,"~", ".", sep=" "))
 fm.subsets <- regsubsets(fm.formula,data=reg.df,nvmax=nvmax,force.in=force.in,
                          method=subsets.method)
 sum.sub <- summary(fm.subsets)
 reg.df <- na.omit(managers.df[,c(i,names(which(sum.sub$which[as.character(factor.set),-1]==TRUE))  )])
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
  stop("ERROR! number of force.in should less or equal to factor.set")
}
  



} else if (fit.method == "DLS"){
  

  if (factor.set == length(force.in)) {  
  # define weight matrix 
for (i in manager.names) {
  reg.df = na.omit(managers.df[, c(i, force.in)])
 t.length <- nrow(reg.df)
 w <- rep(decay.factor^(t.length-1),t.length)
   for (k in 2:t.length) {
    w[k] = w[k-1]/decay.factor 
  }   
# sum weigth to unitary  
 w <- w/sum(w) 
 fm.formula = as.formula(paste(i,"~", ".", sep=""))                              
 fm.fit = lm(fm.formula, data=reg.df,weight=w)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
 } 
} else if  (factor.set > length(force.in)) {
  for (i in manager.names) {
  reg.df = na.omit(managers.df[, c(i, factor.names)])
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
 reg.df <- na.omit(managers.df[,c(i,names(which(sum.sub$which[as.character(factor.set),-1]==TRUE))  )])
 fm.fit = lm(fm.formula, data=reg.df,weight=w)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
 }
} else {
  stop("ERROR! number of force.in should less or equal to factor.set")
}


} else if (fit.method=="Robust") {
  for (i in manager.names) {
 reg.df = na.omit(managers.df[, c(i, factor.names)])
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


} else if (variable.selection == "stepwise") {

  
  if (fit.method == "OLS") {
# loop over all assets and estimate time series regression
for (i in manager.names) {
 reg.df = na.omit(managers.df[, c(i, factor.names)])
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


}  else if (fit.method == "DLS"){
  # define weight matrix 
for (i in manager.names) {
  reg.df = na.omit(managers.df[, c(i, factor.names)])
  t.length <- nrow(reg.df)
  w <- rep(decay.factor^(t.length-1),t.length)
  for (k in 2:t.length) {
    w[k] = w[k-1]/decay.factor 
  }   
# sum weigth to unitary  
 w <- w/sum(w) 
 fm.formula = as.formula(paste(i,"~", ".", sep=""))                              
 fm.fit = step(lm(fm.formula, data=reg.df,weight=w),trace=0)
 fm.summary = summary(fm.fit)
 reg.list[[i]] = fm.fit
 Alphas[i] = coef(fm.fit)[1]
 Betas.names = names(coef(fm.fit)[-1])
 Betas[i,Betas.names] = coef(fm.fit)[-1]
 ResidVars[i] = fm.summary$sigma^2
 R2values[i] =  fm.summary$r.squared
 } 

} else if (fit.method=="Robust") {  
  for (i in manager.names) {
 assign("reg.df" , na.omit(managers.df[, c(i, factor.names)]),envir = .GlobalEnv )
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
  
} else if (variable.selection == "lar" || variable.selection == "lasso") {
  # use min Cp as criteria to choose predictors
  
    for (i in manager.names) {
 reg.df = na.omit(managers.df[, c(i, factor.names)])
 reg.df = as.matrix(reg.df)
 lars.fit = lars(reg.df[,factor.names],reg.df[,i],type=variable.selection,trace=FALSE)
 sum.lars <- summary(lars.fit)
 if (lars.criteria == "Cp") {
 s<- which.min(sum.lars$Cp)
 } else {
 lars.cv <- cv.lars(reg.df[,factor.names],reg.df[,i],trace=FALSE,
                    type=variable.selection,mode="step",plot.it=FALSE)
 s<- which.min(lars.cv$cv)
   }
 coef.lars <- predict(lars.fit,s=s,type="coef",mode="step")
 reg.list[[i]] = lars.fit
 fitted <- predict(lars.fit,reg.df[,factor.names],s=s,type="fit",mode="step")
 Alphas[i] = (fitted$fit - reg.df[,factor.names]%*%coef.lars$coefficients)[1]
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
            alpha.vec = Alphas,
            beta.mat  = Betas,
            r2.vec    = R2values,
            residVars.vec = ResidVars,
            call      = this.call,
            ret.assets = ret.assets,
            factors   = factors,
            variable.selection = variable.selection
            )
class(ans) = "MacroFactorModel"
return(ans)
}

