#' Class performance prediction from user-defined scenarios
#' 
#' Class performance prediction from user-defined scenarios
#' 
#' 
#' @param fit.list A list containing lm objects for classes with sufficient
#' history. May not have the same number of classes as in the matrix Betas.
#' @param scenarioData matrix containing factor performance for user-defined
#' scenarios.
#' @param Betas matrix containing factor betas for all classes. May not have
#' the same number of classes as in the list object fit.list.
#' @param zero.alpha logical flag indicating if intercept should be subtracted
#' from predictions.
#' @param userBetas If userBetas=\code{FALSE}, then forecast confidence
#' intervals are included; otherwise, only forecasts are included.
#' @return matrix containing scenario predictions. If userBetas=\code{FALSE},
#' then forecast confidence intervals are included; otherwise, only forecasts
#' are included. No confidence intervals are computed for classes with proxy
#' betas
#' @author Eric Zivot and Yi-An Chen.
#' @examples
#' 
#' data(managers.df)
#' ret.assets = managers.df[,(1:6)]
#' factors    = managers.df[,(7:9)]
#' manager.names = colnames(managers.df[,(1:6)])
#' factor.names  = colnames(managers.df[,(7:9)])
#' w.vec = rep(1/6,6)
#' # fit the factor model with OLS
#' fit <- fitMacroeconomicFactorModel(ret.assets,factors,fit.method="OLS",
#'                                    variable.selection = "all subsets",
#'                                    factor.set = 3)
#' # Compute scenario prediction using our factor data. 
#' fit.list <- fit$asset.fit
#' # make up some scenario for factors
#' scenarioData <- matrix(rnorm(15,sd=0.01),5,3) 
#' colnames(scenarioData)= colnames(factors)
#' rownames(scenarioData)=c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5")
#' pred <- scenarioPredictions(fit.list,scenarioData,fit$beta.mat,zero.alpha = TRUE,userBetas = TRUE)
#' head(pred)
#' 
scenarioPredictions <-
function(fit.list, scenarioData, Betas, zero.alpha = TRUE, userBetas = FALSE) {
## inputs:
##  fit.list        list containing lm objects for classes with sufficient history. May not
##                  have the same number of classes as in the matrix Betas 
##  scenarioData    matrix containing factor performance for user-defined scenarios
##  Betas           matrix containing factor betas for all classes. May not have the same
##                  number of classes as in the list object fit.list
##  zero.alpha      logical flag indicating if intercept should be subtracted from predictions
## outputs:
##                  matrix containing scenario predictions. If userBetas=FALSE, then
##                  forecast confidence intervals are included; otherwise, only forecasts
##                  are included. No confidence intervals are computed for classes with
##                  proxy betas
##
## function to reshape list output
  n.scenarios = nrow(scenarioData)
  tmp.fun = function (x, n.scenarios) matrix(t(x), 1, n.scenarios*3, byrow=TRUE)

## determine classes with proxy betas
  fitIds = names(fit.list)
  classIds = rownames(Betas)
  proxyIds = setdiff(classIds, fitIds)

  if (!userBetas) {
## compute predictions from fitted factor models using lm predict() method
## loop over all classes with sufficient history and compute scenario predictions
    pred.list = list()
    for (i in fitIds) {
      tmp.fit = fit.list[[i]]
## factors used in estimation
      factorIds = names(coef(tmp.fit))[-1]
      tmp.pred = predict(tmp.fit, newdata = data.frame(scenarioData[, factorIds, drop=FALSE]),
                         interval = "prediction")
      if (zero.alpha) {
## subtract intercept (alpha) from prediction results
        tmp.pred = tmp.pred - coef(tmp.fit)[1]
      }
      pred.list[[i]] = tmp.pred
      }
## unlist results into matrix
    pred.fit.mat = t(sapply(pred.list, FUN = tmp.fun, n.scenarios))
    colnames(pred.fit.mat) =  paste(rep(rownames(scenarioData), each=3), c("fit", "lwr", "upr"), sep=".")
## compute predictions for classes with proxy betas. No forecast confidence intervals here.
    pred.proxy.mat = Betas[proxyIds, ] %*% t(scenarioData)
    colnames(pred.proxy.mat) =  paste(rownames(scenarioData), "fit", sep=".")
    pred.mat = matrix(0, length(classIds), ncol(pred.fit.mat))
    colnames(pred.mat) = colnames(pred.fit.mat)
    rownames(pred.mat) = classIds
    pred.mat[fitIds, ] = pred.fit.mat
    pred.mat[proxyIds, colnames(pred.proxy.mat)] = pred.proxy.mat
  } else {
## compute predictions based on user betas: no confidence intervals in this case
      pred.mat = Betas %*% t(scenarioData)
  }
  return(pred.mat)
}

