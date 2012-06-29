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

