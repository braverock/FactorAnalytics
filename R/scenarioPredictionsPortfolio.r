scenarioPredictionsPortfolio <-
function(scenarioData, factorData, beta.mat, cov.fm, w.vec,
                                         level = 0.95, zero.alpha = TRUE, userBetas = FALSE) {
## inputs:
##  scenarioData    matrix containing factor performance for user-defined scenarios
##  factorData      dataframe containing historical factor performance over estimation window.
##  beta.mat        n x k matrix of factor betas.
##  cov.fm  	      n x n excess return covariance matrxi based on
##				          estimated factor model
##  w.vec           n x 1 vector of portfolio weights
##  level           probablility level for confidence interval.
##  zero.alpha      logical flag indicating if intercept should be subtracted from predictions
##  userBetas       logical. If userBetas=\code{FALSE}, then forecast confidence intervals are 
##                  included; otherwise, only forecasts are included. 
## outputs:
##                  matrix containing scenario predictions. If userBetas=FALSE, then
##                  forecast confidence intervals are included; otherwise, only forecasts
##                  are included. No confidence intervals are computed for classes with
##                  proxy betas
## comments:
  require(MASS)
  level = level + (1 - level)/2
  q.z = qnorm(level)
  n.scenarios = nrow(scenarioData)
  Betas <- t(crossprod(w.vec, beta.mat))
  var.p = as.numeric(t(w.vec) %*% cov.fm %*% w.vec)
  factorIds = rownames(Betas)
  nonZeroBetaIds = factorIds[which(Betas != 0)]
  pred.fit.mat = t(scenarioData %*% Betas)
  colnames(pred.fit.mat) = paste(rownames(scenarioData), "fit", sep=".")
  rownames(pred.fit.mat) = "portfolio"
  if (!userBetas) {
  ## compute forecast prediction variance
    F.mat = as.matrix(factorData[, nonZeroBetaIds])
  ## use generalized inverse to deal with possibly that rank(F.mat) < k
    tmp.mat = scenarioData[, nonZeroBetaIds] %*% ginv(t(F.mat) %*% F.mat) %*% t(scenarioData[, nonZeroBetaIds])
    pred.var = as.numeric(var.p) * (1 + diag(tmp.mat))
    lwr = pred.fit.mat - q.z*sqrt(pred.var)
    colnames(lwr) = paste(rownames(scenarioData), "lwr", sep=".")
    upr = pred.fit.mat + q.z*sqrt(pred.var)
    colnames(upr) = paste(rownames(scenarioData), "upr", sep=".")
    pred.mat = matrix(0, 1, 3*n.scenarios)
    colnames(pred.mat) =  paste(rep(rownames(scenarioData), each=3), c("fit", "lwr", "upr"), sep=".")
    rownames(pred.mat) = "portfolio"
    pred.mat[, colnames(pred.fit.mat)] = pred.fit.mat
    pred.mat[, colnames(lwr)] = lwr
    pred.mat[, colnames(upr)] = upr
  } else {
    pred.mat = pred.fit.mat
  }
  return(pred.mat)
}

