## scenarioPredictionsPortfolio.r
## purpose: Portfolio performance prediction from user-defined scenarios
##
## author: Eric Zivot
## created: May 13, 2010
## update history

scenarioPredictionsPortfolio <- function(scenarioData, factorData, Betas, ResidVar,
                                         level = 0.95, zero.alpha = TRUE, userBetas = FALSE) {
## inputs:
##  scenarioData    matrix containing factor performance for user-defined scenarios
##  factorData      dataframe containing historical factor performance over estimation window.
##  Betas           k x 1 matrix containing portfolio factor betas
##  zero.alpha      logical flag indicating if intercept should be subtracted from predictions
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
    pred.var = as.numeric(ResidVar) * (1 + diag(tmp.mat))
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