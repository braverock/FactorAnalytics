
factorModelSimulation <- function(n.sim=5000, factorBetas, factorData, residualMoments,
                                  residual.dist = c("normal","Cornish-Fisher", "skew-t")) {
## Simulate performance for specified funds from fitted factor models
## Simulations are computed using a semi-parametric methodology. Factor performance
## is simulated by bootstrapping with replacement from historical performance, and
## a fitted factor model return conditional on factor performance is computed by
## applying estimated factor model coefficients to the bootstrapped factor performance.
## Residuals are simulated parametrically from one of three distributions: normal
## distribution, Cornish-Fisher distribution, skew-t distribution. Unconditional
## performance is then computed by adding the risidual simulations to the conditional
## performance.
## inputs:
## n.sim            scalar, number of simulations
## factorBetas      n.fund x k matrix of factor model betas for n.fund funds
## factorData       n x k matrix of historical factor performance
## residualMoments  n.fund x 7 vector of residual moments with columns sigma, skew,
##                  ekurt, location, scale, shape, df
## residual.dist    character value indicating the residual distribution. Valid choices
##                  are "normal" for normal distribution; "Cornish-Fisher" for
##                  Cornish-Fisher distribution; "skew-t' for skew-t distribution.
## output:
## n.sim x n.fund matrix of simulated performance.
## Remarks:
## 1. The factor model has the form
## R(t) = beta'F(t) + e(t), e(t) ~ D(0,theta)
## where beta = fitted factor model parameters, F(t) = risk factors, e(t) = residuals,
## and theta = parameters of residual distribution D. If D = normal, then theta =
## sd; if D = Cornish-Fisher, then theta = (sd, skew, kurt)'; if D = skew-t, then
## theta = (location, scale, skew, df)'. Performance is simulated using the following
## algorithm:
## 1. bootstrap B values of F(t) from historical data giving F*(t), t=1,...,B
## 2. Compute conditional performance: R*(t)|F*(t) = beta'F*(t)
## 3. Simulate B values of e(t) from parametric distribution giving e*(t), t=1,...,B
## 4. Compute marginal performance: R*(t) = R*(t)|F*(t) + e*(t)
##
## 2. The following non-base R functions are used:
##    rCornishFisher() from the BaaRiskTools package
##    rst() from the sn package
## References:
## 1. Jiang, Y. (2009). Overcoming Data Challenges in Fund-of-Funds Portfolio Management,
##    PhD Thesis, Department of Statistics, University of Washington.
## 2. Goodworth, T. and C. Jones (2007). "Factor-based, Non-parametric Risk Measurement
##    Framework for Hedge Funds and Fund-of-Funds," The European Journal of Finance.
##
factorData = as.matrix(factorData)
factorBetas = as.matrix(factorBetas)
residual.dist = residual.dist[1]
## bootstrap factor data
bootIdx = sample(nrow(factorData),  n.sim, replace=TRUE)
factorDataBoot = factorData[bootIdx, ]

## construct bootstrapped returns from factor model
fundReturnsBoot = matrix(0, n.sim, nrow(factorBetas))
colnames(fundReturnsBoot) = rownames(factorBetas)
## loop over funds
for (i in 1:nrow(factorBetas)) {
  if (residual.dist == "normal") {
    residualsBoot = as.matrix(rnorm(n.sim, sd=sqrt(residualMoments[i,"sigma"])))
  }
  if (residual.dist == "Cornish-Fisher") {
    residualsBoot = as.matrix(rCornishFisher(n.boot, sigma=(ResidMoments[i,"sigma"]),
                                             skew=ResidMoments[i,"skew"],
                                             ekurt=ResidMoments[i,"ekurt"]))
  }
  if (residual.dist == "skew-t") {
  residualsBoot = as.matrix(rzt(n.boot, location=ResidMoments[i, "location"]
                                scale=ResidMoments[i, "scale"]),
                                shape=ResidMoments[i, "shape"],
                                df=ResidMoments[i, "df"])
  }
  fundReturnsBoot[, i] = ( as.matrix(factorDataBoot[, colnames(factorBetas)]) %*% t(factorBetas[i, ,drop=F])
                          + residualsBoot )
}
return(fundReturnsBoot)
}