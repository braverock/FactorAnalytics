
  #fpath <- system.file("tinytest","timeSeriesReturns.csv", 
  #                     package="FactorAnalytics")
  returns.z <- zoo::read.zoo(file="timeSeriesReturns.csv", header=TRUE, sep=",", 
                        as.is=TRUE, FUN=zoo::as.yearmon)
  returns.z <- window(returns.z, start="2008-01-01", end="2012-12-31")  
  assets <- names(returns.z)[1:30]
  ex.rets <- returns.z[,assets]-returns.z$rf
  carhart <- returns.z[,c("mktrf","smb","hml","umd")]  
  
  # fit Carhart 4-factor model using lm
  ff4 <- lm(ex.rets ~ carhart)
  sum4 = summary(ff4)
  rsq4 <- as.numeric(sapply(X = sum4, FUN = "[", "r.squared"))
  Sigma.F <- var(carhart)
  beta.hat <- coef(ff4)[-1,]
  Sigma.eps <- diag(as.numeric(sapply(X = sum4, FUN = "[", "sigma")))
  Sigma.R <- t(beta.hat) %*% Sigma.F %*% beta.hat + Sigma.eps^2
  
  # fit Carhart 4-factor mode via fitTsfm
  ff.mod <- fitTsfm(asset.names=assets, 
                    factor.names=c("mktrf","smb","hml","umd"), 
                    data=cbind(ex.rets,carhart))
  
  # compare beta and r2
  expect_equivalent(as.matrix(ff.mod$beta), t(coef(ff4)[-1,]))
  expect_equal(as.numeric(ff.mod$r2), as.numeric(sapply(X=sum4, FUN="[", "r.squared")))
  