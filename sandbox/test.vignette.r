

setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/vignettes")



# test example files

data(managers.df)
fit <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                data=managers.df,fit.method="OLS")
factorData= managers.df[,c("EDHEC.LS.EQ","SP500.TR")]  
Beta.mat=fit$beta
residualData=as.matrix(fit$resid.variance,1,6) 
n.boot=1000
 # bootstrap returns data from factor model with residuals sample from normal distribution
 bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="normal",
                                   residualData, Alpha.mat=NULL, boot.method="random",
                                   seed = 123, return.factors = "TRUE", return.residuals = 
                                   "TRUE")
#' # Cornish-Fisher distribution
#' # build different residualData matrix
 residualData <- cbind(c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,2,1,0))
 colnames(residualData) <- c("var","skew","ekurt")
 rownames(residualData) <- colnames(managers.df[,(1:6)])
 bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="Cornish-Fisher",
                                   residualData, Alpha.mat=NULL, boot.method="random",
                                   seed = 123, return.factors = "TRUE", return.residuals =
                                   "TRUE")
#' 
#' 
#' # skew-t distribution
#' # build residualData matrix
 residualData <- cbind(rnorm(6),c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,6,10,100))
 colnames(residualData) <- c("location","scale","shape","df")
 rownames(residualData) <- colnames(managers.df[,(1:6)])
 bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="skew-t",
                                   residualData, Alpha.mat=NULL, boot.method="random",
                                   seed = 123, return.factors = "TRUE", return.residuals =
                                   "TRUE")

data(Stock.df)
 fit.fund <- fitFundamentalFactorModel(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP")
                                      , data=stock,returnsvar = "RETURN",datevar = "DATE",  
                                      assetvar = "TICKER",
                                      wls = TRUE, regression = "classic", 
                                      covariance = "classic", full.resid.cov = FALSE)
#' # If not specify anything, predict() will give fitted value
pred.fund <-  predict(fit.fund)
#' 
#' # generate random data
 testdata <- stock[,c("DATE","TICKER")]
 testdata$BOOK2MARKET <- rnorm(n=42465)
 testdata$LOG.MARKETCAP <- rnorm(n=42465)
pred.fund2 <- predict(fit.fund,testdata,new.assetvar="TICKER",new.datevar="DATE")


data(Stock.df)
#' # there are 447 assets  
 exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
 test.fit <- fitFundamentalFactorModel(data=stock,exposure.names=exposure.names,
                                        datevar = "DATE", returnsvar = "RETURN",
                                        assetvar = "TICKER", wls = TRUE, 
                                        regression = "classic", 
                                        covariance = "classic", full.resid.cov = TRUE, 
                                        robust.scale = TRUE)
 
summary(test.fit) 
print(test.fit)

data(managers.df)
fit.macro <- fitTimeSeriesFactorModel(assets.names=colnames(managers.df[,(1:6)]),
                                       factors.names=c("EDHEC.LS.EQ","SP500.TR"),
                                       data=managers.df,fit.method="OLS")
print(fit.macro)
summary(fit.macro)

data(Stock.df)
# there are 447 assets  
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
 test.fit <- fitFundamentalFactorModel(data=stock,exposure.names=exposure.names,
                                        datevar = "DATE", returnsvar = "RETURN",
                                        assetvar = "TICKER", wls = TRUE, 
                                        regression = "classic", 
                                        covariance = "classic", full.resid.cov = TRUE, 
                                        robust.scale = TRUE)
 
 summary(test.fit)

test.fit$factor.returns


data(stat.fm.data)

