# test file 
library("roxygen2")
library("PerformanceAnalytics")


#load data from the database
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/data")
load('managers.df.rda')
load('stat.fm.data.RData')
ret.assets = managers.df[,(1:6)]

names(managers.df)
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("fitTSFM.r")
source("fitStatisticalFactorModel.r")
source("impliedFactorReturns.r")
source("factorModelSdDecomposition.r")
source("factorModelEsDecomposition.r")
source("factorModelVaRDecomposition.r")


# load data 
data("managers.df")
colnames(managers.df)

# fit the factor model with OLS
fit.ts <- fitTSFM (asset.names=colnames(managers.df[,(1:6)]),
                   factor.names=c("EDHEC.LS.EQ","SP500.TR"),
                   data=managers.df,fit.method="OLS"), 
                   variable.selection="none")
                                       

names(fit.ts)
class(fit.ts) 
fit.ts$asset.fit
fit.ts$alpha
fit.ts$beta
fit$r2
fit$resid.sd
fit$asset.fit$HAM1
fit$call
fit.ts$factor.names
summary(fit.ts$asset.fit$HAM1)
fit.macro$variable.selection

predict(fit$asset.fit$HAM1,newdata) 
fitted(fit$asset.fit$HAM1)
names(fit.macro)
summary(fit.ts)

# test factormodel performance attribution
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("paFM.r")

fm.attr <- paFM(fit.ts)
names(fm.attr)
fm.attr$cum.ret.attr.f
fm.attr$cum.spec.ret
fm.attr$attr.list

# test benchmark
# data.xts <- checkData(managers.df) 
# fm.attr2 <- paFM(fit.ts,benchmark=data.xts[,9])
# fm.attr2$cum.ret.attr.f
# fm.attr2$cum.spec.ret
# fm.attr2$attr.list


source("summary.pafm.r")
summary(fm.attr)



# risk factor contribution to ETL
# combine fund returns, factor returns and residual returns for HAM1
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("factorModelVaRDecomposition.r")
source("factorModelEsDecomposition.r")
tmpData = cbind(managers.df[,1],managers.df[,c("EDHEC.LS.EQ","SP500.TR")] ,
                residuals(fit.macro$asset.fit$HAM1)/fit.macro$resid.sd[1])
colnames(tmpData)[c(1,4)] = c("HAM1", "residual")
factorModelEsDecomposition(tmpData, fit.macro$beta[1,],
                                            (fit.macro$resid.sd[1])^2, tail.prob=0.05,
                                                   VaR.method="gaussian")

factorModelVaRDecomposition(tmpData, fit.macro$beta[1,],
                            (fit.macro$resid.sd[1])^2, tail.prob=0.05,
                            VaR.method="kernel")





source("print.tsfm.r")  
print(fit.macro)

source("predict.tsfm.r")
predict(fit.macro)
newdata <- data.frame(EDHEC.LS.EQ = rnorm(n=120), SP500.TR = rnorm(n=120) )
rownames(newdata) <- rownames(fit.macro$data)
predict(fit.macro,newdata = newdata)



source("summary.tsfm.r")
summary(fit.macro)


source("plot.tsfm.r")
plot(fit.macro)
plot(fit.macro,plot.single=TRUE,asset.name="HAM1")

# summary of HAM1 
 summary(fit$asset.fit$HAM1)


source("factorModelCovariance.r")
factors    = managers.df[,(7:8)]
factorModelCovariance(fit.macro$beta,var(factors),fit.macro$resid.sd)


 # plot actual vs. fitted over time for HAM1
# use chart.TimeSeries() function from PerformanceAnalytics package
 dataToPlot = cbind(fitted(fit$asset.fit$HAM1), na.omit(managers.df$HAM1))
 colnames(dataToPlot) = c("Fitted","Actual")
chart.TimeSeries(dataToPlot, main="FM fit for HAM1",
                  colorset=c("black","blue"), legend.loc="bottomleft")


# transform to xts/zoo
library(PerformanceAnalytics)
data.xts <- checkData(managers.df) 
class(data.xts)
assets.t <- colnames(data.xts)[1:6]
factors.t <- colnames(data.xts)[7:9]


fitTest <- c(assets=assets.t,factors=factors.t,data=data.xts,fit.method="OLS",
             variable.selection="all subsets",factor.set=3)
class(fitTest)

#test quadratic term and up beta 
#load data from the database
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/data")
load('managers.df.rda')

data <- managers.df
data <- cbind(data,rnorm(120))
colnames(data)[10] <- "evwret"
asset.names <- colnames(managers.df[,(1:6)])
factor.names=c("EDHEC.LS.EQ","SP500.TR")
market.name = "evwret" 
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("fitTSFM.r")
args(fitTSFM)
fit.macro <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
                     factor.names=c("EDHEC.LS.EQ","SP500.TR"),
                     data=data, fit.method="OLS", variable.selection = "lar",
                     add.up.market = T, add.market.sqd = F,
                     market.name = "evwret", lars.criterion = "cv")


names(fit.macro)
fit.macro$beta



setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/data")
load('stat.fm.data.RData')
save(sfm.dat,sfm.apca.dat,file="stat.fm.data.RData")
data("stat.fm.data")


# sfm.dat is for pca
# sfm.apca.dat is for apca
class(sfm.dat)
rownames(sfm.dat) <- as.Date(as.yearmon(rownames(sfm.dat),"%b %Y")) 
class(sfm.apca.dat)
rownames(sfm.apca.dat) <- as.Date(dates,"%m/%d/%Y")
rownames(sfm.apca.dat) <- as.character(as.Date(rownames(sfm.apca.dat),"%m/%d/%Y"))

setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("fitStatisticalFactorModel.r")

# pca
args(fitStatisticalFactorModel)
fit.stat <- fitStatisticalFactorModel(sfm.dat,k=2)

names(fit.stat)
fit.stat$loadings
fit.stat$residuals
sfm.pca.fit$asset.fit
fit.stat$assets.names
fit.stat$data
newdata <- data.frame(F.1 = rnorm(n=120), F.2 = rnorm(n=120) )
predict(sfm.pca.fit,newdata)









source("summary.StatFactorModel.r")
summary(fit.stat)


source("print.StatFactorModel.r")
print(fit.stat)

class(sfm.pca.fit)
names(sfm.pca.fit)
sfm.pca.fit$factors
sfm.pca.fit$loadings
sfm.pca.fit$r2
sfm.pca.fit$residuals
sfm.pca.fit$resid.variance
sfm.pca.fit$mimic
sfm.pca.fit$asset.fit
factorModelCovariance(t(fit.stat$loadings),var(fit.stat$factors),sqrt(fit.stat$resid.variance))

# test factormodel performance attribution
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("paFM.r")

fm.attr <- paFM(fit.stat)
names(fm.attr)
fm.attr$cum.ret.attr.f
fm.attr$cum.spec.ret
fm.attr$attr.list

summary(fm.attr)

# test benchmark
# create a benchmark
date.s <- index(fm.attr$attr.list[[1]])
t <- length(date.s)
bench.stat <- xts(rnorm(t),as.Date(date.s))
fm.attr2 <- paFM(fit.stat,benchmark=bench.stat)
fm.attr2$cum.ret.attr.f
fm.attr2$cum.spec.ret
fm.attr2$attr.list





fit <- sfm.apca.fit.bn
class(sfm.apca.fit.bn$factors)
names(sfm.apca.fit.bn)

sfm.apca.fit.bn$asset.fit[[1]]


cov.factors = var(fit.stat$factors)
names = colnames(fit.stat$asset.ret)
factor.sd.decomp.list = list()
for (i in names) {
  factor.sd.decomp.list[[i]] =
    factorModelSdDecomposition(fit.stat$loadings[,i],
                               cov.factors, fit.stat$resid.variance[i])
}



# double check with xts
sfm.xts <- xts(sfm.dat,as.yearmon(rownames(sfm.dat),"%b %Y"))
data <- sfm.xts
sfm.pca.fit <- fitStatisticalFactorModel(sfm.xts,k=2,
                                         ckeckData.method="xts")


setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("predict.StatFactorModel.r")
predict(fit.stat)
testdata <- fit.stat$factors
predict(fit.stat,testdata)



source("plot.StatFactorModel.r")
plot(fit.stat,max.show=12)
plot(fit.stat,plot.single=TRUE, asset.name = "WEYER" )

# apca
sfm.apca.fit <- fitStatisticalFactorModel(sfm.apca.dat,k=2)
names(sfm.apca.fit)
sfm.apca.fit$asset.fit
sfm.apca.fit$residuals
newdata <- data.frame(F.1 = rnorm(n=182), F.2 = rnorm(n=182) )
predict(sfm.apca.fit,newdata)

sfm.apca.fit$residuals
sfm.apca.fit$mimic
sfm.apca.fit$r2
sfm.apca.fit$data

fm.attr <- paFM(sfm.apca.fit)
names(fm.attr)

factorModelCovariance(t(sfm.apca.fit$loadings),
                      var(sfm.apca.fit$factors),sqrt(sfm.apca.fit$resid.variance))

source("plot.StatFactorModel.r")
plot(sfm.apca.fit,max.show=10)
plot(sfm.apca.fit,plot.single=TRUE, asset.name = "ABM" )

fit.stat = sfm.apca.fit

source("print.StatFactorModel.r")
print(fit.stat)

# apca with bai and Ng method
sfm.apca.fit.bn <- fitStatisticalFactorModel(sfm.apca.dat,k="bn")
class(sfm.apca.fit.bn)
names(sfm.apca.fit.bn)
sfm.apca.fit.bn$mimic
sfm.apca.fit.bn$asset.fit


plot(sfm.apca.fit.bn)

fm.bn.attr <- paFM(sfm.apca.fit.bn)
names(fm.bn.attr)
fm.bn.attr$cum.ret.attr.f
fm.bn.attr$cum.spec.ret
fm.bn.attr$attr.list$GZSOXX
summary(fm.bn.attr)

# apca with ck method
sfm.apca.fit.ck <- fitStatisticalFactorModel(sfm.apca.dat,k="ck")
class(sfm.apca.fit.ck)
names(sfm.apca.fit.ck)
sfm.apca.fit.ck$factors
sfm.apca.fit.ck$loadings
sfm.apca.fit.ck$mimic
sfm.apca.fit.ck$asset.fit
checkData(sfm.apca.fit.ck$data)

source("paFM.r")

fm.ck.attr <- paFM(sfm.apca.fit.ck)
names(fm.ck.attr)
fm.ck.attr$cum.ret.attr.f
fm.ck.attr$cum.spec.ret
fm.ck.attr$attr.list$GZSOXX
summary(fm.bn.attr)

# create benchmark
date.ck <- index(fm.ck.attr$attr.list[[1]])
t <- length(date.ck)
bench.ck <- xts(rnorm(t),as.Date(date.ck))

fm.ck.attr2 <- paFM(sfm.apca.fit.ck,benchmark=bench.ck)
names(fm.ck.attr)
fm.ck.attr2$cum.ret.attr.f
fm.ck.attr2$cum.spec.ret
fm.ck.attr2$attr.list$GZSOXX
summary(fm.bn.attr)



# use xts input

sfm.apca.xts <- xts(sfm.apca.dat,as.Date(rownames(sfm.apca.dat),"%m/%d/%Y") )
sfm.apca.fit.ck.xts <- fitStatisticalFactorModel(sfm.apca.xts,k="ck",
                                             ,ckeckData.method="xts")


sfm.apca.fit.ck.xts$loadings
sfm.apca.fit.ck.xts$mimic
sfm.apca.fit.ck.xts$asset.fit
sfm.apca.fit.ck.xts$factors


source("summary.StatFactorModel.r")
summary(fit.stat)
summary(sfm.apca.fit)

# load data
#load data from the database
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/data")
load("stock.RData")
load("CRSP.RData") #old data set. substitute by stock.RData
stock <- data
save(stock,file="Stock.df.RData")
# names(data)[8:12]  <- c("LTDEBT","NET.SALES","COMMON.EQUITY","NET.INCOME","STOCKHOLDERS.EQUITY")
# data <- data[,-1] 
# save(data,file="stock.RData")

# BARRA type factor model
names(stock)
head(data)
class(data)



setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("fitFundamentalFactorModel.r")
source("predict.FundamentalFactorModel.r")

args(fitFundamentalFactorModel)


# there are 447 assets  
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
returnsvar = "RETURN"
datevar = "DATE"
assetvar = "TICKER"
data(Stock.df)
fit.fund <- fitFundamentalFactorModel(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP")
                                 , data=stock,returnsvar = "RETURN",datevar = "DATE",  
                                      assetvar = "TICKER",
                                      wls = TRUE, regression = "classic", 
                                      covariance = "classic", full.resid.cov = FALSE)
names(fit.fund)
test.fit$cov.returns
test.fit$cov.resids
names(test.fit$cov.factor)
test.fit$cov.factor$cov
fit.fund$factor.returns
fit$resid.variance
fit$residuals
fit$tstats
fit$call
fit.fund$asset.names
fit.fund$beta


#test performance attribution

setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("paFM.r")

fm.attr <- paFM(fit.fund)
names(fm.attr)
fm.attr$cum.ret.attr.f
fm.attr$cum.spec.ret
fm.attr$attr.list

summary(fm.attr)



# test print method
source("print.FundamentalFactorModel.r")
print(fit.fund)


# test summary method


# test predict function
source("predict.FundamentalFactorModel.r")
predict(fit.fund)
predict(fit.fund,stock,new.assetvar="TICKER",new.datevar="DATE")
x1[,1]
x2[,1]
# very similar 

testdata <- data[,c("DATE","TICKER")]
testdata$BOOK2MARKET <- rnorm(n=42465)
testdata$LOG.MARKETCAP <- rnorm(n=42465)
predict(fit.fund,testdata,new.assetvar="TICKER",new.datevar="DATE")

source("plot.FundamentalFactorModel.r") 
plot(fit.fund,max.show=10)
plot(fit.fund,plot.single=TRUE,asset.name = "JJSF")

# FM return covariance 
factorModelCovariance(fit.fund$beta,fit.fund$factor.cov$cov,sqrt(fit.fund$resid.variance))


# BARRA type Industry Factor Model
data(Stock.df)
class(stock$GICS.SECTOR)
exposure.names <- c("GICS.SECTOR") 
# the rest keep the same
fit.ind2 <- fitFundamentalFactorModel(data=stock,exposure.names=exposure.names, 
                                     datevar = "DATE", returnsvar = "RETURN", 
                                     assetvar = "TICKER",
                                     wls = TRUE, regression = "classic", 
                                     covariance = "classic", full.resid.cov = FALSE,
                                     robust.scale = TRUE)

fit.ind2$beta


names(test.fit2)
names(test.fit2$cov.returns)
test.fit2$cov.resids
names(test.fit2$cov.factor)
test.fit2$cov.factor$cov
test.fit2$factor
test.fit2$resid.variance
test.fit2$residuals
test.fit2$tstats
test.fit2$call

source("predict.FundamentalFactorModel.r")
predict(fit.fund)
source("plot.FundamentalFactorModel.r") 
plot(fit.fund,max.show=6)
plot(fit.fund,plot.single=TRUE,asset.name = "JJSF")

#test industry model and style model
# test standardized factor exposures
data(Stock.df)
dim(stock)
# add one more colnames as standardized weight
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("fitFundamentalFactorModel.r")
stock$s.weight <- rnorm(42465,10,1)^2
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP","GICS.SECTOR") 
fit.fund <- fitFundamentalFactorModel(exposure.names=exposure.names,
                                      , data=stock,returnsvar = "RETURN",datevar = "DATE",  
                                      assetvar = "TICKER",weight.var = "s.weight",
                                      standardized.factor.exposure = FALSE,
                                      wls = TRUE, regression = "classic", 
                                      covariance = "classic", full.resid.cov = FALSE)
names(fit.fund)
fit.fund$factor.returns

source("paFM.r")
fund.attr <- paFM(fit.fund)
names(fund.attr)
fund.attr$cum.ret.attr.f
fund.attr$cum.spec.ret
fund.attr$attr.list[[1]]
summary(fund.attr)

fit <- fit.fund
# test of factorModelCovariance.r with fundamental factor models
# input 
# beta.mat, factor.cov, residVars.vec
data(managers.df)
data(stat.fm.data)
data(CommonFactors)
data(stock)
source("factorModelCovariance.r")
unique(stock[,"DATE"])
# take beta as the last data input
exposure.names <- c("BOOK2MARKET", "LOG.MARKETCAP") 
beta.mat1 <- subset(stock,DATE == "2003-12-31")[,exposure.names]
dim(beta.mat1)
beta.mat1 <- cbind(rep(1,447),beta.mat1)
# FM return covariance 
fit.fund <- fitFundamentalFactorModel(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP")
                                      , data=stock,returnsvar = "RETURN",datevar = "DATE",  
                                      assetvar = "TICKER",
                                      wls = TRUE, regression = "classic", 
                                      covariance = "classic", full.resid.cov = FALSE)
ret.cov.fundm <- factorModelCovariance(beta.mat1,fit.fund$factor.cov$cov,sqrt(fit.fund$resid.variance))
fit.fund$returns.cov$cov == ret.cov.fundm


# test summary.fitFundamentalFactorModel
source("summary.FundamentalFactorModel.r")
summary(fit.fund)
summary(test.fit2)


source("factorModelSdDecomposition.r")
source("factorModelEsDecomposition.r")
source("factorModelVaRDecomposition.r")



# test factor model monte carlo method
factorData=fit.macro$data[,fit.macro$factors.names]
Beta.mat=fit.macro$beta
residualData=as.matrix(fit.macro$resid.variance,1,6) 
n.boot=1000
# bootstrap returns data from factor model with residuals sample from normal distribution
source("factorModelMonteCarlo.r")
bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="normal",
                                  residualData, Alpha.mat=NULL, boot.method="block",
                                  seed = 123, return.factors = "TRUE", return.residuals = 
                                    "TRUE")

names(bootData)
# Cornish-Fisher distribution
# build different residualData matrix
residualData <- cbind(c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,2,1,0))
colnames(residualData) <- c("var","skew","ekurt")
rownames(residualData) <- colnames(managers.df[,(1:6)])
bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="Cornish-Fisher",
                                  residualData, Alpha.mat=NULL, boot.method="block",
                                  seed = 123, return.factors = "TRUE", return.residuals =
                                    "TRUE")

bootData
# skew-t distribution
# build residualData matrix
residualData <- cbind(rnorm(6),c(1,2,1,3,0.1,0.5),rnorm(6),c(2,3,1,6,10,100))
colnames(residualData) <- c("location","scale","shape","df")
rownames(residualData) <- colnames(managers.df[,(1:6)])
bootData <- factorModelMonteCarlo(n.boot, factorData,Beta.mat, residual.dist="skew-t",
                                  residualData, Alpha.mat=NULL, boot.method="block",
                                  seed = 123, return.factors = "TRUE", return.residuals =
                                    "TRUE")



bootData
           
# test factor
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/data")
load("CommonFactors.RData")
load("factors.rda") # unkonw data set 


data(Stock.df)
fit.fund <- fitFundamentalFactorModel(exposure.names=c("BOOK2MARKET", "LOG.MARKETCAP")
                                       , data=stock,returnsvar = "RETURN",datevar = "DATE",  
                                      assetvar = "TICKER",
                                       wls = TRUE, regression = "classic", 
                                       covariance = "classic", full.resid.cov = FALSE)
  idx <- fit.fund$data[,fit.fund$assetvar]  == "STI"         
  asset.ret <- fit.fund$data[idx,fit.fund$returnsvar]  
  tmpData = cbind(asset.ret, fit.fund$factors,
                  fit.fund$residuals[,"STI"]/sqrt(fit.fund$resid.variance["STI"]) )
   colnames(tmpData)[c(1,length(tmpData[1,]))] = c("STI", "residual")
   factorModelEsDecomposition(tmpData, 
                           fit.fund$beta["STI",],
                         fit.fund$resid.variance["STI"], tail.prob=0.05,
                           VaR.method = "historical" )

i = "STI"
idx <- fit.fund$data[,fit.fund$assetvar]  == "STI"  
asset.ret <- fit.fund$data[idx,fit.fund$returnsvar]
tmpData = cbind(asset.ret, fit.fund$factor.returns,
                fit.fund$residuals[,"STI"]/sqrt(fit.fund$resid.variance["STI"]))
colnames(tmpData)[c(1,length(tmpData[1,]))] = c("STI", "residual")
  factorModelEsDecomposition(tmpData, fit.fund$beta["STI",],
                             fit.fund$resid.variance["STI"], tail.prob=0.05,
                             VaR.method="historical")



# paFM example
setwd("C:/Users/Yi-An Chen/Documents/R-project/returnanalytics/pkg/FactorAnalytics/R")
source("paFM.r")

source("summary.pafm.r")
data(managers.df)
fit.ts <- fitTSFM(asset.names=colnames(managers.df[,(1:6)]),
                  factor.names=c("EDHEC.LS.EQ","SP500.TR"),
                  data=managers.df,fit.method="OLS", variable.selection="none")

# withoud benchmark
fm.attr <- paFM(fit.ts)
summary(fm.attr)

source("plot.pafm.r")
plot(fm.attr,legend.loc="topleft",max.show=6)
plot(fm.attr,plot.single=TRUE,fundName="HAM1")