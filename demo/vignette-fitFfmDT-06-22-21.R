
require(methods)
library(data.table)
library(FactorAnalytics)

## ----echo=F,message=FALSE-----------------------------------------------------
rm(list=ls())

## ----eval=F,message=FALSE,warning=FALSE---------------------------------------
## help(factorDataSetDjia5Yrs)


## ----message=FALSE,warning=FALSE----------------------------------------------
data("factorDataSetDjia5Yrs")
dataDjia5Yr = factorDataSetDjia5Yrs
head(dataDjia5Yr,2)



## ----message=FALSE,warning=FALSE----------------------------------------------
asset.var="TICKER" 
ret.var="RETURN" 
date.var = "DATE"
exposure.vars= c("SECTOR","SIZE","P2B","EV2S")
spec1 <- specFfm(data = dataDjia5Yr,asset.var = asset.var, ret.var = ret.var, 
	date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
	addIntercept = T, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)
# standardize the expsoures Cross-Sectionally
spec1 <- standardizeExposures(spec1, Std.Type = "CrossSection") 
# fit the model
mdlFit <- fitFfmDT(spec1) 

# extract regression results
results <- extractRegressionStats(spec1, fitResults = mdlFit)
#retrofit object 
fitDjia5Yr <- FactorAnalytics::convert(SpecObj = spec1, FitObj = mdlFit,
			RegStatsObj = results) 
names(fitDjia5Yr)


## ----label='djia5yr ffm rsq',echo=T-------------------------------------------
fmRsq(fitDjia5Yr, rsqAdj = T, plt.type = 2, isPrint = F,lwd = .7,
		stripText.cex = .8,axis.cex=.8)


## ----label='djia5yr ffm vifs',echo=T------------------------------------------
vif(fitDjia5Yr, isPlot = T, isPrint = F, lwd = .7,stripText.cex = .8,axis.cex=.8)


## ----label='djia5yr tStats',echo=T,fig.width=6,fig.height=4-------------------
fmTstats(fitDjia5Yr,whichPlot="tStats",color="blue",lwd=.7,layout=c(3,4),
			stripText.cex = .8,axis.cex=.8)


## ----label='djia5YrNumberSigTstats',echo=T,fig.width=6,fig.height=4-----------
fmTstats(fitDjia5Yr,whichPlot = "significantTstatsV", color = "blue",
			stripText.cex = .8,axis.cex=.8,layout=c(3,4))


## ----message=FALSE,warning=FALSE----------------------------------------------
data(wtsDjiaGmvLo)
wtsDjia = wtsDjiaGmvLo


## ----message=FALSE,warning=FALSE----------------------------------------------
repExposures(fitDjia5Yr, wtsDjia, isPlot = FALSE, digits = 1,
			stripText.cex = .8,axis.cex=.8) 


## ----label='datDjiaExposuresMeanVolBarplots',echo=T---------------------------
repExposures(fitDjia5Yr, wtsDjia, isPrint = F,isPlot = T, which = 3,
             add.grid=F, zeroLine=F, color='Cyan')


## ----label='datDjia5YrExposuresTimeSeries',echo=T-----------------------------
repExposures(fitDjia5Yr,wtsDjia,isPrint=F,isPlot=T,which=1,add.grid=F, 				zeroLine = T, color = 'Blue',stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrExposuresBoxPlots',echo=T-------------------------------
repExposures(fitDjia5Yr, wtsDjia, isPrint = FALSE, isPlot = TRUE, 
			which = 2,	notch = F, layout = c(3,3)) 


## ----message=FALSE,warning=FALSE----------------------------------------------
repReturn(fitDjia5Yr, wtsDjia, isPlot = FALSE, digits = 2) 


## ----label='datDjia5YrPortRetFacSpecific',echo=T------------------------------
repReturn(fitDjia5Yr, wtsDjia, isPrint = FALSE, isPlot = TRUE, which = 1,
          add.grid = TRUE, scaleType = 'same',color = 'Blue',
			stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrPortStyleFacRet',echo=T---------------------------------
repReturn(fitDjia5Yr, wtsDjia, isPrint = FALSE, isPlot = TRUE, which = 2,
          add.grid = TRUE, zeroLine = T, color = "Blue",scaleType = 'same',
			stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrPortSectorFacRet',echo=T--------------------------------
repReturn(fitDjia5Yr, wtsDjia, isPrint = FALSE, isPlot = TRUE, which = 3,
          add.grid = TRUE, zeroLine = T, color = "Blue", scaleType = 'same',
			stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrPortRetBitsAllBoxplots',echo=T--------------------------
repReturn(fitDjia5Yr, wtsDjia, isPrint = FALSE, isPlot = TRUE, which = 4)


## ----message=F,warning=F------------------------------------------------------
asset.var="TICKER" 
ret.var="RETURN" 
date.var = "DATE"
exposure.vars= c("SIZE","P2B","EV2S")
spec1 <- specFfm(data = dataDjia5Yr,asset.var = asset.var, ret.var = ret.var, 
	date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
	addIntercept = T, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)
# standardize the expsoures Cross-Sectionally
spec1 <- standardizeExposures(spec1, Std.Type = "CrossSection") 
# fit the model
mdlFit <- fitFfmDT(spec1, fit.method = "WLS") 

# extract regression results
results <- extractRegressionStats(spec1, fitResults = mdlFit)
#retrofit object 
fitDjia5YrIntStyle <- convert(SpecObj = spec1, FitObj = mdlFit,
			RegStatsObj = results) 



## ----message=F,warning=F------------------------------------------------------
data(wtsDjiaGmvLo)
wtsDjia = wtsDjiaGmvLo


## ----label='repRiskSdPlotPrintFPCR',echo=T------------------------------------
repRisk(fitDjia5YrIntStyle, wtsDjia, risk = "Sd", decomp = "FPCR",
		nrowPrint = 10,sliceby = "factor", isPrint = T, isPlot = T,
		layout = c(5,1),stripText.cex = .8,axis.cex=.8)


## ----label='repRiskEsPlotFPCR',echo=T-----------------------------------------
repRisk(fitDjia5YrIntStyle, wtsDjia, risk = "ES", decomp = "FPCR",
		nrowPrint = 10,sliceby = "factor", isPrint = F, isPlot = T,
		layout = c(5,1),stripText.cex = .8,axis.cex=.8)


## ----label='repRiskEsPlotFCR',echo=T------------------------------------------
repRisk(fitDjia5YrIntStyle, wtsDjia, risk = "ES", decomp = "FCR",
		nrowPrint = 10,sliceby = "factor", isPrint = F, isPlot = T,
		layout = c(5,1),stripText.cex = .8,axis.cex=.8)


## ----message=F,warning=F------------------------------------------------------
repRisk(fitDjia5YrIntStyle, wtsDjia, risk = c("Sd","ES","VaR"),
		decomp = "FPCR",sliceby = "factor",isPrint = T,isPlot = TRUE,
		layout = c(5,1),portfolio.only = T,stripText.cex = .8,axis.cex=.8)


## ----message=F,warning=F------------------------------------------------------
args(fmmcSemiParam)


## ----message=F, warning=F-----------------------------------------------------

data("factorDataSetDjia5Yrs")
N = 22
exposure.vars <- c("P2B", "MKTCAP", "SECTOR")

spec1 <- specFfm(data = dataDjia5Yr,asset.var = asset.var, ret.var = ret.var, 
	date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
	addIntercept = FALSE, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)
# standardize the expsoures , you can also not call this
spec1 <- standardizeExposures(spec1, Std.Type = "None") 
# fit the model
mdlFit <- fitFfmDT(spec1) 
fit.ffm <- extractRegressionStats(spec1, fitResults = mdlFit) 



## ----message=F, warning=F-----------------------------------------------------
resid.par = fit.ffm$residuals
fmmcDat=fmmcSemiParam(B=1000,factor.ret=fit.ffm$factor.returns, 				beta=fit.ffm$beta,resid.par=resid.par,
				boot.method = "random",resid.dist = "empirical")
names(fmmcDat)


## ----message=F, warning=F-----------------------------------------------------
data = factorDataSetDjia5Yrs
djiaDat = tapply(data$RETURN,list(data$DATE,data$TICKER),I)
djiaRet = xts(djiaDat,as.yearmon(rownames(djiaDat)))


## ----message=F, warning=F-----------------------------------------------------
round(apply(djiaRet,2,mean)[1:10],3)
round(apply(fmmcDat$sim.fund.ret,2,mean)[1:10],3)


## ----message=F, warning=F-----------------------------------------------------
round(apply(djiaRet,2,sd)[1:10],3)
round(apply(fmmcDat$sim.fund.ret,2,sd)[1:10],3)


## ----message=F, warning=F-----------------------------------------------------
resid.mean = apply(B=1000, coredata(fit.ffm$residuals), 2, mean, na.rm=T)
resid.sd = matrix(sqrt(fit.ffm$resid.var))  
resid.par = cbind(resid.mean, resid.sd)
fmmcDatNormal=fmmcSemiParam(factor.ret=fit.ffm$factor.returns, 			beta=fit.ffm$beta,resid.par=resid.par, boot.method = "random")


## ----message=F, warning=F-----------------------------------------------------
round(apply(djiaRet,2,mean)[1:10],3)
round(apply(fmmcDatNormal$sim.fund.ret,2,mean)[1:10],3)
round(apply(djiaRet,2,sd)[1:10],3)
round(apply(fmmcDatNormal$sim.fund.ret,2,sd)[1:10],3)


## ----message=F,warnings=F-----------------------------------------------------
dat = factorDataSetDjia5Yrs

spec1 <- specFfm(data = dat, asset.var="TICKER", ret.var="RETURN",
              date.var="DATE", exposure.vars = "SECTOR",weight.var = NULL,
			addIntercept = F, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)

# fit the model
mdlFit <- fitFfmDT(spec1) 

# extract regression results
results <- extractRegressionStats(spec1, fitResults = mdlFit)
#retrofit object 
fitSec <- convert(SpecObj = spec1, FitObj = mdlFit,
			RegStatsObj = results) 


round(coef(summary(fitSec)$sum.list[[1]])[,1],3)
round(fitSec$factor.returns[1,],3)


## ----message=F,warnings=F-----------------------------------------------------

spec1 <- specFfm(data = dat, asset.var="TICKER", ret.var="RETURN",
              date.var="DATE", exposure.vars = "SECTOR",weight.var = NULL,
			addIntercept = T, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)

# fit the model
mdlFit <- fitFfmDT(spec1) 

# extract regression results
results <- extractRegressionStats(spec1, fitResults = mdlFit)

fitSecInt <-  convert(SpecObj = spec1, FitObj = mdlFit,
			RegStatsObj = results) 


round(coef(summary(fitSecInt)$sum.list[[1]])[,1],2)
round(fitSecInt$factor.returns[1,],2)
round(sum(fitSecInt$factor.returns[1,-1]),2) 


## ----message=F,warning=F------------------------------------------------------
# Country Incremental Components of Asset Returns
set.seed(10000)
Bind = cbind(rep(1,30),c(rep(1,10),rep(0,20)),c(rep(0,10),rep(1,10),rep(0,10)),
             c(rep(0,20),rep(1,10)))
cty1 = matrix(rep(c(0,1), 15))
cty2 =  matrix(rep(c(1,0), 15))
Bmic = cbind(Bind, cty1,cty2)
dimnames(Bmic)[[2]] = c("mkt","sec1","sec2","sec3", "cty1", "cty2")
r.add = rnorm(30,4,.2)
r.cty1 = rep(0,30)
r.cty2 = rep(0,30)
for(i in 1:30) {
  if(Bmic[i,"cty1"]==1)  {r.cty1[i] = r.add[i];r.cty2[i] = 0}
      else {r.cty1[i] = 0;r.cty2[i] = r.add[i] + 1}
}

# Asset Returns for Market+Industry+Country Model
mu = c(1,2,3)
sd = c(.2,.2,.2)
r = list()
r.mic = list()
fitMic = list()
fitMic1 = list()
for(i in 1:5){
set.seed(1099923+(i-1))
r[[i]]= c(rnorm(10,mu[1],sd[1]),rnorm(10,mu[2],sd[2]),
			rnorm(10,mu[3],sd[3]))
r.mic[[i]] = r[[i]] + r.cty1 + r.cty2
}


## ----label='qqnormRetMICmodel',echo=T-----------------------------------------
qqnorm(r.mic[[1]],main = "MIC Model Equity Returns for First Period",
       xlab="NORMAL QQ-PLOT",ylab="RETURNS")


## ----message=F,warnings=F-----------------------------------------------------
Returns = unlist(r.mic) 
COUNTRY = rep(rep(c("US", "India"), 15), 5)
SECTOR = rep(rep(c("SEC1", "SEC2", "SEC3"), each = 10),5)
TICKER = rep(c(LETTERS[1:26], paste0("A",LETTERS[1:4])),5)
DATE = rep(seq(as.Date("2000/1/1"),by = "month",length.out = 5),each = 30)
data.mic = data.frame("DATE"=as.character(DATE), TICKER, Returns, 
						SECTOR, COUNTRY)
exposure.vars = c("SECTOR", "COUNTRY")
fit = fitFfm(data=data.mic, asset.var="TICKER", ret.var="Returns", 
              date.var="DATE", exposure.vars=exposure.vars,
				addIntercept = T)
fit$factor.returns

