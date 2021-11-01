library(data.table)
library(PCRM)
library(FactorAnalytics)
rm(list=ls())

data(stocksCRSP)
data(scoresSPGMI)

names(stocksCRSP)

commonNames <- intersect(names(stocksCRSP),names(scoresSPGMI))
facModDatmerged <- merge(stocksCRSP, scoresSPGMI, by = commonNames)

colNames <- c("Date","TickerLast","CapGroup","Sector","Return","Ret13WkBill",         
              "mktIndexCRSP","BP","LogMktCap","SEV")  

facModDat <- facModDatmerged[, .SD , .SDcols = colNames] # select some of the columns
# change the names of some of the columns
setnames(facModDat, old = c("TickerLast"  , "Ret13WkBill" , "mktIndexCRSP" ,"LogMktCap"  ),
         new = c("Ticker","RF","MKT","SIZE"))
summary(facModDat)
# get rid of NA sector

facDat <- facModDat[!is.na(Sector)] # 250 stocks
length(unique(facDat$Ticker))
facDatSmall <- facDat[CapGroup == "SmallCap"] # 68 smallcap stocks
length(unique(facDatSmall$Ticker))
facDatSmall[, CapGroup := NULL ] # Remove CapGroup variable
head(facDatSmall,1)

head(facDatSmall,2)
head(unique(facDatSmall$Date)) # Confirm that it is monthly data
facDat <-facDatSmall[facDatSmall$Date >= as.Date("2006-01-31") & 
                          facDatSmall$Date <= as.Date("2010-12-31"), ]
range(facDat$Date)
names(facDat)
class(facDat)

# Create an xts object returns for 20 stocks
returnMat = tapply(facDat[["Return"]],list(facDat$Date,facDat$Ticker),I)
returns = xts(returnMat,as.yearmon(rownames(returnMat)))
returns20 <- returns[,1:20]
tsPlotMP(returns20,scaleType = "free",layout = c(2,10),stripText.cex = .7,Pct=F)

# Create a data.table object facDat20 for the 20 stocks just created
Tickers20 <- names(returns20)
facDat20 <- facDat[Ticker %in% Tickers20,]

# Create gmvLOcrsp weight vector for returns20
library(PCRM)
library(PortfolioAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
funds <- colnames(returns20)
pspec.base <- portfolio.spec(funds)
pspec.fi <- add.constraint(portfolio=pspec.base,type="full_investment")
pspec.uc <- add.objective(portfolio=pspec.fi,type="risk",name="var")
pspec.lo <- add.constraint(portfolio=pspec.uc,type="long_only")
opt.lo <- optimize.portfolio(returns20,pspec.lo,optimize_method="quadprog")
names <- c("WTS","MU","SD","SR")
out.lo <- opt.outputMvo(opt.lo,returns20,names=names,digits = 4)
wtsGmvLO <- out.lo$WTS


## ----message=FALSE,warning=FALSE----------------------------------------------
asset.var="Ticker" 
ret.var="Return" 
date.var = "Date"
exposure.vars= c("Sector","SIZE","BP","SEV")
spec1 <- specFfm(data = facDat20,asset.var = asset.var, ret.var = ret.var, 
	date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
	addIntercept = T, rob.stats = FALSE)

class(spec1$dataDT)
# lag the exposures
spec1 <- lagExposures(spec1)
# standardize the expsoures Cross-Sectionally
spec1 <- standardizeExposures(spec1, Std.Type = "CrossSection") 
# fit the model
mdlFit <- fitFfmDT(spec1) 
names(mdlFit)

# extract regression results
results <- extractRegressionStats(spec1, fitResults = mdlFit)
#retrofit object 
FitfacDat20 <- FactorAnalytics::convert(SpecObj = spec1, FitObj = mdlFit,
			RegStatsObj = results) 
names(FitfacDat20)


## ----label='djia5yr ffm rsq',echo=T-------------------------------------------
fmRsq(FitfacDat20, rsqAdj = T, plt.type = 2, isPrint = F,lwd = .7,
		stripText.cex = .8,axis.cex=.8)


## ----label='djia5yr ffm vifs',echo=T------------------------------------------
vif(FitfacDat20, isPlot = T, isPrint = F, lwd = .7,stripText.cex = .8,axis.cex=.8)


## ----label='djia5yr tStats',echo=T,fig.width=6,fig.height=4-------------------
fmTstats(FitfacDat20,whichPlot="tStats",color="blue",lwd=.7,layout=c(3,5),
			stripText.cex = .8,axis.cex=.8)


## ----label='djia5YrNumberSigTstats',echo=T,fig.width=6,fig.height=4-----------
fmTstats(FitfacDat20,whichPlot = "significantTstatsV", color = "blue",
			stripText.cex = .8,axis.cex=.8,layout=c(3,5))


## ----message=FALSE,warning=FALSE----------------------------------------------


## ----message=FALSE,warning=FALSE----------------------------------------------
repExposures(FitfacDat20, wtsGmvLO, isPlot = FALSE, digits = 1,
			stripText.cex = .8,axis.cex=.8) 


## ----label='datDjiaExposuresMeanVolBarplots',echo=T---------------------------
repExposures(FitfacDat20, wtsGmvLO, isPrint = F,isPlot = T, which = 3,
             add.grid=F, zeroLine=F, color='Cyan')


## ----label='datDjia5YrExposuresTimeSeries',echo=T-----------------------------
repExposures(FitfacDat20,wtsGmvLO,isPrint=F,isPlot=T,which=1,add.grid=F,
             zeroLine = T, color = 'Blue',stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrExposuresBoxPlots',echo=T-------------------------------
repExposures(FitfacDat20, wtsGmvLO, isPrint = FALSE, isPlot = TRUE, 
			which = 2,	notch = F, layout = c(3,3),color = "cyan") 


## ----message=FALSE,warning=FALSE----------------------------------------------
repReturn(FitfacDat20, wtsGmvLO, isPlot = FALSE, digits = 2) 


## ----label='datDjia5YrPortRetFacSpecific',echo=T------------------------------
repReturn(FitfacDat20, wtsGmvLO, isPrint = FALSE, isPlot = TRUE, which = 1,
          add.grid = TRUE, scaleType = 'same',color = 'Blue',
			stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrPortStyleFacRet',echo=T---------------------------------
repReturn(FitfacDat20, wtsGmvLO, isPrint = FALSE, isPlot = TRUE, which = 2,
          add.grid = TRUE, zeroLine = T, color = "Blue",scaleType = 'same',
			stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrPortSectorFacRet',echo=T--------------------------------
repReturn(FitfacDat20, wtsGmvLO, isPrint = FALSE, isPlot = TRUE, which = 3,
          add.grid = TRUE, zeroLine = T, color = "Blue", scaleType = 'same',
			stripText.cex = .8,axis.cex=.8)


## ----label='datDjia5YrPortRetBitsAllBoxplots',echo=T--------------------------
repReturn(FitfacDat20, wtsGmvLO, isPrint = FALSE, isPlot = TRUE, which = 4)


## ----message=F,warning=F------------------------------------------------------
asset.var="Ticker" 
ret.var="Return" 
date.var = "Date"
exposure.vars= c("SIZE","BP","SEV")
spec1 <- specFfm(data = facDat20,asset.var = asset.var, ret.var = ret.var, 
	date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
	addIntercept = T, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)
# standardize the expsoures Cross-Sectionally
spec1 <- standardizeExposures(spec1, Std.Type = "CrossSection") 
# fit the model
#  mdlFit <- fitFfmDT(spec1, fit.method = "WLS") 
#  the above WLS choice gives a result mdlFit with no errors
#  but then the first repRisk on line 188 below gives all NAs
#  This doees not happern with OLS below
mdlFit <- fitFfmDT(spec1) 

# extract regression results
results <- extractRegressionStats(spec1, fitResults = mdlFit)
#retrofit object 
fitfacDat20Style <- convert(SpecObj = spec1, FitObj = mdlFit,
			RegStatsObj = results) 



## ----message=F,warning=F------------------------------------------------------


## ----label='repRiskSdPlotPrintFPCR',echo=T------------------------------------
repRisk(fitfacDat20Style, wtsGmvLO, risk = "Sd", decomp = "FPCR",
		nrowPrint = 21,sliceby = "factor", isPrint = T, isPlot = T,
		layout = c(5,1),stripText.cex = .8,axis.cex=.8)


## ----label='repRiskEsPlotFPCR',echo=T-----------------------------------------
repRisk(fitfacDat20Style, wtsGmvLO, risk = "ES", decomp = "FPCR",
		nrowPrint = 10,sliceby = "factor", isPrint = F, isPlot = T,
		layout = c(5,1),stripText.cex = .8,axis.cex=.8)

## ----label='repRiskEsPlotFCR',echo=T------------------------------------------
repRisk(fitfacDat20Style, wtsGmvLO, risk = "ES", decomp = "FCR",
		nrowPrint = 10,sliceby = "factor", isPrint = F, isPlot = T,
		layout = c(5,1),stripText.cex = .8,axis.cex=.8)

## ----message=F,warning=F------------------------------------------------------
repRisk(fitfacDat20Style, wtsGmvLO, risk = c("Sd","ES","VaR"),
		decomp = "FPCR",sliceby = "factor",isPrint = T,isPlot = F,
		layout = c(5,1),portfolio.only = T,stripText.cex = .8,axis.cex=.8)

# I added the following to get the data and check the row sums
repRiskDat <- repRisk(fitfacDat20Style, wtsGmvLO, risk = c("Sd","ES","VaR"),
                      decomp = "FPCR",sliceby = "factor",isPrint = T,isPlot = F,
                      layout = c(5,1),portfolio.only = T,stripText.cex = .8,axis.cex=.8)
# Hand checked calculation for the above gives row sums 100.0, 99.8, 100.0 

## ----message=F,warning=F------------------------------------------------------
args(fmmcSemiParam)


## ----message=F, warning=F-----------------------------------------------------

exposure.vars= c("Sector","SIZE","BP")

spec1 <- specFfm(data = facDat20,asset.var = asset.var, ret.var = ret.var, 
	date.var = date.var, exposure.vars = exposure.vars,weight.var = NULL,
	addIntercept = FALSE, rob.stats = FALSE)
# lag the exposures
spec1 <- lagExposures(spec1)
# standardize the expsoures , you can also not call this
spec1 <- standardizeExposures(spec1, Std.Type = "None")
# fit the model
mdlFit <- fitFfmDT(spec1) 
results <- extractRegressionStats(spec1, fitResults = mdlFit) 
#retrofit object 
fit.ffm <- convert(SpecObj = spec1, FitObj = mdlFit,
                  RegStatsObj = results) 

# The above was originally
# results <- extractRegressionStats(spec1, fitResults = mdlFit) 
# and I was trying the above to fix problem, but it didn't fix it.

# Ignore the following lines through 250 (my exploration, Doug)
names(fit.ffm)
names(fit.ffm$beta)
class(fit.ffm$beta)
dimnames(fit.ffm$beta)

names(fit.ffm$factor.returns)
class(fit.ffm$factor.returns)


## ----message=F, warning=F-----------------------------------------------------
resid.par = fit.ffm$residuals
fmmcDat=fmmcSemiParam(B=1000,factor.ret=fit.ffm$factor.returns,
                      beta=fit.ffm$beta,resid.par=resid.par,
                      boot.method = "random",resid.dist = "empirical")
names(fmmcDat)



## ----message=F, warning=F-----------------------------------------------------
round(apply(returns20,2,mean)[1:10],3)
round(apply(fmmcDat$sim.fund.ret,2,mean)[1:10],3)


## ----message=F, warning=F-----------------------------------------------------
round(apply(returns20,2,sd)[1:10],3)
round(apply(fmmcDat$sim.fund.ret,2,sd)[1:10],3)


## ----message=F, warning=F-----------------------------------------------------
resid.mean = apply(B=1000, coredata(fit.ffm$residuals), 2, mean, na.rm=T)
resid.sd = matrix(sqrt(fit.ffm$resid.var))  
resid.par = cbind(resid.mean, resid.sd)
fmmcDatNormal=fmmcSemiParam(factor.ret=fit.ffm$factor.returns,
                            beta=fit.ffm$beta,resid.par=resid.par,
                            boot.method = "random")


## ----message=F, warning=F-----------------------------------------------------
round(apply(returns20,2,mean)[1:10],3)
round(apply(fmmcDatNormal$sim.fund.ret,2,mean)[1:10],3)
round(apply(returns20,2,sd)[1:10],3)
round(apply(fmmcDatNormal$sim.fund.ret,2,sd)[1:10],3)


## ----message=F,warnings=F-----------------------------------------------------
spec1 <- specFfm(data = facDat20, asset.var="Ticker", ret.var="Return",
              date.var="Date", exposure.vars = "Sector",weight.var = NULL,
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

spec1 <- specFfm(data = facDat20, asset.var="Ticker", ret.var="Return",
              date.var="Date", exposure.vars = "Sector",weight.var = NULL,
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

