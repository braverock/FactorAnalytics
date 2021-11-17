library(FactorAnalytics)
library(data.table)

data(stocksCRSP)
names(stocksCRSP)
data(scoresSPGMI)
names(scoresSPGMI)

commonNames <- intersect(names(stocksCRSP),names(scoresSPGMI))
facModDatmerged <- merge(stocksCRSP, scoresSPGMI, by = commonNames)

names(facModDatmerged)

colNames <- c("Date","TickerLast","CapGroup","Sector","Return","Ret13WkBill",         
              "mktIndexCRSP","BP","LogMktCap","SEV")

facModDat <- facModDatmerged[, .SD , .SDcols = colNames]
# change the names of some of the columns
setnames(facModDat, old = c("TickerLast"  , "Ret13WkBill" , "mktIndexCRSP" ,"LogMktCap"  ),
         new = c("Ticker","RF","MKT","SIZE"))
unique(facModDat[,Sector])
# get rid of NA sector
facDat <- facModDat[!is.na(Sector)] # 250 stocks
length(unique(facDat$Ticker))
facDatSmallCap <- facDat[CapGroup == "SmallCap"] # 68 smallcap stocks
length(unique(facDatSmall$Ticker))
facDatSmall[, CapGroup := NULL ] # Remove CapGroup variable
head(facDatSmall,2)
head(unique(facDatSmall$Date)) # Confirm that it is monthly data
facDat <-facDatSmall[facDatSmall$Date >= as.Date("2006-01-31") & 
                          facDatSmall$Date <= as.Date("2010-12-31"), ]
range(facDat$Date)

# Create an xts object returns for 20 stocks
library(PCRM)  # Had to add this because I removed library(PCRM)
returnMat = tapply(facDat[["Return"]],list(facDat$Date,facDat$Ticker),I)
returns = xts(returnMat,as.yearmon(rownames(returnMat)))
returns20 <- returns[,1:20]
names(returns20)
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

