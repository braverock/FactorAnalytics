# to do: clean data in stocksCRSP and factorsSPGMI
### 1. clean sector names
### 2. delete 7 securities in financials, real estate, and utilities sectors
### 3. fix cap group assignments and create CapGroup and CapGroupL variables
### 4. rename "TickerLast" as "TickerL"

load("C:/FA/FactorAnalytics/data/factorsSPGMI.rda")
load("C:/FA/FactorAnalytics/data/stocksCRSP.rda")

factorsSPGMI_tmp <- factorsSPGMI
stocksCRSP_tmp <- stocksCRSP

### 1. clean up list of sectors for factorsSPGMI & stocksCRSP

# confirm factorsSPGMI and stocksCRSP have incorrect sectors (misspelled)
unique(factorsSPGMI_tmp$Sector) # contains 16 sectors with dupes
unique(stocksCRSP_tmp$Sector) # containts 16 sectors with dupes

# replacement data
bad_sectors <- unique(factorsSPGMI_tmp$Sector)
good_sectors <- c("InfoTech","Industrials","Healthcare","ConsumStap","Energy",
                  "Materials","ConsumDisc","ComServices","Utilities",
                  "RealEstate","Healthcare","Financials","ConsumDisc",
                  "InfoTech","ConsumStap","ComServices")
sector_table <- data.frame(cbind(bad_sectors,good_sectors))
colnames(sector_table) <- c("BadSectors","GoodSectors")

# replacements
factorsSPGMI_tmp$Sector <- sector_table$GoodSectors[match(factorsSPGMI_tmp$Sector,sector_table$BadSectors)]
stocksCRSP_tmp$Sector <- sector_table$GoodSectors[match(stocksCRSP_tmp$Sector,sector_table$BadSectors)]

# confirm factorsSPGMI and stocksCRSP have correct sectors
unique(factorsSPGMI_tmp$Sector) # contains 11 sectors with no dupes
unique(stocksCRSP_tmp$Sector) # containts 11 sectors with no dupes

### 2. delete 7 securities to leave temp sample of 293 stocks in 8 sectors (no
### financials, utilities, or real estate)

# get tickers to remove
to_remove <- unique(factorsSPGMI_tmp[factorsSPGMI_tmp$Sector %in% c("Financials","RealEstate","Utilities"),]$TickerLast)
to_remove_crsp <- unique(stocksCRSP_tmp[stocksCRSP_tmp$Sector %in% c("Financials","RealEstate","Utilities"),]$TickerLast)
(to_remove == to_remove_crsp) # confirm same stocks to be removed from both data sets

# check size of factorsSPGMI and stocksCRSP
dim(factorsSPGMI_tmp)[1] # 276 * 300 = 82800 rows
dim(stocksCRSP_tmp)[1] # 276 * 300 = 82800 rows

# delete members of to_remove from factorsSPGMI and stocksCRSP
stocksCRSP_tmp <- stocksCRSP_tmp[!(stocksCRSP_tmp$TickerLast %in% to_remove),]
factorsSPGMI_tmp <- factorsSPGMI_tmp[!(factorsSPGMI_tmp$TickerLast %in% to_remove),]

# check size of factorsSPGMI and stocksCRSP & same membership
dim(factorsSPGMI_tmp)[1] # 276 * 293 = 80868 rows
dim(stocksCRSP_tmp)[1] # 276 * 300 = 80868 rows
unique(sort(stocksCRSP_tmp$TickerLast)) == unique(sort(factorsSPGMI_tmp$TickerLast))

### 3. fix cap group assignments and create CapGroup and CapGroupL variables
# cap group membership deemed incorrect, to be replaced with new assignments.
# Suggest a point-in-time replacement based on CRSP 70/85/98 percentile splits
# into LargeCap (up to 75% mkt coverage), MidCap (75% to 85%), SmallCap (85% to 
# 98%), and MicroCap (above 98%), replace current CapGroup with reassigned data 
# at each point in time (e.g. membership changes); create new variable CapGroupL

# import market percentiles (data from from CompuStat via Vestcor)
MktCapPercentiles <- read.csv("MktCapPercentiles.csv")
factorsSPGMI_tmp$MktCap <- exp(factorsSPGMI_tmp$LogMktCap)

factorsSPGMI_tmp$CapGroupLC <- MktCapPercentiles$LC[match(as.Date(factorsSPGMI_tmp$Date),as.Date(MktCapPercentiles$Date))]
factorsSPGMI_tmp$CapGroupMC <- MktCapPercentiles$MC[match(as.Date(factorsSPGMI_tmp$Date),as.Date(MktCapPercentiles$Date))]
factorsSPGMI_tmp$CapGroupSC <- MktCapPercentiles$SC[match(as.Date(factorsSPGMI_tmp$Date),as.Date(MktCapPercentiles$Date))]

factorsSPGMI_tmp$CapGroup <- ifelse(factorsSPGMI_tmp$MktCap < factorsSPGMI_tmp$CapGroupSC,"MicroCap",
                                    ifelse(factorsSPGMI_tmp$MktCap < factorsSPGMI_tmp$CapGroupMC,"SmallCap",
                                           ifelse(factorsSPGMI_tmp$MktCap < factorsSPGMI_tmp$CapGroupLC,"MidCap",
                                                  "LargeCap")))

# create CapGroupL variable
end_dat <- factorsSPGMI_tmp[factorsSPGMI_tmp$Date == "2015-12-31",]
factorsSPGMI_tmp$CapGroupL <- end_dat$CapGroup[match(factorsSPGMI_tmp$TickerLast,end_dat$TickerLast)]


# match CapGroup and CapGroupL in stocksCRSP
factorsSPGMI_tmp$MatchVar <- paste(factorsSPGMI_tmp$Date,factorsSPGMI_tmp$Ticker,sep="-")
stocksCRSP_tmp$MatchVar <- paste(stocksCRSP_tmp$Date,stocksCRSP_tmp$Ticker,sep="-")
stocksCRSP_tmp$CapGroup <- factorsSPGMI_tmp$CapGroup[match(stocksCRSP_tmp$MatchVar,factorsSPGMI_tmp$MatchVar)]
stocksCRSP_tmp$CapGroupL <- factorsSPGMI_tmp$CapGroupL[match(stocksCRSP_tmp$MatchVar,factorsSPGMI_tmp$MatchVar)]

# reorder columns
factorsSPGMI_tmp <- factorsSPGMI_tmp[,c("Date","Ticker","TickerLast","Company",
                                        "CapGroup","CapGroupL","GICS",
                                        "Sector","AnnVol12M","Beta60M","BP","EP",
                                        "LogMktCap","PM12M1M","AccrualRatioCF",
                                        "AstAdjChg1YOCF","CFROIC","Chg1YAstTo",
                                        "EBITDAEV","FCFP","PM1M","SEV")]

stocksCRSP_tmp <- stocksCRSP_tmp[,c("Date","Ticker","TickerLast","Company",
                                    "CapGroup","CapGroupL","GICS","Sector","Return",
                                    "RetExDiv","Price","PrcSplitAdj","Ret4WkBill",
                                    "Ret13WkBill","Ret1YrBill","mktIndexCRSP")]

### 4. rename TickerLast as TickerL
names(factorsSPGMI_tmp)[names(factorsSPGMI_tmp) == "TickerLast"] <- "TickerL"
names(stocksCRSP_tmp)[names(stocksCRSP_tmp) == "TickerLast"] <- "TickerL"


### 5. save data
#setwd("C:/FA/FactorAnalytics/data")
factorsSPGMI <- factorsSPGMI_tmp
stocksCRSP <- stocksCRSP_tmp
save(factorsSPGMI,file="factorsSPGMI.rda")
save(stocksCRSP,file="stocksCRSP.rda")
