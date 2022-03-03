library(FactorAnalytics)
library(data.table)
data(stocksCRSP)
names(stocksCRSP)

# Probably add lines 7-12 to FFM Vignette
dim(stocksCRSP)  # Next lines to make dim more clear
range(stocksCRSP[,Date])
length(unique(stocksCRSP[,Ticker]))
(nTickerLast <- length(unique(stocksCRSP[,TickerLast])))
(nDates <- length(unique(stocksCRSP[,Date])))
nDates*nTickerLast
dim(stocksCRSP)

# Get last date
lastDateCRSP <- tail(stocksCRSP[,Date],1)
stocksCRSPLast <- stocksCRSP[Date == lastDateCRSP]

# Sector counts, messed up by multiple names for some sectors
stocksCRSPLast[ , .N, by = Sector] # Nice data.table feature

# Capgroup counts
stocksCRSPLast[ , .N, by = CapGroup]
# I will add a barplot version in the FFM Vignette
