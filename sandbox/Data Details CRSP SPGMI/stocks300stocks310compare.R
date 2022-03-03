library(data.table)
library(FactorAnalytics)
data(stocksCRSP)
gics <- fread('stocksTickers310GICSgovindSPGMI.csv')
# unique(stocksCRSP$TickerLast) %in% unique(gics$Ticker)
sum(unique(stocksCRSP$TickerLast) %in% unique(gics$Ticker))
setdiff(unique(gics$Ticker),unique(stocksCRSP$TickerLast))

