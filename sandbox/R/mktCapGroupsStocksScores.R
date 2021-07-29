library(FactorAnalytics)

# Make Market Cap Groups of stocksCRSP
data(stocksCRSP) # Load stocksCRSP into current session
stocks <- stocksCRSP
stocksMicro <- subset(stocks,CapGroup == "MicroCap",select = names(stocks))
stocksSmall <- subset(stocks,CapGroup == "SmallCap",select = names(stocks))
stocksMid <- subset(stocks,CapGroup == "MidCap",select = names(stocks))
stocksLarge <- subset(stocks,CapGroup == "LargeCap",select = names(stocks))

nMicro <- length(unique(stocksMicro$Ticker))
nSmall <- length(unique(stocksSmall$Ticker))
nMid <- length(unique(stocksMid$Ticker))
nLarge <- length(unique(stocksLarge$Ticker))
nAll <- c(nMicro,nSmall,nMid,nLarge)
sum(nAll)

# Make Market Cap Groups of scoresSPGMI
data(scoresSPGMI) # Load scoresSGMI into current session
scores <- scoresSPGMI
scoresMicro <- subset(scores,CapGroup == "MicroCap",select = names(scores))
scoresSmall <- subset(scores,CapGroup == "SmallCap",select = names(scores))
scoresMid <- subset(scores,CapGroup == "MidCap",select = names(scores))
scoresLarge <- subset(scores,CapGroup == "LargeCap",select = names(scores))

nMicro <- length(unique(scoresMicro$Ticker))
nSmall <- length(unique(scoresSmall$Ticker))
nMid <- length(unique(scoresMid$Ticker))
nLarge <- length(unique(scoresLarge$Ticker))
nAll <- c(nMicro,nSmall,nMid,nLarge)
sum(nAll)




