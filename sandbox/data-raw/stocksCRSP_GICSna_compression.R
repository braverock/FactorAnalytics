## Adjust the for GICS missing values, to convert original
library(FactorAnalytics)
data("stocksCRSP")
head(stocksCRSP$GICS)

## convert factor to character
stocksCRSP$GICS <- as.character(stocksCRSP$GICS)

## handle missing GICS values, transform "-" to NA.
GICS_NA <- is.na(gsub("-", NA, stocksCRSP$GICS))
 # Test: how many stocks? Doug thinks its over 50 missing GICS values
 NROW(unique(stocksCRSP[GICS_NA, "Ticker"]))
 # convert
  stocksCRSP$GICS <- gsub("-", NA, stocksCRSP$GICS)

## Write to file
save(stocksCRSP, file = paste0("data/stocksCRSP.rda"), 
     compress = "xz", compression_level = 9)
