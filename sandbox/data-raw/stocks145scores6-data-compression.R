# Read in data with base
stocks145scores6 <- read.csv("~/R/FactorAnalytics/sandbox/stocks145scores6.csv")

# Format DATE variable
stocks145scores6$DATE <- as.Date(stocks145scores6$DATE)

## Write to file
save(stocks145scores6, file = paste0("data/stocks145scores6.rda"), 
     compress = "xz", compression_level = 9)