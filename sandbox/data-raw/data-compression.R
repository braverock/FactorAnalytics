## Load and compress all data sets in the data directory over 1MB

# View datasets in data directory
file.info(list.files("data/", full.names=TRUE))

# extact file names with size over 1.7 MB
datasets <- file.info(list.files("data/", full.names=TRUE))

datasets1MB <- rownames(datasets)[datasets$size > 1700000]
datasets1MB
## interatively load datasets over 1.7 MB
for (i in datasets1MB) load(i)

# resave and compress all datasets

for (i in c("scoresSPGMI", "stocksCRSP")) {
    
    save(list = i, file = paste0("data/", i, ".rda"), compress = "xz", compression_level = 9)
    
}

# issue with name in stock and Stocks.df.rda. Keep for compatibility
save(stock, file = "data//Stocks.df.rda", compress = "xz", compression_level = 9)

# Change of socresSPGMI to factorsSPGMI
factorsSPGMI <- scoresSPGMI
save(factorsSPGMI, file = "data/factorsSPGMI.rda", compress = "xz", 
     compression_level = 9)

