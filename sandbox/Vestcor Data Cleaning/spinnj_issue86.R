# spinnj_issue86.R
#
# factorsSPGMI and stocksCRSP do not use official GICS naming conventions in 
# sector names. In addition, several securities have obvious misspellings in 
# Sector assignment, resulting in incorrect groupings. This appears to be a 
# result of human intervention/manual data manipulations after the data
# were obtained from CRSP and SPGMI.
#

# load data & create tmp copies
load("./data/factorsSPGMI.rda")
load("./data/stocksCRSP.rda")
factorsSPGMI_tmp <- factorsSPGMI 
stocksCRSP_tmp <- stocksCRSP

# confirm factorsSPGMI and stocksCRSP have incorrect sectors (misspelled)
unique(factorsSPGMI_tmp$Sector) # contains 16 sectors with dupes/misspellings
unique(stocksCRSP_tmp$Sector) # contains 16 sectors with dupes/misspellings

# replacement data as per issue #86 description
bad_sectors <- unique(factorsSPGMI_tmp$Sector)
good_sectors <- c("Information Technology","Industrials","Health Care",
                  "Consumer Staples","Energy","Materials",
                  "Consumer Discretionary","Communication Services","Utilities",
                  "Real Estate","Health Care","Financials",
                  "Consumer Discretionary","Information Technology",
                  "Consumer Staples","Communication Services")
sector_table <- data.frame(cbind(bad_sectors,good_sectors))
colnames(sector_table) <- c("BadSectors","GoodSectors")
sector_table

# replacements
factorsSPGMI_tmp$Sector <- sector_table$GoodSectors[match(factorsSPGMI_tmp$Sector,sector_table$BadSectors)]
stocksCRSP_tmp$Sector <- sector_table$GoodSectors[match(stocksCRSP_tmp$Sector,sector_table$BadSectors)]

# confirm factorsSPGMI and stocksCRSP have correct sectors
unique(factorsSPGMI_tmp$Sector) # contains 11 sectors with no dupes
unique(stocksCRSP_tmp$Sector) # contains 11 sectors with no dupes

# save data
factorsSPGMI <- factorsSPGMI_tmp
stocksCRSP <- stocksCRSP_tmp
# added compression args, using xz algorithm of the highest level
save(factorsSPGMI, file="./data/factorsSPGMI.rda", compress = "xz", compression_level = 9)
save(stocksCRSP, file="./data/stocksCRSP.rda", compress = "xz", compression_level = 9)
