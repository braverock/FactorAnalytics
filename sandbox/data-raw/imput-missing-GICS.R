
data(factorsSPGMI)

# New govind GICS file
library(data.table)
GICS_govind <- fread('sandbox/data-raw/stocksTickers310GICSgovindSPGMI.csv')

# format
GICS_govind$`Start date` <- as.Date(GICS_govind$`Start date`,
                                    format = "%m/%d/%Y")
GICS_govind$`End Date` <- as.Date(GICS_govind$`End Date`,
                                  format = "%m/%d/%Y")
GICS_govind$GICS <- as.character(GICS_govind$GICS)

# remove irrelevant descriptors
GICS_govind <- GICS_govind[,-c(3,4,6)]
names(GICS_govind) <- c("Ticker","Company","GICS","Sector",
                    "StartDate","EndDate") 

 # subset by those with GICs NAs
SPGMI_noGICS <- factorsSPGMI[is.na(factorsSPGMI$GICS), ]
 
 # identify intersecting variables
intersect(names(factorsSPGMI), names(GICS_govind))
 # merge on the two variables that are complete
SPGMI_repair <- merge(x = SPGMI_noGICS, y = GICS_govind, 
                    by = c("Ticker","Company"),
                    all.x = TRUE)

# test the results of the process for NAs on the right GICS_govind side. 
  # 1911 identified.
sum(is.na(SPGMI_repair$GICS.y))
sum(is.na(SPGMI_repair$Sector.y))
# See which specific companies these relate to
unique(SPGMI_repair[is.na(SPGMI_repair$GICS.y), c("Ticker", "Company", "TickerLast")])

# drop old GICS columns with NAs as well as StartDate and EndDate.
SPGMI_repair <- SPGMI_repair[,-c("GICS.x","Sector.x","StartDate","EndDate")]
  names(SPGMI_repair) <- c("Ticker","Company", "Date", "TickerLast","CapGroup",
                           "AnnVol12M","Beta60M","BP","EP","LogMktCap","PM12M1M",        
                           "AccrualRatioCF","AstAdjChg1YOCF","CFROIC","Chg1YAstTo",
                           "EBITDAEV","FCFP","PM1M","SEV","GICS","Sector")

# row bind back into the original data set
  SPGMI_allGICS <- factorsSPGMI[!is.na(factorsSPGMI$GICS), ]

SPGMI_complete <- rbind(SPGMI_allGICS, SPGMI_repair)

# test the results of the process for NAs on the right GICS_govind side. 
# 1911 identified.
sum(is.na(SPGMI_complete$GICS))
sum(is.na(SPGMI_complete$Sector))
# See which specific companies these relate to
NA_index <- is.na(SPGMI_complete$GICS)
unique( SPGMI_complete[NA_index, c("Ticker", "Company", "TickerLast")] )

# how many sectors do we have now?
unique( SPGMI_complete$Sector)

  ########
 # CRSP #
########
  
  sum(unique(stocksCRSP$TickerLast) %in% unique(GICS_govind$Ticker))
setdiff(unique(GICS_govind$Ticker), unique(SPGMI_allGICS$TickerLast))
