
load("~/R/FactorAnalytics/sandbox/data/factorsSPGMI-missing-GICS.rda")
load("~/R/FactorAnalytics/sandbox/data/stocksCRSP-missings-GICS.rda")

# Old GICS
library(readxl)
GICS_v1 <- read_excel("sandbox/data-raw/stocksTickers310GICS_v1.xlsx", 
                      col_types = c("text", "text", "numeric", "text", 
                                    "text", "text", "date", "date"), 
                      skip = 2)

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

# drop old GICS columns with NAs and fill them in with 
SPGMI_repair <- SPGMI_repair[,-c("GICS.x","Sector.x")]
  names(SPGMI_repair) <- c("Ticker","Company", "Date", "TickerLast","CapGroup",
                           "AnnVol12M","Beta60M","BP","EP","LogMktCap","PM12M1M",        
                           "AccrualRatioCF","AstAdjChg1YOCF","CFROIC","Chg1YAstTo",
                           "EBITDAEV","FCFP","PM1M","SEV","GICS","Sector",
                           "StartDate","EndDate")

# row bind back into the original data set
  SPGMI_allGICS <- factorsSPGMI[!is.na(factorsSPGMI$GICS), ]
SPGMI_allGICS$StartDate <- NA  
SPGMI_allGICS$EndDate <- NA  

SPGMI_complete <- rbind(SPGMI_allGICS, SPGMI_repair)

  ########
 # CRSP #
########
  
  sum(unique(stocksCRSP$TickerLast) %in% unique(GICS_govind$Ticker))
setdiff(unique(GICS_govind$Ticker), unique(stocksCRSP$TickerLast))
