############
# Load data #
##############
# load existing CRSP data
data("stocksCRSP")
library(data.table)
GICS_govind <- fread('sandbox/data-raw/stocksTickers310GICSgovindSPGMI.csv')

# Format and clean GICS #
GICS_govind$GICS <- as.character(GICS_govind$GICS)
GICS_govind$Sector <- ifelse(GICS_govind$Sector == "", NA, GICS_govind$Sector)
GICS_govind$`Start date` <- as.Date(GICS_govind$`Start date`,
                                    format = "%m/%d/%Y")
GICS_govind$`End Date` <- as.Date(GICS_govind$`End Date`,
                                  format = "%m/%d/%Y")
# remove less relevant descriptors variables
GICS_govind <- GICS_govind[,-c("CIQ Company ID", "CIQ Company Name")]

# Initial tests for completeness #
sum(unique(stocksCRSP$TickerLast) %in% unique(GICS_govind$Ticker))
setdiff(unique(GICS_govind$Ticker), unique(stocksCRSP$TickerLast))

##############################################
# Imput GICS into missing stocksCRSP$GICS #
############################################

CRSP_noGICS <- stocksCRSP[is.na(stocksCRSP$GICS), ]
# merge on the two variables that are complete
CRSP_repair <- merge(x = CRSP_noGICS, y = GICS_govind, 
                      by.x = c("TickerLast","Company"),
                      by.y = c("Ticker","Company Name")
                     )

# test the results of the process for NAs on the right GICS_govind side. 
sum(is.na(CRSP_repair$GICS.x)) # should be 13800
sum(is.na(CRSP_repair$Sector.x)) # should be 13800
sum(is.na(CRSP_repair$GICS.y)) # should be 0
sum(is.na(CRSP_repair$Sector.y)) # should be 13524

# drop merged GICS.x and Sector.x of all NAs
CRSP_repair <- CRSP_repair[,-c("GICS.x","Sector.x")]
# rename GICS.y and Sector.y to GICS and Sector
colnames(CRSP_repair)[which(colnames(CRSP_repair) =='GICS.y')] <- "GICS"
colnames(CRSP_repair)[which(colnames(CRSP_repair) =='Sector.y')] <- "Sector"
# With formatted columns, now row bind onto stocksCRSP data that is complete
CRSP_allGICS <- stocksCRSP[!is.na(stocksCRSP$GICS), ]
CRSP_complete <- rbind(CRSP_allGICS, CRSP_repair, fill=TRUE)
# Return names and order or original stocksCRSP state
# drop "GICS Descriptor" "Start date" "End Date"  
CRSP_complete <- CRSP_complete[,-c("GICS Descriptor","Start date","End Date")]

# test the results of the process for NAs #
apply(CRSP_complete, 2, function(x) sum(is.na(x)))
# Results: 0 missing values except "Sector", which has 13524

