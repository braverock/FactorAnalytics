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
# Imput GICS into missing factorsSPGMI$GICS #
############################################

SPGMI_noGICS <- factorsSPGMI[is.na(factorsSPGMI$GICS), ]
# merge on the two variables that are complete
SPGMI_repair <- merge(x = SPGMI_noGICS, y = GICS_govind, 
                      by.x = c("TickerLast","Company"),
                      by.y = c("Ticker","Company Name"))

# test the results of the process for NAs on the right GICS_govind side. 
sum(is.na(SPGMI_repair$GICS.x)) # should be 13800
sum(is.na(SPGMI_repair$Sector.x)) # should be 13800
sum(is.na(SPGMI_repair$GICS.y)) # should be 0
sum(is.na(SPGMI_repair$Sector.y)) # should be 13524

# drop merged GICS.x and Sector.x of all NAs
SPGMI_repair <- SPGMI_repair[,-c("GICS.x","Sector.x")]
# rename GICS.y and Sector.y to GICS and Sector
colnames(SPGMI_repair)[which(colnames(SPGMI_repair) =='GICS.y')] <- "GICS"
colnames(SPGMI_repair)[which(colnames(SPGMI_repair) =='Sector.y')] <- "Sector"
# With formatted columns, now row bind onto factorsSPGMI data that is complete
SPGMI_allGICS <- factorsSPGMI[!is.na(factorsSPGMI$GICS), ]
SPGMI_complete <- rbind(SPGMI_allGICS, SPGMI_repair, fill=TRUE)
# Return names and order or original factorsSPGMI state
# drop "GICS Descriptor" "Start date" "End Date"  
SPGMI_complete <- SPGMI_complete[,-c("GICS Descriptor","Start date","End Date")]

# test the results of the process for NAs #
apply(SPGMI_complete, 2, function(x) sum(is.na(x)))
# Results: 0 missing values except "Sector", which has 13524