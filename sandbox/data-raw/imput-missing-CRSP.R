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

#################################
### Save Results to pkg /data ##
###############################
# Check for object efficiency, and hack away a few kbs.
attributes(CRSP_complete)
# rownames appear to be characters which spend more bites than integers
str(attr(CRSP_complete, "row.names"))
class(attr(CRSP_complete, "row.names")) == "integer"

# save results to stocksCRSP, and write to data/ folder.
stocksCRSP <- CRSP_complete
save(stocksCRSP, file = "data/stocksCRSP.rda", compress = "xz", 
     compression_level = 9)

# Explore missing Sectors 
sum(is.na(stocksCRSP$Sector)) # 13,524 observations with missing sectors
# Lets see which specific securities
sectors_missing <- stocksCRSP[is.na(stocksCRSP$Sector),]
sec_missing_df <- unique(sectors_missing[,c("Company", "TickerLast", 
                                            "GICS", "Sector")])
View(sec_missing_df)




#####################################################################
# Explore legacy mismatches between TickerLast, Ticker and GICS. ##
###################################################################
# merge by company name
load("~/R/FactorAnalytics/sandbox/data/legacy-stocksCRSP-missing-GICS.rda")
SPGMI_repair <- merge(x = stocksCRSP, y = GICS_govind, 
                      by.x = "Company",
                      by.y = "Company Name")

# Ticker mismatches and changes over time
sum(SPGMI_repair$TickerLast != SPGMI_repair$Ticker.x) # 11,529 aren't equal
sum(SPGMI_repair$Ticker.y != SPGMI_repair$Ticker.x) # 11,529 aren't equal
sum(SPGMI_repair$TickerLast != SPGMI_repair$Ticker.y) # all equal
# which ones observations have changed overtime or aren't equal?
ticker_mismatch <- SPGMI_repair[SPGMI_repair$Ticker.y != SPGMI_repair$Ticker.x,]
tkr_mismatch_df <- unique(ticker_mismatch[,c("Company", 
                                             "TickerLast", "Ticker.x", "Ticker.y", 
                                             "GICS.x", "GICS.y", 
                                             "Sector.x", "Sector.y", 
                                             "Start date", "End Date")])
View(tkr_mismatch_df)

# any GICS mismatches between stocksCRSP$GICS & GICS_govind$GICS?
gics_mismatch <- SPGMI_repair[SPGMI_repair$GICS.x != SPGMI_repair$GICS.y,]

gics_mis_df <- unique(gics_mismatch[,c("Company", "TickerLast", "Ticker.x","Ticker.y", 
                                       "GICS.x", "Sector.x", "GICS.y", "Sector.y", 
                                       "Start date", "End Date")])
View(gics_mis_df)


