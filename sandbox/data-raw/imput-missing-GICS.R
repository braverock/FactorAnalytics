############
# Load data #
##############

# load existing SPGMI data
data(factorsSPGMI)
# load the GICS file
library(data.table)
GICS_govind <- fread('sandbox/data-raw/stocksTickers310GICSgovindSPGMI.csv')

#########################
# Format and clean GICS #
#########################

GICS_govind$GICS <- as.character(GICS_govind$GICS)
GICS_govind$Sector <- ifelse(GICS_govind$Sector == "", NA, GICS_govind$Sector)

GICS_govind$`Start date` <- as.Date(GICS_govind$`Start date`,
                                    format = "%m/%d/%Y")
GICS_govind$`End Date` <- as.Date(GICS_govind$`End Date`,
                                  format = "%m/%d/%Y")
# remove less relevant descriptors variables
GICS_govind <- GICS_govind[,-c("CIQ Company ID", "CIQ Company Name")]

##########################
# tests for completeness #
##########################
sum(unique(factorsSPGMI$TickerLast) %in% unique(GICS_govind$Ticker))
setdiff(unique(GICS_govind$Ticker),unique(factorsSPGMI$TickerLast))

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

# Check for object efficiency, and hack away a few kbs where you can!
attributes(SPGMI_complete)
# rownames appear to be characters which spend more bites than integers
str(row.names(SPGMI_complete))
# reassign as integers
attr(SPGMI_complete, "row.names") <- as.integer(row.names(SPGMI_complete))
# save results to factorsSPGMI, and write to data/ folder.
factorsSPGMI <- SPGMI_complete

save(factorsSPGMI, file = "data/factorsSPGMI.rda", compress = "xz", 
     compression_level = 9)

#####################################################################
# Explore mismatches between TickerLast, Ticker.x/y and GICS.x/y ##
###################################################################

# merge by company name
SPGMI_repair <- merge(x = factorsSPGMI, y = GICS_govind, 
                      by.x = c("Company"),
                      by.y = c("Company Name"))

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

# any GICS mismatches between factorsSPGMI$GICS & GICS_govind$GICS?
gics_mismatch <- SPGMI_repair[SPGMI_repair$GICS.x != SPGMI_repair$GICS.y,]

gics_mis_df <- unique(gics_mismatch[,c("Company", "TickerLast", "Ticker.x","Ticker.y", 
                                "GICS.x", "Sector.x", "GICS.y", "Sector.y", 
                                 "Start date", "End Date")])
View(gics_mis_df)

  ########
 # CRSP #
########
  
  sum(unique(stocksCRSP$TickerLast) %in% unique(GICS_govind$Ticker))
setdiff(unique(GICS_govind$Ticker), unique(SPGMI_allGICS$TickerLast))
