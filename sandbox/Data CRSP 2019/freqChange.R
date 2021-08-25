# written by Daniel Xia, dansummer94@gmail.com

library(readxl)
library(xts)
library(lubridate)
library(PerformanceAnalytics)
library(quantmod)

rf_dly = read_excel("../../FactorAnalyticsData/risk-free 202010 output for Dec 1992 - Copy.xlsx")
rf_dly = xts(rf_dly[2:4], order.by = rf_dly$CALDT)

rf_mly = apply.monthly(rf_dly, Return.cumulative)
rf_wly = apply.weekly(rf_dly, Return.cumulative)

# convert indices
end_of_month = function(date) {
    date = as.Date(date)
    days = days_in_month(date)
    day(date) = days
    date
}

end_on_friday = function(date) {
    date = as.Date(date)
    day = wday(date, week_start = getOption("lubridate.week.start", 1))
    date + (5-day)
}

# do not use sapply to avoid error in date format
index(rf_mly) = do.call(c, lapply(index(rf_mly), end_of_month))
index(rf_wly) = do.call(c, lapply(index(rf_wly), end_on_friday))

rf_mly = rf_mly["1993/"]
rf_wly = rf_wly["1993/"]

write.zoo(rf_mly, file="../../FactorAnalyticsData/risk-free montly.xlsx", sep=",")
write.zoo(rf_wly, file="../../FactorAnalyticsData/risk-free weekly.csv", sep=",")