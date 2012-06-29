# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either
# version 2 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Library General Public License for more details.
#
# You should have received a copy of the GNU General
# Public License along with this library; if not,
# go here: http://www.gnu.org/licenses/gpl.html
# or write to the Free Software Foundation, Inc.,
# 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

# Copyright 2006-2007 Peter Carl, Brian G. Peterson
# $Id: FF.R,v 1.1 2007/04/20 15:06:51 peter Exp $

################################################################################
# FUNCTIONS:
#
# download.FFfactors
################################################################################

download.FFfactors <-
function (start = "1926-07-01", end = Sys.Date(), url=
    "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors.zip", file="F-F_Research_Data_Factors.zip")
{ # @author Sandrine Dudoit, sandrine@stat.berkeley.edu, from "SMA" library
  # @author modified by Peter Carl

    # Description:

    # Downloads, unzips, and reads the Fama-French factors into a dataframe

    # Inputs:

    # start = start date of the data, defaults to the earliest date available in the FF file
    # end = last date of the data, defaults to two months ago
    # url = the URL to download the file from
    # file = the filename of the local copy of the downloaded file

    # Outputs:

    # A data frame of the Fama French factors

    # @todo: 

    rownames = seq(as.Date(start), end, by="month")
    download.file(url, file, mode='wb')
    unzip = unz(file,"F-F_Research_Data_Factors.txt")

    ffdata = read.table(unzip, header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE, skip=3, nrows = length(rownames)-2)
    ffdata = ffdata/100

    attach(ffdata)

    return(ffdata)
}

download.FFsize <-
function (start = "1926-07-01", end = Sys.Date(), url=
    "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Portfolios_Formed_on_ME.zip", file="Portfolios_Formed_on_ME.zip")
{ # @author Peter Carl

    # Downloads, unzips, and reads the Fama-French data into a dataframe
    # Returns of portfolios formed on Market Equity, which is price times 
    # shares outstanding.  Price is from CRSP, shares outstanding are from 
    # Compustat (if available) or CRSP.
    # This function downloads the value weighted returns rather than the 
    # equal weighted returns, which are in the same file.
    # Updated annually

    rownames = seq(as.Date(start), end, by="month")
    download.file(url, file, mode='wb')
    unzip = unz(file,"Portfolios_Formed_on_ME.txt")
    currentmonth = as.numeric(format(Sys.Date(),"%m"))
    ffdata = read.table(unzip, header=FALSE, sep="", na.strings="NA", dec=".", strip.white=TRUE, skip=13, nrows = (length(rownames)-currentmonth))
    ffdata = ffdata[,3:5]
    colnames(ffdata)= c("Small","Mid","Large")
    rownames(ffdata)= as.character(rownames[1:(length(rownames)-currentmonth)])
    ffdata = ffdata/100
    #attach(ffdata)
    return(ffdata)
}

download.FFsizestyle <-
function (start = "1926-07-01", end = Sys.Date(), url=
    "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Benchmark_Portfolios_Monthly.zip", file="F-F_Benchmark_Portfolios_Monthly.zip")
{ # @author Peter Carl

    # Downloads, unzips, and reads the Fama-French data into a dataframe
    # Returns of portfolios formed on six size-style portfolios
    # The Fama/French benchmark portfolios are rebalanced quarterly using 
    # two independent sorts, on size (market equity, ME) and book-to-market 
    # (the ratio of book equity to market equity, BE/ME). The size breakpoint 
    # (which determines the buy range for the Small and Big portfolios) is the
    #  median NYSE market equity. The BE/ME breakpoints (which determine the 
    # buy range for the Growth, Neutral, and Value portfolios) are the 30th 
    # and 70th NYSE percentiles.
    # Updated monthly, second week of the month

    # http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/Data_Library/f-f_portfolios.html

    # Sm Value    | Lg Value
    # ------------+--------------70th BE/ME Percentile
    # Sm Neutral  | Lg Neutral
    # ------------+--------------30th BE/ME Percentile
    # Sm Growth   | Lg Growth
    #             ^Median ME

    rownames = seq(as.Date(start), end, by="month")
    download.file(url, file, mode='wb')
    unzip = unz(file,"F-F_Benchmark_Portfolios_Monthly.txt")

    ffdata = read.table(unzip, header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE, skip=0, nrows = length(rownames)-2)
    colnames(ffdata)= c("Large Growth", "Large Neutral", "Large Value", "Small Growth","Small Neutral","Small Value")
    rownames(ffdata)= as.character(rownames[1:(length(rownames)-2)])
    ffdata = ffdata/100
    #attach(ffdata)
    return(ffdata)
}

download.FF12industry <-
function (start = "1926-07-01", end = Sys.Date(), url=
    "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/12_Industry_Portfolios.zip", file="12_Industry_Portfolios.zip")
{ # @author Peter Carl

    # Downloads, unzips, and reads the Fama-French data into a dataframe
    # Returns of portfolios formed on twelve industry portfolios
    # Updated annually

    rownames = seq(as.Date(start), end, by="month")
    download.file(url, file, mode='wb')
    unzip = unz(file,"12_Industry_Portfolios.txt")

    currentmonth = as.numeric(format(Sys.Date(),"%m"))
    ffdata = read.table(unzip, header=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE, skip=11, nrows = (length(rownames)-currentmonth))
    rownames(ffdata)= as.character(rownames[1:(length(rownames)-currentmonth)])
    ffdata = ffdata/100
    #attach(ffdata)
    return(ffdata)
}

###############################################################################
# R (http://r-project.org/) Econometrics for Performance and Risk Analysis
#
# Copyright (c) 2004-2007 Peter Carl and Brian G. Peterson
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file COPYING
#
# $Id: FF.R,v 1.1 2007/04/20 15:06:51 peter Exp $
#
################################################################################
# $Log: FF.R,v $
# Revision 1.1  2007/04/20 15:06:51  peter
# - contributing a collection of FF functions to CVS
#
#
################################################################################