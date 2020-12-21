#' @name FactorAnalytics-package
#' @title FactorAnalytics
#' @aliases FactorAnalytics-package FactorAnalytics
#'
#' @description The FactorAnalytics package contains fitting and analysis
#' methods for the three main types of factor models used in conjunction with
#' portfolio construction, optimization and risk management, namely
#' fundamental factor models, time series factor models and statistical factor
#' models. The purpose of this project is to add key improvements to the
#' package that will make it its basic features and capabilities close to
#' those of commercial portfolio optimization and risk management products.
#'
#' @section CRSP data included in FactorAnalytics:
#'
#' FactorAnalytics contains data contributed by the Center for
#' Research in Security Prices at the University of Chicago's Booth School of
#' Business (CRSP). The CRSP data is provided for use in the academic hardcopy
#' and ebook and for examples in the User Manual and Vignettes for, and
#' containment in, the R package FactorAnalytics. The CRSP-contributed datasets
#' are not covered by the GPL.  Use of the data in derivative works is not
#' permitted without the express written permission of CRSP.
#'
#' The package developers wish to thank S&P Global Markets and the Center for
#' Research in Security Prices for the generous inclusion of this data in
#' FactorAnalytics. This data greatly facilitates the research and educational
#' purpose of FactorAnalytics and allows users of the package to work with a
#' large dataset that while not independently economically meaningful, is still
#' large enough to demonstrate model construction of real models.
#'
#' @section Fundamental Factor Scores from S&P Global Market Intelligence:
#'
#' S&P Global Market Intelligence has kindly provided firm fundamentals data
#' referred to as “scores” or “alpha factors” for educational use in the open
#' source FactorAnalytics R package. The data is contained in the R data frame
#' object “factorDataSPGMI” consisting of the following cross-section of scores
#' for approximately 300 stocks from 1990 to 2015: AccrualRatioCF, AnnVol12M,
#' Beta60M, BP, Chg1YEPS, DivP, EBITDAEV, EP, EQ-style, LogMktCap, PM12M1M, ROE.
#' This data greatly facilitates the educational value to users of the
#' fundamental factor model in FactorAnalytics. The package developers wish to
#' thank S&P Global Market Intelligence for contributing this data to the
#' FactorAnalytics package. The S&P-contributed datasets are not covered by the
#' GPL.  Use of the data in derivative works is not permitted without the express
#' written permission of Standard and Poor Global Markets.
#'
#' @docType package
NULL

#' @name factorDataSPGMI
#' @title Fundamental factor scores from S&P Global Market Intelligence
#' @description S&P Global Market Intelligence has kindly provided firm fundamentals data
#' referred to as scores or alpha factors for educational use in the open source FactorAnalytics
#' R package. The data is contained in the R data frame object factorDataSPGMI
#' consisting of the following cross-section of scores for approximately 300 stocks from 1990 to
#' 2015: AccrualRatioCF, AnnVol12M, Beta60M, BP, Chg1YEPS, DivP, EBITDAEV, EP, EQ-style,
#' LogMktCap, PM12M1M, ROE. This data greatly facilitates the educational value to users of the
#' fundamental factor model in FactorAnalytics. The package developers wish to thank S&P Global
#' Market Intelligence for contributing this data to the FactorAnalytics package.
#' @docType data
#' @source S&P Global Market Intelligence
#' @usage data("factorDataSPGMI")
NULL

#' @name factorDataSetDjia14Yrs
#' @title DJIA stocks Compustat factors 14yrs
#' @description Contains returns for 30 DJIA stocks spanned across 9 Sectors -ENERGY, COSTAP, INDUS,T MATRLS,
#'               FINS, INFOTK, HEALTH, CODISC, and TELCOM stocks along with 4 factor data (MKTCAP, ENTVAL, P2B, EV2S, SIZE)
#'              starting from Jan 2000 to march 2013.
#'
#'              The 9 Sectors correspond to Energy,ConsumerStaples, Industrials, Materials, Financials,
#'              InformationTechnology, HealthCare, ConsumerDiscretionary and Telecommunications respectively.
#' @docType data
#' @source TBA
#' @usage data("factorDataSetDjia")
NULL

#' @name factorDataSetDjia5Yrs
#' @title DJIA stocks Compustat factors 5yrs
#' @description  Contains returns for 30 DJIA stocks spanned across 9 Sectors -ENERGY, COSTAP, INDUS,T MATRLS,
#'               FINS, INFOTK, HEALTH, CODISC, and TELCOM stocks along with 4 factor data (MKTCAP, ENTVAL, P2B, EV2S, SIZE)
#'               starting from  from Jan 2008 to Dec 2012.
#'
#'               The 9 Sectors correspond to Energy, ConsumerStaples, Industrials, Materials, Financials,
#'               InformationTechnology, HealthCare, ConsumerDiscretionary and Telecommunications respectively.
#' @docType data
#' @source TBA
#' @usage data("factorDataSetDjia5Yrs")
NULL

#' @name stocks145scores6
#' @title CRSP stocks Capital IQ scores
#' @description Contains returns for 145 stocks starting from Jan 1990 to Dec 2014 spanned across 10 Sectors-
#'              ENERGY, COSTAP, INDUS,T MATRLS, FINS, INFOTK, HEALTH, CODISC, UTILS and TELCOM
#'              along with 6 factors: ROE, BP, MOM121, SIZE, VOL121, EP
#'
#'              The 10 Sectors correspond to Energy, ConsumerStaples, Industrials, Materials, Financials,
#'              InformationTechnology, HealthCare, ConsumerDiscretionary,
#'              Utilities and Telecommunications respectively.
#'
#' @docType data
#' @source TBA
#' @usage data("stocks145scores6")
#'
#'
NULL

#' @name wtsDjiaGmv
#' @title DJIA GMV portfolio weights
#' @description Contains weights obtained after optimizing the portfolio returns of the 30 DJIA stocks (from dataset factorDataSetDjia5Yrs)
#'              for a global minimum variance portfolio starting from Jan 2008 to Dec 2012.
#' @docType data
#' @source TBA
#' @usage data("wtsDjiaGmv")
NULL

#' @name wtsDjiaGmvLo
#' @title DJIA GMV long-only portfolio weights
#' @description Contains weights obtained after optimizing the portfolio returns of the 30 DJIA stocks (from dataset factorDataSetDjia5Yrs)
#'              for a long-only global minimum variance portfolio starting from Jan 2008 to Dec 2012.
#' @docType data
#' @source TBA
#' @usage data("wtsDjiaGmvLo")
NULL

#' @name wtsStocks145Gmv
#' @title CRSP 145 stocks GMV portfolio weights
#' @description Contains weights obtained after optimizing the portfolio returns of 145 stocks (from dataset stocks145scores6)
#'              for a global minimum variance portfolio starting from Jan 1990 to Dec 2014.
#' @docType data
#' @source TBA
#' @usage data("wtsStocks145Gmv")
NULL

#' @name wtsStocks145GmvLo
#' @title CRSP 145 stocks GMV long-only weights
#' @description Contains weights obtained after optimizing the portfolio returns of 145 stocks (from dataset stocks145scores6)
#'              for a long-only global minimum variance portfolio starting from Jan 1990 to Dec 2014.
#' @docType data
#' @source TBA
#' @usage data("wtsStocks145GmvLo")
NULL

#' @name RussellData
#' @title Russell data
#' @description 16 Russell data
#' @docType data
#' @source TBA
#' @usage data("RussellData")
NULL

#' @name stocksCRSP
#' @title CRSP stocks data
#' @description stocksCRSP
#' @docType data
#' @source TBA
#' @usage data("stocksCRSP")
NULL

#' @name stocksCRSPscoresSPGMI
#' @title cleaned SPGMI and CRSP data
#' @description cleaned 300 stocks' 12 factor scores that merged with CRSP data
#' @docType data
#' @source TBA
#' @usage data("stocksCRSPscoresSPGMI")
NULL

#' @name stocksCRSPscoresSPGMIraw
#' @title raw SPGMI and CRSP data
#' @description raw 300 stocks' 12 factor scores that merged with CRSP data
#' @docType data
#' @source TBA
#' @usage data("stocksCRSPscoresSPGMIraw")
NULL

#' @name scoresSPGMI
#' @title cleaned SPGMI data
#' @description cleaned 300 stocks' 12 factor scores data
#' @docType data
#' @source S&P Global Market Intelligence
#' @usage data("scoresSPGMI")
NULL

#' @name scoresSPGMIraw
#' @title raw SPGMI data
#' @description raw 300 stocks' 12 factor scores data
#' @docType data
#' @source TBA
#' @usage data("scoresSPGMIraw")
NULL

#' @name scoresSPGMIraw
#' @title raw SPGMI data
#' @description raw 300 stocks' 12 factor scores data
#' @docType data
#' @source TBA
#' @usage data("scoresSPGMIraw")
NULL


#' @name stocksWithFactorsData300
#' @title 300 stocks with factors
#' @description the dataset has been replaced with stocksCRSPscoresSPGM
#' @docType data
#' @source TBA
#' @usage data("stocksWithFactorsData300")
NULL




