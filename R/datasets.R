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
#' referred to as â€œscoresâ€� or â€œalpha factorsâ€� for educational use in the open
#' source FactorAnalytics R package. The data is contained in the R data frame
#' object â€œfactorDataSPGMIâ€� consisting of the following cross-section of scores
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

#' @name stocksCRSP
#' @title CRSP stocks data
#' @description stocksCRSP
#' @docType data
#' @source TBA
#' @usage data("stocksCRSP")
NULL


#' @name scoresSPGMI
#' @title cleaned SPGMI data
#' @description cleaned 300 stocks' 12 factor scores data
#' @docType data
#' @source S&P Global Market Intelligence
#' @usage data("scoresSPGMI")
NULL

#' @name TresauryYields
#' @title Tresaury Yields
#' @description
#' @docType data
#' @source 
#' @usage data("TresauryYields")
NULL

#' @name Stocks.df
#' @title Stocks.df
#' @description
#' @docType data
#' @source 
#' @usage data("Stocks.df")
NULL

#' @name StockReturns
#' @title StockReturns
#' @description
#' @docType data
#' @source 
#' @usage data("StockReturns")
NULL