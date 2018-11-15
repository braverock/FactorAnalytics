#' @name factorDataSPGMI
#' @title Fundamental factor scores from S&P Global Market Intelligence
#' @description S&P Global Market Intelligence has kindly provided firm fundamentals data
#' referred to as scores or alpha factors for educational use in the open source factorAnalytics
#' R package. The data is contained in the R data frame object factorDataSPGMI
#' consisting of the following cross-section of scores for approximately 300 stocks from 1990 to
#' 2015: AccrualRatioCF, AnnVol12M, Beta60M, BP, Chg1YEPS, DivP, EBITDAEV, EP, EQ-style,
#' LogMktCap, PM12M1M, ROE. This data greatly facilitates the educational value to users of the
#' fundamental factor model in factorAnalytics. The package developers wish to thank S&P Global
#' Market Intelligence for contributing this data to the factorAnalytics package.
#' @docType data
#' @source S&P Global Market Intelligence
#' @usage data("factorDataSPGMI")
NULL

#' @name factorDataSetDjia
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

#' @name managers
#' @title time-series data
#' @description Hypothetical Alternative Asset Manager and Benchmark Data for Time Series Factor Model Fit
#' @docType data
#' @source TBA
#' @usage data("managers")
NULL

#' @name managers.ffm
#' @title managers data for ffm
#' @description Hypothetical Alternative Asset Manager and Benchmark Data for Time Series Factor Model Fit
#' @docType data
#' @source TBA
#' @usage data("managers.ffm")
NULL

#' @name mktUS
#' @title US Market Returns
#' @description Monthly returns including all distributions, on a value-weighted market portfolio of NYSE/AMEX/NASDAQ 
#' @docType data
#' @source WRDS
#' @usage data("mktUS")
NULL

#' @name mktSP
#' @title S&P 500 Returns
#' @description S&P 500 return from Yahoo
#' @docType data
#' @source Yahoo
#' @usage data("mktSP")
NULL

#' @name cusumData
#' @title Parvest and Russell2500
#' @description Data in the example is an xts object containing two monthly returns in each column.
#'              The first column is the fund returns and the second column is the
#'              benchmark returns. The data is from Jan 2005 to Dec 2003.
#' @docType data
#' @source TBA
#' @usage data("cusumData")
NULL

#' @name RussellData
#' @title Russell data
#' @description 16 Russell data
#' @docType data
#' @source TBA
#' @usage data("RussellData")
NULL

#' @name riskFreeRate
#' @title Risk-free rates
#' @description 10 year US Bond yields
#' @docType data
#' @source TBA
#' @usage data("riskFreeRate")
NULL
