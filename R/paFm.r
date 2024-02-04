#' @title Compute cumulative mean attribution for factor models
#'
#' @description Decompose total returns into returns attributed to factors and
#' specific returns. An object of class \code{"pafm"} is generated, with
#' methods for generic functions \code{plot}, \code{summary} and \code{print}.
#'
#' @details Total returns can be decomposed into returns attributed to factors
#' and specific returns. \cr \eqn{R_t = \sum  b_k * f_kt + u_t, t=1...T} \cr
#' \code{b_k} is exposure to factor k and \code{f_kt} is factor k's return at
#' time t. The return attributed to factor k is \code{b_k * f_kt} and specific
#' return is \code{u_t}.
#'
#' @importFrom PerformanceAnalytics checkData Return.cumulative chart.TimeSeries
#' @importFrom xts xts
#' @importFrom zoo index
#' @importFrom methods is
#'
#' @param fit an object of class \code{tsfm}, \code{sfm} or \code{ffm}.
#' @param ... other arguments/controls passed to the fit methods.
#'
#' @return The returned object is of class \code{"pafm"} containing
#' \item{cum.ret.attr.f}{N X K matrix of cumulative return attributed to
#' factors.}
#' \item{cum.spec.ret}{length-N vector of cumulative specific returns.}
#' \item{attr.list}{list of time series of attributed returns for every
#' portfolio.}
#'
#' @author Yi-An Chen and Sangeetha Srinivasan
#'
#' @references Grinold, R. and Kahn, R. (1999) Active Portfolio Management: A
#' Quantitative Approach for Producing Superior Returns and Controlling Risk.
#' McGraw-Hill.
#'
#' @seealso \code{\link{fitTsfm}}, \code{\link{fitFfm}}
#' for the factor model fitting functions.
#'
#' The \code{pafm} methods for generic functions:
#' \code{\link{plot.pafm}}, \code{\link{print.pafm}} and
#' \code{\link{summary.pafm}}.
#'
#' @examples
#' data(managers, package = 'PerformanceAnalytics')
#' fit <- fitTsfm(asset.names=colnames(managers[, (1:6)]),
#'                factor.names=c("EDHEC LS EQ","SP500 TR"),
#'                data=managers)
#' # without benchmark
#' paFm(fit)
#'
#' @export
#'

paFm <- function(fit, ...) {

  # check input object validity
  if (!inherits(fit, c("tsfm", "sfm", "ffm"))) {
    stop("Invalid argument: fit should be of class 'tsfm', 'sfm' or 'ffm'.")
  }

  # TSFM chunk

  if (is(fit)=="tsfm") {

    # return attributed to factors
    cum.attr.ret <- fit$beta
    cum.spec.ret <- fit$alpha
    factorNames <- fit$factor.names
    fundNames <- fit$asset.names

    attr.list <- list()

    for (k in fundNames) {
      fit.lm <- fit$asset.fit[[k]]

      ## extract information from lm, lmRob or lars object
      reg.xts <- na.omit(fit$data[, c(k, factorNames)])
      dates <- as.Date(zoo::index(reg.xts))
      actual.xts <- xts::xts(fit.lm$model[1], dates)
      # attributed returns
      # active portfolio management p.512 17A.9
      # top-down method

      cum.ret <- PerformanceAnalytics::Return.cumulative(actual.xts)
      # setup initial value
      attr.ret.xts.all <- xts::xts(order.by = dates)

      for ( i in factorNames ) {

        if (is.na(fit$beta[k, i])) {
          cum.attr.ret[k, i] <- NA
          attr.ret.xts.all <- xts::xts(x = rep(NA, length(dates)),
                                  order.by = dates)

        } else {
          attr.ret.xts <- actual.xts -
            xts::xts(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]), dates)
          cum.attr.ret[k, i] <- cum.ret -
            PerformanceAnalytics::Return.cumulative(actual.xts-attr.ret.xts)
          attr.ret.xts.all <- merge(attr.ret.xts.all, attr.ret.xts)
        }
      }

      # specific returns
      spec.ret.xts <- actual.xts -
        xts::xts(as.matrix(fit.lm$model[,factorNames])%*%as.matrix(fit.lm$coef[-1]),
            dates)
      cum.spec.ret[k,1] <- cum.ret - PerformanceAnalytics::Return.cumulative(actual.xts-spec.ret.xts)
      attr.list[[k]] <- merge(attr.ret.xts.all, spec.ret.xts)
      colnames(attr.list[[k]]) <- c(factorNames, "specific.returns")
    }
  }

  if (is(fit)=="ffm" ) {
    # if benchmark is provided
    #       if (!is.null(benchmark)) {
    #         stop("use fitFundamentalFactorModel instead")
    #       }
    # return attributed to factors
    factor.returns <- fit$factor.returns[, -1]
    factor.names <- colnames(fit$beta)
    date <- zoo::index(factor.returns)
    ticker <- fit$asset.names

    #cumulative return attributed to factors
    if (factor.names[1] == "(Intercept)") {
      # discard intercept
      cum.attr.ret <- matrix(, nrow=length(ticker), ncol=length(factor.names),
                             dimnames=list(ticker, factor.names))[, -1]
    } else {
      cum.attr.ret <- matrix(, nrow=length(ticker), ncol=length(factor.names),
                             dimnames=list(ticker, factor.names))
    }
    cum.spec.ret <- rep(0, length(ticker))
    names(cum.spec.ret) <- ticker

    # make list of every asstes and every list contains return attributed to
    # factors and specific returns
    attr.list <- list()

    for (k in ticker) {
      idx <- which(fit$data[, fit$assetvar]== k)
      returns <- fit$data[idx, fit$returnsvar]
      num.f.names <- intersect(fit$exposure.names, factor.names)

      # check if there is industry factors
      if (length(setdiff(fit$exposure.names, factor.names)) > 0) {
        ind.f <- matrix(rep(fit$beta[k, ][-(1:length(num.f.names))],
                            length(idx)), nrow=length(idx), byrow=TRUE)
        colnames(ind.f) <- colnames(fit$beta)[-(1:length(num.f.names))]
        exposure <- cbind(fit$data[idx, num.f.names], ind.f)
      } else {
        exposure <- fit$data[idx, num.f.names]
      }

      attr.factor <- exposure * coredata(factor.returns)
      specific.returns <- returns - apply(attr.factor, 1, sum)
      attr <- cbind(attr.factor, specific.returns)
      attr.list[[k]] <- xts::xts(attr, as.Date(date))
      cum.attr.ret[k, ] <- apply(attr.factor, 2, PerformanceAnalytics::Return.cumulative)
      cum.spec.ret[k] <- PerformanceAnalytics::Return.cumulative(specific.returns)
    }
  }

  if (is(fit)=="sfm") {

    # return attributed to factors
    cum.attr.ret <- fit$loadings
    cum.spec.ret <- fit$r2
    factorNames <- colnames(fit$loadings)
    fundNames <- rownames(fit$loadings)
    data <- PerformanceAnalytics::checkData(fit$data)
    # create list for attribution
    attr.list <- list()
    # pca method

    if ( dim(fit$data)[1] > dim(fit$data)[2] ) {

      for (k in fundNames) {
        fit.lm <- fit$asset.fit[[k]]
        ## extract information from lm object
        date <- zoo::index(data[,k])
        # probably needs more general Date setting
        actual.xts <- xts::xts(fit.lm$model[1], as.Date(date))
        # attributed returns
        # active portfolio management p.512 17A.9
        cum.ret <-   PerformanceAnalytics::Return.cumulative(actual.xts)
        # setup initial value
        attr.ret.xts.all <- xts::xts(, as.Date(date))

        for (i in factorNames) {
          attr.ret.xts <- actual.xts -
            xts::xts(as.matrix(fit.lm$model[i])%*%as.matrix(fit.lm$coef[i]),
                as.Date(date))
          cum.attr.ret[k,i] <- cum.ret -
            PerformanceAnalytics::Return.cumulative(actual.xts - attr.ret.xts)
          attr.ret.xts.all <- merge(attr.ret.xts.all, attr.ret.xts)
        }

        # specific returns
        spec.ret.xts <- actual.xts -
          xts::xts(as.matrix(fit.lm$model[, -1])%*%as.matrix(fit.lm$coef[-1]),
              as.Date(date))
        cum.spec.ret[k] <- cum.ret - PerformanceAnalytics::Return.cumulative(actual.xts- spec.ret.xts)
        attr.list[[k]] <- merge(attr.ret.xts.all, spec.ret.xts)
        colnames(attr.list[[k]]) <- c(factorNames, "specific.returns")
      }
    } else {
      # apca method:
      #   fit$loadings # N X K
      #   fit$factors  # T X K
      date <- zoo::index(fit$factors)

      for (k in fundNames) {
        attr.ret.xts.all <- xts::xts(, as.Date(date))
        actual.xts <- xts::xts(fit$data[,k], as.Date(date))
        cum.ret <- PerformanceAnalytics::Return.cumulative(actual.xts)

        for (i in factorNames) {
          attr.ret.xts <- xts::xts(fit$factors[,i]*fit$loadings[k,i], as.Date(date))
          attr.ret.xts.all <- merge(attr.ret.xts.all, attr.ret.xts)
          cum.attr.ret[k,i] <- cum.ret - PerformanceAnalytics::Return.cumulative(actual.xts -
                                                             attr.ret.xts)
        }
        spec.ret.xts <- actual.xts - xts::xts(fit$factors%*%t(fit$loadings[k,]),
                                         as.Date(date))
        cum.spec.ret[k] <- cum.ret - PerformanceAnalytics::Return.cumulative(actual.xts- spec.ret.xts)
        attr.list[[k]] <- merge(attr.ret.xts.all, spec.ret.xts)
        colnames(attr.list[[k]]) <- c(factorNames, "specific.returns")
      }
    }
  }

  ans <- list(cum.ret.attr.f=cum.attr.ret, cum.spec.ret=cum.spec.ret,
              attr.list=attr.list)
  class(ans) <- "pafm"
  return(ans)
}


# If benchmark is provided, active return attribution will be calculated.
#  active returns = total returns  - benchmark returns. Specifically,
# \eqn{R_t^A = \sum_j b_{j}^A * f_{jt} + u_t^A},t=1..T, \eqn{b_{j}^A} is
# \emph{active exposure} to factor j and \eqn{f_{jt}} is factor j. The active
# returns attributed to factor j is \eqn{b_{j}^A * f_{jt}} specific returns is
# \eqn{u_t^A}
