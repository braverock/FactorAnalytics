
### function to calculate z-scores for numeric exposure i using weights w
## x is a data.frame object, i is a character string and w has same length as x 
# rob.stats is a logical argument to compute robust location and scale

zScore <- function(x, i, w, rob.stats, z.score, asset.names) {
  if (grepl(z.score, "crossSection")) {
    if (rob.stats) {
      x_bar <- median(w * x[[i]])
      (x[[i]] - x_bar)/mad(x[[i]], center = x_bar)
    } else {
      x_bar <- mean(w * x[[i]]) 
      n <- length(x[[i]])
      # use equal weighted squared deviation about the weighted mean
      (x[[i]] - x_bar)/sqrt(sum((x[[i]] - x_bar) ^ 2)/(n - 1))
    }
  } else {
    N <- length(asset.names)
    exposures <- matrix(w * x[[i]], nrow = N)
    sigmaEWMA <- stdExpo <- exposures
    meanExp <- apply(exposures, 1, mean)
    sigmaExp <- apply(exposures, 1, sd)
    
    for (j in 1:N) {
      ts <- (exposures[j, ] - meanExp[j])^2
      var_past_2 <- sigmaExp[j] ^ 2
      sigmaEWMA[j, ] <- sapply(ts, function(x) var_past_2 <<- 0.10 * x + 0.90 * var_past_2)
      if (any(sigmaEWMA[j, ] == 0)) {
        sigmaEWMA[j, ] <- 1
      }
    }
    as.vector((exposures -  meanExp) / sqrt(sigmaEWMA))
  }
}


#' @param object a fit object of class \code{ffm} which is returned by 
#' \code{fitFfm}

#' @rdname fitFfm
#' @method coef ffm
#' @export

coef.ffm <- function(object, ...) {
  # these are the last period factor exposures
  # already computed through fitFfm
  return(object$beta)
}

#' @rdname fitFfm
#' @method fitted ffm
#' @export

fitted.ffm <- function(object, ...) {  
  # get fitted values for all assets in each time period
  # transpose and convert into xts/zoo objects
  fitted.xts <- PerformanceAnalytics::checkData(t(sapply(object$factor.fit, fitted)))
  names(fitted.xts) <- object$asset.names
  return(fitted.xts)
}

#' @rdname fitFfm
#' @method residuals ffm
#' @export

residuals.ffm <- function(object, ...) {
  return(object$residuals)
}
