% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitFfM2_rolling.R
\name{roll.fitFfmDT}
\alias{roll.fitFfmDT}
\title{roll.fitFfmDT}
\usage{
roll.fitFfmDT(
  ffMSpecObj,
  windowSize = 60,
  refitEvery = 1,
  refitWindow = c("Expanding", "Rolling"),
  stdExposuresControl = list(Std.Type = "timeSeries", lambda = 0.9),
  stdReturnControl = list(GARCH.params = list(omega = 0.09, alpha = 0.1, beta = 0.81)),
  fitControl = list(fit.method = c("LS", "WLS", "Rob", "W-Rob"), resid.scaleType =
    c("STDDEV", "EWMA", "ROBEWMA", "GARCH"), lambda = 0.9, GARCH.params = list(omega =
    0.09, alpha = 0.1, beta = 0.81), GARCH.MLE = FALSE),
  full.resid.cov = TRUE,
  analysis = c("ISM", "NEW")
)
}
\arguments{
\item{ffMSpecObj}{a specFFm object}

\item{windowSize}{the size of the fit window}

\item{refitEvery}{the frequency of fitting}

\item{refitWindow}{choice of expanding or rolling}

\item{stdExposuresControl}{for exposure standardization; (give the Std.Type and lambda)}

\item{stdReturnControl}{choices to standardize the returns using GARCH controls}

\item{fitControl}{list of options for fitting the ffm}

\item{full.resid.cov}{True or False toggle}

\item{analysis}{choice of "ISM" or "NEW"}
}
\description{
roll.fitFfmDT rolls the fundamental factor model
}
