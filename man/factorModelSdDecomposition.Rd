\name{factorModelSdDecomposition}
\alias{factorModelSdDecomposition}
\title{Compute factor model standard deviation decomposition}
\usage{
  factorModelSdDecomposition(beta.vec, factor.cov, sig2.e)
}
\arguments{
  \item{beta.vec}{k x 1 vector of factor betas with factor
  names in the rownames.}

  \item{factor.cov}{k x k factor excess return covariance
  matrix.}

  \item{sig2.e}{scalar, residual variance from factor
  model.}
}
\value{
  an S3 object containing \itemize{ \item{Sd.fm} Scalar,
  std dev based on factor model. \item{mSd.fm} (K+1) x 1
  vector of factor marginal contributions to sd.
  \item{cSd.fm} (K+1) x 1 vector of factor component
  contributions to sd. \item{pcSd.fm} (K+1) x 1 vector of
  factor percentage component contributions to sd. }
}
\description{
  Compute the factor model factor standard deviation
  decomposition for an asset based on Euler's theorem given
  factor model parameters.
}
\details{
  The factor model has the form \cr \code{R(t) = beta'F(t)
  + e(t) = beta.star'F.star(t)}\cr where beta.star = (beta,
  sig.e)' and F.star(t) = [F(t)', z(t)]'. By Euler's
  theorem:\cr \code{Sd.fm = sum(cSd.fm) =
  sum(beta.star*mSd.fm)} \cr
}
\examples{
# load data from the database
data("stat.fm.data")
fit.stat <- fitStatisticalFactorModel(sfm.dat,k=2)
cov.factors = var(fit.stat$factors)
names = colnames(fit.stat$asset.ret)
factor.sd.decomp.list = list()
for (i in names) {
 factor.sd.decomp.list[[i]] =
   factorModelSdDecomposition(fit.stat$loadings[,i],
                              cov.factors, fit.stat$resid.variance[i])
}
}
\author{
  Eric Zivot and Yi-An Chen
}
\references{
  \enumerate{ \item Hallerback (2003), "Decomposing
  Portfolio Value-at-Risk: A General Analysis", The Journal
  of Risk 5/2. \item Yamai and Yoshiba (2002)."Comparative
  Analyses of Expected Shortfall and Value-at-Risk: Their
  Estimation Error, Decomposition, and Optimization Bank of
  Japan. \item Meucci (2007). "Risk Contributions from
  Generic User-Defined Factors," Risk. }#'
}

