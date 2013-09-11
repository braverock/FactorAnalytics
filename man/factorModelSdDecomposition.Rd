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
  vector of factor marginal contributions to risk sd.
  \item{cSd.fm} (K+1) x 1 vector of factor component
  contributions to risk sd. \item{pcSd.fm} (K+1) x 1 vector
  of factor percentage component contributions to risk sd.
  }
}
\description{
  Compute the factor model factor standard deviation
  decomposition for an asset based on Euler's theorem given
  factor model parameters.
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

