\name{summary.cox.kmi}
\alias{summary.cox.kmi}

\title{Summary method for cox.kmi objects}
\description{
  Provides a summary of the fitted model.
}
\usage{
\S3method{summary}{cox.kmi}(object, conf.int = 0.95, scale = 1, ...)
}
\arguments{
  \item{object}{An object of class \code{cox.kmi}.}
  \item{conf.int}{Level of the confidence intervals. Default is 0.95}
  \item{scale}{Vector of scale factors for the coefficients, default to
    1. The confidence limits are for the risk change associated with one
    scale unit.}
  \item{\dots}{Further arguments}
}
\value{
  An object of class \code{summary.cox.kmi} with the following
  components:
  \item{call}{The matched call}
  \item{coefficients}{A matrix with 5 columns including the regression
    coefficients, subdistribution hazard ratios, standard-errors,
    t-statistics and corresponding two-sided p-values.}
  \item{conf.int}{A matrix with 4 columns that consists of the
    subdistribution hazard ratios, exp(-coef) and the lower and upper
    bounds of the confidence interval.}
  \item{individual.fit}{A list of \code{summary.coxph} objects for each
    imputed data set}
}
\author{Arthur Allignol, \email{arthur.allignol@gmail.com}}
\seealso{\code{\link{cox.kmi}}, \code{\link{print.summary.cox.kmi}},
  \code{\link[survival]{summary.coxph}}}
\keyword{methods}
