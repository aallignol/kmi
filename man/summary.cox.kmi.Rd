\name{summary.cox.kmi}
\alias{summary.cox.kmi}
\title{Summary method for cox.kmi objects}
\description{
  Provides a summary of the fitted model comparable to that of
  \code{coxph}
}
\usage{
\S3method{summary}{cox.kmi}(object, conf.int = 0.95, scale = 1, ...)
}
\arguments{
  \item{object}{An object of class \code{cox.kmi}}
  \item{conf.int}{Level of the confidence intervals. Default is 0.95}
  \item{scale}{vector of scale factors for the coefficients, defaults to
    1. The confidence limits are for the risk change associated with one
    scale unit}
  \item{\dots}{Further Arguments}
}
\details{
  Also computes a summary of individual models using \code{summary.coxph}.
}
\value{
  An object of class \code{summary.cox.kmi} with components
  \item{call}{The matched call}
  \item{coefficients}{Coefficients}
  \item{conf.int}{Confidence intervals}
  \item{cox.kmi.fit}{A list of \code{summary.coxph} objects for each
    imputed data set}
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{cox.kmi}}, \code{\link{print.summary.cox.kmi}}}
\keyword{methods}