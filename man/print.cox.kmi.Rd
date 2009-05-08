\name{print.cox.kmi}
\alias{print.cox.kmi}
\title{Print method for cox.kmi objects}
\description{
  Print method for \code{cox.kmi} objects. 
}
\usage{
\S3method{print}{cox.kmi}(x, print.ind = FALSE, ...)
}
\arguments{
  \item{x}{An object of class \code{cox.kmi}.}
  \item{print.ind}{A logical specifying whether to print the results of
    the analyses performed on each imputed data set. By default, only
    the pooled estimates are printed.}
  \item{\dots}{Further arguments}
}
\value{
  No value returned
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{cox.kmi}}, \code{\link{summary.cox.kmi}}}
\keyword{print}
