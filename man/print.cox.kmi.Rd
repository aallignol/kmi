\name{print.cox.kmi}
\alias{print.cox.kmi}
\title{Print method for cox.kmi objects}
\description{
  Print method for \code{cox.kmi} objects. Pooled estimates are printed
  in the same fashion as \code{coxph} objects. 
}
\usage{
\S3method{print}{cox.kmi}(x, print.imp = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{cox.kmi}}
  \item{print.imp}{Logical. Whether to print the results from fitted
    models on each data set (using \code{print.coxph}). Default is
    \code{FALSE}}
  \item{\dots}{Further arguments for the print method}
}
\value{
  No value returned
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{cox.kmi}}}
\keyword{print}