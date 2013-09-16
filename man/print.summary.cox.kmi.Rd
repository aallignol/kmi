\name{print.summary.cox.kmi}
\alias{print.summary.cox.kmi}
\title{Print method for summary.cox.kmi objects}
\description{
  Print method for \code{summary.cox.kmi} objects.
}
\usage{
\S3method{print}{summary.cox.kmi}(x,
digits = max(getOption("digits") - 3, 3),
signif.stars = getOption("show.signif.stars"),
print.ind = FALSE, ...)
}
\arguments{
  \item{x}{An object of class \code{summary.cox.kmi}.}
  \item{digits}{Significant digits to print.}
  \item{signif.stars}{Logical. If TRUE, 'significance stars' are
    printed for each coefficient.}
  \item{print.ind}{Logical specifying whether to print a summary of the
    models fitted on each imputed data set. Default is FALSE}
  \item{\dots}{Further arguments}
}
\value{
  No value returned
}
\author{Arthur Allignol, \email{arthur.allignol@uni-ulm.de}}
\seealso{\code{\link{summary.cox.kmi}}}
\keyword{print}