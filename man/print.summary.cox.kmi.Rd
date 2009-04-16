\name{print.summary.cox.kmi}
\alias{print.summary.cox.kmi}
\title{Print method for summary.cox.kmi objects}
\description{
  Print method for \code{summary.cox.kmi} objects
}
\usage{
\S3method{print}{summary.cox.kmi}(x,
                digits = max(getOption("digits") - 3, 3),
                signif.stars = getOption("show.signif.stars"),
                print.imp = FALSE, ...)
}
\arguments{
  \item{x}{An object of class \code{summary.cox.kmi}}
  \item{digits}{minimum number of significant digits to be used for most
    numbers}
  \item{signif.stars}{logical; if TRUE, P-values are additionally
    encoded visually as significance stars in order to help scanning
    of long coefficient tables. It defaults to the show.signif.stars
    slot of options}
  \item{print.imp}{Logical. Whether to print a summary for all models
    fitted on imputed data sets. Default is FALSE}
  \item{\dots}{Further arguments for print}
}
\value{
  No value returned
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{summary.cox.kmi}}}
\keyword{print}