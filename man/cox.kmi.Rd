\name{cox.kmi}
\alias{cox.kmi}
\title{Cox proportional hazards model applied to imputed data sets}
\description{
  The function fits a Cox proportional hazards model to estimate the
  regression coefficients in a proportional subdistribution hazards
  model to the imputed data sets, and pooled the results together.
}
\usage{
cox.kmi(formula, kmi.object, ...)
}
\details{
  Fits a Cox proportional hazards model on each imputed data set to
  estimate the regression coefficients in a proportional
  subdistribution hazards model, and pooled the results together.

  The \code{data} argument in \code{formula} must not be supplied.
  This is done internally. However the complete data set with the
  covariates must be supplied in the call to \code{kmi} which is then
  copied in the \code{kmi} object.

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{a formula object, with the response on the left of a ~
    operator, and the terms on the right. The response must be a
    survival object as returned by the Surv function.} 
  \item{kmi.object}{An object of class \code{kmi}, as returned by the
    \code{kmi} function}
  \item{\dots}{Further arguments for the \code{coxph} function}
}
\value{
  Returns an object of class \code{cox.kmi} with the following
  components: 
  \item{call}{The matched call}
  \item{coef}{Pooled regression coefficients}
  \item{var}{Pooled variance estimates}
  \item{cox.kmi.fit}{A list containing the estimates for each data
    set. This is a list of \code{coxph} objects}
}
\note{
  Results are not pooled for frailties yet
}
\references{Ruan and Gray}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link[survival]{coxph}}, \code{\link{kmi}},
  \code{\link{print.cox.kmi}}, \code{\link{summary.cox.kmi}}}
\examples{
### to be written
}
\keyword{models}
\keyword{regression}
