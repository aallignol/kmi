\name{cox.kmi}
\alias{cox.kmi}

\title{Cox proportional hazards model applied to imputed data sets}
\description{
  This function fits Cox proportional hazards models to each imputed
  data set to estimate the regression coefficients in a proportional
  subdistribution hazards model, and pools the results.
}
\usage{
cox.kmi(formula, imp.data, df.complete = Inf, ...)
}
\arguments{
  \item{formula}{A formula object, with the response on the left of a
    \code{~} operator, and the terms on the right. The response must be
    a survival object as returned by the \code{\link[survival]{Surv}}
    function.}
  \item{imp.data}{An object of class \code{kmi}.}
  \item{df.complete}{Complete data degrees of freedom.}
  \item{\dots}{Further arguments for the \code{\link[survival]{coxph}}
    function.}
}
\details{
  Fits a Cox proportional hazards model on each imputed data set to
  estimate the regression coefficients in a proportional
  subdistribution hazards model, and pools the results, using the
  \code{\link[mitools]{MIcombine}} function of the mitools package.
}
\value{
  An object of class \code{cox.kmi} including the following components:
  \item{coefficients}{Pooled regression coefficient estimates}
  \item{variance}{Pooled variance estimate}
  \item{nimp}{Number of multiple imputations}
  \item{df}{degrees of freedom}
  \item{call}{The matched call}
  \item{individual.fit}{A list of \code{coxph} objects. One for each
    imputed data set.}
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link[survival]{coxph}},
  \code{\link[mitools]{MIcombine}}, \code{\link{print.cox.kmi}},
  \code{\link{summary.cox.kmi}}}
\examples{
data(icu.pneu)

set.seed(1313)
imp.dat <- kmi(Surv(start, stop, status) ~ 1, data = icu.pneu,
               etype = event, id = id, failcode = 2, nimp = 5)

fit.kmi <- cox.kmi(Surv(start, stop, event == 2) ~ pneu, imp.dat)

summary(fit.kmi)

### Now using the censoring-complete data
fit <- coxph(Surv(start, adm.cens.exit, event == 2) ~ pneu, icu.pneu)

summary(fit)

## estimation of the censoring distribution adjusted on covariates
dat.cova <- kmi(Surv(start, stop, status) ~ age + sex,
                data = icu.pneu, etype = event, id = id,
                failcode = 2, nimp = 5)

fit.kmi2 <- cox.kmi(Surv(start, adm.cens.exit, event == 2) ~ pneu + age,
                    dat.cova)

summary(fit.kmi2)
}
\keyword{survival}
\keyword{models}
\keyword{regression}
