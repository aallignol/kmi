\name{kmi-package}
\alias{kmi-package}
\docType{package}
\title{
  Kaplan-Meier multiple imputation for the analysis of cumulative
  incidence functions in the competing risks setting
}
\description{
  The package performs a Kaplan-Meier multiple imputation to recover the
  missing potential censoring information from competing risks events,
  so that standard right-censored methods could be applied to the
  imputed data sets to perform analyses of the cumulative incidence
  functions.
}
\details{
\tabular{ll}{
Package: \tab kmi\cr
Version: \tab 0.3-3\cr
Date: \tab 2010\cr
Depends: \tab survival, mitools\cr
License: \tab GPL (>= 2)\cr
}

Index:
\preformatted{
cox.kmi                 Cox proportional hazards model applied to
                        imputed data sets
icu.pneu                Hospital acquired penumonia in ICU
kmi                     Kaplan-Meier multiple imputation for competing
                        risks
print.cox.kmi           Print method for cox.kmi objects
print.summary.cox.kmi   Print method for summary.cox.kmi objects
summary.cox.kmi         Summary method for cox.kmi objects
}

The \code{kmi} function performs the imputation, while \code{cox.kmi} is
a wrapper of the \code{coxph} function that performs the Cox analysis for
the subdistribution hazard on each imputed data set, then pools the
results.

}
\author{
Arthur Allignol <arthur.allignol@gmail.com>

Maintainer: Arthur Allignol <arthur.allignol@gmail.com>
}
\references{
  Ruan, P.K. and Gray, R.J. (2008). Analyses of cumulative
  incidence functions via non-parametric multiple
  imputation. \emph{Statistics in Medicine}, 27(27):5709--5724.

  Allignol, A. and Beyersmann, J. (2010). Software for fitting
  nonstandard proportional subdistribution hazards
  models. \emph{Biostatistics}, doi:10.1093/biostatistics/kxq018 
  
  Fine, J.P. and Gray, R.J. (1999). A Proportional Hazards Model for the
  Subdistribution of a Competing Risk. \emph{Journal of the American
    Statistical Association}. 94(446):496--509.
}
\keyword{package}
\keyword{survival}
