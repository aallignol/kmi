\name{kmi}
\alias{kmi}
\title{Kaplan-Meier multiple imputation}
\description{
  The function computes a Kaplan-Meier multiple imputation method that
  recovers the missing potential censoring information from competing
  risks failure times for the analysis of cumulative incidence functions
}
\usage{
kmi(data, time.col, event.col, id.col = NULL, nimp = 10, failcode = 1,
    censcode = 0, bootstrap = FALSE, nboot = 10, index, seed)
}

\arguments{
  \item{data}{A data set at least containing the failure times and
  status indicator}
  \item{time.col}{A character vector indicating the name of the failure
  times in \code{data}}
  \item{event.col}{A character vector indicating the name of the failure
  indicator in \code{data}}
  \item{id.col}{In the presence of time-dependent covariates, where
  there is several rows per individual, a character vector giving the
  name of the patient id in \code{data}. Otherwise set to \code{NULL}}
  \item{nimp}{Number of imputation}
  \item{failcode}{code denoting the failure time of interest. Default is
  1}
  \item{censcode}{code denoting the censored observations. Default is 0}
  \item{bootstrap}{Logical indicating whether to perform a bootstrap
  estimation of the censoring distribution}
  \item{nboot}{Number of bootstrap samples}
  \item{index}{Index for the bootstrapping. If supplied, must be a list
  of length \code{nboot}. Otherwise an index is drawn with replacement}
  \item{seed}{Random seed}
}
\details{
  It was shown that if censoring times are observed for all individuals,
  methods for standard right-censored survival data can be used to
  analyze cumulative incidence functions from competing risks (Fine and
  Gray 1999). Therefore the idea is to impute potential censoring times
  for individuals who have failed from the competing events. The
  censoring times are imputed from the conditional Kaplan-Meier
  estimator of the censoring distribution.

  Estimation of the censoring distribution may be improved through
  bootstrapping.
}
\value{
  An object of class \code{kmi} with the following components:
  \item{data}{Original data set}
  \item{imp.dat}{A list containing the imputed failure times and status
    indicator}
  \item{info}{A vector containing the column number in \code{data} of
    \code{time.col}, \code{event.col} and \code{id.col}}
  \item{nimp}{Number of imputation}
}
\references{
  Ruan & Gray (2008)
  
  Fine & Gray (1999)
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}

\seealso{\code{\link{cox.kmi}}}
\examples{
### I've to put something
}

\keyword{survival}