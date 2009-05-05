\name{kmi}
\alias{kmi}
\title{Kaplan-Meier multiple imputation for competing risks}
\description{
  The function computes a Kaplan-Meier multiple imputation method that
  recovers the missing potential censoring information from competing
  risks failure times for the analysis of cumulative incidence functions.
}
\usage{
kmi(formula, data, id = NULL, etype, failcode = 1, nimp = 10,
    epsilon = 1, bootstrap = FALSE, nboot = 10, index)
}
\arguments{
  \item{formula}{A \code{formula} object, that must have a \code{Surv}
    object at the left of a ~ operator. The right hand side should be 1.}
  \item{data}{A data.frame in which to interpret the variables given in
    the formula, etype and id. It is mandatory.}
  \item{id}{Used to identify individual subjects when one subject can
    have several rows of data, e.g., with time-dependent covariates. Set
    to NULL when there is only one raw of data per subject.}
  \item{etype}{Variable specifying the type of competing event. When
    \code{status == 1} in \code{formula}, \code{etype} describes the
    type of event, otherwise, for censored observation, (\code{status ==
      0}), the value of \code{etype} is ignored.}
  \item{failcode}{Indicates the failure time of interest. Imputation
    will be performed on the other competing events. Default is 1.}
  \item{nimp}{Number of multiple imputation. Default is 10.}
  \item{epsilon}{When the last time is an event, a censoring time equal
    to max(time) + epsilon is added. By default, epsilon is set to 1.}
  \item{bootstrap}{Logical. Whether to estimate the censoring
    distribution using bootstrap samples. Default is FALSE.}
  \item{nboot}{If \code{bootstrap} is set to TRUE, \code{nboot}
    determines the number of bootstrap samples.}
  \item{index}{Index for the bootstrapping. If supplied, must be a list
    of length \code{nboot}. Otherwise an index is drawn with
    replacement.}
}
\details{
  It was shown that if censoring times are observed for all individuals,
  methods for standard right-censored survival data can be used to
  analyze cumulative incidence functions from competing risks (Fine and
  Gray 1999). Therefore the idea proposed by Ruan and Gray (2008) is to
  impute potential censoring times for individuals who have failed from
  the competing events. The censoring times are imputed from the
  conditional Kaplan-Meier estimator of the censoring distribution.

  Estimation of the censoring distribution may be improved through
  bootstrapping.
}
\value{
  An object of class \code{kmi} with the following components:
  \item{imputed.data}{A list of matrix giving the imputed times in the
  first column and the imputed events in the second column.}
  \item{original.data}{The original data set}
  \item{info}{Gives the names of the time and event indicator column in
  data.}
  \item{call}{The matched call.}
}
\references{Ruan, P.K. and Gray, R.J. (2008). Analyses of cumulative
  incidence functions via non-parametric multiple
  imputation. \emph{Statistics in Medicine}, 27(27):5709--5724.

  Fine, J.P. and Gray, R.J. (1999). A Proportional Hazards Model for the
  Subdistribution of a Competing Risk. \emph{Journal of the American
    Statistical Association}. 94(446):496--509.
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\note{
  This multiple imputation technique does not work for left-truncated
  data.
}
\seealso{\code{\link{cox.kmi}}, \code{\link[survival]{Surv}},
  \code{\link[survival]{survfit}}}
\examples{
data(icu.pneu)

dat <- kmi(Surv(entry, exit, status) ~ 1, data = icu.pneu,
           etype = event, id= id, failcode = 3, nimp = 5)

## another way to specify the formula
icu.pneu$ev <- icu.pneu$event
icu.pneu$ev[icu.pneu$status == 0] <- 0

dat <- kmi(Surv(entry, exit, ev != 0) ~ 1, data = icu.pneu,
           etype = ev, id= id, failcode = 3, nimp = 5)
}
\keyword{survival}