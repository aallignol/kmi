\name{kmi}
\alias{kmi}
\title{Kaplan-Meier Multiple Imputation for Competing Risks}
\description{
  The function performs a non parametric multiple imputation that aims
  at recovering the missing potential censoring times from competing
  events.
}
\usage{
kmi(formula, data, id = NULL, etype, failcode = 1, nimp = 10,
    epsilon = 1, bootstrap = FALSE, nboot = 10)
}
\arguments{
  \item{formula}{A \code{formula} object, that must have a \code{Surv}
    object on the left of a \code{~} operator. Covariates could be added
    on the right hand side of the formula. They will be used to model
    the censoring distribution. See Details.}
  \item{data}{A data.frame in which to interpret the variables given in
    the formula, \code{etype} and \code{id}. It is mandatory.}
  \item{id}{Used to identify individual subjects when one subject can
    have several rows of data, e.g., with time-dependent covariates. Set
    to \code{NULL} when there is only one raw of data per subject.}
  \item{etype}{Variable specifying the type of competing event. When
    \code{status == 1} in \code{formula}, \code{etype} describes the
    type of event, otherwise, for censored observation, (\code{status ==
      0}), the value of \code{etype} is ignored.}
  \item{failcode}{Indicates the failure cause of interest. Imputation
    will be performed on the other competing events. Default is 1.}
  \item{nimp}{Number of multiple imputation. Default is 10.}
  \item{epsilon}{When the last time is an event, a censoring time equal
    to \code{max(time) + epsilon} is added. By default, \code{epsilon}
    is set to 1.}
  \item{bootstrap}{Logical. Whether to estimate the censoring
    distribution using bootstrap samples. Default is FALSE.}
  \item{nboot}{If \code{bootstrap} is set to TRUE, \code{nboot}
    determines the number of bootstrap samples.}
}
\details{
  It was shown that if censoring times are observed for all individuals,
  methods for standard right-censored survival data can be used to
  analyse cumulative incidence functions from competing risks (Fine and
  Gray 1999). Therefore the idea proposed by Ruan and Gray (2008) is to
  impute potential censoring times for individuals who have failed from
  the competing events. The censoring times are imputed from the
  conditional Kaplan-Meier estimator of the censoring distribution.

  Estimation of the censoring distribution may be improved through
  bootstrapping. Estimation might also be improved fitting a model for
  the censoring distribution. When covariates are given, a proportional
  hazards model on the hazard of censoring is fit. The censoring
  times are then imputed from the estimated model.

  The competing risks model formulation in \code{formula} mimics the one
  in \code{\link[survival]{survfit}}.
}
\value{
  An object of class \code{kmi} with the following components:
  \item{imputed.data}{A list of matrices giving the imputed times in the
  first column and imputed event type in the second column. The event
  status for imputed times take value 0 (censored).}
  \item{original.data}{The original data set}
  \item{info}{Gives the names of the time and event indicator column in
    the original data set.}
  \item{call}{The matched call.}
}
\section{Warning}{
  When a proportional hazards model is fit for modelling the censoring
  distribution, the censoring times are imputed from the imputed model.
  When there is missing covariate information for the prediction, mean
  imputation is used.
}  
\note{
  This multiple imputation technique does not work for left-truncated
  data.
}
\references{Ruan, P.K. and Gray, R.J. (2008). Analyses of cumulative
  incidence functions via non-parametric multiple
  imputation. \emph{Statistics in Medicine}, 27(27):5709--5724.

  Allignol, A. and Beyersmann, J. (2010). Software for fitting
  nonstandard proportional subdistribution hazards
  models. \emph{Biostatistics}, doi:10.1093/biostatistics/kxq018 

  Fine, J.P. and Gray, R.J. (1999). A Proportional Hazards Model for the
  Subdistribution of a Competing Risk. \emph{Journal of the American
    Statistical Association}. 94(446):496--509.
}
\author{Arthur Allignol, \email{arthur.allignol@fdm.uni-freiburg.de}}
\seealso{\code{\link{icu.pneu}}, \code{\link{cox.kmi}},
  \code{\link[survival]{Surv}},  \code{\link[survival]{survfit}}}
\examples{
data(icu.pneu)

dat <- kmi(Surv(start, stop, status) ~ 1, data = icu.pneu,
           etype = event, id= id, failcode = 2, nimp = 5)

## another way to specify the formula if there is no status
## variable
icu.pneu$ev <- icu.pneu$event
icu.pneu$ev[icu.pneu$status == 0] <- 0

dat <- kmi(Surv(start, stop, ev != 0) ~ 1, data = icu.pneu,
           etype = ev, id= id, failcode = 2, nimp = 5)

## with covariates to model the censoring distribution
dat.cova <- kmi(Surv(start, stop, status) ~ age + sex,
                data = icu.pneu, etype = event, id = id,
                failcode = 2, nimp = 5)
}
\keyword{survival}