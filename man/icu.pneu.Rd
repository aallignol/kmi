\name{icu.pneu}
\alias{icu.pneu}
\docType{data}
\title{Hospital acquired penumonia in ICU} 
\description{
  This data set is a random sample drawn from the SIR-3 study that aimed
  at analysing the effect of nosocomial infections on the length of ICU
  stay. Patients were included in the study if they had stayed at least
  2 days in the unit. The sample includes information to assess the
  effect on nosocomial pneumonia on the length of stay. The endpoint is
  either discharge alive from the ICU or dead in the unit.
}
\usage{data(icu.pneu)}
\format{
  A data frame with 1421 observations on the following 6 variables.
  \describe{
    \item{\code{id}}{Individual patient id}
    \item{\code{entry}}{Entry time into the study.}
    \item{\code{exit}}{Failure time.}
    \item{\code{status}}{Censoring status. 0 if the observation is
      censored, 1 otherwise}
    \item{\code{event}}{Event type. 2 is discharge alive, 3 is death in
      ICU.}
    \item{\code{pneu}}{Nosocomial pneumonia indicator}
  }
\source{
  Beyersmann, J., Gastmeier, P., Grundmann, H.,
  Baerwolff, S., Geffers, C., Behnke, M.,
  Rueden, H., and Schumacher, M. Use of multistate
  models to assess prolongation of intensive care unit stay due to
  nosocomial infection. \emph{Infection Control and Hospital
  Epidemiology}, 27:493-499, 2006.
}
\examples{
data(icu.pneu)
}
\keyword{datasets}
