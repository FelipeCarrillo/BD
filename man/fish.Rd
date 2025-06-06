% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rb.R
\docType{data}
\name{fish}
\alias{fish}
\title{Salmon catch using screw-traps in Red Bluff, CA}
\format{
A data frame with 365 rows and 6 columns:
\describe{
  \item{wk}{week is a integer from 1 to 52}
  \item{month}{Month is a integer 1 to 12}
  \item{td}{Trap efficiency generated by fish trials}
  \item{xd}{Volume of water sampled by screw-traps}
  \item{pd}{Number of fish caught by screw-traps}
  \item{year}{Yearly number of fish caught by screw-traps}

  ...
}
}
\usage{
fish
}
\description{
A dataset containing one year worth of data
}
\examples{
library(BD)
dim(fish);head(fish)
}
\keyword{datasets}
