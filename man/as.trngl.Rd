% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trngl.R
\name{as.trngl}
\alias{as.trngl}
\alias{as.trngl.trngl}
\alias{as.trngl.matrix}
\title{Coerce to trngl}
\usage{
as.trngl(triangle)

\method{as.trngl}{trngl}(triangle)

\method{as.trngl}{matrix}(triangle)
}
\arguments{
\item{triangle}{A \emph{cumulative} claims triangle}
}
\value{
A trngl
}
\description{
\code{as.trngl} is an S3 generic with methods for matrix and trngl itself.
The latter clears the outliers which are currently flagged.
}
\examples{
triangle <- UKMotor
triangle[1, 7] <- 10 * triangle[1, 7]
res <- mackParamSim(triangle, "single",
  cond = TRUE,
  dist = "normal",
  n_boot = 1e2,
  n_sim = 1e2,
  progress = FALSE
)
print(triangle)
triangle <- as.trngl(triangle)
print(triangle) # no outliers marked
raw.triangle <- unclass(UKMotor)
as.trngl(raw.triangle)
}
