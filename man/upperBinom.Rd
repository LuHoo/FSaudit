% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attribute_binomial.R
\name{upperBinom}
\alias{upperBinom}
\title{Binomial upper bound.}
\usage{
upperBinom(n, k = 0, alpha)
}
\arguments{
\item{n}{Sample size. Positive integer.}

\item{k}{Number of deviations found in the sample. Integer less than or equal
to \code{n}.}

\item{alpha}{Significance level. A number greater than 0 and less than 1.}
}
\value{
The one-sided upper bound on the fraction of deviations in the
  population at a confidence level of \code{(1 - alpha)}.
}
\description{
Calculates the upper bound on the fraction of deviations using the binomial
distribution, for sampling with replacement or as an approximation for
sampling without replacement.
}
