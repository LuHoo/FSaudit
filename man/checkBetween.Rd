% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variable_checks.R
\name{checkBetween}
\alias{checkBetween}
\title{Checks if variable has value within interval}
\usage{
checkBetween(var, lower, upper)
}
\arguments{
\item{var}{Variable. An input variable to a function.}

\item{lower}{Variable. An input variable to a function, or a numeric value.}

\item{upper}{Variable. An input variable to a function, or a numeric value.}
}
\description{
Some functions throw errors when an input variable has a value outside a
certain interval. This then results in an R error message that is often
incomprehensible to a user.
}
\details{
This function traps when a variable has a value outside the expected interval
and presents an error message that better explains the cause of the function
to halt.
}
