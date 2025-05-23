% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extension_grammar.R
\name{extend}
\alias{extend}
\alias{extend.musStateObject}
\alias{extend.cvsStateObject}
\title{Sample extension.}
\usage{
extend(state, ...)

\method{extend}{musStateObject}(state, ...)

\method{extend}{cvsStateObject}(state, ...)
}
\arguments{
\item{state}{State object. This typically is an \code{mus_obj} object. The
\code{significant} method is not defined for other state objects.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
The form of the value returned by \code{evaluate} depends on the
  class of its argument. See the documentation of the particular methods for
  details of what is produced by that method.
}
\description{
The \code{extend} function is a generic function for sample
  extension. The function invokes particular \emph{methods} which depend on
  the class of the first argument.
}
\details{
In some cases it may be desirable to extend a sample, particularly
  when the sampling results are insufficiently precise.

  \code{extend.musStateObject} calculates the required size of the extension,
  where it may be convenient to set the expected misstatement equal to the
  projected misstatement of the initial sample.

  \code{extend.cvsStateObject} uses improved estimates of the (stratum)
  standard deviation of audit values.
}
