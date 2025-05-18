#' Stratification.
#'
#' @description The \code{stratify} function is a generic function for
#'   stratification. The function invokes particular \emph{methods} which depend
#'   on the class of the first argument.
#'
#' @details The \code{stratify} function stratifies the population for a CVS
#'   sampling procedure. It therefore only applies to the \code{cvsStateObject}.
#'   Within the workflow this step is optional. If omitted, a sample
#'   consecutively selected will be simple random without stratification.  The
#'   function requires as arguments \code{L}, the desired number of strata, the
#'   \code{stratMeth}, which can take \code{equal} and \code{cumulative} as
#'   arguments. For \code{equal} stratification is with equal recorded values;
#'   the method attempts to create \code{L} for which the total recorded value
#'   is approximately equal in each stratum. The \code{cumulative} method uses
#'   the cumulative square root of frequencies method. This method requires an
#'   additional argument value for \code{classes}, the number of classes
#'   (initial strata) used.
#' @param state State object. This typically is a \code{cvs_obj} object. The
#'   \code{significant} method is not defined for other state objects.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The function returns \code{classes}, a summary data frame with
#'   details of the classes, \code{stratSum}, a summary data frame with
#'   information per stratum, and \code{strat}, a vector of length \code{popn},
#'   containing the stratum labels.
#' @rdname stratify
#' @export
stratify <- function(state, ...) UseMethod("stratify")

#' @rdname stratify
#' @export
stratify.cvsStateObject <- function(state, ...) {
  state <- modifyState(state, lazyeval::lazy_dots(...))
  stratResults <-
    cvs_stratify(state$id,
                 state$bv,
                 state$popn,
                 state$n,
                 state$strata,
                 state$classes,
                 state$stratMeth)

  state$stratSumm <- stratResults$stratSumm # summary table strata

  # Returning sorted order of id and bv
  state$id    <- stratResults$population$id
  state$bv    <- stratResults$population$bv
  state$strat <- stratResults$population$strat
  state$alloc <- stratResults$alloc

  if (state$stratMeth == "cumulative") {
    state$classSumm <- stratResults$classSumm
  }

  state
}
