
#' Removing significant items from the sampling frame.
#'
#' @description The \code{significant} function removes individually significant
#'   items from the sampling frame for an MUS experiment.
#'
#' @details Prior to selecting the sample from a sampling frame, auditors may
#'   want to remove individually significant items, and test these separately.
#'   If not removed, all sampling units in the sampling frame are interpreted as
#'   aggregate items, from which one or more sampling units need to be selected
#'   by means of the indicated monetary unit.
#'
#'   Not all selection methods facilitate proper indication of a selected
#'   monetary unit. For example, when working with aggregate items, it is not
#'   advised to use the \code{sieve} or \code{modified sieve} selection methods.
#'
#'   Since significant items only pertain to sampling with unequal
#'   probabilities, this function is only defined in the context of a
#'   \code{musStateObject}.
#' @param state State object. This typically is an \code{mus_obj} object. The
#'   \code{significant} method is not defined for other state objects.
#' @param ... Further arguments passed to or from other methods.
#'
#' @rdname significant
#' @export
significant <- function(state, ...) UseMethod("significant")

#' @rdname significant
#' @export
significant.musStateObject <- function(state, ...) {
  state <- modifyState(state, lazyeval::lazy_dots(...))
  res <-  mus_significant(state$id,
                          state$bv,
                          state$n,
                          state$pm,
                          state$cl,
                          state$ee,
                          state$dist,
                          state$evalMeth)

  state$signItems <- res$significant


  state$bv      <- res$nonsignificant$bv
  state$id      <- res$nonsignificant$item
  state$n       <- res$sampleSize
  state$popBv   <- sum(res$nonsignificant$bv)
  state$popSign <- sum(res$significant$bv)

  return(state)

}
