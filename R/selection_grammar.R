#' Sample selection.
#'
#' @description The \code{select} function is a generic function for sample
#'   selection. The function invokes particular \emph{methods} which depend on
#'   the class of the first argument.
#' @description The \code{select} function selects sample items of a given size
#'   from the sampling frame. The sample size is either calculated with the
#'   \code{getSampleSize} function or user-defined.
#'
#'   \code{select.musStateObject} uses a selection procedure without replacement
#'   that is proportional to the book values. If individually significant items
#'   are not removed from the sampling frame prior to selection of the sample,
#'   they are interpreted as aggregates, and multiple monetary units can be
#'   selected from them.
#'
#'   \code{select.cvsStateObject} selects either a simple random sample, if the
#'   population is not stratified, or a stratified random sample, where sampling
#'   units are selected with simple random sampling in each stratum.
#'
#' @param state State object. This could be a \code{mus_obj} or a \code{cvs_obj}
#'   object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @rdname select
#' @export
select <- function(state, ...) UseMethod("select")


#' @rdname select
#' @export
select.musStateObject <- function(state, ...) {

  state <- modifyState(state, lazyeval::lazy_dots(...))
  res <- mus_select(state$id,
                    state$bv,
                    state$n,
                    state$aggregates,
                    state$selMeth,
                    state$seed,
                    state$pm,
                    state$cl,
                    state$ee,
                    state$dist,
                    state$evalMeth)

  state$sample    <- res$sample
  state$id        <- res$id
  state$bv        <- res$bv
  state$popBv     <- res$popBv
  state$popSign   <- res$popSign
  state$signItems <- res$signItems

  state$n <- nrow(state$sample) # overwrite with real sample size
  state

}

#' @rdname select
#' @export
select.cvsStateObject <- function(state, ...) {
  state <- modifyState(state, lazyeval::lazy_dots(...))


  if (state$strata == 1 && length(state$n) == 1) {

    state$strat <- rep(1, state$popn)

    state$alloc <- data.frame(strat  = 1,
                              sdAvh  = sd(state$bv),
                              popNh  = state$popn,
                              popBvh = state$popBv,
                              nh     = state$n)

  } else if (state$strata == 1 && is.null(state$alloc$nh) && is.null(state$n)) {
    stop("Provide or calculate sample size first.")
  }
  # select the sample items from the stratified population
  population <- data.frame(item = state$id,
                           bv = state$bv,
                           strat = state$strat)
  state$sample <- cvs_select(population, state$alloc$nh, state$seed)
  state

}
