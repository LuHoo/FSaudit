#' Sample extension.
#'
#' @description The \code{extend} function is a generic function for sample
#'   extension. The function invokes particular \emph{methods} which depend on
#'   the class of the first argument.
#' @details In some cases it may be desirable to extend a sample, particularly
#'   when the sampling results are insufficiently precise.
#'
#'   \code{extend.musStateObject} calculates the required size of the extension,
#'   where it may be convenient to set the expected misstatement equal to the
#'   projected misstatement of the initial sample.
#'
#'   \code{extend.cvsStateObject} uses improved estimates of the (stratum)
#'   standard deviation of audit values.
#'
#' @param state State object. This typically is an \code{mus_obj} object. The
#'   \code{significant} method is not defined for other state objects.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return The form of the value returned by \code{evaluate} depends on the
#'   class of its argument. See the documentation of the particular methods for
#'   details of what is produced by that method.
#' @rdname extend
#' @export
#'
extend <- function(state, ...) UseMethod("extend")


#' @rdname extend
#' @export
extend.musStateObject <- function(state, ...) {

  state <- modifyState(state, lazyeval::lazy_dots(...))


  if (is.null(state$significant)) {
    # No significant item selection is done, make all items non significant and
    # construct the frame overload all items
    nsi <- data.frame(item = state$samplingUnits, book_value = state$bookValues)
    # Exclude Linting

    # Empty significant list
    si <- data.frame(item = c(), book_value = c()) # Exclude Linting
    state$significant <- list("Individually significant values" = si,
                              "Non significant items" = nsi, # Exclude Linting
                              "sample size" = state$n)
  }


  state$extension <- mus_extend_n(state$samplingUnits,
                                  state$bookValues,
                                  state$significant,
                                  state$sample,
                                  state$performanceMateriality,
                                  state$confidenceLevel,
                                  state$expectedError,
                                  state$distribution,
                                  state$selectionMethod,
                                  state$seed)

  if (state$extension != "No new items required") {
    #Assumes same data.frame format for selection and extension
    state$sample <- rbind(state$sample, state$extension$`Extended samples`)

    state$significant$indSign <-
      rbind(state$significant$indSign,
            state$extension$indSign)

    #overwrite because we are removing items from the complete list
    state$significant$nonSign <-
      state$extension$nonSign

    state$n <- dim(state$sample)[1] #Update the current sample size

  }

  return(state)
}

#' @rdname extend
#' @export
extend.cvsStateObject <- function(state, ...) {

  state <- modifyState(state, lazyeval::lazy_dots(...))

  df <- data.frame(item = state$ id,
                   book_value = state$bv,
                   stratum_lbl = state$strat)
  state$extension <-
    cvs_extend(df, state$strata, state$sample, state$av,
               state$desPrec, state$cl,
               state$stratMeth)

  if (state$extension != "No new items required") {
    #Assumes same data.frame format for selection and extension
    state$sample <- rbind(state$sample, state$extension)
  }

  return(state)
}
