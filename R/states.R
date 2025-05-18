#' @title MUS state object.
#'
#' @description Monetary Unit Sample state object. It contains information
#'   pertaining to the sample, and it is filled in consecutive stages either by
#'   the user or by a method applied to the object.
#'
#' @details A sampling procedure consists of several stages. Some of the stages
#'   require the data from earlier stages. The state object records such inputs
#'   and provides them to later methods as required. The benefit is that these
#'   inputs do not have to be provided by the user repeatedly, but are obtained
#'   form the recorded value in the object.
#'
#'   The typical workflow for an MUS sampling procedure is: \enumerate{ \item
#'   \code{split_population} Splits population into positive, zero-value and
#'   negative values. \item \code{size} Calculates the sample size. \item
#'   \code{significant}Removes individually significant items from the sampling
#'   frame. \item \code{select} Selects the sample. \item \code{evaluate}
#'   Evaluates the sample. \item \code{extend} Calculates incremental sample
#'   size, and if this is greater than zero extends the sample. }
#'
#' @param ... Arguments passed to or from other methods.
#'
#' @return The \code{mus_obj} is updated at every single stage in the sampling
#'   procedure. When the \code{bv} argument is assigned, the \code{popn} and
#'   \code{popBv} arguments are calculated. The \code{size} method requires
#'   input of \code{pm} and calculates the sample size \code{n} argument. The
#'   \code{significant} method calculates the \code{popSign} and
#'   \code{signItems} arguments, and updates the values of \code{popn}
#'   \code{popBv}. The \code{select} method fills the \code{sample} argument.
#'   The \code{evaluate} method fills the \code{results} argument.
#'
#' @examples
#' split <- split_population(inventoryData$item, inventoryData$bv)
#' myMusObject <- mus_obj(id = split$pos_bv$item,
#'                        bv = split$pos_bv$bv)
#' myMusObject <- size(myMusObject,
#'                     pm = 100000)
#' myMusObject <- significant(myMusObject)
#' myMusObject <- select(myMusObject,
#'                       seed = 20201209)
#' true_values <- inventoryData[match(myMusObject$sample$item,
#'                                    inventoryData$item),
#'                              c("item", "av")]
#' myMusObject <- evaluate(myMusObject, av = true_values$av)
#'
#' @importFrom stats aggregate
#'
#' @export
mus_obj <- function(...) {
  state <- structure(
    list(

      # Create object
      id = NULL,
      bv = NULL,         # untested part of sampling unit's book value
      # ->
      popn = NULL,       # Population size
      popBv = NULL,      # total population book value

      # Calculate sample size
      cl = 0.95,
      pm = NULL,         # performance materiality
      ee = 0,            # expected misstatement
      dist = "hyper",    # distribution
      # ->
      n = NULL,

      # Select sample
      seed = NULL,
      aggregates = FALSE,
      selMeth = "randomized.fixed", # selection method
      # ->
      sample = NULL,
      popSign = 0,       # population value significant items
      signItems = NULL,  # Vector of significant items

      # Obtain audit values
      av = NULL,

      # Evaluate sample
      bvTested = NULL,   # book values of tested detail items
      bvOrig = NULL,     # original value of sampling unit
      evalMeth = "cell", # evaluation method
      # ->
      evalResults = NULL

    ),
    class = "musStateObject"
  )
  state <- modifyState(state, lazyeval::lazy_dots(...))
}


#' @title CVS state object.
#'
#' @description Classical Variables Sampling state object. It contains
#'   information pertaining to the sample, and it is filled in consecutive
#'   stages either by the user or by a method applied to the object.
#'
#' @param ... Arguments passed to or from other methods.
#'
#' @return The \code{cvs_obj} is updated at every single stage in the sampling
#'   procedure. When the \code{bv} argument is assigned, the \code{popn} and
#'   \code{popBv} arguments are calculated. The \code{stratify} method assigns
#'   values to the \code{classes}, \code{strata} and \code{stratSumm} arguments.
#'   The \code{size} method requires input of \code{desPrec} and calculates the
#'   sample size \code{n} argument. The \code{select} method fills the
#'   \code{sample} argument. The \code{evaluate} method fills the \code{results}
#'   argument.
#'
#' @examples
#' myCvsObject <- cvs_obj(id = inventoryData$item, bv = inventoryData$bv)
#' myCvsObject <- stratify(myCvsObject, strata = 3, classes = 50,
#'                         stratMeth = "equal")
#' myCvsObject <- size(myCvsObject,
#'                     desPrec = 200000)
#' myCvsObject <- select(myCvsObject,
#'                       seed = 20201209)
#' true_values <- inventoryData[match(myCvsObject$sample$item,
#'                                    inventoryData$item),
#'                              c("item", "av")]
#' myCvsObject <- evaluate(myCvsObject,
#'                         av = true_values$av)
#' myCvsObject$evalResults$`Effective df`
#' myCvsObject$evalResults$Estimates
#' myCvsObject$evalResults$`Most likely total audited amount mean`
#' myCvsObject$evalResults$`Most likely total error mean`
#' myCvsObject$evalResults$`Achieved precision mean`
#' myCvsObject$evalResults$`Difference estimation`
#' myCvsObject$evalResults$`Most likely total audited amount difference`
#' myCvsObject$evalResults$`Most likely total error difference`
#' myCvsObject$evalResults$`Achieved precision difference`
#' myCvsObject$evalResults$`Ratio estimation`
#' myCvsObject$evalResults$`Most likely total audited amount ratio`
#' myCvsObject$evalResults$`Most likely total error ratio`
#' myCvsObject$evalResults$`Achieved precision ratio`
#' myCvsObject$evalResults$`Regression estimation`
#' myCvsObject$evalResults$`Most likely total audited amount regression`
#' myCvsObject$evalResults$`Most likely total error regression`
#' myCvsObject$evalResults$`Achieved precision regression`
#' @export

cvs_obj <- function(...) {
  state <- structure(
    list(

      # Create object
      id = NULL,
      bv = NULL,
      # ->
      popn = NULL, # Population size
      popBv = NULL, # Total population book value
      n = NULL, # Target overall sample size

      # Stratify the population
      strata = 1, # Number of strata required. By default it is 1
      classes = 200,
      stratMeth = "equal",
      # ->
      classSumm = NULL,
      stratSumm = NULL, # Summary df
      strat = NULL, # Vector with stratum numbers per bv

      # Calculate sample size
      cl = 0.95,
      desPrec = NULL, # Desired precision
      sdAv = NULL, # Estimated standard deviation audit values per
      # stratum
      # ->
      alloc = NULL,
      # Data frame with variables strat, Sh, Nh, Bh, and nh

      # Select the sample
      seed = NULL,
      # ->

      sample = NULL,
      # sample is a dataframe with variables item, book_value, stratum_lbl

      # Obtain audit values
      av = NULL,

      # Evaluate sample
      # ->
      results = NULL

    ),
    class = "cvsStateObject"
  )
  state <- modifyState(state, lazyeval::lazy_dots(...))

}

#' @title Attribute state object.
#'
#' @description Attribute sample state object. It contains information
#'   pertaining to the sample, and it is filled in consecutive stages either by
#'   the user or by a method applied to the object.
#'
#' @details A sampling procedure consists of several stages. Some of the stages
#'   require the data from earlier stages. The state object records such inputs
#'   and provides them to later methods as required. The benefit is that these
#'   inputs do not have to be provided by the user repeatedly, but are obtained
#'   form the recorded value in the object.
#'
#'   For now, the workflow of an attribute sampling procedure only consists of
#'   the sample size calculation stage. This produces a sample size, where the
#'   sample evaluation is simply the comparison of the number of deviations
#'   found \code{k} versus the critical number \code{c}, or the percentage of
#'   deviations found \code{k} / \code{n} versus the expected deviation range
#'   \code{eer}.
#'
#' @param ... Arguments passed to or from other methods.
#'
#' @return The \code{att_obj} is updated after running the \code{size} method,
#'   by assigning the \code{n} attribute.
#'
#' @examples
#' myAttObject <- att_obj(alpha = .1,
#'                        popdev = 60,
#'                        popn = 1200,
#'                        c = 0)
#' myAttObject <- size(myAttObject)
#' myAttObject$n
#'
#' myAttObject <- att_obj(alpha = .1,
#'                        popdev = 60,
#'                        popn = 1200,
#'                        ee = 12)
#' myAttObject <- size(myAttObject)
#' myAttObject$n
#'
#' myAttObject <- att_obj(alpha = .1,
#'                        tdr = .05,
#'                        c = 1,
#'                        dist = "binom")
#' myAttObject <- size(myAttObject)
#' myAttObject$n
#'
#' myAttObject <- att_obj(alpha = .1,
#'                        tdr = .05,
#'                        c = 2,
#'                        dist = "pois")
#' myAttObject <- size(myAttObject)
#' myAttObject$n
#'
#' n_binom_eer <- size(myAttObject,
#'                     alpha = .1,
#'                     tdr = .05,
#'                     eer = 0.02,
#'                     dist = "binom")
#' n_binom_eer$n
#'
#' myAttObject <- att_obj(alpha = .1,
#'                        popdev = 60,
#'                        popn = 1200,
#'                        c = 0)
#' myAttObject <- size(myAttObject)
#' myAttObject$n
#'
#'
#' @importFrom stats aggregate
#'
#' @export
att_obj <- function(...) {
  state <- structure(
    list(
      alpha = NULL, # Significance level
      tdr = NULL, # Tolerable deviation rate
      popdev = NULL, # Number of deviations in population
      popn = NULL, # Number of items in population
      c = NULL, # Critical number of deviations
      ee = NULL, # Expected error
      eer = NULL, # expected errors rate values between 0 and 1
      dist = "hyper", # distribution
      n = NULL  # sample size
    ),
    class = "attStateObject"
  )

  state <- modifyState(state, lazyeval::lazy_dots(...))

}


############## Utils

modifyState <- function(state, dots) {

  if (length(dots) > 0) {

    ids <- length(dots)
    keys <- names(dots)

    values <- lazyeval::lazy_eval(dots)
    for (i in 1:ids) {

      if (keys[[i]] == "bv") {
        # Compute total book value
        state["popBv"] <- as.numeric(sum(values[[i]]))
        # Compute total population size
        state["popn"] <- length(values[[i]])
        # Check if seed is assigned. If not, assign.
        if (is.null(unlist(state["seed"]))) state["seed"] <-
            runif(1, -2^31, 2^31)
      }

      myText <- paste("The supplied argument, ", "'", keys[i], "'",
                      " is not a part of the function...!", sep = "")
      if (!keys[i] %in% names(state)) {
        warning(myText)
      } else {
        # Update state object with values
        if (!is.null(values[[i]]) && !any(values[[i]] == ""))
          state[[keys[i]]] <- values[[i]]
      }
    }
  }
  return(state)
}
