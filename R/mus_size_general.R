#' MUS size calculation.
#'
#' @description  Calculates the sample size for a monetary unit sample.
#'
#' @param cl Confidence level. A number greater than 0 and less than 1.
#' @param bv Population book value. It could be either a vector of book values,
#'   from which the total book value is computed as the rounded sum of book
#'   values for subsequent computations, or the total population book value.
#' @param pm Performance materiality. A positive integer.
#' @param ee Expected error. A positive integer less than \code{pm}.
#' @param dist Name of the distribution used. Default distribution is
#'   \code{hyper} for the hypergeometric distribution. Other options available
#'   are \code{binom} for the binomial distribution and \code{pois} for the
#'   Poisson distribution.
#' @param evalMeth Evaluation method used. The sample size calculation is
#' slightly more efficient if the \code{evalMethod} is \code{cell}.
#' @return The minimum sample size required to conclude with \code{cl}
#'   confidence that the true monetary error does not exceed \code{pm} if the
#'   projected error doesn't exceed \code{ee}.

mus_size <- function(cl, bv, pm, ee = 0, dist = "hyper", evalMeth = "cell") {

  bv <- abs(bv)

  if (dist == "binom") {
    return(musSizeBinom(cl, bv, pm, ee, evalMeth))
  } else if (dist == "pois") {
    return(musSizePois(cl, bv, pm, ee, evalMeth))
  } else {
    return(musSizeHyper(cl, bv, pm, ee, evalMeth))
  }
}

#' MUS size calculation for cell evaluation method.
#'
#' @description  Calculates the sample size for a monetary unit sample.
#'
#' @param cl Confidence level. A number greater than 0 and less than 1.
#' @param popBv Population book value. Total book value of the elements in the
#'   population.
#' @param pm Performance materiality. A positive integer.
#' @param ee Expected error. A positive integer less than \code{pm}.
#' @param dist Name of the distribution used. Default distribution is
#'   \code{hyper} for the hypergeometric distribution. Other options available
#'   are \code{binom} for the binomial distribution and \code{pois} for the
#'   Poisson distribution.
#' @return The minimum sample size required to conclude with \code{cl}
#'   confidence that the true monetary error does not exceed \code{pm} if the
#'   projected error doesn't exceed \code{ee}.

findSizeCell <- function(cl, popBv, pm, ee, dist) {

  finalUpl <- pm + 1
  n <- 0

  while (finalUpl > pm) {
    n <- n + 1

    sampInt <- popBv / n
    expTaint <- ee / sampInt
    m <- ceiling(expTaint)
    taintValues <- rep(1, m)

    # Things go wrong if expTaint is an integer
    if (expTaint %% 1 > 0) {
      taintValues[m] <- expTaint %% 1
    }

    cellRes <- cellResult(taintValues = taintValues,
                          popBv = popBv,
                          n = n,
                          alpha = 1 - cl,
                          dist = "hyper")

    finalUpl <- cellRes$`Upper confidence bound`
  }

  return(cellRes$`Sample size used`)

}


#' MUS Evaluation.
#'
#' @description  Evaluate a monetary unit sample by means of cell, Stringer
#'   bound or pps method.
#'
#' @param cl Confidence level. A number greater than 0 and less than 1.
#' @param bv Vector with book values of the sampling units.
#' @param av Vector with audit values of the sampling units.
#' @param popBv Population book value. Total monetary value of the population.
#' @param dist Distribution used. Default distribution is \code{hyper} for the
#'   hypergeometric distribution. Other options available are \code{binom} for
#'   the binomial distribution and \code{pois} for the Poisson distribution.
#' @param evalMeth Name of the method used for MUS evaluation: \code{cell},
#'   \code{stringer} or \code{pps}.
#' @param aggregates Boolean, indicating whether the sample is selected from
#'   aggregate balances (TRUE) or not (FALSE, default).
#' @param bvTested Vector with book values of the detail items, if sample is
#'   selected from aggregate balances.
#' @return Upper precision limit on the total monetary misstatement in a
#'   population of \code{popBv - popSign} at \code{cl} confidence.
#' @importFrom stats pbinom phyper qf qgamma

mus_eval <- function(cl, bv, av, popBv,
                     dist = "hyper", evalMeth = "cell",
                     aggregates = FALSE, bvTested) {

  checkBetween(cl, 0, 1)
  checkPositive(popBv)
  checkOptions(dist, c("hyper", "binom", "pois"))
  checkOptions(evalMeth, c("cell", "stringer", "pps"))
  checkLengths(bv, av)
  checkStrictsign(bv)

  alpha <-  1 - cl

  # Create dataframe sample to collect sample results
  if (aggregates == TRUE) {
    bvEval <- bvTested
  } else {
    bvEval <- bv
  }

  sample <- data.frame(bv = bvEval, av = av)
  sample$error <- sample$bv - sample$av
  sample$taint <- sample$error / sample$bv

  # Calculate sampling interval
  n <- nrow(sample) # We do not test if this is consistent with n in the object!
  si <- popBv / n

  # Summary information
  popBv <- round((popBv), 0)
  # popBv is total population book value excl individually significant items

  # Stringer bound method

  if (evalMeth == "stringer") {

    # Stringer calculations for overstatements and understatements
    stringerCalc <- function(type = "Over", popBv, n) {

      if (type == "Over") {
        sample <- sample[order(sample$taint, decreasing = TRUE), ]
        taintValues <- sample[sample$taint > 0, "taint"]
      } else {
        sample <- sample[order(sample$taint, decreasing = FALSE), ]
        taintValues <- -sample[sample$taint < 0, "taint"]
      }

      m <- length(taintValues)
      # Number of nonzero differences for either over or under

      bp <- data.frame(matrix(ncol = 8, nrow = 1))
      calcs <- data.frame(matrix(ncol = 8, nrow = m))
      names(bp) <- c("m", "taint", "projMis100", "projMisst", "mU", "mUincr",
                     "pgw100", "precision")
      names(calcs) <- c("m", "taint", "projMis100", "projMisst", "mU", "mUincr",
                        "pgw100", "precision")
      bp$m <- 0
      bp$mU <- upper(0, popBv, n, alpha = 0.05, dist)
      bp$precision <- bp$mU * max(taintValues, 1)

      if (m > 0) {
        calcs$m <- 1:m
        calcs$taint <- taintValues
        calcs$projMis100 <- calcs$m * popBv / n
        calcs$projMisst <- calcs$taint * popBv / n

        upperBound <- function(x) {
          upper(x, popBv, n, alpha = 0.05, dist) * max(taintValues, 1)
        }

        calcs$mU <- sapply(1:m, upperBound)
        calcs$prevmU <- sapply(0:(m - 1), upperBound)
        calcs$prevmU[1] <- max(upperBound(0), bp$precision)
        calcs$mUincr <- calcs$mU - calcs$prevmU
        calcs$pgw100 <- (calcs$mUincr - popBv / n)
        calcs$precision <- calcs$pgw100 * calcs$taint
        calcs$prevmU <- NULL
      }

      stringer <- rbind(bp, calcs)
      projMisst <- abs(sum(taintValues * si))

      precAch <- sum(stringer$precision)
      ub <- projMisst + precAch

      result <- list("Precision calculation" = stringer,
                     "Sample size used" = n,
                     "Nonzero differences" = m,
                     "Sampling interval" = si,
                     "Projected misstatement" = projMisst,
                     "Basic precision" = bp$precision,
                     "Precision achieved" = precAch,
                     "Upper confidence bound" = ub)
    }

    # Evaluate overstatements
    overStatement <- stringerCalc(type = "Over", popBv, n)

    # Evaluate understatements
    underStatement <- stringerCalc(type = "Under", popBv, n)

    # Provide general summary information
    result <- list("Over" = overStatement,
                   "Under" = underStatement)
  }

  # Cell bound method

  if (evalMeth == "cell") {

    # Cell calculations for overstatements and understatements
    cellCalc <- function(type = "Over", popBv, n, dist) {

      if (type == "Over") {
        sample <- sample[order(sample$taint, decreasing = TRUE), ]
        taintValues <- sample[sample$taint > 0, "taint"]
      } else {
        sample <- sample[order(sample$taint, decreasing = FALSE), ]
        taintValues <- -sample[sample$taint < 0, "taint"]
      }

      result <- cellResult(taintValues, popBv, n, alpha, dist)

      return(result)
    }

    # Evaluate overstatements
    overStatement <- cellCalc(type = "Over", popBv, n, dist)

    # Evaluate understatements
    underStatement <- cellCalc(type = "Under", popBv, n, dist)

    # Provide general summary information
    result <- list("Over" = overStatement,
                   "Under" = underStatement)
  }

  # PPS evaluation method


  if (evalMeth == "pps") {

    est_pps <- popBv * (1 / n) * sum(sample$av / sample$bv)

    nonzeroes <- sample[sample$error != 0, ]

    m <- length(nonzeroes$bv) # Number of nonzero differences

    # Calculate standard error

    sd_est <- NA
    se <- NA
    precAch <- NA
    upperBound <- NA
    lowerBound <- NA

    if (m > 2) {
      sd_est <- sqrt((sum(sample$taint^2) - (sum(sample$taint)^2) / n) /
                       (n - 1))
      se <- (popBv * sd_est) / sqrt(n)

      precAch  <- se * qt(1 - alpha / 2, m - 1)

      upperBound <- est_pps + precAch
      lowerBound <-  est_pps - precAch
    }

    result <- list("Nonzero diff" = m,
                   "pps estimate" = est_pps,
                   "Error estimate" = popBv - est_pps,
                   "Precision" = precAch,
                   "Effective df" = m - 1,
                   "Lower bound" = lowerBound,
                   "Upper bound" = upperBound)
  }
  return(result)
}

#' MUS Cell Evaluation.
#'
#' @description  Performs cell evaluation, used in sample size calculation and
#' evaluation.
#'
#' @param taintValues Taint values. Vector with taintings in descending order.
#' @param popBv Population book value. Total monetary value of the population.
#' @param n Sample size.
#' @param alpha Significance level.
#' @param dist Distribution used. Default distribution is \code{hyper} for the
#'   hypergeometric distribution. Other options available are \code{binom} for
#'   the binomial distribution and \code{pois} for the Poisson distribution.
#' @return Upper precision limit on the total monetary misstatement in a
#'   population of \code{popBv - popSign} at \code{cl} confidence.
#' @importFrom stats pbinom phyper qf qgamma

cellResult <- function(taintValues, popBv, n, alpha, dist) {

  si <- popBv / n
  m <- length(taintValues)
  bp <- data.frame(matrix(ncol = 9, nrow = 1))
  calcs <- data.frame(matrix(ncol = 9, nrow = m))
  names(bp)    <- c("m", "mum", "taint", "projMisst", "t_ave", "mUPrev",
                    "loadspread", "simplespread", "stageUPL")
  names(calcs) <- c("m", "mum", "taint", "projMisst", "t_ave", "mUPrev",
                    "loadspread", "simplespread", "stageUPL")

  bp$m <- 0
  bp$mum <- upper(0, popBv, n, alpha, dist)
  bp$simplespread <- bp$mum
  bp$loadspread <- bp$mum
  bp$stageUPL <- bp$mum * max(taintValues, 1)

  if (m > 0) {
    calcs$m <- 1:m
    upperBound <- function(x) {
      upper(x, popBv, n, alpha, dist)
    }

    calcs$mum <- sapply(1:m, upperBound)
    calcs$taint <- taintValues
    calcs$projMisst <- calcs$taint * popBv / n
    calcs$t_ave <- cumsum(taintValues) / 1:m
    calcs$simplespread <- calcs$mum * calcs$t_ave

    for (i in 1:m) {
      if (i == 1) {
        calcs$mUPrev[i] <- bp$stageUPL
      } else {
        calcs$mUPrev[i] <- calcs$stageUPL[i - 1]
      }

      calcs$loadspread[i] <- calcs$projMisst[i] + calcs$mUPrev[i]
      calcs$simplespread[i] <- calcs$mum[i] * calcs$t_ave[i]
      calcs$stageUPL[i] <- max(calcs$loadspread[i], calcs$simplespread[i])
    }

  }

  cell <- rbind(bp, calcs)

  projMisst <- sum(taintValues) * si
  finalUpl <- max(cell$stageUPL)
  asr <- round(finalUpl - projMisst, 2)

  result <- list("Precision calculation" = cell,
                 "Sample size used" = n,
                 "Nonzero differences" = m,
                 "Cell width" = si,
                 "Projected misstatement" = projMisst,
                 "Basic precision" = bp$stageUPL,
                 "Precision achieved" = asr,
                 "Upper confidence bound" = finalUpl)

  return(result)
}




#' MUS Selection.
#'
#' @description  Select a monetary unit sample by means of random, (randomized)
#'   fixed-interval, cell, or (modified) sieve selection.
#'
#' @param id Unique id of the population elements.
#' @param bv Vector with book values of the population elements.
#' @param n Sample size.
#' @param aggregates Population elements are aggregates. Default value is FALSE.
#' @param selMeth Optional. By default, it takes fixed interval
#'   selection. Otherwise, user can define the options, such as, a. random b.
#'   fixed c. randomized.fixed d. cell e. sieve f. modified.sieve
#' @param seed Single value, interpreted as an integer. By default it is 1234.
#' @param pm Performance materiality.
#' @param cl Confidence level.
#' @param ee Expected error.
#' @param dist Distribution.
#' @param evalMeth Evaluation method.
#' @return A data frame of selected sampling units, indicating the unique
#'   identifier, book value, and monetary unit selected within the sampling
#'   unit. For the \code{modified.sieve} method, additionally the original and
#'   final ranks are provided.
#' @importFrom stats runif

mus_select <- function(id, bv, n, aggregates = FALSE, selMeth = "fixed", seed,
                       pm, cl = 0.95, ee = 0, dist,
                       evalMeth = "cell") {

  checkUnique(id)
  checkLengths(id, bv)
  checkPositive(n)
  checkInteger(n)
  checkOptions(selMeth, c("random", "fixed", "randomized.fixed", "cell",
                          "sieve", "modified.sieve"))
  checkStrictsign(bv)

  bv <- abs(bv)

  set.seed(seed)

  if (aggregates == FALSE) {
    # eliminate any significant items
    res <- mus_significant(id, bv, n, pm, cl = 0.95, ee, dist, evalMeth)

    # return attributes
    bv <- res$nonsignificant$bv
    id <- res$nonsignificant$item
    n <- res$sampleSize
    popBv <- sum(res$nonsignificant$bv)
    popSign <- sum(res$significant$bv)
    signItems <- length(res$significant$bv)

  }

  population <- data.frame(item = id, bv = abs(bv))

  popBv <- abs(as.numeric(sum(population$bv)))
  si <- popBv / n

  population$cum <- cumsum(population$bv)
  population$unit <- NA
  population$previous <- population$cum - population$bv

  ranNum <- runif(nrow(population))

  if (selMeth == "randomized.fixed") {
    population$randNum <- ranNum[seq_len(nrow(population))]
    population <- population[order(population$randNum), ]
    population$cum <- cumsum(population$bv)
    population$previous <- population$cum - population$bv
    population$randNum <- NULL
    selMeth <- "fixed"
  }

  switch(selMeth,
         "random" = {
           ranMu <- ranNum[1:n] * popBv
           selUnits <- findInterval(ranMu, population$cum) + 1
           musSample <- population[selUnits, ]
           row.names(musSample) <- 1:n
           musSample$unit <- round(ranMu - population[selUnits, 5], 2)
         },

         "fixed" = {
           ranMu <- ranNum[1] * si + 0:(n - 1) * si
           selUnits <- findInterval(ranMu, population$cum) + 1
           musSample <- population[selUnits, ]
           row.names(musSample) <- 1:n
           musSample$unit <- round(ranMu - population[selUnits, 5], 2)
         },

         "cell" = {
           cellStarts <- seq(0, popBv - si, si)
           ranMu <- cellStarts + ranNum[1:n] * si

           selUnits <- findInterval(ranMu, population$cum) + 1
           musSample <- population[selUnits, ]
           row.names(musSample) <- 1:n
           musSample$unit <- round(ranMu - population[selUnits, 5], 2)
         },

         "sieve" = {
           musSample <- population
           musSample$unit <- round(ranNum * si, 2)
           musSample$selected <- (musSample$bv > musSample$unit)
           musSample <- musSample[musSample$selected == TRUE, ]
           musSample <-
             musSample[, which(names(musSample) %in% c("item", "bv", "unit"))]
         },

         "modified.sieve" = {
           population$unit <- round(ranNum * si, 2)
           population$randNum <- ranNum
           population$mesh <- population$bv / ranNum
           # At which sample size will items become significant?
           population$signAt <- popBv / population$bv

           population <- population[order(population$mesh,
                                          decreasing = TRUE), ]
           population$rank <- NA
           population$rank <- seq_len(nrow(population))

           population$new_rank <- pmin(population$rank, population$signAt)
           population <- population[order(population$new_rank), ]

           musSample <- population[which(population$new_rank <= n), ]
           musSample <- musSample[, which(names(musSample) %in%
                                            c("item", "bv", "unit"))]
         }
  )

  if (aggregates == TRUE) {
    popSign <- NULL
    signItems <- NULL
  }

  res <- list(sample = musSample,
              id = id,
              bv = bv,
              popBv = popBv,
              popSign = popSign,
              signItems = signItems
  )

  return(res)
}

#' MUS Selection of individually significant items.
#'
#' @description  Eliminates individually significant items from the sampling
#'   frame.
#'
#' @param id Sampling unit.
#' @param bv Vector with book values of the population elements.
#' @param n Sample size. Is NULL by default, but could be preassigned by the
#'   user, in which case it needs to be passed to the function.
#' @param pm Performance materiality.
#' @param cl Confidence level. A number greater than 0 and less than 1. The
#'   default value is 0.95
#' @param ee Expected error. The default value is 0.
#' @param dist Name of the distribution used. Default distribution is
#'   \code{hyper} for the hypergeometric distribution. Other options available
#'   are \code{binom} for the binomial distribution and \code{pois} for the
#'   Poisson distribution.
#' @param evalMeth Evaluation method.
#'
#' @return A data frame of individually significant items and one of
#'   non-significant items. In addition, a revised sample size for the
#'   population of non-significant items.
#' @importFrom stats runif

mus_significant <- function(id,
                            bv,
                            n = NULL,
                            pm,
                            cl = 0.95,
                            ee = 0,
                            dist,
                            evalMeth) {

  checkUnique(id)
  checkLengths(id, bv)
  checkPositive(pm)
  checkInteger(pm)
  checkBetween(cl, 0, 1)
  checkPositive(ee)
  checkLessthan(ee, pm)
  checkOptions(dist, c("hyper", "binom", "pois"))

  population <- data.frame(item = id, bv = abs(bv))

  popBv <- round(as.numeric(sum(population$bv)), 0)
  if (missing(n)) {
    n <- mus_size(cl, population$bv, pm, ee, dist, evalMeth)
  }
  si <- popBv / n

  # Individually significant items
  significants <- which(population$bv >= si)
  ind_sig <- population[significants, ]
  temp_sig <- ind_sig

  while (nrow(temp_sig) > 0) {
    population <- population[-significants, ]
    popBv <- round(as.numeric(sum(population$bv)), 0)

    n <- mus_size(cl = cl,
                  bv = population$bv,
                  pm = pm,
                  ee = ee,
                  dist = dist,
                  evalMeth = evalMeth) # Recalculating the sample size

    if (n != 0) {
      si <- popBv / n

      temp_sig <- population[which(population$bv >= si), ]
      ind_sig <- rbind(ind_sig, temp_sig)
    } else {
      temp_sig <- data.frame()
    }
  }

  return(list("significant" = ind_sig,
              "nonsignificant" = population,
              "sampleSize" = n))
}

#' Extend MUS sample.
#'
#' @description  The function extends an initial MUS sample to respond to a
#'   projected misstatement that is higher than the expected error.
#'
#' @details In some cases auditors may want to extend a MUS sample, particularly
#'   if the initial sample yield a projected misstatement larger than the
#'   expected error, but clearly less than performance materiality. The
#'   \code{mus_extend_n} function facilitates extending the sample, while taking
#'   care of the population elements that become individually significant as the
#'   sample size increases.
#'
#' @param id Sampling unit. Unique identifier.
#' @param bv Vector of population book values, from which the total book value
#'   is computed as the rounded sum of book values for subsequent computations.
#' @param sign_items Vector containing a data frame with unique identifier and
#'   book value of individually significant items, a data frame with unique
#'   identifier and book value of non-significant items, and a sample size based
#'   on the total book value of the non-significant items. Obtained from
#'   \code{mus_significant}.
#' @param initial Data fram with unique identifier and book value of initial
#'   sample. Obtained from \code{mus_select}.
#' @param pm Performance materiality.
#' @param cl Confidence level. A number greater than 0 and less than 1. The
#'   default value is 0.95.
#' @param ee Expected error. The default value is 0.
#' @param dist Name of the distribution used. Default distribution is
#'   \code{hyper} for the hypergeometric distribution. Other options available
#'   are \code{binom} for the binomial distribution and \code{pois} for the
#'   Poisson distribution.
#' @param selMeth Optional. Default is \code{fixed}. Other options available are
#'   \code{random} \code{randomized.fixed} \code{cell} \code{sieve} and
#'   \code{modified.sieve}.
#' @param seed A single value, interpreted as an integer.
#'
#' @return A list containing a data frame of the extended sample, a data frame
#'   with the new individually significant items, and a data frame with the non-
#'   significant items.

mus_extend_n <- function(id, bv, sign_items, initial, pm, cl = 0.95, ee = 0,
                         dist = "hyper", selMeth = "fixed", seed) {

  # Validation of the parameters supplied
  # Needs to be reviewed !!!!!
  if (!is.numeric(bv))
    stop("\nThe popolation book values should be numeric\n\n")
  if (length(id) != length(bv))
    stop("Book values and sample units should be of the same length")
  if (any(bv > 0) & any(bv < 0))
    stop("Book value contains both positive and negative numbers. Use the
       split_population function and select either all positive or all negative
       bv as the input.")
  if (!is.list(sign_items))
    stop("Should be a list of dataframes with significant and non_significant
         items")
  if (nrow(sign_items[[1]])) {
    if (!is.numeric(sign_items[[1]][, 2]))
      stop("Non-numeric values for the book values in significant items")
  }
  if (!is.numeric(sign_items[[2]][, 2]))
    stop("Non-numeric values for the book values in non significant items")
  if (!is.numeric(initial[, 2]))
    stop("Non-numeric values for the population book values")


  population <- data.frame("sampling_unit" = id, "bv" = bv)

  n0 <-  nrow(initial)
  new_n <- mus_size(cl, bv, pm, ee, dist)

  if (new_n > n0) {

    new_sign_items <- mus_significant(population$sampling_unit,
                                      population$bv, pm, cl, ee, dist)
    new_df <- subset(population, !(population$sampling_unit %in%
                                     new_sign_items[[1]]$item))

    n1 <- mus_size(cl, new_df$bv, pm, ee, dist)

    # remove already selected items from the population from which new
    # significant items are removed
    new_df1 <- subset(new_df, !(new_df$sampling_unit %in% initial$item))

    increment <- n1 - n0 # extended sample size

    # select new items

    if (increment > 0) {
      extension <- mus_select(new_df1$sampling_unit,
                              new_df1$bv,
                              increment, selMeth, seed)

    } else {
      extension <- data.frame()
    }
    return(list("extension" = extension,
                "significant" = new_sign_items[[1]],
                "nonsignificant" = new_sign_items[[2]]))

  }
  warning("No new items required")
}


#' Split population.
#'
#' @description Splits the population into positive, negative and zero-value
#'   book values.
#'
#' @details Monetary unit sampling is applied to populations with book values
#'   that are strictly positive or negative. Auditors may therefore be
#'   interested in those items that are not covered by the sampling procedure.
#'
#' @param id Sampling unit. Unique identifier.
#' @param bv Vector of population book values.
#'
#' @return The function returns a summary table with the number of sampling
#'   units and total book value for each of the positive, negative, and
#'   zero-value elements in the population, as well as tibbles with the unique
#'   identifier and book value of the population elements for each of the
#'   subpopulations. A summary of book values based on the split and list out
#'   the data corresponding to each category.
#' @importFrom tibble as_tibble
#' @examples
#' split_population(inventoryData$item, inventoryData$bv)
#' @export

split_population <- function(id, bv) {

  checkUnique(id)
  checkLengths(id, bv)
  checkNumeric(bv)

  population <- data.frame("item" = id, "bv" = bv)

  pos_bv <- as_tibble(population[population$bv > 0, ])
  neg_bv <- as_tibble(population[population$bv < 0, ])
  zero_bv <- as_tibble(population[population$bv == 0, ])

  summary_bv <- data.frame("no_bv" =  c(nrow(pos_bv),
                                        nrow(neg_bv),
                                        nrow(zero_bv),
                                        nrow(population)),
                           "sum_bv" = c(sum(pos_bv$bv),
                                        sum(neg_bv$bv),
                                        sum(zero_bv$bv),
                                        sum(population$bv)))
  row.names(summary_bv) <- c("pos_bv", "neg_bv", "zero_bv", "total")

  final <- list(summary_bv = summary_bv,
                pos_bv = pos_bv,
                neg_bv = neg_bv,
                zero_bv = zero_bv)
  return(final)
}
