#' Stratification.
#'
#' Stratifies a population according to the equal recorded value boundaries or
#' cumulative square root of frequencies methods.
#'
#' The function assigns a stratum number from 1 to strata to each element in the
#' population.
#'
#' @param id Unique id. A unique identifier for population elements.
#' @param bv A list of population of book values.
#' @param popn Number of sampling units in the sampling frame. Calculated from
#'   the length of \code{id}.
#' @param n Sample size. Calculated with the \code{size} method, or entered by
#'   the user.
#' @param strata Desired number of strata. A strictly positive integer less than
#'   half of the number of population elements.
#' @param stratMeth Stratification method. Default method \code{equal} chooses
#'   equal recorded values boundaries; \code{cumulative} uses the cumulative
#'   square root of frequencies method.
#' @param classes Desired number of classes for use with the cumulative method.
#'   Default is 200.
#' @return A list containing the following: \describe{ \item{Stratification
#'   method}{\code{equal} or \code{cumulative}} \item{Strata summary}{Data frame
#'   consisting of results per stratum, indicating the stratum label, the
#'   minimum book value, the maximum book value, the number of items, and the
#'   cumulative sum of the square root of the frequencies}
#'   \item{Population}{Data frame consisting of the unique identifier, book
#'   value and stratum label} \item{Class summary}{Data frame of intermediate
#'   results per class, showing minimum and maximum book value, frequency,
#'   square root of frequency, cumulative sum of square roots of frequency, and
#'   the ultimate stratum.} }
#'
#' @importFrom stats aggregate

cvs_stratify <- function(id, bv, popn = NULL, n = NULL, strata, classes = 200,
                         stratMeth = "equal") {

  checkUnique(id)
  checkNumeric(bv)
  checkPositive(bv)
  checkPositive(strata)
  checkInteger(strata)
  checkPositive(classes)
  checkInteger(classes)
  checkOptions(stratMeth, c("equal", "cumulative"))

  population <- data.frame(id, bv)
  popn <- nrow(population)

  if (strata > popn / 2)
    stop("Number of strata should be less than half the population size.")

  if (strata == 1) {
    stop("Nothing to stratify. Enter a value for 'strat' larger than 1.")
  }

  population <- population[order(population[, 2]), , drop = FALSE]

  if (stratMeth == "equal") {

    targetSize <- sum(population[2]) / strata # Target size of stratum bv

    population$strat <- floor(cumsum(population$bv) / targetSize) + 1
    population$strat[popn] <- strata

    stratSumm <- aggregate(bv ~ as.factor(strat), data = population, sum)
    max_df    <- aggregate(bv ~ as.factor(strat), data = population, max)[, 2]
    min_df    <- aggregate(bv ~ as.factor(strat), data = population, min)[, 2]
    freq_df  <- aggregate(bv ~ as.factor(strat), data = population, length)[, 2]

    stratSumm$freq  <- freq_df
    stratSumm$minBv <- min_df
    stratSumm$maxBv <- max_df

    names(stratSumm) <- c("strat", "cum_bv", "freq", "minBv", "maxBv")


    # sdAvh: true SD of stratum h avs, or approximated by sd of book values
    sdAvh  <- aggregate(bv ~ strat, data = population, FUN = "sd")
    popNh  <- aggregate(bv ~ strat, data = population, FUN = "length")[, 2]
    popBvh <- aggregate(bv ~ strat, data = population, FUN = "sum")[, 2]
    alloc  <- cbind(sdAvh, popNh, popBvh)
    alloc$nh <- 0
    colnames(alloc) <- c("strat", "sdAvh", "popNh", "popBvh", "nh")
    sdAvh  <- sdAvh[, 2]

    if (!is.null(n)) { # Allocation
      nh <- round(n / sum(alloc$popBvh) * alloc$popBvh, 0)
      alloc$nh <- nh
    }

    return(list(stratSumm = stratSumm,
                population = population,
                alloc = alloc))
  }

  if (stratMeth == "cumulative") {
    checkLessthan(strata, classes)
    checkLessthan(classes, popn)

    stratRes <- stratification::strata.cumrootf(x = population$bv,
                      n = max(n, 50),
                      Ls = strata,
                      alloc = c(0.5, 0, 0.5),
                      nclass = classes)

    lowestBv <- min(population$bv)
    classWidth <- (max(population$bv) - lowestBv) / classes
    population$class <- ceiling((population$bv - lowestBv) / classWidth)
    population$class[1] <- 1
    population$one <- 1

    uniqueClass <- unique(population$class)
    freq <-  aggregate(one ~ class, data = population, FUN = "sum")[2]
    minBv <- aggregate(bv ~ class, data = population, FUN = "min")[2]
    maxBv <- aggregate(bv ~ class, data = population, FUN = "max")[2]
    classSumm <- data.frame(cl = uniqueClass,
                            freq = freq,
                            minBv = minBv,
                            maxBv = maxBv)

    colnames(classSumm) <- c("cl", "freq", "minBv", "maxBv")

    classSumm$sqrtFreq <- sqrt(classSumm$freq)
    classSumm$cmSum <- cumsum(classSumm$sqrtFreq)
    boundaries <- stratRes$bh
    boundaries[strata] <- max(bv)
    classSumm$strat <- pmin(findInterval(classSumm$maxBv, boundaries) + 1,
                            strata)

    lowestBv  <- aggregate(minBv ~ strat,    data = classSumm, FUN = "min")[2]
    highestBv <- aggregate(maxBv ~ strat,    data = classSumm, FUN = "max")[2]
    freq      <- aggregate(freq ~ strat,     data = classSumm, FUN = "sum")[2]
    sm        <- aggregate(sqrtFreq ~ strat, data = classSumm, FUN = "sum")[2]

    stratSumm <- data.frame(strat = seq_len(strata),
                            minBv = lowestBv,
                            maxBv = highestBv,
                            freq = freq,
                            cumSum = sm)

    names(stratSumm) <- c("strat", "minBv", "maxBv", "freq", "cumSum")
    population$strat <- stratRes$stratumID

    # sdAvh: true SD of stratum h avs, or approximated by sd of book values
    sdAvh  <- aggregate(bv ~ strat, data = population, FUN = "sd")
    popNh  <- aggregate(bv ~ strat, data = population, FUN = "length")[, 2]
    popBvh <- aggregate(bv ~ strat, data = population, FUN = "sum")[, 2]

    alloc <- cbind(sdAvh, popNh, popBvh)
    alloc$nh <- 0
    colnames(alloc) <- c("strat", "sdAvh", "popNh", "popBvh", "nh")
    sdAvh <- sdAvh[, 2]

    if (!is.null(n)) { # Allocation
      nh <-
        round(n * (popNh * sdAvh) / sum(popNh * sdAvh), 0)
      alloc$nh <- nh
    }

    # Need to return population as it may be in different order than before

    return(list(stratSumm = stratSumm,
                population = population,
                alloc = alloc,
                classSumm = classSumm))
  }
}

#' Sample size calculation and allocation to strata.
#'
#' The function calculates the sample size and allocates it to the strata using
#' either the proportional or the optimum (Neyman) method
#'
#' @param bv Book value. A vector of book values, typically obtained from the
#'   object. values and stratum labels. Obtained from \code{cvs_stratify}.
#' @param strat Stratum number. A vector the stratum numbers of each \code{bv}.
#'   Obtained from \code{stratify}.
#' @param strata Number of strata used. Obtained from \code{stratify}.
#' @param stratMeth Stratification method: \code{equal} or \code{cumulative}.
#'   Obtained from \code{cvs_stratify}.
#' @param cl Confidence level at which to reach the desired precision. A number
#'   between 0 and 1.
#' @param desPrec Desired precision (monetary amount).
#' @param sdAv Vector with length equal to \code{strata}. Estimated standard
#'   deviation of the population audit values. If omitted the standard deviation
#'   of the book values is calculated and used as an approximation.
#' @param alloc Allocation table of sample size.
#'
#' @return A data frame containing the stratum label, estimated standard
#'   deviations of the audit values, number of items, total book value, and
#'   allocated sample size. If the stratification method used in
#'   \code{cvs_stratify} is \code{equal} then proportional allocation is used to
#'   allocate the sample size; if stratification method is \code{cumulative}
#'   then Neyman allocation is used.

cvs_size <- function(bv, strat, strata, stratMeth, cl, desPrec, sdAv = NULL,
                     alloc) {
  checkNumeric(bv)
  checkPositive(strata)
  checkInteger(strata)
  checkOptions(stratMeth, c("equal", "cumulative"))
  checkBetween(cl, 0, 1)
  checkPositive(desPrec)
  checkPositive(sdAv)

  alpha <- 1 - cl
  p <- 1 - alpha / 2

  if (strata == 1) {
    strat <- rep(1, length(bv))
    alloc$popNh[1] <- length(bv)

    if (!is.null(sdAv)) {
      alloc$sdAvh[1] <- sdAv
    } else {
      alloc$sdAvh[1] <- sd(bv)
    }

    degrFreed <- 200
    t <- qt(p, degrFreed)
    gamma <-
      (desPrec^2 / ((alloc$popNh[1] - 1) * t^2 * alloc$sdAvh[1]^2))
    n <- ceiling(alloc$popNh[1] / (1 + gamma))
    alloc$nh <- n

    if (alloc$nh < 201) {
      degrFreed <- alloc$nh - 1
      t <- qt(p, degrFreed)
      gamma <-
        (desPrec^2 / ((alloc$popNh[1] - 1) * t^2 * alloc$sdAvh[1]^2))
      n <- ceiling(alloc$popNh[1] / (1 + gamma))
      alloc$nh <- n
    }

    sizeResults <- list(alloc = alloc,
                        n = n)

    return(sizeResults)

  } else {

    degrFreed <- 200
    t <- qt(p, degrFreed)

    n <- round(t^2 * sum(alloc$popNh * alloc$sdAvh)^2 /
                 (desPrec^2 + t^2 * sum(alloc$popNh * alloc$sdAvh^2)), 0)

    if (n < 200) {
      degrFreed <- n - 1

      if (degrFreed != 0) {
        t <- qt(cl, degrFreed)
        n <- round(t^2 * sum(alloc$popNh * alloc$sdAvh)^2 /
                     (desPrec^2 + t^2 * sum(alloc$popNh * alloc$sdAvh^2)), 0)
      }
    }

    # Allocation
    if (stratMeth == "equal") {
      nh <- round(n / sum(alloc$popBvh) * alloc$popBvh, 0)
    }
    if (stratMeth == "cumulative") {
      nh <- round(n * (alloc$popNh * alloc$sdAvh) /
                    sum(alloc$popNh * alloc$sdAvh), 0)
    }

    alloc$nh <- pmin(alloc$popNh, nh)

    return(list(alloc = alloc,
                n = n))
  }
}

#' Sample selection.
#'
#' The function selects the sample as allocated to the strata.
#'
#' @param population : A data frame consisting of population unique identifiers,
#'   book values and stratum labels. Obtained from \code{cvs_stratify}.
#' @param nh : Numeric vector of sample sizes per stratum. Obtained from
#'   \code{cvs_size}.
#' @param seed : Numeric, interpreted as an integer, between -2147483647
#'   (\eqn{2^32 - 1}) and 2147483647 (\eqn{2^32 - 1}).
#'
#' @return A data.frame with unique identifier, book value and stratum label of
#'   the selected elements.

cvs_select <- function(population, nh, seed) {

  checkNumeric(population$bv)
  checkPositive(nh)
  checkInteger(nh)
  sampleSize <- sum(nh)
  populationSize <- nrow(population)
  checkLessthan(sampleSize, populationSize)

  set.seed(seed)
  nLbls <- length(nh)
  dfNew <- c()
  for (x in 1:nLbls) {
    temp <- subset(population, strat == x)
    dfNew <- rbind(dfNew, temp[sample(nrow(temp), nh[x]), ])
  }

  return(dfNew)
}

#' Extend CVS sample.
#'
#' The function extends an initial simple or stratified CVS sample to achieve a
#' given desired precision.
#'
#' @param population Data frame consisting of the unique identifier, book
#'   value and stratum label. Obtained from \code{cvs_stratify}.
#' @param strata Number of strata.
#' @param sample A data frame with unique identifier, book
#'   value and stratum label of the selected elements in the initial sample.
#'   Obtained from \code{cvs_select}.
#' @param av A list of audit values.
#' @param desPrec Desired precision (monetary amount).
#' @param cl Confidence level at which to reach the desired precision.
#' @param stratMeth Method used for stratification (\code{equal} or
#'   \code{cumulative}).Obtained from \code{cvs_stratify}
#' @return a data.frame with extended samples selected for a simple or
#'   stratified random sample

cvs_extend <- function(population, strata, sample, av,
                       desPrec, cl, stratMeth) {

  # Input checks nog controleren!!
  if (!is.numeric(population[, 2]))
    stop("Non-numeric values for the population book values")
  if (!is.numeric(sample[, 2]))
    stop("Non-numeric values for the sample book values")
  if (!isTRUE(all(floor(sample$strat) ==
                  sample$strat)) |
      any(sample$strat <= 0))
    stop("Stratum label should be a positive integer")
  if (!is.numeric(av))
    stop("audit values should be a list of numeric values")

  sd_bv <- sd(population$bv)
  sd_av <- sd(av)

  # Only select items not part of the original sample
  df_sub <- population[!(population$bv %in% sample$bv), ]

  if (sd_av > sd_bv) {
    #obtain additional items
    alloc <- cvs_size(df_sub, strata, desPrec, cl, stratMeth, sd_av)
    nh <- alloc$nh
    output <- cvs_select(df_sub, nh, seed)
    return(output) # Return extended items frame
  } else {
    warning("No new items required")
  }
}

#' Evaluate CVS sample.
#'
#' Evaluates simple and stratified random samples with classical variables
#' methods, using the mean per unit, difference, ratio and regression
#' estimators.
#'
#' @param av List of audit values corresponding to the selected sampling units.
#' @param sample The selected sample. Obtained from \code{cvs_select}.
#' @param alloc A data.frame containing the stratum label, estimated standard
#'   deviations of the audit values, number of items, total book value, and
#'   allocated sample size. Obtained from \code{cvs_size}.
#' @param popBv Total book value of the population.
#' @param cl Confidence level. A number greater than 0 and less than 1. Obtained
#'   from \code{cvs_size}.
#'
#' @return List with results of the evaluation, including the effective number
#'   of degrees of freedom, a summary table with estimates, and for each of the
#'   four evaluation methods, a table with the sample size, number of nonzero
#'   differences, mean error value, and estimated regressor standard deviation,
#'   as well as the most likely audited amount, the most likely error, and the
#'   achieved precision.
#'
#' @importFrom stats var qt cor aggregate

cvs_eval <- function(av, sample, alloc, popBv, cl) {
  checkBetween(cl, 0, 1)

  alpha <- (1 - cl)

  popNh <- alloc$popNh
  nh <- alloc$nh


  sample$av <- av
  sample$diff <- sample$bv - sample$av

  mh <- aggregate(diff ~ strat, data = sample,
                 FUN = function(x) sum(x != 0))
  colnames(mh) <- c("strat", "mh")
  m <- mh[, 2]

  stratSumm <- merge(alloc, mh, by = "strat", all.x = TRUE)

  sum_bv  <- aggregate(bv ~ strat,   data = sample, FUN = "sum")[, 2]
  var_bv   <- aggregate(bv ~ strat,   data = sample, FUN = "var")[, 2]
  sum_av  <- aggregate(av ~ strat,   data = sample, FUN = "sum")[, 2]
  var_av   <- aggregate(av ~ strat,   data = sample, FUN = "var")[, 2]
  sum_d   <- aggregate(diff ~ strat, data = sample, FUN = "sum")[, 2]
  var_diff <- aggregate(diff ~ strat, data = sample, FUN = "var")[, 2]
  mev <- ((sum_av - sum_bv) / nh)
  sample$bvav <- sample$bv * sample$av
  sumBvAv <- aggregate(bvav ~ strat, data = sample, FUN = "sum")[, 2]

  sum_d_sq  <- aggregate(diff ~ strat, data = sample,
                                FUN = function(x) sum(x^2))[, 2]

  # Replace sdAvh in alloc with estimates from av
  alloc$sdAvh <- sqrt(var_av)

  g <- (popNh * (popNh - nh)) / nh

  effectiveDf <- function(g, variance, nh, m) {
    if (sum(m) > 2 & sum(m) < 20) {
      sum(m) - length(variance)
    } else {
      round((sum(g * variance)^2) / sum((g^2 * variance^2) / (nh - 1)), 0)
    }
  }

  aveSampBv <- sum_bv / nh
  aveSampAv <- sum_av / nh

  # Estimated audit value
  # mpu
  estAvMpuh <- popNh * aveSampAv
  estAvMpu  <- sum(estAvMpuh)
  # dif
  estAvDifh <- stratSumm$popBvh + (popNh * mev)
  estAvDif  <- sum(estAvDifh)
  # rat
  ratio     <- sum(popNh * aveSampAv) / sum(popNh * aveSampBv)
  estAvRat  <- ratio * popBv
  # regr
  covBvAv   <- (sumBvAv - nh * aveSampBv * aveSampAv) / (nh - 1)
  slope     <- sum(g * covBvAv) / sum(g * var_bv)
  estAvReg  <- estAvMpu + slope * (popBv - sum(popNh * (sum_bv / nh)))

  # Estimated error
  estErrMpu <- sum(popBv) - estAvMpu
  estErrDif <- sum(popBv) - estAvDif
  estErrRat <- sum(popBv) - estAvRat
  estErrReg <- sum(popBv) - estAvReg

  #Estimated precision
if (sum(m) >= 3) {
  mean_dm <- - (sum_d) / m # Average nonzero difference

  varDiffNonzeroes <- (sum_d_sq - (m * mean_dm^2)) / (m - 1)

  pU <- sapply(seq_len(length(nh)), function(x)
    upperHyper(popNh[x], nh[x], m[x], alpha) / popNh[x])

  varRatio <- var_av - 2 * ratio * covBvAv + ratio^2 * var_bv

}

  varDiff <- sapply(seq_len(length(nh)), function(x)
    if (m[x] >= 3 & m[x] <= 9) {
      pU[x] * varDiffNonzeroes[x] + pU[x] * (1 - pU[x]) * mean_dm[x]^2
    }else if (m[x] <= 2) {
      var_av[x]
    }else{
      var_diff[x]
    }
  )

  varRatio <- sapply(seq_len(length(nh)), function(x)
    if (m[x] >= 3 & m[x] <= 9)
      pU[x] * (nh[x] - 1) / (m[x] - 1) * varRatio[x]
    else if (m[x] <= 2) {
      var_av[x]
    }else{
      varRatio[x]
    })

  varRegr <- sapply(seq_len(length(nh)), function(x)
    if (m[x] >= 3 & m[x] <= 9) {
      (varDiff[x] - 2 * (slope - 1) * (covBvAv[x] - var_bv[x]) +
              (slope - 1)^2 * var_bv[x])
    } else if (m[x] <= 2) {
      var_av[x]
    } else {
      (nh[x] - 1) / (nh[x] - 2) *
        (var_av[x] - 2 * slope * covBvAv[x] + slope^2 * var_bv[x])
    })

  dfMpu   <- round(sum(g * var_av)^2 / sum((g * var_av)^2 / (nh - 1)), 0)
  dfDif   <- effectiveDf(g, varDiff, nh, m)
  dfRatio <- effectiveDf(g, varRatio, nh, m)
  dfReg   <- effectiveDf(g, varRegr, nh, m)

  precAchMpu <- qt(1 - alpha / 2, dfMpu)   * sqrt(sum(g * var_av))
  precAchDif <- qt(1 - alpha / 2, dfDif)   * sqrt(sum(g * varDiff))
  precAchRat <- qt(1 - alpha / 2, dfRatio) * sqrt(sum(g * varRatio))
  precAchReg <- qt(1 - alpha / 2, dfReg)   * sqrt(sum(g * varRegr))

  # Aggregating the results

  # mpu
  mpu <- data.frame(stratSumm[c("strat", "nh", "mh")],
                     mev, var_av)
  colnames(mpu) <-
    c("Stratum", "Size", "#_Errors", "Mean_error_value", "Est_regressor_var")

  # diff
  resultsDiff <-
    data.frame(stratSumm[c("strat", "nh", "mh")], mev, varDiff)
  colnames(resultsDiff) <-
    c("Stratum", "Size", "#_Errors", "Mean_error_value", "Est_regressor_var")

  # ratio
  resultsRat <-
    data.frame(stratSumm[c("strat", "nh", "mh")], mev, varRatio)
  colnames(resultsRat) <-
    c("Stratum", "Size", "#_Errors", "Mean_error_value", "Est_regressor_var")

  # regr
  resultsReg <- data.frame(stratSumm[c("strat", "nh", "mh")],
                           mev, varRegr)
  colnames(resultsReg) <-
    c("Stratum", "Size", "#_Errors", "Mean_error_value", "Est_regressor_var")

  # Create summary table of results

  estimates <- data.frame(matrix(ncol = 4, nrow = 6))
  names(estimates) <- c("mpu", "diff", "ratio", "regr")
  estimates$mpu   <- c(estAvMpu,
                       estErrMpu,
                       precAchMpu,
                       estAvMpu - precAchMpu,
                       estAvMpu + precAchMpu,
                       dfMpu)
  estimates$diff  <- c(estAvDif,
                       estErrDif,
                       precAchDif,
                       estAvDif - precAchDif,
                       estAvDif + precAchDif,
                       dfDif)
  estimates$ratio <- c(estAvRat,
                       estErrRat,
                       precAchRat,
                       estAvRat - precAchRat,
                       estAvRat + precAchRat,
                       dfRatio)
  estimates$regr  <- c(estAvReg,
                       estErrReg,
                       precAchReg,
                       estAvReg - precAchReg,
                       estAvReg + precAchReg,
                       dfReg)
  row.names(estimates) <- c("audit value",
                            "misstatement",
                            "precision",
                            "lower bound",
                            "upper bound",
                            "effective df")


  return(list("Estimates" = estimates,
              "Mean per unit estimation" = mpu,
              "Most likely total audited amount mpu" = estAvMpu,
              "Most likely total error mpu" = estErrMpu,
              "Achieved precision mpu" = precAchMpu,
              "Difference estimation" = resultsDiff,
              "Most likely total audited amount difference" = estAvDif,
              "Most likely total error difference" = estErrDif,
              "Achieved precision difference" = precAchDif,
              "Ratio estimation" = resultsRat,
              "Most likely total audited amount ratio" = estAvRat,
              "Most likely total error ratio" = estErrRat,
              "Achieved precision ratio" = precAchRat,
              "Regression estimation" = resultsReg,
              "Most likely total audited amount regression" = estAvReg,
              "Most likely total error regression" = estErrReg,
              "Achieved precision regression" = precAchReg))
}
