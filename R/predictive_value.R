

#' Compute positive and negative predictive value
#' @param sensitivity,specificity,prevalence vectors of confusion matrix rates
#' @return a tibble with the columns `sensitivity`, `specificity`, `prevalence`,
#'   `ppv`, `npv` where `ppv` and `npv` are the positive predictive value and
#'   the negative predictive value.
#' @export
#' @concept roc
#' @details These vectors passed into this function should be some common length
#' and/or length 1. For example, 4 sensitivities, 4 specificities and 1
#' incidence will work because the sensitivities and specificities have a common
#' length and we can safely recycle (reuse) the incidence value. But 4
#' sensitivities, 2 specificities, and 1 incidence will *fail* because there is
#' not a common length.
#' @examples
#' compute_predictive_value_from_rates(
#'   sensitivity = .9,
#'   specificity = .8,
#'   prevalence = .05
#' )
#'
#' compute_predictive_value_from_rates(
#'   sensitivity = .67,
#'   specificity = .53,
#'   prevalence = c(.15, .3)
#' )
compute_predictive_value_from_rates <- function(
  sensitivity,
  specificity,
  prevalence
) {
  # want equal-length vectors or scalars
  lengths <- lengths(list(sensitivity, specificity, prevalence))
  if (any(! lengths %in% c(1, max(lengths)))) {
    lengths <- ifelse(
      lengths == 1,
      paste0(lengths, " (always compatible)"),
      lengths
    )
    rlang::abort(c(
      "Rates must have compatible lengths (a common length and/or length 1)",
      "i" = paste0("length(sensitivity) = ", lengths[1]),
      "i" = paste0("length(specificity) = ", lengths[2]),
      "i" = paste0("length(prevalence)  = ", lengths[3])
    ))
  }

  # ppv = true positive / tested positive
  true_positive <- sensitivity * prevalence
  false_positive <- (1 - specificity) * (1 - prevalence)
  tested_positive <- true_positive + false_positive
  ppv <- true_positive / tested_positive

  # npv = true negative / tested negative
  true_negative <- specificity * (1 - prevalence)
  false_negative <- (1 - sensitivity) * prevalence
  tested_negative <- true_negative + false_negative
  npv <- true_negative / tested_negative

  tibble::tibble(
    sensitivity = sensitivity,
    specificity = specificity,
    prevalence = prevalence,
    ppv = ppv,
    npv = npv
  )
}
