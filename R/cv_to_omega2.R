#' Convert coefficient of variation (CV) to variance (ω²)
#'
#' @description
#' `cv_to_omega2()` converts coefficient of variation (CV) to variance (ω²) and
#' allows you to compare the output of different conversion methods.
#'
#' @param cv A numeric vector of cv values.
#' @param units Specify the way cv is supplied.
#'  * `"percent"` treats cv as a percent (e.g. 30% is formatted as 30). This is
#'  the default.
#'  * `"fraction"` treats cv as a fraction (e.g. 30% is formatted as 0.3).
#' @param method Specify the conversion method.
#'  * `"comparison"` gives an overview of both method results and their difference
#'  in percent. This is the default.
#'  * `"true"` converts using the inverse of the 'true' method for conversion of
#'  variance to CV.
#'  * `"approximate"` converts using the inverse of the 'approximate' method for
#'  conversion of variance to CV.
#' @param digits Integer indicating the number of significant digits to be used
#'  for the variance.
#'
#' @returns When `method = "comparison"`, a data frame containing:
#'  * The original CV;
#'  * The variance according to the inverse of the 'true' method;
#'  * The variance according to the inverse of the 'approximate' method;
#'  * The difference between the 'true' and 'approximate' variance in percent.
#'
#'  When `method = "true"` or `"approximate"`, a numeric vector.
#' @export
#'
#' @examples
#' cv_percent <- c(32.3, 20.4)
#' cv_to_omega2(cv_percent)
#'
#' # Specifying either method = "true" or "approximate" outputs a vector
#' cv_to_omega2(cv_percent, method = "approximate")
#'
#' # Fractions also work
#' cv_fraction <- c(0.323, 0.204)
#' cv_to_omega2(cv_fraction, units = "fraction")
#'
cv_to_omega2 <- function(
  cv,
  units = c("percent", "fraction"),
  method = c("comparison", "true", "approximate"),
  digits = 3
) {
  if (!is.numeric(cv)) {
    stop("`cv` must be numeric")
  }
  if (any(is.na(cv))) {
    stop("`cv` contains missing value(s)")
  }
  units <- match.arg(units)
  method <- match.arg(method)
  if (units == "percent" && any(cv < 1)) {
    warning(
      "One or more CV values are below 1 while `units = 'percent'`.\n\nPlease verify that the supplied CV values are percentages rather than fractions.",
      call. = FALSE
    )
  }
  if (units == "fraction" && any(cv > 1)) {
    warning(
      "One or more CV values are above 1 while `units = 'fraction'`.\n\nPlease verify that the supplied CV values are fractions rather than percentages",
      call. = FALSE
    )
  }
  if (units == "percent") {
    cv = cv / 100
  }
  if (method == "approximate") {
    omega = cv^2
  } else if (method == "true") {
    omega = log(cv^2 + 1)
  } else if (method == "comparison") {
    true = log(cv^2 + 1)
    appr = cv^2
    diff_percent = round(appr / true * 100 - 100, digits = 1)
    omega = data.frame(
      "CV(%)" = cv * 100,
      "True \U03C9\U00B2" = true,
      "Appr \U03C9\U00B2" = appr,
      "Difference(%)" = diff_percent,
      check.names = FALSE
    )
  }
  if (is.vector(omega)) {
    omega <- signif(omega, digits = digits)
  } else {
    omega[c("True \U03C9\U00B2", "Appr \U03C9\U00B2")] <- signif(
      omega[c("True \U03C9\U00B2", "Appr \U03C9\U00B2")],
      digits = digits
    )
    return(omega)
  }
}
