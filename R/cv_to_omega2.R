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
    omega[c("True \u03C9\U00B2", "Appr \u03C9\U00B2")] <- signif(
      omega[c("True \u03C9\U00B2", "Appr \U03C9\U00B2")],
      digits = digits
    )
    return(omega)
  }
}
