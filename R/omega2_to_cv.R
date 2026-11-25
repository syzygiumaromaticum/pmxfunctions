omega2_to_cv <- function(
  omega2,
  method = c("comparison", "true", "approximate"),
  digits = 3
) {
  if (!is.numeric(omega2)) {
    stop("`omega2` must be numeric")
  }
  if (any(is.na(omega2))) {
    stop("`omega2` contains missing value(s)")
  }
  method <- match.arg(method)
  if (method == "approximate") {
    cv = 100 * sqrt(omega2)
  } else if (method == "true") {
    cv = 100 * sqrt(exp(omega2) - 1)
  } else if (method == "comparison") {
    true = 100 * sqrt(exp(omega2) - 1)
    appr = 100 * sqrt(omega2)
    diff_percent = round(appr / true * 100 - 100, digits = 1)
    cv = data.frame(
      "\U03C9\U00B2" = omega2,
      "True CV(%)" = true,
      "Appr CV(%)" = appr,
      "Difference(%)" = diff_percent,
      check.names = FALSE
    )
  }
  if (is.vector(cv)) {
    cv <- signif(cv, digits = digits)
  } else {
    cv[c("True CV(%)", "Appr CV(%)")] <- signif(
      cv[c("True CV(%)", "Appr CV(%)")],
      digits = digits
    )
    return(cv)
  }
}
