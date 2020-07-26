#' W-Ratio Test
#'
#' This function performs the W-Ratio test on a data set.  This also returns a data frame with the detailed test results.
#' @param data Dataset.  Must be between 3 and 20 values.
#' @param alpha Desired Alpha Level. Must be 0.05, 0.10, 0.15, 0.2, or NULL.  If NULL is selected alpha will be set based upon the dataset size.
#' @export
#' @examples
#' WRatioTest(electrons)

WRatioTest <- function(data, alpha = NULL) {
  # if alpha is not specified a default alpha is set based on the sample size---
  if(is.null(alpha)) {
    if (length(data) %in% 3:4) {alpha <- 0.2}
    else if (length(data) %in% 5:7) {alpha <- 0.15}
    else if (length(data) %in% 8:9) {alpha <- 0.10}
    else if (length(data) > 9) {alpha <- 0.05}
  }
  # checks to ensure the dataset and alpha meet the requirments of the test-----
  stopifnot(length(data) >= 3 & length(data) <= 20)
  stopifnot(alpha %in% c(.01,.05,.15,.2))
  # create values for the table ------------------------------------------------
  w_ratios <- WRatios(data)
  critical_w_ratios <- WCrit(length(data),alpha)
  test_results <- w_ratios <= critical_w_ratios
  tempdata <- sort(data)
  outputframe <- data.frame(ordered_values = tempdata[1:(length(tempdata)-1)],
                           ordered_values2 = tempdata[-1],
                           w_ratios,
                           critical_w_ratios,
                           test_results)
  if (all(test_results)) {
    cat("The W-Ratio test at alpha=", alpha,
        "has identified the data set as homogeneous \n",
        "because no W-Ratio was above its critical \n")
  } else {
    cat("The W-Ratio test at alpha=", alpha,
        "has identified the data set as hetergeneous \n",
        "because atleast one W-Ratio was above its critical value.\n")
  }
  return(outputframe)
}
