#' W-Ratio Values
#'
#' This function returns the W-Ratios of a dataset.
#' @param data Dataset must be a vector.
#' @export
#' @examples
#' WRatios(electrons)

WRatios <- function(data) {
  tempdata <- sort(data)
  denominator <- (tempdata[length(tempdata)] - tempdata[1])
  ratios <- c()
  for(i in 2:length(tempdata)-1) {
    ratio <- ((tempdata[i+1] - tempdata[i]) / denominator)
    ratios <- append(ratios,ratio)
  }
  return(ratios)
}


