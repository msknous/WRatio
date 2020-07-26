#' W-Ratio Critical Values
#'
#' This function returns the critical values for the W-Ratio Test.
#' @param size Length of Data Set.  Must between 3 and 20.
#' @param alpha Desired Alpha Level. Must be 0.05, 0.10, 0.15, or 0.2.
#' @export
#' @examples
#' WCrit(3,.01)

WCrit <- function(size, alpha) {
  # perform checks to ensure arguments meet the requirements of the function ---
  stopifnot(size >= 3 & size <= 20)
  stopifnot(alpha %in% c(.01,.05,.15,.2))
  # pulls and returns critical values from an internal data frame --------------
  return(as.vector(t(WRatio:::wratiotable[which(WRatio:::wratiotable$n == size &
          WRatio:::wratiotable$a == alpha),][3:(1+size)])))
}
