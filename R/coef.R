#' Fetch the parameters in grey forecasting object
#'
#' @param object
#' a greyforecasting object
#'
#' @export
#'
#' @examples
#' g<-gm(y)
#'
#' coef(g)
#'
coef.greyforecasting <- function(object) {
  cat("parameters a and b are:\n")
  print(object$parameter)
}
