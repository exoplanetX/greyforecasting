#' fetch the parameters in grey forecasting object
#' usage:
#' g<-gm(y)
#' coef(g)
#' @export
coef.greyforecasting <- function(x){
  cat("parameters a and b are:\n")
  print(x$parameter)
}
