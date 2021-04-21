#' Make the summary of greyforecasting object
#'
#' @param x
#' a greyforecasting object
#'
#' @examples
#' g<-gm(y)
#' summary(g)
#' @export
summary.greyforecasting <- function(x) {
  # if(is.na(names(y1))){return("the first argument'names contains NA")}
  original <- x$original[(length(x$original) - length(x$simulation) + 1):length(x$original)]
  simulation <- x$simulation

  ae <- abs(original - simulation)
  pae <- ae / original
  varname <- names(original) # [(length(x$original)-length(x$simulation)+1):length(x$simulation)]
  print(data.frame(row.names = varname, ae = ae, pae = pae))
  test <- abs(x$testvalue - x$forecasts[1:length(x$testvalue)]) / x$testvalue
  cat("MSE in -sample is:", mean(pae), "\n")
  cat("MSE out-sample is:", test)
}
