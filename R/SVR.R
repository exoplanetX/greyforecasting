#' @export
svr <- function(y) {
  require(e1071)
  ds <- data.frame(y = y, t = 1:length(y))
  svr.model <- svm(y ~ t, data = ds, type = "eps-regression", kernel = "radial")
  MAPE <- mean(abs(ds$y - svr.model$fitted) / ds$y)
  cat(MAPE)
  return(svr.model)
}
