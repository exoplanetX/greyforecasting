#' Smooth buffer operator with variable weight
#'
#' @param y original data
#' @param alpha weight,alpha=0.5
#' @examples format:y <- svwbo(y, alpha = 0.5)
#' @export
svwbo <- function(y, alpha = 0.5) {
  if (is.na(alpha)) {
    alpha <- 0.5
  } else {
    if (!(alpha <= 1 && alpha >= 0)) {
      alpha <- 0.5
      warning("parameter alpha out of possible range")
    }
  }
  for (i in rev(seq_along(y[1:(length(y) - 1)]))) {
    y[i] <- alpha * y[i] + (1 - alpha) * y[i + 1]
  }
  y
}
