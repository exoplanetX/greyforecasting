#' Means of the Background value
#'
#' @param y
#'
#' @return
#' Background Object
#' @export
background <- function(y) {
  y1 <- cumsum(y)
  (y1[2:length(y)] + y1[1:(length(y) - 1)]) / 2
}
