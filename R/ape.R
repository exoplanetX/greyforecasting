#' @export
ape <- function(original, simulation) {
  abs(original - simulation) / original
}
