#' APE
#'
#' @param original
#' Observation value
#' @param simulation
#' Simulation value
#' @export
#'
ape <- function(original, simulation) {
  abs(original - simulation) / original
}
