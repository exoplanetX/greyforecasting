#' MAPE
#'
#' @param original
#' Observation value
#' @param simulation
#' Simulation value
#' @export
#'
mape <- function(original, simulation) {
  error <- mean(abs(original - simulation) / original)
  return(error)
}
