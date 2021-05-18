#' Combine multi-graphs based on matrix "layoutset"
#'
#' @param layoutset
#' matrix "layoutset" may consist of dimensions or facets sequences
#'
#' @param ...
#'
#' #' @export

combine <- function(layoutset, ...) {
  graphset <- list(...)
  n <- length(graphset)
  for (i in 1:n) {
    if (!is.function(graphset[[i]])) {
      return("input element must be graphic function for grey modeling")
    }
  }
  if (is.vector(layoutset)) {
    par(mfrow = layoutset)
  } else {
    layout(layoutset)
  }
  for (j in 1:n) {
    graphset[[j]]()
  }
}
