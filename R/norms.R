#' distance norm computation
#'
norms <- function(x,p=2){
  sum(abs(x)^p)^(1/p)
}
