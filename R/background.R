#'
#' @export
background <- function(x1){
  n=length(x1)
  r=(x1[2:n]+x1[1:(n-1)]) / 2
  return(r)
}
