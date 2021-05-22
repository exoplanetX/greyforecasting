#'
#' @export

background2 <- function(y){
  n=length(y)
  y1<-cumsum(y)
  r=(y1[2:n]+y1[1:(n-1)]) / 2
  return(r)
}
