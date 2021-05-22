#' mean imaage operator
#'
mimage <- function(x){
  r=x/mean(x)
  return(r)
}
