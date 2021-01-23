#' @export
bo.obj <- function(x,formula=NULL,expression=NULL){
  obj <-list(
    data=x,
    formula=formula,
    expression=expression
  )
  class(obj) <- "bo"
  obj
}
