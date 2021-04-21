#' Compare errors according to data point's name
#'
#' @param y1 first data poinnt
#' @param y2 second data point
#'
#' @return
#'
#' absolute errors:ae
#' percentage absolute errors:pae
#'
#' @export
#'
compareseq <- function(y1, y2) {
  # if(is.na(names(y1))){return("the first argument'names contains NA")}
  s <- 1
  varname <- c()
  ae <- c()
  pae <- c()
  n1 <- length(y1)
  n2 <- length(y2)
  for (i in seq_along(y2)) {
    for (j in seq_along(y1)) {
      if (names(y2)[i] == names(y1)[j]) {
        varname[s] <- names(y2)[i]
        ae[s] <- abs(y2[i] - y1[j])
        pae[s] <- ae[s] / y1[j]
        s <- s + 1
      }
    }
  }
  data.frame(row.names = varname, ae = ae, pae = pae)
}
