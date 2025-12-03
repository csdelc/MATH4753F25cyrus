#' Birthday
#'
#' @param x a birthday or number out opf 365
#'
#' @returns a probability from 0 to 1
#' @export
#'
#' @examples
#' birthday(20:25)
birthday <- function(x){
  1 - exp(lchoose(365,x) + lfactorial(x) -x*log(365))
}
