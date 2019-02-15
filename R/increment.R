#' Increment and decrement functions
#'
#' @param x
#' @param value
#'
#' @return
#' @export
#'
#' @examples
"+<-" = function(x, value) {
    x + value
}

#' @export
#' @rdname "+-"
"-<-" = function(x, value) {
    x - value
}
