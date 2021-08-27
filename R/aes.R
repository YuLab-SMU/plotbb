##' aesthetic mapping
##'
##'
##' aesthetic mapping for bbplot
##'
##' @title bb_aes
##' @param x x variable
##' @param y y variable
##' @param ... other mappings
##' @return enquo expression describing variables mapping to aesthetic attributes 
##' @importFrom rlang enquos
##' @export
##' @author Guangchuang Yu
bb_aes <- function(x, y, ...) {
    enquos(x = x, y = y, ..., .ignore_empty = "all")
}
