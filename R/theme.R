##' bbplot theme
##'
##' setting visual details of bbplot
##' @title bbtheme
##' @param p bbplot object
##' @param ... parameters for graphics::par
##' @return bbplot object
##' @export
##' @author Guangchuang Yu
bbtheme <- function(p, ...) {
    p$theme <- modifyList(p$theme, list(...))
    return(p)
}
