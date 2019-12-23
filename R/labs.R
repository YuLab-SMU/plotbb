##' change labels for bbplot
##'
##' setting one or several of 'title', 'sub', 'xlab', and 'ylab'
##' @title bblabs
##' @rdname bblabs
##' @param p bbplot object
##' @param title title
##' @param sub sub
##' @param xlab xlab
##' @param ylab ylab
##' @importFrom graphics title
##' @export
##' @return bbplot object 
##' @author Guangchuang Yu
bblabs <- function(p, title = NULL, sub = NULL, xlab = NULL, ylab = NULL) {
    if (!is.null(title)) p <- bbtitle(p, title)
    if (!is.null(sub)) p <- bbsub(p, sub)
    if (!is.null(xlab)) p <- bbxlab(p, xlab)
    if (!is.null(ylab)) p <- bbylab(p, ylab)
    return(p)
}

##' @rdname bblabs
##' @export
bbtitle <- function(p, title) {
    p$labs$title <- function() title(main = title)
    return(p)
}

##' @rdname bblabs
##' @export
bbsub <- function(p, sub) {
    p$labs$sub <- function() title(sub = sub)
    return(p)
}

##' @rdname bblabs
##' @export
bbxlab <- function(p, xlab) {
    p$labs$xlab <- function() title(xlab = xlab)
    return(p)
}

##' @rdname bblabs
##' @export
bbylab <- function(p, ylab) {
    p$labs$ylab <- function() title(ylab = ylab)
    return(p)
}
