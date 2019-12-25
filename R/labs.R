##' change labels for bbplot
##'
##' setting one or several of 'title', 'sub', 'xlab', and 'ylab'
##' @title bb_labs
##' @rdname bblabs
##' @param title title
##' @param sub sub
##' @param xlab xlab
##' @param ylab ylab
##' @importFrom graphics title
##' @export
##' @return A modified bbplot object 
##' @author Guangchuang Yu
bb_labs <- function(title = NULL, sub = NULL, xlab = NULL, ylab = NULL) {
    labs <- list(main = title,
                 sub = sub,
                 xlab = xlab,
                 ylab = ylab
                 )
    labs <- labs[!vapply(labs, is.null, logical(1))]
    structure(labs, class = "bb_labs")
}

##' @rdname bblabs
##' @export
bb_title <- function(title) {
    bb_labs(title = title)
}

##' @rdname bblabs
##' @export
bb_sub <- function(sub) {
    bb_labs(sub = sub)
}

##' @rdname bblabs
##' @export
bb_xlab <- function(xlab) {
    bb_labs(xlab = xlab)
}

##' @rdname bblabs
##' @export
bb_ylab <- function(ylab) {
    bb_labs(ylab = ylab)
}


##' @method bbplot_add bb_labs
##' @export
bbplot_add.bb_labs <- function(object, plot) {
    plot$labs <- modifyList(plot$labs, object)
    plot
}
