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
    structure(list(main = title,
                   sub = sub,
                   xlab = xlab,
                   ylab = ylab
                   ),
              class = "bb_labs")
}

##' @rdname bblabs
##' @export
bb_title <- function(title) {
    structure(list(main = title), class = "bb_labs")
}

##' @rdname bblabs
##' @export
bb_sub <- function(sub) {
    structure(list(sub = sub), class = "bb_labs")
}

##' @rdname bblabs
##' @export
bb_xlab <- function(p, xlab) {
    structure(list(xlab = xlab), class = "bb_labs")
}

##' @rdname bblabs
##' @export
bb_ylab <- function(p, ylab) {
    structure(list(ylab = ylab), class = "bb_labs")
}


##' @method bbplot_add bb_labs
##' @export
bbplot_add.bb_labs <- function(object, plot) {
    plot$labs <- modifyList(plot$labs, object)
    plot
}
