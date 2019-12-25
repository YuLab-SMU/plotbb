##' bbplot theme
##'
##' setting visual details of bbplot
##' @title bb_theme
##' @rdname bb-theme
##' @param ... parameters for graphics::par
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_theme <- function(...) {
    structure(list(...), class = "bb_theme")
}

##' @method bbplot_add bb_theme
##' @export
bbplot_add.bb_theme <- function(object, plot) {
    plot$theme <- modifyList(plot$theme, object)
    plot
}


##' @rdname bb-theme
##' @export
bb_theme_expand <- function(...) {
    theme_default <- bb_theme(
        mar=c(3,3,2,1),
        mgp=c(2,0.4,0),
        tck=-.01,
        cex.axis=.9,
        las=1
    )
    modifyList(theme_default, bb_theme(...))
}

##' @rdname bb-theme
##' @export
bb_theme_grey <- function(...) {
    theme_default <- bb_theme(
        cex.axis = .9, bg = "grey85",
        fg = "grey20",
        col.axis = "grey20",
        col.lab = "grey20")
    modifyList(theme_default, bb_theme(...))
}
