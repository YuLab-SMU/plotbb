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
    default <- list(
        cex.axis = .9,
        las = 1
    )
    params <- modifyList(
        default,
        list(...)
    )
    structure(params, class = "bb_theme")
}

##' @method bbplot_add bb_theme
##' @export
bbplot_add.bb_theme <- function(object, plot) {
    plot$theme <- plot$theme <= object
    plot
}


##' @rdname bb-theme
##' @export
bb_theme_expand <- function(...) {
    bb_theme(
        mar=c(3,3,2,1),
        mgp=c(2,0.4,0),
        tck=-.01
    ) <= bb_theme(...)
}

##' @rdname bb-theme
##' @export
bb_theme_grey <- function(...) {
    bb_theme(
        bg = "grey85",
        fg = "grey20",
        col.axis = "grey20",
        col.lab = "grey20"
    ) <= bb_theme(...)
}

##' @rdname bb-theme
##' @export
bb_theme_deepblue <- function(...) {
    bb_theme(
        bg = "#002E49",
        fg = "#CCCCCC",
        col.axis = "#BEBEBE",
        col.lab = "#FFFFFF"
    ) <= bb_theme(...)
}

build_theme <- function(default, ...) {
    ## default is a list of default setting
    do.call(bb_theme, default) <= bb_theme(...)
}
