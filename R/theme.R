##' bbplot theme
##'
##' setting visual details of bbplot
##' @title bb_theme
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
