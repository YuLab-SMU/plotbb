##' @method + bbplot
##' @export
`+.bbplot` <- function(e1, e2) {
    if (is.null(e2)) return(e1)
    bbplot_add(e2, e1)
}

##' Add custom objects to bbplot
##'
##' This generic allows you to add your own methods for
##' adding custom objects to a bbplot object.
##'
##' @title bbplot_add
##' @param object An object to add to the plot
##' @param plot The bbplot object to add `object` to
##' @return A modified bbplot object
##' @keywords internal
##' @export
##' @author Guangchuang Yu
bbplot_add <- function(object, plot) {
    UseMethod("bbplot_add")
}



add_layer <- function(plot, layer) {
    nlayer <- length(plot$layer)
    if (nlayer == 0) {
        plot$layer <- list(layer)
    } else {
        plot$layer[[nlayer + 1]] <- layer
    }
    return(plot)
}


