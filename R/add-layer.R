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



add_layer <- function(plot, layer, layer_name = "customized layer") {
    nlayer <- length(plot$layer) + 1
    plot$layer[[nlayer]] <- layer
    names(plot$layer)[nlayer] <- layer_name
    return(plot)
}

build_layer <- function(mapping, data, ..., layer) {
    structure(list(mapping = mapping,
                   data = data,
                   params = list(...),
                   layer = layer
                   ),
              class = "bb_layer")
}

##' @method <= bb_theme
##' @importFrom methods is
##' @export
`<=.bb_theme` <- function(e1, e2) {
    stopifnot(is(e2, "bb_theme"))
    modifyList(e1, e2)
}

##' @method bbplot_add bb_palette
##' @export
bbplot_add.bb_palette <- function(object, plot) {
    assign("palette", object$palette, envir = plot$env)
    return(plot)
}

##' @method bbplot_add formula
bbplot_add.formula <- function(object, plot) {
    envir <- parent.frame()
    layer <- plot_fun(object)
    with_env(layer, envir)
    add_layer(plot, layer)
}

##' @method bbplot_add expression
bbplot_add.expression <- bbplot_add.formula

##' @method bbplot_add function
bbplot_add.function <- bbplot_add.formula

