##' @method + bbplot
##' @export
`+.bbplot` <- function(e1, e2) {
    if (is.null(e2)) return(e1)
    if (inherits(e2, "bbplot")) {
        bb_abort("`+` adds components to one plot; it does not combine two ",
                 "complete plots. Use `aplot::plot_list()` to arrange ",
                 "several `bbplot` objects.")
    }
    ## A component method may add layers/components through `+`; those are
    ## produced by the component itself and must not be re-added a second time
    ## when a facet panel is rebuilt. Mark them so the facet loops can skip
    ## them and re-run the owning component instead.
    if (isTRUE(getOption("plotbb.in_add"))) {
        attr(e2, "bb_generated") <- TRUE
    }
    e1$adds[[length(e1$adds) + 1]] <- e2
    bbplot_add(e2, e1)
}

##' Add custom objects to bbplot
##'
##' This generic is the extension point for new kinds of components. A method
##' receives the object and the plot and must return the modified plot; it
##' should describe the resulting plot instead of drawing, and can rely on the
##' public \code{bb_layer()} (or the other public constructors) to add the
##' drawing.
##'
##' A method must not draw directly, change the device or rely on internal
##' helpers such as \code{build_layer()}; those are implementation details and
##' are not part of the stable contract. Packages register the method with
##' \code{S3method(bbplot_add, <class>)} in their \code{NAMESPACE}. A component
##' added this way takes part in the \code{+} order and in the graphics-state
##' restoration like every other component, and it is re-applied to every facet
##' panel. The layers a method adds are not added a second time when a panel is
##' rebuilt: the owning component is re-run instead.
##'
##' @title bbplot_add
##' @param object An object to add to the plot
##' @param plot The bbplot object to add `object` to
##' @return A modified bbplot object
##' @seealso \code{\link{bb_layer}}
##' @export
##' @examples
##' bb_note <- function(label) {
##'     structure(list(label = label), class = "bb_note")
##' }
##'
##' bbplot_add.bb_note <- function(object, plot) {
##'     plot + bb_layer(function(x, y, ...) {
##'         graphics::text(mean(range(x)), mean(range(y)), object$label)
##'     })
##' }
##'
##' bbplot(mtcars, bb_aes(mpg, disp)) +
##'     bb_point(pch = 19) +
##'     bb_note("a third-party component")
##' @author Guangchuang Yu
bbplot_add <- function(object, plot) {
    old <- getOption("plotbb.in_add")
    options(plotbb.in_add = TRUE)
    on.exit(options(plotbb.in_add = old), add = TRUE)
    UseMethod("bbplot_add")
}



add_layer <- function(plot, layer, layer_name = "customized layer") {
    nlayer <- length(plot$layer) + 1
    plot$layer[[nlayer]] <- layer
    names(plot$layer)[nlayer] <- layer_name
    return(plot)
}

build_layer <- function(mapping, data, ..., inherit = TRUE, limits = NULL, layer) {
    structure(list(mapping = mapping,
                   data = data,
                   inherit = isTRUE(inherit),
                   limits = limits,
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
    if (is.null(plot$scales)) plot$scales <- list(col = NULL, pch = NULL, lty = NULL, cex = NULL)
    if (!exists("scales", envir = plot$env, inherits = FALSE)) {
        assign("scales", list(col = NULL, pch = NULL, lty = NULL, cex = NULL), envir = plot$env)
    }

    if (!is.null(plot$data) && !is.null(plot$mapping$col)) {
        col_var <- eval_mapping(plot$mapping, "col", plot$data)
        if (!is.null(col_var)) {
            if (is.numeric(col_var)) {
                object$domain <- range(col_var, na.rm = TRUE)
            } else {
                object$domain <- bb_discrete_levels(col_var)
            }
        }
    }

    plot$scales$col <- object
    scales <- get("scales", envir = plot$env, inherits = FALSE)
    scales$col <- object
    assign("scales", scales, envir = plot$env)
    return(plot)
}

##' @method bbplot_add formula
##' @export
bbplot_add.formula <- function(object, plot) {
    envir <- parent.frame()
    layer <- plot_fun(object)
    with_env(layer, envir)
    add_layer(plot, layer)
}

##' @method bbplot_add expression
##' @export
bbplot_add.expression <- bbplot_add.formula

##' @method bbplot_add call
##' @export
bbplot_add.call <- bbplot_add.formula

##' @method bbplot_add function
##' @export
bbplot_add.function <- bbplot_add.formula

