##' layer
##'
##' bbplot layers
##' @title layer
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param position one of 'identity' or 'jitter'
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics points
##' @export
##' @examples
##' library(plotbb)
##' p <- bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl))) +
##'   bb_point() + bb_lm(bb_aes(group=cyl), lwd=2)
##' @author Guangchuang Yu
bb_point <- function(mapping = NULL, data = NULL, position = "identity", ...) {
    build_layer(mapping, data, ...,  layer = ly_point)
}

ly_point <- function(plot, mapping = NULL, data = NULL, position = "identity", ...) {
    position <- match.arg(position, c("identity", "jitter"))

    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    x <- data[[xvar(mapping)]]
    y <- data[[yvar(mapping)]]

    if (position == "jitter") {
        x <- jitter(x)
        y <- jitter(y)
    }

    params <- list(...)
    params <- modifyList(params, list(x = x, y = y))

    ly <- function() {
        if (!is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data,
                              palette = get("palette", envir = plot$env))
            params <- modifyList(params, list(col = col_vec))
        }

        do.call(points, params)
    }
    add_layer(plot, ly, "point layer")
}

##' @method bbplot_add bb_layer
##' @export
bbplot_add.bb_layer <- function(object, plot) {
    ly <- object$layer
    params <- c(object, unlist(object$params))
    params <- params[names(params) != 'params']
    params <- params[names(params) != 'layer']
    params$plot <- plot

    do.call(ly, params)
}

