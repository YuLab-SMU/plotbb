##' @rdname layer
##' @export
bb_tile <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_tile)
}


ly_tile <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    ly <- function() {
        data$col <- bb_col(mapping, data,
                           palette = get("palette", envir = plot$env))
        for (i in 1:nrow(data)) {
            rectangle(data$x[i], data$y[i], col = data$col[i])
        }
    }
    layer_name <- "heatmap layer"
    plot <- add_layer(plot, ly, layer_name)
    return(plot)
}

##' @importFrom graphics polygon
rectangle <- function(x, y, col, ...) {
    r0 = 0.5
    xx <- c(x - r0, x-r0, x + r0, x+ r0)
    yy <- c(y - r0, y + r0, y+r0, y - r0)
    polygon(xx, yy, col = col, ...)
}

