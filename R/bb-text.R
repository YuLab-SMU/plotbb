##' @rdname layer
##' @export
bb_text <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_text)
}

##' @importFrom graphics text
ly_text <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)
    xy <- bb_eval_xy(mapping, data)
    x <- xy$x
    y <- xy$y
    
    params <- list(...)
    params <- modifyList(params, list(x = x, y = y))

    label <- eval_mapping(mapping, 'label', data)
    if (!is.null(label) && is.null(params$label) && is.null(params$labels)) {
        params <- modifyList(params, list(label = label))
    }


    ly <- function() {
        if (!is.null(mapping$col) && is.null(params$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            params <- modifyList(params, list(col = col_vec))
        }

        do.call(text, params)
    }

    add_layer(plot, ly, "text layer")
}

