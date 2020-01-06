##' @rdname layer
##' @export
bb_text <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_text)
}

##' @importFrom graphics text
ly_text <- function(plot, mapping = NULL, data = NULL, palette = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)
    x <- data[[xvar(mapping)]]
    y <- data[[yvar(mapping)]]
    
    params <- list(...)
    params <- modifyList(params, list(x = x, y = y))

    lv <- get_mapping(mapping, 'label')
    if (lv != "NULL") {
        label <- data[[lv]]
        params <- modifyList(params, list(label = label))
    }

    if (!is.null(mapping$col)) {
        col_vec <- bb_col(mapping, data, palette = palette)
        params <- modifyList(params, list(col = col_vec))
    }

    ly <- function() do.call(text, params)
    add_layer(plot, ly, "text layer")
}

