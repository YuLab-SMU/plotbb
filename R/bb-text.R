##' @rdname layer
##' @export
bb_text <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_text)
}

##' @importFrom graphics text
ly_text <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_layer_data(plot, data, mapping)
    mapping <- bb_mapping(plot, mapping)
    params <- list(...)
    xy <- bb_eval_xy(mapping, data)

    prepared <- bb_prepare_aes(xy$x, xy$y, mapping, data, "bb_text", params,
                               extra = "label")
    if (is.null(prepared)) {
        return(add_layer(plot, function() invisible(NULL), "text layer"))
    }
    x <- prepared$x
    y <- prepared$y
    keep <- prepared$keep

    params <- modifyList(params, list(x = x, y = y))

    label <- eval_mapping(mapping, 'label', data)
    if (!is.null(label)) {
        label <- rep(label, length.out = prepared$n)[keep]
    }
    if (!is.null(label) && is.null(params$label) && is.null(params$labels)) {
        params <- modifyList(params, list(label = label))
    }


    ly <- function() {
        if (!is.null(mapping$col) && is.null(params$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot),
                                     keep, prepared$n)
            params <- modifyList(params, list(col = col_vec))
        }

        do.call(text, params)
    }

    add_layer(plot, ly, "text layer")
}
