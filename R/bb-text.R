##' @rdname layer
##' @export
bb_text <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ...,  layer = ly_text)
}

##' @importFrom graphics text
ly_text <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params, extra = "label")
    xy <- bb_eval_xy(mapping, data)

    prepared <- bb_prepare_aes(xy$x, xy$y, mapping, data, "bb_text", params,
                               extra = "label")
    if (is.null(prepared)) {
        return(add_layer(plot, function() invisible(NULL), "text layer"))
    }
    x <- prepared$x
    y <- prepared$y
    keep <- prepared$keep
    params <- bb_layer_constants(params, keep, prepared$n)

    params <- modifyList(params, list(x = x, y = y))

    constant_col <- !is.null(params$col)
    constant_label <- !is.null(params$label) || !is.null(params$labels)

    label <- eval_mapping(mapping, 'label', data)
    if (!is.null(label)) {
        label <- rep(label, length.out = prepared$n)[keep]
    }
    if (!is.null(label) && !constant_label) {
        params <- modifyList(params, list(label = label))
    }


    ly <- function() {
        if (!constant_col && !is.null(mapping$col)) {
            col_vec <- bb_aes_subset(bb_col(mapping, data, plot = plot),
                                     keep, prepared$n)
            draw <- modifyList(params, list(col = col_vec))
        } else {
            draw <- params
        }

        do.call(text, draw)
    }

    add_layer(plot, ly, "text layer")
}
