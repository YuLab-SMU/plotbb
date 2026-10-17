## Horizontal bars. Kept separate from the vertical implementation so the
## category axis and the rectangle geometry can follow their own conventions.

bb_bar_horizontal <- function(mapping = NULL, data = NULL, stat = "identity",
                              width = 0.9, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, layer = ly_bar_horizontal,
                stat = stat, width = width, orientation = "horizontal", ...)
}

ly_bar_horizontal <- function(plot, mapping = NULL, data = NULL,
                              stat = "identity", width = 0.9,
                              inherit = TRUE, ...) {
    stat <- match.arg(stat, c("identity", "count"))
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    params$orientation <- NULL
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)
    if (is.null(params$col) && !is.null(params$fill)) {
        params$col <- params$fill
        params$fill <- NULL
    }
    if (is.null(data)) stop("bb_bar requires data")

    x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
    if (is.null(x_raw)) {
        bb_abort("`bb_bar(orientation = \"horizontal\")` requires an `x` mapping.")
    }

    if (identical(stat, "count")) {
        x_chr_all <- as.character(x_raw)
        x_chr <- x_chr_all[!is.na(x_chr_all)]
        if (!length(x_chr)) {
            return(add_layer(plot, function() invisible(NULL), "bar layer"))
        }
        labels <- bb_discrete_levels(x_raw)
        counts <- as.numeric(table(factor(x_chr, levels = labels)))
        positions <- seq_along(labels)
        col_per_bar <- NULL
        if (!is.null(mapping$col)) {
            col_vec <- if (!is.null(mapping$fill)) {
                bb_fill(mapping, data, plot = plot)
            } else {
                bb_col(mapping, data, plot = plot)
            }
            if (!is.null(col_vec)) {
                col_vec <- bb_aes_subset(col_vec, !is.na(x_chr_all),
                                         length(x_raw))
                map <- tapply(col_vec, x_chr_all[!is.na(x_chr_all)],
                              function(z) z[[1]])
                col_per_bar <- unname(map[labels])
            }
        }
        plot$canvas <- function(xlim = NULL, ylim = NULL, log = "",
                                xaxt = NULL, yaxt = NULL) {
            xlim <- xlim %||% c(0, max(counts, na.rm = TRUE))
            ylim <- ylim %||% c(min(positions - width / 2),
                                max(positions + width / 2))
            args <- list(NA, NA, type = "n", xlim = xlim, ylim = ylim,
                         xlab = "", ylab = "", yaxt = "n")
            if (nzchar(log)) args$log <- log
            if (!is.null(xaxt)) args$xaxt <- xaxt
            do.call(graphics::plot.default, args)
            if (is.null(yaxt)) {
                graphics::axis(2, at = positions, labels = labels)
            }
        }
        ly <- function() {
            args <- c(list(xleft = 0, xright = counts,
                           ybottom = positions - width / 2,
                           ytop = positions + width / 2), params)
            if (is.null(args$col) && !is.null(col_per_bar)) {
                args$col <- col_per_bar
            }
            do.call(graphics::rect, args)
            invisible(NULL)
        }
        return(add_layer(plot, ly, "bar layer"))
    }

    y_raw <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
    if (is.null(y_raw)) {
        bb_abort("`bb_bar(orientation = \"horizontal\", stat = \"identity\")` ",
                 "requires a `y` mapping for the categories.")
    }
    if (!is.numeric(x_raw)) {
        bb_abort("`bb_bar(orientation = \"horizontal\")` requires numeric `x` ",
                 "values.")
    }
    n <- max(length(x_raw), length(y_raw))
    x_raw <- rep(x_raw, length.out = n)
    y_raw <- rep(y_raw, length.out = n)
    ok <- is.finite(x_raw) & !is.na(y_raw)
    if (!any(ok)) {
        return(add_layer(plot, function() invisible(NULL), "bar layer"))
    }
    if (is.factor(y_raw) || is.character(y_raw)) {
        labels <- unique(as.character(y_raw[ok]))
        positions <- match(as.character(y_raw), labels)
    } else {
        labels <- NULL
        positions <- as.numeric(y_raw)
    }
    col_vec <- if (is.null(params$col)) {
        if (!is.null(mapping$fill)) {
            bb_fill(mapping, data, plot = plot)
        } else if (!is.null(mapping$col)) {
            bb_col(mapping, data, plot = plot)
        } else {
            NULL
        }
    } else {
        NULL
    }
    plot$canvas <- function(xlim = NULL, ylim = NULL, log = "",
                            xaxt = NULL, yaxt = NULL) {
        xlim <- xlim %||% range(c(0, x_raw), na.rm = TRUE)
        ylim <- ylim %||% c(min(positions - width / 2),
                            max(positions + width / 2))
        args <- list(NA, NA, type = "n", xlim = xlim, ylim = ylim,
                     xlab = "", ylab = "", yaxt = "n")
        if (nzchar(log)) args$log <- log
        if (!is.null(xaxt)) args$xaxt <- xaxt
        do.call(graphics::plot.default, args)
        if (!is.null(labels) && is.null(yaxt)) {
            graphics::axis(2, at = seq_along(labels), labels = labels)
        } else if (is.null(labels) && is.null(yaxt)) {
            graphics::axis(2)
        }
    }
    ly <- function() {
        args <- c(list(xleft = 0, xright = x_raw[ok],
                       ybottom = positions[ok] - width / 2,
                       ytop = positions[ok] + width / 2), params)
        if (is.null(args$col) && !is.null(col_vec)) {
            args$col <- rep(col_vec, length.out = n)[ok]
        }
        do.call(graphics::rect, args)
        invisible(NULL)
    }
    add_layer(plot, ly, "bar layer")
}
