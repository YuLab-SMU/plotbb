##' @rdname layer
##' @param stat one of 'identity' or 'count'
##' @param width bar width or errorbar cap width
##' @param orientation `"vertical"` (default) or `"horizontal"`. A horizontal
##'   count bar keeps the categories on `x` and puts counts on the x axis; a
##'   horizontal identity bar takes numeric values from `x` and categories from
##'   `y`.
##' @export
bb_bar <- function(mapping = NULL, data = NULL, stat = "identity", width = 0.9,
                   inherit = TRUE, orientation = c("vertical", "horizontal"), ...) {
    orientation <- match.arg(orientation)
    if (identical(orientation, "horizontal")) {
        return(bb_bar_horizontal(mapping, data, stat = stat, width = width,
                                 inherit = inherit, ...))
    }
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_bar,
                stat = stat, width = width)
}

ly_bar <- function(plot, mapping = NULL, data = NULL, stat = "identity", width = 0.9, inherit = TRUE, ...) {
    stat <- match.arg(stat, c("identity", "count"))

    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)
    ## A constant `fill` names the interior of the bars/boxes; after the
    ## mapping has been resolved (so the constant also removes a mapped fill),
    ## it becomes the `col` the base function understands.
    if (is.null(params$col) && !is.null(params$fill)) {
        params$col <- params$fill
        params$fill <- NULL
    }
    if (is.null(data)) stop("bb_bar requires data")

    x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
    if (is.null(x_raw)) {
        bb_abort("`bb_bar()` requires an `x` mapping.")
    }


    if (stat == "count") {
        x_chr_all <- as.character(x_raw)
        x_chr <- x_chr_all[!is.na(x_chr_all)]
        if (length(x_chr) == 0) {
            plot$canvas <- function(xlim = NULL, ylim = NULL, log = "",
                                    xaxt = NULL, yaxt = NULL) {
                args <- list(NA, NA, type = "n", xlab = "", ylab = "",
                             xaxt = "n")
                if (!is.null(xlim)) args$xlim <- xlim
                if (!is.null(ylim)) args$ylim <- ylim
                if (nzchar(log)) args$log <- log
                if (!is.null(yaxt)) args$yaxt <- yaxt
                do.call(graphics::plot.default, args)
            }
            ly <- function() {
                invisible(NULL)
            }
            return(add_layer(plot, ly, "bar layer"))
        }

        lev <- bb_discrete_levels(x_raw)

        tab <- table(factor(x_chr, levels = lev))
        labels <- names(tab)
        xpos <- seq_along(tab)
        y <- as.numeric(tab)

        ## colours are one per bar, aligned with the levels of x
        col_per_bar <- NULL
        if (!is.null(mapping$col)) {
            col_var <- eval_mapping(mapping, "col", data)
            col_vec <- if (!is.null(mapping$fill)) {
                bb_fill(mapping, data, plot = plot)
            } else {
                bb_col(mapping, data, plot = plot)
            }
            if (!is.null(col_var) && !is.null(col_vec)) {
                col_vec <- bb_aes_subset(col_vec, !is.na(x_chr_all), length(x_raw))
                col_map <- tapply(col_vec, x_chr_all[!is.na(x_chr_all)], function(z) z[[1]])
                col_per_bar <- unname(col_map[labels])
            }
        }

        plot$canvas <- function(xlim = NULL, ylim = NULL, log = "",
                                xaxt = NULL, yaxt = NULL) {
            xlim_default <- c(min(xpos - width/2), max(xpos + width/2))
            ylim_default <- c(0, max(y, na.rm = TRUE))
            xlim <- xlim %||% xlim_default
            ylim <- ylim %||% ylim_default
            args <- list(NA, NA, type = "n", xlim = xlim, ylim = ylim,
                         xlab = "", ylab = "", xaxt = "n")
            if (nzchar(log)) args$log <- log
            if (!is.null(yaxt)) args$yaxt <- yaxt
            do.call(graphics::plot.default, args)
            if (is.null(xaxt)) graphics::axis(1, at = xpos, labels = labels)
        }

        ly <- function() {
            args <- c(
                list(
                    xleft = xpos - width/2,
                    ybottom = 0,
                    xright = xpos + width/2,
                    ytop = y
                ),
                params
            )

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
        bb_abort("`bb_bar(stat = \"identity\")` requires a `y` mapping.")
    }

    n <- max(length(x_raw), length(y_raw))
    bb_check_length(y_raw, length(x_raw), "y", "bb_bar")
    ok <- bb_valid_rows(rep(x_raw, length.out = n), rep(y_raw, length.out = n))
    if (!any(ok)) {
        return(add_layer(plot, function() invisible(NULL), "bar layer"))
    }
    x_raw <- rep(x_raw, length.out = n)
    y_raw <- rep(y_raw, length.out = n)

    if (!is.numeric(x_raw)) {
        x_chr <- as.character(x_raw)
        labels <- unique(x_chr[!is.na(x_chr)])
        xpos <- match(x_chr, labels)
    } else {
        labels <- NULL
        xpos <- x_raw
    }
    y <- y_raw

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

    if (!is.null(labels)) {
        plot$canvas <- function(xlim = NULL, ylim = NULL, log = "",
                                xaxt = NULL, yaxt = NULL) {
            xlim_default <- c(min(xpos - width/2), max(xpos + width/2))
            ylim_default <- range(c(0, y), na.rm = TRUE)
            xlim <- xlim %||% xlim_default
            ylim <- ylim %||% ylim_default
            args <- list(NA, NA, type = "n", xlim = xlim, ylim = ylim,
                         xlab = "", ylab = "", xaxt = "n")
            if (nzchar(log)) args$log <- log
            if (!is.null(yaxt)) args$yaxt <- yaxt
            do.call(graphics::plot.default, args)
            if (is.null(xaxt)) graphics::axis(1, at = seq_along(labels),
                                               labels = labels)
        }
    }

    ly <- function() {
        args <- c(
            list(
                xleft = (xpos - width/2)[ok],
                ybottom = 0,
                xright = (xpos + width/2)[ok],
                ytop = y[ok]
            ),
            params
        )

        if (is.null(args$col) && !is.null(col_vec)) {
            args$col <- rep(col_vec, length.out = n)[ok]
        }

        do.call(graphics::rect, args)
        invisible(NULL)
    }

    add_layer(plot, ly, "bar layer")
}
