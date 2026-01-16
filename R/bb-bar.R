##' @rdname layer
##' @param stat one of 'identity' or 'count'
##' @param width bar width or errorbar cap width
##' @export
bb_bar <- function(mapping = NULL, data = NULL, stat = "identity", width = 0.9, ...) {
    build_layer(mapping, data, ..., layer = ly_bar, stat = stat, width = width)
}

ly_bar <- function(plot, mapping = NULL, data = NULL, stat = "identity", width = 0.9, ...) {
    stat <- match.arg(stat, c("identity", "count"))

    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)
    if (is.null(data)) stop("bb_bar requires data")

    x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))

    params <- list(...)

    if (stat == "count") {
        x_chr_all <- as.character(x_raw)
        x_chr <- x_chr_all[!is.na(x_chr_all)]
        if (length(x_chr) == 0) {
            plot$canvas <- function() {
                graphics::plot.default(NA, NA, type = "n", xlab = "", ylab = "", xaxt = "n")
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

        plot$canvas <- function() {
            xlim <- c(min(xpos - width/2), max(xpos + width/2))
            ylim <- c(0, max(y, na.rm = TRUE))
            graphics::plot.default(NA, NA, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = "n")
            graphics::axis(1, at = xpos, labels = labels)
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

            if (is.null(args$col) && !is.null(mapping$col)) {
                col_var <- eval_mapping(mapping, "col", data)
                if (!is.null(col_var)) {
                    col_vec <- bb_col(mapping, data, plot = plot)
                    col_map <- tapply(col_vec, as.character(col_var), function(z) z[[1]])
                    col_fill <- unname(col_map[labels])
                    args$col <- col_fill
                }
            }

            do.call(graphics::rect, args)
            invisible(NULL)
        }

        return(add_layer(plot, ly, "bar layer"))
    }

    y_raw <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))

    if (!is.numeric(x_raw)) {
        x_chr <- as.character(x_raw)
        labels <- unique(x_chr)
        xpos <- match(x_chr, labels)
    } else {
        labels <- NULL
        xpos <- x_raw
    }
    y <- y_raw

    if (!is.null(labels)) {
        plot$canvas <- function() {
            xlim <- c(min(xpos - width/2), max(xpos + width/2))
            ylim <- range(c(0, y), na.rm = TRUE)
            graphics::plot.default(NA, NA, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "", xaxt = "n")
            graphics::axis(1, at = seq_along(labels), labels = labels)
        }
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

        if (is.null(args$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            if (!is.null(col_vec)) {
                args$col <- col_vec
            }
        }

        do.call(graphics::rect, args)
        invisible(NULL)
    }

    add_layer(plot, ly, "bar layer")
}
