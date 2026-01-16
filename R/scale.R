##' change col palette
##'
##' 
##' @title bb_scale_col_palette
##' @rdname bb-scale
##' @param palette color palette
##' @param values colors for discrete scale.
##' @param low low color for continuous scale.
##' @param high high color for continuous scale.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_scale_col_palette <- function(palette = NULL) {
    structure(list(palette = palette, type = "palette"), class = "bb_palette")
}

##' @rdname bb-scale
##' @export
bb_scale_col_manual <- function(values) {
    structure(list(palette = values, type = "manual"), class = "bb_palette")
}

##' @rdname bb-scale
##' @export
bb_scale_col_gradient <- function(low = "#132B43", high = "#56B1F7") {
    structure(list(palette = c(low, high), type = "gradient"), class = "bb_palette")
}

##' @title aesthetic scales
##' @rdname bb-scale-aes
##' @param values values used for mapping.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_scale_pch_manual <- function(values) {
    structure(list(aesthetic = "pch", type = "manual", values = values), class = "bb_scale")
}

##' @rdname bb-scale-aes
##' @export
bb_scale_lty_manual <- function(values) {
    structure(list(aesthetic = "lty", type = "manual", values = values), class = "bb_scale")
}

##' @rdname bb-scale-aes
##' @export
bb_scale_cex_manual <- function(values) {
    structure(list(aesthetic = "cex", type = "manual", values = values), class = "bb_scale")
}

##' @rdname bb-scale-aes
##' @param range numeric length-2 range for continuous mapping.
##' @export
bb_scale_cex_continuous <- function(range = c(0.8, 1.6)) {
    stopifnot(is.numeric(range), length(range) == 2)
    structure(list(aesthetic = "cex", type = "continuous", range = range), class = "bb_scale")
}

bb_scale_col_map <- function(scale, x) {
    if (is.null(scale)) return(NULL)
    type <- scale$type %||% "palette"
    palette <- scale$palette
    domain <- scale$domain

    if (type == "manual") {
        key <- as.character(x)
        ukey <- domain %||% bb_discrete_levels(key)
        cols <- rep(palette, length.out = length(ukey))
        if (!is.null(names(palette))) {
            cols <- palette[ukey]
        }
        names(cols) <- ukey
        return(cols[key])
    }

    if (type == "gradient") {
        f <- scales::col_numeric(palette, domain %||% x)
        return(f(x))
    }

    if (is.numeric(x)) {
        if (is.null(palette)) palette <- "viridis"
        f <- scales::col_numeric(palette, domain %||% x)
        return(f(x))
    }

    ucol <- domain %||% bb_discrete_levels(x)
    if (is.null(palette)) palette <- "Set2"
    if (is.character(palette) && length(palette) > 1) {
        cols <- rep(palette, length.out = length(ucol))
        if (!is.null(names(palette))) {
            cols <- palette[ucol]
        }
        names(cols) <- ucol
        return(cols[as.character(x)])
    }
    f <- scales::col_factor(palette, ucol)
    cols <- f(ucol)
    names(cols) <- ucol
    cols[as.character(x)]
}

bb_scale_map <- function(scale, x) {
    if (is.null(scale)) return(NULL)
    type <- scale$type %||% "manual"
    if (identical(type, "continuous")) {
        if (!is.numeric(x)) return(NULL)
        rng <- scale$range %||% c(0.8, 1.6)
        if (!is.numeric(rng) || length(rng) != 2) return(NULL)
        scales::rescale(x, to = rng, from = range(x, na.rm = TRUE))
    } else {
        key <- as.character(x)
        ukey <- scale$domain %||% bb_discrete_levels(key)
        vals <- scale$values
        if (is.null(vals)) return(NULL)
        mapped <- rep(vals, length.out = length(ukey))
        if (!is.null(names(vals))) {
            mapped <- vals[ukey]
        }
        names(mapped) <- ukey
        unname(mapped[key])
    }
}

##' @method bbplot_add bb_scale
##' @export
bbplot_add.bb_scale <- function(object, plot) {
    aesthetic <- object$aesthetic %||% NA_character_
    if (is.na(aesthetic) || !aesthetic %in% c("pch", "lty", "cex")) return(plot)

    if (is.null(plot$scales)) plot$scales <- list(col = NULL, pch = NULL, lty = NULL, cex = NULL)
    if (!exists("scales", envir = plot$env, inherits = FALSE)) {
        assign("scales", list(col = NULL, pch = NULL, lty = NULL, cex = NULL), envir = plot$env)
    }

    if (!is.null(plot$data) && !is.null(plot$mapping[[aesthetic]])) {
        v <- eval_mapping(plot$mapping, aesthetic, plot$data)
        if (!is.null(v)) {
            if (identical(object$type %||% "manual", "continuous")) {
                object$domain <- range(v, na.rm = TRUE)
            } else {
                object$domain <- bb_discrete_levels(v)
            }
        }
    }

    plot$scales[[aesthetic]] <- object
    scales <- get("scales", envir = plot$env, inherits = FALSE)
    scales[[aesthetic]] <- object
    assign("scales", scales, envir = plot$env)
    plot
}

##' Add legend for aesthetic mapping.
##'
##' @title bb_legend
##' @param position legend position passed to \code{graphics::legend}.
##' @param title legend title.
##' @param aesthetic currently only supports \code{"col"}.
##' @param pch point shape used in legend.
##' @param bty box type passed to \code{graphics::legend}.
##' @param ... additional parameters passed to \code{graphics::legend}.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_legend <- function(position = "topright", title = NULL, aesthetic = "col", pch = 19, bty = "n", ...) {
    structure(
        list(
            position = position,
            title = title,
            aesthetic = aesthetic,
            pch = pch,
            bty = bty,
            params = list(...)
        ),
        class = "bb_legend"
    )
}

##' @method bbplot_add bb_legend
##' @export
bbplot_add.bb_legend <- function(object, plot) {
    ly <- function() {
        mapping <- plot$mapping
        data <- plot$data
        if (is.null(data)) return(invisible(NULL))

        aesthetic <- object$aesthetic %||% "col"
        if (!aesthetic %in% c("col", "pch", "lty", "cex")) return(invisible(NULL))

        map_quo <- mapping[[aesthetic]]
        if (is.null(map_quo)) return(invisible(NULL))

        aes_var <- eval_mapping(mapping, aesthetic, data)
        if (is.null(aes_var)) return(invisible(NULL))

        legend_title <- object$title %||% rlang::as_label(map_quo)

        if (identical(aesthetic, "col") && is.numeric(aes_var)) {
            scale <- plot$scales$col
            if (is.null(scale)) return(invisible(NULL))

            dom <- scale$domain %||% range(aes_var, na.rm = TRUE)
            if (length(dom) != 2 || any(!is.finite(dom))) return(invisible(NULL))

            pal <- scale$palette
            if (is.null(pal)) pal <- "viridis"
            f <- scales::col_numeric(pal, domain = dom)

            n <- 100
            z <- seq(dom[[1]], dom[[2]], length.out = n)
            cols <- f(z)

            usr <- par("usr")
            xspan <- usr[[2]] - usr[[1]]
            yspan <- usr[[4]] - usr[[3]]

            mar_x <- xspan * 0.02
            mar_y <- yspan * 0.02
            bar_w <- xspan * 0.05
            bar_h <- yspan * 0.45

            pos <- object$position %||% "topright"
            if (pos %in% c("topright", "right")) {
                x1 <- usr[[2]] - mar_x
                x0 <- x1 - bar_w
                y1 <- usr[[4]] - mar_y
                y0 <- y1 - bar_h
            } else if (pos %in% c("topleft", "left")) {
                x0 <- usr[[1]] + mar_x
                x1 <- x0 + bar_w
                y1 <- usr[[4]] - mar_y
                y0 <- y1 - bar_h
            } else if (pos %in% c("bottomright")) {
                x1 <- usr[[2]] - mar_x
                x0 <- x1 - bar_w
                y0 <- usr[[3]] + mar_y
                y1 <- y0 + bar_h
            } else if (pos %in% c("bottomleft")) {
                x0 <- usr[[1]] + mar_x
                x1 <- x0 + bar_w
                y0 <- usr[[3]] + mar_y
                y1 <- y0 + bar_h
            } else {
                x1 <- usr[[2]] - mar_x
                x0 <- x1 - bar_w
                y1 <- usr[[4]] - mar_y
                y0 <- y1 - bar_h
            }

            yy <- seq(y0, y1, length.out = n + 1)
            for (i in seq_len(n)) {
                graphics::rect(x0, yy[[i]], x1, yy[[i + 1]], col = cols[[i]], border = NA)
            }
            graphics::rect(x0, y0, x1, y1, border = "grey30")

            ticks <- pretty(dom, n = 5)
            ticks <- ticks[ticks >= dom[[1]] & ticks <= dom[[2]]]
            if (length(ticks)) {
                at <- y0 + (ticks - dom[[1]]) / (dom[[2]] - dom[[1]]) * (y1 - y0)
                graphics::axis(4, at = at, labels = ticks, las = 1, cex.axis = 0.8, tck = -0.02)
            }
            graphics::mtext(legend_title, side = 4, line = 2.5, cex = 0.8)
            return(invisible(NULL))
        }

        lvl <- as.character(aes_var)
        keep <- !is.na(lvl)
        if (!any(keep)) return(invisible(NULL))
        lvl <- lvl[keep]

        labels <- bb_discrete_levels(aes_var)
        labels <- labels[labels %in% lvl]
        if (length(labels) == 0) return(invisible(NULL))

        if (identical(aesthetic, "col")) {
            aes_vec <- bb_col(mapping, data, plot = plot)
            aes_vec <- aes_vec[keep]
            aes_map <- tapply(aes_vec, lvl, function(z) z[[1]])
            aes_map <- unname(aes_map[labels])
            args <- c(
                list(
                    x = object$position,
                    legend = labels,
                    col = aes_map,
                    pch = object$pch,
                    title = legend_title,
                    bty = object$bty
                ),
                object$params
            )
            do.call(graphics::legend, args)
            return(invisible(NULL))
        }

        if (identical(aesthetic, "pch")) {
            aes_vec <- bb_pch(mapping, data, plot = plot)
            aes_vec <- aes_vec[keep]
            aes_map <- tapply(aes_vec, lvl, function(z) z[[1]])
            aes_map <- unname(aes_map[labels])
            args <- c(
                list(
                    x = object$position,
                    legend = labels,
                    pch = aes_map,
                    title = legend_title,
                    bty = object$bty
                ),
                object$params
            )
            do.call(graphics::legend, args)
            return(invisible(NULL))
        }

        if (identical(aesthetic, "lty")) {
            aes_vec <- bb_lty(mapping, data, plot = plot)
            aes_vec <- aes_vec[keep]
            aes_map <- tapply(aes_vec, lvl, function(z) z[[1]])
            aes_map <- unname(aes_map[labels])
            args <- c(
                list(
                    x = object$position,
                    legend = labels,
                    lty = aes_map,
                    title = legend_title,
                    bty = object$bty
                ),
                object$params
            )
            do.call(graphics::legend, args)
            return(invisible(NULL))
        }

        aes_vec <- bb_cex(mapping, data, plot = plot)
        aes_vec <- aes_vec[keep]
        aes_map <- tapply(aes_vec, lvl, function(z) z[[1]])
        aes_map <- unname(aes_map[labels])
        args <- c(
            list(
                x = object$position,
                legend = labels,
                pch = object$pch,
                pt.cex = aes_map,
                title = legend_title,
                bty = object$bty
            ),
            object$params
        )
        do.call(graphics::legend, args)
        invisible(NULL)
    }

    add_layer(plot, ly, "legend")
}


