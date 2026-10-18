## Colour scales ------------------------------------------------------------

##' change col palette
##'
##' @title bb_scale_col_palette
##' @rdname bb-scale
##' @param palette color palette
##' @param values colors for discrete scale.
##' @param limits optional limits of the trained domain. Values outside the
##'   limits follow `oob`; levels outside the limits are not shown.
##' @param na.value colour used for missing values and for values not covered by
##'   a discrete scale.
##' @param drop whether unobserved factor levels are dropped from the trained
##'   scale and the legend.
##' @param low low color for continuous scale.
##' @param high high color for continuous scale.
##' @param oob out-of-bounds behaviour for a continuous scale: `"squish"`
##'   (default) clamps to the limits, `"censor"` maps them to `na.value`.
##' @param breaks optional breaks for a continuous colour bar. Defaults to
##'   `pretty()` on the trained domain.
##' @param labels optional labels for the breaks (continuous) or a named vector
##'   renaming discrete levels.
##' @param minor_breaks optional positions for minor ticks on a continuous colour
##'   bar; `NULL` uses the midpoints between the major breaks.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_scale_col_palette <- function(palette = NULL, limits = NULL,
                                 na.value = "grey50", drop = TRUE,
                                 breaks = NULL, labels = NULL,
                                 minor_breaks = NULL) {
    bb_colour_scale("col", "palette", palette = palette, limits = limits,
                    na.value = na.value, drop = drop, breaks = breaks,
                    labels = labels, minor_breaks = minor_breaks)
}

##' @rdname bb-scale
##' @export
bb_scale_col_manual <- function(values, limits = NULL, na.value = "grey50",
                                drop = TRUE, breaks = NULL, labels = NULL,
                                minor_breaks = NULL) {
    bb_colour_scale("col", "manual", palette = values, limits = limits,
                    na.value = na.value, drop = drop, breaks = breaks,
                    labels = labels, minor_breaks = minor_breaks)
}

##' @rdname bb-scale
##' @export
bb_scale_col_gradient <- function(low = "#132B43", high = "#56B1F7",
                                  limits = NULL, na.value = "grey50",
                                  oob = c("squish", "censor"),
                                  breaks = NULL, labels = NULL,
                                  minor_breaks = NULL) {
    bb_colour_scale("col", "gradient", palette = c(low, high),
                    limits = limits, na.value = na.value,
                    oob = match.arg(oob), breaks = breaks, labels = labels,
                    minor_breaks = minor_breaks)
}

##' Fill scales
##'
##' `fill` is a separate aesthetic from `col`: use these constructors when a
##' layer maps `fill` (`bb_tile()`, `bb_density()`, `bb_bar()`,
##' `bb_boxplot()`). They take the same arguments as the `col` scales.
##'
##' @title bb_scale_fill_palette
##' @rdname bb-scale-fill
##' @inheritParams bb_scale_col_palette
##' @export
bb_scale_fill_palette <- function(palette = NULL, limits = NULL,
                                  na.value = "grey50", drop = TRUE,
                                  breaks = NULL, labels = NULL,
                                  minor_breaks = NULL) {
    bb_colour_scale("fill", "palette", palette = palette, limits = limits,
                    na.value = na.value, drop = drop, breaks = breaks,
                    labels = labels, minor_breaks = minor_breaks)
}

##' @rdname bb-scale-fill
##' @export
bb_scale_fill_manual <- function(values, limits = NULL, na.value = "grey50",
                                 drop = TRUE, breaks = NULL, labels = NULL,
                                 minor_breaks = NULL) {
    bb_colour_scale("fill", "manual", palette = values, limits = limits,
                    na.value = na.value, drop = drop, breaks = breaks,
                    labels = labels, minor_breaks = minor_breaks)
}

##' @rdname bb-scale-fill
##' @export
bb_scale_fill_gradient <- function(low = "#132B43", high = "#56B1F7",
                                   limits = NULL, na.value = "grey50",
                                   oob = c("squish", "censor"),
                                   breaks = NULL, labels = NULL,
                                   minor_breaks = NULL) {
    bb_colour_scale("fill", "gradient", palette = c(low, high),
                    limits = limits, na.value = na.value,
                    oob = match.arg(oob), breaks = breaks, labels = labels,
                    minor_breaks = minor_breaks)
}

bb_colour_scale <- function(aesthetic, type, palette = NULL, limits = NULL,
                            na.value = "grey50", drop = TRUE,
                            oob = "squish", breaks = NULL, labels = NULL,
                            minor_breaks = NULL) {
    if (!is.null(minor_breaks) && !is.numeric(minor_breaks)) {
        bb_abort("`minor_breaks` must be numeric or NULL.")
    }
    structure(
        list(
            aesthetic = aesthetic,
            type = type,
            palette = palette,
            values = NULL,
            limits = limits,
            domain = NULL,
            breaks = breaks,
            labels = labels,
            minor_breaks = minor_breaks,
            na.value = na.value,
            oob = oob,
            drop = isTRUE(drop)
        ),
        class = "bb_palette"
    )
}

## Symbol scales ------------------------------------------------------------

##' @title aesthetic scales
##' @rdname bb-scale-aes
##' @param values values used for mapping.
##' @param limits optional limits of the trained domain.
##' @param na.value value used for missing values.
##' @param drop whether unobserved levels are dropped.
##' @param oob out-of-bounds behaviour, `"squish"` or `"censor"`.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_scale_pch_manual <- function(values, limits = NULL, na.value = 1,
                                drop = TRUE) {
    bb_symbol_scale("pch", "manual", values = values, limits = limits,
                    na.value = na.value, drop = drop)
}

##' @rdname bb-scale-aes
##' @export
bb_scale_lty_manual <- function(values, limits = NULL, na.value = 1,
                                drop = TRUE) {
    bb_symbol_scale("lty", "manual", values = values, limits = limits,
                    na.value = na.value, drop = drop)
}

##' @rdname bb-scale-aes
##' @export
bb_scale_cex_manual <- function(values, limits = NULL, na.value = 1,
                                drop = TRUE) {
    bb_symbol_scale("cex", "manual", values = values, limits = limits,
                    na.value = na.value, drop = drop)
}

##' @rdname bb-scale-aes
##' @param range numeric length-2 range for continuous mapping.
##' @export
bb_scale_cex_continuous <- function(range = c(0.8, 1.6), limits = NULL,
                                    na.value = 1,
                                    oob = c("squish", "censor")) {
    if (!is.numeric(range) || length(range) != 2L ||
        any(!is.finite(range))) {
        bb_abort("`range` in `bb_scale_cex_continuous()` must be two finite ",
                 "numbers.")
    }
    bb_symbol_scale("cex", "continuous", range = range, limits = limits,
                    na.value = na.value, oob = match.arg(oob))
}

bb_symbol_scale <- function(aesthetic, type, values = NULL,
                            range = c(0.8, 1.6), limits = NULL,
                            na.value = 1, drop = TRUE, oob = "squish") {
    structure(
        list(
            aesthetic = aesthetic,
            type = type,
            palette = NULL,
            values = values,
            range = range,
            limits = limits,
            domain = NULL,
            breaks = NULL,
            labels = NULL,
            na.value = na.value,
            oob = oob,
            drop = isTRUE(drop)
        ),
        class = "bb_scale"
    )
}

## Scale storage ------------------------------------------------------------

## The trained scales live in the plot's environment as well as in the list
## field, because a layer closure captures the plot object once and reads the
## scales at draw time; writing to the shared environment keeps both views in
## sync.
bb_get_scales <- function(plot) {
    if (is.null(plot)) return(list())
    if (!is.null(plot$env) && exists("scales", envir = plot$env, inherits = FALSE)) {
        return(get("scales", envir = plot$env, inherits = FALSE))
    }
    plot$scales %||% list()
}

bb_set_scales <- function(plot, scales) {
    plot$scales <- scales
    if (!is.null(plot$env)) {
        assign("scales", scales, envir = plot$env)
    }
    plot
}

bb_store_scale <- function(plot, scale) {
    aesthetic <- scale$aesthetic %||% "col"
    scales <- bb_get_scales(plot)
    scales[[aesthetic]] <- scale
    bb_set_scales(plot, scales)
}

##' @method bbplot_add bb_palette
##' @export
bbplot_add.bb_palette <- function(object, plot) {
    bb_store_scale(plot, object)
}

##' @method bbplot_add bb_scale
##' @export
bbplot_add.bb_scale <- function(object, plot) {
    bb_store_scale(plot, object)
}

## Training -----------------------------------------------------------------

## Every value of one aesthetic that the plot draws: the plot-level mapping on
## the plot data, plus each layer's effective mapping on its own data. A
## constant aesthetic is already removed from the effective mapping, so it never
## trains a scale.
bb_scale_values <- function(plot, aesthetic) {
    values <- list()
    add <- function(v) {
        if (!is.null(v) && length(v)) values[[length(values) + 1L]] <<- v
    }

    if (!is.null(plot$data)) {
        add(eval_mapping(plot$mapping, aesthetic, plot$data))
    }

    for (obj in plot$adds) {
        if (!inherits(obj, "bb_layer")) next
        mapping <- bb_layer_aesthetics(plot, obj$mapping,
                                       obj$inherit %||% TRUE,
                                       obj$params %||% list())
        if (is.null(mapping[[aesthetic]])) next
        data <- if (is.data.frame(obj$data)) obj$data else plot$data
        if (is.null(data)) next
        add(eval_mapping(mapping, aesthetic, data))
    }
    values
}

bb_scale_is_continuous <- function(scale, values) {
    type <- scale$type %||% "manual"
    if (type %in% c("gradient", "continuous")) return(TRUE)
    if (type == "manual") return(FALSE)
    numeric_values <- unlist(values, use.names = FALSE)
    is.numeric(numeric_values) && length(numeric_values) > 0
}

## Discrete levels: factor levels keep their declared order; character values
## keep first-appearance order; numeric values of a manual scale keep
## appearance order. `drop = TRUE` removes unobserved factor levels.
bb_scale_levels <- function(values, drop = TRUE) {
    from_factor <- character()
    observed <- character()
    for (v in values) {
        if (is.null(v)) next
        if (is.factor(v)) {
            from_factor <- union(from_factor, levels(v))
        }
        key <- as.character(v)
        observed <- union(observed, key[!is.na(key)])
    }
    lev <- union(from_factor, observed)
    if (isTRUE(drop)) lev <- intersect(lev, observed)
    lev
}

bb_scale_numeric_domain <- function(values, limits = NULL) {
    if (!is.null(limits)) {
        limits <- sort(as.numeric(limits))
        if (length(limits) != 2L || any(!is.finite(limits))) {
            bb_abort("`limits` must be two finite numbers.")
        }
        if (limits[[1]] == limits[[2]]) limits <- limits + c(-0.5, 0.5)
        return(limits)
    }
    nums <- unlist(lapply(values, function(v) {
        if (is.numeric(v)) v[is.finite(v)] else numeric()
    }), use.names = FALSE)
    if (!length(nums)) return(NULL)
    rng <- range(nums)
    if (rng[[1]] == rng[[2]]) rng <- rng + c(-0.5, 0.5)
    rng
}

bb_scale_train <- function(scale, values) {
    if (is.null(scale)) return(NULL)
    continuous <- bb_scale_is_continuous(scale, values)

    if (continuous) {
        scale$domain <- bb_scale_numeric_domain(values, scale$limits)
        return(scale)
    }

    lev <- bb_scale_levels(values, scale$drop %||% TRUE)
    if (!is.null(scale$limits)) {
        lev <- as.character(scale$limits)
    }
    scale$domain <- lev
    scale
}

##' Train the scales of a plot from all of its data
##'
##' Called by the print methods before drawing. Training is a pure computation:
##' it does not open a device and does not draw.
##'
##' @title bb_train_scales
##' @param plot a \code{bbplot} object
##' @return The plot with trained scales
##' @noRd
bb_train_scales <- function(plot) {
    scales <- bb_get_scales(plot)
    if (!length(scales)) return(plot)
    for (aesthetic in names(scales)) {
        scale <- scales[[aesthetic]]
        if (is.null(scale)) next
        scales[[aesthetic]] <- bb_scale_train(scale,
                                             bb_scale_values(plot, aesthetic))
    }
    bb_set_scales(plot, scales)
}

## Mapping ------------------------------------------------------------------

bb_map_aesthetic <- function(plot, aesthetic, values, palette = NULL) {
    if (is.null(values)) return(NULL)
    scale <- bb_get_scales(plot)[[aesthetic]]
    if (is.null(scale)) {
        return(bb_default_aesthetic(plot, aesthetic, values, palette = palette))
    }
    bb_scale_map_values(scale, values, aesthetic)
}

bb_scale_map_values <- function(scale, x, aesthetic = scale$aesthetic %||% "col") {
    colour <- aesthetic %in% c("col", "fill")
    na.value <- scale$na.value %||% if (colour) "grey50" else 1
    continuous <- bb_scale_is_continuous(scale, list(x))

    if (continuous) {
        if (all(is.na(x))) return(rep(na.value, length(x)))
        if (!is.numeric(x)) {
            bb_abort("a continuous `", aesthetic, "` scale requires a numeric ",
                     "mapping.")
        }
        domain <- scale$domain
        if (is.null(domain) || length(domain) != 2L ||
            any(!is.finite(domain))) {
            domain <- bb_scale_numeric_domain(list(x))
            if (is.null(domain)) return(rep(na.value, length(x)))
        }
        x <- as.numeric(x)
        oob <- scale$oob %||% "squish"
        out <- x
        if (identical(oob, "squish")) {
            out <- pmin(pmax(out, domain[[1]]), domain[[2]])
        } else {
            outside <- !is.na(out) & (out < domain[[1]] | out > domain[[2]])
            out[outside] <- NA_real_
        }
        if (colour) {
            palette <- scale$palette %||% "viridis"
            f <- scales::col_numeric(palette, domain = domain,
                                     na.color = na.value)
            return(f(out))
        }
        rng <- scale$range %||% c(0.8, 1.6)
        mapped <- scales::rescale(out, to = rng, from = domain)
        mapped[is.na(out)] <- na.value
        return(mapped)
    }

    lev <- scale$domain %||% bb_scale_levels(list(x), TRUE)
    key <- as.character(x)

    if (colour) {
        cols <- bb_discrete_colours(scale$palette, lev, na.value)
        out <- unname(cols[key])
        unmatched <- !is.na(key) & is.na(out)
        if (any(unmatched)) {
            bb_warn("value(s) not covered by the `", aesthetic, "` scale were ",
                    "mapped to `na.value`: ",
                    paste(utils::head(unique(key[unmatched]), 3), collapse = ", "))
            out[unmatched] <- na.value
        }
        out[is.na(key)] <- na.value
        return(out)
    }

    vals <- bb_discrete_symbols(scale$values, lev, na.value)
    out <- unname(vals[key])
    out[!is.na(key) & is.na(out)] <- na.value
    out[is.na(key)] <- na.value
    out
}

bb_discrete_colours <- function(palette, levels, na.value = "grey50") {
    n <- length(levels)
    if (n == 0L) return(stats::setNames(character(), character()))
    if (is.null(palette)) palette <- "Set2"
    if (is.character(palette) &&
        (length(palette) > 1L || !is.null(names(palette)))) {
        if (!is.null(names(palette))) {
            cols <- palette[levels]
            cols[is.na(cols)] <- na.value
            return(stats::setNames(unname(cols), levels))
        }
        return(stats::setNames(rep(palette, length.out = n), levels))
    }
    f <- scales::col_factor(palette, levels = levels)
    stats::setNames(f(levels), levels)
}

bb_discrete_symbols <- function(values, levels, na.value = 1) {
    n <- length(levels)
    if (n == 0L) return(stats::setNames(numeric(), character()))
    if (is.null(values)) return(stats::setNames(rep(na.value, n), levels))
    if (!is.null(names(values))) {
        out <- values[levels]
        out[is.na(out)] <- na.value
        return(stats::setNames(unname(out), levels))
    }
    stats::setNames(rep(values, length.out = n), levels)
}

bb_default_aesthetic <- function(plot, aesthetic, x, palette = NULL) {
    if (aesthetic %in% c("col", "fill")) {
        return(bb_default_colour(plot, x, palette = palette))
    }
    if (is.numeric(x)) {
        if (aesthetic == "pch") return(as.integer(x))
        return(x)
    }
    lev <- bb_discrete_levels(x)
    if (!length(lev)) return(rep(1, length(x)))
    key <- as.character(x)
    pal <- switch(aesthetic,
        pch = rep(1:25, length.out = length(lev)),
        lty = rep(1:6, length.out = length(lev)),
        cex = rep(c(0.8, 1.1, 1.4, 1.7), length.out = length(lev)),
        rep(1, length(lev))
    )
    stats::setNames(pal, lev)[key]
}

bb_default_colour <- function(plot, x, palette = NULL) {
    palette <- bb_resolve_palette(plot, palette)
    n <- length(x)
    if (is.numeric(x)) {
        finite <- x[is.finite(x)]
        if (!length(finite)) return(rep("grey50", n))
        rng <- range(finite)
        if (rng[[1]] == rng[[2]]) rng <- rng + c(-0.5, 0.5)
        pal <- palette %||% "viridis"
        f <- scales::col_numeric(pal, rng, na.color = "grey50")
        return(f(x))
    }
    lev <- bb_discrete_levels(x)
    if (!length(lev)) return(rep("grey50", n))
    cols <- bb_discrete_colours(palette, lev, "grey50")
    unname(cols[as.character(x)])
}

## Legends ------------------------------------------------------------------

##' Add legend for aesthetic mapping.
##'
##' @title bb_legend
##' @param position legend position passed to \code{graphics::legend}, or a
##'   numeric x position for an outside legend.
##' @param title legend title.
##' @param aesthetic currently supports \code{"col"}, \code{"fill"}, \code{"pch"}, \code{"lty"}, \code{"cex"}.
##' @param pch point shape used in legend.
##' @param bty box type passed to \code{graphics::legend}.
##' @param outside logical, whether to place the legend outside the plotting area.
##' @param orientation orientation of a continuous colour bar,
##'   \code{"vertical"} or \code{"horizontal"}.
##' @param layer optional layer index or name: the legend then describes that
##'   layer's mapping. The colour scale is still the shared one, so this selects
##'   the mapping/title, not a separate domain.
##' @param style \code{"auto"} or \code{"bar"}; a bar draws a discrete
##'   col/fill legend as adjacent colour blocks (a discrete gradient).
##' @param title_width optional positive integer; wraps a long legend title.
##' @param width relative width of an outside legend column (default 1.4).
##' @param ... additional parameters passed to \code{graphics::legend}.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_legend <- function(position = "topright", title = NULL, aesthetic = "col",
                      pch = 19, bty = "n", outside = FALSE,
                      orientation = c("vertical", "horizontal"),
                      layer = NULL, style = c("auto", "bar"),
                      title_width = NULL, width = 1.4, ...) {
    orientation <- match.arg(orientation)
    style <- match.arg(style)
    aesthetic <- match.arg(aesthetic,
                           c("col", "fill", "pch", "lty", "cex"))
    if (!is.null(layer) &&
        !(is.numeric(layer) && length(layer) == 1L) &&
        !(is.character(layer) && length(layer) == 1L)) {
        bb_abort("`layer` in `bb_legend()` must be a layer index, a layer ",
                 "name or NULL.")
    }
    if (!is.numeric(width) || length(width) != 1L || !is.finite(width) ||
        width <= 0) {
        bb_abort("`width` in `bb_legend()` must be a positive number.")
    }
    if (!is.null(title_width)) {
        if (!is.numeric(title_width) || length(title_width) != 1L ||
            is.na(title_width) || title_width < 1 ||
            title_width != as.integer(title_width)) {
            bb_abort("`title_width` in `bb_legend()` must be a positive ",
                     "integer or NULL.")
        }
        title_width <- as.integer(title_width)
    }
    structure(
        list(
            position = position,
            title = title,
            aesthetic = aesthetic,
            pch = pch,
            bty = bty,
            outside = outside,
            orientation = orientation,
            layer = layer,
            style = style,
            title_width = title_width,
            width = width,
            params = list(...)
        ),
        class = "bb_legend"
    )
}

## Legend positions: the strings graphics::legend() accepts, plus a numeric x
## for an outside legend. A colour bar supports the four corners and the two
## sides; other strings are a clear error instead of a silently misplaced bar.
bb_legend_positions <- function() {
    c("topright", "topleft", "bottomright", "bottomleft",
      "right", "left", "top", "bottom", "center")
}

bb_check_legend_position <- function(position, continuous = FALSE) {
    if (is.numeric(position)) return(invisible(position))
    ok <- bb_legend_positions()
    if (continuous) {
        ok <- setdiff(ok, c("top", "bottom", "center"))
    }
    if (!is.character(position) || length(position) != 1L ||
        !position %in% ok) {
        bb_abort("unsupported legend position: ",
                 paste(position, collapse = ", "), ". Use one of ",
                 paste(ok, collapse = ", "),
                 if (!continuous) " or a numeric x." else
                     " or a numeric x for an outside legend.")
    }
    invisible(position)
}

bb_legend_scale <- function(plot, aesthetic) {
    scales <- bb_get_scales(plot)
    scale <- scales[[aesthetic]]
    if (is.null(scale) && identical(aesthetic, "fill")) {
        ## A fill legend for a col mapping keeps using the col scale.
        scale <- scales$col
    }
    scale
}

## A per-layer legend selects the layer whose mapping the legend describes. The
## scale is still the shared, plot-trained one; `layer` chooses the mapping,
## data and default title, not a second domain.
bb_legend_layer <- function(plot, layer, aesthetic) {
    objects <- Filter(function(obj) inherits(obj, "bb_layer"), plot$adds)
    if (!length(objects)) {
        bb_abort("`bb_legend(layer = )` needs at least one layer.")
    }
    if (is.character(layer)) {
        idx <- match(layer, names(plot$layer))
    } else {
        idx <- layer
    }
    if (is.na(idx) || !is.numeric(idx) || idx != as.integer(idx) ||
        idx < 1L || idx > length(objects)) {
        bb_abort("`layer` in `bb_legend()` must refer to an existing layer ",
                 "(1 to ", length(objects), ").")
    }
    obj <- objects[[as.integer(idx)]]
    mapping <- bb_layer_aesthetics(plot, obj$mapping,
                                   obj$inherit %||% TRUE,
                                   obj$params %||% list())
    list(
        mapping = mapping,
        data = if (is.data.frame(obj$data)) obj$data else plot$data
    )
}

bb_wrap_title <- function(title, width) {
    if (is.null(title) || is.null(width)) return(title)
    paste(strwrap(as.character(title), width = width), collapse = "\n")
}

bb_outside_legend_width <- function(legends) {
    if (!length(legends)) return(1.4)
    max(vapply(legends, function(obj) obj$width %||% 1.4, numeric(1)))
}

bb_draw_legend <- function(object, plot) {
    aesthetic <- object$aesthetic %||% "col"
    if (!aesthetic %in% c("col", "fill", "pch", "lty", "cex")) {
        return(invisible(NULL))
    }
    data <- plot$data
    mapping <- plot$mapping
    if (!is.null(object$layer)) {
        resolved <- bb_legend_layer(plot, object$layer, aesthetic)
        data <- resolved$data
        mapping <- resolved$mapping
    }
    if (is.null(data)) return(invisible(NULL))

    map_aes <- aesthetic
    if (is.null(mapping[[map_aes]]) && identical(aesthetic, "fill")) {
        map_aes <- "col"
    }
    if (is.null(mapping[[map_aes]])) {
        if (!is.null(object$layer)) {
            bb_abort("the selected layer does not map the `", aesthetic,
                     "` aesthetic.")
        }
        return(invisible(NULL))
    }

    values <- eval_mapping(mapping, map_aes, data)
    if (is.null(values)) return(invisible(NULL))

    scale <- bb_legend_scale(plot, aesthetic)
    ## A fill legend for a col mapping (no fill scale, no fill mapping) keeps
    ## mapping through the col aesthetic, so the legend colours match the layer.
    map_aesthetic <- aesthetic
    if (identical(aesthetic, "fill") &&
        is.null(mapping$fill) && is.null(bb_get_scales(plot)$fill)) {
        map_aesthetic <- "col"
    }
    title <- object$title %||% rlang::as_label(mapping[[map_aes]])
    title <- bb_wrap_title(title, object$title_width)

    continuous <- if (!is.null(scale)) {
        bb_scale_is_continuous(scale, list(values))
    } else {
        aesthetic %in% c("col", "fill") && is.numeric(values)
    }

    if (aesthetic %in% c("col", "fill") && continuous) {
        bb_check_legend_position(object$position, continuous = TRUE)
        bb_draw_colourbar(object, plot, scale, values, title)
        return(invisible(NULL))
    }

    bb_check_legend_position(object$position, continuous = FALSE)
    bb_draw_discrete_legend(object, plot, scale, values, aesthetic, title,
                            map_aesthetic)
}

bb_draw_discrete_legend <- function(object, plot, scale, values,
                                    aesthetic, title,
                                    map_aesthetic = aesthetic) {
    ## A continuous non-colour aesthetic (cex) has no colour bar; summarise its
    ## domain with a few breaks instead of listing every observed value.
    if (!is.null(scale) && !aesthetic %in% c("col", "fill") &&
        bb_scale_is_continuous(scale, list(values))) {
        domain <- scale$domain
        if (is.null(domain) || length(domain) != 2L ||
            any(!is.finite(domain))) {
            domain <- range(values[is.finite(values)])
        }
        if (!length(domain) || any(!is.finite(domain))) return(invisible(NULL))
        breaks <- pretty(domain, n = 5)
        mapped <- bb_map_aesthetic(plot, aesthetic, breaks)
        if (is.null(mapped)) return(invisible(NULL))
        args <- list(
            x = object$position,
            legend = format(breaks, trim = TRUE),
            title = title,
            bty = object$bty
        )
        if (identical(aesthetic, "cex")) {
            args$pch <- object$pch
            args$pt.cex <- unname(mapped)
        } else if (identical(aesthetic, "pch")) {
            args$pch <- unname(mapped)
        } else if (identical(aesthetic, "lty")) {
            args$lty <- unname(mapped)
        }
        args <- c(args, object$params)
        if (isTRUE(object$outside) && is.character(args$x) &&
            args$x %in% c("topright", "right", "bottomright")) {
            op <- graphics::par(xpd = TRUE)
            on.exit(graphics::par(op), add = TRUE)
            args$inset <- c(-0.2, 0)
        }
        do.call(graphics::legend, args)
        return(invisible(NULL))
    }

    if (!is.null(scale) && !is.null(scale$domain)) {
        levels <- scale$domain
    } else {
        levels <- bb_discrete_levels(values)
    }
    if (!length(levels)) return(invisible(NULL))

    labels <- levels
    if (!is.null(scale$labels)) {
        if (!is.null(names(scale$labels))) {
            renamed <- unname(scale$labels[levels])
            labels[!is.na(renamed)] <- renamed[!is.na(renamed)]
        } else if (length(scale$labels) == length(levels)) {
            labels <- as.character(scale$labels)
        }
    }

    mapped <- bb_map_aesthetic(plot, map_aesthetic, levels)
    if (is.null(mapped)) return(invisible(NULL))

    args <- list(
        x = object$position,
        legend = labels,
        title = title,
        bty = object$bty
    )
    if (identical(aesthetic, "fill")) {
        args$fill <- unname(mapped)
        args$border <- NA
    } else if (identical(aesthetic, "col")) {
        args$col <- unname(mapped)
        args$pch <- object$pch
    } else if (identical(aesthetic, "pch")) {
        args$pch <- unname(mapped)
    } else if (identical(aesthetic, "lty")) {
        args$lty <- unname(mapped)
    } else if (identical(aesthetic, "cex")) {
        args$pch <- object$pch
        args$pt.cex <- unname(mapped)
    }
    if (identical(object$style %||% "auto", "bar") &&
        aesthetic %in% c("col", "fill")) {
        args$fill <- unname(mapped)
        args$border <- NA
        args$col <- NULL
        args$pch <- NULL
        args$lty <- NULL
    }
    args <- c(args, object$params)

    if (isTRUE(object$outside) && is.character(args$x) &&
        args$x %in% c("topright", "right", "bottomright")) {
        op <- graphics::par(xpd = TRUE)
        on.exit(graphics::par(op), add = TRUE)
        args$inset <- c(-0.2, 0)
    }

    invisible(do.call(graphics::legend, args))
}

bb_draw_colourbar <- function(object, plot, scale, values, title) {
    domain <- scale$domain
    if (is.null(domain) || length(domain) != 2L || any(!is.finite(domain))) {
        domain <- bb_scale_numeric_domain(list(values))
    }
    if (is.null(domain)) return(invisible(NULL))

    palette <- scale$palette %||% "viridis"
    na.value <- scale$na.value %||% "grey50"
    f <- scales::col_numeric(palette, domain = domain, na.color = na.value)

    breaks <- scale$breaks
    if (is.null(breaks)) {
        breaks <- pretty(domain, n = 5)
    }
    breaks <- breaks[breaks >= domain[[1]] & breaks <= domain[[2]]]
    labels <- scale$labels
    if (!is.null(labels) && length(labels) != length(breaks)) {
        labels <- NULL
    }

    usr <- graphics::par("usr")
    xspan <- usr[[2]] - usr[[1]]
    yspan <- usr[[4]] - usr[[3]]
    pos <- object$position %||% "topright"
    orientation <- object$orientation %||% "vertical"

    outside <- is.numeric(pos)
    if (outside) {
        ## A dedicated legend panel uses [0, 1] user coordinates.
        x0 <- as.numeric(pos)
        y_anchor <- object$params$y %||% 0.85
        bar_w <- 0.10
        bar_h <- 0.30
        x1 <- x0 + bar_w
        y1 <- y_anchor
        y0 <- y1 - bar_h
    } else {
        mar_x <- xspan * 0.02
        mar_y <- yspan * 0.02
        if (identical(orientation, "horizontal")) {
            bar_w <- xspan * 0.45
            bar_h <- yspan * 0.06
        } else {
            bar_w <- xspan * 0.05
            bar_h <- yspan * 0.45
        }
        if (grepl("right", pos)) {
            x1 <- usr[[2]] - mar_x
            x0 <- x1 - bar_w
        } else {
            x0 <- usr[[1]] + mar_x
            x1 <- x0 + bar_w
        }
        if (grepl("bottom", pos)) {
            y0 <- usr[[3]] + mar_y
            y1 <- y0 + bar_h
        } else {
            y1 <- usr[[4]] - mar_y
            y0 <- y1 - bar_h
        }
    }

    n <- 100L
    if (identical(orientation, "horizontal")) {
        xx <- seq(x0, x1, length.out = n + 1L)
        for (i in seq_len(n)) {
            graphics::rect(xx[[i]], y0, xx[[i + 1L]], y1,
                           col = f(domain[[1]] +
                                   (domain[[2]] - domain[[1]]) * (i - 0.5) / n),
                           border = NA)
        }
        graphics::rect(x0, y0, x1, y1, border = "grey30")
        at <- x0 + (breaks - domain[[1]]) / (domain[[2]] - domain[[1]]) * (x1 - x0)
        graphics::text(at, y0 - yspan * 0.03, labels %||% breaks,
                       cex = 0.8, pos = 1)
    } else {
        yy <- seq(y0, y1, length.out = n + 1L)
        for (i in seq_len(n)) {
            graphics::rect(x0, yy[[i]], x1, yy[[i + 1L]],
                           col = f(domain[[1]] +
                                   (domain[[2]] - domain[[1]]) * (i - 0.5) / n),
                           border = NA)
        }
        graphics::rect(x0, y0, x1, y1, border = "grey30")
        at <- y0 + (breaks - domain[[1]]) / (domain[[2]] - domain[[1]]) * (y1 - y0)
        graphics::text(x1 + xspan * 0.01, at, labels %||% breaks,
                       cex = 0.8, adj = c(0, 0.5))
    }

    minor <- scale$minor_breaks
    if (is.null(minor) && length(breaks) >= 2L) {
        minor <- (utils::head(breaks, -1L) + utils::tail(breaks, -1L)) / 2
    }
    minor <- minor[is.finite(minor) & minor > domain[[1]] &
                   minor < domain[[2]]]
    if (length(minor)) {
        if (identical(orientation, "horizontal")) {
            at_minor <- x0 + (minor - domain[[1]]) /
                (domain[[2]] - domain[[1]]) * (x1 - x0)
            graphics::segments(at_minor, y0, at_minor, y0 - yspan * 0.012,
                               col = "grey30")
        } else {
            at_minor <- y0 + (minor - domain[[1]]) /
                (domain[[2]] - domain[[1]]) * (y1 - y0)
            graphics::segments(x1, at_minor, x1 + xspan * 0.012, at_minor,
                               col = "grey30")
        }
    }

    if (identical(orientation, "horizontal")) {
        graphics::text((x0 + x1) / 2, y1 + yspan * 0.03, title, cex = 0.8)
    } else {
        graphics::text(x1 + xspan * 0.01, y1 + yspan * 0.02, title,
                       cex = 0.8, adj = c(0, 0))
    }
    invisible(NULL)
}

##' @method bbplot_add bb_legend
##' @export
bbplot_add.bb_legend <- function(object, plot) {
    ## An outside legend is drawn once in the layout column, not inside the
    ## panel; adding it as a layer would draw a clipped copy over the data.
    if (isTRUE(object$outside)) return(plot)

    ly <- function() {
        bb_draw_legend(object, plot)
    }

    add_layer(plot, ly, "legend")
}
