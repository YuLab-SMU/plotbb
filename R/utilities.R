get_plotbb_env <- function() {
    if(!exists(".bbplot")) .plotbb_env_initial()
    get(".bbplot")
}

with_env <- function(f, e=parent.frame()) {
    stopifnot(is.function(f))
    environment(f) <- e
    f
}


plot_fun <- function(x) {
    if (inherits(x, "formula")) {
        ## convert to expression
        x <- parse(text=as.character(x)[2])
    } 

    function() {
        if (inherits(x, "function"))
            return(x())
        eval(x)
    }
}


bb_data <- function(p, data) {
    data %||% p$data    
}

##' @importFrom utils modifyList
##
## The result is a plain named list of quosures. It must not stay a
## "quosures" object: rlang refuses to `c()` quosure lists with anything else,
## and layers are evaluated with `rlang::exec()`.
bb_mapping <- function(p, mapping, inherit = TRUE) {
    bb_merge_mapping(p$mapping, mapping, inherit = inherit)
}

bb_as_mapping_list <- function(mapping) {
    if (is.null(mapping)) return(list())
    if (!is.list(mapping)) return(list())
    structure(as.list(mapping), class = NULL)
}

## Merge global and layer mappings.
##
## A layer inherits the plot mapping and overrides it aesthetic by aesthetic:
## `bb_point(bb_aes(col = factor(am)))` keeps the global `x`/`y` and replaces
## `col`. With `inherit = FALSE` the layer uses only its own mapping, so a
## mapping that is not repeated locally becomes unavailable rather than falling
## back to the global one.
bb_merge_mapping <- function(global, local = NULL, inherit = TRUE) {
    local <- bb_as_mapping_list(local)
    if (!isTRUE(inherit)) return(local)
    out <- bb_as_mapping_list(global)
    if (!length(local)) return(out)
    for (aesthetic in names(local)) {
        out[[aesthetic]] <- local[[aesthetic]]
    }
    out
}

## The effective mapping of a layer object, honouring `inherit` and constants.
##
## This is the same rule the drawing code follows, so a layer's ranges and the
## grouping of a summary layer agree with what is actually drawn.
bb_layer_mapping <- function(plot, layer_obj) {
    if (!inherits(layer_obj, "bb_layer")) return(bb_mapping(plot, NULL))
    inherit <- layer_obj$inherit
    if (is.null(inherit)) inherit <- TRUE
    bb_layer_aesthetics(plot, layer_obj$mapping, inherit,
                        layer_obj$params %||% list())
}

## The aesthetics a layer draws as constants.
##
## A constant is not a mapping and never goes through a scale, so it must also
## remove the inherited mapping for that aesthetic. Enforcing this in one place
## means every layer — built-in or added later — gets the same rule: a constant
## wins, and neither a scale nor the grouping of a summary layer can revive the
## mapping it overrides.
bb_constant_aesthetics <- function(params, extra = character()) {
    intersect(names(params), c("col", "fill", "pch", "lty", "cex", extra))
}

## The mapping a layer actually draws from: the merged mapping minus every
## aesthetic that is given as a constant. `inherit` only affects the merge.
bb_layer_aesthetics <- function(plot, mapping, inherit, params,
                                extra = character()) {
    mapping <- bb_merge_mapping(plot$mapping, mapping, inherit)
    overridden <- bb_constant_aesthetics(params, extra)
    if (length(overridden)) mapping[overridden] <- NULL
    mapping
}

xvar <- function(mapping) {
    get_mapping(mapping, 'x')
}

yvar <- function(mapping) {
    get_mapping(mapping, 'y')
}

##' @importFrom rlang quo_name
get_mapping <- function(mapping, name) {
    x <- mapping[[name]]
    if (is.null(x)) return(NULL)
    quo_name(x)
}

parse_mapping <- function(mapping, name, data) {
    v <- get_mapping(mapping, name)
    v2 <- gsub(".*\\((\\w+)\\).*", "\\1", v)
    eval(parse(text=sub(v2, "data[[v2]]", v)))
}

eval_mapping <- function(mapping, name, data) {
    x <- mapping[[name]]
    if (is.null(x)) return(NULL)
    rlang::eval_tidy(x, data = data)
}

bb_eval_or_fallback <- function(mapping, data, name, fallback_var = NULL) {
    ## A layer's own data need not carry every inherited variable; an
    ## expression that cannot be evaluated on this data simply contributes
    ## nothing to the range instead of raising "object not found".
    v <- tryCatch(eval_mapping(mapping, name, data), error = function(e) {
        ## Only a variable absent from this layer's data is tolerated; a broken
        ## expression is still an error and should surface.
        if (grepl("not found", conditionMessage(e), fixed = TRUE)) return(NULL)
        stop(e)
    })
    if (!is.null(v)) return(v)
    if (!is.null(fallback_var) && !is.null(data) &&
        fallback_var %in% names(data)) {
        return(data[[fallback_var]])
    }
    NULL
}

bb_eval_xy <- function(mapping, data) {
    x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
    y <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
    list(x = x, y = y)
}

bb_resolve_palette <- function(plot, palette = NULL) {
    if (!is.null(palette)) return(palette)
    if (!is.null(plot) && !is.null(plot$env) && exists("palette", envir = plot$env, inherits = FALSE)) {
        return(get("palette", envir = plot$env, inherits = FALSE))
    }
    NULL
}

bb_call_canvas <- function(canvas, xlim = NULL, ylim = NULL, log = "",
                             xaxt = NULL, yaxt = NULL) {
    fml <- formals(canvas)
    if (is.null(fml)) return(canvas())

    has_dots <- "..." %in% names(fml)
    args <- list()
    if (!is.null(xlim) && (has_dots || "xlim" %in% names(fml))) args$xlim <- xlim
    if (!is.null(ylim) && (has_dots || "ylim" %in% names(fml))) args$ylim <- ylim
    if (nzchar(log)) {
        if (!(has_dots || "log" %in% names(fml))) {
            bb_abort("a positional scale needs a canvas that accepts `log`; ",
                     "this canvas does not.")
        }
        args$log <- log
    }
    if (!is.null(xaxt)) {
        if (!(has_dots || "xaxt" %in% names(fml))) {
            bb_abort("a positional scale needs a canvas that accepts `xaxt`.")
        }
        args$xaxt <- xaxt
    }
    if (!is.null(yaxt)) {
        if (!(has_dots || "yaxt" %in% names(fml))) {
            bb_abort("a positional scale needs a canvas that accepts `yaxt`.")
        }
        args$yaxt <- yaxt
    }
    do.call(canvas, args)
}

## Histograms and densities need a numeric variable to summarise. When `x` is
## discrete (a factor, or a character column of groups), the distribution of
## the numeric `y` variable is computed per group instead of failing, so that
## `bbplot(d, bb_aes(factor(cyl), mpg)) + bb_hist()` works like the grouped
## boxplot does.
bb_stat_values <- function(x, y = NULL) {
    if (is.numeric(x)) return(list(x = x, group = NULL))
    ## An all-NA column read from a file is logical; it is still "the numeric
    ## variable, with nothing in it" and should be skipped with a warning
    ## rather than reported as a type error.
    if (is.logical(x) && all(is.na(x))) {
        return(list(x = as.numeric(x), group = NULL))
    }
    if (is.null(y) || !is.numeric(y) || length(y) != length(x)) {
        bb_abort("a histogram or density needs a numeric variable: map a numeric ",
                 "`x`, or a discrete `x` with a numeric `y`.")
    }
    list(x = y, group = x)
}

## The grouping variable of a statistical layer.
##
## One rule for `bb_hist()` and `bb_density()` (and the same precedence other
## grouped layers use): a discrete `x` groups the numeric variable, otherwise an
## explicit `group` mapping wins, then `col`, then `fill`. A constant aesthetic
## is not a mapping and never groups.
bb_stat_group <- function(values, mapping, data) {
    eval_mapping(mapping, "group", data) %||%
        values$group %||%
        eval_mapping(mapping, "col", data) %||%
        eval_mapping(mapping, "fill", data)
}

##' Resolve the grouping values of a statistical layer
##'
##' The one grouping rule used by the statistical layers: an explicit `group`
##' mapping wins, then a discrete `x`, then a mapped `col`, then a mapped `fill`.
##' Custom statistical layers can call this function so they group like the
##' built-in ones.
##'
##' @title bb_group_values
##' @param mapping the effective aesthetic mapping
##' @param data the layer data
##' @param x optional x values; used when they are a factor or character
##' @return A vector of group values, or NULL when there is no grouping
##' @export
##' @author Guangchuang Yu
bb_group_values <- function(mapping, data, x = NULL) {
    explicit <- eval_mapping(mapping, "group", data)
    if (!is.null(explicit)) return(explicit)
    if (!is.null(x) && (is.factor(x) || is.character(x))) return(x)
    col <- eval_mapping(mapping, "col", data)
    if (!is.null(col)) return(col)
    eval_mapping(mapping, "fill", data)
}

bb_discrete_levels <- function(x) {
    if (is.null(x)) return(character())
    if (is.factor(x)) {
        lev <- levels(x)
        return(lev[!is.na(lev)])
    }
    chr <- as.character(x)
    chr <- chr[!is.na(chr)]
    unique(chr)
}

bb_range_union <- function(rng, x) {
    if (is.null(x)) return(rng)
    if (length(x) == 0) return(rng)

    if (inherits(x, "Date") || inherits(x, "POSIXct")) {
        x <- x[!is.na(x)]
        if (!length(x)) return(rng)
        xr <- range(x)
        if (is.null(rng)) return(xr)
        return(range(c(as.numeric(rng), as.numeric(xr))))
    }
    
    if (is.factor(x)) {
        x <- as.integer(x)
    }
    if (is.character(x)) {
        # Attempt to convert character to integer based on sorted unique levels (simple assumption)
        # This matches how we might treat discrete variables if no strict scale is enforced yet
        x <- as.integer(factor(x))
    }

    ok <- tryCatch(is.finite(x), error = function(e) NULL)
    if (is.null(ok)) return(rng)
    x <- x[ok]
    if (length(x) == 0) return(rng)
    xr <- range(x, na.rm = TRUE)
    if (is.null(rng)) return(xr)
    range(c(rng, xr), na.rm = TRUE)
}

## limits must always be finite: when no layer contributes a range for an
## axis, fall back to [0, 1] instead of handing Inf to plot()
bb_safe_limits <- function(rng, fallback = c(0, 1)) {
    rng <- bb_expand_range(rng)
    if (is.null(rng) || length(rng) != 2 || any(!is.finite(rng))) {
        return(fallback)
    }
    rng
}

bb_expand_range <- function(rng, mult = 0.04) {
    if (is.null(rng)) return(NULL)
    if (!all(is.finite(rng))) return(rng)
    span <- diff(rng)
    if (!is.finite(span) || span == 0) {
        pad <- ifelse(rng[[1]] == 0, 1, abs(rng[[1]]) * mult)
        return(rng + c(-pad, pad))
    }
    pad <- span * mult
    rng + c(-pad, pad)
}

## Layers whose drawing coordinates are computed from the data (counts, density,
## fitted values) contribute their ranges through `bb_layer_limits()`; adding
## their mapped x/y to the canvas instead would mix two coordinate systems.
## Automatic axis labels for summary layers.
##
## A histogram puts counts on y; a density over a discrete x puts the numeric
## variable on x and the density on y. The labels created by `bbplot()` follow
## the mapping, so they are replaced only when they still say what the mapping
## says; a label set by the user is kept.
bb_summary_labs <- function(plot) {
    if (is.null(plot$data) || is.null(plot$mapping)) return(plot)
    layers <- Filter(function(o) inherits(o, "bb_layer"), plot$adds)
    is_ly <- function(fun) any(vapply(layers, function(o) identical(o$layer, fun),
                                      logical(1)))
    dens <- is_ly(ly_density)
    hist <- is_ly(ly_hist)
    if (!dens && !hist) return(plot)

    x_label <- if (!is.null(plot$mapping$x)) rlang::as_label(plot$mapping$x)
    y_label <- if (!is.null(plot$mapping$y)) rlang::as_label(plot$mapping$y)

    x_values <- eval_mapping(plot$mapping, "x", plot$data)
    x_discrete <- !is.null(x_values) && !is.numeric(x_values)

    auto_x <- is.null(plot$labs$xlab) || identical(plot$labs$xlab, x_label)
    auto_y <- is.null(plot$labs$ylab) || identical(plot$labs$ylab, y_label)

    if (x_discrete && !is.null(y_label)) {
        if (auto_x) plot$labs$xlab <- y_label
        if (auto_y) plot$labs$ylab <- if (dens) "density" else "count"
    } else if (dens) {
        if (auto_y) plot$labs$ylab <- "density"
    } else if (hist) {
        if (auto_y) plot$labs$ylab <- "count"
    }
    plot
}

bb_plot_summarises <- function(plot) {
    any(vapply(plot$adds, function(obj) {
        inherits(obj, "bb_layer") &&
            (isTRUE((obj$meta %||% list())$summary) ||
                 identical(obj$layer, ly_hist) ||
                 identical(obj$layer, ly_density) ||
                 identical(obj$layer, ly_violin))
    }, logical(1)))
}

bb_layer_position <- function(plot, layer_obj) {
    objects <- Filter(function(obj) inherits(obj, "bb_layer"), plot$adds)
    Position(function(obj) identical(obj, layer_obj), objects)
}

bb_layer_is_reference <- function(layer_obj) {
    if (!inherits(layer_obj, "bb_layer")) return(FALSE)
    any(vapply(list(ly_hline, ly_vline), identical, logical(1), layer_obj$layer))
}

bb_layer_is_summary <- function(layer_obj) {
    if (!inherits(layer_obj, "bb_layer")) return(FALSE)
    if (isTRUE((layer_obj$meta %||% list())$summary)) return(TRUE)
    summary_layers <- list(ly_hist, ly_density, ly_bar, ly_boxplot, ly_lm,
                           ly_signif, ly_violin)
    any(vapply(summary_layers, identical, logical(1), layer_obj$layer))
}

bb_layer_limits <- function(layer_obj, plot) {
    if (!inherits(layer_obj, "bb_layer")) return(NULL)

    ## Ranges the layer declares explicitly (`bb_layer(limits = ...)`) take
    ## part in the canvas even though they are not mapped from the data, e.g. a
    ## reference line or an annotation drawn outside the data range.
    declared <- layer_obj$limits
    xlim <- bb_range_union(NULL, declared$x)
    ylim <- bb_range_union(NULL, declared$y)

    ## Read the ranges accumulated so far (including declared ones), so an early
    ## exit never discards a `limits` declaration.
    current <- function() {
        if (is.null(xlim) && is.null(ylim)) return(NULL)
        list(xlim = xlim, ylim = ylim)
    }

    data <- bb_data(plot, layer_obj$data)
    mapping <- bb_layer_mapping(plot, layer_obj)
    ly <- layer_obj$layer
    meta <- layer_obj$meta %||% list()

    if (is.function(meta$range)) {
        extra <- tryCatch(meta$range(plot, data, mapping),
                          error = function(e) NULL)
        if (!is.null(extra)) {
            if (!is.list(extra) || is.null(names(extra)) ||
                !all(names(extra) %in% c("x", "y"))) {
                bb_abort("`range` in `bb_layer()` must return a list with ",
                         "`x` and/or `y`.")
            }
            xlim <- bb_range_union(xlim, extra$x)
            ylim <- bb_range_union(ylim, extra$y)
        }
        if (isTRUE(meta$summary)) return(current())
    }

    if (identical(ly, ly_hline) || identical(ly, ly_vline)) {
        if (isTRUE(layer_obj$params$expand_limits)) {
            if (identical(ly, ly_hline)) {
                ref <- bb_reference_values(mapping, data,
                                           layer_obj$params$yintercept,
                                           "yintercept")
                ylim <- bb_range_union(ylim, ref$values)
            } else {
                ref <- bb_reference_values(mapping, data,
                                           layer_obj$params$xintercept,
                                           "xintercept")
                xlim <- bb_range_union(xlim, ref$values)
            }
        }
        return(current())
    }

    if (is.null(data) || !length(mapping)) {
        if (is.null(xlim) && is.null(ylim)) return(NULL)
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_ribbon)) {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        ymin <- eval_mapping(mapping, "ymin", data)
        ymax <- eval_mapping(mapping, "ymax", data)
        if (is.null(x) || is.null(ymin) || is.null(ymax)) return(current())
        xlim <- bb_range_union(xlim, x)
        ylim <- bb_range_union(ylim, ymin)
        ylim <- bb_range_union(ylim, ymax)
        return(current())
    }

    if (identical(ly, ly_segment)) {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        y <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
        xend <- bb_eval_or_fallback(mapping, data, "xend", NULL) %||% bb_eval_or_fallback(mapping, data, "x1", NULL)
        yend <- bb_eval_or_fallback(mapping, data, "yend", NULL) %||% bb_eval_or_fallback(mapping, data, "y1", NULL)
        xlim <- bb_range_union(xlim, x)
        xlim <- bb_range_union(xlim, xend)
        ylim <- bb_range_union(ylim, y)
        ylim <- bb_range_union(ylim, yend)
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_errorbar)) {
        orientation <- layer_obj$params$orientation %||% "y"
        width <- layer_obj$params$width %||% 0.5
        if (identical(orientation, "y")) {
            x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
            ymin <- bb_eval_or_fallback(mapping, data, "ymin", NULL)
            ymax <- bb_eval_or_fallback(mapping, data, "ymax", NULL)
            xlim <- bb_range_union(xlim, x - width/2)
            xlim <- bb_range_union(xlim, x + width/2)
            ylim <- bb_range_union(ylim, ymin)
            ylim <- bb_range_union(ylim, ymax)
            return(list(xlim = xlim, ylim = ylim))
        }

        y <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
        xmin <- bb_eval_or_fallback(mapping, data, "xmin", NULL)
        xmax <- bb_eval_or_fallback(mapping, data, "xmax", NULL)
        xlim <- bb_range_union(xlim, xmin)
        xlim <- bb_range_union(xlim, xmax)
        ylim <- bb_range_union(ylim, y - width/2)
        ylim <- bb_range_union(ylim, y + width/2)
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_lm)) {
        formula <- stats::as.formula(paste(yvar(mapping), "~", xvar(mapping)))
        grp <- eval_mapping(mapping, "group", data)
        se <- layer_obj$params$se %||% TRUE
        level <- layer_obj$params$level %||% 0.95
        if (is.null(grp)) {
            d2 <- lm_data(formula, data, se = se, level = level)
            if (is.null(d2)) return(current())
            xlim <- bb_range_union(xlim, c(d2$x0, d2$x1))
            ylim <- bb_range_union(ylim, c(d2$y0, d2$y1))
            if (se && !is.null(d2$ci)) {
                ylim <- bb_range_union(ylim, c(d2$ci$lwr, d2$ci$upr))
            }
            return(list(xlim = xlim, ylim = ylim))
        }

        grp_chr <- as.character(grp)
        keep <- !is.na(grp_chr)
        if (!any(keep)) return(current())
        for (g in unique(grp_chr[keep])) {
            d <- data[grp_chr == g, , drop = FALSE]
            d2 <- lm_data(formula, d, se = se, level = level)
            if (is.null(d2)) next
            xlim <- bb_range_union(xlim, c(d2$x0, d2$x1))
            ylim <- bb_range_union(ylim, c(d2$y0, d2$y1))
            if (se && !is.null(d2$ci)) {
                ylim <- bb_range_union(ylim, c(d2$ci$lwr, d2$ci$upr))
            }
        }
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_bar)) {
        stat <- layer_obj$params$stat %||% "identity"
        width <- layer_obj$params$width %||% 0.9

        x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        if (identical(stat, "count")) {
            x_chr_all <- as.character(x_raw)
            x_chr <- x_chr_all[!is.na(x_chr_all)]
            if (length(x_chr) == 0) return(current())

            lev <- bb_discrete_levels(x_raw)
            tab <- table(factor(x_chr, levels = lev))
            xpos <- seq_along(tab)
            y <- as.numeric(tab)
            xlim <- bb_range_union(xlim, xpos - width/2)
            xlim <- bb_range_union(xlim, xpos + width/2)
            ylim <- bb_range_union(ylim, c(0, y))
            return(list(xlim = xlim, ylim = ylim))
        }

        y_raw <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
        if (is.numeric(x_raw)) {
            xpos <- x_raw
        } else {
            labels <- unique(as.character(x_raw))
            xpos <- match(as.character(x_raw), labels)
        }
        xlim <- bb_range_union(xlim, xpos - width/2)
        xlim <- bb_range_union(xlim, xpos + width/2)
        ylim <- bb_range_union(ylim, c(0, y_raw))
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_boxplot)) {
        x <- bb_eval_or_fallback(mapping, data, "x", NULL)
        y <- bb_eval_or_fallback(mapping, data, "y", NULL)
        width <- layer_obj$params$boxwex %||% 0.8

        if (!is.null(x)) {
            if (is.factor(x) || is.character(x)) {
                xi <- as.integer(as.factor(x))
                xlim <- bb_range_union(xlim, xi - width/2)
                xlim <- bb_range_union(xlim, xi + width/2)
            } else {
                 xlim <- bb_range_union(xlim, x - width/2)
                 xlim <- bb_range_union(xlim, x + width/2)
            }
        } else {
            xlim <- c(1 - width/2, 1 + width/2)
        }
        ylim <- bb_range_union(ylim, y)
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_smooth)) {
        x <- bb_eval_or_fallback(mapping, data, "x", NULL)
        y <- bb_eval_or_fallback(mapping, data, "y", NULL)
        if (is.null(x) || is.null(y) || !is.numeric(x) || !is.numeric(y)) {
            return(current())
        }
        ok <- is.finite(x) & is.finite(y)
        if (!any(ok)) return(current())
        xlim <- bb_range_union(xlim, x[ok])
        ylim <- bb_range_union(ylim, y[ok])
        cached <- if (exists("bb_layer_ranges", envir = plot$env,
                             inherits = FALSE)) {
            get("bb_layer_ranges", envir = plot$env, inherits = FALSE)
        } else {
            list()
        }
        idx <- bb_layer_position(plot, layer_obj)
        if (!is.null(idx) && length(cached) >= idx) {
            band <- cached[[idx]]
            if (!is.null(band)) {
                xlim <- bb_range_union(xlim, band$x)
                ylim <- bb_range_union(ylim, band$y)
            }
        }
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_violin)) {
        x <- bb_eval_or_fallback(mapping, data, "x", NULL)
        y <- bb_eval_or_fallback(mapping, data, "y", NULL)
        if (is.null(y) || !is.numeric(y)) return(current())
        width <- layer_obj$params$width %||% 0.9
        n <- max(length(y), length(x) %||% 0L)
        y <- rep(y, length.out = n)
        x <- if (is.null(x)) NULL else rep(x, length.out = n)
        grp <- bb_group_values(mapping, data, x)
        pos <- bb_violin_positions(x, grp, n)
        ok <- is.finite(y) & is.finite(pos$positions) & !is.na(pos$groups)
        if (!any(ok)) return(current())
        xlim <- bb_range_union(xlim, pos$positions[ok] - width / 2)
        xlim <- bb_range_union(xlim, pos$positions[ok] + width / 2)
        ylim <- bb_range_union(ylim, y[ok])
        ## With trim = FALSE the density support extends past the data, so the
        ## range has to include the estimated support of every group.
        trim <- layer_obj$params$trim %||% TRUE
        if (isFALSE(trim)) {
            params <- layer_obj$params %||% list()
            bw <- params$bw
            adjust <- params$adjust %||% 1
            kernel <- params$kernel %||% "gaussian"
            n_grid <- params$n %||% 512L
            grp <- pos$groups
            gy <- rep(y, length.out = n)
            for (g in sort(unique(grp))) {
                values <- gy[grp == g & is.finite(gy)]
                if (length(values) < 2L || stats::sd(values) == 0) next
                args <- list(x = values, adjust = adjust, kernel = kernel,
                             n = n_grid, trim = FALSE)
                if (!is.null(bw)) args$bw <- bw
                d <- suppressWarnings(tryCatch(
                    do.call(stats::density, args), error = function(e) NULL
                ))
                if (!is.null(d)) ylim <- bb_range_union(ylim, d$x)
            }
        }
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_hist)) {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        y <- eval_mapping(mapping, "y", data)
        if (is.null(x)) return(current())
        values <- bb_stat_values(x, y)
        keep <- is.finite(values$x)
        x <- values$x[keep]
        if (length(x) < 2) return(current())

        ## Grouped histograms share one break grid, so the panel has to cover the
        ## tallest single group rather than the pooled counts.
        grp <- bb_stat_group(values, mapping, data)
        group <- if (is.null(grp)) rep("all", length(x)) else
            as.character(rep(grp, length.out = length(values$x))[keep])
        breaks <- layer_obj$params$breaks %||% "Sturges"
        all_breaks <- graphics::hist(x, breaks = breaks, plot = FALSE)$breaks
        for (g in unique(group[!is.na(group)])) {
            sub_x <- x[group == g]
            if (length(sub_x) < 2) next
            h <- graphics::hist(sub_x, breaks = all_breaks, plot = FALSE)
            xlim <- bb_range_union(xlim, h$breaks)
            ylim <- bb_range_union(ylim, c(0, h$counts))
        }
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_density)) {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        y <- eval_mapping(mapping, "y", data)
        if (!is.null(x)) {
            values <- bb_stat_values(x, y)
            x <- values$x
        }
        if (!is.null(x) && is.numeric(x)) {
            grp <- bb_stat_group(values, mapping, data)
            if (is.null(grp)) {
                x <- x[is.finite(x)]
                if (length(x) >= 2) {
                    d <- stats::density(x, na.rm = TRUE)
                    xlim <- bb_range_union(xlim, d$x)
                    ylim <- bb_range_union(ylim, c(0, d$y))
                }
            } else {
                grp_chr <- as.character(grp)
                for (g in unique(grp_chr[!is.na(grp_chr)])) {
                    sub_x <- x[grp_chr == g & is.finite(x)]
                    if (length(sub_x) >= 2) {
                        d <- stats::density(sub_x, na.rm = TRUE)
                        xlim <- bb_range_union(xlim, d$x)
                        ylim <- bb_range_union(ylim, c(0, d$y))
                    }
                }
            }
        }
        return(list(xlim = xlim, ylim = ylim))
    }

    if (identical(ly, ly_signif)) {
        tip_length <- layer_obj$params$tip_length %||% 0.03
        text_gap <- layer_obj$params$text_gap %||% 0.01
        step_increase <- layer_obj$params$step_increase %||% 0.05

        comparisons <- layer_obj$params$comparisons %||% NULL
        y_position_param <- layer_obj$params$y_position %||% NULL

        if (!is.null(comparisons)) {
            x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
            y_raw <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
            if (is.null(x_raw) || is.null(y_raw)) return(current())

            if (!is.list(comparisons)) comparisons <- list(comparisons)
            ncomp <- length(comparisons)
            if (ncomp == 0) return(current())

            y_span <- diff(range(y_raw, na.rm = TRUE))
            if (!is.finite(y_span) || y_span == 0) y_span <- 1

            if (is.null(y_position_param)) {
                y_top <- max(y_raw, na.rm = TRUE)
                y <- y_top + y_span * step_increase * seq_len(ncomp)
            } else if (length(y_position_param) == 1) {
                y <- rep(y_position_param, ncomp)
            } else {
                y <- y_position_param
            }

            if (is.numeric(x_raw)) {
                lev <- NULL
                get_pos <- function(x) as.numeric(x)
            } else {
                lev <- bb_discrete_levels(x_raw)
                get_pos <- function(x) match(as.character(x), lev)
            }

            xmin <- numeric(0)
            xmax <- numeric(0)
            for (i in seq_len(ncomp)) {
                comp <- comparisons[[i]]
                if (length(comp) < 2) next
                p1 <- get_pos(comp[[1]])
                p2 <- get_pos(comp[[2]])
                if (is.na(p1) || is.na(p2)) next
                xmin <- c(xmin, min(p1, p2))
                xmax <- c(xmax, max(p1, p2))
            }
            if (!length(xmin)) return(current())

            xlim <- bb_range_union(xlim, xmin)
            xlim <- bb_range_union(xlim, xmax)
            ylim <- bb_range_union(ylim, y - y_span * tip_length)
            ylim <- bb_range_union(ylim, y + y_span * text_gap)
            return(list(xlim = xlim, ylim = ylim))
        }

        xmin <- bb_eval_or_fallback(mapping, data, "xmin", NULL)
        xmax <- bb_eval_or_fallback(mapping, data, "xmax", NULL)
        y <- bb_eval_or_fallback(mapping, data, "y_position", NULL) %||% bb_eval_or_fallback(mapping, data, "y", NULL)
        if (is.null(xmin) || is.null(xmax) || is.null(y)) return(current())

        xlim <- bb_range_union(xlim, xmin)
        xlim <- bb_range_union(xlim, xmax)
        y_span <- diff(range(y, na.rm = TRUE))
        if (!is.finite(y_span) || y_span == 0) y_span <- 1
        ylim <- bb_range_union(ylim, y - y_span * tip_length)
        ylim <- bb_range_union(ylim, y + y_span * text_gap)
        return(list(xlim = xlim, ylim = ylim))
    }

    xy <- bb_eval_xy(mapping, data)
    xlim <- bb_range_union(xlim, xy$x)
    ylim <- bb_range_union(ylim, xy$y)
    list(xlim = xlim, ylim = ylim)
}

bb_eval_one <- function(quo, data) {
    if (is.null(quo)) return(NULL)
    rlang::eval_tidy(quo, data = data)
}


bb_plot_limits <- function(plot) {
    xlim <- NULL
    ylim <- NULL

    ## Histograms and densities draw summary coordinates, not the mapped x/y:
    ## a histogram puts counts on y, and a density over a discrete x puts the
    ## numeric variable on x and the density on y. Adding the plot-level x/y
    ## here would mix the two coordinate systems and flatten the panels.
    summarises <- bb_plot_summarises(plot)

    if (!summarises && !is.null(plot$data) && !is.null(plot$mapping)) {
        xy <- bb_eval_xy(plot$mapping, plot$data)
        xlim <- bb_range_union(xlim, xy$x)
        ylim <- bb_range_union(ylim, xy$y)
    }

    ## A layer with its own data is a first-class input: its coordinates take
    ## part in the ranges, so `bb_point(data = d)` is not drawn outside a
    ## canvas built from the plot data alone.
    if (length(plot$adds)) {
        for (obj in plot$adds) {
            if (!inherits(obj, "bb_layer") || !is.data.frame(obj$data)) next
            ## A summary layer's mapped y is data, not a coordinate; a
            ## reference line does not change the range unless it declares so.
            if (bb_layer_is_summary(obj) || bb_layer_is_reference(obj)) next
            mapping <- bb_layer_mapping(plot, obj)
            if (!length(mapping)) next
            xy <- bb_eval_xy(mapping, obj$data)
            xlim <- bb_range_union(xlim, xy$x)
            ylim <- bb_range_union(ylim, xy$y)
        }
    }

    if (length(plot$adds)) {
        for (obj in plot$adds) {
            lim <- bb_layer_limits(obj, plot)
            if (!is.null(lim$xlim)) xlim <- bb_range_union(xlim, lim$xlim)
            if (!is.null(lim$ylim)) ylim <- bb_range_union(ylim, lim$ylim)
        }
    }

    list(
        xlim = bb_safe_limits(xlim),
        ylim = bb_safe_limits(ylim)
    )
}

##' @importFrom scales col_numeric
##' @importFrom scales col_factor
bb_col <- function(mapping, data, plot = NULL, palette = NULL) {
    v <- eval_mapping(mapping, "col", data)
    if (is.null(v)) return(NULL)
    bb_map_aesthetic(plot, "col", v, palette = palette)
}

bb_fill <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "fill", data)
    if (is.null(v)) return(NULL)
    bb_map_aesthetic(plot, "fill", v)
}

bb_pch <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "pch", data)
    if (is.null(v)) return(NULL)
    bb_map_aesthetic(plot, "pch", v)
}

bb_lty <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "lty", data)
    if (is.null(v)) return(NULL)
    bb_map_aesthetic(plot, "lty", v)
}

bb_cex <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "cex", data)
    if (is.null(v)) return(NULL)
    bb_map_aesthetic(plot, "cex", v)
}

`%||%` <- function (a, b) {
    if (!is.null(a)) 
        a
    else b
}

##' @importFrom magrittr %>%
##' @export
magrittr::`%>%`


utils::globalVariables(".")
