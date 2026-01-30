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
bb_mapping <- function(p, mapping) {
    if (!is.null(mapping)) {
        mapping <- modifyList(p$mapping, mapping)
    } else {
        mapping <- p$mapping
    }

    return(mapping)
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
    v <- eval_mapping(mapping, name, data)
    if (!is.null(v)) return(v)
    if (!is.null(fallback_var)) return(data[[fallback_var]])
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

bb_call_canvas <- function(canvas, xlim = NULL, ylim = NULL) {
    fml <- formals(canvas)
    if (is.null(fml)) return(canvas())

    has_dots <- "..." %in% names(fml)
    args <- list()
    if (!is.null(xlim) && (has_dots || "xlim" %in% names(fml))) args$xlim <- xlim
    if (!is.null(ylim) && (has_dots || "ylim" %in% names(fml))) args$ylim <- ylim
    do.call(canvas, args)
}

bb_discrete_levels <- function(x) {
    if (is.null(x)) return(character())
    if (is.factor(x)) {
        lev <- levels(x)
        return(lev[!is.na(lev)])
    }
    chr <- as.character(x)
    chr <- chr[!is.na(chr)]
    sort(unique(chr))
}

bb_range_union <- function(rng, x) {
    if (is.null(x)) return(rng)
    if (length(x) == 0) return(rng)
    
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

bb_layer_limits <- function(layer_obj, plot) {
    if (!inherits(layer_obj, "bb_layer")) return(NULL)

    data <- bb_data(plot, layer_obj$data)
    mapping <- bb_mapping(plot, layer_obj$mapping)

    if (is.null(data) || is.null(mapping)) return(NULL)

    ly <- layer_obj$layer
    xlim <- NULL
    ylim <- NULL

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
        if (is.null(grp)) {
            d2 <- lm_data(formula, data)
            if (is.null(d2)) return(NULL)
            xlim <- bb_range_union(xlim, c(d2$x0, d2$x1))
            ylim <- bb_range_union(ylim, c(d2$y0, d2$y1))
            return(list(xlim = xlim, ylim = ylim))
        }

        grp_chr <- as.character(grp)
        keep <- !is.na(grp_chr)
        if (!any(keep)) return(NULL)
        for (g in unique(grp_chr[keep])) {
            d <- data[grp_chr == g, , drop = FALSE]
            d2 <- lm_data(formula, d)
            if (is.null(d2)) next
            xlim <- bb_range_union(xlim, c(d2$x0, d2$x1))
            ylim <- bb_range_union(ylim, c(d2$y0, d2$y1))
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
            if (length(x_chr) == 0) return(NULL)

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
        
        xlim <- NULL
        ylim <- NULL
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

    if (identical(ly, ly_hist)) {
        x <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
        h <- graphics::hist(x, plot = FALSE)
        xlim <- bb_range_union(xlim, h$breaks)
        ylim <- bb_range_union(ylim, c(0, h$counts)) # Assuming counts for now
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
            if (is.null(x_raw) || is.null(y_raw)) return(NULL)

            if (!is.list(comparisons)) comparisons <- list(comparisons)
            ncomp <- length(comparisons)
            if (ncomp == 0) return(NULL)

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
            if (!length(xmin)) return(NULL)

            xlim <- bb_range_union(xlim, xmin)
            xlim <- bb_range_union(xlim, xmax)
            ylim <- bb_range_union(ylim, y - y_span * tip_length)
            ylim <- bb_range_union(ylim, y + y_span * text_gap)
            return(list(xlim = xlim, ylim = ylim))
        }

        xmin <- bb_eval_or_fallback(mapping, data, "xmin", NULL)
        xmax <- bb_eval_or_fallback(mapping, data, "xmax", NULL)
        y <- bb_eval_or_fallback(mapping, data, "y_position", NULL) %||% bb_eval_or_fallback(mapping, data, "y", NULL)
        if (is.null(xmin) || is.null(xmax) || is.null(y)) return(NULL)

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

    if (!is.null(plot$data) && !is.null(plot$mapping)) {
        xy <- bb_eval_xy(plot$mapping, plot$data)
        xlim <- bb_range_union(xlim, xy$x)
        ylim <- bb_range_union(ylim, xy$y)
    }

    if (length(plot$adds)) {
        for (obj in plot$adds) {
            lim <- bb_layer_limits(obj, plot)
            if (!is.null(lim$xlim)) xlim <- bb_range_union(xlim, lim$xlim)
            if (!is.null(lim$ylim)) ylim <- bb_range_union(ylim, lim$ylim)
        }
    }

    list(
        xlim = bb_expand_range(xlim),
        ylim = bb_expand_range(ylim)
    )
}

##' @importFrom scales col_numeric
##' @importFrom scales col_factor
bb_col <- function(mapping, data, plot = NULL, palette = NULL) {
    col_var <- eval_mapping(mapping, 'col', data)
    if (is.null(col_var)) return(NULL)

    if (!is.null(plot) && !is.null(plot$env) && exists("scales", envir = plot$env, inherits = FALSE)) {
        scales <- get("scales", envir = plot$env, inherits = FALSE)
        if (!is.null(scales$col)) {
            return(bb_scale_col_map(scales$col, col_var))
        }
    }

    if (!is.null(plot) && !is.null(plot$scales) && !is.null(plot$scales$col)) {
        return(bb_scale_col_map(plot$scales$col, col_var))
    }

    palette <- bb_resolve_palette(plot, palette)

    if (is.numeric(col_var)) {
        if (is.null(palette)) palette <- "viridis"
        f <- scales::col_numeric(palette, col_var)
        col_vec <- f(col_var)
    } else {
        ucol <- sort(unique(as.character(col_var)))
        if (is.null(palette)) palette <- "Set2"
        if (is.character(palette) && length(palette) > 1) {
            cols <- rep(palette, length.out = length(ucol))
            if (!is.null(names(palette))) {
                cols <- palette[ucol]
            }
            names(cols) <- ucol
            col_vec <- cols[as.character(col_var)]
        } else {
            f <- scales::col_factor(palette, ucol)
            cols <- f(ucol)
            names(cols) <- ucol
            col_vec <- cols[as.character(col_var)]
        }
    }
    return(col_vec)
}

bb_pch <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "pch", data)
    if (is.null(v)) return(NULL)

    if (!is.null(plot) && !is.null(plot$env) && exists("scales", envir = plot$env, inherits = FALSE)) {
        scales <- get("scales", envir = plot$env, inherits = FALSE)
        if (!is.null(scales$pch)) {
            out <- bb_scale_map(scales$pch, v)
            if (!is.null(out)) return(out)
        }
    }

    if (is.numeric(v)) return(as.integer(v))
    lev <- bb_discrete_levels(v)
    key <- as.character(v)
    pal <- rep(1:25, length.out = length(lev))
    names(pal) <- lev
    unname(pal[key])
}

bb_lty <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "lty", data)
    if (is.null(v)) return(NULL)

    if (!is.null(plot) && !is.null(plot$env) && exists("scales", envir = plot$env, inherits = FALSE)) {
        scales <- get("scales", envir = plot$env, inherits = FALSE)
        if (!is.null(scales$lty)) {
            out <- bb_scale_map(scales$lty, v)
            if (!is.null(out)) return(out)
        }
    }

    if (is.numeric(v)) return(v)
    lev <- bb_discrete_levels(v)
    key <- as.character(v)
    pal <- rep(1:6, length.out = length(lev))
    names(pal) <- lev
    unname(pal[key])
}

bb_cex <- function(mapping, data, plot = NULL) {
    v <- eval_mapping(mapping, "cex", data)
    if (is.null(v)) return(NULL)

    if (!is.null(plot) && !is.null(plot$env) && exists("scales", envir = plot$env, inherits = FALSE)) {
        scales <- get("scales", envir = plot$env, inherits = FALSE)
        if (!is.null(scales$cex)) {
            out <- bb_scale_map(scales$cex, v)
            if (!is.null(out)) return(out)
        }
    }

    if (is.numeric(v)) return(v)
    lev <- bb_discrete_levels(v)
    key <- as.character(v)
    pal <- rep(c(0.8, 1.1, 1.4, 1.7), length.out = length(lev))
    names(pal) <- lev
    unname(pal[key])
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
