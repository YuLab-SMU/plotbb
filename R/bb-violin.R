##' violin layer
##'
##' `bb_violin()` draws one density polygon per group: `x` selects the group
##' (a factor, character or numeric position), `y` is the numeric variable whose
##' distribution is estimated with `stats::density()`. The density is scaled to
##' `width` in x units, so groups stay comparable; `adjust` and `bw` control the
##' bandwidth, `kernel`, `trim` and `n` are passed to `density()`.
##'
##' The layer follows the grammar contract: it inherits data and mapping, accepts
##' its own, draws in `+` order, honours constant or mapped `fill`/`col`, and
##' contributes the group positions and the numeric values to the ranges. Only
##' vertical violins are drawn. A group needs at least two finite values and a
##' non-zero spread; other groups are skipped with a warning. Rows with a
##' non-finite `y` are dropped.
##'
##' @title bb_violin
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... additional parameters: `width` (default 0.9), `adjust` (1),
##'   `bw`, `kernel`, `trim`, `n`, `alpha`, `border`, `fill`/`col`.
##' @return A modified bbplot object
##' @importFrom graphics polygon
##' @export
##' @author Guangchuang Yu
bb_violin <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_violin)
}

ly_violin <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)

    width <- params$width %||% 0.9
    adjust <- params$adjust %||% 1
    kernel <- match.arg(params$kernel %||% "gaussian",
                        c("gaussian", "epanechnikov", "rectangular",
                          "triangular", "biweight", "cosine", "optcosine"))
    trim <- params$trim %||% TRUE
    if (!is.logical(trim) || length(trim) != 1L || is.na(trim)) {
        bb_abort("`trim` in `bb_violin()` must be TRUE or FALSE.")
    }
    n_grid <- params$n %||% 512L
    bw <- params$bw
    alpha <- params$alpha %||% 1
    border <- params$border %||% NA

    if (!is.numeric(n_grid) || length(n_grid) != 1L || n_grid < 2 ||
        n_grid != as.integer(n_grid)) {
        bb_abort("`n` in `bb_violin()` must be an integer of at least 2.")
    }
    n_grid <- as.integer(n_grid)
    if (!is.null(bw)) {
        if (!is.numeric(bw) || length(bw) != 1L || !is.finite(bw) ||
            bw <= 0) {
            bb_abort("`bw` in `bb_violin()` must be a positive number or NULL.")
        }
    }

    for (nm in c("width", "adjust")) {
        value <- get(nm)
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
            value <= 0) {
            bb_abort("`", nm, "` in `bb_violin()` must be a positive number.")
        }
    }
    if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
        alpha < 0 || alpha > 1) {
        bb_abort("`alpha` in `bb_violin()` must be a single number between 0 ",
                 "and 1.")
    }

    ly <- function() {
        y <- eval_mapping(mapping, "y", data)
        if (is.null(y)) {
            bb_abort("`bb_violin()` requires a numeric `y` mapping.")
        }
        if (!is.numeric(y)) {
            bb_abort("`bb_violin()` requires a numeric `y` mapping.")
        }
        x <- eval_mapping(mapping, "x", data)
        n <- max(length(y), length(x) %||% 0L)
        bb_check_length(y, n, "y", "bb_violin")
        if (!is.null(x)) bb_check_length(x, n, "x", "bb_violin")
        y <- rep(y, length.out = n)
        x <- if (is.null(x)) NULL else rep(x, length.out = n)

        ok <- is.finite(y)
        if (!any(ok)) {
            bb_warn("`bb_violin()` has no finite `y` values to draw; skipped.")
            return(invisible(NULL))
        }

        if (is.null(x)) {
            groups <- rep("all", n)
            positions <- rep(1, n)
            labels <- NULL
        } else if (is.factor(x) || is.character(x)) {
            labels <- sort(unique(as.character(x[ok])))
            groups <- as.character(x)
            positions <- match(groups, labels)
        } else {
            positions <- as.numeric(x)
            groups <- as.character(positions)
            labels <- sort(unique(positions[ok]))
        }

        ok <- ok & is.finite(positions)
        if (!any(ok)) {
            bb_warn("`bb_violin()` has no finite group positions to draw; ",
                    "skipped.")
            return(invisible(NULL))
        }

        group_map <- eval_mapping(mapping, "group", data)
        if (!is.null(group_map)) {
            groups <- as.character(rep(group_map, length.out = n))
        }

        fill_vec <- NULL
        if (!is.null(mapping$fill)) {
            fill_vec <- bb_fill(mapping, data, plot = plot)
        } else if (!is.null(mapping$col)) {
            fill_vec <- bb_col(mapping, data, plot = plot)
        }
        fill_vec <- if (is.null(fill_vec)) NULL else
            rep(fill_vec, length.out = n)

        drawn <- character()
        for (g in sort(unique(groups[ok]))) {
            idx <- ok & groups == g
            values <- y[idx]
            values <- values[is.finite(values)]
            if (length(values) < 2L || stats::sd(values) == 0) {
                bb_warn("`bb_violin()` skipped group '", g,
                        "': it needs at least two finite values with a ",
                        "non-zero spread.")
                next
            }
            density_args <- list(x = values, adjust = adjust,
                                 kernel = kernel, n = n_grid)
            if (!is.null(bw)) density_args$bw <- bw
            d <- tryCatch(do.call(stats::density, density_args),
                          error = function(e) NULL)
            if (!is.null(d) && isTRUE(trim)) {
                keep_density <- d$x >= min(values) & d$x <= max(values)
                d$x <- d$x[keep_density]
                d$y <- d$y[keep_density]
            }
            if (is.null(d) || !any(is.finite(d$y)) || max(d$y) == 0) {
                bb_warn("`bb_violin()` skipped group '", g,
                        "': its density could not be estimated.")
                next
            }
            pos <- positions[idx][1]
            half <- width / 2
            scale <- half / max(d$y)
            poly_x <- c(pos - d$y * scale, rev(pos + d$y * scale))
            poly_y <- c(d$x, rev(d$x))
            fill <- if (!is.null(fill_vec)) {
                fill_vec[idx][1]
            } else {
                constant <- params$fill %||% params$col
                if (is.null(constant)) {
                    "grey70"
                } else {
                    rep(constant, length.out = n)[idx][1]
                }
            }
            fill <- scales::alpha(fill, alpha)
            do.call(graphics::polygon,
                    list(x = poly_x, y = poly_y, col = fill,
                         border = border))
            drawn <- c(drawn, g)
        }
        invisible(NULL)
    }

    add_layer(plot, ly, "violin layer")
}
