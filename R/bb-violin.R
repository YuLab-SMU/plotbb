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
##' contributes the group positions and the numeric values to the ranges. With
##' `orientation = "horizontal"`, map `x` to the numeric values and `y` to the
##' groups; the default vertical orientation is the other way round. A group needs
##' at least two finite values and a non-zero spread; other groups are skipped with
##' a warning. Rows with a non-finite value are dropped.
##'
##' @title bb_violin
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param orientation `"vertical"` (default; `x` groups, `y` values) or
##'   `"horizontal"` (`x` values, `y` groups).
##' @param ... additional parameters: `width` (default 0.9), `adjust` (1),
##'   `bw`, `kernel`, `trim`, `n`, `alpha`, `border`, `fill`/`col`.
##' @return A modified bbplot object
##' @importFrom graphics polygon
##' @export
##' @author Guangchuang Yu
bb_violin <- function(mapping = NULL, data = NULL, inherit = TRUE,
                      orientation = c("vertical", "horizontal"), ...) {
    orientation <- match.arg(orientation)
    build_layer(mapping, data, inherit = inherit, orientation = orientation,
                ..., layer = ly_violin)
}

ly_violin <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)

    orientation <- params$orientation %||% "vertical"
    if (!orientation %in% c("vertical", "horizontal")) {
        bb_abort("`orientation` in `bb_violin()` must be 'vertical' or ",
                 "'horizontal'.")
    }

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
        if (identical(orientation, "horizontal")) {
            value <- eval_mapping(mapping, "x", data)
            group_var <- eval_mapping(mapping, "y", data)
            value_name <- "x"
            group_name <- "y"
        } else {
            value <- eval_mapping(mapping, "y", data)
            group_var <- eval_mapping(mapping, "x", data)
            value_name <- "y"
            group_name <- "x"
        }
        if (is.null(value)) {
            bb_abort("`bb_violin()` requires a numeric `", value_name,
                     "` mapping.")
        }
        if (!is.numeric(value)) {
            bb_abort("`bb_violin()` requires a numeric `", value_name,
                     "` mapping.")
        }
        n <- max(length(value), length(group_var) %||% 0L)
        bb_check_length(value, n, value_name, "bb_violin")
        if (!is.null(group_var)) {
            bb_check_length(group_var, n, group_name, "bb_violin")
        }
        value <- rep(value, length.out = n)
        group_var <- if (is.null(group_var)) NULL else
            rep(group_var, length.out = n)

        ok <- is.finite(value)
        if (!any(ok)) {
            bb_warn("`bb_violin()` has no finite `", value_name,
                    "` values to draw; skipped.")
            return(invisible(NULL))
        }

        grp <- bb_group_values(mapping, data, group_var)
        pos <- bb_violin_positions(group_var, grp, n)
        groups <- pos$groups
        positions <- pos$positions

        ok <- ok & is.finite(positions) & !is.na(groups)
        if (!any(ok)) {
            bb_warn("`bb_violin()` has no finite group positions to draw; ",
                    "skipped.")
            return(invisible(NULL))
        }

        fill_vec <- NULL
        if (!is.null(mapping$fill)) {
            fill_vec <- bb_fill(mapping, data, plot = plot)
        } else if (!is.null(mapping$col)) {
            fill_vec <- bb_col(mapping, data, plot = plot)
        }
        fill_vec <- if (is.null(fill_vec)) NULL else
            rep(fill_vec, length.out = n)

        for (g in sort(unique(groups[ok]))) {
            idx <- ok & groups == g
            values <- value[idx]
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
            if (identical(orientation, "horizontal")) {
                poly_x <- c(d$x, rev(d$x))
                poly_y <- c(pos - d$y * scale, rev(pos + d$y * scale))
            } else {
                poly_x <- c(pos - d$y * scale, rev(pos + d$y * scale))
                poly_y <- c(d$x, rev(d$x))
            }
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
        }
        invisible(NULL)
    }

    add_layer(plot, ly, "violin layer")
}

## One position rule for drawing and for the canvas. A factor/character x keeps
## its levels as positions; a numeric x is either the position itself (when it is
## the only grouping) or a group index when group/col/fill decides the groups.
bb_violin_positions <- function(x, group, n) {
    if (is.null(x)) {
        groups <- if (is.null(group)) rep("all", n) else
            as.character(rep(group, length.out = n))
        labels <- if (is.null(group)) NULL else sort(unique(groups))
        positions <- if (is.null(group)) rep(1, n) else match(groups, labels)
        return(list(groups = groups, positions = positions, labels = labels))
    }
    if (is.factor(x) || is.character(x)) {
        labels <- sort(unique(as.character(x)))
        groups <- if (is.null(group)) as.character(x) else
            as.character(rep(group, length.out = n))
        positions <- match(as.character(x), labels)
        return(list(groups = groups, positions = positions, labels = labels))
    }
    if (is.null(group)) {
        groups <- as.character(x)
        positions <- as.numeric(x)
        return(list(groups = groups, positions = positions,
                    labels = sort(unique(positions))))
    }
    groups <- as.character(rep(group, length.out = n))
    labels <- sort(unique(groups))
    list(groups = groups, positions = match(groups, labels), labels = labels)
}
