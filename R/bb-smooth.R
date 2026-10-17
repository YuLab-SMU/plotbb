##' smooth layer
##'
##' `bb_smooth()` fits an explicit model per group and draws the fitted curve,
##' optionally with a confidence band. The fit method is never implicit: choose
##' `"lm"` or `"loess"` (the default), pass extra fitting arguments through
##' `method.args`, and control the band with `se` and `level`.
##'
##' `x` and `y` are mapped from the layer's data, `group` splits the fit, and a
##' mapped `col` colours each curve. The `formula` response must be `y`; transform
##' the data or the mapping rather than the response, so the band stays on the
##' drawn scale. The confidence band uses `fill` (or the
##' curve colour at low alpha). Small samples that cannot support the requested
##' fit are reported with a warning and fall back to a linear fit; a group with
##' fewer than two points is skipped. Fits are computed when the layer is added,
##' so repeated draws do not re-fit.
##'
##' @title bb_smooth
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... additional parameters: `method` ("loess" or "lm"), `formula`
##'   (default `y ~ x`), `se`, `level`, `n` (prediction grid size),
##'   `method.args`, `alpha`, `col`/`fill`/`lty`/`lwd`.
##' @return A modified bbplot object
##' @importFrom graphics polygon lines
##' @export
##' @author Guangchuang Yu
bb_smooth <- function(mapping = NULL, data = NULL, inherit = TRUE, ...) {
    build_layer(mapping, data, inherit = inherit, ..., layer = ly_smooth)
}

bb_smooth_curves <- function(x, y, grp, method, formula, se, level,
                                  n_grid, method_args) {
    curves <- list()
    for (g in sort(unique(grp))) {
        idx <- grp == g
        gx <- x[idx]
        gy <- y[idx]
        if (length(gx) < 2L) {
            bb_warn("`bb_smooth()` skipped group '", g,
                    "': it needs at least two points.")
            next
        }
        fit_data <- data.frame(x = gx, y = gy)
        grid <- seq(min(gx), max(gx), length.out = n_grid)
        newdata <- data.frame(x = grid)
        fit <- NULL
        if (method == "loess") {
            if (length(unique(gx)) < 4L) {
                bb_warn("`bb_smooth()` group '", g, "' has too few distinct ",
                        "x values for loess; using a linear fit.")
                method_g <- "lm"
            } else if (length(gx) < 6L) {
                bb_warn("`bb_smooth()` group '", g, "' has too few points ",
                        "for loess; using a linear fit.")
                method_g <- "lm"
            } else {
                method_g <- "loess"
            }
            if (method_g == "loess") {
                fit <- suppressWarnings(tryCatch(
                    do.call(stats::loess,
                            c(list(formula = formula, data = fit_data),
                              method_args)),
                    error = function(e) NULL
                ))
                if (is.null(fit)) {
                    bb_warn("`bb_smooth()` could not fit loess for group '",
                            g, "'; using a linear fit.")
                }
            }
        } else {
            method_g <- "lm"
        }
        if (is.null(fit)) {
            fit <- tryCatch(
                do.call(stats::lm,
                        c(list(formula = formula, data = fit_data),
                          method_args)),
                error = function(e) NULL
            )
        }
        if (is.null(fit)) {
            bb_warn("`bb_smooth()` could not fit group '", g, "; skipped.")
            next
        }

        pred <- tryCatch({
            if (inherits(fit, "lm")) {
                stats::predict(fit, newdata = newdata,
                        interval = if (isTRUE(se)) "confidence" else "none",
                        level = level)
            } else {
                stats::predict(fit, newdata = newdata, se = isTRUE(se))
            }
        }, error = function(e) NULL)
        if (is.null(pred)) {
            bb_warn("`bb_smooth()` could not predict group '", g,
                    "; skipped.")
            next
        }
        if (inherits(fit, "lm") && isTRUE(se)) {
            lower <- pred[, "lwr"]
            upper <- pred[, "upr"]
            fitted <- pred[, "fit"]
        } else if (inherits(fit, "loess") && isTRUE(se)) {
            fitted <- pred$fit
            df <- max(1, fit$n - fit$trace.hat)
            tq <- stats::qt(1 - (1 - level) / 2, df)
            lower <- fitted - tq * pred$se.fit
            upper <- fitted + tq * pred$se.fit
        } else {
            fitted <- as.numeric(pred)
            lower <- upper <- NULL
        }

        curves[[length(curves) + 1L]] <- list(
            group = g,
            grid = grid,
            fitted = fitted,
            lower = lower,
            upper = upper
        )
    }


    curves
}

ly_smooth <- function(plot, mapping = NULL, data = NULL, inherit = TRUE, ...) {
    data <- bb_layer_data(plot, data, mapping)
    params <- list(...)
    mapping <- bb_layer_aesthetics(plot, mapping, inherit, params)

    method <- match.arg(params$method %||% "loess", c("loess", "lm"))
    formula <- params$formula %||% stats::as.formula("y ~ x")
    if (!inherits(formula, "formula")) {
        bb_abort("`formula` in `bb_smooth()` must be a formula.")
    }
    se <- params$se %||% TRUE
    level <- params$level %||% 0.95
    n_grid <- params$n %||% 80L
    method_args <- params$method.args %||% list()
    alpha <- params$alpha %||% 0.2

    if (!is.logical(se) || length(se) != 1L || is.na(se)) {
        bb_abort("`se` in `bb_smooth()` must be TRUE or FALSE.")
    }
    if (!is.numeric(level) || length(level) != 1L || !is.finite(level) ||
        level <= 0 || level >= 1) {
        bb_abort("`level` in `bb_smooth()` must be a number between 0 and 1.")
    }
    if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
        alpha < 0 || alpha > 1) {
        bb_abort("`alpha` in `bb_smooth()` must be a single number between 0 ",
                 "and 1.")
    }
    if (!is.numeric(n_grid) || length(n_grid) != 1L || n_grid < 2 ||
        n_grid != as.integer(n_grid)) {
        bb_abort("`n` in `bb_smooth()` must be an integer of at least 2.")
    }
    n_grid <- as.integer(n_grid)
    if (!is.list(method_args)) {
        bb_abort("`method.args` in `bb_smooth()` must be a list.")
    }

    x <- eval_mapping(mapping, "x", data)
    y <- eval_mapping(mapping, "y", data)
    if (is.null(x) || is.null(y)) {
        bb_abort("`bb_smooth()` requires `x` and `y` mappings.")
    }
    if (!is.numeric(x) || !is.numeric(y)) {
        bb_abort("`bb_smooth()` requires numeric `x` and `y`.")
    }
    n <- max(length(x), length(y))
    bb_check_length(x, n, "x", "bb_smooth")
    bb_check_length(y, n, "y", "bb_smooth")
    x <- rep(x, length.out = n)
    y <- rep(y, length.out = n)
    grp <- bb_group_values(mapping, data)
    grp <- if (is.null(grp)) rep("all", n) else
        as.character(rep(grp, length.out = n))
    ok <- is.finite(x) & is.finite(y)
    if (!any(ok)) {
        bb_warn("`bb_smooth()` has no finite `x`/`y` values to draw; skipped.")
        return(invisible(NULL))
    }
    keep <- ok & !is.na(grp)
    x <- x[keep]
    y <- y[keep]
    grp <- grp[keep]
    if (!length(x)) {
        bb_warn("`bb_smooth()` has no rows with a finite group to draw; ",
                "skipped.")
        return(invisible(NULL))
    }

    col_all <- if (!is.null(mapping$col)) {
        rep(bb_col(mapping, data, plot = plot), length.out = n)[keep]
    } else if (!is.null(params$col)) {
        rep(params$col, length.out = n)[keep]
    } else {
        rep("steelblue", length(x))
    }
    fill_all <- if (!is.null(mapping$fill)) {
        rep(bb_fill(mapping, data, plot = plot), length.out = n)[keep]
    } else if (!is.null(params$fill)) {
        rep(params$fill, length.out = n)[keep]
    } else {
        col_all
    }
    colour_of <- function(idx) col_all[idx][1]
    fill_of <- function(idx, curve_col) {
        if (!is.null(mapping$fill) || !is.null(params$fill)) {
            fill_all[idx][1]
        } else {
            curve_col
        }
    }

    formula_vars <- all.vars(formula)
    if (!all(formula_vars %in% c("x", "y"))) {
        bb_abort("`formula` in `bb_smooth()` may only use the mapped `x` and ",
                 "`y`; got: ", paste(formula_vars, collapse = ", "), ".")
    }
    if (!is.symbol(formula[[2]]) ||
        !identical(as.character(formula[[2]]), "y")) {
        bb_abort("the response of `formula` in `bb_smooth()` must be `y`; ",
                 "transform the data or the mapping instead of the response.")
    }

    curves <- bb_smooth_curves(x, y, grp, method, formula, se, level,
                                n_grid, method_args)
    if (!length(curves)) return(invisible(NULL))
    for (i in seq_along(curves)) {
        idx <- grp == curves[[i]]$group
        curves[[i]]$col <- colour_of(idx)
        curves[[i]]$fill <- fill_of(idx, curves[[i]]$col)
    }

    if (!length(curves)) return(invisible(NULL))

    ## Cache the drawn range on the plot environment, indexed by the layer's
    ## position among the grammar layers, so computing the canvas does not fit
    ## the model a second time.
    band_x <- range(unlist(lapply(curves, `[[`, "grid")))
    band_y <- range(unlist(lapply(curves, function(curve) {
        c(curve$fitted, curve$lower, curve$upper)
    })))
    layer_index <- sum(vapply(plot$adds, inherits, logical(1), "bb_layer"))
    cached <- if (exists("bb_layer_ranges", envir = plot$env,
                         inherits = FALSE)) {
        get("bb_layer_ranges", envir = plot$env, inherits = FALSE)
    } else {
        list()
    }
    cached[[layer_index]] <- list(x = band_x, y = band_y)
    assign("bb_layer_ranges", cached, envir = plot$env)

    ly <- function() {
        for (curve in curves) {
            if (!is.null(curve$lower) && !is.null(curve$upper)) {
                poly_x <- c(curve$grid, rev(curve$grid))
                poly_y <- c(curve$lower, rev(curve$upper))
                do.call(graphics::polygon,
                        list(x = poly_x, y = poly_y,
                             col = scales::alpha(curve$fill, alpha),
                             border = NA))
            }
            do.call(graphics::lines,
                    list(x = curve$grid, y = curve$fitted,
                         col = curve$col,
                         lty = params$lty %||% 1,
                         lwd = params$lwd %||% 1))
        }
        invisible(NULL)
    }

    add_layer(plot, ly, "smooth layer")
}
