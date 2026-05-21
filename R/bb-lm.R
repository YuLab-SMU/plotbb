##' @rdname layer
##' @param se display confidence interval around smooth? (TRUE by default, see ggplot2)
##' @param level level of confidence interval to use (0.95 by default)
##' @export
bb_lm <- function(mapping = NULL, data = NULL, se = TRUE, level = 0.95, ...) {
    build_layer(mapping, data, se = se, level = level, ...,  layer = ly_lm)
}

##' @importFrom graphics segments polygon
##' @importFrom graphics par
ly_lm <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    layer_name <- "linear regression layer"
    formula <- stats::as.formula(paste(yvar(mapping), '~', xvar(mapping)))
    params <- list(...)
    se <- params$se %||% TRUE
    level <- params$level %||% 0.95

    if (is.null(mapping$group)) {
        d2 <- lm_data(formula, data, se = se, level = level)
        if (is.null(d2)) return(plot)
        ly <- function() {
            if (se && !is.null(d2$ci)) {
                ci <- d2$ci
                fill <- params$fill %||% "grey60"
                alpha <- params$alpha %||% 0.4
                graphics::polygon(c(ci$x, rev(ci$x)), c(ci$lwr, rev(ci$upr)), 
                                  col = scales::alpha(fill, alpha), border = NA)
            }
            args <- params
            args$se <- NULL
            args$level <- NULL
            args$fill <- NULL
            args$alpha <- NULL
            
            args$x0 <- d2$x0
            args$y0 <- d2$y0
            args$x1 <- d2$x1
            args$y1 <- d2$y1
            do.call(graphics::segments, args)
        }
        plot <- add_layer(plot, ly, layer_name)
        return(plot)
    }

    grp <- eval_mapping(mapping, 'group', data)
    ugrp <- unique(grp)

    ly <- function() {
        cols <- NULL
        if (is.null(params$col) && !is.null(mapping$col)) {
            col_vec <- bb_col(mapping, data, plot = plot)
            if (!is.null(col_vec)) {
                col_map <- tapply(col_vec, as.character(grp), function(z) z[[1]])
                cols <- unname(col_map[as.character(ugrp)])
            }
        }

        d2_list <- lapply(ugrp, function(g) {
            d <- data[grp == g, ]
            lm_data(formula, d, se = se, level = level)
        })
        keep <- !vapply(d2_list, is.null, logical(1))
        if (!any(keep)) return(invisible(NULL))
        
        d2_list <- d2_list[keep]
        if (!is.null(cols)) cols <- cols[keep]

        for (i in seq_along(d2_list)) {
            d2 <- d2_list[[i]]
            c_col <- if (!is.null(cols)) cols[i] else params$col %||% "black"
            
            if (se && !is.null(d2$ci)) {
                ci <- d2$ci
                fill <- params$fill %||% c_col
                alpha <- params$alpha %||% 0.4
                graphics::polygon(c(ci$x, rev(ci$x)), c(ci$lwr, rev(ci$upr)), 
                                  col = scales::alpha(fill, alpha), border = NA)
            }
            
            args <- params
            args$se <- NULL
            args$level <- NULL
            args$fill <- NULL
            args$alpha <- NULL
            
            args$x0 <- d2$x0
            args$y0 <- d2$y0
            args$x1 <- d2$x1
            args$y1 <- d2$y1
            args$col <- c_col
            
            do.call(graphics::segments, args)
        }
    }

    plot <- add_layer(plot, ly, layer_name)
    return(plot)
}

lm_data <- function(formula, data, se = FALSE, level = 0.95) {
    s <- tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
    if (is.null(s)) return(NULL)
    
    term_labels <- attr(stats::terms(s), "term.labels")
    if (length(term_labels) != 1) return(NULL)

    mf <- stats::model.frame(s)
    x_name <- term_labels[[1]]
    if (!x_name %in% names(mf)) return(NULL)

    x <- mf[[x_name]]
    if (!is.numeric(x)) return(NULL)
    x <- x[is.finite(x)]
    if (length(x) == 0) return(NULL)

    co <- stats::coef(s)
    if (length(co) < 2 || any(!is.finite(co[1:2]))) return(NULL)

    x0 <- min(x)
    x1 <- max(x)
    y0 <- co[[1]] + x0 * co[[2]]
    y1 <- co[[1]] + x1 * co[[2]]
    
    res <- list(x0 = x0, x1 = x1, y0 = y0, y1 = y1)
    
    if (se && stats::df.residual(s) > 0) {
        x_seq <- seq(x0, x1, length.out = 80)
        nd <- data.frame(x_seq)
        names(nd) <- x_name
        pred <- tryCatch(stats::predict(s, newdata = nd, interval = "confidence", level = level), error = function(e) NULL)
        if (!is.null(pred)) {
            res$ci <- data.frame(x = x_seq, fit = pred[, "fit"], lwr = pred[, "lwr"], upr = pred[, "upr"])
        }
    }
    
    res
}

lm_env <- function(data) {
    lm_env <- new.env()
    lm_env$x0 <- data$x0
    lm_env$x1 <- data$x1
    lm_env$y0 <- data$y0
    lm_env$y1 <- data$y1
    if (!is.null(data$col))
        lm_env$col <- data$col
    return(lm_env)
}

## .ly_lm <- function(formula, data, col = par("fg"), lty = par("lty"), lwd = par("lwd")) {
##     s <- stats::lm(formula, data = data)
##     x <- as.character(formula)[3]

##     x0 <- min(data[[x]])
##     x1 <- max(data[[x]])
##     y0 <- s$coefficients[1] + x0 * s$coefficients[2]
##     y1 <- s$coefficients[1] + x1 * s$coefficients[2]
##     lm_env <- new.env()
##     lm_env$x0 <- x0
##     lm_env$x1 <- x1
##     lm_env$y0 <- y0
##     lm_env$y1 <- y1
##     lm_env$col <- col
##     lm_env$lty <- lty
##     lm_env$lwd <- lwd
##     ly <- function() segments(x0 = x0, y0 = y0,
##                               x1 = x1, y1 = y1,
##                               col = col, lty = lty,
##                               lwd = lwd)
##     with_env(ly, lm_env)
## }
