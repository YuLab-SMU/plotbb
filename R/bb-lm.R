##' @rdname layer
##' @export
bb_lm <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ...,  layer = ly_lm)
}

##' @importFrom graphics segments
##' @importFrom graphics par
ly_lm <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)

    layer_name <- "linear regression layer"
    formula <- stats::as.formula(paste(yvar(mapping), '~', xvar(mapping)))
    if (is.null(mapping$group)) {
        d2 <- lm_data(formula, data)
        if (is.null(d2)) return(plot)
        ly <- function() {
            segments(x0 = d2$x0,
                     y0 = d2$y0,
                     x1 = d2$x1,
                     y1 = d2$y1, ...)
        }
        plot <- add_layer(plot, ly, layer_name)
        return(plot)
    }

    params <- list(...)
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
            lm_data(formula, d)
        })
        keep <- !vapply(d2_list, is.null, logical(1))
        if (!any(keep)) return(invisible(NULL))
        d2 <- do.call('rbind', d2_list[keep])
        if (!is.null(cols)) cols <- cols[keep]

        if (is.null(cols)) {
            segments(x0 = d2$x0,
                     y0 = d2$y0,
                     x1 = d2$x1,
                     y1 = d2$y1, ...)
        } else {
            d2$col <- cols
            segments(x0 = d2$x0,
                     y0 = d2$y0,
                     x1 = d2$x1,
                     y1 = d2$y1,
                     col = d2$col, ...)
        }    
    }

    #with_env(ly, lm_env(d2))
    plot <- add_layer(plot, ly, layer_name)
    return(plot)
}


lm_data <- function(formula, data) {
    s <- stats::lm(formula, data = data)
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
    data.frame(x0 = x0, x1 = x1,
               y0 = y0, y1 = y1)
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
