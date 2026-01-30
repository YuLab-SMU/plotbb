##' significance layer
##'
##' @title bb_signif
##' @rdname layer
##' @param mapping aesthetic mapping
##' @param data layer data
##' @param ... addition parameter for the layer
##' @return A modified bbplot object
##' @importFrom graphics segments
##' @importFrom graphics text
##' @export
##' @author Guangchuang Yu
bb_signif <- function(mapping = NULL, data = NULL, ...) {
    build_layer(mapping, data, ..., layer = ly_signif)
}

ly_signif <- function(plot, mapping = NULL, data = NULL, ...) {
    data <- bb_data(plot, data)
    mapping <- bb_mapping(plot, mapping)
    
    params <- list(...)
    
    tip_length <- params$tip_length %||% 0.03
    params$tip_length <- NULL
    text_gap <- params$text_gap %||% 0.01
    params$text_gap <- NULL

    comparisons <- params$comparisons %||% NULL
    params$comparisons <- NULL

    y_position_param <- params$y_position %||% NULL
    params$y_position <- NULL

    annotations_param <- params$annotations %||% NULL
    params$annotations <- NULL

    step_increase <- params$step_increase %||% 0.05
    params$step_increase <- NULL

    test <- params$test %||% stats::wilcox.test
    params$test <- NULL

    test_args <- params$test_args %||% list()
    params$test_args <- NULL

    map_signif_level <- params$map_signif_level %||% TRUE
    params$map_signif_level <- NULL
    
    ly <- function() {
        usr <- par("usr")
        y_range <- usr[4] - usr[3]
        tip_h <- y_range * tip_length

        text_gap_abs <- y_range * text_gap

        if (!is.null(comparisons)) {
            x_raw <- bb_eval_or_fallback(mapping, data, "x", xvar(mapping))
            y_raw <- bb_eval_or_fallback(mapping, data, "y", yvar(mapping))
            if (is.null(x_raw) || is.null(y_raw)) return(invisible(NULL))

            if (!is.list(comparisons)) comparisons <- list(comparisons)
            ncomp <- length(comparisons)
            if (ncomp == 0) return(invisible(NULL))

            if (is.numeric(x_raw)) {
                x_chr <- as.character(x_raw)
                lev <- NULL
                get_pos <- function(x) as.numeric(x)
                get_idx <- function(x) x_raw == as.numeric(x)
            } else {
                x_chr <- as.character(x_raw)
                lev <- bb_discrete_levels(x_raw)
                get_pos <- function(x) match(as.character(x), lev)
                get_idx <- function(x) x_chr == as.character(x)
            }

            if (is.null(y_position_param)) {
                y_span <- diff(range(y_raw, na.rm = TRUE))
                if (!is.finite(y_span) || y_span == 0) y_span <- 1
                y_top <- max(y_raw, na.rm = TRUE)
                y_position <- y_top + y_span * step_increase * seq_len(ncomp)
            } else if (length(y_position_param) == 1) {
                y_position <- rep(y_position_param, ncomp)
            } else {
                y_position <- y_position_param
            }

            xmin <- numeric(0)
            xmax <- numeric(0)
            y <- numeric(0)
            label <- character(0)

            resolve_test <- function(x) {
                if (is.function(x)) return(x)
                if (is.character(x)) {
                    x <- match.arg(x, c("t.test", "wilcox.test"))
                    if (identical(x, "t.test")) return(stats::t.test)
                    return(stats::wilcox.test)
                }
                stats::wilcox.test
            }
            test_fun <- resolve_test(test)
    if (identical(test_fun, stats::wilcox.test) && is.null(test_args$exact)) {
        test_args$exact <- FALSE
    }

    segments_params <- params
    segments_params$cex <- NULL
    segments_params$font <- NULL
    segments_params$family <- NULL

    text_params <- list(col = params$col, cex = params$cex, font = params$font, family = params$family)
    text_params <- text_params[!vapply(text_params, is.null, logical(1))]

            signif_label <- function(p) {
                if (is.null(p) || !is.finite(p)) return(NA_character_)
                if (!isTRUE(map_signif_level)) return(sprintf("p=%.3g", p))
                if (p <= 0.001) return("***")
                if (p <= 0.01) return("**")
                if (p <= 0.05) return("*")
                "ns"
            }

            for (i in seq_len(ncomp)) {
                comp <- comparisons[[i]]
                if (length(comp) < 2) next
                x1 <- comp[[1]]
                x2 <- comp[[2]]

                p1 <- get_pos(x1)
                p2 <- get_pos(x2)
                if (is.na(p1) || is.na(p2)) next

                idx1 <- get_idx(x1)
                idx2 <- get_idx(x2)
                y1 <- y_raw[idx1]
                y2 <- y_raw[idx2]

                pval <- NA_real_
                if (length(y1) && length(y2)) {
                    pval <- tryCatch(
                        do.call(test_fun, c(list(x = y1, y = y2), test_args))$p.value,
                        error = function(e) NA_real_
                    )
                }

                ann <- if (!is.null(annotations_param)) {
                    if (length(annotations_param) >= i) annotations_param[[i]] else NA_character_
                } else {
                    signif_label(pval)
                }

                xmin <- c(xmin, min(p1, p2))
                xmax <- c(xmax, max(p1, p2))
                y <- c(y, y_position[[i]])
                label <- c(label, ann)
            }

            if (length(xmin) == 0) return(invisible(NULL))

            do.call(graphics::segments, c(list(x0 = xmin, y0 = y, x1 = xmax, y1 = y), segments_params))
            do.call(graphics::segments, c(list(x0 = xmin, y0 = y, x1 = xmin, y1 = y - tip_h), segments_params))
            do.call(graphics::segments, c(list(x0 = xmax, y0 = y, x1 = xmax, y1 = y - tip_h), segments_params))

            if (length(label) && !all(is.na(label))) {
                x_mid <- (xmin + xmax) / 2
                do.call(graphics::text, c(list(x = x_mid, y = y + text_gap_abs, labels = label, pos = 3, offset = 0), text_params))
            }

            return(invisible(NULL))
        }

        xmin <- bb_eval_or_fallback(mapping, data, "xmin", NULL)
        xmax <- bb_eval_or_fallback(mapping, data, "xmax", NULL)
        y <- bb_eval_or_fallback(mapping, data, "y_position", NULL) %||% bb_eval_or_fallback(mapping, data, "y", NULL)
        label <- bb_eval_or_fallback(mapping, data, "label", NULL) %||% bb_eval_or_fallback(mapping, data, "annotations", NULL)
        if (is.null(xmin) || is.null(xmax) || is.null(y)) return(invisible(NULL))

        do.call(graphics::segments, c(list(x0 = xmin, y0 = y, x1 = xmax, y1 = y), segments_params))
        do.call(graphics::segments, c(list(x0 = xmin, y0 = y, x1 = xmin, y1 = y - tip_h), segments_params))
        do.call(graphics::segments, c(list(x0 = xmax, y0 = y, x1 = xmax, y1 = y - tip_h), segments_params))

        if (!is.null(label)) {
            x_mid <- (xmin + xmax) / 2
            do.call(graphics::text, c(list(x = x_mid, y = y + text_gap_abs, labels = label, pos = 3, offset = 0), text_params))
        }
    }
    
    add_layer(plot, ly, "signif layer")
}
