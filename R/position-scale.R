##' Positional scales
##'
##' These scales control the x and y axes: user limits, breaks, labels, a log10
##' transform (`bb_scale_x_log10()`, `bb_scale_y_log10()`) and date axes
##' (`bb_scale_x_date()`, `bb_scale_y_date()`). They are separate from the
##' aesthetic scales (`col`, `fill`, `pch`, `lty`, `cex`).
##'
##' `limits` sets the final axis range; values outside it are clipped by the
##' device. A log scale requires positive limits. Date scales use
##' `graphics::axis.Date()` and take Date limits/breaks.
##'
##' @title bb_scale_x_continuous
##' @param limits optional two limits for the axis.
##' @param breaks optional break positions; `NULL` uses the base axis.
##' @param labels optional labels for the breaks; requires `breaks`.
##' @param trans axis transform: `"identity"` or `"log10"`.
##' @param format date format for a date axis.
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_scale_x_continuous <- function(limits = NULL, breaks = NULL,
                                  labels = NULL, trans = c("identity", "log10")) {
    bb_position_scale("x", "continuous", limits = limits, breaks = breaks,
                      labels = labels, trans = match.arg(trans))
}

##' @rdname bb_scale_x_continuous
##' @export
bb_scale_y_continuous <- function(limits = NULL, breaks = NULL,
                                  labels = NULL, trans = c("identity", "log10")) {
    bb_position_scale("y", "continuous", limits = limits, breaks = breaks,
                      labels = labels, trans = match.arg(trans))
}

##' @rdname bb_scale_x_continuous
##' @export
bb_scale_x_log10 <- function(limits = NULL, breaks = NULL, labels = NULL) {
    bb_position_scale("x", "continuous", limits = limits, breaks = breaks,
                      labels = labels, trans = "log10")
}

##' @rdname bb_scale_x_continuous
##' @export
bb_scale_y_log10 <- function(limits = NULL, breaks = NULL, labels = NULL) {
    bb_position_scale("y", "continuous", limits = limits, breaks = breaks,
                      labels = labels, trans = "log10")
}

##' @rdname bb_scale_x_continuous
##' @export
bb_scale_x_date <- function(limits = NULL, breaks = NULL, labels = NULL,
                            format = "%Y-%m-%d") {
    bb_position_scale("x", "date", limits = limits, breaks = breaks,
                      labels = labels, format = format)
}

##' @rdname bb_scale_x_continuous
##' @export
bb_scale_y_date <- function(limits = NULL, breaks = NULL, labels = NULL,
                            format = "%Y-%m-%d") {
    bb_position_scale("y", "date", limits = limits, breaks = breaks,
                      labels = labels, format = format)
}

bb_position_scale <- function(aesthetic, type, limits = NULL, breaks = NULL,
                              labels = NULL, trans = "identity",
                              format = "%Y-%m-%d") {
    if (!is.null(limits)) {
        if (type == "date") {
            limits <- as.Date(limits)
        } else if (!is.numeric(limits) || length(limits) != 2L ||
                   any(!is.finite(limits))) {
            bb_abort("`limits` in a positional scale must be two finite ",
                     "numbers.")
        }
        limits <- sort(limits)
    }
    if (type == "continuous" && identical(trans, "log10")) {
        if (!is.null(limits) && any(limits <= 0)) {
            bb_abort("a log10 positional scale requires positive `limits`.")
        }
    }
    if (!is.null(labels) && is.null(breaks)) {
        bb_abort("`labels` in a positional scale requires `breaks`.")
    }
    if (!is.null(labels) && !is.null(breaks) &&
        length(labels) != length(breaks)) {
        bb_abort("`labels` and `breaks` must have the same length.")
    }
    structure(
        list(
            aesthetic = aesthetic,
            type = type,
            limits = limits,
            breaks = breaks,
            labels = labels,
            trans = trans,
            format = format
        ),
        class = "bb_position_scale"
    )
}

##' @method bbplot_add bb_position_scale
##' @export
bbplot_add.bb_position_scale <- function(object, plot) {
    scales <- plot$position_scales %||% list()
    scales[[object$aesthetic]] <- object
    plot$position_scales <- scales
    plot
}

bb_position_scales <- function(plot) {
    plot$position_scales %||% list()
}

bb_position_scale_summary <- function(plot) {
    scales <- bb_position_scales(plot)
    lapply(scales, function(scale) {
        list(type = scale$type, limits = scale$limits,
             breaks = scale$breaks, labels = scale$labels,
             trans = scale$trans, format = scale$format)
    })
}

bb_apply_position_limits <- function(plot, limits) {
    scales <- bb_position_scales(plot)
    for (aesthetic in c("x", "y")) {
        scale <- scales[[aesthetic]]
        if (is.null(scale) || is.null(scale$limits)) next
        limits[[paste0(aesthetic, "lim")]] <- scale$limits
    }
    limits
}

bb_position_log <- function(plot) {
    scales <- bb_position_scales(plot)
    x <- !is.null(scales$x) && identical(scales$x$trans, "log10")
    y <- !is.null(scales$y) && identical(scales$y$trans, "log10")
    if (x && y) "xy" else if (x) "x" else if (y) "y" else ""
}

bb_position_axis_flags <- function(plot) {
    scales <- bb_position_scales(plot)
    list(
        log = bb_position_log(plot),
        xaxt = if (!is.null(scales$x)) "n" else NULL,
        yaxt = if (!is.null(scales$y)) "n" else NULL
    )
}

bb_draw_position_axes <- function(plot) {
    scales <- bb_position_scales(plot)
    usr <- graphics::par("usr")
    draw_axis <- function(side, scale, range) {
        if (identical(scale$type, "date")) {
            at <- scale$breaks
            if (is.null(at)) at <- graphics::axTicks(side)
            at <- as.Date(at, origin = "1970-01-01")
            args <- list(side = side,
                         x = as.Date(range, origin = "1970-01-01"),
                         at = at, format = scale$format)
            if (!is.null(scale$labels)) args$labels <- scale$labels
            do.call(graphics::axis.Date, args)
        } else {
            args <- list(side = side)
            if (!is.null(scale$breaks)) args$at <- scale$breaks
            if (!is.null(scale$labels)) args$labels <- scale$labels
            do.call(graphics::axis, args)
        }
    }
    if (!is.null(scales$x)) draw_axis(1, scales$x, usr[1:2])
    if (!is.null(scales$y)) draw_axis(2, scales$y, usr[3:4])
    invisible(NULL)
}
