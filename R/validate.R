## Input validation helpers.
##
## Layers are evaluated when the plot is *built* (i.e. when the mapping is
## resolved and its values checked). Failures therefore show up where the user
## wrote the layer, instead of failing during drawing with an obscure
## base-graphics message.

bb_abort <- function(...) {
    rlang::abort(paste0(...), call = NULL)
}

bb_layer_label <- function(mapping, default) {
    mapping[["layer"]] %||% default
}

bb_layer_data <- function(plot, data, mapping = NULL) {
    if (is.null(data) && is.null(plot$data)) {
        bb_abort(bb_layer_label(mapping, "this layer"),
                 " requires data: supply it in `bbplot()` or through the ",
                 "layer's `data` argument.")
    }
    label <- bb_layer_label(mapping, "this layer")
    data <- bb_as_data_frame(data, label)
    ly_data <- bb_as_data_frame(bb_data(plot, data), label)
    if (is.null(ly_data)) {
        if (is.null(plot$data) && !is.null(data)) {
            bb_abort(bb_layer_label(mapping, "the layer"),
                     " requires data: supply it in bbplot() or via the layer's `data` argument.")
        }
        return(NULL)
    }
    ly_data
}

## Coerce layer data to a data frame, or explain why that is impossible.
##
## A layer's `data` is a first-class input (ranges, scales and panels all use
## it), so a silently `NULL` frame would hide the mistake behind "no data to
## draw" much later.
bb_as_data_frame <- function(data, label = "this layer") {
    if (is.null(data)) return(NULL)
    if (is.data.frame(data)) return(data)
    out <- tryCatch(as.data.frame(data), error = function(e) NULL)
    if (is.null(out)) {
        bb_abort("`data` for ", label, " must be a data frame, or something ",
                 "that can be converted into one; got <", class(data)[[1]],
                 ">.")
    }
    out
}

## TRUE for rows whose numeric coordinates are all finite.
##
## Handled positionally on purpose: `list(x, y)` drops NULL entries, which
## would silently skip the check for a missing coordinate.
bb_valid_rows <- function(x, y) {
    n <- max(length(x) %||% 0L, length(y) %||% 0L)
    ok <- rep(TRUE, n)
    for (v in list(x, y)) {
        if (is.null(v)) next
        if (!is.numeric(v)) {
            ## a non-numeric coordinate cannot be placed or range-checked
            return(rep(FALSE, n))
        }
        ok <- ok & is.finite(rep(v, length.out = n))
    }
    ok
}

bb_check_length <- function(value, n, aesthetic, layer) {
    if (is.null(value)) return(invisible(NULL))
    len <- length(value)
    if (len == n || len == 1L) return(invisible(NULL))
    bb_abort("`", aesthetic, "` in `", layer, "` has length ", len,
             ", but ", n, " row(s) were mapped; it must be length ", n,
             " or 1.")
}

bb_mapped_length <- function(mapping, data, aesthetic) {
    quo <- mapping[[aesthetic]]
    if (is.null(quo)) return(NULL)
    if (is.null(data)) return(1L)
    v <- eval_mapping(mapping, aesthetic, data)
    if (is.null(v)) return(NULL)
    length(v)
}

## Validate the coordinates of a layer and its aesthetic values.
##
## `aes` is a named list of extra aesthetic *values* the layer was given, e.g.
## the `col =` argument of `bb_point()`. Both mapped aesthetics and literal
## arguments must be length 1 or one value per row. `extra` names further
## aesthetics handled by the layer itself (e.g. `label` of `bb_text()`).
##
## Returns a list with the drawable rows (`keep`), the x/y coordinates of those
## rows and the total number of rows. Errors when no row can be drawn.
bb_prepare_aes <- function(x, y, mapping, data, layer, aes = list(),
                           extra = character(), n = NULL) {
    n <- n %||% max(length(x) %||% 0L, length(y) %||% 0L)
    if (n == 0L) {
        bb_abort("`", layer, "` has no data to draw: `",
                 if (is.null(x)) "y" else "x", "` maps to an empty vector.")
    }
    if (!is.null(x) && !is.null(y) && length(x) != length(y) &&
        length(x) != 1L && length(y) != 1L) {
        bb_abort("x and y mapped in `", layer, "` have different lengths (",
                 length(x), " and ", length(y), ").")
    }
    ok <- bb_valid_rows(x, y)
    if (!any(ok)) {
        bb_abort("`", layer, "` has no finite coordinates to draw ",
                 "(all ", n, " row(s) contain NA, NaN or infinite values).")
    }

    for (a in c("col", "fill", "pch", "lty", "cex", extra)) {
        mapped <- bb_mapped_length(mapping, data, a)
        if (a == "col" && !is.null(mapped)) {
            mapped <- eval_mapping(mapping, a, data)
        }
        bb_check_length(mapped, n, a, layer)
        bb_check_length(aes[[a]], n, a, layer)
    }

    x <- if (is.null(x)) NULL else rep(x, length.out = n)[ok]
    y <- if (is.null(y)) NULL else rep(y, length.out = n)[ok]
    list(x = x, y = y, keep = ok, n = n)
}

## Subset length-n constants to the rows a layer actually draws.
##
## A length-n constant is row-aligned, exactly like a mapped aesthetic: when
## rows with NA or infinite coordinates are dropped, the constants must follow,
## otherwise a colour/pch/cex vector silently shifts onto the wrong points.
bb_layer_constants <- function(params, keep, n,
                               aesthetics = c("col", "fill", "pch", "lty",
                                              "cex", "label")) {
    for (a in intersect(names(params), aesthetics)) {
        value <- params[[a]]
        if (!is.null(value) && length(value) == n) {
            params[[a]] <- value[keep]
        }
    }
    params
}

## Recycle an aesthetic vector to `n` values and subset it to drawable rows.
bb_aes_subset <- function(value, keep, n = length(keep)) {
    if (is.null(value)) return(NULL)
    rep(value, length.out = n)[keep]
}

## warnings are only emitted once per top-level print() call
bb_warn <- function(...) {
    env <- get_plotbb_env()
    msg <- paste0(...)
    warned <- if (exists("warned", envir = env, inherits = FALSE)) {
        get("warned", envir = env, inherits = FALSE)
    } else {
        character()
    }
    if (msg %in% warned) return(invisible(NULL))
    assign("warned", c(warned, msg), envir = env)
    warning(msg, call. = FALSE)
    invisible(NULL)
}

bb_reset_warnings <- function() {
    assign("warned", character(), envir = get_plotbb_env())
}

## Jitter coordinates reproducibly: the caller's RNG state is restored
## afterwards, so the same `seed` always yields the same positions and
## plotting never advances the user's random stream.
bb_jitter_many <- function(seed = NULL, x = NULL, y = NULL, width = NULL, height = NULL) {
    if (is.null(x) && is.null(y)) return(list(x = x, y = y))
    restore <- bb_rng_snapshot()
    on.exit(restore(), add = TRUE)

    if (!is.null(seed)) {
        if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed)) {
            bb_abort("`seed` of `bb_point()` must be a single finite number, got: ",
                     paste(utils::head(seed, 3), collapse = ", "))
        }
        seed_int <- suppressWarnings(as.integer(seed))
        if (is.na(seed_int)) {
            bb_abort("`seed` of `bb_point()` must be representable as an integer.")
        }
        seed <- seed_int
        set.seed(seed)
    }
    list(x = jitter(x, amount = width), y = jitter(y, amount = height))
}

bb_rng_snapshot <- function() {
    env <- globalenv()
    has_seed <- exists(".Random.seed", envir = env, inherits = FALSE)
    if (has_seed) old <- get(".Random.seed", envir = env, inherits = FALSE)
    function() {
        if (has_seed) {
            assign(".Random.seed", old, envir = env)
        } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
            rm(".Random.seed", envir = env)
        }
        invisible(NULL)
    }
}

## Resolve a mapped aesthetic into the values a layer will draw.
##
## Kept deliberately small: a mapped aesthetic is read from the plot that is
## being drawn, so the scale in effect is the last one in the `+` chain. It is
## *not* resolved when the layer is added: freezing values at that point makes
## a later `bb_scale_*()` change or be ignored depending on where it sits in
## the chain.
bb_probe_aesthetic <- function(plot, mapping, data, aesthetic, keep, n) {
    if (is.null(mapping[[aesthetic]])) return(NULL)
    value <- switch(aesthetic,
        col = bb_col(mapping, data, plot = plot),
        pch = bb_pch(mapping, data, plot = plot),
        cex = bb_cex(mapping, data, plot = plot),
        lty = bb_lty(mapping, data, plot = plot),
        eval_mapping(mapping, aesthetic, data)
    )
    if (is.null(value)) return(NULL)
    if (!is.null(keep) && !is.null(n)) bb_aes_subset(value, keep, n) else value
}
