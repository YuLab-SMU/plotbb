## Graphics state management for bbplot
##
## base graphics parameters are device-global. print.bbplot() therefore
## snapshots par(), applies the plot theme, and restores the snapshot when
## drawing finishes -- both on normal return and on error.
##
## The parameters listed in `bb_manual_par` are restored *unconditionally*
## (and exactly), because bbplot itself sets them while drawing. The other
## restorable parameters are restored with a tolerance, so that device
## bookkeeping (e.g. `par("new")` being reset by a device coords query) does
## not trigger spurious restores.

## parameters that bbplot modifies explicitly and restores unconditionally
bb_manual_par <- c("bg", "mar", "oma", "mgp", "tck", "las", "mfrow", "new", "pty")

## parameters restored for compatibility with code that changes them inside a
## custom base-graphics layer
bb_legacy_par <- c("cex", "cex.axis", "cex.lab", "cex.main", "cex.sub",
                   "col", "col.axis", "col.lab", "col.main", "col.sub",
                   "fg", "font", "font.axis", "font.lab", "font.main",
                   "font.sub", "lend", "ljoin", "lmitre", "lty", "lwd",
                   "srt")

bb_restorable_par <- c(bb_manual_par, bb_legacy_par)

## parameters reported by par(no.readonly = TRUE) that restore safely.
##
## `bg` is kept even when it is the empty string (the device cannot hand it
## back, and "" means "no background"), which matters for PDF devices.
bb_par_snapshot <- function() {
    snapshot <- suppressWarnings(graphics::par(no.readonly = TRUE))
    keep <- names(snapshot) %in% bb_restorable_par
    numeric <- vapply(snapshot, is.numeric, logical(1))
    snapshot[keep & (numeric | names(snapshot) == "bg")]
}

bb_par_restore_value <- function(name, old, current) {
    if (is.null(current)) return(old)
    if (length(old) != length(current)) return(old)
    if (name %in% bb_manual_par) {
        if (identical(unname(old), unname(current))) return(NULL)
        return(old)
    }
    if (isTRUE(all.equal(unname(old), unname(current), tolerance = 1e-6))) return(NULL)
    old
}

## restore the snapshot onto the *current* device; returns the names of the
## parameters that were restored
bb_par_restore <- function(snapshot) {
    if (is.null(snapshot) || !length(snapshot)) return(invisible(character()))
    device <- grDevices::dev.cur()
    if (is.null(device) || device == 1L) return(invisible(character()))

    current <- suppressWarnings(tryCatch(graphics::par(no.readonly = TRUE),
                                         error = function(e) NULL))
    if (is.null(current)) return(invisible(character()))

    restore <- list()
    for (nm in names(snapshot)) {
        value <- bb_par_restore_value(nm, snapshot[[nm]], current[[nm]])
        if (!is.null(value)) restore[[nm]] <- value
    }
    if (!length(restore)) return(invisible(character()))

    ## `"new"` is a drawing flag, not a device setting: never leave it TRUE
    if (identical(restore[["new"]], TRUE)) restore[["new"]] <- NULL
    if (!length(restore)) return(invisible(character()))

    suppressWarnings(tryCatch(graphics::par(restore), error = function(e) NULL))
    invisible(names(restore))
}
