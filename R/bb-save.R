##' Save a plot to a file
##'
##' `bb_save()` draws into a temporary file next to the target and moves it into
##' place only after the plot has been drawn. It never closes a device the caller
##' had already open, and on failure it closes its own device, keeps the original
##' error, removes the temporary file and leaves an existing target untouched
##' (even when `overwrite = TRUE`).
##'
##' The device is chosen from the file extension (`pdf`, `png`, `jpeg`/`jpg`,
##' `tiff`/`tif`, `svg`) unless `device` is given. Supplying a device that
##' conflicts with a known extension is an error. Vector devices take their size
##' in inches; raster devices take `width`, `height`, `units` and `res`.
##'
##' An existing file is never overwritten unless `overwrite = TRUE`. A missing
##' directory is an error.
##'
##' @title bb_save
##' @param plot a \code{bbplot} object
##' @param filename output file
##' @param width,height device size
##' @param units one of \code{"in"}, \code{"cm"}, \code{"mm"}, \code{"px"}
##' @param res resolution for raster devices
##' @param bg background colour
##' @param device optional device name (\code{"pdf"}, \code{"png"}, ...) or a
##'   device function such as \code{grDevices::pdf}
##' @param overwrite whether an existing file may be replaced
##' @param ... additional arguments passed to the device
##' @return The file name, invisibly
##' @seealso \code{\link{bb_build}}
##' @export
##' @examples
##' p <- bbplot(mtcars, bb_aes(mpg, disp)) + bb_point()
##' f <- tempfile(fileext = ".pdf")
##' bb_save(p, f)
##' unlink(f)
bb_save <- function(plot, filename, width = 7, height = 5, units = "in",
                    res = 300, bg = "white", device = NULL,
                    overwrite = FALSE, ...) {
    if (!inherits(plot, "bbplot")) {
        bb_abort("`bb_save()` requires a `bbplot` object.")
    }
    if (!is.character(filename) || length(filename) != 1L ||
        is.na(filename) || !nzchar(filename)) {
        bb_abort("`filename` must be a single non-empty path.")
    }
    if (!is.logical(overwrite) || length(overwrite) != 1L ||
        is.na(overwrite)) {
        bb_abort("`overwrite` must be TRUE or FALSE.")
    }
    target <- path.expand(filename)
    directory <- dirname(target)
    if (!dir.exists(directory)) {
        bb_abort("the output directory does not exist: ", directory)
    }
    if (file.exists(target) && !isTRUE(overwrite)) {
        bb_abort("`", filename, "` already exists; use `overwrite = TRUE` to ",
                 "replace it.")
    }

    units <- match.arg(units, c("in", "cm", "mm", "px"))
    extension <- tolower(tools::file_ext(target))
    info <- bb_save_resolve_device(device, extension)
    dims <- bb_save_dimensions(width, height, units, res, info$raster)

    ## Draw into a temporary file next to the target and move it into place only
    ## after the plot has been drawn successfully. A failure therefore never
    ## truncates or deletes an existing file, even with `overwrite = TRUE`.
    temp <- tempfile(
        pattern = paste0(tools::file_path_sans_ext(basename(target)), "-"),
        tmpdir = directory,
        fileext = if (nzchar(extension)) paste0(".", extension) else ".tmp"
    )
    ## `dev.off()` does not return to the device that was current before: with
    ## several devices open it moves to the next one in the list. Remember the
    ## caller's device and restore it explicitly after closing our own.
    previous_id <- grDevices::dev.cur()

    if (identical(info$name, "custom")) {
        formals <- names(formals(info$fun))
        args <- list(width = dims$width, height = dims$height, bg = bg)
        if ("units" %in% formals) args$units <- dims$units
        if ("res" %in% formals) args$res <- dims$res
    } else if (info$raster) {
        args <- list(width = dims$width, height = dims$height,
                     units = dims$units, res = dims$res, bg = bg)
    } else {
        args <- list(width = dims$width, height = dims$height, bg = bg)
    }
    extra <- list(...)
    if (length(extra)) args <- c(args, extra)

    dev_id <- NULL
    completed <- FALSE
    on.exit({
        if (!is.null(dev_id) && grDevices::dev.cur() == dev_id) {
            grDevices::dev.off()
        }
        if (!completed && file.exists(temp)) {
            unlink(temp)
        }
        if (previous_id > 1L && previous_id %in% grDevices::dev.list() &&
            grDevices::dev.cur() != previous_id) {
            grDevices::dev.set(previous_id)
        }
    }, add = TRUE)

    do.call(info$fun, c(list(temp), args))
    dev_id <- grDevices::dev.cur()
    print(plot)
    grDevices::dev.off()
    dev_id <- NULL
    if (previous_id > 1L && previous_id %in% grDevices::dev.list() &&
        grDevices::dev.cur() != previous_id) {
        grDevices::dev.set(previous_id)
    }
    bb_save_move(temp, target)
    completed <- TRUE
    invisible(target)
}

## Move the finished temporary file into place. `file.rename()` replaces an
## existing file on POSIX; on platforms where it refuses, copy over the target
## and remove the temporary file.
bb_save_move <- function(from, to) {
    if (isTRUE(suppressWarnings(file.rename(from, to)))) {
        return(invisible(to))
    }
    if (!isTRUE(suppressWarnings(file.copy(from, to, overwrite = TRUE)))) {
        bb_abort("could not move the generated file into place: ", to)
    }
    unlink(from)
    invisible(to)
}

bb_save_device_map <- function() {
    list(
        pdf = list(fun = grDevices::pdf, raster = FALSE, name = "pdf"),
        png = list(fun = grDevices::png, raster = TRUE, name = "png"),
        jpeg = list(fun = grDevices::jpeg, raster = TRUE, name = "jpeg"),
        tiff = list(fun = grDevices::tiff, raster = TRUE, name = "tiff"),
        svg = list(fun = grDevices::svg, raster = FALSE, name = "svg")
    )
}

bb_save_canonical_extension <- function(extension) {
    switch(extension,
           jpg = "jpeg",
           tif = "tiff",
           extension)
}

bb_save_resolve_device <- function(device, extension) {
    devices <- bb_save_device_map()
    canonical <- bb_save_canonical_extension(extension)

    if (is.null(device)) {
        if (!nzchar(canonical) || !canonical %in% names(devices)) {
            bb_abort("cannot infer a device from the file extension",
                     if (nzchar(extension)) paste0(" `.`", extension) else "",
                     "; supply `device`.")
        }
        return(devices[[canonical]])
    }

    if (is.character(device)) {
        if (length(device) != 1L || !device %in% names(devices)) {
            bb_abort("unknown `device`: ", paste(device, collapse = ", "),
                     ". Use one of ", paste(names(devices), collapse = ", "),
                     " or a device function.")
        }
        chosen <- devices[[device]]
        if (nzchar(canonical) && canonical %in% names(devices) &&
            !identical(devices[[canonical]]$fun, chosen$fun)) {
            bb_abort("the file extension `.`", extension,
                     " conflicts with `device = \"", device, "\"`.")
        }
        return(chosen)
    }

    if (is.function(device)) {
        if (nzchar(canonical) && canonical %in% names(devices) &&
            !identical(devices[[canonical]]$fun, device)) {
            bb_abort("the file extension `.`", extension,
                     " conflicts with the supplied device function.")
        }
        return(list(fun = device,
                    raster = "res" %in% names(formals(device)),
                    name = "custom"))
    }

    bb_abort("`device` must be NULL, a device name or a device function.")
}

bb_save_dimensions <- function(width, height, units, res, raster) {
    check_positive <- function(value, name) {
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
            value <= 0) {
            bb_abort("`", name, "` must be a positive number.")
        }
    }
    check_positive(width, "width")
    check_positive(height, "height")

    if (raster) {
        if (!is.numeric(res) || length(res) != 1L || !is.finite(res) ||
            res <= 0) {
            bb_abort("`res` must be a positive number.")
        }
        return(list(width = width, height = height, units = units,
                    res = as.integer(round(res))))
    }

    inches <- switch(units,
        `in` = c(width, height),
        cm = c(width, height) / 2.54,
        mm = c(width, height) / 25.4,
        px = {
            if (!is.numeric(res) || length(res) != 1L || !is.finite(res) ||
                res <= 0) {
                bb_abort("`units = \"px\"` for a vector device requires a ",
                         "positive `res`.")
            }
            c(width, height) / res
        }
    )
    list(width = inches[[1]], height = inches[[2]])
}
