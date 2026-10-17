##' set bb_theme
##' 
##' setting bb_theme for ordinary base plot command. It internally use par to set global graphic parameters. 
##' Users need to explictely call unset_bb_theme() to restore original setting.
##'
##' Themes are single-level: calling `set_bb_theme()` while a theme is already
##' active replaces it without losing the original graphics parameters, so
##' `unset_bb_theme()` always restores the state from before the *first* call.
##' Prefer `with_bb_theme()` when the theme should be scoped to a block of code.
##' @title set_bb_theme
##' @param theme bb_theme
##' @return setting selected theme as default (has side effect and will affect other base plot)
##' @export
##' @examples
##' library(plotbb)
##' set_bb_theme(bb_theme_deepblue)
##' bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl))) + bb_point(pch=19)
##' unset_bb_theme()
##' @author Guangchuang Yu
set_bb_theme <- function(theme) {
    not_set <- is.null(getOption("bb_old_par"))
    if (not_set) {
        old.par <- par(no.readonly=TRUE) #, new = TRUE)
        options(bb_old_par = old.par)
    }
    if (is(theme, "function")) theme <- theme()
    par(theme, no.readonly = TRUE)
    invisible(theme)
}

##' unset bb_theme
##' 
##' remove all the themes by set_bb_theme. The saved pre-theme graphics
##' parameters are discarded afterwards, so the function is safe to call
##' repeatedly.
##' @title unset_bb_theme
##' @return unset theme (i.e., restore par setting)
##' @export
##' @author Guangchuang Yu
unset_bb_theme <- function() {
    old.par <- getOption("bb_old_par")
    if (!is.null(old.par)) {
        suppressWarnings(par(old.par, no.readonly = TRUE))
        options(bb_old_par = NULL)
    }
    invisible()
}

##' Run code with a theme applied to the current device
##'
##' `with_bb_theme()` applies `theme` with `par()` for the duration of `code`
##' and restores the previous graphics parameters afterwards, including when
##' `code` signals an error. Nothing is stored in options, so it can be nested
##' and does not interact with `set_bb_theme()`.
##' @title with_bb_theme
##' @param theme a `bb_theme` object or a function returning one.
##' @param code an expression to evaluate.
##' @return the value of `code`, invisibly.
##' @export
##' @examples
##' library(plotbb)
##' with_bb_theme(bb_theme_deepblue(), {
##'   bbplot(mtcars, bb_aes(mpg, disp, col=factor(cyl))) + bb_point(pch=19)
##' })
##' @author Guangchuang Yu
with_bb_theme <- function(theme, code) {
    if (is(theme, "function")) theme <- theme()
    stopifnot(is(theme, "bb_theme"))

    old.par <- par(no.readonly = TRUE)
    restore <- function() {
        device <- grDevices::dev.cur()
        if (!is.null(device) && device != 1L) {
            suppressWarnings(par(old.par, no.readonly = TRUE))
        }
        invisible(NULL)
    }
    on.exit(restore(), add = TRUE)

    par(theme, no.readonly = TRUE)
    code <- substitute(code)
    invisible(eval(code, parent.frame()))
}

##' bbplot theme
##'
##' setting visual details of bbplot
##' @title bb_theme
##' @rdname bb-theme
##' @param ... parameters for graphics::par
##' @return A modified bbplot object
##' @export
##' @author Guangchuang Yu
bb_theme <- function(...) {
    default <- list(
        cex.axis = .9,
        las = 1
    )
    params <- modifyList(
        default,
        list(...)
    )
    structure(params, class = "bb_theme")
}



##' @method bbplot_add bb_theme
##' @export
bbplot_add.bb_theme <- function(object, plot) {
    plot$theme <- plot$theme <= object
    plot
}


##' @rdname bb-theme
##' @export
bb_theme_expand <- function(...) {
    bb_theme(
        mar=c(3,3,2,1),
        mgp=c(2,0.4,0),
        tck=-.01
    ) <= bb_theme(...)
}

##' @rdname bb-theme
##' @export
bb_theme_grey <- function(...) {
    bb_theme(
        bg = "grey85",
        fg = "grey20",
        col.axis = "grey20",
        col.lab = "grey20"
    ) <= bb_theme(...)
}

##' @rdname bb-theme
##' @export
bb_theme_deepblue <- function(...) {
    bb_theme(
        bg = "#002E49",
        fg = "#CCCCCC",
        col.axis = "#BEBEBE",
        col.lab = "#FFFFFF"
    ) <= bb_theme(...)
}

build_theme <- function(default, ...) {
    ## default is a list of default setting
    do.call(bb_theme, default) <= bb_theme(...)
}

##' @rdname bb-theme
##' @export
bb_theme_bw <- function(...) {
    bb_theme(
        bg = "white",
        fg = "black",
        col.axis = "black",
        col.lab = "black"
    ) <= bb_theme(...)
}

##' @rdname bb-theme
##' @export
bb_theme_minimal <- function(...) {
    bb_theme(
        bg = "white",
        fg = "black",
        col.axis = "black",
        col.lab = "black",
        bty = "n",
        tck = -0.01
    ) <= bb_theme(...)
}
