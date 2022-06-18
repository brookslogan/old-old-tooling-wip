
##' @include caldate.R
##' @include weeksteps.R
NULL

#' @export
methods::setOldClass("calweek")

calweek.attribute.keys = c("starting.wday","owning.wday","convention.name","year.week.separator")

#' @export
calweek_of = function(obj, ...) {
    UseMethod("calweek_of", obj)
}
#' @export
calweek_of.calweek = function(obj, ...) obj
#' @export
calweek_of.caldate = function(obj, starting.wday, owning.wday, convention.name=NULL, year.week.separator="w") {
    owning.caldate.days.from.origin = obj - (wday0_of.caldate(obj) - starting.wday) %% 7L + (owning.wday - starting.wday) %% 7L
    structure(
        owning.caldate.days.from.origin,
        class="calweek",
        starting.wday=starting.wday,
        owning.wday=owning.wday,
        convention.name=convention.name,
        year.week.separator=year.week.separator
    )
    ## fixme vs. numbering by week...
    ## xxx structure vs class<-, attributes<-, etc.
}
#' @export
calweek_of.default = function(obj, starting.wday, owning.wday, convention.name=NULL, year.week.separator="w", ...) {
    calweek_of.caldate(caldate_of(obj, ...), starting.wday, owning.wday, convention.name=convention.name, year.week.separator=year.week.separator)
}
## fixme todo of integer, character

#' @export
`[.calweek` = function(x, i, ...) {
    result = unclass(x)[i]
    class(result) <- "calweek"
    attributes(result) <- attributes(x)[calweek.attribute.keys]
    result
}
#' @export
`[[.calweek` = function(x, i, ...) {
    result = unclass(x)[[i]]
    class(result) <- "calweek"
    attributes(result) <- attributes(x)[calweek.attribute.keys]
    result
}

#' @export
print.calweek = function(x, ...) {
    owning.caldate = x
    attributes(owning.caldate)[c("starting.wday","owning.wday","convention.name","year.week.separator")] <- NULL
    class(owning.caldate) <- "caldate"
    starting.wday = attr(x, "starting.wday")
    owning.wday = attr(x, "owning.wday")
    convention.name = attr(x, "convention.name")
    year.week.separator = attr(x, "year.week.separator")
    if (is.null(convention.name)) {
        cat(paste0('<calweek(',starting.wday,',',owning.wday,')>:\n'))
    } else {
        cat(paste0('<calweek(',starting.wday,',',owning.wday,'); "',convention.name,'" convention>:\n'))
    }
    print(sprintf("%d%s%02d",
                  year_of.caldate(owning.caldate),
                  year.week.separator,
                  n1_of_same_wday_in_year_up_to.caldate(owning.caldate)
                  ))
    invisible(x)
}

#' @export
as.character.calweek = function(x, ...) {
    owning.caldate = x
    attributes(owning.caldate)[c("starting.wday","owning.wday","convention.name","year.week.separator")] <- NULL
    class(owning.caldate) <- "caldate"
    year.week.separator = attr(x, "year.week.separator")
    sprintf("%d%s%02d",
            year_of.caldate(owning.caldate),
            year.week.separator,
            n1_of_same_wday_in_year_up_to.caldate(owning.caldate)
            )
}

#' @export
year_of.calweek = function(obj) {
    owning.caldate = obj
    attributes(owning.caldate)[calweek.attribute.keys] <- NULL
    class(owning.caldate) <- "caldate"
    year_of.caldate(owning.caldate)
}

#' @export
week0_of.calweek = function(obj) {
    owning.caldate = obj
    attributes(owning.caldate)[calweek.attribute.keys] <- NULL
    class(owning.caldate) <- "caldate"
    n0_of_same_wday_in_year_up_to.caldate(owning.caldate)
}

#' @export
week1_of.calweek = function(obj) {
    owning.caldate = obj
    attributes(owning.caldate)[calweek.attribute.keys] <- NULL
    class(owning.caldate) <- "caldate"
    n1_of_same_wday_in_year_up_to.caldate(owning.caldate)
}

#' @export
advance.calweek = function(obj, amt) UseMethod("advance.calweek", e2)

#' @export
advance.calweek.integer = function(obj, amt) {
    result = unclass(obj) + 7L*amt
    class(result) <- "calweek"
    result
}
## todo numeric

#' @export
advance.calweek.weeksteps = function(obj, amt) {
    result = unclass(obj) + 7L*unclass(amt)
    class(result) <- "calweek"
    result
}

## todo advance.calweek.numeric?

## `+.calweek` = function(e1, e2) {
##     ## This function assumes it is called via S3 dispatch or directly with object classes consistent with S3 dispatch.  However, S3 dispatch for Ops like `+` tries both args, so =e1= may not actually be a calweek as one might expect.  Additionally, e2 might be missing if used as a unary operator.  Delegate to an `advance` method, ensuring that the first argument is a calweek.
##     if (missing(e2)) {
##         stop ('Unary `+` not supported on calweek objects.')
##     } else if (is(e1, "calweek")) {
##         ## Inlined `ordered_dispatch_+.calweek`
##         UseMethod("ordered_dispatch_+.calweek", e2)
##     } else {
##         `ordered_dispatch_+.calweek`(e2, e1)
##     }
## }

## when updating, update inline versions:
#' @export
`ordered_dispatch_+.calweek` = function(e1, e2) UseMethod("ordered_dispatch_+.calweek", e2)

#' @export
`ordered_dispatch_+.calweek.integer` = function(e1, e2) {
    stop ('Adding a calweek and an integer is forbidden as it could lead to mistakes if used by habit on a calweek-like integer that has not be converted to a calweek yet; additionally, there have been issues getting custom Ops to work within grouped_df objects (among other things).  Use the advance method or weeksteps-like objects instead.')
}
## todo numeric

#' @export
`ordered_dispatch_+.calweek.default` = function(e1, e2) {
    UseMethod("advance.calweek", e2)
}

#' @export
`-.calweek` = function(e1, e2) {
    ## This function assumes it is called via S3 dispatch or directly with object classes consistent with S3 dispatch.  However, S3 dispatch for Ops like `-` tries both args, so =e1= may not actually be a calweek as one might expect.  Additionally, e2 might be missing if used as a unary operator.  Delegate to an `advance` method, ensuring that the first argument is a calweek.
    if (missing(e2)) {
        stop ('Unary `-` not supported on calweek objects.')
    } else if (inherits(e1, "calweek")) {
        ## Inlined `ordered_dispatch_-.calweek`
        UseMethod("ordered_dispatch_-.calweek", e2)
    } else {
        `ordered_dispatch_-.calweek`(e2, e1)
    }
}

## when updating, update inline versions:
#' @export
`ordered_dispatch_-.calweek` = function(e1, e2) UseMethod("ordered_dispatch_-.calweek", e2)

#' @export
`ordered_dispatch_-.calweek.integer` = function(e1, e2) {
    stop ('Subtracting a calweek and an integer is forbidden as it could lead to mistakes if used by habit on a calweek-like integer that has not be converted to a calweek yet; use the advance method or weeksteps-like objects instead.')
}
## todo numeric

#' @export
`ordered_dispatch_-.calweek.default` = function(e1, e2) {
    UseMethod("advance.calweek", -e2)
}

#' @export
Ops.calweek = function(e1, e2) {
    ## xxx move + and - overrides here?
    stop ('Ops not supported for calweek yet.')
}

## ## fixme S3 vs. S4...., + vs. Ops, ...... nothing seems to make grouped_df's recognize...
## setMethod("+", signature(e1="calweek", e2="integer"), `ordered_dispatch_+.calweek.integer`)
setMethod("+", signature(e1="calweek"), `ordered_dispatch_+.calweek.integer`)

setMethod("Ops", signature(e1="calweek", e2="integer"), function(e1,e2) stop('calweek Ops disabled'))

setMethod("Arith", signature(e1="calweek", e2="integer"), function(e1,e2) stop('calweek Ops disabled'))

setMethod("Arith", signature(e1="calweek", e2="ANY"), function(e1,e2) stop('calweek Ops disabled'))

## Registering `[` in S4 system helps avoid some loss of attributes and class in dplyr.
setMethod("[", signature(x="calweek"), `[.calweek`)

## xxx trying to get tibble/dplyr to detect...

#' @export
format.calweek = function(x, ...) {
    format(as.character(x), ...)
}

#' @export
show.calweek = function(object) {
    print.calweek(object)
    invisible(object)
}

## c.calweek = function(...) {
##     dots = list(...)
##     if (!identical(list("calweek"),
##                    unique(lapply(dots, function(arg) {
##                        attributes(arg)[calweek.attribute.keys ]
##                    })))) {
##         stop ('All arguments must have a single class "calweek".')
##     }
##     if (length(unique(lapply(dots, function(arg) {
##         attributes(arg)[calweek.attribute.keys ]
##     }))) != 1L) {
##         stop ('All arguments must have the same calweek-associated attributes.')
##     }
##     result = fixme finish
## }

## todo caldate(s)_of.calweek

## todo calweek convention objects

## todo step vs. shift? shift could potentially be fractional or even irrational... could potentially have both...

## xxx advance -> step?

## fixme tibble group_by summarize `+` incompatibility --- corrupted math & unclassed

## fixme tibble group_by incompatibility w/ attrs... even mutating col to be itself within grouped_df breaks

## fixme todo other Ops?

## todo check & match package...; think about compatibility with OpenAPI

## xxx dplyr issue 3923, PR 2209?, vctrs

## todo as.data.frame.calweek?

## fixme todo `c` method checking that attributes are the same... (slow... use convention IDs/objects and maintain table? but interaction with saving, ............)

## xxx NextMethod works to forward to mode-based defaults? (internal generics?) could potentially simplify some things.
