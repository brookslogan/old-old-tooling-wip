
##' @include epitimeR-utils.R
##' @include generics.R
NULL

methods::setOldClass("caldate")

## Choose an caldate origin that will be represented by the integer 0L within
## caldate objects. The current selection is the same as the Date origin, which
## is mapped to 0L by as.integer, which allows for quick conversions from Date
## objects. However, certain other operations could be faster with a different
## choice of caldate origin.
caldate.origin.Date = as.Date("1970-01-01")
caldate.origin.Date.as.integer = as.integer(caldate.origin.Date)
caldate.origin.POSIXlt = as.POSIXlt(caldate.origin.Date)
caldate.origin.year = caldate.origin.POSIXlt[["year"]] + 1900L
caldate.origin.wday0 = caldate.origin.POSIXlt[["wday"]]

caldate_of.Date =
    if (caldate.origin.Date.as.integer == 0L) {
        function(obj, ...) {
            `class<-`(
                as.integer(obj),
                "caldate"
            )
        }
    } else {
        function(obj, ...) {
            `class<-`(
                as.integer(obj) - caldate.origin.Date.as.integer,
                "caldate"
            )
        }
    }
caldate_of.caldate = function(obj, ...) obj
caldate_of.default = function(obj, ...) {
    caldate_of(as.Date(obj, ...))
}
## todo caldate_of yyyymmdd integer? conflicts with default behavior... override or use different name?

caldate.origin.caldate = caldate_of(caldate.origin.Date)

## Leap year patterns repeat every 400 years, as does much of the common of
## interest about dates. Precalculate some date information for 400 years' worth
## of dates, to use to more quickly extract information about other dates. This
## is not particularly memory-efficient, and caching issues may make it not very
## time-efficient either; an alternative to consider is to use iterator concepts
## and/or lazy "algebraic" data frames building off of these concepts to
## compactly represent the patterns below.
date.cycle.period = 365L*400L + 100L - 4L + 1L
date.cycle.years.per.cycle = 400L
date.cycle.dates = as.Date("1970-01-01") + seq_len(date.cycle.period) - 1L
date.cycle.year.offset.pattern = as.POSIXlt(date.cycle.dates)[["year"]] %pipeR>>% {. - .[[1L]]}
date.cycle.yday0.pattern = as.POSIXlt(date.cycle.dates)[["yday"]] %pipeR>>% {. - .[[1L]]}
date.cycle.mon0.pattern = as.POSIXlt(date.cycle.dates)[["mon"]]
date.cycle.mday1.pattern = as.POSIXlt(date.cycle.dates)[["mday"]]

caldate_of_ymd_vecs = function(y,m,d) {
    ## todo speed up with tables?
    caldate_of(as.Date(paste0(y,"-",m,"-",d)))
}
caldate_of_ymd.integer = function(obj) {
    d = obj %% 100L
    ym = obj %/% 100L
    m = ym %% 100L
    y = ym %/% 100L
    caldate_of_ymd_vecs(y,m,d)
}
caldate_of_ymd.numeric = function(obj) {
    if (!all(as.integer(obj) == obj)) {
        stop ('obj must not have fractional part')
    }
    caldate_of_ymd.integer(as.integer(obj))
}
## xxx below names vs. mon1, mday1?...
## xxx default keys being 1L, 2L, 3L?
caldate_of_ymd.list = function(obj, y.key="year", m.key="mon", d.key="mday") {
    y = obj[[y.key]]
    m = obj[[m.key]]
    d = obj[[d.key]]
    caldate_of_ymd_vecs(y,m,d)
}
caldate_of_ymd.data.frame = caldate_of_ymd.list

year_of.caldate = function(obj) {
    days.from.origin = unclass(obj)
    periods.from.origin = days.from.origin %/% date.cycle.period
    period.offset = days.from.origin %% date.cycle.period
    caldate.origin.year +
        date.cycle.years.per.cycle*periods.from.origin +
        date.cycle.year.offset.pattern[period.offset+1L]
}

yday0_of.caldate = function(obj) {
    days.from.origin = unclass(obj)
    period.offset = days.from.origin %% date.cycle.period
    date.cycle.yday0.pattern[period.offset+1L]
}
yday1_of.caldate = function(obj) {
    yday0_of(obj) + 1L
}

mon0_of.caldate = function(obj) {
    days.from.origin = unclass(obj)
    period.offset = days.from.origin %% date.cycle.period
    date.cycle.mon0.pattern[period.offset+1L]
}
mon1_of.caldate = function(obj) {
    mon0_of.caldate(obj) + 1L
}

mday1_of.caldate = function(obj) {
    days.from.origin = unclass(obj)
    period.offset = days.from.origin %% date.cycle.period
    date.cycle.mday1.pattern[period.offset+1L]
}
mday0_of.caldate = function(obj) {
    mday1_of.caldate(obj) - 1L
}

wday0_of.caldate = function(obj) {
    ## Note:
    ## * obj's wday0 is congruent to (unclassed obj + offset) mod 7
    ## * caldate.origin.wday0 is congruent to (unclassed origin + offset) mod 7
    ## * unclassed origin = 0
    ## * so caldate.origin.wday0 is congruent to offset mod 7
    ## * so obj's wday0 is congruent to (unclassed obj + caldate.origin.wday0) mod 7
    days.from.origin = unclass(obj)
    wday0 = (days.from.origin + caldate.origin.wday0) %% 7L
    wday0
}
wday1_of.caldate = function(obj) {
    wday0 = get_wday0.caldate(obj)
    wday7 = wday0 + 7L*(wday0==0L) # (implicit conversion from TRUE/FALSE to 1/0)
    wday7
}

n0_of_same_wday_in_year_up_to.caldate = function(obj) {
    yday0_of(obj) %/% 7L
}
n1_of_same_wday_in_year_up_to.caldate = function(obj) {
    ## xxx potential good use of an inlining function
    yday0_of(obj) %/% 7L + 1L
}

advance.caldate = function(obj, amt) UseMethod("advance.caldate", amt)

advance.caldate.integer = function(obj, amt) {
    result = unclass(obj) + amt
    class(result) <- "caldate"
    result
}

advance.caldate.numeric = function(obj, amt) {
    amt.as.integer = as.integer(amt)
    if (!all(amt == amt.as.integer)) {
        stop ('=amt= must contain all integral values')
    }
    advance.caldate.integer(obj, amt.as.integer)
}

`+.caldate` = function(e1, e2) {
    ## This function assumes it is called via S3 dispatch or directly with object classes consistent with S3 dispatch.  However, S3 dispatch for Ops like `+` tries both args, so =e1= may not actually be a caldate as one might expect.  Additionally, =e2= might be missing if used as a unary operator.  Delegate to an `advance` method, ensuring that the first argument is a caldate.
    if (missing(e2)) {
        stop ('Unary `+` not supported on caldate objects.')
    } else if (inherits(e1, "caldate")) {
        ## Inlined `ordered_dispatch_+.caldate`
        UseMethod("ordered_dispatch_+.caldate", e2)
    } else {
        `ordered_dispatch_+.caldate`(e2, e1)
    }
}

## when updating, update inline versions:
`ordered_dispatch_+.caldate` = function(e1, e2) UseMethod("ordered_dispatch_+.caldate", e2)

`ordered_dispatch_+.caldate.default` = function(e1, e2) {
    UseMethod("advance.caldate", e2)
}

`-.caldate` = function(e1, e2) {
    ## This function assumes it is called via S3 dispatch or directly with object classes consistent with S3 dispatch.  However, S3 dispatch for Ops like `-` tries both args, so =e1= may not actually be a caldate as one might expect.  Additionally, e2 might be missing if used as a unary operator.  Delegate to an `advance` method, ensuring that the first argument is a caldate.
    if (missing(e2)) {
        stop ('Unary `-` not supported on caldate objects.')
    } else if (inherits(e1, "caldate")) {
        ## Inlined `ordered_dispatch_-.caldate`
        UseMethod("ordered_dispatch_-.caldate", e2)
    } else {
        `ordered_dispatch_-.caldate`(e2, e1)
    }
}

## when updating, update inline versions:
`ordered_dispatch_-.caldate` = function(e1, e2) UseMethod("ordered_dispatch_-.caldate", e2)

`ordered_dispatch_-.caldate.default` = function(e1, e2) {
    UseMethod("advance.caldate", -e2)
}

Ops.caldate = function(e1, e2) {
    ## xxx move + and - overrides here?
    stop ('Ops not supported for caldate yet.')
}

c.caldate = function(...) {
    if (!all(sapply(list(...), function(obj) is(obj, "caldate")))) {
        stop ('All arguments to =c.caldate= must be =caldate= objects.')
    }
    `class<-`(NextMethod(), "caldate")
}

`[.caldate` = function(...) {
    `class<-`(NextMethod(), "caldate")
}

##' @method as.Date calweek
as.Date.caldate =
    if (caldate.origin.Date.as.integer == 0L) {
        function(x, ...) {
            ## xxx assumes things about structure of Date objects... check if they can be relied on
            x.as.Date = x
            class(x.as.Date) <- "Date"
            x.as.Date
        }
    } else {
        function(x, ...) {
            as.Date(unclass(x), caldate.origin.Date)
        }
    }

format.caldate = function(x, ...) {
    format.Date(as.Date(x, ...))
}

as.character.caldate = function(x, ...) {
    as.character.Date(as.Date.caldate(x, ...), ...)
}

print.caldate = function(x, ...) {
    cat('<caldate>:\n')
    print(as.character.caldate(x))
}

## xxx "<caldate>" would be too long for frequent use in tibble headers; pillar_shaft recommends no more than 4 chars instead of the 7; but thinking of calweek and season model weeks etc., would probably want the longer form when printed outside, so may just have a different label for in tibbles

## fixme licensing?:
as.data.frame.caldate = as.data.frame.Date

rep.caldate = function(x, ...) {
    `class<-`(NextMethod(), "caldate")
}

## todo defaults for many of above functions via caldate conversions
## todo caldate to ymd integer conversions
## todo caldate to df conversions
## todo caldate stringifying functions
## todo as.Date.caldate
## todo "cal" -> "greg", then use "sol" for year-long season related objects?
## todo https://tibble.tidyverse.org/articles/extending.html, slightly reformatted
## fixme gpl vs. mit issues
## xxx should cal*** instead refer only to things without year?
## todo `weekdays`, etc., forwarding
## todo use scoping prefix in attribute names? (& class names? maybe not feasible)
## todo check with data.frame, tibble, data.table

## xxx class --> c("caldate","Date") ??? then just override any unwanted Date behavior with caldate methods?

## todo in general, a way to optimize out pipes from functions, loops, etc.? also provide "inline" function that produces error if ever called, but that the optimizer will use as an instruction to inline a function call.
