
##' @include utils.R
NULL

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

as_caldate = function(obj, ...) {
    UseMethod("as_caldate", obj)
}
as_caldate.Date =
    if (caldate.origin.Date.as.integer == 0L) {
        function(obj, ...) {
            structure(
                as.integer(obj),
                class="caldate"
            )
        }
    } else {
        function(obj, ...) {
            structure(
                as.integer(obj) - caldate.origin.Date.as.integer,
                class="caldate"
            )
        }
    }
as_caldate.default = function(obj, ...) {
    as_caldate(as.Date(obj, ...))
}

caldate.origin.caldate = as_caldate(caldate.origin.Date)

ymd_to_caldate = function(obj) {
    UseMethod("ymd_to_caldate", obj)
}
ymd_to_caldate.integer = function(obj) {
    d = obj %% 100L
    ym = obj %/% 100L
    m = ym %% 100L
    y = ym %/% 100L
    ## todo speed up with tables?
    as_caldate(as.Date(paste0(y,"-",m,"-",d)))
}

## todo `+` override... S3*S3 vs. S4?

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
## date.cycle.nth0.wday.of.year.pattern = date.cycle.yday0.pattern[1:30] %/% 7L
## date.cycle.wday0.pattern = as.POSIXlt(date.cycle.dates)[["wday"]] # this actually has a period of 7; it could be more compactly represented with a length-7 vector, or just directly calculated instead



get_year = function(obj) {
    UseMethod("get_year", obj)
}
get_year.caldate = function(obj) {
    days.from.origin = unclass(obj)
    periods.from.origin = days.from.origin %/% date.cycle.period
    period.offset = days.from.origin %% date.cycle.period
    caldate.origin.year +
        date.cycle.years.per.cycle*periods.from.origin +
        date.cycle.year.offset.pattern[period.offset+1L]
}
get_yday0 = function(obj) {
    UseMethod("get_yday0", obj)
}
get_yday0.caldate = function(obj) {
    days.from.origin = unclass(obj)
    period.offset = days.from.origin %% date.cycle.period
    date.cycle.yday0.pattern[period.offset+1L]
}
get_yday1 = function(obj) {
    UseMethod("get_yday1", obj)
}
get_yday1.caldate = function(obj) {
    get_yday0.caldate(obj) + 1L
}

get_mon0 = function(obj) {
    UseMethod("get_mon0", obj)
}
get_mon0.caldate = function(obj) {
    days.from.origin = unclass(obj)
    period.offset = days.from.origin %% date.cycle.period
    date.cycle.mon0.pattern[period.offset+1L]
}
get_mon1 = function(obj) {
    UseMethod("get_mon1", obj)
}
get_mon1.caldate = function(obj) {
    get_mon0.caldate(obj) + 1L
}
get_mday1 = function(obj) {
    UseMethod("get_mday1", obj)
}
get_mday1.caldate = function(obj) {
    days.from.origin = unclass(obj)
    period.offset = days.from.origin %% date.cycle.period
    date.cycle.mday1.pattern[period.offset+1L]
}
get_mday0 = function(obj) {
    UseMethod("get_mday0", obj)
}
get_mday0.caldate = function(obj) {
    get_mday1.caldate(obj) - 1L
}

get_wday0 = function(obj) {
    UseMethod("get_wday0", obj)
}
get_wday0.caldate = function(obj) {
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
get_wday7 = function(obj) {
    UseMethod("get_wday7", obj)
}
get_wday7.caldate = function(obj) {
    wday0 = get_wday0.caldate(obj)
    wday7 = wday0 + 7L*(wday0==0L) # (implicit conversion from TRUE/FALSE to 1/0)
    wday7
}
n0_of_same_wday_in_year_up_to = function(obj) {
    get_yday0(obj) %/% 7L
}
n1_of_same_wday_in_year_up_to = function(obj) {
    get_yday0(obj) %/% 7L + 1L
}

print.caldate = function(x, ...) {
    cat("<caldate>:\n")
    print(sprintf("%d-%02d-%02d", get_year(x), get_mon1(x), get_mday1(x)))
    invisible(x)
}

## xxx todo fixme limits on print amount

## xxx wday7, etc. -> wday7_of, etc.?

## xxx differentiate get_year from owning_year for weeks... get_ vs. parent_ vs. containing_? then also get_epiyear, get_epimonth vs. owning_year, owning_month, etc.  Then what to call yday's?
## xxx potential restructuring for ease of implementation if extender does not
## want to implement as.Date or as_caldate or all of the number-variant methods:
##  - get_wday0
##  - get_wday0.default --- use get_wday7_helper
##  - get_wday0.caldate
##  - get_wday7
##  - get_wday7.default --- use get_wday0_helper
##  - get_wday7.caldate
##  - get_wday0_helper
##  - get_wday0_helper.default --- try converting to caldate
##  - get_wday0_helper.caldate
##  - get_wday7_helper
##  - get_wday7_helper.default --- try converting to caldate
##  - get_wday7_helper.caldate

## xxx todo converting defaults?

## xxx remove braces when possible for efficiency? e.g., around UseMethod's?

## todo method returning ymd, use in print
## todo method returning yw, ywwd, use in calweek?
## xxx get -> of?  try to standardize small subset of words like get, as, of, from, to, ...
