
## xxx naming: ndays vs n_days? calyear (+ if make generic to accept such a class)?:
n_days_in_year = function(year) {
    is.leap.year = year %% 4L == 0L & year %% 100L != 0L | year %% 400L == 0L
    365L + is.leap.year # implicit logical->integer conversion
}

year.cycle.origin = 0L
year.cycle.years = year.cycle.origin + (0:399)
year.cycle.length = length(year.cycle.years)
year.cycle.n.days.pattern = n_days_in_year(year.cycle.years)
year.cycle.day.offset.pattern = cumsum(year.cycle.n.days.pattern) %pipeR>>% {c(0L,.[-length(.)])}
year.cycle.total.days = sum(year.cycle.n.days.pattern)
year.cycle.origin.jan1.days.from.caldate.origin = as.integer(as.Date(paste0(year.cycle.origin,"-01-01"))) - caldate.origin.Date.as.integer

jan1_of_year_as_caldate =
    if (year.cycle.origin == 0L) {
        function(year) {
            n.year.cycles.from.origin = year %/% year.cycle.length
            position.in.cycle = year %% year.cycle.length
            days.from.caldate.origin =
                n.year.cycles.from.origin*year.cycle.total.days +
                year.cycle.day.offset.pattern[position.in.cycle+1L] +
                year.cycle.origin.jan1.days.from.caldate.origin
            structure(
                days.from.caldate.origin,
                class="caldate"
            )
        }
    } else {
        function(year) {
            offset.from.year.origin = year - year.cycle.origin
            n.year.cycles.from.origin = offset.from.year.origin %/% year.cycle.length
            position.in.cycle = offset.from.year.origin %% year.cycle.length
            days.from.caldate.origin =
                n.year.cycles.from.origin*year.cycle.total.days +
                year.cycle.day.offset.pattern[position.in.cycle+1L] +
                year.cycle.origin.jan1.days.from.caldate.origin
            structure(
                days.from.caldate.origin,
                class="caldate"
            )
        }
    }

## todo finish... maybe change pattern to align with caldate origin?

## use to speed up creation of caldate and calweek objects

## calweek = function(yyyyww.int, starting.wday, owning.wday, convention.name=NULL) {
##     doesn't work for all conventions....
##     fixme finish
##     structure(
##         owning.caldate.days.from.origin,
##         class="calweek",
##         starting.wday=starting.wday,
##         owning.wday=owning.wday,
##         convention.name=convention.name
##     )
## }

calweek_of_yw1_vecs = function(year, week1, starting.wday, owning.wday, convention.name=NULL, year.week.separator="w") {
    ## todo finish checks... make "match" package?
    ## if (!is.numeric(year) || !is.numeric(week) || !is.numeric(starting.wday) || !is.numeric(owning.wday)) {
    ##     stop ('year, week, starting.wday, and owning.wday arguments must be numeric')
    ## }
    ## year.as.integer = as.integer(year)
    ## week.as.integer = as.integer(week)
    ## starting.wday.as.integer = ...... + range checks...... + length checks
    ## length compatibility checks?
    ## if (any(year.as.integer != year) || any(week.as.integer != week) || ......) {
    ##     stop ('year and week arguments must contain all integer values')
    ## }
    jan1.days.from.caldate.origin = unclass(jan1_of_year_as_caldate(year))
    wday.offset = (owning.wday - jan1.days.from.caldate.origin - caldate.origin.wday0) %% 7L
    owning.caldate.days.from.origin = jan1.days.from.caldate.origin + wday.offset + 7L*(week1-1L)
    structure(
        owning.caldate.days.from.origin,
        class="calweek",
        starting.wday=starting.wday,
        owning.wday=owning.wday,
        convention.name=convention.name,
        year.week.separator=year.week.separator
    )
}

## xxx naming & allowed inputs? calyear? only a generic n_calweeks_in & require user to input calyear[_of](year)?
n_calweeks_in_year = function(year, owning.wday) {
    ## todo more efficient implementation
    result = (unclass(calweek_of_yw1_vecs(year+1L, 1L, 0L, owning.wday)) - unclass(calweek_of_yw1_vecs(year, 1L, 0L, owning.wday))) %/% 7L
    attributes(result)[c("starting.wday","owning.wday","convention.name","year.week.separator")] <- NULL
    result
}

## ## pillar interop / trying to get tibble/dplyr to detect...
## is_vector_s3.calweek = function(x) TRUE
## type_sum.calweek = function(x) "calweek"
## pillar_shaft.calweek = function(x, ...) {
##     ## fixme GPL-3..................................
##     pillar::pillar_shaft(as.character.calweek(x), ...)
## }
## ## fixme finish instructions on github README
## ## fixme pillar is GPL-3....
## ## setMethod("is_vector_s3", signature(x="calweek"), is_vector_s3.calweek)
## ## setMethod("type_sum", signature(x="calweek"), type_sum.calweek)
## ... some other page shows different interface...



## todo try caching year & week inputs/calculations in attributes?

## with_class = function(obj, clazz) {
##     class(obj) <- clazz
##     obj
## }
## to.be.classed = 1:100
## microbenchmark::microbenchmark(
## {result=to.be.classed;structure(to.be.classed, class="caldate")},
## {result=to.be.classed;result=to.be.classed;class(result)<-"caldate";result},
## {result=to.be.classed;class(result)<-"caldate";result},
## {result=to.be.classed;with_class(to.be.classed,"caldate")},
## {result=to.be.classed;`class<-`(to.be.classed,"caldate")},
## {result=to.be.classed;to.be.classed%>>%with_class("caldate")},
## times=1e6
## )
