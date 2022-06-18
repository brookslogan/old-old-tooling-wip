
devtools::load_all("..")
devtools::load_all("../../epiforecast-R/epiforecast")
source("../scratch/jsonlike-utils.R")

fetch_epidata = function(source.name, nonauth.args, auth.args,
                         cache, cache.invalidation.period,
                         accept.empty.results = FALSE,
                         is_auth_name = function(arg.names) arg.names=="auth"
                         ) {
    regular.arg.names = namesp(nonauth.args)
    auth.arg.names = namesp(auth.args)
    if (any(regular.arg.names == "") ||
        any(auth.arg.names == "")) {
        stop ("All nonauth.args and auth.args must be fully named (matching parameter names in the corresponding Epidata function).")
    }
    if (any(is_auth_name(regular.arg.names)) ||
        !all(is_auth_name(auth.arg.names))
        ) {
        stop ("auth arg should either not appear or appear only in auth.args; other args must appear in nonauth.args.")
    }
    ## todo version using formals to allow unnamed args?
    source_fn = Epidata[[source.name]]
    .list = environment(source_fn)[[".list"]]
    branches.and.leaf = c(source.name, paste0(names(nonauth.args),"=",sapply(nonauth.args,.list)))
    branches = branches.and.leaf[seq_len(length(branches.and.leaf)-1L)]
    leaf = paste0(branches.and.leaf[[length(branches.and.leaf)]])
    subcache = cache_path(cache, branches)
    fetch_resource(
        function() {
            do.call(source_fn, c(nonauth.args, auth.args))
        },
        function(response) {
            if (!(
                identical(response[["result"]], 1L) &&
                identical(response[["message"]], "success") ||
                accept.empty.results &&
                identical(response[["result"]], -2L) &&
                identical(response[["message"]], "no results")
            )) {
                stop (sprintf("Epidata response did not look usable; result=%d, message=%s, accept.empty.results=%s",
                              response[["result"]], response[["message"]],
                              as.character(accept.empty.results)))
            }
            return (TRUE)
        },
        subcache, leaf,
        cache.invalidation.period
    )
}

epidata.cache = simple_rds_cache("~/.epiforecast-cache/epicache")

## fetch_epidata("fluview",
##               list(regions="nat",
##                    ## epiweeks=list(201001,Epidata$range(201005,201008))
##                    epiweeks=list(123401L)
##                    ),
##               list(),
##               epidata.cache,
##               as.difftime(1,units="days")
## )

## fetch_epidata("cdc",
##               list(locations="nat",
##                    epiweeks=list(201001,Epidata$range(201005,201008))
##                    ),
##               list(auth="bogus"),
##               epidata.cache,
##               as.difftime(1,units="days")
##               )

fetch_epidata_by_issue = function(source.name, nonauth.args, auth.args,
                                  guaranteed.lag.for.regular.issues,
                                  irregular.issues = integer(0L),
                                  add_epiweek_lag = add_epiweek_integer,
                                  add_issue_int = add_epiweek_integer,
                                  is_auth_name = function(arg.names) arg.names=="auth",
                                  cache = epidata.cache,
                                  cache.invalidation.period = as.difftime(8, units="hours")
                                  ) {
    beginning.of.time.epiweek = 123401L
    end.of.time.epiweek = Date_to_epiweek(Sys.Date() + 365000L)
    regular.issues = fetch_epidata(source.name,
                                   c(nonauth.args, list(
                                                       epiweeks=Epidata[["range"]](beginning.of.time.epiweek,end.of.time.epiweek),
                                                       lag=guaranteed.lag.for.regular.issues
                                                   )),
                                   auth.args,
                                   cache,
                                   cache.invalidation.period,
                                   is_auth_name = is_auth_name
                                   ) %>>%
        (epidata) %>>%
        sapply(`[[`, "issue")
    known.issues = c(regular.issues, irregular.issues)
    ## xxx Epidata$range for irregular issues not allowed.

    fetches = known.issues %>>% lapply(function(known.issue) {
        fetch_epidata(source.name,
                      c(nonauth.args, list(
                                          issues=known.issue,
                                          fixme changes and breaks cache... need to have fixed end epiweek for issue.
                                          epiweeks=Epidata[["range"]](beginning.of.time.epiweek,end.of.time.epiweek)
                                      )),
                      auth.args,
                      cache,
                      as.difftime(Inf,units="days"),
                      is_auth_name = is_auth_name
                      )
    })

    sorted.known.issues = sort(known.issues)
    empty.issue.froms =
        c(add_epiweek_lag(beginning.of.time.epiweek, guaranteed.lag.for.regular.issues),
          add_issue_int(sorted.known.issues, 1L))
    empty.issue.tos =
        c(add_issue_int(sorted.known.issues, -1L),
          add_epiweek_lag(end.of.time.epiweek, guaranteed.lag.for.regular.issues))
    include.empty.issue.range =
        empty.issue.froms <= empty.issue.tos
    pruned.empty.issue.froms = empty.issue.froms[include.empty.issue.range]
    pruned.empty.issue.tos = empty.issue.tos[include.empty.issue.range]
    pruned.empty.issue.ranges = lapply(seq_along(pruned.empty.issue.froms), function(range.i) {
        Epidata[["range"]](pruned.empty.issue.froms[[range.i]], pruned.empty.issue.tos[[range.i]])
    })

    empty.response = fetch_epidata(source.name,
                                   c(nonauth.args, list(
                                                       issues=pruned.empty.issue.ranges,
                                                       epiweeks=Epidata[["range"]](beginning.of.time.epiweek,end.of.time.epiweek)
                                                   )),
                                   auth.args,
                                   cache,
                                   cache.invalidation.period,
                                   accept.empty.results=TRUE,
                                   is_auth_name = is_auth_name
                                   )
    if (!(
        identical(empty.response[["result"]], -2L) &&
        identical(empty.response[["message"]], "no results")
    )) {
        stop (paste0("Response that was supposed to be empty based on available issues was not empty; either the assumptions for finding available issues were wrong or there was a new issue added while this function was running.\nUnexpected/problematic issues: ",toString(sort(unique(sapply(empty.response[["epidata"]],`[[`,"issue")))),"\nResponse that should have been empty:\n",paste(capture.output(str(empty.response)),collapse="\n")))
    }

    return (fetches)
}

## fetch_epidata_by_issue(
##     "fluview",
##     list(regions="nat"),
##     ## list(regions="hhs1"),
##     ## list(regions="ca"),
##     list()
## ) %>>%
##     lapply(`[[`, "epidata") %>>%
##     dplyr::combine() %>>%
##     row_major_list_list_as_tbl() %>>%
##     {.}

## fixme if use hash, how to handle key conflicts?  do cache drivers need to also take key-objects in addition to key-names?

## todo generalize fetch_epidata?

## todo reorder epidata args for cache?
