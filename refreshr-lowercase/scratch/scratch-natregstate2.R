
source("../scratch/scratch-epicache.R", chdir=TRUE)

ilinet.location.info =
    dplyr::bind_rows(
               tibble::tibble(
                           Location = "US National",
                           epidata.code = "nat",
                           tier = "national",
                           is.public.on.fluview = TRUE,
                           first.observed.cdcseason = 1997L,
                           first.recorded.issue = 200347L
                       ),
               tibble::tibble(
                           Location = paste0("HHS Region ",1:10),
                           epidata.code = paste0("hhs",1:10),
                           tier = "hhs",
                           is.public.on.fluview = TRUE,
                           first.observed.cdcseason = 1997L,
                           first.recorded.issue = 200949L
                       ),
               tibble::tibble(
                           Location =
                               c(state.name,
                                 "District of Columbia", "Puerto Rico", "Virgin Islands", "New York City"
                                 )[c(1:8,51L,9:50,52:54)], # place DC in alphabetized part
                           epidata.code =
                               c(state.abb %>>% tolower() %>>% dplyr::recode("ny"="ny_minus_jfk"),
                                 "dc", "pr", "vi", "jfk"
                                 )[c(1:8,51L,9:50,52:54)], # place DC in alphabetized part
                           tier = "state",
                           is.public.on.fluview = ! epidata.code %in% c("fl"),
                           first.observed.cdcseason =
                               dplyr::recode(epidata.code,
                                             "pr" = 2013L,
                                             "vi" = 2015L,
                                             .default = 2010L
                                             ),
                           first.recorded.issue = 201740L
                       )
           )

ilinet.aggregate.location.df =
    dplyr::bind_rows(
               tibble::tibble(
                           "aggregate.tier" = "national",
                           "aggregate.code" = "nat",
                           "part.code" = list(paste0("hhs",1:10))
                       ),
               tibble::enframe(
                           list(
                               "hhs1" = c('ct', 'ma', 'me', 'nh', 'ri', 'vt'),
                               "hhs2" = c('jfk', 'nj', 'ny_minus_jfk', 'pr', 'vi'),
                               "hhs3" = c('dc', 'de', 'md', 'pa', 'va', 'wv'),
                               "hhs4" = c('al', 'fl', 'ga', 'ky', 'ms', 'nc', 'sc', 'tn'),
                               "hhs5" = c('il', 'in', 'mi', 'mn', 'oh', 'wi'),
                               "hhs6" = c('ar', 'la', 'nm', 'ok', 'tx'),
                               "hhs7" = c('ia', 'ks', 'mo', 'ne'),
                               "hhs8" = c('co', 'mt', 'nd', 'sd', 'ut', 'wy'),
                               "hhs9" = c('az', 'ca', 'hi', 'nv'),
                               "hhs10" = c('ak', 'id', 'or', 'wa')
                           ),
                           "aggregate.code", "part.code"
                       ) %>>%
               tibble::add_column("tier" = "hhs", .before=1L)
           ) %>>%
    tidyr::unnest(part.code) %>>%
    tibble::add_column("aggregate.Location" = ilinet.location.info[["Location"]][match(.[["aggregate.code"]], ilinet.location.info[["epidata.code"]])]) %>>%
    tibble::add_column("part.Location" = ilinet.location.info[["Location"]][match(.[["part.code"]], ilinet.location.info[["epidata.code"]])]) %>>%
    {.}
## todo census regions

ilinet_observed_locations = function(season) {
    ilinet.location.info %>>%
        dplyr::filter(season >= first.observed.cdcseason)
}

ilinet_observed_location_parts = function(aggregate.Locations, season) {
    tibble::tibble(aggregate.Location = aggregate.Locations) %>>%
        dplyr::left_join(ilinet.aggregate.location.df, by="aggregate.Location") %>>%
        dplyr::left_join(ilinet.location.info, by=c("part.Location"="Location")) %>>%
        dplyr::filter(season >= first.observed.cdcseason) %>>%
        dplyr::select(aggregate.Location, part.Location) %>>%
        tidyr::nest(part.Location, .key="part.Locations") %>>%
        tibble::deframe()
}





{
    Rprof("epicache-natregstate.Rprof.out")
    ilinet.raw.history.dfs =
        ilinet.location.info %>>%
        dplyr::filter(is.public.on.fluview) %>>%
        dplyr::rowwise() %>>%
        dplyr::mutate(data = {
            ## todo also cache this function
            print(epidata.code)
            fetch_epidata_by_issue(
                "fluview",
                list(regions=epidata.code),
                list(),
                if (tier=="state") 30L
                else 0L
            ) %>>%
                lapply(`[[`, "epidata") %>>%
                dplyr::combine() %>>%
                row_major_list_list_as_tbl() %>>%
                list() %>>%
                {.}
        }) %>>%
        dplyr::ungroup() %>>%
        {stats::setNames(.[["data"]], .[["Location"]])} %>>%
        {.}
    Rprof(NULL)
    summaryRprof("epicache-natregstate.Rprof.out")
}



## ilinet.raw.history.dfs %>>%
##     dplyr::bind_rows(.id="Location") %>>%
##     dplyr::mutate(Location = factor(Location, ilinet.location.info[["Location"]])) %>>%
##     dplyr::group_by(epiweek, issue) %>>%
##     dplyr::do(missing.Locations = setdiff(
##                   ilinet_observed_locations(
##                       .[["epiweek"]][[1L]] %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)
##                   ),
##                   .[["Location"]]
##               )) %>>%
##     dplyr::ungroup() %>>%
##     {.}

ilinet.tier.known.epiweek.issue.dfs =
    ilinet.raw.history.dfs %>>%
    tibble::enframe("Location", "raw.history.df") %>>%
    dplyr::left_join(ilinet.location.info, by="Location") %>>%
    dplyr::group_by(tier) %>>%
    dplyr::do(known.epiweek.issue.df =
                  dplyr::bind_rows(.[["raw.history.df"]]) %>>%
                  dplyr::distinct(epiweek, issue) %>>%
                  dplyr::mutate(epiweek.cdcseason = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
                  {.}
              ) %>>%
    dplyr::ungroup() %>>%
    tibble::deframe() %>>%
    {.}

ilinet.missing.raw.history.rows =
    ilinet.raw.history.dfs %>>%
    tibble::enframe("Location", "raw.history.df") %>>%
    dplyr::left_join(ilinet.location.info, by="Location") %>>%
    dplyr::rowwise() %>>%
    dplyr::summarize(
               Location = Location,
               missing.epiweek.issue.df =
                   tier.known.epiweek.issue.dfs[[tier]] %>>%
                   dplyr::filter(epiweek.cdcseason >= first.observed.cdcseason) %>>%
                   dplyr::filter(issue >= first.recorded.issue) %>>%
                   dplyr::anti_join(raw.history.df, by=c("epiweek","issue")) %>>%
                   list()
           ) %>>%
    dplyr::ungroup() %>>%
    tidyr::unnest(missing.epiweek.issue.df) %>>%
    ## (missing.epiweek.issue.df) %>>%
    print(n=100L)
    ## {.}



## missing.epiweek.issue.rows %>>%
##     dplyr::left_join(
##                ilinet.aggregate.location.df %>>%
##                dplyr::filter(tier == "hhs"),
##                by=c("Location"="part.Location")
##            )

## ilinet.aggregate.location.df %>>%
##     dplyr::filter(tier == "hhs") %>>%
##     dplyr::
##     {.}

## ilinet.raw.history.dfs %>>%
##     tibble::enframe("Location", "raw.history.df") %>>%
##     dplyr::left_join(ilinet.location.info, by="Location") %>>%
##     dplyr::group_by(tier) %>>%
##     dplyr::do(known.epiweek.issue.df =
##                   dplyr::bind_rows(.[["raw.history.df"]]) %>>%
##                   dplyr::distinct(epiweek, issue) %>>%
##                   dplyr::mutate(epiweek.cdcseason = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
##                   {.}
##               ) %>>%
##     dplyr::ungroup() %>>%
##     tibble::deframe() %>>%
##     {.}

## ilinet.aggregate.location.df %>>%
##     dplyr::filter(tier == "hhs") %>>%
##     dplyr::group_by(aggregate.Location) %>>%
##     dplyr::do(
##                asdf =
##                    ilinet.raw.history.dfs[part.Location] %>>%
##                    Reduce(f=dplyr::)
##            ) %>>%
##     dplyr::ungroup()
##     {.}

## tibble::tibble(
##             L = c("a","b"),
##             d = list(1:2,3:4)
##         ) %>>%
##     tidyr::spread(L, d) %>>%
##     {.}

ilinet.integer.colnames =
    colnames(ilinet.raw.history.dfs[[1L]]) %>>%
    {.[grepl("^num", .)]}

## ilinet.raw.history.dfs[c("HHS Region 2","New York","New York City","New Jersey","Puerto Rico","Virgin Islands")] %>>%
##     dplyr::bind_rows(.id="Location") %>>%
##     dplyr::rowwise() %>>%
##     dplyr::mutate(count.data = )

ilinet.raw.history.dfs[c("HHS Region 2","New York","New York City","New Jersey","Puerto Rico","Virgin Islands")] %>>%
    lapply(function(df) df %>>%
                        dplyr::filter(epiweek==201143L, issue==201740L)
                        ## dplyr::filter(epiweek==201243L, issue==201740L)
                        ## dplyr::filter(epiweek==201343L, issue==201740L)
                        ## dplyr::filter(epiweek==201443L, issue==201740L)
                        ## dplyr::filter(epiweek==201543L, issue==201740L)
           ) %>>%
    dplyr::bind_rows(.id="Location") %>>%
    dplyr::mutate(tier = dplyr::if_else(Location=="HHS Region 2","hhs","state")) %>>%
    dplyr::group_by(tier) %>>%
    dplyr::summarize(num_patients = sum(num_patients),
                     codes = toString(ilinet.location.info[["epidata.code"]][match(Location, ilinet.location.info[["Location"]])])) %>>%
    dplyr::ungroup() %>>%
    dplyr::mutate(diff=diff(num_patients)) %>>%
    {.}

## ilinet.raw.history.dfs %>>%
##     dplyr::bind_rows(.id="Location") %>>%
##     dplyr::mutate(Location = factor(Location, ilinet.location.info[["Location"]])) %>>%
##     {
##         tibble::add_column(.[!names(.) %in% ilinet.integer.colnames],
##                            integer.dat = as.matrix(.[ilinet.integer.colnames]))
##     } %>>%
##     dplyr::select(-wili, -ili) %>>%
##     tidyr::spread(Location, integer.dat) %>>%
##     {.}

## library("rlang")

## ilinet.raw.history.dfs %>>%
##     dplyr::bind_rows(.id="Location") %>>%
##     dplyr::mutate(Location = factor(Location, ilinet.location.info[["Location"]])) %>>%
##     tidyr::nest(!!!names(.)[grep("^num",names(.))]) %>>%
##     {.}


## todo instead just go through every geo relation and check when possible, fill in when possible?
## xxx can maybe estimate weights more quickly by factoring in cen and hhs together
