
source("../scratch/scratch-epicache.R", chdir=TRUE)

flusight.location.info =
    tibble::tibble(
                flusight.name =
                    c("US National",
                      paste0("HHS Region ",1:10),
                      c(state.name,
                        "District of Columbia", "Puerto Rico", "Virgin Islands", "New York City"
                        )[c(1:8,51L,9:50,52:54)] # place DC in alphabetized part
                      ),
                epidata.code =
                    c("nat",
                      paste0("hhs",1:10),
                      c(state.abb %>>% tolower() %>>% dplyr::recode("ny"="ny_minus_jfk"),
                        "dc", "pr", "vi", "jfk"
                        )[c(1:8,51L,9:50,52:54)] # place DC in alphabetized part
                      ),
                geography.level =
                    c("national",
                      rep("hhs", 10L),
                      rep("state", 54L)
                      ),
                is.atomic =
                    c(FALSE,
                      rep(FALSE, 10L),
                      rep(TRUE, 54L)
                      ),
                is.public.on.fluview =
                    ! epidata.code %in% c("fl"),
                constituent.atoms =
                    c(list(epidata.code[is.atomic]),
                      list(
                          c('ct', 'ma', 'me', 'nh', 'ri', 'vt'),
                          c('jfk', 'nj', 'ny_minus_jfk', 'pr', 'vi'),
                          c('dc', 'de', 'md', 'pa', 'va', 'wv'),
                          c('al', 'fl', 'ga', 'ky', 'ms', 'nc', 'sc', 'tn'),
                          c('il', 'in', 'mi', 'mn', 'oh', 'wi'),
                          c('ar', 'la', 'nm', 'ok', 'tx'),
                          c('ia', 'ks', 'mo', 'ne'),
                          c('co', 'mt', 'nd', 'sd', 'ut', 'wy'),
                          c('az', 'ca', 'hi', 'nv'),
                          c('ak', 'id', 'or', 'wa')
                          ),
                      as.list(epidata.code[is.atomic])
                    )
                )

{
    Rprof("epicache-natregstate.Rprof.out")
    natregstate.raw.fluview.history.dfs =
        flusight.location.info %>>%
        dplyr::filter(is.public.on.fluview) %>>%
        dplyr::rowwise() %>>%
        dplyr::mutate(data = {
            ## todo also cache this function
            print(epidata.code)
            fetch_epidata_by_issue(
                "fluview",
                list(regions=epidata.code),
                list(),
                if (geography.level=="state") 30L
                else 0L
            ) %>>%
                lapply(`[[`, "epidata") %>>%
                dplyr::combine() %>>%
                row_major_list_list_as_tbl() %>>%
                list() %>>%
                {.}
        }) %>>%
        dplyr::ungroup() %>>%
        {stats::setNames(.[["data"]], .[["flusight.name"]])} %>>%
        {.}
    Rprof(NULL)
}

summaryRprof("epicache-natregstate.Rprof.out")

## Epidata$fluview("ct", Epidata$range(123401,345601),
## ## Epidata$fluview("ca", Epidata$range(123401,345601),
##                 201823
##                 ## 201834
##                 ## 201929
##                 ## 201932
##                 ) %>>%

## Epidata$fluview(epiweeks=Epidata$range(123401,345601),
##                 regions="ms",
##                 ## issues=201748L
##                 ## issues=201749L
##                 issues=201750L
##                 ) %>>%
##     (epidata) %>>%
##     row_major_list_list_as_tbl() %>>%
##     dplyr::arrange(lag)

natregstate.raw.fluview.history.dfs %>>%
    dplyr::bind_rows(.id="location") %>>%
    dplyr::group_by(issue, epiweek) %>>%
    ## dplyr::summarize(nlocs=dplyr::n()) %>>%
    dplyr::summarize(locs=toString(sort(location))) %>>%
    dplyr::ungroup() %>>%
    ## dplyr::group_by(nlocs) %>>%
    dplyr::group_by(locs) %>>%
    dplyr::summarize(count=dplyr::n()) %>>%
    dplyr::ungroup() %>>%
    dplyr::group_by(locs) %>>%
    dplyr::mutate(loc.list=strsplit(locs,", ")) %>>%
    dplyr::mutate(missing.loc.list = list(setdiff(flusight.location.info$flusight.name,loc.list[[1L]]))) %>>%
    dplyr::ungroup() %>>%
    (missing.loc.list) %>>%
    {.}

flusight.location.info %>>%
    dplyr::count(geography.level)

natregstate.raw.fluview.history.dfs %>>%
    dplyr::bind_rows(.id="location") %>>%
    dplyr::group_by(issue, epiweek) %>>%
    dplyr::filter(! "Puerto Rico" %in% location && ! "Virgin Islands" %in% location && "Tennessee" %in% location) %>>%
    dplyr::ungroup() %>>%
    dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    ## dplyr::count(epiweek.season, issue.season) %>>%
    ## dplyr::count(epiweek.season, issue.season, lag) %>>%
    dplyr::group_by(epiweek.season, issue.season) %>>%
    dplyr::summarize(n=dplyr::n(), minlag=min(lag), maxlag=max(lag)) %>>%
    dplyr::ungroup() %>>%
    ## dplyr::distinct(issue,epiweek) %>>%
    ## dplyr::distinct(issue) %>>%
    {.}

natregstate.raw.fluview.history.dfs %>>%
    dplyr::bind_rows(.id="location") %>>%
    dplyr::group_by(issue, epiweek) %>>%
    dplyr::filter("Puerto Rico" %in% location && ! "Virgin Islands" %in% location) %>>%
    dplyr::ungroup() %>>%
    dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    ## dplyr::count(epiweek.season, issue.season) %>>%
    ## dplyr::count(epiweek.season, issue.season, lag) %>>%
    dplyr::group_by(epiweek.season, issue.season) %>>%
    dplyr::summarize(n=dplyr::n(), minlag=min(lag), maxlag=max(lag)) %>>%
    dplyr::ungroup() %>>%
    ## dplyr::distinct(issue,epiweek) %>>%
    ## dplyr::distinct(issue) %>>%
    {.}

natregstate.raw.fluview.history.dfs %>>%
    dplyr::bind_rows(.id="location") %>>%
    dplyr::group_by(issue, epiweek) %>>%
    dplyr::filter(issue %in% c(201740L, 201741L)) %>>%
    ## dplyr::filter(! "Puerto Rico" %in% location && ! "Virgin Islands" %in% location && "Tennessee" %in% location) %>>%
    ## dplyr::filter("Puerto Rico" %in% location && "Virgin Islands" %in% location && "Tennessee" %in% location) %>>%
    dplyr::ungroup() %>>%
    dplyr::group_by(issue, epiweek) %>>%
    ## dplyr::summarize(num_patients_prvi = num_patients[[match(c("hhs2"),region)]] - sum(num_patients[match(c("ny_minus_jfk","jfk","nj"),region)])) %>>%
    dplyr::summarize(num_providers_prvi = num_providers[[match(c("hhs2"),region)]] - sum(num_providers[match(c("ny_minus_jfk","jfk","nj"),region)])) %>>%
    ## dplyr::summarize(zero = num_patients[[match(c("hhs2"),region)]] - sum(num_patients[match(c("ny_minus_jfk","jfk","nj","pr","vi"),region)])) %>>%
    dplyr::ungroup() %>>%
    ## dplyr::filter(num_patients_prvi != 0L) %>>%
    dplyr::filter(num_providers_prvi != 0L) %>>%
    ## dplyr::filter(zero != 0L) %>>%
    dplyr::arrange(epiweek, issue) %>>%
    print(n=1000L)
    ## {.}

natregstate.raw.fluview.history.dfs[["Puerto Rico"]] %>>% dplyr::filter(issue==201740L, epiweek <= 201740L) %>>% dplyr::arrange(-epiweek)
natregstate.raw.fluview.history.dfs[["Puerto Rico"]] %>>% dplyr::filter(issue==201741L, epiweek <= 201740L) %>>% dplyr::arrange(-epiweek)
natregstate.raw.fluview.history.dfs[["Virgin Islands"]] %>>% dplyr::filter(issue==201740L, epiweek <= 201740L) %>>% dplyr::arrange(-epiweek)
natregstate.raw.fluview.history.dfs[["Virgin Islands"]] %>>% dplyr::filter(issue==201741L, epiweek <= 201740L) %>>% dplyr::arrange(-epiweek)

natregstate.raw.fluview.history.dfs[["Puerto Rico"]] %>>% (issue) %>>% unique() %>>% sort()
natregstate.raw.fluview.history.dfs[["Puerto Rico"]] %>>% (epiweek) %>>% unique() %>>% sort()
natregstate.raw.fluview.history.dfs[["Virgin Islands"]] %>>% (issue) %>>% unique() %>>% sort()
natregstate.raw.fluview.history.dfs[["Virgin Islands"]] %>>% (epiweek) %>>% unique() %>>% sort()

qwer =
    natregstate.raw.fluview.history.dfs %>>%
    dplyr::bind_rows(.id="location") %>>%
    dplyr::group_by(issue, epiweek) %>>%
    ## dplyr::filter("Puerto Rico" %in% location && "Virgin Islands" %in% location) %>>%
    dplyr::filter("Pennsylvania" %in% location) %>>%
    dplyr::ungroup() %>>%
    dplyr::select(location, issue, epiweek, wili) %>>%
    tidyr::spread(location, wili) %>>%
    ## dplyr::select(location, issue, epiweek, num_patients) %>>%
    ## tidyr::spread(location, num_patients) %>>%
    ## dplyr::mutate(season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    ## dplyr::group_by(season) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(31L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(31L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(21L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(21L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(23L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(23L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(29L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(29L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(31L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(31L,0L,3L)) %>>%
    dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(35L,0L,3L)) %>>%
    dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(35L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(39L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(39L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(40L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(41L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(41L,0L,3L)) %>>%
    ## dplyr::mutate(epiweek.season = epiweek %>>% epiweek_to_Date(0L) %>>% seasonOfDate(43L,0L,3L)) %>>%
    ## dplyr::mutate(issue.season = issue %>>% epiweek_to_Date(0L) %>>% seasonOfDate(43L,0L,3L)) %>>%
    ## dplyr::filter(!dplyr::between(epiweek%%100L, 21L,39L)) %>>%
    ## dplyr::filter(!dplyr::between(issue%%100L, 21L,39L)) %>>%
    ## dplyr::group_by(epiweek.season, issue.season) %>>%
    dplyr::group_by(epiweek.season) %>>%
    ## dplyr::group_by(issue.season) %>>%
    ## dplyr::do(coefs=lm(.$`HHS Region 2` ~ .$`New York` + .$`New York City` + .$`New Jersey` + .$`Puerto Rico` + .$`Virgin Islands` + 0) %>>% coef() %>>% tibble::enframe()) %>>%
    ## dplyr::do(coefs=lm(.$`HHS Region 2` ~ .$`New York` + .$`New York City` + .$`New Jersey` + .$`Puerto Rico` + .$`Virgin Islands` + 0) %>>% coef()) %>>%
    ## dplyr::do(fit=lm(`HHS Region 2` ~ `New York` + `New York City` + `New Jersey` + `Puerto Rico` + `Virgin Islands` + 0, .),
    dplyr::do(
               ## fit=lm(`HHS Region 3` ~ 0 + `Delaware` + `District of Columbia` + `Maryland` + `Pennsylvania` + `Virginia` + `West Virginia`, .),
               fit=lm(`HHS Region 5` ~ 0 + `Illinois` + `Indiana` + `Michigan` + `Minnesota` + `Ohio` + `Wisconsin`, .),
               ## fit=lm(`HHS Region 7` ~ 0 + `Iowa` + `Kansas` + `Missouri` + `Nebraska`, .),
               ## fit=lm(`US National` ~ 0 + `HHS Region 1` + `HHS Region 2` + `HHS Region 3` + `HHS Region 4` + `HHS Region 5` + `HHS Region 6` + `HHS Region 7` + `HHS Region 8` + `HHS Region 9` + `HHS Region 10`, .),
               dat=.) %>>%
    dplyr::rowwise() %>>%
    dplyr::mutate(coefs=list(coef(fit)),
                  ## rmse=sqrt(mean(residuals(fit)^2)),
                  q90=quantile(abs(residuals(fit)), 0.90),
                  ## q99=quantile(abs(residuals(fit)), 0.99),
                  maxabsres=max(abs(residuals(fit))),
                  n=length(residuals(fit)),
                  coefstring=toString(round(coef(fit),3L))) %>>%
    dplyr::ungroup() %>>%
    dplyr::select(-fit, -coefs, -dat, dplyr::everything()) %>>%
    ## dplyr::arrange(epiweek.season, issue.season) %>>%
    dplyr::arrange(epiweek.season) %>>%
    ## dplyr::arrange(issue.season) %>>%
    ## tidyr::unnest(coefs) %>>%
    ## as.list() %>>%
    {.}
qwer

## xxx what about Mariana Islands, Guam?
## todo make geo membership a function of time?
## xxx fit quantiles of natl and regional residuals from popwt prediction?
## xxx missingness indicator (potentially multi-way) interactions? multiple imputation?
## xxx HHS Region 3: weighting changes at 201540@201740 problematic no matter what?
## xxx don't need weighting for filling in CT for example... just work w/ counts...
