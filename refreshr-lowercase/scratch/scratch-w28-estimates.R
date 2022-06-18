source("../scratch/scratch-natregstate.R", chdir=TRUE)

uiop0 =
    natregstate.raw.fluview.history.dfs[c("US National",paste0("HHS Region ",1:10))] %>>%
    dplyr::bind_rows(.id="Location") %>>%
    dplyr::mutate(Location=factor(Location, c("US National",paste0("HHS Region ",1:10)))) %>>%
    epiforecast::augmentWeeklyDFWithTimingSynonyms(epiforecast::usa.flu.first.week.of.season) %>>%
    dplyr::filter(!is.na(wili)) %>>%
    dplyr::filter(!dplyr::between(week,21L,39L)) %>>%
    dplyr::group_by(Location, epiweek) %>>%
    dplyr::summarize(
               ground.truth = wili[issue==epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)][1L],
               w20 = wili[issue==epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::prev_or_curr_week(20L)][1L],
               ## w40 = wili[issue==epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::curr_or_next_week(40L)][1L],
               ## w4040 = wili[issue==epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)][1L],
               ## i404052 = epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(52L)
               wge40 = wili[issue>=epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::curr_or_next_week(40L)][1L],
               wge404052 = wili[issue>=epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(52L)][1L]
               ## w404052 = wili[issue==epiweek[[1L]]%>>%epiforecast:::prev_or_curr_week(31L)%>>%epiforecast:::curr_or_next_week(28L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(41L)%>>%epiforecast:::curr_or_next_week(40L)%>>%epiforecast:::curr_or_next_week(52L)][1L]
           ) %>>%
    dplyr::ungroup() %>>%
    {.}
uiop = uiop0 %>>%
    na.omit() %>>%
    dplyr::group_by(Location) %>>%
    dplyr::summarize(
               w20mae = mean(abs(ground.truth-w20)),
               wge40mae = mean(abs(ground.truth-wge40)),
               ## w40mae = mean(abs(ground.truth-w40)),
               ## w4040mae = mean(abs(ground.truth-w4040)),
               wge404052mae = mean(abs(ground.truth-wge404052)),
               w20q90ae = quantile(abs(ground.truth-w20), 0.9),
               wge40q90ae = quantile(abs(ground.truth-wge40), 0.9),
               ## w40q90ae = quantile(abs(ground.truth-w40), 0.9),
               ## w4040q90ae = quantile(abs(ground.truth-w4040), 0.9),
               w404052q90ae = quantile(abs(ground.truth-wge404052), 0.9),
               n = dplyr::n()
           ) %>>%
    dplyr::ungroup() %>>%
    {.}
uiop

natregstate.raw.fluview.history.dfs[c("US National",paste0("HHS Region ",1:10))] %>>%
    dplyr::bind_rows(.id="Location") %>>%
    dplyr::mutate(Location=factor(Location, c("US National",paste0("HHS Region ",1:10)))) %>>%
    epiforecast::augmentWeeklyDFWithTimingSynonyms(41L) %>>%
    ## dplyr::filter(season >= 2010L, season <= 2018L) %>>%
    dplyr::filter(season >= 2010L, season <= 2017L) %>>%
    dplyr::group_by(Location, season) %>>%
    dplyr::summarize(
               has.w20 = any(issue==(season[[1L]]+1L)*100L+20L),
               has.w28 = any(issue==(season[[1L]]+1L)*100L+28L),
               has.any.w2939 = any(dplyr::between(issue,(season[[1L]]+1L)*100L+29L,(season[[1L]]+1L)*100L+39L)),
               has.w40 = any(issue==(season[[1L]]+1L)*100L+40L)
           ) %>>%
    dplyr::ungroup() %>>%
    (~ avail1 <- .) %>>%
    dplyr::group_by(Location) %>>%
    ## dplyr::summarize_at(dplyr::vars(dplyr::starts_with("has")), sum) %>>%
    dplyr::count(has.w20, has.w28, has.any.w2939, has.w40) %>>%
    dplyr::ungroup() %>>%
    (~ avail2 <- .) %>>%
    {.}
