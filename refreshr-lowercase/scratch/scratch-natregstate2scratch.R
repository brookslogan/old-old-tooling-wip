## flusight.season.atoms =
##     tibble::tibble(season=2010:2019) %>>%
##     dplyr::rowwise() %>>%
##     dplyr::do(
##            ) %>>%
##     {.}

## flusight.location.hierarchy =
##     dplyr::bind_rows(
##                tibble::tibble(
##                            tier = "hhs",
##                            child = ........,
##                            parent = "HHS Region 1"
##                        )
##            )

