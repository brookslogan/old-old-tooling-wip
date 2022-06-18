row_major_list_list_as_tbl_1 = function(list.list) {
  if (!is.list(list.list)) {
    stop ("Expected =list.list= to be a list (containing entries corresponding to rows).")
  }
  lapply(list.list, function(entry) {
    if (!is.list(entry)) {
      stop ("Expected each entry in =list.list= to be a list (containing elements corresponding to values for each field).")
    }
    ## Convert this entry from a list to a tibble with a single row, removing
    ## any NULL elements. Rely on dplyr::bind_rows to deal with mismatches
    ## between columns included in the different rows. (If a field is NULL in
    ## every entry in which it appears, there will be no corresponding column in
    ## the combined tibble. Otherwise, missing values will be filled in based on
    ## the type of the column.)
    row = entry %>>%
      ## Remove NULL's:
      Filter(f=Negate(is.null)) %>>%
      ## Ensure that all complicated elt's within the entry have been
      ## represented using list-type columns:
      lapply(function(elt) {
        if (length(elt) != 1L) {
          list(elt)
        } else {
          elt
        }
      }) %>>%
      tibble::as_tibble()
    stopifnot(nrow(row)==1L)
    row
  }) %>>%
    ## Combine the rows into a larger tibble.
    dplyr::bind_rows()
}

row_major_list_list_as_tbl_2 = function(list.list) {
  col.major.df.of.lists = as.data.frame(t(sapply(list.list, identity)))
  df = col.major.df.of.lists %>>%
    lapply(function(col.as.list) {
      col.as.list[sapply(col.as.list, is.null)] <- NA
      do.call(c, col.as.list)
    }) %>>%
    as.data.frame(check.names=FALSE, stringsAsFactors=FALSE)
  tbl = df %>>% tibble::as_tibble()
  return (tbl)
}

row_major_list_list_as_tbl = row_major_list_list_as_tbl_2
