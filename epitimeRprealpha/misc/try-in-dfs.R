
library("pipeR")

test.caldate = caldate_of("2020-01-01") + 0:4

data.frame(date=test.caldate)

tibble::tibble(date=test.caldate)

data.table::data.table(date=test.caldate)
