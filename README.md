
This contains WIP on tooling packages I was eventually hoping to use to replace
functionality in the ILI production forecasting system. The only progress made
was toward some very basic utilities rather than something like epiprocess and
epipredict. Something that did work was some faster-than-Date date calculations
and some generalized week calculations, with a naming scheme on related
functions to hint at possible values. Some work appears to have been lost, and
some capitalization issue appears to have confused cloud syncing. Things that
were in progress that I recall include:
- trying to get these custom classes to render correctly inside tibbles and
  data.tables, with some progress (which may not have made it through cloud
  sync), and trying to understand what license implications there would be
  referencing a generic from `pillars` or `vctrs`, as they were GPL-3 at the
  time, especially given that we had some GPL-2-only packages (although this
  might not have been an issue, as tidyverse GPL-3 packages were already used by
  more permissively-licensed tidyverse packages). Now that these and other
  tidyverse packages have been changed to a more permissive license, these
  worries may be simplified or eliminated
- expanding supported arithmetic between classes
- due to possible mixups between +1 and +7 when dealing with week-related
  classes, making special classes that users would have to use to do this type
  of arithmetic on objects representing time intervals (caldates, calweeks,
  etc.). IIRC, lubridate periods did not quite seem to do the trick, or there
  was fear of more license issues
- (new issue: other fast date classes like IDate in data.table, maybe others...)
- in caching functions, whether to use a last-update timestamp vs. a lease-type
  state, and maybe some other efficiency or interface issues that might have
  required starting somewhat from scratch
- (not in this repo) testing out implementing some things in Rust using rustinr,
  which might have had some smart marshalling or SEXP data structure implemented
  (involving more potential license issues), but which now archived and points
  to other projects
- an iterator library-like way of representing sequences, finite and infinite
  repetitions, etc., in vectors and maybe in data frames, matrices, etc.,
  compactly; some part of this has already been implemented in base R via
  ALTREPs. These would be useful in more cleanly/elegantly implementing caldate
  internals.

There are/were some issues with running this on current versions of R (e.g.,
POSIXlt objects no longer allowing use of double brackets to access information
--- IIRC the cross-version-compatible accessor to use is `$` rather than `[[` or
`[`, and getting S3 to work; these have now been hastily tackled in
epitimeRprealpha).
