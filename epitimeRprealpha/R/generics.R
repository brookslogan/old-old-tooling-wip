
## xxx as_cal* functions?
calyear_of = function(obj, ...) UseMethod("calyear_of")
calweek_of = function(obj, ...) UseMethod("calweek_of")
caldate_of = function(obj, ...) UseMethod("caldate_of")
caldate_of_ymd = function(obj) UseMethod("caldate_of_ymd")
caldate_of_ywwd0 = function(obj, ...) UseMethod("caldate_of_ywwd0")
caldate_of_ywwd7 = function(obj, ...) UseMethod("caldate_of_ywwd7")

year_of = function(obj) UseMethod("year_of")
yday0_of = function(obj) UseMethod("yday0_of")
yday1_of = function(obj) UseMethod("yday1_of")
mon0_of = function(obj) UseMethod("mon0_of")
mon1_of = function(obj) UseMethod("mon1_of")
mday0_of = function(obj) UseMethod("mday0_of")
mday1_of = function(obj) UseMethod("mday1_of")
week0_of = function(obj, ...) UseMethod("week0_of")
week1_of = function(obj, ...) UseMethod("week1_of")
wday0_of = function(obj) UseMethod("wday0_of")
wday7_of = function(obj) UseMethod("wday7_of")
ymd_df_of = function(obj, ...) UseMethod("ymd_of")
ywwd0_df_of = function(obj, ...) UseMethod("ywwd0_of")
ywwd7_df_of = function(obj, ...) UseMethod("ywwd7_of")

## xxx make ymd, ywwd0, ywwd7 classes?
## xxx split ymd, ywwd0, ywwd7 methods into explicit ones for single integer vs. df vs. ...?

n0_of_same_wday_in_year_up_to = function(obj) UseMethod("n0_of_same_wday_in_year_up_to ")
n1_of_same_wday_in_year_up_to = function(obj) UseMethod("n1_of_same_wday_in_year_up_to ")

advance = function(obj, amt) UseMethod("advance")

## advance by n intervals, ...
## (cal)year to calweeks, ...
## subtract...
## starting_caldate_of
## owning_caldate_of

## todo "of" vs. "as" vs. "to" vs. ...; of for extracting strict subset of information vs. full conversion and/or any addition of information? or....
