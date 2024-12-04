# safe if
# The levels are either all increasing or all decreasing.
# Any two adjacent levels differ by at least one and at most three.


library(data.table)
library(plyr)
library(dplyr)


lists_dt <- fread("2/lists.txt", fill=T)

## apply safety logic iteratively (ugh, already?)
safe_unsafe <- apply(lists_dt, 1, function(x) {
  num_diffs <- diff(na.omit(x))
  ## all diffs have to be increasing or decreasing
  ## all diffs must be 1, 2 or 3
  if((all(num_diffs > 0) | all(num_diffs < 0)) & all(abs(num_diffs) %in% 1:3)){
    return("SAFE")} else {return("UNSAFE")}
}) 

which(safe_unsafe == "SAFE" )%>% length
which(safe_unsafe == "UNSAFE" )%>% length
safe_reports <- lists_dt[which(safe_unsafe == "SAFE" ), .N]
safe_reports

#### part 2
### Now, the same rules apply as before, except if removing a single level from an unsafe report
### would make it safe, the report instead counts as safe.


## 1. create 8 data tables each with one column missing 
all_variations <- sapply(1:ncol(lists_dt), function(x) lists_dt[, -..x], simplify=F)

## check safe logic
safe_unsafe_test_fun <- function(x) {
  num_diffs <- diff(na.omit(x))
  ## all diffs have to be increasing or decreasing
  ## all diffs must be 1, 2 or 3
  if((all(num_diffs > 0) | all(num_diffs < 0)) & all(abs(num_diffs) %in% 1:3)){
    return("SAFE")} else {return("UNSAFE")}
}

## Iterate (ugh) over each 
all_vars_safety_check_list_0 <- lapply(all_variations, function(x) apply(x, 1, safe_unsafe_test_fun))
all_vars_safety_check_list_1 <- lapply(all_vars_safety_check_list_0, function(x) data.table(1:length(x), x)) 

### add in original data
safe_unsafe_dt  <- data.table(V1=1:length(safe_unsafe), x=safe_unsafe)
all_vars_safety_check_list_1[[9]] <- safe_unsafe_dt


check_safety <- all_vars_safety_check_list_1 %>% rbindlist

safe_reports_part_2 <- check_safety[x=="SAFE"][, .N, by=V1][order(V1), .N]
safe_reports_part_2

