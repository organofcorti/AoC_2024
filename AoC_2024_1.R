#1. match order of lists, measure distance between them, sum total distance.

library(data.table)
library(plyr)
library(dplyr)


lists_dt <- fread("1/lists.txt")

## part 1
list_1_sorted <- lists_dt[, V1] %>% sort 
list_2_sorted <- lists_dt[, V2] %>% sort
list_distance <-  (list_1_sorted - list_2_sorted) %>% abs %>% sum
list_distance

# part 2
# how often each number from the left list appears in the right list
# Calculate a total similarity score by adding up each number in the left list after multiplying it by the number of times that number appears in the right list

left_side_dt <- lists_dt[, list(V1)]
right_side_dt <- lists_dt[, list(V1=V2)]
left_in_right <- semi_join(left_side_dt, right_side_dt, by="V1")
list_distance_part_2 <- right_side_dt[which(right_side_dt[, V1]%in%left_in_right[, V1])][, .N, by=V1][, V1*N] %>% sum
list_distance_part_2