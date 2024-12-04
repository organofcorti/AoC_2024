### part 1
### find the correctly formatted "mul" functions
### then multiply them and then sum them

library(data.table)
library(plyr)
library(dplyr)
library(stringr)

## import
corr_data <- readLines("3/corrupted_data.txt")

## regex
mult_funs <- sapply(1:length(corr_data), function(x) str_extract_all(corr_data[x], pattern="mul\\([0-9]+,[0-9]+\\)")) %>% unlist

## convert mul to prod
prod_funs <- str_replace_all(mult_funs, "mul", "prod")

## calculate products and sum
product_sum <- sapply(prod_funs, function(x) eval(parse(text=x))) %>% sum
product_sum

## part 2: find do and don't
## regex
library(zoo)
do_dont_mul <- sapply(1:length(corr_data), function(x) str_extract_all(corr_data[x], pattern="(do\\(\\))|(don't\\(\\))|(mul\\([0-9]+,[0-9]+\\))")) %>% unlist
                      
## 
start_strs_index <- data.table(index = which(do_dont_mul == "do()"), instr = "start")
stop_strs_index <- data.table(index = which(do_dont_mul == "don't()"), instr = "stop")

instruct_index <- rbind(start_strs_index, stop_strs_index)[order(index)]

key_dt <- data.table(index= 1:length(do_dont_mul), instr=NA)

combined <- rbind(instruct_index, key_dt)[order(index)]
combined <- combined[, .SD[1], by=index]
combined <- cbind(combined, do_dont_mul)

if(is.na(combined[1,instr])) combined[1,instr := "start"]

# fill NA with last observation
combined[, instr := na.locf(instr)]
combined[, do_dont_prod_funs :=  str_replace_all(do_dont_mul, "mul", "prod")]
combined[instr=="start" & !grepl("do", do_dont_mul) , evaluation := eval(parse(text=do_dont_prod_funs)), by=index]

combined[!is.na(evaluation), sum(evaluation)]



