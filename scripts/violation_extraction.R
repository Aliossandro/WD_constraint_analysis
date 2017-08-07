library(chron)
library(lubridate)
library(plyr)
library(xts)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(data.table)
library(zoo)
library(dplyr)
library(stringr)
library(forecast)

setwd('~/Documents/PhD/wikidata_scripts/')

### ADD FILE NAMES WITH EXTRACTED VIOLATIONS
constr_viol <- read.csv('extracted_ptalk/wikidatawiki-20160203-pages-meta-history4-violations1.csv',  sep = '\t', quote = "", allowEscapes = T,   row.names = NULL, 
                            header = F, stringsAsFactors = FALSE, encoding = "UTF-8")


#clean timestamp
constr_viol$V2 <- gsub("Z", "", constr_viol$V2)
constr_viol$V2 <- gsub("T", " ", constr_viol$V2)

constr_viol_clean <- constr_viol[grep('^201[0-9]', constr_viol$V2),]
constr_viol_clean$timestamp <- as.POSIXct(constr_viol_clean$V2, format = "%Y-%m-%d %H:%M:%S", origin="1970-01-01")

#month data
constr_viol_clean$month <- as.yearmon(constr_viol_clean$timestamp)

### Extract violation number from reports
constr_viol_clean$violations <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\s\\".{1,22}\\"\\sviolations\\s\\=\\=Violations count:\\s)[0-9]{1,}(?=\\*\\s)')))))})
constr_viol_clean$violations <- as.numeric(constr_viol_clean$violations)

constr_viol_clean$item_count <- sapply(constr_viol_clean$V5, function(x){as.numeric(unlist(str_extract_all(x, perl('(?<=\\|item count=)[0-9]{1,}'))))})
constr_viol_clean$item_count <- as.numeric(constr_viol_clean$item_count)

constr_viol_clean$V1 <- gsub("Wikidata:Database reports/Constraint violations/", "", constr_viol_clean$V1)

# SIngle violation types
constr_viol_clean$unique_value <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Unique\\svalue\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$unique_value <- as.numeric(constr_viol_clean$unique_value)

constr_viol_clean$one_of <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"One\\sof\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$one_of <- as.numeric(constr_viol_clean$one_of)

constr_viol_clean$conflict_with <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Conflicts\\swith\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$conflict_with <- as.numeric(constr_viol_clean$conflict_with)

constr_viol_clean$self_link <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Self\\slink\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$self_link <- as.numeric(constr_viol_clean$self_link)

item_prop_funct <- function(x){
  list_viol <- str_match_all(x, '((\\"Item\\s\\{\\{[P|Q]\\|[0-9]{1,}\\}\\}\\s\\=\\s\\{\\{[P|Q]\\|[0-9]{1,}\\}\\})|(\\"Item\\s\\{\\{[P|Q]\\|[0-9]{1,}\\}\\})\\"\\sviolations\\s\\=\\=Violations\\scount\\:\\s)[0-9]{1,}')
  list_extracted <- str_extract_all(list_viol, perl('(?<=Violations count:\\s)[0-9]{1,}'))
  viol_count <- sum(as.numeric(unlist(list_extracted)))
  }


constr_viol_clean$item_prop <- sapply(constr_viol_clean$V5, function(y){item_prop_funct(y)})

constr_viol_clean$item_prop <- as.numeric(constr_viol_clean$item_prop)


constr_viol_clean$single_value <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Single\\svalue\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$single_value <- as.numeric(constr_viol_clean$single_value)

type_funct <- function(x){
  list_viol <- str_match_all(x, '(\\"Type\\s\\{\\{[Q]\\|[0-9]{1,}\\}\\}\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')
  list_extracted <- str_extract_all(list_viol, perl('(?<=Violations count:\\s)[0-9]{1,}'))
  viol_count <- sum(as.numeric(unlist(list_extracted)))
}

constr_viol_clean$type <- sapply(constr_viol_clean$V5, function(y){type_funct(y)})
constr_viol_clean$type <- as.numeric(constr_viol_clean$type)

# constr_viol_clean$type <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Type\\s\\{\\{[Q]\\|[0-9]{1,}\\}\\}\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})

# regmatches(constr_viol_clean[301,], m)

t_req_claim_funct <- function(x){
  list_viol <- str_match(x, '\\"Target required claim\\s\\{\\{[P]\\|[0-9]{1,}\\}\\}\\"\\sviolations\\s\\=\\=Violations count\\:\\s[0-9]{1,}')
  list_extracted <- str_extract_all(list_viol, perl('(?<=Violations count:\\s)[0-9]{1,}'))
  viol_count <- sum(as.numeric(unlist(list_extracted)))
}

constr_viol_clean$target_req_claim <- sapply(constr_viol_clean$V5, function(x){t_req_claim_funct(x)})
constr_viol_clean$target_req_claim <- as.numeric(constr_viol_clean$target_req_claim)


constr_viol_clean$inverse <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Inverse\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$inverse <- as.numeric(constr_viol_clean$inverse)

value_type_funct <- function(x){
  list_viol <- str_match(x, '\\"Value\\stype\\s\\{\\{[Q]\\|[0-9]{1,}\\}\\}\\"\\sviolations\\s\\=\\=Violations count\\:\\s[0-9]{1,}')
  list_extracted <- str_extract_all(list_viol, perl('(?<=Violations count:\\s)[0-9]{1,}'))
  viol_count <- sum(as.numeric(unlist(list_extracted)))
}

constr_viol_clean$value_type <- sapply(constr_viol_clean$V5, function(x){value_type_funct(x)})
constr_viol_clean$value_type <- as.numeric(constr_viol_clean$value_type)

constr_viol_clean$format <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Format\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$format <- as.numeric(constr_viol_clean$format)

constr_viol_clean$qualifier <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Qualifier\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$qualifier <- as.numeric(constr_viol_clean$qualifier)

constr_viol_clean$qualifiers <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Qualifiers\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$qualifiers <- as.numeric(constr_viol_clean$qualifiers)

constr_viol_clean$symmetric <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Symmetric\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$symmetric <- as.numeric(constr_viol_clean$symmetric)

constr_viol_clean$units <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Units\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$units <- as.numeric(constr_viol_clean$units)

constr_viol_clean$existing_file <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Existing\\sfile\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$existing_file <- as.numeric(constr_viol_clean$existing_file)

constr_viol_clean$diff_within_range <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Diff within range\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$diff_within_range <- as.numeric(constr_viol_clean$diff_within_range)

constr_viol_clean$range <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Range\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$range <- as.numeric(constr_viol_clean$range)

constr_viol_clean$bad_value <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Bad value\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$bad_value <- as.numeric(constr_viol_clean$bad_value)

constr_viol_clean$multi_value <- sapply(constr_viol_clean$V5, function(x){sum(as.numeric(unlist(str_extract_all(x, perl('(?<=\\"Multi value\\"\\sviolations\\s\\=\\=Violations count\\:\\s)[0-9]{1,}')))))})
constr_viol_clean$multi_value <- as.numeric(constr_viol_clean$multi_value)

# Extract violation stats
constr_stats <- subset(constr_viol_clean, select = -c(V4,V5))
constr_stats <- constr_stats[grep('^201.*', constr_stats$timestamp),]
constr_stats[is.na(constr_stats)] <- 0
constr_stats <- constr_stats[grep('^[P][0-9]{1,}', constr_stats$V1),]

constr_stats <- merge(constr_stats, constr_viol_extr, by = c('V1', 'V2', 'timestamp', 'month', 'V3'))



constr_viol_extr <- read.csv('constr_stats_1_new.csv',  sep = ',', quote = "", allowEscapes = T,   row.names = NULL, 
                        header = T, stringsAsFactors = FALSE,  encoding = "UTF-8")
constr_viol_extr_2 <- read.csv('constr_stats_2.csv',  sep = ',', quote = "", allowEscapes = T,   row.names = NULL, 
                             header = T, stringsAsFactors = FALSE, encoding = "UTF-8")
constr_viol_extr <- rbind(constr_viol_extr, constr_viol_extr_2)

constr_viol_extr$timestamp <- as.POSIXct(constr_viol_extr$timestamp, format = "%Y-%m-%d %H:%M:%S", origin="1970-01-01")
constr_viol_extr$month <- as.yearmon(constr_viol_extr$timestamp)

max_constr_viol <- ddply(constr_viol_extr,~month+V1,function(x){x[which.max(x$timestamp),]})
max_constr_viol <- subset(max_constr_viol, select = -V3)
constr_numbers <- max_constr_viol[,(4:26)]
constr_numbers <- aggregate(.~month, constr_numbers, sum)
col_idx <- grep("item_count", names(constr_numbers))
constr_numbers <- constr_numbers[, c(col_idx, (1:ncol(constr_numbers))[-col_idx])]
col_idx <- grep("violations", names(constr_numbers))
constr_numbers <- constr_numbers[, c(col_idx, (1:ncol(constr_numbers))[-col_idx])]
constr_numbers <- constr_numbers[!(constr_numbers$month == 'Feb 2016'|constr_numbers$month == 'Apr 2013'),]

constr_evol <- constr_numbers[,(4:23)]/constr_numbers$item_count
constr_evol <- subset(constr_evol, select = -c(existing_file))
constr_evol[is.na(constr_evol)] <- 0

### Linear trend estimation
constr_evol_ts <- sapply(constr_evol, FUN = function(x) ts(x) )

y <- ts(constr_evol$diff_within_range)
fit <- tslm(y ~ trend)
summary(fit)
plot(forecast(fit, h=20))

