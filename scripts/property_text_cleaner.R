library(plyr)
library(dplyr)
library(stringi)
library(chron)
library(zoo)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(methods)
library(grid)
library(gridExtra)
library(doBy)
library(reshape2)
library(vegan)
library(data.table)

setwd("/Volumes/LaCie/Wikidata_data/")

#bot list
bot_list_complete <- scan("bot_list.csv", what="", sep=",")

#load property talk pages
property_talks <- read.csv('ptalks_all.csv',  sep = '\t', quote = "", allowEscapes = T,   row.names = NULL, 
                           header = F, stringsAsFactors = FALSE, encoding = "UTF-8")

#delete non-properties
property_talks <- property_talks[(grep('footer|template|Template:Property talk|Template:Property talk/doc|Archive', property_talks$V1,invert = T)),]
property_talks <- property_talks[(grep('^20[0-9]{1,2}', property_talks$V2)),]


#delete 'Property talk:'
property_talks$V1 <- gsub("Property talk:", "", property_talks$V1)

#extract constraints
property_talks$constraints <- sapply(property_talks$V5, function(x){regmatches(x, regexpr("((\\{\\{Constraint:)+.*?(\\}\\}))+", x, perl = T))})
#replace 'character(0)' with 'None'
property_talks$constraints <- lapply(property_talks$constraints, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})
property_talks$constraints <- unlist(property_talks$constraints)


### single constraints extraction
#single value
property_talks$single_value <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Single value).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$single_value <- lapply(property_talks$single_value, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$unique_value <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Unique value).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$unique_value <- lapply(property_talks$unique_value, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$format <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Format).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$format <- lapply(property_talks$format, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$one_of <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:One of).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$one_of <- lapply(property_talks$one_of, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$symmetric <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Symmetric).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$symmetric <- lapply(property_talks$symmetric, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$inverse <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Inverse).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$inverse <- lapply(property_talks$inverse, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$commons_link <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Commons link).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$commons_link <- lapply(property_talks$commons_link, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$target_required_claim <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Target required claim).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$target_required_claim <- lapply(property_talks$target_required_claim, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$item <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Item).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$item <- lapply(property_talks$item, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$type <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Type).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$type <- lapply(property_talks$type, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$value_type <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Value type).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$value_type <- lapply(property_talks$value_type, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$range <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Range).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$range <- lapply(property_talks$range, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$diff_within_range <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Diff within range).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$diff_within_range <- lapply(property_talks$diff_within_range, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$multi_value <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Multi value).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$multi_value <- lapply(property_talks$multi_value, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$conflicts_with <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Conflicts with).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$conflicts_with <- lapply(property_talks$conflicts_with, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$qualifiers <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Qualifiers).*?(?=\\{\\{[^QP]|$|\n|\r)", x, perl = T))})
property_talks$qualifiers <- lapply(property_talks$qualifiers, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$qualifier <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Qualifier\\}\\})", x, perl = T))})
property_talks$qualifier <- lapply(property_talks$qualifier, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$units <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Units)", x, perl = T))})
property_talks$units <- lapply(property_talks$units, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$existing_file <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Existing file)", x, perl = T))})
property_talks$existing_file <- lapply(property_talks$existing_file, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})

property_talks$person_value <- lapply(property_talks$constraints, function(x){regmatches(x, regexpr("(\\{\\{Constraint\\:Person value)", x, perl = T))})
property_talks$person_value <- lapply(property_talks$person_value, function(x){if(identical(x, character(0)) == TRUE){x <- 'None'}  else {x <- x}})


#unlist new columns
property_talks[,(7:24)] <- sapply(property_talks[,(7:25)], function(x) unlist(x))
property_talks$qualifier <- unlist(property_talks$qualifier)
property_talks$qualifiers <- unlist(property_talks$qualifiers)
property_talks$units <- unlist(property_talks$units)
property_talks$unique_value <- unlist(property_talks$unique_value)
property_talks$diff_within_range <- unlist(property_talks$diff_within_range)
property_talks$commons_link <- unlist(property_talks$commons_link)
property_talks$conflicts_with <- unlist(property_talks$conflicts_with)
property_talks$multi_value <- unlist(property_talks$multi_value)
property_talks$range <- unlist(property_talks$range)
property_talks$value_type <- unlist(property_talks$value_type)
property_talks$target_required_claim <- unlist(property_talks$target_required_claim)
property_talks$type <- unlist(property_talks$type)
property_talks$item <- unlist(property_talks$item)
property_talks$inverse <- unlist(property_talks$inverse)
property_talks$symmetric <- unlist(property_talks$symmetric)
property_talks$format <- unlist(property_talks$format)
property_talks$one_of <- unlist(property_talks$one_of)
property_talks$single_value <- unlist(property_talks$single_value)
property_talks$existing_file <- unlist(property_talks$existing_file)
property_talks$person_value <- unlist(property_talks$person_value)


#time
property_talks$V2 <- gsub("Z", "", property_talks$V2)
property_talks$V2 <- gsub("T", " ", property_talks$V2)
property_talks$timestamp <- as.POSIXct(property_talks$V2, format = "%Y-%m-%d %H:%M:%S", origin="1970-01-01")
property_talks <- subset(property_talks, select = - V2)

#NAs interpolation
ind <- which(is.na(property_talks$timestamp))
property_talks$timestamp[ind] <- sapply(ind, function(i) with(property_talks, format(as.POSIXct(mean(c(timestamp[i-1], timestamp[i+1]))), origin="1970-01-01"), format = "%Y-%m-%d %H:%M:%S"))

#month
property_talks$month <- as.yearmon(property_talks$timestamp)


#number of constraints
property_talks$no_constr <- rowSums(property_talks[,(5:24)] != 'None')
###max by month
property_talks <- data.table(property_talks)
property_max <- property_talks[ , .SD[which.max(timestamp)], by = list(V1, month)]
property_max <- subset(property_max, select = c('V1', 'month', 'no_constr'))


###combinations of all properties and all months
property_months <- expand.grid(unique(as.factor(property_max$V1)), unique(as.factor(property_max$month)))
colnames(property_months) <- c('property_stat', 'month')
property_months$month <- as.yearmon(property_months$month)
property_months$V1 <- as.character(property_months$property_stat)

property_new <- merge(property_months, property_max, by.x = c('V1', 'month'), all.x = TRUE)
property_new <- property_new %>% group_by(V1) %>% na.locf %>% ungroup
colnames(property_new) <- c('property_stat', 'month', 'no_constr')
property_new$property_stat <- as.factor(property_new$property_stat)
property_new$month <- as.yearmon(property_new$month)

###historical data of property uses (no. of statements and no. of claims)
property_story <- read.csv('data/prop_story.csv')
property_story$month <- as.yearmon(property_story$month)
colnames(property_story) <- c('no_claim', 'no_item', 'month', 'property_stat',  'no_statement',  'no_user')
property_story <- merge(property_new, property_story, by = c('property_stat', 'month'), all.x = TRUE)
property_story[is.na(property_story)] <- 0
property_story$no_constr <- as.numeric(property_story$no_constr)

# 
# #load property features datasets
property_features <- read.csv('prop_items_users_per_month.csv')
property_features_1 <- read.csv('prop_edits_per_month.csv')
property_features_2 <- read.csv('prop_claim_per_month.csv')

property_features <- merge(property_features, property_features_1, by = c('stat_property', 'month_edit'))
property_features <- merge(property_features, property_features_2, by = c('stat_property', 'month_edit'))
property_features$month <- as.yearmon(as.character(property_features$month_edit))
property_features <- property_features[,-2]
rm(property_features_1)
rm(property_features_2)

colnames(property_features) <- c('property_stat', 'no_item', 'no_user', 'no_edits', 'no_statement', 'no_claim', 'stat_claim_ratio',  'month')
property_features_new <- merge(property_months, property_features, by.x = c('property_stat', 'month'), all.x = TRUE)

#remove rows with NAs
property_features_new <- property_features_new[complete.cases(property_features_new),]
property_features_new$month <- as.yearmon(property_features_new$month)

property_to_merge <- property_features_new[,c(1,2,6,9)]
property_story <- merge(property_story, property_to_merge, by = c('property_stat', 'month'), all.x = TRUE)
property_story$no_edits[is.na(property_story$no_edits)] <- 0

###prepare disagreeement score dataset
dis_panda <- read.csv('data/dis_panda_month.csv')
dis_properties <- aggregate(dis_score ~ property_stat + month, dis_panda, mean)
dis_properties_roll <- aggregate(dis_score_roll ~ property_stat + month, dis_panda, mean)

colnames(property_months) <- c('property_stat', 'month')
property_months$month <- as.yearmon(property_months$month)

dis_properties$month <- as.yearmon(dis_properties$month)
dis_properties <- merge(property_months, dis_properties, by.x = c('property_stat', 'month'), all.x = TRUE)
dis_properties$dis_score[is.na(dis_properties$dis_score)] <- 0

dis_properties_roll$month <- as.yearmon(dis_properties_roll$month)
dis_properties_roll <- merge(property_months, dis_properties_roll, by.x = c('property_stat', 'month'), all.x = TRUE)
dis_properties_roll <- dis_properties_roll %>% group_by(property_stat) %>% na.locf %>% ungroup
dis_properties_roll$dis_score_roll[is.na(dis_properties_roll$dis_score_roll)] <- 0
dis_properties_roll <- dis_properties_roll[order(dis_properties_roll$property_stat, dis_properties_roll$month),]
dis_properties_roll$dis_score_roll <- as.numeric(dis_properties_roll$dis_score_roll)

dis_properties_roll$diff <- ave(dis_properties_roll$dis_score_roll, factor(dis_properties_roll$property_stat), FUN=function(x) c(NA,diff(x)))
dis_properties_roll$diff[is.na(dis_properties_roll$diff)] <- 0
dis_properties_roll$month <- as.yearmon(dis_properties_roll$month)


### monthly changes
# property_story$diff_claim <- ave(property_story$no_claim, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# property_story$diff_item <- ave(property_story$no_item, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# property_story$diff_statement <- ave(property_story$no_statement, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# property_story$diff_user <- ave(property_story$no_user, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# property_story$diff_constr <- ave(property_story$no_constr, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# property_story$diff_kd <- ave(property_story$stat_claim_ratio, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# property_story$diff_edit <- ave(property_story$no_edit, factor(property_story$property_stat), FUN=function(x) c(NA,diff(x)))
# 
# property_story <- property_story[which(property_story$no_edit > 0),]
# property_story[is.na(property_story)] <- 0
# property_story[is.na(property_story$stat_claim_ratio)] <- 0
# property_story <- property_story[which(property_story$stat_claim_ratio > 0 ),]

###extract statistics about variables
stats_prop <- c(mean(property_story$no_claim), mean(property_story$no_item), mean(property_story$no_statement),
                mean(property_story$no_user), mean(property_story$stat_claim_ratio), mean(property_story$no_edit), median(property_story$no_claim), median(property_story$no_item), median(property_story$no_statement),
                median(property_story$no_user), median(property_story$stat_claim_ratio), median(property_story$no_edit), sd(property_story$no_claim), sd(property_story$no_item), sd(property_story$no_statement),
                sd(property_story$no_user), sd(property_story$stat_claim_ratio), sd(property_story$no_edit))

stats_prop_median <- c(median(property_story$no_claim), median(property_story$no_item), median(property_story$no_statement),
                       median(property_story$no_user), median(property_story$stat_claim_ratio))

stats_prop_sd <- c(sd(property_story$no_claim), sd(property_story$no_item), sd(property_story$no_statement),
                   sd(property_story$no_user), sd(property_story$stat_claim_ratio))


###dataset from regression models
property_new$month <- as.yearmon(property_new$month)
property_new$no_constr <- as.numeric(property_new$no_constr)

properties_clean <- property_features_new[!is.na(property_features_new$no_edits),]
properties_clean <- properties_clean[,-3]
properties_clean <- merge(properties_clean, property_new, by = c('property_stat', 'month'))
dis_properties_roll$month <- as.yearmon(dis_properties_roll$month)
properties_clean <- merge(properties_clean, dis_properties_roll, by = c('property_stat', 'month'))
properties_clean <- properties_clean[,-10]
properties_clean$diff_constr <- ave(properties_clean$no_constr, factor(properties_clean$property_stat), FUN=function(x) c(NA,diff(x)))

properties_clean$diff_kd <- ave(properties_clean$stat_claim_ratio, factor(properties_clean$property_stat), FUN=function(x) c(NA,diff(x)))
properties_clean[is.na(properties_clean)] <- 0
### longitudinal regression models

#KD
fit_1 <- lm(stat_claim_ratio ~ no_item + no_user + no_claim + no_edits + no_constr, data=properties_clean)
fit_2 <- lm(stat_claim_ratio ~ no_item + no_user +  no_claim + no_edits + no_constr + diff_constr, data=properties_clean)

#Edit wars
property_ew <- properties_clean[which(properties_clean$diff > 0),]
fit_3 <- lm(diff ~ no_item + no_user +  no_claim + no_edits + no_constr , data=property_ew)
fit_4 <- lm(diff ~ no_item + no_user +  no_claim + no_edits + no_constr + diff_constr, data=property_ew)
