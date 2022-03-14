#---------------LOAD & TRANSFORM DATA --------------
# loads necessary packages 
library(haven)
library(dplyr)
library(readr)
library(reshape2)
# --------------- Alliance Counter Overtime --------------
  # cleaned data: states_count_by_year
# country-year level data (e.g., one row for US-2020)
atop5_0sy <- read_dta("ATOP 5_0 (dta)/atop5_0sy.dta")

# Correlates of War country code, country names, and abbreviations
cowid <- read_csv("ATOP 5_0 (dta)/COW country codes.csv") %>%
  distinct(StateAbb, CCode, StateNme) # unique names

states_count_by_year <-data.frame(year = c(1815:2018))
# iterate thru all countries 
for(i in 1:nrow(cowid)) { 
  code = as.character(cowid[i,]$CCode)      # local vars
  name = cowid[i,]$StateAbb
  # each time count the allies for 1 country 
  state_data <- atop5_0sy %>% filter(state==code)
  state_data$count = rowSums(!is.na(state_data %>% select(starts_with("atopid"))))
  state_data = subset(state_data, select=c(year,count)) 
  state_data = rename(state_data, !!name := count)
  # result is in single file with each country = 1 col, named by StateAbb
  states_count_by_year = merge(x = states_count_by_year, y = state_data, by = 
                                 "year", all.x = TRUE)
}
# save to dta



