#---------------LOAD & TRANSFORM DATA --------------
# loads necessary packages 
library(haven)
library(dplyr)
library(tidyr)
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
# save to dta?

# ------------ Alliance Network ------------------
atop5_0dy <- read_dta("ATOP 5_0 (dta)/atop5_0dy.dta")
nw_data <- atop5_0dy %>% 
  rename(from = mem1, to = mem2) %>% # rename(newvarname = oldvarname)
  filter(year==2018 & defense==1) # keep the rows matching the conditions
# count the number of alliances shared for each country pairs.
# I named the new variable width because it will determine the edges' width. 
nw_data$width <- rowSums(!is.na(nw_data %>% select(starts_with("atopid"))))
# for the graph's simplicity (it took too long to load the entire graph), 
# I will choose the US' allies only
US_allies <- nw_data %>% filter(from==2) %>% # where the first country ID is 2
  pull(to) # extract the 'to' column as a vector (from a data frame)
# Keep the rows whose country ID is either the US or its allies
nw_data_sm <- nw_data %>% filter(from %in% c(2, US_allies) | 
                                   to %in% c(2, US_allies))

# the link-level data for the network graph
links <- nw_data_sm %>% select(from, to, width)

# pivot_longer: change the data format from wide to long
nodes <- pivot_longer(nw_data_sm, cols=c(from, to)) %>% 
  select(-name) %>% # remove the variable 'name'
  rename(id = value) %>% # rename the variable 'value' to 'id'
  group_by(id) %>% # group the rows by country ID
  summarize(num_allies=n(),
            size=n()+10) %>% # count the number of rows (=allies) for each country
  ungroup() %>%  
  left_join(cowid, by=c("id" = "CCode")) %>%
  mutate(label = StateAbb,
         title = StateNme)


