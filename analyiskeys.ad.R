rm(list=ls())
library(dplyr)
library(stringr)
library(ggplot2)
library(openxlsx)
setwd("~/Dropbox (INVNT)/Data from R/Data from R/Analysis Keys")
data <- read.csv('2019.y.analysiskeys.csv', na.strings = c('',"NA"))

colname <- c('project_group', 'project_name', "pipeline_stage", 'cost', 'revenue', 'audience_size', 'Country', "deliverable", 'lead_generation', "product_type", 'State',  'office', 'completion_month', "account_director")
names(data) <- colname


cancelled <- str_detect(data$project_name, "Cancelled") | str_detect(data$project_name, "cancelled") | 
  str_detect(data$project_name, "Dead pitch") | str_detect(data$project_name, "Dead Pitch") | 
  str_detect(data$project_name, "duplicate") | str_detect(data$project_name, "Duplicate") | str_detect(data$project_name, "Bethel Woods")

data <- data[!(cancelled),]

samsung <- str_detect(data$project_group, "101869")

data <- data[!(samsung),]

NArm <- (is.na(data$revenue) | is.na(data$cost))

data <- data[!(NArm),]

ad_n <- data %>%
  group_by(account_director) %>% 
  summarise(n = n())

#house <- data %>% filter(account_director == "House US")

grouped <- data %>% 
  group_by(project_group) %>% 
  dplyr::summarize(group_revenue = sum(revenue), group_cost = sum(cost)) %>% 
  mutate(group_margin = (group_revenue - group_cost) / group_revenue)

data2 <- merge(grouped, data, by = "project_group", all.x = TRUE)

data2 <- data2[!duplicated(data2[1]),]

data2$completion_month[data2$deliverable == "Retainer"] <- "Dec, 2019"

yr_profit <- sum(data2$group_revenue - data2$group_cost)
q1_profit <- sum(data2$group_revenue[data2$completion_month == "Jan, 2019" | data2$completion_month == "Feb, 2019" | data2$completion_month == "Mar, 2019"] - data2$group_cost[data2$completion_month == "Jan, 2019" | data2$completion_month == "Feb, 2019" | data2$completion_month == "Mar, 2019"])
q2_profit <- sum(data2$group_revenue[data2$completion_month == "Apr, 2019" | data2$completion_month == "May, 2019" | data2$completion_month == "Jun, 2019"] - data2$group_cost[data2$completion_month == "Apr, 2019" | data2$completion_month == "May, 2019" | data2$completion_month == "Jun, 2019"])
q3_profit <- sum(data2$group_revenue[data2$completion_month == "Jul, 2019" | data2$completion_month == "Aug, 2019" | data2$completion_month == "Sep, 2019"] - data2$group_cost[data2$completion_month == "Jul, 2019" | data2$completion_month == "Aug, 2019" | data2$completion_month == "Sep, 2019"])


data2$revenue_block <- 0

for(i in 1:nrow(data2)){
  if(data2$group_revenue[i] < 100000) {data2$revenue_block[i] = paste("less than $100,000")}
  else if(data2$group_revenue[i] >= 100000 & data2$group_revenue[i] < 200000) {data2$revenue_block[i] = paste("$100,000 - $200,000")}
  else if(data2$group_revenue[i] >= 200000 & data2$group_revenue[i] < 300000) {data2$revenue_block[i] = paste("$200,000 - $300,000")}
  else if(data2$group_revenue[i] >= 300000 & data2$group_revenue[i] < 400000) {data2$revenue_block[i] = paste("$300,000 - $400,000")}
  else if(data2$group_revenue[i] >= 400000 & data2$group_revenue[i] < 500000) {data2$revenue_block[i] = paste("$400,000 - $500,000")}
  else if(data2$group_revenue[i] >= 500000 & data2$group_revenue[i] < 600000) {data2$revenue_block[i] = paste("$500,000 - $600,000")}
  else if(data2$group_revenue[i] >= 600000 & data2$group_revenue[i] < 700000) {data2$revenue_block[i] = paste("$600,000 - $700,000")}
  else if(data2$group_revenue[i] >= 700000 & data2$group_revenue[i] < 800000) {data2$revenue_block[i] = paste("$700,000 - $800,000")}
  else if(data2$group_revenue[i] >= 800000 & data2$group_revenue[i] < 900000) {data2$revenue_block[i] = paste("$800,000 - $900,000")} 
  else if(data2$group_revenue[i] >= 900000 & data2$group_revenue[i] < 1000000) {data2$revenue_block[i] = paste("$900,000 - $1,000,000")}
  else {data2$revenue_block[i] = paste("greater than $1,000,000")}
}

##### Year

audience <- data2 %>%
  group_by(audience_size) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

country <- data2 %>%
  group_by(Country) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

deliverable <- data2 %>%
  group_by(deliverable) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

lead <- data2 %>%
  group_by(lead_generation) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

product_type <- data2 %>%
  group_by(product_type) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

state <- data2 %>%
  group_by(State) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

office <- data2 %>%
  group_by(office) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

revenue_block <- data2 %>%
  group_by(revenue_block) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

month <- data2 %>%
  group_by(completion_month) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / yr_profit)

lists_of_datasets_y <- list("Audience Size" = audience, "Country" = country, "Deliverable" = deliverable, "Lead Generation" = lead, "Product Type" = product_type, 
                            "State" = state, "Office" = office, "Revenue Block" = revenue_block, "Completion Month" = month)

####################### Q1

audience_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(audience_size) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

country_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(Country) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

deliverable_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(deliverable) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

lead_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(lead_generation) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

product_type_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(product_type) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

state_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(State) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

office_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(office) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

revenue_block_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(revenue_block) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

month_q1 <- data2 %>%
  filter(completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019" ) %>% 
  group_by(completion_month) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q1_profit)

lists_of_datasets_q1 <- list("Audience Size" = audience_q1, "Country" = country_q1, "Deliverable" = deliverable_q1, "Lead Generation" = lead_q1, "Product Type" = product_type_q1, 
                          "State" = state_q1, "Office" = office_q1, "Revenue Block" = revenue_block_q1, "Completion Month" = month_q1)

#####Q2

audience_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(audience_size) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

country_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(Country) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

deliverable_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(deliverable) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

lead_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(lead_generation) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

product_type_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(product_type) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

state_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(State) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

office_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(office) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

revenue_block_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(revenue_block) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

month_q2 <- data2 %>%
  filter(completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019" ) %>% 
  group_by(completion_month) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q2_profit)

lists_of_datasets_q2 <- list("Audience Size" = audience_q2, "Country" = country_q2, "Deliverable" = deliverable_q2, "Lead Generation" = lead_q2, "Product Type" = product_type_q2, 
                             "State" = state_q2, "Office" = office_q2, "Revenue Block" = revenue_block_q2, "Completion Month" = month_q2)

#####Q3

audience_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(audience_size) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

country_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(Country) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

deliverable_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(deliverable) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

lead_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(lead_generation) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

product_type_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(product_type) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

state_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(State) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

office_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(office) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

revenue_block_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(revenue_block) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

month_q3 <- data2 %>%
  filter(completion_month == "Jul, 2019" | completion_month == "Aug, 2019" | completion_month == "Sep, 2019" ) %>% 
  group_by(completion_month) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

lists_of_datasets_q3 <- list("Audience Size" = audience_q3, "Country" = country_q3, "Deliverable" = deliverable_q3, "Lead Generation" = lead_q3, "Product Type" = product_type_q3, 
                             "State" = state_q3, "Office" = office_q3, "Revenue Block" = revenue_block_q3, "Completion Month" = month_q3)

#####Q4

audience_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(audience_size) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

country_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(Country) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

deliverable_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(deliverable) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

lead_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(lead_generation) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

product_type_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(product_type) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

state_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(State) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

office_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(office) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

revenue_block_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(revenue_block) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

month_q4 <- data2 %>%
  filter(completion_month == "Oct, 2019" | completion_month == "Nov, 2019" | completion_month == "Dec, 2019" ) %>% 
  group_by(completion_month) %>% 
  summarise(Revenue = sum(group_revenue), Cost = sum(group_cost), Average.Margin = mean(group_margin), n = n()) %>% 
  mutate(Overall.Margin = (Revenue - Cost)/Revenue, Profit = Revenue - Cost, Percent.Total.Profit = Profit / q3_profit)

lists_of_datasets_q4 <- list("Audience Size" = audience_q4, "Country" = country_q4, "Deliverable" = deliverable_q4, "Lead Generation" = lead_q4, "Product Type" = product_type_q4, 
                             "State" = state_q4, "Office" = office_q4, "Revenue Block" = revenue_block_q4, "Completion Month" = month_q4)




write.xlsx(lists_of_datasets_q1, file = "2019.q1.analysiskeysreport.xlsx")
write.xlsx(lists_of_datasets_q2, file = "2019.q2.analysiskeysreport.xlsx")
write.xlsx(lists_of_datasets_q3, file = "2019.q3.analysiskeysreport.xlsx")
write.xlsx(lists_of_datasets_q4, file = "2019.q4.analysiskeysreport.xlsx")
write.xlsx(lists_of_datasets_y, file = "2019.y.analysiskeysreport.xlsx")



###### Lists

###Q1
q1_rb <- data2 %>% 
  filter((completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019") & lead_generation == "Repeat Business")

q1_rfp <- data2 %>% 
  filter((completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019") & lead_generation == "RFP")

q1_ecr <- data2 %>% 
  filter((completion_month == "Jan, 2019" | completion_month == "Feb, 2019" | completion_month == "Mar, 2019") & lead_generation == "Existing Client Relationship")


setwd("~/Dropbox (INVNT)/Data from R/Data from R/Analysis Keys/Tables from R")

write.xlsx(q1_ecr, "q1_ecr.xlsx")
write.xlsx(q1_rfp, "q1_rfp.xlsx")
write.xlsx(q1_rb, "q1_rb.xlsx")

###Q2
q2_rb <- data2 %>% 
  filter((completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019") & lead_generation == "Repeat Business")

q2_rfp <- data2 %>% 
  filter((completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019") & lead_generation == "RFP")

q2_ecr <- data2 %>% 
  filter((completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019") & lead_generation == "Existing Client Relationship")

q2_ref <- data2 %>% 
  filter((completion_month == "Apr, 2019" | completion_month == "May, 2019" | completion_month == "Jun, 2019") & lead_generation == "Referral")


setwd("~/Dropbox (INVNT)/Data from R/Data from R/Analysis Keys/Tables from R")

write.xlsx(q2_ecr, "q2_ecr.xlsx")
write.xlsx(q2_rfp, "q2_rfp.xlsx")
write.xlsx(q2_rb, "q2_rb.xlsx")
write.xlsx(q2_ref, "q2_ref.xlsx")