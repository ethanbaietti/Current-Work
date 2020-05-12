library(dplyr)
library(ggvis)
library(stringr)
library(ggplot2)
library(rpart)
library(randomForest)
library(rpart.plot)
library(caret)
install.packages('e1071', dependencies=TRUE)
library(ModelMetrics)
library(glmmML)
library(corrplot)
library(rms)
library(MASS)
library(fastDummies)


rm(list=ls())
setwd("~/Dropbox (INVNT)/Data from R/Data from R")
################################

data <- read.csv('17.19.projectitems.csv', na.strings = c('',"NA"))
colname <- c('project_name', "project_group", 'project_number', 'completion_date', 'cost', 'revenue', 'task', 'item_type', 'item_name', 'labor', 'client', 
             'pitch_cost', 'Country', 'State', 'lead_generation', "product_type", 'office', "deliverable_type", "audience_size", "ac1", "ac2")

names(data) <-colname
data$cost[is.na(data$cost)] <- 0 
data$revenue[is.na(data$revenue)] <- 0
data$pitch_cost[is.na(data$pitch_cost)]<- 0
data$labor[is.na(data$labor)] <- 0
data$item_name <- as.character(data$item_name)

####Changing Design into Labor
data$labor[str_detect(data$item_name, "CAD Design")] <- 1
data$labor[str_detect(data$item_name, "Audio Design")] <- 1
data$labor[str_detect(data$item_name, "Sound Design")] <- 1
data$labor[str_detect(data$item_name, "Lighting Design")] <- 1
data$labor[str_detect(data$item_name, "Video Design")] <- 1
data$labor[str_detect(data$item_name, "Rigging Design")] <- 1
data$labor[str_detect(data$item_name, "Scenic Design")] <- 1
data$labor[str_detect(data$item_name, "Graphic Design")] <- 1


########################################################################################################################
#############################################    Removing Dead Pitch & Cancelled
########################################################################################################################

cancelled <- str_detect(data$project_name, "Cancelled") | str_detect(data$project_name, "Duplicate") | 
  str_detect(data$project_name, "Dead Pitch") | str_detect(data$project_name, "Dead pitch") | 
  str_detect(data$project_name, "cancelled") | str_detect(data$client, "inVNT")
data <- data[!cancelled,]

########################################################################################################################
#############################################   Account Code Designation 
########################################################################################################################

data$ac1 <- as.character(data$ac1)
data$ac2 <- as.character(data$ac2)
data$ac1[data$item_name == "Manager of Account Services" | data$item_name == "Director of Account Services" | data$item_name == "Managing Director of Account Services"] <- "Account Services"
data$ac2[data$item_name == "Manager of Account Services" | data$item_name == "Director of Account Services" | data$item_name == "Managing Director of Account Services"] <- "Administration"
data$ac1[data$item_name == "Director of Photography"] <- "Video Production"
data$ac1[data$item_name == "Executive Creative Director"] <- "Creative Director"
data$ac2[data$item_name == "Executive Creative Director"] <- "Creative"
data$ac1[data$item_name == "Shipping"] <- "Logistics"
data$ac2[data$item_name == "Shipping"] <- "Logistics"
data$ac1[data$item_name == "Union Labor"] <- "Local Labor"
data$ac2[data$item_name == "Union Labor"] <- "Local Labor"
data$ac2 <- ifelse(is.na(data$ac2), data$ac1, data$ac2)

########################################################################################################################
#############################################    Project & Group Totals 
########################################################################################################################

grouptotals <- data %>% 
  group_by(project_group) %>% 
  summarise(total_group_cost = sum(cost), total_group_revenue = sum(revenue)) ##summation of group total

data3 <- merge(data, grouptotals, by = "project_group", all.x = TRUE) ##merge data with total cost and total revenue

data3$group_margin <- (data3$total_group_revenue - data3$total_group_cost) / data3$total_group_revenue # group margin

data3 <- data3 %>% 
  filter(total_group_revenue > 0)

########################################################################################################################
#############################################    Proposal Costs
########################################################################################################################

proposal <- str_detect(data3$task, "PROPOSAL")
data4 <- data3[!proposal,]

########################################################################################################################
#############################################    Labor
########################################################################################################################
data5 <- data4[data4$labor==1,]

grouplabor <- data5 %>% 
  group_by(project_group) %>% 
  summarise(group_labor_cost = sum(cost), group_labor_revenue = sum(revenue)) %>% 
  mutate(labor_margin = (group_labor_revenue - group_labor_cost)/group_labor_revenue)


data6 <- merge(data3, grouplabor, by = "project_group", all.x = TRUE)

data6$percent_group_labor <- data6$group_labor_cost / data6$total_group_cost
data6$percent_group_labor <- ifelse(is.na(data6$group_labor_cost) & is.na(data6$percent_group_labor), 0, data6$percent_group_labor)

data6$labor_margin <- ifelse((!is.finite(data6$labor_margin)), -100 , data6$labor_margin)
  

########################################################################################################################
#############################################    Adding Year
########################################################################################################################

data6$Year <- format(as.Date(data6$completion_date, "%m/%d/%y"),"%Y")


####################################################################################################################
#####################################   Non- Labor
###################################################################################################################

data7 <- data4[data4$labor == 0,]

########################################################################################################################
#############################################    Audio
######################################################################################################################## 

avl_df <- data7 %>% 
  filter(item_name == "Audio, Visual & Lighting")

########################################################################################################################
#############################################    Audio Revenue/Cost 
######################################################################################################################## 

audio_df <- data7 %>% 
  filter(ac1 == "Audio Equipment" | item_name == "Equipment" |  item_name == "Audio, Visual & Lighting")
                                                                                                                                                
                                                                                                                                                
audio_df$cost <- ifelse(audio_df$item_name == "Audio, Visual & Lighting", audio_df$cost * .3, audio_df$cost)
audio_df$revenue <- ifelse(audio_df$item_name == "Audio, Visual & Lighting", audio_df$revenue * .3, audio_df$revenue)
audio_df$cost <- ifelse(audio_df$item_name == "Equipment", audio_df$cost * .3, audio_df$cost)
audio_df$revenue <- ifelse(audio_df$item_name == "Equipment", audio_df$revenue * .3, audio_df$revenue)


audio_tot <- audio_df %>% 
  group_by(project_group) %>% 
  summarise(total_audio_cost = sum(cost), total_audio_revenue = sum(revenue))


data6 <- merge(data6, audio_tot, by = "project_group", all.x = TRUE)


data6$audio_percent <- data6$total_audio_cost / data6$total_group_cost
data6$audio_percent <- ifelse(is.na(data6$total_audio_cost) & is.na(data6$audio_percent), 0, data6$audio_percent)

data6$audio_margin <- (data6$total_audio_revenue - data6$total_audio_cost)/data6$total_audio_revenue
data6$audio_margin <- ifelse((is.na(data6$audio_margin & data6$audio_percent == 0)), 0 , ifelse((!is.finite(data6$audio_margin) & data6$audio_percent != 0), -100, data6$audio_margin))


########################################################################################################################
#############################################    Video Revenue/Cost 
######################################################################################################################## 

video <- str_detect(data7$ac1, "Video Equipment") & !(str_detect(data7$task, "SHOOT")) & !(str_detect(data7$task, "LOGISTICS")) & !(str_detect(data7$task, "VIDEO PRODUCTION")) & 
  !(str_detect(data7$task, "Opening")) & !(str_detect(data7$task, "OPENING")) & !(str_detect(data7$task, "SIZZLE")) | str_detect(data7$item_name, "Audio, Visual & Lighting") | str_detect(data7$item_name, "Equipment")

video_df <- data7[video,]

exclusions_v <- str_detect(video_df$ac1, "Rigging") | str_detect(video_df$item_name, "Tax") | str_detect(video_df$item_name, "Heavy Equipment") | str_detect(video_df$task, "VIDEO PRODUCTION") | str_detect(video_df$task, "RIGGING") | str_detect(video_df$task, "SHOOT") | str_detect(video_df$task, stringr::fixed("LIGHTING EQUIPMENT")) | str_detect(video_df$task, "AUDIO EQUIPMENT") & !str_detect(video_df$ac1, "Video Equipment")

video_df <- video_df[!exclusions_v,]


video_df <- data7 %>% 
  filter(ac1 == "Video Equipment" | item_name == "Equipment" |  item_name == "Audio, Visual & Lighting")

exclusions_v <-  str_detect(video_df$task, "SHOOT") | str_detect(video_df$task, "VIDEO PRODUCTION") | str_detect(video_df$task, "Opening") | (str_detect(video_df$task, "OPENING") & !str_detect(video_df$project_group, "101306")) | 
  str_detect(video_df$task, "SIZZLE") | (str_detect(video_df$task, "LIGHTING") & !str_detect(video_df$project_group, "101275")) | str_detect(video_df$task, "SCENIC") | str_detect(video_df$task, "LOGISTICS") 
video_df <- video_df[!exclusions_v,]

video_df$cost <- ifelse(video_df$item_name == "Audio, Visual & Lighting", video_df$cost * .4, video_df$cost)
video_df$revenue <- ifelse(video_df$item_name == "Audio, Visual & Lighting", video_df$revenue * .4, video_df$revenue)
video_df$cost <- ifelse(video_df$item_name == "Equipment", video_df$cost * .4, video_df$cost)
video_df$revenue <- ifelse(video_df$item_name == "Equipment", video_df$revenue * .4, video_df$revenue)


video_tot <- video_df %>% 
  group_by(project_group) %>% 
  summarise(total_video_cost = sum(cost), total_video_revenue = sum(revenue))

data6 <- merge(data6, video_tot, by = "project_group", all.x = TRUE)

data6$video_percent <- data6$total_video_cost/ data6$total_group_cost
data6$video_percent <- ifelse(is.na(data6$total_video_cost) & is.na(data6$video_percent), 0, data6$video_percent)

data6$video_margin <- (data6$total_video_revenue - data6$total_video_cost) / data6$total_video_revenue
data6$video_margin <- ifelse((is.na(data6$video_margin & data6$video_percent == 0)), 0 , ifelse(!is.finite(data6$video_margin) & data6$video_percent != 0, -100, data6$video_margin))


########################################################################################################################
#############################################    Video Production Revenue/Cost
######################################################################################################################## 

video_prod_df <- data6 %>% 
  filter(ac1 == "Video Production" | str_detect(data6$task, "SHOOT"))

exclusions_vp <- str_detect(video_prod_df$task, "ON SITE") | str_detect(video_prod_df$task, "ONSITE") | str_detect(video_prod_df$task, "ON-SITE")

video_prod_df <- video_prod_df[!exclusions_vp,]

video_prod_tot <- video_prod_df %>% 
  group_by(project_group) %>% 
  summarise(total_video_production_cost = sum(cost), total_video_production_revenue = sum(revenue))

data6 <- merge(data6, video_prod_tot, by = "project_group", all.x = TRUE)

data6$video_prod_percent <- data6$total_video_production_cost / data6$total_group_cost
data6$video_prod_percent <- ifelse(is.na(data6$total_video_production_cost) & is.na(data6$video_prod_percent), 0, data6$video_prod_percent)

data6$video_prod_margin <- (data6$total_video_production_revenue - data6$total_video_production_cost) / data6$total_video_production_revenue
data6$video_prod_margin <- ifelse((is.na(data6$video_prod_margin & data6$video_prod_percent == 0)), 0 , ifelse(!is.finite(data6$video_prod_margin) & data6$video_prod_percent != 0, -100, data6$video_prod_margin))


########################################################################################################################
#############################################    Lighting Revenue/Cost 
########################################################################################################################

light_df <- data7 %>% 
  filter(ac1 == "Lighting Equipment" | item_name == "Equipment" |  item_name == "Audio, Visual & Lighting")

exclusions_l <- str_detect(light_df$task, "SHOOT")

light_df <- light_df[!exclusions_l,]
  
light_df$cost <- ifelse(light_df$item_name == "Audio, Visual & Lighting", light_df$cost * .3, light_df$cost)
light_df$revenue <- ifelse(light_df$item_name == "Audio, Visual & Lighting", light_df$revenue * .3, light_df$revenue)
light_df$cost <- ifelse(light_df$item_name == "Equipment", light_df$cost * .3, light_df$cost)
light_df$revenue <- ifelse(light_df$item_name == "Equipment", light_df$revenue * .3, light_df$revenue)


light_tot <- light_df %>%
  group_by(project_group) %>%  
  summarise(total_light_cost = sum(cost), total_light_revenue = sum(revenue))

data6 <- merge(data6, light_tot, by = "project_group", all.x = TRUE)

data6$light_percent <- data6$total_light_cost / data6$total_group_cost
data6$light_percent <- ifelse(is.na(data6$total_light_cost) & is.na(data6$light_percent), 0, data6$light_percent)

data6$light_margin <- (data6$total_light_revenue - data6$total_light_cost) / data6$total_light_revenue
data6$light_margin <- ifelse((is.na(data6$light_margin & data6$light_percent == 0)), 0 , ifelse(!is.finite(data6$light_margin) & data6$light_percent != 0, -100, data6$light_margin))


########################################################################################################################
#############################################    Scenic Revenue/Cost 
######################################################################################################################## 

scenic_df <- data7 %>% 
  filter(ac1 == "Scenic Equipment")

scenic_tot <- scenic_df %>%
  group_by(project_group) %>%  
  summarise(total_scenic_cost = sum(cost), total_scenic_revenue = sum(revenue))

data6 <- merge(data6, scenic_tot, by = "project_group", all.x = TRUE)

data6$scenic_percent <- data6$total_scenic_cost / data6$total_group_cost
data6$scenic_percent <- ifelse(is.na(data6$total_scenic_cost) & is.na(data6$scenic_percent), 0, data6$scenic_percent)

data6$scenic_margin <- (data6$total_scenic_revenue - data6$total_scenic_cost) / data6$total_scenic_revenue
data6$scenic_margin <- ifelse((is.na(data6$scenic_margin & data6$scenic_percent == 0)), 0 , ifelse(!is.finite(data6$scenic_margin) & data6$scenic_percent != 0, -100, data6$scenic_margin))

########################################################################################################################
#############################################   Local Labor Revenue/Cost
########################################################################################################################

loc_labor_df <- data7 %>% 
  filter(ac1 == "Local Labor" | item_name == "Labor" | item_name == "Union Labor")

loc_lbr_tot <- loc_labor_df %>% 
  group_by(project_group) %>% 
  summarise(total_local_labor_cost = sum(cost), total_local_labor_revenue = sum(revenue))

data6 <- merge(data6, loc_lbr_tot, by = "project_group", all.x = TRUE)

data6$local_labor_percent <- data6$total_local_labor_cost / data6$total_group_cost
data6$local_labor_percent <- ifelse(is.na(data6$total_local_labor_cost) & is.na(data6$local_labor_percent), 0, data6$local_labor_percent)

data6$local_labor_margin <- (data6$total_local_labor_revenue - data6$total_local_labor_cost) / data6$total_local_labor_revenue
data6$local_labor_margin <- ifelse((is.na(data6$local_labor_margin & data6$local_labor_percent == 0)), 0 , ifelse(!is.finite(data6$local_labor_margin) & data6$local_labor_percent != 0, -100, data6$local_labor_margin))


########################################################################################################################
#############################################   Logistics Revenue/Cost
########################################################################################################################
log_df <- data7 %>% 
  filter(ac1 == "Logistics" | ac2 == "Logistics")

log_tot <- log_df %>% 
  group_by(project_group) %>% 
  summarise(total_logistics_cost = sum(cost), total_logistics_revenue = sum(revenue))

data6 <- merge(data6, log_tot, by = "project_group", all.x = TRUE)

data6$logistics_percent <- data6$total_logistics_cost / data6$total_group_cost
data6$logistics_percent <- ifelse(is.na(data6$total_logistics_cost) & is.na(data6$logistics_percent), 0, data6$logistics_percent)

data6$logistics_margin <- (data6$total_logistics_revenue - data6$total_logistics_cost) / data6$total_logistics_revenue
data6$logistics_margin <- ifelse((is.na(data6$logistics_margin & data6$logistics_percent == 0)), 0 , ifelse(!is.finite(data6$logistics_margin) & data6$logistics_percent != 0, -100, data6$logistics_margin))

########################################################################################################################
#############################################   Overtime Revenue/Cost
########################################################################################################################
ot_df <- data7 %>% 
  filter(ac1 == "Overtime/Double Time")

over_tot <- ot_df %>% 
  group_by(project_group) %>% 
  summarise(total_overtime_cost = sum(cost), total_overtime_revenue = sum(revenue))

data6 <- merge(data6, over_tot, by = "project_group", all.x = TRUE)

data6$overtime_percent <- data6$total_overtime_cost / data6$total_group_cost
data6$overtime_percent <- ifelse(is.na(data6$total_overtime_cost) & is.na(data6$overtime_percent), 0, data6$overtime_percent)

data6$overtime_margin <- (data6$total_overtime_revenue - data6$total_overtime_cost) / data6$total_overtime_revenue
data6$overtime_margin <- ifelse((is.na(data6$overtime_margin & data6$overtime_percent == 0)), 0 , ifelse(!is.finite(data6$overtime_margin) & data6$overtime_percent != 0, -100, data6$overtime_margin))

########################################################################################################################
#############################################   Proposal Costs 
########################################################################################################################

proposal_df <- data3[proposal,]

prop_tot <- proposal_df %>% 
  group_by(project_group) %>% 
  summarise(total_proposal_cost = sum(cost))

data6 <- merge(data6, prop_tot, by = "project_group", all.x = TRUE)

data6$proposal_percent <- data6$total_proposal_cost / data6$total_group_cost


########################################################################################################################
##########################################     Adding Revenue Column
########################################################################################################################
data6$revenue_block <- 0 

for(i in 1:nrow(data6)){
  if(data6$total_group_revenue[i] < 100000) {data6$revenue_block[i] = paste("less than $100,000")}
  else if(data6$total_group_revenue[i] >= 100000 & data6$total_group_revenue[i] < 200000) {data6$revenue_block[i] = paste("$100,000 - $200,000")}
  else if(data6$total_group_revenue[i] >= 200000 & data6$total_group_revenue[i] < 300000) {data6$revenue_block[i] = paste("$200,000 - $300,000")}
  else if(data6$total_group_revenue[i] >= 300000 & data6$total_group_revenue[i] < 400000) {data6$revenue_block[i] = paste("$300,000 - $400,000")}
  else if(data6$total_group_revenue[i] >= 400000 & data6$total_group_revenue[i] < 500000) {data6$revenue_block[i] = paste("$400,000 - $500,000")}
  else if(data6$total_group_revenue[i] >= 500000 & data6$total_group_revenue[i] < 600000) {data6$revenue_block[i] = paste("$500,000 - $600,000")}
  else if(data6$total_group_revenue[i] >= 600000 & data6$total_group_revenue[i] < 700000) {data6$revenue_block[i] = paste("$600,000 - $700,000")}
  else if(data6$total_group_revenue[i] >= 700000 & data6$total_group_revenue[i] < 800000) {data6$revenue_block[i] = paste("$700,000 - $800,000")}
  else if(data6$total_group_revenue[i] >= 800000 & data6$total_group_revenue[i] < 900000) {data6$revenue_block[i] = paste("$800,000 - $900,000")} 
  else if(data6$total_group_revenue[i] >= 900000 & data6$total_group_revenue[i] < 1000000) {data6$revenue_block[i] = paste("$900,000 - $1,000,000")}
  else {data6$revenue_block[i] = paste("greater than $1,000,000")}
}
########################################################################################################################
#############################################   Binary Variables
########################################################################################################################

data6$video_prod <- +(data6$total_video_production_cost > 0 & !is.na(data6$total_video_production_cost) | data6$total_video_production_revenue > 0 & !is.na(data6$total_video_production_revenue))
data6$good_margin <- +(data6$group_margin > .3)
data6$proposal <- +(data6$total_proposal_cost > 0 & !is.na(data6$total_proposal_cost))
data6$overtime <- +(data6$total_overtime_cost > 0 & !is.na(data6$total_overtime_cost) | data6$total_overtime_revenue > 0 & !is.na(data6$total_overtime_revenue))


########################################################################################################################
###########################################    full data 
########################################################################################################################


fulldata <- data6

########################################################################################################################
#############################################   Project Data
########################################################################################################################



project_data <- fulldata[!duplicated(fulldata[1]),]

#project_data$labor_margin[!is.finite(project_data$labor_margin)] <- -1
#project_data$audio_margin[!is.finite(project_data$audio_margin)] <- -1
#project_data$video_margin[!is.finite(project_data$video_margin)] <- -1
#project_data$light_margin[!is.finite(project_data$light_margin)] <- -1
#project_data$scenic_margin[!is.finite(project_data$scenic_margin)] <- -1
#project_data$local_labor_margin[!is.finite(project_data$local_labor_margin)] <- -1
#project_data$video_prod_margin[!is.finite(project_data$video_prod_margin)] <- -1
#project_data$logistics_margin[!is.finite(project_data$logistics_margin)] <- -1

#project_data$percent_group_labor[!is.finite(project_data$percent_group_labor)] <- 0
#project_data$audio_percent[!is.finite(project_data$audio_percent)] <- 0
#project_data$video_percent[!is.finite(project_data$video_percent)] <- 0
#project_data$light_percent[!is.finite(project_data$light_percent)] <- 0
#project_data$scenic_percent[!is.finite(project_data$scenic_percent)] <- 0
#project_data$local_labor_percent[!is.finite(project_data$local_labor_percent)] <- 0
#project_data$video_prod_percent[!is.finite(project_data$video_prod_percent)] <- 0
#project_data$logistics_percent[!is.finite(project_data$logistics_percent)] <- 0

project_data2 <- dummy_cols(project_data, select_columns = c("lead_generation", "deliverable_type", "office"), remove_first_dummy = TRUE)

colnames(project_data2)[colnames(project_data2) == "lead_generation_RFP"] <- "lg_RFP"
colnames(project_data2)[colnames(project_data2) == "lead_generation_Existing Client Relationship"] <- "lg_ecr"
colnames(project_data2)[colnames(project_data2) == "lead_generation_Referral"] <- "lg_ref"
colnames(project_data2)[colnames(project_data2) == "deliverable_type_Creative Services Only"] <- "dt_cs"
colnames(project_data2)[colnames(project_data2) == "deliverable_type_Production Management Only"] <- "dt_pm"
colnames(project_data2)[colnames(project_data2) == "deliverable_type_Tradeshow"] <- "dt_ts"
colnames(project_data2)[colnames(project_data2) == "office_New York"] <- "office_ny"
colnames(project_data2)[colnames(project_data2) == "office_Detroit"] <- "office_det"
colnames(project_data2)[colnames(project_data2) == "office_Washington DC"] <- "office_DC"
colnames(project_data2)[colnames(project_data2) == "office_London"] <- "office_UK"
colnames(project_data2)[colnames(project_data2) == "office_Sydney"] <- "office_AUS"

project_data2_prod <- project_data2 %>%
  filter(deliverable_type != "Creative Services Only")


project_data_bin <- project_data2 %>%
  dplyr::select(labor_margin, percent_group_labor, Year, audio_percent, audio_margin, video_percent, video_margin, light_percent, light_margin, scenic_percent, scenic_margin, 
                local_labor_percent, local_labor_margin, logistics_percent, logistics_margin, revenue_block, video_prod, proposal, overtime, good_margin, 
                lg_RFP, lg_ecr, lg_ref, dt_cs, dt_pm, dt_ts, office_ny, office_det, office_DC, office_UK, office_AUS)

project_data_cont <- project_data2 %>%
  dplyr::select(group_margin, labor_margin, percent_group_labor, Year, audio_percent, audio_margin, video_percent, video_margin, light_percent, light_margin, scenic_percent, scenic_margin, 
                local_labor_percent, local_labor_margin, logistics_percent, logistics_margin, revenue_block, video_prod, proposal, overtime,
                lg_RFP, lg_ecr, lg_ref, dt_cs, dt_pm, dt_ts, office_ny, office_det, office_DC, office_UK, office_AUS)

project_data_bin_prod <- project_data2_prod %>% 
  dplyr::select(labor_margin, percent_group_labor, Year, audio_percent, audio_margin, video_percent, video_margin, light_percent, light_margin, scenic_percent, scenic_margin, 
                local_labor_percent, local_labor_margin, logistics_percent, logistics_margin, revenue_block, video_prod, proposal, overtime, good_margin, 
                lg_RFP, lg_ecr, lg_ref, dt_cs, dt_pm, dt_ts, office_ny, office_det, office_DC, office_UK, office_AUS)

project_data_cont_prod <- project_data2 %>%
  dplyr::select(group_margin, labor_margin, percent_group_labor, Year, audio_percent, audio_margin, video_percent, video_margin, light_percent, light_margin, scenic_percent, scenic_margin, 
                local_labor_percent, local_labor_margin, logistics_percent, logistics_margin, revenue_block, video_prod, proposal, overtime,
                lg_RFP, lg_ecr, lg_ref, office_ny, office_det, office_DC, office_UK, office_AUS)

########################################################################################################################
#############################################   Corrleation Plot
########################################################################################################################

corr_table <- project_data_cont %>% 
  dplyr::select(group_margin, labor_margin, percent_group_labor, audio_percent, audio_margin, video_percent, video_margin, light_percent, light_margin, scenic_percent, scenic_margin, 
         local_labor_percent, local_labor_margin, logistics_percent, logistics_margin)
corr_table %>% cor %>% corrplot





########################################################################################################################
########################################################################################################################
###########################################    Modeling
########################################################################################################################
########################################################################################################################
n <- nrow(project_data_bin)
n_trainb <- round(.7 * n)
set.seed(239)
trainb_indicies <- sample(1:n, n_trainb)

b_train <- project_data_bin[trainb_indicies,]
b_test <- project_data_bin[-trainb_indicies,]



########################################################################################################################
#############################################   Linear Model 
########################################################################################################################




mod_all <- lm(group_margin ~., data = project_data_cont)

summary(mod_all)
plot(mod_all$fitted.values, mod_all$residuals)
qqnorm(mod_all$residuals)

project_data_cont_scale <- scale(project_data_cont)

gvlma(mod_all)


mod_prod <- lm(group_margin~ ., data = project_data_cont_prod)

summary(mod_prod)
gvlma(mod_prod)

qqnorm(mod_prod$residuals)
ggplot(lin_mod_pro, aes(y=group_margin, x= audio_margin)) + geom_point() + geom_smooth(method = 'lm')


vif(mod_prod)

lin_mod_pro %>% select_if(is.numeric) %>% cor() %>%  corrplot()

########################################################################################################################
#############################################   Logit Model
########################################################################################################################
n <- nrow(project_data_bin)
n_trainb <- round(.7 * n)
set.seed(239)
trainb_indicies <- sample(1:n, n_trainb)
b_train <- project_data_bin[trainb_indicies,]
b_test <- project_data_bin[-trainb_indicies,]

logit_model <- glm(good_margin ~., family = 'binomial', b_train)
summary(logit_model)

logit_predict_test <- predict(logit_model,type = "response")
summary(logit_predict_test)
tapply(logit_predict_test, b_train$good_margin, mean)

table(b_train$good_margin, logit_predict_test > .5) ### Confusion Matrix
sens <- 14/(75+14) #Sensitivity = 15%
spef <- 59 / (59+25) #Specificity = 70%



logit_model2 <- stepAIC(logit_model, trace = 0)
logit_model2
########################################################################################################################
#############################################   Tree Based Models 
###############################################################################################################

#####################################
##################Desicision Tree 
###################################


dt_mod <- rpart(good_margin ~., b_train, method = "class")
dt_predict <- predict(dt_mod, b_test, type = "class")

pruned <- prune(dt_mod, cp = 0.03)
rpart.plot(dt_mod)
rpart.plot(pruned)

confusionMatrix(b_test$good_margin, dt_predict) 

#######
####   GINI/Information Split      #####################################
#####################################

dt_mod_gini <- rpart(good_margin ~ ., b_train, method = "class", parms = list(split = "gini"))
dt_mod_info <- rpart(good_margin ~ ., b_train, method = "class", parms = list(split = "information"))

pred_gini <- predict(dt_mod_gini, b_test, type = "class")
pred_info <- predict(dt_mod_info, b_test, type = "class")

ce(b_test$good_margin, pred_gini)
ce(b_test$good_margin, pred_info)

rpart.plot(dt_mod, yesno = 2, type = 2, extra = 0)

#####################################
################## Regression  Tree 
###################################
set.seed(353)
assign <- sample(1:3, size = nrow(project_data_cont_prod), prob = c(.7,.15,.15), replace = TRUE)

r_train <- project_data_cont_prod[assign == 1,]
r_valid <- project_data_cont_prod[assign == 2,]
r_test <- project_data_cont_prod[assign == 3,]

rmod <- rpart(group_margin ~., r_train, method = "anova")

pred_reg <- predict(rmod, r_test)
rmse(r_test$group_margin, pred_reg)

pred_reg2 <- predict(rmod, r_train)
rmse(r_train$group_margin, pred_reg2)

rpart.plot(dt_mod, yesno = 2, type = 0, extra = 0)


#####################################
##################   Random Forest
###################################
set.seed(433)

project_data_bin$revenue_block <- as.factor(project_data_bin$revenue_block)
project_data_bin$Year <- as.factor(project_data_bin$Year)

trainb_indicies <- sample(1:n, n_trainb)

b_train <- project_data_bin[trainb_indicies,]
b_test <- project_data_bin[-trainb_indicies,]

rf_mod <- randomForest(good_margin~., b_train, na.action = na.pass)
print(rf_mod)

err <- rf_mod$err.rate
head(err)

oob_err <- err[nrow(err), "OOB"]
plot(rf_mod)

rf_pred <- predict(rf_mod, b_test, type = "response")
rf_cm <- table(rf_pred, b_test$good_margin)
print(rf_cm)
accuracy_rf <- sum(diag(rf_cm))/sum(rf_cm)#74%


paste0('OOB accuracy:', 1 - oob_err) #### .6742424

rf_pred2 <- predict(rf_mod, b_test, type = "prob")
head(rf_pred2)

auc(b_test$good_margin == 1, rf_pred2[,"1"]) #####0.701

varImp(rf_mod)

######tuning

res <- tuneRF(subset(b_train, select = -good_margin), 
              b_train$good_margin, ntreeTry = 500, doBest = TRUE) ##mtry = 1

##### Grid Search ####
mtry <- seq(2, ncol(b_train)*0.8 ,1 )
nodesize <- seq(3,8,2)
sampsize <- nrow(b_train)* c(.7, .75, .8)

hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)
oob_err <- c()

for(i in 1:nrow(hyper_grid)) {
  
  model <- randomForest(good_margin~., b_train, 
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

opt_ <- which.min(oob_err)
print(hyper_grid[opt_,]) # mtry = 3 node = 7 samp = 198

####tuned model 

rf_mod_t <- randomForest(good_margin~., b_train, mtry = 4, nodesize = 7, sampsize = 173.25)
rf_pred_t <- predict(rf_mod_t, b_test, type = "class")

rf_cm_t <- confusionMatrix(rf_pred_t, b_test$good_margin)
print(rf_cm_t)

paste0("test accuracy:", rf_cm_t$overall[1]) ######.6363636
paste0('OOB accuracy:', 1 - oob_err) #### .6742424

varImp(rf_mod_t)



