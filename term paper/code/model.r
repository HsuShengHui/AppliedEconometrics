library(dplyr)
library(ggplot2)
library(tidyverse)
library(fixest)
##############################################################
# Setup
setwd("C:/Users/h2408/Downloads/Applied Econometrics/term paper")
distance_threshold <- "50"  # 距離閾值
##############################################################
# Please describe how you load the data into R
treatment = read.csv(paste0("workdata/Treatment_", distance_threshold, "m_all.csv"))
control = read.csv(paste0("workdata/Control_", distance_threshold, "m_all.csv"))

# Check if there are any missing values for any variables in the dataset
sum(is.na(treatment))
sum(is.na(control))

# Briefly describe three R commands you used during the data cleaning process.
treatment <- treatment %>% mutate(group = "treatment")
control <- control %>% mutate(group = "control")
combined_data <- bind_rows(treatment, control)

# DID method
combined_data <- combined_data %>%
  mutate(post = ifelse(year2month >= "2022-12-01", 1, 0),  # 定義干預後的期間
         did = ifelse(group == "treatment" & post == 1, 1, 0),  # DID 交互項
         quarter = floor((as.numeric(month) - 1) / 3) + 1)  # 季度
##############################################################
# 基本 DID 模型
basic_model <- lm(accident_count ~ group + post + did, data = combined_data)
summary(basic_model)
# 使用地區固定效應
district_model_fe <- feols(accident_count ~ group + post + did | district, data = combined_data)
summary(district_model_fe)
# 使用地點固定效應(address)
address_model_fe <- feols(accident_count ~ post + did | address, data = combined_data)
summary(address_model_fe)
# 使用地區+地點固定效應(address)
district_address_model_fe <- feols(accident_count ~ post + did | district + address, data = combined_data)
summary(district_address_model_fe)
# 使用年+月份固定效應(year+month)
year_month_model_fe <- feols(accident_count ~ group + did | year + month, data = combined_data)
summary(year_month_model_fe)
# 使用地區+年+月份固定效應(district+year+month)
district_year_month_model_fe <- feols(accident_count ~ did | address + district + year + month, data = combined_data)
summary(district_year_month_model_fe)
# 加入所有控制變數(1)
control_vars_model_fe <- feols(accident_count ~ did + TemperatureAverage + RainDay| address + district + year + month, 
                               data = combined_data)
summary(control_vars_model_fe)
# 加入所有控制變數(2)
control_vars_model_fe2 <- feols(accident_count ~ did + carNumberPer + motorcycleNumberPer | address + district + year + month, 
                                data = combined_data)
summary(control_vars_model_fe2)
# 加入所有控制變數(3)
control_vars_model_fe3 <- feols(accident_count ~ did + TemperatureAverage + RainDay + carNumberPer + motorcycleNumberPer | address + district + year + month, 
                                data = combined_data)
summary(control_vars_model_fe3)
##############################################################
# 基本 DID 模型
basic_model <- lm(injury_count ~ group + post + did, data = combined_data)
summary(basic_model)
# 使用地區固定效應
district_model_fe <- feols(injury_count ~ group + post + did | district, data = combined_data)
summary(district_model_fe)
# 使用地點固定效應(address)
address_model_fe <- feols(injury_count ~ post + did | address, data = combined_data)
summary(address_model_fe)
# 使用地區+地點固定效應(address)
district_address_model_fe <- feols(injury_count ~ post + did | district + address, data = combined_data)
summary(district_address_model_fe)
# 使用年+月份固定效應(year+month)
year_month_model_fe <- feols(injury_count ~ group + did | year + month, data = combined_data)
summary(year_month_model_fe)
# 使用地區+年+月份固定效應(district+year+month)
district_year_month_model_fe <- feols(injury_count ~ did | address + district + year + month, data = combined_data)
summary(district_year_month_model_fe)
# 加入所有控制變數(1)
control_vars_model_fe <- feols(injury_count ~ did + TemperatureAverage + RainDay| address + district + year + month, 
                               data = combined_data)
summary(control_vars_model_fe)
# 加入所有控制變數(2)
control_vars_model_fe2 <- feols(injury_count ~ did + carNumberPer + motorcycleNumberPer | address + district + year + month, 
                                data = combined_data)
summary(control_vars_model_fe2)
# 加入所有控制變數(3)
control_vars_model_fe3 <- feols(injury_count ~ did + TemperatureAverage + RainDay + carNumberPer + motorcycleNumberPer | address + district + year + month, 
                                data = combined_data)
summary(control_vars_model_fe3)
