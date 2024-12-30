library(dplyr)
library(ggplot2)
library(tidyverse)
library(fixest)
##############################################################
# Setup
setwd("C:/Users/h2408/Downloads/Applied Econometrics/term paper")
distance_threshold <- "10"  # 距離閾值
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
##############################################################
# Plot the average number of accidents per month for the treatment and control groups
combined_data$year2month <- as.Date(paste0(combined_data$year2month, "-01"))
ggplot(combined_data, aes(x = year2month, y = accident_count, color = group, group = group)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_vline(xintercept = as.Date("2022-12-01"), linetype = "dashed", color = "red") +
  labs(
    title = "Average Number of Accidents per Month",
    x = "Month",
    y = "Average Number of Accidents",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot as a PNG file
ggsave(paste0("pic/average_accidents_", distance_threshold, "m.png"), width = 10, height = 6, dpi = 300)
##############################################################
# Plot the average number of injuries per month for the treatment and control groups
combined_data$year2month <- as.Date(paste0(combined_data$year2month, "-01"))
ggplot(combined_data, aes(x = year2month, y = injury_count, color = group, group = group)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  geom_vline(xintercept = as.Date("2022-12-01"), linetype = "dashed", color = "red") +
  labs(
    title = "Average Number of Injuries per Month",
    x = "Month",
    y = "Average Number of Injuries",
    color = "Group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Save the plot as a PNG file
ggsave(paste0("pic/average_injuries_", distance_threshold, "m.png"), width = 10, height = 6, dpi = 300)
##############################################################
# Descriptive Statistics for Treatment and Control Groups
# Calculate overall mean and std
overall_stats <- combined_data %>% 
  summarise(
    across(
      where(is.numeric),  # 僅對數值型變數計算
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), 
      .names = "{.col}_{.fn}"
    )
  )

# Calculate mean and std for each group
group_stats <- combined_data %>% 
  group_by(group) %>% 
  summarise(
    across(
      where(is.numeric),  # 僅對數值型變數計算
      list(mean = ~mean(.x, na.rm = TRUE), sd = ~sd(.x, na.rm = TRUE)), 
      .names = "{.col}_{.fn}"
    )
  )

# Combine overall and group stats
stats_combined <- bind_rows(overall_stats, group_stats)
# save descriptive statistics to a CSV file
write.csv(stats_combined, paste0("output/descriptive_stats_", distance_threshold, "m.csv"), row.names = FALSE)
##############################################################
# DID method
combined_data <- combined_data %>%
  mutate(post = ifelse(year2month >= "2022-12-01", 1, 0),  # 定義干預後的期間
         did = ifelse(group == "treatment" & post == 1, 1, 0),  # DID 交互項
         quarter = floor((as.numeric(month) - 1) / 3) + 1)  # 季度
# 基本 DID 模型
basic_model <- lm(accident_count ~ group + post + did, data = combined_data)
summary(basic_model)

# 使用地點固定效應(address)
address_model_fe <- feols(accident_count ~ post + did | address, data = combined_data)
summary(address_model_fe)

# 使用年度固定效應(year)
year_model_fe <- feols(accident_count ~ group + post + did | year, data = combined_data)
summary(year_model_fe)

# 使用年+月份固定效應(year+month)
year_month_model_fe <- feols(accident_count ~ group + did | year + month, data = combined_data)
summary(year_month_model_fe)

# 使用站點+年+月份固定效應(district+year+month)
address_year_month_model_fe <- feols(accident_count ~ did | address + year + month, data = combined_data)
summary(address_year_month_model_fe)

# 加入控制變數(TemperatureAverage + RainDay)
control_vars_model_fe <- feols(accident_count ~ did + TemperatureAverage + RainDay | address + year + month, 
                               data = combined_data)
summary(control_vars_model_fe)

# Combine and save the regression results to a CSV file
model_results <- list(basic_model, address_model_fe, year_model_fe, year_month_model_fe, address_year_month_model_fe, control_vars_model_fe)
model_names <- c("basic_model", "address_model_fe", "year_model_fe", "year_month_model_fe", "address_year_month_model_fe", "control_vars_model_fe")
names(model_results) <- model_names
model_results <- lapply(model_results, function(x) x$coeftable)
write.csv(model_results, paste0("output/regression_results_", distance_threshold, "m.csv"))
##############################################################