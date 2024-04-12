library(tidyverse)
library(janitor)
##import data from csv file

gas <- read.csv("gas.csv") %>% clean_names()
electric <- read.csv("electric.csv") %>% clean_names()

colnames(electric)
colnames(gas)


gas <- gas %>% 
  rename(
    gasm3 = consumption_m)
  
electric <- electric %>% 
  rename(
    kwh = consumption_k_wh)

  


# gas$timestamp <- ymd_hms(gas$start, tz = "UTC")
# electric$timestamp <- ymd_hms(electric$start, tz = "UTC")
# 
# min_timestamp <- max(min(gas$timestamp), min(electric$timestamp))
# max_timestamp <- min(max(gas$timestamp), max(electric$timestamp))
# 
# gas <- gas %>% 
#   filter(timestamp >= min_timestamp & timestamp <= max_timestamp)
# 
# electric <- electric %>% 
#   filter(timestamp >= min_timestamp & timestamp <= max_timestamp)

merged_df <- gas %>% full_join(electric, by = "start")

merged_df <- merged_df %>% 
  select(gasm3, kwh, start, end.x) %>% 
  rename(end = end.x) %>% 
  mutate(timestamp = ymd_hms(start),
         date = as.Date(timestamp),
         time = format(timestamp, "%H:%M:%S"),
         weekday = wday(timestamp, label = TRUE),
         month = month(timestamp, label = TRUE)) %>% 
  na.omit()


##lets see which day of the week uses most electricity



merged_df %>% 
  group_by(weekday) %>% 
  summarise(totalkwh = sum(kwh))


merged_df %>% 
  group_by(weekday) %>% 
  summarise(totalm3 = sum(gasm3)) %>% 
  plot()

merged_df %>% 
  group_by(month) %>% 
  summarise(totalm3 = sum(gasm3)) %>% 
  plot()

merged_df %>% 
  group_by(month) %>% 
  summarise(totalkwh = sum(kwh)) %>% plot()

##mean daily use per month

merged_df %>% 
  group_by(month, weekday) %>% 
  summarise(totalgas = sum(gasm3)) %>% 
  group_by(month) %>% 
  summarise(mean_gas = mean(totalgas))


daily_use <- merged_df %>% 
  group_by(date) %>% 
  summarise(total_kwh = sum(kwh))


daily_use %>% inner_join(merged_df) -> merged_df
##heatmap




heatmap_plot <- ggplot(merged_df, aes(x = weekday, y = month, fill = total_kwh)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey") +
  labs(x = "Day of Week", y = "Month", fill = "Energy Consumption") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

heatmap_plot


daily_gas <- merged_df %>% 
  group_by(date) %>% 
  summarise(total_gas = sum(gasm3)) %>% inner_join(merged_df)
daily_gas

gasmap_plot <- ggplot(daily_gas, aes(x=weekday, y = month, fill = total_gas))+
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey") +
  labs(x = "Day of Week", y = "Month", fill = "Energy Consumption") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

gasmap_plot
