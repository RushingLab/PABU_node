locations <- readRDS("data/trilateration/Error.sd.cov.Estimated.Locations90p-90_2023-05-10_2023-11-20.rds")

library(dplyr)
library(lubridate)

tag_summary <- locations %>%
  group_by(TagId) %>%
  summarise(
    max_time = max(Time.group),
    min_time = min(Time.group),
    duration_days = as.numeric(difftime(max_time, min_time, units = "days")) # number of days tag was picked up via nodes
  )
tag_summary <- tag_summary %>%
  filter(TagId != "6166071E") %>%
  mutate(motus_days = c(41,38,6,26,25,39,20,41,22,40, 18,40,28,12,29,2,1)) # number of days tag was picked up via motus

tags <- tag_summary[c(1:15),] # tags without the 1 day outliers

node_overall_summary <- tag_summary %>%
  summarise(
    avg_duration = mean(duration_days, na.rm = TRUE),
    sd_duration = sd(duration_days, na.rm = TRUE),
    range_duration = max(duration_days, na.rm = TRUE) - min(duration_days, na.rm = TRUE),
    max_duration = max(duration_days, na.rm = TRUE),
    min_duration = min(duration_days, na.rm = TRUE)
  )

motus_overall_summary <- tag_summary %>%
  summarise(
    avg_duration = mean(motus_days, na.rm = TRUE),
    sd_duration = sd(motus_days, na.rm = TRUE),
    range_duration = max(motus_days, na.rm = TRUE) - min(motus_days, na.rm = TRUE),
    max_duration = max(motus_days, na.rm = TRUE),
    min_duration = min(motus_days, na.rm = TRUE)
  )

