library(tidyverse)

employment_trends <- read.csv("~/Desktop/Stat 184/employment_trends.csv")

employment_trends_clean <- employment_trends %>%
  filter(REF_DATE >= "2015-01" & REF_DATE <= "2024-12",
         UOM != "Dollars") %>%
  mutate(
    REF_DATE = as.character(REF_DATE),
    Employment = replace_na(VALUE, 0), 
    Period = ifelse(REF_DATE < "2020-01", "Pre-Pandemic", "Post-Pandemic")
  ) %>%
  select(-c(DGUID, SYMBOL, TERMINATED, SCALAR_FACTOR, SCALAR_ID, STATUS, DECIMALS, Estimate, VECTOR, COORDINATE, UOM_ID)) %>%
  rename(
    Industry = North.American.Industry.Classification.System..NAICS., 
    Region = GEO,
    Date = REF_DATE
  )

write.csv(employment_trends_clean, "employment_trends_clean.csv", row.names = FALSE)

post_pandemic_employment_trends <- employment_trends_clean %>%
  filter(Date >= "2020-01" & Date <= "2024-12")

pre_pandemic_employment_trends <- employment_trends_clean %>%
  filter(Date >= "2015-01" & Date <= "2019-12")

View(employment_trends_clean)

# Pre-Pandemic Stacked Bar Chart
pre_stacked_data <- pre_pandemic_employment_trends %>%
  group_by(Industry) %>%
  summarise(Total_Employment = sum(Employment, na.rm = TRUE)) %>%
  mutate(Proportion = Total_Employment / sum(Total_Employment))

# Post-Pandemic Stacked Bar Chart
post_stacked_data <- post_pandemic_employment_trends %>%
  group_by(Industry) %>%
  summarise(Total_Employment = sum(Employment, na.rm = TRUE)) %>%
  mutate(Proportion = Total_Employment / sum(Total_Employment))

# Group regions into categories
employment_trends_group <- employment_trends_clean %>%
  mutate(
    Region_Group = case_when(
      Region %in% c("British Columbia", "Alberta", "Saskatchewan", "Manitoba") ~ "Western Canada",
      Region %in% c("Ontario", "Quebec") ~ "Central Canada",
      Region %in% c("New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador") ~ "Atlantic Canada",
      TRUE ~ "Northern Territories"
    )
  )


