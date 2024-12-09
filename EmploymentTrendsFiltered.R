library(dplyr)
employment_trends <- read.csv("employment_trends.csv")

employment_trends_clean <- employment_trends %>%
  filter(REF_DATE >= "2015-01" & REF_DATE <= "2024-12",
         UOM != "Dollars") %>%
  mutate(REF_DATE = as.character(REF_DATE)) %>%
  rename("Industry" = North.American.Industry.Classification.System..NAICS., 
         "Employment" = VALUE, 
         "Region" = GEO,
         "Date" = REF_DATE
  ) %>%
  select(-c(DGUID, SYMBOL, TERMINATED, SCALAR_FACTOR, SCALAR_ID, STATUS, DECIMALS, Estimate, VECTOR, COORDINATE, UOM_ID)) %>%
  filter(!is.na(Employment))

write.csv(employment_trends_clean, "employment_trends_clean.csv", row.names = FALSE)

post_pandemic_employment_trends <- employment_trends_clean %>%
  filter(Date >= "2020-01" & Date <= "2024-12")

pre_pandemic_employment_trends <- employment_trends_clean %>%
  filter(Date >= "2015-01" & Date <= "2019-12")

