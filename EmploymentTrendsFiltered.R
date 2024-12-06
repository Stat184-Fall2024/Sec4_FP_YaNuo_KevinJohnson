library(dplyr)
employment_trends <- read.csv("employment_trends.csv")

employment_trends <- employment_trends %>%
  mutate(REF_DATE = as.character(REF_DATE)) %>%
  rename("Industry" = North.American.Industry.Classification.System..NAICS., 
         "Employment" = VALUE, 
         "Region" = GEO,
         "Date" = REF_DATE
         )

post_pandemic_employment_trends <- employment_trends %>%
  filter(Date >= "2020-01" & Date <= "2024-12")

pre_pandemic_employment_trends <- employment_trends %>%
  filter(Date >= "2015-01" & Date <= "2019-12")
 