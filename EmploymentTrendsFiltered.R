library(dplyr)
employment_trends <- read.csv("employment_trends.csv")

employment_trends <- employment_trends %>%
  mutate(REF_DATE = as.character(REF_DATE)) %>%
  rename("Industry" = North.American.Industry.Classification.System..NAICS., "Employment" = VALUE)

post_pandemic_employment_trends <- employment_trends %>%
  filter(REF_DATE >= "2020-01" & REF_DATE <= "2024-12")

pre_pandemic_employment_trends <- employment_trends %>%
  filter(REF_DATE >= "2015-01" & REF_DATE <= "2019-12")
 