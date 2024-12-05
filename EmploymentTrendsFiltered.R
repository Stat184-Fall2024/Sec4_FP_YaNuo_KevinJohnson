library(dplyr)

employment_trends <- employment_trends %>%
  mutate(REF_DATE = as.character(REF_DATE))

employment_trends_filtered <- employment_trends %>%
  filter(REF_DATE >= "2020-01" & REF_DATE <= "2024-12")

