library(tidyverse)

data <- read_csv("../../data/forbidden.csv") %>% 
  mutate(
    `Total Pay` = `Total Pay` %>%
      gsub(x = ., pattern = "[$]", replacement = "") %>%
      gsub(x = ., pattern = ",", replacement = "") %>%
      as.numeric(),
    `Start Date` = `Start Date` %>%
      lubridate::mdy()
  )

data %>% 
  #mutate(Indicator = `Dept Description` == "L&S/Statistics/Statistics") %>% 
  ggplot() +
  geom_jitter(aes(x = factor(`Fiscal year`), y = `Total Pay`/1000000), shape = "x", width = 0.2) +
  labs(x = "Fiscal Year", y = "Total Pay (USD, millions)") +
  theme_bw()
  