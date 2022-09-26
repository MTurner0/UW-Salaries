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
  filter(!str_starts(`Dept Description`, "Ath/")) %>% 
  ggplot() +
  geom_jitter(aes(x = factor(`Fiscal year`), y = `Total Pay`/1000), shape = "x", width = 0.2) +
  labs(x = "Fiscal Year", y = "Total Pay (USD, in thousands)") +
  theme_bw()
  