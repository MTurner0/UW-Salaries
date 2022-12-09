require(tidyverse)

data <- read_csv("../data/forbidden-all.csv") %>%
  mutate(
    `Total Pay` = `Total Pay` %>%
      gsub(x = ., pattern = "[$]", replacement = "") %>%
      gsub(x = ., pattern = ",", replacement = "") %>%
      as.numeric(),
    `Start Date` = `Start Date` %>%
      lubridate::mdy()
  ) %>%
  select(-Details)

data %>%
  select(Campus, `Dept Description`) %>%
  distinct() %>%
  arrange(Campus, `Dept Description`) %>%
  write_csv("../data/descriptions.csv")

data %>%
  select(Campus, `Dept Description`) %>%
  distinct() %>%
  arrange(Campus, `Dept Description`) %>%
  mutate(
    `Dept Description` = str_replace(`Dept Description`, "\\")
    )

data %>%
  select(Campus, `Dept Description`) %>%
  distinct() %>%
  arrange(Campus, `Dept Description`) %>%
  filter(str_detect(`Dept Description`, "\\"))
