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

# Makes a scatterplot of total pay by year (the athletics department is filtered
# out)
data %>%
  filter(!str_starts(`Dept Description`, "Ath/")) %>%
  ggplot() +
  geom_jitter(
    aes(x = factor(`Fiscal year`),
        y = `Total Pay`/1000),
    shape = "x",
    width = 0.2
    ) +
  labs(
    x = "Fiscal Year",
    y = "Total Pay (USD, in thousands)"
    ) +
  theme_bw()

# Compares summary statistics for professors in the econ and stats departments
data %>%
  filter(
    (
      str_detect(`Dept Description`, "Economics") |
      str_detect(`Dept Description`, "Statistics")
    )
    & str_detect(Title, "Professor")
  ) %>%
  group_by(`Dept Description`) %>%
  summarize(
    min = min(`Total Pay`),
    `1st Q.` = quantile(`Total Pay`, 0.25),
    median = median(`Total Pay`),
    mean = mean(`Total Pay`),
    `3rd Q.` = quantile(`Total Pay`, 0.75),
    max = max(`Total Pay`)
    ) %>%
  arrange(desc(median))

# Exploring how to break up the Department Description column
data %>%
  mutate(
    School = str_split(`Dept Description`, pattern = "/") %>%
              unlist() %>%
              .[1]
    )

# Fun fact: Kris asked me this question when I showed him this dataset
# Distributions of pay for various job positions across the stats department
data %>%
  filter(str_detect(`Dept Description`, "Statistics")) %>%
  group_by(Title) %>%
  summarize(
    min = min(`Total Pay`),
    `1st Q.` = quantile(`Total Pay`, 0.25),
    median = median(`Total Pay`),
    mean = mean(`Total Pay`),
    `3rd Q.` = quantile(`Total Pay`, 0.75),
    max = max(`Total Pay`)
    ) %>%
  arrange(desc(mean)) %>%
  head()

# Which professors outside of the econ, athletics, and business departments have
# the highest median pay?
data %>%
  filter(
    !(
      str_detect(`Dept Description`, "Economics") |
      str_detect(`Dept Description`, "Ath/") |
      str_detect(`Dept Description`, "Business")
      ) &
    str_detect(Title, "Professor")
    ) %>%
  group_by(`Dept Description`) %>%
  summarize(median = median(`Total Pay`)) %>%
  slice_max(order_by = median, n = 50) %>%
  print(n = 50)
