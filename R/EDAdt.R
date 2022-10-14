library(data.table)
library(ggplot2)
library(stringr)

setDTthreads(threads = 0)

data <- fread(
  "../data/forbidden-all.csv",
  drop = "Details"
  )

data[
  ,
  c("Total Pay", "Start Date") := .(
    gsub(`Total Pay`, pattern = "[$,]", replacement = "") |> as.numeric(),
    lubridate::mdy(`Start Date`)
  )
]

# Makes a scatterplot of total pay by year (the athletics department is filtered
# out)
data[!str_starts(`Dept Description`, "Ath/")] |>
  ggplot() +
  geom_jitter(
    aes(x = factor(`Fiscal year`),
        y = `Total Pay`),
    shape = "x",
    width = 0.2
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, prefix = "$", suffix = "K", big.mark = ",")
    ) +
  labs(
    title = "Pay for employees across the UW System, 2017-2021",
    subtitle = "Athletic department excluded",
    x = "Fiscal Year",
    y = "Total Pay (USD)"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

data |>
  ggplot() +
  geom_jitter(
    aes(x = factor(`Fiscal year`),
        y = `Total Pay`),
    shape = "x",
    width = 0.2
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, prefix = "$", suffix = "K", big.mark = ",")
  ) +
  labs(
    title = "Pay for employees across the UW System, 2017-2021",
    x = "Fiscal Year",
    y = "Total Pay (USD)"
  ) +
  theme_bw()

# Compares summary statistics for professors in the econ and stats departments
data[
  (
    str_detect(`Dept Description`, "Economics") |
    str_detect(`Dept Description`, "Statistics")
  ) & str_detect(Title, "Professor"),
  .(
    Department = str_extract(`Dept Description`, "Economics|Statistics"),
    Campus,
    `Total Pay`
    )
][
  ,
  .(Min = min(`Total Pay`),
  `1st Q.` = quantile(`Total Pay`, 0.25),
  Median = median(`Total Pay`),
  Mean = mean(`Total Pay`),
  `3rd Q.` = quantile(`Total Pay`, 0.75),
  Max = max(`Total Pay`)),
  by = .(Department, Campus)
][
  order(-Median)
]



# Salary summary statistics across econ departments, pivoted
data[
    str_detect(`Dept Description`, "Economics") &
    str_detect(Title, "Professor"),
    .(Campus = str_remove(Campus, "UW "), `Total Pay`)
][
  ,
  .(Min = min(`Total Pay`),
    `1st Q.` = quantile(`Total Pay`, 0.25),
    Median = median(`Total Pay`),
    Mean = mean(`Total Pay`),
    `3rd Q.` = quantile(`Total Pay`, 0.75),
    Max = max(`Total Pay`)),
  by = Campus
] |>
  melt(measure.vars = 2:7)

# Plot of salary distributions for econ profs
data[
  str_detect(`Dept Description`, "Economics") &
    str_detect(Title, "Professor"),
  .(Campus = str_remove(Campus, "UW "), `Total Pay`)
] |>
  ggplot(
    aes(x = `Total Pay`/1000,
        y = forcats::fct_reorder(Campus, `Total Pay`, median))
    ) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Econ Professor salaries across the UW system",
       x = "Total Pay (USD, in Thousands)",
       y = "Campus")

data[
  str_detect(`Dept Description`, "Economics") &
    str_detect(Title, "Professor"),
  .(Campus = str_remove(Campus, "UW "), `Total Pay`)
] |>
  ggplot(
    aes(x = `Total Pay`/1000)
  ) +
  geom_histogram() +
  facet_wrap(~forcats::fct_reorder(Campus, `Total Pay`, median, .desc = TRUE))+
  theme_bw() +
  labs(title = "Econ Professor salaries across the UW system",
       x = "Total Pay (USD, in Thousands)",
       y = "Campus")

# Exploring how to break up the Department Description column
data[
  ,
  .(School = str_split(`Dept Description`, pattern = "/") |>
      {\(x)(unlist(x)[1])}()
    )
]

data %>%
  mutate(
    School = str_split(`Dept Description`, pattern = "/") %>%
              unlist() %>%
              .[1]
    )

# Fun fact: Kris asked me this question when I showed him this dataset
# Distributions of pay for various job positions across the stats department
data[
  str_detect(`Dept Description`, "Statistics")
][
  ,
  .(Min = min(`Total Pay`),
    `1st Q.` = quantile(`Total Pay`, 0.25),
    Median = median(`Total Pay`),
    Mean = mean(`Total Pay`),
    `3rd Q.` = quantile(`Total Pay`, 0.75),
    Max = max(`Total Pay`)),
  by = .(Title)
][
  order(-Median)
]

# Which professors outside of the econ, athletics, and business departments have
# the highest median pay?
data[
  !(
    str_detect(`Dept Description`, "Economics|Ath|Business|Wsb")
  ) &
  str_detect(Title, "Professor"),
  .(median = median(`Total Pay`)),
  by = `Dept Description`
][
  order(-median)
]

# Campus representation
data[
  ,
  .(Campus = str_remove(Campus, "UW "))
][
  ,
  .N,
  by = Campus
] |>
  ggplot(aes(x = N, y = reorder(Campus, N))) +
  geom_col() +
  theme_bw() +
  labs(
    title = "Salary information across the University of Wisconsin System",
    x = "Employee salaries on record (count)",
    y = "Campus"
    ) +
  scale_x_continuous(labels = scales::label_number(scale = 1e-3, suffix = "K"))

data[
  ,
  .(missing = mean(Name == "")),
  by = Campus
]
