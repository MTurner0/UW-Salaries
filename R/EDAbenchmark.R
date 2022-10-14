library(dplyr)
library(readr)
library(data.table)
library(stringr)
library(microbenchmark)

setDTthreads(threads = 0)

loading_dp <- function(){
  .data <- read_csv("../data/forbidden-all.csv") %>%
    mutate(
      `Total Pay` = `Total Pay` %>%
        gsub(x = ., pattern = "[$]", replacement = "") %>%
        gsub(x = ., pattern = ",", replacement = "") %>%
        as.numeric(),
      `Start Date` = `Start Date` %>%
        lubridate::mdy()
    ) %>%
    select(-Details)
  return(.data)
}

loading_dt <- function(){
  .data <- fread(
    "../data/forbidden-all.csv",
    drop = "Details"
  )
  .data[
    ,
    c("Total Pay", "Start Date") := .(
      gsub(`Total Pay`, pattern = "[$,]", replacement = "") |> as.numeric(),
      lubridate::mdy(`Start Date`)
    )
  ]
  return(.data)
}

loading <- microbenchmark(loading_dp, loading_dt, times = 1000)

summary_dt <- function(.data){
  .data[
    (
      str_detect(`Dept Description`, "Economics|Statistics")
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
}

summary_dp <- function(.data) {
  .data %>%
    filter(
      (
        str_detect(`Dept Description`, "Economics|Statistics")
      ) & str_detect(Title, "Professor")
    ) %>%
    mutate(
      Department = str_extract(`Dept Description`, "Economics|Statistics")
    ) %>%
    group_by(Department) %>%
    summarize(
      min = min(`Total Pay`),
      `1st Q.` = quantile(`Total Pay`, 0.25),
      median = median(`Total Pay`),
      mean = mean(`Total Pay`),
      `3rd Q.` = quantile(`Total Pay`, 0.75),
      max = max(`Total Pay`)
    ) %>%
    arrange(desc(median))
}

summarizing <- microbenchmark(summary_dp, summary_dt, times = 1000)

high_dp <- function(.data) {
  .data %>%
    filter(
      !(
        str_detect(`Dept Description`, "Economics|Ath|Business|Wsb")
      ) &
        str_detect(Title, "Professor")
    ) %>%
    group_by(`Dept Description`) %>%
    summarize(median = median(`Total Pay`)) %>%
    arrange(-median)
}

high_dt <- function(.data) {
  .data[
    !(
      str_detect(`Dept Description`, "Economics|Ath|Business|Wsb")
    ) &
      str_detect(Title, "Professor"),
    .(median = median(`Total Pay`)),
    by = `Dept Description`
  ][
    order(-median)
  ]
}

high <- microbenchmark(high_dp, high_dt, times = 1000)

print(loading)
print(summarizing)
print(high)
