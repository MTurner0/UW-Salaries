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

letsci <- data %>%
  filter(Campus == "UW Madison" &
         str_detect(`Dept Description`, "L&S")) %>%
  select(`Dept Description`) %>%
  distinct() %>%
  mutate(
    School = str_split_i(`Dept Description`, "/", 1),
    Subschool = str_split_i(`Dept Description`, "/", 2),
    Department = str_split_i(`Dept Description`, "/", 3),
  )

edges <- tibble(
  source = c(letsci$School, letsci$Subschool),
  target = c(letsci$Subschool, letsci$Department)
) %>%
  drop_na()

nodes <- tibble(
  names = unique(c(edges$source, edges$target)),
  id = 1:length(names)
)

library(tidygraph)
library(ggraph)

G <- tbl_graph(
  nodes = nodes,
  edges = edges,
  directed = TRUE
)

ggraph(G) +
  geom_edge_link(width = 0.2) +
  geom_node_label(aes(label = names))
