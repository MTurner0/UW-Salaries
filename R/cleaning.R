require(tidyverse)

edge_builder <- function(letsci, nodes) {
  edge_builder1 <- nodes %>%
    filter(type == "School") %>%
    right_join(letsci, by = c("name" = "School")) %>%
    rename(School = name, School_ID = id) %>%
    select(-type)
  
  edge_builder2 <- nodes %>%
    filter(type == "Subschool") %>%
    right_join(edge_builder1, by = c("name" = "Subschool")) %>%
    rename(Subschool = name, Subschool_ID = id) %>%
    select(-type)
  
  edge_builder3 <- nodes %>%
    filter(type == "Department") %>%
    right_join(edge_builder2, by = c("name" = "Department")) %>%
    rename(Department = name, Department_ID = id) %>%
    select(-type)
  
  tibble(
    source = c(edge_builder3$School_ID, edge_builder3$Subschool_ID),
    target = c(edge_builder3$Subschool_ID, edge_builder3$Department_ID)
  ) %>%
    drop_na()
}

