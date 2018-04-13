library(haven)
library(tidyverse)

dataset <-
  read_spss("data-raw/Study3_dataforYzerbyt_withcontrolcond.sav") %>%
  mutate(condition =
           case_when(condit == 1 ~ "Low discrimination",
                     condit == 2 ~ "High discrimination",
                     condit == 3 ~ "No-article control")) %>%
  select(condition,
         linked_fate = lfate,
         hypodescent = hypo)

usethis::use_data(dataset)
