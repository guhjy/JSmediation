library(tidyverse)
library(reshape2)

dohle_siegrist_raw <- read_delim(
  "data-raw/dohle.csv",
                    ";", escape_double = FALSE, trim_ws = TRUE
  )

# dataset coding (Dohl & Siegrist, 2014, Exp. 1)
# a: effectiveness simple
# b: side effect simple
# c: effectivness complex
# d: side effect complex
# m1: hazardous simple
# m2: hazardous complex
# y1: willingness simple
# y2: willingness complex

dohle_siegrist<-
  dohle_siegrist_raw %>%
  select(-age, -sex, -order) %>%
  gather(measure, score,
         -subject) %>%
  mutate(
    name =
      case_when(
        str_detect(measure, regex("1$")) ~ "simple",
        str_detect(measure, regex("2$")) ~ "complex"
      ),
    measure =
      case_when(
        str_detect(measure, regex("^m")) ~ "hazardousness",
        str_detect(measure, regex("^y")) ~ "willingness"
      )
  ) %>%
  drop_na() %>%
  dcast(subject + name ~ measure,
        value.var = "score")

dohle_siegrist %>%
  JSmediation::mdt_within(name, hazardousness, willingness, subject) %>%
  JSmediation::add_index()

usethis::use_data(dohle_siegrist, overwrite = TRUE)

