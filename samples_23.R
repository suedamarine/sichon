library(tidyverse)

# import data

samples_23 <- read.csv("data/samples_23.csv")

samples_23 <- samples_23 %>%
  mutate(across("tss", str_replace, "<", ""),
         across("tss", str_replace, "Not Deteched", "0"),
         tss = as.numeric(tss),
         across("chlorine", str_replace, "Not Deteched", "0"),
         chlorine = as.numeric(chlorine)) %>%
  select(-`str_replace(tss, "<", "")`)

unique(samples_23$chlorine)

samples_23 %>% filter(tan >= 1)

