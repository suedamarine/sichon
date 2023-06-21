library(tidyverse)

nursery.feed.composition <- read.csv("data/nursery_feed_composition.csv")

nursery.feed.input <- read.csv("data/nursery_feed_input.csv")

nursery.protein <- nursery.feed.composition %>%
  filter(variable == "protein") %>%
  mutate(value = value / 100) %>%
  rename(protein.proportion = value) %>%
  select(-variable)


nursery.protein.input <- nursery.feed.input %>%
  left_join(nursery.protein, by = "feed") %>%
  mutate(value = value / 1000,
         protein = value * protein.proportion) %>%
  filter(!feed == "th")

nursery.doc.protein <- nursery.protein.input %>%
  group_by(doc) %>%
  summarise(protein.sum = sum(protein))
