library(tidyverse)

# filter to fine out number of occurrances of above level bacti results gvc
water.system.gbv.amber <- water.quality23.join %>%
  filter(department.x == "Water System" & gvc >= 80 & gvc <= 100)

water.system.gbv.red <- water.quality23.join %>%
  filter(department.x == "Water System" & gvc >100)

length(water.system.gbv.amber$gvc)

length(water.system.gbv.red$gvc)

water.system.gbv.amber %>% ggplot(aes(date, gvc)) +
  geom_line()

nursery.tankR1.ph.p <- water.quality23.join %>%
  filter(department.x == "Nursery" & tank == "1" & !is.na(ph)) %>%
  ggplot(aes(date, ph)) +
  geom_rect(aes(xmin = min(water.quality23.join$date), xmax = max(water.quality23.join$date), ymin = 8, ymax = 8.4),
            alpha = 1/5,
            fill = "lightgreen") +
  geom_hline(yintercept = c(7.1, 8.0, 8.4, 8.8), linetype = c("dashed", "dashed", "dashed", "dashed"),
             color = c("orange","orange", "orange", "orange"), size = 1.0) +
  geom_line() +
  xlab("Date") +
  ylab("pH") +
  theme_minimal()


