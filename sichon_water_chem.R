# load libraries
library(tidyverse)
library(lubridate)
library(skimr)

# import data
water.quality.23 <- read.csv("data/water_quality.csv")
water.quality.bacteria.23 <- read.csv("data/water_quality_bacteria.csv")

# mutate dates
water.quality.23 <- water.quality.23 %>%
  mutate(date = dmy(date))


water.quality.bacteria.23 <- water.quality.bacteria.23 %>%
  mutate(date = dmy(date))

water.quality23.join <- full_join(water.quality.23, water.quality.bacteria.23,
                                  by = c("date", "unit", "tank"))

water.quality23.innerjoin <- inner_join(water.quality.23, water.quality.bacteria.23,
                                  by = c("date", "unit", "tank"))



df <- water.quality23.join %>% filter(department.x == "Maturation")
unique(df$tank)

df <- water.quality23.join %>% filter(department.y == "Nursery")
uniq <- unique(df$tank) %>%
  str_sort() 

uniq.sub <- uniq[1:45]

df <- water.quality23.join %>% filter(department.x == "Water System")
unique(df$tank)

# summarise water quality data
# tan
tan.nursery.summary <- water.quality23.join %>%
  filter(department.x == "Nursery") %>%
  skim(ph, salinity, alkalinity, tan)

write_csv(tan.nursery.summary, "tabs/skim_nursery_1.csv")
  
tan.nursery.summary.2 <- water.quality23.join %>%
  filter(department.x == "Nursery"  & !is.na(tan)) %>%
  group_by(tank) %>%
  summarise(median.tan = median(tan),
            max.tan = max(tan)) %>%
  arrange(tank)
  

# try a few plots to see what is going on
tan.plot.nursery.1 <- water.quality23.join %>%
  filter(department.x == "Nursery" & tank == "1") %>%
  ggplot(aes(date, tan)) +
  geom_line() +
  geom_hline(yintercept = c(1, 3), linetype = c("dashed", "dotted"),
             color = c("orange", "red"), size = 1.2) +
  xlab("Date") +
  ylab("TAN (mg/l)") +
  theme_minimal()

# Open a pdf file
pdf("plots/tan_plot_nursery_1.pdf") 

# 2. Create a plot
tan.plot.nursery.1

# Close the pdf file
dev.off()

mtgvc <- water.quality23.join %>% filter(department.y == "Maturation")
maturation.tanks <- unique(mtgvc$tank) %>%
  str_sort() 

maturation.tanks.sub <- maturation.tanks[1:42]

water.quality23.join %>%
  filter(department.x == "Nursery" & tank == "1" & !is.na(nh3)) %>%
  ggplot(aes(date, nh3)) +
  geom_rect(aes(xmin = min(water.quality23.join$date), xmax = max(water.quality23.join$date), ymin = 0, ymax = 0.16),
            alpha = 1/5,
            fill = "lightgreen") +
  geom_rect(aes(xmin = min(water.quality23.join$date), xmax = max(water.quality23.join$date), ymin = 0.3, ymax = Inf),
                                            alpha = 1/5,
                                            fill = "red") +
  geom_hline(yintercept = c(0.16, 0.3), linetype = c("dashed"),
             color = c("orange"), size = 1.0) +
  geom_point() +
  labs(x = "Date", y = expression(NH[3]~(mg/l))) +
  theme_minimal()

water.quality23.join %>%
  filter(department.y == "Nursery" & tank == "1" & !is.na(gvc)) %>%
  ggplot(aes(date, gvc)) +
  geom_point(na.rm = TRUE) +
  labs(x = "TAN", y = "GVC") +
  theme_minimal()

water.quality23.join %>%
  filter(department.x == "Nursery") %>%
  select(c(no2, gvc)) %>%
  head(50)

lm <- lm(log(gvc + 1) ~ nh3, data = water.quality23.join)

