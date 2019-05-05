library(ggplot2)
library(circlize)
library(dplyr)
library(tibble)
library(tidyverse)
library(grid)
library(devtools)
library(ggfortify)
library(lubridate)
library(data.table)
options(digits=10)


shooting_df <- read.csv('/Users/Eden/Desktop/TDI_Application/data/school-shootings-data.csv')
levels(shooting_df$race_ethnicity_shooter1)
shooting_df$race_ethnicity_shooter1 <- factor(shooting_df$race_ethnicity_shooter1, labels = c("NA", "Asian", "American Indian", "Black", "Hispanic", "Middle Eastern", "White"))
shooting_df$gender_shooter1 <- factor(shooting_df$gender_shooter1, labels = c("NA","B", "Female", "Male"))
levels(shooting_df$gender_shooter1)

shooting_per_year <- shooting_df %>%
  group_by(year) %>%
  summarize(casualties=sum(casualties))
shooting_per_year

ggplot(shooting_per_year) +
  geom_line(mapping = aes(year, casualties)) +
  labs(title='Number of Annual School Shooting Casualties since 1999') +
  geom_point(mapping = aes(year, casualties)) +
  geom_text(mapping = aes(year, casualties, label = rownames(shooting_per_year), vjust=-1))

shooting_df %>%
  filter(race_ethnicity_shooter1 != 'NA') %>%
  filter(gender_shooter1 != "B") %>%
  ggplot() +
  geom_bar(mapping = aes(race_ethnicity_shooter1, fill=gender_shooter1)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Breakdown of Shooters by Race and Gender")

race_counts <- shooting_df %>%
  filter(race_ethnicity_shooter1 != 'NA') %>%
  count(race_ethnicity_shooter1) %>%
  mutate(prop=n/sum(n))

ggplot(race_counts) +
  geom_point(mapping = aes(race_ethnicity_shooter1, prop)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


