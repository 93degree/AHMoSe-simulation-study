## Load libraries ----
library(tidyverse)

## Define constants ----
recode_evaluation <- c(
  `very poor` = 1,
  `poor` = 2,
  `average` = 3,
  `good` = 4,
  `excellent` = 5
)

# Average temperature (in degree Celsius) for the growing season (April 1st to
# October 31st) from Tagarakis et al. (2014)
growing_average_temp <- c(
  `2010` = 21.4,
  `2011` = 20.5,
  `2012` = 22.2
)

## Load & preprocess data ----
grape_quality_data <-
  read_csv(here::here("data", "grape_quality_data.csv")) %>%
  # Recode Expert evaluation
  mutate(`Expert evaluation` = recode(
    `Expert evaluation`,
    !!!recode_evaluation
  )) %>%
  # Drop columns that correspond to the fuzzy model information
  select(-c(Output, `Fuzzy evaluation`, `Agreement of evaluation (%)`)) %>%
  rename(GTQ = `Expert evaluation`) %>%
  # Generate Row and Column from Cell
  separate("Cell", c("Row", "Col")) %>%
  # Add average temperature for the growing season
  mutate(Temp = recode(Year, !!!growing_average_temp))

## Scenario A ----
# Train data 2010-2011
# Test data 2012
# Only original features: Anth, BW, TA, TSS

path_A <- here::here("data", "scenario-A", "datasets")
if (!dir.exists(path_A)) {
  dir.create(path_A, recursive = TRUE)
}

test_data <- grape_quality_data %>%
  filter(Year == 2012) %>%
  select(-c(Year, Row, Col, Temp))

train_data <- grape_quality_data %>%
  filter(Year != 2012) %>%
  select(-c(Year, Row, Col, Temp))

write_csv(test_data, file.path(path_A, "test_data.csv"))
write_csv(train_data, file.path(path_A, "train_data.csv"))

## Scenario B ----
# Train data 2010-2011
# Test data 2012
# Original features: Anth, BW, TA, TSS + Row, Col and Temp

path_B <- here::here("data", "scenario-B", "datasets")
if (!dir.exists(path_B)) {
  dir.create(path_B, recursive = TRUE)
}


test_data <- grape_quality_data %>%
  filter(Year == 2012) %>%
  select(-Year)

train_data <- grape_quality_data %>%
  filter(Year != 2012) %>%
  select(-Year)

write_csv(test_data, file.path(path_B, "test_data.csv"))
write_csv(train_data, file.path(path_B, "train_data.csv"))
