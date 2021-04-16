## Load libraries ----
library(tidyverse)

## Define constants ----
recode_evaluation <- c(
  `Very Poor` = 1,
  `Poor` = 2,
  `Average` = 3,
  `Good` = 4,
  `Excellent` = 5
)

# Bins breaks and labels from Tagarakis et al. (2014)
bins <- list(
  Anth = c(200, 600, 800, 1000, 1400),
  BW = c(1.00, 1.60, 2.00, 2.50),
  TSS = c(15, 21, 30),
  TA = c(3, 5, 7, 12)
)

bins_labels <- list(
  Anth = c("L", "M", "H", "VH"),
  BW = c("L", "M", "H"),
  TSS = c("L", "H"),
  TA = c("L", "M", "H")
)

## Load & preprocess data ----
combinations_weight <-
  read_csv(here::here("data", "grape_quality_data.csv")) %>%
  # Discard everything that is not input data.
  select(TSS, TA, Anth, BW) %>%
  # Bin each column in the predefined levels
  imap_dfc(~ cut(.x, breaks = bins[[.y]], labels = bins_labels[[.y]])) %>%
  mutate_all(as.character) %>%
  # Count the appearances of each combination on the input data
  group_by_all() %>%
  tally(name = "weight")

fuzzy_rules <-
  read_csv(here::here("data", "fuzzy_rules.csv")) %>%
  mutate(GTQ = recode(GTQ, !!!recode_evaluation))

# Add de GTQ value for each combination from the fuzzy rules table
comb_weight_fuzzyGTQ <- left_join(combinations_weight, fuzzy_rules)

## Calculate Knowledge intervals ----
feature_label_WQM <- comb_weight_fuzzyGTQ %>%
  # Calculate the weighted quality mean (WQM) for each level of each feature
  pivot_longer(c(TSS, TA, Anth, BW),
    names_to = "feature", values_to = "level"
  ) %>%
  group_by(feature, level) %>%
  summarise(WQM = weighted.mean(GTQ, weight)) %>%
  ungroup() %>%
  # Calculate the lower(y1) and upper(y2) bounds of the 0.5 radious interval
  # around the WQM
  mutate(y1 = pmax(1, WQM - 0.5), y2 = pmin(5, WQM + 0.5))

# Join feature and label
label <-
  imap(bins_labels, ~ paste(.y, .x, sep = "_")) %>%
  flatten_chr()
# Lower range of each break
x1 <- bins %>%
  map(~ head(., -1)) %>%
  flatten_dbl()
# Upper range of each break
x2 <- bins %>%
  map(~ tail(., -1)) %>%
  flatten_dbl()

# Join all the data
intervals <- tibble(label, x1, x2) %>%
  separate(label, c("feature", "level"), remove = FALSE) %>%
  left_join(feature_label_WQM) %>%
  select(feature, label, x1, x2, WQM, y1, y2)


## Save data in csv ----
write_csv(intervals, here::here("data", "knowledge_intervals.csv"))
