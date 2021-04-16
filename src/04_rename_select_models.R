# Load libraries ----
library(tidyverse)

## Rename models ----
RenameModels <- function(scenario_name) {
  # Rename models files names
  scenario_dir <- here::here("data", scenario_name)
  models_dir <- here::here("data", scenario_name, "models")
  files_names <- list.files(path = models_dir)
  files_from <- file.path(models_dir, files_names)
  files_to <- file.path(
    models_dir,
    sub("_AutoML_\\d{8}_\\d{6}", "", files_names)
  )
  file.rename(files_from, files_to)

  # Rename leaderboard
  leaderboard <- read_csv(file.path(scenario_dir, "leaderboard.csv")) %>%
    mutate(model_id = sub("_AutoML_\\d{8}_\\d{6}", "", model_id))

  write_csv(leaderboard, file.path(scenario_dir, "leaderboard.csv"))

  return(invisible(leaderboard))
}

## Select models ----
SelectModels <- function(scenario_name) {
  scenario_dir <- here::here("data", scenario_name)
  leaderboard <- read_csv(file.path(scenario_dir, "leaderboard.csv"))

  # Select 2 best models from each family
  selected_models <-
    leaderboard %>%
    # Add column with model family
    separate(model_id, c("family"), remove = FALSE, extra = "drop") %>%
    # Add column with original rank
    mutate(rank = row_number()) %>%
    # Select the 2 with lower rmse per family
    group_by(family) %>%
    top_n(-2, rmse) %>%
    ungroup()

  models_info <- selected_models %>%
    select(model_id, rmse) %>%
    rename(model = model_id, RMSE = rmse)

  write_csv(selected_models, file.path(scenario_dir, "selected_models.csv"))
  write_csv(models_info, file.path(scenario_dir, "models_info.csv"))
  return(invisible(selected_models))
}

## Apply to each scenario ----
scenarios <- c("scenario-A", "scenario-B")
map(scenarios, RenameModels)
map(scenarios, SelectModels)
