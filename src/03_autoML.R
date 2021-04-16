# Load libraries ----
library(tidyverse)
library(h2o)

## Initialize h2o ----
h2o.init(min_mem_size = "7G")

## Auto ML ----
SaveModel <- function(model_id, path) {
  model <- h2o.getModel(model_id)
  h2o.saveModel(model, path)
  return(invisible(model))
}

GenerateAndSaveModels <- function(scenario_name) {
  scenario_dir <- here::here("data", scenario_name)
  ## Generate Models ----
  # Load Data
  train_h2o <-
    h2o.importFile(here::here(
      "data", scenario_name,
      "datasets", "train_data.csv"
    ))
  # Regression target variable
  y <- "GTQ"

  # AutoML
  aml <- h2o.automl(
    y = y,
    training_frame = train_h2o,
    exclude_algos = c("DeepLearning", "StackedEnsemble"),
    stopping_metric = "RMSE",
    sort_metric = "RMSE",
    seed = 93,
    project_name = paste0(scenario_name, format(Sys.time(), "_%d%m%Y_%H%M%S"))
  )

  ## Save results ----
  leaderboard <- as_tibble(aml@leaderboard)
  event_log <- as_tibble(aml@event_log)
  write_csv(leaderboard, file.path(scenario_dir, "leaderboard.csv"))
  write_csv(event_log, file.path(scenario_dir, "event_log.csv"))

  ## Save models ----
  models_dir <- here::here("data", scenario_name, "models")
  if (!dir.exists(models_dir)) {
    dir.create(models_dir)
  }

  map2(leaderboard$model_id, models_dir, SaveModel)

  return(invisible(leaderboard))
}

## Apply to each scenario ----
scenarios <- c("scenario-A", "scenario-B")
map(scenarios, GenerateAndSaveModels)

## Shutdown h2o ----
h2o.shutdown(prompt = FALSE)
