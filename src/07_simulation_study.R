# Load libraries ----
library(tidyverse)
library(h2o)

## Initialize h2o ----
h2o.init(min_mem_size = "7G")

## Calculate AHMoSe results ---
CalculateResults <- function(scenario_name) {
  scenario_dir <- here::here("data", scenario_name)
  models_dir <- here::here("data", scenario_name, "models")
  ## Calculate weighted mean agreement (WMA)
  # WMA is the mean of the different agreement levels weighted by the
  # relevance/importance of each for the model. Note that the WMA is equivalent
  # to the percentage of the blue area on the AHMoSe summary plot
  models_shap <-
    read_csv(here::here("data", scenario_name, "models_data.csv"))
  intervals <-
    read_csv(here::here("data", "knowledge_intervals.csv")) %>%
    mutate(id = row_number())

  WMA <- models_shap %>%
    # Calculate the agreement for each feature adn observation
    mutate(agree = pmap_int(
      list(feature, value, expected_value),
      ~ intervals %>%
        filter(feature == ..1 & x1 < ..2 & ..2 <= x2) %>%
        mutate(agree = y1 <= ..3 & ..3 <= y2) %>%
        select(agree) %>%
        as.integer()
    )) %>%
    # Replace NA (values that don't have interval) with 0
    mutate(agree = replace_na(agree, 0L)) %>%
    # Calculate agreement and importance as in the AHMoSe marimekko chart
    mutate(importance = abs(shap_value)) %>%
    select(model, feature, agree, importance) %>%
    group_by(model, feature) %>%
    summarise_all(mean) %>%
    # Normalize importance
    mutate(importance = importance / sum(importance)) %>%
    # Calculated the weighted mean agreement (WMA)
    mutate(score = agree * importance) %>%
    group_by(model) %>%
    summarise(WMA = sum(score))

  ## Calculate test RMSE ----
  models_results <-
    read_csv(here::here("data", scenario_name, "selected_models.csv"))

  test_h2o <-
    h2o.importFile(here::here(
      "data", scenario_name,
      "datasets", "test_data.csv"
    ))

  CalculteTestRMSE <- function(model_id) {
    path <- file.path(models_dir, model_id)
    model <- h2o.loadModel(path = path)
    performance <- h2o.performance(model, test_h2o)
    return(performance@metrics$RMSE)
  }

  rmse_scores <-
    models_results %>%
    select(model_id, rmse, rank) %>%
    mutate(test_rmse = map_dbl(model_id, CalculteTestRMSE))


  ## All scores together ----
  all_scores <- inner_join(rmse_scores, WMA, by = c("model_id" = "model")) %>%
    arrange(desc(WMA))
  write_csv(all_scores, file.path(scenario_dir, "all_scores.csv"))
  return(invisible(all_scores))
}

## Apply to each scenario ----
scenarios <- c("scenario-A", "scenario-B")
map(scenarios, CalculateResults)

## Shutdown h2o ----
h2o.shutdown(prompt = FALSE)
