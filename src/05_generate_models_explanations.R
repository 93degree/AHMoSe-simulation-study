# Load libraries ----
library(tidyverse)
library(h2o)

## Initialize h2o ----
h2o.init(min_mem_size = "7G")

## h2o wrapper predict function ----
h2oPredict <- function(model, x) {
  x_h2o <- as.h2o(x)
  y <- as.vector(h2o.predict(model, x_h2o))
  return(y)
}
## Generate explanations ----
GenerateExplanations <- function(scenario_name) {
  scenario_dir <- here::here("data", scenario_name)
  datasets_dir <- here::here("data", scenario_name, "datasets")
  models_dir <- here::here("data", scenario_name, "models")
  ## Load data ----
  # We load the data without the ground-truth
  train_input <-
    read_csv(file.path(datasets_dir, "train_data.csv")) %>%
    # delete GTQ column
    select(-GTQ)

  test_input <-
    read_csv(file.path(datasets_dir, "test_data.csv")) %>%
    # delete GTQ column
    select(-GTQ)

  # SHAP Vlaues
  selected_models <-
    read_csv(file.path(scenario_dir, "selected_models.csv"))

  h2o.no_progress()
  # Fucntion that calculate shap values for a model
  CalculateAllObsShapValues <- function(model_id) {
    # Load the model
    model_h2o <- h2o.loadModel(file.path(
      models_dir,
      model_id
    ))
    # Create function to calculate each SHAP value
    CalculateShapValues <- function(...) {
      ive <- shapper::individual_variable_effect(
        model_h2o,
        new_observation = data.frame(...),
        data = train_input,
        predict_function = h2oPredict,
        label = model_id
      )
      return(as_tibble(ive))
    }

    # Calculate shap_values
    shap_values <- test_input %>%
      pmap_df(CalculateShapValues)

    h2o.removeAll()
    return(shap_values)
  }

  all_shap_values <- selected_models %>%
    select(model_id) %>%
    mutate(shap = map(model_id, CalculateAllObsShapValues))

  ## Prepare and Save Data for AHMoSe ----
  models_data <- all_shap_values %>%
    unnest(cols = c(shap)) %>%
    # Add observation number from test set
    left_join(test_input %>% mutate(item = row_number())) %>%
    # Rename columns
    rename(
      model = model_id,
      feature = `_vname_`,
      shap_value = `_attribution_`,
    ) %>%
    # Calculate expected value
    mutate(expected_value = `_yhat_mean_` + shap_value) %>%
    # Get the corresponding feature value for each explanation
    mutate(value = pmap_dbl(
      .,
      function(..., feature) list(...)[[feature]]
    )) %>%
    # Get necessary rows for AHMoSe
    select(model, item, feature, value, shap_value, expected_value) %>%
    arrange(model, feature, item)

  write_csv(models_data, file.path(scenario_dir, "models_data.csv"))
  return(invisible(models_data))
}

## Apply to each scenario ----
scenarios <- c("scenario-A", "scenario-B")
map(scenarios, GenerateExplanations)

## Shutdown h2o ----
h2o.shutdown(prompt = FALSE)
