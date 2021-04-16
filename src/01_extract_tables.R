## Load libraries ----
library(tidyverse)

## PDF file ----
pdf_file <- here::here("data-raw", "Tagarakis2014.pdf")

if (file.exists(pdf_file)) {
  ## Extract fuzzy rules ----
  fuzzy_rules <-
    tabulizer::extract_tables(pdf_file,
      pages = 8,
      output = "data.frame"
    ) %>%
    .[[1]] %>%
    set_names(.[1, ]) %>%
    as.matrix() %>%
    .[-1, ] %>%
    {
      rbind(.[, 1:4], .[, 5:8])
    } %>%
    as_tibble() %>%
    separate("TSS TA", c("TSS", "TA"))

  ## Extract grape data ----
  ExtractGrapeData <- function(pages, area) {
    df <-
      tabulizer::extract_tables(pdf_file,
        pages = pages,
        area = area,
        guess = FALSE
      ) %>%
      # Join two first rows and set them as headers
      map(~ .x %>%
        as_tibble(.name_repair = "minimal") %>%
        set_names(trimws(paste(.[1, ], .[2, ]))) %>%
        slice(-c(1, 2))) %>%
      # Bind both sections of the table
      {
        bind_rows(.[[1]], .[[2]])
      } %>%
      # Join the Fuzzy Evaluation of observations that expand in 2 rows
      mutate(group = row_number() - if_else(Cell == "", 1, 0)) %>%
      group_by(group) %>%
      mutate(`Fuzzy evaluation` = paste0(`Fuzzy evaluation`, collapse = " ")) %>%
      slice(1) %>%
      ungroup() %>%
      select(-group) %>%
      # Transform columns to numeric (deleting thousands space where there is)
      mutate_at(
        vars(-c(Cell, `Fuzzy evaluation`, `Expert evaluation`)),
        ~ as.numeric(gsub(" ", "", .))
      )

    return(df)
  }

  data_2010 <- ExtractGrapeData(
    c(10, 11),
    list(c(90, 45, 625, 400), c(62, 45, 350, 400))
  ) %>% rename(Output = output)
  data_2011 <- ExtractGrapeData(
    c(12, 13),
    list(c(90, 45, 625, 400), c(62, 45, 400, 400))
  )
  data_2012 <- ExtractGrapeData(
    c(14, 15),
    list(c(90, 45, 625, 400), c(62, 45, 390, 400))
  )

  # Fix error in paper
  data_2010[9, 9] <- 3.10

  # Join data, prevously adding columns with the year
  grape_quality_data <- bind_rows(
    data_2010 %>% mutate(Year = 2010),
    data_2011 %>% mutate(Year = 2011),
    data_2012 %>% mutate(Year = 2012)
  )

  ## Save data in csv ----
  write_csv(fuzzy_rules, here::here("data", "fuzzy_rules.csv"))
  write_csv(grape_quality_data, here::here("data", "grape_quality_data.csv"))
} else {
  warning(paste0("File Tagarakis2014.pdf is not in data-raw folder. ",
                 "Loading backup extracted tables."))
  fuzzy_rules <- 
    read_csv( here::here("data-raw", "backup_fuzzy_rules.csv"))
  grape_quality_data <- 
    read_csv( here::here("data-raw", "backup_grape_quality_data.csv"))
  
  write_csv(fuzzy_rules, here::here("data", "fuzzy_rules.csv"))
  write_csv(grape_quality_data, here::here("data", "grape_quality_data.csv"))
  
}
