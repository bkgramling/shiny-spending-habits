library(yaml)
library(tidyverse)

load_csv_using_yaml <- function(
  directory_to_use,
  metadata_file_name = 'metadata.yaml'
) {
   directory_to_use <- 'data/mint'
   metadata_file_name <- 'metadata.yaml'
  
  ## We'll read the file in as a string, and send that to yaml::read_yaml(),
  ## since read_yaml() will throw an error if there's not a blank line at
  ## the bottom of the file. So we'll add a blank line ourselves first.
  yaml_data <- paste0(
    readr::read_file(file.path(directory_to_use, metadata_file_name)),
    '\n'
  ) %>% 
    yaml::read_yaml(text = .)
  
  ## See http://serialmentor.com/blog/2016/6/13/reading-and-combining-many-tidy-data-files-in-R
  ## re: reading in all files in a directory:
  file_names <- dir(directory_to_use, pattern = "*.csv")
  
  data_from_directory <- file_names %>%
    # Read in each file
    purrr::map_dfr(function(x) read_csv(file.path(directory_to_use, x)))
  
  ## This expects arguments that are selectors for a list:
  list_na_if_null <- function(list_to_parse, ...) {
    selectors <- list(...) %>% purrr::as_vector()
    
    dplyr::if_else(
      list_to_parse %>% purrr::pluck(selectors) %>% is.null(),
      as.character(NA),
      list_to_parse %>% purrr::pluck(selectors)
    )
  }
  # yaml_data %>% list_na_if_null('a', 'b')  ## Will look for list[['a']][['b']]
  # yaml_data %>% list_na_if_null('source_title')
  
  source_title <- yaml_data %>% list_na_if_null('source_title')
  date_column <- yaml_data %>% list_na_if_null('date_column')
  date_format <- yaml_data %>% list_na_if_null('date_format')
  description_column_name <- yaml_data %>% list_na_if_null('description_column_name')
  amount_column <- yaml_data %>% list_na_if_null('amount_column')
  transaction_type_column <- yaml_data %>% list_na_if_null('transaction_type_column')
  category_column <- yaml_data %>% list_na_if_null('category_column')
  account_column <- yaml_data %>% list_na_if_null('account_column')
  label_column <- yaml_data %>% list_na_if_null('label_column')
  notes_column <- yaml_data %>% list_na_if_null('notes_column')
  
  ## Implement our YAML data from above:
  data_from_directory %>% 
    purrr::when(
      !is.na(date_format) ~ (.) %>% 
        dplyr::mutate(
          Date = as.Date(!!as.name(date_column), format = date_format)
        )
    ) %>% 
    dplyr::mutate(
      Month = format(Date, "%m") %>% as.integer(),
      Year = format(Date, "%Y") %>% as.integer(),
      Day = format(Date, "%d") %>% as.integer()
    ) %>%
    dplyr::mutate(
      debit_column = if_else(
        data_from_directory$`Transaction Type`=='debit', as.numeric(data_from_directory$Amount),0
      )
    ) %>%
    dplyr::mutate(
      credit_column = if_else(
        data_from_directory$`Transaction Type`=='credit', as.numeric(data_from_directory$Amount),0
      )
    ) %>%
    dplyr::mutate(
      Source = source_title,
      Credit = credit_column,
      Debit = debit_column
    )  %>% 
    dplyr::select(
      Source, Date, Month, Day, Year, Credit, Debit, Description,
      Category, `Account Name`, Labels, Notes
    ) %>% 
    dplyr::distinct()
}

