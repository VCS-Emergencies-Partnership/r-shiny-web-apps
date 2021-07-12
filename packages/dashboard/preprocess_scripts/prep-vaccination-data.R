library("feather")
library("httr")
library("purrr")
library("readxl")
library("tidyverse")

#' Use existence of environment variable to determine whether we're on
#' databricks
#'
#' @export
is_databricks = function() {
  Sys.getenv("DATABRICKS_RUNTIME_VERSION") != ""
}

#' The datalake is mounted onto /mnt/ on Databricks, and /data/ on the DSVM
#'
#' @export
get_mount_point = function() {
  ifelse(isTRUE(is_databricks()), "/mnt/", "/data/")
}

#' Establish connection with blob storage
#'
#' @export
get_container = function() {
  blob = AzureStor::storage_endpoint(Sys.getenv("BLOB_ENDPOINT"),
                                     sas = Sys.getenv("BLOB_SAS"))
  AzureStor::storage_container(blob, "processed")
}

#' Write data to blob or local storage
#'
#' @param writer The function to use to write data, eg. feather::write_feather
#' @param data The data to write
#' @param filename The name of the file to write to (this should not be a path)
#' @param local_dir Optional: Directory to write data to (for non-databricks runs)
#' @param cont Optional: connection to an Azure blob container
#' @export
write_data = function(writer,
                      data,
                      filename,
                      local_dir = "~/r-shiny-web-apps/packages/dashboard/data/areas_to_focus/") {
  # If a container is passed write to it
  if (isTRUE(is_databricks())) {
    # Get extension of file from filename
    file_ext = strsplit(filename, "\\.")[[1]][2]
    # Create path to temporary file
    tmp_path = glue::glue("{tempfile()}.{file_ext}")
    # Write data to temporary file
    writer(data, tmp_path)
    AzureStor::storage_upload(get_container(),
                              src = tmp_path,
                              dest = filename)
    # Otherwise, write locally
  } else {
    writer(data, file.path(local_dir, filename))
  }
}

mnt = get_mount_point()

# CHECK Population data still exists
pop_data = glue::glue("/{mnt}/data-lake/raw/ons-populstion-estimates-mid-year-2019/2021-04-12-10-30-27/ons-populstion-estimates-mid-year-2019.xlsx")

if (!file.exists(pop_data)) {
  stop("Population data missing")
}

pop_eng_2019 =
  read_excel(
    glue::glue("/{mnt}/data-lake/raw/ons-populstion-estimates-mid-year-2019/2021-04-12-10-30-27/ons-populstion-estimates-mid-year-2019.xlsx"),
    sheet = "Mid-2019 Persons",
    skip = 4
  )
# In vac data - this population data is referenced - https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates

ages = pop_eng_2019 %>%
  select(8:ncol(.))

# Function to sum populations by age rage
population_by_age = function(bracket, age_start, age_end) {
  # Cols to select
  if (age_end == "over") {
    # Because age zero is in and R is 1 indexed, the index of the column containing the right age is age plus 1
    age_start = as.integer(age_start) + 1

    age_range_calc =
      ages %>%
      select(age_start:ncol(.)) %>%
      mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
      mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
      select(total_pop_age_range) %>%
      mutate("age_range" = bracket) %>%
      head(n = 1)
  } else {
    # Because age zero is in and R is 1 indexed, the index of the column containing the right age is age plus 1
    age_start = as.integer(age_start) + 1
    age_end = as.integer(age_end) + 1

    age_range_calc =
      ages %>%
      select(age_start:age_end) %>%
      mutate(total_pop_by_area = rowSums(across(where(is.numeric)))) %>%
      mutate(total_pop_age_range = sum(total_pop_by_area)) %>%
      select(total_pop_age_range) %>%
      mutate("age_range" = bracket) %>%
      head(n = 1)
  }

  return(age_range_calc)
}

# Population by region

# Vaccination data
# Now retrieving from raw section
if (!dir.exists(glue::glue("/{mnt}/data-lake/raw/nhs-weekly-vaccination-data/"))) {
  stop("Vaccination data has moved")
}

get_requests = list.dirs(glue::glue("/{mnt}/data-lake/raw/nhs-weekly-vaccination-data/"))

file_name =
  paste(tail(get_requests, n = 1),
        "nhs_weekly_vaccination_data.xlsx",
        sep = "/")

if (!file.exists(file_name)) {
  stop("Vaccination file name has changed")
}

# Get latest update period
vaccination_metadata = read_excel(file_name,
                                  sheet = "LTLA",
                                  skip = 2)

vaccination_metadata = vaccination_metadata %>%
  select(1:2)

colnames(vaccination_metadata) = c("Info", "Value")

vaccination_source = vaccination_metadata[2, ]
vaccination_time_period = vaccination_metadata[1, ]
vaccination_publish_date = vaccination_metadata[4, ]

# Read in table of data
vaccination_data_raw = read_excel(file_name,
                                  sheet = "LTLA",
                                  skip = 12)

# For homepage remove first geog data
vaccination_data_summary = head(vaccination_data_raw , 1)

# Remove first 6 cols which are geog data
vaccination_data_summary  =
  vaccination_data_summary %>%
  select(7:ncol(.))

if (startsWith(colnames(vaccination_data_summary)[1], ".") == TRUE) {
  stop("Vaccination columns changed")
}

# Remove all columns that are just NA
vaccination_data_summary  =
  vaccination_data_summary %>%
  select(where(~ !all(is.na(.x))))

# What age groups are we working with
age_brackets = strsplit(colnames(vaccination_data_summary), "[.]")
age_brackets = map(age_brackets, 1) %>%
  as_vector() %>%
  unique()
age_brackets = age_brackets[age_brackets != ""]
age_brackets

age_bracket_populations = data.frame(matrix(ncol = 2, nrow = 0))
colnames(age_bracket_populations) = c("total_pop_age_range",
                                      "age_range")

# Calculate population
for (x in age_brackets) {
  if (startsWith(x, "Under") == TRUE) {
    top_age = str_split(x, " ")
    # Because it is under this age needs to be -1
    top_age = as.integer(top_age[[1]][2]) - 1
    top_age = as.character(top_age)
    bottom_age = "0"
  } else {
    if (endsWith(x, "+")) {
      bottom_age = str_split(x, "[+]")
      bottom_age = bottom_age[[1]][1]
      #print(bottom_age)
      top_age = "over"
    } else {
      age_range = str_split(x, "-")
      bottom_age = age_range[[1]][1]
      top_age = age_range[[1]][2]
    }
  }

  # Call function
  population_of_age_bracket = population_by_age(x, bottom_age, top_age)
  age_bracket_populations = rbind(age_bracket_populations,
                                  population_of_age_bracket)
}

# Size of dataframe should be length of colnames*2 + 1 (as there's a total column)
size_of_df_expected = as.integer(length(age_brackets) + length(age_brackets)) + 1
if (dim(vaccination_data_summary)[2] != size_of_df_expected) {
  stop("something wrong with even split between age ranges in first and second dose")
}

# This works because first and second dose will (should) have the same number of age bracket columns
first_dose_index_end = length(age_brackets)
second_dose_index_start = as.integer(length(age_brackets)) + 1
second_dose_index_end =
  as.integer(length(age_brackets) + length(age_brackets))

first_doses =
  vaccination_data_summary %>%
  select(1:as.integer(first_dose_index_end))
second_doses =
  vaccination_data_summary %>%
  select(all_of(second_dose_index_start):all_of(as.integer(second_dose_index_end)))

# Replace colnames with clean colnames
colnames(first_doses) = age_brackets
colnames(second_doses) = age_brackets

# Add which doses are which
first_doses = first_doses %>%
  mutate("dose" = "First dose")
second_doses = second_doses %>%
  mutate("dose" = "Second dose")

# Combine doses
both_doses = rbind(first_doses, second_doses)

# Transpose longer - number of age brackets needs to be dynamic
both_doses_tr =
  pivot_longer(
    both_doses,
    cols = 1:as.integer(length(age_brackets)),
    names_to = "age_range",
    values_to = "number_of_doses"
  )

# Join data sets
doses_by_population =
  left_join(both_doses_tr, age_bracket_populations, by = "age_range")

final_doses_by_population = doses_by_population %>%
  mutate(prop_of_population = round((
    number_of_doses / total_pop_age_range
  ) * 100, 1)) %>%
  mutate(source = vaccination_source$Value) %>%
  mutate(published = vaccination_publish_date$Value) %>%
  mutate(time_span = vaccination_time_period$Value)
#write_feather(final_doses_by_population, "~/vaccination_rate.feather")
#glimpse(final_doses_by_population)

write_data(
  feather::write_feather,
  final_doses_by_population,
  "vaccination_rate.feather"
)

# # calculate
# vaccination_data_raw = read_excel(file_name,
#                                    sheet = "LTLA", skip = 12)
#
#
