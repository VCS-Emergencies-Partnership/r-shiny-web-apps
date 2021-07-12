library("feather")
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
  ifelse(isTRUE(is_databricks()), "/dbfs/mnt/", "/data/")
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

# Proportion respondents reporting need function
proportion_responded = function(survey_file) {
  pulse_needed = read_csv(survey_file)

  # CHECK - ARE THE COLUMNS I NEED IN THERE?
  cols_needed =
    c(
      "_index",
      "What is your Multi-Agency Cell area?",
      "In which county/unitary authority does your organisation operate?"
    )
  # Are all the columns i use with easy names there?
  area_names_still_present =
    (cols_needed %in% colnames(pulse_needed))
  # Is there still a question containing this string:
  question_still_present =
    grepl(
      "following sectors are the top three priority concerns in your area in the next 14 days",
      colnames(pulse_needed)
    )
  question_still_present_cols =
    pulse_needed[question_still_present]

  # If logical vector is not all T - there's a missing column name
  # or if there's no columns containing the following sectors comment the question has changed
  if (all(area_names_still_present) == FALSE ||
      dim(question_still_present_cols)[2] == 0) {
    stop("Couldn't find required columns in pulse check survey")
  } else {
    # Filter by column number to get concerns in next 14 days other relating to this is col 404 - not needed as this is counted but column on what was required is provided which is other:
    pulse_needed =
      pulse_needed %>% select(
        `_index`,
        `What is your Multi-Agency Cell area?`,
        `In which county/unitary authority does your organisation operate?`,
        contains(
          "following sectors are the top three priority concerns in your area in the next 14 days?/"
        )
      )

    # How many respondents
    respondents = nrow(pulse_needed)

    # Try remove ridiculous long names
    pulse_needed_clean = pulse_needed %>%
      rename_with( ~ paste0(
        sub(
          "In your view, which of the following sectors are the top three priority concerns in your area in the next 14 days?/*",
          "",
          .
        )
      ),
      starts_with("In your"))

    # group_by
    concerns =
      pivot_longer(pulse_needed_clean,
                   4:15,
                   names_to = "Concerns",
                   values_to = "total")

    concerns_by_type =
      concerns %>%
      group_by(`Concerns`) %>%
      summarise(group_total = sum(total,
                                  na.rm = TRUE)) %>%
      mutate(proportion_respondents = round((group_total / respondents) * 100, 1))

    concerns_by_type$clean_groups =
      gsub("[[:punct:]]", "", concerns_by_type$Concerns)

    # Remove pulse none and other data and clean group names
    not_wanted = c("None", "Not sure")
    concerns_by_type =
      concerns_by_type %>%
      filter(!clean_groups %in% not_wanted) %>% #, Concerns != "?/Other") %>%
      mutate(
        "clean_names" = case_when(
          clean_groups == "Employment advice  support" ~ "Employment",
          clean_groups == "IT access  digital divide" ~ "Digital exclusion",
          clean_groups == "Hardship  financial support" ~ "Financial hardship",
          clean_groups == "Housing temporary accommodation" ~ "Housing",
          clean_groups == "Protection  safeguarding" ~ "Safegaurding",
          TRUE ~ as.character(.$clean_groups)
        )
      ) %>%
      mutate("total_respondents" = respondents)

    return(concerns_by_type)

  }

}

# Retrieving from raw section
get_pulse = list.dirs(glue::glue("/{get_mount_point()}/data-lake/raw/pulse_check_raw/"))
latest_file_name = paste(tail(get_pulse, n = 1),
                         "pulse_check_raw.csv",
                         sep = "/")
last_but_one_file_name = paste(tail(get_pulse, n = 2),
                               "pulse_check_raw.csv",
                               sep = "/")
last_but_one_file_name = last_but_one_file_name[1]

# Does file exist
if (!file.exists(latest_file_name) ||
    !file.exists(last_but_one_file_name)) {
  message = paste0("Problem: a file is missing")
  stop(message)
} else {
  latest_survey = proportion_responded(latest_file_name)
  previous_survey = proportion_responded(last_but_one_file_name)

  glimpse(previous_survey)
  glimpse(latest_survey)

  joined_surveys =
    left_join(latest_survey, previous_survey, by = "clean_names")
  #glimpse(joined_surveys)
  greatest_change =
    joined_surveys %>%
    mutate("greatest_diff" = proportion_respondents.x - proportion_respondents.y) %>%
    select(1:6, 12) %>%
    rename_with( ~ str_remove(., ".x"))

  glimpse(greatest_change)

  write_data(
    feather::write_feather,
    greatest_changes,
    "pulse_check_summary.feather",
    local_dir = "~/r-shiny-web-apps/packages/dashboard/data/vcs_indicators/"
  )
}
