library("tidyverse")
library("feather")

# Use existence of environment variable to determine whether we're on databricks
on_databricks = is.environment("DATABRICKS_RUNTIME_VERSION")

# The datalake is mounted onto /mnt/ on Databricks, and /data/ on the DSVM
mnt_point = ifelse(isTRUE(on_databricks), "/mnt/", "/data/")

# Establish connection with blob storage
if (isTRUE(on_databricks)) {
  blob = AzureStor::storage_endpoint(Sys.getenv("BLOB_ENDPOINT"),
                                     sas = Sys.getenv("BLOB_SAS"))
  out_cont = AzureStor::storage_container(blob, "processed")
}

# Look in raw section
covid_dirs = list.dirs(
  path = glue::glue("{mnt_point}/data-lake/raw/coronavirus-cases/"),
  full.names = TRUE
)
covid_file = paste0(tail(covid_dirs, n = 1), "/coronavirus_cases.csv")

# Does the file exist
if (!file.exists(covid_file)) {
  message = glue::glue("Problem: {covid_file} doesn't exist!")
  stop(message)
} else {
  retrieve_covid_data = read_csv(covid_file)

  # CHECK - columns used present
  # CHECK - are the columns needed present?
  cols_needed =
    c(
      "areaCode",
      "areaName",
      "date",
      "newCasesBySpecimenDate",
      "newCasesBySpecimenDateChangePercentage",
      "newCasesBySpecimenDateRollingRate"
    )
  # CHECK - are all the columns I use with easy names there?
  cols_still_present = (cols_needed %in% colnames(retrieve_covid_data))

  # If logical vector is not all T there's a missing column name
  if (all(cols_still_present) == F) {
    stop("Problem - column names changed")
  } else {
    # Rolling seven day average data based on covid specimen date is only
    # complete for up to 5 days before current date so need to filter on 5 days
    # before current day - but it is updated late in the day we really need 6
    # days prior
    latest_specimen_date = as.Date(Sys.Date()) - 6

    # Retrieve latest 7 day rolling cases per 100,000 and percentage change in
    # cases
    latest_covid_data =
      retrieve_covid_data %>% rename(LAD19CD = areaCode) %>%
      # Today's seven day rolling average
      filter(as.Date(date) == as.Date(latest_specimen_date) &
               str_detect(LAD19CD, "^E"))

    # Read in area lookups
    area_lookup =
      read_csv(
        "https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv"
      )
    area_lookup_tc2lad =
      area_lookup %>%
      select("LAD19CD", "TacticalCell") %>%
      unique()

    # Align with tactical cells
    latest_covid_data2tactical_cell =
      left_join(area_lookup_tc2lad, latest_covid_data, by = "LAD19CD") %>%
      filter(
        TacticalCell != "Scotland" &
          TacticalCell != "Wales" &
          TacticalCell != "Northern Ireland and the Isle of Man"
      )

    # CHECK - Are Hackney, London, Cornwall and Isles of Scilly combined?
    combined_authorities =
      c("Hackney and City of London", "Cornwall and Isles of Scilly")
    test_combined_auth_present =
      latest_covid_data2tactical_cell %>%
      filter(areaName %in% combined_authorities)

    if (dim(test_combined_auth_present)[1] != 0) {
      # Correct for combined authorities
      # Correct Hackney and City of London combined
      duplicate_hackney = latest_covid_data2tactical_cell %>%
        filter(areaName == "Hackney and City of London") %>%
        mutate("clean_areaNames" = "Hackney") %>%
        mutate("clean_LAD19CD" = "E09000012")

      duplicate_city = latest_covid_data2tactical_cell %>%
        filter(areaName == "Hackney and City of London") %>%
        mutate("clean_areaNames" = "City of London") %>%
        mutate("clean_LAD19CD" = "E09000001")

      # Correct Cornwall and Isle of Scilly
      duplicate_cornwall = latest_covid_data2tactical_cell %>%
        filter(areaName == "Cornwall and Isles of Scilly") %>%
        mutate("clean_areaNames" = "Cornwall") %>%
        mutate("clean_LAD19CD" = "E06000052")

      duplicate_isle_scilly = latest_covid_data2tactical_cell %>%
        filter(areaName == "Cornwall and Isles of Scilly") %>%
        mutate("clean_areaNames" = "Isles of Scilly") %>%
        mutate("clean_LAD19CD" = "E06000053")

      # Remove combined authorities
      latest_covid_data2tactical_cell =
        latest_covid_data2tactical_cell %>%
        mutate("clean_LAD19CD" = LAD19CD) %>%
        mutate("clean_areaNames" = areaName) %>%
        filter(!areaName %in% combined_authorities)

      # Latest covid data
      latest_covid_data2tactical_cell =
        rbind(
          latest_covid_data2tactical_cell,
          duplicate_hackney,
          duplicate_city,
          duplicate_cornwall,
          duplicate_isle_scilly
        )
      latest_covid_data2tactical_cell =
        latest_covid_data2tactical_cell %>%
        select(-LAD19CD) %>% rename(LAD19CD = "clean_LAD19CD") %>%
        # Remove NAs that have been introduced by cleaning stuff up.
        filter(!is.na(areaName))
    }

    # If running on databricks write to blob storage
    fname = "areas2focus_covid.feather"
    if (isTRUE(on_databricks)) {
      feather_path = glue::glue("{tempfile()}.feather")
      feather::write_feather(latest_covid_data2tactical_cell, feather_path)
      AzureStor::storage_upload(out_cont,
                                src = feather_path,
                                dest = fname)
      # Otherwise, write locally
    } else {
      feather::write_feather(
        latest_covid_data2tactical_cell,
        glue::glue(
          "~/r-shiny-web-apps/packages/dashboard/data/areas_to_focus/{fname}"
        )
      )
    }
  }
}
