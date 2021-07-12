library("feather")
library("lubridate")
library("PostcodesioR")
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

# Lookup table
lookup =
  read_csv(
    "https://raw.githubusercontent.com/britishredcrosssociety/covid-19-vulnerability/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv"
  ) %>%
  select(LAD19CD, TacticalCell) %>%
  mutate(
    "TacticalCell" = case_when(
      TacticalCell == "South West and the Channel Islands" ~ "South and the Channel Islands",
      TacticalCell == "Central" ~ "Midlands and East",
      TRUE ~ (as.character(.$TacticalCell))
    )
  ) %>%
  filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  ) %>%
  unique()

# Today's date
# Need to know current date to append to request for support
date_time = Sys.time()
date_time = str_split(date_time, " ")
date = format(as.Date(date_time[[1]][1]), "%d-%m-%Y")

# Now retrieving from raw section
get_requests = list.dirs(glue::glue("/{get_mount_point()}/data-lake/raw/vcsep-requests-for-support/"))
file_name = paste(tail(get_requests, n = 1),
                  "vcsep-requests-for-support.csv",
                  sep = "/")

if (!file.exists(file_name)) {
  message = paste0("Promblem:", file_name, "or path doesn't exist")
  stop(message)
}

requests = read_csv(file_name)

# CHECK IS THIS IS TODAYS DATA EXTRACT
date_of_last_update = str_split(tail(get_requests, n = 1), "/")
date_of_last_update = tail(date_of_last_update[[1]], n = 1)
date_format = str_split(date_of_last_update, "-")
date_of_last_update =
  paste0(date_format[[1]][3], "-", date_format[[1]][2], "-", date_format[[1]][1])

if (date != date_of_last_update) {
  stop("problem - NOT TODAY'S DATA")
}

# Do the columns i need exist
columns_in_raw_data = colnames(requests)
cols_needed = c("status",
                "request_date",
                "multi_agency_cell",
                "postcode")
are_cols_present = (cols_needed %in% columns_in_raw_data)

# are_cols_present not all T means missing names
if (all(are_cols_present) == FALSE) {
  print("broken - columns changed")
}

# Filtering for current live requests
requests = requests %>%
  filter(!is.na(request_date)) %>%
  separate(request_date,
           c("clean_date", NA),
           remove = FALSE,
           sep = " ") %>%
  mutate("formatted_date" = ymd(clean_date)) %>%
  mutate("postcode" = case_when(postcode == "NW65HE" ~ "NW6 5HE",
                                TRUE ~ (as.character(postcode)))) %>%
  mutate(
    "TacticalCell" = case_when(
      multi_agency_cell == "South West and the Channel Islands" ~ "South and the Channel Islands",
      multi_agency_cell == "Central" ~ "Midlands and East",
      TRUE ~ (as.character(.$multi_agency_cell))
    )
  )

# Extract postcodes
open_postcodes = requests$postcode
open_postcodes = as.list(as.vector(requests$postcode))

wanted_data = list()
# Geocode
for (i in open_postcodes) {
  df = tryCatch({
    # what i want returned
    postcode_lookup(i)
  },
  error = function(cond) {
    return(NA)
  })

  if (!is.na(df) && !is.null(df)) {
    wanted =
      df %>%
      select("postcode",
             "admin_district_code",
             "longitude",
             "latitude")
    wanted_data[[i]] = wanted
  }

}

# Make dataframe
postcode2lad = do.call(rbind, wanted_data)

# Join to request data:
requests_geocoded =
  left_join(requests, postcode2lad, by = "postcode")

all_requests =
  requests_geocoded %>%
  mutate("request_status" =
           case_when(!grepl("Closed", status) ~ "Active",
                     TRUE ~ (as.character("Closed"))
           )) %>%
  mutate("request_status_col" = case_when(!grepl("Closed", status) ~ "red",
                                          TRUE ~ (as.character("blue")))) %>%
  mutate("request_status_radius" = case_when(!grepl("Closed", status) ~ 6,
                                             TRUE ~ 4)) %>%
  mutate("request_status_opacity" = case_when(!grepl("Closed", status) ~ 1,
                                              TRUE ~ 0.4))
#glimpse(all_requests)

write_data(
  feather::write_feather,
  all_requests,
  "all_requests.feather",
  local_dir = "~/r-shiny-web-apps/packages/dashboard/data/vcs_indicators/"
)
