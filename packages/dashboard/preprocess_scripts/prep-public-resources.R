library("feather")
library("tidyverse")
library("readxl")

#' Use existence of environment variable to determine whether we're on
#' databricks
#'
is_databricks = function() {
  Sys.getenv("DATABRICKS_RUNTIME_VERSION") != ""
}

#' The datalake is mounted onto /mnt/ on Databricks, and /data/ on the DSVM
#'
get_mount_point = function() {
  ifelse(isTRUE(is_databricks()), "/dbfs/mnt/", "/data/")
}

#' Establish connection with blob storage
#'
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

# Look in raw section
resources_dirs = list.dirs(path = glue::glue("{get_mount_point()}/data-lake/raw/collection-of-public-resources-links-2021/"),
                           full.names = TRUE)
resources_file = paste0(tail(resources_dirs, n = 1), "/collection-of-public-resources-links-2021.xlsx")

resources = read_excel(resources_file)



wanted = c("YES", "Yes", "yes")
# Filter out no
to_include = resources %>%
  filter(`Relevance/Include?` %in% wanted)

# Write to feather
write_data(
  feather::write_feather,
  to_include,
  "resource_bank.feather",
  local_dir = "~/r-shiny-web-apps/packages/dashboard/data/resource_bank/"
)
