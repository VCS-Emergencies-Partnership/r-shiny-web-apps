library("dplyr")
library("glue")
library("magrittr")
library("pals")
library("tidyverse")

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
                      local_dir = "~/r-shiny-web-apps/packages/dashboard/data") {
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


capac_path <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/de_2019_lads/data/capacity/disasters-emergencies/england/de-index-quint.csv"
vuln_path <- "https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/de_2019_lads/data/vulnerability/disasters-emergencies/england/de-index-quint.csv"

# Read in D&E capacity and vulnerability
capac <- read_csv(capac_path) %>%
  rename("LAD19CD" = lad_code, `DE Capacity quintile` = de_domain_quantiles)

vuln <- read_csv(vuln_path) %>%
  rename("LAD19CD" = lad_code, `DE Vulnerability quintile` = de_domain_quantiles)

de_ri <- vuln %>%
  left_join(capac, by = "LAD19CD") 

# Check missings
de_ri %>%
  filter_at(vars(everything()), any_vars(is.na(.)))

# Create 3 buckets for vulnerability
quantiles_vuln = de_ri %>%
  pull(`DE Vulnerability quintile`) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# Create 3 buckets for resilience
quantiles_res = de_ri %>%
  pull(`DE Capacity quintile`) %>%
  quantile(probs = seq(0, 1, length.out = 4), na.rm = TRUE)

# https://nowosad.github.io/post/cbc-bp2/ --> using brewer.seseq2
bivariate_color_scale = tibble(
  # High inequality, high income
  "3 - 3" = "#000000",
  "2 - 3" = "#b36600",
  # Low inequality, high income #f0f0f0
  "1 - 3" = "#f3b300",
  "3 - 2" = "#376387",
  # Medium inequality, medium income
  "2 - 2" = "#b3b3b3",
  "1 - 2" = "#f3e6b3",
  # High inequality, low income
  "3 - 1" = "#509dc2",
  "2 - 1" = "#b4d3e1",
  # Low inequality, low income "#f3f3f3"
  "1 - 1" = "#d9d9d9",
  # For areas with no capacity info leave with no colour
  "1 - NA" = "",
  "2 - NA" = "",
  "3 - NA" = ""
) %>%
  gather("group", "fill")

# Cut into groups defined above and join fill
de_ri_colours <- de_ri %>%
  mutate(
    vuln_quantiles_de = cut(
      `DE Vulnerability quintile`,
      breaks = quantiles_vuln,
      include.lowest = TRUE
    ),
    res_quantiles_de = cut(
      `DE Capacity quintile`,
      breaks = quantiles_res,
      include.lowest = TRUE
    ),
    # By pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(as.numeric(vuln_quantiles_de), "-",
                  as.numeric(res_quantiles_de))
  ) %>%
  # We now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group") %>%
  rename(group_de = group, fill_de = fill)

# Write resilience index with appropriate fill
write_data(feather::write_feather,
                    de_ri_colours,
                   "de_ri.feather",
                   local_dir = "data")

