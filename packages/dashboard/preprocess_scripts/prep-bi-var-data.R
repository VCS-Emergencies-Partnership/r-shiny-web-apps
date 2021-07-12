# Prep bivariate data for map
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#create-a-bivariate-choropleth

library("dplyr")
library("glue")
library("magrittr")
library("pals")
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
  ifelse(isTRUE(on_databricks), "/mnt/", "/data/")
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

data_repo = "https://github.com/britishredcrosssociety"
LA_vi_path = "covid-19-vulnerability/raw/master/output/vulnerability-LA.csv"
LA_res_path = "resilience-index/raw/main/depreciated/data/processed/resilience%20index.csv"

# Read in vulnerability indices
# Local authority level
LA_vi = readr::read_csv(glue::glue("{data_repo}/{LA_vi_path}"))
LA_vi %<>% rename("LAD19CD" = Code)

# Read in resilience indices
# Local authority level
LA_res = readr::read_csv(glue::glue("{data_repo}/{LA_res_path}"))

# Create 3 buckets for vulnerability
quantiles_vuln = LA_vi %>%
  pull(`Vulnerability quintile`) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# Create 3 buckets for resilience
quantiles_res = LA_res %>%
  pull(`Capacity quintile`) %>%
  quantile(probs = seq(0, 1, length.out = 4))

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
  "1 - 1" = "#d9d9d9"
) %>%
  gather("group", "fill")

# Cut into groups defined above and join fill
LA_res %<>%
  mutate(
    vuln_quantiles = cut(
      `Vulnerability quintile`,
      breaks = quantiles_vuln,
      include.lowest = TRUE
    ),
    res_quantiles = cut(
      `Capacity quintile`,
      breaks = quantiles_res,
      include.lowest = TRUE
    ),
    # By pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group = paste(as.numeric(vuln_quantiles), "-",
                  as.numeric(res_quantiles))
  ) %>%
  # We now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group")

# Write resilience index with appropriate fill
write_data(feather::write_feather,
                   LA_res,
                   "resilience_index_bivar.feather",
                   local_dir = "../data/")

# Separate the groups
forlegend = bivariate_color_scale %>%
  separate(group,
           into = c("vuln_quantiles",
                    "res_quantiles"),
           sep = " - ") %>%
  mutate(vuln = as.integer(vuln_quantiles),
         res = as.integer(res_quantiles))

# Regenerate legend plot
g.legend =
  ggplot(forlegend, aes(vuln, res, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  scale_x_discrete(position = "top") +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    plot.margin  =  margin(t  =  10, b = 10, l = 10)
  ) +
  labs(x = "Higher vulnerability",
       y = "Higher capacity") +
  theme(
    axis.title = element_text(color = "black", size = 40),
    axis.title.y = element_text(
      angle = 90,
      vjust = -8,
      hjust = 0.5
    )
  ) +
  coord_fixed() +
  annotate(
    "segment",
    x = 0.5,
    y = 3.75,
    xend = 3.5,
    yend = 3.75,
    size = 3,
    col = "black",
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  annotate(
    "segment",
    x = 0.2,
    y = 3.5,
    xend = 0.2,
    yend = 0.5,
    size = 3,
    col = "black",
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  annotate(
    geom = "text",
    x = 3,
    y = 3,
    label = "Most in\n need",
    color = "white",
    size = 10.5438
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 1,
    label = "Least in\n need",
    color = "black",
    size = 10.5438
  )

# print legend
g.legend
# saving legend
par()
g.legend

if (isTRUE(is_databricks())) {
  fpath = glue::glue("{tempfile()}.png")
} else {
  fpath = "../www/bivar-legend_v2.png"
}

dev.print(
  png,
  fpath,
  bg = "transparent",
  width = 800,
  height = 600
)

if (isTRUE(is_databricks())) {
  AzureStor::storage_upload(get_container(),
                            src = fpath,
                            dest = "bivar-legend_v2.png")
}

dev.off()
