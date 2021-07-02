# Prep bivariate data for map
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#create-a-bivariate-choropleth

library("AzurStor")
library("dplyr")
library("tidyverse")
library("magrittr")
library("pals")

# Use existence of environment variable to determine whether we're on databricks
on_databricks = is.environment("DATABRICKS_RUNTIME_VERSION")

# Establish connection with blob storage
if (isTRUE(on_databricks)) {
  blob = AzureStor::storage_endpoint(Sys.getenv("BLOB_ENDPOINT"),
                                     sas = Sys.getenv("BLOB_SAS"))
  out_cont = AzureStor::storage_container(blob, "processed")
}

data_repo = "https://github.com/britishredcrosssociety"

# Read in vulnerability indices
# Local authority level
LA_vi_path = "covid-19-vulnerability/raw/master/output/vulnerability-LA.csv"
LA_vi = readr::read_csv(glue::glue("{data_repo}/{LA_vi_path}"))
LA_vi %<>% rename("LAD19CD" = Code)

# Read in resilience indices
# Local authority level
LA_res_path = "resilience-index/raw/main/depreciated/data/processed/resilience%20index.csv"
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
bivariate_color_scale <- tibble(
  "3 - 3" = "#000000",
  # high inequality, high income
  "2 - 3" = "#b36600",
  "1 - 3" = "#f3b300",
  # low inequality, high income #f0f0f0
  "3 - 2" = "#376387",
  "2 - 2" = "#b3b3b3",
  # medium inequality, medium income
  "1 - 2" = "#f3e6b3",
  "3 - 1" = "#509dc2",
  # high inequality, low income
  "2 - 1" = "#b4d3e1",
  "1 - 1" = "#d9d9d9" # low inequality, low income "#f3f3f3"
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
# If running on databricks write to blob storage
fname = "resilience_index_bivar.feather"
if (isTRUE(on_databricks)) {
  feather_path = glue::glue("{tempfile()}.feather")
  feather::write_feather(LA_res, feather_path)
  AzureStor::storage_upload(out_cont,
                            src = feather_path,
                            dest = fname)
  # Otherwise, write locally
} else {
  feather::write_feather(LA_res, file.path("..", "data", fname))
}

# Separate the groups
forlegend = bivariate_color_scale %>%
  separate(group,
           into = c("vuln_quantiles", "res_quantiles"),
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

# Print legend
g.legend

# Saving legend
par()
g.legend

fname = "bivar-legend_v2"
# If running on databricks write to blob storage
if (isTRUE(on_databricks)) {
  leg_path = glue::glue("{tempfile()}.png")
  dev.print(png,
            leg_path,
            bg = "transparent",
            width = 800,
            height = 600)
  AzureStor::storage_upload(out_cont,
                            src = leg_path,
                            dest = fname)
  # Otherwise, write locally
} else {
  dev.print(
    png,
    file.path("..", "www", glue::glue("{fname}.png")),
    bg = "transparent",
    width = 800,
    height = 600
  )
}
