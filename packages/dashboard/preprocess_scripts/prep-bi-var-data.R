


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

ggplot2::ggsave(
  fpath,
  bg = "transparent",
  width = 800,
  height = 600,
  units = "mm"
)

if (isTRUE(is_databricks())) {
  AzureStor::storage_upload(get_container(),
                            src = fpath,
                            dest = "bivar-legend_v2.png")
}
