library("feather")
library("tidyverse")

box::use(./helpers)

resources = read_csv("~/vcs-indicators/resource_bank_8june.csv")

resources$`Relevance/Include?`

wanted = c("YES", "Yes", "yes")
# Filter out no
to_include = resources %>%
  filter(`Relevance/Include?` %in% wanted)

# Write to feather
helpers$write_data(
  feather::write_feather,
  to_include,
  "resource_bank.feather",
  local_dir = "~/r-shiny-web-apps/packages/dashboard/data/resource_bank/"
)
