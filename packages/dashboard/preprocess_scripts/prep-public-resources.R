library("feather")
library("tidyverse")

resources = read_csv("~/vcs-indicators/resource_bank_8june.csv")

resources$`Relevance/Include?`

wanted = c("YES", "Yes", "yes")
# filter out no
to_include = resources %>%
  filter(`Relevance/Include?` %in% wanted)

# write to feather
write_feather(
  to_include,
  "~/r-shiny-web-apps/packages/dashboard/data/resource_bank/resource_bank.feather"
)
