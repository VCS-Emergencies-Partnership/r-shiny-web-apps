# pre process data for vcsep-shiny-dashboard

library("feather")
library("httr")
library("readxl")
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

mnt = get_mount_point()

# Get look up table
not_using_other_countries = c("Wales",
                              "Scotland",
                              "Northern Ireland and the Isle of Man")
area_lookup = read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv")
area_lookup_tc2lad =
  area_lookup %>%
  select("LAD19CD", "TacticalCell") %>%
  filter(!TacticalCell %in% not_using_other_countries) %>%
  unique()

# Population level data
pop_eng_2019 =
  read_excel(
    glue::glue("/{mnt}/data-lake/raw/ons-populstion-estimates-mid-year-2019/2021-04-12-10-30-27/ons-populstion-estimates-mid-year-2019.xlsx"),
    sheet = "Mid-2019 Persons",
    skip = 4
  )

# Select LA Code (2019 boundaries) and All Ages
pop_eng = pop_eng_2019 %>%
  select(LAD19CD = `LA Code (2019 boundaries)`,
         la_name = `LA name (2019 boundaries)`,
         population = `All Ages`) %>%
  group_by(LAD19CD) %>%
  summarise(`All Ages` = sum(`population`)) %>%
  filter(str_detect(LAD19CD, "^E"))

pop_eng_tc = left_join(pop_eng, area_lookup_tc2lad,
                       by = "LAD19CD",
                       keep = FALSE)

# I think there are 317 lads in England with local authority 2019
# Calculate population of tactical cells
pop_tc = pop_eng_tc %>%
  group_by(TacticalCell) %>%
  summarise_at(vars(`All Ages`), list(sum)) %>%
  mutate("eng_pop" = sum(`All Ages`))

pop_eng_lad_tc =
  left_join(pop_eng_tc,
            pop_tc,
            by = "TacticalCell",
            keep = FALSE) %>%
  rename("lad_pop" = `All Ages.x`, "tc_pop" = `All Ages.y`)

# Store eng pop seperately
eng_pop = unique(pop_eng_lad_tc$eng_pop)

# INDICATOR DATA
# Read in data - check each exists
does_file_exist = function(input_file) {
  if (!file.exists(input_file)) {
    message = paste0("could not find file", input_file)
    stop(message)
  } else {
    data = read_csv(input_file)
    return(data)
  }

}

bame = does_file_exist(glue::glue("/{mnt}/data-lake/curated/annual-population-survey-ethnicity-data/bame-ethnicity-lad19.csv"))
asylum = does_file_exist(glue::glue("/{mnt}/data-lake/curated/asylum-section-95-support/asylum-lad19.csv"))
digital_exclusion_tc = does_file_exist(glue::glue("/{mnt}/data-lake/curated/digital-exclusion/digital-exclusion-tc.csv"))
digital_exclusion_lad = does_file_exist(glue::glue("/{mnt}/data-lake/curated/digital-exclusion/digital-exclusion-lad19.csv"))
fuelp = does_file_exist(glue::glue("/{mnt}/data-lake/curated/fuel-poverty-eng/fuel-poverty-lad19.csv"))
homelessness = does_file_exist(glue::glue("/{mnt}/data-lake/curated/homelessness-eng/homelessness-lad19.csv"))
shielding = does_file_exist(glue::glue("/{mnt}/data-lake/curated/shielding-patients-list-eng/shielding-lad19.csv"))
ucred = does_file_exist(glue::glue("/{mnt}/data-lake/curated/stat-xplore-people-on-universal-credit/universal-credit-lad19.csv"))

# Function to check columns i expected are present
correct_columns = function(columns_expected, data_read) {
  # Are cols in data
  # Are all the columns i use with easy names there?
  cols_still_present = (columns_expected %in% colnames(data_read))
  if (all(cols_still_present) == FALSE) {
    col_message = paste0("Columns not found")
    stop(col_message)
  }
  else {
    return(data_read)
  }
}

# BAME population in LAD
# Already calculated % of lad population which is bame
# only have bame data for 133 out of the 317 LADs in England
# CHECK COLUMNS EXPECTED EXIST
bame_cols =
  c(
    "LAD19CD",
    "numerator-bame-not-uk-born",
    "numerator-bame-uk-born",
    "Denominator",
    "Percentage of population who are ethnic minority"
  )
bame = correct_columns(bame_cols, bame)
# Add in tactical cell
bame_data =
  left_join(bame,
            area_lookup_tc2lad,
            by = "LAD19CD",
            keep = FALSE) %>%
  filter(TacticalCell != "Wales" &
           TacticalCell != "Scotland") %>%
  unique()

# BAME population in England (based upon annual population survey - obviously some data missing)
# data for 200 local
england_proportion_bame =
  ((
    sum(bame_data$`numerator-bame-not-uk-born`, na.rm = TRUE) +
      sum(bame_data$`numerator-bame-uk-born`, na.rm = TRUE)
  ) / sum(bame_data$Denominator, na.rm = TRUE)) * 100
# According to the annual population survey
bame_data =
  bame_data %>%
  mutate(england_proportion_bame = round(england_proportion_bame, 1))

# group_by tactical cell and sum
groupColumns = c("TacticalCell")
dataColumns = c("numerator-bame-not-uk-born",
                "numerator-bame-uk-born",
                "Denominator")

bame_lad_values2tc_total =
  plyr::ddply(bame_data,
              groupColumns,
              function(x) colSums(x[dataColumns], na.rm = TRUE))

bame_lad_values2tc_total_final =
  bame_lad_values2tc_total %>%
  mutate(total_bame_in_tc = `numerator-bame-not-uk-born` + `numerator-bame-uk-born`) %>%
  mutate(tc_proportion = round((total_bame_in_tc / Denominator) * 100, 1)) %>%
  rename("tc_denominator" = Denominator) %>%
  select(-`numerator-bame-not-uk-born`,-`numerator-bame-uk-born`)

# Join to LAD data
bame_data =
  left_join(bame_data,
            bame_lad_values2tc_total_final,
            by = "TacticalCell",
            keep = FALSE)

# Asylum data
# CHECK COLUMNS I WAS EXPECTING
asylum_cols =
  c(
    "LAD19CD",
    "Subsistence Only",
    "Dispersed Accommodation",
    "People receiving Section 95 support",
    "latest_data"
  )
asylum = correct_columns(asylum_cols, asylum)

# Calculate proportion receiving support
# Join to areas2uk
asylum_data =
  left_join(area_lookup_tc2lad,
            asylum,
            by = "LAD19CD",
            keep = FALSE) %>%
  unique() %>%
  left_join(., pop_eng_lad_tc, by = c("LAD19CD", "TacticalCell"))

# Currently we are just aggregating England
asylum_data =
  asylum_data %>% filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  )

asylum_data = asylum_data %>%
  mutate("lad_prop_receving_section95_support" = round((
    `People receiving Section 95 support` / lad_pop
  ) * 100, 2)) %>%
  mutate("People receiving Section 95 support (people per 1000)" = round((
    `People receiving Section 95 support` / lad_pop
  ) * 1000, 2))


# Do they want to know what proprtion of the population that is
# or geographically where the highest proportion of section95s
# one divides the number receiving support by population of LAD
# the other by total receiving support
# based on how small the proportions would be if i did of the total population i'm going for of the people recieving section 95 support
# what proprotion are receving support in each tactical cell and lad
# however having discussed with Mike P going to do proportion of the population - the numbers will be small

# also note when calculation proportions of populations am using consistent figures across all.
# So I do NOT just calculate the population based on those that have data available for the indicator.

asylum_data_tc = asylum_data %>%
  group_by(TacticalCell, tc_pop) %>%
  summarise_at(vars("People receiving Section 95 support"),
               list(sum),
               na.rm = TRUE) %>%
  rename("tc_People receiving Section 95 support" = `People receiving Section 95 support`,
         "People receiving Section 95 support") %>%
  mutate("tc_prop_people_recieving_section_95_support" = round((
    `tc_People receiving Section 95 support` / tc_pop
  ) * 100, 2)) %>%
  mutate("tc_people_recieving_section_95_support_cases_per_1000" = round((
    `tc_People receiving Section 95 support` / tc_pop
  ) * 1000, 2)) #%>%

asylum_data =
  left_join(
    asylum_data,
    asylum_data_tc,
    by = c("TacticalCell", "tc_pop"),
    keep = FALSE
  )

asylum_data = asylum_data %>%
  mutate(
    "eng_people_recieving_section_95_support" = sum(`People receiving Section 95 support`,
                                                    na.rm = TRUE)
  ) %>%
  mutate("prop_eng_receiving_section_95_support" = round((
    `eng_people_recieving_section_95_support` / eng_pop
  ) * 100, 2)) %>%
  mutate("eng_receiving_section_95_support_cases_per_1000" = round((
    `eng_people_recieving_section_95_support` / eng_pop
  ) * 1000, 2))

#write_csv(asylum_data, "./people_at_risk_table/asylum-indicator.csv")

# --- Digital exclusion ---
# just going to combine the two file
de_cols =
  c(
    "LAD19CD",
    "Proportion of neighbourhoods in 20% most digitally excluded",
    "Extent of population living in highly digitally excluded areas",
    "Population-weighted digitally exclusion score"
  )

digital_exclusion_lad = correct_columns(de_cols, digital_exclusion_lad)

digital_exclusion_data =
  left_join(area_lookup_tc2lad,
            digital_exclusion_lad,
            by = "LAD19CD",
            keep = FALSE) %>%
  unique() %>%
  select("LAD19CD", "TacticalCell", everything()) %>%
  # remove wales/scotland/NI for now
  filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  ) %>%
  mutate(
    "percent_digitally_excluded" = round(
      `Proportion of neighbourhoods in 20% most digitally excluded` * 100,
      1
    )
  ) %>%
  select(
    "LAD19CD",
    "TacticalCell",
    `Proportion of neighbourhoods in 20% most digitally excluded`,
    "percent_digitally_excluded"
  )

# CHECK TACTICAL CELL DE data
de_cols =
  c(
    "TacticalCell",
    "Proportion of neighbourhoods in 20% most digitally excluded",
    "Extent of population living in highly digitally excluded areas",
    "Population-weighted digitally exclusion score"
  )

digital_exclusion_tc = correct_columns(de_cols, digital_exclusion_tc)

digital_exclusion_data =
  digital_exclusion_tc %>%
  rename(
    `tc_Proportion of neighbourhoods in 20% most digitally excluded` =
      `Proportion of neighbourhoods in 20% most digitally excluded`,
    `tc_Extent of population living in highly digitally excluded areas` =
      `Extent of population living in highly digitally excluded areas`,
    `tc_Population-weighted digitally exclusion score` =
      `Population-weighted digitally exclusion score`
  ) %>%
  mutate(
    "tc_percent_digitally_excluded" = round(
      `tc_Proportion of neighbourhoods in 20% most digitally excluded` * 100,
      1
    )
  ) %>%
  select(
    "TacticalCell",
    `tc_Proportion of neighbourhoods in 20% most digitally excluded`,
    "tc_percent_digitally_excluded"
  ) %>%
  left_join(digital_exclusion_data, ., by = "TacticalCell", keep = FALSE)


#write_csv(digital_exclusion_data, "./people_at_risk_table/digital-exclusion-indicator.csv")

# Shielding
# Check cols
shielding_cols =
  c(
    "LAD19CD",
    "Clinically extremely vulnerable",
    "la_name",
    "Clinically extremely vulnerable (per 1000)",
    "Proportion Clinically extremely vulnerable"
  )

shielding = correct_columns(shielding_cols, shielding)

# Join lad population
shielding_data =
  left_join(area_lookup_tc2lad, shielding, by = "LAD19CD", keep = FALSE) %>%
  unique() %>%
  filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  )

# Join population data
shielding_data =
  left_join(pop_eng_lad_tc,
            shielding,
            by = c("LAD19CD"),
            keep = FALSE)

# Calculate number per tactical cell per 1000
shielding_data_tc =
  shielding_data %>%
  group_by(TacticalCell, tc_pop) %>%
  summarise_at(vars(`Clinically extremely vulnerable`),
               list(sum),
               na.rm = TRUE) %>%
  # Calculate number of people shielding per 1000
  mutate(`tc_Clinically vulnerable per 1000` = round((`Clinically extremely vulnerable` /
                                                        tc_pop) * 1000, 1)) %>%
  mutate(`tc_Clinically vulnerable proportion of population` = round((`Clinically extremely vulnerable` /
                                                                        tc_pop) * 100, 1)) %>%
  rename(`tc_Clinically extremely vulnerable` = `Clinically extremely vulnerable`)

shielding_data =
  left_join(
    shielding_data,
    shielding_data_tc,
    by = c("TacticalCell", "tc_pop"),
    keep = FALSE
  )

# Shielding in England
shielding_data = shielding_data %>%
  mutate("total_shielding_eng" = sum(`Clinically extremely vulnerable`,
                                     na.rm = TRUE)) %>%
  mutate("proportion_total_shielding_Eng" = round((total_shielding_eng /
                                                     eng_pop) * 100, 1))

#write_csv(shielding_data, "./people_at_risk_table/shielding-indicator.csv")

# Homelessness
homeless_cols = c("LAD19CD", "Homelessness (rate per 1000)")
homelessness = correct_columns(homeless_cols, homelessness)

homelessness_data =
  left_join(area_lookup_tc2lad,
            homelessness,
            by = "LAD19CD",
            keep = FALSE) %>%
  unique() %>%
  left_join(.,
            pop_eng_lad_tc,
            by = c("LAD19CD", "TacticalCell"),
            keep = FALSE) %>%
  filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  )

homelessness_data_tc = homelessness_data %>%
  group_by(TacticalCell) %>%
  summarise_at(vars(`Homelessness (rate per 1000)`),
               list(mean),
               na.rm = TRUE) %>%
  rename("tc_Homelessness (rate per 1000)" = `Homelessness (rate per 1000)`)

homelessness_data =
  left_join(
    homelessness_data,
    homelessness_data_tc,
    by = c("TacticalCell"),
    keep = FALSE
  )

homelessness_data = homelessness_data %>%
  mutate("eng_rate_per_1000" = mean(`Homelessness (rate per 1000)`,
                                    na.rm = TRUE))

#write_csv(homelessness_data, "./people_at_risk_table/homelessness-indicator.csv")

# Universal credit
# Check columns
unem_cols =
  c(
    "Month Year",
    "National - Regional - LA - OAs",
    "LAD19CD",
    "Not in employment",
    "In employment",
    "Percentage in employment",
    "Percentage not in employment"
  )

ucred = correct_columns(unem_cols, ucred)

# For both local authority, tactical cell and England
# need to determine the proportion of the population unemployed on universal credit
ucred_data =
  left_join(area_lookup_tc2lad, ucred, by = "LAD19CD", keep = FALSE) %>%
  unique() %>%
  left_join(.,
            pop_eng_lad_tc,
            by = c("LAD19CD", "TacticalCell"),
            keep = FALSE) %>%
  # Remove Wales/Scotland/NI for now
  filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  )

# Local authority proportion of population unemployed on universal credit
ucred_data = ucred_data %>%
  mutate("lad_prop_upemployed_on_ucred" = round((`Not in employment` / lad_pop) *
                                                  100, 0))

# tc and eng level total and proportion unemployed on universal credit
ucred_data_tc = ucred_data %>%
  group_by(TacticalCell, tc_pop) %>%
  summarise_at(vars(`Not in employment`), list(sum), na.rm = TRUE) %>%
  rename(`tc_Not in employment` = `Not in employment`) %>%
  mutate("tc_prop_unemployed_on_universal_credit" = round((`tc_Not in employment` /
                                                             tc_pop) * 100, 1)) #%>%
#mutate("eng_pop_with_ucred_data"=sum(tc_pop_with_ucred_data)) %>%

ucred_data =
  left_join(
    ucred_data,
    ucred_data_tc,
    by = c("TacticalCell", "tc_pop"),
    keep = FALSE
  ) %>%
  unique()

ucred_data = ucred_data %>%
  mutate("eng_total_unemployed_on_ucred" = sum(`Not in employment`,
                                               na.rm = TRUE)) %>%
  mutate("prop_eng_pop_unemployed_on_ucred" = round((eng_total_unemployed_on_ucred /
                                                       eng_pop) * 100, 1))

# Fuel poverty
# CHECK COLS
fuelp_cols =
  c(
    "LAD19CD",
    "Number of households1",
    "Number of households in fuel poverty1",
    "Proportion of households fuel poor (%)"
  )

fuelp = correct_columns(fuelp_cols, fuelp)

# Join tactical cells (this is households so don't need population)
fuelp_data =
  left_join(area_lookup_tc2lad, fuelp, by = "LAD19CD", keep = FALSE) %>%
  unique()  %>%
  # remove Wales/Scotland/NI for now
  filter(
    TacticalCell != "Northern Ireland and the Isle of Man",
    TacticalCell != "Scotland",
    TacticalCell != "Wales"
  )


# Calculate proportion of households fuel poor tactical
fuelp_data_tc_eng = fuelp_data %>%
  group_by(TacticalCell) %>%
  summarise_at(
    vars(`Number of households1`, `Number of households in fuel poverty1`),
    list(sum),
    na.rm = TRUE
  ) %>%
  rename(
    `tc_Number of households1` = `Number of households1`,
    `tc_Number of households in fuel poverty1` = `Number of households in fuel poverty1`
  ) %>%
  mutate("tc_prop_households_fuel_poor" = round((
    `tc_Number of households in fuel poverty1` / `tc_Number of households1`
  ) * 100,
  1
  )) %>%
  mutate("eng_total_households" = round(sum(`tc_Number of households1`), 0)) %>%
  mutate("eng_total_fuel_poor_households" = round(sum(`tc_Number of households in fuel poverty1`), 0)) %>%
  mutate("eng_prop_households_fuel_poor" = round((eng_total_fuel_poor_households /
                                                    eng_total_households) * 100,
                                                 1
  ))


fuelp_data =
  left_join(fuelp_data, fuelp_data_tc_eng, by = "TacticalCell", keep = FALSE)


# Do we want to join them all together
# Join altogether
all_data =
  left_join(
    digital_exclusion_data,
    asylum_data,
    by = c("LAD19CD", "TacticalCell"),
    keep = FALSE
  ) %>%
  # Add bame
  left_join(.,
            bame_data,
            by = c("LAD19CD", "TacticalCell"),
            keep = FALSE) %>%
  # Add covid
  #left_join(., covid_data, by=c("LAD19CD","TacticalCell","lad_pop","tc_pop","eng_pop"), keep=F) %>%
  # Add fuel poverty
  left_join(.,
            fuelp_data,
            by = c("LAD19CD", "TacticalCell"),
            keep = FALSE) %>%
  # Add homelessness
  left_join(
    .,
    homelessness_data,
    by = c("LAD19CD", "TacticalCell", "lad_pop", "tc_pop", "eng_pop"),
    keep = FALSE
  )  %>%
  # Add ucred
  left_join(
    .,
    ucred_data,
    by = c("LAD19CD", "TacticalCell", "lad_pop", "tc_pop", "eng_pop"),
    keep = FALSE
  ) %>%
  # Clinically shielding
  left_join(
    .,
    shielding_data,
    by = c("LAD19CD", "TacticalCell", "lad_pop", "tc_pop", "eng_pop"),
    keep = FALSE
  )

# Has something bizarre happened
any_columns_of_just_NA = all_data %>% select(where(~ !all(is.na(.x))))

if (dim(any_columns_of_just_NA)[2] != dim(all_data)[2]) {
  stop("Something wrong - a column with just NAs is present")
} else {
  write_data(
    feather::write_feather,
    all_data,
    "people-at-risk.feather",
    local_dir = "~/r-shiny-web-apps/packages/dashboard/data/people_at_risk/"
  )
}
