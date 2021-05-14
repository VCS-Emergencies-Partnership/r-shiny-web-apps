library(sf)
library(tidyverse)
library(httr)
library(jsonlite)
library('rmapshaper')
library('geojsonio')
library(feather)

LAD_boundaries <- st_read('/home/izzy-everall/flooding_metoffice_data/lad19_eng_wales_sc_ni.geojson')
LAD_boundaries <- LAD_boundaries %>% rename('LAD19CD'=lad19cd)
# join tactical cell 
area_lookup <- read_csv("https://github.com/britishredcrosssociety/covid-19-vulnerability/raw/master/data/lookup%20mosa11%20to%20lad17%20to%20lad19%20to%20tactical%20cell.csv") 
area_lookup_tc2lad <- area_lookup %>% select('LAD19CD', 'TacticalCell')

LAD_boundaries_with_tc <- left_join(area_lookup_tc2lad, LAD_boundaries, by='LAD19CD', keep=F)  %>% unique()
LAD_boundaries_with_tc <- st_as_sf(LAD_boundaries_with_tc)

# call to see if any flood warnings
query <- paste0('http://environment.data.gov.uk/flood-monitoring/id/floods/')
flood_warning <- GET(query)
flood_warning_content <- content(flood_warning, type = 'text', encoding = "UTF-8")
flood_warning_data <- fromJSON(flood_warning_content)
flood_warning_df <- as_tibble(flood_warning_data$items, .name_repair='minimal') 
flood_warning_df <- do.call(data.frame, flood_warning_df)

if (dim(flood_warning_df)[1]==0) {
  
  flood_warning_polygons <- data.frame(matrix(ncol = 1, nrow = 0))
  flood_warning_points <- data.frame(matrix(ncol= 1, nrow=0))
  
  x <- c("geometry")
  colnames(flood_warning_polygons) <- x
  colnames(flood_warning_points) <- x
  
  flood_warning_meta <- data.frame(matrix(ncol = 13, nrow = 0))
  meta_columns <- c("polygon", "severity", "servityLevel",
                    "floodArea.riverOrSea", "floodAreaID",
                    "description", "message",
                    "timeMessageChanged", "timeRaised",
                    "timeSeverityChanged", "alertlevelmeaning",
                    "lastupdateday","messageurl")
  
  colnames(flood_warning_meta) <- meta_columns
  
  #write_sf(flood_warning_information, './flooding_metoffice_data/current_live_metoffice_floodwarnings.geojson')
  
  write_sf(flood_warning_polygons, './flooding_metoffice_data/current_live_warnings_polygons.geojson')
  write_feather(flood_warning_meta, './flooding_metoffice_data/current_live_warnings_metadata.feather')
  write_sf(flood_warning_points, './flooding_metoffice_data/current_live_warnings_points.geojson')
  
  
} else {

# only intested in severity level level 4 means reduced
flood_warnings_of_interest <- flood_warning_df %>% filter(severityLevel <= 3)
#flood_warnings_of_interest <- flood_warning_df
flood_warnings_of_interest <- flood_warnings_of_interest %>% rename(polygon=floodArea.polygon)
#get list of unique polygos
polygons_of_interest <- unique(flood_warnings_of_interest$polygon)

#write_csv(flood_warnings_of_interest, 'test_flood_data.csv')
# #loop through each flood and find which msoas it overlaps:
flood_overlap <- function (x) {
  #retrieve flood warning polygon
  get_poly <- URLencode(x)
  flood_boundary <- st_read(get_poly)
  #which msoas does this overlap in which local authority
  row_in_df_intersect <- st_intersects(flood_boundary, LAD_boundaries_with_tc)
  row_in_df_intersect <- as.vector(row_in_df_intersect[[1]])
  lads_with_flood_risk <- LAD_boundaries_with_tc %>% filter(row_number() %in% row_in_df_intersect)
  #print(lads_with_flood_risk)
  #extract just msoa, lad and vulnerability decile
  lads_with_flood_risk <- lads_with_flood_risk %>% select('LAD19CD', 'TacticalCell', 'lad19nm')
  #remove msoa geometry
  st_geometry(lads_with_flood_risk) <- NULL
  
  # simplify flood boundary 
  #flood_boundary_simp <- ms_simplify(flood_boundary, keep=0.01, keep_shapes=T) 
  #print('worked')
  #print(flood_boundary_simp)
  
  #add polygon to each of the dataframes
  flood_boundary_simp_comp <- flood_boundary %>% mutate('polygon' = x)
  lads_with_flood_risk <- lads_with_flood_risk %>% mutate('polygon' = x)
  
  #merge flood geometry with overlapping msoa and lads
  flood_warning2lad2tc <- left_join(flood_boundary_simp_comp, lads_with_flood_risk, by='polygon', keep=F)
  
  #flood_warning2lad2tc <- ms_simplify(flood_warning2lad2tc, keep=0.001, keep_shapes=T)
  # deal with odd number of columsn
  flood_warning2lad2tc <- flood_warning2lad2tc %>% select("AREA","FWS_TACODE","TA_NAME","DESCRIP","LA_NAME","QDIAL","RIVER_SEA","polygon","TacticalCell","LAD19CD","lad19nm","geometry")
  
  #return dataframes
  return(flood_warning2lad2tc)
  
}


# #create output dataframe
output_flood_data = NULL

#test <- polygons_of_interest[[24]]
#test_url <- URLencode(test)
#test_poly <- st_read(test_url)
#test_simplify <- ms_simplify(test_poly, keep=0.01, keep_shapes=T)


# #for each polygon
for (i in polygons_of_interest) {
  #retrieve msoas it overlaps with - only returns msoas where flood is predicted at msoa as vulnerability decile of >=9
  flood_area_of_interest <- flood_overlap(i)
  flood_reduced <- tryCatch(
    {
      ms_simplify(flood_area_of_interest, keep=0.01, keep_shapes=T)
    },
    # catch unable to simplify error
    error = function(e){
      flood_area_of_interest
    }
  )
 
  #print(flood_reduced)
  #add polygon to join back to final df
  output_flood_data = rbind(output_flood_data, flood_reduced)
}

# 
# #select data from flood warning df to join to flood polygon df
flood_warning_information <- flood_warnings_of_interest %>% 
  select('polygon', 'severity', 'severityLevel', 'floodArea.riverOrSea', 'floodAreaID', 'description', 'message', 'timeMessageChanged', 'timeRaised','timeSeverityChanged') %>%
  # alert level meaning
  mutate('alertlevelmeaning'=case_when(severityLevel == 3 ~ 'flooding is possible, be prepared',
                                       severityLevel == 2 ~ 'flooding is expected, immediate action required',
                                       severityLevel == 1 ~ 'severe flooding, danger to life')) 

# format flood alert date and time 
format_dates <- flood_warning_information %>% select('polygon', 'timeMessageChanged') %>%
  separate(timeMessageChanged, c("lastupdateday", "lastupdatetime"), sep='T')



 # if no flood warnings 
if (is.null(output_flood_data)) {
  
  flood_warning_information <- left_join(flood_warning_information, format_dates)
  
  flood_warning_information <- flood_warning_information %>% 
    mutate('messageurl'=paste('https://flood-warning-information.service.gov.uk/target-area/',floodAreaID, sep=''))
  # 
  # #join the data
  #final_flood_warning_info_of_interest <- left_join(output_flood_data, flood_warning_information) %>% unique() %>%
  #  select(-'QDIAL', -'FWS_TACODE', -'AREA', -'TA_NAME',-'DESCRIP',-'LA_NAME',-'RIVER_SEA',-'polygon',-'floodArea.riverOrSea',-'timeMessageChanged',-'timeRaised',-'timeSeverityChanged')
  
  # write to file 
  write_sf(flood_warning_information, './flooding_metoffice_data/current_live_metoffice_floodwarnings.geojson')
  
  # try three smaller output files:
  # 1). # flood area id and polygon
  flood_areaid2polygon <- flood_warning_information %>%
    select('floodAreaID') %>%
    unique()
  
  # write to file
  write_sf(flood_areaid2polygon, './flooding_metoffice_data/current_live_warnings_polygons.geojson')
  
  # 2). .feather with metadata - but no warnings
  flood_areaid2lad2metadata <- flood_warning_information
  
  write_feather(flood_areaid2lad2metadata, './flooding_metoffice_data/current_live_warnings_metadata.feather')
  
  # 3) # add centroids to data
  # transform to utm because st_centroid doesn't work with lng lat - or won't be accurate specifically i think

  
  centroids <- flood_warning_information

  write_sf(centroids, './flooding_metoffice_data/current_live_warnings_points.geojson')
  
  
  } else 
  if (!is.null(output_flood_data)) {
  flood_warning_information <- left_join(flood_warning_information, format_dates, by='polygon', keep=F)

  # add url for message 
  flood_warning_information <- flood_warning_information %>% 
  mutate('messageurl'=paste('https://flood-warning-information.service.gov.uk/target-area/',floodAreaID, sep=''))
# 
  # #join the data
  final_flood_warning_info_of_interest <- left_join(output_flood_data, flood_warning_information, by='polygon', keep=F) %>% unique() %>%
  select(-'QDIAL', -'FWS_TACODE', -'AREA', -'TA_NAME',-'DESCRIP',-'LA_NAME',-'RIVER_SEA',-'polygon',-'floodArea.riverOrSea',-'timeMessageChanged',-'timeRaised',-'timeSeverityChanged')



# write to file 
write_sf(final_flood_warning_info_of_interest, './flooding_metoffice_data/current_live_metoffice_floodwarnings.geojson')

# try three smaller output files:
# 1). # flood area id and polygon
flood_areaid2polygon <- final_flood_warning_info_of_interest %>%
  select('floodAreaID') %>%
  unique()

# write to file
write_sf(flood_areaid2polygon, './flooding_metoffice_data/current_live_warnings_polygons.geojson')

# 2). .feather with metadata
flood_areaid2lad2metadata <- final_flood_warning_info_of_interest %>%
  st_drop_geometry()

write_feather(flood_areaid2lad2metadata, './flooding_metoffice_data/current_live_warnings_metadata.feather')

# 3) # add centroids to data
# transform to utm because st_centroid doesn't work with lng lat - or won't be accurate specifically i think
final_flood_warnings_centroids <- flood_areaid2polygon %>%
   st_transform(32617)
 
centroids <- st_centroid(final_flood_warnings_centroids) %>%
   # select lad19CD and geometry 
   st_transform(4326)

write_sf(centroids, './flooding_metoffice_data/current_live_warnings_points.geojson')

  
}

}

# 
# # convert long and lat to dataframe columns 
#  centroids <- centroids %>%
#    mutate(long = unlist(map(centroids$geometry,1)),
#           lat = unlist(map(centroids$geometry,2))) %>%
#    st_drop_geometry()
#  
#  centroids_df <- as.data.frame(centroids)






