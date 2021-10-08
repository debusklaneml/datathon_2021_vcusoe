
lat_long_fun <- function(address) {
  
  address_df <- dplyr::as_tibble(address) 
  lat_long_df <-tidygeocoder::geocode(
    address_df,
    address = value
  ) 
  
  lat_long_df_adj <- lat_long_df %>% 
    select(-value)
  
}

create_map_df <- function(x, y, name = "input site", ind) {
  
  x_df <- st_as_sf(x, coords = c("long", "lat"), crs = 4326) %>%
    mutate(sitename = name) %>%
    select(sitename, geometry)
  
  y_df <- y %>%
    select(sitename)
  
  bind_rows(y_df, x_df) %>%
    mutate(type = case_when(
      sitename == name ~ "inp",
      row_number() == ind ~ "closest",
      TRUE ~ "other"
    ))

}

get_closest_ind <- function(x, y) {
  
  x_recoded <- st_as_sf(x, coords = c("long", "lat"), crs = 4326)
  
  nn_out <- st_nn(x_recoded, y)
  
  row_ind <- simplify(nn_out)
  
  row_ind
}

make_closest_text <- function(ind, df) {
  
  nearest <- df[ind, ]
  
  paste0("The closest site participating in the after school meals program is ", str_to_title(nearest$sitename), " at ", nearest$streetaddd)
}

pal <- leaflet::colorFactor(c("#378805", "red", "navy"), domain = c("closest", "inp", "other"))
