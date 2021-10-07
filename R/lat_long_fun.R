# address to give lat long from it as a list

lat_long_fun <- function(address) {
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, tidygeocoder)
  
  address_df <- dplyr::as_tibble(address) 
  lat_long_df <-tidygeocoder::geocode(
    address_df,
    address = value
    ) 
  
  lat_long_df_adj <- lat_long_df %>% 
    select(-value)
  
}

