# ---- 0: Load Packages ----

require(sf)
require(tidyverse)

# ---- 1: Prepare Data ----

hs = st_read(here::here('SS_Runs', 'Yearly', 'ss_1234rm_FULL_r125m', 
                        'ss_layers', 'hs_full_devrm.shp'))
trails = st_read(here::here('data', 'MAD_Trails', 'MAD_TRAILS.shp'))
trails = trails[!is.na(trails$TRAIL_NAME), ]

trails = st_transform(trails, st_crs(hs))


# ---- 2: Determine Nearest Trailhead ----

nearest_trailhead <- function(hs, trails) {
  # Ensure hs and trails are sf objects
  stopifnot("sf" %in% class(hs), "sf" %in% class(trails))
  
  # Find the nearest trail for each polygon in hs
  nearest_indices <- st_nearest_feature(hs, trails)
  
  # Calculate the minimum distance to the nearest trail
  distances <- st_distance(hs, trails[nearest_indices, ], by_element = TRUE)
  
  # Extract the TRAIL_NAME of the nearest trail using the nearest_indices
  nearest_trail_names <- trails$TRAIL_NAME[nearest_indices]
  
  # Add the nearest trail name and distance to hs
  hs$nearest_trail_name <- nearest_trail_names
  hs$nearest_distance <- distances
  
  return(hs)
}

# Apply the function
hs_trails <- nearest_trailhead(hs, trails)

