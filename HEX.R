# --- 1. Install and Load Necessary Packages----
# install.packages(c("sf", "osmdata", "dplyr", "leaflet", "units", "lwgeom"))
library(sf)          # Spatial data handling
library(osmdata)     # OpenStreetMap data querying
library(dplyr)       # Data manipulation
library(leaflet)     # Interactive maps
library(units)       # Handling distance/area units
library(lwgeom)      # For st_make_valid if needed
library(tidyverse)

extrafont::loadfonts()

# --- 2. Define Area of Interest (AOI) and Parameters----
aoi_name <- "Hamburg Germany"
target_crs <- st_crs(32632) # UTM Zone 32N for Hamburg (metric projection)
hex_cellsize_m <- 1000

print(paste("Using Target CRS:", target_crs$input))
print(paste("Hexagon Cell Size (approx):", hex_cellsize_m, "m"))

# --- 3. Get Hamburg Administrative Boundary----
print("Querying OSM for Hamburg administrative boundary...")
# Assumes admin_level=4 for Hamburg City-State works
hamburg_boundary_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "4") %>%
  osmdata_sf()



# Assumes multipolygons are returned and selects the first one
hamburg_boundary_sf <- hamburg_boundary_osm$osm_multipolygons %>%
  filter(name == "Hamburg" & border_type == "state")%>%
  st_make_valid()


# Project the boundary
hamburg_boundary_proj <- st_transform(hamburg_boundary_sf, target_crs)
print("Hamburg boundary obtained and projected.")

# --- 4. Query OSM Data for Features----
bbox_hamburg <- st_bbox(hamburg_boundary_sf) # Use bbox for queries

# --- 4a. Parks----
print("Querying OSM for parks...")
park_tags <- c("park", "nature_reserve", "playground", "garden", "dog_park")

parks_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "leisure", value = park_tags) %>%
  osmdata_sf()

# Combine polygons and multipolygons directly
parks_sf <- parks_osm$osm_polygons %>%
  filter(!sf::st_is_empty(.)) %>% # Keep basic empty check
  st_make_valid()

parks_proj <- st_transform(parks_sf, target_crs)
parks_union_proj <- st_union(parks_proj) # Assumes parks_proj is not empty
print(paste("Found and combined", nrow(parks_sf), "park areas."))

# --- 4b. Main Station (Hamburg Hbf) ----
print("Querying OSM for Hamburg Hauptbahnhof...")
hbf_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "name", value = "Hamburg Hauptbahnhof") %>%
  add_osm_feature(key = "railway", value = "station") %>%
  osmdata_sf()

# Prioritize point, then polygon centroid (assumes one is found)
if (!is.null(hbf_osm$osm_points) && nrow(hbf_osm$osm_points) > 0) {
  hbf_sf <- hbf_osm$osm_points[1, ]
  print("Found Hamburg Hbf as a point.")
} else {
  # Assumes polygons exist if points don't
  hbf_sf <- st_centroid(hbf_osm$osm_polygons[1, ]) # Use osm_polygons, could also check multipolygons
  print("Found Hamburg Hbf as a polygon, using centroid.")
}
hbf_proj <- st_transform(hbf_sf, target_crs)

# --- 4c. Bars----
print("Querying OSM for bars...")
bars_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "amenity", value = c("bar", "pub")) %>%
  osmdata_sf()
bars_sf <- bars_osm$osm_points %>% filter(!sf::st_is_empty(.)) # Keep basic empty check
bars_proj <- st_transform(bars_sf, target_crs)
print(paste("Found", nrow(bars_sf), "bars/pubs."))

# --- 4d. Restaurants----
print("Querying OSM for restaurants...")
restaurants_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()
restaurants_sf <- restaurants_osm$osm_points %>% filter(!sf::st_is_empty(.)) # Keep basic empty check
restaurants_proj <- st_transform(restaurants_sf, target_crs)
print(paste("Found", nrow(restaurants_sf), "restaurants."))

# --- 4e. Other Leisure Activities----
print("Querying OSM for other leisure activities...")
leisure_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "leisure") %>%
  osmdata_sf()

# Combine points and centroids of polygons/multipolygons
leisure_points_sf <- leisure_osm$osm_points
leisure_poly_centroids_sf <- bind_rows(leisure_osm$osm_polygons, leisure_osm$osm_multipolygons) %>%
  filter(!sf::st_is_empty(.)) %>% # Keep basic empty check
  st_make_valid() %>%
  st_centroid()

all_leisure_locations_sf <- bind_rows(leisure_points_sf, leisure_poly_centroids_sf) %>%
  filter(!sf::st_is_empty(.)) # Keep basic empty check


# Filter out the park-like leisure types
park_leisure_tags_to_exclude <- park_tags
other_leisure_sf <- all_leisure_locations_sf %>%
  filter(! (leisure %in% park_leisure_tags_to_exclude) )

other_leisure_proj <- st_transform(other_leisure_sf, target_crs)
print(paste("Found", nrow(other_leisure_sf), "other leisure locations (points/centroids)."))


# --- 4f. Bus Stops ----
print("Querying OSM for bus stops...")
bus_stops_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "highway", value = "bus_stop") %>%
  # Query platforms separately to potentially filter later if needed
  # add_osm_feature(key = "public_transport", value = "platform") %>%
  osmdata_sf()

bus_stops_sf <- if (!is.null(bus_stops_osm$osm_points)) bus_stops_osm$osm_points %>% filter(!sf::st_is_empty(.)) else st_sf(geometry = st_sfc(crs=4326))
bus_stops_sf <- bus_stops_sf %>%
  arrange(name) %>%  # optional: prioritize by order
  group_by(name) %>%
  slice(1) %>%       # keep the first geometry per name
  ungroup()


# If querying platforms too: filter them, e.g., by checking associated route relations or tags like bus=yes
bus_stops_proj <- st_transform(bus_stops_sf, target_crs)
print(paste("Found", nrow(bus_stops_sf), "bus stops."))


# --- 4g. Subway Stations (U-Bahn)----
print("Querying OSM for subway stations (U-Bahn)...")
# Querying stations explicitly tagged as subway
subway_stations_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "station", value = "subway") %>%
  osmdata_sf()

# Get points and polygon centroids
subway_points_sf <- if (!is.null(subway_stations_osm$osm_points)) subway_stations_osm$osm_points else st_sf(geometry = st_sfc(crs=4326))
subway_polygons_sf <- subway_stations_osm$osm_polygons %>%
  filter(!sf::st_is_empty(.)) %>%
  st_make_valid()
subway_poly_centroids_sf <- if (nrow(subway_polygons_sf) > 0) st_centroid(subway_polygons_sf) else st_sf(geometry = st_sfc(crs=4326))

# Combine and ensure unique locations if points and polygons overlap significantly for the same station
# Simple approach: just bind rows. A more complex approach might dissolve by name.
subway_stations_sf <- bind_rows(subway_points_sf, subway_poly_centroids_sf) %>% filter(!sf::st_is_empty(.))
# Optional: Add further filtering, e.g., ensure railway=station or public_transport=station is also present if needed

subway_stations_sf <- subway_stations_sf %>%
  arrange(name) %>%  # optional: prioritize by order
  group_by(name) %>%
  slice(1) %>%       # keep the first geometry per name
  ungroup()


subway_stations_proj <- st_transform(subway_stations_sf, target_crs)
print(paste("Found", nrow(subway_stations_sf), "potential subway station locations (points/centroids)."))


# --- 5. Create Hexagonal Grid----
print("Creating hexagonal grid...")
hex_grid <- st_make_grid(hamburg_boundary_proj, cellsize = hex_cellsize_m, square = FALSE)
hex_grid_sf <- st_sf(geometry = hex_grid) %>% mutate(hex_id = row_number())

# Keep only hexagons that intersect the Hamburg boundary using st_filter
hex_grid_hamburg <- st_filter(hex_grid_sf, hamburg_boundary_proj, .predicate = st_intersects)
print(paste("Created grid with", nrow(hex_grid_hamburg), "hexagons intersecting Hamburg."))

# Calculate hexagon centroids
hex_centroids_proj <- st_centroid(hex_grid_hamburg)

# --- 6. Calculate Metrics per Hexagon----

# --- 6a. Proximity to Nearest Park ----
print("Calculating proximity to nearest park...")
dist_to_park <- st_distance(hex_centroids_proj, parks_union_proj)
hex_grid_hamburg$dist_park_m <- drop_units(dist_to_park[,1]) # Assumes distance calc works

# --- 6b. Proximity to Main Station----
print("Calculating proximity to Hamburg Hbf...")
dist_to_hbf <- st_distance(hex_centroids_proj, hbf_proj)
hex_grid_hamburg$dist_hbf_m <- drop_units(dist_to_hbf[,1]) # Assumes distance calc works

# --- 6c. Count Points within Hexagons----
print("Counting features within hexagons...")

# Simplified function to count points in hexagons
count_points_in_hex_simple <- function(hex_sf, points_proj, col_name) {
  if(nrow(points_proj) == 0 || nrow(hex_sf) == 0) {
    hex_sf[[col_name]] <- 0L
    return(hex_sf)
  }
  points_proj_valid <- points_proj %>% filter(!st_is_empty(st_geometry(.)))
  if(nrow(points_proj_valid) == 0) {
    hex_sf[[col_name]] <- 0L
    return(hex_sf)
  }
  
  # Use join = st_intersects predicate for point-in-polygon test
  points_in_hex <- st_join(points_proj_valid, hex_sf %>% select(hex_id),
                           join = st_intersects, left = FALSE)
  
  if(nrow(points_in_hex) == 0) {
    counts <- data.frame(hex_id=integer(), count_col=integer())
  } else {
    counts <- points_in_hex %>%
      st_drop_geometry() %>%
      count(hex_id, name = "count_col")
  }
  
  hex_sf <- hex_sf %>%
    left_join(counts, by = "hex_id") %>%
    mutate(!!col_name := replace_na(count_col, 0L)) %>%
    select(-any_of("count_col"))
  return(hex_sf)
}

# Count all features including new PT stops
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, bars_proj, "bar_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, restaurants_proj, "restaurant_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, other_leisure_proj, "other_leisure_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, bus_stops_proj, "bus_stop_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, subway_stations_proj, "subway_station_count") # New

print("Finished calculating metrics.")


# --- 6d. Generate Summary Table----
# (Optional summary)
# print("--- Overall Summary Statistics for Hexagons ---")
# hex_summary_data <- st_drop_geometry(hex_grid_hamburg)
# print(summary(hex_summary_data %>% select(dist_park_m, dist_hbf_m, bar_count, restaurant_count, other_leisure_count, bus_stop_count, subway_station_count)))
# print("---")


# 6e. Calculate Attractivity Index ---- 
print("Calculating Attractivity Index...")

# --- Define Weights (adjust as needed, should sum to 1) ---
weights <- list(
  park_dist = 0.15,  # Proximity to parks
  hbf_dist  = 0.05,  # Proximity to main station
  bars      = 0.15,  # Density of bars (reduced)
  restaurants = 0.15, # Density of restaurants (reduced)
  leisure   = 0.15,  # Density of other leisure (reduced)
  bus_stops = 0.20,  # Density of bus stops (New)
  subway_stations = 0.15 # Density of subway stations (New)
)
# Check weights sum
if(abs(sum(unlist(weights)) - 1) > 1e-9) warning("Weights do not sum precisely to 1!")


# --- Min-Max normalization function (0-1 scale) ---
# Handles NA values and cases where all values are the same (zero range)
normalize_min_max <- function(x, na_val = 0.5) { # Return 0.5 score if range is zero
  # If all NA or only one non-NA value, range is effectively zero
  valid_x <- x[!is.na(x) & is.finite(x)]
  if (length(valid_x) == 0) return(NA_real_) # Return NA if no valid values
  
  min_x <- min(valid_x)
  max_x <- max(valid_x)
  range_x <- max_x - min_x
  
  if (range_x == 0) {
    # Assign default value (na_val) if range is zero, NA stays NA
    norm_x <- ifelse(is.na(x), NA_real_, na_val)
  } else {
    norm_x <- (x - min_x) / range_x
    # Ensure result is within [0, 1] even with potential floating point issues
    norm_x <- pmax(0, pmin(1, norm_x))
  }
  return(norm_x)
}


hex_grid_hamburg <- hex_grid_hamburg %>%
  mutate(
    # Normalize counts (higher is better, scale 0-1)
    bar_score = normalize_min_max(bar_count),
    restaurant_score = normalize_min_max(restaurant_count),
    leisure_score = normalize_min_max(other_leisure_count),
    bus_stop_score = normalize_min_max(bus_stop_count), # New
    subway_station_score = normalize_min_max(subway_station_count), # New
    
    # Normalize distances (lower is better), then invert score (1 - normalized)
    dist_park_norm = normalize_min_max(dist_park_m),
    park_dist_score = 1 - dist_park_norm, # Higher score = closer to park
    
    dist_hbf_norm = normalize_min_max(dist_hbf_m),
    hbf_dist_score = 1 - dist_hbf_norm, # Higher score = closer to Hbf
    
    # --- Calculate final weighted index ---
    # Replace NA scores with 0 before weighting
    attractivity_index = ( weights$park_dist * replace_na(park_dist_score, 0) ) +
      ( weights$hbf_dist  * replace_na(hbf_dist_score, 0) ) +
      ( weights$bars      * replace_na(bar_score, 0) ) +
      ( weights$restaurants * replace_na(restaurant_score, 0) ) +
      ( weights$leisure   * replace_na(leisure_score, 0) ) +
      ( weights$bus_stops * replace_na(bus_stop_score, 0) ) +         # New
      ( weights$subway_stations * replace_na(subway_station_score, 0) ) # New
  )

print("Attractivity Index calculated.")
print("Summary of Index:")
print(summary(hex_grid_hamburg$attractivity_index))
print(paste("Min Index:", round(min(hex_grid_hamburg$attractivity_index, na.rm=TRUE), 4)))
print(paste("Max Index:", round(max(hex_grid_hamburg$attractivity_index, na.rm=TRUE), 4)))


# --- 7. Visualization (Updated for Index) ----
print("Preparing index map visualization...")

# Transform final grid results back to WGS84 for Leaflet
hex_grid_viz <- st_transform(hex_grid_hamburg, 4326)

# Create an sf polygon from the bounding box used for queries
main_hamburg_bbox_poly <- st_bbox(c(xmin=9.626770, ymin=53.383328, xmax=10.351868, ymax=53.748711)) %>% # query_bbox defined in Section 4
  st_as_sfc() %>%
  st_set_crs(4326) # Ensure it's WGS84

hex_grid_viz <- st_filter(hex_grid_viz, main_hamburg_bbox_poly)

# --- Create Neo-Brutalist inspired color palette for the index ---
print("Creating Neo-Brutalist inspired palette...")

# Define custom colors (adjust as desired)
# Example: Dark Grey -> Electric Blue -> Lime Green -> Bright Yellow
neo_brutalist_colors <- c("#69D2E7", "#90EE90", "#E3A018", "#FF69B4", "#9723c9")

# Define the number of distinct color bins (quantiles)
n_color_bins <- 4 # Results in 4 distinct colors for 4 quantiles

# Use colorQuantile to create discrete color steps based on data distribution
pal_index <- colorQuantile(
  palette = neo_brutalist_colors,
  domain = hex_grid_viz$attractivity_index,
  n = n_color_bins,         # Number of quantiles/bins
  na.color = "#bdbdbd"      # Color for NA values
)


# --- Create detailed popup content ---
# Using htmltools::HTML to prevent potential cross-site scripting issues if names had HTML
popup_content <- paste0(
  "<div style='
    background-color: #ffffff;
    padding: 15px;
    border-radius: 12px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    font-family: Jost, sans-serif;
    font-size: 13px;
    line-height: 1.4;
    max-width: 260px;
  '>",
  "<div style='
    font-family: \"Bebas Neue\", sans-serif;
    font-size: 18px;
    margin-bottom: 6px;
    letter-spacing: 0.5px;
  '><b>Hex ID: ", htmltools::htmlEscape(hex_grid_viz$hex_id), "</b></div>",
  "<div><b>Attractivity Index:</b> ", round(hex_grid_viz$attractivity_index, 3), "</div>",
  "<hr style='margin: 8px 0;'>",
  "📍 <b>Dist. to Park:</b> ", ifelse(is.na(hex_grid_viz$dist_park_m), "N/A", round(hex_grid_viz$dist_park_m, 0)), " m<br>",
  "🚉 <b>Dist. to Hbf:</b> ", ifelse(is.na(hex_grid_viz$dist_hbf_m), "N/A", round(hex_grid_viz$dist_hbf_m, 0)), " m<br>",
  "🍻 <b>Bars/Pubs:</b> ", hex_grid_viz$bar_count, "<br>",
  "🍽️ <b>Restaurants:</b> ", hex_grid_viz$restaurant_count, "<br>",
  "🎯 <b>Other Leisure:</b> ", hex_grid_viz$other_leisure_count, "<br>",
  "🚌 <b>Bus Stops:</b> ", hex_grid_viz$bus_stop_count, "<br>",
  "🚇 <b>U-Bahn Stations:</b> ", hex_grid_viz$subway_station_count,
  "</div>"
)


# --- Create HTML for the Explanation Box (Added PT stops) ---
explanation_html <- paste0(
  "<div style='
    background-color: #ffffff;
    padding: 15px;
    border-radius: 12px;
    font-family: Jost, sans-serif;
    font-size: 13px;
    line-height: 1.4;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    max-width: 270px;
  '>",
  "<div style='
    font-family: \"Bebas Neue\", sans-serif;
    font-size: 18px;
    margin-bottom: 6px;
  '><b>Attractivity Index Explained</b></div>",
  "<hr style='margin: 8px 0;'>",
  "<p style='margin: 0;'>Index (0–1) based on weighted, normalized metrics:</p>",
  "<ul style='margin: 6px 0 8px 16px; padding-left: 0;'>",
  "<li>Proximity to Parks (", weights$park_dist * 100, "%)</li>",
  "<li>Proximity to Hbf (", weights$hbf_dist * 100, "%)</li>",
  "<li>Bars/Pubs (", weights$bars * 100, "%)</li>",
  "<li>Restaurants (", weights$restaurants * 100, "%)</li>",
  "<li>Other Leisure (", weights$leisure * 100, "%)</li>",
  "<li>Bus Stops (", weights$bus_stops * 100, "%)</li>",
  "<li>U-Bahn Stations (", weights$subway_stations * 100, "%)</li>",
  "</ul>",
  "<p style='font-size: 11px; margin-top: 6px; color: #444;'>Higher index = more attractive. Proximity scores are inverted: closer = higher.</p>",
  "</div>"
)





# --- Create Leaflet map showing the index ---
viz_map_index <- leaflet(data = hex_grid_viz, options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Basemap") %>%
  addPolygons(
    fillColor = ~pal_index(attractivity_index),
    weight = 0.5,          # Thin borders
    opacity = 0.7,         # Slightly transparent borders
    color = "#444444",     # Dark grey borders
    fillOpacity = 0.4,     # Fill transparency
    highlightOptions = highlightOptions( # Options on hover
      weight = 2,
      color = "#FFFFFF", # White border on hover
      fillOpacity = 0.9,
      bringToFront = TRUE),
    popup = popup_content, # Use the detailed popup
    popupOptions = popupOptions(maxWidth = 300),
    label = ~paste("Index:", round(attractivity_index, 3)), # Simple label on hover
    labelOptions = labelOptions(
      style = list("font-family" = "Jost", "font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto")
  ) %>%
  addLegend(
    pal = pal_index,
    values = ~attractivity_index,
    opacity = 0.7,
    title = "Attractivity<br>Index", # Use <br> for line break in title
    position = "bottomright"
  ) %>%
  # --- Add the Explanation Control Box to the Map --- <<< NEW >>>
  addControl(
    html = explanation_html,
    position = "bottomleft" # Position it in the bottom left corner
  )%>%
  setView( lng = 10
           , lat = 53.4
           , zoom = 10 ) %>%
  setMaxBounds( lng1 = 9.626770
                , lat1 = 53.383328
                , lng2 = 10.351868
                , lat2 = 53.748711 
                )


# Print the map
print(viz_map_index)
saveWidget(viz_map_index, file = "Widgets/viz_map_index.html", selfcontained = TRUE)


print("--- Script finished ---")

# --- Create Summary Table for Hex Grid Attractivity Analysis (Straightforward) ---
# WARNING: This version has NO checks and WILL error if objects are missing or invalid!

# Ensure necessary libraries are loaded
library(dplyr)
library(sf)
library(units)
library(tibble)

# --- Configuration ---
hex_rounding_digits <- 2 # For averages/medians/counts per hex
index_rounding_digits <- 4 # For the final index values
dist_rounding_digits <- 0 # For distances in meters

# --- Data Extraction and Calculation (No Checks) ---
# Create an empty list to hold summary metrics
hex_summary_metrics <- list()

print("Generating Hex Grid Attractivity analysis summary table (NO CHECKS)...")

# 1. Analysis Context & Parameters (Assume variables exist)
hex_summary_metrics[["Area of Interest"]] <- aoi_name
hex_summary_metrics[["Target CRS"]] <- target_crs$input
hex_summary_metrics[["Hexagon Cell Size (m)"]] <- hex_cellsize_m
num_hexagons <- nrow(hex_grid_hamburg)
hex_summary_metrics[["Number of Hexagons Analyzed"]] <- num_hexagons

# 2. Input Feature Counts (Assume sf objects exist)
hex_summary_metrics[["Input: Parks/Green Spaces Found (OSM Polygons)"]] <- nrow(parks_sf)
hex_summary_metrics[["Input: Bars/Pubs Found (OSM Points)"]] <- nrow(bars_sf)
hex_summary_metrics[["Input: Restaurants Found (OSM Points)"]] <- nrow(restaurants_sf)
hex_summary_metrics[["Input: Other Leisure Locations Found (OSM Points/Centroids)"]] <- nrow(other_leisure_sf)
hex_summary_metrics[["Input: Bus Stops Found (OSM Points)"]] <- nrow(bus_stops_sf)
hex_summary_metrics[["Input: Subway Stations Found (OSM Points/Centroids)"]] <- nrow(subway_stations_sf)

# 3. Index Composition (Weights) (Assume weights list exists)
hex_summary_metrics[["--- Attractivity Index Weights ---"]] <- "" # Separator
for (factor_name in names(weights)) {
  metric_name <- paste("Weight:", factor_name)
  hex_summary_metrics[[metric_name]] <- paste0(weights[[factor_name]] * 100, "%")
}
hex_summary_metrics[["--- End Weights ---"]] <- "" # Separator

# 4. Summary Statistics for Hexagon Metrics & Index (Assume hex_grid_hamburg exists and has rows/columns)
hex_data_summary <- st_drop_geometry(hex_grid_hamburg)

hex_summary_metrics[["--- Hexagon Metric Summaries ---"]] <- "" # Separator

# Summarize distances (Assume columns exist)
dist_cols <- c("dist_park_m", "dist_hbf_m")
dist_summary <- hex_data_summary %>%
  summarise(
    across(all_of(dist_cols), list(
      Avg = ~ mean(.x, na.rm = TRUE),
      Median = ~ median(.x, na.rm = TRUE),
      Min = ~ min(.x, na.rm = TRUE),
      Max = ~ max(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )
hex_summary_metrics[["Avg. Dist. to Park (m)"]] <- round(dist_summary$dist_park_m_Avg, dist_rounding_digits)
hex_summary_metrics[["Median Dist. to Park (m)"]] <- round(dist_summary$dist_park_m_Median, dist_rounding_digits)
hex_summary_metrics[["Min Dist. to Park (m)"]] <- round(dist_summary$dist_park_m_Min, dist_rounding_digits)
hex_summary_metrics[["Max Dist. to Park (m)"]] <- round(dist_summary$dist_park_m_Max, dist_rounding_digits)
hex_summary_metrics[["Avg. Dist. to Hbf (m)"]] <- round(dist_summary$dist_hbf_m_Avg, dist_rounding_digits)
hex_summary_metrics[["Median Dist. to Hbf (m)"]] <- round(dist_summary$dist_hbf_m_Median, dist_rounding_digits)
hex_summary_metrics[["Min Dist. to Hbf (m)"]] <- round(dist_summary$dist_hbf_m_Min, dist_rounding_digits)
hex_summary_metrics[["Max Dist. to Hbf (m)"]] <- round(dist_summary$dist_hbf_m_Max, dist_rounding_digits)

# Summarize counts (Assume columns exist)
count_cols <- c("bar_count", "restaurant_count", "other_leisure_count", "bus_stop_count", "subway_station_count")
count_summary <- hex_data_summary %>%
  summarise(
    across(all_of(count_cols), list(
      Total = ~ sum(.x, na.rm = TRUE),
      Avg_Hex = ~ mean(.x, na.rm = TRUE),
      Median_Hex = ~ median(.x, na.rm = TRUE),
      Max_Hex = ~ max(.x, na.rm = TRUE)
    ), .names = "{.col}_{.fn}")
  )
# Add count summaries to list
for (col in count_cols) {
  col_nice_name <- tools::toTitleCase(gsub("_", " ", sub("_count", "", col)))
  hex_summary_metrics[[paste("Total", col_nice_name, "(in hexes)")]] <- count_summary[[paste0(col, "_Total")]]
  hex_summary_metrics[[paste("Avg.", col_nice_name, "per Hex")]] <- round(count_summary[[paste0(col, "_Avg_Hex")]], hex_rounding_digits)
  hex_summary_metrics[[paste("Median", col_nice_name, "per Hex")]] <- round(count_summary[[paste0(col, "_Median_Hex")]], hex_rounding_digits)
  hex_summary_metrics[[paste("Max", col_nice_name, "in one Hex")]] <- count_summary[[paste0(col, "_Max_Hex")]]
}

# Summarize Attractivity Index (Assume column exists)
index_summary <- hex_data_summary %>%
  filter(!is.na(attractivity_index)) %>% # Keep NA filter for summary stats
  summarise(
    Min_Index = min(attractivity_index),
    Max_Index = max(attractivity_index),
    Mean_Index = mean(attractivity_index),
    Median_Index = median(attractivity_index)
  )
hex_summary_metrics[["--- Attractivity Index Summary ---"]] <- "" # Separator
hex_summary_metrics[["Min Attractivity Index"]] <- round(index_summary$Min_Index, index_rounding_digits)
hex_summary_metrics[["Max Attractivity Index"]] <- round(index_summary$Max_Index, index_rounding_digits)
hex_summary_metrics[["Mean Attractivity Index"]] <- round(index_summary$Mean_Index, index_rounding_digits)
hex_summary_metrics[["Median Attractivity Index"]] <- round(index_summary$Median_Index, index_rounding_digits)

# --- Format and Print Table ---
summary_table_hex <- tibble::enframe(hex_summary_metrics, name = "Metric", value = "Value")

# Ensure all values are character for printing
summary_table_hex$Value <- sapply(summary_table_hex$Value, function(x) {
  if (inherits(x, "units")) { format(units::drop_units(x)) }
  else { as.character(x) } # Convert everything else
})

print("--- Hex Grid Attractivity Analysis Summary (Straightforward - NO CHECKS) ---")
options(width = 120)
print(summary_table_hex, n = nrow(summary_table_hex))
options(width = 80) # Reset width

# --- Optional: Save to CSV ---
output_csv_hex_summary <- "Data/hamburg_hex_attractivity_summary_straightforward.csv"
write.csv(summary_table_hex, output_csv_hex_summary, row.names = FALSE, fileEncoding = "UTF-8")
print(paste("Hex attractivity summary table saved to:", output_csv_hex_summary))
