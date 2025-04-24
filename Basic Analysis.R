# --- 1. Install and Load Necessary Packages ---
# Make sure you have these packages installed. If not, run install.packages("package_name")
library(sf)          # For handling spatial data (Simple Features)
library(osmdata)     # For downloading OpenStreetMap data
library(dplyr)       # For data manipulation
library(leaflet)     # For interactive maps
library(units)       # For handling units explicitly
library(lwgeom)      # Potentially needed for st_make_valid
extrafonts::loadfonts()
# Potentially needed for later analysis steps:
# library(tidytransit) # For GTFS (public transport schedules)
# library(sfnetworks)  # For network analysis
# library(dodgr)       # For efficient network routing/analysis
# library(r5r)         # For advanced multimodal routing (OSM + GTFS)

# --- 2. Define Area of Interest (AOI) ---
aoi_name <- "Hamburg, Germany"
aoi_bbox <- c(9.626770,53.383328,10.351868,53.748711)
target_crs <- st_crs(32632) # UTM Zone 32N for Hamburg

print(paste("Target CRS:", target_crs$input))

# --- 3. Query OSM Data (Simplified) ---

# Example: Get major roads for CAR traffic
print("Querying OSM for major roads (cars)...")
roads_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "trunk", "primary", "secondary", "tertiary",
                            "motorway_link", "trunk_link", "primary_link", "secondary_link",
                            "residential", "living_street", "unclassified")) %>%
  osmdata_sf()
# Assume lines are returned
roads_sf <- roads_osm$osm_lines %>% filter(!st_is_empty(.)) # Basic empty check

# Example: Get dedicated cycleways for BIKE traffic
print("Querying OSM for cycleways...")
cycleways_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "highway", value = "cycleway") %>%
  osmdata_sf()
# Assume lines are returned
cycleways_sf <- cycleways_osm$osm_lines %>% filter(!st_is_empty(.)) # Basic empty check

# Example: Get footways for FOOT traffic
print("Querying OSM for footways...")
footways_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "highway", value = c("footway", "pedestrian", "path", "steps", "living_street", "track")) %>%
  osmdata_sf()
# Assume lines are returned
footways_sf <- footways_osm$osm_lines %>% filter(!st_is_empty(.)) # Basic empty check

# Example: Get Public Transport Stops (basic infrastructure)
print("Querying OSM for public transport stops...")
pt_stops_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "public_transport", value = c("stop_position", "platform", "station")) %>%
  add_osm_feature(key = "highway", value = "bus_stop") %>%
  add_osm_feature(key = "railway", value = c("stop", "station", "tram_stop", "halt")) %>%
  osmdata_sf()
# Assume points are returned
pt_stops_sf <- pt_stops_osm$osm_points %>% filter(!st_is_empty(.)) # Basic empty check

# --- 3b. Query Additional OSM Data for KPIs (Simplified) ---

# Get Green Spaces (Parks, Forests, etc.)
print("Querying OSM for green spaces...")
green_spaces_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "leisure", value = c("park", "nature_reserve", "playground", "garden", "dog_park")) %>%
  add_osm_feature(key = "landuse", value = c("forest", "grass", "greenfield", "recreation_ground")) %>%
  add_osm_feature(key = "natural", value = c("wood", "scrub", "heath", "grassland")) %>%
  osmdata_sf()
# Combine polygons and multipolygons directly
green_spaces_sf <- green_spaces_osm$osm_polygons %>%
  filter(!st_is_empty(.)) %>% # Basic empty check
  st_make_valid()

# Get Building *Points* (Alternative to heavy footprint download)
print("Querying OSM for building points/nodes...")
# This queries NODES tagged as buildings, much lighter than polygons.
# It might miss buildings only mapped as areas.
buildings_osm_points <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "building")%>%
  osmdata_sf()

building_points_sf <- buildings_osm_points$osm_points %>% filter(!st_is_empty(.)) # Basic empty check
print(paste("Found", nrow(building_points_sf), "OSM nodes tagged as buildings."))


# Get Amenities/Shops (Points of Interest - POIs)
print("Querying OSM for amenities and shops...")
pois_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "amenity") %>%
  add_osm_feature(key = "shop") %>%
  osmdata_sf()
# Just use points
all_pois_sf <- pois_osm$osm_points %>% filter(!st_is_empty(.)) # Basic empty check


# --- 4. Fetch Administrative Boundaries (Hamburg Districts) ---
print("Querying OSM for administrative boundaries (Districts)...")
districts_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "admin_level", value = "9") %>% # Stadtteile in Hamburg
  osmdata_sf()

# Assume multipolygons are returned
districts_sf <- districts_osm$osm_multipolygons %>%
  filter(!st_is_empty(.)) %>% # Basic empty check
  st_make_valid() %>%
  select(osm_id, name) # Keep only relevant columns


# --- 5. Prepare Data & Reproject (Simplified) ---
print(paste("Reprojecting data to target CRS:", target_crs$input))

# Direct transformation, assumes inputs are valid sf objects
districts_proj <- st_transform(districts_sf, target_crs)
roads_proj <- st_transform(roads_sf, target_crs)
cycleways_proj <- st_transform(cycleways_sf, target_crs)
footways_proj <- st_transform(footways_sf, target_crs)
pt_stops_proj <- st_transform(pt_stops_sf, target_crs)
green_spaces_proj <- st_transform(green_spaces_sf, target_crs)
building_points_proj <- st_transform(building_points_sf, target_crs) # Project building points
all_pois_proj <- st_transform(all_pois_sf, target_crs)

# --- 6. Calculate Measures per District ---

# --- 6a. Area Calculation ---
districts_proj$district_area_m2 <- st_area(districts_proj)
districts_proj$district_area_km2 <- set_units(districts_proj$district_area_m2, "km^2")
districts_proj <- districts_proj %>% mutate(unique_district_id = row_number()) # Ensure unique ID

# --- 6b. Network Length Calculation (Simplified Function) ---
calculate_length_in_polygons_simple <- function(lines_sf, polygons_sf, poly_id_col = "unique_district_id") {
  print(paste("Performing intersection for length calculation..."))
  intersections <- st_intersection(lines_sf, polygons_sf) # Warning: Can error if inputs empty/invalid
  
  print("Calculating lengths...")
  intersections$length_m <- st_length(intersections)
  
  print("Aggregating lengths...")
  length_by_poly <- intersections %>%
    st_drop_geometry() %>%
    group_by(!!sym(poly_id_col)) %>%
    # Use summarise_at for robustness if length_m isn't created properly (though unlikely now)
    summarise(total_length_m = sum(length_m, na.rm = TRUE), .groups = 'drop')
  
  polygons_with_length <- polygons_sf %>%
    left_join(length_by_poly, by = poly_id_col) %>%
    # Assume join works, replace NA with 0 units
    mutate(total_length_m = ifelse(is.na(total_length_m), set_units(0, "m"), total_length_m))
  
  return(polygons_with_length)
}

print("Calculating Road Network Lengths...")
districts_roads <- calculate_length_in_polygons_simple(roads_proj, districts_proj)
print("Calculating Cycle Network Lengths...")
districts_cycleways <- calculate_length_in_polygons_simple(cycleways_proj, districts_proj)
print("Calculating Foot Network Lengths...")
districts_footways <- calculate_length_in_polygons_simple(footways_proj, districts_proj)


# --- 6c. Point/Polygon Count and Area Calculation (Simplified Functions) ---

# Simplified Function to count points within polygons
count_points_in_polygons_simple <- function(points_sf, polygons_sf, poly_id_col = "unique_district_id") {
  print("Assigning points to polygons...")
  # Use points_sf that intersect polygons_sf
  points_in_poly <- st_filter(points_sf, polygons_sf, .predicate = st_intersects)
  # Join attributes of polygons (like ID) to the points
  points_joined <- st_join(points_in_poly, polygons_sf %>% select(!!sym(poly_id_col)), join = st_intersects)
  
  print("Counting points per polygon...")
  count_by_poly <- points_joined %>%
    st_drop_geometry() %>%
    count(!!sym(poly_id_col), name = "point_count") # Use dplyr::count
  
  polygons_with_count <- polygons_sf %>%
    left_join(count_by_poly, by = poly_id_col) %>%
    mutate(point_count = ifelse(is.na(point_count), 0L, point_count)) # Use integer 0
  
  return(polygons_with_count)
}

# Simplified Function to calculate area of features within polygons
calculate_feature_area_in_polygons_simple <- function(features_sf, polygons_sf, poly_id_col = "unique_district_id") {
  print("Performing intersection for area calculation...")
  intersections <- st_intersection(features_sf, polygons_sf) # Warning: Can error if inputs empty/invalid
  
  print("Calculating feature areas...")
  intersections$feature_area_m2 <- st_area(intersections)
  
  print("Aggregating feature areas...")
  area_by_poly <- intersections %>%
    st_drop_geometry() %>%
    group_by(!!sym(poly_id_col)) %>%
    summarise(total_feature_area_m2 = sum(feature_area_m2, na.rm = TRUE), .groups = 'drop')
  
  polygons_with_area <- polygons_sf %>%
    left_join(area_by_poly, by = poly_id_col) %>%
    mutate(total_feature_area_m2 = ifelse(is.na(total_feature_area_m2), set_units(0, "m^2"), total_feature_area_m2))
  
  return(polygons_with_area)
}

print("Counting PT Stops...")
districts_pt_stops <- count_points_in_polygons_simple(pt_stops_proj, districts_proj)
print("Counting POIs (Amenities/Shops)...")
districts_pois <- count_points_in_polygons_simple(all_pois_proj, districts_proj)
print("Calculating Green Space Area...")
districts_green_space <- calculate_feature_area_in_polygons_simple(green_spaces_proj, districts_proj)
print("Counting Building Points...") # Changed calculation
districts_building_counts <- count_points_in_polygons_simple(building_points_proj, districts_proj, "unique_district_id") %>%
  rename(building_count = point_count) # Rename result


# --- 7. Combine Measures and Calculate Densities / Ratios (KPI Foundations) ---

# Combine all calculated measures into one sf object
# Start with the base district geometry and area
district_measures <- districts_proj %>%
  select(unique_district_id, name, district_area_m2, district_area_km2, geometry)

# Join lengths
district_measures <- district_measures %>%
  left_join(st_drop_geometry(districts_roads) %>% select(unique_district_id, total_length_m), by = "unique_district_id") %>%
  rename(road_length_m = total_length_m) %>%
  left_join(st_drop_geometry(districts_cycleways) %>% select(unique_district_id, total_length_m), by = "unique_district_id") %>%
  rename(cycle_length_m = total_length_m) %>%
  left_join(st_drop_geometry(districts_footways) %>% select(unique_district_id, total_length_m), by = "unique_district_id") %>%
  rename(foot_length_m = total_length_m)

# Join counts
district_measures <- district_measures %>%
  left_join(st_drop_geometry(districts_pt_stops) %>% select(unique_district_id, point_count), by = "unique_district_id") %>%
  rename(pt_stop_count = point_count) %>%
  left_join(st_drop_geometry(districts_pois) %>% select(unique_district_id, point_count), by = "unique_district_id") %>%
  rename(poi_count = point_count) %>%
  left_join(st_drop_geometry(districts_building_counts) %>% select(unique_district_id, building_count), by = "unique_district_id") # Join building counts

# Join areas (only green space now)
district_measures <- district_measures %>%
  left_join(st_drop_geometry(districts_green_space) %>% select(unique_district_id, total_feature_area_m2), by = "unique_district_id") %>%
  rename(green_space_area_m2 = total_feature_area_m2)

# Replace NAs introduced by joins with 0 or 0 units (simplified)
district_measures <- district_measures %>%
  mutate(
    across(ends_with("_length_m") | ends_with("_area_m2"), ~ tidyr::replace_na(., set_units(0, "m"))), # Handle length/area units
    across(ends_with("_count"), ~ tidyr::replace_na(., 0L)) # Handle counts (integer 0)
  )


# Calculate Densities and Ratios (Building metric changed)
district_measures <- district_measures %>%
  mutate(
    # Network Densities (km per km^2)
    road_density_km_km2 = drop_units(set_units(road_length_m, "km") / district_area_km2),
    cycle_density_km_km2 = drop_units(set_units(cycle_length_m, "km") / district_area_km2),
    foot_density_km_km2 = drop_units(set_units(foot_length_m, "km") / district_area_km2),
    
    # Point Densities (count per km^2)
    pt_stop_density_per_km2 = pt_stop_count / drop_units(district_area_km2),
    poi_density_per_km2 = poi_count / drop_units(district_area_km2),
    building_density_per_km2 = building_count / drop_units(district_area_km2), # NEW METRIC
    
    # Area Ratios (%)
    green_space_ratio_pct = drop_units(green_space_area_m2 / district_area_m2) * 100,
    # building_coverage_ratio_pct = drop_units(building_area_m2 / district_area_m2) * 100, # REMOVED
    
    # Infrastructure Ratios
    cycle_to_road_ratio = ifelse(drop_units(road_length_m) > 0, drop_units(cycle_length_m / road_length_m), 0),
    foot_to_road_ratio = ifelse(drop_units(road_length_m) > 0, drop_units(foot_length_m / road_length_m), 0),
    active_transport_density_km_km2 = cycle_density_km_km2 + foot_density_km_km2
  ) %>%
  # Handle potential division by zero if area is 0
  mutate(across(where(is.numeric) & !is.geometry, ~ifelse(is.infinite(.), 0, .))) %>% # Replace Inf with 0
  mutate(across(ends_with("_km2") | ends_with("_pct"), ~ifelse(drop_units(district_area_m2) == 0, 0, .)))


print("--- Measure calculation complete. ---")
# Display first few rows of the results
print(head(st_drop_geometry(district_measures)))


# --- 8. Calculate Proximity/Accessibility Measures (Simplified) ---

# --- 8a. Buffers around PT stops ---
buffer_dist_m <- 400
print(paste("Calculating", buffer_dist_m, "m buffers around PT stops..."))

# Assume pt_stops_proj is not empty
pt_stop_buffers <- st_buffer(pt_stops_proj, dist = buffer_dist_m) %>%
  st_union() %>%
  st_sf()
pt_buffer_total_area_m2 <- st_area(pt_stop_buffers)
print(paste("Total area within", buffer_dist_m, "m of a PT stop:", round(set_units(pt_buffer_total_area_m2, "km^2"), 2)))

# Calculate percentage of each district covered by PT buffers
print("Calculating district coverage by PT buffers...")
district_pt_coverage <- st_intersection(district_measures, pt_stop_buffers)
district_pt_coverage$covered_area_m2 <- st_area(district_pt_coverage)

coverage_summary <- district_pt_coverage %>%
  st_drop_geometry() %>%
  group_by(unique_district_id) %>%
  summarise(total_covered_area_m2 = sum(covered_area_m2, na.rm = TRUE), .groups = 'drop')

district_measures <- district_measures %>%
  left_join(coverage_summary, by = "unique_district_id") %>%
  mutate(
    total_covered_area_m2 = ifelse(is.na(total_covered_area_m2), set_units(0, "m^2"), total_covered_area_m2),
    pt_coverage_ratio_pct = drop_units(total_covered_area_m2 / district_area_m2) * 100,
    pt_coverage_ratio_pct = ifelse(drop_units(district_area_m2) == 0 | is.infinite(pt_coverage_ratio_pct), 0, pt_coverage_ratio_pct)
  ) %>%
  select(-total_covered_area_m2)


# --- 8b. Buffers around Green Spaces ---
buffer_dist_green_m <- 300
print(paste("Calculating", buffer_dist_green_m, "m buffers around green spaces..."))

# Assume green_spaces_proj is not empty
green_space_buffers <- st_buffer(green_spaces_proj, dist = buffer_dist_green_m) %>%
  st_union() %>%
  st_sf()
green_buffer_total_area_m2 <- st_area(green_space_buffers)
print(paste("Total area within", buffer_dist_green_m, "m of a green space:", round(set_units(green_buffer_total_area_m2, "km^2"), 2)))

# Calculate percentage of each district covered by green space buffers
print("Calculating district coverage by green space buffers...")
district_green_coverage <- st_intersection(district_measures, green_space_buffers)
district_green_coverage$covered_green_area_m2 <- st_area(district_green_coverage)

green_coverage_summary <- district_green_coverage %>%
  st_drop_geometry() %>%
  group_by(unique_district_id) %>%
  summarise(total_covered_green_area_m2 = sum(covered_green_area_m2, na.rm = TRUE), .groups = 'drop')

district_measures <- district_measures %>%
  left_join(green_coverage_summary, by = "unique_district_id") %>%
  mutate(
    total_covered_green_area_m2 = ifelse(is.na(total_covered_green_area_m2), set_units(0, "m^2"), total_covered_green_area_m2),
    green_access_coverage_ratio_pct = drop_units(total_covered_green_area_m2 / district_area_m2) * 100,
    green_access_coverage_ratio_pct = ifelse(drop_units(district_area_m2) == 0 | is.infinite(green_access_coverage_ratio_pct), 0, green_access_coverage_ratio_pct)
  ) %>%
  select(-total_covered_green_area_m2)

print("--- Proximity measure calculation complete. ---")
print(head(st_drop_geometry(district_measures %>% select(name, pt_coverage_ratio_pct, green_access_coverage_ratio_pct))))


# --- 9. Visualization (Simplified) ---

# --- 9a. Basic Network Visualization ---
print("Creating Leaflet network map...")
# Direct transform
roads_wgs84 <- st_transform(roads_sf, 4326)
cycleways_wgs84 <- st_transform(cycleways_sf, 4326)
footways_wgs84 <- st_transform(footways_sf, 4326)
pt_stops_wgs84 <- st_transform(pt_stops_sf, 4326)

# Map creation assumes data exists
map_networks <- leaflet() %>%
  addTiles(group = "OSM Base Map") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Base Map") %>%
  fitBounds(lng1 = aoi_bbox[1,1], lat1 = aoi_bbox[2,1],
            lng2 = aoi_bbox[1,2], lat2 = aoi_bbox[2,2]) %>%
  addPolylines(data = roads_wgs84, color = "grey", weight = 1, group = "Roads (Car)", popup = ~paste("Type:", highway)) %>%
  addPolylines(data = cycleways_wgs84, color = "red", weight = 2, group = "Cycleways") %>%
  addPolylines(data = footways_wgs84, color = "darkgreen", weight = 1, opacity = 0.7, group = "Footways") %>%
  addCircleMarkers(data = pt_stops_wgs84, color = "purple", radius = 3, stroke = FALSE, fillOpacity = 0.8, group = "PT Stops",
                   popup = ~paste(  "<div style='background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 5px; font-size: 12px;'>", # Removed font-family for default
                                  , "Name:"
                                  , name
                                  , "<br>Type:"
                                  , ifelse(!is.na(railway)
                                           , railway
                                           , ifelse(!is.na(highway)
                                                    , highway
                                                    , public_transport))
                                  , "</div>")
                   ) %>%
  addLayersControl(
    baseGroups = c("OSM Base Map", "CartoDB Base Map"),
    overlayGroups = c("Roads (Car)", "Cycleways", "Footways", "PT Stops"),
    options = layersControlOptions(collapsed = FALSE)
  )
# print(map_networks) # Uncomment to display

# --- 9b. Visualize Calculated Measures/Densities ---
print("Creating Leaflet density/measure map...")

# Direct transform
district_measures_wgs84 <- st_transform(district_measures, crs = st_crs(4326))

# Simplified palette creator (less robust to NA/single values but shorter)
create_palette_simple <- function(domain, palette = "viridis", rev = FALSE) {
  colorNumeric(palette = palette, domain = domain, na.color = "#bdbdbd", reverse = rev)
}

# Create palettes directly, assuming columns exist and have valid domains
pal_road_den <- create_palette_simple(district_measures_wgs84$road_density_km_km2)
pal_cycle_den <- create_palette_simple(district_measures_wgs84$cycle_density_km_km2)
pal_foot_den <- create_palette_simple(district_measures_wgs84$foot_density_km_km2)
pal_pt_stop_den <- create_palette_simple(district_measures_wgs84$pt_stop_density_per_km2)
pal_poi_den <- create_palette_simple(district_measures_wgs84$poi_density_per_km2)
pal_green_ratio <- create_palette_simple(district_measures_wgs84$green_space_ratio_pct)
pal_bldg_den <- create_palette_simple(district_measures_wgs84$building_density_per_km2) # New palette
pal_cycle_road_ratio <- create_palette_simple(district_measures_wgs84$cycle_to_road_ratio, palette="RdYlBu")
pal_pt_coverage <- create_palette_simple(district_measures_wgs84$pt_coverage_ratio_pct)
pal_green_access <- create_palette_simple(district_measures_wgs84$green_access_coverage_ratio_pct)


# Simplified function to add layers
add_measure_layer_simple <- function(map, value_col_name, pal, legend_title, group_name) {
  value_formula <- as.formula(paste0("~", value_col_name))
  label_formula <- as.formula(paste0("~paste(name, ': ', round(", value_col_name, ", 2))"))
  
  map %>% addPolygons(
    fillColor = ~pal(eval(value_formula)),
    weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
    label = label_formula,
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
    group = group_name
  ) %>%
    addLegend(pal = pal, values = value_formula, opacity = 0.7, title = legend_title,
              position = "bottomright", group = group_name)
}

# Create Leaflet map
measure_map <- leaflet(data = district_measures_wgs84) %>%
  addTiles(group = "OSM Base Map") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Base Map") %>%
  setView( lng = 10
           , lat = 53.4
           , zoom = 10 ) %>%
  setMaxBounds( lng1 = 9.626770
                , lat1 = 53.383328
                , lng2 = 10.351868
                , lat2 = 53.748711 
  )

# Add layers for key measures directly
measure_map <- measure_map %>%
  add_measure_layer_simple("road_density_km_km2", pal_road_den, "Road Dens.<br>(km/km²)", "Road Density") %>%
  add_measure_layer_simple("cycle_density_km_km2", pal_cycle_den, "Cycle Dens.<br>(km/km²)", "Cycleway Density") %>%
  add_measure_layer_simple("foot_density_km_km2", pal_foot_den, "Footway Dens.<br>(km/km²)", "Footway Density") %>%
  add_measure_layer_simple("pt_stop_density_per_km2", pal_pt_stop_den, "PT Stop Dens.<br>(stops/km²)", "PT Stop Density") %>%
  add_measure_layer_simple("poi_density_per_km2", pal_poi_den, "POI Dens.<br>(POIs/km²)", "POI Density") %>%
  add_measure_layer_simple("green_space_ratio_pct", pal_green_ratio, "Green Space<br>(%)", "Green Space Ratio") %>%
  add_measure_layer_simple("building_density_per_km2", pal_bldg_den, "Building Dens.<br>(bldgs/km²)", "Building Density") %>% # Updated layer
  add_measure_layer_simple("cycle_to_road_ratio", pal_cycle_road_ratio, "Cycle/Road<br>Ratio", "Cycle/Road Ratio") %>%
  add_measure_layer_simple("pt_coverage_ratio_pct", pal_pt_coverage, paste0("PT Access<br>(% Area within ", buffer_dist_m, "m)"), "PT Access Coverage") %>%
  add_measure_layer_simple("green_access_coverage_ratio_pct", pal_green_access, paste0("Green Access<br>(% Area within ", buffer_dist_green_m, "m)"), "Green Access Coverage")

# Define overlay groups
overlay_groups <- c("Road Density", "Cycleway Density", "Footway Density",
                    "PT Stop Density", "POI Density", "Green Space Ratio",
                    "Building Density", "Cycle/Road Ratio", # Updated group name
                    "PT Access Coverage", "Green Access Coverage")

# Add Layer Controls
measure_map <- measure_map %>%
  addLayersControl(
    baseGroups = c("OSM Base Map", "CartoDB Base Map"),
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  hideGroup(overlay_groups[-1]) # Hide all but the first


# Print the measure map
print(measure_map)

print("--- Analysis and visualization complete. ---")

library(htmlwidgets)

saveWidget(measure_map, file = "Widgets/measure_map.html", selfcontained = TRUE)