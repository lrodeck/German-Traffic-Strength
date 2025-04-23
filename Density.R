origin_bb# --- Prerequisites ---
# Assuming the following objects exist from the previous step:
# roads_sf, cycleways_sf, footways_sf, aoi_name, aoi_bbox
# Also ensure required packages are loaded: sf, osmdata, dplyr, leaflet

# --- 1. Fetch Administrative Boundaries (Hamburg Districts) ---
print("Querying OSM for administrative boundaries (Districts)...")
districts_osm <- opq(bbox = aoi_bbox) %>%
  # admin_level=9 often corresponds to Stadtteile in German cities
  add_osm_feature(key = "admin_level", value = "9") %>%
  # We expect polygons for boundaries
  osmdata_sf()

# Extract polygons, ensure they are valid
districts_sf <- districts_osm$osm_multipolygons %>%
  filter(!sf::st_is_empty(.)) %>% # Remove empty geometries
  sf::st_make_valid() # Attempt to fix invalid geometries

# Basic check if districts were found
if (nrow(districts_sf) == 0) {
  stop("No districts (admin_level=9) found for the specified area. Check OSM tagging or try a different admin_level.")
} else {
  print(paste("Found", nrow(districts_sf), "districts (admin_level=9)."))
  # Optional: Inspect names
  # print(districts_sf$name)
}

# --- 2. Prepare Data & Reproject ---
# Use UTM zone 32N (EPSG:32632) for Hamburg for accurate measurements
target_crs <- st_crs(32632)

districts_proj <- st_transform(districts_sf, target_crs)
roads_proj <- st_transform(roads_sf, target_crs)
cycleways_proj <- st_transform(cycleways_sf, target_crs)
footways_proj <- st_transform(footways_sf, target_crs)

# --- 3. Calculate Network Length per District ---

# Function to calculate length within polygons
calculate_length_in_polygons <- function(lines_sf, polygons_sf, poly_id_col = "osm_id") {
  # Ensure polygons have unique IDs if needed (osm_id might not be unique if split)
  polygons_sf <- polygons_sf %>% mutate(unique_poly_id = row_number())
  poly_id_col <- "unique_poly_id" # Use the new unique ID
  
  # Intersect lines with polygons
  print("Performing intersection...")
  intersections <- st_intersection(lines_sf, polygons_sf)
  
  # Calculate length of intersected segments (in meters)
  print("Calculating lengths...")
  intersections$length_m <- st_length(intersections)
  
  # Convert length to numeric (removes units)
  intersections$length_m <- as.numeric(intersections$length_m)
  
  # Aggregate length by polygon ID
  print("Aggregating lengths...")
  length_by_poly <- intersections %>%
    st_drop_geometry() %>% # Remove geometry for faster aggregation
    group_by(!!sym(poly_id_col)) %>%
    summarise(total_length_m = sum(length_m, na.rm = TRUE))
  
  # Join back to polygons
  polygons_with_length <- polygons_sf %>%
    left_join(length_by_poly, by = poly_id_col) %>%
    mutate(total_length_m = ifelse(is.na(total_length_m), 0, total_length_m)) # Replace NA with 0 length
  
  return(polygons_with_length)
}

# Calculate lengths for each mode
print("Calculating road lengths per district...")
districts_roads <- calculate_length_in_polygons(roads_proj, districts_proj)
print("Calculating cycleway lengths per district...")
districts_cycleways <- calculate_length_in_polygons(cycleways_proj, districts_proj)
print("Calculating footway lengths per district...")
districts_footways <- calculate_length_in_polygons(footways_proj, districts_proj)


# --- 4. Calculate Area per District ---
print("Calculating district areas...")
districts_proj$area_m2 <- as.numeric(st_area(districts_proj)) # Area in m^2

# --- 5. Calculate Density ---
print("Calculating densities...")


density_data <- districts_proj %>%
  mutate(unique_poly_id = row_number())%>%
  left_join(st_drop_geometry(districts_roads) %>% select(unique_poly_id, total_length_m), by = "unique_poly_id", suffix = c("", ".roads")) %>%
  rename(road_length_m = total_length_m) %>%
  left_join(st_drop_geometry(districts_cycleways) %>% select(unique_poly_id, total_length_m), by = "unique_poly_id", suffix = c("", ".cycle")) %>%
  rename(cycle_length_m = total_length_m) %>%
  left_join(st_drop_geometry(districts_footways) %>% select(unique_poly_id, total_length_m), by = "unique_poly_id", suffix = c("", ".foot")) %>%
  rename(foot_length_m = total_length_m) %>%
  # Replace NAs introduced by joins with 0
  mutate(
    road_length_m = ifelse(is.na(road_length_m), 0, road_length_m),
    cycle_length_m = ifelse(is.na(cycle_length_m), 0, cycle_length_m),
    foot_length_m = ifelse(is.na(foot_length_m), 0, foot_length_m)
  ) %>%
  # Calculate densities: (length in m / 1000) / (area in m^2 / 1,000,000)
  mutate(
    road_density_km_km2 = (road_length_m / 1000) / (area_m2 / 1e6),
    cycle_density_km_km2 = (cycle_length_m / 1000) / (area_m2 / 1e6),
    foot_density_km_km2 = (foot_length_m / 1000) / (area_m2 / 1e6)
  )

# Handle potential division by zero if area is 0 (unlikely for districts)
density_data <- density_data %>%
  mutate(
    road_density_km_km2 = ifelse(area_m2 == 0, 0, road_density_km_km2),
    cycle_density_km_km2 = ifelse(area_m2 == 0, 0, cycle_density_km_km2),
    foot_density_km_km2 = ifelse(area_m2 == 0, 0, foot_density_km_km2)
  )


# --- 6. Visualize Density with Leaflet ---

# We need to transform back to WGS84 (EPSG:4326) for Leaflet
density_data_wgs84 <- st_transform(density_data, crs = st_crs(4326))%>%
  filter(name %in% c("Altona","Bergedorf", "Harburg", "Hamburg-Mitte", "Eimsbüttel", "Hamburg-Nord", "Wandsbek"))

# Create color palettes for each density measure
pal_road <- colorNumeric(palette = "viridis", domain = density_data_wgs84$road_density_km_km2, na.color = "#bdbdbd")
pal_cycle <- colorNumeric(palette = "viridis", domain = density_data_wgs84$cycle_density_km_km2, na.color = "#bdbdbd")
pal_foot <- colorNumeric(palette = "viridis", domain = density_data_wgs84$foot_density_km_km2, na.color = "#bdbdbd")



# Create Leaflet map for densities
density_map <- leaflet(data = density_data_wgs84) %>%
  addTiles(group = "OSM Base Map") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Base Map") %>% # Add a lighter base map
  fitBounds(lng1 = aoi_bbox[1,1], lat1 = aoi_bbox[2,1], # Zoom map to the bounding box
            lng2 = aoi_bbox[1,2], lat2 = aoi_bbox[2,2]) %>%
  
  # Add Road Density Layer
  addPolygons(
    fillColor = ~pal_road(road_density_km_km2),
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = ~paste(name, ": ", round(road_density_km_km2, 2), " km/km²"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"),
    group = "Road Density (km/km²)"
  ) %>%
  addLegend(pal = pal_road, values = ~road_density_km_km2, opacity = 0.7, title = "Road Density",
            position = "bottomright", group = "Road Density (km/km²)") %>%
  
  # Add Cycleway Density Layer
  addPolygons(
    fillColor = ~pal_cycle(cycle_density_km_km2),
    weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
    label = ~paste(name, ": ", round(cycle_density_km_km2, 2), " km/km²"), labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
    group = "Cycleway Density (km/km²)"
  ) %>%
  addLegend(pal = pal_cycle, values = ~cycle_density_km_km2, opacity = 0.7, title = "Cycleway Density",
            position = "bottomright", group = "Cycleway Density (km/km²)") %>%
  leaflet::setView(lng=origin_pt_lon, lat=origin_pt_lat, zoom = 10)%>%
  # Add Footway Density Layer
  addPolygons(
    fillColor = ~pal_foot(foot_density_km_km2),
    weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
    highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
    label = ~paste(name, ": ", round(foot_density_km_km2, 2), " km/km²"), labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
    group = "Footway Density (km/km²)"
  ) %>%
  addLegend(pal = pal_foot, values = ~foot_density_km_km2, opacity = 0.7, title = "Footway Density",
            position = "bottomright", group = "Footway Density (km/km²)") %>%
  
  # Add Layer Controls to toggle visibility
  addLayersControl(
    baseGroups = c("CartoDB Base Map","OSM Base Map"),
    overlayGroups = c("Road Density (km/km²)", "Cycleway Density (km/km²)", "Footway Density (km/km²)"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Initially hide all but the road density layer and its legend
  hideGroup(c("Cycleway Density (km/km²)", "Footway Density (km/km²)"))


# Print the density map
print(density_map)

# Possible next steps: Accessibility analysis, Public Transport integration.