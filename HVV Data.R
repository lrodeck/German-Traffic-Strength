# --- Prerequisites ---
# Ensure required packages are loaded
library(tidytransit)
library(sf)
library(dplyr)
library(leaflet)
library(lubridate) # For easier date handling
library(osmdata)
library(units)
library(tibble) # Needed for summary table enframe
library(htmlwidgets) # For saveWidget

# --- Settings ---
gtfs_local_path <- "Data/hvv_gtfs_20200211.zip" # CRITICAL: This file MUST exist!
peak_start_time <- "07:00:00"
peak_end_time <- "09:00:00"
frequency_threshold_dep_per_hr <- 6 # Minimum departures/hour for "frequent"
walking_distance_m <- 400 # Buffer distance around frequent stops

# Use a projected CRS for accurate buffering (UTM 32N for Hamburg)
target_crs <- st_crs(32632)
# WGS84 for Leaflet output
wgs84_crs <- st_crs(4326)

# --- Bounding Box for Filtering ---
filter_bbox_coords <- c(xmin = 9.626770, ymin = 53.383328, xmax = 10.351868, ymax = 53.748711)
filter_bbox_poly_wgs84 <- st_bbox(filter_bbox_coords) %>%
  st_as_sfc() %>%
  st_set_crs(wgs84_crs)
filter_bbox_poly_proj <- st_transform(filter_bbox_poly_wgs84, target_crs)
print(paste("Using filter bounding box:", paste(round(filter_bbox_coords,4), collapse=", ")))

# --- 1. Define GTFS data source ---
# Assumes file exists, NO download check
print(paste("Using GTFS file:", gtfs_local_path))

# --- 2. Read GTFS Data ---
print("Reading GTFS data...")
hvv_gtfs <- read_gtfs(gtfs_local_path)
print("GTFS data read successfully.")

# --- 3. Identify Weekday Service IDs ---
print("Identifying active weekday service IDs...")
# Use current system date directly - ENSURE GTFS covers this date!
target_date <- as.Date(Sys.Date())
print(paste("Using target date for service ID check:", format(target_date, "%Y-%m-%d (%A)")))

# Check calendar.txt (assumes it exists)
weekday_service_ids_base <- hvv_gtfs$calendar %>%
  filter(tuesday == 1, # Service runs on Tuesdays
         start_date <= target_date,
         end_date >= target_date) %>%
  pull(service_id)

# Check calendar_dates.txt (assumes it exists)
# Assumes 'date' column is already Date type or correct format
hvv_gtfs$calendar_dates <- hvv_gtfs$calendar_dates %>% mutate(date = ymd(date))
added_service_ids <- hvv_gtfs$calendar_dates %>%
  filter(date == target_date, exception_type == 1) %>%
  pull(service_id)
removed_service_ids <- hvv_gtfs$calendar_dates %>%
  filter(date == target_date, exception_type == 2) %>%
  pull(service_id)

# Update the list of active service IDs
weekday_service_ids <- setdiff(union(weekday_service_ids_base, added_service_ids), removed_service_ids)
print(paste("Identified", length(weekday_service_ids), "active service IDs for", format(target_date)))

# --- 4. Calculate Stop Frequencies during Morning Peak ---
print(paste("Calculating stop frequencies between", peak_start_time, "and", peak_end_time, "..."))

# Filter stop_times (assumes weekday_service_ids has members)
gtfs_filtered <- hvv_gtfs
trips_on_weekday <- gtfs_filtered$trips %>% filter(service_id %in% weekday_service_ids)
gtfs_filtered$stop_times <- gtfs_filtered$stop_times %>% filter(trip_id %in% trips_on_weekday$trip_id)

# Calculate frequencies (assumes function works and returns data)
stop_freq_peak <- get_stop_frequency(gtfs_filtered,
                                     start_time = peak_start_time,
                                     end_time = peak_end_time,
                                     service_ids = weekday_service_ids)

# Calculate departures per hour (assumes result has n_departures)
time_window_hours <- (lubridate::hms(peak_end_time) - lubridate::hms(peak_start_time)) / lubridate::hours(1)
time_window_hours_num <- as.numeric(time_window_hours)
stop_freq_peak <- stop_freq_peak %>%
  mutate(departures_per_hr = n_departures / time_window_hours_num)

# --- 5. Identify High-Frequency Stops ---
print("Identifying high-frequency stops...")
high_freq_stop_ids <- stop_freq_peak %>%
  filter(departures_per_hr >= frequency_threshold_dep_per_hr) %>%
  pull(stop_id)

# --- 6. Get Spatial Stop Locations & Filter by BBOX ---
print("Getting spatial stop locations...")
stops_sf_all <- stops_as_sf(hvv_gtfs$stops) # All stops initially

# APPLY BOUNDING BOX FILTER TO STOPS (Assume CRS is WGS84 or transform works)
print(paste("Filtering stops to bounding box... Original count:", nrow(stops_sf_all)))
stops_sf_all <- st_transform(stops_sf_all, wgs84_crs) # Ensure WGS84
stops_sf <- st_filter(stops_sf_all, filter_bbox_poly_wgs84, .predicate = st_within)
print(paste("Filtered stops count (within BBOX):", nrow(stops_sf)))

# Filter spatial stops for high-frequency stops (within BBOX)
high_freq_stops_sf <- stops_sf %>%
  filter(stop_id %in% high_freq_stop_ids) %>%
  left_join(stop_freq_peak %>% select(stop_id, departures_per_hr), by = "stop_id")
print(paste("Found", nrow(high_freq_stops_sf), "high-frequency stops within the bounding box."))

# --- 7. Create Service Area Buffers & Clip to BBOX ---
print("Creating service area buffers...")

# Project stops (Assume high_freq_stops_sf has rows)
high_freq_stops_proj <- st_transform(high_freq_stops_sf, target_crs)

# Create buffers
service_buffers_proj <- st_buffer(high_freq_stops_proj, dist = walking_distance_m)

# Dissolve overlapping buffers
well_serviced_area_proj_unclipped <- st_union(service_buffers_proj)

# CLIP Service Area Buffer to BBOX
print("Clipping service area to bounding box...")
well_serviced_area_proj_unclipped <- st_make_valid(well_serviced_area_proj_unclipped)
filter_bbox_poly_proj <- st_make_valid(filter_bbox_poly_proj) # Ensure valid
well_serviced_area_proj <- st_intersection(well_serviced_area_proj_unclipped, filter_bbox_poly_proj)
print("Service area clipped.")

# Calculate area after clipping
clipped_area_m2 <- st_area(well_serviced_area_proj)
print(paste("Clipped well-serviced area:", round(units::set_units(clipped_area_m2, "km^2"), 2)))

# Transform final clipped area and filtered stops back to WGS84
well_serviced_area_wgs84 <- st_transform(well_serviced_area_proj, wgs84_crs)
high_freq_stops_wgs84 <- st_transform(high_freq_stops_sf, wgs84_crs)
all_stops_wgs84 <- st_transform(stops_sf, wgs84_crs) # Use the BBOX-filtered stops_sf

# --- 8. Analyze Coverage by District ---
# Assumes service area exists and OSM query works
print("Analyzing district coverage using the clipped service area...")
print("Querying OSM for administrative boundaries (Districts)...")
hamburg_bbox_osm <- c(9.626770,53.383328,10.351868,53.748711) # Bbox for OSM query

districts_osm <- opq(bbox = hamburg_bbox_osm) %>%
  add_osm_feature(key = "admin_level", value = "9") %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Process districts (Assume query successful and returns multipolygons)
districts_sf_raw <- districts_osm$osm_multipolygons %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid() %>%
  select(osm_id, name)

# Project districts
districts_proj <- st_transform(districts_sf_raw, target_crs)

# Calculate district area
districts_proj$district_area_m2 <- as.numeric(st_area(districts_proj))

# Calculate intersection area with the clipped service area
intersection_proj <- st_intersection(st_make_valid(districts_proj), st_make_valid(well_serviced_area_proj)) %>% mutate(unique_district_join_id = row_number())
intersection_proj$intersection_area_m2 <- as.numeric(st_area(intersection_proj))

# Aggregate intersection area by district
districts_proj <- districts_proj %>% mutate(unique_district_join_id = row_number())
coverage_by_district <- intersection_proj %>%
  st_drop_geometry() %>%
  group_by(unique_district_join_id) %>%
  summarise(covered_area_m2 = sum(intersection_area_m2, na.rm = TRUE))

# Join back and calculate percentage
districts_with_coverage <- districts_proj %>%
  left_join(coverage_by_district, by = "unique_district_join_id") %>%
  mutate(
    covered_area_m2 = ifelse(is.na(covered_area_m2), 0, covered_area_m2),
    coverage_pct = round((covered_area_m2 / district_area_m2) * 100, 1)
  ) %>%
  mutate(coverage_pct = pmin(coverage_pct, 100)) # Cap at 100%

# Transform for Leaflet
districts_coverage_wgs84 <- st_transform(districts_with_coverage, wgs84_crs)%>%
  filter(name %in% c("Bergedorf", "Harburg", "Hamburg-Mitte", "Altona", "Eimsbüttel", "Hamburg-Nord", "Wandsbek"))
# Define your custom colors
neo_brutalist_colors <- c("#69D2E7", "#90EE90", "#E3A018", "#FF69B4", "#9723c9")

# Use colorNumeric for a smooth gradient interpolating between the colors
valid_coverage_values <- districts_coverage_wgs84$coverage_pct[!is.na(districts_coverage_wgs84$coverage_pct)]
if (length(valid_coverage_values) > 0) {
  pal_coverage <- colorNumeric(
    palette = neo_brutalist_colors,
    domain = valid_coverage_values, # Use only non-NA values for domain calculation
    na.color = "#bdbdbd"       # Color for NA values
  )
} else {
  # Fallback if no valid data - create a dummy palette
  print("Warning: No valid coverage percentage values found for palette domain. Using dummy.")
  pal_coverage <- colorNumeric("Greys", domain = c(0,100), na.color = "#bdbdbd")
}
print("District coverage analysis complete.")

# --- 9. Visualize ---
print("Creating Leaflet map...")
map_center_lon <- mean(filter_bbox_coords[c(1, 3)])
map_center_lat <- mean(filter_bbox_coords[c(2, 4)])
initial_zoom <- 11


info_box <- htmltools::HTML("
  <div style='
    background-color: rgba(255, 255, 255, 0.95);
    padding: 15px;
    border-radius: 12px;
    font-family: Jost, sans-serif;
    font-size: 13px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    max-width: 260px;
    line-height: 1.4;
  '>
    <div style='font-family: \"Bebas Neue\", sans-serif; font-size: 18px; margin-bottom: 6px;'>
      🚌 PT Accessibility Map
    </div>
    <div>
      Highlighting high-frequency PT stops and district-level service coverage based on GTFS peak hours.
    </div>
  </div>
")


pt_map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Base Map") %>%
  setView(lng = map_center_lon, lat = map_center_lat, zoom = initial_zoom) %>%
  
  # All Subway Stops (faint yellow)
  addCircleMarkers(
    data = all_stops_wgs84,
    radius = 1,
    weight = 0.5,
    color = "#ffcc00",
    fillOpacity = 0.3,
    stroke = FALSE,
    popup = ~htmltools::htmlEscape(stop_name),
    group = "All Stops (within BBOX)"
  ) %>%
  
  # High-Frequency Stops (bold red)
  addCircleMarkers(
    data = high_freq_stops_wgs84,
    radius = 2,
    weight = 1,
    color = "#E31A1C",
    fillColor = "#E31A1C",
    fillOpacity = 0.8,
    stroke = TRUE,
    popup = ~paste(htmltools::htmlEscape(stop_name), "<br>",
                   round(departures_per_hr, 1), "dep/hr"),
    group = "High-Frequency Stops"
  ) %>%
  
  # Well-Serviced Area Buffer (blue outline)
  addPolygons(
    data = well_serviced_area_wgs84,
    color = "#007aff",
    weight = 1,
    smoothFactor = 0.5,
    opacity = 0.8,
    fillOpacity = 0.2,
    group = "Well-Serviced Area (Clipped)"
  ) %>%
  
  # District Coverage (Neo-Brutalist palette)
  addPolygons(
    data = districts_coverage_wgs84,
    fillColor = ~pal_coverage(coverage_pct),
    weight = 1,
    opacity = 0.9,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.6,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste(htmltools::htmlEscape(name), ": ", coverage_pct, "%"),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "12px",
      direction = "auto"
    ),
    group = "District Coverage (%)"
  ) %>%
  
  # Legend for coverage
  addLegend(
    data = districts_coverage_wgs84,
    pal = pal_coverage,
    values = ~coverage_pct,
    opacity = 0.7,
    title = "District<br>Coverage (%)",
    position = "bottomright",
    group = "District Coverage (%)"
  ) %>%
  
  # Layers control
  addLayersControl(
    baseGroups = "Base Map",
    overlayGroups = c(
      "Filter BBOX",
      "All Stops (within BBOX)",
      "High-Frequency Stops",
      "Well-Serviced Area (Clipped)",
      "District Coverage (%)"
    ),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  
  hideGroup("All Stops (within BBOX)") %>%
  
  setView(lng = 10, lat = 53.4, zoom = 10) %>%
  setMaxBounds(
    lng1 = 9.626770, lat1 = 53.383328,
    lng2 = 10.351868, lat2 = 53.748711
  )%>%
  addControl(html = info_box, position = "bottomleft")

font_css <- htmltools::tags$head(
  htmltools::HTML("
    <link href='https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Jost&display=swap' rel='stylesheet'>
    <style>
      .leaflet-control-layers {
        font-family: 'Jost', sans-serif;
        font-size: 13px;
        background-color: rgba(255, 255, 255, 0.95);
        border-radius: 10px;
        box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
        padding: 8px 10px;
        max-height: 300px;
        overflow-y: auto;
      }
      .leaflet-popup-content {
        font-family: 'Jost', sans-serif;
        font-size: 13px;
      }
      .leaflet-popup-content h3 {
        font-family: 'Bebas Neue', sans-serif;
        font-size: 18px;
        margin-top: 0;
        margin-bottom: 6px;
      }
    </style>
  ")
)

# Inject into the map
pt_map <- htmlwidgets::prependContent(pt_map, font_css)


# Display the map
print(pt_map)

# Ensure Widgets directory exists
if (!dir.exists("Widgets")) { dir.create("Widgets") }
saveWidget(pt_map, file = "Widgets/pt_map_filtered_clipped_straightforward.html", selfcontained = TRUE)
print("Straightforward filtered/clipped map saved to Widgets/pt_map_filtered_clipped_straightforward.html")


# --- 10. Create Updated Summary Table ---
print("Generating updated GTFS analysis summary table (straightforward)...")
gtfs_rounding_digits <- 1
summary_metrics <- list()

# 1. Analysis Context & Parameters
summary_metrics[["Analysis Target Date"]] <- format(target_date)
summary_metrics[["Peak Time Window"]] <- paste(peak_start_time, "-", peak_end_time)
summary_metrics[["Frequency Threshold (dep/hr)"]] <- frequency_threshold_dep_per_hr
summary_metrics[["Service Area Buffer (m)"]] <- walking_distance_m
summary_metrics[["GTFS Data Source Hint"]] <- basename(gtfs_local_path)
summary_metrics[["Filter BBOX Applied"]] <- paste(round(filter_bbox_coords, 4), collapse=", ")

# 2. Overall GTFS Stats (Reflecting BBOX filter)
total_stops_in_bbox <- nrow(stops_sf)
summary_metrics[["Total Stops in GTFS (within BBOX)"]] <- total_stops_in_bbox
summary_metrics[["Active Weekday Service IDs"]] <- length(weekday_service_ids)

# 3. Peak Frequency Analysis Results (Reflecting BBOX filter)
num_high_freq_stops_in_bbox <- nrow(high_freq_stops_sf)
num_stops_analyzed_in_bbox <- length(intersect(stop_freq_peak$stop_id, stops_sf$stop_id))
summary_metrics[["Stops with Departures in Peak (within BBOX)"]] <- num_stops_analyzed_in_bbox
summary_metrics[["High-Frequency Stops (within BBOX, Count)"]] <- num_high_freq_stops_in_bbox
summary_metrics[["High-Frequency Stops (within BBOX, %)"]] <- round((num_high_freq_stops_in_bbox / num_stops_analyzed_in_bbox) * 100, gtfs_rounding_digits)

# Avg/Median departures
freq_stats_filtered <- high_freq_stops_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(departures_per_hr)) %>%
  summarise(
    mean_dep_hr = mean(departures_per_hr),
    median_dep_hr = median(departures_per_hr)
  )
summary_metrics[["Avg. Departures/Hour (High-Freq Stops in BBOX)"]] <- round(freq_stats_filtered$mean_dep_hr, gtfs_rounding_digits)
summary_metrics[["Median Departures/Hour (High-Freq Stops in BBOX)"]] <- round(freq_stats_filtered$median_dep_hr, gtfs_rounding_digits)

# 4. Service Area Coverage Results (Reflecting BBOX clip)
total_serviced_area_clipped <- st_area(well_serviced_area_proj)
summary_metrics[["Total Well-Serviced Area (Clipped, km²)"]] <- round(set_units(total_serviced_area_clipped, "km^2"), 2)

# 5. District Coverage Analysis Results (Reflecting clipped service area)
district_stats_filtered <- districts_with_coverage %>%
  filter(name %in% c("Bergedorf", "Harburg", "Hamburg-Mitte", "Altona", "Eimsbüttel", "Hamburg-Nord", "Wandsbek"))%>%
  st_drop_geometry() %>%
  filter(!is.na(coverage_pct)) %>%
  summarise(
    num_districts = n(),
    avg_coverage = mean(coverage_pct, na.rm = TRUE),
    median_coverage = median(coverage_pct, na.rm = TRUE),
    min_coverage = min(coverage_pct, na.rm = TRUE),
    max_coverage = max(coverage_pct, na.rm = TRUE),
    districts_over_50_pct = sum(coverage_pct >= 50, na.rm = TRUE),
    districts_over_80_pct = sum(coverage_pct >= 80, na.rm = TRUE)
  )
summary_metrics[["Number of Districts Analyzed (Coverage)"]] <- district_stats_filtered$num_districts
summary_metrics[["Average District Coverage (%, Clipped Area)"]] <- round(district_stats_filtered$avg_coverage, gtfs_rounding_digits)
summary_metrics[["Median District Coverage (%, Clipped Area)"]] <- round(district_stats_filtered$median_coverage, gtfs_rounding_digits)
summary_metrics[["Min District Coverage (%, Clipped Area)"]] <- round(district_stats_filtered$min_coverage, gtfs_rounding_digits)
summary_metrics[["Max District Coverage (%, Clipped Area)"]] <- round(district_stats_filtered$max_coverage, gtfs_rounding_digits)
summary_metrics[["Districts >= 50% Coverage (Clipped Area, Count)"]] <- district_stats_filtered$districts_over_50_pct
summary_metrics[["Districts >= 80% Coverage (Clipped Area, Count)"]] <- district_stats_filtered$districts_over_80_pct

# --- Format and Print Updated Table ---
summary_table_gtfs_filtered <- tibble::enframe(summary_metrics, name = "Metric", value = "Value")
summary_table_gtfs_filtered$Value <- sapply(summary_table_gtfs_filtered$Value, function(x) {
  if (is.numeric(x)) format(x, digits = 2)
  else if (is.numeric(x) && is.na(x)) "NA" # Should ideally not happen now
  else as.character(x)
})
print("--- GTFS Frequency Analysis Summary (Filtered & Clipped - Straightforward) ---")
print(as.data.frame(summary_table_gtfs_filtered))
write.csv2(as.data.frame(summary_table_gtfs_filtered), "Data/summary_table_gtfs_filtered.csv")
print("--- Straightforward Script Finished ---")

