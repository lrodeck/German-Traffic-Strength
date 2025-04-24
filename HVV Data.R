# --- Prerequisites ---
# Ensure required packages are loaded
library(tidytransit)
library(sf)
library(dplyr)
library(leaflet)
library(lubridate) # For easier date handling
library(osmdata)
# --- 1. Define the GTFS data source ---
# URL provided by the user
gtfs_url <- "https://daten.transparenz.hamburg.de/Dataport.HmbTG.ZS.Webservice.GetRessource100/GetRessource100.svc/dbe5f144-b806-4377-aac3-d3572b139b23/Upload__hvv_Rohdaten_GTFS_Fpl_20250108.ZIP"

# It's often better to download the file first, then read it locally
# Option 1: Download first (Recommended)
gtfs_local_path <- "Data/hvv_gtfs_20200211.zip"
download.file(gtfs_url, gtfs_local_path, mode = "wb") # 'wb' is important for zip files


# --- Settings ---
gtfs_local_path <- "Data/hvv_gtfs_20200211.zip" # Make sure this file exists!
peak_start_time <- "07:00:00"
peak_end_time <- "09:00:00"
frequency_threshold_dep_per_hr <- 6 # Minimum departures/hour for "frequent"
walking_distance_m <- 400 # Buffer distance around frequent stops

# Use a projected CRS for accurate buffering (UTM 32N for Hamburg)
target_crs <- st_crs(32632)
# WGS84 for Leaflet output
wgs84_crs <- st_crs(4326)

# --- 1. Read GTFS Data ---

hvv_gtfs <- read_gtfs(gtfs_local_path)
print("GTFS data read successfully.")

# --- 2. Identify Weekday Service IDs ---
# Find service IDs active on a typical Tuesday in Feb 2020
# (Feb 11, 2020 was a Tuesday)
target_date <- as.Date(Sys.Date())

# Check calendar.txt first (defines recurring weekly services)
weekday_service_ids <- hvv_gtfs$calendar %>%
  filter(tuesday == 1, # Service runs on Tuesdays
         start_date <= target_date,
         end_date >= target_date) %>%
  pull(service_id)


# Check calendar_dates.txt for exceptions (added/removed services on specific dates)

# Services added on the target date
added_service_ids <- hvv_gtfs$calendar_dates %>%
  filter(date == target_date, exception_type == 1) %>%
  pull(service_id)

# Services explicitly removed on the target date
removed_service_ids <- hvv_gtfs$calendar_dates %>%
  filter(date == target_date, exception_type == 2) %>%
  pull(service_id)

# Update the list of active service IDs
weekday_service_ids <- setdiff(union(weekday_service_ids, added_service_ids), removed_service_ids)


print(paste("Identified", length(weekday_service_ids), "active service IDs for", target_date))

# --- 3. Calculate Stop Frequencies during Morning Peak ---
print(paste("Calculating stop frequencies between", peak_start_time, "and", peak_end_time, "..."))

# Filter stop_times to include only trips running on the target weekday services
# This is important for accurate frequency calculation if get_stop_frequencies doesn't handle service_id filtering internally
gtfs_filtered <- hvv_gtfs
trips_on_weekday <- gtfs_filtered$trips %>% filter(service_id %in% weekday_service_ids)
gtfs_filtered$stop_times <- gtfs_filtered$stop_times %>% filter(trip_id %in% trips_on_weekday$trip_id)



stop_freq_peak <- get_stop_frequency(gtfs_filtered, # Use the filtered GTFS object
                                     start_time = peak_start_time,
                                     end_time = peak_end_time)

# Calculate departures per hour (frequency = n_departures / time_window_hours)
time_window_hours <- (lubridate::hms(peak_end_time) - lubridate::hms(peak_start_time)) / lubridate::hours(1)
stop_freq_peak <- stop_freq_peak %>%
  mutate(departures_per_hr = n_departures / as.numeric(time_window_hours)) # Use as.numeric on duration

# --- 4. Identify High-Frequency Stops ---
print("Identifying high-frequency stops...")
high_freq_stop_ids <- stop_freq_peak %>%
  filter(departures_per_hr >= frequency_threshold_dep_per_hr) %>%
  pull(stop_id)

# --- 5. Get Spatial Stop Locations ---
stops_sf <- stops_as_sf(hvv_gtfs$stops)

# Filter for high-frequency stops
high_freq_stops_sf <- stops_sf %>%
  filter(stop_id %in% high_freq_stop_ids) %>%
  # Join frequency data for potential popups/styling
  left_join(stop_freq_peak %>% select(stop_id, departures_per_hr), by = "stop_id")

print(paste("Found", nrow(high_freq_stops_sf), "stops meeting frequency threshold."))

# --- 6. Create Service Area Buffers ---
print("Creating service area buffers...")
# Project stops to target CRS for accurate buffering
high_freq_stops_proj <- st_transform(high_freq_stops_sf, target_crs)

# Create buffers
service_buffers_proj <- st_buffer(high_freq_stops_proj, dist = walking_distance_m)

# Dissolve overlapping buffers into one multipart polygon
well_serviced_area_proj <- st_union(service_buffers_proj)

# Transform back to WGS84 for Leaflet
well_serviced_area_wgs84 <- st_transform(well_serviced_area_proj, wgs84_crs)
high_freq_stops_wgs84 <- st_transform(high_freq_stops_sf, wgs84_crs) # Also transform stops
all_stops_wgs84 <- st_transform(stops_sf, wgs84_crs) # And all stops for context

# --- 7. Optional: Analyze Coverage by District ---
print("Querying OSM for administrative boundaries (Districts)...")

# --- 7a Define Area of Interest (AOI) ---
aoi_name <- "Hamburg, Germany"
aoi_bbox <- getbb(aoi_name)
target_crs <- st_crs(32632) # UTM Zone 32N for Hamburg

districts_osm <- opq(bbox = aoi_bbox) %>%
  add_osm_feature(key = "admin_level", value = "9") %>% # Stadtteile in Hamburg
  osmdata_sf()

# Assume multipolygons are returned
districts_sf <- districts_osm$osm_multipolygons %>%
  filter(!st_is_empty(.)) %>% # Basic empty check
  st_make_valid() %>%
  select(osm_id, name) # Keep only relevant columns

districts_proj <- st_transform(districts_sf, target_crs)
districts_proj <- st_transform(districts_sf, target_crs) # Project if not already done

districts_proj$district_area_m2 <- as.numeric(st_area(districts_proj))
  
# Calculate intersection area
intersection_proj <- st_intersection(st_make_valid(districts_proj), st_make_valid(well_serviced_area_proj)) # Ensure valid geometries
intersection_proj$intersection_area_m2 <- as.numeric(st_area(intersection_proj))
intersection_proj <- intersection_proj %>%
  mutate(unique_poly_id = row_number())
# Aggregate intersection area by district
coverage_by_district <- intersection_proj %>%
  st_drop_geometry() %>%
  group_by(unique_poly_id) %>% # Assuming unique_poly_id exists from density step
  summarise(covered_area_m2 = sum(intersection_area_m2))
districts_proj <- districts_proj %>%
  mutate(unique_poly_id = row_number())
# Join back and calculate percentage
districts_with_coverage <- districts_proj %>%
  left_join(coverage_by_district, by = "unique_poly_id") %>%
  mutate(
    covered_area_m2 = ifelse(is.na(covered_area_m2), 0, covered_area_m2),
    coverage_pct = round((covered_area_m2 / district_area_m2) * 100, 1)
  ) %>%
  mutate(coverage_pct = ifelse(district_area_m2 == 0, 0, coverage_pct)) # Handle zero area districts

# Transform for Leaflet
districts_coverage_wgs84 <- st_transform(districts_with_coverage, wgs84_crs)
# Create palette for district coverage
pal_coverage <- colorNumeric("YlGnBu", domain = districts_coverage_wgs84$coverage_pct, na.color = "#bdbdbd")


# --- 8. Visualize ---
print("Creating Leaflet map...")

# Define center coordinates for Hamburg (approximate)
hamburg_center_lon <- 10.0
hamburg_center_lat <- 53.55
initial_zoom <- 10 # Adjust as needed


# Base map
pt_map <- leaflet() %>%
  # addTiles(group = "OSM Base Map")
  addProviderTiles(providers$CartoDB.Positron, group = "Base Map")%>%
  setView(lng = hamburg_center_lon, lat = hamburg_center_lat, zoom = initial_zoom)

# Add layer for all stops (optional, for context)
pt_map <- pt_map %>%
  addCircleMarkers(data = all_stops_wgs84,
                   radius = 1, weight=0.5, color = "#ffcc00", fillOpacity = 0.3, stroke = FALSE,
                   popup = ~stop_name,
                   group = "All Stops (Feb 2020)")

# Add layer for high-frequency stops
pt_map <- pt_map %>%
  addCircleMarkers(data = high_freq_stops_wgs84,
                   radius = 2, weight=1, color = "#ff3b30", fillColor="#ff3b30", fillOpacity = 0.8, stroke = TRUE,
                   popup = ~paste(stop_name, "<br>", round(departures_per_hr, 1), "dep/hr"),
                   group = "High-Frequency Stops")

# Add layer for the well-serviced area buffer
pt_map <- pt_map %>%
  addPolygons(data = well_serviced_area_wgs84,
              color = "#007aff", weight = 1, smoothFactor = 0.5,
              opacity = 0.8, fillOpacity = 0.2,
              group = "Well-Serviced Area (400m Buffer)")


# Define layer groups without the optional one
overlay_groups <- c("All Stops (Feb 2020)", "High-Frequency Stops", "Well-Serviced Area (400m Buffer)")


# Add layer controls
pt_map <- pt_map %>%
  addLayersControl(
    baseGroups = "Base Map",
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  # Initially hide all stops layer for clarity
  hideGroup("All Stops (Feb 2020)") # %>%

# Print the map
print(pt_map)

library(htmlwidgets)

saveWidget(pt_map, file = "Widgets/pt_map.html", selfcontained = TRUE)

