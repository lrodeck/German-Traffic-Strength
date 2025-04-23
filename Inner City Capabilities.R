# --- Prerequisites ---
# Assuming objects like cycleways_sf, aoi_bbox exist from previous steps.
# Ensure required packages are loaded: sf, osmdata, dplyr, leaflet
# Install and load dodgr if you haven't already
# install.packages("dodgr")
library(dodgr)
library(sf)
library(dplyr)
library(leaflet)
library(osmdata) # For geocoding the origin
library(tidyverse)
# --- 1. Prepare Network for dodgr (Example: Cycleways) ---


graph_cycle <- weight_streetnet(roads_sf)
v <- dodgr_vertices (graph_cycle)

# --- 2. Define Origin Point ---
origin_name <- "Hamburg Hauptbahnhof, Hamburg, Germany"
print(paste("Geocoding origin point:", origin_name))
origin_bb <- getbb(origin_name, format_out = "polygon") # Get bounding box polygon
origin_pt_lon <- origin_point[1]
origin_pt_lat <- origin_point[2]
aoi_bb <- getbb(origin_name, format_out = "polygon") # Get bounding box polygon

graph_cycle$edge_type <- graph_cycle$highway
from <- match_pts_to_verts(v, origin_bb)
to <- v$id
d_matrix <- dodgr_distances(graph = graph_cycle, from = from, to = to)

# --- 3. Calculate Isodistances ---
# Calculate distances from the origin vertex to all reachable vertices
# Let's set maximum distances (in meters) for our isodistance bands
distances_m <- c(1000, 2000, 3000, 5000, 7000, 10000, 20000) # e.g., 1km, 2km, 3km, 5km, 7km, 10km

print(paste("Calculating isodistances from origin up to", max(distances_m), "meters..."))
isodists <- dodgr_isodists(graph = graph_cycle,
                           from = from,
                           dlim = distances_m)



# --- 4. Prepare Results for Visualization ---
# We use the d_matrix calculated by dodgr_dists
min_dists <- apply(d_matrix, 2, max, na.rm = TRUE)

# Convert the named vector of minimum distances into a data frame
# The names of min_dists are the 'to' vertex IDs (as characters)
dist_df <- data.frame(
  vid = names(min_dists),
  d = min_dists,
  stringsAsFactors = FALSE
) %>%
  filter(is.finite(d)) # Remove non-reachable vertices (Inf distance)

# Filter this data frame to keep only points within your maximum distance
max_dist <- max(distances_m)
reachable_dist_df <- dist_df %>%
  filter(d <= max_dist)

# Get the coordinates of all vertices from the graph
# (Your original code for this was good)
graph_vertices <- dplyr::bind_rows(
  graph_cycle %>% select(vid = from_id, lon = from_lon, lat = from_lat),
  graph_cycle %>% select(vid = to_id, lon = to_lon, lat = to_lat)
) %>%
  distinct(vid, .keep_all = TRUE) %>%
  mutate(vid = as.character(vid)) # Ensure vid is character for joining

# Join the calculated distances (d) with the vertex coordinates
reachable_vertices_with_dist <- graph_vertices %>%
  inner_join(reachable_dist_df, by = "vid") # Use inner_join to keep only reachable vertices

# Check if any points were found
if (nrow(reachable_vertices_with_dist) == 0) {
  stop("No vertices found within the maximum distance. Check origin point or max distance.")
}

# Convert to an sf object for mapping with Leaflet
reach_sf <- st_as_sf(reachable_vertices_with_dist, coords = c("lon", "lat"), crs = 4326)




# --- 5. Visualize Reachability with Leaflet ---
print("Creating Leaflet accessibility map...")

# Create a color palette based on distance
pal_dist <- colorNumeric(palette = "plasma", domain = reach_sf$d, reverse = TRUE) # Reversed plasma: close=yellow, far=purple

accessibility_map <- leaflet(data = reach_sf) %>%
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Base Map") %>%
  addTiles(group = "OSM Base Map") %>%
  # Add the reachable network points, colored by distance
  addCircleMarkers(
    radius = 1, # Small points for vertices
    color = ~pal_dist(d),
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste("Distance:", round(d, 0), "m"), # Show distance on hover/click
    group = "Reachable Points (Cycle)",
  ) %>%
  addMiniMap(
    position = "bottomleft"
    , zoomLevelOffset = -3
  )%>%
  # Add the origin point marker
  addAwesomeMarkers(
    lng=origin_pt_lon, lat=origin_pt_lat,
    icon=awesomeIcons(icon='flag-checkered', markerColor = 'red', library='fa'),
    popup = origin_name,
    group = "Origin"
  ) %>%
  
  # Add a legend for the distance
  addLegend(
    pal = pal_dist,
    values = ~d,
    opacity = 0.7,
    title = "Distance (m) <br> by bike",
    position = "bottomright",
    group = "Reachable Points (Cycle)"
  ) %>%
  
  # Add Layer Controls
  addLayersControl(
    baseGroups = c("CartoDB Base Map", "OSM Base Map"),
    overlayGroups = c("Origin", "Reachable Points (Cycle)"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Print the accessibility map
print(accessibility_map)

print("--- Accessibility analysis and visualization complete. ---")
# Possible next steps: Time-based isochrones, Public Transport integration (GTFS), comparing modes.