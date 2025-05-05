# --- 1. Install and Load Necessary Packages----
# install.packages(c("sf", "osmdata", "dplyr", "leaflet", "units", "lwgeom", "tidyverse", "geminiR"))
library(sf) # Spatial data handling
library(osmdata) # OpenStreetMap data querying
library(dplyr) # Data manipulation
library(leaflet) # Interactive maps
library(units) # Handling distance/area units
library(lwgeom) # For st_make_valid if needed
library(tidyverse) # Data manipulation and piping
library(geminiR) # For calling the Gemini API

extrafont::loadfonts() # Assuming extrafont is installed for custom fonts

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

# Assumes multipolygons are returned and selects the first one named "Hamburg" with border_type "state"
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

# Handle potential empty parks_sf before union
if (nrow(parks_sf) > 0) {
  parks_proj <- st_transform(parks_sf, target_crs)
  parks_union_proj <- st_union(parks_proj) # Assumes parks_proj is not empty
  print(paste("Found and combined", nrow(parks_sf), "park areas."))
} else {
  print("No park polygons found in the query.")
  parks_union_proj <- st_sf(geometry = st_sfc(crs = target_crs)) # Create empty sf object with target CRS
}


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
} else if (!is.null(hbf_osm$osm_polygons) && nrow(hbf_osm$osm_polygons) > 0) {
  hbf_sf <- st_centroid(hbf_osm$osm_polygons[1, ]) # Use osm_polygons, could also check multipolygons
  print("Found Hamburg Hbf as a polygon, using centroid.")
} else {
  hbf_sf <- st_sf(geometry = st_sfc(st_point(), crs=4326)) # Empty point geometry if not found
  print("Hamburg Hbf not found as point or polygon.")
}

hbf_proj <- st_transform(hbf_sf, target_crs)

# --- 4c. Bars----
print("Querying OSM for bars...")
bars_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "amenity", value = c("bar", "pub")) %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(bars_osm$osm_points) && nrow(bars_osm$osm_points) > 0) {
  bars_sf <- bars_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(bars_sf), "bars/pubs as points."))
} else {
  bars_poly_centroids_sf <- bind_rows(bars_osm$osm_polygons, bars_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  bars_sf <- bars_poly_centroids_sf
  print(paste("Found", nrow(bars_sf), "bars/pubs as polygon/multipolygon centroids."))
}
bars_proj <- st_transform(bars_sf, target_crs)


# --- 4d. Restaurants----
print("Querying OSM for restaurants...")
restaurants_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(restaurants_osm$osm_points) && nrow(restaurants_osm$osm_points) > 0) {
  restaurants_sf <- restaurants_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(restaurants_sf), "restaurants as points."))
} else {
  restaurants_poly_centroids_sf <- bind_rows(restaurants_osm$osm_polygons, restaurants_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  restaurants_sf <- restaurants_poly_centroids_sf
  print(paste("Found", nrow(restaurants_sf), "restaurants as polygon/multipolygon centroids."))
}
restaurants_proj <- st_transform(restaurants_sf, target_crs)


# --- 4e. Other Leisure Activities (excluding parks)----
print("Querying OSM for other leisure activities...")
leisure_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "leisure") %>%
  osmdata_sf()

# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(leisure_osm$osm_points) && nrow(leisure_osm$osm_points) > 0) {
  all_leisure_locations_sf <- leisure_osm$osm_points
  print(paste("Found", nrow(all_leisure_locations_sf), "other leisure locations as points."))
} else {
  leisure_poly_centroids_sf <- bind_rows(leisure_osm$osm_polygons, leisure_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  all_leisure_locations_sf <- leisure_poly_centroids_sf
  print(paste("Found", nrow(all_leisure_locations_sf), "other leisure locations as polygon/multipolygon centroids."))
}

# Filter out the park-like leisure types
park_leisure_tags_to_exclude <- park_tags # Re-using park_tags defined earlier
other_leisure_sf <- all_leisure_locations_sf %>%
  filter(! (leisure %in% park_leisure_tags_to_exclude) )

other_leisure_proj <- st_transform(other_leisure_sf, target_crs)


# --- 4f. Bus Stops ----
print("Querying OSM for bus stops...")
bus_stops_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "highway", value = "bus_stop") %>%
  osmdata_sf()

# Bus stops are typically points, but handle potential polygons
if (!is.null(bus_stops_osm$osm_points) && nrow(bus_stops_osm$osm_points) > 0) {
  bus_stops_sf <- bus_stops_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(bus_stops_sf), "bus stops as points."))
} else {
  bus_stops_poly_centroids_sf <- bind_rows(bus_stops_osm$osm_polygons, bus_stops_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  bus_stops_sf <- bus_stops_poly_centroids_sf
  print(paste("Found", nrow(bus_stops_sf), "bus stops as polygon/multipolygon centroids."))
}

bus_stops_sf <- bus_stops_sf %>%
  # Optional: Remove duplicates based on name/location if desired
  arrange(name) %>% # optional: prioritize by order
  group_by(name) %>%
  slice(1) %>% # keep the first geometry per name
  ungroup()

bus_stops_proj <- st_transform(bus_stops_sf, target_crs)


# --- 4g. Subway Stations (U-Bahn)----
print("Querying OSM for subway stations (U-Bahn)...")
# Querying stations explicitly tagged as subway
subway_stations_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "station", value = "subway") %>%
  # Also include public_transport=station where station=subway might be missing but relation exists
  add_osm_feature(key = "public_transport", value = "station") %>%
  osmdata_sf()

# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(subway_stations_osm$osm_points) && nrow(subway_stations_osm$osm_points) > 0) {
  subway_stations_sf <- subway_stations_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(subway_stations_sf), "subway stations as points."))
} else {
  subway_poly_centroids_sf <- bind_rows(subway_stations_osm$osm_polygons, subway_stations_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  subway_stations_sf <- subway_poly_centroids_sf
  print(paste("Found", nrow(subway_stations_sf), "subway stations as polygon/multipolygon centroids."))
}

subway_stations_sf <- subway_stations_sf %>%
  # Optional: Remove duplicates based on name/location if desired
  arrange(name) %>% # optional: prioritize by order
  group_by(name) %>%
  slice(1) %>% # keep the first geometry per name
  ungroup()

subway_stations_proj <- st_transform(subway_stations_sf, target_crs)
print(paste("Found", nrow(subway_stations_sf), "potential subway station locations."))


# --- 4h. Schools ----
print("Querying OSM for schools...")
schools_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "school") %>%
  osmdata_sf()

# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(schools_osm$osm_points) && nrow(schools_osm$osm_points) > 0) {
  schools_sf <- schools_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(schools_sf), "school locations as points."))
} else {
  schools_poly_centroids_sf <- bind_rows(schools_osm$osm_polygons, schools_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  schools_sf <- schools_poly_centroids_sf
  print(paste("Found", nrow(schools_sf), "school locations as polygon/multipolygon centroids."))
}
schools_proj <- st_transform(schools_sf, target_crs)


# --- 4i. Kindergartens ----
print("Querying OSM for kindergartens...")
kindergartens_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "kindergarten") %>%
  osmdata_sf()

# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(kindergartens_osm$osm_points) && nrow(kindergartens_osm$osm_points) > 0) {
  kindergartens_sf <- kindergartens_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(kindergartens_sf), "kindergarten locations as points."))
} else {
  kindergartens_poly_centroids_sf <- bind_rows(kindergartens_osm$osm_polygons, kindergartens_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  kindergartens_sf <- kindergartens_poly_centroids_sf
  print(paste("Found", nrow(kindergartens_sf), "kindergarten locations as polygon/multipolygon centroids."))
}
kindergartens_proj <- st_transform(kindergartens_sf, target_crs)


# --- 4j. Supermarkets ----
print("Querying OSM for supermarkets...")
supermarkets_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "shop", value = "supermarket") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(supermarkets_osm$osm_points) && nrow(supermarkets_osm$osm_points) > 0) {
  supermarkets_sf <- supermarkets_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(supermarkets_sf), "supermarket locations as points."))
} else {
  supermarkets_poly_centroids_sf <- bind_rows(supermarkets_osm$osm_polygons, supermarkets_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  supermarkets_sf <- supermarkets_poly_centroids_sf
  print(paste("Found", nrow(supermarkets_sf), "supermarket locations as polygon/multipolygon centroids."))
}
supermarkets_proj <- st_transform(supermarkets_sf, target_crs)


# --- 4k. Cafes ----
print("Querying OSM for cafes...")
cafes_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "cafe") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(cafes_osm$osm_points) && nrow(cafes_osm$osm_points) > 0) {
  cafes_sf <- cafes_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(cafes_sf), "cafe locations as points."))
} else {
  cafes_poly_centroids_sf <- bind_rows(cafes_osm$osm_polygons, cafes_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  cafes_sf <- cafes_poly_centroids_sf
  print(paste("Found", nrow(cafes_sf), "cafe locations as polygon/multipolygon centroids."))
}
cafes_proj <- st_transform(cafes_sf, target_crs)


# --- 4l. Doctors ----
print("Querying OSM for doctors...")
doctors_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "doctors") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(doctors_osm$osm_points) && nrow(doctors_osm$osm_points) > 0) {
  doctors_sf <- doctors_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(doctors_sf), "doctor locations as points."))
} else {
  doctors_poly_centroids_sf <- bind_rows(doctors_osm$osm_polygons, doctors_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  doctors_sf <- doctors_poly_centroids_sf
  print(paste("Found", nrow(doctors_sf), "doctor locations as polygon/multipolygon centroids."))
}
doctors_proj <- st_transform(doctors_sf, target_crs)


# --- 4m. Hospitals ----
print("Querying OSM for hospitals...")
hospitals_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(hospitals_osm$osm_points) && nrow(hospitals_osm$osm_points) > 0) {
  hospitals_sf <- hospitals_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(hospitals_sf), "hospital locations as points."))
} else {
  hospitals_poly_centroids_sf <- bind_rows(hospitals_osm$osm_polygons, hospitals_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  hospitals_sf <- hospitals_poly_centroids_sf
  print(paste("Found", nrow(hospitals_sf), "hospital locations as polygon/multipolygon centroids."))
}
hospitals_proj <- st_transform(hospitals_sf, target_crs)


# --- 4n. Museums ----
print("Querying OSM for museums...")
museums_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "tourism", value = "museum") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(museums_osm$osm_points) && nrow(museums_osm$osm_points) > 0) {
  museums_sf <- museums_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(museums_sf), "museum locations as points."))
} else {
  museums_poly_centroids_sf <- bind_rows(museums_osm$osm_polygons, museums_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  museums_sf <- museums_poly_centroids_sf
  print(paste("Found", nrow(museums_sf), "museum locations as polygon/multipolygon centroids."))
}
museums_proj <- st_transform(museums_sf, target_crs)


# --- 4o. Cinemas ----
print("Querying OSM for cinemas...")
cinemas_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "cinema") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(cinemas_osm$osm_points) && nrow(cinemas_osm$osm_points) > 0) {
  cinemas_sf <- cinemas_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(cinemas_sf), "cinema locations as points."))
} else {
  cinemas_poly_centroids_sf <- bind_rows(cinemas_osm$osm_polygons, cinemas_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  cinemas_sf <- cinemas_poly_centroids_sf
  print(paste("Found", nrow(cinemas_sf), "cinema locations as polygon/multipolygon centroids."))
}
cinemas_proj <- st_transform(cinemas_sf, target_crs)


# --- 4p. Sports Centres ----
print("Querying OSM for sports centres...")
sports_centres_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "leisure", value = "sports_centre") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(sports_centres_osm$osm_points) && nrow(sports_centres_osm$osm_points) > 0) {
  sports_centres_sf <- sports_centres_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(sports_centres_sf), "sports centre locations as points."))
} else {
  sports_centres_poly_centroids_sf <- bind_rows(sports_centres_osm$osm_polygons, sports_centres_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  sports_centres_sf <- sports_centres_poly_centroids_sf
  print(paste("Found", nrow(sports_centres_sf), "sports centre locations as polygon/multipolygon centroids."))
}
sports_centres_proj <- st_transform(sports_centres_sf, target_crs)


# --- 4q. Libraries ----
print("Querying OSM for libraries...")
libraries_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711), timeout = 120) %>%
  add_osm_feature(key = "amenity", value = "library") %>%
  osmdata_sf()
# Prioritize points, otherwise use polygon/multipolygon centroids
if (!is.null(libraries_osm$osm_points) && nrow(libraries_osm$osm_points) > 0) {
  libraries_sf <- libraries_osm$osm_points %>% filter(!sf::st_is_empty(.))
  print(paste("Found", nrow(libraries_sf), "library locations as points."))
} else {
  libraries_poly_centroids_sf <- bind_rows(libraries_osm$osm_polygons, libraries_osm$osm_multipolygons) %>%
    filter(!sf::st_is_empty(.)) %>%
    st_make_valid() %>%
    st_centroid()
  libraries_sf <- libraries_poly_centroids_sf
  print(paste("Found", nrow(libraries_sf), "library locations as polygon/multipolygon centroids."))
}
libraries_proj <- st_transform(libraries_sf, target_crs)

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
# --- 6a Add District Information ----
print("Querying OSM for Hamburg districts (admin_level=6)...")
districts_osm <- opq(bbox = c(9.626770,53.383328,10.351868,53.748711)) %>%
  add_osm_feature(key = "boundary", value = "administrative") %>%
  add_osm_feature(key = "admin_level", value = "9") %>%
  osmdata_sf()

# Process district multipolygons (most likely represent districts)
districts_sf <- districts_osm$osm_multipolygons  %>%
  filter(!sf::st_is_empty(.)) %>%
  st_make_valid() %>%
  # Optional: Filter by name if specific districts are needed, or if query pulls unrelated features
  filter(name %in% c("Altona", "Eimsbüttel", "Hamburg-Nord", "Wandsbek", "Bergedorf", "Harburg", "Hamburg-Mitte")) %>%
  select(district_name = name) # Keep only name and rename the column

# Project district boundaries to the target CRS
districts_proj <- st_transform(districts_sf, target_crs)
print(paste("Found", nrow(districts_proj), "potential district polygons."))



# Perform spatial join: Join hexagon centroids to district polygons
# This assigns the district name whose polygon contains the centroid
print("Joining hexagon centroids to district polygons...")
hex_district_join <- st_join(hex_centroids_proj, districts_proj, join = st_within, left = TRUE) %>%
  st_drop_geometry() # Drop geometry after join, only keep hex_id and district_name

# Join the district name back to the main hexagon grid object
hex_grid_hamburg <- hex_grid_hamburg %>%
  left_join(hex_district_join, by = "hex_id")

print("Added district information to hexagons.")
print(paste("Number of hexagons with district name:", sum(!is.na(hex_grid_hamburg$district_name))))
print(paste("Number of hexagons without district name:", sum(is.na(hex_grid_hamburg$district_name)))) # Might be outer boundary hexagons

# --- 6b. Proximity to Nearest Park ----
print("Calculating proximity to nearest park...")
dist_to_park <- st_distance(hex_centroids_proj, parks_union_proj)
hex_grid_hamburg$dist_park_m <- drop_units(dist_to_park[,1]) # Assumes distance calc works

# --- 6c. Proximity to Main Station----
print("Calculating proximity to Hamburg Hbf...")
dist_to_hbf <- st_distance(hex_centroids_proj, hbf_proj)
hex_grid_hamburg$dist_hbf_m <- drop_units(dist_to_hbf[,1]) # Assumes distance calc works

# --- 6d. Count Points within Hexagons----
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
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, bus_stops_proj, "bus_stop_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, subway_stations_proj, "subway_station_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, schools_proj, "school_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, kindergartens_proj, "kindergarten_count")
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, supermarkets_proj, "supermarket_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, cafes_proj, "cafe_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, doctors_proj, "doctors_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, hospitals_proj, "hospital_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, museums_proj, "museum_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, cinemas_proj, "cinema_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, sports_centres_proj, "sports_centre_count") # New
hex_grid_hamburg <- count_points_in_hex_simple(hex_grid_hamburg, libraries_proj, "library_count") # New

hex_grid_hamburg <- hex_grid_hamburg%>%
  filter(!is.na(district_name))

print("Finished calculating metrics.")


# --- 6e. Generate Summary Table----
# (Optional summary)
# print("--- Overall Summary Statistics for Hexagons ---")
# hex_summary_data <- st_drop_geometry(hex_grid_hamburg)
# print(summary(hex_summary_data %>% select(dist_park_m, dist_hbf_m, bar_count, restaurant_count, other_leisure_count, bus_stop_count, subway_station_count)))
# print("---")


# 6f. Calculate Attractivity Index ---- 
print("Calculating Attractivity Index...")

# --- Define Weights (adjust as needed, should sum to 1) ---
weights <- list(
  park_dist = 0.06, # Proximity to parks
  hbf_dist = 0.05, # Proximity to main station
  bars = 0.05, # Density of bars (adjusted)
  restaurants = 0.05, # Density of restaurants (adjusted - increased to make sum 1)
  leisure = 0.05, # Density of other leisure (adjusted)
  bus_stops = 0.11, # Density of bus stops (adjusted)
  subway_stations = 0.11, # Density of subway stations (adjusted)
  schools = 0.08, # Density of schools (adjusted)
  kindergartens = 0.08, # Density of kindergartens (adjusted)
  supermarkets = 0.08, # Density of supermarkets (New)
  cafes = 0.06, # Density of cafes (New)
  doctors = 0.06, # Density of doctors (New)
  hospitals = 0.03, # Density of hospitals (New)
  museums = 0.02, # Density of museums (New)
  cinemas = 0.03, # Density of cinemas (New)
  sports_centres = 0.05, # Density of sports centres (New)
  libraries = 0.03 # Density of libraries (New)
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
    bus_stop_score = normalize_min_max(bus_stop_count),
    subway_station_score = normalize_min_max(subway_station_count),
    school_score = normalize_min_max(school_count),
    kindergarten_score = normalize_min_max(kindergarten_count),
    supermarket_score = normalize_min_max(supermarket_count), # New
    cafe_score = normalize_min_max(cafe_count), # New
    doctors_score = normalize_min_max(doctors_count), # New
    hospital_score = normalize_min_max(hospital_count), # New
    museum_score = normalize_min_max(museum_count), # New
    cinema_score = normalize_min_max(cinema_count), # New
    sports_centre_score = normalize_min_max(sports_centre_count), # New
    library_score = normalize_min_max(library_count), # New
    
    
    # Normalize distances (lower is better), then invert score (1 - normalized)
    dist_park_norm = normalize_min_max(dist_park_m),
    park_dist_score = 1 - dist_park_norm, # Higher score = closer to park
    
    dist_hbf_norm = normalize_min_max(dist_hbf_m),
    hbf_dist_score = 1 - dist_hbf_norm, # Higher score = closer to Hbf
    
    # --- Calculate final weighted index ---
    # Replace NA scores with 0 before weighting
    attractivity_index = ( weights$park_dist * replace_na(park_dist_score, 0) ) +
      ( weights$hbf_dist * replace_na(hbf_dist_score, 0) ) +
      ( weights$bars * replace_na(bar_score, 0) ) +
      ( weights$restaurants * replace_na(restaurant_score, 0) ) +
      ( weights$leisure * replace_na(leisure_score, 0) ) +
      ( weights$bus_stops * replace_na(bus_stop_score, 0) ) +
      ( weights$subway_stations * replace_na(subway_station_score, 0) ) +
      ( weights$schools * replace_na(school_score, 0) ) +
      ( weights$kindergartens * replace_na(kindergarten_score, 0) ) +
      ( weights$supermarkets * replace_na(supermarket_score, 0) ) + # New
      ( weights$cafes * replace_na(cafe_score, 0) ) + # New
      ( weights$doctors * replace_na(doctors_score, 0) ) + # New
      ( weights$hospitals * replace_na(hospital_score, 0) ) + # New
      ( weights$museums * replace_na(museum_score, 0) ) + # New
      ( weights$cinemas * replace_na(cinema_score, 0) ) + # New
      ( weights$sports_centres * replace_na(sports_centre_score, 0) ) + # New
      ( weights$libraries * replace_na(library_score, 0) ) # New
  )

print("Attractivity Index calculated.")
print("Summary of Index:")
print(summary(hex_grid_hamburg$attractivity_index))
print(paste("Min Index:", round(min(hex_grid_hamburg$attractivity_index, na.rm=TRUE), 4)))
print(paste("Max Index:", round(max(hex_grid_hamburg$attractivity_index, na.rm=TRUE), 4)))


# --- 8. Add AI Analysis to Hexagons ----
print("Starting AI analysis for each hexagon using the geminiR package...")

# Load necessary library for interacting with the Gemini API
# install.packages("geminiR") # Install geminiR package from CRAN
library(geminiR)
setAPI("AIzaSyBlZdzXdPOvEEeCR3tFDQMb1GIlo28RFy8") # check https://makersuite.google.com/app/apikey

# --- Function to construct the AI prompt for a single hexagon ---
# This function takes a single row (a hexagon's data) and formats it into a prompt string.
# Customize this prompt to guide the AI's analysis.
construct_ai_prompt <- function(hex_data_row) {
  # Extract relevant data points (adjust column names as needed)
  hex_id <- hex_data_row$hex_id
  district <- hex_data_row$district_name
  dist_park <- hex_data_row$dist_park_m
  dist_hbf <- hex_data_row$dist_hbf_m
  bars <- hex_data_row$bar_count
  restaurants <- hex_data_row$restaurant_count
  leisure <- hex_data_row$other_leisure_count
  bus_stops <- hex_data_row$bus_stop_count
  subway_stations <- hex_data_row$subway_station_count
  schools <- hex_data_row$school_count
  kindergartens <- hex_data_row$kindergarten_count
  supermarkets <- hex_data_row$supermarket_count # New
  cafes <- hex_data_row$cafe_count # New
  doctors <- hex_data_row$doctors_count # New
  hospitals <- hex_data_row$hospital_count # New
  museums <- hex_data_row$museum_count # New
  cinemas <- hex_data_row$cinema_count # New
  sports_centres <- hex_data_row$sports_centre_count # New
  libraries <- hex_data_row$library_count # New
  attractivity <- hex_data_row$attractivity_index # The calculated index
  
  # Handle potential NA values gracefully in the prompt
  district_text <- if (is.na(district)) "an unknown district" else paste("the district of", district)
  park_text <- if (is.na(dist_park)) "unknown distance to a park" else paste(round(dist_park), "meters from the nearest park")
  hbf_text <- if (is.na(dist_hbf)) "unknown distance to Hamburg Hauptbahnhof" else paste(round(dist_hbf), "meters from Hamburg Hauptbahnhof")
  
  # Construct the prompt string, including new data points
  prompt <- paste0(
    "Analyze the characteristics of an area in Hamburg, Germany, located in ", district_text, ". ",
    "Based on the following data points, provide a concise summary of its key features and potential appeal:\n",
    "- Distance to nearest park: ", park_text, "\n",
    "- Distance to Hamburg Hauptbahnhof: ", hbf_text, "\n",
    "- Number of bars/pubs: ", bars, "\n",
    "- Number of restaurants: ", restaurants, "\n",
    "- Number of other leisure locations: ", leisure, "\n",
    "- Number of bus stops: ", bus_stops, "\n",
    "- Number of subway stations: ", subway_stations, "\n",
    "- Number of schools: ", schools, "\n",
    "- Number of kindergartens: ", kindergartens, "\n",
    "- Number of supermarkets: ", supermarkets, "\n", # New
    "- Number of cafes: ", cafes, "\n", # New
    "- Number of doctors: ", doctors, "\n", # New
    "- Number of hospitals: ", hospitals, "\n", # New
    "- Number of museums: ", museums, "\n", # New
    "- Number of cinemas: ", cinemas, "\n", # New
    "- Number of sports centres: ", sports_centres, "\n", # New
    "- Number of libraries: ", libraries, "\n", # New
    "- Calculated Attractivity Index (0-1 scale): ", round(attractivity, 3), "\n\n",
    "Focus on summarizing its accessibility (parks, Hbf, public transport), density of amenities (bars, restaurants, leisure, retail, healthcare, culture, sports, libraries), and presence of educational facilities. Provide a brief overall assessment based on the attractivity index. Keep the analysis to 3-4 sentences." # Increased sentence count slightly
  )
  
  return(prompt)
}


# --- Function to call Gemini using the geminiR package ---
# This function sends the prompt to Gemini and extracts the analysis text from the response.
call_gemini_analysis <- function(prompt) {
  # Use geminiR::gemini_generate_content to call the API
  # The API key should be set via Sys.setenv or gemini_init before this function is called.
  response <- tryCatch({
    gemini(
      prompt = prompt
    )
  }, error = function(e) {
    message("Gemini API call failed: ", e$message)
    return(NULL) # Return NULL on error
  })
  
  if (is.null(response)) {
    message("NULL response from Gemini API.")
    return(NA_character_) # Return NA on API error
  }
  
  # geminiR::gemini_generate_content typically returns a list, with the generated text
  # often accessible directly or within a specific structure depending on the response format.
  # We'll try to extract the text content.
  analysis_text <- NA_character_ # Default to NA if extraction fails
  return(response)
}

# --- Iterate through hexagons and get AI analysis ---
# Initialize a new column for the AI analysis
hex_grid_hamburg$ai_analysis <- NA_character_

# Loop through each row (hexagon) in the data frame
# Using seq_len(nrow(hex_grid_hamburg)) is safer than 1:nrow(hex_grid_hamburg) for 0 rows
for (i in seq_len(nrow(hex_grid_hamburg))) {
  print(paste("Processing hexagon", i, "of", nrow(hex_grid_hamburg)))
  
  # Get the data for the current hexagon
  current_hex_data <- hex_grid_hamburg[i, ]
  
  # Construct the prompt
  prompt <- construct_ai_prompt(current_hex_data)
  # print(paste("Prompt for hexagon", i, ":", prompt)) # Optional: Print prompt for debugging
  
  # Call Gemini using the geminiR package
  analysis <-  call_gemini_analysis(prompt)
  
  # Store the analysis result
  hex_grid_hamburg$ai_analysis[i] <- analysis
  
  # Optional: Add a small delay between calls to avoid hitting rate limits
  Sys.sleep(3) # Sleep for 0.1 seconds
}

print("Finished AI analysis for all hexagons using geminiR.")

# Now hex_grid_hamburg has a new column 'ai_analysis' with the generated text.
# You can inspect the results:
# print(head(hex_grid_hamburg %>% select(hex_id, district_name, attractivity_index, ai_analysis)))

# To use this as static data, you would typically save the hex_grid_hamburg object
# For example, save as an RData file:
# save(hex_grid_hamburg, file = "hex_grid_hamburg_with_analysis.RData")
# Or save as a GeoPackage (if you want to use it in GIS software):
# st_write(hex_grid_hamburg, "hex_grid_hamburg_with_analysis.gpkg", append = FALSE)




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
neo_brutalist_colors <- c("#FF4911", "#9723c9", "#69D2E7", "#2FFF2F")

# Define the number of distinct color bins (quantiles)
n_color_bins <- 4 # Results in 4 distinct colors for 4 quantiles

# Use colorQuantile to create discrete color steps based on data distribution
pal_index <- colorNumeric(
  palette = neo_brutalist_colors,
  domain = hex_grid_viz$attractivity_index,
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
    max-width: 270px;
    max-height: 200px;
    overflow-y: scroll;
  '>",
  "<div style='
    font-family: \"Bebas Neue\", sans-serif;
    font-size: 18px;
    margin-bottom: 6px;
    letter-spacing: 0.5px;
  '><b>Hex ID: ", htmltools::htmlEscape(hex_grid_viz$hex_id), "</b></div>",
  # Add District Name if available
  ifelse(!is.na(hex_grid_viz$district_name), paste0("<div><b>District:</b> ", htmltools::htmlEscape(hex_grid_viz$district_name), "</div>"), ""),
  "<div><b>Attractivity Index:</b> ", round(hex_grid_viz$attractivity_index, 3), "</div>",
  "<hr style='margin: 8px 0;'>",
  "📍 <b>Dist. to Park:</b> ", ifelse(is.na(hex_grid_viz$dist_park_m), "N/A", paste0(round(hex_grid_viz$dist_park_m, 0), " m")), "<br>",
  "🚉 <b>Dist. to Hbf:</b> ", ifelse(is.na(hex_grid_viz$dist_hbf_m), "N/A", paste0(round(hex_grid_viz$dist_hbf_m, 0), " m")), "<br>",
  "🍻 <b>Bars/Pubs:</b> ", hex_grid_viz$bar_count, "<br>",
  "🍽️ <b>Restaurants:</b> ", hex_grid_viz$restaurant_count, "<br>",
  "🎯 <b>Other Leisure:</b> ", hex_grid_viz$other_leisure_count, "<br>",
  "🚌 <b>Bus Stops:</b> ", hex_grid_viz$bus_stop_count, "<br>",
  "🚇 <b>U-Bahn Stations:</b> ", hex_grid_viz$subway_station_count, "<br>",
  "🏫 <b>Schools:</b> ", hex_grid_viz$school_count, "<br>",
  "🎒 <b>Kindergartens:</b> ", hex_grid_viz$kindergarten_count, "<br>",
  "🛒 <b>Supermarkets:</b> ", hex_grid_viz$supermarket_count, "<br>", # New
  "☕ <b>Cafes:</b> ", hex_grid_viz$cafe_count, "<br>", # New
  "🩺 <b>Doctors:</b> ", hex_grid_viz$doctors_count, "<br>", # New
  "🏥 <b>Hospitals:</b> ", hex_grid_viz$hospital_count, "<br>", # New
  "🏛️ <b>Museums:</b> ", hex_grid_viz$museum_count, "<br>", # New
  "🎬 <b>Cinemas:</b> ", hex_grid_viz$cinema_count, "<br>", # New
  "🏟️ <b>Sports Centres:</b> ", hex_grid_viz$sports_centre_count, "<br>", # New
  "📚 <b>Libraries:</b> ", hex_grid_viz$library_count, # New
  "<hr style='margin: 8px 0;'>", # Add another separator before the AI analysis
  # Add the AI Analysis section
  "🧠 <b>AI Analysis:</b><br>",
  ifelse(is.na(hex_grid_viz$ai_analysis) | hex_grid_viz$ai_analysis == "",
         "Analysis not available.", # Message if analysis is missing
         htmltools::htmlEscape(hex_grid_viz$ai_analysis) # Display analysis, escaping HTML
  ),
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
    overflow: auto;
  '>",
  "<div style='
    font-family: \"Bebas Neue\", sans-serif;
    font-size: 18px;
    margin-bottom: 6px;
  '><b>Attractivity Index Explained</b></div>",
  "<hr style='margin: 8px 0;'>",
  "<p style='margin: 0;'>Index (0–1) based on weighted, normalized metrics:</p>",
  "<ul style='margin: 6px 0 8px 16px; padding-left: 0;'>",
  "<li>Proximity to Parks (", round(weights$park_dist * 100, 1), "%)</li>",
  "<li>Proximity to Hbf (", round(weights$hbf_dist * 100, 1), "%)</li>",
  "<li>Bars/Pubs (", round(weights$bars * 100, 1), "%)</li>",
  "<li>Restaurants (", round(weights$restaurants * 100, 1), "%)</li>",
  "<li>Leisure & Culture (", round((weights$leisure + weights$museums + weights$cinemas) * 100, 1), "%)</li>",
  "<li>Bus Stops (", round(weights$bus_stops * 100, 1), "%)</li>",
  "<li>U-Bahn Stations (", round(weights$subway_stations * 100, 1), "%)</li>",
  "<li>Educational Facilities (", round((weights$schools + weights$kindergartens) * 100, 1), "%)</li>",
  "<li>Everyday Amenities (", round((weights$supermarkets + weights$cafes) * 100, 1), "%)</li>",
  "<li>Healthcare Facilities (", round((weights$doctors + weights$hospitals) * 100, 1), "%)</li>",
  "<li>Sports Centres (", round(weights$sports_centres * 100, 1), "%)</li>",
  "<li>Libraries (", round(weights$libraries * 100, 1), "%)</li>",
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
htmlwidgets::saveWidget(viz_map_index, file = "Widgets/viz_map_index.html", selfcontained = TRUE)


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
