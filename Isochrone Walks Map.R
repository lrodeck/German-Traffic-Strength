# --- 1. Install and load required packages ---
library(osrm)
library(leaflet)

# --- 2. Define Urban Hubs in Hamburg ---
urban_hubs <- data.frame(
  id = c("Hamburg Hauptbahnhof", "Jungfernstieg", "Universität Hamburg", "Sternschanze", "Bahnhof Altona", "Hamburg Harburg", "Hafencity", "Wilhelmsburg", "Eppendorf", "Winterhude", "Wandsbek Markt", "Barmbek", "Bergedorf", "Eimsbüttel"),
  lon = c(10.006883725373468, 9.99291262534852, 9.984865084681049, 9.962111206809865, 9.93500422402423, 9.989408503775284, 9.999733548663391, 10.0074032134677, 9.981447612732463, 10.019276201329255, 10.068003410627544, 10.04442310312302, 10.207157209066542, 9.95171696240839), 
  lat = c(53.55249966181489, 53.55307843926604, 53.56649665705108, 53.56179594364584, 53.551840562903756, 53.45718989371573, 53.541238778797904, 53.498832180202996, 53.58519757910208, 53.58368051163418, 53.572331444574196, 53.58679041286183, 53.489532749185265, 53.5761658330116)
)


# --- 3. Simulated Bike Travel Time (in minutes) ---
# Simulate 3, 6, 9-minute bike rides
time_breaks_bike <- c(5, 10, 15)

# --- 4. Generate Isochrones for Each Hub ---
isochrones_list <- list()

for (i in 1:nrow(urban_hubs)) {
  hub_name <- urban_hubs$id[i]
  hub_coords <- c(urban_hubs$lon[i], urban_hubs$lat[i])
  
  iso <- osrmIsochrone(
    loc = hub_coords,
    breaks = time_breaks_bike,
    res = 70,
    osrm.profile = "foot"
  )
  
  iso$hub_name <- hub_name
  isochrones_list[[hub_name]] <- iso
}



# --- 5. Custom Font-Style HTML for Tooltips ---
title_style <- "font-family: 'Bebas Neue', sans-serif; font-size: 16px;"
body_style <- "font-family: 'Jost', sans-serif; font-size: 13px;"

# --- 6. Define Custom Colors per Hub ---
palettes <- list(
  "Hamburg Hauptbahnhof" = c("#4B75B1", "#69D2E7", "#7BECEA"),
  "Jungfernstieg"        = c("#BF4B54", "#FF69B4", "#FF7BD7"),
  "Universität Hamburg"  = c("#6AB58A", "#90EE90", "#DCF8BA"),
  "Sternschanze"         = c("#EF8051", "#EA8C34", "#E3A018"),
  "Bahnhof Altona"       = c("#9723c9", "#A779E4", "#C5B7F3"),
  "Hamburg Harburg"      = c("#2A2E45", "#7C8199", "#B2B5C1"),  # muted deep blue/gray tones
  "Hafencity"            = c("#167E5D", "#34C9A3", "#A7F3D0"),   # teal/greenish tones
  "Wilhelmsburg" = c("#167E5D", "#34C9A3", "#A7F3D0"),
  "Eppendorf" = c("#234E70", "#7CAEDB", "#B8D9F6"),
  "Winterhude" = c("#9C4221", "#F6AD55", "#FED7AA"),
  "Wandsbek Markt" = c("#3E2C41", "#9F7AEA", "#E9D8FD"),
  "Barmbek" = c("#22543D", "#68D391", "#C6F6D5"),
  "Bergedorf" = c("#2C5282", "#63B3ED", "#BEE3F8"),
  "Eimsbüttel" = c("#7B341E", "#F6AD55", "#FBD38D")
)

hub_images <- list(
  "Hamburg Hauptbahnhof" = "https://www.bahnhof.de/cms/images/dfbcea77-88dc-4b57-8009-9172caff2067",
  "Jungfernstieg" = "https://lsbg.hamburg.de/resource/image/974612/landscape_ratio3x2/565/377/b06b421a6d95d18f4af944df907c588a/D71A8200F36EF2CEC96408CC350EABC5/jungfernstieg-titelbild-neu.jpg",
  "Universität Hamburg" = "https://img.sparknews.funkemedien.de/241403990/241403990_1705048809_v16_9_1200.jpeg",
  "Sternschanze" = "https://www.hamburg-tourism.de/images/anpiJ1EASvE/rs:fill-down:958:716/sharpen:1.2/cb:/g:ce/aHR0cHM6Ly93d3cuaGFtYnVyZy10b3VyaXNtLmRlL2ZpbGVhZG1pbi9yZWRha3Rpb24vMV9EYXNfaXN0X0hhbWJ1cmcvSGFtYnVyZ19uYWNoaGFsdGlnX2VybGViZW4vTmFjaGhhbHRpZ2VfSGlnaGxpZ2h0cy9uYWNoaGFsdGlnZS1oaWdobGlnaHRzLXN0ZXJuc2NoYW56ZS1rYXJvdmllcnRlbC1mbG9oc2NoYW56ZS1jLWRhbS10aGlzaXNqdWxpYS1waG90b2dyYXBoeS13ZWItYmlsZGVyZ2FsZXJpZS0xMTI2eDYzMC5qcGc",
  "Bahnhof Altona"       = "https://upload.wikimedia.org/wikipedia/commons/1/12/Bahnhof_Hamburg-Altona_Au%C3%9Fenansicht.jpg",
  "Hamburg Harburg"      = "https://www.hamburg.com/resource/image/19350/landscape_ratio16x9/1240/697/5694dc4b2d9829febc74a972001c02c2/B2EB79E4F50C6A4D2AE7D1DCD959806D/km1-hamburg-harburg.jpg",
  "Hafencity" = "https://www.hamburg-tourism.de/images/Ec0tmHd5ESs/rs:fill-down:2500:974/sharpen:1.2/cb:/g:ce/aHR0cHM6Ly9oaHQuaW14cGxhdGZvcm0uZGUvZGF0YS9pbWFnZXMvaGFmZW5jaXR5X21hZ2VsbGFuLXRlcnJhc3Nlbl9jLTIwMTYtYW5kcmVhcy12YWxsYnJhY2h0LmpwZw",
  "Wilhelmsburg" = "https://www.hamburg.de/resource/image/374262/landscape_ratio16x9/714/402/13d5432f9fdfec3c1a86c52a0296de40/29230F270A9DA68E587161A7796ABCFF/energiebunker-bild.jpg",
  "Eppendorf" = "https://www.hamburg.com/resource/image/19318/landscape_ratio16x9/1240/697/48fc7bd4d1dfc59ed99e517737664bd4/C68014F796D9AAD38BF504D8E4FD2630/km1-eppendorf-bild.jpg",
  "Winterhude" = "https://www.hamburg.com/resource/image/19408/landscape_ratio16x9/1240/697/3640432a14c73d03b6ee851bd645ac12/C6F7A192B89300B819D8294C3BA9E740/km1-winterhude-bars-hamburg.jpg",
  "Wandsbek Markt" = "https://www.hamburg.de/resource/image/279012/landscape_ratio16x9/714/402/97619ad4722bc2bae605bd20bd0d3bc7/C4E95CBA6EE7488AD3F94FF1D27BDA8B/wandsbek-markt-6-.jpg",
  "Barmbek" = "https://www.hamburg.de/resource/image/374492/landscape_ratio16x9/714/402/589f07720bd0e906b2e4e7e34141815c/34E375454454B1D69BD6267B50415D9B/zinnschmelze-bild.jpg" ,
  "Bergedorf" = "https://www.hamburg.com/resource/image/19302/landscape_ratio16x9/1240/697/4c5e0b3a9aaae884cb4b9a1826568f93/74AB851736281E79529BD9C5DB29E930/km1-hamburg-bergedorf.jpg",
  "Eimsbüttel" = "https://quadro.burda-forward.de/ctf/71353ae8-e0cb-42d7-abe1-5e6c673f730c.2ed54cfa-396e-4d1b-b1ee-b8b117d8322e.jpeg?im=RegionOfInterestCrop%3D%281280%2C720%29%2CregionOfInterest%3D%282567.5%2C1444.5%29&hash=a397114c8b683442bc221e7ebb148f21b3561b849d8d1e5e140348324edf5d71"
)

{# --- 7. Create the Leaflet Map ---
map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron)%>%
  setView(lat = 53.55259622749756
          , lng =  9.986599589176762
          , zoom = 11.8)


for (hub_name in names(isochrones_list)) {
  iso_data <- isochrones_list[[hub_name]]
  pal <- colorFactor(palette = palettes[[hub_name]], domain = iso_data$max)
  
  img_url <- hub_images[[hub_name]]
  img_html <- if (!is.null(img_url)) {
    paste0("<img src='", img_url, "' style='width:100%; border-radius:10px; margin-bottom:10px; max-height:150px; object-fit: cover;'>")
  } else {
    ""
  }
  
  # Create HTML popup with embedded fonts
  iso_data$popup_html <- paste0(
    "<div style='
    background-color: #ffffff;
    padding: 15px;
    border-radius: 12px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    font-family: Jost, sans-serif;
    font-size: 13px;
    line-height: 1.4;
    max-width: 250px;
  '>",
    img_html,
    "<div style='
    font-family: \"Bebas Neue\", sans-serif;
    font-size: 20px;
    margin-bottom: 6px;
    line-height: 1.2;
    letter-spacing: 0.5px;
  '>", hub_name, "</div>",
    "<div style='margin-bottom: 4px;'><b>Reachable in:</b> ", iso_data$max, " minutes by bike</div>",
    "</div>"
  )
  
  iso_data$label_text <- paste0(iso_data$isomax, " minutes from ", iso_data$hub_name)
  
  
  map <- map %>%
    addPolygons(
      data = iso_data,
      fillColor = ~pal(isomax),
      color = "white",
      weight = 1,
      fillOpacity = 0.5,
      group = hub_name,
      popup = ~popup_html,
      label = ~label_text
    )
}


# Create popup HTMLs for the markers
hub_marker_popups <- purrr::map_chr(urban_hubs$id, function(hub_name) {
  img_url <- hub_images[[hub_name]]
  img_html <- if (!is.null(img_url)) {
    paste0("<img src='", img_url, "' style='width:100%; border-radius:10px; margin-bottom:10px; max-height:150px; object-fit: cover;'>")
  } else {
    ""
  }
  
  popup_html <- paste0(
    "<div style='
      background-color: #ffffff;
      padding: 15px;
      border-radius: 12px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      font-family: Jost, sans-serif;
      font-size: 13px;
      line-height: 1.4;
      max-width: 250px;
    '>",
    img_html,
    "<div style='
      font-family: \"Bebas Neue\", sans-serif;
      font-size: 20px;
      margin-bottom: 6px;
      line-height: 1.2;
      letter-spacing: 0.5px;
    '>", hub_name, "</div>",
    "<div>Urban hub</div>",
    "</div>"
  )
  return(popup_html)
})

# Add hub markers
map <- map %>%
  addCircleMarkers(
    lng = urban_hubs$lon,
    lat = urban_hubs$lat,
    popup = hub_marker_popups,
    label = urban_hubs$id,
    radius = 3,
    color = "#333",
    stroke = FALSE,
    fillOpacity = 0.5,
    group = "Urban Hubs"
  )

# Add Layer Control
map <- map %>%
  addLayersControl(
    overlayGroups = c(urban_hubs$id, "Urban Hubs"),
    options = layersControlOptions(collapsed = TRUE)
  ) %>%
  setMaxBounds( lng1 = 9.626770
                , lat1 = 53.383328
                , lng2 = 10.351868
                , lat2 = 53.748711 
  )


# --- Create styled HTML info box content ---
info_html <- htmltools::HTML("
  <div style='
    background-color: rgba(255, 255, 255, 0.9);
    padding: 15px;
    border-radius: 12px;
    font-family: \"Jost\", sans-serif;
    font-size: 13px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    max-width: 250px;
    line-height: 1.4;
  '>
    <div style='font-family: \"Bebas Neue\", sans-serif; font-size: 20px; margin-bottom: 8px;'>
      🚶 Hamburg Walk Isochrones
    </div>
    <b>Mode:</b> Walking<br>
    <b>Hubs:</b> 7 Urban Centers<br>
    <b>Time Ranges:</b> 5, 10, 15 minutes<br>
    Use the layer menu to show/hide each hub’s reach.<br>
  </div>
")

# --- Add the info box to the map ---
map <- map %>%
  addControl(html = info_html, position = "bottomleft")


legend_html <- htmltools::HTML("
  <div style='
    background-color: rgba(255,255,255,0.95);
    padding: 15px;
    border-radius: 12px;
    font-family: \"Jost\", sans-serif;
    font-size: 13px;
    max-height: 300px;
    overflow-y: auto;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    max-width: 280px;
    line-height: 1.4;
    margin: 10px;
  '>
    <div style='font-family: \"Bebas Neue\", sans-serif; font-size: 20px; margin-bottom: 10px;'>
      ⏱️ Isochrone Legend
    </div>

    <div style='margin-bottom: 8px;'>
      <b>Time Intervals:</b><br>
      <span style='display:inline-block;width:12px;height:12px;background:#4B75B1;margin-right:6px;border-radius:2px;'></span> 5 min<br>
      <span style='display:inline-block;width:12px;height:12px;background:#69D2E7;margin-right:6px;border-radius:2px;'></span> 10 min<br>
      <span style='display:inline-block;width:12px;height:12px;background:#7BECEA;margin-right:6px;border-radius:2px;'></span> 15 min
    </div>

    <div style='margin-top:12px;'>
      <b>Each hub uses unique colors</b>.<br>
      Use the layer menu to toggle visibility.
    </div>
  </div>
")

map <- map %>%
  addControl(html = legend_html, position = "bottomright")


# -- Add custom HTML and CSS to embed fonts and style the layer control --

font_css <- htmltools::tags$head(
  htmltools::HTML("
    <link href='https://fonts.googleapis.com/css2?family=Bebas+Neue&family=Jost&display=swap' rel='stylesheet'>
    <style>
      .leaflet-control-layers {
        font-family: 'Jost', sans-serif;
        font-size: 13px;
        background-color: rgba(255, 255, 255, 0.9);
        border-radius: 8px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.2);
        padding: 6px 8px;
      }
      .leaflet-popup-content {
        font-family: 'Jost', sans-serif;
        font-size: 14px;
      }
      .leaflet-popup-content h3 {
        font-family: 'Bebas Neue', sans-serif;
        font-size: 20px;
        margin-top: 0;
        margin-bottom: 5px;
      }
    </style>
  ")
)



# Attach custom header with fonts + CSS to the map
map <- htmlwidgets::prependContent(map, font_css)
}

# Print the map
map

htmlwidgets::saveWidget(map, "Widgets/Walkability.html", selfcontained = T)

library(dplyr)
library(sf)
library(units)


# Use this if your isochrones are polygons per hub per time level

summary_by_zone <- purrr::map_dfr(names(isochrones_list), function(hub_name) {
  iso <- isochrones_list[[hub_name]]
  if (nrow(iso) == 0) return(NULL)
  
  iso <- st_make_valid(iso)
  iso_proj <- st_transform(iso, 32632)
  
  hub_index <- which(urban_hubs$id == hub_name)
  hub_point <- st_point(c(urban_hubs$lon[hub_index], urban_hubs$lat[hub_index])) %>%
    st_sfc(crs = 4326) %>%
    st_transform(32632)
  
  result <- iso_proj %>%
    group_by(Time_Min = isomax) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    mutate(
      sample_points = purrr::map(
        geometry,
        ~ st_sample(.x, size = 100, type = "random") %>% st_set_crs(st_crs(hub_point))
      ),
      dist_m = purrr::map(sample_points, ~ st_distance(hub_point, .x) %>% drop_units()),
      Avg_Distance_m = purrr::map_dbl(dist_m, mean),
      Max_Distance_m = purrr::map_dbl(dist_m, max),
      Hub = hub_name
    ) %>%
    select(Hub, Time_Min, Avg_Distance_m, Max_Distance_m)%>%
    st_drop_geometry()
  
  
  return(result)
})

summary_by_zone

readr::write_csv(summary_by_zone, "Data/walk_isochrones_summary.csv")


