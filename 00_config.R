# ==============================================================================
# SCRIPT 0: CONFIGURATION
# ==============================================================================
# Everything the other scripts read comes from here. Edit this file, not them.
# Run order: 01_download.R -> 02_prepare_trails.R -> 03_match_runs_to_trails.R
#            -> app/app.R (Shiny). 04_static_figures.R is optional.

# --- DIRECTORIES ---------------------------------------------------------------
# With an RStudio project open, getwd() is the project folder and relative paths
# work on any machine. Override dir_project if you keep scripts elsewhere.
dir_project <- getwd()
dir_data    <- file.path(dir_project, "data")           # RDS outputs of each script
# One RDS per activity (API cache). The folder name changed from "streams" to
# "streams_full" when the download switched to full-resolution requests, so old
# (possibly downsampled) cache files are ignored and fetched again. Delete
# data/streams/ once the new download has finished.
dir_streams <- file.path(dir_data, "streams_full")
dir_figures <- file.path(dir_project, "figures")
dir_gpx     <- file.path(dir_project, "gpx")            # optional: Strava bulk export files

# --- TRAIL DATA SOURCE ----------------------------------------------------------
# "file": your own trail and park layers (path_trails / path_parks below).
# "osm" : 02a_fetch_osm.R downloads trails inside parks and protected areas
#         from OpenStreetMap for the small box in osm_bbox (see OPENSTREETMAP
#         below) and points path_trails / path_parks at the result.
trail_source <- "file"

# Trail and park layers. Shapefile (.shp), a shapefile folder, or GeoJSON all work.
# EBRPD publishes these on its GIS Open Data hub (linked from ebparks.org/maps).
# The copies in EBRPD/ are used by default, so no machine-specific path is needed.
path_parks  <- file.path(dir_project, "EBRPD", "EBRPD_Park_Boundaries_-7891795993450239876")
path_trails <- file.path(dir_project, "EBRPD", "EBRPD_Roads_and_Trails_-1064327229162888676")

# Optional city boundary, used only by 04_static_figures.R.
path_city_boundary <- "C:/Users/kstar/Desktop/strava/California_Incorporated_Cities/"
target_city        <- "Berkeley"

# --- FIELD NAMES IN THE TRAIL AND PARK LAYERS ---------------------------------
# Defaults match the EBRPD schema. Change these to use another agency's layers.
trail_name_fields   <- c("LOCALNAME", "REGNAME_1", "REGNAME_2", "REGNAME_3", "REGNAME_4")
trail_park_field    <- "PARK_NAME"
trail_type_field    <- "TYPE_NEW"
trail_access_field  <- "ACCESS"
trail_surface_field <- "SURFACE"
park_name_field     <- "NAME"
park_status_field   <- "STATUS"
park_acres_field    <- "GIS_ACRES"

# --- PROJECTIONS ---------------------------------------------------------------
crs_global    <- 4326   # WGS84 lat/lon, what Strava and leaflet use
crs_projected <- 26910  # NAD83 / UTM zone 10N, metres. All distance work happens here.
                        # Elsewhere, use your UTM zone (trail_source = "osm" picks it automatically).

# --- ACTIVITY FILTERS ----------------------------------------------------------
target_activities <- c("Run")            # e.g. c("Run", "Hike", "Walk")
# Dates must be quoted strings. Unquoted, 2025-11-15 is arithmetic (= 1999) and
# R reads it as a date in 1975, so the filter silently did nothing.
filter_start_date <- "2025-11-15"        # "" = no limit
filter_end_date   <- ""                  # "" = today
min_distance_km   <- 0.5                 # drops accidental recordings
ignore_keywords   <- c("treadmill", "indoor")

# --- MATCHING PARAMETERS -------------------------------------------------------
node_spacing_m   <- 10    # trail sample spacing. Each sample is one "node".
run_densify_m    <- 5     # run tracks get a point at least this often
run_gap_m        <- 200   # a jump longer than this between stream points is a GPS gap
max_node_dist_m  <- 250   # nearest-run distance is capped here (keeps the table small)
default_match_m  <- 15    # slider default in the app. 15 m suits full-res streams;
                          # use 30 m if you only have summary polylines.
done_fraction    <- 0.90  # a trail is "completed" at this share of nodes run (CityStrides uses 90%)

# --- MAP DISPLAY ---------------------------------------------------------------
# Run tracks on the app map are simplified by this many metres to keep the map
# responsive with full-resolution streams. Matching always uses unsimplified
# data. 0 = no simplification (slow with hundreds of runs); 1 m is invisible
# at any zoom level.
run_display_simplify_m <- 1

# --- STRAVA API ----------------------------------------------------------------
# Never put the secret in this file. Add these three lines to your .Renviron
# (usethis::edit_r_environ()), then restart R:
#   STRAVA_APP_NAME=DataImporter
#   STRAVA_CLIENT_ID=198319
#   STRAVA_CLIENT_SECRET=your_new_secret
# Rotate the secret on strava.com/settings/api if it has ever been committed or shared.
app_name      <- Sys.getenv("STRAVA_APP_NAME", "DataImporter")
app_client_id <- Sys.getenv("STRAVA_CLIENT_ID")
app_secret    <- Sys.getenv("STRAVA_CLIENT_SECRET")

# Strava rate limits are per app and shown on your API settings page. The download
# script pauses when it hits the 15-minute limit and resumes automatically.
api_pause_sec        <- 1.0    # wait between stream requests
api_limit_wait_sec   <- 15 * 60

# --- ROUTE SUGGESTIONS (app) ----------------------------------------------------
# The app's "Suggest a route" option strings unrun trail together into a loop.
# Trail ends within route_snap_m of another trail count as connected; GIS layers
# rarely share exact end points. Raise it if routes stop at obvious junctions.
route_snap_m       <- 15
route_default_mi   <- 5       # starting value of the length slider
route_max_mi       <- 20      # slider maximum

# --- OPENSTREETMAP (trail_source = "osm") ----------------------------------------
# Only this box is downloaded, in tiles, one request at a time, and cached in
# data/osm/. The public Overpass servers are shared and run on donations:
# keep the box to the parks you actually run in.
osm_bbox           <- c(xmin = -122.27, ymin = 37.86, xmax = -122.22, ymax = 37.93)  # lon/lat (Tilden)
osm_park_pattern   <- ""      # regex on park name, e.g. "Tilden|Wildcat"; "" = every park in the box
osm_min_park_acres <- 20      # drops pocket parks and playgrounds
osm_max_area_km2   <- 250     # the script refuses a bigger box
osm_tile_deg       <- 0.05    # tile size in degrees (about 5 km)
osm_cache_days     <- 90      # tiles younger than this are not downloaded again
osm_pause_sec      <- 5       # wait between tile requests
osm_overpass_url   <- "https://overpass-api.de/api/interpreter"

# --- OPTIONAL OUTPUTS ----------------------------------------------------------
upload_to_gdrive <- FALSE      # 04_static_figures.R only

# --- DERIVED SETTINGS ---------------------------------------------------------
if (trail_source == "osm") {
  # 02a_fetch_osm.R writes these files with OSM tag names as fields
  path_trails <- file.path(dir_data, "osm", "trails.geojson")
  path_parks  <- file.path(dir_data, "osm", "parks.geojson")
  trail_name_fields   <- "name"
  trail_park_field    <- ""            # parks are assigned spatially in 02
  trail_type_field    <- "type"
  trail_access_field  <- "access"
  trail_surface_field <- "surface"
  park_name_field     <- "name"
  park_status_field   <- "status"
  park_acres_field    <- "acres"
  # OSM can be anywhere, so use the WGS84 UTM zone of the box instead of 10N
  crs_projected <- 32600 + floor((mean(osm_bbox[c("xmin", "xmax")]) + 180) / 6) + 1 +
                   ifelse(mean(osm_bbox[c("ymin", "ymax")]) < 0, 100, 0)
}

for (d in c(dir_data, dir_streams, dir_figures)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
