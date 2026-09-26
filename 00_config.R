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
dir_streams <- file.path(dir_data, "streams")           # one RDS per activity (API cache)
dir_figures <- file.path(dir_project, "figures")
dir_gpx     <- file.path(dir_project, "gpx")            # optional: Strava bulk export files

# Trail and park layers. Shapefile (.shp) or GeoJSON (.geojson) both work.
# EBRPD publishes these on its GIS Open Data hub (linked from ebparks.org/maps).
path_trails <- "C:/Users/kstar/Desktop/strava/EBRPD_Roads_and_Trails_2536103029614549705/Roads_and_Trails-by_Access.shp"
path_parks  <- "C:/Users/kstar/Desktop/strava/EBRPD_Park_Boundaries_-5406634565931984723/Ebrparkp_m.shp"

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

# --- ACTIVITY FILTERS ----------------------------------------------------------
target_activities <- c("Run")            # e.g. c("Run", "Hike", "Walk")
filter_start_date <- 2025-11-15                  # "" = no limit, or "2024-01-01"
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

# --- OPTIONAL OUTPUTS ----------------------------------------------------------
upload_to_gdrive <- FALSE      # 04_static_figures.R only

for (d in c(dir_data, dir_streams, dir_figures)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
