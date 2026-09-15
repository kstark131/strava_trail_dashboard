# ==============================================================================
# SCRIPT 1: DOWNLOAD FROM STRAVA
# ==============================================================================
# What changed versus the earlier version and why:
#  * Trail matching now uses the full-resolution "latlng" stream (a point every
#    second or so, 6 decimal places) instead of map.summary_polyline, which is a
#    simplified line. The summary polyline is kept only as a fallback.
#  * Streams are cached one file per activity in data/streams/. Re-running the
#    script only fetches activities you have not downloaded yet, so a normal
#    refresh is a handful of requests instead of hundreds.
#  * Rate limits are handled: the script waits for the next 15-minute window
#    when Strava returns 429 and then continues.
#  * Coordinates are never rounded. If you export to JSON, use digits = NA
#    (jsonlite rounds to 4 decimals by default, which is an 8 to 11 m error).
#
# Output: data/activities.rds (one row per activity)
#         data/streams.rds    (one row per GPS point, all activities)

library(sf)
library(dplyr)
library(purrr)
library(rStrava)
library(googlePolylines)

source("00_config.R")

if (app_client_id == "" || app_secret == "") {
  stop("STRAVA_CLIENT_ID / STRAVA_CLIENT_SECRET are not set. See 00_config.R for the .Renviron lines.")
}

# ------------------------------------------------------------------------------
# 1. AUTH
# ------------------------------------------------------------------------------
# strava_oauth caches the token in .httr-oauth in the project folder. Add that
# file to .gitignore.
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret,
                                            app_scope = "activity:read_all", cache = TRUE))

# ------------------------------------------------------------------------------
# 2. ACTIVITY LIST
# ------------------------------------------------------------------------------
apply_activity_filters <- function(df) {
  if (nzchar(filter_start_date)) df <- filter(df, as.Date(start_date_local) >= as.Date(filter_start_date))
  if (nzchar(filter_end_date))   df <- filter(df, as.Date(start_date_local) <= as.Date(filter_end_date))
  if (length(target_activities)) df <- filter(df, type %in% target_activities)
  df <- filter(df, distance >= min_distance_km)
  kw <- ignore_keywords[nzchar(ignore_keywords)]
  if (length(kw)) df <- filter(df, !grepl(paste(kw, collapse = "|"), name, ignore.case = TRUE))
  if ("manual" %in% names(df)) df <- filter(df, !as.logical(manual))   # manual entries have no GPS
  df
}

my_acts <- get_activity_list(stoken)

activities <- compile_activities(my_acts, units = "metric") %>%   # distance in km, elevation in m
  mutate(id = as.character(id)) %>%
  apply_activity_filters() %>%
  mutate(date = as.Date(start_date_local),
         distance_mi = distance / 1.609344) %>%
  arrange(desc(start_date_local))

message(nrow(activities), " activities after filters; ", sum(activities$distance_mi), " mi total")

# ------------------------------------------------------------------------------
# 3. STREAMS (cached, incremental, rate-limit aware)
# ------------------------------------------------------------------------------
stream_types <- c("latlng", "time", "distance", "altitude", "grade_smooth")

fetch_stream <- function(act_id) {
  f <- file.path(dir_streams, paste0(act_id, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  for (attempt in 1:3) {
    res <- tryCatch(
      get_activity_streams(my_acts, stoken, id = act_id, types = stream_types),
      error = function(e) e
    )
    if (!inherits(res, "error")) {
      if (is.null(res) || nrow(res) == 0 || !all(c("lat", "lng") %in% names(res))) {
        res <- data.frame()                       # no GPS (treadmill etc.)
      }
      res <- as.data.frame(res)
      if (nrow(res)) res$activity_id <- as.character(act_id)
      saveRDS(res, f)
      Sys.sleep(api_pause_sec)
      return(res)
    }
    msg <- conditionMessage(res)
    if (grepl("429|Too Many Requests", msg)) {
      message("Rate limit hit at ", format(Sys.time(), "%H:%M"), ". Waiting ", api_limit_wait_sec / 60, " min.")
      Sys.sleep(api_limit_wait_sec)
    } else {
      message("Activity ", act_id, ": ", msg)
      Sys.sleep(5)
    }
  }
  NULL
}

todo <- setdiff(activities$id, sub("\\.rds$", "", list.files(dir_streams, pattern = "\\.rds$")))
message(length(todo), " activities to download, ", nrow(activities) - length(todo), " already cached")

stream_list <- lapply(activities$id, fetch_stream)
streams_raw <- bind_rows(stream_list)
expected_cols <- c("activity_id", "lat", "lng", "time", "distance", "altitude", "grade_smooth")
for (col in setdiff(expected_cols, names(streams_raw))) {
  streams_raw[[col]] <- rep(if (col == "activity_id") NA_character_ else NA_real_, nrow(streams_raw))
}

# Strava returns "time" as seconds since start and (in rStrava) distance in km.
# Verify once: the last stream distance should be close to the activity distance.
if (nrow(streams_raw) > 0) {
  chk <- streams_raw %>% group_by(activity_id) %>% summarise(stream_km = max(distance, na.rm = TRUE), .groups = "drop") %>%
    inner_join(select(activities, id, distance), by = c("activity_id" = "id"))
  ratio <- median(chk$stream_km / chk$distance, na.rm = TRUE)
  if (is.finite(ratio) && (ratio > 5 || ratio < 0.2)) {
    warning("Stream distance does not look like km (median ratio to activity km = ", round(ratio, 2), "). Check units.")
  }
}

streams <- streams_raw %>%
  filter(!is.na(lat), !is.na(lng)) %>%
  mutate(source = "stream") %>%
  arrange(activity_id, time)

# ------------------------------------------------------------------------------
# 4. FALLBACK: summary polyline for activities with no usable stream
# ------------------------------------------------------------------------------
missing_ids <- setdiff(activities$id, unique(streams$activity_id))
if (length(missing_ids)) {
  fallback <- activities %>%
    filter(id %in% missing_ids, !is.na(map.summary_polyline), nzchar(map.summary_polyline)) %>%
    select(id, map.summary_polyline) %>%
    pmap_dfr(function(id, map.summary_polyline) {
      xy <- googlePolylines::decode(map.summary_polyline)[[1]]
      if (is.null(xy) || nrow(xy) < 2) return(NULL)
      data.frame(activity_id = id, lat = xy$lat, lng = xy$lon, time = seq_len(nrow(xy)),
                 distance = NA_real_, altitude = NA_real_, grade_smooth = NA_real_, source = "summary")
    })
  message(length(unique(fallback$activity_id)), " activities use the summary polyline (no stream available)")
  streams <- bind_rows(streams, fallback) %>% arrange(activity_id, time)
}

# ------------------------------------------------------------------------------
# 5. OPTIONAL: Strava bulk export (GPX files), no API needed
# ------------------------------------------------------------------------------
# Settings > My Account > Download or Delete Your Account > Request Your Archive.
# Put the GPX files in gpx/ and this reads them at full device resolution.
# FIT/TCX files need conversion first (e.g. gpsbabel) or the FITfileR package.
load_gpx_dir <- function(dir) {
  files <- list.files(dir, pattern = "\\.gpx$", full.names = TRUE, ignore.case = TRUE)
  map_dfr(files, function(f) {
    pts <- tryCatch(st_read(f, layer = "track_points", quiet = TRUE), error = function(e) NULL)
    if (is.null(pts) || nrow(pts) < 2) return(NULL)
    xy <- st_coordinates(pts)
    data.frame(activity_id = tools::file_path_sans_ext(basename(f)), lat = xy[, 2], lng = xy[, 1],
               time = seq_len(nrow(pts)), distance = NA_real_,
               altitude = if ("ele" %in% names(pts)) pts$ele else NA_real_,
               grade_smooth = NA_real_, source = "gpx")
  })
}
if (dir.exists(dir_gpx) && length(list.files(dir_gpx, pattern = "\\.gpx$", ignore.case = TRUE))) {
  gpx <- load_gpx_dir(dir_gpx)
  message(length(unique(gpx$activity_id)), " GPX files loaded")
  # GPX activity ids are file names, so they are kept separate from API ids.
  streams <- bind_rows(streams, gpx)
}

# ------------------------------------------------------------------------------
# 6. SAVE
# ------------------------------------------------------------------------------
saveRDS(activities, file.path(dir_data, "activities.rds"))
saveRDS(streams,    file.path(dir_data, "streams.rds"))
message("Saved ", nrow(streams), " GPS points from ", length(unique(streams$activity_id)), " activities")
