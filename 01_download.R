# ==============================================================================
# SCRIPT 1: DOWNLOAD FROM STRAVA
# ==============================================================================

library(sf)
library(dplyr)
library(purrr)
library(rStrava)
library(httr)
library(jsonlite)
library(googlePolylines)

source("00_config.R")

if (app_client_id == "" || app_secret == "") {
  stop("STRAVA_CLIENT_ID / STRAVA_CLIENT_SECRET are not set. See 00_config.R for the .Renviron lines.")
}

# ------------------------------------------------------------------------------
# 1. AUTH
# ------------------------------------------------------------------------------
# strava_oauth caches the token in .httr-oauth in the project folder. Add that
# file to .gitignore. httr refreshes the token automatically when it expires.
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret,
                                            app_scope = "activity:read_all", cache = TRUE))

# ------------------------------------------------------------------------------
# 2. ACTIVITY LIST
# ------------------------------------------------------------------------------
apply_activity_filters <- function(df) {
  if (!is.character(filter_start_date) || !is.character(filter_end_date)) {
    stop("filter_start_date and filter_end_date in 00_config.R must be quoted, e.g. \"2025-11-15\".")
  }
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

message(nrow(activities), " activities after filters; ", round(sum(activities$distance_mi)), " mi total")

# ------------------------------------------------------------------------------
# 3. STREAMS (full resolution, cached, incremental, rate-limit aware)
# ------------------------------------------------------------------------------
stream_types <- c("latlng", "time", "distance", "altitude", "grade_smooth")

# One request to GET /activities/{id}/streams. Returns a data frame (possibly
# with zero rows when the activity has no GPS), or signals an error.
request_stream <- function(act_id) {
  resp <- httr::GET(
    sprintf("https://www.strava.com/api/v3/activities/%s/streams", act_id),
    stoken,
    query = list(keys = paste(stream_types, collapse = ","), key_by_type = "true")
  )
  status <- httr::status_code(resp)
  if (status == 429) stop("429 Too Many Requests")
  if (status == 404) return(data.frame())                     # no streams for this activity
  if (status >= 400) stop("HTTP ", status, ": ", httr::content(resp, as = "text", encoding = "UTF-8"))

  s <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), simplifyVector = TRUE)
  ll <- s$latlng$data                                          # n x 2 matrix of lat, lng
  if (is.null(ll) || length(ll) == 0) return(data.frame())    # treadmill, manual, GPS off
  if (!is.matrix(ll)) ll <- do.call(rbind, ll)
  n <- nrow(ll)

  col <- function(k, scale = 1) {
    v <- s[[k]]$data
    if (is.null(v) || length(v) != n) rep(NA_real_, n) else as.numeric(v) * scale
  }
  out <- data.frame(
    activity_id  = as.character(act_id),
    lat          = ll[, 1],
    lng          = ll[, 2],
    time         = col("time"),               # seconds since start
    distance     = col("distance", 1 / 1000), # API returns metres; stored in km as before
    altitude     = col("altitude"),
    grade_smooth = col("grade_smooth")
  )
  # What Strava says it sent: "high" with original_size == n means every point.
  attr(out, "resolution")    <- s$latlng$resolution
  attr(out, "original_size") <- s$latlng$original_size
  out
}

fetch_stream <- function(act_id) {
  f <- file.path(dir_streams, paste0(act_id, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  for (attempt in 1:3) {
    res <- tryCatch(request_stream(act_id), error = function(e) e)
    if (!inherits(res, "error")) {
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
  NULL                                           # not cached, so it is retried next run
}

todo <- setdiff(activities$id, sub("\\.rds$", "", list.files(dir_streams, pattern = "\\.rds$")))
message(length(todo), " activities to download, ", nrow(activities) - length(todo), " already cached")

stream_list <- lapply(activities$id, fetch_stream)
streams_raw <- bind_rows(stream_list)
expected_cols <- c("activity_id", "lat", "lng", "time", "distance", "altitude", "grade_smooth")
for (col in setdiff(expected_cols, names(streams_raw))) {
  streams_raw[[col]] <- rep(if (col == "activity_id") NA_character_ else NA_real_, nrow(streams_raw))
}

# Sanity check on units: the last stream distance should be close to the activity distance.
if (nrow(streams_raw) > 0) {
  chk <- streams_raw %>% group_by(activity_id) %>%
    summarise(stream_km = suppressWarnings(max(distance, na.rm = TRUE)), .groups = "drop") %>%
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
# 6. RESOLUTION REPORT
# ------------------------------------------------------------------------------
# Full-resolution running tracks have a point every 1 to 5 m. Summary polylines
# and resampled streams are 20 m or more.
xy_m <- streams %>%
  st_as_sf(coords = c("lng", "lat"), crs = crs_global) %>%
  st_transform(crs_projected) %>%
  st_coordinates()
spacing <- streams %>%
  select(activity_id, source) %>%
  mutate(x = xy_m[, 1], y = xy_m[, 2]) %>%
  group_by(activity_id, source) %>%
  summarise(n_pts = n(),
            spacing_m = median(sqrt(diff(x)^2 + diff(y)^2)),
            .groups = "drop")
rep_tbl <- spacing %>%
  group_by(source) %>%
  summarise(activities = n(), median_points = median(n_pts), median_spacing_m = round(median(spacing_m, na.rm = TRUE), 1),
            .groups = "drop")
message("Track resolution by source:")
print(rep_tbl)
coarse <- filter(spacing, source == "stream", spacing_m > 10)
if (nrow(coarse)) {
  message(nrow(coarse), " stream activities have a median point spacing over 10 m. ",
          "Delete their files in data/streams_full/ and re-run to fetch them again.")
}

# ------------------------------------------------------------------------------
# 7. SAVE
# ------------------------------------------------------------------------------
saveRDS(activities, file.path(dir_data, "activities.rds"))
saveRDS(streams,    file.path(dir_data, "streams.rds"))
message("Saved ", nrow(streams), " GPS points from ", length(unique(streams$activity_id)), " activities")
