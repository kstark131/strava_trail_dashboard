# ==============================================================================
# SCRIPT 90 (optional): EXPORT RUNS FOR THE PYTHON/HTML DASHBOARD
# ==============================================================================
# Writes strava_Run.json in the format build_dashboard.py expects, using the
# full-resolution streams instead of summary polylines. digits = NA keeps all
# decimal places; the default (4) is what degraded the earlier export.
library(dplyr)
library(purrr)
library(jsonlite)
source("00_config.R")

activities <- readRDS(file.path(dir_data, "activities.rds"))
streams    <- readRDS(file.path(dir_data, "streams.rds"))

coords <- streams %>%
  arrange(activity_id, time) %>%
  group_by(activity_id) %>%
  summarise(coordinates = list(unname(as.matrix(cbind(lat, lng)))), .groups = "drop")

out <- activities %>%
  transmute(id = id, name = name, type = type,
            distance = distance,                       # km
            moving_time = moving_time, elapsed_time = elapsed_time,
            start_date = start_date, total_elevation_gain = total_elevation_gain,
            average_speed = average_speed) %>%
  left_join(coords, by = c("id" = "activity_id")) %>%
  mutate(coordinates = map(coordinates, ~ if (is.null(.x)) matrix(numeric(0), ncol = 2) else .x))

write_json(out, file.path(dir_data, "strava_Run.json"), digits = NA, auto_unbox = TRUE, pretty = FALSE)
message("Wrote ", file.path(dir_data, "strava_Run.json"))
