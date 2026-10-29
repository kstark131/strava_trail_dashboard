# Runs the pipeline end to end. Each script can also be run on its own.
# Steps 2a and 2 are skipped when their outputs are newer than their inputs,
# so a normal refresh is: download new runs, re-match, open the app.
source("00_config.R")

# TRUE when `output` is missing or older than any file in `inputs` (folders are searched)
outdated <- function(output, inputs) {
  files <- unlist(lapply(inputs, function(p) if (dir.exists(p)) list.files(p, full.names = TRUE, recursive = TRUE) else p))
  files <- files[file.exists(files)]
  !file.exists(output) || any(file.mtime(files) > file.mtime(output))
}

source("01_download.R")                            # only new activities are fetched
# OSM: re-run when the settings change. To pick up recent OSM edits, delete
# data/osm/ (or run 02a_fetch_osm.R yourself, which refreshes tiles older than osm_cache_days).
if (trail_source == "osm" && outdated(path_trails, c("00_config.R", "02a_fetch_osm.R"))) source("02a_fetch_osm.R")
if (outdated(file.path(dir_data, "trails.rds"), c(path_trails, path_parks, "02_prepare_trails.R", "00_config.R"))) {
  source("02_prepare_trails.R")
} else message("Trail and park layers unchanged, skipping 02_prepare_trails.R")
source("03_match_runs_to_trails.R")
shiny::runApp("app")
