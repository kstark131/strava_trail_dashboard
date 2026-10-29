# Trail completion in R and Shiny

An R port of the trail dashboard, split into scripts you can change one at a time.

```
00_config.R                       paths, field names, matching parameters (no secrets)
01_download.R                     Strava auth, activity list, cached full-resolution streams
02_prepare_trails.R               trail and park layers -> segments, sample nodes, park assignment
03_match_runs_to_trails.R         nearest-run distance per node, miles run per park, app bundle
04_static_figures.R               your previous figure script, adapted (optional)
90_export_json_for_html_dashboard.R  writes strava_Run.json for the Python/HTML version (optional)
app/app.R                         the Shiny dashboard
run_all.R                         runs 01, 02, 03 and starts the app
```

## Setup

1. Open `trail_dashboard.Rproj` in RStudio so relative paths resolve.
2. Install packages:
   ```r
   install.packages(c("sf", "lwgeom", "dplyr", "purrr", "stringr", "tidyverse", "rStrava", "httr",
                      "googlePolylines", "RANN", "shiny", "bslib", "leaflet", "DT", "jsonlite"))
   ```
   `bslib` needs version 0.6 or later for `value_box(theme = ...)`.
3. Put the Strava credentials in `.Renviron` (`usethis::edit_r_environ()`), then restart R:
   ```
   STRAVA_APP_NAME=DataImporter
   STRAVA_CLIENT_ID=198319
   STRAVA_CLIENT_SECRET=...
   ```
4. Set `path_trails` and `path_parks` in `00_config.R`.
5. `source("run_all.R")`, or run the scripts in order.

## Changes, October 2026

**Map: plain trails view.** The sidebar has a "Trail colours" choice. "Plain trails" draws every trail in one colour (service roads grey and dashed) with no run or completion data, so you can look at the trail network on its own. "Show my run tracks" is a separate checkbox, off by default. The layer control (top right) also has on/off toggles for trails and park boundaries.

**Map: more basemaps.** Esri topo, Light (Carto Positron), Satellite (Esri imagery), USGS topo, USGS imagery with topo, OpenTopoMap, OpenStreetMap, shaded relief and Dark. None needs an API key. Edit the `BASEMAPS` list near the top of `app/app.R` to add, remove or reorder them; the first one is the default.

**Track resolution.** Three things were simplifying the tracks:

1. `rStrava::get_activity_streams()` always requests `resolution = "high"` with `series_type = "distance"`, which asks Strava to resample the stream rather than return the raw recording. `01_download.R` now calls the streams endpoint directly with no resolution parameter, which returns every point.
2. The old download cached an empty file whenever the rStrava result did not have `lat`/`lng` columns, and those activities silently fell back to the summary polyline. Now only a definite "no GPS" answer is cached; errors are retried on the next run.
3. The app's run layer was simplified by 3 m and leaflet smoothed it further on screen. It is now 1 m (`run_display_simplify_m` in `00_config.R`) with less on-screen smoothing, and run tracks draw above the trails so you can see how they line up.

The stream cache moved to `data/streams_full/`, so the next run of `01_download.R` downloads every activity again at full resolution. At Strava's default read limit that is a few hours for 650 activities; leave it running, or use the bulk GPX export (below) for the backfill. Delete the old `data/streams/` folder afterwards.

`01_download.R` now prints a resolution report: activities per source and the median spacing between points. Full-resolution running tracks are 1 to 5 m apart. `03_match_runs_to_trails.R` prints the same spacing and how many activities use summary polylines.

**Date filter fix.** `filter_start_date <- 2025-11-15` without quotes is the number 1999, which R turned into a date in 1975, so the filter did nothing. It is now quoted, and the download stops with a message if a date is not quoted.

Even at full resolution, a track will usually sit a few metres off the mapped trail: phone and watch GPS is typically accurate to 3 to 10 m, worse under tree cover and in canyons, and the EBRPD centrelines have their own error. That is what the match distance slider is for.

## What changed in the download (earlier)

- Trail matching uses the `latlng` stream, not `map.summary_polyline`. Streams have a point about every second at 6 decimals. The summary polyline is a simplified line.
- Streams are cached one file per activity. Re-running fetches only new activities.
- Strava returns HTTP 429 when the 15-minute limit is reached. The script waits and continues.
- Activities with no stream (treadmill, manual) fall back to the summary polyline and are tagged `source = "summary"`.
- Alternative for the initial backfill: request your archive from Strava (Settings, My Account, Download or Delete Your Account) and drop the GPX files in `gpx/`. `01_download.R` reads them at full device resolution with no API calls. FIT files need converting first.

## How the matching works

Each trail segment is sampled every 10 m (`nodes`). Each run is turned into a line, split at GPS gaps over 200 m, densified to a point every 5 m, and indexed with a KD-tree (`RANN::nn2`). Every node stores the distance to the nearest run point. The app applies the match-distance slider to those stored distances, so changing the threshold is instant and needs no rebuild. A named trail (same name inside the same park) is completed when 90% or more of its length is within the threshold.

With full-resolution streams, 15 m is a sensible default. With summary polylines only, use 30 m.

## The app, and where to start changing it

`app/app.R` is one file. The flow is:

```
inputs (park, thr, svc, view, q)
  -> scope()      trails in scope (park and service-road filter)
  -> cov()        coverage fraction per segment at the current threshold
  -> groups()     one row per named trail
  -> park_rows()  one row per park
  -> value boxes, table, map
```

Every output reads from one of those reactives, so a new filter only has to be added to `scope()` to reach everything. The map is drawn by `draw_trails()`, which takes the colour mode and works on both the first render and later updates. Suggested next changes:

1. Add a date range input. `node_dist` is precomputed against all runs, so the app would need to recompute it for the selected runs: save `run_xy` and `run_pts$activity_id` from `03_match_runs_to_trails.R` into `app_data`, then call `RANN::nn2()` inside a reactive.
2. Add a "Trails to finish" tab: `groups()` filtered to 50 to 89% complete, sorted by remaining miles.
3. Colour the overview map by percent instead of three classes (`leaflet::colorNumeric`).
4. Replace the value boxes with `plotly` charts of completion over time.

## Outputs in `data/`

| file | written by | contents |
| --- | --- | --- |
| activities.rds | 01 | one row per activity, distance in km and miles |
| streams_full/*.rds | 01 | API cache, one file per activity |
| streams.rds | 01 | one row per GPS point: activity_id, lat, lng, time (s), distance (km), altitude, grade_smooth, source |
| trails.rds | 02 | sf LINESTRING per segment, UTM 10N metres, with park, name, type, access, surface, length_m, is_service |
| nodes.rds | 02 | sf POINT per sample node: trail_id, node_i, n_nodes |
| parks.rds | 02 | sf polygon per park, unioned by name |
| node_dist.rds | 03 | trail_id, node_i, dist_m |
| park_runs.rds | 03 | miles run and run count inside each park |
| app_data.rds | 03 | everything the app loads |
