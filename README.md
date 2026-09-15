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
   install.packages(c("sf", "lwgeom", "dplyr", "purrr", "stringr", "tidyverse", "rStrava",
                      "googlePolylines", "RANN", "shiny", "bslib", "leaflet", "DT", "jsonlite"))
   ```
   `bslib` needs version 0.6 or later for `value_box(theme = ...)`.
3. Put the Strava credentials in `.Renviron` (`usethis::edit_r_environ()`), then restart R:
   ```
   STRAVA_APP_NAME=DataImporter
   STRAVA_CLIENT_ID=198319
   STRAVA_CLIENT_SECRET=...
   ```
   The old `00_config.R` had the secret in plain text. Rotate it on the Strava API settings page before using it again.
4. Set `path_trails` and `path_parks` in `00_config.R`.
5. `source("run_all.R")`, or run the scripts in order.

## What changed in the download

- Trail matching now uses the `latlng` stream, not `map.summary_polyline`. Streams have a point about every second at 6 decimals. The summary polyline is a simplified line and the earlier JSON export rounded it further to 4 decimals (jsonlite's default), which put many points 15 to 40 m off the trail. That is what produced the missed trails.
- Streams are cached one file per activity in `data/streams/`. Re-running fetches only new activities.
- Strava returns HTTP 429 when the 15-minute limit is reached. The script waits and continues, so the first full download of 650 activities takes a few 15-minute windows; leave it running.
- Activities with no stream (treadmill, manual) fall back to the summary polyline and are tagged `source = "summary"`.
- Alternative for the initial backfill: request your archive from Strava (Settings, My Account, Download or Delete Your Account) and drop the GPX files in `gpx/`. `01_download.R` reads them at full device resolution with no API calls. FIT files need converting first.

## How the matching works

Same method as the HTML version. Each trail segment is sampled every 10 m (`nodes`). Each run is turned into a line, split at GPS gaps over 200 m, densified to a point every 5 m, and indexed with a KD-tree (`RANN::nn2`). Every node stores the distance to the nearest run point. The app applies the match-distance slider to those stored distances, so changing the threshold is instant and needs no rebuild. A named trail (same name inside the same park) is completed when 90% or more of its length is within the threshold.

With full-resolution streams, 15 m is a sensible default. With summary polylines only, use 30 m.

## The app, and where to start changing it

`app/app.R` is one file. The flow is:

```
inputs (park, thr, svc, q)
  -> scope()      trails in scope (park and service-road filter)
  -> cov()        coverage fraction per segment at the current threshold
  -> groups()     one row per named trail
  -> park_rows()  one row per park
  -> value boxes, table, map
```

Every output reads from one of those reactives, so a new filter only has to be added to `scope()` to reach everything. Suggested first changes, in increasing difficulty:

1. Add a date range input. `node_dist` is precomputed against all runs, so the app would need to recompute it for the selected runs: save `run_xy` and `run_pts$activity_id` from `03_match_runs_to_trails.R` into `app_data`, then call `RANN::nn2()` inside a reactive (250k queries take a second or two).
2. Add a "Trails to finish" tab: `groups()` filtered to 50 to 89% complete, sorted by remaining miles.
3. Colour the overview map by percent instead of three classes (`leaflet::colorNumeric`).
4. Replace the value boxes with `plotly` charts of completion over time.

The map draws whole segments in the overview (fast) and splits segments into run and unrun pieces only when a park is selected (`split_by_coverage()`, which uses `lwgeom::st_linesubstring`).

## Outputs in `data/`

| file | written by | contents |
| --- | --- | --- |
| activities.rds | 01 | one row per activity, distance in km and miles |
| streams.rds | 01 | one row per GPS point: activity_id, lat, lng, time (s), distance (km), altitude, grade_smooth, source |
| trails.rds | 02 | sf LINESTRING per segment, UTM 10N metres, with park, name, type, access, surface, length_m, is_service |
| nodes.rds | 02 | sf POINT per sample node: trail_id, node_i, n_nodes |
| parks.rds | 02 | sf polygon per park, unioned by name |
| node_dist.rds | 03 | trail_id, node_i, dist_m |
| park_runs.rds | 03 | miles run and run count inside each park |
| app_data.rds | 03 | everything the app loads |

## Caveats

These scripts were written without an R session available, so run them once and expect small fixes (a column name that differs in your rStrava version, for example). The two places most likely to need a look are the column names returned by `get_activity_streams()` in `01_download.R` (expected: `lat`, `lng`, `time`, `distance`, `altitude`, `grade_smooth`) and the stream distance unit check, which warns if the stream does not look like km.
