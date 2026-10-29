# Trail completion in R and Shiny

An R port of the trail dashboard, split into scripts you can change one at a time.

```
00_config.R                       paths, field names, matching parameters (no secrets)
01_download.R                     Strava auth, activity list, cached full-resolution streams
02a_fetch_osm.R                   optional: trails and parks from OpenStreetMap instead of your own layers
02_prepare_trails.R               trail and park layers -> segments, sample nodes, park assignment
03_match_runs_to_trails.R         nearest-run distance per node, miles run per park, app bundle
04_static_figures.R               your previous figure script, adapted (optional)
90_export_json_for_html_dashboard.R  writes strava_Run.json for the Python/HTML version (optional)
app/app.R                         the Shiny dashboard
app/route_suggest.R               route suggestions (sourced by app.R)
run_all.R                         runs the pipeline (skipping unchanged steps) and starts the app
docs/HOSTING.md                   options and a roadmap for putting the app online
```

## Setup

1. Open `strava_trail_dashboard.Rproj` in RStudio so relative paths resolve.
2. Install packages:
   ```r
   install.packages(c("sf", "lwgeom", "dplyr", "purrr", "stringr", "tidyverse", "rStrava", "httr",
                      "googlePolylines", "RANN", "igraph", "shiny", "bslib", "leaflet", "DT", "jsonlite"))
   ```
   `bslib` needs version 0.6 or later for `value_box(theme = ...)`.
3. Put the Strava credentials in `.Renviron` (`usethis::edit_r_environ()`), then restart R:
   ```
   STRAVA_APP_NAME=DataImporter
   STRAVA_CLIENT_ID=198319
   STRAVA_CLIENT_SECRET=...
   ```
4. The EBRPD layers in `EBRPD/` are used by default. To use other layers, set
   `path_trails` and `path_parks` in `00_config.R`; to use OpenStreetMap, see below.
5. `source("run_all.R")`, or run the scripts in order. `run_all.R` skips
   `02_prepare_trails.R` when the layers and config have not changed since it last ran.

## Route suggestions

Open "Route suggestion" in the sidebar, choose a park and tick "Suggest a route
through unrun trail" (it is off by default). Set the target length with the
slider; "Loop back to the start" is on by default, and off gives a one-way route.

The route starts where most of your runs in that park began. Click the map to
start somewhere else. The sidebar lists the trails in order, with how much of
each is new, and "GPX" downloads the route for a watch or phone app.

How it works (`app/route_suggest.R`): the park's trails become a graph whose
vertices are the same 10 m sample nodes used for matching, so partly run
trails are split exactly where your coverage stops. Trail ends within
`route_snap_m` (15 m) of another trail are joined, because GIS layers rarely
share exact end points. From the start, the route repeatedly takes the unrun
stretch with the most new distance per metre walked, as long as it can still
get back within the target length. It is a quick heuristic, not an optimum,
and it only knows what the trail layer knows: check closures and one-way or
bike-only rules before you go. It respects the match distance and the
service-road setting, and runs in well under a second for the largest EBRPD parks.

## Trails from OpenStreetMap

Set `trail_source <- "osm"` in `00_config.R` and set `osm_bbox` to a box
around the parks you run in. `02a_fetch_osm.R` downloads named parks, nature
reserves and protected areas in that box, and the paths, footways, tracks,
bridleways and steps inside them, then writes GeoJSON files that
`02_prepare_trails.R` reads as usual. Roads, sidewalks, crossings, private
land and parks under `osm_min_park_acres` are left out.

To keep the load on the shared Overpass servers small, the box is capped at
`osm_max_area_km2` (250 km2), split into tiles, requested one at a time with a
pause, and cached in `data/osm/cache/` for `osm_cache_days`. Re-running uses
the cache. `osm_park_pattern` narrows the request to parks whose name matches,
e.g. `"Tilden|Wildcat"`. The projection switches to the box's UTM zone
automatically, so OSM works outside the Bay Area too. OSM trail names and park
boundaries are less consistent than an agency layer, and many paths have no
name ("Unnamed trail"). The app credits OpenStreetMap when this source is used.

## Look and layout, October 2026

Warmer page and sidebar colours, the park name next to the title, a progress
bar in the "Completed" box, and a bar behind each percentage in the table.
Map options and route suggestions are folded into an accordion so the stats
and table stay near the top. The CSS is one short block (`CSS`) near the top
of `app/app.R`.

## Putting it online

Not implemented yet. `docs/HOSTING.md` compares the options (Tailscale,
Cloudflare Tunnel + Access, shinyapps.io / Posit Connect Cloud, a small VPS)
and lays out a phased plan, starting with a privacy zone around home.

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

inputs (park, thr, svc, route, route_mi, route_loop, map click)
  -> route()      suggested route (route_suggest.R), drawn in the "route" layer
```

Every output reads from one of those reactives, so a new filter only has to be added to `scope()` to reach everything. The map is drawn by `draw_trails()`, which takes the colour mode and works on both the first render and later updates. Suggested next changes:

1. Add a date range input. `node_dist` is precomputed against all runs, so the app would need to recompute it for the selected runs: save `run_xy` and `run_pts$activity_id` from `03_match_runs_to_trails.R` into `app_data`, then call `RANN::nn2()` inside a reactive.
2. Add a "Trails to finish" tab: `groups()` filtered to 50 to 89% complete, sorted by remaining miles.
   (Route suggestions now cover the map side of this.)
3. Colour the overview map by percent instead of three classes (`leaflet::colorNumeric`).
4. Replace the value boxes with `plotly` charts of completion over time.

## Outputs in `data/`

| file | written by | contents |
| --- | --- | --- |
| activities.rds | 01 | one row per activity, distance in km and miles |
| streams_full/*.rds | 01 | API cache, one file per activity |
| streams.rds | 01 | one row per GPS point: activity_id, lat, lng, time (s), distance (km), altitude, grade_smooth, source |
| osm/trails.geojson, osm/parks.geojson | 02a | OSM trails and parks (trail_source = "osm"); osm/cache/ holds the raw tile responses |
| trails.rds | 02 | sf LINESTRING per segment, UTM 10N metres, with park, name, type, access, surface, length_m, is_service |
| nodes.rds | 02 | sf POINT per sample node: trail_id, node_i, n_nodes |
| parks.rds | 02 | sf polygon per park, unioned by name |
| node_dist.rds | 03 | trail_id, node_i, dist_m |
| park_runs.rds | 03 | miles run and run count inside each park |
| app_data.rds | 03 | everything the app loads |
