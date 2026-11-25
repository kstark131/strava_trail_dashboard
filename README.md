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
app/exclusions.R                  exclusion rules and your saved edits (sourced by app.R)
app/exclusions.geojson            trails and areas you excluded by hand (created when you first save)
deploy_connect_cloud.R            publishes the app to Posit Connect Cloud behind a login
run_all.R                         runs the pipeline (skipping unchanged steps) and starts the app
docs/HOSTING.md                   options and a roadmap for putting the app online
```

## Setup

1. Open `strava_trail_dashboard.Rproj` in RStudio so relative paths resolve.
2. Install packages:
   ```r
   install.packages(c("sf", "dplyr", "purrr", "stringr", "tidyverse", "rStrava", "httr",
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

## Excluding private and closed trails

Open "Exclusions" in the sidebar. Excluded trails leave every total, the table
and the CSV, and are drawn grey and dashed (with their own switch in the layer
control).

**Rules** use the trail layer's own data. Each has a switch, and the ones in
`exclude_rules_default` (`00_config.R`) start on. Miles are for the EBRPD layers,
service roads not counted:

| rule | what it matches | miles | on by default |
| --- | --- | --- | --- |
| Land not open to the public | segment lies only on park land with STATUS "Landbank" | 30 | yes |
| Notes say closed / golf course | name or NOTES match `rule_closed_pattern` | ~10 | yes |
| Notes say proposed / future trail | `rule_proposed_pattern` | ~10 | yes |
| Paved roads | TYPE_NEW in `rule_road_types` | 60 | no |
| Outside park boundaries | no park polygon contains the segment | 253 | no |

**Your own edits.** Choose a park and tick "Review and edit on the map".
Trails become thick and tappable (green run, red not run, grey excluded):

- Tap a trail to exclude it with the chosen reason; tap a grey one to bring it
  back, including ones a rule excluded. Tapping again undoes the tap.
- Choose "outline an area", tap points around private land or a golf course,
  then "Finish area": segments at least half inside are excluded.
- "Undo last" steps back; "Save" writes `app/exclusions.geojson`.

Segments are saved by the layer's own feature id (`trail_uid_field`, EBRPD's
GlobalID), so edits survive a fresh download of the trail layer. Commit the
file to keep it. Route suggestions never use trails excluded by hand, on closed
land, or marked closed or proposed; paved roads and trails outside parks stay
usable as connectors.

## Online with Posit Connect Cloud

`deploy_connect_cloud.R` publishes the app following Posit's
[command-line method](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html).
Posit is moving shinyapps.io into Connect Cloud (free shinyapps.io apps are
migrated automatically from January 2027), so this replaces the earlier
shinyapps.io script.

One-time setup (details at the top of the script):

```r
install.packages(c("rsconnect", "shinymanager"))
rsconnect::connectCloudUser()       # browser opens; authorize
```

and set `privacy_zones` in `00_config.R`: run tracks within `privacy_radius_m`
of those points are cut from the online copy. Then, after each refresh:

```r
source("run_all.R")                 # or just 01 and 03
source("deploy_connect_cloud.R")
```

After the first publish, add `APP_USER` and `APP_PASSWORD` under **Settings >
Variables** for the content on Connect Cloud and restart it. The password is
stored only there. On the free plan every app is public and listed on your
profile page, so the app's own login screen is what keeps it private; until the
password is set, the hosted app shows only a "login not configured" page.

The upload holds the app code, your exclusions and a trimmed copy of
`app_data.rds` (no run names or dates) written to `app/data/` (git-ignored; the
local app keeps reading `data/`). Your Strava secret and raw streams never leave
your machine. Exclusion edits made online last only until the app restarts, so
use Download there, put the file in `app/` and publish again. To use RStudio's
Publish button instead of `rsconnect`, set `deploy_upload <- FALSE`, run the
script to prepare the data, and include `data/app_data.rds` and
`exclusions.geojson` when publishing `app/app.R`.

## Using it on your phone

**The address.** Use the app's share link, not the `connect.posit.cloud/...`
page that opens after publishing (that is the management page, which asks you
to sign in to Posit). The share link is

    https://01a20636-0d10-e872-1352-f994248ec78b.share.connect.posit.cloud

or, after you set a custom name under Settings > URL on Connect Cloud,
`https://kylestark-<name>.share.connect.posit.cloud`. It is also on the Share
button of the content's card. If you delete the content and publish again it
gets a new id, so check the card.

**Signing in.** Opening the app needs only the app's own login (APP_USER /
APP_PASSWORD from Settings > Variables), not a Posit account. You will be asked
each time the page is opened fresh, so let your phone's password manager save
it. A Posit sign-in is needed only to publish or change settings.

**Make it feel like an app.** In Safari, Share > Add to Home Screen (Chrome on
Android: menu > Add to Home screen). It opens full screen from an icon. The app
sleeps when nobody is using it, so the first load can take several seconds.

**On a phone the layout changes:**

- The map fills the screen. The sidebar opens with the menu button (top left)
  and closes by itself after you choose a park, pick a trail in the table, or
  turn on "Review and edit on the map".
- A line at the top of the map shows the park and how much of it you've run.
- Tap a trail for its name and progress (hover labels don't work on touch screens).
- The ◎ button follows your GPS position: tap it once (allow location access
  when asked) and a blue dot with an accuracy circle tracks you as you move;
  the button turns blue while tracking. The map centres on you once, then
  leaves panning to you. Short GPS dropouts under trees don't stop it. Tap again
  to stop, since tracking uses battery. With "Suggest a route" on, the first
  fix also moves the route start to where you are. Location needs HTTPS, which
  the Connect Cloud link has; it won't work over plain http on your Wi-Fi.

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

## Map colours, November 2026

"Trail colours" now defaults to **Run / not run**: every trail is cut where your
coverage stops, the parts you have run are drawn in muted green, and the parts
you have not are drawn on top in red, in both the overview and a single park.
There is no "partly run" colour in this mode. The old three-way view is still
there as **By trail: completed, partly run, not run**, which colours each whole
named trail by its share run. The cutting is done in the app without `lwgeom`
(which is no longer needed) and is cached per match distance, so the first draw
at a new distance takes about a second and later ones are instant.

## Look and layout, October 2026

Warmer page and sidebar colours, the park name next to the title, a progress
bar in the "Completed" box, and a bar behind each percentage in the table.
Map options and route suggestions are folded into an accordion so the stats
and table stay near the top. The CSS is one short block (`CSS`) near the top
of `app/app.R`.

## Putting it online

Not implemented yet. `docs/HOSTING.md` compares the options (Tailscale,
Cloudflare Tunnel + Access, Posit Connect Cloud, a small VPS)
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

The default is 30 m (`default_match_m`), which forgives GPS drift under trees and offsets in the trail centrelines. 15 m is stricter and suits full-resolution streams in open country.

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
| app/exclusions.geojson | app | your saved exclusions (in `app/`, not `data/`, so it is committed and deployed) |
| trails.rds | 02 | sf LINESTRING per segment, UTM 10N metres, with uid, park, name, type, access, surface, notes, length_m, is_service, in_closed_land |
| nodes.rds | 02 | sf POINT per sample node: trail_id, node_i, n_nodes |
| parks.rds | 02 | sf polygon per park, unioned by name |
| node_dist.rds | 03 | trail_id, node_i, dist_m |
| park_runs.rds | 03 | miles run and run count inside each park |
| app_data.rds | 03 | everything the app loads |
