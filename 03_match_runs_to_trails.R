# ==============================================================================
# SCRIPT 3: MATCH RUNS TO TRAILS
# ==============================================================================
# For every trail node, finds the distance to the nearest point on any run.
# The match threshold itself is applied in the app, so the slider works without
# re-running this script. Re-run this after every download.
#
# Output: data/node_dist.rds (trail_id, node_i, dist_m)
#         data/park_runs.rds (miles run inside each park boundary)
#         data/app_data.rds  (everything the Shiny app needs, in lat/lon)

library(sf)
library(dplyr)
library(purrr)
library(RANN)      # fast nearest neighbour (KD-tree)

source("00_config.R")
sf_use_s2(FALSE)

activities <- readRDS(file.path(dir_data, "activities.rds"))
streams    <- readRDS(file.path(dir_data, "streams.rds"))
trails     <- readRDS(file.path(dir_data, "trails.rds"))
nodes      <- readRDS(file.path(dir_data, "nodes.rds"))
parks      <- readRDS(file.path(dir_data, "parks.rds"))

# ------------------------------------------------------------------------------
# 1. RUN TRACKS AS LINES (in metres), split at GPS gaps
# ------------------------------------------------------------------------------
pts <- st_as_sf(streams, coords = c("lng", "lat"), crs = crs_global, remove = FALSE) %>%
  st_transform(crs_projected)
xy  <- st_coordinates(pts)

track <- tibble(activity_id = pts$activity_id, time = pts$time, x = xy[, 1], y = xy[, 2]) %>%
  arrange(activity_id, time) %>%
  group_by(activity_id) %>%
  mutate(step  = sqrt((x - lag(x))^2 + (y - lag(y))^2),
         piece = cumsum(is.na(step) | step > run_gap_m)) %>%   # new piece after a gap
  ungroup()

# keep only the study area: within 5 km of any trail
bb <- st_bbox(trails)
track <- track %>%
  group_by(activity_id) %>%
  filter(any(x >= bb["xmin"] - 5000 & x <= bb["xmax"] + 5000 & y >= bb["ymin"] - 5000 & y <= bb["ymax"] + 5000)) %>%
  ungroup()

pieces <- track %>%
  group_by(activity_id, piece) %>%
  filter(n() >= 2) %>%
  group_split()

run_lines <- st_sf(
  activity_id = map_chr(pieces, ~ .x$activity_id[1]),
  piece       = map_int(pieces, ~ .x$piece[1]),
  geometry    = st_sfc(map(pieces, ~ st_linestring(as.matrix(.x[, c("x", "y")]))), crs = crs_projected)
)
message(length(unique(run_lines$activity_id)), " activities in the study area")

# ------------------------------------------------------------------------------
# 2. DENSIFY RUNS AND BUILD THE SEARCH INDEX
# ------------------------------------------------------------------------------
run_dense <- st_segmentize(run_lines, dfMaxLength = run_densify_m)
run_pts   <- st_cast(run_dense, "POINT", warn = FALSE)      # keeps activity_id per point
run_xy    <- st_coordinates(run_pts)
message(nrow(run_xy), " densified run points")

# ------------------------------------------------------------------------------
# 3. NEAREST RUN DISTANCE FOR EVERY NODE
# ------------------------------------------------------------------------------
node_xy <- st_coordinates(nodes)
nn <- RANN::nn2(data = run_xy, query = node_xy, k = 1)
node_dist <- nodes %>%
  st_drop_geometry() %>%
  select(trail_id, node_i, n_nodes) %>%
  mutate(dist_m = pmin(nn$nn.dists[, 1], max_node_dist_m))

# quick look at the default threshold
cov <- node_dist %>% group_by(trail_id) %>% summarise(frac = mean(dist_m <= default_match_m), .groups = "drop") %>%
  inner_join(st_drop_geometry(trails), by = "trail_id")
message(sprintf("At %d m: %.1f of %.1f trail miles run (%.1f%%), service roads excluded",
                default_match_m,
                sum(cov$frac * cov$length_m * !cov$is_service) / 1609.344,
                sum(cov$length_m * !cov$is_service) / 1609.344,
                100 * sum(cov$frac * cov$length_m * !cov$is_service) / sum(cov$length_m * !cov$is_service)))

# ------------------------------------------------------------------------------
# 4. MILES RUN INSIDE EACH PARK
# ------------------------------------------------------------------------------
# Counted from the densified points (one every run_densify_m), which is accurate
# to a few metres per run and far faster than st_intersection on 600+ lines.
hits <- st_intersects(parks, run_pts)
park_runs <- tibble(
  park   = parks$park,
  run_mi = lengths(hits) * run_densify_m / 1609.344,
  n_runs = map_int(hits, ~ {
    if (!length(.x)) return(0L)
    tab <- table(run_pts$activity_id[.x])
    sum(tab * run_densify_m >= 100)            # at least 100 m inside the park
  }),
  run_ids = map(hits, ~ names(which(table(run_pts$activity_id[.x]) * run_densify_m >= 100)))
)

# ------------------------------------------------------------------------------
# 5. BUNDLE FOR THE APP (lat/lon for leaflet, metres kept for line splitting)
# ------------------------------------------------------------------------------
runs_ll <- run_lines %>%
  st_simplify(dTolerance = 3) %>%                # lighter map layer; matching used the full data
  st_transform(crs_global) %>%
  left_join(activities %>% select(id, run_name = name, date, distance_km = distance, distance_mi),
            by = c("activity_id" = "id"))

app_data <- list(
  built        = Sys.Date(),
  params       = list(node_spacing_m = node_spacing_m, max_node_dist_m = max_node_dist_m,
                      default_match_m = default_match_m, done_fraction = done_fraction),
  activities   = activities,
  totals       = list(runs = nrow(activities), miles = sum(activities$distance_mi, na.rm = TRUE),
                      runs_in_area = length(unique(run_lines$activity_id))),
  trails_proj  = trails,                         # metres, for st_linesubstring
  trails_ll    = st_transform(trails, crs_global),
  node_dist    = node_dist,
  parks_ll     = parks %>% st_simplify(dTolerance = 5) %>% st_transform(crs_global) %>%
                   left_join(park_runs, by = "park"),
  runs_ll      = runs_ll
)

saveRDS(node_dist, file.path(dir_data, "node_dist.rds"))
saveRDS(park_runs, file.path(dir_data, "park_runs.rds"))
saveRDS(app_data,  file.path(dir_data, "app_data.rds"))
message("Saved data/app_data.rds. Run the app with: shiny::runApp('app')")
