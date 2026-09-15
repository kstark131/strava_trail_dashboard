# ==============================================================================
# SCRIPT 2: PREPARE TRAILS AND PARKS
# ==============================================================================
# Reads the trail centerlines and park polygons, projects them to metres,
# splits multipart lines, samples each trail every node_spacing_m, and assigns
# every trail segment to a park. Nothing here depends on your runs, so it only
# needs re-running when the trail or park layers change.
#
# Output: data/trails.rds  (sf LINESTRING, one row per trail segment)
#         data/nodes.rds   (sf POINT, one row per sample node)
#         data/parks.rds   (sf POLYGON, one row per park, boundaries unioned by name)

library(sf)
library(dplyr)
library(purrr)
library(stringr)

source("00_config.R")
sf_use_s2(FALSE)   # planar operations; everything is projected

# ------------------------------------------------------------------------------
# helpers
# ------------------------------------------------------------------------------
first_nonempty <- function(df, fields) {
  fields <- intersect(fields, names(df))
  out <- rep("", nrow(df))
  for (f in fields) {
    v <- str_squish(coalesce(as.character(df[[f]]), ""))
    v[tolower(v) == "unknown"] <- ""
    out <- ifelse(out == "" & v != "", v, out)
  }
  out
}
col_or <- function(df, field, default = "") {
  if (field %in% names(df)) str_squish(coalesce(as.character(df[[field]]), default)) else rep(default, nrow(df))
}
norm_name <- function(x) {
  x <- tolower(x)
  x <- gsub("&", "and", x, fixed = TRUE)
  x <- gsub("\\b(regional|trail|park|preserve|shoreline)\\b", " ", x)
  gsub("[^a-z0-9]", "", x)
}

# ------------------------------------------------------------------------------
# 1. PARKS
# ------------------------------------------------------------------------------
parks_raw <- st_read(path_parks, quiet = TRUE) %>%
  st_zm(drop = TRUE) %>%
  st_transform(crs_projected) %>%
  st_make_valid()

parks <- parks_raw %>%
  mutate(park   = col_or(parks_raw, park_name_field),
         status = col_or(parks_raw, park_status_field),
         acres  = suppressWarnings(as.numeric(col_or(parks_raw, park_acres_field, "0")))) %>%
  filter(park != "") %>%
  group_by(park) %>%
  summarise(acres  = sum(acres, na.rm = TRUE),
            status = paste(sort(unique(status[status != ""])), collapse = ", "),
            .groups = "drop") %>%
  st_make_valid()

message(nrow(parks), " parks")

# ------------------------------------------------------------------------------
# 2. TRAILS
# ------------------------------------------------------------------------------
trails_raw <- st_read(path_trails, quiet = TRUE) %>%
  st_zm(drop = TRUE) %>%
  st_transform(crs_projected)

trails <- trails_raw %>%
  mutate(name      = first_nonempty(trails_raw, trail_name_fields),
         park_attr = col_or(trails_raw, trail_park_field),
         type      = col_or(trails_raw, trail_type_field),
         access    = col_or(trails_raw, trail_access_field),
         surface   = col_or(trails_raw, trail_surface_field)) %>%
  select(name, park_attr, type, access, surface) %>%
  st_cast("MULTILINESTRING") %>%
  st_cast("LINESTRING", warn = FALSE)               # one row per part

trails$trail_id <- seq_len(nrow(trails))
trails$length_m <- as.numeric(st_length(trails))
trails <- trails %>%
  mutate(name       = ifelse(name == "", paste("Unnamed", tolower(ifelse(type == "", "segment", type))), name),
         is_service = str_detect(access, regex("service", ignore_case = TRUE)) |
                      str_detect(name,   regex("^service road", ignore_case = TRUE))) %>%
  filter(length_m >= 1)

message(nrow(trails), " trail segments, ", round(sum(trails$length_m) / 1609.344), " miles")

# ------------------------------------------------------------------------------
# 3. NODES: regular samples along every segment
# ------------------------------------------------------------------------------
# type = "regular" with n points places them at (i - 0.5) / n of the length, so
# node i represents the bin [(i-1)/n, i/n]. The app uses that to draw run and
# unrun pieces of each line.
n_nodes <- pmax(2L, as.integer(ceiling(trails$length_m / node_spacing_m)))
node_geom <- st_line_sample(st_geometry(trails), n = n_nodes, type = "regular")

nodes <- st_sf(trail_id = trails$trail_id, geometry = node_geom) %>%
  st_cast("POINT", warn = FALSE) %>%
  group_by(trail_id) %>%
  mutate(node_i = row_number(), n_nodes = n()) %>%
  ungroup()

got <- as.integer(table(factor(nodes$trail_id, levels = trails$trail_id)))
if (!all(got == n_nodes)) warning(sum(got != n_nodes), " segments returned a different node count than requested")
message(nrow(nodes), " nodes")

# ------------------------------------------------------------------------------
# 4. PARK ASSIGNMENT
# ------------------------------------------------------------------------------
# Order of preference:
#  a) the trail layer's own park attribute, when it names a park in the park layer
#  b) the smallest park polygon that contains the segment's middle node
#  c) a park polygon whose name matches the trail name (regional trails)
#  d) the park attribute even if no polygon exists (a "pseudo park" with no boundary)
#  e) "Outside park boundaries"
mid_nodes <- nodes %>%
  group_by(trail_id) %>% slice(ceiling(n() / 2)) %>% ungroup()

spatial <- st_join(mid_nodes["trail_id"], parks %>% arrange(acres) %>% select(park_spatial = park),
                   join = st_within, left = TRUE) %>%
  st_drop_geometry() %>%
  distinct(trail_id, .keep_all = TRUE)        # smallest containing park wins

park_lookup <- setNames(parks$park, norm_name(parks$park))

trails <- trails %>%
  left_join(spatial, by = "trail_id") %>%
  mutate(park_name_match = map_chr(name, ~ {
           k <- norm_name(.x); if (k %in% names(park_lookup)) park_lookup[[k]] else NA_character_ }),
         park = case_when(
           park_attr %in% parks$park       ~ park_attr,
           !is.na(park_spatial)            ~ park_spatial,
           !is.na(park_name_match)         ~ park_name_match,
           park_attr != ""                 ~ park_attr,
           TRUE                            ~ "Outside park boundaries")) %>%
  select(trail_id, park, name, type, access, surface, length_m, is_service, park_attr)

message(sum(trails$park == "Outside park boundaries"), " segments outside any park boundary")

# ------------------------------------------------------------------------------
# 5. SAVE
# ------------------------------------------------------------------------------
saveRDS(trails, file.path(dir_data, "trails.rds"))
saveRDS(nodes,  file.path(dir_data, "nodes.rds"))
saveRDS(parks,  file.path(dir_data, "parks.rds"))
