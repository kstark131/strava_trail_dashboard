# ==============================================================================
# SCRIPT 2a (optional): TRAILS AND PARKS FROM OPENSTREETMAP
# ==============================================================================
# Used when trail_source = "osm" in 00_config.R, in place of your own layers.
# Downloads, for the box osm_bbox only:
#   parks:  named leisure=park / nature_reserve and boundary=protected_area /
#           national_park polygons, minus private ones and those under
#           osm_min_park_acres
#   trails: highway=path / footway / track / bridleway / steps that fall inside
#           those polygons (sidewalks, crossings and private ways excluded)
# and writes data/osm/trails.geojson and data/osm/parks.geojson, which
# 02_prepare_trails.R then reads like any other trail and park layer.
#
# Being a good citizen of the public Overpass API:
#  * the box is capped at osm_max_area_km2 and split into osm_tile_deg tiles
#  * one request at a time, osm_pause_sec apart, backing off on 429 / 504
#  * each tile's response is cached in data/osm/cache/ and reused for
#    osm_cache_days, so re-running this script normally downloads nothing
#  * only the parks and trails are requested, not roads, buildings or POIs
# Delete data/osm/cache/ (or a single tile) to force a fresh download.
#
# OSM data is (c) OpenStreetMap contributors, ODbL. The app shows the credit.

library(sf)
library(dplyr)
library(httr)
library(jsonlite)

source("00_config.R")
sf_use_s2(FALSE)

dir_osm   <- file.path(dir_data, "osm")
dir_cache <- file.path(dir_osm, "cache")
dir.create(dir_cache, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. CHECK THE BOX AND SPLIT IT INTO TILES
# ------------------------------------------------------------------------------
bb <- osm_bbox
stopifnot(all(c("xmin", "ymin", "xmax", "ymax") %in% names(bb)), bb[["xmax"]] > bb[["xmin"]], bb[["ymax"]] > bb[["ymin"]])
area_km2 <- (bb[["xmax"]] - bb[["xmin"]]) * 111.32 * cos(mean(bb[c("ymin", "ymax")]) * pi / 180) *
            (bb[["ymax"]] - bb[["ymin"]]) * 110.57
if (area_km2 > osm_max_area_km2) {
  stop(sprintf("osm_bbox covers %.0f km2, more than osm_max_area_km2 (%.0f). Use a smaller box around the parks you run in.",
               area_km2, osm_max_area_km2))
}

xs <- unique(c(seq(bb[["xmin"]], bb[["xmax"]], by = osm_tile_deg), bb[["xmax"]]))
ys <- unique(c(seq(bb[["ymin"]], bb[["ymax"]], by = osm_tile_deg), bb[["ymax"]]))
tiles <- expand.grid(i = seq_len(length(xs) - 1), j = seq_len(length(ys) - 1))
tiles <- data.frame(xmin = xs[tiles$i], xmax = xs[tiles$i + 1], ymin = ys[tiles$j], ymax = ys[tiles$j + 1])
message(sprintf("OSM box: %.0f km2 in %d tile(s)", area_km2, nrow(tiles)))

# ------------------------------------------------------------------------------
# 2. QUERY, ONE TILE AT A TIME, CACHED
# ------------------------------------------------------------------------------
overpass_query <- function(t) {
  b <- sprintf("%.5f,%.5f,%.5f,%.5f", t$ymin, t$xmin, t$ymax, t$xmax)     # Overpass wants S,W,N,E
  nm <- if (nzchar(osm_park_pattern)) sprintf('["name"~"%s",i]', gsub('"', "", osm_park_pattern)) else '["name"]'
  paste0(
    '[out:json][timeout:90][maxsize:67108864];\n',
    '(\n',
    '  way["leisure"~"^(park|nature_reserve)$"]', nm, '(', b, ');\n',
    '  relation["leisure"~"^(park|nature_reserve)$"]', nm, '(', b, ');\n',
    '  way["boundary"~"^(protected_area|national_park)$"]', nm, '(', b, ');\n',
    '  relation["boundary"~"^(protected_area|national_park)$"]', nm, '(', b, ');\n',
    ')->.parks;\n',
    '.parks out geom;\n',
    '.parks map_to_area->.pa;\n',
    'way["highway"~"^(path|footway|track|bridleway|steps)$"]["footway"!~"^(sidewalk|crossing)$"]',
    '["access"!~"^(private|no)$"]["foot"!~"^(private|no)$"](area.pa)(', b, ');\n',
    'out geom;\n')
}

# cache file name is an MD5 of the query, so changing the box, pattern or filters refetches
query_hash <- function(q) { f <- tempfile(); writeLines(q, f); unname(substr(tools::md5sum(f), 1, 12)) }

fetch_tile <- function(t) {
  q <- overpass_query(t)
  file <- file.path(dir_cache, paste0("tile_", query_hash(q), ".json"))
  if (file.exists(file) && difftime(Sys.time(), file.mtime(file), units = "days") < osm_cache_days) {
    return(fromJSON(file, simplifyVector = FALSE))
  }
  for (attempt in 1:4) {
    resp <- POST(osm_overpass_url, body = list(data = q), encode = "form", timeout(120),
                 user_agent("strava_trail_dashboard (personal trail-completion map; R httr)"))
    status <- status_code(resp)
    if (status == 200) break
    if (status %in% c(429, 502, 503, 504) && attempt < 4) {
      wait <- 60 * attempt
      message(sprintf("  Overpass returned %d, waiting %d s", status, wait))
      Sys.sleep(wait)
    } else {
      stop("Overpass returned HTTP ", status, ": ", substr(content(resp, as = "text", encoding = "UTF-8"), 1, 300))
    }
  }
  txt <- content(resp, as = "text", encoding = "UTF-8")
  js <- fromJSON(txt, simplifyVector = FALSE)
  if (!is.null(js$remark) && grepl("error|timed out|runtime", js$remark, ignore.case = TRUE)) {
    stop("Overpass could not finish the query: ", js$remark, ". Use a smaller osm_tile_deg.")
  }
  writeLines(txt, file)
  Sys.sleep(osm_pause_sec)
  js
}

elements <- list()
for (k in seq_len(nrow(tiles))) {
  message(sprintf("Tile %d of %d", k, nrow(tiles)))
  elements <- c(elements, fetch_tile(tiles[k, ])$elements)
}

# ------------------------------------------------------------------------------
# 3. ELEMENTS -> sf
# ------------------------------------------------------------------------------
tag <- function(el, k) { v <- el$tags[[k]]; if (is.null(v)) "" else as.character(v) }
geom_xy <- function(g) do.call(rbind, lapply(g, function(p) c(p$lon, p$lat)))

ways <- Filter(function(el) el$type == "way" && length(el$geometry) >= 2, elements)
rels <- Filter(function(el) el$type == "relation", elements)
is_park <- function(el) nzchar(tag(el, "leisure")) || nzchar(tag(el, "boundary"))
is_trail <- function(el) nzchar(tag(el, "highway"))

# parks: closed ways, and multipolygon relations assembled from their member ways
park_rows <- list()
for (el in Filter(is_park, ways)) {
  xy <- geom_xy(el$geometry)
  if (nrow(xy) >= 4 && all(xy[1, ] == xy[nrow(xy), ])) {
    park_rows[[length(park_rows) + 1]] <- list(el = el, geom = st_polygon(list(xy)))
  }
}
rings_to_polygon <- function(members, role) {
  ls <- lapply(Filter(function(m) m$type == "way" && identical(m$role, role) && length(m$geometry) >= 2, members),
               function(m) st_linestring(geom_xy(m$geometry)))
  if (!length(ls)) return(NULL)
  p <- st_collection_extract(st_polygonize(st_union(st_sfc(ls))), "POLYGON")
  if (!length(p)) NULL else st_union(p)
}
for (el in rels) {
  outer <- rings_to_polygon(el$members, "outer")
  if (is.null(outer)) next
  inner <- rings_to_polygon(el$members, "inner")
  g <- if (is.null(inner)) outer else st_difference(outer, inner)
  park_rows[[length(park_rows) + 1]] <- list(el = el, geom = g[[1]])
}
if (!length(park_rows)) stop("No named parks or protected areas found in osm_bbox", if (nzchar(osm_park_pattern)) " matching osm_park_pattern")

parks <- st_sf(
  osm_id = vapply(park_rows, function(r) paste0(r$el$type, "/", r$el$id), ""),
  name   = vapply(park_rows, function(r) tag(r$el, "name"), ""),
  status = vapply(park_rows, function(r) {
             x <- c(tag(r$el, "leisure"), tag(r$el, "boundary"), tag(r$el, "protection_title")); paste(x[nzchar(x)], collapse = ", ") }, ""),
  access = vapply(park_rows, function(r) tag(r$el, "access"), ""),
  geometry = st_sfc(lapply(park_rows, `[[`, "geom"), crs = 4326)
) %>%
  distinct(osm_id, .keep_all = TRUE) %>%
  filter(!access %in% c("private", "no")) %>%
  st_make_valid() %>%
  st_transform(crs_projected)
st_agr(parks) <- "constant"
box <- st_transform(st_as_sfc(st_bbox(bb[c("xmin", "ymin", "xmax", "ymax")], crs = st_crs(4326))), crs_projected)
parks <- st_intersection(parks, box) %>%                     # clip to the box
  filter(st_dimension(geometry) == 2) %>%
  group_by(osm_id, name, status) %>% summarise(.groups = "drop") %>%
  mutate(acres = as.numeric(st_area(geometry)) / 4046.86) %>%
  filter(acres >= osm_min_park_acres) %>%
  st_transform(4326)

# trails: one row per way, deduplicated across tiles, kept only inside the kept parks
trail_ways <- Filter(is_trail, ways)
trails <- st_sf(
  osm_id  = vapply(trail_ways, function(el) paste0("way/", el$id), ""),
  name    = vapply(trail_ways, function(el) { n <- tag(el, "name"); if (nzchar(n)) n else tag(el, "ref") }, ""),
  highway = vapply(trail_ways, function(el) tag(el, "highway"), ""),
  access  = vapply(trail_ways, function(el) tag(el, "access"), ""),
  surface = vapply(trail_ways, function(el) tag(el, "surface"), ""),
  geometry = st_sfc(lapply(trail_ways, function(el) st_linestring(geom_xy(el$geometry))), crs = 4326)
) %>%
  distinct(osm_id, .keep_all = TRUE) %>%
  mutate(type = recode(highway, path = "Trail", footway = "Footpath", track = "Fire road / track",
                       bridleway = "Bridle trail", steps = "Steps"))
trails <- trails[lengths(st_intersects(st_transform(trails, crs_projected),
                                        st_buffer(st_transform(parks, crs_projected), 25))) > 0, ]

message(nrow(parks), " parks, ", nrow(trails), " trail ways, ",
        round(sum(as.numeric(st_length(st_transform(trails, crs_projected)))) / 1609.344), " miles")

# ------------------------------------------------------------------------------
# 4. SAVE (read by 02_prepare_trails.R through path_trails / path_parks)
# ------------------------------------------------------------------------------
st_write(trails, path_trails, delete_dsn = TRUE, quiet = TRUE)
st_write(parks,  path_parks,  delete_dsn = TRUE, quiet = TRUE)
message("Wrote ", path_trails, " and ", path_parks)
