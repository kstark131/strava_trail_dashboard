# ==============================================================================
# EXCLUSIONS (sourced by app.R)
# ==============================================================================
# Trails left out of the completion numbers, from two sources:
#
# 1. Rules on the trail layer's own data, switched on and off in the app:
#      closed_land  segment lies only on land marked park_closed_status (EBRPD "Landbank")
#      closed       name or notes match rule_closed_pattern (closed, golf course, ...)
#      proposed     name or notes match rule_proposed_pattern
#      paved_road   type is one of rule_road_types
#      outside      outside every park boundary
# 2. Your own edits, saved in exclusions.geojson next to this file:
#      kind "segment": one trail segment, found by its uid; action "exclude",
#                      or "include" to bring back a segment a rule or area removed
#      kind "area":    a polygon; segments at least half inside it are excluded
#
# Excluded segments drop out of every total and are drawn grey and dashed. Route
# suggestions also avoid the ones you could not walk (BLOCKING): closed land,
# closed or proposed trails, and anything you excluded by hand.

EXCL_FILE    <- "exclusions.geojson"
EXCL_REASONS <- c("Private", "Closed", "Golf course", "Not a trail", "Other")

RULE_LABELS <- c(closed_land = "Land not open to the public (Landbank)",
                 closed      = "Notes say closed, no public access or golf course",
                 proposed    = "Notes say proposed or future trail",
                 paved_road  = "Paved roads",
                 outside     = "Outside park boundaries")
RULE_BLOCKING <- c("closed_land", "closed", "proposed")

empty_exclusions <- function() {
  st_sf(kind = character(), action = character(), uid = character(), reason = character(),
        name = character(), park = character(), added = character(), geometry = st_sfc(crs = 4326))
}

read_exclusions <- function(path = EXCL_FILE) {
  x <- if (file.exists(path)) tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
  if (is.null(x) || !nrow(x)) return(empty_exclusions())
  x <- st_transform(x, 4326)
  for (k in setdiff(names(empty_exclusions()), c(names(x), "geometry"))) x[[k]] <- NA_character_
  st_geometry(x) <- "geometry"
  x[, names(empty_exclusions())]
}

write_exclusions <- function(x, path = EXCL_FILE) {
  if (!nrow(x)) return(writeLines('{"type": "FeatureCollection", "features": []}', path))
  tmp <- tempfile(fileext = ".geojson")
  st_write(x, tmp, quiet = TRUE)
  file.copy(tmp, path, overwrite = TRUE)
}

# TRUE where each rule matches, one column per rule (rules whose data is missing never match)
rule_hits <- function(trails, params, outside_name) {
  txt <- paste(trails$name, if (is.null(trails$notes)) "" else trails$notes)
  data.frame(
    closed_land = if (is.null(trails$in_closed_land)) FALSE else trails$in_closed_land %in% TRUE,
    closed      = nzchar(params$rule_closed_pattern) & grepl(params$rule_closed_pattern, txt, ignore.case = TRUE),
    proposed    = nzchar(params$rule_proposed_pattern) & grepl(params$rule_proposed_pattern, txt, ignore.case = TRUE),
    paved_road  = trails$type %in% params$rule_road_types,
    outside     = trails$park == outside_name
  )
}

# Which segments are excluded, and why. Returns a data frame with trail_id,
# reason and blocking (TRUE when routes should avoid it).
apply_exclusions <- function(trails_proj, hits, rules_on, manual) {
  reason <- rep(NA_character_, nrow(trails_proj)); blocking <- rep(FALSE, nrow(trails_proj))
  for (r in intersect(names(RULE_LABELS), rules_on)) {
    h <- hits[[r]] & is.na(reason)
    reason[h] <- RULE_LABELS[[r]]; blocking[h] <- r %in% RULE_BLOCKING
  }

  areas <- manual[manual$kind == "area" & manual$action == "exclude", ]
  if (nrow(areas)) {
    ap <- st_transform(st_geometry(areas), st_crs(trails_proj))
    for (k in seq_along(ap)) {
      cand <- which(lengths(st_intersects(trails_proj, ap[k])) > 0)
      if (!length(cand)) next
      inside <- vapply(cand, function(i) sum(as.numeric(st_length(st_intersection(st_geometry(trails_proj)[i], ap[k])))), 0)
      hit <- cand[inside >= 0.5 * trails_proj$length_m[cand]]
      reason[hit] <- paste("Area:", areas$reason[k]); blocking[hit] <- TRUE
    }
  }

  segs <- manual[manual$kind == "segment", ]
  i <- match(segs$uid, trails_proj$uid)                 # uids no longer in the layer are ignored
  ex <- i[segs$action == "exclude" & !is.na(i)]; inc <- i[segs$action == "include" & !is.na(i)]
  reason[ex] <- paste("Marked:", segs$reason[segs$action == "exclude" & !is.na(i)]); blocking[ex] <- TRUE
  reason[inc] <- NA; blocking[inc] <- FALSE

  keep <- !is.na(reason)
  data.frame(trail_id = trails_proj$trail_id[keep], reason = reason[keep], blocking = blocking[keep])
}

# Flip one segment: excluded -> included and back, keeping the manual list
# minimal (a click that undoes an earlier click removes that entry).
toggle_segment <- function(manual, seg, excluded_now, reason) {
  mine <- manual$kind == "segment" & manual$uid %in% seg$uid
  if (any(mine)) return(manual[!mine, ])                # undo the earlier edit
  entry <- st_sf(kind = "segment", action = if (excluded_now) "include" else "exclude", uid = seg$uid,
                 reason = if (excluded_now) "" else reason, name = seg$name, park = seg$park,
                 added = format(Sys.Date()), geometry = st_geometry(seg))
  rbind(manual, entry)
}
