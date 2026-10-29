# ==============================================================================
# ROUTE SUGGESTIONS (sourced by app.R)
# ==============================================================================
# Suggests a route of about a target length that picks up as much unrun trail
# as possible in one park.
#
# 1. build_route_graph(): a walkable graph of the park's trails. Vertices are
#    each segment's two end points plus its sample nodes (the same nodes
#    node_dist describes), so partly run trails split exactly where the run
#    part ends. Consecutive vertices along a segment are joined, and each
#    segment end is joined to the nearest vertex of every other segment within
#    snap_m: trail layers rarely share exact end points, and a trail often ends
#    part way along another.
# 2. suggest_route(): a greedy orienteering heuristic. From the current point,
#    take the unrun stretch with the most new metres per metre walked to reach
#    and cover it, as long as the route can still get home within the target.
#    Repeat until nothing fits, then return to the start (or stop, one way).
#
# Everything is in projected metres (trails_proj). The graph depends only on
# the park, so it is cached; the unrun values depend on the match distance and
# the service-road setting and are recomputed on every call (milliseconds).

library(igraph)
library(RANN)

route_graph_cache <- new.env()

build_route_graph <- function(park, trails_proj, node_dist, snap_m = 15) {
  key <- paste(park, snap_m)
  if (!is.null(route_graph_cache[[key]])) return(route_graph_cache[[key]])

  tr <- trails_proj[trails_proj$park == park, ]
  if (!nrow(tr)) return(NULL)
  n_nodes <- node_dist$n_nodes[match(tr$trail_id, node_dist$trail_id)]

  # vertices: end, node 1..n at (i - 0.5) / n of the length (as in 02), end
  xy <- st_coordinates(st_geometry(tr))
  parts <- split(as.data.frame(xy[, 1:2]), factor(xy[, "L1"], levels = seq_len(nrow(tr))))
  verts <- do.call(rbind, lapply(seq_len(nrow(tr)), function(k) {
    p <- as.matrix(parts[[k]]); n <- n_nodes[k]
    s <- c(0, cumsum(sqrt(rowSums(diff(p)^2))))
    at <- c(0, (seq_len(n) - 0.5) / n, 1) * s[length(s)]
    data.frame(trail_id = tr$trail_id[k], node_i = 0:(n + 1L), end = c(TRUE, rep(FALSE, n), TRUE), at = at,
               x = approx(s, p[, 1], at, ties = list("ordered", mean))$y,
               y = approx(s, p[, 2], at, ties = list("ordered", mean))$y)
  }))
  verts$v <- seq_len(nrow(verts))

  # edges along each segment (vertex ids are consecutive within a segment)
  nxt   <- c(verts$trail_id[-1] == verts$trail_id[-nrow(verts)], FALSE)
  along <- data.frame(from = verts$v[nxt], to = verts$v[nxt] + 1L,
                      len = diff(verts$at)[nxt[-nrow(verts)]], trail_id = verts$trail_id[nxt])

  # junctions: each segment end to the nearest vertex of each other segment
  ends <- which(verts$end)
  nn <- RANN::nn2(verts[, c("x", "y")], verts[ends, c("x", "y")], k = min(30, nrow(verts)),
                  searchtype = "radius", radius = snap_m)
  j <- data.frame(from = rep(verts$v[ends], ncol(nn$nn.idx)), to = as.vector(nn$nn.idx),
                  len = as.vector(nn$nn.dists))
  j <- j[j$to > 0, ]
  j <- j[verts$trail_id[j$from] != verts$trail_id[j$to], ]
  j <- j[order(j$len), ]
  j <- j[!duplicated(cbind(j$from, verts$trail_id[j$to])), ]
  j <- j[!duplicated(cbind(pmin(j$from, j$to), pmax(j$from, j$to))), ]
  j$len <- pmax(j$len, 0.01)
  j$trail_id <- NA_integer_

  edges <- rbind(along, j)
  g <- graph_from_data_frame(edges[, c("from", "to")], directed = FALSE,
                             vertices = data.frame(name = verts$v))
  E(g)$weight <- edges$len

  G <- list(g = g, verts = verts, edges = edges, n_along = nrow(along),
            comp = components(g)$membership,
            names = setNames(tr$name, tr$trail_id))
  route_graph_cache[[key]] <- G
  G
}

# share of each vertex that is unrun (1) or run / out of scope (0). Segment
# ends take the value of their neighbouring node.
vertex_unrun <- function(G, node_dist, scope_ids, thr) {
  vt <- G$verts
  nd <- node_dist[node_dist$trail_id %in% unique(vt$trail_id), ]
  key_v <- paste(vt$trail_id, pmin(pmax(vt$node_i, 1L), nd$n_nodes[match(vt$trail_id, nd$trail_id)]))
  d <- nd$dist_m[match(key_v, paste(nd$trail_id, nd$node_i))]
  as.numeric(!is.na(d) & d > thr & vt$trail_id %in% scope_ids)
}

# vertex to start from: nearest to (x, y) if given, else to the most common
# place your runs started (rounded to 250 m), else to the middle of the
# largest connected piece of the park's trail network.
start_vertex <- function(G, xy = NULL, run_starts = NULL) {
  vt <- G$verts
  big <- vt$v[G$comp == as.integer(names(which.max(table(G$comp))))]
  if (is.null(xy) && length(run_starts) && nrow(run_starts)) {
    near <- RANN::nn2(vt[big, c("x", "y")], run_starts, k = 1)$nn.dists[, 1] < 2000
    if (any(near)) {
      cell <- paste(round(run_starts[near, 1] / 250), round(run_starts[near, 2] / 250))
      top <- names(which.max(table(cell)))
      xy <- colMeans(run_starts[near, , drop = FALSE][cell == top, , drop = FALSE])
    }
  }
  if (is.null(xy)) return(big[RANN::nn2(vt[big, c("x", "y")], t(colMeans(vt[big, c("x", "y")])), k = 1)$nn.idx[1]])
  RANN::nn2(vt[, c("x", "y")], matrix(xy, ncol = 2), k = 1)$nn.idx[1]
}

suggest_route <- function(G, unrun, start, target_m, loop = TRUE) {
  g <- G$g; ed <- G$edges; vt <- G$verts
  gain <- ed$len * (unrun[ed$from] + unrun[ed$to]) / 2      # unrun metres per edge

  # candidate stretches: runs of consecutive unrun edges along one segment
  a_idx <- seq_len(G$n_along)
  is_un <- gain[a_idx] > 0
  brk   <- c(TRUE, !is_un[-1] | !is_un[-length(is_un)] | ed$trail_id[a_idx][-1] != ed$trail_id[a_idx][-length(is_un)])
  grp   <- cumsum(brk)[is_un]
  cand  <- split(a_idx[is_un], grp)
  if (!length(cand)) return(NULL)
  c_a   <- vapply(cand, function(e) ed$from[e[1]], 0L)
  c_b   <- vapply(cand, function(e) ed$to[e[length(e)]], 0L)
  c_len <- vapply(cand, function(e) sum(ed$len[e]), 0)

  d_home <- if (loop) distances(g, v = start)[1, ] else rep(0, vcount(g))
  cur <- start; used <- 0; route_e <- integer(0); route_gain <- numeric(0)
  take <- function(e) {                                               # walk edges e, banking their gain
    ge <- gain[e]; ge[duplicated(e)] <- 0
    gain[e] <<- 0
    route_e <<- c(route_e, e); route_gain <<- c(route_gain, ge)
  }

  repeat {
    d_cur <- distances(g, v = cur)[1, ]
    c_gain <- vapply(cand, function(e) sum(gain[e]), 0)
    go_f <- d_cur[c_a] + c_len; go_r <- d_cur[c_b] + c_len           # enter at a or at b
    ok_f <- c_gain > 1 & used + go_f + d_home[c_b] <= target_m
    ok_r <- c_gain > 1 & used + go_r + d_home[c_a] <= target_m
    sc <- c(ifelse(ok_f, c_gain / go_f, -Inf), ifelse(ok_r, c_gain / go_r, -Inf))
    if (!any(is.finite(sc))) break
    best <- which.max(sc); k <- (best - 1L) %% length(cand) + 1L; fwd <- best <= length(cand)
    entry <- if (fwd) c_a[k] else c_b[k]
    leg <- as.integer(shortest_paths(g, from = cur, to = entry, output = "epath")$epath[[1]])
    stretch <- if (fwd) cand[[k]] else rev(cand[[k]])
    take(c(leg, stretch))
    used <- used + d_cur[entry] + c_len[k]
    cur <- if (fwd) c_b[k] else c_a[k]
  }
  if (!length(route_e)) return(NULL)
  if (loop && cur != start) {
    take(as.integer(shortest_paths(g, from = cur, to = start, output = "epath")$epath[[1]]))
    used <- used + d_home[cur]
  }

  # vertex sequence: walk the edges, flipping each one to continue from the last vertex
  v <- start
  for (e in route_e) v <- c(v, if (ed$from[e] == v[length(v)]) ed$to[e] else ed$from[e])
  # trail names in order, with distance on each
  nm <- G$names[as.character(ed$trail_id[route_e])]
  nm[is.na(nm)] <- ""
  keep <- nzchar(nm); r <- rle(nm[keep])
  step <- rep(seq_along(r$lengths), r$lengths)
  steps <- data.frame(name  = r$values,
                      len_m = as.numeric(tapply(ed$len[route_e][keep], step, sum)),
                      new_m = as.numeric(tapply(route_gain[keep], step, sum)))
  list(xy = as.matrix(vt[v, c("x", "y")]), length_m = used, new_m = sum(route_gain),
       steps = steps, loop = loop)
}

# minimal GPX 1.1 track from a lon/lat matrix
write_gpx <- function(lonlat, name, file) {
  pts <- sprintf('      <trkpt lat="%.6f" lon="%.6f"/>', lonlat[, 2], lonlat[, 1])
  name <- gsub("&", "&amp;", gsub("<", "&lt;", name, fixed = TRUE), fixed = TRUE)
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<gpx version="1.1" creator="strava_trail_dashboard" xmlns="http://www.topografix.com/GPX/1/1">',
               sprintf("  <trk><name>%s</name><trkseg>", name), pts, "  </trkseg></trk>", "</gpx>"), file)
}
