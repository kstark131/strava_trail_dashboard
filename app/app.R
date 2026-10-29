# ==============================================================================
# SHINY APP: trail completion dashboard
# ==============================================================================
# Run from the project folder:  shiny::runApp("app")
# Reads data/app_data.rds written by 03_match_runs_to_trails.R.
#
# Layout: sidebar (park, match distance, map options, route suggestion, stats,
# table) and a leaflet map. Every input feeds the `cov()` reactive; everything
# else derives from it. Route suggestions live in route_suggest.R.

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(lwgeom)     # st_linesubstring, used to draw run and unrun pieces of a trail
source("route_suggest.R")

sf_use_s2(FALSE)
M_PER_MI <- 1609.344
OUTSIDE  <- "Outside park boundaries"

app_path <- if (file.exists("data/app_data.rds")) "data/app_data.rds" else "../data/app_data.rds"
D <- readRDS(app_path)

trails_ll   <- D$trails_ll
trails_proj <- D$trails_proj
node_dist   <- D$node_dist
parks_ll    <- D$parks_ll
runs_ll     <- D$runs_ll
park_choices <- c("All parks" = "", sort(unique(trails_ll$park)))

# route settings come from 00_config.R via app_data; defaults for older bundles
RP <- modifyList(list(route_snap_m = 15, route_default_mi = 5, route_max_mi = 20), as.list(D$params))
trail_source <- if (is.null(D$trail_source)) "file" else D$trail_source

COL <- list(done = "#1c8a5a", partial = "#d9930f", todo = "#c4463b", run = "#3a5fcd", ink = "#1f2a26",
            trail = "#8a4b14", service = "#6b6b6b", route = "#6a2c91")

# A few rules on top of the bslib theme: a warm off-white page, softer cards,
# a tidy accordion and legend.
CSS <- "
  .bslib-sidebar-layout > .sidebar { background: #f3f1ea; border-right: 1px solid #e2ded2; }
  .bslib-value-box { border: 1px solid #e2ded2; box-shadow: none; }
  .bslib-value-box .value-box-value { font-weight: 600; }
  .accordion-button { padding: .55rem .8rem; font-weight: 500; }
  .accordion-body { padding: .6rem .8rem; }
  .info.legend { border-radius: 8px; box-shadow: 0 1px 4px rgba(0,0,0,.15); font-size: 12px; }
  .progress-thin { height: 6px; background: rgba(255,255,255,.35); margin-top: .35rem; }
  .route-steps { font-size: .85rem; padding-left: 1.2rem; margin: .4rem 0; max-height: 14rem; overflow-y: auto; }
  .subtitle { font-weight: 400; color: #6b7c74; margin-left: .5rem; font-size: .9em; }
  table.dataTable tbody td { vertical-align: middle; }
"

# Basemaps offered in the layer control (top right). The first one is shown
# when the app opens; reorder to change the default. All are free and need no
# API key. Names are leaflet provider names: names(providers) lists them all.
BASEMAPS <- c(
  "Esri topo"        = "Esri.WorldTopoMap",
  "Light"            = "CartoDB.Positron",
  "Satellite"        = "Esri.WorldImagery",
  "USGS topo"        = "USGS.USTopo",
  "USGS imagery"     = "USGS.USImageryTopo",
  "OpenTopoMap"      = "OpenTopoMap",
  "OpenStreetMap"    = "OpenStreetMap.Mapnik",
  "Shaded relief"    = "Esri.WorldShadedRelief",
  "Dark"             = "CartoDB.DarkMatter"
)

# ------------------------------------------------------------------------------
# helpers
# ------------------------------------------------------------------------------
fmt1 <- function(x) formatC(x, format = "f", digits = 1, big.mark = ",")
fmt0 <- function(x) formatC(x, format = "d", big.mark = ",")
pct <- function(f) {
  vapply(f, function(x) paste0(formatC(100 * x, format = "f", digits = ifelse(x >= 0.995 | x == 0, 0, 1)), "%"), character(1))
}
cls  <- function(f) ifelse(f >= D$params$done_fraction, "done", ifelse(f > 0.005, "partial", "todo"))

# Covered/uncovered pieces of a set of trails at a threshold. Node i covers the
# bin [(i-1)/n, i/n] of the line, so runs of covered nodes become [from, to].
split_by_coverage <- function(ids, thr) {
  nd <- node_dist %>% filter(trail_id %in% ids) %>% arrange(trail_id, node_i)
  iv <- nd %>%
    group_by(trail_id) %>%
    group_modify(~ {
      cov <- .x$dist_m <= thr
      r <- rle(c(cov))
      ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
      n <- length(cov)
      tibble(covered = r$values, from = (starts - 1) / n, to = ends / n)
    }) %>%
    ungroup()

  if (!nrow(iv)) return(NULL)

  g <- st_geometry(trails_proj)[match(iv$trail_id, trails_proj$trail_id)]

  # st_linesubstring row by row (it does not vectorise over from/to)
  geom_list <- lapply(seq_len(nrow(iv)), function(i) {
    st_linesubstring(g[i], iv$from[i], iv$to[i])
  })

  st_sf(iv, geometry = do.call(c, geom_list)) %>%
    st_transform(4326)
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = span("Trail completion", span(class = "subtitle", textOutput("subtitle", inline = TRUE))),
  theme = bs_theme(version = 5, bg = "#fbfaf6", fg = COL$ink, primary = "#1f6f4a", secondary = "#6b7c74",
                   base_font = font_google("IBM Plex Sans"), "border-radius" = "0.6rem") |>
          bs_add_rules(CSS),
  sidebar = sidebar(
    width = 400,
    selectInput("park", "Park", choices = park_choices, selected = ""),
    sliderInput("thr", "Match distance (m)", min = 5, max = 60, value = D$params$default_match_m, step = 5),
    checkboxInput("svc", "Include service roads", value = FALSE),
    accordion(
      open = FALSE,
      accordion_panel(
        "Map options",
        radioButtons("view", "Trail colours",
                     choices = c("Completion (run / not run)" = "completion",
                                 "Plain trails (no run data)" = "plain"),
                     selected = "completion"),
        checkboxInput("showruns", "Show my run tracks", value = FALSE)
      ),
      accordion_panel(
        "Route suggestion",
        checkboxInput("route", "Suggest a route through unrun trail", value = FALSE),
        conditionalPanel(
          "input.route",
          sliderInput("route_mi", "Target length (mi)", min = 1, max = RP$route_max_mi,
                      value = RP$route_default_mi, step = 0.5),
          checkboxInput("route_loop", "Loop back to the start", value = TRUE),
          p(class = "small text-muted",
            "Starts where most of your runs here began. Click the map to move the start."),
          uiOutput("route_info")
        )
      )
    ),
    layout_columns(
      value_box("Miles run", textOutput("s_run"), p(textOutput("s_run_l", inline = TRUE), class = "small text-muted")),
      value_box("Trail miles", textOutput("s_trail")),
      value_box("Trail miles completed", textOutput("s_done"), theme = "primary"),
      value_box("Completed", textOutput("s_pct"), uiOutput("s_pct_bar"),
                p(textOutput("s_pct_l", inline = TRUE), class = "small"), theme = "primary"),
      col_widths = c(6, 6, 6, 6)
    ),
    div(class = "d-flex gap-2 align-items-center",
        textInput("q", NULL, placeholder = "Filter by name", width = "100%"),
        downloadButton("csv", "CSV", class = "btn-sm")),
    DTOutput("tbl"),
    p(class = "small text-muted mt-2",
      sprintf("%s runs in the study area, built %s. Each trail is sampled every %d m; a sample counts as run when a track passes within the match distance. A trail is completed at %d%% of samples.",
              fmt0(D$totals$runs_in_area), format(D$built), as.integer(D$params$node_spacing_m), round(100 * D$params$done_fraction)),
      if (trail_source == "osm") "Trail and park data \u00a9 OpenStreetMap contributors (ODbL)." else NULL)
  ),
  leafletOutput("map", height = "100%")
)

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  # trails in scope for the current park and service-road setting
  scope <- reactive({
    t <- trails_ll
    if (!input$svc) t <- filter(t, !is_service)
    if (nzchar(input$park)) t <- filter(t, park == input$park)
    t
  })

  # coverage fraction per trail segment at the current threshold
  cov <- reactive({
    ids <- scope()$trail_id
    node_dist %>%
      filter(trail_id %in% ids) %>%
      group_by(trail_id) %>%
      summarise(frac = mean(dist_m <= input$thr), .groups = "drop") %>%
      right_join(st_drop_geometry(scope()), by = "trail_id") %>%
      mutate(frac = coalesce(frac, 0), done_m = frac * length_m)
  })

  # one row per named trail within a park
  groups <- reactive({
    cov() %>%
      group_by(park, name) %>%
      summarise(length_m = sum(length_m), done_m = sum(done_m), segments = n(),
                types = paste(sort(unique(type)), collapse = ", "),
                ids = list(trail_id), .groups = "drop") %>%
      mutate(frac = done_m / length_m)
  })

  # one row per park
  park_rows <- reactive({
    g <- groups()
    pr <- st_drop_geometry(parks_ll) %>% select(park, run_mi, n_runs)
    g %>%
      group_by(park) %>%
      summarise(length_m = sum(length_m), done_m = sum(done_m), trails = n(),
                trails_done = sum(frac >= D$params$done_fraction), .groups = "drop") %>%
      mutate(frac = done_m / length_m) %>%
      left_join(pr, by = "park") %>%
      mutate(run_mi = coalesce(run_mi, 0), n_runs = coalesce(as.integer(n_runs), 0L))
  })

  # ---- stats
  output$subtitle <- renderText(if (nzchar(input$park)) input$park else "All parks")
  output$s_pct_bar <- renderUI({
    l <- sum(cov()$length_m); f <- if (l > 0) sum(cov()$done_m) / l else 0
    div(class = "progress progress-thin", div(class = "progress-bar bg-light", style = sprintf("width: %.1f%%", 100 * f)))
  })
  output$s_trail <- renderText(paste(fmt1(sum(cov()$length_m) / M_PER_MI), "mi"))
  output$s_done  <- renderText(paste(fmt1(sum(cov()$done_m) / M_PER_MI), "mi"))
  output$s_pct   <- renderText({ l <- sum(cov()$length_m); if (l > 0) pct(sum(cov()$done_m) / l) else "0%" })
  output$s_pct_l <- renderText({
    g <- groups(); sprintf("%s of %s trails at %d%% or more", fmt0(sum(g$frac >= D$params$done_fraction)), fmt0(nrow(g)), round(100 * D$params$done_fraction))
  })
  output$s_run <- renderText({
    if (nzchar(input$park)) {
      p <- parks_ll %>% st_drop_geometry() %>% filter(park == input$park)
      paste(fmt1(if (nrow(p)) coalesce(p$run_mi, 0) else 0), "mi")
    } else paste(fmt1(D$totals$miles), "mi")
  })
  output$s_run_l <- renderText({
    if (nzchar(input$park)) {
      p <- parks_ll %>% st_drop_geometry() %>% filter(park == input$park)
      n <- if (nrow(p)) coalesce(as.integer(p$n_runs), 0L) else 0L
      if (nrow(p)) sprintf("inside the boundary, %s runs", fmt0(n)) else "no boundary polygon for this park"
    } else sprintf("all %s runs", fmt0(D$totals$runs))
  })

  # ---- table
  table_rows <- reactive({
    q <- tolower(trimws(input$q))
    if (nzchar(input$park)) {
      r <- groups() %>% arrange(frac, name)
      if (nzchar(q)) r <- filter(r, grepl(q, tolower(name), fixed = TRUE))
      r
    } else {
      r <- park_rows() %>% arrange(frac, park)
      if (nzchar(q)) r <- filter(r, grepl(q, tolower(park), fixed = TRUE))
      r
    }
  })

  output$tbl <- renderDT({
    r <- table_rows()
    if (nzchar(input$park)) {
      d <- data.frame(Trail = paste0(r$name, "<br><span class='text-muted small'>", r$types,
                                     ifelse(r$segments > 1, paste0(", ", r$segments, " segments"), ""), "</span>"),
                      Miles = round(r$length_m / M_PER_MI, 1),
                      Done  = round(100 * r$frac, 1))
    } else {
      d <- data.frame(Park = paste0(r$park, "<br><span class='text-muted small'>", r$trails_done, " of ", r$trails, " trails completed</span>"),
                      `Trail mi` = round(r$length_m / M_PER_MI, 1),
                      Done = round(100 * r$frac, 1),
                      `Run mi` = round(r$run_mi, 1), check.names = FALSE)
    }
    datatable(d, escape = FALSE, rownames = FALSE, selection = "single",
              options = list(pageLength = 15, dom = "tp", order = list(list(2, "asc")))) %>%
      formatStyle("Done", color = styleInterval(c(0.5, 100 * D$params$done_fraction - 0.01), c(COL$todo, COL$partial, COL$done)), fontWeight = "500",
                  background = styleColorBar(c(0, 100), "#dcefe4"), backgroundSize = "100% 70%",
                  backgroundRepeat = "no-repeat", backgroundPosition = "center")
  })

  # row click: park view -> select the park; trail view -> zoom to the trail
  observeEvent(input$tbl_rows_selected, {
    i <- input$tbl_rows_selected
    if (is.null(i)) return()
    r <- table_rows()[i, ]
    if (!nzchar(input$park)) {
      updateSelectInput(session, "park", selected = r$park)
    } else {
      hl <- filter(trails_ll, trail_id %in% r$ids[[1]])
      b <- st_bbox(hl)
      leafletProxy("map") %>%
        fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]]) %>%
        clearGroup("highlight") %>%
        addPolylines(data = hl, color = "#ffffff", weight = 9, opacity = 0.9, group = "highlight",
                     options = pathOptions(pane = "highlight")) %>%
        addPolylines(data = hl, color = COL$ink, weight = 3, opacity = 0.9, dashArray = "6 6", group = "highlight",
                     options = pathOptions(pane = "highlight"))
    }
  })

  output$csv <- downloadHandler(
    filename = function() sprintf("%s_%dm.csv", if (nzchar(input$park)) gsub("[^A-Za-z0-9]+", "_", input$park) else "all_parks", as.integer(input$thr)),
    content = function(f) {
      r <- table_rows()
      if (nzchar(input$park)) {
        out <- r %>% transmute(park, trail = name, types, segments, trail_miles = round(length_m / M_PER_MI, 2),
                               completed_miles = round(done_m / M_PER_MI, 2), completed_pct = round(100 * frac, 1))
      } else {
        out <- r %>% transmute(park, trail_miles = round(length_m / M_PER_MI, 2), completed_miles = round(done_m / M_PER_MI, 2),
                               completed_pct = round(100 * frac, 1), trails, trails_done, miles_run_in_park = round(run_mi, 2), runs = n_runs)
      }
      write.csv(out, f, row.names = FALSE)
    }
  )

  # ---- map
  # Layer groups: "Trails" and "Park boundaries" are toggles in the layer
  # control; "runs" is toggled by the sidebar checkbox; "highlight" is the
  # selected trail. Panes keep the stacking order fixed no matter which layer
  # was redrawn last: park lines < trails < run tracks < highlight.

  legend_for <- function(m, mode) {
    m <- removeControl(m, "legend")
    if (mode == "plain") {
      addLegend(m, position = "bottomright", layerId = "legend", opacity = 0.9,
                colors = c(COL$trail, COL$service, COL$run), labels = c("Trail", "Service road", "My run tracks"))
    } else {
      addLegend(m, position = "bottomright", layerId = "legend", opacity = 0.9,
                colors = c(COL$done, COL$partial, COL$todo, COL$run),
                labels = c("Run", "Partly run (overview)", "Not run", "My run tracks"))
    }
  }

  # draw_trails() works on both a fresh leaflet() object (initial render) and a
  # leafletProxy (updates), so the drawing code lives in one place.
  draw_trails <- function(m, c_df, park, thr, mode) {
    park_view <- nzchar(park)

    if (mode == "plain") {
      # trails only: one colour, service roads dashed, no completion data
      t <- trails_ll %>% filter(trail_id %in% c_df$trail_id) %>%
        mutate(label = sprintf("%s: %s mi%s", name, fmt1(length_m / M_PER_MI), ifelse(nzchar(surface), paste0(", ", surface), "")))
      tr <- filter(t, !is_service); sv <- filter(t, is_service)
      opts <- pathOptions(pane = "trails", interactive = park_view)
      if (nrow(sv)) m <- addPolylines(m, data = sv, color = COL$service, weight = 2, opacity = 0.85, dashArray = "5 4",
                                      smoothFactor = 0.5, label = if (park_view) ~label else NULL,
                                      group = "Trails", options = opts)
      if (nrow(tr)) m <- addPolylines(m, data = tr, color = COL$trail, weight = if (park_view) 3 else 2, opacity = 0.9,
                                      smoothFactor = 0.5, label = if (park_view) ~label else NULL,
                                      group = "Trails", options = opts)
    } else if (park_view) {
      # park view: split each line into run and unrun pieces
      pieces <- split_by_coverage(c_df$trail_id, thr)
      if (!is.null(pieces)) {
        g <- c_df %>% group_by(park, name) %>% summarise(gfrac = sum(done_m) / sum(length_m), glen = sum(length_m), .groups = "drop")
        info <- st_drop_geometry(trails_ll) %>% select(trail_id, name, park) %>% inner_join(g, by = c("name", "park"))
        pieces <- pieces %>% left_join(info, by = "trail_id") %>%
          mutate(label = sprintf("%s: %s of %s mi run", name, pct(gfrac), fmt1(glen / M_PER_MI)))
        todo <- filter(pieces, !covered); done <- filter(pieces, covered)
        if (nrow(todo)) m <- addPolylines(m, data = todo, color = COL$todo, weight = 3, opacity = 0.8, label = ~label,
                                          smoothFactor = 0.5, group = "Trails", options = pathOptions(pane = "trails"))
        if (nrow(done)) m <- addPolylines(m, data = done, color = COL$done, weight = 4, opacity = 0.95, label = ~label,
                                          smoothFactor = 0.5, group = "Trails", options = pathOptions(pane = "trails"))
      }
    } else {
      # overview: whole segments coloured by class, no per-piece splitting (faster)
      t <- trails_ll %>% inner_join(select(c_df, trail_id, frac), by = "trail_id") %>% mutate(class = cls(frac))
      colours <- c(done = COL$done, partial = COL$partial, todo = COL$todo)
      for (k in c("todo", "partial", "done")) {
        tk <- filter(t, class == k)
        if (nrow(tk)) m <- addPolylines(m, data = tk, color = colours[[k]], weight = if (k == "done") 2.5 else 2,
                                        opacity = 0.85, group = "Trails",
                                        options = pathOptions(pane = "trails", interactive = FALSE))
      }
    }

    # park boundaries
    if (park_view) {
      pk <- filter(parks_ll, park == .env$park)
      if (nrow(pk)) m <- addPolygons(m, data = pk, fill = FALSE, color = COL$ink, weight = 1.5, dashArray = "4 4",
                                     group = "Park boundaries", options = pathOptions(pane = "parks", interactive = FALSE))
    } else {
      m <- addPolygons(m, data = parks_ll, fill = FALSE, color = COL$ink, weight = 1, opacity = 0.35, dashArray = "4 4",
                       group = "Park boundaries", options = pathOptions(pane = "parks", interactive = FALSE))
    }

    legend_for(m, mode)
  }

  view_bbox <- function(park) {
    if (nzchar(park)) {
      bb <- st_bbox(filter(trails_ll, park == .env$park))
      pk <- filter(parks_ll, park == .env$park)
      if (nrow(pk)) {
        pb <- st_bbox(pk)
        bb <- c(xmin = min(bb["xmin"], pb["xmin"]), ymin = min(bb["ymin"], pb["ymin"]),
                xmax = max(bb["xmax"], pb["xmax"]), ymax = max(bb["ymax"], pb["ymax"]))
      }
      bb
    } else st_bbox(trails_ll)
  }

  output$map <- renderLeaflet({
    bb <- view_bbox(isolate(input$park))
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addMapPane("parks", zIndex = 405) %>%
      addMapPane("trails", zIndex = 410) %>%
      addMapPane("runs", zIndex = 420) %>%
      addMapPane("route", zIndex = 425) %>%
      addMapPane("highlight", zIndex = 430)
    for (nm in names(BASEMAPS)) {
      m <- addProviderTiles(m, BASEMAPS[[nm]], group = nm)
    }
    m <- m %>%
      addLayersControl(baseGroups = names(BASEMAPS),
                       overlayGroups = c("Trails", "Park boundaries"),
                       position = "topright",
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      addPolylines(data = runs_ll, color = COL$run, weight = 2, opacity = 0.6, group = "runs",
                   smoothFactor = 0.5, options = pathOptions(pane = "runs", interactive = FALSE)) %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

    if (!isolate(input$showruns)) m <- hideGroup(m, "runs")

    draw_trails(m, isolate(cov()), isolate(input$park), isolate(input$thr), isolate(input$view))
  })

  # redraw trails and boundary when scope, threshold or colour mode changes
  observeEvent(list(input$park, input$thr, input$svc, input$view), {
    proxy <- leafletProxy("map") %>% clearGroup("Trails") %>% clearGroup("Park boundaries") %>% clearGroup("highlight")
    draw_trails(proxy, cov(), input$park, input$thr, input$view)
  }, ignoreInit = TRUE)

  # zoom only when the park changes (not on every slider move)
  observeEvent(input$park, {
    bb <- view_bbox(input$park)
    leafletProxy("map") %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
  }, ignoreInit = TRUE)

  observeEvent(input$showruns, {
    proxy <- leafletProxy("map")
    if (input$showruns) showGroup(proxy, "runs") else hideGroup(proxy, "runs")
  }, ignoreInit = TRUE)

  # ---- route suggestion (off by default; park view only)
  # The start is where most of your runs began, or a map click. The route is
  # recomputed when the park, match distance, service-road setting, length or
  # loop option changes; the length slider is debounced so dragging is smooth.
  route_start <- reactiveVal(NULL)                 # projected x, y of a map click
  observeEvent(input$park, route_start(NULL), ignoreInit = TRUE)
  observeEvent(input$map_click, {
    if (!isTRUE(input$route) || !nzchar(input$park)) return()
    p <- st_transform(st_sfc(st_point(c(input$map_click$lng, input$map_click$lat)), crs = 4326), st_crs(trails_proj))
    route_start(st_coordinates(p)[1, 1:2])
  })

  run_starts <- local({                           # first point of every run, projected, computed once
    xy <- NULL
    function() {
      if (is.null(xy)) {
        first <- runs_ll[!duplicated(runs_ll$activity_id), ]
        if (!nrow(first)) return(xy <<- matrix(numeric(0), ncol = 2))
        c0 <- st_coordinates(st_transform(first, st_crs(trails_proj)))
        xy <<- c0[!duplicated(c0[, "L1"]), c("X", "Y"), drop = FALSE]
      }
      xy
    }
  })

  route_mi <- debounce(reactive(input$route_mi), 600)

  route <- reactive({
    req(isTRUE(input$route), nzchar(input$park))
    G <- build_route_graph(input$park, trails_proj, node_dist, RP$route_snap_m)
    if (is.null(G)) return(NULL)
    unrun <- vertex_unrun(G, node_dist, scope()$trail_id, input$thr)
    start <- start_vertex(G, route_start(), run_starts())
    r <- suggest_route(G, unrun, start, route_mi() * M_PER_MI, isTRUE(input$route_loop))
    if (is.null(r)) return(list(none = TRUE))
    r$ll <- st_coordinates(st_transform(st_sfc(st_linestring(r$xy), crs = st_crs(trails_proj)), 4326))[, 1:2]
    r
  })

  observe({
    proxy <- leafletProxy("map") %>% clearGroup("route")
    if (!isTRUE(input$route) || !nzchar(input$park)) return()
    r <- route()
    if (is.null(r) || isTRUE(r$none)) return()
    ll <- r$ll
    proxy %>%
      addPolylines(lng = ll[, 1], lat = ll[, 2], color = "#ffffff", weight = 8, opacity = 0.9, group = "route",
                   options = pathOptions(pane = "route", interactive = FALSE)) %>%
      addPolylines(lng = ll[, 1], lat = ll[, 2], color = COL$route, weight = 4, opacity = 0.95, group = "route",
                   options = pathOptions(pane = "route", interactive = FALSE)) %>%
      addCircleMarkers(lng = ll[1, 1], lat = ll[1, 2], radius = 7, color = "#ffffff", weight = 2, fillColor = COL$route,
                       fillOpacity = 1, label = "Start", group = "route", options = pathOptions(pane = "route"))
  })

  output$route_info <- renderUI({
    if (!nzchar(input$park)) return(p(class = "small text-muted", "Choose a park to get a route."))
    r <- route()
    if (is.null(r)) return(p(class = "small text-muted", "This park has no trails in the trail layer."))
    if (isTRUE(r$none)) return(p(class = "small", "Nothing left to run within reach of this start. Try a longer route or another start."))
    st <- r$steps[r$steps$len_m >= 50, ]
    tagList(
      p(class = "mb-1", strong(sprintf("%s mi %s", fmt1(r$length_m / M_PER_MI), if (r$loop) "loop" else "one way")),
        sprintf(", %s mi of it new", fmt1(r$new_m / M_PER_MI))),
      tags$ol(class = "route-steps",
              lapply(seq_len(nrow(st)), function(i)
                tags$li(sprintf("%s, %s mi%s", st$name[i], fmt1(st$len_m[i] / M_PER_MI),
                                if (st$new_m[i] >= 50) sprintf(" (%s new)", fmt1(st$new_m[i] / M_PER_MI)) else "")))),
      downloadButton("route_gpx", "GPX", class = "btn-sm")
    )
  })

  output$route_gpx <- downloadHandler(
    filename = function() sprintf("route_%s_%smi.gpx", gsub("[^A-Za-z0-9]+", "_", input$park), fmt1(route_mi())),
    content = function(f) {
      r <- route(); req(!is.null(r$ll))
      write_gpx(r$ll, sprintf("%s, %s mi", input$park, fmt1(r$length_m / M_PER_MI)), f)
    }
  )
}

shinyApp(ui, server)
