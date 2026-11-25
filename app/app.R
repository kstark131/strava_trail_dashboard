# ==============================================================================
# SHINY APP: trail completion dashboard
# ==============================================================================
# Run from the project folder:  shiny::runApp("app")
# Reads data/app_data.rds written by 03_match_runs_to_trails.R.
#
# Layout: sidebar (park, match distance, map options, route suggestion, stats,
# table) and a leaflet map. Every input feeds the `cov()` reactive; everything
# else derives from it. Route suggestions live in route_suggest.R, exclusions
# in exclusions.R.
#
# Set APP_PASSWORD (and optionally APP_USER) in the environment to put a login
# screen in front of the app. The hosted copy (deploy_connect_cloud.R) always
# requires one; on Connect Cloud it is set under the content's Variables.

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
source("route_suggest.R")
source("exclusions.R")

sf_use_s2(FALSE)
M_PER_MI <- 1609.344
OUTSIDE  <- "Outside park boundaries"

# ../data is the pipeline's output when running locally; data/ (inside app/)
# is the trimmed copy deploy_connect_cloud.R uploads, used only when hosted.
app_path <- if (file.exists("../data/app_data.rds")) "../data/app_data.rds" else "data/app_data.rds"
D <- readRDS(app_path)

trails_ll   <- D$trails_ll
trails_proj <- D$trails_proj
node_dist   <- D$node_dist
parks_ll    <- D$parks_ll
runs_ll     <- D$runs_ll
park_choices <- c("All parks" = "", sort(unique(trails_ll$park)))

# route and exclusion settings come from 00_config.R via app_data; defaults for older bundles
RP <- modifyList(list(route_snap_m = 15, route_default_mi = 5, route_max_mi = 20,
                      exclude_rules_default = c("closed_land", "closed", "proposed"),
                      rule_closed_pattern = "closed|no public|not open|golf|private road",
                      rule_proposed_pattern = "proposed|future trail", rule_road_types = "Paved Road"),
                 as.list(D$params))
trail_source <- if (is.null(D$trail_source)) "file" else D$trail_source

# exclusion rules are evaluated once; the switches only choose which apply
HITS <- rule_hits(trails_proj, RP, OUTSIDE)
rule_mi <- vapply(HITS, function(h) sum(trails_proj$length_m[h & !trails_proj$is_service]) / M_PER_MI, 0)
rule_choices <- setNames(names(RULE_LABELS), sprintf("%s (%s mi)", RULE_LABELS, formatC(rule_mi, format = "f", digits = 1)))
rule_choices <- rule_choices[rule_mi > 0 | names(rule_choices) %in% RP$exclude_rules_default]
# The deployed bundle is marked hosted. Files the app writes there vanish on
# restart, so exclusion edits are downloaded instead of saved.
HOSTED <- isTRUE(D$hosted)

COL <- list(done = "#1c8a5a", partial = "#d9930f", todo = "#c4463b", run = "#3a5fcd", ink = "#1f2a26",
            trail = "#8a4b14", service = "#6b6b6b", route = "#6a2c91", excluded = "#8f8f8f", draft = "#d1006f")

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
  .map-stats { display: none; background: rgba(255,255,255,.92); padding: 4px 9px; border-radius: 8px;
               font-size: 13px; box-shadow: 0 1px 4px rgba(0,0,0,.2); }

  /* phones (below bslib's sidebar breakpoint): the map fills the screen, the
     sidebar slides over it, and a stats line sits on the map instead */
  @media (max-width: 575.98px) {
    .map-stats { display: block; }
    .sidebar-note { display: none; }
    .bslib-value-box .value-box-title { font-size: .8rem; }
    .bslib-value-box .value-box-value { font-size: 1.25rem; }
    .bslib-value-box .value-box-area { padding: .5rem .75rem !important; }
    .form-control, .selectize-input, .selectize-input input { font-size: 16px; }  /* stops iOS zooming into inputs */
    .info.legend { font-size: 11px; padding: 4px 6px; line-height: 1.3; }
    .leaflet-bar a, .leaflet-touch .leaflet-bar a { width: 36px; height: 36px; line-height: 36px; }
  }
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

# Part of a projected LINESTRING between fractions `from` and `to` of its length.
line_substring <- function(l, from, to) {
  p <- unclass(l)[, 1:2, drop = FALSE]
  s <- c(0, cumsum(sqrt(rowSums(diff(p)^2)))); s <- s / s[length(s)]
  ends <- cbind(approx(s, p[, 1], c(from, to), ties = list("ordered", mean))$y,
                approx(s, p[, 2], c(from, to), ties = list("ordered", mean))$y)
  st_linestring(rbind(ends[1, ], p[s > from & s < to, , drop = FALSE], ends[2, ]))
}

# Run and unrun pieces of every trail at a threshold (lat/lon). Node i covers
# the bin [(i-1)/n, i/n] of the line, so runs of covered nodes become
# [from, to]. Segments that are all run or all unrun keep their geometry; only
# mixed ones are cut. Cached per threshold, since the overview needs them all.
piece_cache <- new.env()
coverage_pieces <- function(thr) {
  key <- as.character(thr)
  if (is.null(piece_cache[[key]])) {
    nd  <- node_dist[order(node_dist$trail_id, node_dist$node_i), ]
    cov <- nd$dist_m <= thr
    brk <- c(TRUE, diff(nd$trail_id) != 0 | diff(cov) != 0)       # first node of each run
    last <- c(which(brk)[-1] - 1L, nrow(nd))                        # last node of each run
    iv <- data.frame(trail_id = nd$trail_id[brk], covered = cov[brk],
                     from = (nd$node_i[brk] - 1) / nd$n_nodes[brk], to = nd$node_i[last] / nd$n_nodes[brk])
    g <- st_geometry(trails_proj)[match(iv$trail_id, trails_proj$trail_id)]
    geoms <- unclass(g)
    for (i in which(iv$from > 0 | iv$to < 1)) geoms[[i]] <- line_substring(g[[i]], iv$from[i], iv$to[i])
    piece_cache[[key]] <- st_transform(st_sf(iv, geometry = st_sfc(geoms, crs = st_crs(trails_proj))), 4326)
  }
  piece_cache[[key]]
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  fillable_mobile = TRUE,          # let the map fill the phone screen
  title = span("Trail completion", span(class = "subtitle", textOutput("subtitle", inline = TRUE))),
  theme = bs_theme(version = 5, bg = "#fbfaf6", fg = COL$ink, primary = "#1f6f4a", secondary = "#6b7c74",
                   base_font = font_google("IBM Plex Sans"), "border-radius" = "0.6rem") |>
          bs_add_rules(CSS),
  sidebar = sidebar(
    id = "sb",
    width = 400,                   # open on desktop, a slide-over panel on phones
    # tells the server whether this is a phone, so it can close the panel after a choice
    tags$script(HTML("$(document).on('shiny:connected', function() {
      Shiny.setInputValue('is_mobile', window.matchMedia('(max-width: 575.98px)').matches);
    });")),
    selectInput("park", "Park", choices = park_choices, selected = ""),
    sliderInput("thr", "Match distance (m)", min = 5, max = 60, value = D$params$default_match_m, step = 5),
    checkboxInput("svc", "Include service roads", value = FALSE),
    accordion(
      open = FALSE,
      accordion_panel(
        "Map options",
        radioButtons("view", "Trail colours",
                     choices = c("Run / not run" = "completion",
                                 "By trail: completed, partly run, not run" = "classes",
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
      ),
      accordion_panel(
        "Exclusions",
        checkboxGroupInput("rules", "Leave out of the totals", choices = rule_choices,
                           selected = intersect(RP$exclude_rules_default, rule_choices)),
        checkboxInput("edit", "Review and edit on the map", value = FALSE),
        conditionalPanel(
          "input.edit",
          radioButtons("edit_tool", NULL,
                       choices = c("Tap a trail to exclude or restore it" = "segment",
                                   "Tap points to outline an area to exclude" = "area")),
          selectInput("edit_reason", "Reason for new exclusions", choices = EXCL_REASONS),
          conditionalPanel(
            "input.edit_tool == 'area'",
            div(class = "d-flex gap-2 mb-2",
                actionButton("area_done", "Finish area", class = "btn-sm btn-primary"),
                actionButton("area_clear", "Clear points", class = "btn-sm"))
          ),
          uiOutput("excl_info"),
          div(class = "d-flex gap-2",
              if (!HOSTED) actionButton("excl_save", "Save", class = "btn-sm btn-primary"),
              actionButton("excl_undo", "Undo last", class = "btn-sm"),
              downloadButton("excl_dl", "Download", class = "btn-sm"))
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
    p(class = "small text-muted mt-2 sidebar-note",
      sprintf("%s runs in the study area, built %s. Each trail is sampled every %d m; a sample counts as run when a track passes within the match distance. A trail is completed at %d%% of samples.",
              fmt0(D$totals$runs_in_area), format(D$built), as.integer(D$params$node_spacing_m), round(100 * D$params$done_fraction)),
      if (trail_source == "osm") "Trail and park data \u00a9 OpenStreetMap contributors (ODbL)." else NULL)
  ),
  leafletOutput("map", height = "100%")
)

# "Follow my location" for phones. The ◎ button toggles live GPS tracking
# (map.locate with watch: true; browsers allow it only over HTTPS, which
# Connect Cloud provides). Each fix redraws a blue dot with an accuracy circle.
# The first fix centres the map; later ones only move the dot, so you can pan
# away while it keeps tracking, and short GPS dropouts don't stop it. The first
# fix is also sent to the server as input$my_location, which moves the route
# start there when route suggestions are on. Tap the button again to stop
# (tracking costs battery).
LOCATE_JS <- "function(el, x) {
  var map = this, me = null, first = false;
  map._trackBtnStyle = function(on) {
    var b = document.querySelector('#' + el.id + ' .locate-btn');
    if (b) b.parentElement.parentElement.style.color = on ? '#1a73e8' : '';
  };
  map._toggleTracking = function() {
    if (map._tracking) {
      map.stopLocate(); map._tracking = false; map._trackBtnStyle(false);
      if (me) { map.removeLayer(me); me = null; }
    } else {
      map._tracking = true; first = true; map._trackBtnStyle(true);
      map.locate({watch: true, enableHighAccuracy: true, maximumAge: 5000});
    }
  };
  map.on('locationfound', function(e) {
    if (me) map.removeLayer(me);
    me = L.layerGroup([
      L.circle(e.latlng, {radius: e.accuracy, weight: 1, color: '#1a73e8', fillOpacity: 0.1, interactive: false}),
      L.circleMarker(e.latlng, {radius: 7, weight: 2, color: '#fff', fillColor: '#1a73e8', fillOpacity: 1, interactive: false})
    ]).addTo(map);
    if (first) {
      first = false;
      map.setView(e.latlng, Math.max(map.getZoom(), 16));
      Shiny.setInputValue('my_location', {lat: e.latlng.lat, lng: e.latlng.lng}, {priority: 'event'});
    }
  });
  // A denied permission, or no fix at all, stops tracking with a message. Once
  // there is a fix, brief dropouts (tree cover, canyons) are ignored: the dot
  // stays at the last position and tracking carries on.
  map.on('locationerror', function(e) {
    if (!map._tracking || (e.code !== 1 && me)) return;
    map.stopLocate(); map._tracking = false; map._trackBtnStyle(false);
    alert(e.code === 1 ? 'Location is blocked. Allow location access for this site in your browser settings.'
                       : 'Location not available: ' + e.message);
  });
}"

stats_html <- function(park, c_df) {
  l <- sum(c_df$length_m); f <- if (l > 0) sum(c_df$done_m) / l else 0
  sprintf("<b>%s</b> &middot; %s of %s mi run", htmltools::htmlEscape(if (nzchar(park)) park else "All parks"),
          pct(f), fmt1(l / M_PER_MI))
}

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
server <- function(input, output, session) {

  close_sidebar_on_mobile <- function() if (isTRUE(input$is_mobile)) toggle_sidebar("sb", open = FALSE)

  # ---- exclusions: rules switched on, plus your own edits (exclusions.R)
  manual <- reactiveVal(read_exclusions())
  history <- reactiveVal(list())                       # earlier states, for "Undo last"
  set_manual <- function(x) { history(c(history(), list(manual()))); manual(x) }
  excl <- reactive(apply_exclusions(trails_proj, HITS, input$rules, manual()))

  # trails for the current park and service-road setting, before and after exclusions
  in_view <- reactive({
    t <- trails_ll
    if (!input$svc) t <- filter(t, !is_service)
    if (nzchar(input$park)) t <- filter(t, park == input$park)
    t
  })
  scope <- reactive(filter(in_view(), !trail_id %in% excl()$trail_id))
  excluded_view <- reactive(in_view() %>% inner_join(excl(), by = "trail_id"))

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
      close_sidebar_on_mobile()
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
    } else if (mode == "classes") {
      addLegend(m, position = "bottomright", layerId = "legend", opacity = 0.9,
                colors = c(COL$done, COL$partial, COL$todo, COL$excluded, COL$run),
                labels = c("Trail completed", "Trail partly run", "Trail not run", "Excluded", "My run tracks"))
    } else {
      addLegend(m, position = "bottomright", layerId = "legend", opacity = 0.9,
                colors = c(COL$done, COL$todo, COL$excluded, COL$run), labels = c("Run", "Not run yet", "Excluded", "My run tracks"))
    }
  }

  # draw_trails() works on both a fresh leaflet() object (initial render) and a
  # leafletProxy (updates), so the drawing code lives in one place.
  # In edit mode (park view only) every piece gets a layerId, so a tap on the
  # map reports which segment it was: "seg_<trail_id>_<n>" or "ex_<trail_id>".
  draw_trails <- function(m, c_df, park, thr, mode, ex = NULL, edit = FALSE) {
    park_view <- nzchar(park)
    edit <- edit && park_view
    if (edit) mode <- "completion"
    # tap a trail for its name and progress (hover labels don't work on touch
    # screens); off while editing, where a tap toggles the trail instead
    pop <- if (park_view && !edit) ~label else NULL

    if (mode == "plain") {
      # trails only: one colour, service roads dashed, no completion data
      t <- trails_ll %>% filter(trail_id %in% c_df$trail_id) %>%
        mutate(label = sprintf("%s: %s mi%s", name, fmt1(length_m / M_PER_MI), ifelse(nzchar(surface), paste0(", ", surface), "")))
      tr <- filter(t, !is_service); sv <- filter(t, is_service)
      opts <- pathOptions(pane = "trails", interactive = park_view)
      if (nrow(sv)) m <- addPolylines(m, data = sv, color = COL$service, weight = 2, opacity = 0.85, dashArray = "5 4",
                                      smoothFactor = 0.5, label = if (park_view) ~label else NULL, popup = pop,
                                      group = "Trails", options = opts)
      if (nrow(tr)) m <- addPolylines(m, data = tr, color = COL$trail, weight = if (park_view) 3 else 2, opacity = 0.9,
                                      smoothFactor = 0.5, label = if (park_view) ~label else NULL, popup = pop,
                                      group = "Trails", options = opts)
    } else if (mode == "completion") {
      # where you have run (muted) and, on top, where you have not (highlighted);
      # no partly-run class: each trail is cut where your coverage stops
      pieces <- coverage_pieces(thr) %>% filter(trail_id %in% c_df$trail_id)
      if (park_view) {
        g <- c_df %>% group_by(park, name) %>% summarise(gfrac = sum(done_m) / sum(length_m), glen = sum(length_m), .groups = "drop")
        info <- st_drop_geometry(trails_ll) %>% select(trail_id, name, park) %>% inner_join(g, by = c("name", "park"))
        pieces <- pieces %>% left_join(info, by = "trail_id") %>%
          mutate(label = sprintf("%s: %s of %s mi run", name, pct(gfrac), fmt1(glen / M_PER_MI)))
        opts <- pathOptions(pane = "trails")
      } else {
        # overview: one multi-line per class keeps the browser fast with every park on screen
        pieces <- pieces %>% group_by(covered) %>% summarise(do_union = FALSE, .groups = "drop")
        opts <- pathOptions(pane = "trails", interactive = FALSE)
      }
      if (edit) pieces <- mutate(pieces, label = paste0(label, ". Tap to exclude"),
                                 lid = paste0("seg_", trail_id, "_", seq_len(n())))
      done <- filter(pieces, covered); todo <- filter(pieces, !covered)
      lab <- if (park_view) ~label else NULL
      if (nrow(done)) m <- addPolylines(m, data = done, color = COL$done, weight = if (edit) 6 else if (park_view) 3 else 2,
                                        opacity = 0.7, label = lab, popup = pop, layerId = if (edit) done$lid else NULL,
                                        smoothFactor = 0.5, group = "Trails", options = opts)
      if (nrow(todo)) m <- addPolylines(m, data = todo, color = COL$todo, weight = if (edit) 6 else if (park_view) 4 else 2.5,
                                        opacity = 0.95, label = lab, popup = pop, layerId = if (edit) todo$lid else NULL,
                                        smoothFactor = 0.5, group = "Trails", options = opts)
    } else {
      # by trail: whole segments coloured by their named trail's share run
      # (completed / partly run / not run). Park view keeps hover labels.
      g <- c_df %>% group_by(park, name) %>% summarise(gfrac = sum(done_m) / sum(length_m), glen = sum(length_m), .groups = "drop")
      t <- trails_ll %>% filter(trail_id %in% c_df$trail_id) %>% inner_join(g, by = c("park", "name")) %>%
        mutate(class = cls(gfrac), label = sprintf("%s: %s of %s mi run", name, pct(gfrac), fmt1(glen / M_PER_MI)))
      colours <- c(done = COL$done, partial = COL$partial, todo = COL$todo)
      for (k in c("todo", "partial", "done")) {
        tk <- filter(t, class == k)
        if (nrow(tk)) m <- addPolylines(m, data = tk, color = colours[[k]], weight = if (park_view) 3.5 else if (k == "done") 2.5 else 2,
                                        opacity = 0.85, label = if (park_view) ~label else NULL, popup = pop, smoothFactor = 0.5, group = "Trails",
                                        options = pathOptions(pane = "trails", interactive = park_view))
      }
    }

    # excluded segments: grey and dashed, on their own toggle in the layer control
    if (!is.null(ex) && nrow(ex) && mode != "plain") {
      if (park_view) {
        ex <- mutate(ex, label = sprintf("%s: excluded (%s)%s", name, reason, if (edit) ". Tap to restore" else ""))
        m <- addPolylines(m, data = ex, color = COL$excluded, weight = if (edit) 6 else 3, opacity = 0.85, dashArray = "6 5",
                          label = ~label, popup = pop, layerId = if (edit) paste0("ex_", ex$trail_id) else NULL,
                          smoothFactor = 0.5, group = "Excluded", options = pathOptions(pane = "trails"))
      } else {
        m <- addPolylines(m, data = st_sf(geometry = st_combine(st_geometry(ex))), color = COL$excluded, weight = 2,
                          opacity = 0.8, dashArray = "4 4", smoothFactor = 0.5, group = "Excluded",
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
                       overlayGroups = c("Trails", "Excluded", "Park boundaries"),
                       position = "topright",
                       options = layersControlOptions(collapsed = TRUE)) %>%
      addScaleBar(position = "bottomleft") %>%
      addEasyButton(easyButton(icon = "<span class='locate-btn' style='font-size:18px'>&#9678;</span>",
                               title = "Follow my location (tap again to stop)",
                               onClick = htmlwidgets::JS("function(btn, map) { map._toggleTracking(); }"))) %>%
      htmlwidgets::onRender(LOCATE_JS) %>%
      addControl(stats_html(isolate(input$park), isolate(cov())), position = "topleft",
                 layerId = "stats", className = "map-stats") %>%
      addPolylines(data = runs_ll, color = COL$run, weight = 2, opacity = 0.6, group = "runs",
                   smoothFactor = 0.5, options = pathOptions(pane = "runs", interactive = FALSE)) %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

    if (!isolate(input$showruns)) m <- hideGroup(m, "runs")

    draw_trails(m, isolate(cov()), isolate(input$park), isolate(input$thr), isolate(input$view),
                isolate(excluded_view()), isolate(isTRUE(input$edit)))
  })

  # redraw trails and boundary when scope, threshold or colour mode changes
  observeEvent(list(input$park, input$thr, input$svc, input$view, excl(), input$edit), {
    proxy <- leafletProxy("map") %>% clearGroup("Trails") %>% clearGroup("Excluded") %>%
      clearGroup("Park boundaries") %>% clearGroup("highlight")
    draw_trails(proxy, cov(), input$park, input$thr, input$view, excluded_view(), isTRUE(input$edit))
  }, ignoreInit = TRUE)

  # zoom only when the park changes (not on every slider move); on a phone,
  # also close the panel so the map is in view
  observeEvent(input$park, {
    bb <- view_bbox(input$park)
    leafletProxy("map") %>% fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    close_sidebar_on_mobile()
  }, ignoreInit = TRUE)
  observeEvent(input$edit, if (isTRUE(input$edit) && nzchar(input$park)) close_sidebar_on_mobile(), ignoreInit = TRUE)

  # the stats line on the map (shown on phones only, see .map-stats)
  observeEvent(list(input$park, cov()), {
    leafletProxy("map") %>% removeControl("stats") %>%
      addControl(stats_html(input$park, cov()), position = "topleft", layerId = "stats", className = "map-stats")
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
    if (!isTRUE(input$route) || !nzchar(input$park) || isTRUE(input$edit)) return()   # taps edit exclusions instead
    p <- st_transform(st_sfc(st_point(c(input$map_click$lng, input$map_click$lat)), crs = 4326), st_crs(trails_proj))
    route_start(st_coordinates(p)[1, 1:2])
  })

  observeEvent(input$my_location, {
    if (!isTRUE(input$route) || !nzchar(input$park) || isTRUE(input$edit)) return()
    p <- st_transform(st_sfc(st_point(c(input$my_location$lng, input$my_location$lat)), crs = 4326), st_crs(trails_proj))
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
    blocked <- excl()$trail_id[excl()$blocking]                 # closed or private: never walk these
    start <- start_vertex(G, route_start(), run_starts(), blocked)
    r <- suggest_route(G, unrun, start, route_mi() * M_PER_MI, isTRUE(input$route_loop), blocked)
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

  # ---- editing exclusions (park view; see exclusions.R for the file format)
  observeEvent(input$edit, {
    if (isTRUE(input$edit) && !nzchar(input$park))
      showNotification("Choose a park to edit its exclusions.", type = "message")
  })

  observeEvent(input$map_shape_click, {
    id <- input$map_shape_click$id
    if (!isTRUE(input$edit) || input$edit_tool != "segment" || is.null(id) || !grepl("^(seg|ex)_", id)) return()
    tid <- as.integer(sub("^[a-z]+_([0-9]+).*$", "\\1", id))
    seg <- trails_ll[trails_ll$trail_id == tid, ]
    set_manual(toggle_segment(manual(), seg, tid %in% excl()$trail_id, input$edit_reason))
  })

  area_pts <- reactiveVal(matrix(numeric(0), ncol = 2))
  observeEvent(input$map_click, {
    if (!isTRUE(input$edit) || input$edit_tool != "area" || !nzchar(input$park)) return()
    area_pts(rbind(area_pts(), c(input$map_click$lng, input$map_click$lat)))
  })
  observeEvent(list(input$area_clear, input$park, input$edit, input$edit_tool), area_pts(matrix(numeric(0), ncol = 2)),
               ignoreInit = TRUE)
  observeEvent(input$area_done, {
    p <- area_pts()
    if (nrow(p) < 3) return(showNotification("Tap at least three points around the area first.", type = "warning"))
    poly <- st_sfc(st_polygon(list(rbind(p, p[1, ]))), crs = 4326)
    set_manual(rbind(manual(), st_sf(kind = "area", action = "exclude", uid = NA_character_, reason = input$edit_reason,
                                     name = "", park = input$park, added = format(Sys.Date()), geometry = poly)))
    area_pts(matrix(numeric(0), ncol = 2))
  })

  # the outline being drawn, and saved areas while editing
  observe({
    proxy <- leafletProxy("map") %>% clearGroup("draft")
    if (!isTRUE(input$edit) || !nzchar(input$park)) return()
    a <- manual()[manual()$kind == "area" & manual()$park %in% input$park, ]
    if (nrow(a)) proxy <- addPolygons(proxy, data = a, color = COL$excluded, weight = 1.5, fillOpacity = 0.15,
                                      label = ~paste("Excluded area:", reason), group = "draft",
                                      options = pathOptions(pane = "parks", interactive = FALSE))
    p <- area_pts()
    if (nrow(p)) {
      if (nrow(p) > 1) proxy <- addPolylines(proxy, lng = c(p[, 1], p[1, 1]), lat = c(p[, 2], p[1, 2]), color = COL$draft,
                                             weight = 2, dashArray = "4 4", group = "draft", options = pathOptions(pane = "highlight"))
      addCircleMarkers(proxy, lng = p[, 1], lat = p[, 2], radius = 5, color = COL$draft, fillOpacity = 1, weight = 1,
                       group = "draft", options = pathOptions(pane = "highlight"))
    }
  })

  observeEvent(input$excl_undo, {
    h <- history(); if (!length(h)) return()
    manual(h[[length(h)]]); history(h[-length(h)])
  })
  observeEvent(input$excl_save, {
    write_exclusions(manual())
    history(list())
    showNotification(sprintf("Saved %d exclusions to app/%s", nrow(manual()), EXCL_FILE), type = "message")
  })
  output$excl_dl <- downloadHandler(
    filename = function() EXCL_FILE,
    content = function(f) write_exclusions(manual(), f)
  )

  output$excl_info <- renderUI({
    e <- excluded_view(); m <- manual()
    unsaved <- length(history()) > 0
    tagList(
      p(class = "small mb-1", sprintf("%s mi excluded here (%d segments); %d of your own edits%s.",
                                     fmt1(sum(e$length_m) / M_PER_MI), nrow(e), nrow(m),
                                     if (unsaved) ", not saved yet" else "")),
      if (HOSTED) p(class = "small text-muted",
                    "Edits on the hosted app last until it restarts. Download the file, put it in app/ and publish again to keep them.")
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

# Login screen. APP_PASSWORD (and APP_USER, default "me") come from the
# environment: Connect Cloud's Variables when hosted, optionally .Renviron
# locally. The hosted copy fails closed: without a password it shows only
# setup instructions, never the data.
if (HOSTED && !nzchar(Sys.getenv("APP_PASSWORD"))) {
  ui <- fluidPage(
    h3("Login not configured"),
    p("Add APP_USER and APP_PASSWORD under Settings > Variables for this content on Connect Cloud,",
      "then restart it. The dashboard stays hidden until then.")
  )
  server <- function(input, output, session) {}
} else if (nzchar(Sys.getenv("APP_PASSWORD"))) {
  library(shinymanager)
  credentials <- data.frame(user = Sys.getenv("APP_USER", "me"), password = Sys.getenv("APP_PASSWORD"),
                            stringsAsFactors = FALSE)
  ui <- secure_app(ui)
  app_server <- server
  server <- function(input, output, session) {
    secure_server(check_credentials = check_credentials(credentials))
    app_server(input, output, session)
  }
}

shinyApp(ui, server)
