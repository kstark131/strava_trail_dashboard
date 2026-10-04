# ==============================================================================
# SHINY APP: trail completion dashboard
# ==============================================================================
# Run from the project folder:  shiny::runApp("app")
# Reads data/app_data.rds written by 03_match_runs_to_trails.R.
#
# Layout: sidebar (park, match distance, toggles, stats, table) and a leaflet
# map. Every input feeds the `cov()` reactive; everything else derives from it.

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(lwgeom)     # st_linesubstring, used to draw run and unrun pieces of a trail

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

COL <- list(done = "#1c8a5a", partial = "#d9930f", todo = "#c4463b", run = "#3a5fcd", ink = "#1f2a26")

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
  
  # Process st_linesubstring row-by-row to prevent vectorization errors
  geom_list <- lapply(seq_len(nrow(iv)), function(i) {
    st_linesubstring(g[i], iv$from[i], iv$to[i])
  })
  
  pieces <- st_sf(iv, geometry = do.call(c, geom_list)) %>%
    st_transform(4326)
  
  pieces
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Trail completion",
  theme = bs_theme(version = 5, base_font = font_google("IBM Plex Sans"), primary = COL$done),
  sidebar = sidebar(
    width = 400,
    selectInput("park", "Park", choices = park_choices, selected = ""),
    sliderInput("thr", "Match distance (m)", min = 5, max = 60, value = D$params$default_match_m, step = 5),
    checkboxInput("svc", "Include service roads", value = FALSE),
    checkboxInput("showruns", "Show runs on map", value = FALSE),
    layout_columns(
      value_box("Miles run", textOutput("s_run"), p(textOutput("s_run_l", inline = TRUE), class = "small text-muted")),
      value_box("Trail miles", textOutput("s_trail")),
      value_box("Trail miles completed", textOutput("s_done"), theme = "primary"),
      value_box("Completed", textOutput("s_pct"), p(textOutput("s_pct_l", inline = TRUE), class = "small"), theme = "primary"),
      col_widths = c(6, 6, 6, 6)
    ),
    div(class = "d-flex gap-2 align-items-center",
        textInput("q", NULL, placeholder = "Filter by name", width = "100%"),
        downloadButton("csv", "CSV", class = "btn-sm")),
    DTOutput("tbl"),
    p(class = "small text-muted mt-2",
      sprintf("%s runs in the study area, built %s. Each trail is sampled every %d m; a sample counts as run when a track passes within the match distance. A trail is completed at %d%% of samples.",
              fmt0(D$totals$runs_in_area), format(D$built), D$params$node_spacing_m, round(100 * D$params$done_fraction)))
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
      mutate(run_mi = coalesce(run_mi, 0), n_runs = coalesce(n_runs, 0L))
  })

  # ---- stats
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
      n <- if (nrow(p)) coalesce(p$n_runs, 0L) else 0L
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
      formatStyle("Done", color = styleInterval(c(0.5, 100 * D$params$done_fraction - 0.01), c(COL$todo, COL$partial, COL$done)), fontWeight = "500")
  })

  # row click: park view -> select the park; trail view -> zoom to the trail
  observeEvent(input$tbl_rows_selected, {
    i <- input$tbl_rows_selected
    if (is.null(i)) return()
    r <- table_rows()[i, ]
    if (!nzchar(input$park)) {
      updateSelectInput(session, "park", selected = r$park)
    } else {
      b <- st_bbox(filter(trails_ll, trail_id %in% r$ids[[1]]))
      leafletProxy("map") %>% fitBounds(b[["xmin"]], b[["ymin"]], b[["xmax"]], b[["ymax"]])
      hl <- filter(trails_ll, trail_id %in% r$ids[[1]])
      leafletProxy("map") %>% clearGroup("highlight") %>%
        addPolylines(data = hl, color = "#ffffff", weight = 9, opacity = 0.9, group = "highlight") %>%
        addPolylines(data = hl, color = COL$ink, weight = 3, opacity = 0.9, dashArray = "6 6", group = "highlight")
    }
  })

  output$csv <- downloadHandler(
    filename = function() sprintf("%s_%dm.csv", if (nzchar(input$park)) gsub("[^A-Za-z0-9]+", "_", input$park) else "all_parks", input$thr),
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
  # draw_trails() works on both a fresh leaflet() object (initial render) and a
  # leafletProxy (updates), so the drawing code lives in one place.
  draw_trails <- function(m, c_df, park, thr) {
    if (nzchar(park)) {
      # park view: split each line into run and unrun pieces
      pieces <- split_by_coverage(c_df$trail_id, thr)
      if (!is.null(pieces)) {
        g <- c_df %>% group_by(park, name) %>% summarise(gfrac = sum(done_m) / sum(length_m), glen = sum(length_m), .groups = "drop")
        info <- st_drop_geometry(trails_ll) %>% select(trail_id, name, park) %>% inner_join(g, by = c("name", "park"))
        pieces <- pieces %>% left_join(info, by = "trail_id") %>%
          mutate(label = sprintf("%s: %s of %s mi run", name, pct(gfrac), fmt1(glen / M_PER_MI)))
        todo <- filter(pieces, !covered); done <- filter(pieces, covered)
        if (nrow(todo)) m <- addPolylines(m, data = todo, color = COL$todo, weight = 3, opacity = 0.8, label = ~label, group = "trails")
        if (nrow(done)) m <- addPolylines(m, data = done, color = COL$done, weight = 4, opacity = 0.95, label = ~label, group = "trails")
      }
      pk <- filter(parks_ll, park == .env$park)
      if (nrow(pk)) m <- addPolygons(m, data = pk, fill = FALSE, color = COL$ink, weight = 1.5, dashArray = "4 4",
                                     group = "parks", options = pathOptions(interactive = FALSE))
    } else {
      # overview: whole segments coloured by class, no per-piece splitting (faster)
      t <- trails_ll %>% inner_join(select(c_df, trail_id, frac), by = "trail_id") %>% mutate(class = cls(frac))
      colours <- c(done = COL$done, partial = COL$partial, todo = COL$todo)
      for (k in c("todo", "partial", "done")) {
        tk <- filter(t, class == k)
        if (nrow(tk)) m <- addPolylines(m, data = tk, color = colours[[k]], weight = if (k == "done") 2.5 else 2,
                                        opacity = 0.85, group = "trails", options = pathOptions(interactive = FALSE))
      }
      m <- addPolygons(m, data = parks_ll, fill = FALSE, color = COL$ink, weight = 1, opacity = 0.35, dashArray = "4 4",
                       group = "parks", options = pathOptions(interactive = FALSE))
    }
    m
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
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Light") %>%
      addProviderTiles(providers$OpenTopoMap, group = "Topo") %>%
      addLayersControl(baseGroups = c("Light", "Topo"), position = "topright") %>%
      addScaleBar(position = "bottomleft") %>%
      addPolylines(data = runs_ll, color = COL$run, weight = 2, opacity = 0.45, group = "runs",
                   options = pathOptions(interactive = FALSE)) %>%
      addLegend(position = "bottomright", colors = c(COL$done, COL$todo, COL$run),
                labels = c("Run", "Not run", "Run tracks"), opacity = 0.9) %>%
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    
    if (!isolate(input$showruns)) {
      m <- hideGroup(m, "runs")
    }
    
    draw_trails(m, isolate(cov()), isolate(input$park), isolate(input$thr))
  })
  # redraw trails and boundary when scope or threshold changes
  observeEvent(list(input$park, input$thr, input$svc), {
    proxy <- leafletProxy("map") %>% clearGroup("trails") %>% clearGroup("parks") %>% clearGroup("highlight")
    draw_trails(proxy, cov(), input$park, input$thr)
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
}

shinyApp(ui, server)
