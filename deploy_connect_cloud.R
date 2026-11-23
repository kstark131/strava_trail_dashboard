# ==============================================================================
# DEPLOY: publish the app to Posit Connect Cloud, behind a login
# ==============================================================================
# Follows Posit's "Deploy a Shiny for R app" guide (command-line method):
# https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html
# Posit is moving shinyapps.io into Connect Cloud, so this replaces the old
# shinyapps.io deploy.
#
# One-time setup
#   1. install.packages(c("rsconnect", "shinymanager"))
#      (rsconnect must be recent enough to have connectCloudUser())
#   2. rsconnect::connectCloudUser()      # your browser opens; authorize access
#   3. Set privacy_zones in 00_config.R (at least your home).
#
# Every update (after run_all.R or 03_match_runs_to_trails.R):
#   source("deploy_connect_cloud.R")
#
# After the FIRST deploy, set the login (once):
#   Connect Cloud > your content > Settings > Variables: add APP_USER and
#   APP_PASSWORD (a long password you don't use elsewhere), then restart or
#   republish. The password lives only there, never in this repo or the upload.
#   Until it is set, the hosted app shows only a "login not configured" page:
#   the app never runs online without a login.
#
# Why a login inside the app: on Connect Cloud's free plan every app is public
# (and listed on your profile page, connect.posit.cloud/<account>). Turning
# public access off needs a paid plan.
#
# What is uploaded: app/app.R, route_suggest.R, exclusions.R, your saved
# exclusions.geojson, and app/data/app_data.rds, a trimmed copy of the data
# (no run names or dates, run tracks cut around privacy_zones). Your Strava
# secret, token, streams and GPX files stay on this machine.
#
# Publishing from RStudio's Publish button instead: run this script with
# deploy_upload <- FALSE in 00_config.R (it then only writes the trimmed data),
# open app/app.R, click Publish, include data/app_data.rds and
# exclusions.geojson, and add APP_USER / APP_PASSWORD as secrets when asked.

library(sf)
source("00_config.R")
sf_use_s2(FALSE)

if (!requireNamespace("shinymanager", quietly = TRUE)) stop('install.packages("shinymanager") first: the hosted app needs it for the login')
if (deploy_upload) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) stop('install.packages("rsconnect") first')
  if (!exists("connectCloudUser", envir = asNamespace("rsconnect"))) stop("Update rsconnect: this version cannot publish to Connect Cloud")
  if (!nrow(rsconnect::accounts())) stop("Connect your account first: rsconnect::connectCloudUser()")
}

D <- readRDS(file.path(dir_data, "app_data.rds"))

# --- privacy: no run names or dates, and no track near home ---------------------
D$activities <- NULL                                   # names and dates; the app only needs the totals
runs <- st_transform(D$runs_ll, crs_projected)
if (length(privacy_zones)) {
  zones <- st_union(st_buffer(st_transform(st_sfc(lapply(privacy_zones, st_point), crs = 4326), crs_projected),
                              privacy_radius_m))
  runs <- suppressWarnings(st_difference(runs, zones))
  runs <- runs[!st_is_empty(runs), ]
  message(length(privacy_zones), " privacy zone(s) of ", privacy_radius_m, " m cut from the run tracks")
} else {
  message("No privacy_zones set in 00_config.R: run tracks near home will be visible online.")
}
if (deploy_run_simplify_m > 0) runs <- st_simplify(runs, dTolerance = deploy_run_simplify_m)
D$runs_ll <- st_transform(runs, 4326)

# Marks the hosted copy: it then requires a login and offers Download instead
# of Save for exclusions (files the app writes do not survive a restart there).
D$hosted <- TRUE

# --- bundle --------------------------------------------------------------------
# app/data/ is git-ignored. The local app keeps reading ../data first, so this
# copy is only used where ../data does not exist, i.e. on Connect Cloud.
dir.create("app/data", showWarnings = FALSE)
saveRDS(D, "app/data/app_data.rds")
message(sprintf("Wrote app/data/app_data.rds (%.1f MB)", file.size("app/data/app_data.rds") / 1e6))

files <- c("app.R", "route_suggest.R", "exclusions.R", "data/app_data.rds")
if (file.exists("app/exclusions.geojson")) files <- c(files, "exclusions.geojson")

# --- upload --------------------------------------------------------------------
if (deploy_upload) {
  rsconnect::deployApp(appDir = "app", appFiles = files, appName = deploy_app_name, forceUpdate = TRUE)
  message("Published. First time? Add APP_USER and APP_PASSWORD under Settings > Variables on Connect Cloud.")
} else {
  message("Data ready. Publish app/app.R from RStudio with these files: ", paste(files, collapse = ", "))
}
