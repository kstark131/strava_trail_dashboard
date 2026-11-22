# ==============================================================================
# DEPLOY: put the app on shinyapps.io, behind a password
# ==============================================================================
# Run after run_all.R (or 03_match_runs_to_trails.R) whenever you want the
# online copy updated:  source("deploy_shinyapps.R")
#
# One-time setup
#   1. install.packages(c("rsconnect", "shinymanager"))
#   2. Create a shinyapps.io account. In its dashboard, Account > Tokens > Show,
#      copy the rsconnect::setAccountInfo(...) line and run it once in R.
#   3. Add a login to .Renviron (usethis::edit_r_environ()), then restart R:
#        APP_USER=me
#        APP_PASSWORD=a-long-password-you-do-not-use-elsewhere
#   4. Set privacy_zones in 00_config.R (below the deploy settings).
#
# What gets uploaded: app/app.R, route_suggest.R, exclusions.R, your saved
# exclusions.geojson, a trimmed copy of data/app_data.rds, and a temporary
# app/.Renviron holding only APP_USER and APP_PASSWORD (deleted again after the
# upload). Your Strava secret, token, streams and GPX files stay on this machine.
#
# The hosted app shows a login screen (shinymanager). Exclusions can be edited
# there, but shinyapps.io does not keep files the app writes, so use Download,
# put the file in app/ and deploy again to keep them.

library(sf)
library(rsconnect)
source("00_config.R")
sf_use_s2(FALSE)

user <- Sys.getenv("APP_USER", "me"); password <- Sys.getenv("APP_PASSWORD")
if (nchar(password) < 10) stop("Set APP_PASSWORD (10 characters or more) in .Renviron and restart R. See the top of this file.")
if (!requireNamespace("shinymanager", quietly = TRUE)) stop('install.packages("shinymanager") first')
if (!nrow(rsconnect::accounts())) stop("Connect rsconnect to shinyapps.io first: run the setAccountInfo() line from its dashboard.")

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

# --- bundle and upload ------------------------------------------------------------
# The bundle copies are removed afterwards, even if the upload fails, so local
# runs read ../data again and the password file never lingers in app/.
tryCatch({
  dir.create("app/data", showWarnings = FALSE)
  saveRDS(D, "app/data/app_data.rds")
  writeLines(c(paste0("APP_USER=", user), paste0("APP_PASSWORD=", password)), "app/.Renviron")
  message(sprintf("Uploading %.1f MB of data", file.size("app/data/app_data.rds") / 1e6))

  files <- c("app.R", "route_suggest.R", "exclusions.R", "data/app_data.rds", ".Renviron")
  if (file.exists("app/exclusions.geojson")) files <- c(files, "exclusions.geojson")
  rsconnect::deployApp(appDir = "app", appName = deploy_app_name, appFiles = files,
                       forceUpdate = TRUE, launch.browser = FALSE)
}, finally = unlink(c("app/data", "app/.Renviron"), recursive = TRUE))
