# ==============================================================================
# SCRIPT 4 (optional): STATIC, ANIMATED, AND 3D FIGURES
# ==============================================================================
# This is your previous 02_visualizations.R with these edits:
#  * Reads streams.rds and activities.rds from 01_download.R (full-resolution
#    points) and rebuilds city_data, final_data, and run_stats here, so the
#    download script no longer depends on a city boundary.
#  * ggplot2 `size` for lines -> `linewidth` (size is deprecated for lines).
#  * `time` is seconds since the start of the activity, so difftime() calls are
#    replaced with plain subtraction.
#  * Google Drive upload runs only when upload_to_gdrive is TRUE in 00_config.R.
#  * make_animations / make_3d flags let you skip the slow sections.
library(sf)
library(tidyverse)
library(gganimate)
library(ggspatial)

source("00_config.R")
make_animations <- TRUE
make_3d         <- TRUE

# --- 1. LOAD DATA ---
streams    <- readRDS(file.path(dir_data, "streams.rds"))
activities <- readRDS(file.path(dir_data, "activities.rds"))

final_data <- streams %>%
  filter(source == "stream") %>%                       # altitude and grade only exist for streams
  rename(lon = lng) %>%
  left_join(activities %>% select(id, start_date, name), by = c("activity_id" = "id")) %>%
  mutate(date = as.Date(start_date)) %>%
  arrange(activity_id, time)

boundary_shp <- st_read(path_city_boundary, quiet = TRUE) %>%
  st_transform(crs_global) %>%
  st_make_valid()
if (nzchar(target_city)) boundary_shp <- filter(boundary_shp, NAME == target_city)

city_data <- final_data %>%
  st_as_sf(coords = c("lon", "lat"), crs = crs_global, remove = FALSE) %>%
  st_filter(boundary_shp) %>%
  st_drop_geometry()

point_stats <- city_data %>%
  arrange(activity_id, time) %>%
  group_by(activity_id) %>%
  mutate(
    time_diff = time - lag(time, default = first(time)),
    raw_dist_diff_km = distance - lag(distance, default = first(distance)),
    is_gap = time_diff > 30,
    dist_inc_km = ifelse(is_gap, 0, raw_dist_diff_km),
    alt_diff_m = altitude - lag(altitude, default = first(altitude)),
    elev_gain_m = ifelse(!is_gap & alt_diff_m > 0, alt_diff_m, 0),
    grade_category = case_when(
      grade_smooth < -2 ~ "Downhill",
      grade_smooth >= -2 & grade_smooth <= 2 ~ "Flat",
      grade_smooth > 2 & grade_smooth <= 8 ~ "Climb",
      grade_smooth > 8 ~ "Steep Climb",
      TRUE ~ "Flat"
    )
  ) %>%
  ungroup()

run_stats <- point_stats %>%
  group_by(activity_id) %>%
  summarise(
    date = min(date),
    total_km = sum(dist_inc_km, na.rm = TRUE),
    dist_miles = total_km * 0.621371,
    total_elev_gain_m = sum(elev_gain_m, na.rm = TRUE),
    total_elev_gain_ft = total_elev_gain_m * 3.28084,
    avg_grade = mean(grade_smooth, na.rm = TRUE),
    hilliness_score = ifelse(total_km > 0, total_elev_gain_m / total_km, 0)
  ) %>%
  arrange(desc(dist_miles)) %>%
  mutate(rank_distance = row_number())

saveRDS(city_data, file.path(dir_data, "city_data.rds"))
saveRDS(run_stats, file.path(dir_data, "run_stats.rds"))

if(!dir.exists(dir_figures)) dir.create(dir_figures, recursive = TRUE)
# --- 2. STATIC MAPS ---

p_all <- ggplot(city_data, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = "gray95", linewidth = 0.5, color = "gray80", inherit.aes = FALSE) +
  geom_path(color = "#fc4c02", linewidth = 0.2, alpha = 0.5) +
  theme_void() +
  labs(title = paste(target_city, "Activity Density"))

ggsave("map_1_heatmap.png", p_all, path=dir_figures, width = 10, height = 10, bg = "white")


p_grade <- city_data %>%
  filter(grade_smooth > -20 & grade_smooth < 20) %>% 
  ggplot(aes(x = lon, y = lat, group = activity_id, color = grade_smooth)) +
  geom_sf(data = boundary_shp, color = "gray20", inherit.aes = FALSE) +
  geom_path(linewidth = 0.5) +
  scale_color_gradient2(low = "cyan", mid = "white", high = "red", midpoint = 0, name = "Grade %") +
  theme_void() +
  labs(title = "The Pain Map: Gradient Analysis")

ggsave("map_2_grade.jpg", p_grade, path=dir_figures, width = 10, height = 10)


p_elev <- ggplot(city_data, aes(x = lon, y = lat, group = activity_id, color = altitude)) +
  geom_sf(data = boundary_shp, fill = NA, color = "black", inherit.aes = FALSE) +
  geom_path(linewidth = 0.5, alpha = 0.6) +
  scale_color_viridis_c(option = "turbo", name = "Elev (m)") +
  theme_void() +
  labs(title = paste("Elevation Profile:", target_city))

ggsave("map_3_altitude.png", p_elev, path=dir_figures, width = 10, height = 10, bg = "white")


top_10_dist_ids <- run_stats %>% arrange(desc(dist_miles)) %>% slice_head(n = 10) %>% pull(activity_id)

data_top_dist <- final_data %>% 
  filter(activity_id %in% top_10_dist_ids) %>%
  left_join(run_stats, by = "activity_id") %>%
  mutate(facet_label = paste0("#", rank_distance, ": ", round(dist_miles, 1), " mi"))

p_facet_dist <- ggplot(data_top_dist, aes(x = lon, y = lat, group = activity_id)) +
  geom_path(color = "#fc4c02", linewidth = 0.8, lineend = "round") +
  facet_wrap(~ reorder(facet_label, rank_distance), scales = "free", ncol = 5) + 
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 9))

ggsave("map_4_top10_longest.png", p_facet_dist, path=dir_figures, width = 12, height = 6, bg = "white")


top_10_climb_ids <- run_stats %>% arrange(desc(total_elev_gain_ft)) %>% slice_head(n = 10) %>% pull(activity_id)

data_top_climb <- final_data %>% 
  filter(activity_id %in% top_10_climb_ids) %>%
  left_join(run_stats, by = "activity_id") %>%
  mutate(facet_label = paste0(round(total_elev_gain_ft, 0), " ft climb"))

p_facet_climb <- ggplot(data_top_climb, aes(x = lon, y = lat, group = activity_id)) +
  geom_path(color = "darkred", linewidth = 0.8, lineend = "round") +
  facet_wrap(~ reorder(facet_label, -total_elev_gain_ft), scales = "free", ncol = 5) + 
  theme_void() +
  theme(strip.text = element_text(face = "bold", size = 9, color = "darkred")) +
  labs(title = "The Sufferfest: Top 10 Climbs")

ggsave("map_5_top10_climbs.png", p_facet_climb, path=dir_figures, width = 12, height = 6, bg = "white")


# --- 3. ANIMATED MAPS ---
if (make_animations) {

data_elapsed <- city_data %>%
  group_by(activity_id) %>%
  mutate(
    start_time = min(time),
    elapsed_seconds = time - start_time
  ) %>% ungroup()

anim_elapsed <- ggplot(data_elapsed, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = NA, color = "black", inherit.aes = FALSE) +
  geom_path(alpha = 0.3, color = "#fc4c02", linewidth = 0.5) +
  theme_void() +
  transition_reveal(elapsed_seconds) +
  labs(title = "Elapsed: {round(frame_along/60)} mins")

anim_save("anim_elapsed.gif", animate(anim_elapsed, nframes = 200, fps = 20, renderer = gifski_renderer()), path=dir_figures)


data_prop <- city_data %>%
  group_by(activity_id) %>% 
  mutate(
    start_time = min(time),
    total_dur = max(time) - min(time),
    progress = ifelse(total_dur == 0, 0, 
                      (time - start_time) / total_dur)
  ) %>% ungroup()

anim_prop <- ggplot(data_prop, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = NA, color = "black", inherit.aes = FALSE) +
  geom_path(alpha = 0.3, color = "purple", linewidth = 0.5) +
  theme_void() +
  transition_reveal(progress) +
  labs(title = "Progress: {round(frame_along * 100)}%")

anim_save("anim_proportional.gif", animate(anim_prop, nframes = 200, fps = 20, renderer = gifski_renderer()), path=dir_figures)


seq_data <- city_data %>%
  arrange(date, time) %>% 
  group_by(activity_id) %>%
  mutate(run_duration = time - min(time)) %>%
  ungroup() %>%
  arrange(date, time) %>% 
  group_by(activity_id) %>%
  mutate(run_total_time = max(run_duration)) %>% 
  slice(1) %>% 
  ungroup() %>%
  mutate(start_offset = lag(cumsum(run_total_time + 100), default = 0)) %>%
  dplyr::select(activity_id, start_offset) %>%
  right_join(city_data, by = "activity_id") %>%
  mutate(
    run_duration = time - min(time),
    pseudo_time = start_offset + run_duration
  ) 

anim_seq <- ggplot(seq_data, aes(x = lon, y = lat, group = activity_id)) +
  geom_sf(data = boundary_shp, fill = NA, color = "gray10", inherit.aes = FALSE) +
  geom_path(color = "#fc4c02", linewidth = 0.5, lineend = "round") +
  theme_void() +
  transition_reveal(pseudo_time)

final_anim <- animate(anim_seq, nframes = 400, fps = 15, width = 800, height = 800, renderer = gifski_renderer())
anim_save("anim_sequential.gif", animation = final_anim, path = dir_figures)

}

# --- 4. 3D MAPS ---
if (make_3d) {
library(elevatr)
library(raster)
library(rayshader)

p_hex <- ggplot(city_data, aes(x = lon, y = lat)) +
  geom_hex(bins = 80, aes(fill = after_stat(count))) + 
  scale_fill_viridis_c(option = "magma", name = "Run Count") +
  theme_void() +
  theme(legend.position = "none") 

plot_gg(p_hex, width = 5, height = 5, multicore = TRUE, scale = 250, zoom = 0.6, phi = 45, theta = 30, windowsize = c(1000, 1000))
render_snapshot(file.path(dir_figures, paste0(tolower(target_city), "_3d_density.png")))

elev_raster <- get_elev_raster(boundary_shp, z = 12, clip = "locations")
elev_matrix <- raster_to_matrix(elev_raster)

elev_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(elev_matrix, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elev_matrix), 0) %>%
  plot_3d(elev_matrix, zscale = 10, fov = 0, theta = 45, zoom = 0.75, phi = 45)

render_path(
  extent = raster::extent(elev_raster), 
  lat = data_top_climb$lat, 
  long = data_top_climb$lon, 
  altitude = data_top_climb$altitude + 5, 
  zscale = 10, 
  color = "#fc4c02", 
  linewidth = 2
)

}

# --- 5. GOOGLE DRIVE UPLOAD ---
if (upload_to_gdrive) {
library(googledrive)
upload_to_drive <- function(local_path, drive_folder_name = "Strava_R_outputs") {
  require(googledrive)
  target_folder <- drive_find(pattern = drive_folder_name, type = "folder", n_max = 1)
  
  if (nrow(target_folder) == 0) {
    message(paste("Creating new folder on Drive:", drive_folder_name))
    target_folder <- drive_mkdir(drive_folder_name)
  }
  
  drive_upload(media = local_path, path = target_folder, overwrite = TRUE)
}

all_plots <- list.files(dir_figures, full.names = TRUE)

for (file in all_plots) {
  upload_to_drive(file)
}
}
