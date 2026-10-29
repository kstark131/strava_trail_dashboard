# Runs the pipeline end to end. Each script can also be run on its own.
source("01_download.R")            # only new activities are fetched
source("02_prepare_trails.R")      # skip if the trail/park layers have not changed
source("03_match_runs_to_trails.R")
shiny::runApp("app")
