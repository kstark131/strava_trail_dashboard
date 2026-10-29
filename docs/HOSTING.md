# Hosting the dashboard online: options and roadmap

Nothing here is implemented yet. The goal is "not public, but reachable by me
and a few people I choose, from anywhere".

## What actually needs hosting

Only the Shiny app: `app/app.R`, `app/route_suggest.R` and
`data/app_data.rds`. The pipeline (01 to 03) needs your Strava secret and token,
so keep running it on your own machine at first and upload the finished
`app_data.rds`. Never deploy `.Renviron`, `.httr-oauth`, `data/streams_full/` or
`gpx/`.

**Privacy before anything else.** The run tracks show where you live and
when you run. Before the app leaves your network, add a privacy zone: in
`03_match_runs_to_trails.R`, drop track points within ~300 m of a home
location (or the first and last 300 m of every run) from `runs_ll` only. Keep
matching on the full tracks. Also consider leaving `activities` (names, dates)
out of `app_data`, since the app only uses the totals.

## Options, from least to most work

| option | who can open it | cost | effort | notes |
| --- | --- | --- | --- | --- |
| **Tailscale** on the machine that runs the app today | people you invite to your tailnet | free for personal use | 30 min | Nothing is exposed to the internet. Each viewer installs Tailscale. Your computer has to be on. |
| **Cloudflare Tunnel + Cloudflare Access** in front of the same machine | anyone whose email you allow (one-time code by email) | free for small teams | 1 to 2 h | No open ports, HTTPS, works in any browser with no install. Still needs your computer on. |
| **shinyapps.io** or **Posit Connect Cloud** | free tiers are public; password / invite-only needs a paid plan | free to paid | 1 h | `rsconnect::deployApp("app")` after copying `app_data.rds` into `app/`. Least maintenance; check the current plan for private access. |
| **Small VPS** (Hetzner, DigitalOcean, Fly.io) with Docker, plus Cloudflare Access or Caddy basic auth | allowed emails, or a shared password | about $5 to $10 a month | half a day | Always on, full control. Recommended once you want it reliably online. |
| Static page (the `90_export_json...` HTML version) on GitHub Pages / Cloudflare Pages behind Access | allowed emails | free | varies | No R server, but loses the Shiny features (route suggestions, live threshold). |

**Recommendation:** start with Cloudflare Tunnel + Access from your own
computer (it fits "accessible but not public" with no monthly cost), then move
the same container to a small VPS when you want it always on.

## Roadmap

### Phase 0: make the app portable (small code changes)
1. Let the app find its data through an environment variable, e.g.
   `APP_DATA=/srv/data/app_data.rds`, falling back to today's paths.
2. Pin package versions with `renv::init()` / `renv::snapshot()` so the server
   installs exactly what works locally.
3. Add the privacy zone above, and a config flag to leave out run names.
4. Write a `Dockerfile` on `rocker/geospatial` (it already has sf, GDAL and
   PROJ), installing shiny, bslib, leaflet, DT, igraph, RANN; `EXPOSE 3838`;
   `CMD R -e "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"`.
5. Check memory: load `app_data.rds` and run `lobstr::obj_size()`. A 1 GB VPS
   is enough for a few hundred runs; raise `run_display_simplify_m` to 3 to 5 m
   for the hosted copy if the map is slow on phones.

### Phase 1: private access from your own computer
1. Install `cloudflared`, create a tunnel to `localhost:3838`, and give it a
   hostname on a domain you own (or use Tailscale and skip the domain).
2. In Cloudflare Zero Trust, add an Access application for that hostname with
   an "emails" policy listing the people allowed in.
3. Run the app with `shiny::runApp("app", port = 3838)` and test from a phone
   on mobile data.

### Phase 2: always-on host
1. Rent a small VPS, install Docker, run the Phase 0 image with `data/`
   mounted as a volume and `--restart unless-stopped`.
2. Point the same Cloudflare tunnel (or Caddy with HTTPS and basic auth) at it.
3. Add a `deploy.R` or shell script that copies the new `app_data.rds` up
   (`scp` / `rsync`) and restarts the container. Run it after `run_all.R`.

### Phase 3: automatic refresh (optional)
1. Move 01 to 03 onto the server on a weekly cron, or into a scheduled GitHub
   Action that uploads `app_data.rds` as the deploy step.
2. Store the Strava client secret and a **refresh token** as secrets (not the
   `.httr-oauth` file); exchange it for an access token at the start of
   `01_download.R`.
3. Keep `data/streams_full/` on a persistent volume so only new activities are
   fetched; a fresh backfill costs hours of rate limit.
4. Later: Strava webhooks (push events on new activities) instead of a cron.

### Phase 4: other people's runs (only if you want this)
Today everything assumes one athlete. Supporting others means per-user Strava
OAuth inside the app, per-user caches, and Strava's review: new API apps are
limited to one connected athlete until Strava approves more, and the API
agreement restricts showing one athlete's data to others. Read the current
Strava API agreement before going here.
