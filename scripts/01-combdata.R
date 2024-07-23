# combine data script -----------------------------------------------------

## a script `sourced` by the .qmd to combine data ready for analysis.

# info --------------------------------------------------------------------

## use changelog
# snippet changelog

## use snippets for todos
# snippet todo-bug
# snippet todo-check-me
# snippet todo-document-me
# snippet todo-fix-me
# snippet todo-optimise-me
# snippet todo-test-me

# use snippets for code chunks
# snippet saveplot
# snippet loadlatestdata


# change log --------------------------------------------------------------

## changelog


# additional libraries ----------------------------------------------------
library(sf) # for st_*()
library(riverdist) # for xy2segvert() & riverdistance()
library(suncalc) # for getSunlightTimes()


# additional setup --------------------------------------------------------


# read in organised data --------------------------------------------------

## read in latest processed data
if (file.exists(here("data", "pikeperch-homerange-salinity-data.RData"))) {
  load(here("data", "pikeperch-homerange-salinity-data.RData"))
} else {
  message("need to run previous scripts")
}


# river distances between receivers ---------------------------------------

## load clean network (see `manual-river-network-clean.R`)
if (file.exists(here("data", "rn_clean.RData"))) {
  load(here("data", "rn_clean.RData"))
} else {
  stop("There is a manual step needed. Open file `scripts/manual-river-network-clean.R`
       and follow the prompts when running `cleanup(rn)`. Remember to save the clean
       network.")
}

## load "shifted" points
stn_sp <- st_read(here("data", "pts.shp"))

## reproject stn_sp
foo <- st_transform(stn_sp, crs = rn_proj)

## put them on rn_clean
foo_clean <- xy2segvert(x = st_coordinates(foo)[, 1], 
                        y = st_coordinates(foo)[, 2], 
                        rivers = rn_clean)

## add them to simplified stn_dt
stn_sp <- cbind(stn_dt[!duplicated(Sort), ], foo_clean)

## calculate pair-wise distances
dist_pw <- data.table(expand.grid("rowno_1" = 1:nrow(foo_clean), 
                                  "rowno_2" = 1:nrow(foo_clean)))
foo <- vector("numeric", nrow(dist_pw))
for (i in 1:nrow(dist_pw)) {
  foo[i] <- riverdistance(startseg = foo_clean[dist_pw$rowno_1[i], "seg"], 
                          startvert = foo_clean[dist_pw$rowno_1[i], "vert"], 
                          endseg = foo_clean[dist_pw$rowno_2[i], "seg"], 
                          endvert = foo_clean[dist_pw$rowno_2[i], "vert"], 
                          rivers = rn_clean)
}
dist_pw[, dist_m := foo]

## add location names
dist_pw[, c("Location_1", "Location_2") := .(factor(rowno_1, 
                                                    levels = 1:nrow(stn_sp),
                                                    labels = stn_sp$`Location name`),
                                             factor(rowno_2, 
                                                    levels = 1:nrow(stn_sp),
                                                    labels = stn_sp$`Location name`))]


# distances travelled by fish ---------------------------------------------

## add Sort to det_dt
det_dt[, row := 1:.N]

## sort det_dt
setorder(det_dt, "tag_id", "DateTime", "StationName")

## add route
det_dt[, "route" := with(rle(`Location name`), 
                         rep(1:length(lengths), lengths)),
       by = tag_id]

## calculate detection histories
det_hists <- det_dt[!is.na(tag_id), .N, 
                    by = .(tag_id, `Location name`, route)]

## convert to individual routes
rte_hists <- det_hists[, .("Location_1" = `Location name`[-length(`Location name`)], 
                           "Location_2" = `Location name`[-1]), 
                       by = tag_id]

## get route distances
rte_dists <- dist_pw[rte_hists, on = .(Location_1, Location_2)]

## add direction
rte_dists[, Direction := ifelse(rowno_1 > rowno_2, "Upstream", "Downstream")]

## factorise
rte_dists[, Direction := factor(Direction, 
                                levels = c("Downstream", "Upstream"))]

## add dist travelled
rte_dists[, "Distance travelled (m)" := cumsum(dist_m), by = .("ID" = tag_id)]


# time taken travelling by fish -------------------------------------------

## calculate max departure and min arrival for each tag_id
det_times <- det_dt[!is.na(tag_id), 
                    .("Arrive" = Arrival[1], 
                      "Depart" = Departure[.N]), 
                    by = .(tag_id, route)]

## convert to individual route timings
rte_times <- det_times[, .("Depart_1" = Depart[-length(Depart)],
                           "Arrive_2" = Arrive[-1]), 
                       by = tag_id]

## add timings to routes
rte_dists <- cbind(rte_dists, rte_times)

## add time taken
rte_dists[, "Time taken (mins)" := Arrive_2 - Depart_1]


# extra variables ---------------------------------------------------------

## speed of travel
rte_dists[, "Speed (m/min)" := `dist_m` / 
            as.numeric(`Time taken (mins)`)]

## mean longitude and latitude
lonlat <- det_dt[, .("lat" = mean(Latitude, na.rm = TRUE),
                     "lon" = mean(Longitude, na.rm = TRUE))]

## time of day (day or night) of departure and arrival
tod <- getSunlightTimes(date = as.Date(rte_dists$Depart_1), 
                        lon = lonlat$lon, lat = lonlat$lat,
                        keep = c("sunriseEnd", "sunsetStart"),
                        tz = "UTC")
rte_dists[, "Depart_1_tod" := factor(Depart_1 >= tod$sunriseEnd &
                                       Depart_1 <= tod$sunsetStart, 
                                     levels = c(TRUE, FALSE),
                                     labels = c("Day", "Night"))]
rte_dists[, "Arrive_2_tod" := factor(Arrive_2 >= tod$sunriseEnd &
                                       Arrive_2 <= tod$sunsetStart, 
                                     levels = c(TRUE, FALSE),
                                     labels = c("Day", "Night"))]


# add salinity ------------------------------------------------------------

## add character Location_1 & Location_2 columns to data
rec_dt[, "Location_1" := as.character(`Location name`)]
rec_dt[, "Location_2" := as.character(`Location name`)]

## get closest logger records to departure and arrival times
dep_env <- rec_dt[rte_dists[, "datetime" := Depart_1], 
                  roll = "nearest", 
                  on = .(Location_1, datetime)]
arr_env <- rec_dt[rte_dists[, "datetime" := Arrive_2],
                  roll = "nearest", 
                  on = .(Location_2, datetime)]

## add them to rte_dists and rename
rte_dt <- cbind(rte_dists, 
                dep_env[, 
                        .("logger_dep" = logger,
                          "datetime_dep" = datetime,
                          "temp_dep" = temperature,
                          "depth_dep" = depth,
                          "salin_dep" = `salinity clean`)], 
                arr_env[, 
                        .("logger_arr" = logger,
                          "datetime_arr" = datetime,
                          "temp_arr" = temperature,
                          "depth_arr" = depth,
                          "salin_arr" = `salinity clean`)])

## remove duplicate and unnecessary columns
dup_cols <- colnames(rte_dt)[duplicated(colnames(rte_dt))]
rm_cols <- c(dup_cols, "datetime")
if (length(rm_cols) > 0) {
  set(rte_dt, j = rm_cols, value = NULL)
}


# add tide ----------------------------------------------------------------

## get closest tide records to departure and arrival times
dep_tide <- tide_dt[rte_dists[, "datetime" := Depart_1], 
                    roll = "nearest", 
                    on = .(datetime)]
arr_tide <- tide_dt[rte_dists[, "datetime" := Arrive_2], 
                    roll = "nearest", 
                    on = .(datetime)]

## add them to rte_dists and rename
rte_dt <- cbind(rte_dt, 
                dep_tide[, 
                         .("elev_dep" = `Surface elevation`,
                           "state_dep" = State)], 
                arr_tide[, 
                         .("elev_arr" = `Surface elevation`,
                           "state_arr" = State)])


# save results ------------------------------------------------------------

## save list
save_lst <- c(
  
  "stn_dt", # receiver data
  "fish_dt", # tagged pikeperch data
  "env_dt", # environmental data
  "tide_dt", # bodc tide data
  
  "rec_dt", # receiver and environmental data
  "det_dt", # tag detections and receiver data
  "rte_dt" # route data
  
)

## save out data
dat_nm <- "pikeperch-homerange-salinity-data.RData"
save(list = save_lst,
     file = here("data", dat_nm))
