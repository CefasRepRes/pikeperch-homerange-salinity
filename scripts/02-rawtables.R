# raw data tabling script -------------------------------------------------

## a script `sourced` by the .qmd to make tables of the data before analysis.

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
library(lubridate) # for days_in_month


# additional setup --------------------------------------------------------

## https://rpubs.com/geospacedman/routing - could be helpful?


# read in organised data --------------------------------------------------

## read in latest processed data
if (file.exists(here("data", "pikeperch-homerange-salinity-data.RData"))) {
  load(here("data", "pikeperch-homerange-salinity-data.RData"))
} else {
  message("need to run previous scripts")
}


# receiver locations ------------------------------------------------------

## take a copy
tab1 <- stn_dt

## remove "2023*" dates
for(col in c("Receiver Until", "DST Until")) {
  set(tab1, 
      i = which(tab1[[col]] > as.POSIXct("2023-01-01", tz = "UTC")), 
      j = col, 
      value = NA)
}

## reorder columns
setcolorder(tab1, c("River", "Location name", 
                    "UK National Grid Reference", "Latitude", "Longitude"))

## simplify column names
setnames(tab1, 
         c("Receiver From", "Receiver Until", "DST From", "DST Until", "DST No."),
         c("From", "Until", "From", "Until", "Logger No."))

## if present, then delete some columns
rm_cols <- c("logger", "StationName", "Receiver S/N", "No.", "Sort")
for (r in rm_cols) {
  if (r %in% colnames(tab1)) {
    set(tab1, j = r, value = NULL)
  }
}

## write it out
fwrite(tab1, here("table1.csv"))

## receiver coordinates from receivers that made detections
foo <- unique(rec_dt[, .(stnreclog, Longitude, Latitude)])

## tidal receivers
foo[, tidal := ifelse(Longitude > -0.322 & 
                        Latitude < 51.6,
                      "Yes", "No")]
foo[c(11, 12, 13), tidal := "No"]

## remove NAs
foo <- na.omit(foo)

## add numeric label
foo[, "label" := c(18:1, 21, 20, 19)]

## split stnreclog and set colnames
foo[, c("stn", "rec", "log") := tstrsplit(as.character(stnreclog), "\\.")]
setnames(foo, 
         c("tidal", "label", "stn", "rec", "log"),
         c("Tidal", "Label", "Station", "Receiver", "Logger"))

## calc median (range) salinity
fii1 <- rec_dt[stnreclog %in% foo$stnreclog, 
               .("Median salinity (PSU) [range]" = paste0(median(`salinity clean`), 
                                                          " [", 
                                                          min(`salinity clean`), 
                                                          " - ", 
                                                          max(`salinity clean`), 
                                                          "]")), 
               by = stnreclog]

## calc number of fish detected
det_dt[, stnreclog := interaction(`Location name`, `Receiver No.`, logger)]
fii2 <- det_dt[stnreclog %in% foo$stnreclog, 
               .("Number fish detected" = length(unique(na.omit(tag_id)))), 
               by = .(stnreclog)]

## reorganise
tab0 <- foo[, .(Label, Station, Longitude, Latitude, Receiver, Logger, Tidal)]

## merge in
tab0 <- cbind(tab0, merge(fii1, fii2, sort = FALSE)[, -1])

## reorder
setorder(tab0, "Label")

## write it out
fwrite(tab0, here("table0.csv"))


# fish table --------------------------------------------------------------

## calculate home range from detections
hr <- rte_dt[, .(ds = max(`Distance travelled (m)`[Direction == "Downstream"]),
                 us = max(`Distance travelled (m)`[Direction == "Upstream"])), 
             by = tag_id][, .("Acoustic transmitter" = as.numeric(tag_id), 
                              "Home range (m)" = abs(us - ds))]

## make table 2
tab2 <- fish_dt[`Capture event` == 1 &
                  Recapture == 0, .("Acoustic transmitter" = ID, 
                                    "Total length (mm)" = `TL (mm)`,
                                    Age, Sex, 
                                    "Tagging site" = Site, Date, 
                                    "UK National Grid Reference" = NGR, 
                                    Latitude, Longitude)]

## fix Sex
tab2[, Sex := factor(Sex, 
                     levels = c("f", "m", "j", "?"),
                     labels = c("Female", "Male", "Juvenile", "Unknown"))]

## merge in home range calculation
tab2 <- merge(tab2, hr, all.x = TRUE)

## write it out
fwrite(tab2, here("table2.csv"))


# summary of detections dataset -------------------------------------------

## make a summary
tab3 <- rte_dt[!is.na(tag_id), 
               .("Number stations" = length(unique(c(Location_1, Location_2))),
                 "Days detected" = length(unique(c(as.Date(Depart_1), 
                                                   as.Date(Arrive_2)))),
                 "Days at liberty" = min(days_in_month(c(as.Date(Depart_1), 
                                                         as.Date(Arrive_2)))),
                 "Number downstream journeys" = sum(Direction == "Downstream"),
                 "Average distance travelled downstream (m)" = mean(dist_m[Direction == "Downstream"]),
                 "Average speed downstream (m/min)" = mean(`Speed (m/min)`[Direction == "Downstream"]),
                 "Number upstream journeys" = sum(Direction == "Upstream"),
                 "Average distance travelled upstream (m)" = mean(dist_m[Direction == "Upstream"]),
                 "Average speed upstream (m/min)" = mean(`Speed (m/min)`[Direction == "Upstream"])),
               by = .("Acoustic transmitter" = as.integer(tag_id),
                      "Period" = factor(format(Depart_1, "%Y-%m")))]

## set order
setorder(tab3, "Acoustic transmitter", "Period")

## save the summary
fwrite(tab3, here("table3.csv"))


# summary of environmental dataset ----------------------------------------

## make a summary
tab4 <- rec_dt[!is.na(logger), 
               .("Location name" = unique(`Location name`),
                 "From" = min(datetime),
                 "Until" = max(datetime),
                 "Mean salinity" = mean(`salinity clean`),
                 "SD salinity" = sd(`salinity clean`),
                 "Min salinity" = min(`salinity clean`),
                 "Max salinity" = max(`salinity clean`)),
               by = .("Logger" = logger)]

## save the summary
fwrite(tab4, here("table4.csv"))


# tidal forays ------------------------------------------------------------

## order det_dt
setorder(det_dt, "tag_id", "Arrival", "StationName")

## add route
det_dt[, "route" := with(rle(`Location name`),
                         rep(1:length(lengths), lengths)),
       by = tag_id]

## take a copy of det_dt when tags detected
foo <- det_dt[!is.na(tag_id), ]

## mark tidal stations in det_dt
tidal_stns <- tab0[Tidal == "Yes", Station]
foo[, "Tidal" := ifelse(StationName %in% tidal_stns, "Yes", "No")]

## tags that used tidal
tidal_tags <- foo[StationName %in% tidal_stns, unique(tag_id)]

## get time spent at each location visited
tidal_forays <- foo[, .("Arrival" = min(Arrival),
                        "Departure" = max(Departure)),
                    by = .("Acoustic transmitter" = tag_id, 
                           "Movement number" = route, 
                           "Station" = StationName,
                           "Tidal" = Tidal)]
tidal_forays[, "Time spent" := difftime(Departure, Arrival, units = "days")]

## remove tags without multiple detections
fii <- names(which(table(tidal_forays$`Acoustic transmitter`) > 1))
tab5 <- subset(tidal_forays, `Acoustic transmitter` %in% fii)

## add tidal movement number
tab5[, "Tidal movement number" := with(rle(Tidal), 
                                       rep(1:length(lengths), lengths)),
     by = `Acoustic transmitter`]

## write it
# fwrite(tab5, here("table5.csv"))


# save data ---------------------------------------------------------------

## save list
save_lst <- c(
  
  "tab0", # receivers with detections - alongside figure 1
  "tab1", # receivers summary
  "tab2", # fish summary
  "tab3", # detection summary
  "tab4", # salinity summary
  "tab5"  # time spend in tidal area summary
  
)

## save out data
dat_nm <- "pikeperch-homerange-salinity-rawtables.RData"
save(list = save_lst,
     file = here("data", dat_nm))
