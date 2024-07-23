# data organisation script ------------------------------------------------

## a script `sourced` by the .qmd to organise the data for the analysis. See data-source.md

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
library(readxl) # for read_xlsx
library(gsignal) # for findpeaks
library(zoo) # for na.approx
library(sf) # for st_* functions
library(Hmisc) # for capitalize
library(raster) # for raster
library(stringr) # for str_remove
library(forecast) # for tsoutliers


# additional setup --------------------------------------------------------


# spatial data ------------------------------------------------------------

## read processed river_line shapefile
### Built in QGIS using combination of "gis_osm_water_a_free_1.shp" files from
### "greater-london-latest-free.shp" and "surrey-latest-free.shp"
river_line <- st_read(here("data", "river_line_complete.shp"))

## read, transform, and subset poly of thames
secs <- st_read(here("data", "WFD_Transitional_Water_Bodies_Cycle_3.shp"))
secs <- st_transform(secs, crs = st_crs(river_line))
secs <- subset(secs, WB_NAME %in% c("THAMES MIDDLE", 
                                    "THAMES UPPER"))
secs$Section <- capitalize(tolower(secs$WB_NAME))

## read processed river_poly raster, else make it
if (file.exists(here("data", "river_poly.tiff"))) {
  river_poly <- raster(here("data", "river_poly.tiff"))
} else {
  r <- raster(crs = crs(river_line), extent = st_bbox(river_line), 
              resolution = 0.0001)
  foo1 <- rasterize(river_line, r, background = 0)
  foo2 <- rasterize(secs, r, background = 0)
  river_poly <- foo1 + foo2
  river_poly[which(values(river_poly) > 1)] <- 1
  river_poly[which(values(river_poly) == 0)] <- NA
  writeRaster(river_poly, filename = here("data", "river_poly.tiff"))
}


# read salinity logger files ----------------------------------------------

## DST CTDs record temperature and conductivity and calculate salinity using relationship given in working\Pikeperch files for Stephen Gregory\Field work\Conductivity ranges.xls

## get the list of ".DAT" files
logg_lst <- list.files(path = here("working", "Pikeperch files for Stephen Gregory", 
                                   "DST CTD"),
                       pattern = "\\.DAT$", 
                       full.names = TRUE,
                       recursive = TRUE)
names(logg_lst) <- gsub("^.*/[0-9]S(.*)(\\.DAT$)", "\\1", logg_lst)

## get the data
env_lst <- lapply(logg_lst, fread, skip = 14, sep = "\t", dec = ",",
                  fill = TRUE, header = FALSE)
env_dt <- rbindlist(env_lst, idcol = "logger")

## column names
setnames(env_dt, 
         paste0("V", 1:5), 
         c("row", "datetime", "temperature", "depth", "salinity"))

## inspection of the logger data reveals some bad ones - remove them
## ggplot2::ggplot(env_dt, aes(x = datetime, y = salinity, group = logger)) + 
##  geom_line() + facet_wrap(~ logger, scales = "free_y")
# rm_loggers <- c(1772, 1795, 1800)
# env_dt <- subset(env_dt, !logger %in% rm_loggers)

## adjust datetime
env_dt[, datetime := as.POSIXct(gsub("^([0-9]+)\\.([0-9]+)\\.([0-9]+) (.*)$", 
                                     "20\\3-\\2-\\1 \\4", 
                                     datetime), tz = "UTC")]


# BODC tide data ----------------------------------------------------------

## read data
tide_dt <- fread(here("working", "bodc-tide-data", "2005SHE.txt"), 
                 sep = " ", skip = 11, header = FALSE)

## set column names
setnames(tide_dt, 
         old = paste0("V", 1:5), 
         new = c("Cycle number", "Date", "Time", "Surface elevation", "Residual"))

## fix data - creates warnings
tide_dt[, "Surface elevation" := as.numeric(`Surface elevation`)]

## make datetime
tide_dt[, "datetime" := as.POSIXct(paste(Date, Time), tz = "UTC")]

## make temporary zoo data
foo <- zoo(tide_dt$`Surface elevation`)

## fill NAs and add back to tide_dt
tide_dt[, "Surface elevation"] <- as.numeric(na.approx(foo))

## add tide state
pks <- findpeaks(tide_dt$`Surface elevation`, 
                 DoubleSided = TRUE, MinPeakDistance = 10)

## make cuts
foo <- cut(1:nrow(tide_dt), c(0, pks$loc, nrow(tide_dt)))

## recode foo into flood and ebb
tide_dt[, "State"] <- factor(foo, 
                             levels = levels(foo), 
                             labels = rep(c("flood", "ebb"), 
                                          nlevels(foo)/2))

## check
if (do_chks) {
  tidestate_p <- ggplot(tide_dt[1:1000, ], aes(x = datetime)) + 
    geom_line(aes(y = ifelse(State == "flood",
                             `Surface elevation`,
                             NA),
                  colour = "flood"),
              linewidth = 1.25) +
    geom_line(aes(y = ifelse(State == "ebb",
                             `Surface elevation`,
                             NA),
                  colour = "ebb"),
              linewidth = 1.25) +
    labs(x = "Time", y = "Surface elevation", colour = "Legend") +
    scale_colour_manual(values = c("flood" = "blue", "ebb" = "red")) +
    sgg()
}


# read receiver location data ---------------------------------------------

## from notes at working\Pikeperch files for Stephen Gregory\Field work\Zander tracking notes.doc

## read in raw data
stn_dat <- read_xlsx(here("data", "PikeperchReceiverLocations_sg.xlsx"),
                     sheet = "Receivers and DST", range = "A3:M32", 
                     .name_repair = "minimal")
stn_dt <- data.table(stn_dat)

## make ".. From" and ".. Until" variables posix datetime
stn_dt[, "Receiver From"] <- as.POSIXct(paste(stn_dt$`Receiver From`, "00:00:00"), 
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC")
stn_dt[, "DST From"] <- as.POSIXct(paste(stn_dt$`DST From`, "00:00:00"), 
                                        format = "%Y-%m-%d %H:%M:%S",
                                        tz = "UTC")
stn_dt[, "Receiver Until"] <- as.POSIXct(paste(stn_dt$`Receiver Until`, "00:00:00"), 
                                              format = "%Y-%m-%d %H:%M:%S",
                                              tz = "UTC")
stn_dt[, "DST Until"] <- as.POSIXct(paste(stn_dt$`DST Until`, "00:00:00"), 
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "UTC")

# ## for rm_loggers, replace DST No. in stn_dt with closest chosen alternative
# idx <- grep("DST", colnames(stn_dt))
# set(stn_dt, i = 2L, j = idx, stn_dt[1L, ..idx])
# set(stn_dt, i = 5L, j = idx, stn_dt[6L, ..idx])
# set(stn_dt, i = 19L, j = idx, stn_dt[18L, ..idx])


## where Until is NA, use a recent date
for(col in c("Receiver Until", "DST Until")) {
  col_ <- gsub("Until", "From", col)
  set(stn_dt, 
      i = which(!is.na(stn_dt[[col_]]) & is.na(stn_dt[[col]])), 
      j = col, 
      value = as.POSIXct("2023-06-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
}


# read fish biometric data ------------------------------------------------

## from file at SAF_Team_Function\Non-native species drive files\Saulius\Saulius Stakenas\NonNativeProject\Tagged fish 2005.xls
## note 1: "capture event" 2 is in 2006, after all acoustic data had been collected
## note 2: "recapture" events are the same fish caught on the same day.

## read in raw data
fish_cap1 <- read_xls(here("data", "Zander data 2006_11_sg.xls"),
                      sheet = "Zander", range = "A2:K26",
                      .name_repair = "minimal")
fish_cap2 <- read_xls(here("data", "Zander data 2006_11_sg.xls"),
                      sheet = "Zander", range = "L29:U45",
                      col_names = c("No.", "Site", "Date", 
                                    "NGR", "Latitude", "Longitude", 
                                    "TL, mm", "ID", "Age", "Sex"))
fish_caps <- rbindlist(list(data.table(fish_cap1), 
                            data.table(fish_cap2)),
                       fill = TRUE, idcol = "Capture event")

## change a few column names
colnames(fish_caps)[c(2, 9)] <- c("No. in event", # was No.
                                  "TL (mm)") # was TL, mm

## convert POSIXct to date
fish_caps$Date <- as.Date(fish_caps$Date)

## fix species
fish_caps$Species <- "SL"

## get recapture IDs
recap_ids <- fish_caps[which(duplicated(ID) & !is.na(ID)), ID]

## get all capture events for recaptures
fish_recaps <- fish_caps[ID %in% recap_ids, ][order(ID, Date), ]

## combine
fish_dt <- rbindlist(list("0" = fish_caps, 
                          "1" = fish_recaps),
                     idcol = "Recapture")


# read fish detections ----------------------------------------------------

## note: there are also files named "FINALTMP.VEM" & "zander[*].csv"

## get the list of "^PING*" files
ping_lst <- list.files(path = here("working", "Pikeperch files for Stephen Gregory", "Data"),
                       pattern = "^PING", 
                       full.names = TRUE)
names(ping_lst) <- gsub("^.*(PING.*$)", "\\1", ping_lst)

## get the transmitter types
type_lst <- lapply(ping_lst, fread, skip = 7, nrow = 1, fill = TRUE, header = FALSE)
type_dt <- rbindlist(type_lst)

## remove single "256 Sensor" transmitter type that has 10 (cf 8) columns and 2 detections
ping_lst <- ping_lst[-which(type_dt$V2 == "256 Sensor")]

## read the ping data
det_lst <- lapply(ping_lst, fread, skip = 14)
det_dt <- rbindlist(det_lst, fill = TRUE, idcol = "File")

## get headers
hdr_lst <- lapply(ping_lst, fread, skip = 12, nrow = 1, fill = TRUE, header = FALSE)
hdr_dt <- rbindlist(hdr_lst)
hdr_dt$V1 <- "Number"

## apply header
colnames(det_dt) <- c("File", as.character(unique(hdr_dt)))

## add tag_id
det_dt$tag_id <- str_remove(gsub("PING([0-9]*)\\.[[0-9]*", "\\1", det_dt$File),
                            "^0+")

## reduce to tag_ids
det_red <- det_dt[which(tag_id %in% na.omit(fish_caps$ID)), ]

## make Arrival and Departure columns
det_red[, Arrival := as.POSIXct(paste(ArrivalDate, ArrivalTime), tz = "UTC")]
det_red[, Departure := as.POSIXct(paste(DepartureDate, DepartureTime), tz = "UTC")]

## add a midpoint in time
det_red[, DateTime := (Arrival + ((Departure - Arrival) / 2))]

## add receiver coordinates
idx <- match(det_red$`Receiver S/N`, na.omit(stn_dt$`Receiver No.`))
det_red[, "Latitude" ] <- as.numeric(stn_dt[idx, Latitude])
det_red[, "Longitude" ] <- as.numeric(stn_dt[idx, Longitude])

## add station name
det_red[, "StationName"] <- stn_dt[idx, `Location name`]

## check
if (do_chks) {
  
  ## all detections by station_name and tag_id
  foo <- table(det_red[, .(`Receiver S/N`, StationName, 
                           "tag_id" = as.numeric(tag_id))])
  
  ## only detections (removing non-detections)
  foo <- subset(data.table(foo), N != 0)
  
  ## all detections seem to be on specific receivers
  print(foo[, .N, by = .(StationName, `Receiver S/N`)])
  
}


# receiver and environmental data -----------------------------------------

## protect datetime
env_dt[, dt := strftime(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

## combine data
rec_dt <- env_dt[stn_dt[, "logger" := as.character(`DST No.`)], 
                 on = .(logger, datetime >= `DST From`, datetime <= `DST Until`)]

## remove few rows with no logger
# rec_dt <- na.omit(rec_dt)

## create station:receiver interaction variable
rec_dt[, stnrec := interaction(`Location name`, `Receiver No.`)]

## create station:receiver:logger interaction variable
rec_dt[, stnreclog := interaction(`Location name`, `Receiver No.`, logger)]

## clean with forecast::tsoutlier - see https://robjhyndman.com/hyndsight/tsoutliers/
rec_dt[!is.na(logger), `salinity clean` := with(tsoutliers(salinity), 
                                                replace(salinity, index, replacements)), 
       by = stnreclog]

## add back datetime
rec_dt[, datetime := as.POSIXct(dt, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

## remove unnecessary columns
rec_dt[, datetime.1 := NULL]
rec_dt[, dt := NULL]
env_dt[, dt := NULL]

## refactorise
rec_dt[, `Location name` := factor(`Location name`, 
                                  levels = unique(`Location name`))]
rec_dt[, `Receiver No.` := factor(`Receiver No.`, 
                                  levels = unique(`Receiver No.`))]


# tag and receiver data ---------------------------------------------------

## protect DateTime
det_red[, dt := strftime(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]

## combine data
det_dt <- det_red[stn_dt[, c("StationName", "Receiver S/N") := .(as.character(`Location name`),
                                                                 as.integer(`Receiver No.`))], 
                  on = .(StationName, `Receiver S/N`), 
                  allow.cartesian = TRUE]

## refactorise
det_dt[, StationName := factor(StationName, 
                               levels = unique(StationName))]
det_dt[, `Receiver No.` := factor(`Receiver No.`, 
                                  levels = unique(`Receiver No.`))]


# save data ---------------------------------------------------------------

## save list
save_lst <- c(
  
  "stn_dt", # receiver data
  "fish_dt", # tagged pikeperch data
  "env_dt", # environmental data
  "tide_dt", # bodc tide data
  
  "rec_dt", # receiver and environmental data
  "det_dt" # tag detections and receiver data
  
)

## save out data
dat_nm <- "pikeperch-homerange-salinity-data.RData"
save(list = save_lst,
     file = here("data", dat_nm))
