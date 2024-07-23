# raw data plotting script ------------------------------------------------

## a script `sourced` by the .qmd to make exploratory plots of the data before analysis.

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
library(mapdata) # for map()
library(patchwork) # for plots
library(raster) # for raster() 
library(ggrepel) # for geom_text_repel()


# additional setup --------------------------------------------------------

## https://rpubs.com/geospacedman/routing - could be helpful?


# read in organised data --------------------------------------------------

## read in latest processed data
if (file.exists(here("data", "pikeperch-homerange-salinity-data.RData"))) {
  load(here("data", "pikeperch-homerange-salinity-data.RData"))
} else {
  message("need to run previous scripts")
}

if (file.exists(here("data", "pikeperch-homerange-salinity-rawtables.RData"))) {
  load(here("data", "pikeperch-homerange-salinity-rawtables.RData"))
} else {
  message("need to run previous scripts")
}


# receiver and logger locations -------------------------------------------

## read processed river_line shapefile
river_line <- st_read(here("data", "river_line_complete.shp"))

## read processed river_poly raster
river_poly <- raster(here("data", "river_poly.tiff"))

## read poly of thames
secs <- st_read(here("data", "WFD_Transitional_Water_Bodies_Cycle_3.shp"))

## receiver location bounding box
bb <- st_bbox(river_line)

## receiver coordinates
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

## river labels
fii <- data.frame(
  name = c("River Thames", "River Lee"),
  lat = c(51.3, 51.65),
  long = c(-0.4, 0.15))

## map of receiver locations
logger_map <- ggplot() +
  geom_sf(data = river_line, colour = "skyblue3",
          inherit.aes = FALSE) +
  geom_sf(data = secs, fill = "skyblue3", colour = "skyblue3",
          inherit.aes = FALSE) +
  geom_segment(data = tidal_limit, aes(x = x_start, xend = x_end,
                                       y = y_start, yend = y_end),
               linetype = "dashed", inherit.aes = FALSE) +
  geom_point(data = barriers, aes(x = x, y = y),
             shape = 8, size = 5, inherit.aes = FALSE) +
  geom_point(data = foo, aes(x = Longitude, y = Latitude, colour = tidal), 
             size = 3) +
  geom_text_repel(data = foo,
                  aes(x = Longitude, y = Latitude, 
                      label = label), 
                  # min.segment.length = 4, force_pull = 5,
                  # max.overlaps = Inf,
                  size = 6) +
  borders(colour = "black") +
  coord_sf(xlim = bb[c("xmin", "xmax")], 
           ylim = bb[c("ymin", "ymax")] + c(-0.1, 0)) +
  labs(x = "Longitude", y = "Latitude") +
  theme(legend.position = "none") +
  scale_fill_manual(breaks = 1, values = "skyblue3", 
                    na.value = "transparent") +
  scale_colour_manual("Tidal\nreceiver", values = c("No" = "blue", 
                                                    "Yes" = "red")) + 
  geom_text(data = fii, 
            aes(x = long, y = lat, label = name), 
            inherit.aes = FALSE,
            size = 7) +
  sgg()

## world data
worldmap <- st_as_sf(map("world", plot = FALSE, fill = TRUE))

## make inset map showing London in UK
london_map <- ggplot() + 
  geom_sf(data = worldmap, fill = "lightgray") +
  geom_point(aes(x = mean(bb[c("xmin", "xmax")]),
                 y = mean(bb[c("ymin", "ymax")])),
             size = 8, shape = 22, colour = "black") +
  coord_sf(xlim = c(-10, 3), 
           ylim = c(50.3, 59)) +
  labs(x = "Longitude", y = "Latitude") +
  sgg()

## combine maps
pp <- logger_map + 
  inset_element(london_map, 
                left = 0, right = 0.25, 
                top = 1, bottom = 0.75) + 
  theme_void() + theme(plot.background = element_rect())


# summary plots -----------------------------------------------------------

## create total detections by `Location name` and `Receiver No.` dataset for plotting
foo <- det_dt[, .N, by = .(StationName, `Receiver No.`)]

## indicate if no detections
foo[, NoDetections := StationName %in% det_dt[is.na(tag_id), StationName]]

## identify receivers that were reused
foo[, Reuse := ifelse(duplicated(`Receiver No.`) | 
                        duplicated(`Receiver No.`, fromLast = TRUE), 
                    "*", "")]

## make plot; optional reordering on x by N
det_count_all_p <- ggplot(foo, 
                          # aes(x = reorder(factor(StationName), -N, sum), y = N, 
                          aes(x = StationName, y = N, 
                              fill = `Receiver No.`, Reuse = Reuse)) + 
  geom_col() + 
  geom_text(aes(label = paste0(after_stat(y), Reuse), group = StationName), 
            stat = "summary", fun = sum, vjust = -0.5,
            size = 5, family = "Times New Roman") +
  labs(x = "Location name", y = "Number of detections") +
  theme(legend.position = c(0.5, 0.8), legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  sgg()

## identify receivers that were reused
fii <- subset(foo, !NoDetections)
fii[, Reuse := ifelse(duplicated(`Receiver No.`) | 
                        duplicated(`Receiver No.`, fromLast = TRUE), 
                      "*", "")]

## make plot; optional reordering on x by N
det_count_p <- ggplot(fii, 
                      # aes(x = reorder(factor(StationName), -N, sum), y = N, 
                      aes(x = StationName, y = N, 
                          fill = `Receiver No.`, Reuse = Reuse)) + 
  geom_col() + 
  geom_text(aes(label = paste0(after_stat(y), Reuse), group = StationName), 
            stat = "summary", fun = sum, vjust = -0.5,
            size = 5, family = "Times New Roman") +
  labs(x = "Location name", y = "Number of detections") +
  theme(legend.position = c(0.5, 0.8), legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  sgg()

## stations by tag_id on map
aa <- rte_dt[, .N, by = .(tag_id, Location_1, Location_2)]
bb1 <- unique(rec_dt[, .(Location_1, 
                         "Latitude_1" = Latitude, 
                         "Longitude_1" = Longitude)])
aa <- bb1[, .(Location_1, Latitude_1, Longitude_1)][aa, on = "Location_1"]
foo <- aa[, .("N" = sum(N)), by = .(tag_id, Location_1, 
                                    "Latitude" = Latitude_1, 
                                    "Longitude" = Longitude_1)]

## make plot
tag_pings_map_p <- ggplot(foo, aes(x = Longitude, y = Latitude, size = N)) +
  geom_sf(data = river_line, colour = "skyblue3",
          inherit.aes = FALSE) +
  geom_sf(data = secs, fill = "skyblue3", colour = "skyblue3",
          inherit.aes = FALSE) +
  geom_segment(data = tidal_limit, aes(x = x_start, xend = x_end,
                                       y = y_start, yend = y_end),
               linetype = "dashed", inherit.aes = FALSE) +
  geom_point(shape = 1, colour = "red", stroke = 1.25) +
  coord_sf(xlim = bb[c("xmin", "xmax")], 
           ylim = bb[c("ymin", "ymax")]) +
  labs(x = "Longitude", y = "Latitude") +
  facet_wrap(~ tag_id) +
  borders(colour = "black") +
  scale_y_continuous(breaks = seq(round(bb["ymin"], 1),
                                  round(bb["ymax"], 1),
                                  length.out = 3)) + 
  scale_x_continuous(breaks = seq(round(bb["xmin"] + 0.15, 1),
                                  round(bb["xmax"] - 0.15, 1),
                                  length.out = 2)) +
  theme(legend.position = "top") +
  sgg()


# environmental data ------------------------------------------------------

## salinity plot
salinity_plot <- ggplot(rec_dt[!is.na(logger), ], 
                        aes(x = datetime, y = `salinity clean`, 
                            colour = logger)) + 
  geom_line() + 
  facet_wrap(~ `Receiver No.` + `Location name`, scales = "free_y") +
  labs(x = "Time (days)", y = "Salinity (Practical Salinity Unit [1 g / kg])") +
  sgg()

## depth plot
depth_plot <- ggplot(rec_dt[!is.na(logger), ], 
                     aes(x = datetime, y = depth, 
                         colour = logger)) + 
  geom_line() + 
  facet_wrap(~ `Receiver No.` + `Location name`, scales = "free_y") +
  labs(x = "Time (days)", y = "Depth (m)") +
  sgg()
depth_plot2 <- ggplot(rec_dt[!is.na(logger), ], 
                      aes(x = datetime, y = depth)) + 
  geom_line(aes(group = 1)) + 
  facet_wrap(~ stnreclog, scales = "free") +
  labs(x = "Time (days)", y = "Depth (m)") +
  sgg()

## temperature plot
temperature_plot <- ggplot(rec_dt[!is.na(logger), ], 
                           aes(x = datetime, y = temperature, 
                               colour = logger)) + 
  geom_line() + 
  facet_wrap(~ `Receiver No.` + `Location name`, scales = "free_y") +
  labs(x = "Time (days)", y = "Depth (m)") +
  sgg()
temperature_plot2 <- ggplot(rec_dt[!is.na(logger), ], 
                            aes(x = datetime, y = temperature)) + 
  geom_line(aes(group = 1)) + 
  facet_wrap(~ stnreclog, scales = "free") +
  labs(x = "Time (days)", y = "Depth (m)") +
  sgg()

## plot original and "clean"
foo <- melt(rec_dt[!is.na(logger), ], 
            id.vars = c("logger", "datetime"), 
            measure.vars = c("salinity", "salinity clean"), 
            variable.name = "time series", value.name = "salinity")
idx <- with(rec_dt, which(salinity != `salinity clean`))
foo[`time series` == "salinity", ][idx, salinity := NA]
salinity_outliers_plot <- ggplot(foo, 
                                 aes(x = datetime, y = salinity, 
                                     colour = `time series`)) + 
  geom_line() + 
  facet_wrap(~ logger, scales = "free") +
  labs(x = "Time", y = "Salinity (Practical Salinity Unit [1 g / kg])") +
  theme(legend.position = "top") +
  scale_color_discrete("Time series:") + 
  sgg()


# BODC tide data ----------------------------------------------------------

## make a plot of the tide data
tide_p <- ggplot(tide_dt, 
                 aes(x = datetime, y = `Surface elevation`)) + 
  geom_line() + 
  labs(x = "Time (days)", y = "Surface elevation") +
  sgg()

## combine env_dt and tide_dt
tmp <- tide_dt[env_dt,
               roll = "nearest",
               on = .(datetime)]

## plot their correspondence
tide_salin_p <- ggplot(subset(tmp, logger == 1773), 
                       aes(x = datetime)) + 
  geom_line(aes(y = `Surface elevation`, colour = "Surface elevation")) + 
  geom_line(aes(y = salinity, colour = "Salinity")) +
  labs(x = "Time (days)", y = "Salinity or Surface elevation", colour = "Legend") +
  scale_colour_manual(values = c("Salinity" = "blue", 
                                 "Surface elevation" = "red")) +
  sgg()

## standardise Surface elevation (sur_scl) and salinity (sal_scl)
tmp[, c("sur_scl", "sal_scl") := lapply(.SD, 
                                        function(v) 
                                          as.numeric(scale(v))), 
    by = logger, 
    .SDcols = c("Surface elevation", "salinity")]

## remake plot for all loggers
tide_salin_all_p <- ggplot(tmp, 
                           aes(x = datetime)) + 
  geom_line(aes(y = sal_scl, colour = "Salinity")) +
  geom_line(aes(y = sur_scl, colour = "Surface elevation"), alpha = 0.4) + 
  labs(x = "Time (days)", y = "Salinity or Surface elevation", colour = "Legend") +
  scale_colour_manual(values = c("Salinity" = "blue", 
                                 "Surface elevation" = "red")) +
  facet_wrap(~ logger, scales = "free") +
  sgg()


# time spent in tidal areas -----------------------------------------------

## make boxplot
time_in_tidal_p <- ggplot(tab5, 
                          aes(x = `Acoustic transmitter`, 
                              y = as.numeric(`Time spent`), 
                              colour = factor(Tidal))) + 
  geom_boxplot(linewidth = 1.25) + 
  scale_y_sqrt() + 
  labs(x = "Acoustic transmitter", y = "Time spent (days)", colour = "Tidal zone") + 
  sgg()


# figures -----------------------------------------------------------------

## figure 1
fig2 <- pp
if (make_plots) {
  cairo_pdf(here("plots", "figure2.pdf"), 
            # width = 10, height = 10)
            width = 12, height = 12)
  print(fig2)
  dev.off()
  jpeg(here("plots", "figure2.jpg"), 
       # res = 300, width = (480 * 7), height = (480 * 7))
       res = 300, width = (480 * 8), height = (480 * 8))
  print(fig2)
  dev.off()
}

## figure 3
fig3 <- det_count_p
if (make_plots) {
  cairo_pdf(here("plots", "figure3.pdf"), 
            width = 10, height = 10)
  print(fig3)
  dev.off()
  jpeg(here("plots", "figure3.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 7))
  print(fig3)
  dev.off()
}

## figure 4
fig4 <- tag_pings_map_p
if (make_plots) {
  cairo_pdf(here("plots", "figure4.pdf"), 
            width = 10, height = 6)
  print(fig4)
  dev.off()
  jpeg(here("plots", "figure4.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 4))
  print(fig4)
  dev.off()
}

## figure 5
fig5 <- time_in_tidal_p
if (make_plots) {
  cairo_pdf(here("plots", "figure5.pdf"), 
            width = 7, height = 4)
  print(fig5)
  dev.off()
  jpeg(here("plots", "figure5.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 4))
  print(fig5)
  dev.off()
}

## salinity plots
if (make_plots) {
  cairo_pdf(here("plots", "salinity-plot.pdf"), 
            width = 14, height = 7)
  print(salinity_plot)
  dev.off()
  jpeg(here("plots", "salinity-plot.jpg"), 
       res = 300, width = (480 * 10), height = (480 * 5))
  print(salinity_plot)
  dev.off()
}
if (make_plots) {
  cairo_pdf(here("plots", "salinity-plot-cleaned.pdf"), 
            width = 10, height = 7)
  print(salinity_outliers_plot)
  dev.off()
  jpeg(here("plots", "salinity-plot-cleaned.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(salinity_outliers_plot)
  dev.off()
}
salin_plots <- salinity_plot / salinity_outliers_plot
if (make_plots) {
  cairo_pdf(here("plots", "salinity-plots.pdf"), 
            width = 10, height = 12)
  print(salin_plots)
  dev.off()
  jpeg(here("plots", "salinity-plots.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 8))
  print(salin_plots)
  dev.off()
}

## tide plots
tide_plots <- tide_p / tide_salin_p + plot_annotation(tag_levels = "A")
if (make_plots) {
  cairo_pdf(here("plots", "tide-plots.pdf"), 
            width = 10, height = 12)
  print(tide_plots)
  dev.off()
  jpeg(here("plots", "tide-plots.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 8))
  print(tide_plots)
  dev.off()
}


# save data ---------------------------------------------------------------

## save list
save_lst <- c(
  
  "fig2", # logger map
  "fig3", # detection counts
  "fig4",  # detection maps
  "fig5", # time in tidal
  
  "salin_plots", # salinity plots
  "tide_plots" # tide plots
  
)

## save out data
dat_nm <- "pikeperch-homerange-salinity-rawplots.RData"
save(list = save_lst,
     file = here("data", dat_nm))
