# interpretation script ---------------------------------------------------

## a script `sourced` by the .qmd to help with interpretations.


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
library(ggplot2)
library(data.table)


# additional setup --------------------------------------------------------


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


# read in saved statistical analyses --------------------------------------

## read
f_nms <- list.files(path = here("results"), 
                    pattern = "pikeperch-homerange-salinity-results.RData")
if (length(f_nms) != 0) {
  f_dts <- as.Date(gsub("[a-zA-Z_.]", "\\1", f_nms), "%d%m%Y")
  f_fle <- f_nms[order(f_dts, decreasing = TRUE)][1]
  load(here("results", f_fle))
}

f_nms <- list.files(path = here("results"), 
                    pattern = "pikeperch-homerange-salinity-rawplots.RData")
if (length(f_nms) != 0) {
  f_dts <- as.Date(gsub("[a-zA-Z_.]", "\\1", f_nms), "%d%m%Y")
  f_fle <- f_nms[order(f_dts, decreasing = TRUE)][1]
  load(here("results", f_fle))
}


# detections of River Lee fish --------------------------------------------

## range and counts of detections
aa <- subset(det_dt, tag_id %in% 
               fish_caps[grep("Lee", Site), ID])[, 
                                                 .(datetime = range(DateTime),
                                                   count = .N), 
                                                 by = .(tag_id, 
                                                        `Receiver No.`)]

## print it 
print(aa)


# detections of River Thames fish -----------------------------------------

## numbers of journeys 
bb <- rte_dt[tag_id %in% fish_caps[grep("Thames", Site), ID], 
             .N, 
             by = .(tag_id, 
                    Location_1, 
                    Location_2, 
                    "Period" = factor(format(Depart_1, "%Y-%m")))]

## print example subset; 1  2  3  4  5  6  7 18 26 27 28 30 46 47 48 49 57 63 64 65
print(subset(bb, tag_id %in% c(46, 26)))


# use of saline environment -----------------------------------------------

## get the stations recording detections
cc <- stn_dt[Sort <= det_dt[!is.na(File), max(Sort)], ]

## print it
print(cc)

## get the average salinity at the most downstream stations
dd <- env_dt[logger == cc[nrow(cc), `DST No.`], ]

## print it
print(mean(dd$salinity))
print(range(dd$salinity))

## print a comparison
ee <- env_dt[, .(median = mean(salinity),
                 quantile10 = quantile(salinity, prob = 0.1),
                 quantile90 = quantile(salinity, prob = 0.9)), 
             by = logger]

## print another comparison
ff <- rte_dt[!is.na(logger_dep), .(median = median(salin_dep),
                                   quantile10 = quantile(salin_dep, prob = 0.1),
                                   quantile90 = quantile(salin_dep, prob = 0.9)), 
             by = .(Location_1, logger_dep)]

## fish detected downstream
gg <- subset(rte_dt, Location_1 == "Chiswick Pier, Corney Reach Way" | 
               Location_2 == "Chiswick Pier, Corney Reach Way")[, unique(tag_id)]
print(gg)

## duration of each tidal movement
tab5[, "Tidal movement number" := with(rle(Tidal), 
                                       rep(1:length(lengths), lengths)),
     by = `Acoustic transmitter`]
tab5[Tidal == "No", "Tidal movement number"] <- 0
foo <- tab5[`Tidal movement number` != 0, .("Arrival" = min(Arrival),
                                            "Departure" = max(Departure),
                                            "Number of movements" = .N),
            by = .(`Acoustic transmitter`, `Tidal movement number`)]
foo[, "Time spent" := difftime(Departure, Arrival, units = "days")]

## from JEMs pp_plots.qmd

pp_dat <- tab5
pp_dat[, "Time spent" := as.numeric(`Time spent`)]
pp_dat[, "Acoustic transmitter" := as.integer(`Acoustic transmitter`)]

# subset to fish who moved to tidal locs
tidal_fish <- unique(pp_dat[`Tidal movement number` > 1, `Acoustic transmitter`])
pp_sub <- pp_dat[which(`Acoustic transmitter` %in% tidal_fish),]

# subset to tidal movement records only
pp_subbed <- pp_sub[which(`Tidal movement number` != 1 & Tidal == "Yes"), ]

# create dummy start rows for each fish and tidal movement
start_rows <- pp_subbed[, .(`Acoustic transmitter` = unique(`Acoustic transmitter`)), by = .(`Tidal movement number`)]
start_rows[, c("Station", "Time spent") := .("Start", 0)]

# fix levels and set order
ddd <- rbind(pp_subbed, start_rows, fill = T)
ddd$Station <- factor(ddd$Station, levels = c("Start", "Thames Path, Kew Bridge", "Chiswick Pier, Corney Reach Way"))
setorder(ddd, `Acoustic transmitter`, `Tidal movement number`, Arrival)

# calculate cumulative sum of time spent  for each fish tidal movement
ddd[, cum_time_spent := cumsum(`Time spent`), by = .(`Acoustic transmitter`, `Tidal movement number`)]

# get unique id for each fish and tidal movement
ddd[, fish_id := paste0(`Acoustic transmitter`,"-", `Tidal movement number`)]

# id for plot colours
ddd[, station_col := c(1,1,1,1,1,1,1,2,1,1,1,2,1,1,1,2,1,2,1,2,1,2,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,2,1,2,1,1,2,1,1)]

# correct order of tidal movement events
ddd$fish_id <- factor(ddd$fish_id, levels = rev(c(unique(ddd$fish_id))))

# reorder panels
ddd[, `Acoustic transmitter` := factor(`Acoustic transmitter`, levels = c("26", "27", "3", "4"))]

fig5 <- ggplot(ddd, aes(x = cum_time_spent, y = fish_id)) +
  geom_line(aes(colour = factor(station_col), group = fish_id), linewidth = 8) +
  facet_wrap(~`Acoustic transmitter`, scales = 'free') +
  labs(colour = "Tidal receiver:", 
       x = "Days at receiver", 
       y = "Movement events") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        legend.position = "top") +
  scale_colour_manual(breaks = c(1, 2), 
                      # values = c("deepskyblue", "maroon2"), 
                      values = c("#F8766D", "#00BFC4"),
                      labels = c("Thames Path, Kew Bridge", "Chiswick Pier, Corney Reach Way")) +
  scale_x_sqrt() +
  sgg()

## figure 5
if (make_plots) {
  cairo_pdf(here("plots", "figure7.pdf"), 
            width =10, height = 7)
  print(fig5)
  dev.off()
  jpeg(here("plots", "figure7.jpg"), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(fig5)
  dev.off()
}

