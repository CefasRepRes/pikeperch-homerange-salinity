# analysis script ---------------------------------------------------------

## a script `sourced` by the .qmd to run the analysis.


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
library(lme4) # for lmer() and glmer()
library(lmerTest) # for p values
library(AICcmodavg) # for aictable()
library(ggeffects) # for allEffects() [or predictorEffects()]
library(performance) # for r2_nakagawa()
library(DHARMa) # for testResiduals


# additional setup --------------------------------------------------------

## find parsimonious function
find_parsimonious <- function(tab, crit = 2) {
  top_ranked <- tab[1, ]
  most_parsim <- which(!tab[, "Delta_AIC"] >= crit & 
                         tab[, "K"] < top_ranked[, "K"])
  if (length(most_parsim) == 0) {
    most_parsim <- 1
  }
  return(most_parsim)
}
if (do_chks) {
  find_parsimonious(data.frame("Delta_AIC" = c(0, 1, 2, 3, 4), 
                               "K" = c(5, 4, 3, 2, 1)))
  find_parsimonious(data.frame("Delta_AIC" = c(0, 2, 2, 3, 4), 
                               "K" = c(5, 4, 3, 2, 1)))
  find_parsimonious(data.frame("Delta_AIC" = c(0, 1, 2, 3, 1), 
                               "K" = c(5, 6, 3, 2, 1)))
}

## gsignal::poly conflicts with stats::poly; if loaded, unload gsignal
if(any(grepl("gsignal", search()))) {
  detach("package:gsignal")
}


# read in organised data --------------------------------------------------

## read in latest processed data
if (file.exists(here("data", "pikeperch-homerange-salinity-data.RData"))) {
  load(here("data", "pikeperch-homerange-salinity-data.RData"))
} else {
  message("need to run previous scripts")
}



# additions ---------------------------------------------------------------

## add tail length and sex
rte_dt <- cbind(rte_dt,
                fish_dt[match(rte_dt$tag_id, ID), .("Tail length (mm)" = `TL (mm)`,
                                                    "Sex" = Sex)])


# dist exploration --------------------------------------------------------

## distance travelled by salinity at departure
dist_salin_dep_p <- ggplot(rte_dt, aes(x = salin_dep, y = dist_m,
                                       fill = Direction, colour = Direction)) + 
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  facet_wrap(~ state_dep) +
  labs(x = "Surface elevation at departure", 
       y = "Distance travelled (m)") +
  sgg()

## distance travelled by elevation at departure
dist_elev_dep_p <- ggplot(rte_dt, aes(x = elev_dep, y = dist_m,
                                      fill = Direction, colour = Direction)) + 
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  facet_wrap(~ state_dep) +
  labs(x = "Surface elevation at departure", 
       y = "Distance travelled (m)") +
  sgg()

## distance travelled by time of day at departure
dist_tod_dep_p <- ggplot(rte_dt, aes(x = Depart_1_tod, y = dist_m,
                                     fill = state_dep)) + 
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Time of day at departure", 
       y = "Distance travelled (m)") +
  scale_fill_discrete("Tide state") +
  sgg()

## distance travelled by length
dist_len_p <- ggplot(rte_dt, aes(x = `Tail length (mm)`, y = dist_m)) + 
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Tail length (mm)", 
       y = "Distance travelled (m)") +
  sgg()

## distance travelled by sex
dist_sex_p <- ggplot(rte_dt, aes(x = Sex, y = dist_m)) + 
  geom_boxplot() +
  scale_y_log10() +
  labs(x = "Sex", 
       y = "Distance travelled (m)") +
  sgg()

## combine them
dist_expl_p <- (dist_salin_dep_p + dist_tod_dep_p) /
  (dist_len_p + dist_sex_p) +
  plot_annotation(tag_levels = "A")

## plot effects
if (make_plots) {
  cairo_pdf(here('plots', 'dist_expl_plots.pdf'), 
            width = 10, height = 7)
  print(dist_expl_p)
  dev.off()
  jpeg(here('plots', 'dist_expl_plots.jpg'), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(dist_expl_p)
  dev.off()
}


# effect of explanatory variables on distances moved ----------------------

## models
dist_model_nms <- c("T * S * D * H^2 + L + X",
                    "T + S * D * H^2 + L + X",
                    "T * S * D * H^2",
                    "T + S * D * H^2",
                    
                    "S * D * H^2 + L + X", 
                    "S + D * H^2 + L + X",
                    "D + S * H^2 + L + X",
                    "S + D + H^2 + L + X",
                    "S * D * H^2", 
                    "S + D * H^2",
                    "D + S * H^2",
                    "S + D + H^2",
                    
                    "S * H^2 + L + X",
                    "S + H^2 + L + X",
                    "D * H^2 + L + X",
                    "D + H^2 + L + X",
                    "S * H^2",
                    "S + H^2",
                    "D * H^2",
                    "D + H^2",
                    
                    "S * D * H + L + X",
                    "S + D * H + L + X",
                    "D + S * H + L + X",
                    "S + D + H + L + X",
                    "S * D * H",
                    "S + D * H",
                    "D + S * H",
                    "S + D + H",
                    
                    "null")

## model list
dist_formula_lst <- as.list(paste("log(dist_m) ~ 1 +", 
                                  
                                  c("Depart_1_tod * state_dep * Direction * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "Depart_1_tod + state_dep * Direction * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "Depart_1_tod * state_dep * Direction * poly(salin_dep, 2, raw = TRUE)",
                                    "Depart_1_tod + state_dep * Direction * poly(salin_dep, 2, raw = TRUE)",
                                    
                                    "state_dep * Direction * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "state_dep + Direction * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "Direction + state_dep * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "Direction + state_dep + poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "state_dep * Direction * poly(salin_dep, 2, raw = TRUE)",
                                    "state_dep + Direction * poly(salin_dep, 2, raw = TRUE)",
                                    "Direction + state_dep * poly(salin_dep, 2, raw = TRUE)",
                                    "Direction + state_dep + poly(salin_dep, 2, raw = TRUE)",
                                    
                                    "state_dep * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "state_dep + poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "Direction * poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "Direction + poly(salin_dep, 2, raw = TRUE) + `Tail length (mm)` + Sex",
                                    "state_dep * poly(salin_dep, 2, raw = TRUE)",
                                    "state_dep + poly(salin_dep, 2, raw = TRUE)",
                                    "Direction * poly(salin_dep, 2, raw = TRUE)",
                                    "Direction + poly(salin_dep, 2, raw = TRUE)",
                                    
                                    "state_dep * Direction * salin_dep + `Tail length (mm)` + Sex",
                                    "state_dep + Direction * salin_dep + `Tail length (mm)` + Sex",
                                    "Direction + state_dep * salin_dep + `Tail length (mm)` + Sex",
                                    "Direction + state_dep + salin_dep + `Tail length (mm)` + Sex",
                                    "state_dep * Direction * salin_dep",
                                    "state_dep + Direction * salin_dep",
                                    "Direction + state_dep * salin_dep",
                                    "Direction + state_dep + salin_dep",
                                    
                                    ""),
                                  
                                  "+ (1|tag_id)"))

## add names
names(dist_formula_lst) <- dist_model_nms

## fit the models
dist_model_lst <- lapply(dist_formula_lst, 
                         lmer, 
                         data = rte_dt, REML = FALSE)

## make an AIC table
dist_model_aictab <- aictab(dist_model_lst)

## add R2
dist_model_aictab <- cbind(dist_model_aictab,
                           do.call("rbind", lapply(dist_model_lst, r2_nakagawa)))

## find most parsimonious model
dist_parsimonious <- 1 # find_parsimonious(dist_model_aictab) # over-riding coz marginal R2 is double (albeit low) that of the next best model

## best model
dist_best_model <- lmer(dist_formula_lst[[dist_model_aictab$Modnames[dist_parsimonious]]], 
                        data = rte_dt)

## get marginal effects
foo <- ggeffect(dist_best_model)
trms <- paste(names(foo), "[all]")

## make the plot(s)
dist_me_p <- plot(ggeffect(dist_best_model, terms = rev(trms))) + 
  labs(title = "", 
       x = "Salinity (Practical Salinity Unit [1 g / kg]) at departure", 
       y = "log Distance travelled (m)") + 
  theme(text = element_text(size = 16, family = "Times New Roman"), 
        panel.grid = element_blank(), 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 16))

## plot effects
if (make_plots) {
  cairo_pdf(here('plots', 'dist_lme_meplots.pdf'), 
            width = 10, height = 7)
  print(dist_me_p)
  dev.off()
  jpeg(here('plots', 'dist_lme_meplots.jpg'), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(dist_me_p)
  dev.off()
}

## plot the ranefs and residuals
# # random effect qq plot
# foo <- data.table("y" = unlist(ranef(dist_best_model)[["tag_id"]]))
# dist_best_qq_p <- ggplot(foo, aes(sample = y)) + 
#   stat_qq(size = 3) + 
#   stat_qq_line() +
#   labs(x = "Normal theoretical quantiles", y = "Sample quantiles") +
#   sgg()
# # random effect blup plot
# foo <- transform(as.data.table(ranef(dist_best_model)),
#                  lower = condval - 1.96 * condsd, 
#                  upper = condval + 1.96 * condsd)
# dist_best_ranef_p <- ggplot(foo, aes(x = grp, y = condval)) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linetype = "dashed") + 
#   labs(x = "Acoustic tag no.", y = "Best linear unbiased predictor") +
#   sgg()
# # residuals plot
# foo <- data.table("resid" = residuals(dist_best_model))
# dist_best_res_p <- ggplot(foo, aes(x = resid)) +
#   geom_histogram(binwidth = 0.1) +
#   geom_vline(xintercept = 0, linetype = "dashed") + 
#   labs(x = "Residual", y = "Frequency") +
#   sgg()

## combine the diagnostic plots
# dist_best_diag_p <- dist_best_qq_p / dist_best_ranef_p / dist_best_res_p +
#   plot_annotation(tag_levels = "A")
if (make_plots) {
  cairo_pdf(here('plots', 'dist_best_diag_plots.pdf'),
            width = 10, height = 7)
  fii <- capture.output(testResiduals(dist_best_model))
  dev.off()
  jpeg(here('plots', 'dist_best_diag_plots.jpg'),
       res = 300, width = (480 * 7), height = (480 * 5))
  fii <- capture.output(testResiduals(dist_best_model))
  dev.off()
}


# dir exploration ---------------------------------------------------------

## direction travelled by state at departure
dir_state_dep_p <- ggplot(rte_dt, aes(x = state_dep, fill = Direction)) + 
  geom_bar() +
  labs(x = "Tide state",
       y = "Frequency") +
  sgg()

## direction travelled by time of day at departure
dir_tod_dep_p <- ggplot(rte_dt, aes(x = state_dep,
                                    fill = Direction)) + 
  geom_bar() +
  facet_wrap(~ Depart_1_tod) +
  labs(x = "Tide state", 
       y = "Frequency") +
  sgg()

## tail length by direction travelled
dir_len_p <- ggplot(rte_dt, aes(x = Direction,
                                y = `Tail length (mm)`)) + 
  geom_boxplot() +
  labs(x = "Direction", 
       y = "Tail length (mm)") +
  sgg()

## direction travelled by sex
dir_sex_p <- ggplot(rte_dt, aes(x = Sex,
                                fill = Direction)) + 
  geom_bar() +
  labs(x = "Sex", 
       y = "Frequency") +
  sgg()

## combine them
dir_expl_p <- (dir_state_dep_p + dir_tod_dep_p) /
  (dir_len_p + dir_sex_p) +
  plot_annotation(tag_levels = "A")

## plot effects
if (make_plots) {
  cairo_pdf(here('plots', 'dir_expl_plots.pdf'), 
            width = 10, height = 7)
  print(dir_expl_p)
  dev.off()
  jpeg(here('plots', 'dir_expl_plots.jpg'), 
       res = 300, width = (480 * 7), height = (480 * 5))
  print(dir_expl_p)
  dev.off()
}


# effect of explanatory variables on direction moved ----------------------

## models
dir_model_nms <- c("S * T + L + X",
                   "S + T + L + X",
                   "S * T",
                   "S + T",
                   
                   "S + L + X",
                   "T + L + X",
                   "S",
                   "T",
                   
                   "null")

## model list
dir_formula_lst <- as.list(paste("Direction ~ 1", 
                       
                                 c("+ state_dep * Depart_1_tod + `Tail length (mm)` + Sex",
                                   "+ state_dep + Depart_1_tod + `Tail length (mm)` + Sex",
                                   "+ state_dep * Depart_1_tod",
                                   "+ state_dep + Depart_1_tod",
                                   
                                   "+ state_dep + `Tail length (mm)` + Sex",
                                   "+ Depart_1_tod + `Tail length (mm)` + Sex",
                                   "+ state_dep",
                                   "+ Depart_1_tod",
                                   
                                   ""))) #,
                                 
                                 # "+ (1|tag_id)"))

## add names
names(dir_formula_lst) <- dir_model_nms

## fit the models
dir_model_lst <- lapply(dir_formula_lst, 
                        # glmer, 
                        glm,
                        data = rte_dt, family = "binomial")

## make an AIC table
dir_model_aictab <- aictab(dir_model_lst)

## add R2
dir_model_aictab <- cbind(dir_model_aictab,
                          # do.call("rbind", lapply(dir_model_lst, r2_nakagawa)))
                          do.call("rbind", lapply(dir_model_lst, r2)))
setnames(dir_model_aictab, "R2_Tjur", "R2_marginal")

## find most parsimonious model
dir_parsimonious <- 1 # find_parsimonious(dir_model_aictab) # over-riding coz deltaAIC is 1.92, i.e., ~2

## best model
# dir_best_model <- glmer(dir_formula_lst[[dir_model_aictab$Modnames[dir_parsimonious]]], 
#                         data = rte_dt, family = "binomial")
dir_best_model <- glm(dir_formula_lst[[dir_model_aictab$Modnames[dir_parsimonious]]], 
                        data = rte_dt, family = "binomial")

## get marginal effects
foo <- ggeffect(dir_best_model)
trms <- paste(names(foo), "[all]")

## make the plot(s)
dir_me_p <- plot(ggeffect(dir_best_model, terms = rev(trms))) + 
  labs(title = "", x = "Tide state at departure", y = "Probability of upstream travel") + 
  theme(text = element_text(size = 16, family = "Times New Roman"), 
        panel.grid = element_blank(), 
        axis.line.x = element_line(colour = "black"), 
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(colour = "black", size = 16))

## plot effects
if (make_plots) {
  cairo_pdf(here('plots', 'dir_glme_meplots.pdf'),
            width = 10, height = 7)
  plot(dir_me_p, main = NA)
  dev.off()
  jpeg(here('plots', 'dir_glme_meplots.jpg'),
       res = 300, width = (480 * 7), height = (480 * 5))
  plot(dir_me_p, main = NA)
  dev.off()
}

## plot the ranefs and residuals
# # random effect qq plot
# foo <- data.table("y" = unlist(ranef(dir_best_model)[["tag_id"]]))
# dir_best_qq_p <- ggplot(foo, aes(sample = y)) + 
#   stat_qq(size = 3) + 
#   stat_qq_line() +
#   labs(x = "Normal theoretical quantiles", y = "Sample quantiles") +
#   sgg()
# # random effect blup plot
# foo <- transform(as.data.table(ranef(dir_best_model)),
#                  lower = condval - 1.96 * condsd, 
#                  upper = condval + 1.96 * condsd)
# dir_best_ranef_p <- ggplot(foo, aes(x = grp, y = condval)) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.25) +
#   geom_point(size = 3) +
#   geom_hline(yintercept = 0, linetype = "dashed") + 
#   labs(x = "Acoustic tag no.", y = "Best linear unbiased predictor") +
#   sgg()
# # residuals plot
# foo <- data.table("resid" = residuals(dir_best_model))
# dir_best_res_p <- ggplot(foo, aes(x = resid)) +
#   geom_histogram(binwidth = 0.1) +
#   geom_vline(xintercept = 0, linetype = "dashed") + 
#   labs(x = "Residual", y = "Frequency") +
#   sgg()

## combine the diagnostic plots
# dir_best_diag_p <- dir_best_qq_p / dir_best_ranef_p / dir_best_res_p +
#   plot_annotation(tag_levels = "A")
if (make_plots) {
  cairo_pdf(here('plots', 'dir_best_diag_plots.pdf'),
            width = 10, height = 7)
  fii <- capture.output(testResiduals(dir_best_model))
  dev.off()
  jpeg(here('plots', 'dir_best_diag_plots.jpg'),
       res = 300, width = (480 * 7), height = (480 * 5))
  fii <- capture.output(testResiduals(dir_best_model))
  dev.off()
}


# combined me results -----------------------------------------------------

## combined aic tabs
aic_tab <- rbindlist(list("Distance travelled (m)" = dist_model_aictab,
                          "Direction travelled" = dir_model_aictab),
                     idcol = "Variable", fill = TRUE)

## create table 6
tab6 <- aic_tab[, .("Response variable" = Variable,
                    "Model description" = Modnames,
                    "No. parameters" = as.numeric(K),
                    "AICc" = as.numeric(AICc),
                    "$\\delta$AICc" = as.numeric(Delta_AICc),
                    "Cumulative weight" = as.numeric(Cum.Wt))]
foo <- as.numeric(unlist(aic_tab$R2_conditional))
tab6[, "Conditional $R^2$"] <- c(foo,
                                 rep(NA, nrow(tab6) - length(foo)))
tab6[, "Marginal $R^2$"] <- as.numeric(unlist(aic_tab$R2_marginal))

## write it
fwrite(tab6, file = here("results", "aic-table.csv"))

## combined me plots
figure6 <- dist_me_p / dir_me_p + 
  plot_annotation(tag_levels = "A") + 
  plot_layout(guides = "collect")

## plot it
if (make_plots) {
  cairo_pdf(here('plots', 'figure6.pdf'),
            width = 7, height = 10)
  print(figure6)
  dev.off()
  jpeg(here('plots', 'figure6.jpg'),
       res = 300, width = (480 * 5), height = (480 * 7))
  print(figure6)
  dev.off()
}

## create table 6 - model summary
tab6a <- data.table("Variable" = c("Intercept", "Direction (u/s)", "Salinity at departure", "Salinity at departure^2", "Direction (u/s) : Salinity at departure", "Direction (u/s) : Salinity at departure^2"), 
                   "Estimate" = round(summary(dist_best_model)$coefficient[, 1], 3), 
                   "Std. Error" = round(summary(dist_best_model)$coefficient[, 2], 3), 
                   "Df" = round(summary(dist_best_model)$coefficient[, 3], 3), 
                   "t value" = round(summary(dist_best_model)$coefficient[, 4], 3), 
                   "p value" = round(summary(dist_best_model)$coefficient[, 5],3))

tab6b <- data.table("Variable" = c("Intercept", "Tide state at departure"), 
                    "Estimate" = round(summary(dir_best_model)$coefficient[, 1], 3), 
                    "Std. Error" = round(summary(dir_best_model)$coefficient[, 2], 3), 
                    "z value" = round(summary(dir_best_model)$coefficient[, 3], 3), 
                    "p value" = round(summary(dir_best_model)$coefficient[, 4], 3))

## write it
fwrite(tab6a, file = here("results", "dist-mod-sum-table.csv"))
fwrite(tab6b, file = here("results", "dir-mod-sum-table.csv"))                    


# save results ------------------------------------------------------------

## save list
save_lst <- c(
  
  "dist_model_lst", # distance travelled models
  "dir_model_lst", # direction travelled models

  "dist_model_aictab", # distance travelled AICc table
  "dir_model_aictab" # direction travelled AICc table
  
)

## save out data
dat_nm <- "pikeperch-homerange-salinity-results.RData"
save(list = save_lst,
     file = here("results", dat_nm))
