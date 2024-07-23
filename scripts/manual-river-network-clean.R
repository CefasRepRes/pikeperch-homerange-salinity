# manual creating of the riverdist river network --------------------------

# libraries
library(riverdist)

# read as river network
rn <- line2network(path = "data", layer = "river_line_complete", reproject = rn_proj)

# run the clean up script from `riverdist` - this is the manual part
rn_clean <- cleanup(rn)

# save output
save(rn_clean, file = here("data", "rn_clean.RData"))
