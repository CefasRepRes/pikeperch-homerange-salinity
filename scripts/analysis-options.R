# analysis options --------------------------------------------------------

# MCMC settings
setts <- list('na' = 10,
              'nb' = 100,
              'ni' = 500,
              'nt' = 1)
setts.m <- 100
if (!testing) setts <- lapply(setts, function(v) v * setts.m)
setts$nc <- 3
