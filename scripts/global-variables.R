# global variables --------------------------------------------------------

## projection for the riverdist river network
rn_proj <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs +type=crs"

## tidal limit (and points on bank for adding to map)
# tidal_limit <- data.frame("x_start" = -0.382880, "x_tidal_limit" = -0.321107, "x_end" = -0.253047,
#                           "y_start" = 51.454808, "y_tidal_limit" = 51.430123, "y_end" = 51.412556)
tidal_limit <- data.frame("x_start" = -0.488217, "x_tidal_limit" = -0.321107, "x_end" = -0.178739,
                          "y_start" = 51.485328, "y_tidal_limit" = 51.430123, "y_end" = 51.390142)

## barriers
barriers <- data.frame("name" = c("Weybridge Lock", "Sunbury Lock", "Hampton Marina", "Teddington Lock", "Richmond Lock"),
                       "x_start" = c(-0.458107, -0.410867, -0.348946, -0.322279, -0.316427),
                       "x" = c(-0.458000, -0.409760, -0.350094, -0.323055, -0.317204),
                       "x_end" = c(-0.457919, -0.408620, -0.351317, -0.323906, -0.318073),
                       "y_start" = c(51.382495, 51.404736, 51.408872, 51.431445, 51.462779),
                       "y" = c(51.382000, 51.404159, 51.407942, 51.430880, 51.462259),
                       "y_end" = c(51.381784, 51.403375, 51.407079, 51.430136, 51.461738))
