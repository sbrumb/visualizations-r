library(tidyverse)
library(extrafont)
library(rgdal)
library(curl)
library(ggthemes)
library(viridis)
library(geofacet)
library(scales)
library(Cairo)
library(maptools)

employment <- read_csv('data/state transportation employment.csv')

# Calculate each state's percentage of employees; group them in six categories
employment_pct <- employment %>%
  mutate(pct = Employees / sum(Employees),
         pct_category = cut(pct,
                            c(0, .01, .02, .03, .04, .05, 1),
                            c("0-1%", "1-2%", "2-3%", "3-4%", "4-5%",
                              "5% or more")))

# Get a hexagonal tile map
map_url <- paste0("https://andrew.cartodb.com/api/v2/sql?",
                  "filename=us_states_hexgrid&q=SELECT+*+FROM",
                  "+andrew.us_states_hexgrid&format=geojson&api_key=")
res <- curl_fetch_disk(map_url, "hexes.json")
hex <- readOGR(dsn = "hexes.json", encoding = "OGRGeoJSON")

# Calculate center of each hexagon for placing labels
hex_map <- hex %>% fortify(region = "iso3166_2")
cnames <- aggregate(cbind(long, lat) ~ id, data = hex_map,
                    FUN = function(x) mean(range(x)) )

theme_sb <-
  theme_void() +
  theme(
    text = element_text(family = "Gill Sans MT", size = 6),
    plot.title = element_text(size = 9),
    plot.background = element_rect(fill = "white"),
    plot.subtitle = element_text(size = 6, face = "plain", hjust = 0),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.key.height = unit(.6, 'lines'),
    legend.key.width = unit(3, 'lines'),
    plot.margin = unit(c(.1, .1, .1, .1), "in"),
    panel.border = element_blank(),
    panel.spacing = unit(.5, "lines"),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 7),
    legend.text = element_text(size = 6)
  )


ggplot() +
  geom_map(data = employment_pct, map = hex_map,
           aes(fill = pct_category, map_id = Abbreviation),
           color = "white", size = 0.5) +
  expand_limits(x = hex_map$long, y = hex_map$lat) +
  scale_fill_viridis(option = "plasma", begin = 0.1, end = 0.9,
                     na.value = "lightgray", discrete = TRUE) +
  geom_text(data = cnames, aes(long, lat, label = id),
            color = "#ffffff", size = 3, family = "Gill Sans MT") +
  coord_map() +
  labs(title = "Percentage of transportation employees by state") +
  theme_sb

ggsave("plots/hex tile map.png",
       width = 6.5, height = 3.5,
       type = "cairo-png")
