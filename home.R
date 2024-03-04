library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(rayshader)

library(pals)
alphabet2()
glasbey()

library(ggpattern)

states_map <- map_data("state")

ggplot(states_map %>% distinct(region), aes(map_id = region)) +
  geom_map_pattern(
    map = states_map,
    aes(
      pattern_angle = region
    ),
    pattern = "stripe",
    pattern_fill = "purple",
    pattern_aspect_ratio = 1.75,
    fill = "red",
    colour = "black",
  ) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map()


my_df_2 <- my_df |> 
  mutate(
    lon = st_coordinates(geometry)[[1]],
    lat = st_coordinates(geometry)[[2]])


View(my_df_2)

install.packages("tmap")
library(tmap)

min_max <- my_df |> 
  st_drop_geometry() |> 
  summarize(
    min_value = format(round(min(House1_p, na.rm = TRUE), 
                             digits = 1), nsmall = 1),
    max_value = format(round(max(House1_p, na.rm = TRUE), 
                             digits = 1), nsmall = 1)
  )
min.val <- min_max[[1]]
max.val <- min_max[[2]]

my_map <- tm_shape(my_df) +
  tm_polygons(
    "House1_p",
    style = "fixed", 
    palette = "BuPu", 
    border.col = "gray40", 
    lwd = 0.5, 
    border.alpha = 1, 
    breaks = c(-Inf, 15, 25, 35, 45, 55, 65, Inf),
    labels = c(paste0("< 15", " (Min. ", min.val, ")"), 
               "15 ~ 25", "25 ~ 35", "35 ~ 45", "45 ~ 55", 
               "55 ~ 65", paste0(">= 65", " (Max. ", max.val, ")")), 
    title = "One-Person (%)", 
    legend.hist = FALSE, 
    legend.show = TRUE) +
  tm_shape(seoul_gu) + 
  tm_polygons(alpha = 0, border.col = "gray20", lwd = 1.5) +
  tm_legend(
    legend.format = list(digits = 1), 
    legend.title.size = 1.2, 
    legend.text.size = 0.9,
    legend.position = c(0.85, 0.05)) +
  tm_layout(
    frame = TRUE, 
    title = "One-Person Households in Seoul, 2020", 
    title.position = c("center", "top"),
    title.size = 1.8,
    inner.margins = c(0.05, 0.05, 0.08, 0.12)) +
  tm_scale_bar(
    color.dark = "gray50",
    text.size = 0.5,
    breaks = seq(0, 10, 2),
    position = c(0.03, 0.01)) + 
  tm_credits(
    "SANG-IL LEE, Geograhy Education at SNU, 2024", 
    size = 0.6, 
    position = c(0.78, 0.01)
  )

my_map


# my_ratio <- get_asp_ratio(my_map)
my_title <- "first_map"
my.path.name <- "outputs/"
my.file.name <- paste0(my.path.name, my_title, ".png")
tmap_save(
  my_map, 
  filename = my.file.name, 
  width = 11.5, 
  dpi = 600)

# tmap_save(my_map, filename = my.file.name, height = height_f, width = height_f * my_ratio, dpi = 600)




tm_shape(my_df) +
  tm_polygons("House1_p", style = "fixed", palette = "BuPu", border.col = "gray40", lwd = 0.8, border.alpha = 1, 
              breaks = c(-Inf, 15, 25, 35, 45, 55, 65, Inf),
              labels = c(paste0("< 15", " (Min. ", min.val, ")"), "15 ~ 25", "25 ~ 35", "35 ~ 45", "45 ~ 55", "55 ~ 65",   
                         paste0(">= 65", " (Max. ", max.val, ")")), 
              title = "One-Person (%)", legend.hist = FALSE, legend.show = TRUE) +
  tm_shape(seoul_gu) + tm_polygons(alpha = 0, border.col = "gray30", lwd = 2) +
  tm_legend(legend.format = list(digits = 1), legend.title.size = 1.5, legend.text.size = 1.25, legend.position = c(0.85, 0.06)) +
  tm_layout(frame = TRUE, title = "One-Person Households in Seoul, 2020", title.size = 2, title.position = c("center", "top"), 
            bg.color = "white", inner.margins = c(0.05, 0.05, 0.08, 0.12)) +
  tm_scale_bar(color.dark = "gray60", breaks = seq(0, 10, 2), position = c(0.03, 0.01), text.size = 0.6) + 
  tm_credits("SANG-IL LEE, Geograhy Education at SNU, 2023", size = 0.7, position = c(0.8, 0.01))





















 
  tm_shape(tps) + 
  tm_raster(
    style = "fixed", 
    breaks = c(-Inf, 10000, 15000, 20000, 25000, 30000, 40000, Inf),     palette = "Oranges", legend.show = FALSE) +
  tm_shape(subset(
    cont.tps, 
    level %in% c(10000, 15000, 20000, 25000, 30000, 40000))) +  
  tm_iso(
    text = "level", 
    color = "black", 
    lwd = 0.5, 
    size = 0.5, 
    alpha = 0.7) +
  tm_shape(seoul_gu) + 
  tm_polygons(alpha = 0, border.col = "gray20", lwd = 0.75) +
  tm_credits(
    "SANG-IL LEE, Geograhy Education at SNU, 2024", 
    size = 0.3, position = c(0.78, 0.01)
  )
