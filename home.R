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

  
  
  utm_52 <- "+proj=tmerc +lon_0=129 +x_0=500000 +datum=WGS84 +units=m"

a <- ne_countries(type = "countries", scale = "large")
qtm(a)
tmap_options(check.and.fix = TRUE)

library(spData)
data(world)
qtm(world)

world |> 
  st_transform(crs = 5186) |> 
  qtm()

world

library(sf)

st_crs(world)


world.ae <- st_transform(world, "+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
qtm(world.ae)

st_crs(world.ae)

world.robin <- st_transform(world,"+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
tm_shape(world.robin) + tm_fill()  

# Define new meridian
meridian2 <- 150

# Split world at new meridian
wld.new <- st_break_antimeridian(world, lon_0 = meridian2)
wld.rob.sf <-  st_transform(wld.new, 
                            paste("+proj=robin +lon_0=", meridian2 ,
                                  "+k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") )
tm_shape(wld.rob.sf) + tm_borders()


library(giscoR)

world_sf <- gisco_get_countries(resolution = 10)
qtm(world_sf)

crsrobin <- "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world_robinson2 <- world_sf %>%
  st_break_antimeridian(lon_0 = 150) %>% # insert this before transformation
  st_transform(crs = crsrobin)

qtm(world_robinson2)


bb <- c(-5000000, -500000, 5000000, 6000000)
tm_shape(countries, bbox = bb, projection = 32652) + tm_borders(col = "gray50", lwd = 0.75) +
  tm_shape(utm_grid) + tm_borders(col = "black", lwd = 1)

bb <- c(-80000, -40000, 670000, 720000)
tm_shape(sido, bbox = bb, projection = 5186) + tm_fill() +
  tm_grid(x = seq(-80000, 720000, 20000), y = seq(-40000, 720000, 20000), labels.show = FALSE, lwd = 1, alpha = 0.3) +
  tm_shape(origins) + tm_symbols(col = "black", size = 1) +
  tm_shape(topo_50) + tm_borders(col = "black", lwd = 1.5) +
  tm_shape(origin_mid) + tm_symbols(col = "red")

graticule_1 <- ne_download(scale = 50, type = "graticules_1", category = "physical") |> 
  st_as_sf()

a <- graticule_1 |> 
  st_cast("POLYGON")


bound <- tibble(x = c(124, 124, 132, 132), y = c(33, 39, 33, 39))
bound_sf <- st_as_sf(bound, coords = c("x", "y"), crs = 4326) 

bound_sf <- bound_sf |> 
  st_bbox() |> 
  st_as_sfc()
qtm(a)

graticule_sel <- st_intersection(graticule_1, bound_sf)
qtm(graticule_sel)

box_124_125_38_39 <- tibble(x = c(124, 124, 125, 125), y = c(38, 39, 38, 39))
box_125_126_38_39 <- tibble(x = c(125, 125, 126, 126), y = c(38, 39, 38, 39))

a.list <- list(box_124_125_38_39, box_125_126_38_39)

a.list |> 
  st_polygon()
a.list

a <- box_124_125_38_39 |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()

b <- box_125_126_38_39 |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()

a
b

st_combine(st_sfc(a, b))





bb <- tibble(x = c(-100000, 720000), y = c(-40000, 720000)) |> 
  st_as_sf(coords = c("x", "y"), crs = 5186) |> 
  st_bbox() |> 
  st_as_sfc()

bb.grid <- st_make_grid(bb, 20000)

tm_shape(bb.grid) + tm_borders(lty = "dotted", lwd = 0.2) +
  tm_shape(sido) + tm_fill() +
  tm_shape(origins) + tm_symbols(col = "black", size = 1) +
  tm_shape(topo_50) + tm_borders(col = "black", lwd = 1.5) +
  tm_shape(origin_mid) + tm_symbols(col = "red") +
  tm_graticules(x = c(124:132), lwd = 2)


# N52를 중심으로 한 대략적인 지역 선정
bb_utm_52 <- tibble(x = c(81, 81, 171, 171), y = c(-4, 60, -4, 60)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
qtm(bb_utm_52)

# 국가 셰이프 파일에서 해당 부분만 골라내기
countries_bb <- st_intersection(countries, bb_utm_52) |> 
  st_make_valid()
qtm(countries_bb)

# UTM N52의 투영법 적용(EPSG:32652)
countries_bb_utm <- st_transform(countries_bb, crs = 32652)
qtm(countries_bb_utm)

# UTM N52의 가상원점 지정
origin_n52 <- st_as_sf(tibble(x = 0, y = 0), coords = c("x", "y"), crs = 32652) |> 
  st_transform(crs = st_crs(countries))

# 그리드 생성 영역을 지정하고 그리드 생성
bb_n52 <- tibble(x = c(0, 1000000), y = c(-1000000, 7000000)) |> 
  st_as_sf(coords = c("x", "y"), crs = 32652) |> 
  st_bbox() |> 
  st_as_sfc()
qtm(bb_n52)

bb_n52_grid <- st_make_grid(bb_n52, 100000)
qtm(bb_n52_grid)

# 지도 표현 영역 지정
bb <- c(-3000000, -500000, 3500000, 6000000)
tm_shape(countries_bb_utm, bbox = bb) + tm_fill() +
  tm_graticules(x = seq(90, 172, 6), y = seq(0, 40, 10), lwd = 1.5) +
  tm_shape(bb_n52_grid) + tm_borders() +
  tm_shape(origin_n52) + tm_symbols(col = "red")
