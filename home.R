library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(tmaptools)
library(rayshader)

library(pals)
alphabet2()
glasbey()

seoul_EMD <- read_sf(
  "D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/2 Original Cleaning/2021_4Q/SEOUL_EMD_2021_4Q.shp", options = "ENCODING=CP949")
seoul_EMD_2020 <- read_sf(
  "D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/2 Original Cleaning/2020_2Q/SEOUL_EMD_2020_2Q.shp", options = "ENCODING=CP949")
seoul_gu <- read_sf(
  "D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/2 Original Cleaning/2021_4Q/SEOUL_GU_2021_4Q.shp", options = "ENCODING=CP949")
seoul_sido <- read_sf(
  "D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/2 Original Cleaning/2021_4Q/SEOUL_SIDO_2021_4Q.shp", options = "ENCODING=CP949")

house_SDGGEMD_2020 <- read_excel(
  "D:/My R/Population Geography/3 Population Structure/Housing_Size_2020_Adj.xlsx", sheet = 1
)

seoul_EMD_2020 <- seoul_EMD_2020 |> 
  mutate(
    EMD_ID = as.numeric(EMD_ID)
  )

my_df <-seoul_EMD_2020 |> 
  left_join(
    house_SDGGEMD_2020, join_by(EMD_ID == Code)
  )

seoul_gu <- seoul_gu |> 
  left_join(
    house_SDGGEMD_2020, join_by(SGG1_CD == Code)
  )


library(ggpattern)

states_map <- map_data("state")

ggplot(states_map  |> distinct(region), aes(map_id = region)) +
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

# 방향
ggplot(seoul_gu) +
  geom_sf_pattern(
    aes(pattern_angle = SGG1_CD),
    pattern = "stripe",
    pattern_size = 0.1,
    pattern_spacing = 0.02,
    pattern_fill = "white",
    pattern_color = "gray30",
    pattern_density = 1,
    color = "black",
    fill = "white",
    lwd = 0.5,
    show.legend = FALSE
  ) +
  coord_sf() + 
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
  
# 간격
ggplot(seoul_gu) +
  geom_sf_pattern(
    aes(pattern_spacing = -House1_p),
    pattern = "weave",
    pattern_angle = 45,
    pattern_size = 0.1,
    pattern_fill = "white",
    pattern_color = "gray30",
    pattern_density = 1,
    color = "black",
    fill = "white",
    lwd = 0.5,
    show.legend = FALSE
  ) +
  coord_sf() + 
  theme_bw() +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# 질감

library(gridpattern)
magick_25 <- sample(c("bricks", "checkerboard", "circles", "crosshatch", "fishscales", "hexagons",
               "horizontal",  "horizontalsaw",   "hs_bdiagonal", "hs_cross", "hs_diagcross",
               "hs_fdiagonal", "hs_horizontal", "hs_vertical", "left30", "leftshingle", 
               "octagons", "right45", "rightshingle", "smallfishscales", "vertical", 
               "verticalbricks", "verticalleftshingle", "verticalrightshingle", "verticalsaw"), 25)        

ggplot(seoul_gu_df) +
  geom_sf_pattern(
    aes(pattern_type = SGG1_FNM),
    pattern = "magick",
    pattern_scale = 2.5,
    pattern_fill = "black",
    color = "black",
    fill = "white",
    lwd = 0.5,
    show.legend = FALSE
  ) +
  coord_sf() + 
  theme_bw() +
  scale_pattern_type_discrete(choices = magick_25) +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# 모양: 에러는 없지만 구가 텅텅비어 있음. 나중에 다시 시도.

glyphs25 <- c(
  "\u25CF","\u25CB","\u25B2","\u25B3","\u25A0","\u25A1","\u25C6","\u25C7","\u2605",
  "\u2606","\u271A","\u2716","\u2726","\u2736","\u2660","\u2663","\u2665","\u2666",
  "\u25B6","\u25C0","\u25E6","\u2600","\u2601","\u2708","\u2699")
names(glyphs25) <- sort(unique(seoul_gu_df$SGG1_FNM))

my_graph <- ggplot(seoul_gu_df) +
  geom_sf_pattern(
    aes(pattern_shape = SGG1_FNM),
    pattern = "text",
    pattern_density = 0.4,
    pattern_fill = "white",
    pattern_color = "gray30",
    color = "black",
    fill = "white",
    lwd = 0.5,
    show.legend = FALSE
  ) +
  coord_sf() + 
  theme_bw() +
  scale_pattern_shape_manual(values = glyphs25) +
  theme(
    panel.grid = element_blank(), 
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )
my_graph



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

# 높이
  
library(devtools)
install_github("neocarto/mapextrud")
library(mapextrud)


plot(st_geometry(seoul_gu))

seoul_gu_ht <- deform(seoul_gu, flatten = 0.5)
plot(st_geometry(seoul_gu_ht))

extrude2 <- function(x, var, k = 1, lwd = 1, col = "white",
                     border = "black", regular = FALSE, add = FALSE) {
  
  xraw <- x
  
  v <- x[[var]]
  v[is.na(v)] <- 0
  x[[var]] <- v
  x <- x[v > 0, ]
  
  v <- x[[var]]
  v[is.na(v)] <- 0
  x[[var]] <- v
  
  h <- st_bbox(x)[4] - st_bbox(x)[2]
  m <- max(x[[var]], na.rm = TRUE)
  k <- k * 0.1 * h / m
  
  x$id <- row.names(x)
  x$height <- x[[var]] * k
  x$height[is.na(x$height)] <- 0
  
  n1 <- dim(x)[1]
  
  single <- x[st_geometry_type(x) == "POLYGON", ]
  multi  <- x[st_geometry_type(x) == "MULTIPOLYGON", ]
  exploded <- st_cast(multi, "POLYGON", warn = FALSE)
  
  x <- rbind(single, exploded)
  
  n2 <- dim(x)[1]
  if (n2 > n1) {
    message("Splitting multi-part polygon into single polygons. The same value is assigned to each splitted polygon.")
    x$id <- row.names(x)
  }
  
  nodes <- st_cast(x, "MULTIPOINT", warn = FALSE)
  nodes <- st_cast(nodes, "POINT", warn = FALSE)
  
  if (dim(nodes)[1] > 2000 && regular == FALSE) {
    stop("Computation aborted", call. = FALSE)
  }
  
  L1 <- data.frame(st_coordinates(x))
  nodes <- st_sf(cbind(data.frame(nodes), L1 = L1$L1))
  nodes$id2 <- paste(nodes$id, nodes$L1, sep = "_")
  
  nodes$first <- !duplicated(nodes$id2)
  nodes$last  <- !duplicated(nodes$id2, fromLast = TRUE)
  dots1 <- nodes[!nodes$last, ]
  dots2 <- nodes[!nodes$first, ]
  
  p1x <- st_coordinates(dots1)[,1]
  p1y <- st_coordinates(dots1)[,2]
  p2x <- st_coordinates(dots2)[,1]
  p2y <- st_coordinates(dots2)[,2]
  p3x <- p2x
  p3y <- p2y + dots2$height
  p4x <- p1x
  p4y <- p1y + dots1$height
  
  faces <- dots1
  faces$ang <- atan((p2y - p1y) / (p2x - p1x)) * 180 / pi
  faces$pos <- (p1y + p2y) / 2
  
  st_geometry(faces) <- st_as_sfc(
    paste0("POLYGON((", p1x, " ", p1y, ", ",
           p2x, " ", p2y, ", ",
           p3x, " ", p3y, ", ",
           p4x, " ", p4y, ", ",
           p1x, " ", p1y, "))")
  )
  
  if (col %in% names(x)) {
    fill <- c("white", "#d1c9b2", "#b8b1a0")
    faces$fill <- fill[2]
    faces[faces$ang > 0, "fill"] <- fill[3]
  } else {
    if (col == "white") {
      fill <- c("white", "white", "white")
    } else {
      pal <- colorRampPalette(c("white", col, "black"))(11)
      fill <- c(col, pal[3], pal[7])
    }
    faces$fill <- fill[2]
    faces[faces$ang > 0, "fill"] <- fill[3]
  }
  
  tops <- x
  for (i in seq_len(nrow(tops))) {
    st_geometry(tops[i, ]) <- st_geometry(tops[i, ]) + c(0, x[[var]][i] * k)
  }
  tops <- tops[order(tops$height, decreasing = FALSE), ]
  st_crs(tops) <- NA
  
  message("Extrusion et ordering")
  faces <- faces[, c("id", "pos", "fill")]
  
  for (i in tops$id) {
    tops[tops$id == i, "pos"] <- max((faces[faces$id == i, "pos"] %>% st_drop_geometry())[,1]) - 1000
  }
  
  if (col %in% names(x)) {
    tops$fill <- x[[col]]
  } else {
    tops$fill <- fill[1]
  }
  
  tops <- tops[, c("id", "pos", "fill")]
  geom <- rbind(faces, tops)
  geom <- geom[order(geom$pos, decreasing = TRUE), ]
  
  plot(st_geometry(xraw), lwd = lwd, border = border, add = add)
  plot(st_geometry(geom), col = geom$fill, lwd = lwd, border = border, add = TRUE)
}

seoul_gu_ht2 <- seoul_gu_ht |>
  st_simplify(dTolerance = 50)

extrude2(seoul_gu_ht2, var = "House1_p", regular = TRUE, col="#99aed1", k =1.8)
  
    
  
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
library(tmap)
library(sf)

world_sf <- gisco_get_countries(resolution = 10)
qtm(world_sf)

crsrobin <- "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

world_robinson2 <- world_sf |> 
  st_break_antimeridian(lon_0 = 150) |>  # insert this before transformation
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

# 경위선망에서 일부를 골라 폴리곤 만들기: 꼭지점만 존재하기 때문에 투영을 하면 선이 오로지 직선으로만 표시되는 오류!!!

poly_coord <- tibble(
  id = c(rep("NI51", 5), rep("NI52", 5), rep("NJ51", 5), rep("NJ52", 5)), 
  x = c(120, 120, 126, 126, 120, 126, 126, 132, 132, 126, 120, 120, 126, 126, 120, 126, 126, 132, 132, 126),
  y = c(32, 36, 36, 32, 32, 32, 36, 36, 32, 32, 36, 40, 40, 36, 36, 36, 40, 40, 36, 36)
)
imw_kr <- poly_coord |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  group_by(id) |> 
  summarize(geometry = st_combine(geometry)) |> 
  st_cast("POLYGON")
qtm(imw_kr)

imw_kr

### IMW 인덱스 셰이프 파일 만들기

bb_world_1 <- tibble(x = c(-180, 180), y = c(-60, 60)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
bb_world_2 <- tibble(x = c(-180, 180), y = c(60, 76)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
bb_world_3 <- tibble(x = c(-180, 180), y = c(-60, -76)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
bb_world_4 <- tibble(x = c(-180, 180), y = c(76, 88)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
bb_world_5 <- tibble(x = c(-180, 180), y = c(-76, -88)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
bb_world_6 <- tibble(x = c(-180, 180), y = c(88, 90)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
bb_world_7 <- tibble(x = c(-180, 180), y = c(-88, -90)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()

imw_grid_1 <- st_make_grid(bb_world_1, n = c(60, 30)) |> 
  st_as_sf()
imw_grid_2 <- st_make_grid(bb_world_2, n = c(30, 4)) |> 
  st_as_sf()
imw_grid_3 <- st_make_grid(bb_world_3, n = c(30, 4)) |> 
  st_as_sf()
imw_grid_4 <- st_make_grid(bb_world_4, n = c(15, 3)) |> 
  st_as_sf()
imw_grid_5 <- st_make_grid(bb_world_5, n = c(15, 3)) |> 
  st_as_sf()
imw_grid_6 <- st_make_grid(bb_world_6, n = c(1, 1)) |> 
  st_as_sf()
imw_grid_7 <- st_make_grid(bb_world_7, n = c(1, 1)) |> 
  st_as_sf()

qtm(imw_grid_7)

imw_grid <- bind_rows(imw_grid_1, 
                      imw_grid_2,
                      imw_grid_3,
                      imw_grid_4,
                      imw_grid_5,
                      imw_grid_6,
                      imw_grid_7)

gr_1 <- c(str_c("S", LETTERS[15:1]), str_c("N", LETTERS[1:15]))
gr_2 <- c(str_c("N", LETTERS[16:19]), str_c("S", LETTERS[19:16]))
gr_3 <- c(str_c("N", LETTERS[20:22]), str_c("S", LETTERS[22:20]))
gr_4 <- c(str_c("N", LETTERS[26]), str_c("S", LETTERS[26]))

imw_id_1 <- vector()
for (i in 1:length(gr_1)){
  imw_id_i <- str_c(gr_1[i], 1:60)
  imw_id_1 <- c(imw_id_1, imw_id_i)
}
imw_id_2 <- vector()
for (i in 1:length(gr_2)){
  imw_id_i <- str_c(gr_2[i], 1:30)
  imw_id_2 <- c(imw_id_2, imw_id_i)
}
imw_id_3 <- vector()
for (i in 1:length(gr_3)){
  imw_id_i <- str_c(gr_3[i], 1:15)
  imw_id_3 <- c(imw_id_3, imw_id_i)
}  
imw_id_4 <- gr_4

imw_id <- c(imw_id_1, imw_id_2, imw_id_3, imw_id_4)
length(imw_id)  

imw_grid <- imw_grid |> 
  mutate(imw_id) |> 
  relocate(imw_id)
  
st_write(imw_grid, "imw_grid.shp", driver = "ESRI Shapefile", layer_options = "ENCODING=CP949", delete_layer = TRUE)


utm_bound <- c(80, 0, 160, 60)
tm_shape(countries, bbox = utm_bound) + tm_fill() +
  tm_graticules() 

utm_bound <- c(81, -4, 171, 60)

bb_utm <- tibble(x = c(81, 81, 171, 171), y = c(-4, -4, 60, 60)) |> 
  st_as_sf(coords = c("x", "y"), crs = 4326) |> 
  st_bbox() |> 
  st_as_sfc()
qtm(bb_utm)

bb <- c(620000, 1440000, 1300000, 1900000)
tm_shape(topo_50, bbox = bb) + tm_fill() + tm_borders() +
  tm_shape(sido) + tm_borders(col = "gray10", lwd = 0.5) +
  tm_shape(gyeongju) + tm_fill(col = "gray50") +
  tm_shape(daebo) + tm_fill(col = "gray50") +
  tm_shape(topo_250_south) + tm_borders(lwd = 1.5, col = "gray15") + 
  tm_text("MAPID_NO", size = 1.25) +
  tm_graticules()



pop_SDGG <- read_excel(path = "D:/My R/Population Geography/1 Population Growth/POP_1975_2021_SDGG.xlsx")
pop_data <- pop_SDGG |> 
  select(
    Region
  )


my_df |> 
  tm_shape() + 
  tm_polygons(
    col = "SGG1_NM",
    border.col = "gray20", lwd = 0.75, 
    palette = pals::alphabet2(),
    legend.show = FALSE
  )

library(palmerpenguins)
plt <- penguins %>%
  ggplot(aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point()+
  geom_text(x = 45, y = 20, label = "Example of font problem", size = 15/.pt, inherit.aes = FALSE) +
  labs(title = "Bill length and depth relation by species") +
  theme(plot.title = element_text(size = 15))


ragg::agg_png("ragg_10x10.png", width = 10, height = 10, units = "in", res = 300)
plt
dev.off()


# indicatrix --------------------------------------------------------------

# https://mgimond.github.io/tissot/

source("https://raw.githubusercontent.com/mgimond/tissot/master/Tissot_functions.r")

# world <-  readRDS(gzcon(url("https://github.com/mgimond/tissot/raw/master/smpl_world.rds")))
# us <-  readRDS(gzcon(url("https://github.com/mgimond/tissot/raw/master/smpl_US.rds")))

library(spData)

# us.crs <- st_crs(us)
# st_geometry(us) <- st_geometry(us) + c(-360, 0) 
# st_crs(us) <- us.crs

qtm(world)
# qtm(us)

proj.rob    <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84" # Robinson
proj.aea    <- "+proj=aea +lat_1=30 +lat_2=45 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Equal area conic
proj.eqdc   <- "+proj=eqdc +lat_0=37.5 +lon_0=-96 +lat_1=30 +lat_2=45" # Equidistant conic
proj.merc   <- "+proj=merc +ellps=WGS84" # Mercator
proj.ortho1 <- "+proj=ortho +lon_0=-69 +lat_0=45 +ellps=WGS84" # Planar projection
proj.utm19N <- "+proj=utm +zone=19 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # UTM NAD83
proj.cea    <- "+proj=cea +lon_0=0 +lat_ts=0" # Equal Area Cylindrical projection.
proj.carree <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                +datum=WGS84 +units=m +no_defs" # Plate Carree
proj.aeqd   <- "+proj=aeqd +lat_0=45 +lon_0=-69 +x_0=0 +y_0=0 +ellps=WGS84 
                +datum=WGS84 +units=m +no_defs" # Azimuthal Equidistant
proj.gnom   <- "+proj=gnom +lon_0=-100 +lat_0=30" # Gnomonic
proj.lcc    <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +datum=NAD83" # USA Lambert Conformal
proj.wintri <- "+proj=wintri +lat_1=50.467 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

lat <- seq(-80,80, by=20L)
lon <- seq(-150,150, by=30L)
coord <- as.matrix(expand.grid(lon,lat))

# coord2 <- coord_check(coord, proj.out = proj.merc)

# i.lst <- apply(coord2, 1, function(x) ti(coord = x, proj.out = proj.merc))

i.lst <- coord |> as_tibble() |> 
  pmap(\(...) ti(coord = c(...), proj.out = proj.merc))

tsf <- tissot_sf(i.lst, proj.out = proj.merc)

world_merc <- st_transform(world, proj.merc) # Project world map

ggplot() + 
  geom_sf(data = world.merc, fill = "grey90", col = "white") + 
  geom_sf(data = tsf$base, fill = "bisque", col = "grey50") +
  geom_sf(data = tsf$ind,  col="red", fill = NA) +
  geom_sf(data = tsf$mina,  col="red", fill = NA) +
  geom_sf(data = tsf$maja,  col="green", fill = NA) +
  geom_sf(data = tsf$lam,  col="grey50", fill = NA) +
  geom_sf(data = tsf$phi,  col="grey80", fill = NA) +
  coord_sf(ylim=c(-18800000,18800000), crs = proj.merc) +
  theme_bw()

ti.maine <- local_TI(long = -69.5, lat = 44.5, proj.out = proj.merc)


zero_lines <- st_sfc(
  st_linestring(matrix(c(-180, 0, 180, 0), ncol = 2, byrow = TRUE)), 
  st_linestring(matrix(c(0, -85, 0, 85), ncol = 2, byrow = TRUE)),   
  crs = 4326
)

my_map <- tm_shape(world_merc, bbox = c(-180, -85, 180, 85)) + tm_fill(fill = "#fffff0") +
  tm_graticules(x = seq(-180, 180, 10), y = c(-85, seq(-80, 80, 10), 85), 
                labels.show = FALSE, lwd = 0.1, col = "black") +
  tm_shape(zero_lines) + tm_lines(col = "black", lwd = 1) +
  tm_shape(tsf$ind) + tm_fill(fill = "#e65100", fill_alpha = 0.50) +
  tm_layout(inner.margins = c(0, 0, 0, 0), bg.color = "#d1ecf9") +
  tm_credits("SANG-IL LEE, Geography Education at SNU", size = 0.7, position = c(0.80, -0.005))
my_map  

my.ratio <- get_asp_ratio(my_map)

my.title <- "indicatrix 연습"
my.file.name <- paste0("D:/My Cartography/지도제작/", my.title, ".png")
tmap_save(my_map, filename = my.file.name, height = 11.74*1.1, width = my.ratio*11.74*1.1, dpi = 600)

# 심사도법: 참으로 어렵다!!!!!

# 투영 설정
lat_0 <- -25
lon_0 <- 135
proj.gnomonic <- str_glue("+proj=gnom +lat_0={lat_0} +lon_0={lon_0} +R=6.4e6")

# 특수 영역 세계지도 만들기
sf_use_s2(TRUE)

center_point <- st_sfc(st_point(c(lon_0, lat_0)), crs = 4326)
visible_buffer <- st_buffer(center_point, dist = 9900000) 
qtm(visible_buffer) + qtm(center_point)

world_clipped <- st_intersection(World, visible_buffer)
world_gnomonic <- st_transform(world_clipped, proj.gnomonic) 

qtm(world_clipped)
qtm(world_gnomonic)

center_point_0 <- st_sfc(st_point(c(0, 0)), crs = proj.gnomonic) 
world_gnomonic_buffer <- st_buffer(center_point_0, dist = 25000000)

world_gnomonic_clipped <- world_gnomonic |> st_intersection(
  world_gnomonic_buffer
)
  
qtm(world_gnomonic_clipped)

# 좌표 생성
lat <- seq(-50, 40, by = 20)
lon <- seq(-180, 180, by = 30)
coord_df <- expand.grid(lon = lon, lat = lat) %>% as_tibble()

# 중심점으로부터의 각거리(Angular Distance) 계산 함수
# 두 지점 사이의 각거리 c를 구함: cos(c) = sin(phi1)sin(phi2) + cos(phi1)cos(phi2)pos(delta_lambda)
deg2rad <- function(deg) deg * pi / 180

coord_filtered <- coord_df %>%
  mutate(
    phi1 = deg2rad(lat_0),
    phi2 = deg2rad(lat),
    d_lambda = deg2rad(lon - lon_0),
    # 각거리 cos(c) 계산
    cos_c = sin(phi1) * sin(phi2) + cos(phi1) * cos(phi2) * cos(d_lambda)
  ) %>%
  # cos_c가 0보다 커야(즉, 각거리가 90도 미만) 투영이 가능함
  # 수치적 안정성을 위해 약간의 여유(0.05)를 둡니다.
  filter(cos_c > 0.05) 

# Tissot 지표 계산
ts.lst <- coord_filtered %>%
  select(lon, lat) %>%
  pmap(~ ti(coord = c(...), proj.out = proj.gnomonic))

tsf <- tissot_sf(ts.lst, proj.out = proj.gnomonic)

buf_ind <- tsf$ind |> st_intersection(world_gnomonic_buffer)

my_map <- tm_shape(world_gnomonic_clipped, bbox = world_gnomonic_buffer) + tm_fill(fill = "#fffff0") +
  tm_graticules(x = seq(-180, 180, 10), y = c(-85, seq(-80, 80, 10), 85), 
                labels.show = FALSE, lwd = 0.1, col = "black") +
  # tm_shape(zero_lines) + tm_lines(col = "black", lwd = 1) +
  tm_shape(buf_ind) + tm_fill(fill = "#e65100", fill_alpha = 0.50) +
  tm_layout(inner.margins = c(0, 0, 0, 0), bg.color = "#d1ecf9") +
  tm_credits("SANG-IL LEE, Geography Education at SNU", size = 0.7, position = c(0.80, -0.005))
my_map  


# tissot 패키지 --------------------------------------------------------------

# https://github.com/hypertidy/tissot

library(tissot)
library(sf)

proj.rob    <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84" 
proj.rob.150E    <- "+proj=robin +lon_0=150 +x_0=0 +y_0=0 +ellps=WGS84" 
proj.wintri <- "+proj=wintri +lat_1=50.467 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
proj.wintri.150E <- "+proj=wintri +lat_1=50.467 +lon_0=150 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

lat <- seq(-80,80, by=20L)
lon <- seq(-150,150, by=30L)
coord <- as.matrix(expand.grid(lon,lat))

tis_merc <- tissot(coord, proj.merc)
tis_robin <- tissot(coord, proj.rob)
tis_robin_150E <- tissot(coord, proj.rob.150E)

lat <- seq(-90,87.5, by=2.5)
lon <- seq(-180,177.5, by=2.5)
coord <- as.matrix(expand.grid(lon,lat))

tis_robin <- tissot(coord, proj.rob)
tis_robin_150E <- tissot(coord, proj.rob.150E)
tis_wintri <- tissot(coord, proj.wintri)
tis_wintri_150E <- tissot(coord, proj.wintri.150E)


proj.gnomonic <- "+proj=gnom +lat_0=-25 +lon_0=135 +R=6.4e6"

tis_gnomonic <- tissot(coord, proj.gnomonic)
indicatrix_gnomonic <- indicatrix(tis_gnomonic)

plot(indicatrix_gnomonic, scale = 6e5, show.circle = TRUE)
tissot_map()


