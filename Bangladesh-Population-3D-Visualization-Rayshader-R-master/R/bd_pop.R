
options(rgl.useNULL = FALSE)


# Packages ----------------------------------------------------------------

require(tidyverse)
require(sf)
require(tmap)
require(ggplot2)
require(mapview)
require(stars)
require(rayshader)
require(MetBrewer)
require(colorspace)
require(rayrender)
require(magick)
require(extrafont)


# Data --------------------------------------------------------------------


# load population 400m H3 hexagon

bd_hex <-
  st_read("Data/kontur_population_BD_20220630.gpkg") %>% 
  st_transform(3106)

# load population by administrative boundary
bd_admin <-
  st_read("Data/kontur_boundaries_BD_20220407.gpkg") %>% 
  st_transform(3106)



# Creating BD Boundary ----------------------------------------------------


bd_boundary <-
  bd_admin %>% 
  st_geometry %>%
  st_union %>%
  st_sf %>% 
  st_make_valid()



# Clean the data ----------------------------------------------------------


# check the plot
ggplot(bd_hex) +
  geom_sf(aes(fill = population),
          color = "gray66",
          linewidth = 0) +
  geom_sf(
    data = bd_boundary,
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 1
  )

# setting the bd boundary as a bounding box
bbox <- st_bbox(bd_boundary)

# finding the aspect ratio
bottom_left <- st_point(c(bbox[["xmin"]], bbox[["ymin"]])) %>% 
  st_sfc(crs = 3106)
bottom_right <- st_point(c(bbox[["xmax"]], bbox[["ymin"]])) %>%
  st_sfc(crs = 3106)
top_left <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>% 
  st_sfc(crs = 3106)
# top_right <- st_point(c(bbox[["xmin"]], bbox[["ymax"]])) %>% 
#   st_sfc(crs = 3106)



width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

if(width > height) {
  w_ratio = 1
  h_ratio = height / width
  
} else {
  h_ratio = 1.1
  w_ratio = width / height
  
}

# convert to raster to convert to matrix

size = 1000 * 2.5

pop_raster <- st_rasterize(
  bd_hex,
  nx = floor(size * w_ratio) %>% as.numeric(),
  ny = floor(size * h_ratio) %>% as.numeric()
)

pop_matrix <- matrix(pop_raster$population,
                     nrow = floor(size * w_ratio),
                     ncol = floor(size * h_ratio))


# Create color palette 
color <- MetBrewer::met.brewer(name="Paquin", direction = -1)

tx <- grDevices::colorRampPalette(color, bias = 4.5)(256)
swatchplot(tx)
swatchplot(color)

# plotting 3D

rgl::rgl.close()

pop_matrix %>%
  height_shade(texture = tx) %>%
  plot_3d(heightmap = pop_matrix,
          zscale = 250 / 2.5,
          solid = F,
          shadowdepth = 0)

# render_camera(theta = 0,
#               phi = 50,
#               zoom = 0.5,
#               fov = 100
# )

outfile <- glue::glue("Plot/final_plot_bd.png")

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if(!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive = F,
    lightdirection = 225,
    lightaltitude = c(20, 80),
    lightcolor = c(color[2], "white"),
    lightintensity = c(600, 100),
    width = 1980,
    height = 1180,
    samples = 300
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


# Edit & Annotate Image ---------------------------------------------------


pop_raster <- image_read("Plot/final_plot_bd.png")

text_color <- darken(color[9], .5)
#text_color <- darken(color[2], .4)
swatchplot(text_color)

pop_raster %>%
  # image_crop(gravity = "center", geometry = "") %>%
  image_annotate("Bangladesh",
                 gravity = "northeast",
                 location = "+50+50",
                 color = text_color,
                 size = 150,
                 font = "Ananda Namaste",
                 weight = 700,
                 # degrees = 0,
  ) %>%
  image_annotate("POPULATION DENSITY MAP",
                 gravity = "northeast",
                 location = "+50+200",
                 color = text_color,
                 size = 36.5,
                 font = "FuturaBT-Medium",
                 weight = 500,
                 # degrees = 0,
  ) %>%
  image_annotate("Visualization by: foRhAd with Rayshader(@tylermorganwall) | Data: Kontur Population (Released 2022-06-30)",
                 gravity = "southwest",
                 location = "+20+20",
                 color = alpha(text_color, .6),
                 font = "FuturaBT-Medium",
                 size = 20,
                 # degrees = 0,
  ) %>%
  image_write("Plot/final_plot_edited.png", format = "png", quality = 100)


# The End -----------------------------------------------------------------


