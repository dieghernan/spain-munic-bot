# Done in local - GH actions not building rayshader
# 1. Load libraries ----
rm(list = ls())


library(mapSpain)
library(sf)
library(raster)
library(dplyr)
# library(tmap)
library(slippymath)
# library(grid)
# library(rgdal)
# library(osmdata)
library(rtweet)
# library(jsonlite)
library(stringr)
# library(lubridate)
library(rayshader)
library(magick)
library(elevatr)
library(rgdal)




# Load helper funs
source("R/xxx_funs.R")
source("R/environment_vars.R")

init <- Sys.time()
time <- as.character(format(Sys.time(), tz = "CET", usetz = TRUE))


message("Connect with twitter")

api_key <- Sys.getenv("TWITTER_API_KEY")
api_secret_key <- Sys.getenv("TWITTER_API_SECRET")
access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
access_token_secret <- Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")


## authenticate via web browser
token <- create_token(
  app = "spainmunic",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret
)


# 2. Load data ----

mapdata <- esp_get_munic(
  year = 2019,
  epsg = 4326,
  cache_dir = "data",
  moveCAN = FALSE
) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))


# LAU_CODE is INE CODE
mapdata <- mapdata %>% filter(!is.na(LAU_CODE))

# Clean names of municipalities - First option as per INE

mapdata <- mapdata %>% mutate(
  name1 = word(name, sep = " /"),
  namepref = str_extract(name1, pattern = "\\b[^,]+$"),
  namenopref = word(name1, sep = ","),
  newname = ifelse(
    namepref == namenopref,
    namenopref,
    paste0(str_to_title(namepref), " ",
           namenopref)
  ),
  # Replace for newname
  name = stringr::str_squish(newname)
)

# Fix names
codauto <-
  esp_codelist %>% dplyr::select(codauto, ccaa.shortname.es) %>% unique()

cpro <- esp_codelist %>% select(cpro, prov.shortname.es) %>%
  unique()

mapdata <- mapdata %>% left_join(codauto) %>% left_join(cpro)

mapdata <- mapdata %>%
  select(codauto,
         ccaa.shortname.es,
         cpro,
         prov.shortname.es,
         LAU_CODE,
         LAU_CODE_NUM,
         name)

data <- st_drop_geometry(mapdata)



# Load log from url

datalog <-
  read.csv2(
    "https://raw.githubusercontent.com/dieghernan/spain-munic-bot/main/assets/datalog.csv",
    sep = ",",
    stringsAsFactors = FALSE
  )

# Order

datalog <- datalog %>% arrange(datetime) %>% filter(!is.na(datetime))





# 3. Select last gif ----

# Check last gif tweeted

if (file.exists("assets/lastgif.csv")) {
  lastgif <-
    read.csv2("assets/lastgif.csv",
              sep = ",",
              stringsAsFactors = FALSE)
  n_row_gif <- lastgif$n + 1
  
} else {
  # If it doesn't exist, start
  n_row_gif <- 1
}


LAUtogif <- datalog[n_row_gif,]$LAU_CODE_NUM
data_filter <- data %>% filter(LAU_CODE_NUM == LAUtogif)

# Get and register munic
munic <- mapdata[mapdata$LAU_CODE == data_filter$LAU_CODE, ]

# Add pop
pop <- mapSpain::pobmun19 %>%
  mutate(LAU_CODE = paste0(cpro, cmun)) %>%
  mutate(pob19 = paste0(prettyNum(
    pob19, big.mark = ".", decimal.mark = ","
  ))) %>%
  select(LAU_CODE, pob19)

munic <- munic %>% left_join(pop)


df <- munic %>% st_drop_geometry() %>%
  mutate(datetime = time)

message("Munic selected: ",
        df$name,
        " ",
        df$LAU_CODE)

# Get square around munic
square <- square_bbox(munic)

cent <- st_coordinates(square)

# 4. Get DEM -----

sq <- st_sf(d = 1, square) %>% as_Spatial()

DEM <- get_elev_raster(sq, z = 12, clip = "bbox", override_size_check = TRUE) %>%
  crop(extent(sq))

minR <- min(dim(DEM)[1:2])
maxR <- max(dim(DEM)[1:2])
dim(DEM)
# Preserve space
if (maxR > 1000) {
  DEM <- aggregate(DEM, fact = max(2, round(maxR / 1000)))
}
dim(DEM)

# Assign min to NAs - It's sea on this DEM provider
DEM[is.na(DEM)] <- min(values(DEM), na.rm = TRUE)

# 5. Get Tiles ----
# Analyze zoom
gz <- slippymath::bbox_tile_query(st_bbox(square))

# max 24 tiles, min 4 with an upgrade
zoom <- max(gz[gz$total_tiles %in% 4:24, "zoom"])

# Function to get tiles an avoid errors

hlp_gettile <-
  function(munic,
           provider,
           zoom,
           mask = TRUE,
           crop = TRUE) {
    get <- tryCatch(
      esp_getTiles(
        munic,
        provider,
        mask = mask,
        crop = crop,
        zoom = zoom,
        verbose = FALSE
      ),
      error = function(e) {
        return(TRUE)
      }
    )
    
    if (isTRUE(get)) {
      message("Error with zoom ", zoom, ". Try with ", zoom - 1)
      if (zoom < 1) {
        stop("Aborted")
        
      }
      get <- hlp_gettile(munic, provider, zoom - 1, mask)
    } else {
      message("Success with zoom ", zoom)
      return(get)
    }
  }
overlay_raster <- hlp_gettile(square, "PNOA", zoom, mask = FALSE)


mask <- mask(overlay_raster, munic)

tmppng <- tempfile(fileext = ".png")


# Convert to png
png(tmppng,
    height = dim(overlay_raster)[1],
    width = dim(overlay_raster)[2])
par(mar = c(0, 0, 0, 0))
plotRGB(overlay_raster, alpha = 240)
plotRGB(mask, add = TRUE, bgalpha = 0)
dev.off()

img_overlay <- png::readPNG(tmppng)

# 6. Rayshade!! ----

sub <- unique(c(munic$prov.shortname.es, munic$ccaa.shortname.es))
sub <- paste(sub, collapse = ", ")
title <- paste0(munic$name, "\n", sub)

pop <- paste0("Population (2019): ", munic$pob19)
foot <-  paste0(pop,
                "\n",
                "Infraestructura de Datos Espaciales de Espa\u00f1a (IDEE)")

# Correction to zscale
fact <-
  (max(values(DEM), na.rm = TRUE) - min(values(DEM), na.rm = TRUE)) / 100

# Gif config
gif_file <- file.path("assets", "gif", "lastgif")

n_frames <- 360

theta_val <-
  transition_values(
    from = 0,
    to = (360),
    steps = n_frames,
    one_way = TRUE,
    type = "lin"
  )
theta_val <- ifelse(theta_val > 360, theta_val - 360, theta_val)


phi_val1 <-
  transition_values(
    from = 90,
    to = 10,
    steps = (n_frames / 3),
    one_way = TRUE,
    type = "cos"
  )

phi_val <- c(phi_val1, rep(10, n_frames/3), rev(phi_val1))

zoom_val <- transition_values(
  from = 1,
  to = .5,
  steps = n_frames,
  one_way = FALSE,
  type = "cos"
)

# Rayshade 3D with png overlay

DEM_mat <- raster_to_matrix(DEM)
fact
DEM_mat %>%
  sphere_shade(texture = "desert") %>%
  add_overlay(img_overlay) %>%
  plot_3d(DEM_mat, zscale = 5 + fact/2)

# Render gif
render_movie2(
  gif_file,
  title_text = title,
  title_position = "north",
  title_size = 16,
  type = "custom",
  frames = n_frames,
  fps = 30,
  phi = phi_val,
  zoom = zoom_val,
  theta = theta_val,
  loop = TRUE,
  subtitle_text = foot,
  subtitle_position = "southeast",
  subtitle_size = 13,
  progbar = TRUE
)

rgl::rgl.close()


# 8. Tweet -----


# To convert lon lat from decimal to pretty
prettylab <- function(x, type) {
  coordinit <- x
  x <- abs(x)
  D <- as.integer(x)
  m <- (x - D) * 60
  M <- as.integer(m)
  S <- round((m - M) * 60, 2)
  
  if (type == "lon") {
    if (coordinit > 0) {
      lab <- "E"
    } else {
      lab <- "W"
    }
  } else {
    if (coordinit > 0) {
      lab <- "N"
    } else {
      lab <- "S"
    }
  }
  
  label <- paste0(D, "\u00b0 ", M, "' ", S, '\" ', lab)
  return(label)
}
bbx <- st_bbox(munic)
xtick <- bbx[1] + (bbx[3] - bbx[1]) / 2
ytick <- bbx[2] + (bbx[4] - bbx[2]) / 2

xlab <- prettylab(xtick, "lon")
ylab <- prettylab(ytick, "lat")

msg <-
  paste0(munic$name, " (", sprintf("%05d", munic$LAU_CODE_NUM), ")")

sub <- unique(c(munic$prov.shortname.es, munic$ccaa.shortname.es))
sub <- paste(sub, collapse = ", ")

msg <-
  paste(msg, sub, sep = ", ")
msg <- paste0(msg, " - ", ylab, " / ", xlab, " ")
hash <-
  paste0(" #spainmunic", sprintf("%05d", munic$LAU_CODE_NUM), " ")

msg <- paste0(msg, hash)


# Hash2
hash2 <- paste0(" #", gsub(" ", "", munic$prov.shortname.es), " ")
msg <- paste0(msg, hash2)

cat(msg)

addgif <- ifelse((n_row_gif %% 100) == 1,
                 " Done in #rstats using #rayshader, #elevatr, #rspatial, #mapSpain and #rtweet. ",
                 " "
)
# Add credits
addgif2 <- ifelse(nrow(datalog) %% 100 == 2,
                  "Sources @IDEESpain #rstatsES",
                  "")

msggif <- paste0(msg, addgif, addgif2)
msggif <- gsub("  ", " ", msggif)
# Tweet gif
gifpath <- file.path("assets", "gif", "lastgif.mp4")




post_tweet(msggif, media = gifpath)
message("Tweet satellite posted")


# # 7. Save datalog ----
df_log <- data.frame(n = n_row_gif)

write.table(df_log,
            "./assets/lastgif.csv",
            sep = ",",
            row.names = FALSE)

r2 <- Sys.time() - init
message("Time elapsed: ", format(round(r2, 2)))


rm(list = ls())
