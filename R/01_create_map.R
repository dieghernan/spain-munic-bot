# 1. Load libraries ----



library(mapSpain)
library(sf)
library(dplyr)
library(tmap)
library(slippymath)
library(grid)
library(rgdal)
library(osmdata)


time <- as.character(format(Sys.time(), tz = "CET", usetz = TRUE))


# 2. Load data ----




mapdata <- esp_get_munic(year = 2019,
                         cache_dir = "data",
                         moveCAN = FALSE) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))


# LAU_CODE is INE CODE
mapdata <- mapdata %>% filter(!is.na(LAU_CODE))



# Fix names
codauto <-
  esp_codelist %>% select(codauto, ccaa.shortname.es) %>% unique()

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



# Load log

if (file.exists("data/datalog.csv")) {
  datalog <-
    read.csv2("data/datalog.csv",
              sep = ",",
              stringsAsFactors = FALSE)
  
} else {
  # If it doesn't exist, create one empty
  datalog <- data[1, ]
  datalog[1,] <- "xxx"
  datalog$datetime <- time
}


# 3. Select randomly after clean up ----

data_filter <-  data[!data$LAU_CODE_NUM %in% datalog$LAU_CODE_NUM, ]

# Stop if we are done

if (nrow(data_filter) < 1) {
  stop("End with maps - All cities have been mapped")
}


sel <- round(runif(1, 1, nrow(data_filter)), 0)

data_filter <- data_filter[sel,]

# Get and register munic
munic <- mapdata[mapdata$LAU_CODE == data_filter$LAU_CODE,]



df <- munic %>% st_drop_geometry() %>%
  mutate(datetime = time)

message("Munic selected: ", 
        df$name,
        " ",
        df$LAU_CODE)
        
        

# 3. Spatial operations ----


munic <- st_transform(munic, 3857)

# Analyze zoom
bbx <- st_bbox(st_transform(munic, 4326))
gz <- slippymath::bbox_tile_query(bbx)

# max 16 tiles, min 4
zoom <- max(gz[gz$total_tiles %in% 4:16, "zoom"])

# Function to get tiles an avoid errors

hlp_gettile <- function(munic, provider, zoom) {
  get <- tryCatch(
    esp_getTiles(
      munic,
      provider,
      mask = TRUE,
      crop = TRUE,
      zoom = zoom,
      verbose = TRUE
    ),
    error = function(e) {
      return(TRUE)
    }
  )
  
  if (isTRUE(get)) {
    message("Error with zoom ", zoom, ". Retry")
    if (zoom < 1) {
      stop("Aborted")
      
    }
    get <- hlp_gettile(munic, provider, zoom - 1)
  } else {
    return(get)
  }
}
raster <- hlp_gettile(munic, "PNOA", zoom)



# Ready to create map----

title <- paste0(munic$name, "\n(", munic$LAU_CODE, ")")
sub <- unique(c(munic$prov.shortname.es, munic$ccaa.shortname.es))
sub <- paste(sub, collapse = ", ")

# Center
xtick <- bbx[1] + (bbx[3] - bbx[1]) / 2
ytick <- bbx[2] + (bbx[4] - bbx[2]) / 2

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

xlab <- prettylab(xtick, "lon")
ylab <- prettylab(ytick, "lat")


# Overall map
map <- tm_shape(raster) +
  tm_rgba() +
  tm_layout(
    main.title = title,
    asp = 1,
    bg.color = "grey95",
    main.title.position = "center",
    main.title.size = 1,
    main.title.fontface = "bold",
    inner.margins = c(0.1, 0, 0.12, 0),
    outer.margins = c(0, 0.1, 0, 0.1),
    attr.outside = FALSE,
    frame = FALSE,
    design.mode = FALSE
  ) +
  tm_credits(
    "Infraestructura de Datos Espaciales de Espa\u00f1a (IDEE)",
    align = "right",
    fontface = "italic",
    size = 0.8
  ) +
  tm_credits(
    text = paste0("\n", sub, "\n", ylab, " / ", xlab),
    size = 0.85,
    just = "center",
    align = "center",
    position = c("center", "TOP")
  )


# Create Inset
mapESP <- esp_get_country(moveCAN = c(13, 0))
mapProv <- esp_get_prov(prov = munic$cpro, moveCAN = c(13, 0))


municinset <- esp_get_munic(moveCAN = c(13, 0))
municinset <- municinset[municinset$LAU_CODE == munic$LAU_CODE,]
municinset <- st_centroid(municinset, of_largest_polygon = TRUE)


bboxCAN <- esp_get_can_box(moveCAN = c(13, 0), style = "left")

insetmap <- tm_shape(mapESP) +
  tm_polygons(col = "grey75", border.col = "grey75") +
  tm_layout(frame = FALSE,
            bg.color = "transparent") +
  tm_shape(bboxCAN) +
  tm_lines(col = "grey75") +
  tm_shape(mapProv) +
  tm_polygons("grey40") +
  tm_layout(
    design.mode = FALSE,
    asp = 1,
    inner.margins = c(0, 0, 0, 0),
    outer.margins = c(0, 0, 0, 0)
  ) +
  tm_shape(municinset) +
  tm_symbols(col = "firebrick3",
             size = 0.6,
             border.col = "black")

tmap_save(
  tm = map,
  filename = "./data/munic-raster.png",
  insets_tm = insetmap,
  height = 7,
  width = 7,
  insets_vp = viewport(
    x = 0.175,
    y = 0.175,
    w = .35,
    h = .35
  )
)

hist <-
  paste0("./data/archive/", munic$LAU_CODE, "_satellite_mask.png")
tmap_save(
  tm = map,
  filename = hist,
  insets_tm = insetmap,
  height = 7,
  width = 7,
  insets_vp = viewport(
    x = 0.18,
    y = 0.19,
    w = .35,
    h = .35
  )
)




# OSM Lines----
# https://github.com/danielredondo/30diasdegraficos/blob/master/scripts/18_mapa.R

munictransf <- munic %>% st_transform(4326)
municbb <- munictransf %>%  st_bbox()

types <-
  c(
    "motorway",
    "primary",
    "motorway_link",
    "primary_link",
    "secondary",
    "tertiary",
    "secondary_link",
    "tertiary_link",
    "unclassified",
    "residential",
    "living_street",
    "pedestrian",
    "service",
    "track"
  )

osmlines <- opq(municbb)  %>%
  add_osm_feature(key = "highway",
                  value = types) %>%
  osmdata_sf()

obj.lines <- osmlines$osm_lines %>% st_transform(3857)

major <-
  obj.lines %>%
  filter(
    highway %in%  c(
      "motorway",
      "primary",
      "motorway_link",
      "primary_link",
      "secondary",
      "tertiary",
      "secondary_link",
      "tertiary_link"
    )
  )

minor <- obj.lines %>%
  filter(!highway %in% unique(major$highway))

river <- opq(municbb) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

river <- river$osm_lines

# Ready to plot

munictransf2 <- munictransf %>% st_transform(3857)

streetmap <- tm_shape(munictransf2) +
  tm_fill("white") +
  tm_layout(
    main.title = title,
    asp = 1,
    bg.color = "white",
    main.title.position = "center",
    main.title.size = 1,
    main.title.fontface = "bold",
    inner.margins = c(0.1, 0, 0.12, 0),
    outer.margins = c(0, 0.1, 0, 0.1),
    attr.outside = FALSE,
    frame = FALSE,
    design.mode = FALSE
  ) +
  tm_credits(
    "\u00a9 OpenStreetMap Contributors",
    align = "right",
    fontface = "italic",
    size = 0.8,
    bg.color = "white",
    bg.alpha = 0.5
  ) +
  tm_credits(
    text = paste0("\n", sub, "\n", ylab, " / ", xlab),
    size = 0.85,
    just = "center",
    align = "center",
    position = c("center", "TOP"),
    bg.color = "white",
    bg.alpha = 0.5
  )

if (!is.null(river)) {
  river <- river %>%
     st_transform(3857) %>%
     st_intersection(munictransf2)
     
  if (any(!st_is_empty(river))) {
  streetmap <- streetmap +
    tm_shape(river) +
    tm_lines("#7fc0ff", 
    lwd = 1.5, 
    alpha = 0.8)
    }
}

if (!is.null(minor)) {
  minor <- minor %>%
     st_transform(3857) %>%
     st_intersection(munictransf2)
  
  if (any(!st_is_empty(minor))) {
  streetmap <- streetmap +
    tm_shape(minor) +
    tm_lines("grey30", lwd = 1)
    }
}

if (!is.null(major)) {
  major <- major %>%
     st_transform(3857) %>%
     st_intersection(munictransf2)
     
  if (any(!st_is_empty(major))) {
     
  streetmap <- streetmap +
    tm_shape(major) +
    tm_lines(col = "black", 
             lwd = 1.5)
    }
}


tmap_save(
  tm = streetmap,
  filename = "./data/munic-streets.png",
  insets_tm = insetmap,
  height = 7,
  width = 7,
  insets_vp = viewport(
    x = 0.175,
    y = 0.175,
    w = .35,
    h = .35
  )
)

hist2 <-
  paste0("./data/archive/", munic$LAU_CODE, "_streets.png")
tmap_save(
  tm = streetmap,
  filename = hist2,
  insets_tm = insetmap,
  height = 7,
  width = 7,
  insets_vp = viewport(
    x = 0.18,
    y = 0.19,
    w = .35,
    h = .35
  )
)

# Plot the Journey ----

datalog <- rbind(datalog, df) %>% filter(cpro != "xxx") %>% unique()

municall <- esp_get_munic(moveCAN = c(13, 0)) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))


journerylog <- datalog %>% select(LAU_CODE_NUM) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE_NUM),
         order = seq_len(nrow(datalog)))

municall <- municall %>% inner_join(journerylog) %>% arrange(order)
cent <- st_centroid(municall, of_largest_polygon = TRUE)

last <- cent[nrow(cent), ]

line <-
  st_linestring(st_coordinates(cent)) %>% st_sfc(crs = st_crs(cent))



journey <- tm_shape(mapESP) +
  tm_polygons(col = "grey75", border.col = "grey75") +
  tm_layout(
    frame = FALSE,
    asp = 1,
    inner.margins = c(0, 0, 0.1, 0),
    main.title = "spain-munic-bot journey",
    main.title.size = 1.5,
    main.title.position = "center",
    bg.color = "grey95"
  ) +
  tm_shape(bboxCAN) +
  tm_lines(col = "grey75")

if (nrow(cent) > 1) {
  journey <- journey +
    tm_shape(line) +
    tm_lines("firebrick3")
}

# Add last point
journey <- journey +  tm_shape(cent) +
  tm_symbols(
    col = "firebrick1",
    size = 0.2,
    alpha = 0.7,
    border.col = "transparent"
  ) +
  tm_shape(last) +
  tm_text("name",
          size = 0.7,
          shadow = TRUE,
          auto.placement = TRUE)



tmap_save(
  tm = journey,
  filename = "./data/journey.png",
  height = 7,
  width = 7
)

# Clean ----
# Save datalog if everything was correct
write.table(
  datalog,
  "./data/datalog.csv",
  sep = ",",
  row.names = FALSE
)

rm(list = ls())
