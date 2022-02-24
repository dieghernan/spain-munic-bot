# # 1. Load libraries ----
# 
# library(raster)
# library(mapSpain)
# library(sf)
# library(dplyr)
# library(tmap)
# library(slippymath)
# library(grid)
# library(rgdal)
# library(osmdata)
# library(rtweet)
# library(jsonlite)
# library(stringr)
# library(lubridate)
# library(cartography)
# 
# 
# time <- as.character(format(Sys.time(), tz = "CET", usetz = TRUE))
# 
# 
# message("Connect with twitter")
# 
# api_key <- Sys.getenv("TWITTER_API_KEY")
# api_secret_key <- Sys.getenv("TWITTER_API_SECRET")
# access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
# access_token_secret <- Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
# 
# 
# ## authenticate via web browser
# token <- create_token(
#   app = "spainmunic",
#   consumer_key = api_key,
#   consumer_secret = api_secret_key,
#   access_token = access_token,
#   access_secret = access_token_secret
# )
# 
# 
# # 2. Load data ----
# 
# mapdata <- esp_get_munic(year = 2019,
#                          cache_dir = "data",
#                          moveCAN = FALSE) %>%
#   mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))
# 
# 
# # LAU_CODE is INE CODE
# mapdata <- mapdata %>% filter(!is.na(LAU_CODE))
# 
# # Clean names of municipalities - First option as per INE
# 
# mapdata <- mapdata %>% mutate(
#   name1 = word(name, sep = " /"),
#   namepref = str_extract(name1, pattern = "\\b[^,]+$"),
#   namenopref = word(name1, sep = ","),
#   newname = ifelse(
#     namepref == namenopref,
#     namenopref,
#     paste0(str_to_title(namepref), " ",
#            namenopref)
#   ),
#   # Replace for newname
#   name = stringr::str_squish(newname)
# )
# 
# # Fix names
# codauto <-
#   esp_codelist %>% select(codauto, ccaa.shortname.es) %>% unique()
# 
# cpro <- esp_codelist %>% select(cpro, prov.shortname.es) %>%
#   unique()
# 
# mapdata <- mapdata %>% left_join(codauto) %>% left_join(cpro)
# 
# mapdata <- mapdata %>%
#   select(codauto,
#          ccaa.shortname.es,
#          cpro,
#          prov.shortname.es,
#          LAU_CODE,
#          LAU_CODE_NUM,
#          name)
# 
# data <- st_drop_geometry(mapdata)
# 
# 
# 
# # Load log
# 
# if (file.exists("assets/datalog.csv")) {
#   datalog <-
#     read.csv2("assets/datalog.csv",
#               sep = ",",
#               stringsAsFactors = FALSE)
#   
# } else {
#   # If it doesn't exist, create one empty
#   datalog <- data[1, ]
#   datalog[1,] <- "xxx"
#   datalog$datetime <- time
# }
# 
# 
# 
# # 3. Select randomly after clean up ----
# 
# data_filter <-  data[!data$LAU_CODE_NUM %in% datalog$LAU_CODE_NUM, ]
# 
# # Override
# 
# if (file.exists("assets/override.csv")) {
#   message("Override!")
#   override <-
#     read.csv2("assets/override.csv",
#               sep = ",",
#               stringsAsFactors = FALSE)
#   override <- override[1,]
#   file.remove("assets/override.csv")
#   data_filter <-
#     data[data$LAU_CODE_NUM %in% override$LAU_CODE_NUM, ]
#   
#   # Dedupe
#   datalog <- datalog[datalog$LAU_CODE_NUM != data$LAU_CODE_NUM,]
# }
# 
# # Stop if we are done
# 
# if (nrow(data_filter) < 1) {
#   message("End with maps - All cities have been mapped")
#   quit()
# }
# 
# 
# sel <- round(runif(1, 1, nrow(data_filter)), 0)
# 
# data_filter <- data_filter[sel,]
# 
# 
# # Get and register munic
# munic <- mapdata[mapdata$LAU_CODE == data_filter$LAU_CODE,]
# 
# # Add pop
# pop <- mapSpain::pobmun19 %>%
#   mutate(LAU_CODE = paste0(cpro, cmun)) %>%
#   mutate(pob19 = paste0(prettyNum(
#     pob19, big.mark = ".", decimal.mark = ","
#   ))) %>%
#   select(LAU_CODE, pob19)
# 
# munic <- munic %>% left_join(pop)
# 
# 
# df <- munic %>% st_drop_geometry() %>%
#   mutate(datetime = time)
# 
# message("Munic selected: ",
#         df$name,
#         " ",
#         df$LAU_CODE)
# 
# 
# 
# # 4. Spatial operations ----
# 
# 
# munic <- st_transform(munic, 3857)
# 
# square_bbox <- function(x, expand = .1) {
#   bbx <- st_bbox(st_transform(x, 3857))
#   xtick <- bbx[1] + (bbx[3] - bbx[1]) / 2
#   ytick <- bbx[2] + (bbx[4] - bbx[2]) / 2
#   
#   x_dim <- (bbx[3] - bbx[1])
#   y_dim <- (bbx[4] - bbx[2])
#   
#   
#   max_dim <- (max(x_dim, y_dim) / 2) * (1 + expand)
#   
#   square <-
#     c(xtick - max_dim, ytick - max_dim, xtick + max_dim, ytick + max_dim)
#   names(square) <- names(bbx)
#   class(square) <- "bbox"
#   
#   
#   bbx_end <- st_as_sfc(square)
#   bbx_end <- st_set_crs(bbx_end, 3857)
#   bbx_end <- st_transform(bbx_end, st_crs(x))
#   
#   return(bbx_end)
# }
# 
# square <- square_bbox(munic, exp=.2)
# 
# bbx <- st_bbox(st_transform(munic,4326))
# 
# # Analyze zoom
# gz <- slippymath::bbox_tile_query(bbx)
# 
# # max 14 tiles, min 4
# zoom <- max(gz[gz$total_tiles %in% 4:14, "zoom"])
# 
# # Force upgrade to get better resolution
# zoom <- zoom + 1
# # Function to get tiles an avoid errors
# 
# hlp_gettile <-
#   function(munic,
#            provider,
#            zoom,
#            mask = TRUE,
#            crop = TRUE) {
#     get <- tryCatch(
#       cartography::getTiles(
#         x = munic,
#         type = provider,
#         crop = crop,
#         zoom = zoom,
#         verbose = FALSE
#       ),
#       error = function(e) {
#         return(TRUE)
#       }
#     )
#     
#     if (isTRUE(get)) {
#       message("Error with zoom ", zoom, ". Try with ", zoom - 1)
#       if (zoom < 1) {
#         stop("Aborted")
#         
#       }
#       get <- hlp_gettile(munic, provider, zoom - 1, mask)
#     } else {
#       message("Success with zoom ", zoom)
#       return(get)
#     }
#   }
# message("Getting raster")
# raster_nomask <- hlp_gettile(square, "Esri.WorldImagery", zoom, crop = TRUE)
# raster <- raster::mask(raster_nomask, munic)
# 
# # Center
# xtick <- bbx[1] + (bbx[3] - bbx[1]) / 2
# ytick <- bbx[2] + (bbx[4] - bbx[2]) / 2
# 
# #5. Raster map----
# 
# title <- paste0(munic$name, "\n(", munic$LAU_CODE, ")")
# sub <- unique(c(munic$prov.shortname.es, munic$ccaa.shortname.es))
# sub <- paste(sub, collapse = ", ")
# pop <- paste0("Population (2019): ", munic$pob19)
# 
# # To convert lon lat from decimal to pretty
# prettylab <- function(x, type) {
#   coordinit <- x
#   x <- abs(x)
#   D <- as.integer(x)
#   m <- (x - D) * 60
#   M <- as.integer(m)
#   S <- round((m - M) * 60, 2)
#   
#   if (type == "lon") {
#     if (coordinit > 0) {
#       lab <- "E"
#     } else {
#       lab <- "W"
#     }
#   } else {
#     if (coordinit > 0) {
#       lab <- "N"
#     } else {
#       lab <- "S"
#     }
#   }
#   
#   label <- paste0(D, "\u00b0 ", M, "' ", S, '\" ', lab)
#   return(label)
# }
# 
# xlab <- prettylab(xtick, "lon")
# ylab <- prettylab(ytick, "lat")
# 
# 
# # Overall map
# map <- tm_shape(raster, raster.downsample = FALSE) +
#   tm_rgb() +
#   tm_shape(raster_nomask, raster.downsample = FALSE) +
#   tm_rgb(alpha = 0.5) +
#   tm_layout(
#     main.title = title,
#     asp = 1,
#     bg.color = "grey95",
#     main.title.position = "center",
#     main.title.size = 1,
#     main.title.fontface = "bold",
#     inner.margins = c(0.1, 0, 0.12, 0),
#     outer.margins = c(0, 0.1, 0, 0.1),
#     attr.outside = FALSE,
#     frame = FALSE,
#     design.mode = FALSE
#   ) +
#   tm_credits(
#     paste0(
#       pop,
#       "\n",
#       "Infraestructura de Datos Espaciales de Espa\u00f1a (IDEE)"
#     ),
#     align = "right",
#     fontface = "bold.italic",
#     size = 0.8
#   ) +
#   tm_credits(
#     text = paste0("\n", sub, "\n", ylab, " / ", xlab),
#     size = 0.85,
#     just = "center",
#     align = "center",
#     fontface = "bold",
#     position = c("center", "TOP")
#   )
# 
# 
# # Create Inset
# mapESP <- esp_get_country(moveCAN = c(13, 0))
# mapProv <- esp_get_prov(prov = munic$cpro, moveCAN = c(13, 0))
# 
# 
# municinset <- esp_get_munic(moveCAN = c(13, 0))
# municinset <- municinset[municinset$LAU_CODE == munic$LAU_CODE,]
# municinset <- st_centroid(municinset, of_largest_polygon = TRUE)
# 
# 
# bboxCAN <- esp_get_can_box(moveCAN = c(13, 0), style = "left")
# 
# insetmap <- tm_shape(mapESP) +
#   tm_polygons(col = "grey75", border.col = "black") +
#   tm_layout(frame = FALSE,
#             bg.color = "transparent") +
#   tm_shape(bboxCAN) +
#   tm_lines(col = "black") +
#   tm_shape(mapProv) +
#   tm_polygons(col = "grey85", border.col = "black") +
#   tm_layout(
#     design.mode = FALSE,
#     asp = 1,
#     inner.margins = c(0, 0, 0, 0),
#     outer.margins = c(0, 0, 0, 0)
#   ) +
#   tm_shape(municinset) +
#   tm_symbols(col = "red",
#              size = 0.6,
#              border.col = "red")
# 
# tmap_save(
#   tm = map,
#   filename = "./assets/img/munic-satellite.png",
#   insets_tm = insetmap,
#   height = 7,
#   width = 7,
#   insets_vp = viewport(
#     x = 0.18,
#     y = 0.175,
#     w = .35,
#     h = .35
#   )
# )
# 
# 
# #6. OSM map----
# # https://github.com/danielredondo/30diasdegraficos/blob/master/scripts/18_mapa.R
# 
# munictransf <- munic %>% st_transform(4326)
# municbb <- munictransf %>%  st_bbox()
# 
# types <-
#   c(
#     "motorway",
#     "primary",
#     "motorway_link",
#     "primary_link",
#     "secondary",
#     "tertiary",
#     "secondary_link",
#     "tertiary_link",
#     "unclassified",
#     "residential",
#     "living_street",
#     "pedestrian",
#     "service",
#     "track"
#   )
# message("Requesting OSM")
# osmlines <- opq(municbb)  %>%
#   add_osm_feature(key = "highway",
#                   value = types) %>%
#   osmdata_sf()
# 
# message("Streets OK")
# obj.lines <- osmlines$osm_lines %>% st_transform(3857)
# 
# major <-
#   obj.lines %>%
#   filter(
#     highway %in%  c(
#       "motorway",
#       "primary",
#       "motorway_link",
#       "primary_link",
#       "secondary",
#       "tertiary",
#       "secondary_link",
#       "tertiary_link"
#     )
#   )
# 
# minor <- obj.lines %>%
#   filter(!highway %in% unique(major$highway))
# 
# river <- opq(municbb) %>%
#   add_osm_feature(key = "waterway",
#                   value = c("river",
#                             #"stream",
#                             "canal")) %>%
#   osmdata_sf()
# message("River OK")
# river <- river$osm_lines
# 
# 
# waters <- opq(municbb) %>%
#   add_osm_feature(key = "natural",
#                   value = c("water")) %>%
#   osmdata_sf()
# message("Waters OK")
# 
# waterspol <- waters$osm_polygons
# watersmpol <- waters$osm_multipolygons
# 
# 
# # Ready to plot
# 
# munictransf2 <- munictransf %>% st_transform(3857)
# 
# streetmap <- tm_shape(munictransf2) +
#   tm_fill("white") +
#   tm_layout(
#     main.title = title,
#     asp = 1,
#     bg.color = "white",
#     main.title.position = "center",
#     main.title.size = 1,
#     main.title.fontface = "bold",
#     inner.margins = c(0.1, 0, 0.12, 0),
#     outer.margins = c(0, 0.1, 0, 0.1),
#     attr.outside = FALSE,
#     frame = FALSE,
#     design.mode = FALSE
#   ) +
#   tm_credits(
#     paste0(pop, "\n",
#            "\u00a9 OpenStreetMap Contributors"),
#     align = "right",
#     fontface = "italic",
#     size = 0.8,
#     bg.color = "white",
#     bg.alpha = 0.5
#   ) +
#   tm_credits(
#     text = paste0("\n", sub, "\n", ylab, " / ", xlab),
#     size = 0.85,
#     just = "center",
#     align = "center",
#     position = c("center", "TOP"),
#     bg.color = "white",
#     bg.alpha = 0.5
#   )
# 
# 
# if (!is.null(waterspol)) {
#   waterspol <- waterspol %>%
#     st_transform(3857) %>%
#     st_intersection(munictransf2)
#   
#   if (any(!st_is_empty(waterspol))) {
#     streetmap <- streetmap +
#       tm_shape(waterspol) +
#       tm_fill("#98cdff")
#   }
# } else {
#   message("No Water Pols")
# }
# 
# if (!is.null(watersmpol)) {
#   watersmpol <- watersmpol %>%
#     st_transform(3857) %>%
#     st_intersection(munictransf2)
#   
#   if (any(!st_is_empty(watersmpol))) {
#     streetmap <- streetmap +
#       tm_shape(watersmpol) +
#       tm_fill("#98cdff")
#   }
# } else {
#   message("No Water MultiPols")
# }
# 
# if (!is.null(river)) {
#   river <- river %>%
#     st_transform(3857) %>%
#     st_intersection(munictransf2)
#   
#   if (any(!st_is_empty(river))) {
#     streetmap <- streetmap +
#       tm_shape(river) +
#       tm_lines("#98cdff",
#                lwd = 1.5)
#   }
# } else {
#   message("No River")
# }
# 
# if (!is.null(minor)) {
#   minor <- minor %>%
#     st_transform(3857) %>%
#     st_intersection(munictransf2)
#   
#   if (any(!st_is_empty(minor))) {
#     streetmap <- streetmap +
#       tm_shape(minor) +
#       tm_lines("grey30", lwd = 1)
#   }
# } else {
#   message("No minor streets")
# }
# 
# if (!is.null(major)) {
#   major <- major %>%
#     st_transform(3857) %>%
#     st_intersection(munictransf2)
#   
#   if (any(!st_is_empty(major))) {
#     streetmap <- streetmap +
#       tm_shape(major) +
#       tm_lines(col = "black",
#                lwd = 1.5)
#   }
# } else {
#   message("No major streets")
# }
# 
# insetmap2 <- tm_shape(mapESP) +
#   tm_polygons(col = "white", border.col = "black") +
#   tm_layout(frame = FALSE,
#             bg.color = "transparent") +
#   tm_shape(bboxCAN) +
#   tm_lines(col = "black") +
#   tm_shape(mapProv) +
#   tm_fill("grey75") +
#   tm_layout(
#     design.mode = FALSE,
#     asp = 1,
#     inner.margins = c(0, 0, 0, 0),
#     outer.margins = c(0, 0, 0, 0)
#   ) +
#   tm_shape(municinset) +
#   tm_symbols(col = "red",
#              size = 0.6,
#              border.col = "red")
# 
# 
# 
# tmap_save(
#   tm = streetmap,
#   filename = "./assets/img/munic-streets.png",
#   insets_tm = insetmap2,
#   height = 7,
#   width = 7,
#   insets_vp = viewport(
#     x = 0.18,
#     y = 0.175,
#     w = .35,
#     h = .35
#   )
# )
# 
# #7. Plot the Journey ----
# 
# timejson <-
#   as.character(format(Sys.time(), tz = "CET", usetz = TRUE))
# 
# df$datetime <- timejson
# 
# df <- df %>% select(-pob19)
# 
# datalog <- rbind(datalog, df) %>% filter(cpro != "xxx") %>% unique()
# 
# municall <- esp_get_munic(moveCAN = c(13, 0)) %>%
#   mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))
# 
# 
# journerylog <- datalog %>% select(LAU_CODE_NUM) %>%
#   mutate(LAU_CODE_NUM = as.numeric(LAU_CODE_NUM),
#          order = seq_len(nrow(datalog)))
# 
# municall <- municall %>% inner_join(journerylog) %>% arrange(order)
# cent <- st_centroid(municall, of_largest_polygon = TRUE)
# 
# last <- cent[nrow(cent), ]
# 
# line <- st_linestring(st_coordinates(cent)) %>%
#   st_sfc(crs = st_crs(cent))
# 
# # Subtitle
# uniquevisited <-
#   datalog %>% select(LAU_CODE) %>% unique() %>% nrow()
# all <- nrow(mapdata)
# sub <-
#   paste0(prettyNum(uniquevisited, big.mark = ","),
#          " visited of ",
#          prettyNum(all, big.mark = ","))
# perc <- paste0(round(100 * uniquevisited / all, 2), "%")
# 
# sub <- paste0(sub, " (", perc, ").")
# 
# 
# journey <- tm_shape(mapESP) +
#   tm_polygons(col = "grey75", border.col = "grey75") +
#   tm_layout(
#     frame = FALSE,
#     asp = 1,
#     inner.margins = c(0, 0, 0.1, 0),
#     main.title = "spain-munic-bot journey",
#     main.title.size = 1.5,
#     main.title.position = "center",
#     title = sub,
#     title.position = c("center", "top"),
#     title.size = 1,
#     bg.color = "grey95"
#   ) +
#   tm_shape(bboxCAN) +
#   tm_lines(col = "grey75")
# 
# if (nrow(cent) > 1) {
#   journey <- journey +
#     tm_shape(line) +
#     tm_lines("red",
#              alpha = 0.65)
# }
# 
# # Add last point
# journey <- journey +  tm_shape(cent) +
#   tm_symbols(
#     col = "red",
#     size = 0.4,
#     alpha = 0.75,
#     border.col = "grey20"
#   ) +
#   tm_shape(last) +
#   tm_text(
#     "name",
#     shadow = TRUE,
#     bg.color = "white",
#     bg.alpha = 0.7,
#     auto.placement = 1
#   )
# 
# 
# 
# tmap_save(
#   tm = journey,
#   filename = "./assets/img/journey.png",
#   height = 7,
#   width = 7
# )
# 
# # Write JSON file
# 
# lastseen <- paste0(munic$name,
#                    ", ", munic$prov)
# 
# 
# tweet <- list(
#   "lasttweet" = timejson,
#   "progress" = perc,
#   "lastseen" = lastseen,
#   "timestamp" = as.integer(lubridate::now())
# ) %>% toJSON()
# 
# write(tweet, file.path("assets", "lasttweet.json"))
# message("JSON file written")
# 
# #8. Tweet ----
# 
# 
# 
# # Prepare tweet
# 
# msg <-
#   paste0(munic$name, " (", sprintf("%05d", munic$LAU_CODE_NUM), ")")
# 
# sub <- unique(c(munic$prov.shortname.es, munic$ccaa.shortname.es))
# sub <- paste(sub, collapse = ", ")
# 
# msg <-
#   paste(msg, sub, sep = ", ")
# msg <- paste0(msg, " - ", ylab, " / ", xlab, " ")
# hash <-
#   paste0("#spainmunic", sprintf("%05d", munic$LAU_CODE_NUM), " ")
# 
# msg <- paste0(msg, hash)
# 
# # Hash2
# hash2 <- paste0(" #", gsub(" ", "", munic$prov.shortname.es), " ")
# msg <- paste0(msg, hash2)
# 
# addsat <- ifelse((nrow(datalog) %% 500) == 0,
#                  " Done in #rstats using #tmap, #rspatial, #mapSpain and #rtweet. ",
#                  " ")
# addstreet <- ifelse((nrow(datalog) %% 500) == 20,
#                     " Done in #rstats using #tmap, #osmdata, #rspatial, #mapSpain and #rtweet. ",
#                     " "
# )
# # Add credits
# addsat2 <- ifelse(nrow(datalog) %% 530 == 0,
#                   "Sources @IDEESpain #rstatsES",
#                   "")
# addstreet2 <- ifelse(nrow(datalog) %% 530 == 10,
#                      "Sources @openstreetmap #rstatsES",
#                      "")
# 
# msgsatellite <- paste0(msg, addsat, addsat2)
# msgstreet <- paste0(msg, addstreet, addstreet2)
# msgsatellite <- gsub("  ", " ", msgsatellite)
# msgstreet <- gsub("  ", " ", msgstreet)
# # Tweet satellite
# cent <- munic %>% st_geometry() %>%
#   st_centroid(of_largest_polygon = TRUE) %>%
#   st_transform(4326) %>% st_coordinates()
# 
# post_tweet(
#   msgsatellite,
#   media = file.path("assets", "img", "munic-satellite.png"),
#   lat = cent[2],
#   long = cent[1],
#   display_coordinates = TRUE
# )
# message("Tweet satellite posted")
# message("Sleep for 3 seconds")
# 
# Sys.sleep(3)
# 
# post_tweet(
#   msgstreet,
#   media = file.path("assets", "img", "munic-streets.png"),
#   lat = cent[2],
#   long = cent[1],
#   display_coordinates = TRUE
# )
# message("Tweet streets posted")
# 
# if ((nrow(datalog) %% 600) == 0) {
#   Sys.sleep(3)
#   seen <- nrow(datalog)
#   left <- nrow(mapdata) - seen
#   seen <- prettyNum(seen, big.mark = ".", decimal.mark = ",")
#   left <- prettyNum(left, big.mark = ".", decimal.mark = ",")
#   msg <- paste0(
#     "Time to recap! ",
#     seen,
#     " places visited, ",
#     left,
#     " left. Check my journey so far https://dieghernan.github.io/spain-munic-bot/journey #rspatial"
#   )
#   msg <- gsub("  ", " ", msg)
#   post_tweet(msg, media = file.path("assets", "img", "journey.png"))
#   message("Tweet summary posted")
#   
# }
# 
# #9. Clean ----
# # Save datalog if everything was correct
# write.table(datalog,
#             "./assets/datalog.csv",
#             sep = ",",
#             row.names = FALSE)
# 
# rm(list = ls())
