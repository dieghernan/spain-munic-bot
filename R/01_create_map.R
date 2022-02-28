# 1. Load libraries ----

library(CatastRo)
library(mapSpain)
library(dplyr)
library(sf)
library(readr)
library(stringr)
library(stringi)
library(ggplot2)
library(cowplot)
library(rtweet)
library(tidyr)
library(jsonlite)


time <- as.character(format(Sys.time(), tz = "CET", usetz = TRUE))


message("Connect with twitter")

api_key <- Sys.getenv("TWITTER_API_KEY")
api_secret_key <- Sys.getenv("TWITTER_API_SECRET")
access_token <- Sys.getenv("TWITTER_ACCESS_TOKEN")
access_token_secret <- Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
# authenticate via web browser
token <- rtweet_bot(
  api_key = api_key,
  api_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret
)

auth_as(token)

# 2. Load data ----

data <- esp_get_capimun(moveCAN = FALSE) %>%
  filter(!codauto %in% c(15, 16)) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))

mapdata <- data

# Fix names
# Create df
namesprov <- esp_codelist %>%
  select(codauto, ccaa.shortname.es, cpro, prov.shortname.es) %>%
  unique()


data <- data %>%
  mutate(
    nameinit = name,
    name = trimws(tools::toTitleCase(word(name, sep = "/"))),
  ) %>%
  select(cpro, name, LAU_CODE, LAU_CODE_NUM) %>%
  left_join(namesprov)


# Load log

if (file.exists("assets/datalog.csv")) {
  datalog <-
    read_csv("assets/datalog.csv", show_col_types = FALSE)

  datalog <- datalog %>% mutate(
    codauto = as.numeric(codauto),
    cpro = as.numeric(cpro),
    LAU_CODE = as.character(LAU_CODE),
    LAU_CODE_NUM = as.numeric(LAU_CODE_NUM)
  )
} else {
  # If it doesn't exist, create one empty
  datalog <- tibble::tibble(LAU_CODE_NUM = 999999)
}



# 3. Select randomly after clean up ----

data_filter <- data[!data$LAU_CODE_NUM %in% datalog$LAU_CODE_NUM, ]


# Override

if (file.exists("assets/override.csv")) {
  message("Override!")
  override <-
    read_csv("assets/override.csv", show_col_types = FALSE)
  overridenum <- override$LAU_CODE_NUM
  file.remove("assets/override.csv")
  data_filter <-
    data[data$LAU_CODE_NUM %in% overridenum, ]

  # Dedupe
  datalog <-
    datalog[datalog$LAU_CODE_NUM != data_filter$LAU_CODE_NUM, ]
}


# Stop if we are done

if (nrow(data_filter) < 1) {
  message("End with maps - All cities have been mapped")
  quit()
}

sel <- sample(seq_len(nrow(data_filter)), 1)

# Get and register munic
munic <- data_filter %>% slice(sel)

df <- munic %>%
  st_drop_geometry() %>%
  mutate(datetime = time) %>%
  select(
    codauto,
    ccaa.shortname.es,
    cpro,
    prov.shortname.es,
    LAU_CODE,
    LAU_CODE_NUM,
    name,
    datetime
  )



message(
  "Munic selected: ",
  df$name,
  " ",
  df$LAU_CODE
)

# 4. Spatial operations----

# Get cadastral code
code <- catr_get_code_from_coords(munic)



# Get buildings
bu <- catr_atom_get_buildings(code$catrcode, cache_dir = tempdir())
center <- st_geometry(munic)

# area - start with 2500m
init <- 2500

# Handle map
buff <- center %>%
  st_transform(st_crs(bu)) %>%
  st_buffer(init) %>%
  st_geometry()

bu_cut <- suppressWarnings(st_intersection(bu, buff))
perc <- as.double(sum(st_area(bu_cut)) / st_area(buff))

# If not to much building increase buffer
if (perc < 0.1) {
  continue <- TRUE
} else {
  continue <- FALSE
}

init <- 2500
while (continue) {
  if (init > 500) {
    init <- init - 500
  } else {
    init <- init - 100
  }

  # Handle map
  buff <- center %>%
    st_transform(st_crs(bu)) %>%
    st_buffer(init) %>%
    st_geometry()


  bu_cut <- suppressWarnings(st_intersection(bu, buff))


  if (init <= 200) {
    break
  }


  perc <- as.double(sum(st_area(bu_cut)) / st_area(buff))

  if (perc < .1) {
    continue <- TRUE
  } else {
    continue <- FALSE
  }
}

message("\nFinal buffer: ", init, " m.")


# 5. Handle years ----

# Year
# Extract 4 initial positions
year <- substr(bu_cut$beginning, 1, 4)

# Replace all that doesn't look as a number with 0000
year[!(year %in% 0:2500)] <- "0000"

# To numeric
year <- as.integer(year)

# New column
dataviz <- bu_cut %>%
  mutate(year = year)


dis_years <- length(unique(dataviz$year))
cuts <- classInt::classIntervals(dataviz$year,
  style = "quantile",
  n = min(
    15,
    dis_years - 1
  )
)

brk <- unique(as.integer(cuts$brks))

if (length(brk) < 3) {
  quint <- quantile(dataviz$year, seq(0, 1, .1))
  brk <- unique(as.integer(quint))
}

if (length(brk) < 3) {
  brk <- unique(as.integer(dis_years))
}

labs <- paste0(brk[-length(brk)], "-", brk[-1])
labs[1] <- paste0("< ", brk[2])
labs[length(labs)] <- paste0(brk[(length(brk) - 1)], " - ")

dataviz$year_cat <- cut(dataviz$year,
  brk,
  include.lowest = TRUE,
  labels = labs
)



# 6. Prepare map ----

lim_map <- st_bbox(buff)

pp <- ggplot(dataviz) +
  geom_sf(aes(fill = year_cat), color = NA) +
  scale_fill_manual(values = hcl.colors(length(labs), "Spectral")) +
  guides(fill = guide_legend(keywidth = .7, keyheight = .3)) +
  coord_sf(
    xlim = lim_map[c(1, 3)],
    ylim = lim_map[c(2, 4)]
  ) +
  theme_void() +
  labs(
    title = df$name,
    subtitle = df$prov.shortname.es,
    fill = "",
    caption = "Data: D.G Catastro\nBased on a idea of Dominic Royé (@dr_xeo)"
  ) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    legend.justification = .5,
    legend.text = element_text(
      colour = "white",
      size = 10,
      margin = margin(t = 6, b = 6)
    ),
    plot.title = element_text(
      colour = "white",
      hjust = .5,
      margin = margin(t = 15),
      size = 20
    ),
    plot.subtitle = element_text(
      colour = "white",
      hjust = .5,
      margin = margin(t = 15),
      size = 16
    ),
    plot.caption = element_text(
      colour = "white",
      margin = margin(b = 15),
      hjust = 1,
      size = 7
    ),
    plot.margin = margin(r = 15, l = 15)
  )



# Prepare inset
ccaa <- esp_get_ccaa() %>% st_transform(st_crs(center))


boxcan <- esp_get_can_box()

# Need to reextract Canary moved
munic_cent <- esp_get_capimun(moveCAN = TRUE) %>%
  filter(LAU_CODE == munic$LAU_CODE) %>%
  st_transform(st_crs(ccaa))


inset <- ggplot(ccaa) +
  geom_sf(
    fill = NA,
    color = adjustcolor("white", alpha.f = 0.7),
    size = 0.06
  ) +
  geom_sf(
    data = boxcan,
    color = adjustcolor("white", alpha.f = 0.7),
    size = 0.06
  ) +
  geom_sf(
    data = munic_cent,
    color = hcl.colors(15, "Spectral")[8],
    size = 2
  ) +
  theme_void() +
  theme(plot.background = element_rect(
    fill = "black",
    color =
      adjustcolor("white",
        alpha.f = 0.5
      )
  ))


# Add inset to map
# https://geocompr.github.io/post/2019/ggplot2-inset-maps/


gg_w_inset <- ggdraw() +
  draw_plot(pp) +
  draw_plot(inset,
    height = 0.17,
    x = -0.36,
    y = 0.02
  )

name <- file.path("assets", "img", "imgtweet.png")
ggsave(
  name,
  gg_w_inset,
  dpi = 300,
  width = 2100,
  height = 2100,
  units = "px",
  bg = "black"
)


# 7. Save the Journey ----

timejson <-
  as.character(format(Sys.time(), tz = "CET", usetz = TRUE))

df$datetime <- timejson

# Convert cols

datalog <- datalog %>% mutate(
  codauto = as.numeric(codauto),
  cpro = as.numeric(cpro),
  LAU_CODE = as.character(LAU_CODE),
  LAU_CODE_NUM = as.numeric(LAU_CODE_NUM)
)

df <- df %>% mutate(
  codauto = as.numeric(codauto),
  cpro = as.numeric(cpro),
  LAU_CODE = as.character(LAU_CODE),
  LAU_CODE_NUM = as.numeric(LAU_CODE_NUM)
)

datalog <- datalog %>%
  bind_rows(df) %>%
  drop_na() %>%
  distinct()

municall <- esp_get_capimun(moveCAN = c(13, 0)) %>%
  mutate(LAU_CODE_NUM = as.numeric(LAU_CODE))


journerylog <- datalog %>%
  select(LAU_CODE_NUM) %>%
  mutate(
    LAU_CODE_NUM = as.numeric(LAU_CODE_NUM),
    order = seq_len(nrow(datalog))
  )

municall <- municall %>%
  inner_join(journerylog) %>%
  arrange(order)

cent <- municall

last <- cent[nrow(cent), ]

line <- st_linestring(st_coordinates(cent)) %>%
  st_sfc(crs = st_crs(cent))

# Subtitle
uniquevisited <-
  datalog %>%
  select(LAU_CODE) %>%
  unique() %>%
  nrow()

# Pais Vasco y Navarra no están en el censo
ccaa_nopvn <- ccaa %>%
  filter(!codauto %in% c(15, 16))

pvn <- ccaa %>%
  filter(codauto %in% c(15, 16))

all <- nrow(mapdata)
sub <-
  paste0(
    prettyNum(uniquevisited, big.mark = ","),
    " visited of ",
    prettyNum(all, big.mark = ",")
  )
perc <- paste0(round(100 * uniquevisited / all, 2), "%")

sub <- paste0(sub, " (", perc, ").")


journey <- ggplot(ccaa_nopvn) +
  geom_sf(col = NA, fill = "grey75") +
  geom_sf(data = pvn, col = NA, fill = "black") +
  geom_sf(data = boxcan, color = "grey75") +
  theme_void() +
  labs(
    title = "spain-munic-bot journey",
    subtitle = sub
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


if (nrow(cent) > 1) {
  journey <- journey +
    geom_sf(
      data = line,
      color = "red",
      alpha = 0.5,
      size = 0.5
    ) +
    geom_sf(
      data = last,
      size = 2,
      color = "red",
      alpha = 0.6
    )
}

ggsave(
  filename = "./assets/img/journey.png",
  journey,
  height = 7,
  width = 7,
  bg = "grey95"
)

# Write JSON file

lastseen <- paste0(
  munic$name,
  ", ", munic$prov
)


tweet <- list(
  "lasttweet" = timejson,
  "progress" = perc,
  "lastseen" = lastseen,
  "timestamp" = as.integer(lubridate::now())
) %>% toJSON()

write(tweet, file.path("assets", "lasttweet.json"))
message("JSON file written")


# 8. Prepare tweet ----

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


coords <- st_transform(munic, 4326) %>% st_coordinates()
xtick <- coords[1]
ytick <- coords[2]
xlab <- prettylab(xtick, "lon")
ylab <- prettylab(ytick, "lat")

msg <-
  paste0(df$name, " (", df$LAU_CODE, ")")

sub <- unique(c(df$prov.shortname.es, df$ccaa.shortname.es))
sub <- paste(sub, collapse = ", ")

msg <-
  paste(msg, sub, sep = ", ")
msg <- paste0(msg, " - ", ylab, " / ", xlab, " ")
hash <-
  paste0("#spainmunic", df$LAU_CODE, " ")

msg <- paste0(msg, hash)

# Hash2
hash2 <- paste0(" #", gsub(" ", "", df$prov.shortname.es), " ")
msg <- paste0(msg, hash2)


# Add packs

packs <- ifelse((nrow(datalog) %% 500) == 20,
  "Done in #rstats using #ggplot2, #CatastRo, #rspatial, #mapSpain and #rtweet ",
  ""
)

msg <- paste(msg, packs)


# Add credits
cred <- ifelse(nrow(datalog) %% 520 == 0,
  "Sources #CatastroESP #rstatsES ",
  ""
)

msg <- paste(msg, cred)

# Tweet!

post_tweet(
  status = msg,
  media = name,
  media_alt_text = paste0("Map of buildings of ", df$name),
  lat = coords[2],
  long = coords[1],
  display_coordinates = TRUE
)



if ((nrow(datalog) %% 600) == 0) {
  Sys.sleep(3)
  seen <- nrow(datalog)
  left <- nrow(mapdata) - seen
  seen <- prettyNum(seen, big.mark = ".", decimal.mark = ",")
  left <- prettyNum(left, big.mark = ".", decimal.mark = ",")
  msg <- paste0(
    "Time to recap! ",
    seen,
    " places visited, ",
    left,
    " left. Check my journey so far https://dieghernan.github.io/spain-munic-bot #rspatial"
  )
  msg <- gsub("  ", " ", msg)
  post_tweet(msg,
    media = file.path("assets", "img", "journey.png"),
    media_alt_text = "My journey"
  )
  message("Tweet summary posted")
}

# 9. Clean ----
# Save datalog if everything was correct

message("Status; OK")
datalog <- datalog[names(df)]


write_csv(datalog, "./assets/datalog.csv")

rm(list = ls())
