library(dplyr)
library(ggplot2)
library(lubridate)

# 1. Load log ----

datalog <-
  read.csv2("assets/datalog.csv",
            sep = ",",
            stringsAsFactors = FALSE)

datalog <- datalog %>%
  mutate(hour = parse_date_time(datalog$datetime, c("Ymd HMS"), tz = "CET")) %>%
  mutate(hour = floor_date(hour, "hour"))


# 2. Create table with all hours of the days ----
now <-
  lubridate::now(tzone = "CET") %>% ceiling_date("day")
lastweek <-
  (lubridate::now(tzone = "CET") - days(7)) %>% floor_date("day")

now <- now + days(1)

master <- data.frame(hour = seq(lastweek, now, by = "hour"),
                     day = date(seq(lastweek, now, by = "hour")))

master <-
  master %>% left_join(datalog) %>% select(day, hour, LAU_CODE)


#3  Tweets hourly 3day activity ----

# 3 days before

filterdays <- (lubridate::now(tzone = "CET") - days(3))


daily <-
  master %>% filter(hour >= filterdays) %>% filter(!is.na(LAU_CODE))
  
summday <- daily %>% group_by(hour) %>% summarise(n = n())

brday <- seq(0, max(summday$n) + 1, 1)
  
vlinesd <- (daily$hour) %>% floor_date("day") %>% unique()
  
sublines <- vlinesd + hours(12)

sub <-
  paste0("Updated: ", as.character(format(
    Sys.time(), tz = "CET", usetz = TRUE
  )))

p <- ggplot(daily, aes(hour)) +
  geom_bar(fill = "#EB6864") +
  geom_vline(xintercept = vlinesd,
             col = "grey50",
             alpha = 0.4) +
  labs(title = "Bot activity: Hourly", y = "# tweets", x = sub) +
  theme_minimal() +
  scale_x_datetime(breaks = vlinesd, 
  date_labels = "%Y-%m-%d",
  minor_breaks = sublines) +
  scale_y_continuous(breaks = brday,
  minor_breaks = NULL,
  limits = c(0, max(summday$n) + 1)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 3 / 4,
    rect = element_rect(fill = "transparent"),
    axis.text.x.bottom = element_text(size = 7),
    axis.title.x.bottom = element_text(
      face = "italic",
      size = 7,
      margin = margin(10, 0, 0, 0)
    ),
    axis.title.y = element_text(margin = margin(0, 30, 0, 0))
  )




ggsave("assets/img/monitor-hourly.png",
       p,
       width = 7,
       height = 7 * 0.75)


#4  Tweets daily 1 week activity ----

# 3 days before

filterweek <- lastweek


weekly <-
  master %>% filter(hour >= filterweek) %>% filter(!is.na(LAU_CODE))

summ <- weekly %>% group_by(day) %>% summarise(n = n())

br <- seq(0, max(summ$n) + 10, 10)


sub <-
  paste0("Updated: ", as.character(format(
    Sys.time(), tz = "CET", usetz = TRUE
  )))

w <- ggplot(weekly, aes(day)) +
  geom_bar(fill = "#EB6864") +
  scale_y_continuous(breaks = br,
    minor_breaks = NULL
  ) +
  stat_count(
    geom = "text",
    colour = "white",
    fontface = "bold",
    size = 3.5,
    aes(label = ..count..),
    position = position_stack(vjust = 0.5)
  ) +
  labs(title = "Bot activity: Daily", y = "# tweets", x = sub) +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 day") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    aspect.ratio = 3 / 4,
    rect = element_rect(fill = "transparent"),
    axis.text.x.bottom = element_text(size = 7),
    axis.title.x.bottom = element_text(
      face = "italic",
      size = 7,
      margin = margin(10, 0, 0, 0)
      ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(margin = margin(0, 30, 0, 0))
  )




ggsave("assets/img/monitor-daily.png",
       w,
       width = 7,
       height = 7 * 0.75)


# 5. Clean----

rm(list = ls())


