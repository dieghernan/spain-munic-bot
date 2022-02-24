library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggplot2)


# 1. Load log ----
url <-
    "https://raw.githubusercontent.com/dieghernan/spain-munic-bot/main/assets/datalog.csv"

datalog <-
    read.csv2(url,
              sep = ",",
              stringsAsFactors = FALSE)


datalog <- datalog %>%
    mutate(hour = parse_date_time(datalog$datetime, c("Ymd HMS"), tz = "CET")) %>%
    mutate(hour = floor_date(hour, "hour")) %>%
    select(hour, LAU_CODE)

datalog <- datalog %>% mutate(day = as_date(hour, "CET"))


datalog %>% mutate(day = as_date(hour)) %>% group_by(day) %>% summarise(n = n())

# Params
today <- now("CET") %>% floor_date("day") %>% lubridate::as_date()
lastweek <-
    (now("CET") - days(7)) %>% floor_date("day") %>% lubridate::as_date()

min <- min(datalog$hour) %>% lubridate::as_date()


# 2. UI ----

ui <- fluidPage(
    theme = shinytheme("journal"),
    tags$head(tags$style(
        HTML(
            '
        body {
          font-family: -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,"Noto Sans",sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol","Noto Color Emoji";
        }
        div.col-sm-12 {
          max-height:33vh;
        }
        #plothour {
          max-height:30vh;
        }
        #plotday {
          max-height:30vh;
        }
        #button {
          margin-top: 0.5rem;
          color: #fff;
          background-color: #EB6864;
          border-color: #EB6864;
          font-family: "News Cycle","Arial Narrow Bold",sans-serif !important;
          font-weight: 700 !important;
          padding: .25rem .5rem;
          font-size: .875rem;
          line-height: 1.5;
          border-radius: .2rem;
        }
        '
        )
    )),
    fluidRow(
        column(
            12,
            dateRangeInput(
                "range",
                "Select dates",
                lastweek,
                today,
                min = min,
                max = today
            ),
            actionButton("button", "Reset Date"),
            HTML(
                paste0(
                    "<p>",
                    "<small><em>Updated: ",
                    as.character(format(
                        lubridate::now("CET"), tz = "CET", usetz = TRUE
                    )),
                    "</em></small></p>"
                )
            )
        ),
        column(12,
               plotOutput(
                   'plothour',
                   brush = brushOpts("brush", direction = "x", resetOnNew = TRUE)
               )),
        column(12,
               plotOutput('plotday'))
    )
)

# 3. Server ----

server <- function(input, output, session) {
    # Action button
    observeEvent(input$button, {
        updateDateRangeInput(session, "range", start = lastweek, end = today)
    })
    # Brush on days
    observe({
        if (is.null(input$brush))
            return()
        
        # Input should be dates
        start <- structure(input$brush$xmin, class = "POSIXct")
        end <- structure(input$brush$xmax, class = "POSIXct")
        start <-
            lubridate::floor_date(start, "day") %>% lubridate::as_date()
        updateDateRangeInput(session, "range", start = start, end = end)
        
    })
    
    # Reactive filtering
    df <- reactive({
        # Change this to datetimes
        init <- input$range[1] %>%
            lubridate::as_datetime("CET") %>%
            lubridate::floor_date("day")
        end <- input$range[2] %>%
            lubridate::as_datetime("CET") %>%
            lubridate::ceiling_date("day")
        end <- max(init, end)
        
        df <- datalog %>% filter(hour >= init & hour <= end)
        
    })
    
    # Reactive maximum
    maxend <- reactive({
        init <- input$range[1] %>%
            lubridate::as_datetime("CET") %>% floor_date("day")
        
        end <- input$range[2] %>% lubridate::as_datetime("CET")
        end <- max(init, end) %>% ceiling_date("day")
        
    })
    
    
    # Plot hours
    output$plothour <- renderPlot({
        # Format axis
        # Y breaks
        summday <- df() %>% group_by(hour) %>% summarise(n = n())
        
        brday <- seq(0, max(summday$n) + 1, 1)
        
        # X breaks
        now <- maxend()
        min <-
            min(df()$hour) %>% as_datetime("CET") %>% floor_date("day")
        
        vlinesd <-
            seq(as_datetime(min), as_datetime(now), by = "day")
        
        #Plot
        
        ggplot(df(), aes(hour)) +
            geom_bar(fill = "#EB6864") +
            geom_vline(xintercept = vlinesd,
                       col = "grey50",
                       alpha = 0.4) +
            labs(title = "Bot activity: Hourly",
                 y = "# tweets",
                 x = NULL) +
            theme_minimal() +
            scale_x_datetime(
                date_labels = "%y-%m-%d %H:%M",
                timezone = "CET",
                limits = c(min, now)
            ) +
            scale_y_continuous(breaks = brday,
                               minor_breaks = NULL) +
            theme(
                plot.title = element_text(hjust = 0.5),
                rect = element_rect(fill = "transparent"),
                #                axis.text.x.bottom = element_text(size = 7),
                axis.title.y = element_text(margin = margin(0, 30, 0, 0))
            )
    })
    
    # Plot days
    output$plotday <- renderPlot({
        weekly <- df() %>% mutate(day = floor_date(hour, "day"))
        
        limday <-
            c(floor_date(min(weekly$day), "day") - days(1), maxend())
        message(limday)
        
        
        
        
        ggplot(weekly, aes(day)) +
            geom_bar(fill = "#EB6864") +
            stat_count(
                geom = "text",
                colour = "white",
                fontface = "bold",
                size = 5,
                aes(label = ..count..),
                position = position_stack(vjust = 0.5)
            ) +
            scale_x_datetime(date_labels = "%m/%d",
                             limits = limday) +
            labs(title = "Bot activity: Daily",
                 y = "# tweets",
                 x = NULL) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5),
                rect = element_rect(fill = "transparent"),
                #                axis.text.x.bottom = element_text(size = 7),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                axis.title.y = element_text(margin = margin(0, 30, 0, 0))
            )
        
    })
    
}



shinyApp(ui = ui, server = server)
