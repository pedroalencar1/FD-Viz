# Shiny app visualizatio of data and results
# Pedro Alencar - TU Berlin
# https://orcid.org/0000-0001-6221-8580

library(shiny)
library(shinythemes)
library(plotly)
library(leaflet)
library(bslib)
library(caret)
library(ggplot2)
library(dplyr)
library(tidyr)
library(e1071)
library(tidyquant)
library(tidyverse)
library(gapminder)
library(RColorBrewer)
library(tibble)
library(tibbletime)
library(fst)

`%notin%` <- Negate(`%in%`)

# devtools::install_github('pedroalencar1/fdClassify')
# library(fdClassify)

# Global variables - data selection
models <-
  c(
    "Ford and Labosier",
    "Pendergrass et al.",
    "Noguera et al.",
    "Christian et al.",
    "Osman et al.",
    "Multi-criteria"
  )


stations <- list.files()
# this will select only the fluxnet stations
# this is a temporary modification
stations <- stations[substr(stations, start = 7, stop = 22) == "_FLUXNET2015.bin"]
stations <- substr(stations, 1, nchar(stations) - 4)
station_names <- substr(stations, start = 1, stop = 6)

data_stations <- read.delim("stations.txt") #general data from stations 

#general plot paramenters
user_palette <- RColorBrewer::brewer.pal(n = 9, name = "PuBuGn")
user_palette2 <- RColorBrewer::brewer.pal(n = 11, name = "PuOr")
user_palette3 <-
  grDevices::colorRampPalette(brewer.pal(9, "OrRd"))(12)[4:12]


# Events co-occurence
# all_data <- read.csv('all_data_fluxnet.csv')

#create list with all events toguether
all_data_swc <- NULL
for (i in 1:length(stations)){
  # i = 6
  # aux <- read.csv(file = paste(stations[i], '.csv', sep = ''), header = T)[,c(2,4,5,6,7,8,11)]
  # colnames(aux) <- c('Date', models)
  aux <- read_fst(path = paste(stations[i], '.bin', sep = ''))[,c(1,3,4,5,6,7,10,15)]
  aux$Station <- substr(stations[i], start = 1, stop = 6)
  
  all_data_swc <- rbind(all_data_swc, aux)
}

all_data <- all_data_swc[,c(1:7,9)]

# colnames(all_data) <- c('Date', models, 'Station')
all_data$total <- rowSums(all_data[,2:7], na.rm = T)
all_data$Date <- as.Date(all_data$Date)

melt_events <- data.frame(Date = as.Date(sort(rep(seq(as.Date(min(all_data$Date)), 
                                                      as.Date(max(all_data$Date)), 
                                                      'day'), 
                                                  length(stations)))))

melt_events$name <- rep(station_names, length(unique(melt_events$Date)))
melt_events <-left_join(melt_events, all_data[,c(1,8,9)], 
                        by = c("Date" = "Date", "name" = "Station"))
melt_events$year <- lubridate::year(melt_events$Date)

melt_events$name <- as.factor(melt_events$name)
melt_events$Date <- as.Date(melt_events$Date)
melt_events$latitude <-
  VLOOKUP(melt_events$name, data_stations, Station, Latitude)

#### standard break dates for plots
my_breaks <- as.Date(seq(as.Date(min(melt_events$Date)),
                         as.Date(max(melt_events$Date)),
                         by = "6 months"))

# long table - panel 2
all_data$total <- NULL
all_events_long <- pivot_longer(all_data, names_to = 'method', values_to = 'fd', cols = 2:7)
all_events_long$Date <- as.Date(all_events_long$Date)
all_events_long$latitude <- VLOOKUP(all_events_long$Station, data_stations, 'Station', 'Latitude')

## summary
all_events <- data.frame()
for (i in 1:length(station_names)){
  # i = 2
  data_selec <- all_data[all_data$Station == station_names[i],]
  for (j in 1:length(models)){
    # j = 5
    durations <- rle(data_selec[,j+1])
    df_dur <- data.frame(fd = durations$values, lengths = durations$lengths, 
                         positions = runner::runner(durations$lengths, 
                                                    k = length(durations$lengths), 
                                                    f = sum))
    df_dur$begin <- NA
    if (nrow(df_dur) == 1) next
    
    for (k in 2:nrow(df_dur)){
      
      df_dur$begin[k] <- df_dur$positions[k-1]+1
    }
    
    df_dur <- df_dur[df_dur$fd == 1,]
    df_dur$date <- data_selec$Date[df_dur$begin]
    df_dur$station <- station_names[i]
    df_dur$land_use <- data_stations$IGPB2[i]
    df_dur$method <- models[j]
    
    all_events <- rbind(all_events, df_dur)
  }
}

all_events <- all_events[complete.cases(all_events),]
all_events <- all_events[, c(5,6,7,8,2)]

# all_events <- read.csv("all_events_fluxnet.csv")
all_events$month <- lubridate::month(all_events$date)
events_month <- data.frame(
  month = sort(rep(1:12, 22)),
  station = rep(unique(substr(
    stations,
    start = 1, stop = 6)), 12))

events_month <- as_tibble(all_events[, c(6, 2, 4)]) %>%
  group_by(month, station, method) %>%
  dplyr::summarise(value = n())
# aux1$value[aux1$value > 0] <- 1
events_month <-
  pivot_wider(events_month, values_from = 'value', names_from = 'method')
events_month[is.na(events_month)] <- 0
events_month <- events_month[, c(1, 2, 7, 4, 5, 3, 6, 8)]
events_month$`Multi-criteria`[events_month$month %in% c(1, 2, 11, 12)] <- 0
events_month <-
  merge(events_month, data_stations, by.x = "station", by.y = "Station")

# events per year
all_events_summary <- all_events %>%
  group_by(station, method) %>%
  dplyr::summarise(total_evetns = n())

all_events_summary$duration <-  VLOOKUP(
    all_events_summary$station,
    data_stations,
    "Station",
    "Duration"
  )
all_events_summary$event_year <-
  all_events_summary$total_evetns / all_events_summary$duration
# all_events_summary <-
#   all_events_summary[all_events_summary$method %notin%
#                        c("Mod. Ford and Labosier", "Mo and Lettenmeier"), ]

all_events_summary$latitude <-
  VLOOKUP(all_events_summary$station, data_stations, Station, Latitude)

# relative_counting <- read.csv("relative_counting.csv")
# colnames(relative_counting) <- c(
#   "method",
#   "month",
#   "Grasslands",
#   "Croplands",
#   "Mixed forests",
#   "Deciduous broadleaf forests",
#   "Evergreen needleleaf forests",
#   "Evergreen broadleaf forests",
#   "Closed shrublands"
# )

#summary total events by year and station
# totals <- read.csv("totals.csv")
totals <- all_events_summary[,1:4] %>%
  pivot_wider(names_from = 'method', values_from = 'total_evetns')
totals$landuse <- VLOOKUP(totals$station, data_stations, 'Station', 'IGPB2')
totals <- totals[, c(1,4,8,6,3,7,5,2,9)]
colnames(totals) <- c("Station", models, "Duration", "landuse")

##### proportion months with FD
years_lu <- data_stations %>%
  group_by(IGPB2) %>%
  summarise(year = sum(Duration))

relative_counting <- totals[,c(2:7,9)] %>%
  group_by(landuse) %>%
  summarise_all(sum)

relative_counting[,2:7] <- relative_counting[,2:7]/years_lu$year/12 
relative_counting <- as.data.frame(t(relative_counting)) 
colnames(relative_counting) <- relative_counting[1,]
relative_counting <- relative_counting[-1,]
relative_counting$method <- rownames(relative_counting)


##### Auxiliar function
conf_matrix <- function(df, ref) {
  metric1 <- NULL
  for (i in 1:6) {
    # i = 3
    conf_mat <-
      caret::confusionMatrix(data = factor(df[[i]]), reference = factor(ref))
    overall <- data.frame(value = conf_mat$overall)
    byclass <- data.frame(value = conf_mat$byClass)
    conf_mat <- rbind(overall, byclass)
    
    metric1 <- rbind(metric1, t(conf_mat))
  }
  metric1 <- data.frame(t(metric1))
  colnames(metric1) <- models
  
  metric1$metric <- c(row.names(metric1))
  metric1 <- metric1[c(1, 2, 8, 9, 12, 14), ] %>%
    gather(key = "model", value = "value", 1:6) #%>%
    # .[.$model != "Ford and Labosier", ]
  
  return(metric1)
}

stats <- as_tbl_time(all_data[,c(8,1:7)], index = Date) %>%
  # as_tbl_time(Date) %>%
  group_by(Station) %>%
  collapse_by('monthly') %>%
  group_by(Station, Date) %>%
  summarise_all(.funs = max)

stats$`Land Use` <- VLOOKUP(stats$Station, data_stations, 'Station', 'IGPB2')
stats$year <- lubridate::year(stats$Date)
stats$month <- lubridate::month(stats$Date)
# stats <- stats[stats$month %in% 3:10,]
stats$`Ford and Labosier`[stats$month %in% c(1,2,11,12)] <- 0
stats$`Multi-criteria`[stats$month %in% c(1,2,11,12)] <- 0
stats[is.na(stats)] <- 0 #Na as no event



# stats <- read.csv("stats_confmat.csv")
colnames(stats) <-
  c(
    "Station",
    "Date",
    "Ford and Labosier",
    "Pendergrass et al.",
    "Noguera et al.",
    "Christian et al.",
    "Osman et al.",
    "Multi-criteria",
    "Land Use",
    "year",
    "month"
  )

#####


# User interface
ui <- navbarPage(
  "Flash Drought - Interactive Visualisation",
  tabPanel(
    "Events",
    uiOutput("page1"),
    sidebarLayout(
      # Sidebar panel for inputs ----
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        HTML("<h3>Input parameters</h3>"),
        selectInput(
          "file",
          "Choose station and data",
          choices = stations,
          selected = "DE-Kli_FLUXNET2015"
        ),
        selectInput(
          "Method 1",
          "Choose Method 1",
          choices = models,
          selected = "Ford and Labosier"
        ),
        selectInput(
          "Method 2",
          "Choose Method 2",
          choices = models,
          selected = "Noguera et al."
        ),
        sliderInput(
          "range",
          "Choose years",
          min = as.Date("1994-01-01"),
          max = as.Date("2014-12-31"),
          value = c(as.Date("2006-01-01"), as.Date("2006-12-31"))
        ),
        width = 3,
        # submitButton("Update plots", icon('refresh'), width = '100%'),
        actionButton(
          inputId = "update1",
          label = "Update plots",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        leafletOutput("graph6"),
      ),
      mainPanel(
        width = 9,
        plotlyOutput("graph1"),
        br(),
        plotlyOutput("graph2"),
        br(),
        plotlyOutput("graph3"),
        br(),
        plotlyOutput("graph4"),
        br(),
        plotlyOutput("graph5"),
        br(),
        # plotlyOutput("graph_swc"),
        # br(),
        br(), 
        downloadButton("downloadData", "Download Data"),
        h5(
          "App for flash drought visualisation, method and dataset comparison. Six methods are compared:"
        ),
        h6(
          "  * Ford, T. W. & Labosier, C. F. Meteorological conditions associated with the onset of flash drought in the Eastern United States, Agricultural and Forest Meteorology, Elsevier BV, 2017, 247, 414-423"
        ),
        h6(
          "  * Mo, K. C. & Lettenmaier, D. P. Precipitation Deficit Flash Droughts over the United States, Journal of Hydrometeorology, American Meteorological Society, 2016, 17, 1169-1184"
        ),
        h6(
          "  * Pendergrass, A. G. et al. Flash droughts present a new challenge for subseasonal-to-seasonal prediction, Nature Climate Change, Springer Science and Business Media LLC, 2020, 10, 191-199"
        ),
        h6(
          "  * Christian, J. I. et al. Flash drought development and cascading impacts associated with the 2010 Russian heatwave, Environmental Research Letters, IOP Publishing, 2020, 15, 094078"
        ),
        h6(
          "  * Noguera, I. et al. Characteristics and trends of flash droughts in Spain, 1961--2018, Annals of the New York Academy of Sciences, Wiley Online Library, 2020"
        ),
        h6(
          "  * Osman, M. et al. Flash drought onset over the contiguous United States: sensitivity of inventories and trends to quantitative definitions, Hydrology and Earth System Sciences, Copernicus GmbH, 2021, 25, 565-581"
        ),
        h6(
          "ERA5 and FLUXNET2015 data are used to assess the relevant variables (precipitation, temperature, soil water content, actual and potential evapotranspitation"
        ),
        h6(
          "by Pedro Alencar (ORCID: 0000-0001-6221-8580); Code available at https://github.com/pedroalencar1/FD-Viz"
        )
      )
    )
  ),
  ################## PAGE 2
  tabPanel(
    "Statistics",
    uiOutput("page2"),
    sidebarLayout(
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        HTML("<h3>Input parameters</h3>"),
        width = 3,
        selectInput(
          "Station",
          "Choose station (plot 1)",
          choices = data_stations$Station,
          selected = "AT-Neu"
        ),
        actionButton(
          inputId = "update31",
          label = "Update plot 1",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        br(),
        selectInput(
          "Land_Use",
          "Choose Land use (plot 2)",
          choices = sort(unique(data_stations$IGPB2)),
          selected = "Croplands"
        ),
        actionButton(
          inputId = "update32",
          label = "Update plot 2",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        br(),
        sliderInput(
          "range1",
          "Choose years (plot 3)",
          min = 1996,
          max = 2015,
          value = c(2010, 2011),
          step = 1
        ),
        actionButton(
          inputId = "update33",
          label = "Update plot 3",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        br(),
        h5("Metrics extracted from confusion matrix:"),
        img(
          src = "metrics.png",
          align = "center",
          width = "100%",
          contentType = "Images/png"
        ),
      ),
      mainPanel(
        width = 9,
        plotlyOutput("graph7"),
        br(),
        plotlyOutput("graph8"),
        br(),
        plotlyOutput("graph9"),
        br(),
        br(),
        h4(
          "* The reference method was the Ford and Labosier 2017 method. Only events during the growing season (MAMJJASO). were considered"
        ),
        br(),
        br(),
        h5(
          "App for flash drought visualisation, method and dataset comparison. Six methods are compared:"
        ),
        h6(
          "  * Ford, T. W. & Labosier, C. F. Meteorological conditions associated with the onset of flash drought in the Eastern United States, Agricultural and Forest Meteorology, Elsevier BV, 2017, 247, 414-423"
        ),
        h6(
          "  * Pendergrass, A. G. et al. Flash droughts present a new challenge for subseasonal-to-seasonal prediction, Nature Climate Change, Springer Science and Business Media LLC, 2020, 10, 191-199"
        ),
        h6(
          "  * Christian, J. I. et al. Flash drought development and cascading impacts associated with the 2010 Russian heatwave, Environmental Research Letters, IOP Publishing, 2020, 15, 094078"
        ),
        h6(
          "  * Noguera, I. et al. Characteristics and trends of flash droughts in Spain, 1961--2018, Annals of the New York Academy of Sciences, Wiley Online Library, 2020"
        ),
        h6(
          "  * Osman, M. et al. Flash drought onset over the contiguous United States: sensitivity of inventories and trends to quantitative definitions, Hydrology and Earth System Sciences, Copernicus GmbH, 2021, 25, 565-581"
        ),
        h6(
          "ERA5 and FLUXNET2015 data are used to assess the relevant variables (precipitation, temperature, soil water content, actual and potential evapotranspitation"
        ),
        h6(
          "by Pedro Alencar (ORCID: 0000-0001-6221-8580); Code available at https://github.com/pedroalencar1/FD-Viz"
        )
      )
    )
  ),
  # page 3
  tabPanel(
    "Summary",
    uiOutput("page3"),
    sidebarLayout(
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 3,
        HTML("<h3>Input parameters</h3>"),
        selectInput(
          "station_summary",
          "Choose station",
          choices = data_stations$Station,
          selected = "AT-Neu"
        ),
        actionButton(
          inputId = "update41",
          label = "Update plot",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        br(),
        br(),
        selectInput(
          "method_summary",
          "Choose Method",
          choices = models,
          selected = "Ford and Labosier"
        ),
        actionButton(
          inputId = "update42",
          label = "Update plot",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        br(),
        br(),
        selectInput(
          "landuse_summary",
          "Choose Land use",
          choices = sort(unique(data_stations$IGPB2)),
          selected = "Croplands"
        ),
        actionButton(
          inputId = "update43",
          label = "Update plot",
          icon("refresh"),
          width = "100%"
        ),
      ),
      mainPanel(
        width = 9,
        h4("Summary by station"),
        plotlyOutput("graph11"),
        br(),
        h4("Summary by Method"),
        plotlyOutput("graph21"),
        br(),
        plotlyOutput("graph22"),
        br(),
        h4("Summary by Land Use"),
        plotlyOutput("graph31"),
        br(),
        plotlyOutput("graph32"),
        br(),
        h4("Summary by method and landuse"),
        plotlyOutput("graph41"),
        br(),
        h4("Summary by method and station"),
        plotlyOutput("graph51"),
        br(),
        br(),
        # downloadButton('downloadData', 'Download Data'),
        h5(
          "App for visualisation of statistices from flash drought identification."
        ),
        h6(
          "  * Ford, T. W. & Labosier, C. F. Meteorological conditions associated with the onset of flash drought in the Eastern United States, Agricultural and Forest Meteorology, Elsevier BV, 2017, 247, 414-423"
        ),
        h6(
          "  * Mo, K. C. & Lettenmaier, D. P. Precipitation Deficit Flash Droughts over the United States, Journal of Hydrometeorology, American Meteorological Society, 2016, 17, 1169-1184"
        ),
        h6(
          "  * Pendergrass, A. G. et al. Flash droughts present a new challenge for subseasonal-to-seasonal prediction, Nature Climate Change, Springer Science and Business Media LLC, 2020, 10, 191-199"
        ),
        h6(
          "  * Christian, J. I. et al. Flash drought development and cascading impacts associated with the 2010 Russian heatwave, Environmental Research Letters, IOP Publishing, 2020, 15, 094078"
        ),
        h6(
          "  * Noguera, I. et al. Characteristics and trends of flash droughts in Spain, 1961--2018, Annals of the New York Academy of Sciences, Wiley Online Library, 2020"
        ),
        h6(
          "  * Osman, M. et al. Flash drought onset over the contiguous United States: sensitivity of inventories and trends to quantitative definitions, Hydrology and Earth System Sciences, Copernicus GmbH, 2021, 25, 565-581"
        ),
        h6(
          "ERA5 and FLUXNET2015 data are used to assess the relevant variables (precipitation, temperature, soil water content, actual and potential evapotranspitation"
        ),
        h6(
          "by Pedro Alencar (ORCID: 0000-0001-6221-8580); Code available at https://github.com/pedroalencar1/FD-Viz"
        )
      )
    )
  ),
  # page 4
  tabPanel(
    "Flash Drougth Identification",
    uiOutput("page4"),
    sidebarLayout(
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        HTML("<h3>Input parameters</h3>"),
        width = 3,
        selectInput(
          "file2",
          "Choose station and data",
          choices = stations,
          selected = "DE-Kli_FLUXNET2015"
        ),
        actionButton(
          inputId = "update21",
          label = "Update plot",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        selectInput(
          "Method21",
          "Choose Method",
          choices = models,
          selected = "Multi-criteria"
        ),
        actionButton(
          inputId = "update22",
          label = "Update plot",
          icon("refresh"),
          width = "100%"
        ),
        br(),
        leafletOutput("graph61"),
      ),
      mainPanel(
        width = 9,
        plotlyOutput("graph_st"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        plotlyOutput("graph_mt"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        plotlyOutput("graph_all"),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        # plotlyOutput('graph_swc'),
        h6("1 and 0 indicate true/false for a flash droght event. ")
      )
    )
  )
)


server <- function(input, output, session) {
  observeEvent(input$update1, {
    # inputs - PAGE 1
    # file1 = stations[1]
    
    file1 <- input$file
   
    complete_series <- read_fst(path = paste(file1, '.bin', sep = ''))

    
    range <- input$range # dates
    # range <- c(min(complete_series$Date), max(complete_series$Date))
    
    # update limits slider on the fly
    updateSliderInput(session, "range", min = min(as.Date(complete_series$Date)))
    updateSliderInput(session, "range", max = max(as.Date(complete_series$Date)))
    
    complete_series_select <-
      complete_series[(complete_series$Date >= range[1] &
                         complete_series$Date <= range[2]), ]
    
    data_station_select1 <-
      data_stations[which(substr(file1, start = 1, stop = 6) == data_stations$Station), ]
    data_station_select2 <-
      data_stations[which(substr(file1, start = 1, stop = 6) != data_stations$Station), ]
    
    Method_1 <- input$`Method 1`
    Method_2 <- input$`Method 2`
    
    # Method_1 <- 'Ford and Labosier'
    # Method_2 <- 'Multi-criteria'
    
    output$graph1 <- renderPlotly({
      plot_ly(complete_series_select, x = ~Date) %>%
        add_bars(
          y = ~precipitation,
          name = "Prec",
          yaxis = "y2",
          marker = list(color = "rgba(106,90,205,1"),
          width = 86400000
        ) %>%
        add_lines(
          y = ~temperature,
          name = "Temp",
          line = list(color = "#FC8D59")
        ) %>%
        add_bars(
          y = ~ get(Method_1),
          name = Method_1,
          yaxis = "y3",
          marker = list(color = "rgba(0,0,0,0.3"),
          width = 86400000
        ) %>%
        add_bars(
          y = ~ get(Method_2),
          name = Method_2,
          yaxis = "y3",
          marker = list(color = "rgba(255,0,0,0.3"),
          width = 86400000
        ) %>%
        layout(
          title = paste(file1, ": ", input$year, sep = ""),
          xaxis = list(
            title = "Date",
            domain = c(0, 0.95),
            type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "Temperature (ºC)",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            range = c(-10, 35),
            dtick = 10
          ),
          yaxis2 = list(
            title = "Precipitation (mm/d)",
            side = "right",
            color = "black",
            overlaying = "y",
            anchor = "free",
            position = 0.95,
            range = c(45, 0),
            dtick = 10
          ),
          yaxis3 = list(
            title = "",
            side = "right",
            color = "white",
            overlaying = "y",
            anchor = "free",
            position = 1,
            range = c(0, 1),
            dtick = 10
          ),
          showlegend = T
        )
    })
    
    output$graph2 <- renderPlotly({
      plot_ly(complete_series_select, x = ~Date) %>%
        add_bars(
          y = ~precipitation,
          name = "Prec",
          yaxis = "y2",
          marker = list(color = "rgba(106,90,205,1"),
          width = 86400000
        ) %>%
        add_lines(
          y = ~et0,
          name = "ET0",
          line = list(color = "#FC8D59")
        ) %>%
        add_lines(
          y = ~eta,
          name = "ETa",
          line = list(color = "#91CF60")
        ) %>%
        add_bars(
          y = ~ get(Method_1),
          name = Method_1,
          yaxis = "y3",
          marker = list(color = "rgba(0,0,0,0.3"),
          width = 86400000
        ) %>%
        add_bars(
          y = ~ get(Method_2),
          name = Method_2,
          yaxis = "y3",
          marker = list(color = "rgba(255,0,0,0.3"),
          width = 86400000
        ) %>%
        layout(
          title = paste(file1, ": ", input$year, sep = ""),
          xaxis = list(
            title = "Date",
            domain = c(0, 0.95),
            type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "Evapotranspiration (mm/d)",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            range = c(0, 6.75),
            dtick = 1.5
          ),
          yaxis2 = list(
            title = "Precipitation (mm/d)",
            side = "right",
            color = "black",
            overlaying = "y",
            anchor = "free",
            position = 0.95,
            range = c(45, 0),
            dtick = 10
          ),
          yaxis3 = list(
            title = "",
            side = "right",
            color = "white",
            overlaying = "y",
            anchor = "free",
            position = 1,
            range = c(0, 1),
            dtick = 10
          ),
          showlegend = T
        )
    })
    
    output$graph3 <- renderPlotly({
      plot_ly(complete_series_select, x = ~Date) %>%
        add_bars(
          y = ~precipitation,
          name = "Prec",
          yaxis = "y2",
          marker = list(color = "rgba(106,90,205,1"),
          width = 86400000
        ) %>%
        add_lines(
          y = ~swc,
          name = "SWC",
          line = list(color = "#CC6666")
        ) %>%
        add_lines(
          y = ~swc_p20,
          name = "SWC (20p)",
          line = list(color = "rgba(169,169,169,1", width = 1)
        ) %>%
        add_lines(
          y = ~swc_p50,
          name = "SWC (50p)",
          line = list(
            color = "rgba(0,0,0,1",
            dash = "dot",
            width = 1
          )
        ) %>%
        add_bars(
          y = ~ get(Method_1),
          name = Method_1,
          yaxis = "y3",
          marker = list(color = "rgba(0,0,0,0.3"),
          width = 86400000
        ) %>%
        add_bars(
          y = ~ get(Method_2),
          name = Method_2,
          yaxis = "y3",
          marker = list(color = "rgba(255,0,0,0.3"),
          width = 86400000
        ) %>%
        layout(
          title = paste(file1, ": ", input$year, sep = ""),
          xaxis = list(
            title = "Date",
            domain = c(0, 0.95),
            type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "Soil Water Content (%)",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            range = c(0, 45),
            dtick = 10
          ),
          yaxis2 = list(
            title = "Precipitation (mm/d)",
            side = "right",
            color = "black",
            overlaying = "y",
            anchor = "free",
            position = 0.95,
            range = c(45, 0),
            dtick = 10
          ),
          yaxis3 = list(
            title = "",
            side = "right",
            color = "white",
            overlaying = "y",
            anchor = "free",
            position = 1,
            range = c(0, 1),
            dtick = 10
          ),
          showlegend = T
        )
    })
    
    output$graph4 <- renderPlotly({
      plot_ly(complete_series_select, x = ~Date) %>%
        add_lines(
          y = ~spei,
          name = "SPEI",
          line = list(color = user_palette3[2])
        ) %>%
        add_lines(
          y = ~sesr,
          name = "SESR",
          line = list(color = user_palette3[4]),
          visible = "legendonly"
        ) %>%
        add_lines(
          y = ~eddi,
          name = "EDDI",
          yaxis = "y2",
          line = list(color = user_palette3[6]),
          visible = "legendonly"
        ) %>%
        add_lines(
          y = ~score*100,
          name = "Score",
          yaxis = "y2",
          line = list(color = user_palette3[9]),
          visible = "legendonly"
        ) %>%
        add_bars(
          y = ~ get(Method_1),
          name = Method_1,
          yaxis = "y3",
          marker = list(color = "rgba(0,0,0,0.3"),
          width = 86400000
        ) %>%
        add_bars(
          y = ~ get(Method_2),
          name = Method_2,
          yaxis = "y3",
          marker = list(color = "rgba(255,0,0,0.3"),
          width = 86400000
        ) %>%
        layout(
          title = paste(file1, ": ", input$year, sep = ""),
          xaxis = list(
            title = "Date",
            domain = c(0, 0.95),
            type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "SPEI and SESR",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            range = c(-3, 3),
            dtick = 1
          ),
          yaxis2 = list(
            title = "EDDI and Score",
            side = "right",
            color = "black",
            overlaying = "y",
            anchor = "free",
            position = 0.95,
            range = c(0, 120),
            dtick = 20
          ),
          yaxis3 = list(
            title = "",
            side = "right",
            color = "white",
            overlaying = "y",
            anchor = "free",
            position = 1,
            range = c(0, 1),
            dtick = 10
          ),
          showlegend = T
        )
    })
    
    output$graph5 <- renderPlotly({
      plot_ly(complete_series_select, x = ~Date) %>%
        add_lines(
          y = ~et0_anomaly,
          name = "ET0 anom.",
          line = list(color = user_palette3[2])
        ) %>%
        add_lines(
          y = ~eta_anomaly,
          name = "ETa anom.",
          line = list(color = user_palette3[4])
        ) %>%
        add_lines(
          y = ~precipitation_anomaly,
          name = "Prec anom.",
          line = list(color = user_palette3[6]),
          visible = "legendonly"
        ) %>%
        add_lines(
          y = ~temperature_anomaly,
          name = "Temp anom.",
          line = list(color = user_palette3[9]),
          visible = "legendonly"
        ) %>%
        add_bars(
          y = ~ get(Method_1),
          name = Method_1,
          yaxis = "y2",
          marker = list(color = "rgba(0,0,0,0.3"),
          width = 86400000
        ) %>%
        add_bars(
          y = ~ get(Method_2),
          name = Method_2,
          yaxis = "y2",
          marker = list(color = "rgba(255,0,0,0.3"),
          width = 86400000
        ) %>%
        layout(
          title = paste(file1, ": ", input$year, sep = ""),
          xaxis = list(
            title = "Date",
            domain = c(0, 0.95),
            type = "date",
            tickmode = "auto",
            nticks = 20,
            dtick = "M1",
            ticks = "outside"
          ),
          yaxis = list(
            title = "Anomaly (-)",
            side = "left",
            color = "black",
            position = 0,
            anchor = "free",
            range = c(-3, 3),
            dtick = 1
          ),
          yaxis2 = list(
            title = "",
            side = "right",
            color = "white",
            overlaying = "y",
            anchor = "free",
            position = 1,
            range = c(0, 1),
            dtick = 10
          ),
          showlegend = T
        )
    })
    
    #### Map
    output$graph6 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = data_station_select1,
          lat = ~Latitude,
          lng = ~Longitude,
          radius = ~ Duration / 1.5,
          opacity = 1,
          fillOpacity = 1,
          color = c("black"),
          weight = 2,
          popup = paste(
            data_station_select1$Station,
            "<br>",
            data_station_select1$IGPB2,
            "<br>",
            'Climate: ',data_station_select2$climate
          ),
          fillColor = ~color1
        ) %>%
        addCircleMarkers(
          data = data_station_select2,
          lat = ~Latitude,
          lng = ~Longitude,
          radius = ~ Duration / 1.5,
          opacity = 0,
          fillOpacity = 0.5,
          popup = paste(
            data_station_select2$Station,
            "<br>",
            data_station_select2$IGPB2,
            "<br>",
            'Climate: ',data_station_select2$climate
          ),
          fillColor = ~color1
        ) %>%
        addCircleMarkers(
          data = data_stations,
          lat = ~Latitude,
          lng = ~Longitude,
          radius = 1,
          opacity = 0,
          fillOpacity = 1,
          popup = paste(
            data_stations$Station,
            "<br>",
            data_stations$IGPB2,
            "<br>",
            'Climate: ',data_stations$climate
          ),
          fillColor = "black"
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors = unique(data_stations$color1),
          labels = unique(data_stations$IGPB),
          opacity = 1,
          title = "Land Use"
        )
    })
    
    #### Button
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", file1, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(complete_series_select, file)
      }
    )
  })
  
  
  #### PAGE 2
  #### FD co-identification visualisaion
  
  observeEvent(input$update21, {
    file2 <- input$file2
    
    # file2 = stations[1]

    df <- read_fst(paste(file2, ".bin", sep = ""))[, c(1, 3:7, 10)]

    df_long <-
      pivot_longer(df,
                   cols = 2:7,
                   names_to = "method",
                   values_to = "fd"
      )
    df_long$Date <- as.Date(df_long$Date)
    
    my_breaks <-
      as.Date(seq(as.Date(min(df_long$Date)), as.Date(max(df_long$Date)),
                  by = "6 months"
      ))
    
    df_long$fd[is.na(df_long$fd)] <- 0
    df_long$fd <- as.numeric(as.character(df_long$fd))
    df_long$order <- rep(c(1,6,5,4,2,3), nrow(df_long) / 6)
    
    p_station <-
      ggplot(df_long, aes(
        x = Date,
        y = reorder(method, -order),
        fill = fd
      )) +
      geom_tile() +
      scale_x_date(breaks = my_breaks, expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      scale_fill_continuous(
        breaks = c(0, 1),
        low = "#F0F0F0",
        high = "#DE0000",
        name = "Flash Drought"
      ) +
      ggtitle(paste("Flash Drought identification by all methods in ", substr(file2, start = 1, stop = 6), sep = "")) +
      xlab("Date") +
      ylab("Method") +
      theme(
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(angle = 45),
        legend.position = "bottom",
      )
    
    
    
    output$graph_st <- renderPlotly({
      ggplotly(p_station, height = 600)
    })
    # })
    
    
    ## MAP
    
    data_station_select1 <-
      data_stations[which(substr(file2, start = 1, stop = 6) == data_stations$Station), ]
    data_station_select2 <-
      data_stations[which(substr(file2, start = 1, stop = 6) != data_stations$Station), ]
    
    output$graph61 <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircleMarkers(
          data = data_station_select1,
          lat = ~Latitude,
          lng = ~Longitude,
          radius = ~ Duration / 1.5,
          opacity = 1,
          fillOpacity = 1,
          color = c("black"),
          weight = 2,
          popup = paste(
            data_station_select1$Station,
            "<br>",
            data_station_select1$IGPB2,
            "<br>",
            'Climate: ',data_station_select1$climate
          ),
          fillColor = ~color1
        ) %>%
        addCircleMarkers(
          data = data_station_select2,
          lat = ~Latitude,
          lng = ~Longitude,
          radius = ~ Duration / 1.5,
          opacity = 0,
          fillOpacity = 0.5,
          popup = paste(
            data_station_select2$Station,
            "<br>",
            data_station_select2$IGPB2,
            "<br>",
            'Climate: ',data_station_select2$climate
          ),
          fillColor = ~color1
        ) %>%
        addCircleMarkers(
          data = data_stations,
          lat = ~Latitude,
          lng = ~Longitude,
          radius = 1,
          opacity = 0,
          fillOpacity = 1,
          popup = paste(
            data_stations$Station,
            "<br>",
            data_stations$IGPB2,
            "<br>",
            'Climate: ',data_stations$climate
          ),
          fillColor = "black"
        ) %>%
        leaflet::addLegend(
          position = "bottomright",
          colors = unique(data_stations$color1),
          labels = unique(data_stations$IGPB),
          opacity = 1,
          title = "Land Use"
        )
    })
  })
  
  observeEvent(input$update22, {
    method1 <- input$Method21
    # method1<- 'Multi-criteria'
    
    all_events_long_selec <-
      all_events_long[all_events_long$method == method1, ]
    
    p_method <-
      ggplot(all_events_long_selec, aes(
        x = Date,
        y = reorder(Station, latitude),
        fill = fd
      )) +
      geom_tile() +
      scale_x_date(breaks = my_breaks, expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      scale_fill_continuous(
        breaks = c(0, 1),
        na.value = "#FFFFFF",
        low = "#F0F0F0",
        high = "#DE0000",
        name = "Flash Drought"
      ) +
      ggtitle(paste("Flash Drought identification by ", method1, sep = "")) +
      xlab("Date") +
      ylab("Station") +
      theme(
        axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white")
      )
    
    output$graph_mt <- renderPlotly({
      ggplotly(p_method, height = 600)
    })
  })
  

  p_all <-
    ggplot(melt_events, aes(
      x = Date,
      y = reorder(name, latitude),
      fill = total
    )) +
    geom_tile() +
    scale_fill_continuous(
      na.value = "#EAEAEA",
      low = "#FFFFFF",
      high = "#DE0000",
      name = "Co-identification"
    ) +
    scale_x_date(breaks = my_breaks, expand = c(0, 0)) +
    ggtitle("Flash Drought identification aggremment among 6 methods") +
    xlab("Date") +
    ylab("Station") +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "bottom",
    )
  output$graph_all <- renderPlotly({
    ggplotly(p_all, height = 600)
  })


  #### inputs PAGE 3
  #### Statistics - Confusion matrix
  observeEvent(input$update31, {
    selec1 <- stats[stats$Station == input$Station, ]
    # selec1 <- stats[stats$Station == 'BE-Lon',]
    # selec1 <- selec1[lubridate::month(selec1$Date) %in% 3:10, ]
    order <- sort(rep(1:6, 5))
    
    conf_mat1 <-
      conf_matrix(selec1[,3:8], selec1$`Osman et al.`) %>%
      subset(.$model %notin% c("Osman et al.")) %>%
      cbind(order)
    
    # conf_mat1$value[conf_mat1$value < 0] <- 0 #remove some negative values
    
    p7 <- ggplot(conf_mat1, aes(
      x = fct_reorder(model, order),
      y = value,
      fill = metric
    )) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(padding = 0),
        alpha = 0.8,
        width = 0.8
      ) +
      scale_fill_manual(
        name = "Metric",
        values = user_palette3[c(3:8)]
      ) +
      # coord_polar()+
      ggtitle(paste(
        "Comparison of FD identification* for the station ",
        # input$Station,
        sep = ""
      )) +
      labs(x = "", "") +
      theme( # axis.text.x = element_text(angle = 360/(2*pi)*rev( seq( pi/6, 2*pi-pi/6, len=6)))
        axis.text.x = element_text(angle = 45)
      )
    
    output$graph7 <- renderPlotly({
      ggplotly(p7)
    })
  })
  
  
  observeEvent(input$update32, {
    selec2 <- stats[stats$`Land Use` == input$Land_Use, ]
    # selec2 <- stats[stats$`Land Use` == 'Grasslands',]
    # selec2 <- selec2[lubridate::month(selec2$Date) %in% 3:10, ]
    order <- sort(rep(1:6, 5))
    
    conf_mat2 <-
      conf_matrix(selec2[,3:8], selec2$`Osman et al.`) %>%
      subset(.$model %notin% c("Osman et al.")) %>%
      cbind(order)
    
    # conf_mat2$value[conf_mat2$value < 0] <- 0 #remove some negative values
    
    
    p8 <- ggplot(conf_mat2, aes(
      x = fct_reorder(
        model,
        order
      ),
      y = value,
      fill = metric
    )) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(padding = 0),
        alpha = 0.8,
        width = 0.8
      ) +
      scale_fill_manual(
        name = "Metric",
        # values = user_palette2[c(1, 2, 3, 9, 10, 11)]
        values = user_palette3[c(3:8)]
      ) +
      # coord_polar()+
      ggtitle(
        paste(
          "2) Comparison of FD identification* for all stations with ",
          input$Land_Use,
          sep = ""
        )
      ) +
      labs(x = "", "") +
      theme( # axis.text.x = element_text(angle = 360/(2*pi)*rev( seq( pi/6, 2*pi-pi/6, len=6)))
        axis.text.x = element_text(angle = 45)
      )
    
    output$graph8 <- renderPlotly({
      ggplotly(p8)
    })
  })
  
  
  observeEvent(input$update33, {
    range1 <- input$range1 # years
    selec3 <-
      stats[stats$year >= range1[1] & stats$year < range1[2], ]
    # selec3 <- selec3[lubridate::month(selec3$Date) %in% 3:10, ]
    order <- sort(rep(1:6, 5))
    
    conf_mat3 <-
      conf_matrix(selec3[,3:8], selec3$`Osman et al.`) %>%
      subset(.$model %notin% c("Osman et al.")) %>%
      cbind(order)
    
    p9 <- ggplot(conf_mat3, aes(
      x = fct_reorder(
        model,
        order
      ),
      y = value,
      fill = metric
    )) +
      geom_bar(
        stat = "identity",
        position = position_dodge2(padding = 0),
        alpha = 0.8,
        width = 0.8
      ) +
      scale_fill_manual(
        name = "Metric",
        # values = user_palette2[c(1, 2, 3, 9, 10, 11)]
        values = user_palette3[c(3:8)]
      ) +
      # coord_polar()+
      ggtitle(
        paste(
          "Comparison of FD identification* for all stations from",
          range1[1],
          "to",
          range1[2],
          sep = " "
        )
      ) +
      labs(x = "", "") +
      theme( # axis.text.x = element_text(angle = 360/(2*pi)*rev( seq( pi/6, 2*pi-pi/6, len=6)))
        axis.text.x = element_text(angle = 0)
      )
    
    output$graph9 <- renderPlotly({
      ggplotly(p9)
    })
  })
  
  ######
  # PAGE 4

  #### Summaries by station
  observeEvent(input$update41, {
    station_sum <- input$station_summary
    event_sum <-
      events_month[events_month$station == station_sum, ]
    num_years <-
      data_stations$Duration[which(data_stations$Station == station_sum)]

    output$graph11 <- renderPlotly(
      plot_ly(
        event_sum,
        type = "bar",
        colors = rev(user_palette3)
      ) %>%
        # add_trace(
        #   x = ~ month,
        #   y = ~ `Mo and Lettenmeier`,
        #   name = 'Mo and Lettenmeier',
        #   color = user_palette[1]
        # ) %>%
        add_trace(
          x = ~month,
          y = ~`Ford and Labosier`,
          name = "Ford and Labosier",
          color = user_palette3[2]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Pendergrass et al.`,
          name = "Pendergrass et al.",
          color = user_palette3[3]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Noguera et al.`,
          name = "Noguera et al.",
          color = user_palette3[4]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Christian et al.`,
          name = "Christian et al.",
          color = user_palette3[5]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Osman et al.`,
          name = "Osman et al.",
          color = user_palette3[6]
        ) %>%
        # add_trace(
        #   x = ~month,
        #   y = ~`Alencar et al.`,
        #   name = "Alencar et al.",
        #   color = user_palette3[7]
        # ) %>%
        # add_trace(
        #   x = ~ month,
        #   y = ~
        #     `Mod. Ford and Labosier`,
        #   name = 'Mod. Ford and Labosier',
        #   color = user_palette[8]
        # ) %>%
        add_trace(
          x = ~month,
          y = ~`Multi-criteria`,
          name = "Multi-criteria",
          color = user_palette3[9]
        ) %>%
        layout(
          title = paste(
            "Number of events by each method per month at station ",
            station_sum,
            " (Duration: ",
            num_years,
            " years)",
            sep = ""
          ),
          yaxis = list(title = "Number of events"),
          xaxis = list(title = "Month"),
          barmode = "stack",
          plot_bgcolor = "#F0F0F0"
        )
    )
  })

  #### Summaries by method
  observeEvent(input$update42, {
    method_sum <- input$method_summary # read input
    event_sum <- events_month %>% # select df based on input
      group_by(station) %>%
      summarise_all(funs(mean))
    col_id <- which(method_sum == colnames(event_sum))
    event_sum <- event_sum[, c(1, col_id)]
    colnames(event_sum) <- c("station", "method")
    event_sum$mean <- mean(event_sum$method)

    output$graph21 <- renderPlotly(
      plot_ly(event_sum,
              type = "bar",
              colors = user_palette3[col_id - 2]
      ) %>%
        add_trace(
          x = ~station,
          y = ~method,
          name = method_sum,
          color = user_palette3[col_id - 2]
        ) %>%
        add_lines(
          x = ~station,
          y = ~mean,
          line = list(
            color = "rgb(240, 12, 24)",
            width = 3,
            dash = "solid"
          ),
          inherit = F,
          name = "Mean"
        ) %>%
        layout(
          title = paste("Average number of events per year by method ",
                        method_sum,
                        sep = ""
          ),
          yaxis = list(title = "Number of events"),
          xaxis = list(title = "Month"),
          barmode = "stack",
          plot_bgcolor = "#F0F0F0"
        )
    )
  })

  output$graph22 <- renderPlotly(
    plot_ly(
      totals,
      type = "scatter",
      mode = "line+marker",
      colors = rev(user_palette3)
    ) %>%
      add_trace(
        x = ~Station,
        y = ~ `Multi-criteria` / Duration,
        name = "Multi-criteria",
        color = user_palette3[9]
      ) %>%
      # add_trace(
      #   x = ~ Station,
      #   y = ~ `Mod. Ford and Labosier`,
      #   name = 'Mod. Ford and Labosier',
      #   color = user_palette[8]
      # ) %>%
      # add_trace(
      #   x = ~ Station,
      #   y = ~ `Alencar et al.`,
      #   name = 'Alencar et al.',
      #   color = user_palette3[7]
    # ) %>%
    add_trace(
      x = ~Station,
      y = ~ `Osman et al.` / Duration,
      name = "Osman et al.",
      color = user_palette3[6]
    ) %>%
      add_trace(
        x = ~Station,
        y = ~ `Christian et al.` / Duration,
        name = "Christian et al.",
        color = user_palette3[5]
      ) %>%
      add_trace(
        x = ~Station,
        y = ~ `Noguera et al.` / Duration,
        name = "Noguera et al.",
        color = user_palette3[4]
      ) %>%
      add_trace(
        x = ~Station,
        y = ~ `Pendergrass et al.` / Duration,
        name = "Pendergrass et al.",
        color = user_palette3[3]
      ) %>%
      add_trace(
        x = ~Station,
        y = ~ `Ford and Labosier` / Duration,
        name = "Ford and Labosier",
        color = user_palette3[2]
      ) %>%
      # add_trace(
      #   x = ~ Station,
      #   y = ~ `Mo and Lettenmeier`,
      #   name = 'Mo and Lettenmeier',
      #   color = user_palette[1]
      # ) %>%
      layout(
        title = "Number of events by each method at each station",
        yaxis = list(title = "Number of events per year"),
        xaxis = list(title = "Station"),
        # barmode = 'stack',
        plot_bgcolor = "#F0F0F0"
      )
  )


  #### Summaries by land use

  observeEvent(input$update43, {
    landuse_sum <- input$landuse_summary
    event_sum <-
      events_month[events_month$IGPB2 == landuse_sum, ][, c(2:11)]
    event_sum <- event_sum %>% # select df based on input
      group_by(month) %>%
      summarise_all(funs(sum))

    num_years <-
      sum(data_stations$Duration[which(data_stations$IGPB2 == landuse_sum)])

    output$graph31 <- renderPlotly(
      plot_ly(
        event_sum,
        type = "bar",
        colors = rev(user_palette3)
      ) %>%
        # add_trace(
        #   x = ~ month,
        #   y = ~ `Mo and Lettenmeier`,
        #   name = 'Mo and Lettenmeier',
        #   color = user_palette[1]
        # ) %>%
        add_trace(
          x = ~month,
          y = ~`Ford and Labosier`,
          name = "Ford and Labosier",
          color = user_palette3[2]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Pendergrass et al.`,
          name = "Pendergrass et al.",
          color = user_palette3[3]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Noguera et al.`,
          name = "Noguera et al.",
          color = user_palette3[4]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Christian et al.`,
          name = "Christian et al.",
          color = user_palette3[5]
        ) %>%
        add_trace(
          x = ~month,
          y = ~`Osman et al.`,
          name = "Osman et al.",
          color = user_palette3[6]
        ) %>%
        # add_trace(
        #   x = ~month,
        #   y = ~`Alencar et al.`,
        #   name = "Alencar et al.",
        #   color = user_palette3[7]
        # ) %>%
        # add_trace(
        #   x = ~ month,
        #   y = ~ `Mod. Ford and Labosier`,
        #   name = 'Mod. Ford and Labosier',
        #   color = user_palette[8]
        # ) %>%
        add_trace(
          x = ~month,
          y = ~`Multi-criteria`,
          name = "Multi-criteria",
          color = user_palette3[9]
        ) %>%
        layout(
          title = paste(
            "Number of events identified by each method per month in station with\n",
            landuse_sum,
            ". Total number of years (sum for all stations with this land use): ",
            num_years,
            sep = ""
          ),
          yaxis = list(title = "Number of events", domain = c(0, 0.95)),
          xaxis = list(title = "Month"),
          legend = list(x = 1, y = 0.95),
          barmode = "stack",
          plot_bgcolor = "#F0F0F0"
        )
    )
  })

  total_lu <- totals[2:9] %>%
    group_by(landuse) %>%
    summarise_all(funs(sum))

  total_lu[, 2:8] <- total_lu[, 2:8] / total_lu$Duration
  
  output$graph32 <- renderPlotly(
    plot_ly(
      total_lu,
      type = "scatter",
      mode = "line",
      colors = rev(user_palette3)
    ) %>%
      add_trace(
        x = ~landuse,
        y = ~`Multi-criteria`,
        name = "Multi-criteria",
        color = user_palette3[9]
      ) %>%
      # add_trace(
      #   x = ~ landuse,
      #   y = ~ `Mod. Ford and Labosier`,
      #   name = 'Mod. Ford and Labosier',
      #   color = user_palette[8]
      # ) %>%
      # add_trace(
      #   x = ~landuse,
      #   y = ~`Alencar et al.`,
      #   name = "Alencar et al.",
      #   color = user_palette3[7]
      # ) %>%
      add_trace(
        x = ~landuse,
        y = ~`Osman et al.`,
        name = "Osman et al.",
        color = user_palette3[6]
      ) %>%
      add_trace(
        x = ~landuse,
        y = ~`Christian et al.`,
        name = "Christian et al.",
        color = user_palette3[5]
      ) %>%
      add_trace(
        x = ~landuse,
        y = ~`Noguera et al.`,
        name = "Noguera et al.",
        color = user_palette3[4]
      ) %>%
      add_trace(
        x = ~landuse,
        y = ~`Pendergrass et al.`,
        name = "Pendergrass et al.",
        color = user_palette3[3]
      ) %>%
      add_trace(
        x = ~landuse,
        y = ~`Ford and Labosier`,
        name = "Ford and Labosier",
        color = user_palette3[2]
      ) %>%
      # add_trace(
      #   x = ~ landuse,
      #   y = ~ `Mo and Lettenmeier`,
      #   name = 'Mo and Lettenmeier',
      #   color = user_palette[1]
      # ) %>%
      layout(
        title = "Number of events by each method at each land use",
        yaxis = list(title = "Number of events"),
        xaxis = list(title = "Station"),
        # barmode = 'stack',
        plot_bgcolor = "#F0F0F0"
      )
  )


  output$graph41 <- renderPlotly(
    plot_ly(relative_counting, type = "bar", colors = user_palette3) %>%
      add_trace(
        x = ~method,
        y = ~Grasslands,
        name = "Grasslands",
        color = user_palette3[3]
      ) %>%
      add_trace(
        x = ~method,
        y = ~Croplands,
        name = "Croplands",
        color = user_palette3[4]
      ) %>%
      add_trace(
        x = ~method,
        y = ~`Mixed forests`,
        name = "Mixed forests",
        color = user_palette3[5]
      ) %>%
      add_trace(
        x = ~method,
        y = ~`Deciduous broadleaf forests`,
        name = "Deciduous broadleaf forests",
        color = user_palette3[6]
      ) %>%
      add_trace(
        x = ~method,
        y = ~`Evergreen needleleaf forests`,
        name = "Evergreen needleleaf forests",
        color = user_palette3[7]
      ) %>%
      add_trace(
        x = ~method,
        y = ~`Evergreen broadleaf forests`,
        name = "Evergreen broadleaf forests",
        color = user_palette3[8]
      ) %>%
      add_trace(
        x = ~method,
        y = ~`Closed shrublands`,
        name = "Closed shrublands",
        color = user_palette3[9]
      ) %>%
      layout(
        title = "Proportion of months with FD event by model and Land Use ",
        yaxis = list(title = "Number of events"),
        xaxis = list(title = "Month"),
        barmode = "stack",
        plot_bgcolor = "#F0F0F0"
      )
  )

  summary_events <-
    ggplot(
      all_events_summary[all_events_summary$method %notin% c("Alencar et al.", "Mo and Lettenmeier", "Mod. Ford and Labosier"), ],
      aes(
        x = reorder(station, -latitude),
        y = method,
        fill = event_year
      )
    ) +
    geom_tile() +
    scale_fill_continuous(
      na.value = "#FFFFFF",
      # low = '#FDD49E',
      # high = '#7F0000',
      low = "#feead0",
      high = "#C3150E",
      name = "Events per year"
    ) +
    xlab("Station") +
    ggtitle("Number of events per year by method and station") +
    theme(
      axis.text.x = element_text(angle = 90),
      axis.title.y = element_blank(),
      panel.background = element_rect(fill = "white")
    )

  output$graph51 <- renderPlotly(ggplotly(summary_events))
}

options(shiny.fullstacktrace = TRUE)
shinyApp(ui, server)
