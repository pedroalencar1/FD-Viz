library(shiny)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(shinythemes)
library(ggplotlyExtra)
library(plotly)
library(leaflet)
library(maps)
library(rworldmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
# devtools::install_github('pedroalencar1/fdClassify')
# library(fdClassify)

#
# Data Preparation Steps

models <- c("Mo and Lettenmeier","Ford and Labosier", "Pendergrass et al.","Noguera et al." ,
            "Christian et al.", "Osman et al.")
stations <- list.files()
stations <- stations[substr(stations, start = 7, stop = 7) == '_']
stations <- substr(stations,1,nchar(stations)-4)

data_stations <- read.delim('stations.txt')
user_palette <- brewer.pal(n = 9, name = 'PuBuGn')


ui <- fluidPage(
    
    # App title ----
    titlePanel('Flash Drought - Interactive Visualisation'),
    
    sidebarLayout(
        # Sidebar panel for inputs ----
        sidebarPanel(
            HTML("<h3>Input parameters</h3>"),
            selectInput("file", "Choose station and data", choices = stations, selected = 'DE-Kli_FLUXNET2015'),
            # textOutput("file_name"),
            selectInput("Method 1", "Choose Method 1", choices = models, selected = 'Ford and Labosier'),
            selectInput("Method 2", "Choose Method 2", choices = models, selected = 'Noguera et al.'),
            # sliderInput("year","Choose year",min=1994, max=2014,value=2010,step = 1),
            # sliderInput("range","Choose years",min=1994, max=2014,value=c(2006,2007),step = 1),
            sliderInput("range","Choose years",min=as.Date('1994-01-01'), max= as.Date('2014-12-31'),
                        value=c(as.Date('2006-01-01'),as.Date('2006-12-31'))),
            width=3,
            submitButton("Update plots", icon('refresh'), width = '100%'),
            br(),
            leafletOutput("graph6"),
            
        ),
        mainPanel(
            width = 9,
            plotlyOutput("graph1"),
            plotlyOutput("graph2"),
            plotlyOutput("graph3"),
            plotlyOutput("graph4"),
            plotlyOutput("graph5"),
            br(),
            br(),
            downloadButton('downloadData','Download Data'),
            # leafletOutput("graph6"),
            h5("App for flash drought visualisation, method and dataset comparison. Six methods are compared:"),
            h6('  * Ford, T. W. & Labosier, C. F. Meteorological conditions associated with the onset of flash drought in the Eastern United States, Agricultural and Forest Meteorology, Elsevier BV, 2017, 247, 414-423'),
            h6('  * Mo, K. C. & Lettenmaier, D. P. Precipitation Deficit Flash Droughts over the United States, Journal of Hydrometeorology, American Meteorological Society, 2016, 17, 1169-1184'),
            h6('  * Pendergrass, A. G. et al. Flash droughts present a new challenge for subseasonal-to-seasonal prediction, Nature Climate Change, Springer Science and Business Media LLC, 2020, 10, 191-199'),
            h6('  * Christian, J. I. et al. Flash drought development and cascading impacts associated with the 2010 Russian heatwave, Environmental Research Letters, IOP Publishing, 2020, 15, 094078'),
            h6('  * Noguera, I. et al. Characteristics and trends of flash droughts in Spain, 1961--2018, Annals of the New York Academy of Sciences, Wiley Online Library, 2020'),
            h6('  * Osman, M. et al. Flash drought onset over the contiguous United States: sensitivity of inventories and trends to quantitative definitions, Hydrology and Earth System Sciences, Copernicus GmbH, 2021, 25, 565-581'),
            h6('ERA5 and FLUXNET2015 data are used to assess the relevant variables (precipitation, temperature, soil water content, actual and potential evapotranspitation'),
            h6('by Pedro Alencar (ORCID: 0000-0001-6221-8580); Code available at https://github.com/pedroalencar1/FD-Viz')
            
        )
    )
)


server <- function(input, output, session){
    
    observe({
        file1 = input$file
        data1 = read.csv(paste(file1, '.csv', sep = ''))
        # name_file = unlist(strsplit(file1,'.csv'))
        complete_series <- data1[,-c(1)]
        colnames(complete_series) <- c("Date","Mo and Lettenmeier","Ford and Labosier", 
                                       "Pendergrass et al.","Noguera et al." ,
                                       "Christian et al.", "Osman et al.","et0_anomaly",
                                       "eta_anomaly","temperature_anomaly",
                                       "precipitation_anomaly", "swc",
                                       "et0", "eta", "temperature", "precipitation",
                                       "swc_p20" ,"swc_p40", "swc_p50","spei","sesr", "eddi")
        
        range = input$range
        
        #update limits slider on the fly
        updateSliderInput(session, "range", min=min(as.Date(complete_series$Date)))
        updateSliderInput(session, "range", max=max(as.Date(complete_series$Date)))
        
        complete_series_select <- complete_series[(complete_series$Date >= range[1] & complete_series$Date <= range[2]),] 
        # updateSliderInput(session, "range", min=min(year(complete_series$Date)))
        # updateSliderInput(session, "range", max=max(year(complete_series$Date))+1)
        # 
        # complete_series_select <- complete_series[(year(complete_series$Date) >= range[1] & year(complete_series$Date) < range[2]),] 
        
        
        data_station_select1 <- data_stations[which(substr(file1,start = 1, stop = 6) == data_stations$Station),]
        data_station_select2 <- data_stations[which(substr(file1,start = 1, stop = 6) != data_stations$Station),]
        
        output$graph1 <- renderPlotly({
            
            plot_ly(complete_series_select, x = ~Date) %>%
                add_bars(y = ~precipitation, name='Prec',yaxis = 'y2', marker = list(color = 'rgba(106,90,205,1'), width = 1) %>%
                add_lines(y = ~temperature, name='Temp', line = list(color = '#FC8D59')) %>%
                add_bars(y = ~get(input$`Method 1`), name=input$`Method 1`,yaxis = 'y3', marker = list(color = 'rgba(0,0,0,0.3'), width = 1) %>%
                add_bars(y = ~get(input$`Method 2`), name=input$`Method 2`,yaxis = 'y3', marker = list(color = 'rgba(255,0,0,0.3'), width = 1) %>%
                
                layout(
                    title=paste(file1, ': ',input$year, sep = ''),
                    xaxis = list(title = "Date", domain = c(0,0.95), tickmode = 'auto'),
                    yaxis = list(title = 'Temperature (ºC)', side = "left", color = "black", position = 0,
                                 anchor = 'free', range = c(-10,35), dtick = 10),
                    yaxis2 = list(title = 'Precipitation (mm/d)', side = "right", color = "black",
                                  overlaying = "y", anchor = 'free', position = 0.95,
                                  range = c(45,0), dtick = 10),
                    yaxis3 = list(title = '', side = "right", color = "white",
                                  overlaying = "y", anchor = 'free', position = 1,
                                  range = c(0,1), dtick = 10),
                    showlegend = T
                )
        })
        
        output$graph2 <- renderPlotly({
            
            plot_ly(complete_series_select, x = ~Date) %>%
                add_bars(y = ~precipitation, name='Prec',yaxis = 'y2', marker = list(color = 'rgba(106,90,205,1'), width = 1) %>%
                add_lines(y = ~et0, name='ET0', line = list(color = '#FC8D59')) %>%
                add_lines(y = ~eta, name='ETa', line = list(color = '#91CF60')) %>%
                add_bars(y = ~get(input$`Method 1`), name=input$`Method 1`,yaxis = 'y3', marker = list(color = 'rgba(0,0,0,0.3'), width = 1) %>%
                add_bars(y = ~get(input$`Method 2`), name=input$`Method 2`,yaxis = 'y3', marker = list(color = 'rgba(255,0,0,0.3'), width = 1) %>%
                layout(
                    title=paste(file1, ': ',input$year, sep = ''),
                    xaxis = list(title = "Date", domain = c(0,0.95), tickmode = 'auto'),
                    yaxis = list(title = 'Evapotranspiration (mm/d)', side = "left", color = "black", position = 0,
                                 anchor = 'free', range = c(0,6.75), dtick = 1.5),
                    yaxis2 = list(title = 'Precipitation (mm/d)', side = "right", color = "black",
                                  overlaying = "y", anchor = 'free', position = 0.95,
                                  range = c(45,0), dtick = 10),
                    yaxis3 = list(title = '', side = "right", color = "white",
                                  overlaying = "y", anchor = 'free', position = 1,
                                  range = c(0,1), dtick = 10),
                    showlegend = T
                )
        })
        
        output$graph3 <- renderPlotly({
            
            plot_ly(complete_series_select, x = ~Date) %>%
                add_bars(y = ~precipitation, name='Prec',yaxis = 'y2', marker = list(color = 'rgba(106,90,205,1'), width = 1) %>%
                add_lines(y = ~swc, name='SWC', line = list(color = '#CC6666')) %>%
                add_lines(y = ~swc_p20, name='SWC (20p)', line = list(color = 'rgba(169,169,169,1', width=1)) %>%
                add_lines(y = ~swc_p50, name='SWC (50p)', line = list(color = 'rgba(0,0,0,1', dash ="dot",width=1)) %>%
                
                add_bars(y = ~get(input$`Method 1`), name=input$`Method 1`,yaxis = 'y3', marker = list(color = 'rgba(0,0,0,0.3'), width = 1) %>%
                add_bars(y = ~get(input$`Method 2`), name=input$`Method 2`,yaxis = 'y3', marker = list(color = 'rgba(255,0,0,0.3'), width = 1) %>%
                layout(
                    title=paste(file1, ': ',input$year, sep = ''),
                    xaxis = list(title = "Date", domain = c(0,0.95), tickmode = 'auto'),
                    yaxis = list(title = 'Soil Water Content (%)', side = "left", color = "black", position = 0,
                                 anchor = 'free', range = c(0,45), dtick = 10),
                    yaxis2 = list(title = 'Precipitation (mm/d)', side = "right", color = "black",
                                  overlaying = "y", anchor = 'free', position = 0.95,
                                  range = c(45,0), dtick = 10),
                    yaxis3 = list(title = '', side = "right", color = "white",
                                  overlaying = "y", anchor = 'free', position = 1,
                                  range = c(0,1), dtick = 10),
                    showlegend = T
                )
        })
        
        output$graph4 <- renderPlotly({
            
            plot_ly(complete_series_select, x = ~Date) %>%
                add_lines(y = ~spei, name='SPEI', line = list(color =user_palette[4])) %>%
                add_lines(y = ~sesr, name='SESR', line = list(color = user_palette[6]), visible = "legendonly") %>%
                add_lines(y = ~eddi, name='EDDI',yaxis = 'y2', line = list(color = user_palette[8]), visible = "legendonly") %>%
                add_bars(y = ~get(input$`Method 1`), name=input$`Method 1`,yaxis = 'y3', marker = list(color = 'rgba(0,0,0,0.3'), width = 1) %>%
                add_bars(y = ~get(input$`Method 2`), name=input$`Method 2`,yaxis = 'y3', marker = list(color = 'rgba(255,0,0,0.3'), width = 1) %>%
                layout(
                    title=paste(file1, ': ',input$year, sep = ''),
                    xaxis = list(title = "Date", domain = c(0,0.95), tickmode = 'auto'),
                    yaxis = list(title = 'SPEI and SESR', side = "left", color = "black", position = 0,
                                 anchor = 'free', range = c(-3,3), dtick = 1),
                    yaxis2 = list(title = 'EDDI', side = "right", color = "black",
                                  overlaying = "y", anchor = 'free', position = 0.95,
                                  range = c(0,120), dtick = 20),
                    yaxis3 = list(title = '', side = "right", color = "white",
                                  overlaying = "y", anchor = 'free', position = 1,
                                  range = c(0,1), dtick = 10),
                    showlegend = T
                )
        })
        
        output$graph5 <- renderPlotly({
            
            plot_ly(complete_series_select, x = ~Date) %>%
                add_lines(y = ~et0_anomaly, name='ET0 anom.', line = list(color = user_palette[4])) %>%
                add_lines(y = ~eta_anomaly, name='ETa anom.', line = list(color = user_palette[6])) %>%
                add_lines(y = ~precipitation_anomaly, name='Prec anom.', line = list(color = user_palette[7]),visible = "legendonly") %>%
                add_lines(y = ~temperature_anomaly, name='Temp anom.', line = list(color = user_palette[9]), visible = "legendonly") %>%
                add_bars(y = ~get(input$`Method 1`), name=input$`Method 1`,yaxis = 'y2', marker = list(color = 'rgba(0,0,0,0.3'), width = 1) %>%
                add_bars(y = ~get(input$`Method 2`), name=input$`Method 2`,yaxis = 'y2', marker = list(color = 'rgba(255,0,0,0.3'), width = 1) %>%
                layout(
                    title=paste(file1, ': ',input$year, sep = ''),
                    xaxis = list(title = "Date", domain = c(0,0.95), tickmode = 'auto'),
                    yaxis = list(title = 'Anomaly (-)', side = "left", color = "black", position = 0,
                                 anchor = 'free', range = c(-3,3), dtick = 1),
                    yaxis2 = list(title = '', side = "right", color = "white",
                                  overlaying = "y", anchor = 'free', position = 1,
                                  range = c(0,1), dtick = 10),
                    showlegend = T
                )
        })
        
        output$graph6 <- renderLeaflet({
            leaflet() %>% addTiles() %>%
                addCircleMarkers(data = data_station_select1, lat = ~Latitude, lng = ~Longitude, radius = ~Duration/1.5,
                                 opacity = 1, fillOpacity = 1, color = c('black'),weight = 2, 
                                 popup = paste(data_station_select1$Station, '<br>',data_station_select1$IGPB2), fillColor = ~color1) %>%
                addCircleMarkers(data = data_station_select2, lat = ~Latitude, lng = ~Longitude, radius = ~Duration/1.5,
                                 opacity = 0, fillOpacity = 0.5, popup = paste(data_station_select2$Station, '<br>',data_station_select2$IGPB2), 
                                 fillColor = ~color1) %>%
                addCircleMarkers(data = data_stations, lat = ~Latitude, lng = ~Longitude, radius = 1,
                                 opacity = 0, fillOpacity = 1, popup = paste(data_stations$Station, '<br>',data_stations$IGPB2), 
                                 fillColor = 'black')%>%
                addLegend(position = "bottomright",
                          colors = unique(data_stations$color1),
                          labels = unique(data_stations$IGPB), opacity = 1,
                          title = "Land Use")
            
            
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("data-",file1 ,".csv", sep="")
            },
            content = function(file) {
                write.csv(complete_series_select, file)
            }
        )
        
    })
    
}



shinyApp(ui, server)