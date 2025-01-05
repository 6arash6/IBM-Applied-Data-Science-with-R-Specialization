# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(ggrepel)
require(lubridate)
require(ggthemes)



# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  city_dropdown = c("All", "Seoul", "Suzhou", "London", "New York", "Paris") #Redundant
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    select(CITY_ASCII,LAT,LNG,BIKE_PREDICTION_LEVEL,BIKE_PREDICTION,LABEL,DETAILED_LABEL) %>%
    group_by(CITY_ASCII) %>%
    #slice(which.max(BIKE_PREDICTION))
    slice_max(BIKE_PREDICTION)
  # Observe drop-down event
  observeEvent(input$city_dropdown,{
    # Then render output plots with an id defined in ui.R
    
    if(input$city_dropdown == 'All') {
      # Complete this function to render a leaflet map
      output$city_bike_map <- renderLeaflet({
        leaflet(cities_max_bike) %>% addTiles() %>% 
          addCircleMarkers(~LNG,
                           ~LAT,
                           popup=~LABEL,
                           radius=(case_when(
                             cities_max_bike$BIKE_PREDICTION_LEVEL=='small'~ 6,
                             cities_max_bike$BIKE_PREDICTION_LEVEL=='medium'~ 10,
                             TRUE ~ 12
                           )),
                           color=(case_when(
                             cities_max_bike$BIKE_PREDICTION_LEVEL=='small'~ 'green',
                             cities_max_bike$BIKE_PREDICTION_LEVEL=='medium'~ 'yellow',
                             TRUE ~ 'red'
                           )))
      })
    }
    else{
      output$city_bike_map <- renderLeaflet({
        city_name=input$city_dropdown
        selected_city <- cities_max_bike %>% 
          filter(CITY_ASCII==city_name)
        leaflet(selected_city) %>% addTiles() %>% 
          addMarkers(~LNG,
                     ~LAT,
                     popup =~DETAILED_LABEL)
      })
      
      output$temp_line <- renderPlot({
        
        temp_df <- city_weather_bike_df %>%
          select(TEMPERATURE) 
        
        city_name=input$city_dropdown    
        
        ggplot(temp_df, aes(x=1:length(TEMPERATURE),y=TEMPERATURE)) +
          geom_line(color="skyblue", size=1) +
          geom_point() + 
          geom_text(aes(label=paste(TEMPERATURE,"°C")),
                    position=position_jitter(width=2.4,height=2.4), check_overlap = TRUE) +
          coord_cartesian(xlim=c(0,40)) + 
          labs(x="Time/ 3 hours ahead forecast", y="Temperature/°C") +
          ggtitle(paste(city_name,"Temperature Forecast Plot")) 
        
      })
      
      output$bike_line <- renderPlot({
        
        pred_df <- city_weather_bike_df %>%
          select(FORECASTDATETIME, BIKE_PREDICTION)
        
        city_name=input$city_dropdown
        
        ggplot(pred_df, aes(x=as.POSIXct(FORECASTDATETIME), 
                            y=BIKE_PREDICTION)) +
          geom_line(color="orange", size=1,linetype="dashed") +
          geom_point() +
          geom_text(aes(label=BIKE_PREDICTION),
                    position=position_jitter(width=2.4,height=2.4), check_overlap = TRUE) +
          labs(x="Time/ 3 hours ahead forecast", y="Predicted Bike Rentals") +
          ggtitle(paste(city_name,"Bike Demand Forecast Plot")) 
        
      })
      
      output$bike_date_output <- renderText({
        paste("Time =",as.POSIXct(as.integer(input$plot_click$x), origin = "1970-01-01"),
              "\nPredicted Bike Rentals =",input$plot_click$y)
      })
      
      output$humidity_pred_chart <- renderPlot({
        
        humidity_df <- city_weather_bike_df %>%
          select(HUMIDITY, BIKE_PREDICTION)
        
        city_name=input$city_dropdown
        
        ggplot(humidity_df, aes(x=HUMIDITY,y=BIKE_PREDICTION)) +
          geom_point() +
          geom_smooth(method="lm", formula=y~ poly(x,4), color="cadetblue") +
          ggtitle(paste(city_name,"'s Humidity Impact on Predicted Bike Demand")) 
        
      })
    }
  })
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})