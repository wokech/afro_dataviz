# load packages ----
library(shiny)
library(ggplot2)
library(tidyverse)
library(sysfonts)

land_price <- readRDS("C:/R_Files/afrodataviz/sub_pro_3_hass_land_prices/my_app/data/all_data_avg_price.rds")
land_price_data <- readRDS("C:/R_Files/afrodataviz/sub_pro_3_hass_land_prices/my_app/data/all_data_avg_price_data.rds")

# user interface ----
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  navbarPage(
    "Exploring land prices in Nairobi and its suburbs",
     tabPanel("Introduction",
              em("Background info")),
     tabPanel("Nairobi Suburb Land Prices",
              tabsetPanel(
                tabPanel("Change in Land Prices over Time",
                        # Location Input ----
                        checkboxGroupInput(inputId = "location_choice_1", label = "Choose a location(s):",
                                    choices = c("Donholm", "Gigiri", "Karen", "Kileleshwa", "Kilimani", "Kitisuru",     
                                                "Langata", "Lavington", "Loresho", "Muthaiga", "Nyari", "Parklands",    
                                                "Ridgeways", "Riverside", "Runda", "Spring Valley", "Upperhill", 
                                                "Westlands", "Eastleigh"),
                                    selected = "Donholm"),
                      # Location and price output ----
                      plotOutput(outputId = "landprice_linePlot_1")))),
    tabPanel("Nairobi Satellite Town Land Prices",
             tabsetPanel(
               tabPanel("Change in Land Prices over Time",
                        # Location Input ----
                        checkboxGroupInput(inputId = "location_choice_2", label = "Choose a location(s):",
                                    choices = c("Athi River", "Juja", "Kiambu", "Kiserian",     
                                                "Kitengela", "Limuru", "Mlolongo", "Ngong", "Ongata Rongai",    
                                                "Ruaka", "Ruiru", "Syokimau", "Thika", "Tigoni"),
                                    selected = "Athi River"),
                        # Location and price output ----
                        plotOutput(outputId = "landprice_linePlot_2")))),
     tabPanel("Explore the data",
              tabsetPanel(
                tabPanel("Location/Price Data",
                         # location/price data table output
                         DT::dataTableOutput(outputId = "landprice_table"))))
)
)

# server instructions ----
server <- function(input, output) {
 
   # filter locations ----  
  location_df_1 <- reactive({ 
    land_price %>% 
      filter(location %in% input$location_choice_1) 
    
  })
  
  location_df_2 <- reactive({ 
    land_price %>% 
      filter(location %in% input$location_choice_2) 
    
  })
  
  # render the plot ----
  
  output$landprice_linePlot_1 <- renderPlot({ 
    ggplot(na.omit(location_df_1()), aes(quarter_year, average_price)) +
      geom_line(aes(color = location), size = 2) +
      geom_point(aes(color = location), size = 3) +
      theme_minimal() + 
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Year",
           y = "Average Price (KShs)",
           color = "Location") +
      theme(legend.position="bottom",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.background = element_rect(fill = "azure2"),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.x = element_text(size=24),
            axis.title.y = element_text(size=24),
            plot.title = element_text(size = 24, face = "bold"),
            plot.subtitle = element_text(size = 18),
            plot.background = element_rect(fill = "azure2", color = "azure2"),
            panel.background = element_rect(fill = "azure2", color = "azure2"))
  }) 
  
  # render the plot ----
  
  output$landprice_linePlot_2 <- renderPlot({ 
    ggplot(na.omit(location_df_2()), aes(quarter_year, average_price)) +
      geom_line(aes(color = location), size = 2) +
      geom_point(aes(color = location), size = 3) +
      theme_minimal() + 
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Year",
           y = "Average Price (KShs)",
           color = "Location") +
      theme(legend.position="bottom",
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 14),
            legend.background = element_rect(fill = "azure2"),
            panel.grid.major.x=element_blank(),
            panel.grid.minor.x=element_blank(),
            panel.grid.minor.y=element_blank(),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.x = element_text(size=24),
            axis.title.y = element_text(size=24),
            plot.title = element_text(size = 24, face = "bold"),
            plot.subtitle = element_text(size = 18),
            plot.background = element_rect(fill = "azure2", color = "azure2"),
            panel.background = element_rect(fill = "azure2", color = "azure2"))
  }) 
  
  # render the land price data
  output$landprice_table <- DT::renderDataTable({
    DT::datatable(land_price_data,
                  options = list(pageLength = 10),
                  caption = tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    'Table 1: ', tags$em('Location, Year/Quarter, and Price')))
  })
}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)