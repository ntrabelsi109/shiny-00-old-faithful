library(shiny)
library(tidyverse)
library(readxl)
library(sf)
library(plotly)

# Load and prepare data ----
global_health <- read_csv("global_health.csv") 

birth_rate_df <- read_excel("P_Data_Extract_From_World_Development_Indicators_all_years_for_kaggle.xlsx") |> 
  filter(Time == 2021) |> 
  mutate(Year = Time,
         Country = Country_name) |> 
  select(Country, Year, Birth_rate)

global_health <- global_health |> 
  left_join(birth_rate_df) |> 
  mutate(
    Infant_Mortality_Rate = round((Infant_Deaths / (Birth_rate / 1000 * Total_Population)) * 1000, 2),
    Classification = case_when(
      GDP_Per_Capita <= 1045 ~ "Low Income",
      GDP_Per_Capita > 1045 & GDP_Per_Capita <= 4095 ~ "Lower-Middle Income",
      GDP_Per_Capita > 4095 & GDP_Per_Capita <= 12695 ~ "Upper-Middle Income",
      GDP_Per_Capita > 12695 ~ "High Income"
    )
  ) |> 
  filter(Year == 2021, !is.na(Classification))

world_map <- read_sf("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp") |> 
  rename("Country_Code" = "SOV_A3") |> 
  filter(Country_Code != "ATA") |> 
  mutate(Country_Code = recode(Country_Code,
                               "US1" = "USA",
                               "KA1" = "KAZ",
                               "FR1" = "FRA",
                               "CU1" = "CUB",
                               "DN1" = "DNK",
                               "NL1" = "NLD",
                               "CH1" = "CHN",
                               "IS1" = "ISR",
                               "CYN" = "CYP",
                               "GB1" = "GBR",
                               "AU1" = "AUS",
                               "NZ1" = "NZL",
                               "FI1" = "FIN",
                               "SDS" = "SSD"))

# Join map and health data
imr_map_data <- world_map |> 
  left_join(global_health, by = "Country_Code") |> 
  mutate(h_text = paste0(Country, "\nIMR: ", Infant_Mortality_Rate))

# UI ----
ui <- fluidPage(
  titlePanel("Infant Mortality Rate by Country (2021)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "income_filter",
        label = "Select Income Classification:",
        choices = c("All", "High Income", "Upper-Middle Income", "Lower-Middle Income", "Low Income"),
        selected = "All"
      )
    ),
    
    mainPanel(
      plotlyOutput("map", height = "700px")
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    imr_map_data |> 
      mutate(
        # Gray out countries not in selected classification
        Infant_Mortality_Rate = ifelse(
          input$income_filter != "All" & Classification != input$income_filter,
          NA,
          Infant_Mortality_Rate
        ),
        h_text = ifelse(
          is.na(Infant_Mortality_Rate),
          paste0(Country, "\n(Not in selected income group)"),
          paste0(Country, "\nIMR: ", Infant_Mortality_Rate)
        )
      )
  })
  
  output$map <- renderPlotly({
    p <- ggplot() +
      geom_sf(data = filtered_data(),
              aes(fill = Infant_Mortality_Rate, text = h_text),
              linewidth = 0.2) +
      labs(
        fill = "Infant Mortality Rate",
        title = paste("Infant Mortality Rate, 2021 —", input$income_filter),
        caption = "Source: World Bank Open Data"
      ) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
      coord_sf(crs = st_crs("ESRI:54030")) + 
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title.position = "top",
        plot.title = element_text(hjust = 0.5, face = "bold", size = rel(1.5))
      )
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the app ----
shinyApp(ui, server)