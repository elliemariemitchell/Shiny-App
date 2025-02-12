# Loading requisite packages
library(shiny)
library(WDI)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(animation)
library(rsconnect)

# Getting my data. I get this from the WDI API
wdi2<- WDI(country = "all", 
           indicator = c("NY.GDP.PCAP.PP.KD", 
                         "SH.DYN.MORT", 
                         "SP.POP.TOTL", 
                         "SP.DYN.TFRT.IN", 
                         "SP.DYN.LE00.IN"), 
           extra = TRUE, 
           start = 1990, 
           end = 2022)

# Next, I tidy and prepare the dataset by selecting just the variables I want
# in the graph
wdisubset2 <- select(wdi2, 
                     iso2c,
                     iso3c, 
                     country, 
                     year, 
                     SP.DYN.LE00.IN,
                     NY.GDP.PCAP.PP.KD, 
                     SP.POP.TOTL, 
                     SH.DYN.MORT, 
                     SP.DYN.TFRT.IN,
                     region,
                     income)

# Remove observations where region is "Aggregates" (these are non-country units)
wdisubset2 <- wdisubset2 |>
  subset(region != "Aggregates")

# Renaming the WDI variables to be a bit more understandable
wdisubset2 <- rename(wdisubset2, life_exp = SP.DYN.LE00.IN, 
                     Population = SP.POP.TOTL, 
                     GDP = NY.GDP.PCAP.PP.KD, 
                     child_mort = SH.DYN.MORT, 
                     total_fert = SP.DYN.TFRT.IN)

# Dropping NA values
wdisubset2 <- drop_na(wdisubset2, GDP)
wdisubset2 <- drop_na(wdisubset2, Population)
wdisubset2 <- drop_na(wdisubset2, child_mort)
wdisubset2 <- drop_na(wdisubset2, total_fert)
wdisubset2 <- drop_na(wdisubset2, life_exp)

# Creating a new region variable
wdisubset2 <- wdisubset2 %>%
  mutate(region2 = case_when((region == "North America" | region == "Latin America & Caribbean") ~"Americas", 
                             (region == "Sub-Saharan Africa") ~"Africa",
                             (region == "Europe & Central Asia") ~"Europe",
                             (region == "South Asia" | region == "East Asia & Pacific" | region == "Middle East & North Africa") ~ "Asia"))

# Starting to build my webpage. I'm making all of the interactive buttons here.
ui <- fluidPage(
  titlePanel("Hans Rosling Chart"),
  mainPanel(
    sliderInput("selected_year", 
                "Select Year", 
                min = min(wdisubset2$year), 
                max = max(wdisubset2$year), 
                value = min(wdisubset2$year), 
                step = 1, 
                sep = "", 
                animate = T),
    radioButtons("x_axis_transform", 
                 "X Axis Transformation", 
                 choices = c("Linear",
                             "Log"), 
                 selected = "Log"),
    radioButtons("y_axis_transform", 
                 "Y Axis Transformation", 
                 choices = c("Linear",
                             "Log"), 
                 selected = "Log"),
    selectInput("y_axis_variable", 
                "Select Y Axis Variable", 
     choices = c("Child Mortality", 
                 "Life Expectancy", 
                 "Total Fertility"), 
     selected = "Child Mortality"),
    plotlyOutput("scatterplot")
  )
)

# Adding the graph to the shiny app
server <- function(input, output) {
  
  output$scatterplot <- renderPlotly({
    filtered_data <- filter(wdisubset2, year == input$selected_year)
    
    # Choose the y-axis variable based on user selection
    y_variable <- switch(input$y_axis_variable,
                         "Child Mortality" = "child_mort",
                         "Life Expectancy" = "life_exp",
                         "Total Fertility" = "total_fert")
  
    gg <- filtered_data |>
      highlight_key(~country) |>
      ggplot(aes_string(x = "GDP", y = y_variable)) +
      geom_point(aes(size = (Population / 10000000000000)^2, 
                     color = region2, 
                     text = paste0(country, 
                                   "\n", 
                                   "Income:", 
                                   round(GDP, 1), 
                                   "\n",
                                   "Population:",
                                   round(Population, 1))), 
                 alpha = 0.5) +
      theme_minimal() +
      scale_color_manual(values = c("lightblue", 
                                    "lightgreen",
                                    "red1",
                                    "yellow")) +
      scale_size(range = c(1, 6)) +
      labs(title = paste("Hans Rosling Chart -", 
                         input$selected_year),
           x = "GDP 2017", 
           y = input$y_axis_variable) +
      theme(panel.grid.major = element_line(color = "lightgrey",
                                            size = 0.25, 
                                            linetype = "solid"), 
            panel.grid.minor = element_blank()) +
      guides(alpha = "none", 
             size = "none", 
             color = "none")

    # adding conditions for if 'log' or 'linear' is clicked:    
    if (input$x_axis_transform == "Log") {
      gg <- gg + scale_x_log10(labels = scales::comma)
    }
    if (input$y_axis_transform == "Log") {
      gg <- gg + scale_y_log10()
    }
    if (input$x_axis_transform == "Linear") {
      gg <- gg + scale_x_continuous(labels = scales::comma)
    }
    if (input$y_axis_transform == "Linear") {
      gg <- gg + scale_y_continuous()
    }
    
    # Convert ggplot to plotly
    gg <- ggplotly(gg, tooltip = "text", width = 800, height = 450) %>%
      highlight(on = "plotly_hover", opacityDim = .5)
    
    print(gg)
  })
}

shinyApp(ui = ui, server = server)

rsconnect::setAccountInfo(name='elliemitche', token='FEB1789CD59F8E8393F4EF0481A603EB', secret='SsxYgeMdN/5rELUPeL7M1j763aLeHLaLiJ9uNtgo')

library(rsconnect)

rsconnect::deployApp('path/to/your/app')