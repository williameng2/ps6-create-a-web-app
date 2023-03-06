library(shiny)
library(tidyverse)

UAH <- read_delim("UAH-lower-troposphere-long.csv.bz2")

ui <- fluidPage(
    tabsetPanel(
      tabPanel("About", 
        titlePanel("General Information!"),
        HTML(paste("The data we are examining is the <strong>deviation in temperature</strong>
        (<em>in degrees celcius</em>) from the average temperatures from 1991-2020.
        Below are 5 lines of randomly selected data points!")),
        mainPanel(
          tableOutput("data")
        )
      ),
      
      tabPanel("Plots",
        titlePanel("Temperature Deviations (/year) by Region"),
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput("regions", "Choose Regions to Display!", 
                               choices = unique(UAH$region), 
                               selected = "aust")
          ),
          mainPanel(
            plotOutput("plot"),
            textOutput("selected")
          )
        )
      ),
      
      tabPanel("Table",
        titlePanel("Average Deviations in Temperature (by Region)"),
     
        sidebarLayout(
          sidebarPanel(
           radioButtons("period", "Select a Time Frame!",
                      c("Month", "Year", "Decade"), selected = "Decade")
          ),
          mainPanel(
            textOutput("numObservations"), 
            tableOutput("table")
          )
        )
    )
  )
)
  

server <- function(input, output) {
  output$data <- renderTable({
    UAH %>% 
      sample_n(5)
  })
  
  output$plot <- renderPlot({
    UAH %>% 
      filter(region %in% input$regions) %>% 
      group_by(region) %>% 
      ggplot(aes(x=year, y=temp, group = region, color=factor(region)))+
      geom_point()+
      labs(x="Year", y="Deviation in Temperature (deg C)", color="Region")
  })
  
  output$selected <- renderText({
    if (UAH %>% 
      filter(region %in% input$regions) %>%
      nrow() == 0) {
      "No regions are selected!!"
    } else {
        regionsDisplayed <- UAH %>% 
        filter(region %in% input$regions) %>%
        select(region) %>% 
        distinct() %>% 
        unlist()
        paste0("Now Displaying (", 
               length(regionsDisplayed), " reg): ", 
               paste0(regionsDisplayed, collapse = ", "))
    }
  })
  
  output$table <- renderTable({
    if (input$period == "Month") {
      UAH %>%
        group_by(year, month) %>%
        summarize(avg_temp = mean(temp))
    } else if (input$period == "Year") {
      UAH %>%
        group_by(year) %>%
        summarize(avg_temp = mean(temp))
    } else {
      UAH %>%
        group_by(decade = 10*(year %/% 10)) %>%
        summarize(avg_temp = mean(temp))
    }
  })
  
  output$numObservations <- renderText({
    if (input$period == "Month") {
      numObs <- UAH %>%
        group_by(year, month) %>%
        summarize(avg_temp = mean(temp)) %>% 
        nrow()
    } else if (input$period == "Year") {
      numObs <- UAH %>%
        group_by(year) %>%
        summarize(avg_temp = mean(temp)) %>% 
        nrow()
    } else {
      numObs <- UAH %>%
        group_by(decade = 10*(year %/% 10)) %>%
        summarize(avg_temp = mean(temp)) %>% 
        nrow()
    }
    paste("Displaying ", numObs, " Observations!")
  })
}


shinyApp(ui = ui, server = server)