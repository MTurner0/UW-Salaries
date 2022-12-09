library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

#Import Data
data <- readr::read_csv("../data/forbidden-all.csv") %>%
  mutate(
    `Total Pay` = `Total Pay` %>%
      gsub(x = ., pattern = "[$]", replacement = "") %>%
      gsub(x = ., pattern = ",", replacement = "") %>%
      as.numeric(),
    `Start Date` = `Start Date` %>%
      lubridate::mdy()
  ) %>%
  select(-Details)
fiftystatesCAN <- read.csv("../data/fiftystatesCAN.csv")
uniData <- read.csv('../data/uniData.csv')

relevant_info <- uniData %>% 
  select(-c(X,X.1,clean_Name, HousingDeposit))
colnames(relevant_info) <- c("Campus", "Resident-Tuition", "Non-Resident-Tuition", "Minnesota-Reciprocity", "Enrollment-Deposit", "Admitted-Freshman-GPA-IQR","Percent-Admitted", 
                             "Total-Enrollment", "Average-Class-Size","Graduation-Rate","NCAA-Division","Percent-Receive-Financial-Aid","Latitudes","Longitudes")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel("Desired Program Characteristics"),
      #shinythemes::themeSelector(),
      fluidRow(column(6,
          sliderInput(inputId = "OutStateTuitionRange",
              label = "Select Out of State Tuition",
              min = 10000,
              max = 40000,
              value = c(10000,40000),
              width = "220px"),
  helpText("For example: Show universities with out of state tuition between $10,000 and $40,000 per academic year"),
                      ) #close column
              ) #close fluid row
              ), #close sidebar panel
  mainPanel(
    withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder")
                ),
    fluidRow(column(7,
                    helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                    #actionButton(inputId = "draw", label = "Input Event and Times")
                    
    ),
    column(width = 2, offset = 2, conditionalPanel(
      condition = "brushFinder",
      actionButton(inputId = "FinderClear", label = "Clear Table")))),
    br(),
    fluidRow(
      withSpinner(dataTableOutput(outputId = "Table")))
  ) #close main panel
)#close sidebar layout
)#close UI


server <- function(input, output, session) {
  
  relevant_info_finder <- reactive({
    req(input$OutStateTuitionRange)
    #req(Input$School_Rank)
    filter(relevant_info, `Non-Resident-Tuition` >= input$OutStateTuitionRange[1], `Non-Resident-Tuition` <= input$OutStateTuitionRange[2])
  })
  
  
  
  output$scatterplotFinder <- renderPlot({
    isolate({
      fiftystatesCAN %>% 
        filter(State == 'WI') %>% 
        ggplot() +
        geom_polygon(aes(x = long, y = lat, group = group), color = "white", fill = "grey") +
        geom_point(data = uniData, aes(x = Longitudes, y = Latitudes)) +
        geom_text_repel(data = uniData, aes(x = Longitudes, y = Latitudes, label = as.character(clean_Name))) +
        coord_quickmap() +
        theme_void()
    })#close isolate
  }) #close renderPlot
  
  user_clickFinder <- reactiveValues()
  reactive({
    user_clickFinder$Unis <- data.frame(matrix(0, ncol = ncol(relevant_info), nrow = 1))
    names(user_clickFinder$Unis) <- colnames(relevant_info)
  })
  
  observeEvent(input$click_plotFinder, {
    add_row <-     nearPoints(relevant_info_finder(), input$click_plotFinder, xvar = "Longitudes", yvar = "Latitudes", threshold = 5)
    
    user_clickFinder$Unis <- rbind(add_row, user_clickFinder$Unis)  #bind new selection to  uni info df
  })
  
  brushFinder <- reactive({
    req(length(user_clickFinder$Unis) > 1)
    user_clickFinder$Unis
  })
  
  observeEvent({
    input$FinderClear
    #input$EnterTimes
  },{
    user_clickFinder$Unis <- NULL
  })
  
  output$Table <-  renderDataTable({
    
    relevant_info_finder()
  },rownames = TRUE)
}


shinyApp(ui = ui, server = server)









