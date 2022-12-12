library(tidyverse)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(shinyWidgets)
library(shinydashboard)
library(reshape2)
source("cleaning.R")

theme_set(theme_bw())

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

#Preprocess Data tab1
relevant_info <- uniData %>% 
  select(-c(X,X.1,clean_Name, HousingDeposit))
colnames(relevant_info) <- c("Campus", "Resident-Tuition", "Non-Resident-Tuition", "Minnesota-Reciprocity", "Enrollment-Deposit", "Admitted-Freshman-GPA-IQR","Percent-Admitted", 
                             "Total-Enrollment", "Average-Class-Size","Graduation-Rate","NCAA-Division","Percent-Receive-Financial-Aid","Latitudes","Longitudes")
data_noAthletics <- data %>%
  filter(!str_detect(`Dept Description`, "Ath"))


relevant_info %>% 
  select(c("Campus", "Resident-Tuition", "Non-Resident-Tuition","Percent-Admitted", 
         "Total-Enrollment", "Average-Class-Size","Graduation-Rate"))

#create average salary for 2021
temp <- data_noAthletics %>% 
  filter(`Fiscal year`==2021) %>% 
  select(c("Campus", "Total Pay")) %>% 
  group_by(Campus) %>% 
  summarise(`Mean Salary`=mean(`Total Pay`))

#join averages with tabular data
relevant_info <- relevant_info %>% 
  left_join(temp, by = c("Campus" = "Campus"))

#Format data for plot1
temp <- relevant_info %>% 
  select(c("Campus","Resident-Tuition", "Non-Resident-Tuition",'Mean Salary')) %>% 
  melt(id=c("Campus",'Mean Salary')) %>% 
  rename(Tuition=value, `Tuition Type` = variable)


plot1 <- temp %>%  
  ggplot(aes(x=Tuition,y=`Mean Salary`)) + 
  geom_point() +  
  facet_wrap(vars(`Tuition Type`))

plot1 <- plot1 +geom_text_repel(data = subset(temp, `Mean Salary` >= 55000),aes(label = Campus, size = NULL), 
                       nudge_y = 0.7, nudge_x = 3)

#Format data plot2
temp2 <- relevant_info %>% 
  select(c("Campus","Percent-Admitted", "Graduation-Rate", 'Mean Salary'))

#get intercept and slope value for plot2
reg<-lm(formula = `Graduation-Rate` ~ `Percent-Admitted`,
        data=temp2)                      

coeff<-coefficients(reg)          
intercept<-coeff[1]
slope<- coeff[2]
slope <- substr(paste0(slope, ""), 1,5)

plot2 <- temp2 %>%  
ggplot(aes(x=`Percent-Admitted`,y=`Graduation-Rate`, color=`Mean Salary`)) + 
geom_point() + 
  geom_smooth(method = "lm", se=FALSE)

plot2 <- plot2 +geom_text_repel(data = subset(temp2, `Mean Salary` >= 55000),aes(label = Campus, size = NULL), 
                       nudge_y = 1, nudge_x = -1) + scale_color_gradient(low = "green", high = "black")
plot2 <- plot2 + ggtitle(paste(paste("Increasing Percent Admitted by 1 Percent Causes a Decrease in Graduation Rate by", slope), "Percent.")) +
  xlab("Percentage of Undergraduate Students Admitted to the University in 2021") + ylab("Undergraduate Graduation Rate in 2021")

campus <- unique(data$Campus)
data <- data %>%
  mutate(
    School = str_split_i(`Dept Description`, "/", 1),
    Subschool = str_split_i(`Dept Description`, "/", 2),
    Department = str_split_i(`Dept Description`, "/", 3),
  )
#End preprocess data tab1

#Begin Graph Data Preprocess
letsci <- data %>%
  filter(Campus == "UW Madison" &
           str_detect(`Dept Description`, "L&S")) %>%
  select(`Dept Description`) %>%
  distinct() %>%
  mutate(
    School = str_split_i(`Dept Description`, "/", 1),
    Subschool = str_split_i(`Dept Description`, "/", 2),
    Department = str_split_i(`Dept Description`, "/", 3),
  )

nodes <- letsci %>%
  select(-`Dept Description`) %>%
  pivot_longer(School:Department,
               names_to = "type",
               values_to = "name") %>%
  drop_na() %>%
  distinct() %>%
  mutate(
    id = 1:n(),
    type = factor(type, levels = c("School", "Subschool", "Department"))
    )

edges <- edge_builder(letsci, nodes)

library(tidygraph)
library(ggraph)

G <- tbl_graph(
  nodes = nodes,
  edges = edges,
  directed = TRUE
)

#defining theme for tabs 3 and 4
my_theme<-theme(
  plot.background = element_rect(fill = "black", colour = NA),
  panel.background = element_rect(fill = "black", colour = NA),
  axis.text = element_text(colour = "linen"),
  axis.title = element_text(colour = "linen"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "grey50"),
  legend.background = element_rect(fill = "black", size = 4, colour = "black"),
  legend.justification = c(0, 1),
  legend.text = element_text(colour = "white"),
  legend.key= element_rect(fill = "black"),
  strip.background = element_rect(fill = "black", color = "grey50", size = 1),
  strip.text = element_text(colour = "white")
)

ui <- fluidPage(
  useShinydashboard(),
  titlePanel("UW-System Salary Analysis"),
  navbarPage("Navigation",
    tabPanel("UW-System Overview", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          titlePanel("Click a University on the Map for Details or Click Show All"),br(), 
          strong("Reset at any point with the clear button. The table is meant to provide users with relevant background information to contextualize further analysis."), 
          br(),
          strong("Plot One:"),br(),
          p("The first scatterplot shows how graduation rate changes with respect to admitted percentage and mean salary. We find a decreasing trend with slope of -1.18, which at a glance suggests graduation rate drops 1.18 percent for an extra 1 percent of admitted students. We also see that mean salary drops as admitted percentage grows with UW Whitewater being an exception. "), 
          br(), 
          strong("Plot Two:"), br(),
          p("The second scatterplots show how mean salary increases with respect to cost of tuition. Since all universities have drastically different tuitions depending on where the incoming student is from, the scatterplot has been faceted on in-state tuition versus out-of-state tuition. There appears to be much more spread in the out-of-state tuitions with UW Madison having the highest change in tuition. Both subplots show an extremely steep trend.")
        ), #close sidebar panel
        mainPanel(
          fluidRow(valueBoxOutput("schoolBox"),valueBoxOutput("salaryPaidBox"),valueBoxOutput("employeesdBox")),
          withSpinner(plotOutput(outputId = "Map", click = "click_plotFinder")
          ),
          fluidRow(column(7, helpText("Tip: Click locations to populate table below with information on schools in a specific area")),
                  column(width = 2, offset = 2, conditionalPanel(
            condition = "brushFinder",
            actionButton(inputId = "FinderClear", label = "Clear All"), 
            actionButton(inputId = "FinderFill", label = "Show All")))
          ),
          br(),
          fluidRow(
            withSpinner(dataTableOutput(outputId = "Table"))), 
          plotOutput("graduation"), plotOutput("tuition")
        ) #close main panel
      )#close sidebar layout 
    ), #close Tab 1
    tabPanel(
      "UW Madison -- L&S", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
          titlePanel("Selected departments:"),
          #shinythemes::themeSelector(),
          fluidRow(tableOutput(outputId = "print_me") #close column
          ) #close fluid row
        ), #close sidebar panel
        mainPanel(
          h2("Department structure"),
          p("The main UW Madison campus has an entire network of colleges, subschools, and departments."),
          p("Here, you can explore the different subschools and departments within the College of Letters & Sciences."),
          p("Brush the network graph to see salaries for the selected L&S divisions."),
          fluidRow(plotOutput(outputId = "LSgraph", brush = brushOpts(id = "brushy"), height = 800), width = 9),
          withSpinner(plotOutput(outputId = "LSscatter"))
        ) #close main panel
      )#close sidebar layout 
    ), #Close tab 2 
    tabPanel("Department wise Analysis", fluid = TRUE, 
              sidebarLayout(
                sidebarPanel(
                  selectInput("campus", "Select the Campus:", campus,selected = "UW Madison"),
                  uiOutput("school"),
                  #selectInput("school", "Select the School:", choices = as.character(data[data$Campus==input$campus,"School"]), multiple = TRUE,selected = "L&S"),
                  uiOutput("dept"),
                ), #close sidebar panel 
                  mainPanel(
                     h3("Yearly Distrbuition of Total Pay by Department"), plotOutput("tp"),
                     h3("Yearly Median Salary by Department"), plotOutput("md")
                  ) #close main panel
                ) #close sidebar layout
             ), #close tab 3 
    tabPanel("Job Title wise Analysis", fluid = TRUE, 
              sidebarLayout(
                sidebarPanel(
                  selectInput("campus", "Select the Campus:", campus,selected = "UW Madison"),
                  uiOutput("school2"),
                  uiOutput("title")
                ), #close sidebar panel
               mainPanel(
                 h3("Yearly Distrbuition of Total Pay by Job Title"), plotOutput("tpd"),
                 h3("Yearly Median Salary by Department Job Title"), plotOutput("mdd")
               ) #close main panel
              ) #close sidebar layout
            ) #close tab 4
  )#close navbar page
)#close UI

server <- function(input, output, session) {
  
  output$graduation<- renderPlot({
    plot2
  }) 
  output$tuition<- renderPlot({
    plot1
  }) 
  
  num_schools <- reactive({
      nrow(unique(user_clickFinder$Unis))
  })
  
  output$schoolBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(tags$p(num_schools(), style = "font-size: 20px;"),"Number of Selected Schools", 
                             icon = tags$i(icon("school", lib = "font-awesome"), style="font-size: 30px; color: white"),
                             color = "red")
  })
  
  salaryPaid <- reactive({
    names <- unique(user_clickFinder$Unis)$Campus 
    
    amount_paid <- data_noAthletics %>% 
      filter(`Fiscal year` == 2021 & Campus %in% names) %>% 
      summarise(`Total Paid 2021` = sum(`Total Pay`))
      
    val <- amount_paid[[1]]
    len <- floor(log10(val)) + 1
    
    if(len >= 10){
      trail <- "Billion Dollars"
      reduced <- val/1000000000
      temp_text <- substr(paste0(reduced,""), 1,5)
      out <- paste(temp_text,trail)
    } #close billion if
    else{
      if(len >= 7){
        trail <- "Million Dollars"
        reduced <- val/1000000
        temp_text <- substr(paste0(reduced,""), 1,7)
        out <- paste(temp_text,trail)
      } #close million if
      else{
        trail <- "Thousand Dollars"
        reduced <- val/1000
        temp_text <- substr(paste0(reduced,""), 1,7)
        out <- paste(temp_text,trail)
      } #close million else
    } #close billion else
    out
  })
  
  output$salaryPaidBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(tags$p(salaryPaid(), style = "font-size: 20px;"),"Salary Cost", 
                             icon = tags$i(icon("money-bill", lib = "font-awesome"), style="font-size: 30px; color: white"),
                             color = "green")
  })
  
  employeesPaid <- reactive({
    names <- unique(user_clickFinder$Unis)$Campus 
    
    number_paid <- data_noAthletics %>% 
      filter(`Fiscal year` == 2021 & Campus %in% names) %>% 
      summarise(`Count` = n())
    
    val <- number_paid[[1]]
    
    len <- floor(log10(val)) + 1
    trail <- "Thousand People"
    reduced <- val/1000
    temp_text <- substr(paste0(reduced,""), 1,5)
    out <- paste(temp_text,trail)
    out
  })
  
  output$employeesdBox <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(tags$p(employeesPaid(), style = "font-size: 20px;"),"Number of Employees Paid", 
                             icon = tags$i(icon("users", lib = "font-awesome"), style="font-size: 30px; color: white"),
                             color = "blue")
  })
  
  #Map
  output$Map <- renderPlot({
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
    add_row <-     nearPoints(relevant_info, input$click_plotFinder, xvar = "Longitudes", yvar = "Latitudes", threshold = 5)
    
    user_clickFinder$Unis <- rbind(add_row, user_clickFinder$Unis)  #bind new selection to  uni info df
  })
  
  brushFinder <- reactive({
    req(length(user_clickFinder$Unis) > 1)
    user_clickFinder$Unis
  })
  
  observeEvent({input$FinderClear
    },
               {user_clickFinder$Unis <- NULL
  })
  
  observeEvent({input$FinderFill
  },
  {user_clickFinder$Unis <- relevant_info
  })
  
  output$Table <-  renderDataTable({
    if(!is.null(user_clickFinder$Unis)){
      unique(user_clickFinder$Unis) %>% 
      select(-c("Enrollment-Deposit", "Latitudes", "Longitudes"))
    }
  },rownames = TRUE)
  ############################################################## End Tab 1

  set.seed(2022)
  p <-   ggraph(G, layout = "tree", circular = TRUE) +
    geom_edge_link(width = 0.2) +
    geom_node_label(aes(label = name, fill = type),
                    label.padding = unit(0.1, "lines")) +
    scale_fill_brewer(palette = "Set3") +
    theme(legend.position = "none")
  
  plot_df <- ggplot_build(p)
  
  coords <- plot_df$data[[2]]
  
  output$LSgraph <- renderPlot(p)
  
  coords_filt <- reactive({
    if (is.null(input$brushy$xmin)){
      coords
    } else {
      filter(coords, x >= input$brushy$xmin, 
             x <= input$brushy$xmax, 
             y >= input$brushy$ymin, 
             y <= input$brushy$ymax)
    }})

    output$print_me <- renderTable({
      brushed <- coords_filt()

      brushed %>%
        transmute(
          Type = levels(nodes$type)[group],
          Name = label
        )
    })
    
    output$LSscatter <- renderPlot({
      brushed <- coords_filt()
      
      ls_lookup(brushed$label, brushed$group) %>%
        ggplot() +
        geom_jitter(
          aes(x = factor(`Fiscal year`),
              y = `Total Pay`/1000),
          shape = "x",
          width = 0.2
        ) +
        labs(
          x = "Fiscal Year",
          y = "Total Pay (USD, in thousands)"
        )
    })
  output$school <- renderUI({
  selectInput("school", "Select the School:", choices = data[data$Campus==input$campus,"School"], multiple = TRUE,selected = "L&S")
  })
  output$school2 <- renderUI({
    selectInput("school2", "Select the School:", choices = data[data$Campus==input$campus,"School"], multiple = TRUE,selected = "L&S")
  })
  output$dept <- renderUI({
    selectInput("dept", "Select the Department:", unique(data[which(data[,"School"]==input$school),"Subschool"]), multiple = TRUE,selected = c("Economics","Statistics"))
  })
  output$title <- renderUI({
    selectInput("title", "Select the Job Title:", unique(data[which(data[,"School"]==input$school2),"Title"]), multiple = TRUE,selected = c("Professor","Lecturer"))
  })
  current_data <- reactive({data %>% filter(`Campus` %in% input$campus) %>% 
      filter(`School`  %in% input$school) %>%
      filter(`Subschool` %in%  input$dept)
  })
  
  current_data2 <- reactive({data %>% filter(`Campus` %in% input$campus) %>% 
      filter(`School`  %in% input$school2) %>%
      filter(`Title` %in%  input$title)
  })

  output$tp <- renderPlot({
    ggplot(current_data()) + geom_freqpoly(aes(`Total Pay`/1000,color=`Subschool`)) +facet_grid(~`Fiscal year`,scales = "free") + scale_colour_brewer(type = "seq", palette = "Set1") + my_theme
  })
  output$md <- renderPlot({
    current_data() %>%
    group_by(`Fiscal year`,`Campus`,`School`,`Subschool`)%>% 
    summarise(Median=median(`Total Pay`)/1000) %>%
    arrange(`Campus`,`Subschool`,`Fiscal year`) %>%
    ggplot() + geom_line(aes(`Fiscal year`,Median,color="lightblue3"),show.legend = FALSE) + facet_wrap(~`Subschool`,scales = "free") +  geom_point(aes(`Fiscal year`,Median,color="blue"),show.legend = FALSE) +theme(axis.text.x = element_text(angle = 90,size = 6),axis.text.y = element_text(size = 6)) +scale_x_continuous(limits = c(2017,2021)) + my_theme
  })
  
  output$tpd <- renderPlot({
    ggplot(current_data2()) + geom_freqpoly(aes(`Total Pay`/1000,color=`Title`)) +facet_grid(~`Fiscal year`,scales = "free") + scale_colour_brewer(type = "seq", palette = "Set1") + my_theme
  })
  output$mdd <- renderPlot({
    current_data2() %>%
      group_by(`Fiscal year`,`Campus`,`School`,`Title`)%>% 
      summarise(Median=median(`Total Pay`)/1000) %>%
      arrange(`Campus`,`Title`,`Fiscal year`) %>%
      ggplot() + geom_line(aes(`Fiscal year`,Median,color="lightblue3"),show.legend = FALSE) + facet_wrap(~`Title`,scales = "free") +  geom_point(aes(`Fiscal year`,Median,color="blue"),show.legend = FALSE) +theme(axis.text.x = element_text(angle = 90,size = 6),axis.text.y = element_text(size = 6)) +scale_x_continuous(limits = c(2017,2021)) + my_theme
  })
}


shinyApp(ui = ui, server = server)









