#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)

# Set working directory
my_working_dir <- 
  paste("C:/Users/jroch/OneDrive/Desktop/Our Stuff/Joshs Top Secret Files/",
        "University of Kansas Medical Center/DATA_824 Data Visualization",
        " and Acquisition/Shiny lessons/DATA_824_Shiny_Project_LA_Affordable_Housing", sep = "")
setwd(my_working_dir)

# Create a directory for the data
data_directory <- "affordable_housing_data"
if(!file.exists(data_directory)){
  dir.create(data_directory)
}

# Download the dataset
file_url <- paste("https://data.lacity.org/api/views/mymu-zi3s/rows.csv?",
                  "accessType=DOWNLOAD&bom=true&format=true", sep = "")
download.file(file_url, destfile = "lahd_affordable_housing_projects.csv")

# Load the dataset and designate missing values as NA
lahd_housing_df <- read.csv("lahd_affordable_housing_projects.csv", 
                            na.strings = c("", "N/A"), header = TRUE)

# Edit the column names
names(lahd_housing_df) <- gsub("[.]+", "_", names(lahd_housing_df))
colnames(lahd_housing_df)[8] <- "SITE_NUMBER"
colnames(lahd_housing_df)[19] <- "TOTAL_DEVELOPMENT_COST"

# Change some columns to numeric type and other columns to factor type
lahd_housing_df <- lahd_housing_df %>%
  mutate(APN = as.character(APN), 
         PROJECT_NUMBER = as.factor(PROJECT_NUMBER),
         DEVELOPMENT_STAGE = as.factor(DEVELOPMENT_STAGE),
         CONSTRUCTION_TYPE = as.factor(CONSTRUCTION_TYPE), 
         SITE_COUNCIL_DISTRICT = as.factor(SITE_COUNCIL_DISTRICT),
         SITE_NUMBER = as.factor(SITE_NUMBER),
         HOUSING_TYPE = as.factor(HOUSING_TYPE),
         SUPPORTIVE_HOUSING = as.factor(SUPPORTIVE_HOUSING),
         DATE_FUNDED = as.Date(DATE_FUNDED, tryFormats = c("%m/%d/%Y")),
         LAHD_FUNDED = as.numeric(gsub(",", "", LAHD_FUNDED)),
         LEVERAGE = as.numeric(gsub(",", "", LEVERAGE)),
         TAX_EXEMPT_CONDUIT_BOND = as.numeric(gsub(",", "", TAX_EXEMPT_CONDUIT_BOND)),
         TOTAL_DEVELOPMENT_COST = as.numeric(gsub(",", "", TOTAL_DEVELOPMENT_COST)),
         IN_SERVICE_DATE = as.factor(IN_SERVICE_DATE),
         JOBS = as.numeric(gsub(",", "", JOBS)))

# Number of sites for each project
sites_per_project_df <- lahd_housing_df %>% 
  count(PROJECT_NUMBER) %>%
  arrange(PROJECT_NUMBER)

colnames(sites_per_project_df)[2] <- "NUMBER_OF_SITES"

# Data for the 424 unique housing projects
# Joined with the data for number of sites per project
lahd_projects_df <- lahd_housing_df %>% 
  select(-c(1, 3, 6:10, 14, 24, 26:31)) %>% 
  group_by(PROJECT_NUMBER) %>%
  arrange(PROJECT_NUMBER) %>%
  distinct() %>%
  right_join(sites_per_project_df, by = "PROJECT_NUMBER") %>%
  mutate(PROJECT_NUMBER = as.character(PROJECT_NUMBER),
         YEAR_FUNDED = as.character(substr(DATE_FUNDED, 1, 4))) %>%
  ungroup()



# startData <- lahd_housing_df[, -c(1, 2, 3, 6, 9, 21, 22, 23, 24, 26:31)]

startData <- lahd_projects_df %>% 
  select(-c("PROJECT_NUMBER", "IN_SERVICE_DATE")) %>%
  na.omit()

lahd_completed_projects_df <- lahd_projects_df %>% 
  filter(IN_SERVICE_DATE != "Completed but Missing Date" & 
           IN_SERVICE_DATE != "Development") %>%
  mutate(IN_SERVICE_DATE = as.Date(IN_SERVICE_DATE, tryFormats = c("%Y"))) %>% 
  select(-c("PROJECT_NUMBER")) %>%
  na.omit()


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("LAHD Affordable Housing Project Data: 2003 to Present"),
  
  fluidRow(
    
    h3("Distributions of Housing Project Characteristics"),
    
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "GB",label = "Group by Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor))),
        selectInput(inputId = "Metric",label = "Housing Project Metric:",
                    choices = names(select_if(startData,is.numeric))),
        plotOutput("circlePlot")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        dataTableOutput("distTable"),
        plotOutput("distPlot"),
        plotOutput("boxPlot")
      )
    )
  ),
  
  h3("Relationships between Housing Project Metrics"),
  
  fluidRow(
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "HPM1",label = "Housing Project Metric 1:",
                    choices = names(select_if(startData,is.numeric))),
        selectInput(inputId = "HPM2",label = "Housing Project Metric 2:",
                    choices = names(select_if(startData,is.numeric))),
        selectInput(inputId = "HPC",label = "Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor)))
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("scatPlot")
      )
    )
  ),
  
  h3("Housing Project Metrics over Time"),
  
  fluidRow(
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "HPM",label = "Housing Project Metric:",
                    choices = names(select_if(startData,is.numeric))),
        selectInput(inputId = "HPC2",label = "Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor))),
        #dateRangeInput("dateRange","Date Range:",start = today()-365*20),
        radioButtons("housingType", "Housing Type:", 
                     na.omit(levels(startData$HOUSING_TYPE)),
                     selected = "FAMILY"),
        dateRangeInput("dateRange","Date Range:",start = "2003-01-01")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("linePlot"),
        plotOutput("groupedLinePlot")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  updateData <- reactive(
    startData %>% 
      group_by(!!as.name(input$GB)) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE))
  
  output$distTable <- renderDT(
    updateData(), rownames = F, extensions = 'Buttons', filter="top", editable=T,
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10,50,100,-1),c(10,50,100,"All"))
    )
  )
  
  output$distPlot <- renderPlot({
    updateData() %>% 
      ggplot(aes(x=fct_reorder(!!as.name(input$GB), !!as.name(input$Metric)),
                 y=!!as.name(input$Metric),fill=!!as.name(input$GB))) +
      geom_col() +
      coord_flip() +
      xlab("") +
      ylab(paste("TOTAL OF ", input$Metric)) +
      ggtitle(paste("TOTAL OF ", input$Metric, " BY ", input$GB)) +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$circlePlot <- renderPlot({
    updateData() %>% 
      ggplot(aes(x="",
                 y=!!as.name(input$Metric),fill=!!as.name(input$GB))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      #geom_text(label = input$Metric) +
      labs(x = NULL, y = NULL, fill = NULL) +
      #ggtitle(paste("TOTAL OF ", input$Metric, " BY ", input$GB)) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
#  output$distPlot <- renderPlot({
#    updateData() %>% 
#      ggplot(aes(x=!!as.name(input$GB),y=!!as.name(input$Metric),fill=!!as.name(input$GB))) +
#      geom_col() +
#      coord_flip()
#  })
  
  output$boxPlot <- renderPlot({
    startData %>% 
      ggplot(aes(x=!!as.name(input$GB),y=!!as.name(input$Metric),fill=!!as.name(input$GB))) +
      geom_boxplot() +
      coord_flip()  +
      xlab("") +
      ggtitle(paste(input$Metric, " BY ", input$GB)) +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$scatPlot <- renderPlot({
    startData %>% 
      ggplot(aes(x=!!as.name(input$HPM1),y=!!as.name(input$HPM2),color=!!as.name(input$HPC))) +
      geom_point() +
      ggtitle(paste(input$HPM2, " VS ", input$HPM1, " BY ", input$HPC)) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$linePlot <- renderPlot({
    startData %>% 
      filter(DATE_FUNDED >= input$dateRange[1] & DATE_FUNDED <= input$dateRange[2] 
             & HOUSING_TYPE == input$housingType) %>%
      ggplot(aes(x=DATE_FUNDED,y=!!as.name(input$HPM),color=!!as.name(input$HPC2))) +
      geom_point() +
      geom_line() +
      ggtitle(paste(input$HPM, " BY ", input$HPC2, " FOR ", input$housingType,
                    " HOUSING FROM ", input$dateRange[1], 
                    " TO ", input$dateRange[2])) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$groupedLinePlot <- renderPlot({
    startData %>% 
      filter(HOUSING_TYPE == input$housingType) %>%
      group_by(YEAR_FUNDED) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      filter(YEAR_FUNDED >= substr(input$dateRange[1], 1, 4) 
             & YEAR_FUNDED <= substr(input$dateRange[2], 1, 4)) %>%
      ggplot(aes(x=YEAR_FUNDED,y=!!as.name(input$HPM))) +
      #geom_point(color = "blue") +
      #geom_line(color = "purple") +
      geom_col(fill = "steelblue") +
      ggtitle(paste("TOTAL ", input$HPM, " FOR ", input$housingType,
                    " HOUSING FROM ", substr(input$dateRange[1], 1, 4), 
                    " TO ", substr(input$dateRange[2], 1, 4))) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
