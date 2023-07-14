#
# Title: LAHD Affordable Housing Projects Shiny App
#
# Description: This Shiny web application allows users to explore LAHD Affordable 
# Housing Projects dataset. The LAHD Affordable Housing dataset contains 
# information on the affordable housing projects financed by the Los Angeles 
# Housing Department (LAHD) from 2003 to the present. The dataset features, among 
# other variables, the costs, dates, site locations, number of units, housing 
# types, and development status for each project.
#
# Source of data: https://data.lacity.org/
# 
# Author of Shiny app: Joshua Roche
#

# Libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(forcats)

# Set working directory
my_working_dir <- 
  paste("C:/Users/jroch/OneDrive/Desktop/Our Stuff/Joshs Top Secret Files/",
        "University of Kansas Medical Center/DATA_824 Data Visualization",
        " and Acquisition/Shiny lessons/DATA_824_Shiny_Project_LA_Affordable_Housing", 
        sep = "")
setwd(my_working_dir)

# Source (URL) of dataset
source_url = paste("https://data.lacity.org/Housing-and-Real-Estate/",
                   "LAHD-Affordable-Housing-Projects-List-2003-to-Pres/mymu-zi3s",
                   sep = "")

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

# Date stamp
date_stamp <- lahd_housing_df$DATE_STAMP[1]

# Data for the 424 unique housing projects
# Joined with the data for number of sites per project
lahd_projects_df <- lahd_housing_df %>% 
  select(-c(1, 3, 6:10, 14, 24, 26:31)) %>% 
  group_by(PROJECT_NUMBER) %>%
  arrange(PROJECT_NUMBER) %>%
  distinct() %>%
  right_join(sites_per_project_df, by = "PROJECT_NUMBER") %>%
  mutate(PROJECT_NUMBER = as.character(PROJECT_NUMBER),
         YEAR_FUNDED = as.character(substr(DATE_FUNDED, 1, 4)),
         COST_PER_HOUSING_UNIT = TOTAL_DEVELOPMENT_COST / PROJECT_TOTAL_UNITS) %>%
  ungroup()

# Initialize the starting data frame
startData <- lahd_projects_df %>% 
  select(-c("PROJECT_NUMBER", "IN_SERVICE_DATE")) %>%
  #select(-c("IN_SERVICE_DATE")) %>%
  na.omit()


# Define UI for application that draws bar charts, scatter plots, and line charts
# for the affordable housing data
ui <- fluidPage(
  
  titlePanel("LAHD Affordable Housing Project Data: 2003 to Present"),
  
  div(paste("Last Updated on", date_stamp, "at the "), 
      a(href = source_url, "City of Los Angeles Open Data Portal")),
  
  h3("Distributions of Affordable Housing Project Characteristics"),
  
  fluidRow(
    
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "GB",label = "Group by Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor)),
                    selected = "HOUSING_TYPE"),
        selectInput(inputId = "Metric",label = "Housing Project Metric:",
                    choices = names(select_if(startData,is.numeric)),
                    selected = "LAHD_FUNDED"),
        plotOutput("circlePlot")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        h4("Table: Distribution of Project Characteristic"),
        dataTableOutput("distTable"),
        h4("Bar Chart: Distribution of Project Characteristic"),
        plotOutput("distPlot")
      )
    )
  ),
  
  h3("Box Plots of Affordable Housing Project Metrics"),
  
  fluidRow(
    
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "GBBox",label = "Group by Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor)),
                    selected = "HOUSING_TYPE"),
        selectInput(inputId = "MetricBox",label = "Housing Project Metric:",
                    choices = names(select_if(startData,is.numeric)),
                    selected = "LAHD_FUNDED")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        h4("Box Plot"),
        plotOutput("boxPlot")
      )
    )
  ),
  
  h3("Relationships between Affordable Housing Project Metrics"),
  
  fluidRow(
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "HPM1",label = "Housing Project Metric 1:",
                    choices = names(select_if(startData,is.numeric)),
                    selected = "LAHD_FUNDED"),
        selectInput(inputId = "HPM2",label = "Housing Project Metric 2:",
                    choices = names(select_if(startData,is.numeric)),
                    selected = "JOBS"),
        selectInput(inputId = "HPC",label = "Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor)),
                    selected = "HOUSING_TYPE")
      ),
      # Show a plot of the generated distribution
      mainPanel(
        h4("Scatter Plot"),
        plotOutput("scatPlot")
      )
    )
  ),
  
  h3("Affordable Housing Project Metrics over Time"),
  
  fluidRow(
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "HPM",label = "Housing Project Metric:",
                    choices = names(select_if(startData,is.numeric)),
                    selected = "LAHD_FUNDED"),
        selectInput(inputId = "HPC2",label = "Housing Project Characteristic:",
                    choices = names(select_if(startData,is.factor)),
                    selected = "HOUSING_TYPE"),
        radioButtons(inputId = "housingType", label = "Housing Type:", 
                     choices = c("ALL", na.omit(levels(startData$HOUSING_TYPE))),
                     selected = "ALL"),
        dateRangeInput("dateRange","Date Range:",start = "2003-01-01")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h4("Line Chart: Time Series by Year of Funding"),
        plotOutput("linePlot"),
        h4("Column Chart: Time Series by Year of Funding"),
        plotOutput("groupedColPlot"),
        h4("Table: Time Series by Year of Funding"),
        dataTableOutput("distYearTable")
      )
    )
  )
)


# Define server logic required to draw bar charts, scatter plots, and line charts
# for the affordable housing data
server <- function(input, output) {
  
  updateData <- reactive(
    startData %>% 
      group_by(!!as.name(input$GB)) %>% 
      summarise_if(is.numeric, sum, na.rm = TRUE) %>%
      mutate(COST_PER_HOUSING_UNIT = TOTAL_DEVELOPMENT_COST / PROJECT_TOTAL_UNITS)
  )
  
  timeData <- reactive(
    if(input$housingType == "ALL") {
      startData %>% 
        group_by(YEAR_FUNDED, !!as.name(input$HPC2)) %>% 
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        filter(YEAR_FUNDED >= substr(input$dateRange[1], 1, 4) 
               & YEAR_FUNDED <= substr(input$dateRange[2], 1, 4))
    } else {
      startData %>% 
        filter(HOUSING_TYPE == input$housingType) %>%
        group_by(YEAR_FUNDED, !!as.name(input$HPC2)) %>% 
        summarise_if(is.numeric, sum, na.rm = TRUE) %>%
        filter(YEAR_FUNDED >= substr(input$dateRange[1], 1, 4) 
               & YEAR_FUNDED <= substr(input$dateRange[2], 1, 4))
    }
  )
  
  output$distTable <- renderDT(
    updateData(), rownames = F, extensions = 'Buttons', filter = "top", 
    editable = TRUE,
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10,50,100,-1),c(10,50,100,"All"))
    )
  )
  
  output$distPlot <- renderPlot({
    updateData() %>% 
      ggplot(aes(x = fct_reorder(!!as.name(input$GB), !!as.name(input$Metric)),
                 y = !!as.name(input$Metric),fill = !!as.name(input$GB))) +
      geom_col() +
      coord_flip() +
      xlab("") +
      ylab(paste("Total of ", str_replace_all(input$Metric, "_", " "))) +
      ggtitle(paste("Total of ", str_replace_all(input$Metric, "_", " "), 
                    " by ", str_replace_all(input$GB, "_", " "))) +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$circlePlot <- renderPlot({
    updateData() %>% 
      ggplot(aes(x = "",
                 y = !!as.name(input$Metric), fill = !!as.name(input$GB))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      labs(x = NULL, y = NULL, fill = NULL) +
      ggtitle(paste("Total of ", str_replace_all(input$Metric, "_", " "), 
                    " by ", str_replace_all(input$GB, "_", " "))) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
  })
  
  output$boxPlot <- renderPlot({
    startData %>% 
      ggplot(aes(x = !!as.name(input$GBBox), y = !!as.name(input$MetricBox), 
                 fill = !!as.name(input$GBBox))) +
      geom_boxplot() +
      coord_flip()  +
      xlab("") +
      ylab(str_replace_all(input$MetricBox, "_", " ")) +
      labs(fill = str_replace_all(input$GBBox, "_", " ")) +
      ggtitle(paste(str_replace_all(input$MetricBox, "_", " "), " by ", 
                    str_replace_all(input$GBBox, "_", " "))) +
      theme(legend.position = "none",
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$scatPlot <- renderPlot({
    startData %>% 
      ggplot(aes(x = !!as.name(input$HPM1), y = !!as.name(input$HPM2), 
                 color = !!as.name(input$HPC))) +
      geom_point(alpha = 1/2, size = 3) +
      labs(x = str_replace_all(input$HPM1, "_", " "), y = str_replace_all(input$HPM2, "_", " "),
           color = str_replace_all(input$HPC, "_", " ")) +
      ggtitle(paste(str_replace_all(input$HPM2, "_", " "), " vs ", 
                    str_replace_all(input$HPM1, "_", " "), " by ", 
                    str_replace_all(input$HPC, "_", " "))) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$linePlot <- renderPlot({
    timeData() %>%
      ggplot(aes(x = as.numeric(YEAR_FUNDED), y = !!as.name(input$HPM), 
                 color = !!as.name(input$HPC2))) +
      geom_point(size = 3) +
      geom_line(linewidth = 1.5) +
      scale_x_continuous(breaks = seq(2003, 2023, 1)) +
      labs(x = "YEAR FUNDED", y = str_replace_all(input$HPM, "_", " "),
           color = str_replace_all(input$HPC2, "_", " ")) +
      ggtitle(paste("Total ", str_replace_all(input$HPM, "_", " "), " by ", 
                    str_replace_all(input$HPC2, "_", " "), " for ", 
                    str_replace_all(input$housingType, "_", " "),
                    " HOUSING from ", input$dateRange[1], 
                    " to ", input$dateRange[2])) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$groupedColPlot <- renderPlot({
    timeData() %>%
      ggplot(aes(x = as.numeric(YEAR_FUNDED), y = !!as.name(input$HPM), 
                 fill = !!as.name(input$HPC2))) +
      geom_col(position = position_stack()) +
      scale_x_continuous(breaks = seq(2003, 2023, 1)) +
      xlab("YEAR FUNDED") +
      ylab(str_replace_all(input$HPM, "_", " ")) +
      labs(fill = str_replace_all(input$HPC2, "_", " ")) +
      ggtitle(paste("Total ", str_replace_all(input$HPM, "_", " "), " by ",
                    str_replace_all(input$HPC2, "_", " "), " for ", 
                    str_replace_all(input$housingType, "_", " "),
                    " HOUSING from ", substr(input$dateRange[1], 1, 4), 
                    " to ", substr(input$dateRange[2], 1, 4))) +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16, face = "bold"))
  })
  
  output$distYearTable <- renderDT(
    timeData(), rownames = F, extensions = 'Buttons', filter = "top", 
    editable = TRUE,
    options = list(
      dom = 'Blfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      lengthMenu = list(c(10,50,100,-1),c(10,50,100,"All"))
    )
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
