
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

# Date stamp
date_stamp <- format(as.Date(lahd_housing_df$DATE_STAMP[1], format = "%m/%d/&Y"), format = "%b %d, %Y")

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


lahd_projects_df %>% 
  filter(YEAR_FUNDED >= "2003-01-01"
         & YEAR_FUNDED <= "2022-01-01"
         & HOUSING_TYPE == "FAMILY") %>%
  group_by(YEAR_FUNDED) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)


lahd_projects_df %>% 
  filter(YEAR_FUNDED >= "2003-01-01"
         & YEAR_FUNDED <= "2022-01-01"
         & HOUSING_TYPE == "FAMILY") %>%
  group_by(YEAR_FUNDED) %>% 
  summarise(sum, na.rm = TRUE)





