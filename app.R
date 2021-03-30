library(tidyverse)
library(shiny)
library(zipcodeR)
library(plotly)
library(maps)
library(viridis)
library(RColorBrewer)
library(stringr)

# import processed HHS COVID Hospitalization Data
load("data/hhs_data_proc.rda")

# import zipcode and fips data from library(zipcodeR) and library(maps), respectively
data("zip_code_db")
data(county.fips)

# format zipcode and fips data to incorporate with processed HHS data
# this will allow us to add long/lat to the HHS data
zips <- zip_code_db %>%
  data.frame() %>% 
  rename('zip' = zipcode,
         'long' = lng) %>% 
  select(zip, lat, long)

fips <- county.fips %>%
  rename('fips_code' = fips) %>%
  mutate(fips_code = as.character(fips_code))

# merge HHS data with zip and fips data
hhs_data_proc <- hhs_data_proc %>%
  left_join(., zips, by = "zip") %>%
  left_join(., fips, by = "fips_code") %>%
  mutate(city = tolower(city),
         city = str_to_title(city),
         State = str_to_title(State)) %>%
  # reformat population label for aesthetic reasons
  mutate(population = if_else(population == "pediatric", "Pediatric", "Adult"),
         hospitalizations_standardized_region = round(hospitalizations_standardized_region, 1),
         State = str_to_title(State))

# create list of states
state_list <- hhs_data_proc %>% 
  arrange(State) %>%
  distinct(State)

# create list of cities (top 10 cities by U.S population)

city_list <- c("Austin",
               "Boston",
               "Chicago",
               "Dallas",
               "Houston",
               "Los Angeles",
               "New York",
               "Philadelphia",
               "Phoenix",
               "Saint Louis",
               "San Antonio",
               "San Diego",
               "San Jose")

# calculate the number of pediatric/adult hospitalizations per fips_code 
data_sums_by_state <- hhs_data_proc %>%
  group_by(collection_week, State, fips_code, population) %>%
  mutate(hosp_state_fips = if_else(population == "pediatric", 
                                   sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum, na.rm = TRUE), 
                                   sum(previous_day_admission_adult_covid_confirmed_7_day_sum, na.rm = TRUE))) %>%
  distinct(collection_week, State, city, hosp_state_fips, population, long, lat) %>%
  data.frame()

# group by collection_week and remove hospitals with 0 cases
MainStates <- data_sums_by_state %>%
  arrange(collection_week) %>%
  group_by(fips_code, population) %>% 
  filter(!hosp_state_fips == 0) %>%
  ungroup() 

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

# function to plot map and add shiny customizable options
plot_fig <- function(input_collection_week, covid_population) {

  
if (covid_population == "Pediatric & Adult") {
  MainStates <- MainStates
} else if (covid_population == "Pediatric") {
  MainStates <- MainStates %>% 
    filter(population == "Pediatric")
} else if (covid_population == "Adult") {
  MainStates <- MainStates %>% 
    filter(population == "Adult")
}
  
MainStates <- MainStates %>% 
  filter(collection_week == input_collection_week) %>%
  data.frame()
  
fig <- plot_geo(MainStates,
                locationmode = 'USA-states',
                sizes = c(1, 100))

color_pop <- if (covid_population == "Pediatric") {
  "#D95F02"
} else if (covid_population == "Adult") {
  "#1B9E77"
} else if (covid_population == "Pediatric & Adult") {
  c("#1B9E77", "#D95F02")
}

fig <- fig %>% add_markers(
  alpha = 0.2,
  x = ~long,
  y = ~lat,
  size = ~hosp_state_fips,
  color = ~population, 
  colors = color_pop,
  hoverinfo = "text",
  text = ~paste(MainStates$city, ",", MainStates$State, "<br />", MainStates$hosp_state_fips, "per 100,000")
) 
fig <- fig %>% layout(title = paste0('National ', covid_population, ' COVID-19 Hospitalizations'), geo = g)

fig

}

plot_national_ts <- function(data) {
  
  fig <- ggplot(data %>% 
           mutate(hospitalizations_standardized_us = if_else(population == "Pediatric", hospitalizations_standardized_us*10, hospitalizations_standardized_us)) %>% 
           distinct(collection_week, hospitalizations_standardized_us, population),
         aes(x = collection_week, y = hospitalizations_standardized_us, color = population)) + 
    geom_point(size = 2) + 
    geom_line(alpha=0.9, size = 1.5) + 
    scale_color_brewer(palette="Dark2", labels = c("Adult", "Child"), name = "") + 
    ylab("Hospitalizations per 100K Adults") + 
    xlab("") + 
    scale_x_date(date_labels = "%b-%d", date_breaks = "1 month") + 
    scale_y_continuous(sec.axis = sec_axis(~ ./10, name = "Hospitalizations per 100K Children")) + 
    ggtitle(label = paste0("National Pediatric & Adult COVID-19 Hospitalizations")) +
    labs(caption = "Hospitalizations were standardized per 100,000 children or adults using US Census population estimates.") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.text = element_text(size=15),
          legend.position = "top",
          legend.direction = "horizontal",
          axis.text.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 15, color = "#1B9E77", margin=margin(0,10,0,0)),
          axis.text.y.left = element_text(face = "bold",  size = 13, color = "#1B9E77"),
          axis.title.y.right = element_text(face = "bold", size = 15, color='#D95F02', margin=margin(0,0,0,10)),
          axis.text.y.right = element_text(face = "bold", size = 13, color='#D95F02'),
          plot.caption = element_text(hjust = 0.5, size = 15)) 

  print(fig)
}
  
  
plot_regional_fig <- function(region) {

  fig <- ggplot(hhs_data_proc %>%
           filter(Region == region) %>%
           mutate(hospitalizations_standardized_region = if_else(population == "Pediatric", 
                                                                 hospitalizations_standardized_region*10, hospitalizations_standardized_region)) %>%
           distinct(collection_week, Region, population, hospitalizations_standardized_region) %>%
           mutate(Date = collection_week,
                  Population = population,
                  `Hospitalization Rate` = hospitalizations_standardized_region),
         aes(x = Date, y = `Hospitalization Rate`, color = Population)) + 
    geom_point(size = 2) + 
    geom_line(alpha=0.9, size = 1.5) + 
    scale_color_brewer(palette="Dark2", labels = c("Adult", "Child"), name = "") + 
    ylab("Hospitalization per 100k Adults") + 
    xlab("") + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(sec.axis = sec_axis(~ ./10, name = "Hospitalations per 100k Children")) +
    ggtitle(label = paste0("Hospitalizations in the ", region)) +
    labs(caption = "Hospitalizations were standardized per 100,000 children or adults using US Census population estimates.") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.text = element_text(size=15),
          legend.position = "top",
          legend.direction = "horizontal",
          axis.text.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 15, color = "#1B9E77", margin=margin(0,10,0,0)),
          axis.text.y.left = element_text(face = "bold",  size = 13, color = "#1B9E77"),
          axis.title.y.right = element_text(face = "bold", size = 15, color='#D95F02', margin=margin(0,0,0,10)),
          axis.text.y.right = element_text(face = "bold", size = 13, color='#D95F02'),
          plot.caption = element_text(hjust = 0.5, size = 15))
  
  print(fig)
}

plot_state_fig <- function(state) {
  
  
  fig <- ggplot(hhs_data_proc %>%
                  filter(State == state) %>%
                  mutate(hospitalizations_standardized_state = if_else(population == "Pediatric", 
                                                                        hospitalizations_standardized_state*10, hospitalizations_standardized_state)) %>%
                  distinct(collection_week, State, population, hospitalizations_standardized_state) %>%
                  mutate(Date = collection_week,
                         Population = population,
                         `Hospitalization Rate` = hospitalizations_standardized_state),
                aes(x = Date, y = `Hospitalization Rate`, color = Population)) + 
    geom_point(size = 2) + 
    geom_line(alpha=0.9, size = 1.5) + 
    scale_color_brewer(palette="Dark2", labels = c("Adult", "Child"), name = "") + 
    ylab("Hospitalization per 100k Adults") + 
    xlab("") + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(sec.axis = sec_axis(~ ./10, name = "Hospitalations per 100k Children")) +
    ggtitle(paste0("Hospitalizations in ", state)) +
    labs(caption = "Hospitalizations were standardized per 100,000 children or adults using US Census population estimates.") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.text = element_text(size=15),
          legend.position = "top",
          legend.direction = "horizontal",
          axis.text.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 15, color = "#1B9E77", margin=margin(0,10,0,0)),
          axis.text.y.left = element_text(face = "bold",  size = 13, color = "#1B9E77"),
          axis.title.y.right = element_text(face = "bold", size = 15, color='#D95F02', margin=margin(0,0,0,10)),
          axis.text.y.right = element_text(face = "bold", size = 13, color='#D95F02'),
          plot.caption = element_text(hjust = 0.5, size = 15)) 
  
  print(fig)
}

plot_city_fig <- function(top_city) {
  
  if (top_city == "Austin") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Texas")
  } else if (top_city == "Boston") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Massachusetts")
  } else if (top_city == "Chicago") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Illinois")
  } else if (top_city == "Dallas") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Texas")
  } else if (top_city == "Houston") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Texas")
  } else if (top_city == "Los Angeles") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "California")
  } else if (top_city == "New York") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "New York")
  } else if (top_city == "Phildephia") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Pennsylvania")
  } else if (top_city == "Phoenix") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Arizona")
  } else if (top_city == "Saint Louis") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Missori")
  } else if (top_city == "San Antonio") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "Texas")
  } else if (top_city == "San Diego") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "California")
  } else if (top_city == "San Jose") {
    hhs_data_proc_filtered <- hhs_data_proc %>% filter(State == "California")
  }
  
  fig <- ggplot(hhs_data_proc_filtered %>%
                  filter(city == top_city) %>% 
                  group_by(collection_week, city) %>%
                  
                  mutate(hospitalizations = if_else(population == "Pediatric", sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum, na.rm = TRUE), 
                                                    sum(previous_day_admission_adult_covid_confirmed_7_day_sum, na.rm = TRUE)),
                         hospitalizations = if_else(population == "Pediatric", 
                                                    hospitalizations*10, hospitalizations)) %>%
                  distinct(collection_week, city, population, hospitalizations) %>%
                  mutate(Date = collection_week,
                         Population = population),
                aes(x = Date, y = hospitalizations, color = Population)) + 
    geom_point(size = 2) + 
    geom_line(alpha=0.9, size = 1.5) + 
    scale_color_brewer(palette="Dark2", labels = c("Adult", "Child"), name = "") + 
    ylab("Adult Hospitalizations") + 
    xlab("") + 
    scale_x_date(date_labels = "%b", date_breaks = "1 month") + 
    scale_y_continuous(sec.axis = sec_axis(~ ./10, name = "Pediatric Hospitalizations")) +
    ggtitle(paste0("Hospitalizations in ", top_city)) +
    labs(caption = "Hospitalizations reflect absolute value of pediatric or adult hospitalizations") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 20),
          legend.text = element_text(size=15),
          legend.position = "top",
          legend.direction = "horizontal",
          axis.text.x = element_text(face = "bold", size = 13),
          axis.title.y = element_text(face = "bold", size = 15, color = "#1B9E77", margin=margin(0,10,0,0)),
          axis.text.y.left = element_text(face = "bold",  size = 13, color = "#1B9E77"),
          axis.title.y.right = element_text(face = "bold", size = 15, color='#D95F02', margin=margin(0,0,0,10)),
          axis.text.y.right = element_text(face = "bold", size = 13, color='#D95F02'),
          plot.caption = element_text(hjust = 0.5, size = 15)) 
  
  print(fig)
}

#### shiny server
server <- function(input, output, session) {
  
  output$figure <- renderPlotly({
    figure <- plot_fig(input$collection_week, input$population)
    })
  output$figure1 <- renderPlot({
    figure1 <- plot_national_ts(hhs_data_proc)
  })
  output$figure2 <- renderPlot({
    figure2 <- plot_regional_fig(input$Region)
  })
  output$figure3 <- renderPlot({
    figure3 <- plot_state_fig(input$State)
  })
  output$figure4 <- renderPlot({
    figure3 <- plot_city_fig(input$City)
  })
  
}

#### shiny user interface
ui <- navbarPage("COVID-19 Hospitalizations",
                
                 tabPanel("National", 
                          fluidRow(sidebarPanel(width = 3,
                            id="sidebar",
                            radioButtons("View", "View As:",
                                         c("Map" = "Map",
                                           "Time Series" = "Time Series")),
                            selectInput("population", "Population",
                                        list("Pediatric",
                                             "Adult",
                                             "Pediatric & Adult")),
                            sliderInput("collection_week", "Date:",
                                        min = as.Date(min(MainStates$collection_week)),
                                        max = as.Date(max(MainStates$collection_week)),
                                        step = 7,
                                        animate = TRUE,
                                        value = c(as.Date(max(MainStates$collection_week)))
                            )
                          ), 
                          
                          mainPanel(
                            conditionalPanel(
                            condition = "input.View == 'Map'", plotlyOutput('figure')),
                            conditionalPanel(
                              condition = "input.View == 'Time Series'", plotOutput('figure1')))
                          )),
                 tabPanel("Regional", 
                          fluidRow(sidebarPanel(width = 3,
                                                id="sidebar_region",
                                                selectInput("Region", "Region",
                                                            list("Midwest",
                                                                 "Northeast",
                                                                 "South",
                                                                 "West"))), 
                          mainPanel(plotOutput('figure2')))),
                 tabPanel("State",
                          fluidRow(sidebarPanel(width = 3,
                                                id="sidebar_state",
                                                selectizeInput(inputId = "State",
                                                               label = "State", 
                                                               choices = state_list, 
                                                               selected = NULL)), 
                                   mainPanel(plotOutput('figure3')))),
                 tabPanel("City",
                          fluidRow(sidebarPanel(width = 3,
                                                id="sidebar_city",
                                                selectizeInput(inputId = "City",
                                                               label = "City", 
                                                               choices = city_list, 
                                                               selected = NULL)),
                                   mainPanel(plotOutput('figure4')))),
                 tabPanel("About",
                          fluidRow(
                            mainPanel(includeMarkdown("about.md"))))
                 
)

shinyApp(ui = ui, server = server)

