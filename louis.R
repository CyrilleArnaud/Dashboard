## Le chargement de librairies ##

library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(scales)
library('rsconnect')


dat <- readRDS("C:/Users/dell/Desktop/Master_2  Econométrie/RShiny/my_app/subregion_agg.rds")


clean_data <- dat %>%
  select( ! subregion1_name)%>%
  filter(country_name == "Canada", date >="2020-01-01" & date <= "2020-12-31")%>%
  group_by(country_name, date) %>%
  summarise_all(sum) %>%
  select(country_name, date, "new_confirmed") %>%
  arrange()

metric_choices <- colnames(dat)[4:ncol(dat)]
metric_names <- gsub("_", " ", metric_choices)
metric_names <- paste0(toupper(substr(metric_names, 1, 1)), substr(metric_names, 2, nchar(metric_names)))

metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names

dat %>%
  filter(country_name == "Canada" & date >= "2020-06-01" & date <= "2020-06-06") %>%
  select( !country_name ) %>%
  group_by(subregion1_name, date) %>%
  summarise_all(sum) %>%
  select(subregion1_name, date, "new_confirmed") %>%
  filter(subregion1_name != "" & subregion1_name %in% "Ontario")%>%
  set_names(c("subregion1_name", "date", "metric"))%>%
  arrange(date)




ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "COVID 19 Country Comparaison",
    titleWidth = 350
  ),
  
  dashboardSidebar(
    width = 350,
    br(),
    h4("Select Your inputs Here", style = "padding-left:18px"),
    
    ##debut de UI
    
    
    #choice the metric
    selectInput(
      
      inputId = "metric",
      label = strong("Select the metric", style = "font-family: 'arial'; font-size: 12px"),
      choices = metric_list,
      #choices = colnames(dat)[4:ncol(dat)],#
      selected = metric_list[[1]]
      
    ),
    
    ## The first input
    selectInput(
      
      inputId = "Country",
      multiple = TRUE,
      label = strong("Select the country to compare", style = "font-family: 'arial'; font-size: 14px"),
      choices = sort(unique(dat$country_name)),
      selected = c("United States of America","France", "Canada")
      
    ), 
    
    # Select the date
    dateRangeInput(
      
      inputId = "date_range_country",
      label = "Select date range country",
      start = "2020-01-01",
      end = "2020-12-31"
      
    ),
    
    ##Build range the region
    
    dateRangeInput(
      
      inputId = "date_range_regional",
      label = "Select date range regional",
      start = "2020-05-05",
      end = "2020-05-11"
      
    ),
    
    selectInput(
      
      inputId = "Country_single",
      multiple = FALSE,
      label = strong("Select a country: ", style = "font-family: 'arial'; font-size: 14px"),
      choices = unique(dat$country_name),
      selected = c("Canada")
    ), 
    
    selectInput(
      
      inputId = "subregion",
      multiple = TRUE,
      label = "Select a subregion:",
      choices = unique(dat$subregion1_name)
      
    )
    
    # ui fin 
    
  ),
  
  dashboardBody(
    
    tabsetPanel(
      
      type = "pills",
      id = "tab_selected",
      
      tabPanel(
        title = "Country View",
        #"This table 1",
        plotOutput("plot_country")
      ),
      tabPanel(
        title = "Regional View",
        "This table 2"
      ),
      tabPanel(
        title = "Tab 3",
        "This table 3"
      )
      
    )
    
  )
  
)

server <- function(input, output){
  
  
  observe({print(input$metric)})
  observe({print(input$Country)})
  observe({print(input$date_range_country)})
  
  clean_data_region <- reactive({
    
    data_clean_1 <- dat %>%
      filter(country_name == input$Country_single & date >= input$date_range_regional[1] & date <= input$date_range_regional[2]) %>%
      select( !country_name ) %>%
      group_by(subregion1_name, date) %>%
      summarise_all(sum) %>%
      select(subregion1_name, date, input$metric) %>%
      filter(subregion1_name != "" & subregion1_name %in% input$subregion)%>%
      set_names(c("subregion1_name", "date", "metric")) %>%
      arrange(date)
    
  })
  
  clean_data_country <- reactive({
    
    clean_data <- dat %>%
      select( ! subregion1_name)%>%
      filter(country_name %in% input$Country, date >= input$date_range_country[1] & date <= input$date_range_country[2])%>%
      group_by(country_name, date) %>%
      summarise_all(sum) %>%
      select(country_name, date, input$metric) %>%
      set_names(c('country_name', 'date', 'metric'))%>%
      arrange()
    
  })
  
  
  output$plot_country <- renderPlot({
    
    ggplot(clean_data_country(), aes(y = metric, x = date, color = country_name))+
      geom_line(size = 1.5)+
      labs(color= "Country Name")+
      ylab(metric_names[which(metric_choices==input$metric)])+
      xlab("Date")+
      labs(color = "Country Name") +
      scale_y_continuous(label= comma) + theme(plot.title = element_text(hjust = 0.5))+
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+
      ggtitle(metric_names[which(metric_choices==input$metric)])
      
    
  })
  
  
}

shinyApp(ui, server)

