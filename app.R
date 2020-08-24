library(shiny)
library(tidyverse)
library(DT)
library(tidyverse)
library(tidylog)
library(janitor)



# dataset -----------------------------------------------------------------

confirmed_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
deaths_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
recorved_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"

# reading data ------------------------------------------------------------
deaths <- read_csv(deaths_url)
confirmed <- read_csv(confirmed_url)
recovered <- read_csv(recorved_url)

d_deaths <- 
  deaths %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "deaths_num" ) %>% 
  clean_names() %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%y")
  ) %>% 
  group_by(country_region, date) %>% 
  summarise(deaths_num = sum(deaths_num))
  ungroup() %>% 
  group_by(country_region) %>% 
  mutate(deaths_num = deaths_num - lag(deaths_num, default = 0)) %>% 
  ungroup()

d_confirmed <- 
  confirmed %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "confirmed_num" ) %>% 
  clean_names() %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%y")
  ) %>% 
  group_by(country_region, date) %>% 
  summarise(confirmed_num = sum(confirmed_num))
  ungroup() %>% 
  group_by(country_region) %>% 
  mutate(confirmed_num = confirmed_num - lag(confirmed_num, default = 0)) %>% 
  ungroup()

d_recovered <- 
  recovered %>% 
  pivot_longer(-c(`Province/State`, `Country/Region`, Lat, Long), names_to = "date", values_to = "recovered_num" ) %>% 
  clean_names() %>% 
  mutate(
    date = as.Date(date, format = "%m/%d/%y")
  ) %>% 
  group_by(country_region, date) %>% 
  summarise(recovered_num = sum(recovered_num))
  ungroup() %>% 
  group_by(country_region) %>% 
  mutate(recovered_num = recovered_num - lag(recovered_num, default = 0)) %>% 
  ungroup()


# join --------------------------------------------------------------------

d <- 
  d_deaths %>% 
  left_join(d_confirmed) %>% 
  left_join(d_recovered)

#-------------------------------------------------------
# Define UI for application that draws a histogram

ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "country",
        label = "Select Country",
        choices = unique(d$country_region),
        selected = "Saudi Arabia"
      ),
      selectInput(
        inputId = "cases_type",
        label = "Select cases type",
        choices = c("Confirmed", "Recovered", "Deaths")
      ),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Current Situation",
          verbatimTextOutput("summary")
        ),
        tabPanel(
          "Plot",
          plotOutput("plot")
        ),
        tabPanel(
          "Table",
          DTOutput("table")
        )
        
      )
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  country <- reactive({
    input$country
  })
  
  cases_type <- reactive({
    input$cases_type
  })
  
  output$summary <- renderText({
    cnf <-
      d %>%
      filter(country_region == country()) %>%
      select(confirmed_num) %>% 
      sum()
    
    rcv <-
      d %>%
      filter(country_region == country()) %>%
      select(recovered_num) %>% 
      sum()
    
    det <-
      d %>%
      filter(country_region == country()) %>%
      select(deaths_num) %>% 
      sum()
    
    cnf <- paste("\tConfirmed Cases:", cnf, sep=" ")
    rcv <- paste("\tRecovered Cases:", rcv, sep=" ")
    det <- paste("\tDeaths Cases:", det, sep=" ")
    
    last_update <- paste("Last update: ", Sys.Date()-1)
    paste(last_update, cnf, rcv, det, sep="\n")
  })
  
  output$table <- renderDT({
    c_type <- ifelse(cases_type() == "Confirmed", "confirmed_num", ifelse(cases_type() == "Recovered", "recovered_num", "deaths_num"))
    
    d %>%
      filter(country_region == country()) %>% 
      select(date, country_region, c_type)
  })
  
  
  output$plot <- renderPlot({
    c_type <- ifelse(cases_type() == "Confirmed", "confirmed_num", ifelse(cases_type() == "Recovered", "recovered_num", "deaths_num"))
    if(c_type == "confirmed_num") {
      d %>%
        filter(country_region == country()) %>% 
        ggplot(aes(x = date, y = confirmed_num)) +
        geom_line(color='darkblue') + 
        ylab("Confirmed Cases") +
        ggtitle(paste("Evolution of Daily Confirmed Cases in", country(), sep=" "))
    } else if (c_type == "recovered_num") {
      d %>%
        filter(country_region == country()) %>% 
        ggplot(aes(x = date, y = recovered_num)) +
        geom_line(color='green')+ 
        ylab("Recovered Cases") +
        ggtitle(paste("Evolution of Daily Recovered Cases in", country(), sep=" "))
    } else{
      d %>%
        filter(country_region == country()) %>% 
        ggplot(aes(x = date, y = deaths_num)) +
        geom_line(color='red')+ 
        ylab("Deaths Cases") +
        ggtitle(paste("Evolution of Daily Deaths Cases in", country(), sep=" "))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
