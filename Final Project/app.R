######################### libraries
library(shiny)
library(tidyverse)
library(httr)
library(zoo)
library(ggplot2)
library(plotly)
library(urbnmapr)
library(shinydashboard)

######################### population data

# api call 
countyData <- 'https://data.ny.gov/resource/krt9-ym2k.csv'
countyRequest <- GET(countyData)
df <- read_csv(content(countyRequest, "text"))
countyData <- df %>% 
    filter(year == '2019' & geography != "New York State") %>% # 2019 data only, remove NYS totals
    mutate(county = str_trim(gsub(" County", "", geography)), # county name
           county_fips = as.character(fips_code)) 

# when the data was last pulled 
lastUpdated <- as.character(Sys.Date(), format = "%m-%d-%Y")


######################### UI for Shiny app
ui <- fluidPage(
    
    ########## row for description
    fluidRow(
        column(12,
               # Application title
               titlePanel("WFTDA Return to Play Tracker"),
               h5("A dashboard developed for Queen City Roller Derby (QCRD)"),
               br(), 
               shiny::tags$ul(
                   tags$li("Women's Flat Track Derby Association (WFTDA) mandates that its member leagues 
                meet a minimum set conditions before returning to play. One of these requirements is 
                that the league's surrounding area have a maximum of 50 positive covid cases per
                100,000 individuals over a 14 day period. "), 
                   tags$li("Once reaching this threshold, QCRD will hold off an additional 14 days at
                   this baseline before returning to play. This dashboard serves as a way for the league 
                   to monitor the active covid cases in NYS and develop a timeline for return."),
                   tags$li("Covid data is sourced from the ", 
                           tags$a(href = 'https://health.data.ny.gov/Health/New-York-State-Statewide-COVID-19-Testing/xdss-u53e', 
                                  "ny.gov data repository"), ' and is refreshed every time a new county is selected from the
                dropdown list. Population data is sourced from the ', 
                           tags$a(href = 'https://data.ny.gov/Government-Finance/Annual-Population-Estimates-for-New-York-State-and/krt9-ym2k',
                                  "ny.gov government and finance data repository"), '.')
               ),
               br(),
        )
    ),

    ########## row for drop-downs
    fluidRow(
        column(2,
               # Input: Selector for choosing county ----
               selectInput(inputId = "countyname", 
                           label = "Choose a county:",
                           choices = c(unique(as.character(countyData$county))),
                           selected = 'Erie', # default to Erie county
                           selectize=FALSE),
        ),
        column(10,
               # Input: Selector for choosing charttype ----
               selectInput(inputId = "charttype", 
                           label = "Choose data display:",
                           choices = c('14 Day Rolling Sum', 'Daily Cases', 'Daily Tests', 'Daily Percent Positive'),
                           selected = '14 Day Rolling Sum', # default to 14 Day Rolling Sum
                           selectize=FALSE),
        )
    ),

    ########## summary statistics
    h4(textOutput("runningCount")),
    h4(textOutput('roll_sum')),
    br(),
    br(),
    
    ########## row for initial chart and map
    fluidRow(
        column(8,
               plotlyOutput(outputId = 'distPlot')),
        column(4,
               plotlyOutput(outputId = 'map'))
    ),
    br(),
)

######################### Server for Shiny App
# Define server logic required to draw a histogram
server <- function(input, output) {
    observeEvent(input$countyname, 
                 {
                     # api request
                     baseUrl <- 'https://health.data.ny.gov/resource/xdss-u53e.csv'
                     covidUrl <- paste0(baseUrl,'?county=', gsub(" ", "%20",input$countyname)) # based on input selection
                     covidReq <- GET(covidUrl)
                     covidData <- read_csv(content(covidReq, "text"))
                     
                     ######################### threshold analysis
                     pop <- countyData %>% 
                         filter(county == input$countyname) %>% # filter for county
                         select(population) %>% 
                         distinct()
                     threshold <- (pop[[1]]/100000) * 50
                     dailyThreshold <- threshold/14
                     
                     # WFTDA mandates that the 14 day rolling sum of positive cases does not exceed
                     # 50 cases in 100,000 individuals. 
                     ### roll_sum: calculates the 14 day rolling number of positive cases
                     ### meets_criteria: 1 if the 14 day rolling sum <= threshold
                     ### running_count: rolling number of days that the county has met threshold
                     finalData <- covidData %>%
                         arrange(test_date) %>%
                         mutate(threshold = threshold,
                                roll_sum = rollsum(x = new_positives, 14, align = "right", fill = NA)) %>%
                         mutate(meets_criteria = ifelse(roll_sum <= threshold, 1, 0)) %>%
                         mutate(running_count = with(., ave(meets_criteria, cumsum(meets_criteria == 0), FUN = cumsum))) %>%
                         mutate(percent_positive = ifelse(total_number_of_tests == 0, 0, new_positives/total_number_of_tests)) %>%
                         mutate(pct_pos_chg = percent_positive - lag(percent_positive))
                     
                     # OUTPUT: 14 day cases and number consecutive days at threshold
                     currRecord <- finalData %>%
                         filter(row_number() == n()) 
                     
                     output$roll_sum <- renderText({
                         paste('14 Day Total Cases: ', 
                               as.character(currRecord$roll_sum[[1]]))
                     })
                     
                     output$runningCount <- renderText({
                         paste("Number of consecutive days at baseline: ",
                         as.character(currRecord$running_count[[1]]))
                         })
                    
                     
                     # OUTPUT: map of the county
                     output$map <- renderPlotly({
                         selectedCounty <- countyData %>%
                             mutate(val = ifelse(county == input$countyname, 1, 0))
                         
                         nymap <- urbnmapr::counties %>%
                             inner_join(selectedCounty, by = 'county_fips') %>%
                             filter(state_name == 'New York') %>%
                             ggplot(mapping = aes(long, lat, group = group, fill = val, text= geography)) +
                             geom_polygon(color = '#ffffff', size = 0.25) +
                             scale_fill_gradient(labels = scales::percent)+
                             coord_map(projection = 'albers', lat0 = 39, lat1 = 45) +
                             ggtitle("Selected County in NYS")+
                             theme_minimal()+
                             theme(title = element_text(hjust = 1),
                                   axis.text.x=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title = element_blank(),
                                   panel.grid.major = element_blank(),
                                   legend.position = "none") 
                         
                         ggplotly(nymap, tooltip = c("text"))
                         })
                     
                     # OUTPUT: 14 day cumulative total or daily stats
                     output$distPlot <- renderPlotly({
                         
                         # 14 Day Rolling Sum
                         fig1 <- plot_ly(finalData,x = ~as.Date(test_date), y = ~roll_sum, 
                                        name = '14-Day Sum', 
                                        type = 'scatter',
                                        mode = 'lines+markers') %>% 
                             add_trace(y = ~finalData$threshold, 
                                       name = 'Threshold', 
                                       type = 'scatter',
                                       mode = 'lines') %>%
                             layout(title = '14-Day Rolling Sum of Positive Cases',
                                        xaxis = list(title = ""),
                                        yaxis = list(title = 'Cumulative Cases'),
                                        legend = list(orientation = 'h', 
                                                      xanchor = "center",
                                                      x = 0.5)) 
                         
                         # Daily New Cases
                         fig2 <- plot_ly(finalData,x = ~as.Date(test_date), y = ~new_positives, 
                                         name = 'New Cases', 
                                         type = 'scatter',
                                         mode = 'lines+markers') %>% 
                             add_trace(y = dailyThreshold, 
                                       name = 'Threshold', 
                                       type = 'scatter',
                                       mode = 'lines') %>%
                             layout(title = 'Daily New Positive Cases',
                                    xaxis = list(title = ""),
                                    yaxis = list(title = 'Positive Cases'),
                                    legend = list(orientation = 'h', 
                                                  xanchor = "center",
                                                  x = 0.5)) 
                         
                         # Percent Positive Cases
                         fig3 <- plot_ly(finalData,x = ~as.Date(test_date), y = ~percent_positive, 
                                         name = '% Positive', 
                                         mode = 'lines', 
                                         fill = 'tozeroy') %>% 
                             layout(title = 'Daily Percent Positive Rates',
                                    xaxis = list(title = ""),
                                    yaxis = list(title = 'Percent Positive'),
                                    legend = list(orientation = 'h', 
                                                  xanchor = "center",
                                                  x = 0.5))
                         
                         # Daily number of tests
                         fig4 <- plot_ly(finalData,x = ~as.Date(test_date), y = ~total_number_of_tests, 
                                         name = '# Tests', 
                                         mode = 'lines', 
                                         fill = 'lines+markers') %>% 
                             layout(title = 'Daily Number of Covid-19 Tests',
                                    xaxis = list(title = ""),
                                    yaxis = list(title = 'Number of Tests'),
                                    legend = list(orientation = 'h', 
                                                  xanchor = "center",
                                                  x = 0.5))
                         
                         if(input$charttype =='14 Day Rolling Sum')
                             {fig1} 
                         else if (input$charttype =='Daily Cases')
                             {fig2}
                         else if (input$charttype == 'Daily Percent Positive')
                             {fig3}
                         else 
                             {fig4}
                             
                         })
                     
                 })
}

# Run the application 
shinyApp(ui = ui, server = server)
