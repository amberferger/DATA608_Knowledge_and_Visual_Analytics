
################ libraries

library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(DT)

################ base data

url <- 'https://raw.githubusercontent.com/amberferger/DATA608_Knowledge_and_Visual_Analytics/master/module3/data/'
filenm <- 'cleaned-cdc-mortality-1999-2010-2.csv'

df <- read.csv(paste0(url, filenm))


#Question 1:
#As a researcher, you frequently compare mortality rates from particular causes across
#different States. You need a visualization that will let you see (for 2010 only) the crude
#mortality rate, across all States, from one cause (for example, Neoplasms, which are 
#effectively cancers). Create a visualization that allows you to rank States by crude mortality
#for each cause of death.


#Question 2:
#Often you are asked whether particular States are improving their mortality rates (per cause)
#faster than, or slower than, the national average. Create a visualization that lets your clients
#see this for themselves for one cause of death at the time. Keep in mind that the national
#average should be weighted by the national population.

################################# ui
ui <- fluidPage(
  
  # Application title
  titlePanel("Mortality Rates by Cause Across the US"),
  
  selectInput(inputId = "input_cause", 
              label = " Choose a Cause:",
              choices = unique(df$ICD.Chapter),
              selectize=FALSE,
              width = '60%'),
  br(),
  plotlyOutput(outputId = "mortalityPlot"),
  br(),
  br(),
  br(),
  plotlyOutput(outputId = "mortalityChange"),
  br()
)

################################# server
server <- function(input, output, ...) {
  
  ###### output ---- plot: mortality in 2010
  output$mortalityPlot <- renderPlotly({
    
    fig <- plot_ly(data = df %>% filter(Year == '2010' & ICD.Chapter == input$input_cause), 
                   x = ~Crude.Rate, y = ~reorder(State, Crude.Rate),
                   type = 'bar',
                   orientation = 'h')
    
    fig %>% layout(title = paste0('Crude Mortality Rate by State in 2010'),
                   xaxis = list(title = "Crude Mortality Rate"),
                   yaxis = list (title = "State"))
  })
  
  
  ###### output ---- plot: mortality change
  
  output$mortalityChange <- renderPlotly({
    
    base <- df %>%
      filter(ICD.Chapter == input$input_cause) %>%
      group_by(ICD.Chapter, State) %>% # min and max for each state and disease
      mutate(MIN_YEAR = ifelse(min(Year) == Year, 1,0),
             MAX_YEAR = ifelse(max(Year) == Year, 1,0)) %>%
      group_by(Year, ICD.Chapter) %>%
      mutate(TOTAL_POP = sum(Population)) %>% # sum population for year and disease
      ungroup() %>%
      mutate(POP_PORTION = Population/TOTAL_POP) %>% # portion of total population
      mutate(WEIGHTED_RATE = Crude.Rate * POP_PORTION) %>% # rate times portion of total pop
      filter(MIN_YEAR == 1 | MAX_YEAR == 1) %>%
      group_by(MIN_YEAR, ICD.Chapter) %>%
      mutate(NATIONAL_AVG_RATE = sum(WEIGHTED_RATE)) %>%
      ungroup() %>%
      group_by(MIN_YEAR, ICD.Chapter) %>%
      mutate(id = row_number()) %>%
      ungroup()
    
    min <- base %>% filter(MIN_YEAR == 1)
    max <- base %>% filter(MAX_YEAR == 1)
    
    final <- inner_join(min, max, by=c('id', 'ICD.Chapter')) %>%
      select(ICD.Chapter, 
             State = State.x,
             Y1 = Year.x,
             Y1_Crude_Rate = Crude.Rate.x,
             Y1_Weighted_Rate = WEIGHTED_RATE.x,
             Y1_Natl_Avg_Rate = NATIONAL_AVG_RATE.x,
             Y2 = Year.y,
             Y2_Crude_Rate = Crude.Rate.y,
             Y2_Weighted_Rate = WEIGHTED_RATE.y,
             Y2_Natl_Avg_Rate = NATIONAL_AVG_RATE.y) %>%
      mutate(STATE_CHANGE_PCT = Y2_Crude_Rate - Y1_Crude_Rate,
             NATIONAL_CHANGE_PCT = Y2_Natl_Avg_Rate - Y1_Natl_Avg_Rate) %>%
      mutate(CHANGE_DIFF = NATIONAL_CHANGE_PCT - STATE_CHANGE_PCT) %>%
      mutate(COMPARISON = ifelse(CHANGE_DIFF > 0, 'Faster', 'Slower'))
    
    
    fig2 <- plot_ly(data = final, 
                   x = ~CHANGE_DIFF, y = ~reorder(State, CHANGE_DIFF),
                   type = 'bar',
                   orientation = 'h',
                   color = ~COMPARISON,
                   colors = c("forestgreen", "red"))
    
    fig2 %>% layout(title = paste0('Mortality Rate Change compared to National Average'),
                   xaxis = list(title = "Percent Above (+) or Below (-) Average"),
                   yaxis = list (title = "State"))
    
  })
  
  
}

#################################  Run the application 
shinyApp(ui = ui, server = server)
