
##################### Libraries & variables
library(tidyverse)
library(ggplot2)
file <- 'QHP_Landscape_Individual_Market_Dental.csv'

##################### 1
# 1. Write a function to read in all rows between any row X and any row Y in any csv file. 
# The inputs are X, Y, and the name of the CSV file.

readFile <- function(x,y,fileName){
  
  data <- data.frame(read_csv(fileName))
  data <- data[x:y,]
  
  return(data)
}

##################### 2
# read and return data between rows 100 and 1000 (both inclusive) in the CSV file you downloaded from the link provided.
df <- readFile(100, 1000, file)

View(df)
nrow(df)

# select and return only the first 10 rows and the last 8 rows of the datasets from step above.
df2 <- df %>%
  mutate(ROW_ID = seq.int(nrow(.))) %>%
  filter(ROW_ID <= 10 | ROW_ID > nrow(.)-8) 

View(df2)
nrow(df2)

##################### 3
# Now let us repeat the same process but using a different method. Read the full dataset and then in one line (command):

baseData <- data.frame(read_csv(file))

# subset between rows 100 and 1000 (both inclusive) and store the dataframe in a variable df_tmp
df_tmp <- baseData[100:1000,]

View(df_tmp)
nrow(df_tmp)

# subset only the first 10 rows and the last 8 rows if df_tmp and store the dataframe in a variable df_tmp2
df_tmp2 <- df_tmp %>%
  mutate(ROW_ID = seq.int(nrow(.))) %>%
  filter(ROW_ID <= 10 | ROW_ID > nrow(.)-8) %>%
  select(-ROW_ID)

View(df_tmp2)
nrow(df_tmp2)

##################### 4
# Generate an ordered list of the top 5 issuers (Issuer.Name) in df_tmp2 in terms of their occurrence frequency 
# and store the results as a dataframe df_tmp3.

df_tmp3 <- df_tmp2 %>%
  group_by(Issuer.Name) %>%
  summarize(COUNT = n()) %>%
  arrange(desc(COUNT)) %>%
  mutate(ROW_ID = seq.int(nrow(.))) %>%
  filter(ROW_ID <= 5) %>%
  ungroup()

df_tmp3


##################### 5
# 'Delta Dental Insurance Company' and 'Delta Dental of Alaska' have merged into one company called 'Delta Dental'. 
# Reflect this change in df_tmp2 and recalculate top 5 issuers from step 4 using the new merger information. 
# Store the results in dataframe df_tmp4. How would you communicate this new change to your stakeholder using visuals/plot(s)?

df_tmp4 <- df_tmp2 %>%
  mutate(Issuer.Name = ifelse(Issuer.Name == 'Delta Dental Insurance Company' | Issuer.Name == 'Delta Dental of Alaska', 
         'Delta Dental', Issuer.Name)) %>%
  group_by(Issuer.Name) %>%
  summarize(COUNT = n()) %>%
  arrange(desc(COUNT)) %>%
  mutate(ROW_ID = seq.int(nrow(.))) %>%
  filter(ROW_ID <= 5) %>%
  ungroup()

df_tmp4

##################### 6
# In column Child.Only.Offering replace part of the text 'Child-Only' with 'cHILD' and 'Adult' to 'Adult-As_well' 
# store this as a new dataframe df_tmp5.

df_tmp5 <- df_tmp2 %>%
  mutate(Child.Only.Offering = gsub('Adult', 'Adult-As_well',gsub('Child-Only', 'cHILD', Child.Only.Offering)))

View(df_tmp5['Child.Only.Offering'])

##################### 7
# For every one of the last 8 columns in df_tmp2, modify the text to remove any non-numeric values and keep only 
# the numeric ones. For cells where there are no numeric values, put in 0.

numCol <- ncol(df_tmp2)
cols <- colnames(df_tmp2[, (numCol-7): numCol])

changeVals <- function(col){
  x <- as.numeric(gsub('[^0-9.]','',col))
  x <- ifelse(is.na(x), 0, x)
  return(x)
}

df_tmp2[cols] <- sapply(df_tmp2[cols],changeVals)
View(df_tmp2[cols])

# Bonus Question 1: Write a function that programmatically downloads the CSV from the data source, saves it in a 
# desired location and performs step 3. The inputs to the function can be the http link, and the row indices, and the output has 
# to be a dataframe/table.

getData <- function(link, x, y){
  df <- data.frame(read.csv(link))
  df <- df[x:y,]
  write.csv(df, 'dataFile.csv', row.names = FALSE)
  return(df)
}

#link <- 'https://data.healthcare.gov/widgets/cpmk-xcsy'
link <- 'https://data.healthcare.gov/api/views/cpmk-xcsy/rows.csv?accessType=DOWNLOAD&bom=true&format=true&sorting=true'
getData(link,1,10)

# Bonus Question 2: Create an html page(python/flask or R-shiny) which displays the entire csv data in a paginated table showing 
# 10 rows at a time. You can use any existing library to render this table.

library(shiny)
library(DT)

ui <- fluidPage(
  dataTableOutput("tbl")
)

server <- function(input, output) {
  
  output$tbl <- DT::renderDataTable({
    DT::datatable({
      baseData},
      rownames= FALSE)})
}

shinyApp(ui, server)

# Bonus Question 3: Create an api server which takes "State" as an endpoint input and outputs all unique "Plan Marketing Names" 
# for that state based on the csv data.