# Student Name: Edward Duffy
# Student Number: R0025726
# Date Start: 28/11/2023
# Module: Introduction to R STAT8010
# Shiny App Task

# Install libraries
#install.packages('readr')
#install.packages('dplyr')
#install.packages('shiny')

# libraries
library(readr)
library(dplyr)
library(shiny)

# Reading in the dataset
covid_dataset = read.csv('COVID-19_HPSC_Detailed_Statistics_Profile.csv')

# View the dataset
View(covid_dataset)

# Display the column names
colnames(covid_dataset)

# Task 1: a. Filter the dataset to contain only those variables listed above, removing all others
variables_of_interest = c("Date","ConfirmedCovidCases",
                          "TotalConfirmedCovidCases",
                          "ConfirmedCovidDeaths",
                          "TotalCovidDeaths",
                          "StatisticsProfileDate",
                          "HospitalisedCovidCases",
                          "RequiringICUCovidCases",
                          "HealthcareWorkersCovidCases",
                          "Male",
                          "Female",
                          "SevenDayAvg_Cases")


# Filtering the dataset so we have the important variables labelled in the question sheet
covid_dataset_filtered = covid_dataset[variables_of_interest]

# Viewing the filtered dateset
View(covid_dataset_filtered)

# Renaming the columns so they fit the naming convention as requested on the assessment
names(covid_dataset_filtered)[names(covid_dataset_filtered) == "StatisticsProfileDate"] = "StatisticProfileDate"
names(covid_dataset_filtered)[names(covid_dataset_filtered) == "SevenDayAvg_Cases"] = "SevenDayAverageCases"

# Viewing the filtered covid dataset
View(covid_dataset_filtered)

# looking through the column names of the filtered dataset
colnames(covid_dataset_filtered)

# Task 1 b)  As the first few observations contain various NAs and other inconsistencies, 
#remove the first two rows so that the Date variable begins at 3rd March 2020. 
#Ensure both Date and StatisticProfileDate are a Date-type format within the dataframe/tibble.

# using str() to look at the datatypes of the variables within the dataset
str(covid_dataset_filtered)

# Removing the first two rows
covid_dataset_filtered = covid_dataset_filtered[-c(1,2), ]

# head of data to make sure removal was correct
head(covid_dataset_filtered)

# Getting the first few entries of the 'Date' and 'StatisticsProfileDate'
head(covid_dataset_filtered$Date)
head(covid_dataset_filtered$StatisticProfileDate)

# Coverting the 'Date' and 'StatisticsProfileDate' to the Date-type format
covid_dataset_filtered$Date = as.Date(covid_dataset_filtered$Date)
covid_dataset_filtered$StatisticProfileDate = as.Date(covid_dataset_filtered$StatisticProfileDate)

# using the str() method to make sure that its been converted to a date-type
str(covid_dataset_filtered)

# printing the head / first few rows to make sure that we got the desired format
head(covid_dataset_filtered)

# printing the head
# View the format of the 'Date' and 'StatisticProfileDate' columns
print('Date column')
head(covid_dataset_filtered$Date)
print('Statistics profile date column')
head(covid_dataset_filtered$StatisticProfileDate)

# Summary of the dates
#"2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" "2020-03-07" "2020-03-08"
#print('Statistics profile date column')
#"Statistics profile date column"
#head(covid_dataset_filtered$StatisticProfileDate)
#"2020-03-02" "2020-03-03" "2020-03-04" "2020-03-05" "2020-03-06" "2020-03-07"

# Task 1 c) 
#You will notice that there is a slight difference/lag between these two date variables. 
#Check the information at the above link to see what variables each date column applies to. 
#Fix the StatisticProfileDate and associated variables to also begin on 3rd March 2020 and 
#be consistent with the Date variable. Then, once all variables are aligned correctly with 
#Date, remove the StatisticProfileDate variable, as it should now be an exact copy of Date. 

# Writing my csv filtered file
#write.csv(covid_dataset_filtered, "covid_dataset_filtered.csv", row.names = FALSE)

# Split the dataset into two parts
date_part = covid_dataset_filtered %>%
  select(Date, ConfirmedCovidCases, TotalConfirmedCovidCases, ConfirmedCovidDeaths, TotalCovidDeaths)

statistic_part = covid_dataset_filtered %>%
  select(StatisticProfileDate, HospitalisedCovidCases, RequiringICUCovidCases, HealthcareWorkersCovidCases, Male, Female, SevenDayAverageCases)

# Shift the 'StatisticProfileDate' part by one row
statistic_part = statistic_part[-1, ]

# Combine the two parts
combined_dataset = cbind(date_part[-nrow(date_part), ], statistic_part)

# Ensure the dataset starts from 2020-03-03
combined_dataset = combined_dataset[combined_dataset$Date >= as.Date("2020-03-03"), ]

# printing the first few rows of the dataset using head()
head(combined_dataset)

# View the first row of the transformed dataset
head(combined_dataset, 1)

# Viewing the dataset
View(combined_dataset)


# Task 1)  D
#Some NA entries remain in the ConfirmedCovidDeaths column, 
#due to the nature and frequency of recording. 
#To deal with this for visualisation purposes, 
#set all NA values in this column to be 0. 
#Check for NAs elsewhere in the dataframe and deal with them appropriately, if present. 

# Replacing NA values in the 'ConfirmedCovidDeaths' with the value of 0
combined_dataset$ConfirmedCovidDeaths[is.na(combined_dataset$ConfirmedCovidDeaths)] = 0

# Checking if NA's are elsewhere within the data set
na_checker = combined_dataset %>%
  summarise_all(~sum(is.na(.)))

# printing the results
# There are no N/A values remaining 
print(na_checker)

# Task 1 E) 
# The data values for HospitalisedCovidCases, HealthcareWorkersCovidCases,
# RequiringICUCovidCases, Male and Female are all given as cumulative measurements, 
# rather than daily counts. 
# Create new columns for the daily count values related to each of these. 

# Rows we need to change change are:
#HospitalisedCovidCases
#HealthcareWorkersCovidCases,
# RequiringICUCovidCases
# Male and Female 

# Getting the daily counts from cumulative measurements of the above columns
combined_dataset = combined_dataset %>%
  mutate(
    DailyHospitalisedCovidCases = c(NA, diff(HospitalisedCovidCases)),
    DailyHealthcareWorkersCovidCases = c(NA, diff(HealthcareWorkersCovidCases)),
    DailyRequiringICUCovidCases = c(NA, diff(RequiringICUCovidCases)),
    DailyMaleCovidCases = c(NA, diff(Male)),
    DailyFemaleCovidCases = c(NA, diff(Female))
  )

# view the new data set with these new daily columns
View(combined_dataset)

# NA's in first row of the new daily columns
combined_dataset$DailyHospitalisedCovidCases[1] = 0
combined_dataset$DailyHealthcareWorkersCovidCases[1] = 0
combined_dataset$DailyRequiringICUCovidCases[1] = 0
combined_dataset$DailyMaleCovidCases[1] = 0
combined_dataset$DailyFemaleCovidCases[1] = 0

# View the first few rows to confirm the change
head(combined_dataset)

# Both of the date columns align so we need to remove the statistics date column

combined_dataset = combined_dataset %>%
  select(-StatisticProfileDate)

# View the dataset with statistics profile date removed as reqeuested as both dates are the same
View(combined_dataset)

#write.csv(combined_dataset, "covid_dataset_combined.csv", row.names = FALSE)


# Task 2
# Build a Shiny app or dashboard, containing two tabs, the first of which contains a timeseries (line)
# plot (with Date on the x-axis) showing any user-selected variable. This first tab should contain the 
# following capabilities:

# A) The user should be able to specify the date range to be plotted. 

# B) It should contain a checkbox that when selected, allows the user to add up to two extra 
# variables to be shown in the TS plot. Note that each plotted variable should be distinctly 
# presented from each other (using colour, line-type, etc..) on the plot 
#and a legend should also be presented.

# C)  The title of the tab/plot should state how many variables are being plotted, along with the 
# variable names. The user should also be warned if any two selected variables are the 
# same. 

# D) The dates on the x-axis should be presentable for all possible selected date ranges; i.e. no 
#crossover of date labels for a reasonable window size and no fewer than five date ticks. 

# Building the Shiny app which will use the dataset we have filtered 'combined_dataset'


# Defining the UI / User Interface of the application
# Define the UI
# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)


# Explanation of UI

# This code defines the user interface (UI) for the Shiny app, 
# including two tabs: "Time Series Plot" and "Scatterplot."

#The first tab, "Time Series Plot," contains a sidebar layout with several UI elements:

# selectInput elements for choosing primary, secondary, and tertiary variables.
# dateRangeInput for selecting a date range.
# The mainPanel contains the plotOutput element where the time series plot will be displayed.

# The second tab, "Scatterplot," also has a sidebar layout:

# radioButtons are used for selecting the type of variables (Daily Count or Cumulative) for the first and second variables.
# uiOutput elements (variableSelect1 and variableSelect2) are dynamically generated based on the selected variable types.
# The mainPanel contains the plotOutput element where the scatterplot will be displayed.



ui = fluidPage(
  titlePanel("COVID-19 Data Dashboard - Edward Duffy - R00257226"),
  
  tabsetPanel(
    tabPanel("Time Series Plot",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable1", "Choose a primary variable:",
                             choices = colnames(combined_dataset)[-1]), # Exclude 'Date'
                 selectInput("variable2", "Select a secondary variable (optional):",
                             choices = c("None", colnames(combined_dataset)[-1])),
                 selectInput("variable3", "Select a tertiary variable (optional):",
                             choices = c("None", colnames(combined_dataset)[-1])),
                 dateRangeInput("dateRange", "Date range:",
                                start = min(combined_dataset$Date), 
                                end = max(combined_dataset$Date),
                                min = min(combined_dataset$Date), 
                                max = max(combined_dataset$Date))
               ),
               mainPanel(
                 plotOutput("timeSeriesPlot")
               )
             )
    ),
    tabPanel("Scatterplot",
             sidebarLayout(
               sidebarPanel(
                 radioButtons('variableType1','Please choose your first variable:',
                              choices = c('Daily Count', 'Cumulative')),
                 uiOutput('variableSelect1'),
                 radioButtons('variableType2','Please choose your second variable:',
                              choices = c('Daily Count','Cumulative')),
                 uiOutput('variableSelect2')
               ),
               mainPanel(
                 plotOutput("scatterPlot")
               )
             )
    )
  )
)


server = function(input, output, session) {
  # Using an observe block to be able to react to changes based on user inputs
  observe({
    # Getting the variables 1-3 the user selected and putting them in a vector
    selected_vars = c(input$variable1,input$variable2,input$variable3)
    
    # Remove the 'None' variable from the selected variables for the user
    selected_vars = selected_vars[selected_vars != 'None']
    
    # Warning message logic
    # If the user selects the same variable within the app
    # They get the following warning message:
    # Warning: Duplicate variables selected, please choose other variables for each selection!
    if(length(selected_vars)!= length(unique(selected_vars))) {
      showNotification('Warning: Duplicate variables selected, please choose other variables for each selection!')
    }
  })
  
  output$timeSeriesPlot = renderPlot({
    # Filter data based on selected date range
    filtered_data = combined_dataset %>%
      filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
    
    # Initialize the time series plot
    timeSeriesPlot = ggplot(filtered_data, aes(x = Date)) +
      theme_minimal()
    
    # Create a vector to store the names of the selected variables
    selected_vars = setdiff(c(input$variable1, input$variable2, input$variable3), "None")
    
    # Define the colors for the lines of our plot
    line_colors = c("blue", "red", "green")
    
    # Add lines for each selected variable and store their names for the title
    legend_names = c()
    
    # 1) check if the first variable is not None
    # 2) Add the dashed line to the plot
    # 3) Add the variable name to the legend_names vector
    # 4) Repeat this code for each of the three variables
    if(input$variable1 != "None") {
      timeSeriesPlot = timeSeriesPlot + 
        geom_line(aes(y = .data[[input$variable1]], color = input$variable1), size = 1)
      legend_names = c(legend_names, input$variable1)
    }
    if(input$variable2 != "None" && input$variable2 != input$variable1) {
      timeSeriesPlot = timeSeriesPlot + 
        geom_line(aes(y = .data[[input$variable2]], color = input$variable2), size = 1, linetype = "dashed")
      legend_names = c(legend_names, input$variable2)
    }
    if(input$variable3 != "None" && input$variable3 != input$variable1 && input$variable3 != input$variable2) {
      timeSeriesPlot = timeSeriesPlot + 
        geom_line(aes(y = .data[[input$variable3]], color = input$variable3), size = 1, linetype = "dotted")
      legend_names = c(legend_names, input$variable3)
    }
    
    # Set the colors for the legend based on the actual variable names
    timeSeriesPlot = timeSeriesPlot + 
      scale_color_manual(values = line_colors, labels = legend_names)
    
    # Create the plot title
    # If the user has selected variables, creare a title showing their names
    # Display a message to the user if they have not selected any variables to plot
    plot_title = if(length(legend_names) > 0) {
      paste(length(legend_names), 'Variables Plotted:', paste(legend_names, collapse = ', '))
    } else {
      'No variables selected'
    }
    
    # Generate the final plot with a title and a legend
    timeSeriesPlot + labs(title = plot_title, x = "Date", y = "Value", color = "Variable") +
      theme(legend.position = "bottom")
  })
  
  # This code dynamically generates the user interface (UI) 
  # selecting variables for a scatterplot based on the type of variables chosen (Daily Count or Cumulative).
  
  # output$variableSelect1 and output$variableSelect2 are UI elements 
  # that will display select input fields for the first and second variables, respectively.
  
  #The renderUI function generates the UI dynamically based on the logic provided within it.
  
  #Inside each renderUI block, 
  #there is an if statement that checks whether input$variableType1 and input$variableType2 (types of variables selected by the user) 
  # are set to 'Daily Count'. 
  #If 'Daily Count' is selected, it provides a selection list with daily count variables; otherwise, 
  #it provides a selection list with cumulative variables.
  
  #The selectInput function is used to create a dropdown list of variable choices, 
  # the label and available choices depend on the selected variable type.
  output$variableSelect1 = renderUI({
    if(input$variableType1 == 'Daily Count'){
      selectInput('scatterVariable1', 'Select the first variable for your plot',
                  choices = c('ConfirmedCovidCases', 'ConfirmedCovidDeaths',
                              'DailyHospitalisedCovidCases', 'DailyRequiringICUCovidCases',
                              'DailyHealthcareWorkersCovidCases', 'DailyMaleCovidCases',
                              'DailyFemaleCovidCases', 'SevenDayAverageCases'))
    } else {
      selectInput('scatterVariable1', 'Select the first variable for your plot',
                  choices = c("TotalConfirmedCovidCases", "TotalCovidDeaths",
                              "HospitalisedCovidCases", "RequiringICUCovidCases",
                              "HealthcareWorkersCovidCases", "Male", "Female"))
    }
  })
  
  output$variableSelect2 = renderUI({
    if(input$variableType2 == 'Daily Count'){
      selectInput('scatterVariable2', 'Select the second variable for your plot',
                  choices = c('ConfirmedCovidCases', 'ConfirmedCovidDeaths',
                              'DailyHospitalisedCovidCases', 'DailyRequiringICUCovidCases',
                              'DailyHealthcareWorkersCovidCases', 'DailyMaleCovidCases',
                              'DailyFemaleCovidCases', 'SevenDayAverageCases'))
    } else {
      selectInput('scatterVariable2', 'Select the second variable for your plot',
                  choices = c("TotalConfirmedCovidCases", "TotalCovidDeaths",
                              "HospitalisedCovidCases", "RequiringICUCovidCases",
                              "HealthcareWorkersCovidCases", "Male", "Female"))
    }
  })
  
  # EXPLANATION
  # This code defines the logic for rendering a scatterplot based on user-selected variables and types.
  
  # output$scatterPlot displays the scatterplot.
  
  # Inside the renderPlot function, there are the following conditional checks:
  
  # 1) The first if statement checks if the selected variable types are different (Cumulative vs. Daily Count). 
  # If they are different, a warning notification is displayed, and the function returns early.
  
  # 2) The second set of conditional checks ensures that the selected variables for the scatterplot are valid:
  # checking if the selected variables are the same, 
  # if either of them is set to "None," or if both conditions are met. 
  
  # If any of these conditions are true, a warning notification is displayed, and the function returns early.
  # If all conditions are met, the code proceeds to create the scatterplot using ggplot2:
  
  output$scatterPlot = renderPlot({
    if(input$variableType1 != input$variableType2) {
      showNotification('Warning: Different types of variables selected (Cumulative & Daily Count). Please select variables of the same type!', type='warning')
      return()
    }
    
    if(input$scatterVariable1 == input$scatterVariable2 || 
       input$scatterVariable1 == "None" || 
       input$scatterVariable2 == "None") {
      showNotification('Please select two different variables for your scatterplot!', type='warning')
      return()
    }
    
    ggplot(combined_dataset, aes_string(x = input$scatterVariable1, y = input$scatterVariable2)) +
      geom_point() +
      labs(x = input$scatterVariable1, y = input$scatterVariable2, title = 'Scatterplot') +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)





