library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(readxl)
library(repurrrsive)
library(testthat)
library(tidyverse)
library(dplyr)
library(usmap)
library(ggplot2)

app = dash_app()



universities_tuition <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv", delim = ",")
universities_salary <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv",delim=",")

df_filtered <- universities_tuition
salary_filtered <- universities_salary
merged_df <- merge(df_filtered, salary_filtered, by = 'name')
#Right Join: jointdataset <- merge(ChickWeight, LabResults, by = 'Diet', all.y=TRUE)


# national average instate_total tuition
nation_avg_instate <- mean(df_filtered$in_state_total)

# national average early_career_pay for salary
nation_avg_early <- mean(salary_filtered$early_career_pay)

# national average mid_career_pay for salary
nation_avg_mid <- mean(salary_filtered$mid_career_pay)

# state name vector
state <- unique(universities_tuition$state)[-28]


# plot function for tuition
plot_bar_tuition <- function(df){
    plot <- ggplotly(ggplot(df, aes(x=Type, y=Tuition, fill=Type)) + 
                    geom_bar(stat = "identity") +
                    scale_fill_hue(c = 40) +
                    theme(legend.position="none"))
    plot
}

# plot function for salary
plot_bar_salary <- function(df){
    plot <- ggplotly(ggplot(df, aes(Type, Salary)) +   
                    geom_bar(aes(fill = Career_Stage), position = "dodge", stat="identity") +
                    scale_fill_hue(c = 40) +
                    theme(legend.position="none"))
    plot
}

# transform state into proper format for dccDropdown options
state_list <- lapply(
    state,
    function(state) {
        list(label = state,
        value = state)
  }
)

## Dashboard component

# dropdown control for School Type
dropdown_school_type <- dccDropdown(
      id = 'school_type',
      options = list(list(label = 'Private', value = 'Private'),
                    list(label = 'Public', value = 'Public'),
                    list(label = 'For Profit', value = 'For Profit')),
      value = 'Private'
    )

# dropdown control for Degree Length
dropdown_degree_length <- dccDropdown(
      id = 'degree_length',
      options = list(list(label = '4 Year', value = '4 Year'),
                    list(label = '2 Year', value = '2 Year')),
      value = '4 Year'
    )

# dropdown control for State
dropdown_state <- dccDropdown(
      id = 'state',
      options = state_list,
      value = 'California'
    )

# range slider for tuition
range_slider_tuition <- dccRangeSlider(
    id='range-slider-tuition',
    min=0,
    max=80000,
    step=10000,
    value=list(0, 80000),
    marks=list(
        "0" = "0",
        "10000" = "10000",
        "20000" = "20000",
        "30000" = "30000",
        "40000" = "40000",
        "50000" = "50000",
        "60000" = "60000",
        "70000" = "70000",
        "80000" = "80000"
    )
)

# tuition plot
tuition_plot <- dccGraph(
        id = 'tuition_plot'
        )

# salary plot
salary_plot <- dccGraph(
        id = 'salary_plot'
        )


# app
app$layout(
  htmlDiv(
      list(
          htmlLabel('School Type'),
          dropdown_school_type,
          htmlLabel('Degree Length'),
          dropdown_degree_length,
          htmlLabel('State'),
          dropdown_state,
          htmlLabel('Tuition Range'),
          range_slider_tuition,
          tuition_plot,
          salary_plot
      ),
      style = list(width = '30%', display = 'inline-block')
  )
)


# Callback function

## Callback functions for tuition chart
app$callback(
  output('tuition_plot', 'figure'),
  list(
      input(id='school_type', 'value'),
      input(id='degree_length', 'value'),
      input(id='state', 'value'),
      input(id='range-slider-tuition', 'value')
   ),
  function(school_type, degree_length_passed_in, state_passed_in, range_value) {

    newdata <- df_filtered %>%
            filter(type == school_type & degree_length == degree_length_passed_in 
                & state == state_passed_in & in_state_total >= range_value[[1]] & in_state_total <= range_value[[2]])

    if (length(newdata$in_state_total) == 0) {
        in_state_total_avg <- 0
    }
    else {
        in_state_total_avg <- mean(newdata$in_state_total)
    }

    new_df <- data.frame(Type = c("National Average", "State Average", "This School"),
                        Tuition = c(nation_avg_instate, in_state_total_avg, 0))

    
    plot_bar_tuition(new_df)
  }
  
)

## Callback functions for salary chart
app$callback(
  output('salary_plot', 'figure'),
  list(
      input(id='school_type', 'value'),
      input(id='degree_length', 'value'),
      input(id='state', 'value'),
      input(id='range-slider-tuition', 'value')
   ),
  function(school_type, degree_length_passed_in, state_passed_in, range_value) {

    newdata <- merged_df %>%
            filter(type == school_type & degree_length == degree_length_passed_in 
                & state == state_passed_in & in_state_total >= range_value[[1]] & in_state_total <= range_value[[2]])

    # check if data exists
    if (length(newdata$early_career_pay) == 0) {
        early_career_pay <- 0
    }
    else {
        early_career_pay <- mean(newdata$early_career_pay)
    }

    if (length(newdata$mid_career_pay) == 0) {
        mid_career_pay <- 0
    }
    else {
        mid_career_pay <- mean(newdata$mid_career_pay)
    }

    new_df <- data.frame(Type = c("National Average", "State Average", "This School"),
                        Early.Career = c(nation_avg_early, early_career_pay, 0),
                        Mid.Career = c(nation_avg_mid, mid_career_pay, 0)
                        )
    data_longer <- new_df %>% 
                    pivot_longer(cols = 2:3, names_to = 'Career_Stage', values_to = 'Salary')
    

    plot_bar_salary(data_longer)
  }
  
)


app$run_server(debug = T)





