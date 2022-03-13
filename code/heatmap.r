library(plotly)
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(readxl)
library(repurrrsive)
library(testthat)
library(tidyverse)
library(usmap)
library(ggplot2)

app <- dash_app()

universities_tuition <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv", delim = ",")
universities_salary <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv",delim=",")

universities_tuition <- rename(universities_tuition , Name = `name`,
                   State = `state`,
                   State_code = `state_code`,
                   Type = `type`,
                   Degree_length = `degree_length`,
                   Room_and_Board = `room_and_board`,
                   Instate_tuition = `in_state_tuition`,
                   Instate_total = `in_state_total`,
                   Outstate_tuition = `out_of_state_tuition`,
                   Outstate_total = `out_of_state_total`)

universities_salary <- rename(universities_salary , Rank = `rank`,
                   Name = `name`,
                   State_name = `state_name`,
                   Early_career_pay = `early_career_pay`,
                   mid_career_pay = `mid_career_pay`,
                   make_world_better_percent = `make_world_better_percent`,
                   stem_percent = `stem_percent`)


universities_tuition <- universities_tuition %>% 
na.omit(universities_tuition)

universities_salary <- universities_salary %>% 
na.omit(universities_salary)

state_tuition <- universities_tuition %>%
group_by(State) %>%
summarise(Instate_tuition = round(mean(Instate_tuition), 0),Outstate_tuition = round(mean(Outstate_tuition), 0))

state_salary <- universities_salary %>%
group_by(State_name) %>%
summarise(Early_career_pay = round(mean(Early_career_pay), 0),mid_career_pay = round(mean(mid_career_pay), 0))

state_tuition <- na.omit(state_tuition)
state_tuition <- state_tuition %>% add_row(State = 'District of Columbia', Instate_tuition = 6152, Outstate_tuition = 26045, .before = 9)

state_salary <- na.omit(state_salary)
state_salary <- state_salary %>% add_row(State_name = 'District of Columbia', Early_career_pay = 80439, mid_career_pay = 106800, .before = 9)

statepop$Instate_tuition <- state_tuition$Instate_tuition
statepop$Outstate_tuition <- state_tuition$Outstate_tuition
statepop$Early_career_pay <- state_salary$Early_career_pay
statepop$mid_career_pay <- state_salary$mid_career_pay
statepop <- data.frame(statepop)

available_options <- names(statepop)[5:8]

map_option_choice <- lapply(
  available_options,
  function(available_option) {
    list(label = available_option,
         value = available_option)
  }
)

fig <- ggplotly(plot_usmap(data = statepop, values = "mid_career_pay", color = "red") + 
         scale_fill_continuous(
    low = "white", high = "red", name = "mid_career_pay", label = scales::comma
  ) + theme(legend.position = "right"))

app %>% set_layout(
  div(
    dccDropdown(
      id = 'map_choice',
      options = map_option_choice,
      value = 'Instate_tuition'
    ),
    dccGraph(
        id = 'map_us_plot'
    ),
    style = list(width = '30%', display = 'inline-block')
  )
)

app %>% add_callback(
  output('map_us_plot', property = 'figure'),
  input('map_choice', 'value'),

  function(map_value) {
    figure <- ggplotly(plot_usmap(data = statepop, values = map_value, color = "black") + 
         scale_fill_continuous(
    low = "white", high = "blue", name = map_value, label = scales::comma
  ) + theme(legend.position = "right"))

    figure
  }
)


app$run_server(debug=TRUE, dev_tools_hot_reload=FALSE)