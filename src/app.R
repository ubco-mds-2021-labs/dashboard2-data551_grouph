library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)

library(readxl)
library(repurrrsive)
library(testthat)
library(usmap)
library(ggplot2)
library(dplyr)
library(stringr)
library(jsonlite)

## load data
universities_tuition <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv", delim = ",")
universities_salary <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv",delim=",")

df <- universities_tuition
df <- tibble::rowid_to_column(df, "index1")

df_filtered <- df
salary_filtered <- universities_salary
merged_df <- merge(df_filtered, salary_filtered, by = 'name')

## dropdown and barcharts

# national average instate_total tuition
nation_avg_instate <- mean(df_filtered$in_state_total)

# national average early_career_pay for salary
nation_avg_early <- mean(salary_filtered$early_career_pay)

# national average mid_career_pay for salary
nation_avg_mid <- mean(salary_filtered$mid_career_pay)

# state name vector
state <- unique(universities_tuition$state)[-28]
state <- str_sort(state)



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




## heat map

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



## school list
generate_school_items <- function(filtered_school_df) {
  school_nrow <- nrow(filtered_school_df)
    schoolitems <- list()
    for (i in 1:school_nrow) {
      cur_row <- filtered_school_df[i, ]
      # tempdiv <- div(cur_row$name)
      # print(as.integer(cur_row$index1))

      cur_btn <- div(
            list(
                dbcButton("Select", size = "sm", id = list(
                    "type" = "selectbtn",
                    "index" = as.integer(cur_row$index1)
                  ), value = i
                )
            ),
            className = "d-grid gap-2 d-md-flex justify-content-md-end"
      )
      # print(as.integer(cur_row$index1))

      if (cur_row$type == "Public") {
        schooltype_color <- "text-success"
      } else if (cur_row$type == "Private") {
        schooltype_color <- "text-warning"
      } else {
        schooltype_color <- "text-info"
      }
      if (is.na(cur_row$room_and_board) || cur_row$room_and_board == ''){
        cur_row$room_and_board <- "nan"
      }      

      schoolitem <- dbcListGroupItem (
            list(
              div(
                list(
                  html$h5(cur_row$name, className = "mb-1"),
                  html$small(cur_row$type, className = "text-success")
                ),
                className = "d-flex w-100 justify-content-between"
              ),
              dbcRow(
                    list(
                        dbcCol(div(list(
                            html$small(list(
                                html$span("Room and Board: ", className="text-muted"), # nolint
                                as.character(cur_row$room_and_board)
                            ), className = "mb-1")
                        ))),
                        dbcCol(div(list(
                            html$small(list(
                                html$span("Instate Tuition: ", className="text-muted")
                            ), className = "mb-1"),
                            html$small(list(
                                as.character(cur_row$in_state_tuition)
                            ), className="mb-1")
                        ))),
                        dbcCol(div(list(
                            html$small(list(
                                html$span("Outstate Tuition: ", className="text-muted")
                            ), className="mb-1"),
                            html$small(list(
                                as.character(cur_row$out_of_state_tuition)
                            ), className="mb-1")
                        ))),
                        dbcCol(div(list(
                            html$small(list(
                                html$span("Instate Total:  ", className="text-muted"),
                                as.character(cur_row$in_state_total)
                            ), className="mb-1")
                        ))),
                        dbcCol(div(list(
                            html$small(list(
                                html$span("Outstate Total: ", className="text-muted"),
                                as.character(cur_row$out_of_state_total)
                            ), className="mb-1")
                        ))),
                        dbcCol(div(list(
                            cur_btn
                        )))
                    ), style = list(marginTop = "8px"), align="end"
                )
            )
          )
      schoolitems[[i]] <- schoolitem
    }
    schoolitems
}
## heat map
map_dropdown <- dccDropdown(
      id = 'map_choice',
      options = map_option_choice,
      value = 'Instate_tuition',
      style=list(width = '250px')
    )

plot_map <- dccGraph(
        id = 'map_us_plot',
        style=list(width = '100%', height = '300px')
    )

## dropdown and barchart

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
        id = 'tuition_plot',
        style=list(width = '400px', height = '350px')
        )

# salary plot
salary_plot <- dccGraph(
        id = 'salary_plot',
        style=list(width = '400px', height = '350px')
        )




## components

## barchart component

component_barchart <- dbcCard(list(
        # Tuition and Salary bar plot
        dbcRow(
        list(
            dbcCol(tuition_plot, width="auto"),
            dbcCol(salary_plot, width="auto")
            
            )
        )
),style = list(width = "870px", height = "400px", marginTop = "20px"))

## component dropdown

component_control <- dbcCard(list(
       # Dropdown menu for School Type, Degree length, State
        htmlDiv(
            list(
                htmlLabel('School Type'),
                dropdown_school_type,
                htmlLabel('Degree Length'),
                dropdown_degree_length,
                htmlLabel('State'),
                dropdown_state
            ),
            style = list(margin = "auto", width = "440px", marginTop = "20px")
        ),
        # # Tuition Range Slider
        div(list(
            htmlLabel('Tuition Range'),
            range_slider_tuition
        ),style = list(margin = "auto", width = "440px", marginTop = "5px"))
), style = list(width = "500px", height = "400px", marginTop = "20px"))


## component schoollist
component_schoollist <- dbcCard(list(
  h4("School List", style = list(margin = "auto", width = "830px", marginTop = "12px")),
  html$hr(style = list(margin = "10px 10px")),
  div(list(
    dbcListGroup(id = "schoollist", children = list()) #generate_school_items(df)
  ), style = list(overflow = "auto", height = "320px", width = "840px", margin = "auto"))

), style = list(width = "870px", height = "400px", marginTop = "20px"))

# component_heatmap = dbc

component_heatmap <- dbcCard(list(

        dbcRow(list(
            dbcCol(html$h4("US Heat Map"), width="auto"),
            dbcCol(map_dropdown, width="auto")
        ), justify="between", style = list(margin = "auto", width = "480px", marginTop = "12px")),
        dbcRow(list(
            html$hr(style = list(margin = "10px 10px", width = "460px"))
        ), style = list(margin = "auto", width = "480px")),
        dbcRow(dbcCol(plot_map))

), style = list(width ="500px", height = "400px", marginTop = "20px"))


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
# app <- dash_app()
comp_h1 <- h1('Hello Dash')
app %>% set_layout(
    dbcRow(
        list(
            dbcCol(component_heatmap, width="auto"),
            dbcCol(component_schoollist, width="auto")
            
        ), style = list("margin" = "auto", "width" = "1440px")
    ),
    dbcRow(
        list(
            dbcCol(component_control, width="auto"),
            dbcCol(component_barchart, width="auto")
            
        ), style = list("margin" = "auto", "width" = "1440px")
    )
)


# Callback function

app$callback(
  output('schoollist', 'children'),
  list(
      input(id='school_type', 'value'),
      input(id='degree_length', 'value'),
      input(id='state', 'value'),
      input(id='range-slider-tuition', 'value')
   ),
  function(school_type, degree_length_passed_in, state_passed_in, range_value) {

    filtered_df <- df %>%
            filter(type == school_type & degree_length == degree_length_passed_in 
                & state == state_passed_in & in_state_total >= range_value[[1]] & in_state_total <= range_value[[2]])

    generate_school_items(filtered_df)
  }
  
)


## Callback functions for tuition chart
app$callback(
  output('tuition_plot', 'figure'),
  list(
      input(id='school_type', 'value'),
      input(id='degree_length', 'value'),
      input(id='state', 'value'),
      input(id='range-slider-tuition', 'value'),
      input(id = list("index" = ALL, "type" = "selectbtn"), property = "n_clicks")
   ),
  function(school_type, degree_length_passed_in, state_passed_in, range_value,args) {
    schoolid <- -1
    ctx <- app$callback_context()
    if (length(ctx$triggered) == 2){
        # print(ctx$triggered)
        # print(length(ctx$triggered))
        jsonstr <- unlist(strsplit(ctx$triggered$prop_id, "[.]"))[1]
        if (grepl("index", jsonstr, fixed = TRUE)) {
            toJson <- fromJSON(jsonstr)
            schoolid <- toJson$index
        }
    }
    # print(ctx$triggered)
    # print(length(ctx$triggered$prop_id))
    # print(length(ctx$triggered))


    newdata <- df_filtered %>%
            filter(type == school_type & degree_length == degree_length_passed_in 
                & state == state_passed_in & in_state_total >= range_value[[1]] & in_state_total <= range_value[[2]])

    if (schoolid == -1){
        cur_school_tuition <- 0
    } else {
        schoolindex <- schoolid
        # cur_school_tuition <- 0
        cur_school_tuition_row <- newdata[newdata["index1"] == schoolindex,]
        # print(cur_school_tuition_row)
        if (nrow(cur_school_tuition_row) == 0){
            cur_school_tuition <- 0
        } else {
            cur_school_tuition <- cur_school_tuition_row$in_state_total
        }
    }
        

    if (length(newdata$in_state_total) == 0) {
        in_state_total_avg <- 0
    }
    else {
        in_state_total_avg <- mean(newdata$in_state_total)
    }

    new_df <- data.frame(Type = c("National Average", "State Average", "This School"),
                        Tuition = c(nation_avg_instate, in_state_total_avg, cur_school_tuition))

    
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
      input(id='range-slider-tuition', 'value'),
      input(id = list("index" = ALL, "type" = "selectbtn"), property = "n_clicks")
   ),
  function(school_type, degree_length_passed_in, state_passed_in, range_value,args) {
    
    schoolid <- -1
    ctx <- app$callback_context()
    if (length(ctx$triggered) == 2){
        # print(ctx$triggered)
        # print(length(ctx$triggered))
        jsonstr <- unlist(strsplit(ctx$triggered$prop_id, "[.]"))[1]
        if (grepl("index", jsonstr, fixed = TRUE)) {
            toJson <- fromJSON(jsonstr)
            schoolid <- toJson$index
        }
    }

    newdata <- merged_df %>%
            filter(type == school_type & degree_length == degree_length_passed_in 
                & state == state_passed_in & in_state_total >= range_value[[1]] & in_state_total <= range_value[[2]])

    # check if data exists

    if (schoolid == -1){
        cur_early_career_pay <- 0
        cur_mid_career_pay <- 0
    } else {
        schoolindex <- schoolid
        # cur_school_tuition <- 0
        cur_early_career_pay_row <- newdata[newdata["index1"] == schoolindex,]
        # print(cur_school_tuition_row)
        if (nrow(cur_early_career_pay_row) == 0){
            cur_early_career_pay <- 0
        } else {
            cur_early_career_pay <- cur_early_career_pay_row$early_career_pay
        }

        cur_mid_career_pay_row <- newdata[newdata["index1"] == schoolindex,]
        # print(cur_school_tuition_row)
        if (nrow(cur_mid_career_pay_row) == 0){
            cur_mid_career_pay <- 0
        } else {
            cur_mid_career_pay <- cur_mid_career_pay_row$mid_career_pay
        }
    }

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
                        Early.Career = c(nation_avg_early, early_career_pay, cur_early_career_pay),
                        Mid.Career = c(nation_avg_mid, mid_career_pay, cur_mid_career_pay)
                        )
    data_longer <- new_df %>% 
                    pivot_longer(cols = 2:3, names_to = 'Career_Stage', values_to = 'Salary')
    

    plot_bar_salary(data_longer)
  }
  
)


## heat map callback
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





app$run_server(debug = T)
# app %>% run_app()