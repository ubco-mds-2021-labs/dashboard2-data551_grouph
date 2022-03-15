library(dash)
library(dashCoreComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(tidyverse)

## load data
universities_tuition <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv", delim = ",")
universities_salary <- read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv",delim=",")

df <- head(universities_tuition, 10)
df <- tibble::rowid_to_column(df, "index1")

df

## Plotting section

# p <- ggplot(msleep) +
#     aes(
#         x = bodywt,
#         y = sleep_total
#     ) +
#     geom_point() +
#     scale_x_log10()
# p

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
    
component_schoollist <- dbcCard(list(
  h4("School List", style = list(margin = "auto", width = "830px", marginTop = "12px")),
  html$hr(style = list(margin = "10px 10px")),
  div(list(
    dbcListGroup(id = "schoollist", children = generate_school_items(df))
  ), style = list(overflow = "auto", height = "320px", width = "840px", margin = "auto"))

), style = list(width = "870px", height = "400px", marginTop = "20px"))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)
# app <- dash_app()
comp_h1 <- h1('Hello Dash')
app %>% set_layout(
  dbcRow(
        list(
            dbcCol(div(), width="auto"),
            dbcCol(component_schoollist, width="auto")
            
        ), style = list("margin" = "auto", "width" = "1440px")
    )
  # dccGraph(figure=ggplotly(p))
)

app %>% run_app()