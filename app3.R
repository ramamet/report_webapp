library(shiny)
library(dplyr)
library(echarts4r)
library(survival)
library(survminer)
library(DT)

###?

#' linebreak function to avoid repeat use of br()
#'
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
linebreaks <- function(n){
    HTML(strrep(br(), n))
    }


# ----
#' get survfit summary data
#'
#' @param dat
#' @param time_var
#' @param event_var
#'
#' @return
#' @export
#'
#' @examples
#' get_surv_summary(lung, time, status)
get_surv_summary <-   function(dat, time_var, event_var) {
  # unquoting
  time_var <- enquo(time_var)
  event_var <- enquo(event_var)
  
  # data
  dat    <- dat %>%
    rename(time = (!!time_var),
           event = (!!event_var)) %>%
    dplyr::select(time, event)
  
  eq_0 <- glue::glue("survfit( ")
  eq_left <- glue::glue("Surv(time, event)")
  eq_tilde <- glue::glue("~")
  eq_right <- glue::glue(" 1 ")
  eq_data <- glue::glue(", dat)")
  
  comp_eq <-
    glue::glue(eq_0, eq_left, eq_tilde , eq_right, eq_data)
  
  
  # survfit
  #sfit <- survfit(Surv(time, event) ~ 1, data = data )
  
  sfit <- eval(rlang::parse_expr(comp_eq))
  
  # convert survfit modal to dataframe
  res <- surv_summary(sfit)
  
  # transform
  res_df <- res %>%
    as_tibble() |>
    mutate(surv = round(surv, 2))
  
  return(res_df)
  
}

##?

# sfit_df1 <- get_surv_summary(lung, time, status)
# sfit_df2 <- get_surv_summary(veteran, time, status)


# ----

#' Interactive kaplan-meier
#'
#' @param sfit_df
#'
#' @return
#' @export
#'
#' @examples
#' plot_km(sfit_df1)
plot_km <- function(dat) {
  # echarts line plot
  dat |>
    e_charts(time) |>
    e_line(surv)
  
}


# ----

#?
#' get surv group summary data with a categorical variable
#'
#' @param dat
#' @param time_var
#' @param event_var
#' @param grp_var
#'
#' @return
#' @export
#'
#' @examples
#' sfit_grp_df1 <- get_surv_grp_summary(lung, time, status, sex)
get_surv_grp_summary <-
  function(dat, time_var, event_var, grp_var) {
    # unquoting
    time_var <- enquo(time_var)
    event_var <- enquo(event_var)
    #grp_var <- enquo(grp_var)
    if (grp_var == 1) {
      grp_var <- 1
    } else{
      grp_var <- rlang::sym(grp_var)
    }
    
    
    # data
    df    <- dat %>%
      rename(time = (!!time_var),
             event = (!!event_var)) %>%
      dplyr::select(time, event, (!!grp_var))
    
    
    #
    eq_0 <- glue::glue("survfit( ")
    eq_left <- glue::glue("Surv(time, event)")
    eq_tilde <- glue::glue(" ~ ")
    eq_right <- glue::glue(grp_var)
    #eq_right <- paste(grp_var[2])
    eq_data <- glue::glue(", df)")
    
    comp_eq <-
      glue::glue(eq_0, eq_left, eq_tilde , eq_right, eq_data)
    
    
    # survfit
    sfit <- eval(rlang::parse_expr(comp_eq))
    
    # convert survfit modal to dataframe
    res <- surv_summary(sfit)
    
    # transform
    res_df <- res %>%
      as_tibble() |>
      mutate(surv = round(surv, 2))
    
    return(res_df)
    
  }

#
# sfit_grp_df1 <- get_surv_grp_summary(lung, time, status, sex)
# sfit_grp_df2 <- get_surv_grp_summary(veteran, time, status, trt)
# sfit_grp_df3 <- get_surv_grp_summary(veteran, time, status, 1)

# ----


#' Interactive kaplan-meier with a categorical variable
#'
#' @param dat
#'
#' @return
#' @export
#'
#' @examples
plot_km_group <- function(dat) {
  dat |>
    group_by(strata) |>
    e_charts(time) |>
    e_line(surv) |>
    e_tooltip(trigger = "axis")
  
  
}

# plot_km_group(sfit_grp_df1)


#!

#get_surv_censor <- function(dat){

#dat %>%
#        mutate(surv_censor = case_when((n.censor > 0) ~ surv,
#                                       (n.censor == 0) ~ NA_integer_,
#                                       TRUE ~ NA_integer_))
#}

#sfit_cens_df1 <- get_surv_censor(sfit_grp_df1)




# ----

#' Kaplan-Meier advanced plot
#'
#' @param dat
#' @param tit
#' @param sub_tit
#'
#' @return
#' @export
#'
#' @examples
plot_echart_km <-
  function(dat, tit = "Kaplan-Meier Plot", sub_tit = "data") {
    # check "strata" in data
    stopifnot("strata" %in% colnames(dat))
    
    
    # plot
    dat |>
      mutate(surv_censor = case_when((n.censor > 0) ~ surv,
                                     (n.censor == 0) ~ NA_integer_,
                                     TRUE ~ NA_integer_
      )) |>
      group_by(strata) %>%
      e_charts(time) |>
      e_line(
        surv,
        showSymbol = TRUE,
        symbolSize = 3,
        itemStyle = list(opacity = 0.6)
      ) |>
      e_line(
        surv_censor,
        showSymbol = TRUE,
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#7B7C81',
          color = '#91cc75',
          shadowColor = '#91cc75'
        ),
        symbol = 'triangle',
        symbolSize = 6,
        tooltip = list(show = FALSE),
        legend = list(show = FALSE)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_mark_line(
        data = list(yAxis = 0.5),
        y_index = 1,
        symbol = "none",
        lineStyle = list(type = 'dashed'),
        itemStyle = list(color = "#e76f51"),
        title = "50% threshold",
        label = list(position = "insideEndTop")
      ) |>
      e_title(text = tit, subtext = sub_tit) |>
      e_x_axis(
        axisLabel = list(fontSize = 12,  color = '#333'),
        name = "Time",
        nameGap = 40,
        nameLocation = 'middle',
        nameTextStyle = list(
          color = '#333',
          fontSize = 16,
          fontWeight = 500
        )
      ) |>
      e_y_axis(
        axisLabel = list(fontSize = 12,  color = '#333'),
        name = "Survival Probability",
        nameGap = 40,
        nameLocation = 'middle',
        nameTextStyle = list(
          color = '#333',
          fontSize = 16,
          fontWeight = 500
        )
      )
  }



# plot_echart_km(sfit_grp_df1, tit = "KM Plot", sub_tit = "lung")


# ----

#!

#' Kaplan-Meier advanced plot with timeline
#'
#' @param dat
#' @param tit
#' @param sub_tit
#'
#' @return
#' @export
#'
#' @examples
#' plot_echart_timeline_km(sfit_grp_df1, tit = "Kaplan-Meier Plot", sub_tit = "Lung Data", time_line = TRUE)

plot_echart_timeline_km <-
  function(dat,
           tit = "Kaplan-Meier Plot",
           sub_tit = "data",
           time_line = TRUE) {
    # check "strata" in data
    #stopifnot("strata" %in% colnames(dat))
    
    if ("strata" %in% colnames(dat)) {
      dat1 <- dat |>
        mutate(surv_censor = case_when((n.censor > 0) ~ surv,
                                       (n.censor == 0) ~ NA_integer_,
                                       TRUE ~ NA_integer_
        )) |>
        group_by(strata)
    } else{
      dat1 <- dat |>
        mutate(surv_censor = case_when((n.censor > 0) ~ surv,
                                       (n.censor == 0) ~ NA_integer_,
                                       TRUE ~ NA_integer_
        ))
    }
    
    
    
    # plot
    ec1 <- dat1 |>
      e_charts(time) |>
      e_line(
        surv,
        showSymbol = TRUE,
        symbolSize = 3,
        itemStyle = list(opacity = 0.6)
      ) |>
      e_line(
        surv_censor,
        showSymbol = TRUE,
        itemStyle = list(
          borderWidth = 1,
          borderColor = '#7B7C81',
          color = '#91cc75',
          shadowColor = '#91cc75'
        ),
        symbol = 'triangle',
        symbolSize = 6,
        tooltip = list(show = FALSE),
        legend = list(show = FALSE)
      ) |>
      e_tooltip(trigger = "axis") |>
      e_mark_line(
        data = list(yAxis = 0.5),
        y_index = 1,
        symbol = "none",
        lineStyle = list(type = 'dashed'),
        itemStyle = list(color = "#e76f51"),
        title = "50% threshold",
        label = list(position = "insideEndTop")
      ) |>
      e_title(text = tit, subtext = sub_tit) |>
      e_x_axis(
        axisLabel = list(fontSize = 12,  color = '#333'),
        name = "Time",
        nameGap = 40,
        nameLocation = 'middle',
        nameTextStyle = list(
          color = '#333',
          fontSize = 16,
          fontWeight = 500
        )
      ) |>
      e_y_axis(
        axisLabel = list(fontSize = 12,  color = '#333'),
        name = "Survival Probability",
        nameGap = 40,
        nameLocation = 'middle',
        nameTextStyle = list(
          color = '#333',
          fontSize = 16,
          fontWeight = 500
        )
      ) |>
      e_toolbox(
        show = TRUE,
        feature = list(
          dataZoom = list (yAxisIndex = 'none'),
          dataView = list (readOnly = TRUE),
          restore = list(TRUE),
          saveAsImage = list(TRUE)
        )
      )
    
    
    # timeline
    if (time_line) {
      ec2 <- ec1 |>
        e_datazoom(type = "slider")
    } else{
      ec2 <- ec1
    }
    
    
    ec2
  }


#?

ui <- fluidPage(
    
    h2("Lung Data - Survival Analysis")
    , linebreaks(1)
    , fluidRow(
      column(3, uiOutput(("time_ui")))
      ,
      column(3, uiOutput(("event_ui")))
      ,
      column(3, uiOutput(("grp_ui")))
      ,
      column(3,
             tags$div(
               style = "margin-top: 32px;",
               actionButton("run_km", "Run")
             )
      )   
    )

    , linebreaks(2)

    , fluidRow(
        column(6, echarts4rOutput("plot", width = '100%', height = '700px')),
        column(6, 
        linebreaks(2),
        DTOutput('summary_tbl', width = 800)
        )
    )
)

#?
server <- function(input, output, session) {

#? lung data
 select_df <- reactive({
   lung
 })


observeEvent(input$run_km, {
    output$summary_tbl = renderDT(
      get_surv_grp_summary(select_df(), input$time, input$event, input$grp) %>%
      mutate(across(where(is.numeric), round, 4)), 
      options = list(scrollX = TRUE)
    )
})

#
output$time_ui = renderUI({
                   req(select_df())
                   
                   #col_names1 <- select_df() %>% colnames()
                   col_names1 <- select_df() %>% purrr::discard(~ !is.numeric(.)) %>% names()
                   
                   if ("time" %in% c(col_names1)) {
                     sel_col1 <- "time"
                   } else{
                     sel_col1 <- col_names1[1]
                   }
                   
                   selectizeInput(
                     "time",
                     "x-axis",
                     col_names1,
                     selected = sel_col1,
                     multiple = FALSE,
                     size = "sm"
                   )
                   
                 })
                 
                 # variable 2
                 output$event_ui = renderUI({
                   req(input$time)
                   
                   #col_names2 <- select_df() %>%
                   #              dplyr::select(-(input$time)) %>%
                   #              colnames()

                   col_names1 <- select_df() %>% purrr::discard(~ !is.numeric(.)) %>% names() 
                   col_names2 <- col_names1[ !col_names1 == input$time]              
                   
                   if ("event" %in% c(col_names2)) {
                     sel_col2 <- "event"
                   } else if ("status" %in% c(col_names2)) {
                     sel_col2 <- "status"
                   } else{
                     sel_col2 <- col_names2[1]
                   }
                   
                   selectizeInput(
                     "event",
                     "y-axis",
                     col_names2,
                     selected = sel_col2,
                     multiple = FALSE,
                     size = "sm"
                   )
                   
                 })
                 
                 # variable 3
                 output$grp_ui = renderUI({
                   req(input$time)
                   req(input$event)
                   
                   #col_names3 <- select_df() %>%
                   #  dplyr::select(-(input$time),-(input$event)) %>%
                   #  colnames()

                   #non_num_cols <- select_df() %>% purrr::discard(~is.numeric(.)) %>% names()
                   non_num_cols <- select_df() %>% names()
                   col_names3 <- non_num_cols[ !non_num_cols %in% c(input$time, input$event)]
                   #col_names3 <- non_num_cols

                   #?
                   if ("sex" %in% c(col_names3)) {
                     sel_col3 <- "sex"
                   } else{
                     sel_col3 <- col_names3[1]
                   }
                   
                   selectizeInput(
                     "grp",
                     "group",
                     c(1, col_names3),
                     selected = 1,
                     multiple = FALSE,
                     size = "sm"
                   )
                   
                 })
                 
                 #!
                 observeEvent(input$run_km, {
                   output$plot <- renderEcharts4r({
                     req(input$time)
                     req(input$event)
                     req(input$grp)
                     req(select_df())

                     #? advanced plot
                     sfit_grp_df1 <-
                       get_surv_grp_summary(select_df(), input$time, input$event, input$grp)
                     
                     plot_echart_timeline_km(
                       sfit_grp_df1,
                       tit = "Kaplan-Meier Plot",
                       sub_tit = "Lung Data",
                       time_line = TRUE
                     )
                     
                   })
                   
                 })

#? end
}
shinyApp(ui, server)