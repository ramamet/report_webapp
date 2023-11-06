library(shiny)
library(dplyr)
library(echarts4r)
library(survival)
library(survminer)

fun_km_surv <- function(dat, time_var, event_var, grp_var){
  
  new_data <<- dat

  sfit <-  eval(bquote((survfit(
                       Surv(.(as.name(time_var)), .(as.name(event_var))) ~ .(as.name(grp_var)), data = new_data
                     ))))
                   
        # convert survfit modal to dataframe
        res <- surv_summary(sfit)
        
        # transform
        res_df <- res %>%
            as_tibble() |>
            mutate(surv = round(surv, 2))
        
        return(res_df)              
      
      }

 # fun_km_surv(lung, "time", "status", "sex")  


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
           conf_bands = 0,
           time_line = 0) {
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
      ) |>
      e_grid(left = "10%")
    
    
    # timeline
    if(conf_bands>0 & time_line > 0 ) {
      ec2 <- ec1 |>
        e_datazoom(type = "slider") |>
        e_band2(lower, upper,
        color = "#e9ecef", #name = "confidence_bands",
         itemStyle = list(opacity = 0.6, borderWidth = 0),
        tooltip = list(show = FALSE)) 
    } else if(conf_bands > 0 & time_line == 0 ) {
      ec2 <- ec1 |>
        e_band2(lower, upper,
        color = "#e9ecef", #name = "confidence_bands",
         itemStyle = list(opacity = 0.6, borderWidth = 0),
        tooltip = list(show = FALSE)) 
    } else if(conf_bands == 0 & time_line > 0 ) {
      ec2 <- ec1 |>
             e_datazoom(type = "slider") 
    }else{
      ec2 <- ec1
    }    
    
    ec2
  }

#source('./R/fun_ec_km.R')

ui <- fluidPage(
echarts4rOutput(
               "plot", width = '95%', height = '700px'
             )
)
server <- function(input, output, session) {

 output$plot <- renderEcharts4r({
        sfit_grp_df1 <- fun_km_surv( dat = lung, time_var = "time", event_var = "status", grp_var = "sex")  

        plot_echart_timeline_km(
                            sfit_grp_df1,
                            tit = "Kaplan-Meier Plot",
                            sub_tit = "Survival Curve for Censored Data" ,
                            conf_bands = 1 ,
                            time_line = 1             
                            )

 })

}
shinyApp(ui, server)