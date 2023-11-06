
library(gtsummary)
library(shiny)
library(gt)
library(dplyr)
library(purrr)
library(DT)
library(haven)
library(ggplot2)

#?
# Workaround for Chromium Issue 468227
downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

# create the Shiny app

#? ui side
ui <- navbarPage( "ADSL shinylive POC", id = 'navbar_id',
               
# homepage with description
tabPanel("",
icon = icon("home", lib = "glyphicon"),
h2("shinylive: Run 'shiny' Applications in the Browser"),
br(),
h4("
Exporting 'shiny' applications with 'shinylive' allows you to run them entirely in a web browser, without the need for a separate R server. The traditional way of deploying 'shiny' applications involves in a separate server and client: the server runs R and 'shiny', and clients connect via the web browser.
"),
br(),
h4("When an application is deployed with 'shinylive', R and 'shiny' run in the web browser (via 'webR'): the browser is effectively both the client and server for the application. This allows for your 'shiny' application exported by 'shinylive' to be hosted by a static web server.")

#br(),
#tags$img(src = "./shinylive.png", width = "60%", style="display: block; margin-left: auto; margin-right: auto;")

),     

# import data
#tabPanel("Data", 
#  fluidRow(
#    column(3, 
#    uiOutput("import_data_ui1"),
#    actionButton(inputId = 'reset_data',"Reset data", icon = icon("refresh", lib = "glyphicon"))       
#    ),
#    column(9, DT::dataTableOutput('out_table'))
#    )
# ),


  tabPanel("Data", 
   fluidRow(
     column(3, 
     tabsetPanel( id = "tab_data", 
     br(),
     tabPanel('Import',
     fileInput("file1", "Choose File (csv/sas7bdat/xpt)", 
           accept = c( ".csv", ".sas7bdat", ".xpt"))),

     tabPanel("Default",
     p('An ADSL-flavored clinical trial toy dataset from gt package.'),
     p('This tibble contains artificial data for 182 subjects of the GT01 study. Each row corresponds to demographic characteristics of a single trial participant. Two out of 182 study participants were screen failures and thus not treated, the rest of the study population was randomized with a 1:1 ratio to receive either "Placebo" (as comparator) or "Drug 1". The dataset entails subject level demographics such as age, age group, sex, ethnicity, and body mass index (BMI) at baseline, as well as an event flag, indicating whether the subject experienced a specific event throughout the course of the study or not.'),
     br(),
     actionButton('get_local_data',"Load Default data") 
     )                
    )),
     column(9, DT::dataTableOutput('out_table'))
     )
  ),


# table section
tabPanel("Table", 
  fluidRow(
    column(4,     
    wellPanel(uiOutput("custom_ui1")),
    wellPanel(uiOutput("custom_ui2")),
    
    wellPanel(
    p(strong('Filters;')),  
    fluidRow(
    column(6, align = 'right', checkboxInput("ittfl", "ITTFL", FALSE)),
    column(6, align = 'left', checkboxInput("saffl", "SAFFL", FALSE))
    ), 
    sliderInput(inputId = "age", "Age:", min = 1, max = 100, value = c(1,100), step=1, ticks=TRUE)
    ),  
    br(),
    actionButton(inputId = 'reset_filter',"Reset filters", icon = icon("cog", lib = "glyphicon")) 
    

    )

    ,column(8, 
    fluidRow(
    gt::gt_output("summaryds1")
    )
    )
  ))

  # boxplot section
  ,
  tabPanel("Plot", 
  
  fluidRow(
  
  column(3,     
      wellPanel(uiOutput("ggbox_ui1")),
      wellPanel(uiOutput("ggbox_ui2"))
       #br(),
       #downloadButton("download_ggbox", label = "Download Plot")  
       )
  
   ,column(9,
   plotOutput("boxplot")
   ))
  )


# end
)


#? server side
server <- function(input, output, session) {

  # function
   is.Date <- function(x) {
      inherits(x, c("Date", "POSIXt"))
    }

  #?
  observeEvent(input$reset_filter, {
    req(input$file1)

    updateCheckboxInput(session, "ittfl", label = NULL, value = FALSE)
    updateCheckboxInput(session, "saffl", label = NULL, value = FALSE)
    updateSliderInput(session, "age", min = 1, max = 100, value=c(1, 100), step=1)
  })

  #?
  #rv <- reactiveValues() 

  #observeEvent(input$file1, {
  #  rv$file1 <- input$file1
  # })


  #? render import data ui
  #observe({
   
  #if(req(input$navbar) == "Data"){
   
  # output$import_data_ui1 <- renderUI({
  #    fileInput("file1", "Choose File (csv/sas7bdat/xpt)", 
  #                        accept = c( ".csv", ".sas7bdat", ".xpt"))  
  # })
  # }
  # })

 #? reset
 # observeEvent(input$reset_data, {
 #   rv$file1 <- NULL
  
 #   output$import_data_ui1 <- renderUI({
 #   fileInput('file1', label = NULL)
 #   })
 #  })

 #import ds
  import_data <- reactive({  

    req(input$file1) 
    #infile <- rv$file1

    infile <- input$file1
    # validation
    ext <- tools::file_ext(infile$datapath)
    validate(need(ext %in% c("csv","xls","sas7bdat","xpt"), "Please upload the file!"))

    if(ext == "csv"){
     read.csv(infile$datapath)
    } else if(ext == "xpt"){
     read_xpt(infile$datapath)
    } else if(ext == "sas7bdat"){
     read_sas(infile$datapath)
    }

 })

 #? local data
    local_data <- eventReactive(input$get_local_data, {
     # haven::read_xpt('adsl.xpt')
     gt::rx_adsl %>%
     filter(!is.na(TRTA))
     })

     

   #? finalize the data  
   react_data <- reactive({  

       if (input$tab_data == "Default"){
       local_data()
       } else{
       import_data()
       }

   })


   #?
   output$out_table <- DT::renderDataTable({

     #req(input$file1)
     react_data() %>% DT::datatable(options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
     
     })


  #? custom ui part
  
  #
  output$custom_ui1 <- renderUI({
   
   req(react_data())
 
   trt_cols <- react_data() %>% 
               dplyr:: select(starts_with("TRT")) %>%
               purrr::discard(~is.numeric(.)) %>%
               purrr::discard(~is.Date(.)) %>%
               names()

   selectInput("trtvariable", "Select Treatment Variable:",trt_cols, trt_cols[1])

  })  

  # y
  output$custom_ui2 <- renderUI({
   req(react_data())

   non_num_cols <- react_data() %>%
    dplyr:: select(!starts_with("TRT")) %>%
    purrr::discard(~is.numeric(.)) %>%
    names()

   selectizeInput( "variable2", "Select Variables:",non_num_cols,
    options = list(placeholder = 'Choose'),
    multiple = TRUE)
  })  
  
  #? gt table
  runsummaryds1 <- reactive({
    
    req(react_data())
    
    newadsl<- react_data()
    
    #? filter conditions
    if(input$ittfl==TRUE & ("ITTFL" %in% names(newadsl))) {newadsl <- newadsl %>% filter(ITTFL %in% c("Y"))}
    if(input$saffl==TRUE & ("SAFFL" %in% names(newadsl))) {newadsl <- newadsl %>% filter(SAFFL %in% c("Y"))}

    #?
    newadsl<-newadsl%>%filter(AGE>=input$age[1] & AGE<=input$age[2])
    
    req(input$variable2)

    validate(need(nrow(newadsl) != 0, "Data is empty")) 
  
    # table
    newadsl %>%
    dplyr::select(input$trtvariable,!!!input$variable2)%>%
      tbl_summary(by=input$trtvariable,
      missing_text="(Missing)",
      type=all_continuous()~"continuous2",
      statistic=all_continuous()~c("{N_nonmiss}", 
      "{mean} ({sd})", "{median}", "{min}, {max}"))%>%
      add_overall(last=TRUE) %>% 
      as_gt()
    
  })
  
  # gt_summary
  output$summaryds1 <- gt::render_gt({   
  runsummaryds1()    
  })


  #? boxlots 

   output$ggbox_ui1 <- renderUI({
  
   req(react_data())
 
   trt_cols <- react_data() %>% 
               dplyr:: select(starts_with("TRT")) %>%
               purrr::discard(~is.numeric(.)) %>%
               purrr::discard(~is.Date(.)) %>%
               names()

   selectInput("boxp_trtvariable", "Select Treatment Variable:",trt_cols, trt_cols[1])

  })  
  

   # selectinput
  output$ggbox_ui2 <- renderUI({
  req(react_data())
 
    subj_cols <- react_data() %>% 
                    dplyr:: select(!starts_with("TRT")) %>%
                    purrr::discard(~ !is.numeric(.))%>%
                    purrr::discard(~is.Date(.)) %>%
                    names()
   #
    if('AGE' %in% subj_cols){
    sel_subj <- "AGE"
    }else{
    sel_subj <- subj_cols[1]
    }
    
    #
    selectInput("subject_data", "Subject Data",subj_cols, sel_subj)

  }) 

   # reactive plot

   gg_boxplot <- reactive({

    req(react_data())
    req(input$subject_data)

    ggplot(data = react_data(), 
            aes(x = .data[[input$boxp_trtvariable]], 
                y = .data[[input$subject_data]], 
             fill = .data[[input$boxp_trtvariable]])) +
      geom_boxplot() +
      geom_jitter(width = 0.3, alpha = 0.4) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 15)) +
      labs(
        title = "ADSL Data",
        subtitle = "Comparing Treatment Groups",
        x = "",
        #x = attributes(react_data[[input$boxp_trtvariable]]),
        y = attributes(react_data()[[input$subject_data]])
      )


   })
   
   # plot
    output$boxplot <- renderPlot({
    
    gg_boxplot()

  }, res = 100, height = 600)


  #? download boxplot
  output$download_ggbox <- downloadHandler(
      filename = function() { paste("ggbox_",format(Sys.time(), "%Y%m%d_%H%M%S"), '.svg', sep='') },
      content = function(file) {
        ggsave(file, plot = gg_boxplot(), device = "svg", width = 12, height = 8)
      }
    )


# end  
}

# Run the application
shinyApp(ui = ui, server = server)

