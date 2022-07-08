# https://jymaze.shinyapps.io/open_fda_a

library(shiny)
library(shiny)
library(jsonlite)
library(ggplot2)
library(stringr)
library(DT)
library(rsconnect)

ui<- fluidPage(
  titlePanel("Adverse Events Query to OpenFDA", windowTitle = "Adverse Events Query to OpenFDA"),
  fluidRow(
    column(3,
           br(),
           p("The OpenFDA project makes adverse-event data gathered after January 1, 2004,
              available via a public-access portal that enables developers to quickly and
              easily use it in applications. The project is hosted at:"),
           a(href="https://open.fda.gov/", "open.fda.gov"),
           br(),
           br(),
           p("This web-app provides a user-friendly graphical interface to the openFDA web-server"),
           br(),
           p("To explore the adverse event data between two dates:"),
           textInput("api", "1) Enter the name of drug:", "asdsdsd"),
           dateInput('start_date', '2) Enter the start date:', min = '2004-01-01', value = '2004-01-01'),
           dateInput('end_date', '3) Enter the end date:', min = '2004-01-02', value = '2022-06-01'),
           radioButtons('seriousness', '4) Filter by seriousness:', choices = c("All" = 0, "Serious" = 1, "Non-serious" = 2)),
           actionButton("submit", "Submit Request"),
           br(),
           strong(htmlOutput("message")),
           uiOutput("download_button"),
           br(),
           br()
    ),
    column(9,
           plotOutput("plt", width = "auto", height = "640")
    )
  ),
  fluidRow(
    column(12,
           DT::dataTableOutput("tble")
    )
  )
)



adverse_event_plot <- function(data, api_name) {
  ggplot(data = data, aes(x = reorder(Event, Count, FUN = function(x) -x), y = Count)) +
    geom_bar(stat = 'identity', colour = 'white', fill = 'navyblue', alpha = 0.6, width = 0.6) +
    geom_text(data = data, aes(x = Event, y = Count*1.1, label = Count), size = 4,
              position = position_dodge(width = 1)) +
    xlab('') +
    ylab('COUNT\n') +
    ggtitle(paste0('TOP 10 MOST FREQUENT ADVERSE EVENTS FOR ', toupper(api_name), '\n\n')) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, color = 'black', size = 11),
      axis.text.y = element_text(hjust = 1, color = 'black', size = 11),
      legend.position = 'none'
    )
}

clean_data <- function(raw_data) {
  count <- raw_data$results$count
  data.frame(
    Rank = seq_along(raw_data$results$term),
    Event = as.character(raw_data$results$term),
    Count = count,
    Frequency = sprintf("%.3f %%", (count / sum(count)) * 100),
    stringsAsFactors = FALSE
  )
}

subset_data <- function(df) {
  df_subset <- df[1:10, ]
  df_subset$Event <- str_wrap(df_subset$Event, width=15)
  return(df_subset)
}



server<- function(input, output) {
  
  api_data <- reactiveVal(NULL)
  
  result_message <- reactiveVal("Your request did not return any result.<br>Please try again!")
  
  display_drug <- reactiveVal("")
  
  api <- reactive(gsub(' ', '+', tolower(input$api))) # to lowercase, then replace spaces with plus signs
  start_date <- reactive(as.character(input$start_date))
  end_date <- reactive(as.character(input$end_date))
  seriousness <- reactive({
    req(input$seriousness)
    if (input$seriousness == 0) {
      return("")
    } else {
      # 1 for serious, 2 for non serious according to API
      return(paste0('+AND+serious:', as.character(input$seriousness)))
    }
  })
  
  data_url <- reactive({
    req(api(), start_date(), end_date())
    paste0(
      'https://api.fda.gov/drug/event.json?search=(patient.drug.medicinalproduct:',
      api(),
      '+OR+patient.drug.openfda.generic_name:',
      api(),
      '+OR+patient.drug.openfda.substance_name:',
      api(),
      '+OR+patient.drug.openfda.brand_name:',
      api(),
      ')',
      seriousness(),
      '+AND+receivedate:[',
      start_date(),
      '+TO+',
      end_date(),
      ']&count=patient.reaction.reactionmeddrapt.exact&limit=1000'
    )
  })
  
  # retrieve data
  observeEvent(
    input$submit, {
      showNotification(paste0("Submitted request for \"", api(), "\""), type = "default")
      tryCatch(
        {
          raw_data <-  fromJSON(url(data_url()))
          api_data(clean_data(raw_data))
          result_message("5) Download the data:")
          display_drug(api())
          showNotification("DONE!")
        },
        error = function(e){
          showNotification(paste0("ERROR. No results for \"", api(), "\""), type = "error")
          result_message("Your request did not return any result.<br>Please try again!")
          api_data(NULL)
        }
      )
    }
  )
  
  df_subset <- reactive({
    req(api_data())
    subset_data(api_data())
  })
  
  output$message <- renderText(result_message())
  
  output$tble <- DT::renderDataTable({
    req(api_data())
    api_data()
  })
  output$download_button <- renderUI({
    req(api_data())
    downloadButton("download_data", 'Download CSV')
  })
  
  output$plt <- renderPlot({
    req(df_subset(), display_drug())
    tryCatch({
      adverse_event_plot(df_subset(), display_drug())
    }, error = function(e) {
      message(e)
    })
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0(display_drug(), '.csv')
    },
    content = function(file) {
      write.csv(api_data(), file, row.names = FALSE)
    }
  )
  exportTestValues(
    api_data= api_data()
    # plot = adverse_event_plot(df_subset(), display_drug())
  )
}


shinyApp(ui, server)