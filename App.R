library(shiny)
library(ggplot2)
library(colourpicker)

make_clock_data <- function(clock_name, n_segments, filled = 0, color = "#2C3E50") {
  data.frame(
    id = factor(paste0(clock_name, "_", seq_len(n_segments))),
    value = rep(1, n_segments),
    filled = c(rep(TRUE, filled), rep(FALSE, n_segments - filled)),
    color = color
  )
}


ui <- fluidPage(
  titlePanel("Blades in the Dark - Progress Clocks"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("new_clock_name", "New Clock Name", value = "My Clock"),
      selectInput("new_n_segments", "Number of Segments", choices = c(3, 4, 6, 8, 10, 12), selected = 6),
      colourInput("new_clock_color", "Clock Color", value = "#2C3E50"),
      actionButton("add_clock", "Add Clock")
    ),
    
    mainPanel(
      uiOutput("all_clocks_ui")
    )
  )
)

server <- function(input, output, session) {
  clocks <- reactiveValues(data = list())
  
  observeEvent(input$add_clock, {
    name <- input$new_clock_name
    if (name != "" && !(name %in% names(clocks$data))) {
      clocks$data[[name]] <- list(
        name = name,
        n_segments = as.numeric(input$new_n_segments),
        filled = 0,
        color = input$new_clock_color
      )
      
      isolate({
        local({
          clock_name <- name
          
          observeEvent(input[[paste0("advance_", clock_name)]], {
            clock <- clocks$data[[clock_name]]
            if (clock$filled < clock$n_segments) {
              clock$filled <- clock$filled + 1
              clocks$data[[clock_name]] <- clock
            }
          })
          
          observeEvent(input[[paste0("reset_", clock_name)]], {
            clocks$data[[clock_name]]$filled <- 0
          })
          
          observeEvent(input[[paste0("delete_", clock_name)]], {
            clocks$data[[clock_name]] <- NULL
          })
          
          output[[paste0("plot_", clock_name)]] <- renderPlot({
            clock <- clocks$data[[clock_name]]
            req(clock)  # Prevent rendering if the clock was delete
            
            df <- make_clock_data(clock_name, clock$n_segments, clock$filled, clock$color)
            
            ggplot(df, aes(x = "", y = value, fill = filled)) +
              geom_bar(stat = "identity", width = 1) +
              coord_polar(theta = "y") +
              scale_fill_manual(values = c(`FALSE` = "gray90", `TRUE` = clock$color)) +
              theme_void() +
              theme(legend.position = "none")
          })
        })
      })
    }
  })
  
  output$all_clocks_ui <- renderUI({
    if (length(clocks$data) == 0) return(h4("No clocks yet. Add one!"))
    
    clock_uis <- lapply(names(clocks$data), function(name) {
      plot_id <- paste0("plot_", name)
      advance_id <- paste0("advance_", name)
      reset_id <- paste0("reset_", name)
      delete_id <- paste0("delete_", name)
      
      tagList(
        h4(name),
        plotOutput(plot_id, height = "200px"),
        actionButton(advance_id, "Advance"),
        actionButton(reset_id, "Reset"),
        actionButton(delete_id, "Delete", class = "btn-danger"),
        tags$hr()
      )
    })
    
    
    do.call(tagList, clock_uis)
  })
}


shinyApp(ui, server)
