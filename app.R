library(shiny)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

source("keypress_handler.R")
source("helpers.R")
source("plots.R")

theme_set(theme_minimal())

options(shiny.maxRequestSize = 1 * 1024 ^ 3)

colors <- c("#F8766D", "#7CAE00", "#00BFC4")

ui <- pageWithSidebar(
    headerPanel("Shiny Eyes"),

    sidebarPanel(
        fileInput("input_filepath", "Data File",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        fileInput("existing_codes", "Import Existing Codes",
                  accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        selectInput("grouping_variables", "Grouping Variables", NULL, multiple = TRUE),
        selectInput("sample_variable", "Sample Variable", NULL, multiple = FALSE),
        selectInput("x_variable", "Gaze X Variable", NULL, multiple = FALSE),
        selectInput("y_variable", "Gaze Y Variable", NULL, multiple = FALSE),
        textInput("window_1_name", "Window 1 Name", value = "Fixation"),
        textInput("window_2_name", "Window 2 Name", value = "Saccade"),
        textInput("window_3_name", "Window 3 Name", value = "Blink"),
        textInput("gaze_x_scale_min", "Gaze Gif X Scale Min", value = "0"),
        textInput("gaze_x_scale_max", "Gaze Gif X Scale Max", value = "1920"),
        textInput("gaze_y_scale_min", "Gaze Gif Y Scale Min", value = "0"),
        textInput("gaze_y_scale_max", "Gaze Gif Y Scale Max", value = "1080"),
        textInput("n_frames", "Gaze Gif nFrames", value = "18"),
        textInput("percent_skip", "Percent to skip (0-100)", value = 5),
        downloadButton("download_data", "Download Data")
    ),

    mainPanel(
        tags$script(keypress_handler),
        textOutput("chunk_name"),
        actionButton("prev_chunk", "Prev Chunk"),
        actionButton("undo_brush", "Undo Window"),
        actionButton("clear_brush", "Clear Windows"),
        actionButton("next_chunk", "Next Chunk"),
        actionButton("skip", "Skip Ahead"),
        plotOutput("trace_plot", brush = brushOpts(id = "image_brush", direction = "x"), hover = "trace_hover"),
        actionButton("render_gif_button", "Render Gaze Gif"),
        actionButton("hide_gif_button", "Hide Gaze Gif"),
        plotOutput("gaze_gif")
    )
)

server <- function(input, output, session) {
    lazy_loaded_data <- reactive({
        if (!is.null(input$input_filepath$datapath)) {
            data <- read_csv(input$input_filepath$datapath)
        } else {
            data <- NULL
        }
        data
    })
    
    lazy_loaded_codes <- reactive({
        if (!is.null(input$existing_codes$datapath)) {
            data <- read_csv(input$existing_codes$datapath)
        } else {
            data <- NULL
        }
        data
    })

    observe({
        if (!is.null(input$input_filepath)) {
            # Empty string tops list, so events only fire when values are actively changed by the user
            column_names <- c("", colnames(lazy_loaded_data()))
            updateSelectInput(session, "grouping_variables", choices = column_names)
            updateSelectInput(session, "sample_variable", choices = column_names)
            updateSelectInput(session, "x_variable", choices = column_names)
            updateSelectInput(session, "y_variable", choices = column_names)
        }
    })

    lazy_chunks <- reactive({
        data <- lazy_loaded_data()
        if (!is.null(data) & !is.null(input$grouping_variables)) {
            chunks <- distinct_at(data, vars(input$grouping_variables))
        } else {
            chunks <- NULL
        }
        chunks
    })

    stored_data <- reactiveValues(data = NULL)

    row_num_vect <- reactiveValues(row_num = 1)
    
    cur_chunk <- reactive({
      if (!is.null(lazy_chunks())) {
        lazy_chunks() %>%
          slice(row_num_vect$row_num)
      } else {
        NULL
      }
    })

    observeEvent(input$next_chunk, {
        chunks <- lazy_chunks()
        
        single_chunk_windows <- get_single_chunk_windows(chunks,
                                                         row_num_vect$row_num,
                                                         brush_data$windows)

        stored_data$data <- update_stored_data(stored_data$data, single_chunk_windows)

        row_num_vect$row_num <- row_num_vect$row_num + 1
        
        if (row_num_vect$row_num > nrow(chunks)) {
            row_num_vect$row_num <- 1
        }

        if (!is.null(stored_data$data)) {
            stored_data$data <- clear_saved_window_data(chunks,
                                                        row_num_vect$row_num,
                                                        stored_data$data)
        }

        brush_data$windows <- empty_window_data
    })

    observeEvent(input$prev_chunk, {
        chunks <- lazy_chunks()
        
        single_chunk_windows <- get_single_chunk_windows(chunks,
                                                         row_num_vect$row_num,
                                                         brush_data$windows)

        stored_data$data <- update_stored_data(stored_data$data, single_chunk_windows)

        row_num_vect$row_num <- row_num_vect$row_num - 1
        if (row_num_vect$row_num < 1) {
            row_num_vect$row_num <- nrow(chunks)
        }

        if (!is.null(stored_data$data)) {
            stored_data$data <- clear_saved_window_data(chunks,
                                                        row_num_vect$row_num,
                                                        stored_data$data)
        }

        brush_data$windows <- empty_window_data
    })
    
    observeEvent(input$skip, {
      chunks <- lazy_chunks()
      
      single_chunk_windows <- get_single_chunk_windows(chunks,
                                                       row_num_vect$row_num,
                                                       brush_data$windows)
      
      stored_data$data <- update_stored_data(stored_data$data, single_chunk_windows)
      
      perc_skip = as.numeric(input$percent_skip)
      perc_skip = ifelse(is.na(perc_skip), 5, perc_skip)
      row_num_vect$row_num <- row_num_vect$row_num + round(perc_skip / 100 * nrow(chunks))
      
      if (row_num_vect$row_num > nrow(chunks)) {
        row_num_vect$row_num <- 1
      }
      
      if (!is.null(stored_data$data)) {
        stored_data$data <- clear_saved_window_data(chunks,
                                                    row_num_vect$row_num,
                                                    stored_data$data)
      }
      
      brush_data$windows <- empty_window_data
    })

    output$download_data <- downloadHandler(filename = "shiny_eyes_output.csv",
                                            content = function(file) {
                                                chunks <- lazy_chunks()

                                                single_chunk_windows <- get_single_chunk_windows(chunks,
                                                                                                 row_num_vect$row_num,
                                                                                                 brush_data$windows)

                                                stored_data$data <- update_stored_data(stored_data$data, single_chunk_windows)

                                                stored_data$data <- stored_data$data %>%
                                                    arrange(!!!syms(input$grouping_variables), xmin, xmax)

                                                write_csv(stored_data$data, file)},
                                            contentType = "text/csv")

    chunk_data <- reactive({
        chunks <- lazy_chunks()
        
        if (!is.null(chunks)) {
          cur_chunk() %>% 
            left_join(lazy_loaded_data())
        } else {
            NULL
        }})

    output$chunk_name <- renderText({
        get_chunk_title(lazy_chunks(), row_num_vect$row_num)
    })

    brush_data <- reactiveValues(windows = empty_window_data)
    
    chunk_codes <- reactive({
      if (!is.null(cur_chunk()) & !is.null(lazy_loaded_codes()) & input$sample_variable != "" & input$x_variable  != "" & input$y_variable  != "") {
        cur_chunk() %>% 
          left_join(lazy_loaded_codes()) %>% 
          select(xmin, xmax, window_name)
      } else {
        NULL
      }
    })
    
    observeEvent(chunk_codes(), {
      if (is.null(chunk_codes())) {
        brush_data$windows <- empty_window_data
      } else {
        brush_data$windows <- chunk_codes()
      }
    })

    observeEvent(input$undo_brush, {
        if (nrow(brush_data$windows) > 0) {
            brush_data$windows <- head(brush_data$windows, -1)  # Remove last window
        }
    })

    observeEvent(input$clear_brush, {
        brush_data$windows <- empty_window_data

        stored_data$data <- clear_saved_window_data(lazy_chunks(),
                                                    row_num_vect$row_num,
                                                    stored_data$data)
    })

    observeEvent(input$keypress, {
        if (input$keypress %in% c("1", "2", "3")) {
            window_name <- switch(input$keypress,
                                 "1" = input$window_1_name,
                                 "2" = input$window_2_name,
                                 "3" = input$window_3_name)

            brush_data$windows <- rbind(brush_data$windows,
                                        list(xmin = input$image_brush$xmin,
                                             xmax = input$image_brush$xmax,
                                             window_name = window_name),
                                        stringsAsFactors = FALSE)

            brush_data$windows$window_name <- factor(brush_data$windows$window_name,
                                                     levels = c(input$window_1_name,
                                                                input$window_2_name,
                                                                input$window_3_name))
        } else if (input$keypress == "d") {
          delete_x <- input$trace_hover$x
          
          if (!is.null(delete_x) & !is.null(brush_data$windows)) {
            brush_data$windows <- brush_data$windows %>% 
              filter(!(xmin <= delete_x & xmax >= delete_x))
          }
        }
    })

    output$trace_plot <- renderPlot({
        make_trace_plot(input, chunk_data(), brush_data, colors)
    })

    gaze_gif_data <- reactiveValues(render_gif = FALSE)

    observeEvent(input$render_gif_button, {gaze_gif_data$render_gif <- TRUE})
    
    observeEvent(input$hide_gif_button, {gaze_gif_data$render_gif <- FALSE})

    output$gaze_gif <- renderImage({
        make_gaze_gif(gaze_gif_data$render_gif, input, chunk_data())
    }, deleteFile = TRUE)
}

shinyApp(ui, server)
