library(shiny)
library(tidyverse)
library(gganimate)

source("keypress_handler.R")
source("helpers.R")

theme_set(theme_minimal())

options(shiny.maxRequestSize = 1 * 1024 ^ 3)

colors <- c("#F8766D", "#7CAE00", "#00BFC4")

ui <- pageWithSidebar(
    headerPanel("Shiny Eyes"),

    sidebarPanel(
        fileInput("input_filepath", "Data File",
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
        downloadButton("download_data", "Download Data")
    ),

    mainPanel(
        tags$script(keypress_handler),
        textOutput("trial_name"),
        actionButton("prev_chunk", "Prev Chunk"),
        actionButton("undo_brush", "Undo Window"),
        actionButton("clear_brush", "Clear Windows"),
        actionButton("next_chunk", "Next Chunk"),
        plotOutput("trace_plot", brush = brushOpts(id = "image_brush", direction = "x")),
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

    lazy_trials <- reactive({
        data <- lazy_loaded_data()
        if (!is.null(data) & !is.null(input$grouping_variables)) {
            trials <- distinct_at(data, vars(input$grouping_variables))
        } else {
            trials <- NULL
        }
        trials
    })

    stored_data <- reactiveValues(data = NULL)

    row_num_vect <- reactiveValues(row_num = 1)

    observeEvent(input$next_chunk, {
        trials <- lazy_trials()
        
        single_trial_windows <- get_single_trial_windows(trials,
                                                         row_num_vect$row_num,
                                                         brush_data$windows)

        stored_data$data <- update_stored_data(stored_data$data, single_trial_windows)

        row_num_vect$row_num <- row_num_vect$row_num + 1
        
        if (row_num_vect$row_num > nrow(trials)) {
            row_num_vect$row_num <- 1
        }

        if (!is.null(stored_data$data)) {
            stored_data$data <- clear_saved_window_data(trials,
                                                        row_num_vect$row_num,
                                                        stored_data$data)
        }

        brush_data$windows <- empty_window_data
    })

    observeEvent(input$prev_chunk, {
        trials <- lazy_trials()
        
        single_trial_windows <- get_single_trial_windows(trials,
                                                         row_num_vect$row_num,
                                                         brush_data$windows)

        stored_data$data <- update_stored_data(stored_data$data, single_trial_windows)

        row_num_vect$row_num <- row_num_vect$row_num - 1
        if (row_num_vect$row_num < 1) {
            row_num_vect$row_num <- nrow(trials)
        }

        if (!is.null(stored_data$data)) {
            stored_data$data <- clear_saved_window_data(trials,
                                                        row_num_vect$row_num,
                                                        stored_data$data)
        }

        brush_data$windows <- empty_window_data
    })

    output$download_data <- downloadHandler(filename = "shiny_eyes_output.csv",
                                            content = function(file) {
                                                trials <- lazy_trials()

                                                single_trial_windows <- get_single_trial_windows(trials,
                                                                                                 row_num_vect$row_num,
                                                                                                 brush_data$windows)

                                                stored_data$data <- update_stored_data(stored_data$data, single_trial_windows)
                                                write_csv(stored_data$data, file)},
                                            contentType = "text/csv")

    trial_data <- reactive({
        trials <- lazy_trials()
        
        if (!is.null(trials)) {
            trials %>%
                slice(row_num_vect$row_num) %>%
                left_join(lazy_loaded_data())
        } else {
            NULL
        }})

    output$trial_name <- renderText({
        get_chunk_title(lazy_trials(), row_num_vect$row_num)
    })

    brush_data <- reactiveValues(windows = empty_window_data)

    observeEvent(input$undo_brush, {
        if (nrow(brush_data$windows) > 0) {
            brush_data$windows <- head(brush_data$windows, -1)  # Remove last window
        }
    })

    observeEvent(input$clear_brush, {
        brush_data$windows <- empty_window_data
    })

    observeEvent(input$keypress, {
        if (input$keypress != 0) {
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
        }
    })

    output$trace_plot <- renderPlot({
        p <- ggplot()

        if (input$x_variable != "" & input$y_variable != "" & input$sample_variable != "") {
            p <- ggplot(trial_data(), aes_string(x = input$sample_variable)) +
                 geom_line(aes_string(y = input$x_variable), color = "red") +
                 geom_line(aes_string(y = input$y_variable), color = "blue")
        }

        if (nrow(brush_data$windows) > 0) {
            names(colors) <- c(input$window_1_name, input$window_2_name, input$window_3_name)

            p <- p + geom_rect(data = brush_data$windows,
                               aes(x = NULL, xmin = xmin, xmax = xmax, fill = window_name),
                               ymin = -Inf, ymax = Inf, alpha = 0.3) +
                     scale_fill_manual(values = colors)
        }
        p
    })

    gaze_gif_data <- reactiveValues(render_gif = FALSE)

    observeEvent(input$render_gif_button, {gaze_gif_data$render_gif <- TRUE})
    
    observeEvent(input$hide_gif_button, {gaze_gif_data$render_gif <- FALSE})

    output$gaze_gif <- renderImage({
        if (gaze_gif_data$render_gif) {
            gaze_x_scale_min <- as.numeric(input$gaze_x_scale_min)
            gaze_x_scale_max <- as.numeric(input$gaze_x_scale_max)
            gaze_y_scale_min <- as.numeric(input$gaze_y_scale_min)
            gaze_y_scale_max <- as.numeric(input$gaze_y_scale_max)
            n_frames <- as.numeric(input$n_frames)

            p <- ggplot(trial_data(), aes_string(x = input$x_variable, y = input$y_variable)) +
                 annotate(geom = "text", label = "+",
                          x = gaze_x_scale_max/2,
                          y = gaze_y_scale_max/2, size = 15) +
                 geom_point() +
                 coord_equal(xlim = c(gaze_x_scale_min, gaze_x_scale_max),
                             ylim = c(gaze_y_scale_min, gaze_y_scale_max),
                             expand = FALSE) +
                 transition_time(!!sym(input$sample_variable)) +
                 labs(title = "Sample: {round(frame_time)}",
                      x = "",
                      y = "")

            anim_save("outfile.gif", animate(p, nframes = n_frames))
            list(src = "outfile.gif", contentType = "image/gif")
        } else {
            ggsave("outfile.png", ggplot(), width = 1, height = 1, units = "in")
            list(src = "outfile.png", contentType = "image/gif")
        }
    }, deleteFile = TRUE)
}

shinyApp(ui, server)
