library(shiny)
library(tidyverse)
library(gganimate)

theme_set(theme_minimal())

options(shiny.maxRequestSize=1*1024^3)

colors = c('#F8766D', '#7CAE00', '#00BFC4')

ui <- pageWithSidebar(
    headerPanel("Shiny Eyes"),
    
    sidebarPanel(
        fileInput("input_filepath", "Data File", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        selectInput("grouping_variables", "Grouping Variables", NULL, multiple = TRUE),
        selectInput("sample_variable", "Sample Variable", NULL, multiple = FALSE),
        selectInput("x_variable", "Gaze X Variable", NULL, multiple = FALSE),
        selectInput("y_variable", "Gaze Y Variable", NULL, multiple = FALSE),
        textInput("window_1_name", "Window 1 Name", value = "Fixation"),
        textInput("window_2_name", "Window 2 Name", value = "Saccade"),
        textInput("window_3_name", "Window 3 Name", value = "Blink"),
        downloadButton("download_data", "Download Data")
        
    ),
    
    mainPanel(
        tags$script(
            '$(document).on("shiny:value", function(e) {
                if(e.name == "trace_plot") {
                    Shiny.setInputValue("keypress", 0);
                }
            });
            
            $(document).on("keypress", function (e) {
                Shiny.setInputValue("keypress", e.key);
            });'
        ),
        textOutput('trial_name'),
        actionButton('prev_chunk', 'Prev Chunk'),
        actionButton('undo_brush', 'Undo Window'),
        actionButton('clear_brush', 'Clear Windows'),
        actionButton('next_chunk', 'Next Chunk'),
        plotOutput("trace_plot", brush = brushOpts(id = "image_brush", direction = 'x')),
        actionButton('render_gif_button', 'Render Gaze Gif'),
        actionButton('hide_gif_button', 'Hide Gaze Gif'),
        plotOutput("gaze_gif")
    )
)


server <- function(input, output, session) {
    lazy_loaded_data = reactive({
        if(!is.null(input$input_filepath$datapath)) {
            data = read_csv(input$input_filepath$datapath)
        } else {
            data = NULL
        }
        data
    })

    observe({
        try({
            if (!is.null(input$input_filepath)){
                column_names = c("", colnames(lazy_loaded_data()))
                updateSelectInput(session, "grouping_variables", choices = column_names)
                updateSelectInput(session, "sample_variable", choices = column_names)
                updateSelectInput(session, "x_variable", choices = column_names)
                updateSelectInput(session, "y_variable", choices = column_names)
            }
        })
        
    })

    lazy_trials = reactive({
        data = lazy_loaded_data()
        if(!is.null(data) & !is.null(input$grouping_variables)){
            trials = distinct_at(data, vars(input$grouping_variables))
        } else {
            trials = NULL
        }
        trials
    })
    
    stored_data = reactiveValues(data = NULL)
    
    row_num_vect = reactiveValues(row_num = 1)
    
    observeEvent(input$next_chunk, {
        single_trial_windows = lazy_trials() %>%
            slice(row_num_vect$row_num) %>% 
            merge(brush_data$windows)
        
        stored_data$data = rbind(stored_data$data,
                                 single_trial_windows,
                                 stringsAsFactors = FALSE)
        
        row_num_vect$row_num = row_num_vect$row_num + 1
        if(row_num_vect$row_num > nrow(lazy_trials())) {
            row_num_vect$row_num = 1
        }
        
        if(!is.null(stored_data$data)){
            trial_info = lazy_trials() %>%
                slice(row_num_vect$row_num)
            
            stored_data$data = anti_join(stored_data$data, trial_info)
        }
        
        brush_data$windows = tibble(xmin=numeric(0), xmax=numeric(0), window_name=character(0))
    })
    
    observeEvent(input$prev_chunk, {
        single_trial_windows = lazy_trials() %>%
            slice(row_num_vect$row_num) %>% 
            merge(brush_data$windows)
        
        stored_data$data = rbind(stored_data$data,
                                 single_trial_windows,
                                 stringsAsFactors = FALSE)
        
        row_num_vect$row_num = row_num_vect$row_num - 1
        if(row_num_vect$row_num < 1) {
            row_num_vect$row_num = nrow(lazy_trials())
        }
        
        if(!is.null(stored_data$data)){
            trial_info = lazy_trials() %>%
                slice(row_num_vect$row_num)
            
            stored_data$data = anti_join(stored_data$data, trial_info)
        }
        
        brush_data$windows = tibble(xmin=numeric(0), xmax=numeric(0), window_name=character(0))
    })
    
    output$download_data = downloadHandler(filename = "shiny_eyes_output.csv",
                                           content = function(file){write_csv(stored_data$data, file)},
                                           contentType = 'text/csv')
    
    trial_data = reactive({
        if(!is.null(lazy_trials())) {
            lazy_trials() %>%
                slice(row_num_vect$row_num) %>% 
                left_join(lazy_loaded_data()) 
        } else {
            NULL
        }})
        
    output$trial_name = renderText({
        str = ''
        
        if(!is.null(lazy_trials())) {
           for(name in colnames(lazy_trials())) {
               str = paste0(str, name, ':', lazy_trials()[[row_num_vect$row_num, name]], ' ')
           }
        }
        str
    })

    brush_data = reactiveValues(windows = tibble(xmin=numeric(0), xmax=numeric(0), window_name=character(0)))

    observeEvent(input$undo_brush, {
        if(nrow(brush_data$windows) > 0) {
            brush_data$windows = head(brush_data$windows, -1)
        }
    })
    
    observeEvent(input$clear_brush, {brush_data$windows = tibble(xmin=numeric(0), xmax=numeric(0), window_name=character(0))})
    
    observeEvent(input$keypress, {
        if(input$keypress != 0) {
            window_name = switch(input$keypress, '1' = input$window_1_name, '2' = input$window_2_name, '3' = input$window_3_name)

            brush_data$windows = rbind(brush_data$windows,
                                       list(xmin = input$image_brush$xmin, xmax = input$image_brush$xmax, window_name = window_name),
                                       stringsAsFactors = FALSE)
        
            brush_data$windows$window_name = factor(brush_data$windows$window_name,
                                                    levels = c(input$window_1_name, input$window_2_name, input$window_3_name))
        }
    })
    
    output$trace_plot = renderPlot({
        p = ggplot()
        
        if(input$x_variable != "" & input$y_variable != "" & input$sample_variable != "") {
            p = ggplot(trial_data(), aes_string(x = input$sample_variable)) +
                geom_line(mapping=aes_string(y = input$x_variable), color='red') +
                geom_line(mapping=aes_string(y = input$y_variable), color='blue')
        }
        
        if(nrow(brush_data$windows) > 0) {
            names(colors) = c(input$window_1_name, input$window_2_name, input$window_3_name)
            
            p = p +
                geom_rect(data = brush_data$windows, mapping = aes(x = NULL, xmin = xmin, xmax = xmax, fill = window_name), ymin = -Inf, ymax = Inf, alpha = 0.3) +
                scale_fill_manual(values = colors)
        }
        p
    })
    
    gaze_gif_data = reactiveValues(render_gif = FALSE)
    
    observeEvent(input$render_gif_button, {gaze_gif_data$render_gif = TRUE})
    
    observeEvent(input$hide_gif_button, {gaze_gif_data$render_gif = FALSE})
    
    output$gaze_gif = renderImage({
        if(gaze_gif_data$render_gif) {
            p = ggplot(trial_data(), aes_string(x = input$x_variable, y = input$y_variable)) +
                annotate(geom = 'text', label = '+', x = 960, y = 540, size = 15) +
                geom_point() +
                coord_equal(xlim = c(560, 1360), ylim = c(240, 840)) +
                transition_time(!!sym(input$sample_variable)) +
                labs(title = 'Sample: {round(frame_time)}',
                     x = '',
                     y = '')
            
            anim_save("outfile.gif", animate(p, nframes = 18))
            list(src = "outfile.gif", contentType = 'image/gif')
        } else {
            ggsave("outfile.png", ggplot(), width = 1, height = 1, units = "in")
            list(src = "outfile.png", contentType = 'image/gif')
        }
    }, deleteFile = TRUE)
}

shinyApp(ui, server)
