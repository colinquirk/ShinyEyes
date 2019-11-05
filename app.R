library(shiny)
library(tidyverse)

options(shiny.maxRequestSize=1*1024^3)

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
        "Active Window: ", textOutput("active_window", inline = TRUE), HTML('<br/><br/>'),
        downloadButton("download_data", "Download Data")
        
    ),
    
    mainPanel(
        tags$script(
            '$(document).on("keypress", function (e) {
                Shiny.onInputChange("keypress", e.key);
            });'
        ),
        plotOutput("gaze_gif"),
        plotOutput("trace_plot")
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
    
    output$active_window = reactive({
        if (!is.null(input$keypress)){
            switch(input$keypress, '1' = input$window_1_name, '2' = input$window_2_name)
        }
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
    
    #output$download_data = downloadHandler(filename = "shiny_eyes_output.csv",
    #                                       content = function(file){write_csv(loaded_data, file)},
    #                                       contentType = 'text/csv')
    
    output$gaze_gif = renderImage({
        if(input$x_variable != "" & input$y_variable != "" & input$sample_variable != "") {
            trial_data = lazy_loaded_data() %>% 
                filter(subject == 6, trial == 6)
            
            p = ggplot(trial_data, aes_string(x = input$x_variable, y = input$y_variable)) +
                    annotate(geom = 'text', label = '+', x = 960, y = 540, size = 15) +
                    geom_point() +
                    coord_equal(xlim = c(560, 1360), ylim = c(240, 840)) +
                    transition_time(!!sym(input$sample_variable)) +
                    labs(title = 'Sample: {round(frame_time)}',
                         x = '',
                         y = '')
            
            anim_save("outfile.gif", animate(p))
        } else {
            ggsave("outfile.png", ggplot())
            list(src = "outfile.png", contentType = 'image/gif')
        }
        list(src = "outfile.gif", contentType = 'image/gif')
    }, deleteFile = TRUE)
    
    output$trace_plot = renderPlot({
        p = ggplot()

        print(input$gaze_x)
        
        if(input$x_variable != "" & input$y_variable != "" & input$sample_variable != "") {
            trial_data = lazy_loaded_data() %>% 
                filter(subject == 6, trial == 6)
    
            p = ggplot(trial_data, aes_string(x = input$sample_variable)) +
                    geom_line(mapping=aes_string(y = input$x_variable), color='red') +
                    geom_line(mapping=aes_string(y = input$y_variable), color='blue')
        }
        p
    })
}

shinyApp(ui, server)