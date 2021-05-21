#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# source('main2.0.R')

library(tidyverse)
library(reshape2)

AEGN_evaluation <- function (dfs) {
    
    questvec <- NULL #unlist(str_split(rawquest, ", "))
    ratingvec <- c('', 'No', 'A little', 'Moderately', 'Significantly', 'Very significantly', 'Unsure')
    # filelist <- list.files('./Inputs', pattern = "*.csv")
    filesnum <- length(dfs)
    #questvec <- c('Create better networks for your funding', 'Discover funding opportunities', 'Bleep blorp')
    envector <- NULL
    mastervec <- c('Unsure', 'No', 'A little', 'Moderately', 'Significantly', 'Very significantly')
    
    # Read in all the files
    
    dataframes <- dfs %>%
        # Set names to second row
        map(~set_names(.x, .x[1, ])) %>%
        map(~subset(.x, select = which(!duplicated(names(.x)) & 
                                           names(.x) != "" &
                                           names(.x) != "NA" & 
                                           names(.x) != "Open-Ended Response" )) %>% 
                slice(-1) %>%
                map_df(as.factor))
    # Select only columns with impact scores
    
    rating_questions <- dataframes %>%
        map(~.x %>%
                select_if(~all(levels(.) %in% ratingvec)) %>%
                gather(key = "area", value = "response") %>%
                group_by_all() %>%
                summarise(n = n())
        )
    
    # Get totals across all events
    
    all_events <- rating_questions %>%
        bind_rows %>%
        group_by(area, response) %>%
        summarise(n = sum(n))
    
    # Get summary for events
    by_event <- bind_rows(rating_questions, .id = 'event') %>%
        mutate(event = gsub(".csv", "", event)) %>%
        filter(!is.na(response), response != "")
        # select(-event_name)
    
    by_event$response <- factor(by_event$response, levels = c("No", "A little", "Moderately", "Significantly", "Very significantly", "Unsure"))
    
    by_event <- by_event %>%
        expand(event, area, response) %>% 
        left_join(by_event)
    
    # write.csv(all_events, 'all_events.csv')
    # write.csv(by_event, 'by_event.csv')
    return(by_event)
}




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AEGN Events Evaluation Data Processor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file_list", 
                      "Choose CSV File(s)",
                      multiple = TRUE,
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv")
            ),
            selectInput(
                'pivot',
                'Select data format',
                c('Raw', 'Pivot'),
                selected = 'Raw',
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            h5("This tool is designed to process event evaluation exports (.csv format) from SurveyMonkey.
             You can upload one or multiple event evaluation spreadsheets above."),
            h5("For help, email patrick@aegn.org.au.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$contents <- renderDataTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file_list
        
        if (is.null(inFile))
            return(NULL)
        
        dfs <- map(inFile$datapath, ~read.csv(.x))
        names(dfs) <- inFile$name
        summary_data <- AEGN_evaluation(dfs = dfs)
        
        if (input$pivot == 'Raw') {
            summary_data
        } else if (input$pivot == 'Pivot') {
            pivot_table <- summary_data %>%
                pivot_wider(names_from = response, values_from = n)
        
            pivot_table$total <- rowSums(pivot_table %>%
                                             ungroup() %>%
                                             select_if(names(.) %in% c("A little", 
                                                    "Moderately",
                                                    "Very significantly",
                                                    "No",
                                                    "Significantly",
                                                    "Unsure")),
                                         na.rm = TRUE)
            
            pivot_table
        } 
        
    },
    extensions = c('Buttons'), 
    options = list(
        pageLength = -1,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
}

shinyApp(ui, server)
