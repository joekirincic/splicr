
library(tidyverse)
library(shiny)
library(miniUI)
library(shinyjs)
library(rhandsontable)
source("utilities.R")
options(bitmapType="cairo")

raw_data <- tibble(
  entity_id = rep(1:3, 100),
  entity_name = rep(c("A","B","C"), 100),
  date = as_date(now()) %>% seq(., . + days(99), "days") %>% rep(., 3),
  metric_ref = rnorm(mean = 100, sd = 10, n = 300),
  metric_test = metric_ref - rnorm(mean = 0, sd = 10, n = 300),
  metric_diff = metric_ref - metric_test
)

df1 <- raw_data %>% select(-c(metric_test, metric_diff))

df2 <- raw_data %>% select(-c(metric_ref, metric_diff))

splicr <- function(df1, df2){
  ui <- miniPage(
    
    # Application title
    gadgetTitleBar("splicr"),
    
    miniTabstripPanel(
      miniTabPanel("Combine", icon = icon("copy"),
                   # Sidebar with a slider input for number of bins
                   miniContentPanel(
                     h1("Configure splicr for further analysis"),
                     sidebarLayout(
                       sidebarPanel(
                         fluidRow(
                           actionButton("add_mapping", label = "+"),
                           actionButton("remove_mapping", label = "-"),
                           actionButton("reset_mappings", label = "Reset"),
                           br(),
                           br(),
                           rHandsontableOutput("rendered_mappings"),
                           #uiOutput("make_column_chooser_ui"),
                           br(),
                           actionButton("merge_button", "Splice")
                           )),
                       mainPanel(
                         fluidRow(
                           tableOutput("merged_glimpse")
                         )
                       )
                   )
      )),
      miniTabPanel("Analyze", icon = icon("eye"), 
                   miniContentPanel(uiOutput("error_plot_controls"),
                                    plotOutput("error_plot"),
                                    hr(),
                                    uiOutput("missing_data_summary")
                                    )
                   ),
      miniTabPanel("Export", icon = icon("download"), miniContentPanel("This is where the user would go to export stuff."))
    )
  )
  
  server <- function(input, output, session) {
    
    total_mappings <- reactiveVal(1)
    
    mappings <- reactiveVal(tibble(ref_col = names(df1)[1], map_type = c("compare"), test_col = c(names(df2)[1])))
    
    compare_cols <- reactiveVal(NULL)
    
    join_condition <- reactiveVal(NULL)
    
    observeEvent(input$add_mapping, {
      new_count <- total_mappings() + 1
      total_mappings(new_count)
      updated_mappings <- mappings() %>% add_row(ref_col = names(df1)[1], map_type = "compare", test_col = names(df2)[1])
      mappings(updated_mappings)
      compare_cols(mappings() %>% filter(map_type == "compare"))
      output$rendered_mappings <- renderRHandsontable({rhandsontable(mappings(), rowHeaders = NULL) %>%
          hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
          hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
          hot_col("test_col", type = "dropdown", source = names(df2))})
      print(mappings())
    })
    
    observeEvent(input$remove_mapping, {
      new_count <- total_mappings() - 1
      total_mappings(new_count)
      updated_mappings <- mappings()[1:new_count, ]
      mappings(updated_mappings)
      compare_cols(mappings() %>% filter(map_type == "compare"))
      output$rendered_mappings <- renderRHandsontable({rhandsontable(mappings(), rowHeaders = NULL) %>%
        hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
        hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
        hot_col("test_col", type = "dropdown", source = names(df2))})
      print(mappings())
    })
    
    observeEvent(input$reset_mappings, {
      total_mappings(1)
      mappings(tibble(ref_col = names(df1)[1], map_type = c("compare"), test_col = c(names(df2)[1])))
      compare_cols(NULL)
      output$rendered_mappings <- renderRHandsontable({rhandsontable(mappings(), rowHeaders = NULL) %>%
          hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
          hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
          hot_col("test_col", type = "dropdown", source = names(df2))})
      merged <- NULL
      output$merged_glimpse <- NULL
    })
    
    observe({
      if (!is.null(input$rendered_mappings$changes$changes)){
        updated_mappings <- hot_to_r(input$rendered_mappings) %>% as_tibble(.)
        mappings(updated_mappings)
        compare_cols(mappings() %>% filter(map_type == "compare"))
      }
    })
    
    merged <- eventReactive(input$merge_button, {
      cc <- set_names(compare_cols()$test_col, compare_cols()$ref_col)
      
      jc <- infer_join_condition(df1, df2, excluding = compare_cols()$ref_col) %>%
        tibble(
          ref_col = names(.),
          map_type = "join",
          test_col = unname(.)
        ) %>% select(-c(.))
      
      join_condition(jc)
      
      updated_mappings <- bind_rows(compare_cols(), join_condition())
      mappings(updated_mappings)
      
      merged_init <- df1 %>% 
        left_join(df2, by = set_names(join_condition()$test_col, join_condition()$ref_col))
      merged_final <- merged_init %>% add_diff_cols(., cc)
      return(merged_final)
    })
    
    observeEvent(input$merge_button, {
      merged_mini <- merged() %>% head(.) %>% mutate_all(as.character)
      
      output$merged_glimpse <- renderTable(merged_mini, align = "c", bordered = TRUE)
      
    })
    
    observe({
      if (!is.null(input$rendered_mappings$changes$changes)) {
        updated_mappings <- hot_to_r(input$rendered_mappings) %>% as_tibble(.)
        mappings(updated_mappings)
        output$rendered_mappings <- renderRHandsontable({rhandsontable(mappings(), rowHeaders = NULL) %>%
            hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
            hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
            hot_col("test_col", type = "dropdown", source = names(df2))})
        print(mappings())
      }
    })
    
    observeEvent(input$merge_button, {
      
      output$error_plot_controls <- renderUI({
        fluidRow(
          column(4, selectInput("error_plot_x", label = "x-axis", choices = names(merged()))),
          column(4, selectInput("error_plot_y", label = "y-axis", choices = names(merged()))),
          column(4, sliderInput("error_plot_threshold", label = "error threshold (absolute value)", min = 0, max = 10, value = 10))
        )
      })
      
      output$error_plot <- renderPlot({
        plot_errors(merged(), sym(input$error_plot_x), sym(input$error_plot_y), as.numeric(input$error_plot_threshold))
      })
      
      missing_data_summary <- merged() %>% summarise_all(function(x){mean(is.null(.))})
      
      output$missing_data_summary <- renderTable(
        missing_data_summary,
        align = "c", 
        bordered = TRUE, 
        caption = "Missing Data Summary (%)", 
        caption.placement = getOption("xtable.caption.placement", "top")
        )
    })
    
    observeEvent(input$done, {
      returnValue <- merged()
      stopApp(returnValue)
    })
    
    observeEvent(input$cancel, {
      stopApp(NULL)
    })
    
  }
  
  dialogTitle <- "splicr - automated data validation for RStudio"
  # Run the application 
  runGadget(ui, server, viewer = dialogViewer(dialogName = dialogTitle, width = 1000, height = 1000))
  
}