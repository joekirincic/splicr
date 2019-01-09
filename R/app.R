# Main application

# library(tidyverse)
# library(rlang)
# library(lubridate)
# library(shiny)
# library(miniUI)
# library(rhandsontable)
source("R/merging_functions.R")
source("R/plotting_functions.R")

if(Sys.info()["sysname"] == "windows"){
  options(bitmapType="cairo")
}

raw_data <- tibble::tibble(
  entity_id = rep(1:3, 100),
  entity_name = rep(c("A","B","C"), 100),
  date = lubridate::as_date(lubridate::now()) %>% seq(., . + lubridate::days(99), "days") %>% rep(., 3),
  metric1_ref = rnorm(mean = 100, sd = 10, n = 300),
  metric1_test = metric1_ref - rnorm(mean = 0, sd = 10, n = 300),
  metric1_diff = metric1_ref - metric1_test,
  metric2_ref = rnorm(mean = 100, sd = 10, n = 300),
  metric2_test = metric2_ref - rnorm(mean = 0, sd = 10, n = 300),
  metric2_diff = metric2_ref - metric2_test,
  metric3_ref = rnorm(mean = 100, sd = 10, n = 300),
  metric3_test = metric3_ref - rnorm(mean = 0, sd = 10, n = 300),
  metric3_diff = metric3_ref - metric3_test
)

df1 <- raw_data %>% dplyr::select(-c(metric1_test, metric1_diff, metric2_test, metric2_diff, metric3_test, metric3_diff))

df2 <- raw_data %>% dplyr::select(-c(metric1_ref, metric1_diff, metric2_ref, metric2_diff, metric3_ref, metric3_diff))

splicr <- function(df1, df2){
  ui <- miniUI::miniPage(
    
    # Application title
    miniUI::gadgetTitleBar("splicr"),
    
    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Combine", icon = icon("copy"),
                   # Sidebar with a slider input for number of bins
                   miniUI::miniContentPanel(
                     shiny::sidebarLayout(
                       shiny::sidebarPanel(
                         shiny::fluidRow(
                           shiny::actionButton("add_mapping", label = "+"),
                           shiny::actionButton("remove_mapping", label = "-"),
                           shiny::actionButton("reset_mappings", label = "Reset"),
                           shiny::br(),
                           shiny::br(),
                           rhandsontable::rHandsontableOutput("rendered_mappings"),
                           #tags$style(HTML(".handsontable {overflow: visible;}")),
                           shiny::br(),
                           shiny::actionButton("merge_button", "Splice")
                           )),
                       shiny::mainPanel(
                         shiny::fluidRow(
                           shiny::tableOutput("merged_glimpse")
                         )
                       )
                   )
      )),
      miniUI::miniTabPanel("Analyze", icon = icon("eye"), 
                           miniUI::miniContentPanel(
                             miniUI::fillCol(
                               shiny::h3("Errors - Summary Statistics"),
                               shiny::tableOutput("error_summary_table"),
                               shiny::br(),
                               shiny::h3("Errors - Distribution Plot"),
                               shiny::uiOutput("error_plot_controls"),
                               shiny::plotOutput("error_plot")
                             )
                           ))
    )
  )
  
  server <- function(input, output, session) {
    
    total_mappings <- shiny::reactiveVal(1)
    
    mappings <- shiny::reactiveVal(tibble(ref_col = names(df1)[1], map_type = c("compare"), test_col = c(names(df2)[1])))
    
    compare_cols <- shiny::reactiveVal(NULL)
    
    join_condition <- shiny::reactiveVal(NULL)
    
    output$rendered_mappings <- rhandsontable::renderRHandsontable({rhandsontable::rhandsontable(mappings(), rowHeaders = NULL) %>%
        rhandsontable::hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
        rhandsontable::hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
        rhandsontable::hot_col("test_col", type = "dropdown", source = names(df2))})
    
    shiny::observeEvent(input$add_mapping, {
      new_count <- total_mappings() + 1
      total_mappings(new_count)
      updated_mappings <- mappings() %>% tibble::add_row(ref_col = names(df1)[1], map_type = "compare", test_col = names(df2)[1])
      mappings(updated_mappings)
      compare_cols(mappings() %>% dplyr::filter(map_type == "compare"))
      output$rendered_mappings <- rhandsontable::renderRHandsontable({rhandsontable::rhandsontable(mappings(), rowHeaders = NULL) %>%
          rhandsontable::hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
          rhandsontable::hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
          rhandsontable::hot_col("test_col", type = "dropdown", source = names(df2))})
      print(mappings())
    })
    
    shiny::observeEvent(input$remove_mapping, {
      new_count <- total_mappings() - 1
      total_mappings(new_count)
      updated_mappings <- mappings()[1:new_count, ]
      mappings(updated_mappings)
      compare_cols(mappings() %>% dplyr::filter(map_type == "compare"))
      output$rendered_mappings <- rhandsontable::renderRHandsontable({rhandsontable::rhandsontable(mappings(), rowHeaders = NULL) %>%
          rhandsontable::hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
          rhandsontable::hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
          rhandsontable::hot_col("test_col", type = "dropdown", source = names(df2))})
      print(mappings())
    })
    
    observeEvent(input$reset_mappings, {
      total_mappings(1)
      mappings(tibble::tibble(ref_col = names(df1)[1], map_type = c("compare"), test_col = c(names(df2)[1])))
      compare_cols(NULL)
      output$rendered_mappings <- rhandsontable::renderRHandsontable({rhandsontable::rhandsontable(mappings(), rowHeaders = NULL) %>%
          rhandsontable::hot_col("ref_col", type = "dropdown", source = names(df1)) %>%
          rhandsontable::hot_col("map_type", type = "dropdown", source = c("join", "compare")) %>%
          rhandsontable::hot_col("test_col", type = "dropdown", source = names(df2))})
      merged <- NULL
      output$merged_glimpse <- NULL
    })
    
    shiny::observe({
      if (!is.null(input$rendered_mappings$changes$changes)){
        updated_mappings <- rhandsontable::hot_to_r(input$rendered_mappings) %>% tibble::as_tibble(.)
        mappings(updated_mappings)
        compare_cols(mappings() %>% dplyr::filter(map_type == "compare"))
      }
    })
    
    merged <- shiny::eventReactive(input$merge_button, {
      cc <- rlang::set_names(compare_cols()$test_col, compare_cols()$ref_col)
      
      jc <- infer_join_condition(df1, df2, excluding = compare_cols()$ref_col) %>%
        tibble::tibble(
          ref_col = names(.),
          map_type = "join",
          test_col = unname(.)
        ) %>% dplyr::select(-c(.))
      
      join_condition(jc)
      
      updated_mappings <- dplyr::bind_rows(compare_cols(), join_condition())
      mappings(updated_mappings)
      
      merged_init <- df1 %>% 
        left_join(df2, by = rlang::set_names(join_condition()$test_col, join_condition()$ref_col))
      merged_final <- merged_init %>% add_diff_cols(., cc)
      return(merged_final)
    })
    
    shiny::observeEvent(input$merge_button, {
      merged_mini <- merged() %>% dplyr::head(.) %>% dplyr::mutate_all(as.character)
      
      output$merged_glimpse <- shiny::renderTable(merged_mini, align = "c", bordered = TRUE)
      
    })
    
    shiny::observe({
      if (!is.null(input$rendered_mappings$changes$changes)) {
        updated_mappings <- rhandsontable::hot_to_r(input$rendered_mappings) %>% tibble::as_tibble(.)
        mappings(updated_mappings)
        output$rendered_mappings <- rhandsontable::renderRHandsontable({rhandsontable::rhandsontable(mappings(), rowHeaders = NULL) %>%
            rhandsontable::hot_col("ref_col", type = "autocomplete", source = names(df1)) %>%
            rhandsontable::hot_col("map_type", type = "autocomplete", source = c("join", "compare")) %>%
            rhandsontable::hot_col("test_col", type = "autocomplete", source = names(df2))
          })
      }
    })
    
    shiny::observeEvent(input$merge_button, {
      
      output$error_summary_table <- shiny::renderTable({
        merged() %>% dplyr::select(contains("_diff")) %>% tidyr::gather(key = "column", value = "error") %>%
          dplyr::group_by(column) %>%
          dplyr::summarise(mean = mean(error, na.rm = TRUE),
                    std = sd(error),
                    min = min(error),
                    Q1 = quantile(error, c(0.25)),
                    Q2 = quantile(error, c(0.50)),
                    Q3 = quantile(error, c(0.75)),
                    max = max(error))
      })
      
      output$error_plot_controls <- shiny::renderUI({
        shiny::fluidRow(
          shiny::column(4, shiny::selectInput("error_plot_x", label = "x-axis", choices = names(merged()))),
          shiny::column(4, shiny::selectInput("error_plot_y", label = "y-axis", choices = names(merged()) %>% `[`(stringr::str_detect(., "_diff")))),
          shiny::column(4, shiny::numericInput("error_plot_threshold", label = "error threshold (absolute value)", value = 1))
        )
      })
      
      output$error_plot <- shiny::renderPlot({
        plot_errors(merged(), rlang::sym(input$error_plot_x), rlang::sym(input$error_plot_y), as.numeric(input$error_plot_threshold))
      })
      
    })
    
    shiny::observeEvent(input$done, {
      returnValue <- merged()
      shiny::stopApp(returnValue)
    })
    
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })
    
  }
  
  dialogTitle <- "splicr - automated data validation for RStudio"
  # Run the application 
  shiny::runGadget(ui, server, viewer = shiny::dialogViewer(dialogName = dialogTitle, width = 1000, height = 1000))
  
}