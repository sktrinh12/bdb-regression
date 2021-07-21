source('global.R')
source('regression_report.R')

mypptx <- read_pptx("bd_template_homemade.pptx")

## CONSTANTS
ui_font_size <- 16
ui_data_point_size <- 7
ui_eqn_size <- 5
reports_font_size <- 12
reports_data_point_size <- 4
reports_eqn_size <- 5
RAW_SL_TO_DAYS <- 365.25
SAP_SL_TO_DAYS <- 360

fontname <- "Arial"

server = function(input, output) {
    
  ################## Sidebar Panel UI ##########################
  # Model Criteria User Inputs from GUI
  confidenceInterval <- reactive({ input$CI })
  threshold_y <- reactive({ input$threshold })
  poly_order <- reactive({ order_of_polynomial(input$polynomial_order) })
  
  # Download Template Stats File
  output$download_template_file <- downloadHandler(
      filename <- function() {
          paste("stats_template", "xlsx", sep=".")
      },
      content = function(file) {
          file.copy('stability_stats_template.xlsx', file)
      },
      contentType = "text/xlsx"
  )
  # Download Example Stats File
  output$download_example_file <- downloadHandler(
      filename <- function() {
          paste("stats_example", "xlsx", sep=".")
      },
      content = function(file) {
          file.copy('stability_stats_example.xlsx', file)
      },
      contentType = "text/xlsx"
  )
  
  # fileInput UI based on analysis type selected
  output$manual_or_omiq <- renderUI({
      
      if(input$analysis_type == "Manual"){
          tagList(div(style = "display: grid; grid-template-columns: 250px 250px;",
                      div(style = "padding:5px;", downloadButton("download_template_file", "Download Stats Template")),
                      div(style = "padding:5px;", downloadButton("download_example_file", "Download Stats Example"))),
                  br(),
                  div(style = "display: grid; grid-template-columns: auto;",
                      fileInput("raw_upload","Choose stats file to upload",
                                accept = c('text/xlsx',
                                           '.xlsx'))))
      } else if(input$analysis_type == "OMIQ"){
          tagList(
              textInput('plate_id', "Plate ID", placeholder = "YYYY-MM-DD-Initials-Plate #"),
              helpText('This will be the name of the folder that the regression report will be saved to on the VM.'),
              fileInput(
                  "raw_upload",
                  "Choose stats file to upload",
                  accept = c('text/csv',
                             '.csv'))
              
              )
      }
  })
  
  # If OMIQ analysis, add marker name and selectInput to select cell pops to analyze
  # From regression_report.R file
  marker_name <- reactive({
    get_marker_name(input$raw_upload$datapath)
  })
  
  output$marker_name_output <- renderText({
    marker_name()
  })
  
  output$manual_or_omiq_cell_pop <- renderUI({
      req(input$raw_upload)
      if(input$analysis_type == "OMIQ"){
          tagList(
              tags$head(tags$style("
                #marker_name_output{
                display:inline
                }")),
              strong('Marker Name: ',style="display:inline;"), textOutput("marker_name_output"),
              br(),
              br(),
              selectInput('cell_pop_input', "Select Cell Population", choices = c(unique(
                  readr::read_csv(input$raw_upload$datapath)$pop)))
          )
      }
      else{
          tagList()
      }
  })
  
  # rendering UI for downloadable output options
  output$downloadable_outputs_ui <- renderUI({
      if(input$analysis_type == "Manual"){
          tagList(
              fluidRow(
                  column(8,textAreaInput("model_system_data","QC Model System Data",
                                         placeholder = "This will be copied to the top right corner of every PPT slide.",
                                         height = "100px")
                  ),
                  column(4,radioButtons('pop_number','Cell Pop #',
                                        choices = c("P1", "P2", "P3"),
                                        inline = TRUE)
                  )),
              br(),
              div(style = "display: grid; grid-template-columns: 250px 250px; padding: 5px;",
                  div(textInput("filename_output", "Name Final PPT", placeholder = "Exclude .pptx")),
                  div(style = "display: flex; align-items: center; justify-content: center; padding-top: 10px", 
                      downloadButton("pptx_id", "Download PPT", class = "btn-primary")))
              
          )
      }
      else if(input$analysis_type == "OMIQ"){
          tagList(
              actionButton('create_regr_report', "Create Individual Regression Report", class = "btn-primary"),
              br(),
              br(),
              textOutput('indiv_report_text')%>%
                  tagAppendAttributes(style= 'font-size: 16px; font-weight: bold;'),
              br(),
              helpText('Upload all individual regression reports created using the App for this experiment, and the final stability report with the initial regression generated from OMIQ when you are ready to bundle all regression reports together.'),
              br(),
              fileInput('regression_reports_indiv', "Upload Individual Regression Report", multiple = TRUE, accept = c('.pdf')),
              fileInput("omiq_report_upload","Upload OMIQ Stability Report",accept = c(".pdf")),
              actionButton("regression_report_bundled", "Create Bundled Regression Report", class = "btn-primary"),
              br(),
              textOutput('bundld_report_text')%>%
                  tagAppendAttributes(style= 'font-size: 16px; font-weight: bold;')
          )
      }
  })

  ############### Handling initial stats file upload ##################
  # STEP 1: Upload Raw Stats (without % 4C Reference MFI Data)
  # STEP 1a: If OMIQ analysis, need to add additional column indicating whether the stats refer to the sample or control
  raw_upload_data_prelim <- reactive({
    inFile <- input$raw_upload
    ext <- tools::file_ext(input$raw_upload$name)
    if(is.null(inFile))
      return(NULL)
    else if(input$analysis_type == "Manual"){
      
      ifelse(ext != "xlsx", validate("Invalid file input extension. Please upload a .xlsx file only."), NA)
      
      df <- readxl::read_xlsx(inFile$datapath)
    }
    else if(input$analysis_type == "OMIQ"){
      
      ifelse(ext != "csv", validate("Invalid file input extension.. Please upload a .csv file only."), NA)
      
      df <- readr::read_csv(inFile$datapath)
      df <- add_sample_or_control_column(df)
    }
    
    return(df)
  })
  
  # STEP 1b: If OMIQ analysis, need to use stats related to only the cell population selected by user
  raw_upload_data <- reactive({
    req(raw_upload_data_prelim())
    
    if(input$analysis_type == "Manual"){
      df <- raw_upload_data_prelim()
    }
    else if(input$analysis_type == "OMIQ"){
      req(input$cell_pop_input)
      df <- configure_stats(raw_upload_data_prelim(), 
                            as.character(input$cell_pop_input))
    }
    return(df)
  })
  
  # Step 2: Calculate % of 4C Reference MFI Data
  raw_upload_data_with_perct_MFI <- reactive({
    req(input$raw_upload)
    suppressWarnings(calculate_perct_4C_MFI(raw_upload_data()))
  })
  
  ## Step 3: Create wide table of % 4C Reference MFI Data with raw data (no exclusions)
  raw_reference_MFI_data_wide <- reactive({
    req(input$raw_upload)
    create_raw_reference_MFI_table_wide(raw_upload_data_with_perct_MFI())
  })
  
  ###################### How Excluded Data Points are Handled #####################
  
  ################### Excluded Concentrations ####################
  concentration_choice_names <- reactive({ concentration_choiceNames(raw_upload_data()) })
  concentration_choice_values <- reactive({ concentration_choiceValues(concentration_choice_names()) })
  
  output$concentration_checkGroupInput <-
      renderUI({
          req(input$raw_upload)
              checkboxGroupInput(
                  'concentrations_to_include',
                  'Regression Concentrations',
                  choiceNames = c(concentration_choice_names()),
                  choiceValues = c(concentration_choice_values()),
                  selected = c(concentration_choice_values())
              )
      })
  
  # Reactive expression capturing the selected concentrations to include in the analysis
  concentrations_to_include_list <- reactive({ 
      req(input$concentrations_to_include)
      unlist(strsplit(input$concentrations_to_include, " ")) 
  })
  
  # Selected % of 4C Reference MFI Data (based on concentrations selected in UI)
  reference_MFI_data_to_include <- reactive({
      if(is.null(input$raw_upload)){
          return(NULL)
      }
      return(concentrations_to_keep(raw_reference_MFI_data_wide(), concentrations_to_include_list()))
  })
  
  ################### Excluded Individual Data Points ####################

  # 'Melt' % of 4C Reference MFI Data w/ selected concentrations
  selected_melted_data <- reactive({ melt_reference_mfi_table(reference_MFI_data_to_include()) })
  
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, 56)
  )
  # Update values of included rows whenever new data is uploaded
  observeEvent(input$raw_upload, {
    vals$keeprows <- rep(TRUE, nrow(selected_melted_data()))
  })
  
  # Data frame of values to include and exclude for analysis
  keep    <- reactive({ selected_melted_data()[ vals$keeprows, , drop = FALSE] })
  exclude <- reactive({ selected_melted_data()[!vals$keeprows, , drop = FALSE] })
  
  ##################### 'Stats Table' tab ########################

  # Output % of 4C Reference MFI Data Table to UI
  reference_MFI_data_to_include_from_keep <- reactive({
      create_reference_MFI_table_wide_with_keeps(keep())
  })
  
  # Title of table, conditional to stats file upload
  output$table_title <- renderUI({ 
    req(input$raw_upload)
    h3("% of 4C Reference MFI") 
  })
  
  # % of 4C Reference MFI table to 'Stats Table' tab
  output$reference_mfi_data_table_keep_only <- DT::renderDataTable({ 
      if (is.null(input$raw_upload)) {
          return (NULL)
      }
      reference_MFI_data_to_include_from_keep()
  })
  
  ##################### 'Plots' tab ########################
  output$mfi_vs_concentration <- renderPlot({
    req(input$raw_upload)
    mfi_vs_concentration_plot(raw_upload_data())
  })
  output$mfi_vs_time <- renderPlot({
    req(input$raw_upload)
    mfi_vs_time_plot(raw_upload_data())
  })
  output$stain_index <- renderPlot({
    req(input$raw_upload)
    suppressWarnings(stain_index(raw_upload_data()))
  })
  output$signal_to_noise <- renderPlot({
    req(input$raw_upload)
    suppressWarnings(signal_to_noise(raw_upload_data()))
  })
  output$percent_positive <- renderPlot({
    req(input$raw_upload)
    percent_positive(raw_upload_data())
  })
  output$percent_of_4C_MFI <- renderPlot({
    req(input$raw_upload)
    percent_of_4C_MFI(raw_upload_data_with_perct_MFI())
  })
  
  ##################### 'Regression & Shelf-Life' tab ######################
  
  # Calculate confidence intervals
  confidence_bands <-
      reactive({
          find_confidence_bands(
              keep(),
              poly_order(),
              as.numeric(confidenceInterval()),
              as.numeric(threshold_y())
          )
      })
  
  # Create regression plot
  
  # Zoom functionality
  add_zoom <- function(plot_id) {
      ranges <- reactiveValues(x = NULL, y = NULL)
      observeEvent(input[["regression_plot_zoom_dblclick"]], {
          brush <- input[["regression_plot_output_brush"]]
          if (!is.null(brush)) {
              ranges$x <- c(brush$xmin, brush$xmax)
              ranges$y <- c(brush$ymin, brush$ymax)
          } else {
              ranges$x <- NULL
              ranges$y <- NULL
          }
      })
      ranges
  }
  plot_range <- add_zoom("regression_plot_output")
  
  # Regression plot
  regr_plot <- reactive({
      req(input$raw_upload)
      keep    <- selected_melted_data()[ vals$keeprows, , drop = FALSE]
      exclude <- selected_melted_data()[!vals$keeprows, , drop = FALSE]
      keep <- keep[complete.cases(keep),]
      
      p <- regression_plot_global(
          ui_font_size,
          ui_data_point_size,
          ui_eqn_size,
          keep,
          confidence_bands(),
          poly_order(),
          confidenceInterval()
      )
      p <- p + geom_point(data = exclude, size = ui_data_point_size, shape = 21, fill = NA, color = 'black') +
          coord_cartesian(xlim = plot_range$x, ylim = plot_range$y) +
          stat_regline_equation(data=keep, aes(x=Time, y=value,
                                               label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                            formula = y ~ poly(x,poly_order(),raw=TRUE), 
                            method="lm", col="red",
                            label.x.npc="left", label.y.npc="bottom", size=ui_eqn_size)
          
      return(suppressWarnings(p))
     
  })
  
  output$regression_plot_output <- renderPlot({ suppressWarnings(regr_plot()) })
  
  # Toggle points that are clicked
  observeEvent(input$regression_plot_output_click, {
      res <- nearPoints(selected_melted_data(), input$regression_plot_output_click, allRows = TRUE) 
      vals$keeprows <- xor(vals$keeprows, res$selected_) # keeprows array is updated 
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
      vals$keeprows <- rep(TRUE, nrow(selected_melted_data()))
  })
  
  ## Step 7a: Calculate shelf life with confidence interval
  lower_shelf_life <-
      reactive({
          solve_for_lower_shelf_life(
              keep(),
              poly_order(),
              as.numeric(confidenceInterval()),
              as.numeric(threshold_y())
          )
      })
  
  # Output shelf-life to UI
  output$check_lower_shelf_life <- renderUI({
      req(input$raw_upload)
      if(is.null(lower_shelf_life())){
          tags$i(" Does not intersect with MFI Threshold - No Shelf-life can be found",
                 class = "fa fa-times-circle", 
                 style = "color: red; font-size: 20px;"
          )
      }
      else{
          tags$i(paste0(format(lower_shelf_life(),nsmall=2), ' years   ', '(', 
                        round(as.numeric(lower_shelf_life())*RAW_SL_TO_DAYS,0), ' days)'),
                 style="color: #eb6864; font-size: 20px; font-style: normal; font-weight: bold;"
          )
      }
  })
  # Apply rounding ruleset and output SAP (rounded) shelf-life to UI
  output$check_rounded_shelf_life <- renderUI({
      req(input$raw_upload)
      if(is.null(rounded_shelf_life(lower_shelf_life()))){
          tags$i(" Does not intersect with MFI Threshold - No Shelf-life can be found",
                 class = "fa fa-times-circle", 
                 style = "color: red; font-size: 20px;"
          )
      }
      else{
          tags$i(paste0(rounded_shelf_life(lower_shelf_life()), ' years   ', '(', 
                        round(as.numeric(rounded_shelf_life(lower_shelf_life()))*SAP_SL_TO_DAYS,0), ' days)'),
                 style="color: #eb6864; font-size: 20px; font-style: normal; font-weight: bold;"
          )
      }
  })
  
  ## Model p-value, R^2 value and other warnings
  # All model coefficient p-values
  poly_eval <- reactive({ 
    req(raw_upload_data())
    get_model_coeff_pvalues(keep(), poly_order()) 
  })
  # Return p-value based on model polynomial order selected
  model_p_value <- reactive({  
    req(raw_upload_data())
    if(poly_order() == 1){
      p_val <- format(round(poly_eval()$b_pvalue,3),nsmall=3)
    }
    else if(poly_order() == 2){
      p_val <- format(round(poly_eval()$c_pvalue,3),nsmall=3)
    }
    else if(poly_order() == 3){
      p_val <- format(round(poly_eval()$d_pvalue,3),nsmall=3)
    }
    return(p_val)
  })
  
  # Output model coeff. p-value with font color flag and any warnings
  output$model_coeff_pvalue <- renderUI({ 
    req(input$raw_upload)
    if( model_p_value() >= 0.05 ){ ## Model coeff. not statistically significant
      
      tagList(
        div(style = "color: red; font-size: 20px;",strong(model_p_value())),
        tags$i(" Warning: Model coefficient p-value not statistically significant.",
               class = "fas fa-times-circle", 
               style = "color: red; font-size: 20px")
      )
    }
    else if( model_p_value() < 0.05 ){ # Model coeff. is statistically significant
      div(style = "color: #2DC62D; font-size: 20px",
          strong(model_p_value()))
    }
  })
  
  # Warning on sidebar panel below polynomial order options
  output$warning_ui_polynomial_choice <- renderUI({
    req(input$raw_upload)
    
    if(poly_order() == 2 && poly_eval()$c_pvalue >= 0.05){
      tags$i(" Warning: p-value for 2nd order not statistically significant. Consider using linear model instead.",
             class = "fa fa-exclamation-triangle", 
             style = "color: #fcba03; font-size: 20px"
      )
    }
    else if(poly_order() == 3 && poly_eval()$d_pvalue >= 0.05){
      tags$i(" Warning: p-value for 3rd order not statistically significant. Consider using 2nd order or linear model instead.",
             class = "fa fa-exclamation-triangle", 
             style = "color: #fcba03; font-size: 20px"
      )
    }
  })
  
  # R-squared value
  R_sq_val <- reactive({ R_sq(keep(), poly_order()) })
  
  output$warning_ui_rsq <- renderUI({
    req(input$raw_upload)
    if(R_sq_val() < 0.80){
      tags$i(" Warning: R-squared value is below 0.80",
             class = "fa fa-exclamation-triangle", 
             style = "color: #fcba03; font-size: 20px"
      )
    }
  })
  
  # Slope of equation
  slope <- reactive({ best_fit_equation(keep(), poly_order())$coefficients[[2]] })
  
  output$warning_ui_slope <- renderUI({
    req(input$raw_upload)
    if(slope() > 0){
      tags$i(" Warning: Regression slope is positive",
             class = "fa fa-exclamation-triangle", 
             style = "color: #fcba03; font-size: 20px"
      )
    }
  })
  
  ##################### 'Residuals & Normality Checks' tab ######################

  # Calculate residuals and add residual plots for additional quality checks
  residuals_expr <- reactive({ find_residuals(keep(), poly_order()) })
  residual_vs_fit_plot_expr <- reactive({ residual_vs_fit_plot(keep(), poly_order(), font_size = 12, data_point_size = 3)  })
  residual_vs_fit_plot_expr_tooltip <- reactive({ residual_vs_fit_plot_w_tooltip( residual_vs_fit_plot_expr() ) })
  residual_histogram_plot_expr <- reactive({ residual_histogram(keep(), poly_order(), font_size = 12) })
  
  output$residual_fit_plot <- renderPlotly({ 
      req(input$raw_upload)
      residual_vs_fit_plot_expr_tooltip()
  })
  
  output$residual_histogram <- renderPlotly({ 
      req(input$raw_upload)
      residual_histogram_plot_expr() 
  })
  
  normal_probability_plot_expr <-
    reactive({
        normal_probability_plot(keep(),
                                poly_order(),
                                residuals_expr(),
                                font_size = 12, data_point_size = 3)
  })
  normal_probability_plot_expr_tooltip <- reactive({ normal_probability_plot_w_tooltip(normal_probability_plot_expr()) })
  
  anderson_darling_normality_test_pvalue <- reactive({ anderson_darling_normality_test(residuals_expr()) })

  output$normal_prob_plot_output <- renderPlotly({
      req(input$raw_upload) 
      normal_probability_plot_expr_tooltip()
  })
  
  output$anderson_darling_pvalue_table_output <- renderTable({
      req(input$raw_upload) 
      modified_anderson_darling_flextable()
  })
  
  output$anderson_darling_pvalue_output <- renderUI({
      req(input$raw_upload)
      
      if( anderson_darling_normality_test_pvalue() < 0.05 ){ ## Not Normal Distribution
          tagList(
            div(style = "color: red; font-size: 20px;",
              strong(format(round(anderson_darling_normality_test_pvalue(),3),nsmall=3))),
            br(),
            tags$i(" Warning: Residuals do not follow normal distribution",
                   class = "fa fa-exclamation-triangle", 
                   style = "color: #fcba03; font-size: 20px")
          )
      }
      else if( anderson_darling_normality_test_pvalue() >= 0.05 ){ # Normal Distribution
          div(style = "color: #2DC62D; font-size: 20px",
              strong(format(round(anderson_darling_normality_test_pvalue(),3),nsmall=3)))
      }
  })

  ###################### PPT Output Variables/Inputs ################################
  
  ## Variables/Inputs using raw data from stats file upload only (no exclusions)
  
  # 'Melt' table to so all concentrations in 1 column; input to regression plot, etc.
  raw_melted_data <- reactive({ na.omit(melt_reference_mfi_table(raw_reference_MFI_data_wide())) })
  
  # Confidence intervals
  raw_confidence_bands <-
    reactive({
      find_confidence_bands(
        raw_melted_data(),
        poly_order(),
        as.numeric(confidenceInterval()),
        as.numeric(threshold_y())
      )
    })
  
  # Regression plot
  raw_regression_plot_for_report <- reactive({ 
    
    req(input$raw_upload)
    
    p <- regression_plot_global(
      reports_font_size,
      reports_data_point_size,
      reports_eqn_size,
      raw_melted_data(),
      raw_confidence_bands(),
      poly_order(),
      confidenceInterval()
    ) +
      coord_cartesian(ylim=c(0, NA)) + 
      scale_y_continuous(breaks=seq(0, 120, 20)) +
      stat_regline_equation(data=raw_melted_data(),
                            aes(x=Time, y=value,
                                label=paste(..eq.label..)),
                            formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                            label.x=0,label.y=10,size=reports_eqn_size) +
      stat_regline_equation(data=raw_melted_data(),
                            aes(x=Time, y=value,
                                label=paste(..rr.label..)),
                            formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                            label.x=0,label.y=5,size=reports_eqn_size)

    return(suppressWarnings(p))
  })
  
  # shelf-life from lower confidence bound
  raw_lower_shelf_life <-
    reactive({
      solve_for_lower_shelf_life(
        raw_melted_data(),
        poly_order(),
        as.numeric(confidenceInterval()),
        as.numeric(threshold_y())
      )
    })
  
  # shelf-life with rounded ruleset
  rounded_raw_lower_shelf_life <- reactive({
    rounded_shelf_life(raw_lower_shelf_life())
  })
  
  # All model coefficient p-values
  raw_poly_eval <- reactive({ 
    get_model_coeff_pvalues(raw_melted_data(), poly_order()) 
  })

  # Return p-value based on model polynomial order selected
  raw_model_p_value <- reactive({  
    req(raw_upload_data())
    if(poly_order() == 1){
      p_val <- format(round(raw_poly_eval()$b_pvalue,3),nsmall=3)
    }
    else if(poly_order() == 2){
      p_val <- format(round(raw_poly_eval()$c_pvalue,3),nsmall=3)
    }
    else if(poly_order() == 3){
      p_val <- format(round(raw_poly_eval()$d_pvalue,3),nsmall=3)
    }
    return(p_val)
  })
  # R-squared value
  raw_R_sq_val <- reactive({ R_sq(raw_melted_data(), poly_order()) })
  
  # Calculate residuals and add residual plots for additional quality checks
  raw_residuals_expr <- reactive({ find_residuals(raw_melted_data(), poly_order()) })
  raw_residual_vs_fit_plot_expr <- reactive({ residual_vs_fit_plot(raw_melted_data(), poly_order(), reports_font_size, reports_data_point_size)  })
  raw_residual_histogram_plot_expr <- reactive({ residual_histogram(raw_melted_data(), poly_order(), reports_font_size) })
  
  # Check for normal distribution of residuals
  raw_normal_probability_plot_expr <- reactive({ normal_probability_plot(raw_melted_data(), poly_order(), raw_residuals_expr(), reports_font_size, reports_data_point_size) })
  raw_anderson_darling_normality_test_pvalue <- reactive({ anderson_darling_normality_test(raw_residuals_expr()) })
  
  # Table with summary of shelf-life data and quality checks for both raw data (no exclusions) and data included in App
  raw_shelf_life_summary_flextable <- reactive({ 
      df <- tibble(
          "Raw Shelf-Life" = c(paste0(format(raw_lower_shelf_life(),nsmall=2), " yrs (", 
                                      round(raw_lower_shelf_life()*RAW_SL_TO_DAYS,0), " days)")),
          "Rounded Shelf-Life" = c(paste0(rounded_shelf_life(raw_lower_shelf_life()), " yrs (", 
                                          round(rounded_shelf_life(raw_lower_shelf_life())*SAP_SL_TO_DAYS,0), " days)")),
          "R-squared" = c(raw_R_sq_val()),
          "Model p-value"=c(raw_model_p_value())
      )
  })
  
  modified_shelf_life_summary_flextable <- reactive({ 
      df <- tibble(
          "Raw Shelf-Life" = c(paste0(format(lower_shelf_life(), nsmall=2), " yrs (", 
                                      round(lower_shelf_life()*RAW_SL_TO_DAYS,0), " days)")),
          "SAP Shelf-Life" = c(paste0(rounded_shelf_life(lower_shelf_life()), " yrs (", 
                                      round(rounded_shelf_life(lower_shelf_life())*SAP_SL_TO_DAYS,0), " days)")),
          "R-squared" = c(format(round(R_sq_val(),2),nsmall=2)),
          "Model p-value"=c(model_p_value())
      )
  })
  
  # Table with anderson-darling p-value for both raw data (no exclusions) and data included in App
  raw_anderson_darling_flextable <- reactive({
      df <- tibble("Anderson-Darling Normality Test p-value" = c(
              format(round(
                  anderson_darling_normality_test(find_residuals(raw_melted_data(),poly_order())), 
                  3), nsmall = 3)))
  })
  
  modified_anderson_darling_flextable <- reactive({
      df <- tibble("Anderson-Darling Normality Test p-value" = c(
              format(round(
                  anderson_darling_normality_test(find_residuals(keep(),poly_order())), 
                  3), nsmall = 3)))
  })
  
  
  
  
  raw_model_pvalue_color <- reactive({
      if( raw_model_p_value() >= 0.05 ){ ## Model coeff. not statistically significant
         return("red")
      }
      else if( raw_model_p_value() < 0.05 ){ # Model coeff. is statistically significant
          return("#2DC62D")
      }
  })
  
  modified_model_pvalue_color <- reactive({
      if( model_p_value() >= 0.05 ){ ## Model coeff. not statistically significant
          return("red")
      }
      else if( model_p_value() < 0.05 ){ # Model coeff. is statistically significant
          return("#2DC62D")
      }
  })
  
  raw_ad_pvalue_color <- reactive({
      if( raw_anderson_darling_normality_test_pvalue() >= 0.05 ){ ## Normal distribution
          return("#2DC62D")
      }
      else if( raw_anderson_darling_normality_test_pvalue() < 0.05 ){ # Not normal distribution
          return("red")
      }
  })
  
  modified_ad_pvalue_color <- reactive({
      if( anderson_darling_normality_test_pvalue() >= 0.05 ){ ## Normal distribution
          return("#2DC62D")
      }
      else if( anderson_darling_normality_test_pvalue() < 0.05 ){ # Not normal distribution
          return("red")
      }
  })
  
  ## Plots for PPT presentation
  
  # Plot for PPT Presentation with data included in UI (requires specific font/data point size arguments)
  modified_regression_plot_for_report <- reactive({
      
    req(input$raw_upload)
    keep    <- selected_melted_data()[ vals$keeprows, , drop = FALSE]
    exclude <- selected_melted_data()[!vals$keeprows, , drop = FALSE]
    keep <- keep[complete.cases(keep),]
    
    
    p <- regression_plot_global(
        reports_font_size,
        reports_data_point_size,
        reports_eqn_size,
        keep,
        confidence_bands(),
        poly_order(),
        confidenceInterval()
    )
    p <- p + geom_point(data = exclude, size = reports_data_point_size, shape = 21, fill = NA, color = 'black') +
        coord_cartesian(ylim=c(0, NA)) + 
        scale_y_continuous(breaks=seq(0, 120, 20)) +
        stat_regline_equation(data=keep,
                              aes(x=Time, y=value,
                                  label=paste(..eq.label..)),
                              formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                              label.x=0,label.y=10,size=reports_eqn_size) +
        stat_regline_equation(data=keep,
                              aes(x=Time, y=value,
                                  label=paste(..rr.label..)),
                              formula = y ~ poly(x,poly_order(),raw=TRUE), method="lm", col="red",
                              label.x=0,label.y=5,size=reports_eqn_size)
    
    return(suppressWarnings(p))
      
  })
  
  
  ## Text formatting for PPT presentation
  label_fp <- fp_text(bold = TRUE, color = "black", font.size = 16)
  note_fp <- fp_text(bold = FALSE, color = "black", font.size = 16)
  notes_textbox <- reactive({

      bl2 <- block_list(fpar(ftext("Notes: ", label_fp), ftext(input$notes, note_fp)))
  })
  
  model_system_text <- reactive({
      bl1 <- block_list(fpar(
          ftext(input$model_system_data, fp_text(
              bold=FALSE, color = "black", font.size = 12
          ))
      ))
  })
  
  ########################## Powerpoint Output (Manual Workflow only)##############################
  final_name <- reactive({ as.character(paste0(input$filename_output, ".pptx")) })
  
  output$pptx_id <- downloadHandler(
      filename = function(){
          final_name()
      },
      content = function(file){
          
          mypptx  %>% 
              ################### PROTOCOL SLIDE ##################
              add_slide(layout = "Title and Content", master = "Office Theme") %>%
              ph_with(
                  value = "Reagents Protocol", 
                  location = ph_location_type(type = "title")
              ) %>%
              ################### SSC PLOTS SLIDE ##################
              add_slide(layout = "Title and Content", master = "Office Theme") %>%
                  ph_with(
                      value = "SSC Plots", 
                      location = ph_location_type(type = "title")
                  ) %>%
              ################### HISTOGRAM PLOTS SLIDE ##################
              add_slide(layout = "Title and Content", master = "Office Theme") %>%
                  ph_with(
                      value = paste0(input$pop_number, ":  Histogram Plots"),
                      location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################### MFI PLOTS SLIDE ##################
              add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ": MFI Data"), 
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = mfi_vs_concentration_plot(raw_upload_data()), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = mfi_vs_time_plot(raw_upload_data()), 
                  location = ph_location_type(type = "body", index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################### CONCENTRATION VS TIME PLOTS SLIDE 1 ##################
              add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Conc. VS Time"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = suppressWarnings(stain_index(raw_upload_data())), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = suppressWarnings(signal_to_noise(raw_upload_data())), 
                  location = ph_location_type(type = "body", index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################## CONCENTRATION VS TIME PLOTS SLIDE 2 ##################
              add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Conc. VS Time"), 
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = percent_positive(raw_upload_data()), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = percent_of_4C_MFI(raw_upload_data_with_perct_MFI()), 
                  location = ph_location_type(type = "body", index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################### REGRESSION SLIDE - RAW ##################
              add_slide(layout = "Picture with Caption", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Regression - All Data"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = flextable(raw_reference_MFI_data_wide(), cwidth=0.65) %>%
                      add_header_lines("% of 4C Reference MFI") %>%
                      bold(i = 1:2, bold = TRUE, part = "header") %>%
                      align(align="center", part="all")  %>%
                      # fontsize(size = 14, part = "body") %>%
                      fontsize(size = 11, part = "all") %>%
                      hline_top(part = "all", border = fp_border(color="black", width=2)) %>%
                      font(fontname = fontname, part = "all"),
                  location = ph_location_type(type = "body", position_right = F, position_top = T)
              ) %>%
              ph_with(
                  value = raw_regression_plot_for_report(), 
                  location = ph_location_type(type = "pic")
              ) %>%
              ph_with(
                  value = flextable(raw_shelf_life_summary_flextable(), cwidth=1.2) %>%
                      bold(i = 1, bold = TRUE, part = "header") %>%
                      bg(bg = "#FBF59E", part = "header") %>%
                      align(align="center", part="all") %>%
                      fontsize(size = 14, part = "body") %>%
                      color(i = 1, j = 4, color = raw_model_pvalue_color()) %>%
                      color(i = 1, j = 1, color = shelf_life_color(raw_lower_shelf_life())) %>%
                      color(i = 1, j = 2, color = shelf_life_color(rounded_shelf_life(raw_lower_shelf_life()))) %>%
                      hline_top(part = "all", border = fp_border(color="black", width=2)) %>%
                      font(fontname = fontname, part = "all"),
                  location = ph_location_type(type = "body", position_right = F, position_top = F)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################## RESIDUAL VS FIT AND HISTOGRAM - RAW ##################
          add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Residuals"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = residual_vs_fit_plot(raw_melted_data(), poly_order(), reports_font_size, reports_data_point_size), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = residual_histogram(raw_melted_data(), poly_order(), font_size = 12), 
                  location = ph_location_type(type = "body", index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################## NORMALITY OF RESIDUALS - RAW ##################
          add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Normality of Residuals"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = raw_normal_probability_plot_expr(), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = flextable(raw_anderson_darling_flextable(), cwidth=3) %>% 
                      bg(bg = "#FBF59E", part = "header") %>%
                      align(align="center", part="all") %>%
                      fontsize(size = 12, part = "header") %>%
                      color(i = 1, j = 1, color = raw_ad_pvalue_color()) %>%
                      fontsize(size = 16, part = "body") %>%
                      font(fontname = fontname, part = "all"),
                  location = ph_location_type(type = "body", left = 7, index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################### REGRESSION SLIDE - UPDATED ##################
          add_slide(layout = "Picture with Caption", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Regression"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = flextable(reference_MFI_data_to_include_from_keep(), cwidth=0.65) %>%
                      add_header_lines("% of 4C Reference MFI") %>%
                      bold(i = 1:2, bold = TRUE, part = "header") %>%
                      align(align="center", part="all")  %>%
                      fontsize(size = 11, part = "all") %>%
                      hline_top(part = "all", border = fp_border(color="black", width=2)) %>%
                      font(fontname = fontname, part = "all"),
                  location = ph_location_type(type = "body", position_right = F, position_top = T)
              ) %>%
              ph_with(
                  value = modified_regression_plot_for_report(),
                  location = ph_location_type(type = "pic")
              ) %>%
              ph_with(
                  value = flextable(modified_shelf_life_summary_flextable(), cwidth=1.2) %>%
                      bold(i = 1, bold = TRUE, part = "header") %>%
                      bg(bg = "#FBF59E", part = "header") %>%
                      align(align="center", part="all") %>%
                      fontsize(size = 14, part = "body") %>%
                      color(i = 1, j = 4, color = modified_model_pvalue_color()) %>%
                      color(i = 1, j = 1, color = shelf_life_color(lower_shelf_life())) %>%
                      color(i = 1, j = 2, color = shelf_life_color(rounded_shelf_life(lower_shelf_life()))) %>%
                      hline_top(part = "all", border = fp_border(color="black", width=2)) %>%
                      font(fontname = fontname, part = "all"),
                  location = ph_location_type(type = "body", position_right = F, position_top = F)
              ) %>%
              ph_with(
                  value = notes_textbox(),
                  location = ph_location_template(left=8, top=6, width=5, newlabel="new", id=6)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################## RESIDUAL VS FIT AND HISTOGRAM - MODIFIED ##################
          add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Residuals"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = residual_vs_fit_plot(keep(), poly_order(), reports_font_size, reports_data_point_size), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = residual_histogram(keep(), poly_order(), font_size = 12), 
                  location = ph_location_type(type = "body", index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              ################## NORMALITY OF RESIDUALS - RAW ##################
          add_slide(layout = "Two Content", master = "Office Theme") %>%
              ph_with(
                  value = paste0(input$pop_number, ":  Normality of Residuals"),
                  location = ph_location_type(type = "title")
              ) %>%
              ph_with(
                  value = normal_probability_plot_expr(), 
                  location = ph_location_type(type = "body", index = 4, position_right = F)
              ) %>%
              ph_with(
                  value = flextable(modified_anderson_darling_flextable(), cwidth=3) %>% 
                      bg(bg = "#FBF59E", part = "header") %>%
                      align(align="center", part="all") %>%
                      fontsize(size = 12, part = "header") %>%
                      color(i = 1, j = 1, color = modified_ad_pvalue_color()) %>%
                      fontsize(size = 16, part = "body") %>%
                      font(fontname = fontname, part = "all"),
                  location = ph_location_type(type = "body", left = 7, index = 3, position_right = T)
              ) %>%
              ph_with(
                  value = model_system_text(),
                  location = ph_location_template(left=11, top=0, width=3, height=1, newlabel="new", id=6)
              ) %>%
              print(target = file)
      }
  )
  ########################## PDF Report Output (OMIQ Workflowonly) ######################
  # Get optimal (used in label for regression report)
  get_optimal <- reactive({
      df <- readr::read_csv(input$raw_upload$datapath)
      col_names <- colnames(df)
      if("Optimal.with.units" %in% col_names){
          optimal <- unique(df$Optimal.with.units)
      }
      else if("Optimal" %in% col_names & "Optimal.units" %in% col_names){
          optimal <- paste(unique(df$Optimal.units), unique(df$Optimal))
      }
      
      if(is.na(optimal) | is.null(optimal) | optimal == ""){
          return(NA)
      }
      return(optimal)
  })
  
  create_regression_report_modified <- reactive({
    req(input$raw_upload)
    
    validate(
      need(input$cell_pop_input != "", "Wait for cell population list to load") # display custom message in need
    )
    report <- build_regression_report_gui_modified(
      keep(),
      poly_order(),
      as.numeric(input$CI),
      as.numeric(input$threshold),
      input$raw_upload$datapath,
      input$cell_pop_input, 
      marker_name(), 
      get_optimal(),
      paste("Notes:", input$notes),
      input$plate_id)
    while (!is.null(dev.list()))  dev.off()
    return(report)
  })

  
  # When user clicks "Create Individual Regression Report" button, new folder created and regression report generated
  observeEvent(input$create_regr_report, {
      create_regression_report_modified()
      output$indiv_report_text <- renderText("Regression Report Created! Check your Plate ID folder that it completed successfully.")
  })
  
  # When user clicks "Create Bundled Regression Report" button, merges omiq report and regression report together and writes to plate id folder
  observeEvent(input$regression_report_bundled, {
      merged_pdfs_for_gui(input$omiq_report_upload$datapath, input$regression_reports_indiv$datapath, input$plate_id)
      output$bundld_report_text <- renderText("Bundled Regression Report Created! Check your Plate ID folder that it completed successfully.")
  })

}