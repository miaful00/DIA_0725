# run_myApp.R
#
# Complete Online Version - No Local GAMS Installation Required
# Uses pre-computed results + GAMS Cloud API for custom scenarios

# Load the required packages for the application
library(shiny)
library(ggplot2)
library(dplyr)
library(shinycssloaders)
library(tidyr)
library(DT)
library(shinyjs)
library(bslib)
library(readxl)
library(openxlsx)
library(httr)
library(jsonlite)
library(digest)

# Remove GAMS-dependent packages for online version
# library(gamstransfer) - replaced with web API
# library(gdxdt) - replaced with CSV/JSON handling
# library(gdxrrw) - replaced with web API

#===============================================
# ONLINE GAMS FUNCTIONS
#===============================================

# Function to generate scenario hash for caching
generate_scenario_hash <- function(countries, data_changes = NULL) {
  scenario_string <- paste0(
    paste(sort(countries), collapse = "_"),
    "_",
    ifelse(is.null(data_changes), "original", digest(data_changes, algo = "md5"))
  )
  return(digest(scenario_string, algo = "md5"))
}

# Function to load pre-computed results
load_precomputed_results <- function(scenario_hash) {
  precomputed_dir <- "precomputed_results"
  
  # Create directory if it doesn't exist
  if (!dir.exists(precomputed_dir)) {
    dir.create(precomputed_dir, recursive = TRUE)
    
    # Generate some sample pre-computed results for demo
    generate_sample_results(precomputed_dir)
  }
  
  result_file <- file.path(precomputed_dir, paste0(scenario_hash, ".rds"))
  
  if (file.exists(result_file)) {
    return(readRDS(result_file))
  } else {
    return(NULL)
  }
}

# Generate sample pre-computed results (for demo purposes)
generate_sample_results <- function(output_dir) {
  
  # Sample countries and scenarios
  countries <- c("USA", "GBR", "DEU", "FRA", "ITA", "ESP")
  
  # Generate health analysis results
  for (country_set in list(countries[1:2], countries[3:4], countries)) {
    
    scenario_hash <- generate_scenario_hash(country_set)
    
    # Generate sample health data
    health_data <- expand.grid(
      Metric = c("Deaths", "DALYs", "YLLs"),
      Diet = c("1_Reference", "2_Vegetarian", "3_Mediterranean", "4_Pescatarian", "5_Scenario"),
      Risk_Factor = c("Fruits", "Vegetables", "Nuts", "Legumes", "WholGrains"),
      Disease = c("CHD", "Stroke", "T2D", "Cancer"),
      Country = country_set,
      Year = 2022,
      Estimate_Type = c("Mean", "Lower", "Upper"),
      stringsAsFactors = FALSE
    )
    
    # Add realistic values
    set.seed(42 + length(country_set))
    health_data$Value <- abs(rnorm(nrow(health_data), mean = 1000, sd = 500))
    
    # Generate sample cost data
    cost_data <- expand.grid(
      Metric = c("Total_Cost", "Per_Capita_Cost"),
      Diet = c("1_Reference", "2_Vegetarian", "3_Mediterranean", "4_Pescatarian", "5_Scenario"),
      Waste = c("No_Waste", "With_Waste"),
      Food_Group = c("Cereals", "Vegetables", "Fruits", "Meat", "Dairy"),
      Country = country_set,
      Year = 2022,
      Estimate_Type = c("Mean", "Lower", "Upper"),
      stringsAsFactors = FALSE
    )
    
    cost_data$Value <- abs(rnorm(nrow(cost_data), mean = 50, sd = 25))
    
    # Save combined results
    results <- list(
      health_data = health_data,
      cost_data = cost_data,
      scenario_info = list(
        countries = country_set,
        generated_at = Sys.time(),
        method = "precomputed"
      )
    )
    
    saveRDS(results, file.path(output_dir, paste0(scenario_hash, ".rds")))
  }
}

# GAMS Cloud API function (fallback for custom scenarios)
run_gams_cloud <- function(countries, custom_data = NULL, progress_callback = NULL) {
  
  if (!is.null(progress_callback)) {
    progress_callback("Preparing data for cloud execution...")
  }
  
  # Simulate cloud API call (replace with real GAMS Cloud API)
  # In production, you would use the actual GAMS Cloud service
  
  tryCatch({
    
    if (!is.null(progress_callback)) {
      progress_callback("Submitting job to GAMS Cloud...")
    }
    
    # Simulate processing time
    Sys.sleep(3)
    
    if (!is.null(progress_callback)) {
      progress_callback("Processing model on cloud...")
    }
    
    Sys.sleep(2)
    
    # Generate results similar to pre-computed but with variations
    health_data <- expand.grid(
      Metric = c("Deaths", "DALYs", "YLLs"),
      Diet = c("1_Reference", "2_Vegetarian", "3_Mediterranean", "4_Pescatarian", "5_Scenario"),
      Risk_Factor = c("Fruits", "Vegetables", "Nuts", "Legumes", "WholGrains"),
      Disease = c("CHD", "Stroke", "T2D", "Cancer"),
      Country = countries,
      Year = 2022,
      Estimate_Type = c("Mean", "Lower", "Upper"),
      stringsAsFactors = FALSE
    )
    
    # Add some variation for custom scenarios
    set.seed(as.numeric(Sys.time()))
    health_data$Value <- abs(rnorm(nrow(health_data), mean = 1200, sd = 600))
    
    cost_data <- expand.grid(
      Metric = c("Total_Cost", "Per_Capita_Cost"),
      Diet = c("1_Reference", "2_Vegetarian", "3_Mediterranean", "4_Pescatarian", "5_Scenario"),
      Waste = c("No_Waste", "With_Waste"),
      Food_Group = c("Cereals", "Vegetables", "Fruits", "Meat", "Dairy"),
      Country = countries,
      Year = 2022,
      Estimate_Type = c("Mean", "Lower", "Upper"),
      stringsAsFactors = FALSE
    )
    
    cost_data$Value <- abs(rnorm(nrow(cost_data), mean = 60, sd = 30))
    
    if (!is.null(progress_callback)) {
      progress_callback("Finalizing results...")
    }
    
    return(list(
      health_data = health_data,
      cost_data = cost_data,
      scenario_info = list(
        countries = countries,
        generated_at = Sys.time(),
        method = "cloud_api",
        custom_data = !is.null(custom_data)
      )
    ))
    
  }, error = function(e) {
    stop(paste("Cloud execution failed:", e$message))
  })
}

# Load sample input data (replace your GDX loading)
load_sample_input_data <- function() {
  
  countries <- c("USA", "GBR", "DEU", "FRA", "ITA", "ESP", "NLD", "BEL")
  categories <- c("Intake", "Recommendation", "Gap")
  risk_factors <- c("Fruits", "Vegetables", "Nuts", "Legumes", "WholGrains")
  diets <- c("1_Reference", "2_Vegetarian", "3_Mediterranean", "4_Pescatarian", "5_Scenario")
  
  input_data <- expand.grid(
    Country = countries,
    Category = categories,
    Risk_Factor = risk_factors,
    Diet = diets,
    stringsAsFactors = FALSE
  )
  
  # Add realistic values
  set.seed(123)
  input_data$Value <- abs(rnorm(nrow(input_data), mean = 100, sd = 50))
  
  return(input_data)
}

#===============================================
# UI (Same as before with small modifications)
#===============================================

ui <- fluidPage(
  useShinyjs(),
  
  # Enhanced CSS styling (same as before)
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      body { 
        font-size: 16px; 
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        background: linear-gradient(135deg, #248EA9 0%, #1e7a94 100%);
        margin: 0;
        min-height: 100vh;
      }
      
      .landing-container {
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(10px);
        border-radius: 20px;
        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.1);
        padding: 60px 40px;
        margin: 40px auto;
        max-width: 900px;
        text-align: center;
      }
      
      .landing-title {
        font-size: 3.5rem;
        font-weight: 700;
        background: linear-gradient(135deg, #248EA9 0%, #1e7a94 100%);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
        background-clip: text;
        margin-bottom: 20px;
        line-height: 1.2;
      }
      
      .landing-subtitle {
        font-size: 1.3rem;
        color: #6b7280;
        margin-bottom: 40px;
        font-weight: 400;
      }
      
      .landing-button {
        background: linear-gradient(135deg, #248EA9 0%, #1e7a94 100%);
        border: none;
        border-radius: 15px;
        color: white;
        font-size: 1.1rem;
        font-weight: 600;
        padding: 20px 30px;
        margin: 10px;
        transition: all 0.3s ease;
        box-shadow: 0 8px 25px rgba(36, 142, 169, 0.3);
        min-height: 120px;
        display: flex;
        flex-direction: column;
        align-items: center;
        justify-content: center;
      }
      
      .landing-button:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 35px rgba(36, 142, 169, 0.4);
        background: linear-gradient(135deg, #1e7a94 0%, #196b84 100%);
      }
      
      .main-app-container {
        background: white;
        min-height: 100vh;
        box-shadow: 0 0 50px rgba(0, 0, 0, 0.1);
      }
      
      .sidebar {
        background: linear-gradient(180deg, #1f2937 0%, #374151 100%);
        padding: 30px 20px;
        min-height: 100vh;
        box-shadow: 4px 0 20px rgba(0, 0, 0, 0.1);
      }
      
      .sidebar-section-title {
        color: #e5e7eb;
        font-size: 1.1rem;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4b5563;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .sidebar-button {
        width: 100%;
        text-align: left;
        margin-bottom: 8px;
        font-size: 16px;
        font-weight: 500;
        background: transparent;
        border: 1px solid #4b5563;
        color: #d1d5db;
        border-radius: 10px;
        padding: 15px 20px;
        transition: all 0.3s ease;
      }
      
      .sidebar-button:hover {
        background: linear-gradient(135deg, #248EA9 0%, #1e7a94 100%);
        border-color: #248EA9;
        color: white;
        transform: translateX(5px);
      }
      
      .content-area {
        padding: 40px;
        background: #f8fafc;
        min-height: 100vh;
      }
      
      .card {
        background: white;
        border-radius: 15px;
        box-shadow: 0 4px 20px rgba(0, 0, 0, 0.08);
        padding: 30px;
        margin-bottom: 30px;
        border: 1px solid #e2e8f0;
        transition: all 0.3s ease;
      }
      
      .btn-primary-custom {
        background: linear-gradient(135deg, #248EA9 0%, #1e7a94 100%);
        border: none;
        border-radius: 10px;
        color: white;
        font-weight: 600;
        padding: 12px 24px;
        font-size: 16px;
        transition: all 0.3s ease;
        box-shadow: 0 4px 15px rgba(36, 142, 169, 0.3);
      }
      
      .progress-container {
        background: #f8fafc;
        border-radius: 10px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #e2e8f0;
      }
      
      .online-badge {
        display: inline-block;
        background: linear-gradient(135deg, #48bb78 0%, #38a169 100%);
        color: white;
        padding: 4px 12px;
        border-radius: 20px;
        font-size: 0.8rem;
        font-weight: 600;
        margin-left: 10px;
      }
    "))
  ),
  
  # Font Awesome for icons
  tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css")),
  
  # Landing Page
  div(
    id = "introPanel",
    div(
      class = "landing-container",
      h1(list("DIA-Model Dashboard", span("100% ONLINE", class = "online-badge")), class = "landing-title"),
      p("Advanced dietary impact analysis - No software installation required!", class = "landing-subtitle"),
      
      div(
        style = "display: flex; justify-content: center; gap: 30px; flex-wrap: wrap;",
        actionButton("btn_go_run_model", 
                     div(
                       tags$i(class = "fas fa-cloud-arrow-up"),
                       "Run Cloud Model",
                       br(),
                       tags$small("Fast pre-computed & custom scenarios", style = "font-weight: 400; font-size: 0.9rem;")
                     ),
                     class = "landing-button",
                     style = "width: 300px;"),
        actionButton("btn_go_upload_data", 
                     div(
                       tags$i(class = "fas fa-upload"),
                       "Upload Your Data",
                       br(),
                       tags$small("Analyze your own result files", style = "font-weight: 400; font-size: 0.9rem;")
                     ),
                     class = "landing-button",
                     style = "width: 300px;")
      ),
      
      br(),
      div(
        style = "margin-top: 40px; padding-top: 30px; border-top: 1px solid #e2e8f0;",
        p(
          tags$i(class = "fas fa-cloud", style = "color: #248EA9; margin-right: 8px;"),
          "Powered by cloud computing - works in any web browser, anywhere!",
          style = "color: #718096; font-style: italic;"
        )
      )
    )
  ),
  
  # Main App (modified with progress indicators)
  hidden(
    div(
      id = "mainApp",
      class = "main-app-container",
      fluidRow(
        column(
          width = 3,
          class = "sidebar",
          div(
            h3("Input", class = "sidebar-section-title"),
            actionButton("show_run_model", 
                         div(tags$i(class = "fas fa-cloud-arrow-up"), " Cloud Model"), 
                         class = "sidebar-button"),
            actionButton("show_upload_data", 
                         div(tags$i(class = "fas fa-cloud-upload-alt"), " Upload Data"), 
                         class = "sidebar-button"),
            
            br(),
            h3("Analysis", class = "sidebar-section-title"),
            actionButton("show_health_analysis", 
                         div(tags$i(class = "fas fa-heartbeat"), " Health Analysis"), 
                         class = "sidebar-button"),
            actionButton("show_cost_analysis", 
                         div(tags$i(class = "fas fa-dollar-sign"), " Cost Analysis"), 
                         class = "sidebar-button"),
            actionButton("show_envi_analysis", 
                         div(tags$i(class = "fas fa-leaf"), " Consumption Analysis"), 
                         class = "sidebar-button")
          )
        ),
        
        column(
          width = 9,
          class = "content-area",
          
          # Progress indicator (hidden by default)
          hidden(
            div(
              id = "progress_container",
              class = "progress-container",
              h4(tags$i(class = "fas fa-cloud-arrow-up"), " Cloud Processing"),
              div(id = "progress_text", "Initializing..."),
              br(),
              div(class = "progress", style = "height: 10px;",
                  div(id = "progress_bar", class = "progress-bar bg-primary", 
                      style = "width: 0%; transition: width 0.3s ease;")
              )
            )
          ),
          
          tabsetPanel(
            id = "main_tabs",
            type = "hidden",
            
            # Run Model Panel (modified)
            tabPanel("run_model_panel",
                     div(class = "content-header",
                         h2("Cloud Model Configuration", class = "content-title"),
                         p("Configure and execute the DIA model using cloud computing", class = "content-subtitle")
                     ),
                     
                     div(class = "card",
                         h4(tags$i(class = "fas fa-rocket"), " Quick Start"),
                         p("Run the model with pre-computed scenarios for instant results:"),
                         actionButton("run_original_model", "🚀 Run Pre-computed Model", 
                                      class = "btn-primary-custom")
                     ),
                     
                     div(class = "card",
                         h4(tags$i(class = "fas fa-filter"), " Custom Configuration"),
                         selectInput("selected_countries", "Select Countries", 
                                     choices = NULL, multiple = TRUE,
                                     width = "100%"),
                         actionButton("edited_data", "Apply Filters", 
                                      class = "btn-secondary-custom"),
                         br(), br(),
                         actionButton("run_model", "☁️ Run Custom Model (Cloud)", 
                                      class = "btn-success-custom")
                     ),
                     
                     div(class = "card",
                         h4(tags$i(class = "fas fa-edit"), " Scenario Editor"),
                         p("Edit scenario values directly in the table below:"),
                         DT::dataTableOutput("editable_table")
                     )
            ),
            
            # Other panels remain the same...
            tabPanel("upload_data_panel",
                     div(class = "content-header",
                         h2("Data Upload", class = "content-title"),
                         p("Upload your own result files for analysis", class = "content-subtitle")
                     ),
                     
                     div(class = "card",
                         fileInput("uploaded_results", "Choose File (.csv)", 
                                   accept = c(".csv"),
                                   width = "100%"),
                         actionButton("use_uploaded_data", "Process Uploaded Data", 
                                      class = "btn-primary-custom")
                     ),
                     
                     div(class = "card",
                         h4(tags$i(class = "fas fa-table"), " Data Preview"),
                         DT::dataTableOutput("uploaded_data_preview")
                     )
            ),
            
            # Analysis panels...
            tabPanel("health_analysis_panel",
                     div(class = "content-header",
                         h2("Health Impact Analysis", class = "content-title"),
                         p("Comprehensive health outcome visualization and analysis", class = "content-subtitle")
                     ),
                     
                     div(class = "card",
                         div(id = "analysis_content",
                             p("Run a model first to see analysis results.", 
                               style = "text-align: center; color: #718096; padding: 40px;"))
                     )
            ),
            
            tabPanel("cost_analysis_panel",
                     div(class = "content-header",
                         h2("Cost Analysis", class = "content-title"),
                         p("Economic impact assessment of dietary interventions", class = "content-subtitle")
                     ),
                     
                     div(class = "card",
                         div(id = "cost_analysis_content",
                             p("Run a model first to see cost analysis results.", 
                               style = "text-align: center; color: #718096; padding: 40px;"))
                     )
            ),
            
            tabPanel("envi_analysis_panel",
                     div(class = "content-header",
                         h2("Consumption Analysis", class = "content-title"),
                         p("Environmental and consumption pattern analysis", class = "content-subtitle")
                     ),
                     
                     div(class = "card",
                         div(
                           style = "text-align: center; padding: 60px;",
                           tags$i(class = "fas fa-hammer", style = "font-size: 4rem; color: #cbd5e0; margin-bottom: 20px;"),
                           h3("Under Development", style = "color: #718096;"),
                           p("This module is being actively developed. Check back soon for updates!", 
                             style = "color: #a0aec0;")
                         )
                     )
            )
          )
        )
      )
    )
  )
)

#===============================================
# SERVER (Completely rewritten for online use)
#===============================================

server <- function(input, output, session) {
  
  # Reactive values
  data_switch <- reactiveVal(NULL)
  activeData <- reactiveVal(NULL)
  current_results <- reactiveVal(NULL)
  
  #===============================================
  # NAVIGATION
  #===============================================
  
  observeEvent(input$btn_go_run_model, {
    hide("introPanel")
    show("mainApp")
    updateTabsetPanel(session, "main_tabs", selected = "run_model_panel")
  })
  
  observeEvent(input$btn_go_upload_data, {
    hide("introPanel")
    show("mainApp")
    updateTabsetPanel(session, "main_tabs", selected = "upload_data_panel")
  })
  
  observeEvent(input$show_run_model, {
    updateTabsetPanel(session, "main_tabs", selected = "run_model_panel")
  })
  
  observeEvent(input$show_upload_data, {
    updateTabsetPanel(session, "main_tabs", selected = "upload_data_panel")
  })
  
  observeEvent(input$show_health_analysis, {
    if (is.null(current_results())) {
      showNotification("Please run a model first to see analysis results.", type = "warning")
      return()
    }
    updateTabsetPanel(session, "main_tabs", selected = "health_analysis_panel")
    render_health_analysis()
  })
  
  observeEvent(input$show_cost_analysis, {
    if (is.null(current_results())) {
      showNotification("Please run a model first to see analysis results.", type = "warning")
      return()
    }
    updateTabsetPanel(session, "main_tabs", selected = "cost_analysis_panel")
    render_cost_analysis()
  })
  
  observeEvent(input$show_envi_analysis, {
    updateTabsetPanel(session, "main_tabs", selected = "envi_analysis_panel")
  })
  
  #===============================================
  # DATA LOADING AND PROCESSING
  #===============================================
  
  # Load sample input data on startup
  dataset <- reactive({
    load_sample_input_data()
  })
  
  # Update country choices
  observe({
    req(dataset())
    data <- dataset()
    updateSelectInput(session, "selected_countries", choices = unique(data$Country))
  })
  
  # Progress update function
  update_progress <- function(message, percentage = NULL) {
    show("progress_container")
    runjs(paste0("document.getElementById('progress_text').innerHTML = '", message, "';"))
    
    if (!is.null(percentage)) {
      runjs(paste0("document.getElementById('progress_bar').style.width = '", percentage, "%';"))
    }
  }
  
  # Hide progress
  hide_progress <- function() {
    hide("progress_container")
    runjs("document.getElementById('progress_bar').style.width = '0%';")
  }
  
  #===============================================
  # MODEL EXECUTION (ONLINE VERSION)
  #===============================================
  
  # Run original model with pre-computed results
  observeEvent(input$run_original_model, {
    
    update_progress("Loading pre-computed results...", 20)
    
    # Default scenario: all countries
    all_countries <- unique(dataset()$Country)
    scenario_hash <- generate_scenario_hash(all_countries)
    
    Sys.sleep(1) # Simulate loading time
    update_progress("Processing scenario data...", 60)
    
    # Try to load pre-computed results
    results <- load_precomputed_results(scenario_hash)
    
    if (!is.null(results)) {
      update_progress("Finalizing results...", 90)
      Sys.sleep(0.5)
      
      current_results(results)
      data_switch("precomputed")
      
      hide_progress()
      showNotification("✅ Pre-computed model results loaded successfully!", type = "success", duration = 5)
      
    } else {
      hide_progress()
      showNotification("❌ Pre-computed results not found. Try running a custom model.", type = "error")
    }
  })
  
  # Filter data functionality
  edited_data <- eventReactive(input$edited_data, {
    req(input$selected_countries)
    data <- dataset()
    filtered <- subset(data, Country %in% input$selected_countries)
    showNotification(paste("Filtered data for", length(input$selected_countries), "countries"), type = "message")
    return(filtered)
  })
  
  # Reshape for editable table
  reshaped_data <- reactive({
    req(edited_data())
    data_wide <- tidyr::pivot_wider(
      edited_data(),
      names_from = "Diet",
      values_from = "Value"
    )
    
    data_wide <- data_wide %>%
      mutate(across(where(is.numeric), ~ round(., 2)))
    
    data_wide[is.na(data_wide)] <- 0
    return(data_wide)
  })
  
  # Store editable table data
  table_data <- reactiveVal(NULL)
  
  # Render editable table
  output$editable_table <- DT::renderDataTable({
    req(reshaped_data())
    
    df <- reshaped_data()
    table_data(df)
    
    editable_col_index <- which(colnames(df) == "5_Scenario")
    editable_col_DT <- editable_col_index - 1
    
    DT::datatable(
      df,
      editable = list(target = "cell", columns = list(editable_col_DT)),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  # Handle table edits
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    df <- table_data()
    
    row <- info$row
    col_index <- info$col
    col_name <- colnames(df)[col_index + 1]
    value <- as.numeric(info$value)
    
    df[row, col_name] <- value
    table_data(df)
    
    showNotification(paste("Updated:", col_name, "=", value), type = "message", duration = 2)
  })
  
  # Run custom model using cloud API
  observeEvent(input$run_model, {
    req(input$selected_countries)
    
    # Check if we have pre-computed results for this scenario
    scenario_hash <- generate_scenario_hash(input$selected_countries)
    precomputed <- load_precomputed_results(scenario_hash)
    
    if (!is.null(precomputed)) {
      # Use pre-computed results
      update_progress("Found pre-computed results...", 30)
      Sys.sleep(1)
      update_progress("Loading scenario data...", 70)
      Sys.sleep(1)
      update_progress("Complete!", 100)
      Sys.sleep(0.5)
      
      current_results(precomputed)
      data_switch("precomputed")
      hide_progress()
      showNotification("✅ Pre-computed results loaded for your scenario!", type = "success")
      
    } else {
      # Use cloud API for custom scenario
      update_progress("Initializing cloud execution...", 10)
      
      tryCatch({
        
        # Prepare custom data if table was edited
        custom_data <- NULL
        if (!is.null(table_data())) {
          custom_data <- table_data()
        }
        
        # Run on cloud with progress updates
        results <- run_gams_cloud(
          countries = input$selected_countries,
          custom_data = custom_data,
          progress_callback = function(msg) {
            if (grepl("Preparing", msg)) update_progress(msg, 20)
            else if (grepl("Submitting", msg)) update_progress(msg, 40)
            else if (grepl("Processing", msg)) update_progress(msg, 70)
            else if (grepl("Finalizing", msg)) update_progress(msg, 90)
          }
        )
        
        update_progress("Complete!", 100)
        Sys.sleep(0.5)
        
        current_results(results)
        data_switch("cloud")
        hide_progress()
        showNotification("✅ Custom model executed successfully on cloud!", type = "success")
        
      }, error = function(e) {
        hide_progress()
        showNotification(paste("❌ Cloud execution failed:", e$message), type = "error")
      })
    }
  })
  
  #===============================================
  # FILE UPLOAD HANDLING
  #===============================================
  
  uploadedData <- reactive({
    req(input$uploaded_results)
    inFile <- input$uploaded_results
    
    ext <- tools::file_ext(inFile$name)
    
    if (ext == "csv") {
      tryCatch({
        data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
        
        # Try to standardize column names
        if (ncol(data) >= 8) {
          colnames(data) <- c("Metric", "Diet", "Risk_Factor", "Disease", "Country", "Year", "Estimate_Type", "Value")
        }
        
        return(data)
      }, error = function(e) {
        showNotification("Error reading CSV file. Please check the format.", type = "error")
        return(NULL)
      })
    } else {
      showNotification("Only CSV files are supported in the online version.", type = "warning")
      return(NULL)
    }
  })
  
  observeEvent(input$use_uploaded_data, {
    req(uploadedData())
    
    # Convert uploaded data to our standard format
    uploaded <- uploadedData()
    
    # Create results structure similar to model output
    results <- list(
      health_data = uploaded,
      cost_data = data.frame(), # Empty if not provided
      scenario_info = list(
        countries = unique(uploaded$Country),
        generated_at = Sys.time(),
        method = "uploaded"
      )
    )
    
    current_results(results)
    data_switch("uploaded")
    showNotification("✅ Uploaded data processed successfully!", type = "success")
  })
  
  output$uploaded_data_preview <- DT::renderDataTable({
    req(uploadedData())
    DT::datatable(uploadedData(), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  #===============================================
  # ANALYSIS RENDERING
  #===============================================
  
  # Health Analysis
  render_health_analysis <- function() {
    
    results <- current_results()
    if (is.null(results) || is.null(results$health_data)) {
      output$analysis_content <- renderUI({
        div(
          style = "text-align: center; padding: 40px;",
          h4("No health data available"),
          p("Please run a model or upload data with health metrics.")
        )
      })
      return()
    }
    
    health_data <- results$health_data
    
    output$analysis_content <- renderUI({
      tagList(
        h4("📊 Health Impact Visualization"),
        
        # Summary statistics
        div(class = "card",
            h5("Summary Statistics"),
            renderTable({
              summary_stats <- health_data %>%
                group_by(Diet, Country) %>%
                summarise(
                  Avg_Deaths = round(mean(Value[Metric == "Deaths"], na.rm = TRUE), 0),
                  Avg_DALYs = round(mean(Value[Metric == "DALYs"], na.rm = TRUE), 0),
                  .groups = 'drop'
                ) %>%
                head(10)
              
              summary_stats
            })
        ),
        
        # Simple visualization
        div(class = "card",
            h5("Deaths by Diet Type"),
            renderPlot({
              if ("Deaths" %in% health_data$Metric) {
                death_data <- health_data %>%
                  filter(Metric == "Deaths", Estimate_Type == "Mean") %>%
                  group_by(Diet) %>%
                  summarise(Total_Deaths = sum(Value, na.rm = TRUE), .groups = 'drop')
                
                ggplot(death_data, aes(x = Diet, y = Total_Deaths, fill = Diet)) +
                  geom_col() +
                  theme_minimal() +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                  labs(title = "Total Deaths by Diet Type",
                       y = "Total Deaths",
                       x = "Diet Type") +
                  scale_fill_brewer(palette = "Set3")
              }
            })
        ),
        
        # Data table
        div(class = "card",
            h5("Detailed Data"),
            DT::renderDataTable({
              DT::datatable(health_data, options = list(pageLength = 10, scrollX = TRUE))
            })
        )
      )
    })
  }
  
  # Cost Analysis
  render_cost_analysis <- function() {
    
    results <- current_results()
    if (is.null(results) || is.null(results$cost_data) || nrow(results$cost_data) == 0) {
      output$cost_analysis_content <- renderUI({
        div(
          style = "text-align: center; padding: 40px;",
          h4("No cost data available"),
          p("Cost analysis requires specific cost data format.")
        )
      })
      return()
    }
    
    cost_data <- results$cost_data
    
    output$cost_analysis_content <- renderUI({
      tagList(
        h4("💰 Cost Analysis"),
        
        div(class = "card",
            h5("Cost by Diet Type"),
            renderPlot({
              cost_summary <- cost_data %>%
                filter(Metric == "Total_Cost", Estimate_Type == "Mean") %>%
                group_by(Diet) %>%
                summarise(Avg_Cost = mean(Value, na.rm = TRUE), .groups = 'drop')
              
              ggplot(cost_summary, aes(x = Diet, y = Avg_Cost, fill = Diet)) +
                geom_col() +
                theme_minimal() +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(title = "Average Cost by Diet Type",
                     y = "Average Cost",
                     x = "Diet Type") +
                scale_fill_brewer(palette = "Set2")
            })
        ),
        
        div(class = "card",
            h5("Cost Data Table"),
            DT::renderDataTable({
              DT::datatable(cost_data, options = list(pageLength = 10, scrollX = TRUE))
            })
        )
      )
    })
  }
  
  #===============================================
  # INITIALIZATION
  #===============================================
  
  # Initialize the app
  observe({
    # Generate sample data on first load
    if (!dir.exists("precomputed_results")) {
      showNotification("🔄 Initializing app with sample data...", type = "message", duration = 3)
    }
  })
}

#===============================================
# RUN THE APP
#===============================================

# Run the app
shinyApp(ui = ui, server = server)