library(shiny)
library(lavaan)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(semPlot)
library(gganimate)
library(transformr)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)
library(bslib)
library(plotly)
library(cluster)
library(magick)

generate_simulated_data <- function(n = 5000, reexperiencing_mean = 3.01, 
                                    avoidance_mean = 3.34, numbing_mean = 3.14,
                                    hyperarousal_mean = 3.66, external_events = list(), seed = NULL) {
  t <- 12
  n <- floor(n / t) * t  # Adjust n to be divisible by t
  Time <- rep(1:t, each = n / t)
  Group <- rep(1:(n / t), t)
  
  # Seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  } else {
    dynamic_seed <- as.integer((reexperiencing_mean + avoidance_mean + numbing_mean + 
                                  hyperarousal_mean) * 190021)
    set.seed(dynamic_seed)
  }
  
  # Base symptoms
  Reexperiencing <- rnorm(n, mean = reexperiencing_mean, sd = 0.99) + 0.03 * Time - 0.01 * Time^2
  Avoidance <- rnorm(n, mean = avoidance_mean, sd = 1.12) - 0.02 * Time + 0.2 * sin(pi * Time / 6)
  Numbing <- rnorm(n, mean = numbing_mean, sd = 1.00) + 0.1 * log(Time + 1)
  Hyperarousal <- rnorm(n, mean = hyperarousal_mean, sd = 0.84) + 0.05 * Time + rnorm(n, mean = 0, sd = 0.1)
  
  # Create base data frame
  data <- data.frame(Time, Group, Reexperiencing, Avoidance, Numbing, Hyperarousal)
  
  # Process external events
  for (event in external_events) {
    if (!all(c("time", "effect_size", "vars", "decay_type") %in% names(event))) {
      warning("Invalid external event detected and skipped.")
      next
    }
    
    event_time <- event$time
    effect_size <- event$effect_size
    affected_vars <- event$vars
    decay_type <- event$decay_type
    
    for (var in affected_vars) {
      if (var %in% colnames(data)) {
        if (decay_type == "none") {
          data[[var]] <- data[[var]] + ifelse(data$Time == event_time, effect_size, 0)
        } else if (decay_type == "linear") {
          data[[var]] <- data[[var]] + ifelse(data$Time >= event_time, 
                                              effect_size / (data$Time - event_time + 1), 0)
        } else if (decay_type == "exponential") {
          data[[var]] <- data[[var]] + ifelse(data$Time >= event_time, 
                                              effect_size * exp(-0.1 * (data$Time - event_time)), 0)
        }
      }
    }
  }
  
  # PTSD Symptoms (after external events)
  time_effect <- 1 + 0.01 * Time
  data$Reexperiencing <- (0.71 * data$Avoidance + 0.42 * data$Numbing + 0.43 * data$Hyperarousal) * time_effect
  data$Avoidance <- (0.71 * data$Reexperiencing + 0.54 * data$Numbing + 0.46 * data$Hyperarousal) * time_effect
  data$Numbing <- (0.42 * data$Reexperiencing + 0.54 * data$Avoidance + 0.51 * data$Hyperarousal) * time_effect
  data$Hyperarousal <- (0.43 * data$Reexperiencing + 0.46 * data$Avoidance + 0.51 * data$Numbing) * time_effect
  
  # Secondary effects
  data$AlcoholMisuse <- 0.27 * data$Hyperarousal + 0.05 * sin(2 * pi * Time / t)
  data$TraitAnger <- -0.07 * data$Avoidance + 0.09 * data$Numbing + 0.41 * data$Hyperarousal + 0.03 * Time
  data$Depression <- 2.41 * data$Numbing + 2.35 * data$Hyperarousal - 0.1 * exp(-0.2 * Time)
  data$SuicidalIdeation <- 0.36 * data$Numbing + 0.09 * data$Depression + data$Numbing * data$Hyperarousal + 0.02 * Time^2
  data$Aggression <- 0.24 * data$AlcoholMisuse + 0.21 * data$Reexperiencing + 0.71 * data$TraitAnger + 
    data$Reexperiencing * data$Hyperarousal - 0.03 * Time^1.5 + 0.1 * Group
  
  return(data)
}

generate_scenario_data <- function(scenario) {
  if (scenario == "Baseline") {
    return(generate_simulated_data(n = 5000))
  } else if (scenario == "Reduce Anxiety") {
    return(generate_simulated_data(
      n = 5000,
      reexperiencing_mean = 2.5,  # Lower anxiety-related parameters
      hyperarousal_mean = 3.0
    ))
  } else if (scenario == "Increase Numbing Effect") {
    return(generate_simulated_data(
      n = 5000,
      numbing_mean = 4.0  # Increase numbing mean
    ))
  } else if (scenario == "Double Alcohol Misuse Effect") {
    external_events <- list(
      list(time = 6, effect_size = 1.5, vars = c("AlcoholMisuse"), decay_type = "linear")
    )
    return(generate_simulated_data(n = 5000, external_events = external_events))
  }
}

reactive_scenarios <- reactive({
  scenarios <- c("Baseline", "Reduce Anxiety", "Increase Numbing Effect", "Double Alcohol Misuse Effect")
  data_list <- lapply(scenarios, function(scenario) {
    data <- generate_scenario_data(scenario)
    data$Scenario <- scenario
    return(data)
  })
  bind_rows(data_list)
})


# Define UI
ui <- navbarPage(
  title = "PTSD Simulation",
  theme = bs_theme(
    bg = "#F1EFEE",
    fg = "#4E3629",
    primary = "#4E3629",
    secondary = "#59CBE8",
    base_font = font_google("Roboto"),
    heading_font = font_google("Merriweather")
  ),
  
  # Overview Tab
  tabPanel(
    "Overview",
    fluidRow(
      column(
        width = 8, offset = 2,  
        tags$img(src = "brown_logo.png", height = "80px", 
                 style = "display: block; margin: 0 auto;"), 
        br(),
        
        # title
        h1("PTSD Simulation Application", style = "color:#4E3629; text-align: center;"),
        
        br(),
        p(
          "Post-Traumatic Stress Disorder (PTSD) is a significant mental health condition that can develop after experiencing or witnessing traumatic events. Understanding the progression of PTSD symptoms over time and the interplay between various psychological variables is critical for both research and clinical applications.",
          style = "font-size:18px; line-height:1.6; text-align: justify; color:#4E3629;"
        ),
        p(
          "This interactive simulation application provides a platform to simulate, visualize, and analyze PTSD symptom trajectories in a dynamic and customizable way.",
          style = "font-size:18px; line-height:1.6; text-align: justify; color:#4E3629;"
        ),
        
        br(),
        h2("What Does This Application Do?", style = "color:#4E3629; margin-bottom: 15px;"),
        
        tags$ul(
          style = "list-style-type: disc; padding-left: 40px; line-height: 1.8; font-size: 17px; color: #4E3629;",
          tags$li(
            strong("Simulate PTSD Symptom Trajectories:"), 
            " Users can generate longitudinal data simulating key PTSD symptoms, including Reexperiencing, Avoidance, Numbing, and Hyperarousal. The symptoms progress over a 12-month period, with built-in options to adjust symptom severity and external events."
          ),
          tags$li(
            strong("Explore Behavioral Patterns and Scenarios:"), 
            " The app provides tools to explore the effects of specific interventions or scenarios (e.g., reducing anxiety, increasing numbing effects). These comparisons help identify symptom trends under different conditions."
          ),
          tags$li(
            strong("Visualize Patient-Level and Group-Level Trends:"), 
            " Examine mean symptom trajectories over time for a simulated population and explore individual symptom trajectories, such as Suicidal Ideation or Aggression, for a selected patient."
          ),
          tags$li(
            strong("Cluster Analysis for PTSD Symptoms:"), 
            " The app employs K-means clustering to group patients based on their PTSD symptom patterns. Users can visualize cluster results using PCA plots and time trends to better understand symptom heterogeneity within the simulated population."
          )
        ),
        
        br(),
        p(
          "The goal of this application is to provide researchers, clinicians, and educators with an interactive tool to explore PTSD symptom dynamics and uncover actionable insights for treatment and intervention strategies.",
          style = "font-size:18px; line-height:1.6; text-align: justify; color:#4E3629;"
        ),
        br(),
        
        tags$div(
          style = "border: 1px solid #E5E5E5; border-radius: 8px; padding: 15px; background-color: #F9F9F9; margin-top: 20px;",
          tags$h4("Reference", style = "color:#4E3629; margin-bottom: 10px;"),
          tags$p(
            tags$b("Hellmuth, Julianne C., Stappenbeck, Cynthia A., Hoerster, Katherine D., & Jakupcak, Matthew. "),
            "(2012). Modeling PTSD Symptom Clusters, Alcohol Misuse, Anger, and Depression as They Relate to Aggression and Suicidality in Returning U.S. Veterans. ",
            tags$i("Journal of Traumatic Stress, 25(5), 527–534. "),
            tags$a(href = "https://doi.org/10.1002/jts.21732", "https://doi.org/10.1002/jts.21732", target = "_blank"),
            style = "font-size:16px; line-height:1.5; color:#4E3629; text-align: justify;"
          ),
          tags$p(
            tags$a(
              href = "https://bruknow.library.brown.edu/permalink/01BU_INST/528fgv/cdi_proquest_miscellaneous_1611639324",
              "Link to full article",
              target = "_blank",
              style = "font-size:16px; color:#007ACC; text-decoration: none;"
            )
          )
        ),
        
        br(),
        h2("Model of PTSD Symptom Clusters and Their Relationships", style = "color:#4E3629; text-align: center;"),
        div(
          style = "text-align: center; margin-top: 20px;",
          tags$img(src = "Model.png", width = "80%", style = "border: 1px solid #ddd; border-radius: 8px; box-shadow: 0px 2px 5px #ccc;")
        )
      )
    )
  ),
  
  # Data Generator Tab
  tabPanel(
    "Data Generator",
    sidebarLayout(
      sidebarPanel(
        h3("Adjust Base Symptoms"),
        sliderInput(
          inputId = "reexperiencing_mean", 
          label = "Based Reexperiencing Mean (1-5):", 
          min = 1, max = 5, value = 3.01, step = 0.01, 
          animate = animationOptions(interval = 500, loop = TRUE)
        ),
        sliderInput(
          inputId = "avoidance_mean", 
          label = "Based Avoidance Mean (1-5):", 
          min = 1, max = 5, value = 3.34, step = 0.01, 
          animate = animationOptions(interval = 500, loop = TRUE)
        ),
        sliderInput(
          inputId = "numbing_mean", 
          label = "Based Numbing Mean (1-5):", 
          min = 1, max = 5, value = 3.14, step = 0.01, 
          animate = animationOptions(interval = 500, loop = TRUE)
        ),
        sliderInput(
          inputId = "hyperarousal_mean", 
          label = "Based Hyperarousal Mean (1-5):", 
          min = 1, max = 5, value = 3.66, step = 0.01, 
          animate = animationOptions(interval = 500, loop = TRUE)
        ),
        
        # External Events Input
        h3("Add External Events"),
        numericInput("event_time", "Event Time (1-12)", value = 1, min = 1, max = 12, step = 1),
        numericInput("effect_size", "Effect Size", value = 0.5, step = 0.1),
        selectInput("affected_vars", "Affected Variables", 
                    choices = c("Reexperiencing", "Avoidance", "Numbing", "Hyperarousal"), 
                    selected = "Numbing", multiple = TRUE),
        selectInput("decay_type", "Decay Type", 
                    choices = c("none", "linear", "exponential"), selected = "exponential"),
        actionButton("add_event", "Add Event"),
        
        # Generate and Reset Options
        actionButton("generate_data", "Generate Data", class = "btn-success"),
        actionButton("reset_parameters", "Reset Parameters", class = "btn-secondary"),
        
        # Download Option
        downloadButton("download_data", "Download Data")
      ),
      
      mainPanel(
        h3("Generated Data Preview"),
        tableOutput("data_preview"),
        
        h3("External Events"),
        tableOutput("event_preview"),
        
        h3("Summary Data"),
        DT::dataTableOutput("summary_data") 
      )
    )
  ),
  
  tabPanel(
    "Behavioral Animations",
    fluidPage(
      h3("Behavioral Animations"),
      tabsetPanel(
        id = "behavioral_animations_tabs",
        
        # Tab 1: Group-Level Animations
        tabPanel(
          "Group-Level Animations",
          fluidRow(
            column(
              width = 5,  
              h4("Group Suicidal Ideation Animation"),
              div(
                style = "height: 400px;",
                imageOutput("suicidal_animation_plot", height = "100%")
              )
            ),
            column(
              width = 5,  
              h4("Group Aggression Animation"),
              div(
                style = "height: 400px;",
                imageOutput("aggression_animation_plot", height = "100%")
              )
            )
          )
        ),
        
        # Tab 2: Patient-Specific Animations
        tabPanel(
          "Patient-Specific Animations",
          sidebarLayout(
            sidebarPanel(
              selectInput("selected_patient", "Select Patient (Group ID):", 
                          choices = NULL,  
                          selected = NULL)
            ),
            mainPanel(
              fluidRow(
                column(
                  6,
                  h4("Patient Suicidal Ideation Animation"),
                  div(
                    style = "height: 400px;",  # fixed height
                    imageOutput("patient_suicidal_animation_plot", height = "100%")  # Patient Suicidal Ideation
                  )
                ),
                column(
                  6,
                  h4("Patient Aggression Animation"),
                  div(
                    style = "height: 400px;",  # fixed height
                    imageOutput("patient_aggression_animation_plot", height = "100%")  # Patient Aggression
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  
  tabPanel("Scenario Comparison",
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(
                 id = "scenario_comparison_tabs",
                 
                 # Tab 1: Group-level Scenario Comparison
                 tabPanel(
                   "Group-Level Comparison",
                   selectInput("scenario", "Choose Scenario:", 
                               choices = c("Baseline", "Reduce Anxiety", "Increase Numbing Effect")),
                   actionButton("compare", "Compare Scenarios", class = "btn-primary"), 
                   selectInput("variable", "Select Variable to Plot:", 
                               choices = c("SuicidalIdeation", "Aggression")),
                   checkboxGroupInput("scenario_filter", "Select Scenarios to Display:",
                                      choices = c("Baseline", "Reduce Anxiety", "Increase Numbing Effect"),
                                      selected = c("Baseline", "Reduce Anxiety", "Increase Numbing Effect"))
                 ),
                 
                 # Tab 2: Patient-Level Scenario Comparison
                 tabPanel(
                   "Patient-Level Comparison",
                   selectInput("selected_patient2", "Select Patient (Group ID):", 
                               choices = NULL,  # Dynamically populated
                               selected = NULL),
                   actionButton("compare_patient", "Compare Patient Scenarios", class = "btn-primary"), 
                   selectInput("patient_variable", "Select Variable to Plot:", 
                               choices = c("SuicidalIdeation", "Aggression"))
                 )
               )
             ),
             mainPanel(
               tabsetPanel(
                 id = "main_plot_tabs",
                 
                 # Group-Level Comparison Plots
                 tabPanel("Group-Level Plots",
                          plotlyOutput("selected_scenario_plot"),
                          br(),
                          plotlyOutput("overall_comparison_plot")),
                 
                 # Patient-Level Comparison Plots
                 tabPanel("Patient-Level Plots",
                          plotlyOutput("patient_selected_scenario_plot"),
                          br(),
                          plotlyOutput("patient_overall_comparison_plot"))
               )
             )
           )
  ),
  
  tabPanel("Cluster Analysis of Simulated PTSD Data",
           sidebarLayout(
             sidebarPanel(
               sliderInput("clusters", "Number of Clusters:", 
                           min = 2, max = 10, value = 3, step = 1),
               actionButton("run_cluster", "Run Clustering"),
               hr(),
               h4("Cluster Summary"),
               tableOutput("cluster_summary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("PCA Plot", 
                          fluidRow(
                            column(12, plotOutput("pca_plot", height = "600px"))
                          )
                 ),
                 tabPanel("Time Trends", 
                          fluidRow(
                            column(12, plotOutput("time_trends", height = "600px"))
                          )
                 )
               ),
               fluidRow(
                 column(12, 
                        h4("Cluster Summary"),
                        tableOutput("cluster_summary")
                 )
               )
             )
           )
  )
)


# Define Server Logic
server <- function(input, output, session) {
  # Reactive values to store parameters and events
  parameters <- reactiveValues(
    reexperiencing_mean = 3.01,
    avoidance_mean = 3.34,
    numbing_mean = 3.14,
    hyperarousal_mean = 3.66,
    external_events = list()
  )
  
  # Reactive dataset
  dataset <- reactive({
    generate_simulated_data(
      n = 5000,
      reexperiencing_mean = parameters$reexperiencing_mean,
      avoidance_mean = parameters$avoidance_mean,
      numbing_mean = parameters$numbing_mean,
      hyperarousal_mean = parameters$hyperarousal_mean,
      external_events = parameters$external_events
    )
  })
  
  # Add external event
  observeEvent(input$add_event, {
    new_event <- list(
      time = input$event_time,
      effect_size = input$effect_size,
      vars = input$affected_vars,
      decay_type = input$decay_type
    )
    parameters$external_events <- append(parameters$external_events, list(new_event))
  })
  
  # Update parameters and generate data
  observeEvent(input$generate_data, {
    parameters$reexperiencing_mean <- input$reexperiencing_mean
    parameters$avoidance_mean <- input$avoidance_mean
    parameters$numbing_mean <- input$numbing_mean
    parameters$hyperarousal_mean <- input$hyperarousal_mean
  })
  
  # Reset parameters
  observeEvent(input$reset_parameters, {
    parameters$reexperiencing_mean <- 3.01
    parameters$avoidance_mean <- 3.34
    parameters$numbing_mean <- 3.14
    parameters$hyperarousal_mean <- 3.66
    parameters$external_events <- list()
    
    # Reset input fields
    updateNumericInput(session, "reexperiencing_mean", value = 3.01)
    updateNumericInput(session, "avoidance_mean", value = 3.34)
    updateNumericInput(session, "numbing_mean", value = 3.14)
    updateNumericInput(session, "hyperarousal_mean", value = 3.66)
  })
  
  # Preview dataset
  output$data_preview <- renderTable({
    head(dataset())
  })
  
  # Preview events
  output$event_preview <- renderTable({
    if (length(parameters$external_events) == 0) {
      return(data.frame(Event = "No events added yet"))
    }
    
    event_table <- do.call(rbind, lapply(parameters$external_events, as.data.frame))
    return(event_table)
  })
  
  # Preview summary data as a styled table
  output$summary_data <- DT::renderDataTable({
    data <- dataset()
    
    # Calculate mean for each variable
    summary <- data %>%
      select(-Time, -Group) %>%
      summarise(across(everything(), mean)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Mean")
    
    # Render DataTable
    DT::datatable(
      summary,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = FALSE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    )
  })
  
  # Download dataset
  output$download_data <- downloadHandler(
    filename = function() {
      paste("simulated_data.csv")
    },
    content = function(file) {
      write.csv(dataset(), file, row.names = FALSE)
    }
  )
  
  # Reactive dataset
  dataset <- reactive({
    generate_simulated_data(
      n = 5000,
      reexperiencing_mean = input$reexperiencing_mean,
      avoidance_mean = input$avoidance_mean,
      numbing_mean = input$numbing_mean,
      hyperarousal_mean = input$hyperarousal_mean,
      external_events = parameters$external_events
    )
  })
  
  # Populate the dropdown with unique patient IDs (Group)
  observe({
    data <- dataset()
    updateSelectInput(session, "selected_patient", 
                      choices = unique(data$Group), 
                      selected = unique(data$Group)[1])
  })
  
  ### Section 1: Group-level animations ###
  # Render suicidal ideation animation (Group)
  output$suicidal_animation_plot <- renderImage({
    data <- dataset()
    if (nrow(data) > 0) {
      trend_data <- data %>%
        group_by(Time) %>%
        summarise(MeanSuicidalIdeation = mean(SuicidalIdeation, na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = MeanSuicidalIdeation)) +
        geom_line(color = "#4E3629", size = 1) +
        geom_point(color = "#ED1C24", size = 2) +
        labs(
          title = "Group Mean Suicidal Ideation Over Time",
          x = "Months",
          y = "Mean Suicidal Ideation"
        ) +
        theme_minimal() +
        transition_reveal(Time)
      
      anim <- animate(p, nframes = 12, fps = 5, renderer = magick_renderer())
      anim_path <- tempfile(fileext = ".gif")
      image_write(anim, anim_path)
      
      list(src = anim_path, contentType = 'image/gif', alt = "Suicidal Ideation Animation")
    } else {
      NULL
    }
  }, deleteFile = TRUE)
  
  # Render aggression animation (Group)
  output$aggression_animation_plot <- renderImage({
    data <- dataset()
    if (nrow(data) > 0) {
      trend_data <- data %>%
        group_by(Time) %>%
        summarise(MeanAggression = mean(Aggression, na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = MeanAggression)) +
        geom_line(color = "#4E3629", size = 1) +
        geom_point(color = "#007ACC", size = 2) +
        labs(
          title = "Group Mean Aggression Over Time",
          x = "Months",
          y = "Mean Aggression"
        ) +
        theme_minimal() +
        transition_reveal(Time)
      
      anim <- animate(p, nframes = 12, fps = 5, renderer = magick_renderer())
      anim_path <- tempfile(fileext = ".gif")
      image_write(anim, anim_path)
      
      list(src = anim_path, contentType = 'image/gif', alt = "Aggression Animation")
    } else {
      NULL
    }
  }, deleteFile = TRUE)
  
  ### Section 2: Patient-specific animations ###
  # Render suicidal ideation animation (Patient)
  output$patient_suicidal_animation_plot <- renderImage({
    data <- dataset()
    selected_data <- data %>% filter(Group == input$selected_patient)
    
    if (nrow(selected_data) > 0) {
      p <- ggplot(selected_data, aes(x = Time, y = SuicidalIdeation)) +
        geom_line(color = "#4E3629", size = 1) +
        geom_point(color = "#ED1C24", size = 2) +
        labs(
          title = paste("Patient", input$selected_patient, "Suicidal Ideation Over Time"),
          x = "Months",
          y = "Suicidal Ideation"
        ) +
        theme_minimal() +
        transition_reveal(Time)
      
      anim <- animate(p, nframes = 12, fps = 5, renderer = magick_renderer())
      anim_path <- tempfile(fileext = ".gif")
      image_write(anim, anim_path)
      
      list(src = anim_path, contentType = 'image/gif', alt = "Patient Suicidal Ideation Animation")
    } else {
      NULL
    }
  }, deleteFile = TRUE)
  
  # Render aggression animation (Patient)
  output$patient_aggression_animation_plot <- renderImage({
    data <- dataset()
    selected_data <- data %>% filter(Group == input$selected_patient)
    
    if (nrow(selected_data) > 0) {
      p <- ggplot(selected_data, aes(x = Time, y = Aggression)) +
        geom_line(color = "#4E3629", size = 1) +
        geom_point(color = "#007ACC", size = 2) +
        labs(
          title = paste("Patient", input$selected_patient, "Aggression Over Time"),
          x = "Months",
          y = "Aggression"
        ) +
        theme_minimal() +
        transition_reveal(Time)
      
      anim <- animate(p, nframes = 12, fps = 5, renderer = magick_renderer())
      anim_path <- tempfile(fileext = ".gif")
      image_write(anim, anim_path)
      
      list(src = anim_path, contentType = 'image/gif', alt = "Patient Aggression Animation")
    } else {
      NULL
    }
  }, deleteFile = TRUE)
  
  generate_scenario_data <- function(scenario) {
    if (scenario == "Baseline") {
      return(generate_simulated_data(n = 5000))
    } else if (scenario == "Reduce Anxiety") {
      return(generate_simulated_data(
        n = 5000,
        reexperiencing_mean = 2.5,  # Lower anxiety-related parameters
        hyperarousal_mean = 3.0
      ))
    } else if (scenario == "Increase Numbing Effect") {
      return(generate_simulated_data(
        n = 5000,
        numbing_mean = 4.0  # Increase numbing mean
      ))
    }
  }
  
  # Combine data for all scenarios
  output$selected_scenario_plot <- renderPlotly({
    scenarios <- reactive_scenarios()
    if (nrow(scenarios) > 0) {
      trend_data <- scenarios %>%
        filter(Scenario == input$scenario) %>%
        group_by(Time) %>%
        summarise(SelectedVariable = mean(.data[[input$variable]], na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = SelectedVariable)) +
        geom_line(color = "#4E3629", size = 1.5) +
        geom_point(color = "#ED1C24", size = 3) +
        labs(
          title = paste("Trend for", input$scenario),
          x = "Time (Months)",
          y = input$variable
        ) +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Comparison plot: Multiple scenario trends
  output$overall_comparison_plot <- renderPlotly({
    scenarios <- reactive_scenarios()
    if (nrow(scenarios) > 0) {
      trend_data <- scenarios %>%
        filter(Scenario %in% input$scenario_filter) %>%
        group_by(Time, Scenario) %>%
        summarise(SelectedVariable = mean(.data[[input$variable]], na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = SelectedVariable, color = Scenario)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(
          title = "Comparison of Scenarios",
          x = "Time (Months)",
          y = input$variable
        ) +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Auxiliary plot: Overall scenario comparison
  output$overall_comparison_plot <- renderPlotly({
    scenarios <- reactive_scenarios()
    if (nrow(scenarios) > 0) {
      trend_data <- scenarios %>%
        filter(Scenario %in% input$scenario_filter, Time >= 1, Time <= 12) %>%
        group_by(Time, Scenario) %>%
        summarise(SelectedVariable = mean(.data[[input$variable]], na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = SelectedVariable, color = Scenario)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(
          title = "Comparison of Scenarios",
          x = "Time (Months)",
          y = input$variable
        ) +
        theme_minimal() +
        scale_color_manual(values = c("Baseline" = "#4E3629", 
                                      "Reduce Anxiety" = "#00B398", 
                                      "Increase Numbing Effect" = "#003C71"))
      
      ggplotly(p)
    }
  })
  
  # Populate dropdown with unique patient IDs
  observe({
    data <- generate_simulated_data(n = 5000)  # Generate baseline dataset
    updateSelectInput(session, "selected_patient2", 
                      choices = unique(data$Group), 
                      selected = unique(data$Group)[1])
  })
  
  # Patient-level: Selected scenario trend
  output$patient_selected_scenario_plot <- renderPlotly({
    scenarios <- reactive_scenarios()
    if (nrow(scenarios) > 0 && !is.null(input$selected_patient2)) {
      trend_data <- scenarios %>%
        filter(Scenario == input$scenario, Group == input$selected_patient2) %>%
        group_by(Time) %>%
        summarise(SelectedVariable = mean(.data[[input$patient_variable]], na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = SelectedVariable)) +
        geom_line(color = "#4E3629", size = 1.5) +
        geom_point(color = "#ED1C24", size = 3) +
        labs(
          title = paste("Patient", input$selected_patient2, "-", input$scenario, "Trend"),
          x = "Time (Months)",
          y = input$patient_variable
        ) +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  # Patient-level: Multiple scenario trends
  output$patient_overall_comparison_plot <- renderPlotly({
    scenarios <- reactive_scenarios()
    if (nrow(scenarios) > 0 && !is.null(input$selected_patient2)) {
      trend_data <- scenarios %>%
        filter(Scenario %in% input$scenario_filter, Group == input$selected_patient2) %>%
        group_by(Time, Scenario) %>%
        summarise(SelectedVariable = mean(.data[[input$patient_variable]], na.rm = TRUE))
      
      p <- ggplot(trend_data, aes(x = Time, y = SelectedVariable, color = Scenario)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(
          title = paste("Patient", input$selected_patient2, "Comparison of Scenarios"),
          x = "Time (Months)",
          y = input$patient_variable
        ) +
        theme_minimal()
      
      ggplotly(p)
    }
  })
  
  data <- reactive({
    generated_data <- generate_simulated_data(n = 5000, seed = 123)
    if (!is.data.frame(generated_data)) {
      stop("Error: Data generation failed. Please check the function.")
    }
    return(generated_data)
  })
  
  
  # cluster analysis
  cluster_result <- eventReactive(input$run_cluster, {
    
    cluster_data <- data() %>%
      group_by(Group) %>%
      summarize(
        Reexperiencing_mean = mean(Reexperiencing),
        Avoidance_mean = mean(Avoidance),
        Numbing_mean = mean(Numbing),
        Hyperarousal_mean = mean(Hyperarousal),
        AlcoholMisuse_mean = mean(AlcoholMisuse),
        Depression_mean = mean(Depression),
        TraitAnger_mean = mean(TraitAnger),
        Aggression_mean = mean(Aggression)
      )
    
    cluster_data_scaled <- as.data.frame(scale(cluster_data[,-1]))
    kmeans_result <- kmeans(cluster_data_scaled, centers = input$clusters)
    
    cluster_data$Cluster <- as.factor(kmeans_result$cluster)
    return(list(data = cluster_data, kmeans = kmeans_result))
  })
  
  # PCA visualization
  output$pca_plot <- renderPlot({
    req(cluster_result())
    cluster_data <- cluster_result()$data
    cluster_data_scaled <- as.data.frame(scale(cluster_data[,-c(1, ncol(cluster_data))]))
    pca_result <- prcomp(cluster_data_scaled)
    
    cluster_data$PC1 <- pca_result$x[, 1]
    cluster_data$PC2 <- pca_result$x[, 2]
    
    ggplot(cluster_data, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3) +
      labs(title = "PCA Plot of Clusters",
           x = "Principal Component 1",
           y = "Principal Component 2") +
      theme_minimal()
  })
  
  # time-trend
  output$time_trends <- renderPlot({
    req(cluster_result())
    cluster_data <- cluster_result()$data
    data_with_clusters <- merge(data(), cluster_data[, c("Group", "Cluster")], by = "Group")
    
    ggplot(data_with_clusters, aes(x = Time, y = Reexperiencing, color = Cluster, group = Group)) +
      geom_line(alpha = 0.3) +
      stat_smooth(aes(group = Cluster), method = "loess", se = FALSE, size = 1.5) +
      labs(title = "Reexperiencing Over Time by Cluster",
           x = "Time (Months)",
           y = "Reexperiencing Score") +
      theme_minimal()
  })
  
  # cluster summary
  output$cluster_summary <- renderTable({
    req(cluster_result())
    cluster_data <- cluster_result()$data
    cluster_data %>%
      group_by(Cluster) %>%
      summarize(
        across(
          -c(Group),     # delete group column
          mean,
          .names = "mean_{col}"
        ),
        .groups = "drop"
      )
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)