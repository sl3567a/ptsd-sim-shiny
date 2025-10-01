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

