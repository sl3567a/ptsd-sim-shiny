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