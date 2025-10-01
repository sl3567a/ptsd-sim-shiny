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
