# ptsd-sim-shiny
Interactive PTSD longitudinal simulator (12 months): scenarios, time-series plots, k-means + PCA for patient/group patterns.

# PTSD Simulation Application

## Project Overview
This R Shiny application provides an interactive platform for simulating the progression of Post-Traumatic Stress Disorder (PTSD) symptoms over time. The tool is designed for researchers and clinicians to explore the effects of different scenarios on PTSD symptoms, allowing for dynamic visualization and analysis. This model simulates a longitudinal study over twelve months, where each group number represents an individual patient's ID, capturing daily recordings of symptoms throughout the year.

## Features
- **Simulate PTSD Symptom Trajectories:** Users can generate longitudinal data simulating key PTSD symptoms, including Reexperiencing, Avoidance, Numbing, and Hyperarousal. The symptoms progress over a 12-month period, with built-in options to adjust symptom severity and external events.
- **Explore Behavioral Patterns and Scenarios:** The app provides tools to explore the effects of specific interventions or scenarios (e.g., reducing anxiety, increasing numbing effects). These comparisons help identify symptom trends under different conditions.
- **Visualize Patient-Level and Group-Level Trends:** Examine mean symptom trajectories over time for a simulated population and explore individual symptom trajectories, such as Suicidal Ideation or Aggression, for a selected patient.
- **Cluster Analysis for PTSD Symptoms:** The app employs K-means clustering to group patients based on their PTSD symptom patterns. Users can visualize cluster results using PCA plots and time trends to better understand symptom heterogeneity within the simulated population.

## Getting Started

### Prerequisites
Ensure you have R installed on your computer along with the RStudio IDE to run Shiny applications. The following R packages should also be installed:
- Shiny
- Lavaan
- ggplot2
- and others listed in the script comments

You can install these packages using R commands like:
```R
install.packages("shiny")
```

### Link to the Shiny App
Access to the Shiny App:

```sh
https://sl3567a.shinyapps.io/PTSDapp/
```

Open the script `PHP2560R_finalproject_SLYJ.R` in RStudio.

## Main Functions

### `generate_simulated_data` - PTSD Model
- **Purpose**: Generates simulated dataset for PTSD symptoms based on predefined or user-adjusted means.
- **Inputs**:
  - `n`: Number of data points
  - `reexperiencing_mean`: Mean value for Reexperiencing symptom
  - `avoidance_mean`: Mean value for Avoidance symptom
  - `numbing_mean`: Mean value for Numbing symptom
  - `hyperarousal_mean`: Mean value for Hyperarousal symptom
- **Output**: Data frame with simulated PTSD symptoms over time.
- **Mathematical Expression**:
  
  $\text{Re} = \mathcal{N}(\mu_r, \sigma_r) + 0.03 \cdot \text{Time} - 0.01 \cdot \text{Time}^2$
  
  $\text{Av} = \mathcal{N}(\mu_a, \sigma_a) - 0.02 \cdot \text{Time} + 0.2 \cdot \sin\left(\frac{\pi \cdot \text{Time}}{6}\right)$
  
  $\text{Nu} = \mathcal{N}(\mu_n, \sigma_n) + 0.1 \cdot \log(\text{Time} + 1)$
  
  $\text{Hy} = \mathcal{N}(\mu_h, \sigma_h) + 0.05 \cdot \text{Time} + \mathcal{N}(0, 0.1)$
  
  $\text{Reexperiencing} = -(0.71 \cdot \text{Av}+ 0.42 \cdot \text{Nu}+0.43 \cdot \text{Hy})\cdot (1-0.01 \cdot \text{Time})$
  
  $\text{Avoidance} = -(0.71 \cdot \text{Re} + 0.54 \cdot \text{Nu} + 0.46 \cdot \text{Hy})\cdot (1-0.01 \cdot \text{Time})$
  
  $\text{Numbing} = -(0.42 \cdot \text{Re} + 0.54 \cdot \text{Av} + 0.51 \cdot \text{Hy})\cdot (1-0.01 \cdot \text{Time})$
  
  $\text{Hyperarousal} = -(0.43 \cdot \text{Re} + 0.46 \cdot \text{Av} + 0.51 \cdot \text{Nu})\cdot (1-0.01 \cdot \text{Time})$
  
  $\text{Alcohol Misuse} = 0.27 \cdot \text{Hyperarousal} + 0.05 \cdot \sin\left(\frac{2 \pi \cdot \text{Time}}{t}\right)$
  
  $\text{Trait Anger} = -0.07 \cdot \text{Avoidance} + 0.09 \cdot \text{Numbing} + 0.41 \cdot \text{Hyperarousal} + 0.03 \cdot \text{Time}$
  
  $\text{Depression} = 2.41 \cdot \text{Numbing} + 2.35 \cdot \text{Hyperarousal} - 0.1 \cdot e^{-0.2 \cdot \text{Time}}$
  
  $\text{Suicidal Ideation} = 0.36 \cdot \text{Numbing} + 0.09 \cdot \text{Depression} + \text{Numbing} \cdot \text{Hyperarousal} + 0.02 \cdot \text{Time}^2$
  
  $\text{Aggression} = 0.24 \cdot \text{Alcohol Misuse} + 0.21 \cdot \text{Reexperiencing} + 0.71 \cdot \text{Trait Anger} + \text{Reexperiencing} \cdot \text{Hyperarousal}$
  
  $- 0.03 \cdot \text{Time}^{1.5} + 0.1 \cdot \text{Group}$

### `generate_scenario_data`
- **Purpose**: Creates dataset variations by modifying base symptoms to simulate different intervention scenarios.
- **Inputs**:
  - `scenario`: A predefined scenario name such as "Reduce Anxiety" or "Increase Numbing Effect"
- **Output**: Data frame adjusted to the selected scenario parameters.

### `reactive_scenarios`
- **Purpose**: Aggregates data across different scenarios for comparison in the Shiny app.
- **Output**: Combined reactive object containing data frames for all selected scenarios.

## References
- **Hellmuth, Julianne C., Stappenbeck, Cynthia A., Hoerster, Katherine D., & Jakupcak, Matthew.** (2012). Modeling PTSD Symptom Clusters, Alcohol Misuse, Anger, and Depression as They Relate to Aggression and Suicidality in Returning U.S. Veterans. *Journal of Traumatic Stress, 25(5), 527–534.* https://doi.org/10.1002/jts.21732
