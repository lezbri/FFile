# Load necessary libraries
library(readr)      # For reading CSV files
library(ggplot2)    # For creating plots
library(gganimate)  # For creating animations

# Step 1: Read the data
SimulationProject <- read_csv("SimulationProject.csv")

# Step 2: Create two separate models for the regression
model_work <- lm(duration ~ calls + work, data = SimulationProject)
model_school <- lm(duration ~ calls + school, data = SimulationProject)

# Step 3: Generate predicted values for both models
SimulationProject$pred_work <- predict(model_work, newdata = SimulationProject)
SimulationProject$pred_school <- predict(model_school, newdata = SimulationProject)

# Step 4: Create the plot
plot_regression <- ggplot(SimulationProject, aes(x = calls, y = duration)) +
  geom_point(color = "black", size = 2) +  # Plot the actual data points in black
  geom_line(aes(y = duration), color = "black", size = 0.5, alpha = 0.5) +  # Connect the actual data points with a line
  geom_line(aes(y = pred_work), color = "red", size = 1) +  # Regression line for work (red)
  geom_line(aes(y = pred_school), color = "blue", size = 1) +  # Regression line for school (blue)
  labs(title = "Multiple Regression: Duration vs. Calls with Work and School",
       x = "Calls",
       y = "Duration") +
  theme_minimal() +
  theme(legend.position = "none")

# Step 5: Animate the plot by progressively drawing the lines
animated_plot <- plot_regression +
  transition_reveal(calls) +  # Animate the plot as calls change
  ease_aes('linear')  # Linear animation of the lines

# Step 6: Display the animation
animate(animated_plot, nframes = 100, fps = 10, width = 800, height = 600)

# Step 7: Optionally save the animation as a .gif or .mp4 file
# anim_save("regression_animation.gif", animated_plot)
