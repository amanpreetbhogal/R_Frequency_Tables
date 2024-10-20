# The following code allows you to create multiple frequency tables at once
# The tables display the percentages
# You will simply need to specify the variable names and the data frame you are using

# Load these libraries to successfully run this code
library(dplyr)
library(gt)
library(tibble)

# Load data set into R

# Define the list of variables for which you want to create tables
variables <- c("variable1", "variable2", "variable3", "variable4", "variable5")

# Function to create a frequency table for a variable using the gt package
create_gt_table <- function(variable_name, data_frame) {
  
  # Ensure the variable name exists in the data frame
  if (!variable_name %in% names(data_frame)) {
    stop(paste("The variable", variable_name, "is not found in the data frame."))
  }
  
  # Create a frequency table with percentages using the specified variable
  percentage_table <- data_frame %>%
    group_by(!!sym(variable_name)) %>%
    summarise(count = n()) %>%
    mutate(percentage = (count / sum(count)) * 100, .groups = 'drop')  # .groups = 'drop' to avoid grouping issues
  
  # Create a gt table
  gt_table <- percentage_table %>%
    arrange(desc(count)) %>%
    gt() %>%
    tab_header(title = paste(variable_name, "Distribution as Percentage")) %>%
    cols_label(!!sym(variable_name) := variable_name,
               count = "Count",
               percentage = "Percentage")
  
  # Save the gt table as an image
  gtsave(gt_table, filename = paste0(variable_name, "_percentage_table.png"))
}

# Loop through the variables and create tables
for (variable in variables) {
  create_gt_table(variable, road_to_reentry_michigan) # change the second parameter to the name of your data set
}