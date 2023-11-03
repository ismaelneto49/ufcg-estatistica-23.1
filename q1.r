data <- read.csv("/home/ismael/IdeaProjects/ufcg/besteiras/estatistica/penguin.csv")

mean <- mean(data$comprimento_bico.mm.)
median <- median(data$comprimento_bico.mm.)

calculate_mode <- function(x) {
  unique_values <- unique(x)
  value_counts <- table(x)
  max_count <- max(value_counts)
  mode_values <- unique_values[value_counts == max_count]
  return(mode_values)
}

normalizePath("")

# Example: Calculate the mode of a column named "ColumnName" in your data frame
modes_of_column <- calculate_mode(data$ColumnName)