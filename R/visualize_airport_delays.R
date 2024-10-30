# Install necessary packages if not already installed
# install.packages(c("nycflights13", "dplyr", "ggplot2"))

# Load necessary packages
library(nycflights13)
library(dplyr)
library(ggplot2)

# Create a visualization function



visualize_airport_delays <- function() {
  # Data processing: Calculate the average delay for each airport
  airport_delays <- flights %>%
    filter(!is.na(arr_delay)) %>%
    group_by(dest) %>%
    summarise(mean_delay = mean(arr_delay, na.rm = TRUE), .groups = 'drop') %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    filter(!is.na(lon) & !is.na(lat))

  # Print the processed data for debugging, showing the first 20 rows
  print(airport_delays, n = 20)

  # Create the visualization
  ggplot(data = airport_delays, aes(x = lon, y = lat, size = mean_delay)) +
    geom_point(alpha = 0.5) +
    scale_size_continuous(range = c(1, 10), name = "Mean Arrival Delay (minutes)") +
    labs(title = "Mean Arrival Delays by Airport",
         x = "Longitude",
         y = "Latitude") +
    theme_minimal()
}

# Call the visualization function
visualize_airport_delays()
