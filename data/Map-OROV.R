# Install necessary packages if not already installed
install.packages("sf")
install.packages("ggplot2")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("dplyr")
install.packages("remotes")
remotes::install_github("ropensci/rnaturalearthhires")

# Load required libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(dplyr)

# Read data from CSV file
data <- read.csv("Genomes-OROV.csv")

# Summarize the data to get the total count per state
data_summary <- data %>%
  group_by(state) %>%
  summarise(total_count = sum(count))

# Get the shapefile for Brazil
brazil <- ne_states(country = "Brazil", returnclass = "sf")

# Add macro-region information (ensure accurate mapping of states to macro-regions)
macro_region_info <- data.frame(
  name = c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins",
           "Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe",
           "Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul",
           "Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo",
           "Paraná", "Rio Grande do Sul", "Santa Catarina"),
  macro_region = c("North", "North", "North", "North", "North", "North", "North",
                   "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast", "Northeast",
                   "Central-West", "Central-West", "Central-West", "Central-West",
                   "Southeast", "Southeast", "Southeast", "Southeast",
                   "South", "South", "South")
)


# Merge the macro-region info into the brazil data frame
brazil <- brazil %>%
  left_join(macro_region_info, by = c("name" = "name"))

# Manually create a data frame with the centroids of the states
state_coords <- data.frame(
  state = c("Acre", "Mato Grosso", "Santa Catarina", "Bahia", "Minas Gerais"),
  lon = c(-70.5, -55.5, -49.3, -41.5, -44.0),  # Example longitudes
  lat = c(-9.0, -12.5, -27.0, -12.5, -18.0)   # Example latitudes
)

# Recode state names to match the postal codes in the shapefile
data_summary <- data_summary %>%
  mutate(state = recode(state,
                        "Acre" = "Acre",
                        "Mato Grosso" = "Mato Grosso",
                        "Santa Catarina" = "Santa Catarina",
                        "Bahia" = "Bahia",
                        "Minas Gerais" = "Minas Gerais"))

# Merge the data_summary with state_coords
data_summary <- merge(data_summary, state_coords, by = "state")

# Define a color scale for the macro-regions (grey scale)
grey_scale <- scale_fill_manual(values = c("North" = "grey20", "Central-West" = "grey40", 
                                           "South" = "grey60", "Northeast" = "grey80", 
                                           "Southeast" = "grey100"))

# Plot the map with macro-regions in grey scale and genome counts
ggplot(data = brazil) +
  geom_sf(aes(fill = macro_region), color = "black") +
  grey_scale +
  geom_sf_text(aes(label = name), size = 3, color = "black") +
  geom_point(data = data_summary, aes(x = lon, y = lat, size = total_count), shape = 21, fill = "blue", color = "black") +
  geom_text(data = data_summary, aes(x = lon, y = lat, label = total_count), vjust = -1.5, hjust = 0.5, size = 3.5, color = "black") +
  labs(title = "Number of Genomes Obtained from OROV by State",
       x = "Longitude",
       y = "Latitude") +
  scale_size_continuous(range = c(3, 10), name = "Genome Count") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(size = guide_legend(title = "Genome Count"))
