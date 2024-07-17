# Install required packages if not already installed
install.packages(c("rnaturalearth", "rnaturalearthdata"))

# Load the necessary libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Load Nigeria shapefile (you need to have the shapefile)
# You can get Nigeria's shapefile from naturalearth or other sources
nigeria <- ne_countries(scale = "medium", country = "Nigeria", returnclass = "sf")

# Assuming you have a shapefile for vegetation data in Nigeria
# For example, let's say it's named "nigeria_vegetation.shp"
# Load the vegetation shapefile (provide the correct path to your shapefile)
vegetation <- st_read("path_to_your_shapefile/nigeria_vegetation.shp")

# Plot the map
ggplot() +
  geom_sf(data = nigeria, fill = "white", color = "black") +
  geom_sf(data = vegetation, aes(fill = vegetation_type)) + # Assuming your vegetation shapefile has a column named 'vegetation_type'
  labs(title = "Vegetation Map of Nigeria",
       fill = "Vegetation Type") +
  theme_minimal()

pal <- colorBin("viridis", bins = c(0, 0.25, 0.5, 0.75, 1))

# Mapping R file using leaflet package
leaflet(nigeria) %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~wdpaid, lat = ~wdpa_pid, color = ~pal(vegetation$desig_eng)) %>%
  addLegend("bottomright", pal = pal, values = ~desig_eng,
            title = "Proportion stunted") %>%
  addScaleBar(position = c("bottomleft"))
