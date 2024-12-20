##############################################################################################################################
# Programming (Group) Project in R
# Course: Skills: Programming with Advanced Computer Languages
#
# Spatial Data Analysis and Hedonic Housing Price Regression 
#
# Friday, 2024-12-20
#
# Pascal Simon (19-615-731) (CodingXCamp: Pasci)
##############################################################################################################################


##############################################################################################################################
# Motivation for the Project
##############################################################################################################################
# 
# The inspiration for this project lies in my ambition to conduct a comprehensive data analysis in R, leveraging a real-world 
# dataset to explore each step of the analytical process. My fascination with this topic began during my undergraduate studies 
# at HSG, specifically in a course on Real Estate Economics and Investments. In that course, we briefly examined Hedonic 
# Regression Analysis as a method for estimating housing prices. This introduction sparked my interest in applying this 
# technique in a practical, hands-on context to gain deeper insights and experience.
# 
# This project also provided a unique opportunity to work with spatial data for the first time. I was particularly interested 
# in how geographic and spatial information could enhance the dataset by introducing novel and meaningful variables. For example, 
# I’ve always been curious about the impact of public transportation on housing prices. Does proximity to transit increase value
# due to convenience, or decrease it due to noise and congestion? Incorporating spatial data not only facilitates such analyses
# but also allows for the creation of engaging and intuitive visualizations, such as maps, to better communicate insights.
# 
##############################################################################################################################


##############################################################################################################################
# Instructions
##############################################################################################################################
#
# This project is implemented in R and is best executed in RStudio to ensure compatibility and consistent results. 
# 
# Key Guidelines:
# 
# 1. Step-by-Step Execution:
#   - The code is structured sequentially, and each section builds upon the previous one.
#     To ensure accurate results, run the script step by step in the order provided. 
#     Avoid skipping steps or executing sections out of sequence, as this may lead to errors or incomplete analysis.
# 
# 2. Download of Dataset:
#   - For this analysis a real world dataset from Kaggle containing 600'000 observations about US Homes will be used.
#   - Download the Dataset "600K US Housing Properties.csv" from the link below. 
#     Link: https://www.kaggle.com/datasets/polartech/500000-us-homes-data-for-sale-properties/data
#     (Note, that it may take a few seconds, since the csv file is over 200 MB in size)
# 
# 3. Save the Dataset correctly:
#   - Save the dataset in a folder on your computer (for example in a folder called "HousingPriceAnalysis").
#   - Ensure the dataset from kaggle is named "600K US Housing Properties.csv" in your Folder.
dataset_name <- "600K US Housing Properties.csv"
#   - Copy the path to this folder and set it as the path below and run the code, to set it as your working directory.

path <- "/Replace/This/With/The/Path/To/Your/Folder"  # Path to the Folder (e.g. HousingPriceAnalysis), not the File
if (!dir.exists(path)) {
  stop("The specified working directory does not exist. Please check the path.")
}
setwd(path)

# 4. Install and Load Required Packages:
#   - Use the function below to install and load all necessary packages automatically.

# This function ensures all necessary packages are installed and loaded
load_packages <- function(packages) {
  # Loop through each package in the list
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      # Install the package if it's missing
      message(paste("Installing missing package:", pkg))
      install.packages(pkg, dependencies = TRUE)
      
      # Re-check installation
      if (!require(pkg, character.only = TRUE)) {
        stop(paste("Package", pkg, "could not be installed. Please check manually."))
      }
    }
    # Load the package
    library(pkg, character.only = TRUE)
  }
}

# List of required packages
required_packages <- c(
  "ggplot2", "readr", "dplyr", "tidyr", "writexl", 
  "maps", "nominatimlite", "maptiles", "tidyterra", 
  "osmdata", "sf", "patchwork", "leaflet", "htmlwidgets", 
  "regclass", "mgcv", "shiny", "shinythemes", 
  "stargazer", "gt", "broom", "RColorBrewer"
)

# Load and install packages
message("Checking and loading required packages...")
load_packages(required_packages)

# Confirm setup success
cat("\nSetup complete! Working directory set to:", getwd(), "\n")
cat("All required packages are installed and loaded.\n")

# 5. Load in the dataset
#  - Verify the dataset is present in the specified folder.
if (!file.exists(file.path(path, dataset_name))) {
  stop(paste("Dataset not found in the folder. Ensure the file is named:", dataset_name, 
             "and that the working directory is correctly set to the folder where the dataset is stored."))
} else {
  cat("Dataset", dataset_name, "found successfully in the folder:", path, "\n")
  cat("Ready to start the analysis!\n")
}

#   - Read in the CSV Dataset
housing_data <- read_csv(file.path(path, dataset_name))
##############################################################################################################################
# Everything is set up and ready for the analysis! Enjoy :)
##############################################################################################################################
# 
# Note:
# If the code runs correctly, it will generate multiple interactive maps and regression outputs. These will be saved as HTML or 
# PDF files in the folder you have defined as your working directory. The Shiny dashboard created at the end of the script will 
# only function when executed within an R environment, as R is required to perform the calculations necessary for predictions 
# in the background.
# 
##############################################################################################################################


##############################################################################################################################
# Data Inspection, Cleaning,...
##############################################################################################################################

# Explore the dataset to understand its structure and its content
dim(housing_data) # Dimensions (600'000 Observations and 28 columns)
colnames(housing_data) # Names of the columns
summary(housing_data) # Short summary of the dataset
head(housing_data)  # displays first 6 rows
str(housing_data)  # Display the structure of the dataset
sapply(housing_data, function(x) sum(is.na(x)))  # Count missing values per column
nrow(housing_data) - nrow(distinct(housing_data)) # Check if duplicates
# View(housing_data)  # uncomment this line if you want to View the dataset more closely

# Based on the above outputs we can already see that a few columns do not contain any information so they can already be removed
# Additionally many variables are not needed for our analysis, so we drop them here already

# List of columns to drop
columns_to_drop <- c("property_id", "apartment", "price_per_unit", "broker_id", 
                     "property_status", "year_build", "total_num_units", 
                     "listing_age", "RunDate", "agency_name", "agent_name", 
                     "agent_phone", "is_owned_by_zillow")

# Drop the columns from the dataset
housing_data <- housing_data %>%
  select(-all_of(columns_to_drop))

# Verify the columns have been dropped
colnames(housing_data)
# View(housing_data)

# Remove observations with missing values in columns essential for the analysis
# variables like land_space not included, since certain types of properties do not have land_space
essential_columns <- c("city", "latitude", "longitude", "price", 
                       "bedroom_number", "bathroom_number", "living_space", "property_type")

# Filter dataset and display number remaining rows
housing_data <- housing_data %>%
  drop_na(all_of(essential_columns))

nrow(housing_data)
# View(housing_data)

# There are still alot of observations with values of 0 in the important columns
# columns to check for zero values
columns_to_check <- c("price", "bedroom_number", "bathroom_number", "living_space")

# Filter out rows where any of the specified columns have a value of 0
housing_data <- housing_data %>%
  filter(if_all(all_of(columns_to_check), ~ . != 0))

# After Viewing the dataset it became apparent, that there are observations with a negative land_space
# Drop observations where land_space is smaller than 0
housing_data <- housing_data %>%
  filter(land_space >= 0 | is.na(land_space))

nrow(housing_data)

# When for example examining range(housing_data$price), it becomes evident that additional data cleaning is necessary.
range(housing_data$price)
# However, from this point onwards a selection of 2 cities will be done, since the necessary data cleaning steps vary
# from city to city.

# Summarize number of observations for each city and property type
summary_table <- housing_data %>%
  count(city, property_type) %>% # Count observations grouped by city and property_type
  pivot_wider(
    names_from = property_type,
    values_from = n,
    values_fill = 0 # Fill missing combinations with 0
  ) %>%
  # Add a column for total observations per city and sort by it
  mutate(`Total Count` = rowSums(select(., -city))) %>% # Calculate total count across all property types
  arrange(desc(`Total Count`)) # Sort by the total count in descending order

# Display the final summary table and store it to inspect it later
# View(summary_table)
# Create Directory in Workspace to store the City and Property Type Summary
dir.create(file.path(path, "CitySelectionSummary"), recursive = TRUE, showWarnings = FALSE)

# Save as Excel file
write_xlsx(
  summary_table,
  file.path(path, "CitySelectionSummary", "city_property_summary.xlsx")
)

# After looking at the summary_table the decision was made to focus on Houston and Chicago. This selection was chosen
# due to the high number of observations available and the distinct characteristics of the cities.
# The analysis will only be done for Single Family Homes. This ensures easier interpretation of the results.

# create seperate datasets for the analysis from now onwards
# Single Family for Chicago
Single_Chicago <- housing_data %>%
  filter(city == "Chicago" & property_type == "SINGLE_FAMILY")
nrow(Single_Chicago)
# View(Single_Chicago)

# Single Family for Houston, here additional filtering for state since there are a few observations for Houston that are 
# located in a different state
Single_Houston <- housing_data %>%
  filter(city == "Houston" & property_type == "SINGLE_FAMILY" & state == "TX")
nrow(Single_Houston)
# View(Single_Houston)

# There are some very low prices in Chicago, to ensure a more even analysis remove values below 50'000$,
# such that in a similar range as in Houston.
Single_Chicago <- Single_Chicago %>%
  filter(price > 50000)

# Since Single Family Homes were chosen, observations with missing entries in land_space and land_space_unit have to be removed
Single_Chicago <- Single_Chicago %>%
  filter(!is.na(land_space) & !is.na(land_space_unit))

Single_Houston <- Single_Houston %>%
  filter(!is.na(land_space) & !is.na(land_space_unit))

# land space is sometimes stored as sqft and sometimes as acres covert everything into m^2
# Conversion factors
sqft_to_sqm <- 0.092903
acre_to_sqm <- 4046.86

# Function to convert land_space to m²
convert_to_sqm <- function(dataset) {
  dataset %>%
    mutate(
      land_space = case_when(
        land_space_unit == "sqft" ~ land_space * sqft_to_sqm,
        land_space_unit == "acres" ~ land_space * acre_to_sqm
      ),
      land_space_unit = "m^2", # Update unit
      
      # Also living_space to m² (This is always stored in sqft)
      living_space = living_space * sqft_to_sqm
    )
}

# Apply the function to both datasets
Single_Chicago <- convert_to_sqm(Single_Chicago)
Single_Houston <- convert_to_sqm(Single_Houston)

# View(Single_Chicago)
# View(Single_Houston)

# The two Datasets look rather clean already. Further cleaning steps will be done later.

##############################################################################################################################
# Enhancing the Dataset with Spatial Features using OpenStreetMap
##############################################################################################################################

# Function to create a bounding box (bbox)
# This Box is used to extract the information needed from OpenStreetMap
# Using 0.1 as standard margin, this should be enough for all cities. (Margin is needed to include information about nearby 
# amenities for locations close to the boarders of the bounding box)
create_bbox <- function(dataset, margin = 0.1) {
  
  # Calculate bbox coordinates by taking the min and max coordinates from the dataset
  x_min <- min(dataset$longitude, na.rm = TRUE) - margin
  y_min <- min(dataset$latitude, na.rm = TRUE) - margin
  x_max <- max(dataset$longitude, na.rm = TRUE) + margin
  y_max <- max(dataset$latitude, na.rm = TRUE) + margin
  
  # Return bbox as a named list
  bbox <- bbox_to_poly(c(
      x_min,
      y_min,
      x_max,
      y_max
  ))
  
  return(bbox)
}

# Create bounding box for Single_Chicago
chicago_bbox <- create_bbox(Single_Chicago, 0.1)
print(chicago_bbox)

# Create bounding box for Single_Houston
houston_bbox <- create_bbox(Single_Houston, 0.1)
print(houston_bbox)

# Function to create a Leaflet map with log-transformed price (to better show color differences in price)
map_observations <- function(data) {
  
  # Add log-transformed price as a new column to make differences in price more visible
  data$log_price <- log(data$price)
  
  # Define a color palette for the log-transformed price
  palette <- colorNumeric(palette = "YlOrRd", domain = data$log_price)
  
  # Create Leaflet map with interactive elements
  leaflet(data = data) %>%
    # Add default OpenStreetMap tiles to serve as the map background
    addTiles() %>%  # Add default OpenStreetMap tiles
    # Add circle markers to the map, representing data points
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      color = ~palette(log_price),  # Use color scale based on log price
      radius = 4,  # Size of the markers
      fillOpacity = 0.8,
      popup = ~paste( # This is used to display characteristics when pressing on a dot
        "<b>City:</b>", city, "<br>",
        "<b>Latitude:</b>", latitude, "<br>",
        "<b>Longitude:</b>", longitude, "<br>",
        "<b>Price:</b>", price, "<br>",
        "<b>Bedrooms:</b>", bedroom_number, "<br>",
        "<b>Bathrooms:</b>", bathroom_number, "<br>",
        "<b>Living Space:</b>", living_space, "<br>",
        "<b>Land Space:</b>", land_space, "<br>",
        "<b>Property Type:</b>", property_type
      )
    ) %>%
    # Add a legend to explain the color scale for prices
    addLegend(
      position = "bottomright",
      pal = palette,
      values = ~log_price,
      title = "Price",
      labFormat = labelFormat(transform = function(x) round(exp(x))), #Convert log scale back to normal price scale
      opacity = 0.8
    )
}

chicago_map <- map_observations(Single_Chicago)
chicago_map
houston_map <- map_observations(Single_Houston)
houston_map

# Create Directory in Workspace to store Maps
dir.create(file.path(path, "Maps/Chicago"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(path, "Maps/Houston"), recursive = TRUE, showWarnings = FALSE)

# Save Map as HTML file
saveWidget(
  chicago_map,
  file.path(path, "Maps/Chicago", "ObservationsMap.html")
)
saveWidget(
  houston_map,
  file.path(path, "Maps/Houston", "ObservationsMap.html")
)

# Now a first map for both cities has ben created. This map shows where each property is located
# And has a color coding based on its price. Already here some observations can be made.
# For example there houses seam to be more expensive if they are close to the city center.
# Therefore a variable distance to city center will be included, which will be done in a next step

# Function to calculate distance to city center and return the updated data frame
# The function takes the following inputs Dataset and the longitude and latitude coordinates from the city center.
# Coordinates from the City Center have to be extracted manually from Google Earth for example.
add_distance_to_center <- function(data, center_lon, center_lat) {
  # Convert data frame to sf object, crs refers to coordinate reference system used
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Define city center as an sf point
  city_center <- st_sfc(st_point(c(center_lon, center_lat)), crs = 4326)
  
  # Calculate distance to city center and convert to numeric using st_distance function from the sf package
  data$distance_to_center <- as.numeric(st_distance(data_sf, city_center))
  
  # Return updated data frame
  return(data)
}

# Add distance to center for Chicago (Coordinates manually extracted from Google Eart)
Single_Chicago <- add_distance_to_center(Single_Chicago, center_lon = -87.630, center_lat = 41.878) #change coordinates if another city is used

# Add distance to center for Houston (Coordinates manually extracted from Google Eart)
Single_Houston <- add_distance_to_center(Single_Houston, center_lon = -95.37, center_lat = 29.76)

# Check the updated data frame if distance_to_center is there with plausible ranges (in m)
range(Single_Chicago$distance_to_center)
range(Single_Houston$distance_to_center)

# It is said that having access to greenspaces increases the price of a property. To test that
# we can download all the parks from Open Street Map and create a variable distance to the closest Park.

# Function to calculate distance to closest park and return updated data frame
add_distance_to_closest_park <- function(data, bbox) {
  # Convert data to sf object
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Query OpenStreetMap for park data within the bounding box using key = "leisure" and value = "park"
  parks <- opq(bbox = bbox) %>%
    add_osm_feature(key = "leisure", value = "park") %>%
    osmdata_sf() %>%
    .$osm_polygons  # Extract park polygons
  
  # Verify that parks were retrieved
  if (nrow(parks) == 0) {
    warning("No parks found in the specified area.")
    data$distance_to_closest_park <- NA
    return(data)
  }
  
  # Ensure CRS consistency since otherwise unusable results (EPSG:4326 should be the default for osmdata)
  parks <- st_transform(parks, crs = 4326)
  
  # Calculate distance to the closest park for each property
  data$distance_to_closest_park <- apply(
    st_distance(data_sf, parks),  # Distance matrix between properties and parks
    1,  # Row-wise operation
    min  # Get minimum distance for each property
  )
  
  # Return updated data frame with the new column
  return(data)
}

# Note: This next step may take a while to download and calculate everything
Single_Chicago <- add_distance_to_closest_park(Single_Chicago,chicago_bbox)
Single_Houston <- add_distance_to_closest_park(Single_Houston,houston_bbox)


# Additionally having a supermarket could be an attribute that increases the value of a property

# Function to calculate distance to closest grocery shop and return updated data frame 
# (here both supermarket and convenience stores are included)

add_distance_to_closest_shop <- function(data, bbox) {
  # Convert data to sf object
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Query OpenStreetMap for grocery shops (key= shop, value=supermarket and convenience stores) within the bounding box
  shops <- opq(bbox = bbox) %>%
    add_osm_feature(key = "shop", value = c("supermarket", "convenience")) %>%
    osmdata_sf() %>%
    .$osm_points  # Extract points for shops
  
  # Verify that shops were retrieved
  if (nrow(shops) == 0) {
    warning("No grocery shops found in the specified area.")
    data$distance_to_closest_shop <- NA
    return(data)
  }
  
  # Ensure CRS consistency since otherwise unusable results (EPSG:4326 is default for osmdata)
  shops <- st_transform(shops, crs = 4326)
  
  # Calculate distance to the closest shop for each property
  data$distance_to_closest_shop <- apply(
    st_distance(data_sf, shops),  # Distance matrix between properties and grocery shops
    1,  # Row-wise operation
    min  # Get minimum distance for each property
  )
  
  # Return updated data frame with the new column
  return(data)
}

# Note: This next step may take a while to download and calculate everything
Single_Chicago <- add_distance_to_closest_shop(Single_Chicago,chicago_bbox)
Single_Houston <- add_distance_to_closest_shop(Single_Houston,houston_bbox)


# Additionally having public transportation nearby is often said to increase property prices
# However in certain areas this can also be negative due to increased noise and crime

# Function to calculate distance to closest public transportation stop and return updated data frame 
# Here it can be specified if bus stops in the highway category should be included or not.
add_distance_to_closest_transit <- function(data, bbox, include_bus_stop = TRUE) {
  # Convert input data to sf object
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Query for stop positions
  stop_positions <- opq(bbox = bbox) %>%
    add_osm_feature(key = "public_transport", value = "stop_position") %>%
    osmdata_sf() %>%
    .$osm_points
  
  # Initialize combined stops with stop_positions geometry
  combined_stops <- st_sf(geometry = st_geometry(stop_positions), crs = st_crs(stop_positions))
  
  # If include_bus_stop is TRUE, query for bus stops and combine geometries
  if (include_bus_stop) {
    bus_stops <- opq(bbox = bbox) %>%
      add_osm_feature(key = "highway", value = "bus_stop") %>%
      osmdata_sf() %>%
      .$osm_points
    
    # Combine stop positions and bus stops geometries
    combined_stops <- st_sf(geometry = c(st_geometry(stop_positions), st_geometry(bus_stops)), 
                            crs = st_crs(stop_positions))
  }
  
  # Remove duplicates (if any)
  combined_stops <- combined_stops[!duplicated(combined_stops), ]
  
  # Ensure CRS consistency
  if (st_crs(data_sf) != st_crs(combined_stops)) {
    combined_stops <- st_transform(combined_stops, crs = st_crs(data_sf))
  }
  
  # Calculate distance to the closest transit stop for each property
  data$distance_to_closest_transit <- apply(
    st_distance(data_sf, combined_stops),  # Distance matrix between properties and stops
    1,  # Row-wise operation
    min  # Get minimum distance for each property
  )
  
  # Return updated data frame
  return(data)
}

# Note: This next step may take a while to download and calculate everything
Single_Chicago <- add_distance_to_closest_transit(Single_Chicago,chicago_bbox)
Single_Houston <- add_distance_to_closest_transit(Single_Houston,houston_bbox)

# If we inspect the datasets, we can already see the big difference in availability of public transportation
# A variable that counts the Number of stops within 750m was first included, however yields weird results and therefore removed
# again

# Now all the additional variables have been added to the dataset (geospatial information). These variables can now be included in 
# the regression analysis but first we will create some interactive maps to visualize the data.

##############################################################################################################################
# Interactive maps
##############################################################################################################################
colnames(Single_Chicago)

# Function to create and export Leaflet map based on distance to center
# In this function the coordinates of the city center have to be specified manually again
create_and_export_distance_map <- function(data, center_lon, center_lat, path = getwd()) {
  
  # Extract city name dynamically from the dataset name
  dataset_name <- deparse(substitute(data))  # Get the name of the dataset as a string
  city_name <- strsplit(dataset_name, "_")[[1]][2]  # Extract the second part (e.g., 'Houston' from 'Single_Houston')
  
  # Check if required columns exist
  required_columns <- c("longitude", "latitude", "distance_to_center", "property_url", 
                        "address", "street_name", "city", "state", "postcode", "price",
                        "bedroom_number", "bathroom_number", "living_space", "land_space",
                        "distance_to_closest_park", "distance_to_closest_shop", 
                        "distance_to_closest_transit")
  if (!all(required_columns %in% colnames(data))) {
    stop("The input data must include the required columns.")
  }
  
  # Convert to sf object for mapping
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Define city center as a red point
  city_center <- data.frame(
    name = "City Center",
    lon = center_lon,
    lat = center_lat
  )
  city_center_sf <- st_as_sf(city_center, coords = c("lon", "lat"), crs = 4326)
  
  # Define a color palette for distance to center
  palette <- colorNumeric(
    palette = "YlOrRd",  # Yellow-Orange-Red sequential color scale
    domain = data$distance_to_center,
    na.color = "transparent"
  )
  
  # Create Leaflet map
  map <- leaflet(data = data_sf) %>%
    addTiles() %>%
    
    # Add property points color-coded by distance to center
    addCircleMarkers(
      radius = 5,
      color = ~palette(distance_to_center),
      fillOpacity = 0.8,
      popup = ~paste(
        "<b>Address:</b>", address, "<br>",
        "<b>Street Name:</b>", street_name, "<br>",
        "<b>City:</b>", city, "<br>",
        "<b>State:</b>", state, "<br>",
        "<b>Postcode:</b>", postcode, "<br>",
        "<b>Price:</b>", price, "<br>",
        "<b>Bedrooms:</b>", bedroom_number, "<br>",
        "<b>Bathrooms:</b>", bathroom_number, "<br>",
        "<b>Living Space:</b>", living_space, "<br>",
        "<b>Land Space:</b>", land_space, "<br>",
        "<b>Distance to Center:</b>", round(distance_to_center, 2), "m<br>",
        "<b>Distance to Closest Park:</b>", round(distance_to_closest_park, 2), "m<br>",
        "<b>Distance to Closest Shop:</b>", round(distance_to_closest_shop, 2), "m<br>",
        "<b>Distance to Closest Transit:</b>", round(distance_to_closest_transit, 2), "m<br>",
        "<b>Property URL:</b>", paste0("<a href='", property_url, "' target='_blank'>Link</a>")
      )
    ) %>%
    
    # Add city center as a red point
    addCircleMarkers(
      data = city_center_sf,
      radius = 7,
      color = "red",
      fillOpacity = 1,
      popup = "City Center"
    ) %>%
    
    # Add a legend for distance to center
    addLegend(
      position = "bottomright",
      pal = palette,
      values = data$distance_to_center,  # Explicitly use the column values
      title = "Distance to City Center (m)",
      opacity = 0.8
    )
  
  # Create directory for exporting the map
  export_path <- file.path(path, paste0("Maps/", city_name))
  dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define the filename dynamically based on the city name
  file_name <- paste0("DistanceToCenterMap_", city_name, ".html")
  save_path <- file.path(export_path, file_name)
  
  # Save the map as an HTML file
  saveWidget(map, save_path, selfcontained = TRUE)
  
  # Message to confirm export
  cat("Map exported successfully to:", save_path, "\n")
  
  # Return the map (optional for viewing)
  return(map)
}

create_and_export_distance_map(Single_Chicago, center_lon = -87.630, center_lat = 41.878)
create_and_export_distance_map(Single_Houston, center_lon = -95.37, center_lat = 29.76)


# Function to create and export Leaflet map based on distance to closest park
create_and_export_park_distance_map <- function(data, bbox, path = getwd()) {
  
  # Extract city name dynamically from the dataset name
  dataset_name <- deparse(substitute(data))  # Get the name of the dataset as a string
  city_name <- strsplit(dataset_name, "_")[[1]][2]  # Extract the second part (e.g., 'Houston' from 'Single_Houston')
  
  # Check if required columns exist
  required_columns <- c("longitude", "latitude", "distance_to_closest_park", "property_url", 
                        "address", "street_name", "city", "state", "postcode", "price",
                        "bedroom_number", "bathroom_number", "living_space", "land_space",
                        "distance_to_closest_shop", "distance_to_closest_transit")
  if (!all(required_columns %in% colnames(data))) {
    stop("The input data must include the required columns.")
  }
  
  # Convert to sf object for mapping
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Query OpenStreetMap for park polygons within the bounding box
  parks <- opq(bbox = bbox) %>%
    add_osm_feature(key = "leisure", value = "park") %>%
    osmdata_sf() %>%
    .$osm_polygons
  
  # Ensure parks are in the same CRS as the property data
  if (!is.null(parks)) {
    parks <- st_transform(parks, crs = st_crs(data_sf))
  }
  
  # Define a color palette for distance to closest park
  palette <- colorNumeric(
    palette = rev(brewer.pal(9, "Greens")),  # Green sequential color scale for distance to parks
    domain = data$distance_to_closest_park,
    na.color = "transparent" # should not be necessary
  )
  
  # Create Leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    
    
    # Add property points color-coded by distance to closest park
    addCircleMarkers(
      data = data_sf,
      radius = 5,
      color = ~palette(distance_to_closest_park),
      fillOpacity = 0.8,
      popup = ~paste(
        "<b>Address:</b>", address, "<br>",
        "<b>Street Name:</b>", street_name, "<br>",
        "<b>City:</b>", city, "<br>",
        "<b>State:</b>", state, "<br>",
        "<b>Postcode:</b>", postcode, "<br>",
        "<b>Price:</b>", price, "<br>",
        "<b>Bedrooms:</b>", bedroom_number, "<br>",
        "<b>Bathrooms:</b>", bathroom_number, "<br>",
        "<b>Living Space:</b>", living_space, "<br>",
        "<b>Land Space:</b>", land_space, "<br>",
        "<b>Distance to Closest Park:</b>", round(distance_to_closest_park, 2), "m<br>",
        "<b>Distance to Closest Shop:</b>", round(distance_to_closest_shop, 2), "m<br>",
        "<b>Distance to Closest Transit:</b>", round(distance_to_closest_transit, 2), "m<br>",
        "<b>Property URL:</b>", paste0("<a href='", property_url, "' target='_blank'>Link</a>")
      )
    ) %>%
    
    # Add park polygons in red
    addPolygons(
      data = parks,
      color = "red",
      weight = 1,
      fillOpacity = 0.5,
      popup = "Park"
    ) %>%
    
    # Add a legend for distance to closest park
    addLegend(
      position = "bottomright",
      pal = palette,
      values = data$distance_to_closest_park,
      title = "Distance to Closest Park (m)",
      opacity = 0.8
    )
  
  # Create directory for exporting the map
  export_path <- file.path(path, paste0("Maps/", city_name))
  dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define the filename dynamically based on the city name
  file_name <- paste0("DistanceToClosestParkMap_", city_name, ".html")
  save_path <- file.path(export_path, file_name)
  
  # Save the map as an HTML file
  saveWidget(map, save_path, selfcontained = TRUE)
  
  # Message to confirm export
  cat("Map exported successfully to:", save_path, "\n")
  
  # Return the map (optional for viewing)
  return(map)
}

# Create and export the map for Single_Chicago
create_and_export_park_distance_map(Single_Chicago, chicago_bbox)
# Create and export the map for Single_Houston
create_and_export_park_distance_map(Single_Houston, houston_bbox)


# Function to create and export Leaflet map based on distance to closest shop
create_and_export_shop_distance_map <- function(data, bbox, path = getwd()) {
  
  # Extract city name dynamically from the dataset name
  dataset_name <- deparse(substitute(data))  # Get the name of the dataset as a string
  city_name <- strsplit(dataset_name, "_")[[1]][2]  # Extract the second part (e.g., 'Houston' from 'Single_Houston')
  
  # Check if required columns exist
  required_columns <- c("longitude", "latitude", "distance_to_closest_shop", "property_url", 
                        "address", "street_name", "city", "state", "postcode", "price",
                        "bedroom_number", "bathroom_number", "living_space", "land_space",
                        "distance_to_closest_park", "distance_to_closest_transit")
  if (!all(required_columns %in% colnames(data))) {
    stop("The input data must include the required columns.")
  }
  
  # Convert to sf object for mapping
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Query OpenStreetMap for shop points within the bounding box
  shops <- opq(bbox = bbox) %>%
    add_osm_feature(key = "shop", value = c("supermarket", "convenience")) %>%
    osmdata_sf() %>%
    .$osm_points
  
  # Ensure shops are in the same CRS as the property data
  if (!is.null(shops)) {
    shops <- st_transform(shops, crs = st_crs(data_sf))
  }
  
  # Define a Viridis color palette for distance to closest shop
  palette <- colorNumeric(
    palette = "viridis",  # Viridis color scale
    domain = data$distance_to_closest_shop,
    na.color = "transparent"
  )
  
  # Create Leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    
    # Add shop points in red
    addCircleMarkers(
      data = shops,
      radius = 0.5,
      color = "red",
      fillOpacity = 0.2,
      popup = "Shop"
    ) %>%
    
    # Add property points color-coded by distance to closest shop
    addCircleMarkers(
      data = data_sf,
      radius = 5,
      color = ~palette(distance_to_closest_shop),
      fillOpacity = 0.8,
      popup = ~paste(
        "<b>Address:</b>", address, "<br>",
        "<b>Street Name:</b>", street_name, "<br>",
        "<b>City:</b>", city, "<br>",
        "<b>State:</b>", state, "<br>",
        "<b>Postcode:</b>", postcode, "<br>",
        "<b>Price:</b>", price, "<br>",
        "<b>Bedrooms:</b>", bedroom_number, "<br>",
        "<b>Bathrooms:</b>", bathroom_number, "<br>",
        "<b>Living Space:</b>", living_space, "<br>",
        "<b>Land Space:</b>", land_space, "<br>",
        "<b>Distance to Closest Shop:</b>", round(distance_to_closest_shop, 2), "m<br>",
        "<b>Distance to Closest Park:</b>", round(distance_to_closest_park, 2), "m<br>",
        "<b>Distance to Closest Transit:</b>", round(distance_to_closest_transit, 2), "m<br>",
        "<b>Property URL:</b>", paste0("<a href='", property_url, "' target='_blank'>Link</a>")
      )
    ) %>%
    
    
    # Add a legend for distance to closest shop
    addLegend(
      position = "bottomright",
      pal = palette,
      values = data$distance_to_closest_shop,
      title = "Distance to Closest Shop (m)",
      opacity = 0.8
    )
  
  # Create directory for exporting the map
  export_path <- file.path(path, paste0("Maps/", city_name))
  dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define the filename dynamically based on the city name
  file_name <- paste0("DistanceToClosestShopMap_", city_name, ".html")
  save_path <- file.path(export_path, file_name)
  
  # Save the map as an HTML file
  saveWidget(map, save_path, selfcontained = TRUE)
  
  # Message to confirm export
  cat("Map exported successfully to:", save_path, "\n")
  
  # Return the map (optional for viewing)
  return(map)
}

create_and_export_shop_distance_map(Single_Chicago, bbox = chicago_bbox)
create_and_export_shop_distance_map(Single_Houston, bbox = houston_bbox)


# Function to create and export Leaflet map based on distance to closest public transport stop
create_and_export_transit_distance_map <- function(data, bbox, path = getwd()) {
  
  # Extract city name dynamically from the dataset name
  dataset_name <- deparse(substitute(data))  # Get the name of the dataset as a string
  city_name <- strsplit(dataset_name, "_")[[1]][2]  # Extract the second part (e.g., 'Houston' from 'Single_Houston')
  
  # Check if required columns exist
  required_columns <- c("longitude", "latitude", "distance_to_closest_transit", "property_url", 
                        "address", "street_name", "city", "state", "postcode", "price",
                        "bedroom_number", "bathroom_number", "living_space", "land_space",
                        "distance_to_closest_shop", "distance_to_closest_park")
  if (!all(required_columns %in% colnames(data))) {
    stop("The input data must include the required columns.")
  }
  
  # Convert to sf object for mapping
  data_sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # Query for stop positions
  stop_positions <- opq(bbox = bbox) %>%
    add_osm_feature(key = "public_transport", value = "stop_position") %>%
    osmdata_sf() %>%
    .$osm_points
  
  # Query for bus stops
  bus_stops <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway", value = "bus_stop") %>%
    osmdata_sf() %>%
    .$osm_points
  
  # Combine stop positions and bus stops, since unable to get all the information in one request.
  if (!is.null(stop_positions) && !is.null(bus_stops)) {
    combined_stops <- st_sf(geometry = c(st_geometry(stop_positions), st_geometry(bus_stops)), 
                            crs = st_crs(stop_positions))
  } else if (!is.null(stop_positions)) {
    combined_stops <- stop_positions
  } else if (!is.null(bus_stops)) {
    combined_stops <- bus_stops
  } else {
    warning("No transit stops found in the specified area.")
    combined_stops <- NULL
  }
  
  # Ensure transit stops are in the same CRS as the property data
  if (!is.null(combined_stops)) {
    combined_stops <- st_transform(combined_stops, crs = st_crs(data_sf))
  }
  
  # Define a Viridis color palette for distance to closest transit
  palette <- colorNumeric(
    palette = "viridis",  # Viridis color scale
    domain = data$distance_to_closest_transit,
    na.color = "transparent"
  )
  
  # Create Leaflet map
  map <- leaflet() %>%
    addTiles() %>%
    
    # Add transit stops as red points
    addCircleMarkers(
      data = combined_stops,
      radius = 0.5,
      color = "red",
      fillOpacity = 0.5,
      popup = "Transit Stop"
    ) %>%
    
    # Add property points color-coded by distance to closest transit
    addCircleMarkers(
      data = data_sf,
      radius = 5,
      color = ~palette(distance_to_closest_transit),
      fillOpacity = 0.8,
      popup = ~paste(
        "<b>Address:</b>", address, "<br>",
        "<b>Street Name:</b>", street_name, "<br>",
        "<b>City:</b>", city, "<br>",
        "<b>State:</b>", state, "<br>",
        "<b>Postcode:</b>", postcode, "<br>",
        "<b>Price:</b>", price, "<br>",
        "<b>Bedrooms:</b>", bedroom_number, "<br>",
        "<b>Bathrooms:</b>", bathroom_number, "<br>",
        "<b>Living Space:</b>", living_space, "<br>",
        "<b>Land Space:</b>", land_space, "<br>",
        "<b>Distance to Closest Transit:</b>", round(distance_to_closest_transit, 2), "m<br>",
        "<b>Distance to Closest Park:</b>", round(distance_to_closest_park, 2), "m<br>",
        "<b>Distance to Closest Shop:</b>", round(distance_to_closest_shop, 2), "m<br>",
        "<b>Property URL:</b>", paste0("<a href='", property_url, "' target='_blank'>Link</a>")
      )
    ) %>%
    
    # Add a legend for distance to closest transit stop
    addLegend(
      position = "bottomright",
      pal = palette,
      values = data$distance_to_closest_transit,
      title = "Distance to Closest Transit (m)",
      opacity = 0.8
    )
  
  # Create directory for exporting the map
  export_path <- file.path(path, paste0("Maps/", city_name))
  dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define the filename dynamically based on the city name
  file_name <- paste0("DistanceToClosestTransitMap_", city_name, ".html")
  save_path <- file.path(export_path, file_name)
  
  # Save the map as an HTML file
  saveWidget(map, save_path, selfcontained = TRUE)
  
  # Message to confirm export
  cat("Map exported successfully to:", save_path, "\n")
  
  # Return the map (optional for viewing)
  return(map)
}


create_and_export_transit_distance_map(Single_Chicago, bbox = chicago_bbox)
create_and_export_transit_distance_map(Single_Houston, bbox = houston_bbox)


# Here we can see how much better the public transportation system is in Chicago compared to Houston

##############################################################################################################################
# Preparing the data for the regression analysis
##############################################################################################################################

# In the next step, we will transform the price variable using a logarithmic scale.
# This transformation is necessary because the price variable exhibits a right-skewed distribution, 
# which is not ideal for regression analysis. Log transformation helps to make the price distribution 
# closer to normal by compressing the range of high values and spreading out lower values.
# 
# A right-skewed distribution is characterized by most data points being concentrated on the left (lower prices),
# with a long tail extending to the right due to a few extremely large values (higher prices).
# Below is the histogram of the Price variable before applying the log transformation.
hist(Single_Chicago$price, 100)
hist(Single_Houston$price, 150)

Single_Chicago$log_price <- log(Single_Chicago$price)
Single_Houston$log_price <- log(Single_Houston$price)

# Additionally, to make the results of the regression model easier to interpret, 
# standardizing all variables, including the log-transformed price, can be helpful.
# 
# Standardization transforms the data to have a mean of 0 and a standard deviation of 1, 
# allowing for easier comparison of variables that have different units or scales.

# Function to keep specific columns and standardize them
standardize_data <- function(data, columns_to_keep) {
  # Subset the data to only include specified columns
  data <- data[, columns_to_keep, drop = FALSE]
  
  # Create a list to store transformation factors (mean and sd)
  transformation_factors <- list()
  
  # Standardize each column and store the transformation factors
  for (col in columns_to_keep) {
    col_mean <- mean(data[[col]], na.rm = TRUE)
    col_sd <- sd(data[[col]], na.rm = TRUE)
    
    # Standardize the column
    data[[col]] <- (data[[col]] - col_mean) / col_sd
    
    # Store transformation factors
    transformation_factors[[col]] <- list(mean = col_mean, sd = col_sd)
  }
  
  # Return the standardized data and transformation factors
  return(list(data = data, transformation_factors = transformation_factors))
}

# Define the columns to keep
columns_to_keep <- c("log_price", "bedroom_number", "bathroom_number", "living_space", 
                     "land_space", "distance_to_center", "distance_to_closest_park", 
                     "distance_to_closest_shop", "distance_to_closest_transit")

# Standardize Single_Chicago
chicago_result <- standardize_data(Single_Chicago, columns_to_keep)
Single_Chicago_standardized <- chicago_result$data  # Standardized data
chicago_factors <- chicago_result$transformation_factors  # Transformation factors

# Standardize Single_Houston
houston_result <- standardize_data(Single_Houston, columns_to_keep)
Single_Houston_standardized <- houston_result$data  # Standardized data
houston_factors <- houston_result$transformation_factors  # Transformation factors


##############################################################################################################################
# Hedonic Price Regression Analysis
##############################################################################################################################
# After an initial regression, it became apparent that including living space, bedroom_number, and bathroom_number 
# was unnecessary, as they appear to act as proxies for living space. 
# This was observed using the Variance Inflation Factor (VIF) after the regression and through a simple investigation 
# of the dataset's correlations. For example, in the Chicago dataset, bathroom_number and bedroom_number showed correlations 
# of 0.806 and 0.498 with living space, respectively.
cor(Single_Chicago_standardized)


# Final regression model (based on exploratory analysis and several regression runs).
# 
# The last variable, distance_to_center:living_space, is an interaction term that captures a more complex relationship:
# increasing living space by one unit near the city center adds more value compared to increasing it further away 
# from the city center. 
# 
# Whether this interaction term significantly improves the regression model's ability to explain the variance in the data 
# can be tested later using an ANOVA test.

chicago_fit_lm <- lm(log_price ~
                       living_space + 
                       land_space +
                       distance_to_center + 
                       distance_to_closest_park + 
                       distance_to_closest_shop +
                       distance_to_closest_transit + 
                       distance_to_center:living_space, 
                     data = Single_Chicago_standardized)

summary(chicago_fit_lm)

# Diagnostic Plots for the Regression
plot(chicago_fit_lm, which = 1:2) # Tukey-Anscombe Plot (nr1) & QQ-Plot (Nr2)
plot(chicago_fit_lm, which= 4) # cooks distance

# based on cook's distance and a commonly used threshold of 4/n, remove influential observations
cook_threshold <- 4 / nrow(Single_Chicago_standardized) # calculate the cook's threshold
Single_Chicago_standardized <- Single_Chicago_standardized[-c(which(cooks.distance(chicago_fit_lm) > cook_threshold)),] # delete observations that are above this thresholds


# Final Regression Model for Chicago
chicago_final_fit_lm <- lm(log_price ~
                       living_space + 
                       land_space +
                       distance_to_center + 
                       distance_to_closest_park + 
                       distance_to_closest_shop +
                       distance_to_closest_transit + 
                       distance_to_center:living_space, 
                     data = Single_Chicago_standardized)

summary(chicago_final_fit_lm)




# Houston
houston_fit_lm <- lm(log_price ~
                       living_space + 
                       land_space +
                       distance_to_center + 
                       distance_to_closest_park + 
                       distance_to_closest_shop +
                       distance_to_closest_transit + 
                       distance_to_center:living_space, 
                     data = Single_Houston_standardized)

summary(houston_fit_lm)

plot(houston_fit_lm, which = 1:2) # Tukey-Anscombe Plot (nr1) & QQ-Plot (Nr2)
plot(houston_fit_lm, which= 4) # cooks distance

# based on cook's distance and a commonly used threshold of 4/n, remove influential observations
cook_threshold <- 4 / nrow(Single_Houston_standardized) # calculate the cook's threshold
Single_Houston_standardized <- Single_Houston_standardized[-c(which(cooks.distance(houston_fit_lm) > cook_threshold)),] # delete observations that are above this thresholds


# Final Regression Model for Chicago
houston_final_fit_lm <- lm(log_price ~
                             living_space + 
                             land_space +
                             distance_to_center + 
                             distance_to_closest_park + 
                             distance_to_closest_shop +
                             distance_to_closest_transit + 
                             distance_to_center:living_space, 
                           data = Single_Houston_standardized)

summary(houston_final_fit_lm)

# Multicolinearity test, everything below 4 is fine Variance Inflation Factor (VIF)
VIF(chicago_final_fit_lm)
VIF(houston_final_fit_lm)



# Export the Regression results
export_regression <- function(model, path = getwd()) {
  
  # Extract model name dynamically as a string
  model_name <- deparse(substitute(model))
  
  # Extract city name based on model name format
  city_name <- strsplit(model_name, "_")[[1]][1]
  
  # Create directory for exporting the regression results
  export_path <- file.path(path, "/Regression", city_name)
  dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define the filename dynamically based on the city name
  file_name <- paste0(city_name, "_Final_Model", ".html")
  save_path <- file.path(export_path, file_name)
  
  # Export the regression output as an HTML file
  stargazer::stargazer(model,
                       type = "html",
                       title = paste0(city_name, " Final Model"),
                       out = save_path)
  
  message("Regression results exported to: ", save_path)
}


# Export the Regression Results into the Working Directory to further inspect them
export_regression(chicago_final_fit_lm)
export_regression(houston_final_fit_lm)



# Function to plot regression coefficients next to each other for a comparison and export as PDF
plot_regression_coefficients_to_pdf <- function(model1, model2, city1 = "City1", city2 = "City2", path = getwd()) {
  
  # --------------------------
  # 1) Tidy the Model Summaries into Data Frames
  # --------------------------
  model1_tidy <- tidy(model1) %>%
    mutate(city = city1)
  
  model2_tidy <- tidy(model2) %>%
    mutate(city = city2)
  
  # Combine the two data frames
  combined_df <- bind_rows(model1_tidy, model2_tidy)
  
  # --------------------------
  # 2) Create Significance Indicators and Labels
  # --------------------------
  combined_df <- combined_df %>%
    mutate(
      signif = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      label = paste0(round(estimate, 3), signif)
    )
  
  # --------------------------
  # 3) Prepare Plot Data (Including Intercepts)
  # --------------------------
  # Define the desired order of terms
  term_order <- c(
    "(Intercept)",
    "living_space",
    "land_space",
    "distance_to_center",
    "distance_to_closest_park",
    "distance_to_closest_shop",
    "distance_to_closest_transit",
    "living_space:distance_to_center"  # Interaction term, check if colname exists if no value shown
  )
  
  # Update the factor levels to match the order
  combined_df <- combined_df %>%
    mutate(term = factor(term, levels = term_order))
  
  # --------------------------
  # 4) Plotting with ggplot2
  # --------------------------
  plot <- ggplot(combined_df, aes(x = estimate, y = term, color = city)) +
    geom_point(position = position_dodge(width = 0.7), size = 3, shape = 16) +
    geom_errorbar(
      aes(xmin = estimate - std.error, xmax = estimate + std.error),
      width = 0.2, 
      position = position_dodge(width = 0.7)
    ) +
    geom_text(
      aes(label = label),
      position = position_dodge(width = 0.7),
      hjust = -0.4,  # Adjust horizontal position for labels
      size = 3
    ) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    scale_y_discrete(limits = rev(levels(combined_df$term))) +
    theme_minimal() +
    labs(
      title = paste("Coefficient Estimates for", city1, "and", city2),
      x = "Estimate",
      y = "Variable",
      color = "City",
      caption = "Error bars represent ±1 standard error"
    ) +
    theme(
      legend.position = "bottom",
      plot.margin = ggplot2::margin(10, 20, 10, 10)  # Adjust margins as needed
    ) +
    coord_cartesian(clip = "off")  # Allow text to expand beyond plot boundary
  
  # --------------------------
  # 5) Export Plot as PDF
  # --------------------------
  # Create directory for exporting the regression results
  export_path <- file.path(path, "Regression")
  dir.create(export_path, showWarnings = FALSE, recursive = TRUE)
  
  # Define the filename
  pdf_file_name <- paste0("Coefficient_Comparison_", city1, "_", city2, ".pdf")
  pdf_save_path <- file.path(export_path, pdf_file_name)
  
  # Export as PDF
  ggsave(filename = pdf_save_path, plot = plot, device = "pdf", width = 10, height = 8)
  
  # Message to confirm export
  cat("Plot exported successfully to:", pdf_save_path, "\n")
  
  # Return the plot for further use
  return(plot)
}


# Export the Coefficient Comparision Plot using the funciton
plot_regression_coefficients_to_pdf(chicago_final_fit_lm,houston_final_fit_lm, city1 = "Chicago", city2 = "Houston")



##############################################################################################################################
# Hedonic Price Regression Analysis
##############################################################################################################################
# 
# Shiny Dashboard for Predicting the Price of ones Dream Home
# 
# More help was needed from ChatGPT due to the complexity of this part. 
# However, the assistance provided was limited, and many refinements were required to ensure 
# that the values were correctly calculated and displayed.
# The process of reversing the transformations—taking the log of price and standardizing all variables—proved to be challenging.
# This version, however, should successfully handle the reversals while using the regression model as the prediction tool 
# for one's dream home.

# Helper function to standardize a given value
standardize_value <- function(value, mean_val, sd_val) {
  (value - mean_val) / sd_val
}

# Helper function to undo standardization
undo_standardize <- function(value, mean_val, sd_val) {
  value * sd_val + mean_val
}

# UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  titlePanel("Dream Home Price Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Adjust the sliders to input your dream home characteristics."),
      # Create sliders for the Inputs
      sliderInput("living_space", "Living Space (m²):", min = 20, max = 1000, value = 100, step = 5),
      sliderInput("land_space", "Land Space (m²):", min = 50, max = 5000, value = 250, step = 10),
      sliderInput("distance_to_center", "Distance to Center (m):", min = 0, max = 20000, value = 5000, step = 100),
      sliderInput("distance_to_closest_park", "Distance to Closest Park (m):", min = 0, max = 3000, value = 250, step = 50),
      sliderInput("distance_to_closest_shop", "Distance to Closest Shop (m):", min = 0, max = 3000, value = 400, step = 50),
      sliderInput("distance_to_closest_transit", "Distance to Closest Transit (m):", min = 0, max = 3000, value = 800, step = 50)
    ),
    
    mainPanel(
      h3("Predicted Prices for Your Dream Home"),
      fluidRow(
        column(6, h4("Chicago:"), textOutput("predicted_price_chicago")),
        column(6, h4("Houston:"), textOutput("predicted_price_houston"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load pre-trained models
  chicago_model <- lm(log_price ~ living_space + land_space + distance_to_center + 
                        distance_to_closest_park + distance_to_closest_shop + 
                        distance_to_closest_transit + distance_to_center:living_space, 
                      data = Single_Chicago_standardized)
  
  houston_model <- lm(log_price ~ living_space + land_space + distance_to_center + 
                        distance_to_closest_park + distance_to_closest_shop + 
                        distance_to_closest_transit + distance_to_center:living_space, 
                      data = Single_Houston_standardized)
  
  # Load scaling factors for standardization
  chicago_factors <- chicago_factors  # Replace with actual scaling parameters for Chicago
  houston_factors <- houston_factors  # Replace with actual scaling parameters for Houston
  
  # Reactive block for predictions
  prediction_reactive <- reactive({
    req(input$living_space, input$land_space, input$distance_to_center, 
        input$distance_to_closest_park, input$distance_to_closest_shop, input$distance_to_closest_transit)
    
    # Standardize inputs
    ls_chi <- standardize_value(input$living_space, chicago_factors$living_space$mean, chicago_factors$living_space$sd)
    land_chi <- standardize_value(input$land_space, chicago_factors$land_space$mean, chicago_factors$land_space$sd)
    center_chi <- standardize_value(input$distance_to_center, chicago_factors$distance_to_center$mean, chicago_factors$distance_to_center$sd)
    park_chi <- standardize_value(input$distance_to_closest_park, chicago_factors$distance_to_closest_park$mean, chicago_factors$distance_to_closest_park$sd)
    shop_chi <- standardize_value(input$distance_to_closest_shop, chicago_factors$distance_to_closest_shop$mean, chicago_factors$distance_to_closest_shop$sd)
    transit_chi <- standardize_value(input$distance_to_closest_transit, chicago_factors$distance_to_closest_transit$mean, chicago_factors$distance_to_closest_transit$sd)
    
    interaction_chi <- ls_chi * center_chi
    
    newdata_chi <- data.frame(
      living_space = ls_chi,
      land_space = land_chi,
      distance_to_center = center_chi,
      distance_to_closest_park = park_chi,
      distance_to_closest_shop = shop_chi,
      distance_to_closest_transit = transit_chi,
      `living_space:distance_to_center` = interaction_chi
    )
    
    # Predict log prices
    predicted_log_price_chi <- predict(chicago_model, newdata = newdata_chi)
    predicted_price_chi <- exp(undo_standardize(predicted_log_price_chi, chicago_factors$log_price$mean, chicago_factors$log_price$sd))
    
    # Repeat for Houston
    ls_hou <- standardize_value(input$living_space, houston_factors$living_space$mean, houston_factors$living_space$sd)
    land_hou <- standardize_value(input$land_space, houston_factors$land_space$mean, houston_factors$land_space$sd)
    center_hou <- standardize_value(input$distance_to_center, houston_factors$distance_to_center$mean, houston_factors$distance_to_center$sd)
    park_hou <- standardize_value(input$distance_to_closest_park, houston_factors$distance_to_closest_park$mean, houston_factors$distance_to_closest_park$sd)
    shop_hou <- standardize_value(input$distance_to_closest_shop, houston_factors$distance_to_closest_shop$mean, houston_factors$distance_to_closest_shop$sd)
    transit_hou <- standardize_value(input$distance_to_closest_transit, houston_factors$distance_to_closest_transit$mean, houston_factors$distance_to_closest_transit$sd)
    
    interaction_hou <- ls_hou * center_hou
    
    newdata_hou <- data.frame(
      living_space = ls_hou,
      land_space = land_hou,
      distance_to_center = center_hou,
      distance_to_closest_park = park_hou,
      distance_to_closest_shop = shop_hou,
      distance_to_closest_transit = transit_hou,
      `living_space:distance_to_center` = interaction_hou
    )
    
    predicted_log_price_hou <- predict(houston_model, newdata = newdata_hou)
    predicted_price_hou <- exp(undo_standardize(predicted_log_price_hou, houston_factors$log_price$mean, houston_factors$log_price$sd))
    
    list(chicago = predicted_price_chi, houston = predicted_price_hou)
  })
  
  # Display predictions
  output$predicted_price_chicago <- renderText({
    paste0("$", format(round(prediction_reactive()$chicago, 2), big.mark = ","))
  })
  
  output$predicted_price_houston <- renderText({
    paste0("$", format(round(prediction_reactive()$houston, 2), big.mark = ","))
  })
}

# Run the app
shinyApp(ui = ui, server = server)


# End of Code. Have fun experimenting with the sliders and calculating the price of your dream home!
# 
# Note: While this prediction model has some limitations, it provides reasonably good results 
# if the input values are kept within realistic ranges.
# 
# Also, check the folder you specified at the beginning. There should be some interesting outputs waiting for you, 
# including interactive maps, regression results, and other fascinating insights.
