# 1. Load libraries (install if necessary - install.packages("pkg name")) ---------
library(tidyverse)       # Data management
library(terra)           # Raster data management
library(stars)           # Raster data management
library(tmap)            # Mapping
library(rgeos)           # Vector data management
library(meteospain)      # Retrieve AEMET and METEOCAT data
library(keyring)         # API connection environment
library(sf)              # Tidy vector data management
library(rnaturalearth)   # Vector data repo
library(meteoland)       # Meteo interpolations
library(lubridate)       # Dates management
library(RCurl)           # GitHub

# Define variables
first_day <- "2011-01-01"
last_day <- "2013-12-31"
temporal_resol <- "daily" # "daily" or "monthly"
resol <- 1000
save_path <- setwd("path to working dir")

# At first time set API keys using keyring pkg.
# install.packages('keyring')
# library(keyring)
# key_set('meteocat') # A prompt asking for the secret (the API Key) will appear. To obtain the Meteocat API Key, please visit https://apidocs.meteocat.gencat.cat/ 
# key_set('aemet') # A prompt asking for the secret (the API Key) will appear. To obtain the AEMET API Key, please visit https://opendata.aemet.es/centrodedescargas/inicio


# Select variables for Output interpolated raster data: "MeanTemperature",  "MinTemperature",  "MaxTemperature",  "Precipitation",  "MeanRelativeHumidity",  
# "MinRelativeHumidity",  "MaxRelativeHumidity",  "Radiation",  "WindSpeed",  "PET"
variables_raster <- c(
  "MeanTemperature",
  "MinTemperature",
  "MaxTemperature",
  "Precipitation",
  "MeanRelativeHumidity",
  "MinRelativeHumidity",
  "MaxRelativeHumidity",
  "Radiation",
  "WindSpeed",
  "PET"
)

# Group of variables to interpolate - select variables: "Precipitation","Temperature",
# "RelativeHumidity", "Radiation","Wind"
variables_interpolator <- c(
  "Precipitation",
  "Temperature",
  "RelativeHumidity",
  "Radiation",
  "Wind"
)

# Buffer for define station search s radius (in meters)
buffer_stations <- 50000

# Buffer for interpolation output (in meters)
buffer_interpolation <- 500

# Load AOI (Area of Interest)
AOI <- st_read("path_to_AOI") %>% 
  st_transform(st_crs(4326)) %>% 
  summarise()

# 2. Prepare AOIs --------------------------------------------------

# Create AOI for stations using buffer
AOI_stations <- AOI %>% st_buffer(buffer_stations)

# Create AOI for interpolation using buffer
AOI_interpolation <- AOI %>% st_buffer(buffer_interpolation)

# Load and prepare topo data for raster interpolation
url <- "https://github.com/pereGelabert/MDT200_spain/raw/master/MDT200_ETRS89_HU30_Spain.tif"
download.file(url = url,destfile = "./topo.tif",, mode = "wb")
topo <- rast("./topo.tif")
topo <- crop(topo, AOI_interpolation %>% st_transform(st_crs(topo))) %>%
  mask(., AOI_interpolation %>% st_transform(st_crs(topo))) %>% 
  aggregate(., resol / res(.)[1]) %>%
  project(., crs("epsg:4326"))
topo2 <- terrain(topo, c("aspect", "slope"))

# Prepare topo data for raster interpolation
topo <- c(topo, topo2) 
names(topo) <- c("elevation", "aspect", "slope")
topo_data_srtm <- c(
  st_as_stars(topo[[1]]),
  st_as_stars(topo[[2]]),
  st_as_stars(topo[[3]])
)
names(topo_data_srtm) <- c("elevation", "aspect", "slope")

# 3. Available Stations ---------------------------------------------------

# Set API request options for AEMET
AEMET_api_options <- api_options <- aemet_options(
  resolution = 'daily',
  start_date = as.Date('2017-01-01'),
  end_date = today(),
  api_key = key_get('aemet')
)

# Set API request options for METEOCAT
METEOCAT_api_options <- api_options <- aemet_options(
  resolution = 'daily',
  start_date = as.Date('2017-01-01'),
  end_date = today(),
  api_key = key_get('meteocat')
)

# Retrieve AEMET stations location and info
AEMET_stations <- get_stations_info_from('aemet', AEMET_api_options)
# Correct coordinates (Note: Developers have been notified to incorporate the changes in the next version)
AEMET_stations <- AEMET_stations

# Retrieve METEOCAT stations location and info
METEOCAT_stations <- get_stations_info_from('meteocat', METEOCAT_api_options)

# Merge stations info
stations.merge <- bind_rows(AEMET_stations, METEOCAT_stations) %>% 
  st_crop(xmin = -10, xmax = 5, ymin = 35, ymax = 44) # Merge and crop to Iberian Peninsula extent

# 4. Stations selection---------------------------------------------------------------

# Select stations within the AOI
stations.sel <- AOI_stations %>% 
  st_intersection(stations.merge)

## Mapping selected stations-----------------------------------------------------------
ggplot(stations.sel) + 
  geom_sf(data = stations.merge, color = "grey", size = 1) +
  geom_sf(data = st_as_sf(AOI), fill = "brown", alpha = 0.2) +
  geom_sf(data = st_as_sf(AOI_stations), fill = "brown", alpha = 0.2) +
  geom_sf(aes(pch = service), color = "blue", size = 1) + 
  geom_sf(data = ne_countries(scale = 10, returnclass = "sf") %>% 
            st_cast("MULTILINESTRING") %>% 
            st_crop(xmin = -10, xmax = 5, ymin = 35, ymax = 44),
          colour = "grey50", size = 0.2) +
  coord_sf(expand = FALSE) +
  theme_void()

# 5. Create a Monthly dates df to iterate dates and request/retrieve data ---------

# Generate monthly dates for iteration
firstday <- seq(as.Date(first_day), as.Date(last_day), by = "month")
firstday <- firstday[1:length(firstday) - 1]
lastday <- seq(as.Date(first_day), as.Date(last_day), by = "month") - 1
lastday <- lastday[2:length(lastday)]

# Create a monthly dataframe
monthly_df <- tibble(firstday = firstday, lastday = lastday)
AOI_MeteoData.list <- list()

# Iterate through monthly dates and request/retrieve data for interpolation
for (i in 1:nrow(monthly_df)) {
  print(i)
  
  # Set AEMET API options for the AOI
  API_options_AOI_AEMET <- aemet_options(
    resolution = "daily",
    start_date = as.Date(monthly_df[i, 1] %>% pull()),
    end_date = as.Date(monthly_df[i, 2] %>% pull()),
    api_key = key_get('aemet'),
    stations = as.character(stations.sel %>% as_tibble() %>%
                              dplyr::select(station_id) %>%
                              pull())
  )
  
  # Set METEOCAT API options for the AOI
  API_options_AOI_meteocat <- meteocat_options(
    resolution = "daily",
    start_date = as.Date(monthly_df[i, 1] %>% pull()),
    api_key = key_get('meteocat'),
    stations = as.character(stations.sel %>% as_tibble() %>%
                              dplyr::select(station_id) %>%
                              pull())
  )
  
  # Retrieve AEMET data
  AOI_MeteoData_AEMET <- get_meteo_from('aemet', options = API_options_AOI_AEMET) %>% 
    meteospain2meteoland()
  
  # Retrieve METEOCAT data
  AOI_MeteoData_meteocat <- get_meteo_from('meteocat', options = API_options_AOI_meteocat) %>%
    meteospain2meteoland()
  
  # Merge meteo data
  AOI_MeteoData <- bind_rows(AOI_MeteoData_AEMET, AOI_MeteoData_meteocat)
  
  # Add topo metrics to station data
  stations_topo <- stations.sel %>% 
    bind_cols(terra::project(topo2, crs(AOI_MeteoData)) %>% 
                terra::extract(vect(stations.sel))) %>% 
    as_tibble() %>% 
    dplyr::select(station_id, slope, aspect)
  
  # Translate meteo data from meteospain to meteoland
  AOI_MeteoData <- AOI_MeteoData %>% 
    units::drop_units() %>% 
    left_join(stations_topo, by = c("stationID" = "station_id"))
  
  # Create and calibrate the interpolator object
  interpolator <- AOI_MeteoData |> 
    create_meteo_interpolator() |> 
    interpolator_calibration(
      variable = "MinTemperature",
      N_seq = c(5, 20),
      alpha_seq = c(1, 10),
      update_interpolation_params = TRUE
    ) |> 
    interpolator_calibration(
      variable = "MaxTemperature",
      N_seq = c(5, 20),
      alpha_seq = c(1, 10),
      update_interpolation_params = TRUE
    ) |> 
    interpolator_calibration(
      variable = "Precipitation",
      N_seq = c(5, 20),
      alpha_seq = c(1, 10),
      update_interpolation_params = TRUE
    )
  
  # Interpolate data
  rast_interpolated <- topo_data_srtm |> 
    interpolate_data(interpolator, variables = variables_interpolator)
  
  # Temporal aggregation
  if (temporal_resol == "monthly") {
    means <- rast_interpolated |> 
      summarise_interpolated_data(
        fun = "mean",
        frequency = "month"
      )
    sums <- rast_interpolated |> 
      summarise_interpolated_data(
        fun = "sum",
        frequency = "month"
      )
    
    for (a in variables_raster) {
      if (a != "Precipitation") {
        rs <- as(means[a,,,], "Raster")
      } else {
        rs <- as(sums[a,,,], "Raster")
      }
      
      names(rs) <- paste0("prec - ", as.Date(monthly_df[i, 1] %>% pull()))
      path.pre <- paste0("./Outputs/")
      dir.create(path.pre)
      path <- paste0(path.pre, "/", a)
      dir.create(path)
      writeRaster(
        rs,
        paste0(path, "/", a, "-", monthly_df[i, 1] %>% pull(), ".tif")
      )
    }
  } else {
    for (a in variables_raster) {
      rs <- as(rast_interpolated[a,,,], "Raster")
      names(rs) <- paste0(
        a,
        "-",
        seq.Date(
          as.Date(monthly_df[i, 1] %>% pull()),
          as.Date(monthly_df[i, 2] %>% pull()),
          by = "day"
        )
      )
      path.pre <- paste0("./Outputs/")
      dir.create(path.pre)
      path <- paste0(path.pre, "/", a)
      dir.create(path)
      writeRaster(
        rs,
        paste0(path, "/", a, "-", monthly_df[i, 1] %>% pull(), ".tif")
      )
    }
  }
}
