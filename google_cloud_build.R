

#Sys.setenv("GCS_AUTH_FILE" = "punctuality-performance-app-7d24d6638adc.json")
library(googleCloudStorageR)
gcs_auth("punctuality-performance-app-7d24d6638adc.json")


library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(jsonlite)
library(httr)



# API Key hard coded
#api_key <- "46ef7ea8-4b01-48a3-8377-f621ae7c0b60"
api_key <- "ab78f260-9737-4847-9ee8-d663e76f23c9"


# Hard coded fields to keep (41 fields reduced to 12) for arrivals
fields_arrivals <- c("airline_iata", "airline_icao", "flight_iata", "flight_icao", "flight_number",
                     "dep_iata", "dep_icao", 
                     "arr_iata", "arr_icao", "arr_terminal", "arr_time", "arr_actual", "delayed", "duration")

# Hard coded Fields to keep for departures
fields_departures <- c("airline_iata", "airline_icao", "flight_iata", "flight_icao", "flight_number",
                       "dep_iata", "dep_icao", "dep_terminal", "dep_time", "dep_actual",
                       "arr_iata", "arr_icao", "delayed", "duration")



# # 2: Airport Lookup codes
# # Remove redundant fields with select command
airports <- read.csv("Data/airports.csv")

# # Example 3: Airline Lookup codes
# # Match airport code with airport name
airlines <- read.csv("Data/airlines.csv") %>% select(-X)




# core api function
get_airlabs_api_response <- function(key, parameter_name = "parameter1", parameter_value = "value1",
                                     parameter_name2 = "parameter2", parameter_value2 = "value2")  {
  url <- paste0("https://airlabs.co/api/v9/", key)
  params <- list(
    api_key = "ab78f260-9737-4847-9ee8-d663e76f23c9"
  )
  params[[parameter_name]] <- parameter_value
  params[[parameter_name2]] <- parameter_value2
  response <- httr::GET(url, query = params)
  api_response <- jsonlite::fromJSON(content(response, "text"))
  data <- as.data.frame(api_response$response)
  return(data)
}


get_live_flight_data <- function(flight_type, airport) {
  Sys.setenv(TZ = "Europe/London")
  current_time <- Sys.time()
  formatted_time <- format(current_time, "%Y-%m-%d %H:%M")
  if (flight_type == "departures") {
    data <- get_airlabs_api_response(key = "schedules", 
                                     parameter_name = "dep_iata", 
                                     parameter_value = airport) %>%
      group_by(arr_iata, dep_time) %>%
      mutate(cs_airline_iata = ifelse(is.na(cs_airline_iata), 
                                      first(cs_airline_iata[!is.na(cs_airline_iata)]), cs_airline_iata)) %>%
      mutate(row_to_keep = case_when(
        any(airline_iata == cs_airline_iata) ~ which(airline_iata == cs_airline_iata)[1],
        TRUE ~ 1
      )) %>%
      filter(row_number() == row_to_keep) %>%
      select(-row_to_keep) %>%
      mutate(dep_actual = ifelse(exists("dep_actual"), dep_actual, NA)) %>%
      mutate(dep_estimated = ifelse(exists("dep_estimated"), dep_estimated, NA)) %>%
      select(airline_iata, 
             dep_iata, dep_terminal, dep_time, dep_actual,
             dep_estimated, 
             arr_iata, 
             status,
             delayed) %>%
      mutate(dep_actual = ifelse(is.na(dep_actual) & dep_estimated < formatted_time,
                                 dep_estimated, dep_actual)) %>%
      filter(dep_time <= formatted_time)
    data$dep_actual <- as.POSIXct(data$dep_actual)
    data$dep_time <- as.POSIXct(data$dep_time)
    data$dep_estimated <- as.POSIXct(data$dep_estimated)
    data <- data %>%
      filter(!is.na(airline_iata)) %>%
      left_join(airports, by = c("arr_iata" = "iata_code")) %>%
      left_join(airlines, by = c("airline_iata" = "iata_code")) %>% 
      left_join(airports, by = c("dep_iata" = "iata_code")) %>%  
      # For categorising Domestic / International, use ifelse() for this process
      select(-c(country_code.x, country_code.y, name, icao_code)) %>%
      dplyr::rename("airport_name" = "name.x", "airline_name" = "name.y") %>%
      select(airline_name, airport_name, everything())
  } else if (flight_type == "arrivals") {
    data <- get_airlabs_api_response(key = "schedules", 
                                     parameter_name = "arr_iata", 
                                     parameter_value = airport) %>%
      
      group_by(dep_iata, arr_time) %>%
      mutate(cs_airline_iata = ifelse(is.na(cs_airline_iata), 
                                      first(cs_airline_iata[!is.na(cs_airline_iata)]), cs_airline_iata)) %>%
      mutate(row_to_keep = case_when(
        any(airline_iata == cs_airline_iata) ~ which(airline_iata == cs_airline_iata)[1],
        TRUE ~ 1
      )) %>%
      filter(row_number() == row_to_keep) %>%
      select(-row_to_keep) %>%
      mutate(arr_actual = ifelse(exists("arr_actual"), arr_actual, NA)) %>%
      mutate(arr_estimated = ifelse(exists("arr_estimated"), arr_estimated, NA)) %>%
      dplyr::select(airline_iata, 
                    dep_iata, dep_time, dep_time_utc, #dep_actual, 
                    arr_iata, arr_terminal, arr_time, arr_time_utc, arr_actual,
                    arr_estimated, status, delayed) %>%
      mutate(arr_actual = ifelse(is.na(arr_actual) & arr_estimated < formatted_time,
                                 arr_estimated, arr_actual)) %>%
      filter(arr_time <= formatted_time)
    data$arr_actual <- as.POSIXct(data$arr_actual)
    data$arr_time <- as.POSIXct(data$arr_time)
    data$arr_estimated <- as.POSIXct(data$arr_estimated)
    data$dep_time <- as.POSIXct(data$dep_time)
    data$dep_time_utc <- as.POSIXct(data$dep_time_utc)
    data$arr_time_utc <- as.POSIXct(data$arr_time_utc)
    data <- data %>%
      filter(!is.na(airline_iata)) %>%
      left_join(airports, by = c("dep_iata" = "iata_code")) %>%
      left_join(airlines, by = c("airline_iata" = "iata_code")) %>% 
      left_join(airports, by = c("arr_iata" = "iata_code")) %>%  
      # For categorising Domestic / International, use ifelse() for this process
      select(-c(country_code.x, country_code.y, name, icao_code)) %>%
      dplyr::rename("airport_name" = "name.x", "airline_name" = "name.y") %>%
      select(airline_name, airport_name, everything()) 
  } else {
    stop("Invalid flight type. Please specify 'departures' or 'arrivals'.")
  }
  test <- gcs_get_object(paste0(flight_type, "/combined_data_test.csv"), "jersey-otp")
  data_full <- data %>%
    rbind(test)
  print(nrow(data_full))
  return(data_full)
}


bucket_name <- "jersey-otp"

departures_dd <- get_live_flight_data("departures", "JER") 
arrivals_dd <- get_live_flight_data("arrivals", "JER")

#gcs_auth("Shiny/punctuality-performance-app-7d24d6638adc.json")
gcs_delete_object("departures/combined_data_test.csv", bucket_name)
gcs_delete_object("arrivals/combined_data_test.csv", bucket_name)
combined_data <- departures_dd %>% select(-file, -X)
gcs_upload(combined_data, bucket = bucket_name, name = "departures/combined_data_test.csv")
combined_data <- arrivals_dd %>% select(-file, -X)
gcs_upload(combined_data, bucket = bucket_name, name = "arrivals/combined_data_test.csv")

