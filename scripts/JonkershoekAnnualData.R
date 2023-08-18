# summarising Jonkershoek data
# March 2023
# This script summarises the Kent and Belfort recorder data 
# from the Jonkershoek catchments to annual data
# Matches the data with rainfall

# see the supplementary material in Slingsby et al. 2021
require(tidyverse)
require(lubridate)

# function to read the streamflow data
stream <- function(name) {
   if(require(tidyverse)==F) require(tidyverse)
   if(require(lubridate)==F) require(lubridate)
# read in the data and format date and time  
    data <- read_csv(name) %>%
      mutate(DateTime = ymd_hms(Date),
             Date = date(DateTime))
  return(data)
}    

# function to read rainfall data
rain <- function(name) {
  # read in the data and format date and time  
  data <- read_csv(paste0("rainfall/",name)) %>%
    mutate(DateTime = ymd_hms(Date),
           Date = date(DateTime))
  return(data)
}

# merge two datasets by date
merge_data <- function(data1, data2, by_in = "Date") {
  # assumes both datasets have a column called "Date"
  out_data <- left_join(data1,data2, by = by_in)
  return(out_data)
}

# summarise by year
summarise_by_year <- function(data) {
    data <- data %>%
      group_by(Year = year(Date)) %>%
      summarise(Annual_value = sum(Value, na.rm =T))
  
}

# import and merge multiple rainfall stations
rain_import <- function(rain_station_list) {
  # loop through list of stations
  for (j in 1:length(rain_station_list)) {
    # open the rainfall data file(s)
    file_r <- rain(rainfiles[grep(rain_station_list[j],rainfiles)])
    # merge rainfall data files
    if (j == 1) {
      rain_t <- file_r
      #browser()
    } else {
      rain_t <- left_join(rain_t, file_r, by= "Date")
    }
  }
  return(rain_t)
}

# function to summarise across columns of rainfall data and calculate annual
rain_summary <- function(rain_input) {
  # summarise across the numeric columns (rainfall)
  rain_input <- rain_input %>%
    summarise(Value = apply(rain_input[,2:ncol(rain_input)],1,mean))
  browser()
  # calculate annual rainfall
  rain_annual <- summarise_by_year(rain_input) %>%
    rename("annual_rain" = "annual_value")
  
  return(rain_annual)
}


# find name from site
find_site_name <- function(name) {
  # using Kent stream recorder files
  end <- str_locate(name,"_K")
  site <- substr(name,0,(end[1]-1))
  return(site)
}


# Overview of raingauges
rain_gauge <- list("Bosboukloof" = c("5A","5B","11A","11B","25A","25B"),
                   "Biesievlei" = c("12A","12B"),
                   "Tierkloof" = c("6B", "9B", "13B"),
                   "Lambrechtsbos-B" = c("15A", "15B"),
                   "Lambrechtsbos-A" = c("10A", "10B"),
                   "Langrivier" = c("4A", "4B", "8A", "8B", '14A', "14B"))#,
                   #"swartboskloof" = c("26B", "27B", "28B", "29B", 
                   #"30B", "31B", "32B"))

# treatment years
treatment <- list("Bosboukloof" = 1940,
                   "Biesievlei" = 1948,
                   "Tierkloof" = 1956,
                   "Lambrechtsbos-B" = 1964,
                   "Lambrechtsbos-A" = 1974,
                   "Langrivier" = NA)#,
#"swartboskloof" = c("26B", "27B", "28B", "29B", 
#"30B", "31B", "32B"))


# find all the flow data
Kentfilelist <- list.files(pattern = "Kent")
Belfortfilelist <- list.files(pattern="Belfort")

# find all rainfall data
rainfiles <- list.files(path = "rainfall")

# empty storage list
all_data <- list()

# run a loop through the Kent files
for (i in 1:length(Kentfilelist)) {
  # load the Kent recorder file
  Kent_data <- stream(Kentfilelist[i])
  # load the Belfort recorder file
  Belfort_data <- stream(Belfortfilelist[i])
  # merge both files
  stream_data <- merge_data(Belfort_data, Kent_data, by_in = c("Date", "Value"))
  # calculate the annual flows
  stream_annual <- summarise_by_year(stream_data)
  # find the name of the site
  site <- find_site_name(Kentfilelist[i])
  
  # find raingauges
  rain_g <- rain_gauge[grep(site,names(rain_gauge))][[1]]
  
  # read in and merge
  rain_data <- rain_import(rain_g)
  
  # summarise rainfall to annual
  rain_data_annual <- rain_summary(rain_data)
  
  # merge rainfall and flow
  combined_data <- left_join(stream_annual, rain_data_annual) %>%
  # add treatment column and name column
    mutate(treatment = ifelse(Year  < treatment[[site]], 0, 1),
           site = site)
  # save into a list
  all_data[[i]] <- combined_data
  
}

# bind rows for list and save
output <- bind_rows(all_data)
write_csv(output, "annualdata/Jonkershoek_annual_summary.csv")
