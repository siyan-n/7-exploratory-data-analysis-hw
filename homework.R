library(tidyverse)
library(DataExplorer)
library(rstatix)

stations <- c("KCLT", "KCQT", "KHOU", "KIND", "KJAX", "KMDW", "KNYC", "KPHL", "KPHX", "KSEA")
cities <- c("Charlotte", "Los Angeles", "Houston", "Indianapolis", "Jacksonville", 
            "Chicago", "New York City", "Philadelphia", "Phoenix", "Seattle")

# QUESTION 1
#> The data files are in the directory 'us-weather-history'
#> Write a function that takes each station abbreviation and reads
#> the data file and adds the station name in a column
#> Make sure the date column is a date
#> The function should return a tibble
#> Call the function "read_weather" 
#> Check by reading/glimpsing a single station's file

library(here)
here()

data.list <- list.files(
  path = 'us-weather-history',
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = FALSE   
) 

read_weather <- function(x){
  read_csv(paste0("us-weather-history/", x, ".csv")) %>% 
    mutate(
      station = x, 
      date = as.Date(date)
    )
}

# QUESTION 2
#> Use map() and your new function to read in all 10 stations
#> Note that because map_dfr() has been superseded, and map() does not automatically bind rows, 
#> you will need to do so in the code with bind_rows()
#> Save the resulting dataset to "ds"

ds <- map(stations, read_weather)%>%
  bind_rows()

# QUESTION 3
#> Make a factor called "city" based on the station variable
#> (station should be the level and city should be the label)
#> Use rstatix::freq_tables() to check that there are 365 days of data for each city 
#> and save the results to a new tibble
#> Write a logical statement that would trigger a warning if any city did not have 365 days

ds <- ds %>%
  mutate(city = factor(station,
                       levels = stations,
                       labels = cities))

days_city <- ds %>%
  group_by(city) %>%
  summarise(days_c = n_distinct(date))
  if (any(days_city$days_c < 365)) {
    print(str_glue("Oh no!: {(days_city$city)} has less than 365 days"))
  } 

# QUESTION 4
#> In question 3, all of the cities do have 365 days of data, so how would you know
#> if your check worked? Create a test to mess up the dataset to make sure that your
#> check does throw an error if the conditions aren't met

days_city_test <- days_city %>%
  mutate(days_c = days_c - sample(0:3, n(), replace = TRUE))
  if (any(days_city_test$days_c < 365)) {
  print(str_glue("Oh no!: {(days_city_test$city)} has less than 365 days"))
} 

# QUESTION 5
#> At this point, your data should look like the "compiled_data.csv" file
#> in data-clean. If it isn't, read in that file to use for the remaining
#> questions so that you have the right data to work with.

compiled_data <- read_csv("data-clean/compiled_data.csv")

# QUESTION 6
#> Find a function that lets you pull out the month from the date 
#> and make "month" a factor as a new variable in ds

ds <- ds %>%
  mutate(month = factor(sub("^\\d{4}-(\\d{2})-.*", "\\1", date)))

# QUESTION 7
#> Use rstatix to check if there are any outliers for actual_mean_temp.
#> Save the outliers to a new tibble.
#> In a piped statement, filter out those outliers from ds and then use
#> get_summary_stats() to get the mean and SE for actual_mean_temp
#> by month.

actual_mean_temp_outliers <- ds %>% 
  rstatix::identify_outliers(actual_mean_temp)
actual_mean_temp_outliers

ds %>%
  filter(!paste(city, station, date) %in% 
           paste(actual_mean_temp_outliers$city, 
                 actual_mean_temp_outliers$station, 
                 actual_mean_temp_outliers$date)) %>% 
  group_by(month) %>%
  rstatix::get_summary_stats(actual_mean_temp, type = "mean_se")

# QUESTION 8
#> Use the Data Explorer package to plot boxplots of all of the numeric variables in the dataset
#> grouped by city, then do the same thing grouped by month. 
#> Finally, use plot_correlation to investigate correlations between the continuous variables only
#> Check the documentation for plot_correlation for an easy way to do this



# QUESTION 9
#> Create a scatterplot of actual_mean_temp (y axis) by date (x axis)
#> Use facet_wrap to make a separate plot for each city (3 columns)
#> Make the points different colors according to month


# QUESTION 10
#> Write a function that takes the dataset and the abbreviate month as arguments
#> and creates a scatter and line plot of actual temperature (y axis) by date (x axis)
#> filtered for that particular month.
#> Your ggplot will need both geom_line() and geom_scatter().
#> Use the ggtitle() function to add the month as a title
#> The function should save the plot as "eda/month_name.png"
#> The eda folder has an example of what each plot should look like
#> Call the function in a map or loop to generate graphs for each month


