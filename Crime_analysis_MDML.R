# Authors: Andrea Hassler, Jui Arvind Nerurkar, Emmanuel Bazov
#LIBRARY

library(xml2)
library(tidyverse)
library(lubridate)



get_site_content <- function(url){
  require(httr)
  # get the site response
  #one line to get all the information from the site
  #some sites are password protected - examine the content to check where successful in obtaining information 
  response <- httr::GET( url )
  # extract the content
  content <- httr::content( x = response, as = 'text', encoding = 'utf-8' )
  # return 
  return( content )
}

content_to_parsed_html <- function( content ){
  require( xml2 )
  # parse the html with xml2
  parsed_html <- read_html(content)
  # return
  return( parsed_html )
}


extract_all_crimes_dates <- function( parsed_html ){
  require( rvest )
  # get all table cells of the appropriate class 
  all_crimes <- rvest::html_nodes( x = parsed_html, xpath = './/td[contains(@class,"field-name")]' )
  all_dates <- rvest::html_nodes( x = parsed_html, xpath = './/td[contains(@class,"crime-date")]' )
  # extract the text from the cell
  crimes <- rvest::html_text( all_crimes )
  crimes <- trimws(crimes)
  dates <- rvest::html_text( all_dates )
  dates <- trimws(dates)
  #convert time to 24 hour format and select hour from time
  strip_dates <- strptime(dates, "%m/%d/%y - %I:%M %p")
  hours <- format(as.POSIXct(strip_dates) ,format = "%H")
  return(tibble(Crime = crimes, Hours = hours))
}


extract_all_dates <- function( parsed_html ){
  require( rvest )
  # get all table cells of the appropriate class 
  all_dates <- rvest::html_nodes( x = parsed_html, xpath = './/td[contains(@class,"crime-date")]' )
  # extract the text from the cell
  dates <- rvest::html_text( all_dates )
  sep_date_time <- dmy_hms(dates)
  return( dates )
}


######
# A) [30 pts] Create a tibble called crime.data which contains all crimes in all neighborhoods

# Create empty list
crime.data <- list()

# Loop through neighborhoods and add output tibble as element of list
for (i in 1:length(neighborhoods)) {
  crime.data[[i]] <- 
    get_site_content(paste0('https://www.universalhub.com/crime/', neighborhoods[i], '.html' )) %>% 
    content_to_parsed_html() %>%
    extract_all_crimes_dates() %>% 
    mutate(neighborhood = neighborhoods_proper[i])
}

# Combine listed tibbles into one tibble
crime.data <- do.call(rbind, crime.data)

# Add Dorcester page 2
dorchest_page2 <- get_site_content('https://www.universalhub.com/crime/dorchester.html?page=1') %>%
  content_to_parsed_html() %>% 
  extract_all_crimes_dates() %>%
  mutate(neighborhood = 'Dorchester')

# Add Mission Hill back in
mission_hill <- get_site_content('https://www.universalhub.com/crime/mission-hill') %>%
  content_to_parsed_html() %>% 
  extract_all_crimes_dates() %>% 
  mutate(neighborhood = 'Mission Hill')

# Bind all into full dataset
crime.data <- bind_rows(crime.data, dorchest_page2, mission_hill)



#######
# B) [5 pts] What are the five most common crime types (aggregated across neighborhoods and hours), and how many of each such crime occurred? Be alert for misspellings!


# Correct misspellings
crime.data <- crime.data %>% mutate(crime = tolower(crime))
crime.data$crime <- recode(crime.data$crime,'oui'='dui','assaul'='assault',
                           'oui crash'='dui crash' ,'shoot'='shooting', 
                           'shotting'='shooting', 'shote'='shooting', 
                           'shoting'='shooting', 'shootin'='shooting')

# Top 5 crimes
(top5 <- crime.data %>% count(crime) %>%  arrange(desc(n)) %>% slice(1:5))


######
# C) [5 pts] Make a plot of the total number of crimes (aggregated across neighborhoods and crime types) by hour. Write a few sentences about the pattern you observe.

# Plot of crimes by hour aggregated across neighborhoods and crime type 
crime.data %>% filter(crime %in% c(top5$crime)) %>% group_by(hour) %>% count() %>%
  ggplot(aes(hour, n)) +
  geom_line() +
  labs(x = 'Hour', y = 'Total Crimes', title = 'Total Crimes by Hour')

######
# D) [5 pts] Restrict to the five most common crime types, and plot the total number of crimes (aggregated across neighborhoods) for each crime type by hour (i.e., your plot should have five lines). Write a few sentences about the pattern you observe.

crime.data %>% filter(crime %in% c(top5$crime)) %>% group_by(hour, crime) %>% count() %>%
  ggplot(aes(hour, n, group = crime, colour = crime)) +
  geom_line() +
  labs(x = 'Hour', y = 'Total Crimes', title = 'Total Crimes by Hour')


######
# E) [5 pts] Restrict to just the neighborhoods of Dorchester and Downtown, and plot the total number of crimes (aggregated across crime types (include all crime types, not just the top five)) for each of the two neighborhoods by hour (i.e., your plot should have two lines). Write a few sentences about the pattern you observe.

crime.data %>% filter(neighborhood %in% c('Downtown', 'Dorchester')) %>% 
  group_by(hour, neighborhood) %>% count() %>%
  ggplot(aes(hour, n, group = neighborhood, colour = neighborhood)) +
  geom_line() +
  labs(x = 'Hour', y = 'Total Crimes', title = 'Total Crimes by Hour')

######
