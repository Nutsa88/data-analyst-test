
                                ## 1 - Packages ##

library(tidyverse)
library(lubridate)

                         ## 2 - Data Import and Cleaning ##

## Path to files/data sets to load
path_data <- list.files("data", full.names = T)

## Load data sets into list
list_data_sets <- map(path_data, read_csv)

## Set names to each data set in list according to the data file name
## If path string contains 'details', we name data set 'details' in the list
## Same for download and upload
names(list_data_sets) <- map_chr(
  ## Path string
  path_data,
  ~ifelse(
    ## If path to data set contains 'details'
    .x %>% str_detect("details"),
    ## give 'details'
    "details",
    ifelse(
      ## If path to data set contains 'download'
      .x %>% str_detect("download"),
      ## give 'download'
      "download",
      ## If path to data set does not contain 'details' or 'download'
      "upload"
    )
  )
)

## Check the structure of the data
list_data_sets %>% str ## Column classes look good and compatible among
## data sets, example - person_id is numeric in each data set

## Filter, calculate average measured speeds by id and join data sets

## Firstly, get selected ids - people form 'Samsville' and 'Databury'
selected_id <- list_data_sets$details %>%
  filter(city %>% str_detect("Samsville|Databury")) %>%
  pull(person_id)

## Check for duplicates in selected person_id
## Did anyone switch provider and end up twice? Duplicate by error? 
## We have 289 ids and no id is duplicated
selected_id %>% duplicated %>% any

## Filter download/upload data for selected period (January 2021) and cities 
list_data_sets[c("download", "upload")] <- c("download", "upload") %>%
  map(
    ## Select download/upload data set from data list
    ~list_data_sets[[.x]] %>% 
      filter( 
        ## Filter for January 2021
        time_of_measurement %>% str_detect("2021-01"), 
        ## Filter for ids from 'Samsville' and 'Databury'
        person_id %in% selected_id 
      )
  )

## Calculate monthly average speed for January 2021 per person_id
## Filter for successful measurements
## Create new data sets in list, named download_avg, upload_avg
list_data_sets[c("download_avg", "upload_avg")] <- map2(
  ## Arguments for map2
  c("download", "upload"),
  c("measured_download_speed_in_Mbps","measured_upload_speed_in_Mbps"),
  ## User-defined function
  function(.x, .y) {
    ## Turn measured speed column names into symbols
    ## in order to use in function 'summarise' later
    measured_speed = sym(.y)
    
    list_data_sets[[.x]] %>%
      ## Filter for successful measurements
      filter(did_test_complete_successfully == T) %>%
      ## Drop redundant columns
      ## Data is for January 2021 - time column is not needed for January average
      ## Test completed for all filtered ids, so this column is also redundant
      select(-did_test_complete_successfully, -time_of_measurement) %>%
      ## Grouping by person_id to calculate average speed per id
      group_by(person_id) %>%
      ## Calculating average detected speed from January 2021
      ## Naming column of averages according to map input
      ## Also include total obs count per id
      summarise(!!paste("average", .x, "speed", sep = "_") := !!measured_speed %>% mean,
                !!paste(.x, "total_obs", sep = "_") := n())
  }
)

## Calculating 60th percentile of daily average speeds for January 2021
## Also filtering for successful measurements
list_data_sets[c("download60", "upload60")] <- map2(
  ## Arguments for map2
  c("download", "upload"),
  c("measured_download_speed_in_Mbps","measured_upload_speed_in_Mbps"),
  ## User-defined function
  function(.x , .y) {
    ## Turn measured speed column names into symbols
    ## in order to use in function 'summarise' later
    measured_speed = sym(.y)
    
    list_data_sets[[.x]] %>%
      ## Filter for successful measurements
      filter(did_test_complete_successfully == T) %>%
      ## Add day
      mutate(day = time_of_measurement %>% day) %>%
      ## Group by id and day, then calculate daily average
      group_by(person_id, day) %>%
      summarise(daily_speed = !!measured_speed %>% mean) %>%
      ## Calculate 60th percentile within month
      ## drop grouping on day - we calculate percentile for whole month
      group_by(person_id) %>%
      summarise(!!paste("60perc", .x, sep = "_") := daily_speed %>% quantile(0.6))
  }
)

## Joining daily download/upload, 60th percentile and personal information data sets
## Naming data_january as this data is for January only
data_january <- list_data_sets$download_avg %>%
  full_join(list_data_sets$upload_avg) %>%
  full_join(list_data_sets$download60) %>%
  full_join(list_data_sets$upload60) %>%
  ## Using left_join for details data set because we did not filter it for cities
  left_join(list_data_sets$details) %>%
  ## Proper order
  select(person_id, city, type_of_broadband_connection, name_of_isp, 
         average_download_speed, average_upload_speed,
         `60perc_download`, `60perc_upload`,
         download_total_obs, upload_total_obs)

                         
                            ## 3 - Data Quality ##

## Summarising data for January
## We get no NA values when filtered with successful measurements
## This means each id had at least one data point per day, we did not drop NA's 
## in the above code
data_january %>% summary

## We had no duplicates or missing values when we filtered the data 
## by January 2021, test success and selected cities.
## Lets visualise data.

## First plot histograms of average speed in January to detect outliers
## Plot for each city for separate isp and connection types

## Plot for downloads
## Here we see that for ADSL there are speeds over 100, which could be 
## mislabeled/outliers.
## Most data is below 50 for ADSL
## For Fibre, some data are below 50, look like outliers
## No obvious outliers for VDSL within each city
## However, VDSL speed in Samsville is about twice on average compared to Databury
## This may be problematic
plot_outliers_down <- data_january %>%
  ggplot(
    aes(
      x = average_download_speed, fill = name_of_isp
    )
  ) +
  geom_histogram(position = "dodge") +
  labs(title = "Detecting outliers in downloads",
       subtitle =  "Histogram for average download speeds",
       fill = NULL,
       x = "\nAverage download speed\n",
       y = "\nCount\n") + 
  scale_fill_manual(values = c("#0057b7", "#FFDD00")) +
  facet_wrap(~city + type_of_broadband_connection, scales = "free")  +
  theme(
    legend.position = "top",
    plot.margin = margin(r = 25, t = 10),
    legend.key = element_blank()
  )

ggsave(plot = plot_outliers_down, filename = "output/3_plot_outliers_down.png",
       height = 5, width = 7)

## Drop outliers based on downloads
data_january <- data_january %>%
  mutate(
    outlier = ifelse(
      type_of_broadband_connection == "ADSL" & average_download_speed > 50,
      1,
      ifelse(
        type_of_broadband_connection == "Fibre" & average_download_speed < 50,
        1,
        0
      )
    )
  ) %>%
  filter(outlier != 1)

## Plot same histograms for uploads
## No obvious outliers
## Speed in Samsville is higher just like for the downloads
plot_outliers_up <- data_january %>%
  ggplot(
    aes(
      x = average_upload_speed, fill = name_of_isp
    )
  ) +
  geom_histogram(position = "dodge") +
  labs(title = "Detecting outliers in uploads",
       subtitle = "histogram for average upload speeds",
       fill = NULL,
       x = "\nAverage upload speed\n",
       y = "\nCount\n")+
  scale_fill_manual(values = c("#0057b7", "#FFDD00")) +
  facet_wrap(~city + type_of_broadband_connection, scales = "free")  +
  theme(
    legend.position = "top",
    plot.margin = margin(r = 25, t = 10),
    legend.key = element_blank()
  )

ggsave(plot = plot_outliers_up, filename = "output/3_plot_outliers_up.png",
       height = 5, width = 7)


## See total download and upload observation/measurement count that the averages 
## are based on Obs per person for downloads.
## Overall, both upload and download data have many observations/measurements 
## per id in January 2021.
## Thus, monthly average for each person_id can be OK to use aggregate averages
## for city/isp/type.
## We don't have one person_id with 1 measurements in January 2021, and other with 500
## However we have more measurements by ISP Useus
plot_obscount_down <- data_january %>%
  ggplot(
    aes(
      x = person_id, y = download_total_obs, 
      color = type_of_broadband_connection, shape = name_of_isp,
    )
  ) +
  geom_point() +
  labs(title = "Observation/measurement count for downloads",
       subtitle = "Per person_id in January 2021\n",
       shape = "ISP",
       color = "Connection type",
       x = "\nPerson_id\n",
       y = "\nMeasurement count in January 2021\n")  +
  theme(
    plot.margin = margin(r = 25, t = 10),
    legend.key = element_blank()
  )

ggsave(plot = plot_obscount_down, filename = "output/3_plot_obscount_down.png",
       height = 5, width = 7)

## Obs per person for uploads
plot_obscount_up <- data_january %>%
  ggplot(
    aes(
      x = person_id, y = upload_total_obs, color = type_of_broadband_connection,
      shape = name_of_isp
    )
  ) +
  geom_point() +
  labs(title = "Observation/measurement count for uploads",
       subtitle = "Per person_id in January 2021\n",
       shape = "ISP",
       color = "Connection type",
       x = "\nPerson_id\n",
       y = "\nMeasurement count in January 2021\n")  +
  theme(
    plot.margin = margin(r = 25, t = 10),
    legend.key = element_blank()
  )

ggsave(plot = plot_obscount_up, filename = "output/3_plot_obscount_up.png", 
       height = 5, width = 7)

## Drop outlier column
data_january <- data_january %>% select(-outlier)

               
                      ## 4 - Data summarization and presentation ##

                               ##  Summary tables ##

data_differences <- data_january %>% 
  group_by(city, type_of_broadband_connection, name_of_isp) %>%
  summarise(
    across(
      c("average_download_speed", "average_upload_speed", "60perc_download", 
        "60perc_upload"),
      list(
        avg = mean, ## Median
        sd = sd
      )
    )
  ) %>%
  ## Round all numeric columns to first decimals
  mutate_if(is.numeric, round, 1) %>%
  ## Add total obs
  left_join(
    data_january %>%
      group_by(city, type_of_broadband_connection, name_of_isp) %>%
      summarise(
        across(
          c("download_total_obs", "upload_total_obs"),
          sum
        )
      )
  ) %>%
  # Select for ordering
  select(city, type_of_broadband_connection, name_of_isp, download_total_obs,
         average_download_speed_avg,
         average_download_speed_sd, `60perc_download_avg`, `60perc_download_sd`,
         upload_total_obs,
         average_upload_speed_avg, average_upload_speed_sd, `60perc_upload_avg`, 
         `60perc_upload_sd`)

write_csv(data_differences, "output/4a_differences.csv")

## As it is clear from the summary table, average download speed as well
## average upload speed for both Fibrelicious and USeus isps are the 
## highest for Fibre connection and the lowest for ADSL connection. 



                                 ## B Visualisation ##

plot_distributions_4b <- data_january %>%
  ggplot(
    aes(
      x = average_download_speed, fill = name_of_isp
    )
  ) +
  geom_histogram(position = "dodge") +
  labs(x = "\nAverage download speed\n",
       y = "\nCount\n",
       title = "Distributions of average download speed per ID in January 2021",
       subtitle = "Per city, ISP and connection type",
       fill = NULL) +
  scale_fill_manual(values = c("#0057b7", "#FFDD00")) +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~city + type_of_broadband_connection, scales = "free") +
  theme(
    legend.position = "top",
    plot.margin = margin(r = 25, t = 10),
    legend.key = element_blank()
  )

ggsave(plot = plot_distributions_4b, filename = "output/4b_plot_distributions.png",
       height = 5, width = 7)

                                   ## C- Analysis ##

## The average download speed for Fibrelicious in Databury is 31.5 (213.9-182.4) higher,
## but upload speed is 1.3 lower.
## Minimum detected speed for Fibrelicious for downloads is about 20 higher than 
## maximum for Useus.

                                   ## D- Analysis  ##

## Create data frame to look at download speeds by ISP/city/type at different 
# times of day
data_timeofday <- list_data_sets[["download"]] %>% ## Select measured download
  ##data from list
  ## Filter for successful tests
  filter(did_test_complete_successfully == T) %>%
  ## Add hours in decimal
  mutate(hour = time_of_measurement %>% hour) %>%
  ## Add personal info
  left_join(list_data_sets[["details"]]) %>%
  ## Filter out the outliers
  mutate(
    outlier = ifelse(
      type_of_broadband_connection == "ADSL" & measured_download_speed_in_Mbps > 50,
      1,
      ifelse(
        type_of_broadband_connection == "Fibre" & measured_download_speed_in_Mbps < 50,
        1,
        0
      )
    )
  ) %>%
  filter(outlier != 1) %>%
  ## Calculate speeds per hour
  group_by(city, name_of_isp, type_of_broadband_connection, hour) %>%
  summarise(
    mean = measured_download_speed_in_Mbps %>% mean
  )

## Graph hourly speeds
plot_time_of_day <- data_timeofday %>%
  ggplot(
    aes(
      x = hour, y = mean,
      color = name_of_isp,
    )
  ) +
  geom_line() +
  scale_color_manual(values = c("#0057b7", "#FFDD00")) +
  labs(x = "\nTime of day, hour\n",
       y = "\nAverage download speed\n",
       title = "Hourly average download speed in January 2021",
       subtitle = "Per city, ISP and connection type",
       color = NULL) +
  facet_wrap(
    ~city + type_of_broadband_connection, scales = "free"
  ) +
  theme(
    legend.position = "top",
    plot.margin = margin(r = 25, t = 10),
    legend.key = element_blank()
  )

ggsave(plot = plot_time_of_day, filename = "output/4d_plot_time_of_day.png", 
       height = 5, width = 7)


## For ISP the lowest average download speed is in the evening after around 5-6 pm.
## The highest (stable high) is at night and before around 5-6 pm.  