
library(tidyverse)
library(lubridate)

all_itineraries <- read_csv("../itineraries/all_itineraries-meta.csv",
                            col_types = cols(
                              X1 = col_character(),
                              date = col_date(format = ""),
                              user_trip_id = col_character(),
                              itinerary_id = col_character(),
                              planned_duration_mins = col_double(),
                              actual_duration_mins = col_double(),
                              exec_duration_mins = col_double(),
                              planned_start_time = col_datetime(format = ""),
                              actual_start_time = col_datetime(format = ""),
                              exec_start_time = col_datetime(format = ""),
                              trip_length_bucket = col_character(),
                              hour_of_day = col_integer(),
                              period_of_day = col_character(),
                              weekday = col_character(),
                              day_type = col_character()
                            ))

min_trip_dur <- 10
max_trip_dur <- 50
max_trip_start_diff <- 20 


# Filter unwanted itineraries
all_itineraries_filtered <- all_itineraries %>%
  mutate(start_diff = abs(exec_start_time - actual_start_time)/60) %>%
  filter(actual_duration_mins >= min_trip_dur, actual_duration_mins <= max_trip_dur, start_diff <= max_trip_start_diff) %>%
  group_by(date, user_trip_id) %>%
  mutate(hasUserItinerary = min(itinerary_id),
         num_alternatives = n()) %>%
  filter(hasUserItinerary == "0", num_alternatives > 1)
  
length(unique(all_itineraries_filtered$user_trip_id))


all_fo <- all_itineraries_filtered %>%
  group_by(date, user_trip_id) %>%
  mutate(do_fo = min(actual_duration_mins, na.rm=TRUE)) %>%
  filter(actual_duration_mins == do_fo) %>%
  arrange(itinerary_id) %>%
  filter(row_number() == 1)

all_fs <- all_itineraries_filtered %>%
  group_by(date, user_trip_id) %>%
  #filter(!any(is.na(planned_duration_mins))) %>%
  mutate(ds_fs = min(planned_duration_mins, na.rm=TRUE)) %>%
  filter(planned_duration_mins == ds_fs) %>%
  arrange(itinerary_id) %>%
  filter(row_number() == 1)

all_itineraries_meta <- all_itineraries_filtered %>%
  left_join(all_fo, by = c("date","user_trip_id"), suffix = c("", "_fo")) %>%
  left_join(all_fs, by = c("date","user_trip_id"), suffix = c("", "_fs"))

all_itineraries_metrics <- all_itineraries_meta %>%
  mutate(io = (exec_duration_mins - actual_duration_mins_fo)/exec_duration_mins,
         is = (actual_duration_mins_fs - actual_duration_mins_fo)/actual_duration_mins_fo,
         ic = ifelse(itinerary_id == "0",
                     (planned_duration_mins - planned_duration_mins_fs)/planned_duration_mins,
                     NA)) %>%
  select(date:day_type, itinerary_id_fo, planned_duration_mins_fo, actual_duration_mins_fo, itinerary_id_fs, planned_duration_mins_fs, actual_duration_mins_fs, io:ic)


write_csv(all_itineraries_metrics, "data/all_itineraries_metrics.csv")







