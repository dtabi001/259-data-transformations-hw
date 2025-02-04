#PSYC 259 Homework 2 - Data Transformation
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 
#DERRIAN TABILIN

### SETUP: RUN THIS BEFORE STARTING ----------

#Load packages
library(tidyverse)
ds <- read_csv("data_raw/rolling_stone_500.csv")
ds

### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER
glimpse(ds$Year)
ds$Year <- as.numeric(ds$Year, na.rm = TRUE)
typeof(ds$Year)

### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER
ds <- ds %>%
  rename_all(tolower)
ds

### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER #DOESNT WORK, skipped
ds <- ds %>% 
  mutate(decades = floor(year / 10) * 10)
ds

### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER
ds <- arrange(ds, rank)
ds

### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER
top10 <- ds %>%
  filter(rank <= 10) %>%
  select(song:artist)
top10

### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER
ds_sum <- ds %>%
  summarize(earliest = min(year, na.rm = TRUE), 
            latest = max(year, na.rm = TRUE),
            average = mean(year, na.rm = TRUE))
ds_sum

### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year
 
#ANSWER
artistsong <- ds %>%
  filter(year %in% c(ds_sum$earliest,
                     ds_sum$latest,
                     ds_sum$average))
arrange(artistsong, year)


### Question 8 ---------- 

# There's an error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, average-ist, and most recent songs

#ANSWER ### DOESNT WORK, skipped
ds <- ds %>%
  mutate(year = ifelse(year < 1900, year + 100, year + 0))
ds

ds <- ds %>% 
  mutate(decades = floor(year / 10) * 10)
ds

artistsong <- ds %>%
  filter(year %in% c(ds_sum$earliest,
                     ds_sum$latest,
                     ds_sum$average))
arrange(artistsong, year)


### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER #didn't figure out the decades so skipped
ds %>% 
  group_by() %>%
  summarize()


### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER #can't do without decades, skipped
ds %>% 
  count()
slice_max()
  