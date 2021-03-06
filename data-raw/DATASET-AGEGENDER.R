# Compile ONS weekly deaths by region

library(tidyverse)

#URL pieces

url_prefix <- paste("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationand",
                    "community%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%",
                    "2fweeklyprovisionalfiguresondeathsregisteredinenglandand",
                    "wales%2f",
                    sep = "")

url_suffix <- "/publishedweek"

#file types
file_type_2020<- ".xlsx"
file_type_previous <- ".xls"

#Set the most recent week of data for 2020
week_2020 <- "22"
week_2020_int <- as.integer(week_2020)

#Define previous years to download
prev_yrs <- as.character(c(2010:2019))


#Download 2020 data-------------------------------------------------------------

#Create temporary file to put data in
temp.file <- paste(tempfile(), ".xlsx", sep = "")

#Download file from URL
download.file(paste(url_prefix,
                    "2020",
                    url_suffix,
                    week_2020,
                    "2020",
                    file_type_2020,
                    sep = ""),
              temp.file, mode = "wb")

#Clean file
df_2020 <- temp.file %>%
  read_excel(sheet = "Weekly figures 2020", range = cell_rows(5:85)) %>%
  slice(-1:-38) %>%
  slice(-21:-22) %>% #Remove unnecessary rows
  select(-1) %>% #Remove empty first column
  mutate(rn = row_number(),
         gender = as.factor(case_when(rn <= 20 ~ "Men",
                                      rn > 20 ~ "Women"))) %>% #add gender var
  rename(age = 1) %>%
  select(-rn) %>% # rename age
  rename_at(vars(-1, -gender), function(x){paste0("week_", x)}) %>%
  mutate_at(vars(-1, -gender), as.integer) %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               names_prefix = "week_",
               values_to = "deaths",
               values_drop_na = F) %>% # convert from wide to long format
  separate(col = "age", into = c("age_lower","age_upper")) %>%
  mutate(week = as.integer(week),
         age_upper = as.double(age_upper),
         age_lower = as.double(age_lower),
         age_upper = case_when(
           age_lower == 90 ~ 90,
           age_lower!= 90 ~ age_upper,
           is.na(age_lower) ~ age_upper),#Use split string to make ages consistent
         age_groups = case_when(age_upper == 1 ~ 1,
                                age_upper > 1 & age_upper <= 14 ~ 2,
                                age_upper > 14 & age_upper <= 44 ~ 3,
                                age_upper > 44 & age_upper <= 64 ~ 4,
                                age_upper > 64 & age_upper <= 74 ~ 5,
                                age_upper > 74 & age_upper <= 84 ~ 6,
                                age_upper > 84 ~ 7)) %>%
  group_by(age_groups, week, gender) %>%
  summarise(deaths = sum(deaths)) %>% # make ages consist with prev years
  ungroup() %>%
  mutate(age = factor(age_groups,
                      levels = 1:7,
                      labels = c("<1",
                                 "1-14",
                                 "15-44",
                                 "45-64",
                                 "65-74",
                                 "75-84",
                                 "85<")),
         year = "2020") %>% # create factor with ages
  select(age, gender, week, deaths, year)

#Download Previous Years' Data--------------------------------------------------

#Need to download files from previous years
prev_download <- lapply(prev_yrs, function(x) {
  #Create temporary file to put data in
  temp.file <- paste(tempfile(), ".xls", sep = "")

  #Download file from URL
  download.file(paste(url_prefix,
                      {{  x  }},
                      url_suffix,
                      ifelse({{  x  }} <= 2015, "", 52),
                      {{  x  }},
                      file_type_previous,
                      sep = ""),
                temp.file,
                mode = "wb")

  #2011 and 2014 have differently formatted sheets
  if ({{  x  }} == 2011) {
    df <- temp.file %>%
      read_excel(sheet = paste(ifelse({{  x  }} <= 2015,
                                      "Weekly Figures ",
                                      "Weekly figures "),
                               {{  x  }},
                               sep=""),
                 range = cell_rows(4:41)) %>%
      slice(-1:-21)
  }

  if ({{  x  }} == 2014) {
    df <- temp.file %>%
      read_excel(sheet = paste(ifelse({{  x  }} <= 2015,
                                      "Weekly Figures ",
                                      "Weekly figures "),
                               {{  x  }},
                               sep=""),
                 range = cell_rows(3:40)) %>%
      slice(-1:-21)
  }

  if ({{  x  }} != 2011 & {{  x  }} != 2014) {
    df <- temp.file %>%
      read_excel(sheet = paste(ifelse({{  x  }} <= 2015,
                                      "Weekly Figures ",
                                      "Weekly figures "),
                               {{  x  }},
                               sep=""),
                 range = cell_rows(4:40)) %>%
      slice(-1:-20)
  }

  #After 2015 has additional column remove
  if ({{  x  }} > 2015) {
    df <- df %>%
      select(-1)
  }

  #Need to convert to long format
  df <- df %>%
    slice(-8:-9)%>% #Remove blank space between men and women
    dplyr::mutate(rn = row_number(),
           gender = as.factor(case_when(rn <= 7 ~ "Men",
                                        rn > 7 ~ "Women"))) %>% # add gender var
    rename(age = 1) %>%
    select(-rn) %>% # rename region name and id
    rename_at(vars(-1, -gender), function(x){paste0("week_", x)}) %>%
    mutate_at(vars(-1, -gender), as.integer) %>%
    pivot_longer(
      cols = starts_with("week"),
      names_to = "week",
      names_prefix = "week_",
      values_to = "deaths",
      values_drop_na = FALSE) %>% #Convert to Long format
    mutate(week = as.integer(week),
           year = {{  x  }},
           age = case_when(
             age == "Under 1 year" ~ "<1",
             age == "01-14" ~ "1-14",
             age == "85+" ~ "85<",
             age != "Under 1 year" & age != "01-14" ~ age),
           age = as.factor(age)) # Reformat so consist with 2020 factor
})

df_prev <- do.call(rbind, prev_download)



#Combine into one dataset------------------------------------------------------

#Add region id for all years and combine all years in one
ONSweeklyagegender <- df_prev %>%
  bind_rows(df_2020)


usethis::use_data(ONSweeklyagegender, overwrite = TRUE)

# Clean up
rm(list = ls())
