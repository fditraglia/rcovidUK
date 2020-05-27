# Compile ONS weekly deaths by region

library(tidyverse)
library(readxl)
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
week_2020 <- "19"
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
  read_excel(sheet = "Weekly figures 2020", range = cell_rows(5:96)) %>%
  slice(-1:-81) %>% #Remove unnecessary rows
  rename(reg_id = 1, reg_nm = 2) %>% # rename region name and id
  rename_at(vars(-1:-2), function(x){paste0("week_", x)}) %>%
  mutate_at(vars(-1:-2), as.integer) %>%
  pivot_longer(cols = starts_with("week"),
               names_to = "week",
               names_prefix = "week_",
               values_to = "deaths",
               values_drop_na = F) %>% # convert from wide to long format
  mutate(week = as.integer(week),
         year = "2020")

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
                 range = cell_rows(4:53)) %>%
      slice(-1:-39)
  }

  if ({{  x  }} == 2014) {
    df <- temp.file %>%
      read_excel(sheet = paste(ifelse({{  x  }} <= 2015,
                                      "Weekly Figures ",
                                      "Weekly figures "),
                               {{  x  }},
                               sep=""),
                 range = cell_rows(3:52)) %>%
      slice(-1:-39)
  }

  if ({{  x  }} != 2011 & {{  x  }} != 2014) {
    df <- temp.file %>%
      read_excel(sheet = paste(ifelse({{  x  }} <= 2015,
                                      "Weekly Figures ",
                                      "Weekly figures "),
                               {{  x  }},
                               sep=""),
                 range = cell_rows(4:52)) %>%
      slice(-1:-38)
  }

  #Before 2015 has no region id column - add for consistency
  if ({{  x  }} <= 2015) {
    df <- df %>%
      add_column(z = "", .before = 1)
  }

  #Need to convert to long format
  df <- df %>%
    rename(reg_id = 1,
           reg_nm = 2) %>%
    rename_at(vars(-1, -2), function(z) {paste0("week_", z)}) %>%
    mutate_at(vars(-1, -2), as.integer) %>%
    pivot_longer(
      cols = starts_with("week"),
      names_to = "week",
      names_prefix = "week_",
      values_to = "deaths",
      values_drop_na = FALSE) %>% #Convert to Long format
    mutate(week = as.integer(week),
           year = {{  x  }})
})

df_prev <- do.call(rbind, prev_download)



#Combine into one dataset------------------------------------------------------

#Create list of regions and ids so they can be merged in
regions<- df_2020 %>%
  select(c(reg_nm, reg_id)) %>%
  distinct()

#Add region id for all years and combine all years in one
ONSweekly <- df_prev %>%
  select(-reg_id) %>%
  left_join(regions, by = "reg_nm") %>%
  bind_rows(df_2020)


usethis::use_data(ONSweekly, overwrite = TRUE)

# Clean up
rm(list = ls())
