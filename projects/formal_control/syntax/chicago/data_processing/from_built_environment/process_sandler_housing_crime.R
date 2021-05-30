library(tidyverse)
source("./syntax/project_functions.R")

public_housing <- read_csv("D:/Projects/dissertation_data/chicago/sandler_public_housing/Replication/public_housing_geocode.csv")
demo <- read_csv("D:/Projects/dissertation_data/chicago/sandler_public_housing/Replication/CHAdemo_edited.csv")

xxxx <- haven::read_dta("D:/Projects/dissertation_data/chicago/sandler_public_housing/Replication/Public_Housing.dta")

vroom::vroom("D:/Projects/dissertation_data/chicago/sandler_public_housing/Crimedata/murder.csv") %>%
  mutate(death_date = lubridate::mdy(death_date)) %>%
  arrange(death_date)
