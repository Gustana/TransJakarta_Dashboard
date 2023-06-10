library(shinydashboard)
library(plotly)
library(shiny)
library(dplyr)
library(scales)
library(glue)
library(ggplot2)
library(DT)

options(scipen = 999)

file.list <- list.files("dataset/2021/", pattern = "*.csv")

df.list <- lapply(file.path("dataset/2021", file.list), read.csv)

data <- do.call("rbind", df.list)

data <- data %>% 
  mutate(tahun = as.factor(tahun),
         bulan = as.factor(bulan),
         jenis = as.factor(jenis),
         kode_trayek = as.factor(kode_trayek),
         trayek = as.factor(trayek)
  )

empty_trayek <- data %>% 
  filter(trayek == "") %>% 
  select(kode_trayek) %>% 
  pull()

data[data$kode_trayek == empty_trayek[1] & data$trayek == "", "trayek"] <- "Gondangdia - Cikini via Kramat Raya"
data[data$kode_trayek == empty_trayek[2] & data$trayek == "", "trayek"] <- "Gondangdia - Cikini via Salemba Raya"
data$trayek <- droplevels(data$trayek)

# this data for value box and bar chart
pasenger_volume_data <- data %>% select(bulan, jumlah_penumpang) %>% 
  group_by(bulan) %>% 
  summarise(total_penumpang = sum(jumlah_penumpang)) %>% 
  arrange(desc(total_penumpang))

# this data for value box and bar chart
top10_busy_routes <- data %>% 
  group_by(trayek) %>% 
  summarise(total_penumpang = sum(jumlah_penumpang)) %>% 
  arrange(desc(total_penumpang)) %>% 
  top_n(10)

# convert int to month name
get_month_list <- function(){
  
  month_list <- list("All")
  
  for(month in levels(data$bulan)){
    month_list <- append(month_list, month.name[as.integer(month)])
  }
  
  return(month_list)
}