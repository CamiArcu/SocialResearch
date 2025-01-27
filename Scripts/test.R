library(plotly)
library(tidyverse)

setwd("C:/Users/Mariano/Documents/Cami/SocialResearch")

files <- list.files("Data", recursive = T)
files <- grep(".csv", files, value = T)
files <- grep("./", files, value = T)

tables <- lapply(paste0("Data/", files), read_csv)

tables <- lapply(tables, mutate,  Period = paste0(
  floor((Year - 1950) / 5) * 5 + 1950, 
  "-", 
  floor((Year - 1950) / 5) * 5 + 1954
))

tables <- lapply(tables, select, -c(Year, Code))

LAM <- c("Argentina", "Bolivia", "Brasil", "Chile", "Colombia", "Costa Rica", "Cuba", "Ecuador", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Republica Dominicana", "Uruguay", "Venezuela")

LAM_urb <- paste0(LAM, " (urban)")
LAM <- c(LAM, LAM_urb)

Code <- rep(c("ARG", "BOL", "BRA", "CHI", "COL", "CR", "CU", "ECU", "SAL", "GUA", "HON", "MEX", "NIC", "PAN", "PAR", "PE", "DOM", "URU", "VEN"), 2)

years <- 1950:2022

# Stratify years into periods of 5 years
# Create a data frame with years and corresponding periods
stratified_years <- data.frame(
  Year = years,
  Period = paste0(
    floor((years - 1950) / 5) * 5 + 1950, 
    "-", 
    floor((years - 1950) / 5) * 5 + 1954
  )
)

Periods <- unique(stratified_years$Period)
big.table <- data.frame(
  Entity = rep(LAM, each = length(Periods)), 
  Code = rep(Code, each = length(Periods)), 
  Period = rep(Periods, time = length(LAM)))

for (i in 1:length(tables)){
  big.table <-  left_join(big.table, tables[[i]], by = c("Entity", "Period")) 
}
  
Literacy.dt <- group_by(big.table, Code, Period) %>% summarize(Literacy = mean(`Literacy rate`, na.rm = T)) %>%  ungroup()
Literacy.dt <- mutate(Literacy.dt, Period = as.numeric(str_split_i(Literacy.dt$Period, "-", 2)))

ggplot(Literacy.dt, aes(x= Period, color = Code)) +
  geom_line(aes(y = Literacy))

ggplot(filter(Literacy.dt, !is.nan(Literacy)), aes(x= Period, color = Code)) +
  geom_line(aes(y = Literacy))
  

