library(tidyverse)
library(janitor)
library(chorddiag)

#--set wd 
setwd("D:/Documents/GitHub/repositorios/chorddiag_refugees/")

#--read cvs
data <- read.csv("./data/unhcr_popstats_export_asylum_seekers_2019_06_25_173734.csv", fileEncoding = "UTF-8-BOM")
data <- clean_names(data)

#--change values to missing if * 
data$country_territory_of_asylum_residence <- as.character(data$country_territory_of_asylum_residence)
data$applied_during_year[data$applied_during_year=="*"] <- NA  
data$applied_during_year <- as.numeric((as.character(data$applied_during_year)))

#--gráfico: hacia donde van las personas: 
data_clean <- data %>%
  rename(country = country_territory_of_asylum_residence, 
         applied = applied_during_year) %>% 
  select(year, country, applied) %>%  
  filter(!is.na(applied), year == 2018) %>%  
  group_by(year, country) %>%
  summarize(total_applied = sum(applied, na.rm = TRUE)) %>% 
  arrange(year, country) %>%     
  rename(Nicaragua = total_applied) # Renamed to "Nicaragua" for the plot


# data_clean$country[data_clean$country == "United States of America"] <- "USA"
data_matrix <- as.matrix(data_clean[,3])
row.names(data_matrix) <- as.array(data_clean$country)

groupColors <- c("#482173FF", "#7DAA92", "#80FFEC", "#D6EADF", 
                 "#440154FF", "#482677FF","#2D708EFF", "#238A8DFF", 
                 "#55C667FF", "#95D840FF","#FDE725FF", "#39568CFF", 
                 "#FF7F11", "#BEB7A4", "#05A8AA") 

chorddiag(data_matrix, type = "bipartite", 
          margin = 140, 
          groupnameFontsize = 12,
          groupColors = groupColors, 
          groupnamePadding = 30, 
          tooltipGroupConnector = "    &#x25B6;    ")

