#---------------------------------------------------------------------------#
# Nom : India_pop.csv                                       			          #
# Description : Data viz on murderdata                                      #
# Auteur : Pietro Violo                                                     #
# Date : May 1st  2023                                                      #
# Modifications :                                                           #
#---------------------------------------------------------------------------#

options(scipen=999)
rm(list = ls())

#---------------------------------------------------------------------------#
# Library and data                                                          #
#---------------------------------------------------------------------------#
library(tidyverse)


df <- read.csv("./Data/India_pop.csv") %>% 
  mutate(Total.projected.pop = str_remove(Total.projected.pop, "\xca"),
         Total.projected.pop = str_remove_all(Total.projected.pop, ","),
         Total.projected.pop = as.numeric(Total.projected.pop))

# Import total population

df_UN <- read.csv("./Data/WPP2022_TotalPopulationBySex.csv") %>% filter(Time == 2023,
                                                                        Variant == "Medium",
                                                                        LocTypeName == "Country/Area") %>% 
  select(Location, ISO2_code, PopTotal) %>% 
  mutate(PopTotal = PopTotal * 1000)

official_countries <- read.csv("./Data/official_countries_iso.csv")

df_UN <- df_UN %>% filter(ISO2_code %in% official_countries$Code)

#---------------------------------------------------------------------------#
# Data wrangling                                                            #
#---------------------------------------------------------------------------#

# First, take the proportion of each Indian state

df <- df %>% mutate(proportion = Total.projected.pop/sum(Total.projected.pop))

# Add the total projected population by the United Nations

df <- df %>% mutate(State.Pop = proportion * 1428627663,
                    State.Pop = round(State.Pop)) %>% 
  select(State.Name, State.Pop)



# For each Indian state, we would like to find the country having the most similar population size

string_of_countries <- c()

for(i in 1:nrow(df)){
  df_UN$Indianstate <- df[i,2]
  df_UN <- df_UN %>% mutate(difference = abs(PopTotal - Indianstate)) %>% 
    arrange(difference)
  
  string_of_countries <- c(string_of_countries, df_UN[1,1])
}

df$Location <- string_of_countries

df <- left_join(df,df_UN %>% filter(Location %in% string_of_countries) %>% select(Location, PopTotal, ISO2_code)) %>% 
  mutate(difference = PopTotal - State.Pop,
         ISO2_code = tolower(ISO2_code))

# Country flags
flags <- df %>% ggplot(aes(x = 1:36, y = State.Name, country = ISO2_code)) + geom_flag(size = 10) +
  theme(legend.position="bottom",
        plot.background = element_blank(),
        panel.background = element_blank(), 
        legend.background = element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        legend.text = element_text(colour = "white")) +
  scale_color_manual( 
    values = "black")

#---------------------------------------------------------------------------#
# Vizualisation                                                             #
#---------------------------------------------------------------------------#
library(maps)
library(sf)
library(ggflags)

shp1 <- read_sf("./Data/IND_adm/IND_adm1.shp")

map_india <- left_join(shp1, df %>% rename(NAME_1 = State.Name))

map_india <- map_india %>% ggplot() + geom_sf(aes(color = "black")) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(), 
        legend.background = element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        legend.text = element_text(colour = "white")) +
  scale_color_manual( 
    values = "black")

ggsave("graph.png", plot = map_india, bg = "transparent", 
       device = "png", 
       height = 5599, 
       width = 5000, 
       units = "px",
       dpi = 300)


ggsave("flags.png", plot = flags, bg = "transparent", 
       device = "png", 
       height = 5500, 
       width = 5500, 
       units = "px",
       dpi = 500)


