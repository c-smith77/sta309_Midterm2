##################################
## Colin Smith
## Midterm 2 Dashboard
## STA 309
## 11/22/22
#################################
## This code will include the wrangling of an earthquakes
## dataset and the creation of various plots to create
## a meaningful dashboard that explores the Tohoku Earthquake
## and Tsunami in Japan in 2011
#################################

library(tidyverse)
library(ggthemes)
library(ggtext)

# Reading in the earthquake data:
earthquakes <- read_csv("earthquake_record_1965_2016.csv")
earthquakes <- earthquakes %>% 
  filter(Type == "Earthquake") %>%    # Getting rid of non-earthquakes
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%    # making the Data variable month, date, year format
  filter(Date >= "2000-01-01")    # Looking at earthquakes since 2000

## Japan Map of Earthquakes

# Getting world ma data and selecting only coordinates that pertain to japan
world_map <- map_data("world")
japan_map <- world_map %>% 
  filter(region=="Japan")

# Getting just the earthquakes arounf Japan
japan_eq <- earthquakes %>% 
  filter(Latitude <= 45.5572 & Latitude >= 20.4252,
         Longitude <= 153.9863 & Longitude >= 122.9336)
tohoku <- earthquakes %>% 
  filter(ID == "OFFICIAL20110311054624120_30")  # Just the Tohoku Earthquake

# Creating map of Japan, highlighting the Tohoku Earthquake
japan_plot <- ggplot() +
  geom_polygon(data=japan_map, aes(x=long, y = lat, group=group),
               color="gray0", fill= "white") +
  geom_point(data = japan_eq, aes(x=Longitude, y= Latitude), 
             alpha=0.3, shape=1) +
  geom_point(data=tohoku, aes(x=Longitude, y=Latitude), 
             color = "firebrick1", size=2) +
  annotate("segment", x=148, xend=143, y=34, yend=38,
           color="firebrick1", size=.75, arrow = arrow(length = unit(.15, "inches"))) +
  annotate("text", x=149, y=32, label="Tohoku\nEarthquake",
           color="firebrick2") +
  coord_map() +
  theme_map() +
  labs(title = "Map of Japan Earthquakes from 2000 to 2016") +
  theme(plot.title = element_text(family="serif"))


#########################################################################
## Nuclear Explosion Comparison

# Using same dataset, get the nuclear explosions of at least 6.3 magnitude
nukes <- read_csv("earthquake_record_1965_2016.csv")
nukes <- nukes %>% 
  filter(Type == "Nuclear Explosion",
         Magnitude >= 6.3) %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"))

## Add both Hiroshima and Nagasaki Atomic Bombs 
tohoku_WWII <- tohoku %>% 
  add_row(Date = as.Date("1945-08-06"), Latitude = 34.3833, Longitude = 132.4499, Type  = "Nuclear Explosion", Magnitude=6.0, ID = "Hiroshima Atomic Bomb") %>% 
  add_row(Date = as.Date("1945-08-09"), Latitude = 32.7642, Longitude = 129.8726, Type  = "Nuclear Explosion", Magnitude=6.1, ID = "Nagasaki Atomic Bomb")

tohoku_nukes <- bind_rows(tohoku_WWII, nukes) # putting two the data Frames together

# Creating a bar graph of magnitudes, highlighting tohoku EQ's magnitude
nukes_plot <- ggplot(tohoku_nukes) +
  geom_col(aes(x=reorder(ID, Magnitude), y= Magnitude, fill=ID)) + 
  geom_text(aes(x=ID, y=Magnitude, label=Magnitude), hjust=-.1, fontface="bold") +
  annotate("text", x= "OFFICIAL20110311054624120_30", y= 4.5, 
           label = "Tohoku Earthquake", color = "gray99", family = "serif", size=5) +
  annotate("text", x= "Hiroshima Atomic Bomb", y= 3, 
           label = "Hiroshima A-Bomb", color = "gray99", family= "serif", size=5) +
  annotate("text", x= "Nagasaki Atomic Bomb", y= 3.05, 
           label = "Nagasaki A-Bomb", color = "gray99", family = "serif", size=5) +
  coord_flip() +
  scale_fill_manual(values = c("#836953", "#836953", "firebrick1", "#836953", "#836953", "#836953", "#836953", "#836953", "#836953", "#836953", "#836953", "#836953")) +
  theme_minimal() +
  scale_x_discrete(breaks=NULL) +
  scale_y_continuous(breaks=NULL) +
  labs(title = "Magnitude of Tohoku Earthquake Compared to the Highest Magnitude Nuclear Explosions") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(family = "serif"))
###################################################################################
## Climate Change Effect on Earthquakes

# Getting the total number of earthquakes each year, from same dataFrame
earthquakes_total <- read_csv("earthquake_record_1965_2016.csv")
earthquakes_total <- earthquakes_total %>% 
  filter(Type == "Earthquake") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         Year = str_sub(Date, 1, 4),  # Creating a Year variable from Date variable
         Year = as.integer(Year)) %>% 
  group_by(Year) %>% 
  mutate(count = n()) %>%     # Counting the number of earthquakes in each year
  select(Year, count) %>% 
  summarize(count = mean(count))

# Getting the total number of earthquakes each year in Japan, from same dataFrame
earthquakes_total_japan <- read_csv("earthquake_record_1965_2016.csv")
earthquakes_total_japan <- earthquakes_total_japan %>% 
  filter(Type == "Earthquake") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y"),
         Year = str_sub(Date, 1, 4),
         Year = as.integer(Year)) %>%
  filter(Latitude <= 45.5572 & Latitude >= 20.4252,                  # Filtering to earthquakes
         Longitude <= 153.9863 & Longitude >= 122.9336) %>%          #  only in Japan
  group_by(Year) %>% 
  mutate(count=n()) %>% 
  select(Year, count) %>% 
  summarize(count = mean(count))

# Creating a line graph that shows the change in the total earthquakes in the world and in Japan each year
totalEQ_plot <- ggplot() +
  geom_line(data = earthquakes_total, aes(x=Year, y=count), color="#3EAEC1") +
  geom_line(data=earthquakes_total_japan, aes(x=Year, y=count), color = "#C1513E") +
  annotate("text", x=2000, y=350, label="Tohoku\nEarthquake", size=3, 
           family="serif", fontface="bold") +
  annotate("segment", x=2003, xend=2011, y=350, yend=280,
           color="gray0", size=.6, arrow = arrow(length = unit(.1, "inches"))) +
  annotate("segment", x=2003, xend=2011, y=350, yend=710,
           color="gray0", size=.6, arrow = arrow(length = unit(.1, "inches"))) +
  scale_x_continuous(breaks = c(1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016),
                     minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  theme_minimal() +
  labs(title = "<span style='color:#3EAEC1'>Number of Earthquakes in the World</span> and <span style='color:#C1513E'>Number of Earthquakes in Japan</span> since 1965",
       subtitle = "Compared to the change in temperature above, the number of earthquakes in the world has simultaneously increased.\nThere hasn't been an evident increase in earthquakes in Japan, however") +
  theme(plot.title = element_markdown(family="serif"),
        plot.subtitle = element_text(family="serif", size=10, color="gray40"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


# Getting the temperature change data set (has change in temp each year for each country)
warming <- read_csv("Annual_Surface_Temperature_Change.csv")
warming_all <- warming %>% 
  select(!c(ObjectId, ISO2, ISO3,Indicator,Unit, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>%    # Getting rid of extraneous variables
  pivot_longer(!Country, names_to = "Year", values_to = "Temp_change") %>%        # Putting data in tall format
  mutate(Year = str_sub(Year, 2, 5),
         Year = as.numeric(Year)) %>% 
  drop_na() %>% 
  group_by(Year) %>% 
  summarize(avg_change = mean(Temp_change))     # Getting the average change in temp of the world

# Getting the average change in temperature for just Japan
warming_Japan <- warming %>% 
  select(!c(ObjectId, ISO2, ISO3,Indicator,Unit, Source, CTS_Code, CTS_Name, CTS_Full_Descriptor)) %>% 
  pivot_longer(!Country, names_to = "Year", values_to = "Temp_change") %>% 
  mutate(Year = str_sub(Year, 2, 5),
         Year = as.numeric(Year)) %>% 
  filter(Country == "Japan")

# Creating a line graph that shows the change in temperature per year for the wolrd and Japan
warming_plot <- ggplot() +
  geom_line(data = warming_all, aes(x=Year, y=avg_change), group=1, color="#3EAEC1") +
  geom_line(data = warming_Japan, aes(x=Year, y= Temp_change), group=1, color="#C1513E") +
  annotate("segment", x=2006, xend=2011, y=0, yend=0.5,
           color="gray0", size=.6, arrow = arrow(length = unit(.15, "inches"))) +
  annotate("text", x=2006, y=-0.15, label = "Tohoku\nEarthquake", size=3, 
           family="serif", fontface="bold") +
  scale_x_continuous(breaks=c(1961, 1966, 1971, 1976, 1981, 1986, 1991, 1996, 2001, 2006, 2011, 2016, 2021),
                     minor_breaks = NULL) +
  scale_y_continuous(minor_breaks = NULL) +
  theme_minimal() +
  labs(title = "<span style='color:#3EAEC1'>World Average Temperature Change (°C)</span>and <span style='color:#C1513E'>Japan Average Temperarture Change (°C)</span>since 1961",
       subtitle = "Not only is the average change almost always increasing since 1990, but the amount by which it is increasing is increasing") +
  theme(plot.title = element_markdown(family = "serif"),
        plot.subtitle = element_text(family="serif", size = 10, color="gray40"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

########################################################################
## Creating the Dashboard

library(patchwork)

tohoku_dash <- (japan_plot + warming_plot) / (nukes_plot + totalEQ_plot) +
  plot_annotation(title = "Exploring the 2011 Tohoku Earthquake and Tsunami in Japan",
                  caption = "Sources: The 9th Statistical Forum of the International Monetary Fund\nhttps://miamioh.instructure.com/courses/176121/files/26008506/download?download_frd=1\nhttps://en.wikipedia.org/wiki/Atomic_bombings_of_Hiroshima_and_Nagasaki") +
  theme(text = element_text(family="serif"))


## Saving images

ggsave(filename="JapanMap.png", plot=japan_plot,
       device="png", bg = "white",
       width=10, height=8, dpi=300)
ggsave(filename="NukesPlot.png", plot=nukes_plot,
       device="png", bg = "white",
       width=10, height=8, dpi=300)
ggsave(filename="TotalEQPlot.png", plot=totalEQ_plot,
       device="png", bg = "white",
       width=10, height=8, dpi=300)
ggsave(filename="WarmingPlot.png", plot=warming_plot,
       device="png", bg = "white",
       width=10, height=8, dpi=300)

ggsave(filename="TohokuDashboard.png", plot=tohoku_dash,
       dpi=600, width=14, height=8)
