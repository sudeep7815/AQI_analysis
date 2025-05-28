library(tidyverse)
library(janitor)
library(lubridate)

"INDIA-AQI-DATA-2015-2020.csv" %>% 
  read.csv() %>% 
  clean_names()-> aqidf


aqidf %>%
  mutate(year = date %>% year(),
         month = date %>% month(),
         day = date %>% day(),
         week = date %>% week(),
         weekday = date %>% wday(label = T)) ->aqidf1
colnames(aqidf1)

unique(aqidf1$city)


aqidf1 %>% 
  pivot_longer(3:14,names_to = "pollutant", values_to = "values")-> aqidf2

###yearwise pollutant average trends for every city
aqidf2 %>% 
  group_by(year,pollutant) %>% 
  summarise(mean_value = mean(values,na.rm = T))->aqi_yearwise


library(ggplot2)
## line graph
aqi_yearwise %>%
  ggplot(aes(x = year, y = mean_value))+
  geom_line(color="red")+
  facet_wrap(~pollutant,scales = "free_y")+
  labs(title = "Air pollutants trends",
       subtitle = "from 2015 to 2020",
       x = NULL,
       y = "pollutant values",
       caption = "source:AQI India DAtaset")+
  theme_linedraw()-> plot1

ggsave("Air Polutants Trends.pdf",
       plot = plot1,
       units = "in",
       width = 10,
       height = 6)

## y axis should be free within its range,
##as its not for comparision bw the pollutants free y axis will be helpful to visualise each pollutant trend 

##assignments
###2: air quality trends for bengaluru
aqidf2 %>% 
  filter(city == "Bengaluru") %>%
  group_by(year,pollutant) %>% 
  summarise(mean_value = mean(values,na.rm = T))->Bengaluru_aqi_yearwise

Bengaluru_aqi_yearwise %>%
  ggplot(aes(x = year, y = mean_value))+
  geom_line(color="red")+
  facet_wrap(~pollutant,scales = "free_y")+
  labs(title = "Bengaluru Air pollutants trends",
       subtitle = "from 2015 to 2020",
       x = NULL,
       y = "pollutant values",
       caption = "source:AQI India DAtaset")+
  theme_linedraw()-> plot2

ggsave("Bengaluru Air Polutants Trends.pdf",
       plot = plot2,
       units = "in",
       width = 10,
       height = 6)


##3 co trends for all cities













### 4 Air quality trends for bengaluru , Chennai , Mumbai , Hyderabad


aqidf2 %>%
  filter(city %in% c("Bengaluru", "Chennai", "Mumbai", "Hyderabad")) %>%
  group_by(year, pollutant, city) %>%
  summarise(mean_values = mean(values, na.rm = T)) -> metro_trends

metro_trends

metro_trends %>%
  ggplot(aes(x = year, y = mean_values, color = city)) +
  geom_line() +
  facet_wrap(~pollutant, scales = "free_y") +
  labs(
    title = "Air Quality Trends: Bengaluru, Chennai, Mumbai, Hyderabad",
    subtitle = "2015–2020",
    x = NULL,
    y = "Pollutant Values",
    caption = "Source: city_day") +
  theme_linedraw()->plot4

ggsave("All cities pollutant trend.pdf",
       plot = plot4,
       units = "in",
       width = 10,
       height = 6)










# Heat map
aqidf2 %>%
  filter(pollutant == "co") %>%
  group_by(week, weekday, month) %>%
  summarise(meanval = mean(values, na.rm = TRUE)) %>%
  ggplot(aes(x = week,
             y = weekday,
             fill = meanval)) +
  geom_tile() +
  facet_wrap(~month, scales = "free_x") +
# scale_fill_gradient(low = "yellow", high = "red") +
 scale_fill_gradientn(colours = c("darkgreen", "yellow", "red")) +
 theme_minimal() +
 labs(title = "CO heat map",
       subtitle = "For all cities",
       x = NULL,
       y = NULL)




