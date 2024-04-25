install.packages("tidyverse")
install.packages("plotly")
install.packages("maps")

library(tidyverse)
library(plotly)
library(maps)

unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_3 <- read_csv("unicef_indicator_3.csv")
unicef_indicator_4 <- read_csv("unicef_indicator_4.csv")

data_join <- unicef_metadata %>%
  inner_join(unicef_indicator_1, by = c("country","year"= "time_period")) %>%
  inner_join(unicef_indicator_3, by = c("country","year" = "time_period")) %>%
  inner_join(unicef_indicator_4, by = c("country"))

# bar
bar_chart_data <- data_join %>%
  group_by(continent) %>%
  summarise(m_Obs_value=mean(Obs_value, na.rm = TRUE))
ggplot(data = bar_chart_data) +
  aes(x = continent, y = m_Obs_value,fill = continent) +
  geom_bar(stat="identity") +
  labs(x="Continent",y="Percentage of population") +
  theme_minimal() +
  theme(text = element_text(family = "serif"), legend.position = "hide")

map_world <- map_data("world")

# map
data_join_2021 <- data_join %>%
  filter(year==2021)
map_data_join_2021 <- data_join_2021 %>%
  full_join(map_world, by = c("country" = "region"))
ggplot(data=map_data_join_2021) +
  aes(x=long,y=lat, group = group, fill = obs_value) +
  geom_polygon() +
  labs(fill = "Percentage") +
  scale_fill_gradient2(low="#C26B50",high="#529985",midpoint = 50,mid="#DBCE47") +
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(), 
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())

# time series
ggplot(data=data_join) +
  aes(year, obs_value, group = country, color = continent) + 
  geom_line() +
  labs(x="Evolution by year",y="Access to handwashing facility (%)", color = "Continent") +
  theme_minimal() +
  theme(text = element_text(family = "serif"))


# scatter plot
ggplot(data_join_2021) +
  aes(lifeExp, obs_value, color = continent, size = pop)+
  geom_point(alpha= 0.5)+
  geom_smooth(method = "lm", se = FALSE) +
  guides(size=FALSE) +
  labs(
        x="Access to water and soap",
        y="Life Expectancy",
        color= "Continent"
      )+
      theme_minimal()+
  theme(text = element_text(family="serif"))

# table
table_data <- data_join_2021 %>%
  group_by(continent) %>%
  summarise(m_lifeExp=mean(lifeExp, na.rm = TRUE))
library(knitr)

# Print data as a table
kable(table_data)