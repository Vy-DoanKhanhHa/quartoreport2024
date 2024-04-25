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
  geom_bar(stat="identity")

map_world <- map_data("world")

# time series
ggplot(data = data_join) +
  aes(year, obs_value, group = country, color = continent) + 
  geom_line()

ggplotly(timeseries_plot_1)

# map

map_data_join <- full_join(data_join, map_world, by = c("country" = "region"))
data_join_2017 <- data_join %>%
  filter(year==2017)
map_data_join_2017 <- data_join_2017 %>%
  full_join(map_world, by = c("country" = "region"))
ggplot(map_data_join) +
  aes(x=longitude,y=latitude, group = group, fill = obs_value) +
  geom_polygon()

# scatter plot
ggplot(data_join) +
  aes(lifeExp, obs_value, color = continent, size = pop)+
  geom_point(alpha= 0.2)+
  facet_wrap(~ year) +
  scale_x_continuous(
    breaks = c(0, 100),
    labels = scales :: unit_format(unit="k", scale = 0.001)) +
  labs(
        x="Access to water and soap",
        y="Life Expectancy",
        title = "Countries with access to handwashing facility at home in relation to Life Expectancy" 
      )+
      guides(color = "none") +
      theme_classic()
    theme(text = element_text(family="serif"))
