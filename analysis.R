library(tidyverse)
library(maps)
library(mapproj)
trends <- read.csv("~/Junior UW/incarceration-trends/incarceration_trends.csv")

# 1. Which County has the largest black jail population?
highest_black_county <- trends %>%
  group_by(county_name) %>%
  summarize(black_jail_pop = max(black_jail_pop)) %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

highest_black_county

# 1. Which County has the largest white jail population?
highest_white_county <- trends %>%
  group_by(county_name) %>%
  summarize(white_jail_pop = max(white_jail_pop)) %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)

highest_white_county

# 2. Which year was the jail population of black inmates the highest?
black_highest_year <- trends %>%
  group_by(year) %>%
  summarize(black_pop = max(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_pop == max(black_pop, na.rm = TRUE)) %>%
  pull(year)

black_highest_year

# 2. Which year was the jail population of white inmates the highest?
white_highest_year <- trends %>%
  group_by(year) %>%
  summarize(white_pop = max(white_jail_pop, na.rm = TRUE)) %>%
  filter(white_pop == max(white_pop, na.rm = TRUE)) %>%
  pull(year)

white_highest_year

# 3. Which State has the largest black jail population?
highest_black_state <- trends %>%
  group_by(state) %>%
  summarize(black_jail_pop = max(black_jail_pop, na.rm = TRUE, infinite.rm = TRUE)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(state)

highest_black_state

# 3. Which State has the largest white jail population?
highest_white_state <- trends %>%
  group_by(state) %>%
  summarize(white_jail_pop = max(white_jail_pop, na.rm = TRUE, infinite.rm = TRUE)) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(state)

highest_white_state

# 4. Which county in New York has the largest black jail population?
highest_black_ny <- trends %>%
  group_by(state = "NY") %>%
  group_by(county_name) %>%
  summarize(black_jail_pop = max(black_jail_pop, na.rm = TRUE)) %>%
  filter(black_jail_pop == max(black_jail_pop)) %>%
  pull(county_name)

highest_black_ny

# 4. Which county in New York has the largest white jail population?
highest_white_ny <- trends %>%
  group_by(state = "NY") %>%
  group_by(county_name) %>%
  summarize(white_jail_pop = max(white_jail_pop, na.rm = TRUE)) %>%
  filter(white_jail_pop == max(white_jail_pop)) %>%
  pull(county_name)

highest_white_ny

# 5. What is the mean value of black population across all the states in the current year?
mean_black_pop <- trends %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarize(black_pop = max(black_jail_pop, na.rm = TRUE, infinite.rm = TRUE)) %>%
  summarize(black_pop = mean(black_pop)) %>%
  pull(black_pop)

mean_black_pop

# 5. What is the mean value of white population across all the states in the current year?
mean_white_pop <- trends %>%
  group_by(state) %>%
  filter(year == max(year)) %>%
  summarize(white_pop = max(white_jail_pop, na.rm = TRUE, infinite.rm = TRUE)) %>%
  summarize(white_pop = mean(white_pop)) %>%
  pull(white_pop)

mean_white_pop

# Filter black_jail_pop
ny_black_jail <- trends %>%
  group_by(state = "NY") %>%
  group_by(year) %>%
  summarize(black_jail_pop = max(black_jail_pop, na.rm = TRUE))

# Black Imprisonment Trend plot
black_ny_plot <- ggplot(data = ny_black_jail, mapping = aes(x = year, 
                                                      y = black_jail_pop)) +
  geom_bar(mapping = aes(fill = year), stat = 'identity') +
  labs(x = "Year", y = "Cumulative Count", 
       title = "Black Imprisonment Trend in NY")
                            
black_ny_plot

#I included this chart because the incarceration of POC, specifically black
#people continues to be an extremely relevant topic in the United States. I 
#wanted to see how black incarceration rates have changed over the years. 
#Looking at the graph, it appears that there was a steady growth of incarceration
#rates starting in 1985. It peaked in 1993 and then had a gradual downfall which
#continues on into recent years.

# Filter white_jail_pop
ny_white_jail <- trends %>%
  group_by(state = "NY") %>%
  group_by(year) %>%
  summarize(white_jail_pop = max(white_jail_pop, na.rm = TRUE)) 

ny_white_jail

# White Imprisonment Trend plot
white_ny_plot <- ggplot(data = ny_white_jail, mapping = aes(x = year, 
                                                           y = white_jail_pop)) +
  geom_bar(mapping = aes(fill = year), stat = 'identity') +
  labs(x = "Year", y = "Cumulative Count", 
       title = "White Imprisonment Trend in NY")

white_ny_plot

#I created this chart because comparing the black incarceration trends to the
#white incarceration trends is a great way to show the disproportion of the 
#incarceration of POC, specifically black inmates. Comparing the two datasets,
#on average, more black people are imprisoned annually than white people. A 
#pattern that emerged from both datasets was the spike in imprisonments in 1993.

# Get most recent counties data from dataset
counties_mod <- trends %>%
  group_by(county_name) %>%
  filter(year == max(year))

# Use map_data function to join covid 'counties' dataset with the map_data for county
county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

# Merge map data
map_data <- county_shapes %>%
  left_join(counties_mod, by= "fips")

# Incoporate blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Black Imprisonment Population Map
b_pop_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x= long, y= lat, group=group, fill = black_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "pink", high = "purple") +
  blank_theme +
  ggtitle("2018 Black Imprisonment Population in the United States")

b_pop_map

# White Imprisonment Population Map
w_pop_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(x= long, y= lat, group=group, fill = white_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$white_jail_pop)), na.value = "white", low = "pink", high = "purple") +
  blank_theme +
  ggtitle("2018 White Imprisonment Population in the United States")

w_pop_map
