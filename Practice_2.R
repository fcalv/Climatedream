#####################################
# ENVIRONMENT
#####################################
# Set the working environment
install.packages('rstudioapi')
library(rstudioapi)
setwd(dirname(getSourceEditorContext()$path))

# Get essential libraries
library(tidyverse)

# Disable text encoding as 'factors'
options(stringsAsFactors = FALSE)

#####################################
# READ & CLEAN DATA
#####################################
# Data source: CSB
# https://opendata.cbs.nl/statline/#/CBS/en/dataset/83154ENG/table?ts=1583359705077

data <- read.csv2('Environmental_sector__activities_04032020_231238.csv')
data %>% glimpse


# Let's use short column names
colnames(data)
col_description <- colnames(data)

colnames(data) <- c('domain','activity','year','labour','output','value_added')
data %>% glimpse


# The first column always has the same value, thus we drop it
data$domain %>% unique
data <- data %>% select(-domain)
data %>% glimpse


# Let's transform labour values into numbers
data <- data %>% 
  mutate(labour = as.numeric(labour)) 

data %>% 
  mutate(labour_half = labour/2) -> test
test %>% glimpse

# Let's transform the years into numbers too
data <- data %>%
  mutate(year = gsub('\\*', '', year)) %>% 
  mutate(year = as.numeric(year))

data %>% glimpse

list_activity <- data %>%
  select(activity) %>% 
  pull %>%
  unique
print(list_activity)

data_total <- data %>%
  filter(activity == list_activity[1])

gsub('ll*','-','Hello what a lovely hell')


#####################################
# BASIC VISUALIZATION
#####################################

# ...Let's check employment accross activities
data %>%
  filter(activity != list_activity[1]) %>%
  ggplot( aes(x=year, y=labour, fill=activity) ) +
  geom_col()

data %>%
  filter(activity != list_activity[1]) %>%
  ggplot( aes(x=year, y=labour, fill=activity) ) +
  facet_wrap( activity~ .) +
  geom_col()

data %>%
  filter(activity != list_activity[1]) %>%
  ggplot( aes(x=year, y=labour, fill=activity) ) +
  facet_wrap( activity~ ., scales = 'free') +
  geom_col()

data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=year, y=labour, fill=activity) ) +
  facet_wrap( activity~ ., scales = 'free') +
  geom_col()

###### Exercise
# Make barcharts showing the 'output' variable
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=year, y=output, fill=activity) ) +
  facet_wrap( activity~ ., scales = 'free') +
  geom_col()


###### Exercise
# Make barcharts showing the 'value_added' variable
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=year, y=value_added, fill=activity) ) +
  facet_wrap( activity~ ., scales = 'free') +
  geom_col()

###### Exercise
# Use the years as facets now, 
# The activity as the x axis
# and show only the 'value_added' as the y axis
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity) ) +
  facet_wrap( year~ ., scales = 'free') +
  geom_col()

###### Exercise
# Make the bars horizontal
# so we can see the labels better
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity) ) +
  facet_wrap( year~ ., scales = 'free') +
  geom_col() +
  coord_flip()

###### Exercise
# Should you keep the scales 'free' on the y axis?
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity) ) +
  facet_wrap( year~ .) +
  geom_col() +
  coord_flip()

###### Exercise
# When plotting 'year' Vs 'activity' as facets,
# what can you see better?
# What kind of insights can you get with either graphs?

###### Exercise
# Use the activity as facets, the years as the x axis,
# and the 'value_added' as the y axis (again)
# but this time make it a line chart.
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot( aes(x=year, fill=activity) ) +
  facet_wrap( activity~ .) +
  geom_col(aes(y=output, fill=activity)) +
  geom_line(aes(y=value_added,),color = "black") 
  



###### Exercise
# Now plot both 'value_added' and 'output'


#####################################
# NARROW / WIDE DATA
#####################################

# If we want a plot of Output & Value Added as line charts,
# We better make the data 'narrow'
data_narrow <- data %>%
  filter(activity != list_activity[1]) %>%
  gather(labour, output, value_added, key='variable', value='value') 

# Initial data
data %>% 
  filter(activity != list_activity[1]) %>%
  glimpse

# Narrow
data_narrow %>% glimpse
data_narrow %>% head
data_narrow %>% tail

# Let's reconstruct the initial data
# a.k.a. let's make the data 'wide'
data_narrow %>%
  spread(variable, value) %>%
  glimpse

# Now we can make this type of chart
data_narrow %>%
  filter( variable != 'labour' ) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot(aes(year, value, color=variable)) +
  facet_wrap( activity~., scales = 'free') +
  geom_line()


###### Exercise
# Make the latest graphs as stacked bars.
data_narrow %>%
  filter( variable != 'labour' ) %>%
  mutate(activity = substr(activity, 1, 20) %>% paste0('...') ) %>%
  ggplot(aes(year, value, fill=variable)) +
  facet_wrap( activity~., scales = 'free') +
  geom_col()

###### Exercise
# Compare the range of values (y axis) of the barcharts & line graphs.
# What do you observe?
# Can you explain it?
# Which of the 2 graphs is most appropriate?

#####################################
# RADAR CHART
#####################################

# Let's take this graph (from a previous exercise)
# and make it a radar chart
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 10) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added) ) +
  facet_wrap( year~ ., scales = 'free') +
  coord_flip() +
  geom_col()

data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 10) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added) ) +
  facet_wrap( year~ .) +
  # We just change the coordinate system
  coord_polar() +
  geom_col()

data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 10) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity) ) +
  facet_wrap( year~ .) +
  coord_polar() +
  geom_col() +
  # We remove the labels
  theme(axis.text.x=element_blank())

data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 10) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity) ) +
  facet_wrap( year~ .) +
  coord_polar() +
  geom_col()  +
  # We make the backgrounds lighter
  theme_minimal() +
  theme(axis.text.x=element_blank())

data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 25) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity) ) +
  facet_wrap( year~ .) +
  coord_polar() +
  geom_col()  +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  # We clean the legends
  labs(x='',y='',
       title='Value added per sector of activity in the Dutch green economy')


# A customized radar plot
data %>%
  filter(activity != list_activity[1]) %>%
  mutate(activity = substr(activity, 1, 10) %>% paste0('...') ) %>%
  ggplot( aes(x=activity, y=value_added, fill=activity, color=activity) ) +
  facet_wrap( year~ .) +
  coord_polar() +
  # This is kind of a hack... also check color=activity above
  geom_point() + geom_area() +
  theme_minimal() +
  theme(axis.text.x=element_blank()) +
  labs(x='',y='',
       title='Value added per sector of activity in the Dutch green economy')

# Let's save this graph
ggsave('custom_radar.pdf', width=8, height=8)

##### Exercise
# If you remove 'color=activity' in the ggplot() function above,
# what happens?
# Then why should we use 'color=activity'?

##### Exercise
# If you remove 'fill=activity' in the ggplot() function above,
# what would happens?
# Then why should we use 'color=activity'?

##### Exercise
# If I would connect the dots of each radar chart,
# would it be a better visualization?



#####################################
# INTERACTIVE VISUALIZATION
#####################################
# https://plot.ly/r/
library(plotly)

iris %>% glimpse 
diamonds %>% glimpse

# An interactive scatterplot
iris %>%
  plot_ly( x = ~Sepal.Length, y = ~Petal.Length, 
           color = ~Species, colors = "Set1" )


# A customized scatterplot
d <- diamonds[sample(nrow(diamonds), 1000), ]

d %>%
  plot_ly( x = ~carat, y = ~price,
           color = ~carat, size = ~carat )



# A 3D scatterplot
d %>% 
  plot_ly(x = ~carat, y = ~depth, z = ~price, color = ~cut,
          colors = c('#BF382A', '#0C4B8E'), size=1) %>%
  add_markers() %>% 
  layout(scene = list( xaxis = list(title = 'Carat'),
                       yaxis = list(title = 'Depth'),
                       zaxis = list(title = 'Price') ))

##### Exercise
# Use other dimension on the x, y, and z axes.
# What patterns do you see in the data?
iris %>% 
  plot_ly(x = ~Petal.Length, y = ~Sepal.Width, z = ~Sepal.Length, color = ~Species ,
          colors = c('#BF382A', '#0C4B8E'), size=1) %>%
  add_markers() %>% 
  layout(scene = list( xaxis = list(title = 'Petal Length'),
                       yaxis = list(title = 'Sepal Width'),
                       zaxis = list(title = 'Sepal Length') ))

##### Exercise
# Plot the iris dataset in 3D


#####################################
# EXPLORE DATA DISTRIBUTION
#####################################

# A Scatterploit matrix
diamonds %>% glimpse
d %>% pairs()


# A correlation plot
library(corrplot)
d %>% 
  as_tibble %>%
  select(-cut, -color, -clarity) %>%
  cor %>%
  corrplot


