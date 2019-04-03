library(tidyverse)
library(lubridate)
cherries <- read_csv("blossom_dates.csv")

# graphic reference here: http://opiateforthemass.es/articles/sakura/


#creates an separate column, adding beginning date and duration to create an end date for the festival...Cleans up the label names for ease of graphing.

# Data tidying -- Create Date object for Jan 1 of the year and then add the day integers to that to get the dates.
cherries <- cherries %>%
  setNames(c('year', 'peak', 'festival_start', 'duration')) %>%   
  # Clean up names
  mutate(festival_end = festival_start + duration)               
# Day of year of festival end

# Make a plot with a ribbon for the festival, points for the peak blooms, and a smoothing line for the peak blooms
# Label y axis with correctly formatted day

date_label_ticks <- 
  c('2019-03-15','2019-04-01','2019-04-15','2019-05-01','2019-05-15') 
# Can edit

###################### Creates a graphic with the dates of the blossom peak date and the cherry blossom festival. 

#from here: https://mcfromnz.wordpress.com/2014/06/02/shading-between-two-lines-ggplot/
#and thanks to Quentin Read for looking over and recommending label tweaks to the code.

  ggplot( cherries, aes( x = year)) +
  #creates the ribbon in between the two dates of the cherry blossom festival
  geom_ribbon(aes(ymin = festival_start, ymax = festival_end), fill = "#FFB7C5", color = NA, alpha = 0.5) + # Semitransparent +
  geom_point(aes(y= peak), color = 'red', size = 2) +
   #accessed from EPA here: https://www.epa.gov/sites/production/files/2016-08/cherry-blossoms_fig-1.csv
   #explaoins the LOESS Regression line https://en.wikipedia.org/wiki/Local_regression
  geom_smooth(aes(y=peak), method = "lm", se = FALSE, size = 1, color = "red") + coord_flip() + #flips graphic 180...you don't have to change anything else.
    
  #geom_smooth(aes(y=peak), method ='lm', se = FALSE)
  ggtitle("D.C. Tidal Basin cherry trees peak bloom date") +
#Can be added to make a line, instead of points.
#  geom_line(aes(cherries$Year, y= cherries$`days after Jan 1`)) +
  scale_y_continuous(name = 'day', labels = 
                         paste(month(date_label_ticks, label = TRUE), mday(date_label_ticks)), 
                       breaks = yday(date_label_ticks))+
 #removes, background border and grid lines. https://felixfan.github.io/ggplot2-remove-grid-background-margin/
  #removes just gray 
  theme_bw()  
  #removes everything ==  theme(panel.background = element_blank())

  # Very crude linear model
  fit <- lm(peak ~ year, data = cherries)
  summary(fit) # p value is  0.0305 ZOMG!!!! R squared is very very low though, so not much confidence in model
  slope <- fit$coefficients['year'] 
  slope
  # About 0.055 days earlier per year  -1/slope # Equals 1 day every 19ish years
  
## a separate NPS dataset with the actual dates. But didn't have the cherry blossom festival information. 
cherry_blossom <- read_csv("cherry_blossom_peak_bloom.csv")

# to report R value, I use the === Multiple R-squared:  0.04784

