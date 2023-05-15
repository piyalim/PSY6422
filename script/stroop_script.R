#visualising data from "Cognitive dynamics of a single subject: 
#1428 Stroop tests and other measures in a mindfulness meditation context over 2.5 years"
#effect of learning on the stroop task

setwd("C:/Users/piyal/OneDrive/Desktop/R/psy6422/datavizproject")

#load relevant libraries
library(tidyverse)
library(here)
library(ggplot2)

#tried to use library(ggmagnify)
#did not work!

#---------------IMPORT DATA---------------

#load the data
source <- here("data", "cognitive_dynamics_heino.csv")
stroop <- read.csv(source)

#-----------------TIDY DATA------------------

#the column names of the original dataset are long and unweildly
#changing them to make them user/coder friendly
stroop_clean <- stroop %>%
  select(date, stroop_congruent_pre_meditation, stroop_incongruent_pre_meditation) %>%
   rename(congruent = stroop_congruent_pre_meditation,
         incongruent = stroop_incongruent_pre_meditation)

#date is a character, needs to be made into date format
stroop_clean <- stroop_clean %>% 
  mutate(date = dmy(date)) %>%                 #changing string to date
  filter(date > "2014-07-01", na.rm = TRUE)    #deleting outliers


#changing centiseconds to milliseconds
stroop_clean <- stroop_clean %>% 
  mutate(congruent = congruent * 10,
         incongruent = incongruent * 10)

#reshaping dataframe to long
stroop_long <- stroop_clean %>% select(date, congruent, incongruent) %>% 
  gather(condition, time, congruent, incongruent)

#-------------------MAKE THE PLOTS-------------------
#defining the theme
theme_stroop <- function(){
  
  theme_minimal() %+replace%         #replacing the elements I want to change
    
    theme(
      
      #grid elements
      panel.grid.minor = element_blank(),
      
      #text elements
      plot.title = element_text(      #title
        size = 15,                    #set font size
        face = 'bold',                #bold typeface
        hjust = 0,                    #left align
        vjust = 2),                   #raise slightly
      
      plot.subtitle = element_text(   #subtitle
        size = 10,                    #font size
        hjust = 0),                   #left align
      
      axis.title = element_text(      #axis titles
        size = 8),                    #font size
      
      axis.text = element_text(       #axis text
        size = 8),                    #font size
      
      plot.caption = element_text(    #caption
        size = 8,                     #font size
        hjust = 1),                   #right align
      
      legend.position = c(.9,.8),  #positioning legend
      
      plot.background = element_rect(fill = "white", color = NA)
    )
}

#building a colour palette based on the original Stroop colours

palette_stroop <- c("#ff0000", "#0085ff", "#3cff00", "#8c00db", "#543324")


#plotting reaction time for congruent and incongruent trials
#plotting reaction time for congruent and incongruent trials

#creating the basic plot of time of trials across the years
p1 <- ggplot(data = stroop_long,
             mapping = aes(x = date,
                           y = time,
                           colour = condition))

plot1 <- p1 + 
  geom_smooth(method = "lm", se= FALSE)+                                  #drawing the scatter plot
  geom_point(alpha = 0.5) +
  labs(x = "Year",                                                        #labelling the graph
       y = "Time of trials in ms",
       title = "Distributed Practice Improves Performance on Stroop Task",
       subtitle = "Time taken for each Stroop trial over 2.5 years",
       caption = "Source: 'Cognitive dynamics of a single subject'\n Matti Toivo Juhani Heino")+
  theme_stroop() +                                                        #using the theme defined earlier
  scale_colour_manual(values = palette_stroop) +                          #using the colours defined earlier
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+          #defining the breaks on the x-axis
  labs(colour = "Condition")

plot1
  
#wait, what?! Let's see that up close
#creating a magnified inset showing crossover

#would've been easy with ggmagnify

#creating it manually
#coordinates of inset
x0 <- as.Date("2016-01-01", "%Y-%m-%d")
x1 <- as.Date("2016-09-01", "%Y-%m-%d")
y0 <- 140
y1 <- 180

#coordinates of origin
x3 <- as.Date("2016-04-01", "%Y-%m-%d")
x4 <- as.Date("2016-06-30", "%Y-%m-%d")
y3 <- 90
y4 <- 100

#coordinates of section to subset
x01 <- as.Date("2016-04-01", "%Y-%m-%d")
x02 <- as.Date("2016-06-15", "%Y-%m-%d")

y01 <- 70
y02 <- 140


#the inset done manually
p2 <- ggplot(data = stroop_long, aes(x = date,                    #basic plot
                                     y = time,
                                     colour = condition)) +
  geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +        #regression line
  xlim(x01, x02) +                                                #subsetting the part that crosses over
  theme_stroop()+                                                 #use our theme from before
  scale_colour_manual(values = palette_stroop)+                   #using the colours from before
  theme(legend.position = "none") +                               #removing the legend in the inset
  theme(panel.background = element_rect(fill = "ghostwhite"),     #giving it a background to highlight it
        axis.text.y = element_blank(),                            #removing the y-axis text
        axis.title = element_blank())                             #removing the titles from the axes


#adding the inset to the original plot  
plot2 <- plot1 + 
  annotation_custom(ggplotGrob(p2),    #adding the inset
                    xmin = x0,         #defining the coordinates for the inset
                    xmax = x1,
                    ymin = y0,
                    ymax = y1) +
  
  geom_rect(aes(xmin = x0,              #border of inset
                xmax = x1,
                ymin = y0,
                ymax = y1),
            colour = "black",
            linewidth = .6,
            alpha = 0) +
  
  geom_rect(aes(xmin = x3,              #border highlighting the section on graph
                xmax = x4,
                ymin = y3,
                ymax = y4),
            colour = "black",
            linewidth = .6,
            alpha = 0) +
  
  annotate("segment", x = x3, xend = x0,   #connecting graph to inset
           y = y4, yend = y0,   
           colour = "black",
           linewidth = .6,
           linetype = 2) +
  annotate("segment", x = x4, xend = x1,    #connecting graph to inset
           y = y4, yend = y0,
           colour = "black",
           linewidth = .6,
           linetype = 2) 

plot2 

ggsave(here("figs", "stroop_practice_inset.png"), 
       plot = plot2, width = 25, height = 15, units = "cm")


