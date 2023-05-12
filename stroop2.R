#visualising data from "Cognitive dynamics of a single subject: 
#1428 Stroop tests and other measures in a mindfulness meditation context 
#over 2.5 years"
#effect of learning on the stroop task

setwd("C:/Users/piyal/OneDrive/Desktop/R/psy6422/datavizproject")

# install.packages("remotes")
remotes::install_github("hughjonesd/ggmagnify")
library(ggmagnify)


#load relevant libraries
library(tidyverse)
library(here)
library(ggplot2)


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
  #font <- "franklin gothic medium" #assign font family

  theme_minimal() %+replace%         #replacing the elements I want to change
  
  theme(
    
    #grid elements
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    #text elements
    plot.title = element_text(      #title
      #family = font,                #set font family     
      size = 18,                    #set font size
      face = 'bold',                #bold typeface
      hjust = 0,                    #left align
      vjust = 2),                   #raise slightly

    plot.subtitle = element_text(   #subtitle
      #family = font,                #font family
      size = 10),                   #font size
    
    axis.title = element_text(      #axis titles
      #family = font,                #font family
      size = 8),                    #font size
    
    axis.text = element_text(       #axis text
      #family = font,                #font family
      size = 8),                    #font size
    
    plot.caption = element_text(     #caption
      #family = font,                 #font family
      size = 8,                      #font size
      hjust = 1),                    #right align
    
    legend.position = c(.9,.75)  #positioning legend
    )
}

#What is the effect of meditation and task practice on the stroop effect?

#building a colour palette 
#based on the colours used in Stroops original paper

palette_stroop <- c("#ff0000", "#0085ff", "#3cff00", "#8c00db", "#543324")


#plotting reaction time for congruent and incongruent trials
p1 <- ggplot(data = stroop_long,
             mapping = aes(x = date,
                           y = time,
                           colour = condition))

plot1 <- p1 + geom_smooth(method = "lm", se = FALSE)+ 
  geom_point(alpha = 0.5) +
  labs(x = "Year",
       y = "Reaction Time in ms",
       title = "Distributed Practice Improves Performance on Stroop Task",
       caption = "Source: 'Cognitive dynamics of a single subject'\n Matti Toivo Juhani Heino")+
  theme_stroop() +
  scale_colour_manual(values = palette_stroop) +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y")+
  labs(colour = "Condition")

plot1
  
#wait, what?! Let's see that up close
#creating a magnified inset showing crossover

#target coordinates
tx0 = as.Date("2016-10-01", "%Y-%m-%d")
tx1 = as.Date("2016-11-30", "%Y-%m-%d")
ty0 = 80
ty1 = 120
target <- c(tx0, ty0, tx1, ty1)

#inset coordinates
ix0 = as.Date("2016-01-01", "%Y-%m-%d")
ix1 = as.Date("2016-12-01", "%Y-%m-%d")
iy0 = 150
iy1 = 200
inset <- c(ix0, iy0, ix1, iy1)  
  
plot1 + geom_magnify(from = target, to = inset)  




#coordinates of inset
x0 <- as.Date("2016-01-01", "%Y-%m-%d")
x1 <- as.Date("2016-10-01", "%Y-%m-%d")
y0 <- 150
y1 <- 200

#coordinates of origin
x3 <- as.Date("2016-05-01", "%Y-%m-%d")
x4 <- as.Date("2016-06-30", "%Y-%m-%d")
y3 <- 90
y4 <- 100

#the inset done manually
p2 <- ggplot(data = stroop_long, aes(x = date,
                                     y = time,
                                     colour = condition)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(alpha = 0.5)+ 
  xlim(x3, x4) +
  #ylim(y3, y4)+
  theme_stroop()+
  scale_colour_manual(values = palette_stroop)+
  theme(legend.position = "none")+
  theme(#panel.border = element_rect(fill = NA),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_blank())

  
plot1 + 
  annotation_custom(ggplotGrob(p2), 
                    xmin = x0,
                    xmax = x1,
                    ymin = y0,
                    ymax = y1) +
  
  geom_rect(aes(xmin = x0,              #border of inset
                xmax = x1,
                ymin = y0,
                ymax = y1),
              colour = "black",
                size = 1,
               alpha = 0) +
  
  geom_rect(aes(xmin = x3,              #border of section on graph
                xmax = x4,
                ymin = y3,
                ymax = y4),
              colour = "black",
                size = 1,
                alpha = 0) +
  
     annotate("segment", x = x3, xend = x0,   #connecting graph to inset
                         y = y4, yend = y0,   
             colour = "black",
             size = .8,
             linetype = 2) +
    annotate("segment", x = x4, xend = x1,    #connecting graph to inset
                        y = y4, yend = y0,
           colour = "black",
           size = .8,
           linetype = 2) 
  

df <- data.frame(x = c(x3, x0, x4, x1), y = c(y4, y0, y4,y0), grp = c(1,1,2,2))
  
#the path is not working  
geom_path(aes(x,y,group=grp), 
                data=data.frame(x = c(x3, x0, x4, x1), y=c(y3, y0, y4, y1),grp=c(1,1,2,2)),
                linetype="dashed")

geom_path(data = df,
          aes(x,y,group = grp),
          na.rm = FALSE)
