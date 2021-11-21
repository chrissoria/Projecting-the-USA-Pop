library(tidyverse)
library(gganimate)
library(gapminder)
library(gifski)
library(dplyr)

#a column chart showing age proportions only for 2020
Klong %>% 
  filter(year == "2020") %>%
  ggplot(aes(x = year, y = proportion, group = age, fill = age)) +
  geom_col()
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Proportion of population in each age group over time")
  
#a bar chart showing age proportion for just 2020
Klong %>% 
  filter(year == "2020") %>%
  ggplot(aes(x = age, y = proportion, fill = age)) + 
    geom_bar(stat="identity", position = "dodge")+
    ggtitle("Proportion of population in each age group")+
    ylab("proportion")+
    coord_flip()

#proportion frame states by year (every 5)
#This animation will show the changing proportions of the United States using US rates

USfertility <- ggplot(Klong, aes(x = age, y = proportion, fill = age)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = 'year: {closest_state}', x = 'Age', y = 'Proportion') +
  coord_flip()

USfertility

USfertility.animation <- USfertility +
  transition_states(year) +
  shadow_wake(wake_length = .01) 

USfertility.animation

animate(USfertility.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 11)

#Next, I want to produce a chart of what the US would look like if it had Kenya's rates
#But first, I want to create a column and label this data "US Rates."
#This way I can present all on the same image, grouped

Klong$Rates_Applied <- "US Rates"
Klong2$Rates_Applied <- "SK Rates"
Klong3$Rates_Applied <- "Kenya Rates"
Klong4$Rates_Applied <- "Niger Rates"

#Next, I want to plot based on South Korea Fertility rates

SKfertility <- ggplot(Klong2, aes(x = age, y = proportion, fill = age)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = 'year: {closest_state}', x = 'Age', y = 'Proportion') +
  coord_flip()

SKfertility

SKfertility.animation <- USfertility +
  transition_states(year) +
  shadow_wake(wake_length = .01)

SKfertility.animation

animate(SKfertility.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 11)

#Kenya

Kfertility <- ggplot(Klong3, aes(x = age, y = proportion, fill = age)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = 'year: {closest_state}', x = 'Age', y = 'Proportion') +
  coord_flip()

Kfertility.animation <- Kfertility +
  transition_states(year) +
  shadow_wake(wake_length = .01)

Kfertility.animation

animate(Kfertility.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 10)

#Niger

Nfertility <- ggplot(Klong4, aes(x = age, y = proportion, fill = age)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = 'year: {closest_state}', x = 'proportion', y = 'life age') +
  ylab("proportion")+
  coord_flip()

Nfertility.animation <- Nfertility +
  transition_states(year) +
  shadow_wake(wake_length = .01)

Nfertility.animation

animate(Nfertility.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 10)

#Next, I want to plot the population size of the United States, applying US rates, over time

USPopGrowth <- ggplot(tot_pop, aes(x = as.numeric(year), y = pop, color="Oranges"))+
  geom_line(size=2)+
  geom_point()+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#add8e6"),
        panel.grid.minor = element_blank())+
  labs(title = "population growth", x = 'year', y = 'Population')


USPopGrowth

USPopGrowth.animation <- USPopGrowth +
  transition_reveal(as.numeric(year))+
  view_follow(fixed_y = TRUE)

USPopGrowth.animation

animate(USPopGrowth.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 10)

#Next,I want to plot the population of the size of the United Staes, applying Kenya rates

KenyaPopGrowth <- ggplot(tot_pop3, aes(x = as.numeric(year), y = pop, color="Oranges"))+
  geom_line(size=2)+
  geom_point()+
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "#000000"),
        panel.grid.minor = element_blank())+
  labs(title = "population growth", x = 'year', y = 'Population')


KenyaPopGrowth

KenyaPopGrowth.animation <- KenyaPopGrowth +
  transition_reveal(as.numeric(year))+
  view_follow(fixed_y = TRUE)

KenyaPopGrowth.animation

animate(KenyaPopGrowth.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 10, end_pause = 120)

#The US dependency ratio with US rates

DependencyLines <- ggplot(DependencyRatios, aes(x = as.numeric(year)))+
  geom_line(y= Dependecy.US, size=1.5, color="blue", alpha=.75)+
  geom_line(y=Dependecy.Kenya, size=1.5, color="red", alpha=.75)+
  geom_line(y=Dependecy.SK, size=1.5, color="orange", alpha=.75)+
  geom_line(y=Dependecy.Niger, size=1.5, color="darkgreen", alpha=.75)+
  ylim(.5,1.3)+
  theme(panel.background = element_rect(fill = "#000000"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill = "#FFFFFF"),
        )+
  labs(title = "Dependency Ratios Over Time", x = 'year', y = 'Dependency Ratio',
       subtitle = "geen/Niger, Orange/South Korea, Red/Kenya, Blue/US")

DependencyLines

DependencyLines.animation <- DependencyLines +
  transition_reveal(as.numeric(year))+
  view_follow(fixed_y = TRUE)

DependencyLines.animation

animate(DependencyLines.animation, height = 500, width = 800, fps = 30, res = 100, 
        duration = 12, end_pause = 120)