library(tidyverse)
library(skimr)

billboard <- read_csv('https://bcdanl.github.io/data/billboard.csv')

# Q1a
# Describe how the distribution of rating varies across week 1, week 2, and week 3 using the faceted histogram.
head(billboard)

ggplot(billboard)+
  geom_histogram(aes(x=))

q1a <- billboard %>% 
  pivot_longer(cols=wk1:wk76,
               names_to = "week",
               values_to="rating") %>% 
  filter(week %in% c('wk1','wk2','wk3'))

ggplot(q1a, aes(x=rating))+
  geom_histogram()+
    facet_wrap(.~ week)

q2a
ny_pincp <- read_csv('https://bcdanl.github.io/data/NY_pinc_wide.csv')
# Make ny_pincp longer

g2a <- ny_pincp %>% 
  pivot_longer(longer(cols=pincp1969:pincp2019,
                      names_to="year",
                      values_to = "pincp"))



Q2c <- ggplot(Q2b, aes(x = posteam, y = pass)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



