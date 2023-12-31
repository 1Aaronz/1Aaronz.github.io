library(tidyverse)
library(skimr)

stocks <- read_csv('C://Users/aaron/Downloads/archive/data.csv')

# Which ones had the biggest standard deviation? (volatility)
# What stock would you recommend to someone looking to invest? 
# Are any stocks correlated with each other? 
# Does higher volume correlate with a larger increase in price?



# Data manipulation -----------------------------------------------------------------

stocks <- read_csv('C://Users/aaron/Downloads/archive/data.csv')

#First we change the values to be numerical 
stocks <- stocks %>% 
  mutate(Open=as.numeric(str_replace_all(Open, "\\$","")), 
         High=as.numeric(str_replace_all(High, "\\$","")), 
         Close=as.numeric(str_replace_all(`Close/Last`, "\\$","")),
         Low=as.numeric(str_replace_all(Low, "\\$","")), 
         Date=str_replace_all(Date, "\\/", "-"))%>% 
  mutate(Change=Close-Open) %>% 
  mutate(PercentageChange = ((Close - Open) / Open) * 100) %>% 
  group_by(Company)


# Graph 1 -----------------------------------------------------------------

ggplot(stocks, aes(y = PercentageChange, x = log10(Volume))) +
  geom_point(alpha = 0.1, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(~ Company, scales="free_y") +
  labs(
    title = "Percentage Change vs. Volume",
    x = "Log10 of Volume",
    y = "Percentage Change",
    caption = "Blue points represent data, Red line represents linear fit"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    strip.text = element_text(size = 12),
    legend.position = "none"  # Remove legend for better clarity
  )

#It appears that for all companies, any correlation between volume and change in price is negligible
#Tesla has the highest positive correlation, while Microsoft has the highest negative correlation

# Different graph --------------------------------------------------------------

correlation_by_group <- stocks %>%
  group_by(Company) %>%
  summarise(correlation = cor(PercentageChange, Volume))

ggplot(correlation_by_group, aes(x=Company, y=correlation))+
  geom_col()+
  labs(title= "How strongly does Volume and Percentage Change in Price Correlate?")




