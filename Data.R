library(tidyverse)
library(skimr)

stocks <- read_csv('C://Users/aaron/Downloads/archive/data.csv')

# Which ones had the biggest standard deviation? (volatility)
# What stock would you recommend to someone looking to invest? 
# Are any stocks correlated with each other? 
# Does higher volume correlate with a larger increase in price?