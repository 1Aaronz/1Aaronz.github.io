stocks <- read_csv('C://Users/aaron/Downloads/archive/data.csv')

df_cleaned <- df %>%
  mutate(amount = str_replace_all(amount, "\\$", ""))

test <- stocks %>% 
  mutate(Open=str_replace_all(Open, "\\$",""), 
         High=str_replace_all(High, "\\$",""), 
         Close=str_replace_all(`Close/Last`, "\\$",""),
         Low=str_replace_all(Low, "\\$",""))

correlation_df <- as.data.frame(as.table(correlation_matrix))

ggplot(stocks, aes(y=PercentageChange, x=log10(Volume)))+
  geom_point(alpha=.1)+
  geom_smooth(method = "lm")+
  facet_wrap(~ Company, nrow = 5, scales = "free_y")+
  labs(x="Volume Traded (log10 of Volume)", y="Percentage Change")
