# Data pre processing 
df <- read.csv('data.csv',  header = T, sep = "\t", stringsAsFactors = T)
summary(df)

df_items <- df[,-c(241:244)] 
summary(df_items)

df_cleaned <- df_items[apply(df_items, 1, function(row) all(row != 0)), ] 
summary(df_cleaned)

table(df_cleaned$HSinc1) 
table(df_cleaned$HFair9)
table(df_cleaned$OUnco6)
