# Data pre processing 
df <- read.csv('data.csv',  header = T, sep = "\t", stringsAsFactors = T)
summary(df)

df_items <- df[, seq(1, ncol(df), by = 3)]
summary(df_items)
df_items <- df_items[,c(1:56)]
summary(df_items)



FTI <- df_items[sample(nrow(df_items), 1000), ] 
write.table(FTI,'FTI.txt')

