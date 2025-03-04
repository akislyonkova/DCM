# Data pre processing 
df <- read.csv('data.csv')
summary(df)

df_items <- df[,-c(37:39)] #removes, age, gender, and country
summary(df_items)
hist(df_items$Q1) #some inputs include 0
which(df_items$Q1 == 0)

df_cleaned <- df_items[apply(df_items, 1, function(row) all(row != 0)), ] #removes all rows with 0
summary(df_cleaned)
hist(df_cleaned$Q1)

ECR <- df_cleaned[sample(nrow(df_cleaned), 1000), ] # randomly sample 1000 participants from the clean df
write.table(ECR,'ECR.txt')

# Item correlations 
library(polycor)
cor_matrix <- round(hetcor(ECR)$correlations,2) #calculate poly correlations, round to 2 decimal

library(corrplot)
corrplot(cor_matrix[1:12,], diag = F, addCoef.col = 'black')
corrplot(cor_matrix[13:24,13:36], diag = F, addCoef.col = 'black')
corrplot(cor_matrix[25:36,25:36], diag = F, addCoef.col = 'black')
