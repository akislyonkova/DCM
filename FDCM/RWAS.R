library(dplyr)

rwas <- read.csv("data.csv", header = TRUE, stringsAsFactors = F)
auth <- rwas[, c(1:22)]
big5 <- rwas[, c(48:57)]

table(auth$Q1)
table(big5$TIPI1)

# both scales have '0', missing values

df <- cbind(auth,big5)
df <- df[apply(df, 1, function(row) all(row != 0)), ] #removes all rows with 0
summary(df)

auth_clean <- df[, c(1:22)]
big5_clean <- df[, c(23:32)]

# in auth K=9, in big5 K=7
# merge adjesent response categories in auth

auth_clean[auth_clean=="2"] <- 1 # merge "Very strongly disagree" and "Strongly disagree"
table(auth_clean$Q1)

auth_clean[auth_clean=="9"] <- 8 # merge "Very strongly agree" and "Strongly agree"
table(auth_clean$Q1)

auth_recoded <- auth_clean %>%
  mutate_all(~ ifelse(. != 1, . - 1, .))
table(auth_recoded$Q1)

df_clean <- cbind(auth_recoded, big5_clean)
summary(df_clean)
hist(df_cleaned$Q1)




