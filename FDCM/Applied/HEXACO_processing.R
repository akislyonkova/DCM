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

HEXACO <- df_cleaned[sample(nrow(df_cleaned), 1000), ] 
write.table(HEXACO,'HEXACO.txt')

sinc <- HEXACO[,c(1:10)]
fair <- HEXACO[,c(11:20)]
gree <- HEXACO[,c(21:30)]
mode <- HEXACO[,c(31:40)]
 
H <- HEXACO[,c(1:40)]
write.table(H,'Honesty-Humility.txt')

H <- read.table("H.txt")


for (i in 1:4) {
  H[[paste0("total_scoreA", i)]] <- rowSums(H[, (10 * (i - 1) + 1):(10 * i)])
}

items_A1 <- H[, 1:10] 
A1 <- H$total_scoreA1
items_A2 <- H[, 11:20] 
A2 <- H$total_scoreA2
items_A3 <- H[, 21:30] 
A3 <- H$total_scoreA3
items_A4 <- H[, 31:40] 
A4 <- H$total_scoreA4

check  <- function(items, A) {
  correlation <- cor(items, A)
  return(correlation < 0)  
}
check(items_A1, A1)

H_rev <- H[,-c(41:44)]
rev_items <- c(1, 12, 13, 14, 15, 26, 27, 28, 29, 30, 31, 32, 33, 34)
H_rev[, rev_items] <- 8 - H_rev[, rev_items]

write.table(H_rev,'H_rev.txt')


