library(dplyr)

dark3 <- read.csv("sd3_data.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
table(dark3$country)

dark3 <- dark3[sample(nrow(dark3), 1000), ]
respMatrix <- dark3[,-c(28:29)]
respMatrix[respMatrix=="0"]<-1
write.table(respMatrix, "dark3.txt")

# Checking if the items were reversed as intended 

d3 <- read.table("dark3.txt")

head(d3)
colnames(d3) <- paste0("i_", seq(1, 27))
for (i in 1:3) {
  d3[[paste0("total_scoreA", i)]] <- rowSums(d3[, (9 * (i - 1) + 1):(9 * i)])
}

items_A1 <- d3[, 1:9] 
A1 <- d3$total_scoreA1
items_A2 <- d3[, 10:18] 
A2 <- d3$total_scoreA2
items_A3 <- d3[, 19:27] 
A3 <- d3$total_scoreA2

check  <- function(items, A) {
  correlation <- cor(items, A)
  return(correlation < 0)  
}
check(items_A3, A3)


reverse_items <- c(11, 15, 17, 20, 24, 25)
d3_copy <- d3
d3[, reverse_items] <- 6 - d3[, reverse_items]
d3 == d3_copy
d3 <- d3[,-c(28:30)]
write.table(d3, "dark3.txt")






