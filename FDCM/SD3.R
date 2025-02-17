library(dplyr)

dark3 <- read.csv("sd3_data.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
table(dark3$country)

dark3 <- dark3[sample(nrow(dark3), 1000), ]
respMatrix <- dark3[,-c(28:29)]
respMatrix[respMatrix=="0"]<-1
write.table(respMatrix, "dark3.txt")




GB <- filter(dark3, country == 'GB')
CA <- filter(dark3, country == 'CA')
