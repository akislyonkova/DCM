
library(psych)
library(GPArotation)
library(lavaan)
library(cluster)
library(factoextra)

disc <- read.csv("discdat.csv")
describe(disc)

test <- disc[c(1:450),]
train <- disc[c(451:901),]  

poly_cor <- polychoric(train)
rho <- poly_cor$rho

# Cluster analysis 
set.seed(2024) # for reproducibility
distance <- dist(rho)

hclust <- hclust(distance, method = "ward.D")
plot(hclust, main='Dendrogram for DISC data')
member <- cutree(hclust,4)
table(member)

kc <- kmeans(rho, 4)
kc

fviz_cluster(kc, data=rho)+theme_light()

# Factor analysis 
paralel <- fa.parallel(rho, fm = 'ml', fa='fa') 
fa.disc <- fa(train, nfactors = 4, fm = 'mle', rotate = 'oblimin', cor = "poly")
fa.disc$loadings
fa.disc$score.cor

# CFA

model.kmeans<-"
    F1=~AD1+AD2+AD3+AD4+AD5+AD6+AD7+AD8+AD9+AD10+AS10
    F2=~DO1+DO2+DO3+DO4+DO5+DO6+DO7+DO8+DO9+DO10
    F3=~SC1+SC2+SC3+SC4+SC5+SC6+SC7+SC8+SC9+SC10+AS4+AS1
    F4=~AS2+AS3+AS5+AS6+AS7+AS8+AS9
"
cfa.kmeans <- cfa(model.kmeans, data = test, ordered = T)
summary(cfa.kmeans, fit.measures = TRUE)


model.ward<-"
    F1=~AD1+AD2+AD3+AD4+AD5+AD6+AD7+AD8+AD9+AD10
    F2=~DO1+DO2+DO3+DO4+DO5+DO6+DO7+DO8+DO9+DO10
    F3=~SC1+SC2+SC3+SC4+SC5+SC6+SC7+SC8+SC9+SC10+AS1+AS4
    F4=~AS2+AS3+AS5+AS6+AS7+AS8+AS9+AS10
"
cfa.ward <- cfa(model.ward, data = test, ordered = T)
summary(cfa.ward, fit.measures = TRUE)


model.theory<-"
    F1=~AD1+AD2+AD3+AD4+AD5+AD6+AD7+AD8+AD9+AD10
    F2=~DO1+DO2+DO3+DO4+DO5+DO6+DO7+DO8+DO9+DO10
    F3=~SC1+SC2+SC3+SC4+SC5+SC6+SC7+SC8+SC9+SC10
    F4=~AS1+AS2+AS3+AS4+AS5+AS6+AS7+AS8+AS9+AS10
"
cfa.theory <- cfa(model.theory, data = test, ordered = T)
summary(cfa.theory, fit.measures = TRUE)

model.efa<-"
    F1=~AD1+AD2+AD3+AD4+AD5+AD6+AD7+AD8+AD9+AD10+AS10
    F2=~DO1+DO2+DO3+DO4+DO5+DO6+DO7+DO8+DO9+DO10+AS2+AS3+AS5+AS6
    F3=~SC1+SC2+SC3+SC5+SC6+SC7+SC8+SC9+AS4+AS1
    F4=~AS7+AS8+AS9+SC4+SC10
"
cfa.efa <- cfa(model.efa, data = test, ordered = T)
summary(cfa.efa, fit.measures = TRUE)

