
library("ggplot2")

setwd("C://Users//mykha_000//SkyDrive//Kaggle//TitanicGIT")

data.dir <- ".//Data"

titanic <- read.csv(file = ".//Data//train.csv",stringsAsFactors = FALSE)
test <-    read.csv(file = ".//Data//test.csv",stringsAsFactors = FALSE)

# recoding variables 
titanic[,"class"] <- factor(titanic[,"Pclass"])


countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }
titanic[,"n_of_cabins"] <- {
                            ((countSpaces(titanic[,"Cabin"]) == 0) * 0 ) + 
                            ((titanic[,"Cabin"] != "" 
                               & countSpaces(titanic[,"Cabin"]) == 0) * 1) + 
                            ((titanic[,"Cabin"] != "" 
                               & countSpaces(titanic[,"Cabin"]) > 0) * 
                              (countSpaces(titanic[,"Cabin"]) + 1)  )
                            }

titanic[,"title"] <- sapply(titanic$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
titanic$title <- sub(' ', '', titanic$title)

library(car)
scatterplot.matrix( ~ Survived + Age + Pclass | Sex, data=titanic)

linear <- lm(data=titanic, Survived ~ Pclass + Sex+n_of_cabins)

