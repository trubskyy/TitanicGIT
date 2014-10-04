
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
titanic[(
        titanic[,"Cabin"] != "" 
        & titanic[,"n_of_cabins"] == 0)
        , "n_of_cabins"
        ] <- 1 



linear <- lm(data=titanic, Survived ~ Pclass + Sex)

