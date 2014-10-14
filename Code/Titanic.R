library("ggplot2")
library("ggvis")


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

titanic[,"title"] <- sapply(titanic$Name
                            , FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}
                            )

titanic$title <- sub(' ', '', titanic$title)

titanic[
        which(titanic[,"title"] %in% c("Capt"
                                       ,"Col"
                                       ,"Don"
                                       ,"Dr"
                                       ,"Jonkheer"
                                       ,"Major"
                                       ,"Master"
                                       ,"Rev"
                                       ,"Sir"
                                       )
              )
        , "title"] <- "sMr"

titanic[
  which(titanic[,"title"] %in% c("Lady"
                                 ,"Mlle"
                                 ,"Mme"
                                 ,"Ms"
                                 ,"the Countess"
                                )
  )
  , "title"] <- "sMs"

titanic[is.na(titanic[,"Age"])
		,"Age"] <- -99

titanic[,"age.bins"] <- cut(titanic[,"Age"]
                            , breaks = c(-100,0,10,20,30,40,50,60,100)
                            , include.lowest = TRUE
                            , na.action = na.fail
                            )

titanic[,"fare.bins"] <- cut(titanic[,"Fare"]
							 , breaks = c(0,10,20,40,100, max(titanic[,"Fare"]))
							 , include.lowest = TRUE)

char.ix <- grep("[A-Z a-z]",titanic[,"Ticket"],perl=TRUE)

titanic[char.ix ,"char.ticket"] <- 1
titanic[-char.ix,"char.ticket"] <- 0


linear <- lm(data=titanic, Survived ~ Pclass + Sex+n_of_cabins)

