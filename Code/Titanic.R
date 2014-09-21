
library("ggplot2")
setwd("C:/Users/mykha_000/SkyDrive/Kaggle/Titanic")

data.dir <- ".//Data"

titanic <- read.csv(file = ".//Data//train.csv",stringsAsFactors = FALSE)

class <- factor(titanic[,"Pclass"])

linear <- lm(data=titanic, Survived ~ Pclass + Sex)

new line 