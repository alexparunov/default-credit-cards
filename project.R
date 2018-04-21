# database link: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients

api_installed <- require(rstudioapi)
if(!api_installed) install.packages("rstudioapi")

# Setting working directory as path of current file
setwd(dirname(getActiveDocumentContext()$path))

default_dataset <- read.table("default.csv", header=TRUE, na.strings="?", sep=";") #Importing the data set :D
default_dataset$ID <- NULL #Prescindible
summary(default_dataset)

#Pre processing missing values.
sum(is.na(default_dataset))

plot(default_dataset$PAY_AMT6)
sum(default_dataset$default.payment.next.month)
### We are trying to predict if one entry goes to default or not.

