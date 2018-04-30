# database link: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients

#####LIBRARIES######
if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(mice)) install.packages("mice")

# Setting working directory as path of current file
setwd(dirname(getActiveDocumentContext()$path))

Default_Dataset <- read.table("default.csv", header=TRUE, na.strings="?", sep=";") #Importing the data set
Default_Dataset$ID <- NULL #Prescindible

#####DESCRIPTION OF THE DATA SET########
# ID: ID of each client
# LIMIT_BAL: Amount of given credit in NT dollars (includes individual and family/supplementary credit
# SEX: Gender (1=male, 2=female)
# EDUCATION: (1=graduate school, 2=university, 3=high school, 4=others, 5=unknown, 6=unknown)
# MARRIAGE: Marital status (1=married, 2=single, 3=others)
# AGE: Age in years
# PAY_0: Repayment status in September, 2005 (-1=pay duly, 1=payment delay for one month, 2=payment delay for two months, ... 8=payment delay for eight months, 9=payment delay for nine months and above)
# PAY_2: Repayment status in August, 2005 (scale same as above)
# PAY_3: Repayment status in July, 2005 (scale same as above)
# PAY_4: Repayment status in June, 2005 (scale same as above)
# PAY_5: Repayment status in May, 2005 (scale same as above)
# PAY_6: Repayment status in April, 2005 (scale same as above)
# BILL_AMT1: Amount of bill statement in September, 2005 (NT dollar)
# BILL_AMT2: Amount of bill statement in August, 2005 (NT dollar)
# BILL_AMT3: Amount of bill statement in July, 2005 (NT dollar)
# BILL_AMT4: Amount of bill statement in June, 2005 (NT dollar)
# BILL_AMT5: Amount of bill statement in May, 2005 (NT dollar)
# BILL_AMT6: Amount of bill statement in April, 2005 (NT dollar)
# PAY_AMT1: Amount of previous payment in September, 2005 (NT dollar)
# PAY_AMT2: Amount of previous payment in August, 2005 (NT dollar)
# PAY_AMT3: Amount of previous payment in July, 2005 (NT dollar)
# PAY_AMT4: Amount of previous payment in June, 2005 (NT dollar)
# PAY_AMT5: Amount of previous payment in May, 2005 (NT dollar)
# PAY_AMT6: Amount of previous payment in April, 2005 (NT dollar)
# default.payment.next.month: Default payment (1=yes, 0=no)

#Pre processing missing values.
sum(is.na(Default_Dataset))
#There are no missing values in the whole data set. 
plot(Default_Dataset$PAY_AMT6)

summary(Default_Dataset)

#######PRE PROCESSING ##########

# Set SEX as factor and change levels for more clarity
Default_Dataset$SEX <- as.factor(Default_Dataset$SEX)
levels(Default_Dataset$SEX) <- c("male","female")

# MARRIAGE can't be 0, so it's wrong data. Set it as NA and then perform imputation
Default_Dataset$MARRIAGE[Default_Dataset$MARRIAGE == 0] <- NA

# EDUCATION should be in range [1,4], so any other value is wrong. 
Default_Dataset$EDUCATION[Default_Dataset$EDUCATION < 1 | Default_Dataset$EDUCATION > 4] <- NA

# Imputing NAs
imp <- mice(Default_Dataset, m = 1)
Default_Dataset <- complete(imp)

# Set MARRIAGE as factor and change levels for more clarity
Default_Dataset$MARRIAGE <- as.factor(Default_Dataset$MARRIAGE)
levels(Default_Dataset$MARRIAGE) <- c("married","single","other")

# Set EDUCATION as factor and change levels for more clarity
Default_Dataset$EDUCATION <- as.factor(Default_Dataset$EDUCATION)
levels(Default_Dataset$EDUCATION) <- c("graduate school", "university", "high school", "other")

# Set default.payment.next.month as factor and change levels for more clarity
Default_Dataset$default.payment.next.month <- as.factor(Default_Dataset$default.payment.next.month)
levels(Default_Dataset$default.payment.next.month) <- c("no","yes")


# PAY_0,2,3,4,5,6 are categorical variables described above. So we should set them as factors and give proper names

for(i in 6:11) {
  Default_Dataset[,i] <- as.factor(Default_Dataset[,i])
  levs <- c("pay duly", "pay duly", "pay duly") # those are -2, -1, 0
  for(j in 1:8) {
    st <- paste("delay",j,"months")
    levs <- c(levs,st)
  }
  levels(Default_Dataset[,i]) <- levs
}

# Saved pre-processed data set for future preprocessing
save(Default_Dataset, file = "Default_dataset_preprocessed.Rdata")

# We can load data set directly and skip above given steps
load("Default_dataset_preprocessed.Rdata")
summary(Default_Dataset)

#####SOME PLOTS AND OTHER THINGS IN ORDER TO HAVE A CLEAR IDEA OF THE DATA#######

#The second column are the individuals that will default. As we can see, most of them are from low education, but, there still some outliers.
ggplot(Default_Dataset, aes(default.payment.next.month, EDUCATION)) +
  geom_jitter(aes(color = EDUCATION), size = 0.5)

#In this plot we can see that the marital status really do not make a difference and dont tell anything in particular relating the default of the credit card.
# Here we can see that there are some values for 0 marriage, those values are incorrect and must be corrected.
ggplot(Default_Dataset, aes(default.payment.next.month, MARRIAGE)) +
  geom_jitter(aes(color = MARRIAGE), size = 0.5)

#Same that before but with sex
ggplot(Default_Dataset, aes(default.payment.next.month, SEX)) +
  geom_jitter(aes(color = SEX), size = 0.5)


########PCA#########
if(!require(FactoMineR)) install.packages("FactoMineR")

PCADefault <- PCA(Default_Dataset,quali.sup = 24)
PCADefault
#Best and worst represented.
cos1 = PCADefault$ind$cos2 #Return the cos of the individuals
which.max(cos1[,1]) #The best represented individual is the number 27222.
which.min(cos1[,1]) #The worst represented individual is the number 7335

#Contributions
contribution <- PCADefault$ind$contrib

bestinfirstPC <- sort(contribution[,1],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(Contribution) in the first principal component
bestinfirstPC

bestinsecondPC <- sort(contribution[,2],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(contribution) in the second principal component
bestinsecondPC

#Best represented variables
cos2 = PCADefault$var$cos2 #Returns the cos of the variables.
which.max(cos2[,1]) #BILL_AMT4 is the best represented variable in the first factorial plame
which.min(cos2[,1]) #Marriage is the worst represented variable in the first factorial plame

#Most influencial variables
contribution2 <- PCADefault$var$contrib

bestinfirstPCvar <- sort(contribution2[,1],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(Contribution) in the first principal component
bestinfirstPCvar

bestinsecondPCvar <- sort(contribution2[,2],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(contribution) in the second principal component
bestinsecondPCvar
