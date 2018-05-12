# database link: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients


#####LIBRARIES######
if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(mice)) install.packages("mice")
if(!require(mclust)) install.packages("mclust") #Nice clustering library, guide: http://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html
if(!require(plyr)) install.packages("plyr") #"Library for the creation of the new variables."
set.seed(1)
# Setting working directory as path of current file
setwd(dirname(getActiveDocumentContext()$path))
####LOAD ENVIROMENT#####
load("Default_dataset_preprocessed1.Rdata")
#######BEGIN PREPROCESED#######
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
#NEW VARIABLES
#We create 6 new variables that tell if an individual have good account status, paid duly, paid the minimum or have some delay in the payments.
#Those variables can have 4 values. After a very long investigation, even posting on the data set forum, we have made some assumption after inspecting the data.
#-2 : Good Account Status
#-1 : Pay-duly
# 0 : Pay minimum
# else: Delay
#Those new variables will be a factor.
Default_Dataset$PAYSTATUS_0 <- NA
Default_Dataset$PAYSTATUS_2 <- NA
Default_Dataset$PAYSTATUS_3 <- NA
Default_Dataset$PAYSTATUS_4 <- NA
Default_Dataset$PAYSTATUS_5 <- NA
Default_Dataset$PAYSTATUS_6 <- NA

getPaymentStatus <- function(row) {
  for (n in c(0,2,3,4,5,6)) {
    varPay <- paste("PAY_", n, sep = '')
    varStatus <- paste("PAYSTATUS_", n, sep = '')
    if(row[varPay]==-2){
      row[varStatus] = "Good account status" 
      row[varPay]=0
    }
    else if(row[varPay]==-1){
      row[varStatus] = "Pay-duly" 
      row[varPay]=0
    }
    else if(row[varPay]==0){
      row[varStatus] = "Pay minimum" 
    }
    else {
      row[varStatus] = "Delay" 
    }
  }
  return(row)
}
Default_Dataset <- adply(Default_Dataset, 1, getPaymentStatus)
for(i in c(0,2,3,4,5,6))
{
  varStatus <- paste("PAYSTATUS_", i, sep = '')
  Default_Dataset[,varStatus] = as.factor(Default_Dataset[,varStatus])
}
# Saved pre-processed data set for future preprocessing
save(Default_Dataset, file = "Default_dataset_preprocessed1.Rdata")

# We can load data set directly and skip above given steps
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
#Same with the payStatus0
ggplot(Default_Dataset, aes(default.payment.next.month, PAYSTATUS_4)) +
  geom_jitter(aes(color = SEX), size = 0.5)

########PCA#########
if(!require(FactoMineR)) install.packages("FactoMineR")

PCADefault <- PCA(Default_Dataset,quali.sup = c(2,3,4,5,24,25,26,27,28,29,30))
PCADefault
#We can see that the PCA shows that all the variables PAY_X are correlated in an inmense way, so, we could unify this variables into one unic variable.
#We will unify the variables in this way:
#The variables PAYSTATUS_X, as those variables are related with PAY_X will be transformed to
#The variables will be transformed into:
#PAY_X --> Delay. This variable will contain the MEAN delay of this individual in the payments.
#PAY_STATUSX--> AccountStatus. This variable will contain the MOST COMMON status in the account for this individual.
#As the data have to be refactored, we will inspec the PCA once the data is refactored.
######DATA REFACTOR######
Default_Dataset$Delay = ""
Default_Dataset$AccountStatus = ""
refactorDataset <- function(row) {
  valuePay = 0
  valueStatus = 0
  for (n in c(0,2,3,4,5,6)) {
    varPay <- paste("PAY_", n, sep = '')
    varStatus <- paste("PAYSTATUS_", n, sep = '')
    #We assign more value to a good condition and less to a bad condition. In the end we will do a floor of the mean to pick the worst escenario.
    if(as.numeric(row[varStatus])==1){
      valueStatus = valueStatus +1
    }
    else if(as.numeric(row[varStatus])==2){
      valueStatus = valueStatus +4
      
    }
    else if(as.numeric(row[varStatus])==3){
      valueStatus = valueStatus +2
    }
    else {
      valueStatus = valueStatus +3
    }
    valuePay = as.numeric(row[varPay]) + valuePay
  }
  row["Delay"] = ceiling(valuePay/6)
  row["AccountStatus"] = floor(valueStatus/6) #We assign an status that is mean of their status. Using the floor function.
  return(row)
}
Default_Datasetv2 <- adply(Default_Dataset, 1, refactorDataset)#Apply as before
#Delete old columns
Default_Datasetv2 = Default_Datasetv2[,-c(6,7,8,9,10,11,25,26,27,28,29,30)]
#Make factor account status
Default_Datasetv2$AccountStatus = as.factor(Default_Datasetv2$AccountStatus) 
levels(Default_Datasetv2$AccountStatus) <- c("Delay","Paid minimum","Paid Duly","Good Status")
#######PCA SECOND VERSION ##########
PCADefault2 <- PCA(Default_Datasetv2,quali.sup = c(2,3,4,5,18,20))
PCADefault2
#That PCA make a lot of sense, the payments are inversely correlated with the Delay, it makes sense, because if you have more delay, mean that you pay less
#Also inversely correlate with the limit of credit.
#The variables BILL_ATMX and PAY_ATMX are very correlated with themselves, Also make sense.
#Best and worst represented.
cos1 = PCADefault2$ind$cos2 #Return the cos of the individuals
which.max(cos1[,1]) 
which.min(cos1[,1]) 

#Contributions
contribution <- PCADefault2$ind$contrib

bestinfirstPC <- sort(contribution[,1],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(Contribution) in the first principal component
bestinfirstPC

bestinsecondPC <- sort(contribution[,2],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(contribution) in the second principal component
bestinsecondPC

#Best represented variables
cos2 = PCADefault2$var$cos2 #Returns the cos of the variables.
which.max(cos2[,1]) #BILL_AMT4 is the best represented variable in the first factorial plame
which.min(cos2[,1]) #PAY_2 is the worst represented variable in the first factorial plame

#Most influencial variables
contribution2 <- PCADefault2$var$contrib

bestinfirstPCvar <- sort(contribution2[,1],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(Contribution) in the first principal component
bestinfirstPCvar

bestinsecondPCvar <- sort(contribution2[,2],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(contribution) in the second principal component
bestinsecondPCvar

#Significant dimensions
dim <- sum(as.numeric(PCADefault2$eig[,3] <= 80)) #7 significant dimensions

#NIPALS ALGORITHM TO OBTAIN THE 7 PRINCIPAL COMPONENTS
nipals <- function(X, rankX = 7, suppl.col =  c(2,3,4,5,24,25,26,27,28,29,30), eps = 1e-06) {
  X <- as.matrix(X[,-suppl.col])
  
  # Center Matrix X
  for(i in 1:ncol(X)) {
    X[,i] <- (X[,i] - mean(X[,i]))/(sqrt(var(X[,i])))
  }
  X0 <- X
  # Projections of variables
  Phi <- matrix(0, nrow = ncol(X0), ncol = rankX)
  # Projections of individuals
  Psi <- matrix(0, nrow = nrow(X0), ncol = rankX)
  # sqrt(eigenvalues of t(X) %*% X)
  singular_values <- rep(0, length = rankX)
  
  # Initial vector u for determining convergence
  for(h in 1:rankX) {
    u0 <- rep(0, ncol(X0))
    psi <- as.matrix(rowMeans(X0))
    u <- as.numeric(t(X0) %*% psi)
    
    # Initial distance is simply distance to origin, since u0 is a 0 vector.
    dist <- sqrt(as.numeric(u%*%u))
    while(dist > eps) {
      u <- as.numeric(t(X0) %*% psi)
      
      # normalize vector u
      u <- u/sqrt(u %*% u)
      psi <- X0 %*% as.matrix(u)
      
      # Update variables for next iteration
      dist <- sqrt(sum((u - u0)^2))
      u0 <- u
    }
    
    singular_values[h] <- sqrt(sum(psi*psi))
    # Direct formula of finding projection of variable
    Phi[,h] <- singular_values[h]*as.matrix(u)
    # Direct formula of finding projection of individuals
    Psi[,h] <- X0 %*% as.matrix(u)
    
    # deflation
    X0 <- X0 - psi %*% t(as.matrix(u))
  }
  
  # Find SVD of X. U = Standardized Phi
  Phi_stand <- Phi
  for(i in 1:ncol(Phi_stand)) {
    Phi_stand[,i] <- Phi[,i]/(sqrt(Phi[,i] %*% Phi[,i]))
  }
  U <- Phi_stand
  
  # Standardize Psis, so that we can find V = Psi_stand.
  Psi_stand <- Psi
  for(i in 1:ncol(Psi_stand)) {
    Psi_stand[,i] <- Psi[,i]/sqrt(Psi[,i] %*% Psi[,i])
  }
  
  V <- Psi_stand
  
  rownames(Phi) <- colnames(X0)
  rownames(Psi) <- rownames(X0)
  rownames(U) <- colnames(X0)
  rownames(V) <- rownames(X0)
  colNames <- vector(length = ncol(Phi))
  
  for(i in 1:length(colNames)) {
    colNames[i] <- paste("Dim",i)
  }
  colnames(Phi) <- colNames
  colnames(Psi) <- colNames
  
  svd <- list(U = U, V = V)
  
  return(list(Phi = Phi_stand, Psi = Psi, singular_values = singular_values,
              svd = svd))
}

#execute nipals algorithm
nip_res_default = nipals(Default_Dataset)
# In order to plot biplots we need to use matrices U and V returned from niples function

var_prj <- nip_res_default$Phi
ind_prj <- nip_res_default$Psi
U <- nip_res_default$svd$U
V <- nip_res_default$svd$V

# Biplot in R^p
biplot(ind_prj, U) #TAKES A VERYYYYYY LONG TIME.

# Compute rotated principal components obtained in NIPALS function
pc.rot <- varimax(var_prj)
#Not sure about that, Should we take the eigen values? HW 4 review.
# Plot rotated variables
# Plotting unit circle
theta <- seq(0,2*pi, length.out = 100)
circle = data.frame(x = cos(theta), y = sin(theta))
p <- ggplot(circle, aes(x,y)) + geom_path()

df <- data.frame(Dim1 = var_prj[,1], Dim2 = var_prj[,2], variable = colnames(Default_Dataset[,- c(2,3,4,5,24,25,26,27,28,29,30)]))

# Circle with labels
circle_plot <- p + geom_text(data=df, mapping = aes(x=Dim1, y=Dim2, label=variable, colour = variable)) + coord_fixed(ratio = 1) + labs(x = "Dim1", y = "Dim2")
# Adding horizontal and vertical axes
circle_plot <- circle_plot + geom_hline(yintercept = 0, linetype = 2) + geom_vline(xintercept = 0, linetype = 2)
# Adding arrows to each variable
circle_plot <- circle_plot + geom_segment(mapping = aes(x=0,y=0,xend=var_prj[,1]*0.95,yend=var_prj[,2]*0.95), data=df, arrow=arrow(length = unit(0.01,"npc")), lineend = "round", alpha = 0.7)

# Plot rotated variables
circle_plot


########CLUSTERING########
# Select significant dimensions whose cumulative percentage of variance <= 80%
dim <- sum(as.numeric(PCADefault$eig[,3] <= 80))
#Select significant factors
Psi <- PCADefault$ind$coord[,1:dim]
d <- dist(Psi, method = "euclidean")
# Perform hierarchical clustering but not very usefull data...
hc <- hclust(d, method = "ward.D2")
plotHC = plot(hc) 
barplotHC = barplot(hc$height)

#Kmeans
# Select significant dimensions whose cumulative percentage of variance <= 80%
dim <- sum(as.numeric(PCADefault$eig[,3] <= 80))
Psi <- PCADefault$ind$coord[,1:dim]
Psi <- matrix(0, nrow = nrow(DefaultNoSupple), ncol = rankX)
centers = 20
defaultKmeans1 <- kmeans(Default_Dataset[,-c(2,3,4,5,24,25,26,27,28,29,30)], centers =centers, iter.max = 50)
defaultKmeans2 <- kmeans(Default_Dataset[,-c(2,3,4,5,24,25,26,27,28,29,30)], centers =centers, iter.max = 50)
table(defaultKmeans1$cluster,defaultKmeans2$cluster)
clas <- (defaultKmeans2$cluster-1)*centers+defaultKmeans1$cluster
freq <- table(clas)
cdclas <- aggregate(as.data.frame(Default_Dataset),list(clas),mean)[,2:(4+1)]
library(cluster) 
clusplot(Default_Dataset, defaultKmeans$cluster, color=TRUE, shade=TRUE, 
         labels=3, lines=2)
#Here we can see the different clusters. cluster 5 for example are the individuals with perfect behaiviour, always pay and no delays.
