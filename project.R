# database link: https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients

#####LIBRARIES######
if(!require(rstudioapi)) install.packages("rstudioapi")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(plotly)) install.packages("plotly")
if(!require(mice)) install.packages("mice")
if(!require(mclust)) install.packages("mclust") #Nice clustering library, guide: http://rstudio-pubs-static.s3.amazonaws.com/154174_78c021bc71ab42f8add0b2966938a3b8.html
set.seed(1)
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

PCADefault <- PCA(Default_Dataset,quali.sup = c(2,3,4,5,6,7,8,9,10,11,24))
PCADefault
#Best and worst represented.
cos1 = PCADefault$ind$cos2 #Return the cos of the individuals
which.max(cos1[,1]) #The best represented individual is the number 19494
which.min(cos1[,1]) #The worst represented individual is the number 28995

#Contributions
contribution <- PCADefault$ind$contrib

bestinfirstPC <- sort(contribution[,1],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(Contribution) in the first principal component
bestinfirstPC

bestinsecondPC <- sort(contribution[,2],decreasing = TRUE)[1:3] #Returns the individuals that are more influencial(contribution) in the second principal component
bestinsecondPC

#Best represented variables
cos2 = PCADefault$var$cos2 #Returns the cos of the variables.
which.max(cos2[,1]) #BILL_AMT4 is the best represented variable in the first factorial plame
which.min(cos2[,1]) #PAY_AMT6 is the worst represented variable in the first factorial plame

#Most influencial variables
contribution2 <- PCADefault$var$contrib

bestinfirstPCvar <- sort(contribution2[,1],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(Contribution) in the first principal component
bestinfirstPCvar

bestinsecondPCvar <- sort(contribution2[,2],decreasing = TRUE)[1:3] #Returns the variables that are more influencial(contribution) in the second principal component
bestinsecondPCvar

#Significant dimensions
dim <- sum(as.numeric(PCADefault$eig[,3] <= 80)) #5 significant dimensions

#NIPALS ALGORITHM TO OBTAIN THE 5 PRINCIPAL COMPONENTS
nipals <- function(X, rankX = 5, suppl.col =  c(2,3,4,5,6,7,8,9,10,11,24), eps = 1e-06) {
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

df <- data.frame(Dim1 = var_prj[,1], Dim2 = var_prj[,2], variable = colnames(Default_Dataset[,- c(2,3,4,5,6,7,8,9,10,11,24)]))

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

#emclustering 
library(mclust)
Mclust(Default_Dataset, G = NULL, modelNames = NULL, prior = NULL, control = emControl(), initialization = NULL, warn = FALSE)


