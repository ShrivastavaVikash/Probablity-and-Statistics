########  IMPORTING LIBRARIES #######
#####################################

library(readr)
library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) 
library(coin)
library(lm.beta)
library(stargazer)
library(Epi)#ROC Curve
library(DescTools)#Pseudo Rsquare statistics
library(stargazer)
library(foreign)#read SPSS file.
library(arm)#for invlogit calculating predicted probabilities
library(lmtest)#Simple calculation of Chi-square for model
library(car)#Needed to test for colinearity of predictors
library(generalhoslem)#Needed to test assumption of linearity
library("regclass")#For confusion matrix


##############################
###### loading of data #######
##############################

B <- read_csv("bank-additional-full1.csv")

###########################################
#####    checking for any NA Values   #####
###########################################

apply(is.na(B),2,sum)
colSums(B == "unknown")

##############################################
## Converting alphabetical to numerical #######
#############################################

B$y= ifelse(B$y=="yes",1,0)
B$default= ifelse(B$default=="yes",1,0)
B$housing= ifelse(B$housing=="yes",1,0)
B$loan= ifelse(B$loan=="yes",1,0)

B$job <- as.numeric(as.factor(B$job))
B$marital <- as.numeric(as.factor(B$marital))
B$month <- as.numeric(as.factor(B$month))
B$contact <- as.numeric(as.factor(B$contact))
B$poutcome <- as.numeric(as.factor(B$poutcome))


#############################################################
######   DATA CLEANING   ####################################
#############################################################

###### checking to see how many NA are there in my dataset #######
apply(is.na(BS),2,sum)

#############################################################
######         CORRELATION       ############################
#############################################################

#Simple scatterplot of feeling of control and perceived stress
#aes(x,y)
## variable 1
scatter <- ggplot(B, aes(B$education, B$duration))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "Age", y = "Duration")


#Pearson Correlation
stats::cor.test(B$age, B$duration, method='pearson')


mean(B$age)

sd(B$age)
###############################################################################
##############     Age Generate Histogram          ############################
###############################################################################

gg <- ggplot(B, aes(x=B$age))
gg <- gg + ggtitle(x="Histogram for Age")

gg <- gg+labs(x="Age")
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
mean(df$education)#adding a normal curve
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(B$age, na.rm=TRUE), sd=sd(B$duration, na.rm=TRUE)))
gg


################################################
##########   Create a qqplot  ##################
################################################

##### Normal Q-Q Plot ########

qqnorm(B$age) 
qqline(B$age, col=2) #show a line on the plot


######################################################################################################
#####   Finding mean , meadian , SE.mean, CI.mean, var, standard deviation , coeficient variant ######
######################################################################################################

pastecs::stat.desc(B$age, basic=F)

tpskew<-semTools::skew(B$age)

tpkurt<-semTools::kurtosis(B$age)

tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

zpG2<- abs(scale(B$age))

FSA::perc(as.numeric(zpG2), 1.96, "gt")

FSA::perc(as.numeric(zpG2), 3.29, "gt")


#####Variable 2####

gg <- ggplot(B, aes(x=education))
gg <- gg+ggtitle("Histogram for education")
#Change the label of the x axis
gg <- gg + labs(x="Education")
#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=1.5, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
#adding a normal curve
#use stat_function to compute a normalised score for each value of education
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled
gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(B$education, na.rm=TRUE), sd=sd(B$education, na.rm=TRUE)))
#to display the graph request the contents of the variable be shown
gg

# Create a qqplot  

qqnorm(B$education) 

qqline(B$education, col=2) #show a line on the plot

#get summary statistics
mean(B$education)   4.747184
sd(B$education)        2.136482 
length(B$education)  41188


eduskew<-semTools::skew(B$education) 
edukurt<-semTools::kurtosis(B$education)
eduskew[1]/eduskew[2]   --19.48275
edukurt[1]/edukurt[2]   -50.25764 

zedu<- abs(scale(B$education))
FSA::perc(as.numeric(zedu), 1.96, "gt")  [1] 0


###################################
#####   Create histogram    #######
###################################

gs <- ggplot(B, aes(x=B$age))
gs <- gs + labs(x="Age")
gs <- gs + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gs <- gs + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs <- gs + stat_function(fun=dnorm, color="red",args=list(mean=mean(B$age, na.rm=TRUE), sd=sd(B$age, na.rm=TRUE)))
gs



CrossTable(B$job,B$y)
B = B %>% filter( job!= "unknown")

CrossTable(B$housing,B$y)
B = B %>% filter( housing!= "unknown")

B$housing = ifelse(B$housing  == "yes",1,0)
B$default = ifelse(B$default == "yes",1,0)
B$loan = ifelse(B$loan == "yes",1,0)

B$job <- as.numeric(as.factor(B$job))
B$marital <- as.numeric(as.factor(B$marital))
B$education <- as.numeric(as.factor(B$education))
B$month <- as.numeric(as.factor(B$month))
B$contact <- as.numeric(as.factor(B$contact))
B$poutcome <- as.numeric(as.factor(B$poutcome))



#################################################################
####  LOGISTIC REGRESSION     ###################################
#################################################################

#MODEL1

logmodel1 <- glm(y ~ education+duration, data = B, na.action = na.exclude, family = binomial(link=logit))
summary(logmodel1)

modelChi <- logmodel1$null.deviance - logmodel1$deviance
modelChi
pseudo.R2 <- modelChi / logmodel1$null.deviance
pseudo.R2
chidf <- logmodel1$B.null - logmodel1$B.residual
chidf

Epi::ROC(form=B$y ~ B$education+B$duration+B$age, plot="ROC")

generalhoslem::logitgof(B$y, fitted(logmodel1))

#Collinearity
vifmodel<-car::vif(logmodel1)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
1/vifmodel

################################
###     MODEL2  ################
################################

logmodel_two <- glm(y ~ education+job+duration, data = B, na.action = na.exclude, family = binomial(link=logit))
#Full summary of the model
summary(logmodel_two)

lmtest::lrtest(logmodel_two)
modelChi <- logmodel_two$null.deviance - logmodel1$deviance
modelChi
pseudo.R2 <- modelChi / logmodel_two$null.deviance
pseudo.R2
chidf <- logmodel_two$B.null - logmodel_two$B.residual
chidf

Epi::ROC(form=B$y ~ B$marital+B$job+B$education+B$duration, plot="ROC")

generalhoslem::logitgof(B$y, fitted(logmodel_two))

#Collinearity
vifmodel<-car::vif(logmodel_two)#You can ignore the warning messages, GVIF^(1/(2*Df)) is the value of interest
vifmodel
1/vifmodel

#COMPARISON
stargazer(logmodel1, logmodel_two, type="text") #Quick model comparison


