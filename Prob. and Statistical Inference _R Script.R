########  IMPORTING LIBRARIES #######
#####################################

library(readr)
library(pastecs) #For creating descriptive statistic summaries
library(ggplot2) #For creating histograms with more detail than plot
library(semTools) 
library(coin)

############################################################
######    IMPORTING DATASET       ##########################
############################################################

df1 <- read_csv("sperformance-dataset.csv")
View(df1)
str(df1)

#############################################################
######   DATA CLEANING   ####################################
#############################################################


###### checking to see how many NA are there in my dataset #######
apply(is.na(df1),2,sum)


#### CHecking ROWS AND COLUMNS OF MY DATA FRAME df1  ######
dim(df1)

##### to view list of all columns in my data frame df1 #########
colnames(df1)

###############################################################################
##############     pG2 Generate Histogram          ############################
###############################################################################

gg <- ggplot(df1, aes(x=df1$pG2))
gg <- gg + labs(x="Feeling of Control")

#manage binwidth and colours
gg <- gg + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg <- gg + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

#adding a normal curve
#use stat_function to compute a normalised score for each value of tpcoiss
#pass the mean and standard deviation
#use the na.rm parameter to say how missing values are handled

gg <- gg + stat_function(fun=dnorm, color="red",args=list(mean=mean(df1$pG2, na.rm=TRUE), sd=sd(df1$pG2, na.rm=TRUE)))
gg

################################################
##########   Create a qqplot  ##################
################################################

##### Normal Q-Q Plot ########

qqnorm(df1$pG2) 
qqline(df1$pG2, col=2) #show a line on the plot

######################################################################################################
#####   Finding mean , meadian , SE.mean, CI.mean, var, standard deviation , coeficient variant ######
######################################################################################################

pastecs::stat.desc(df1$pG2, basic=F)

tpskew<-semTools::skew(df1$pG2)

tpkurt<-semTools::kurtosis(df1$pG2)

tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

zpG2<- abs(scale(df1$pG2))

FSA::perc(as.numeric(zpG2), 1.96, "gt")

FSA::perc(as.numeric(zpG2), 3.29, "gt")

###################################
#####   Create histogram    #######
###################################

gs <- ggplot(df1, aes(x=df1$pG2))
gs <- gs + labs(x="Perceived Stress")
gs <- gs + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gs <- gs + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs <- gs + stat_function(fun=dnorm, color="red",args=list(mean=mean(df1$pG2, na.rm=TRUE), sd=sd(df1$pG2, na.rm=TRUE)))
gs


#Create a qqplot
qqnorm(df1$pG2)
qqline(df1$pG2, col=2) #show a line on theplot


#Generate regular summary statistics - lots of packages offer mechanisms to do this
pastecs::stat.desc(df1$pG2, basic=F)


tpskew<-semTools::skew(df1$pG2)
tpkurt<-semTools::kurtosis(df1$pG2)

tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

ztpstress<- abs(scale(df1$pG2))

FSA::perc(as.numeric(ztpstress), 1.96, "gt")

FSA::perc(as.numeric(ztpstress), 3.29, "gt")

###############################################################################
##############     pG1 Generate Histogram          ############################
###############################################################################

gg1 <- ggplot(df1, aes(x=df1$pG1))
gg1 <- gg1 + labs(x="Feeling of Control")

#manage binwidth and colours
gg1 <- gg1 + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gg1 <- gg1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")

gg1 <- gg1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(df1$pG1, na.rm=TRUE), sd=sd(df1$pG1, na.rm=TRUE)))

#Create a qqplot
qqnorm(df1$pG1)
qqline(df1$pG1, col=2) #show a line on theplot

pastecs::stat.desc(df1$pG1, basic=F)

tpskew<-semTools::skew(df1$pG1)

tpkurt<-semTools::kurtosis(df1$pG1)

tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

zpG3<- abs(scale(df1$pG1))

FSA::perc(as.numeric(zpG3), 1.96, "gt")

FSA::perc(as.numeric(zpG3), 3.29, "gt")


#Create histogram
gs1 <- ggplot(df1, aes(x=df1$pG1))
gs1 <- gs1 + labs(x="Perceived Stress")
gs1 <- gs1 + geom_histogram(binwidth=2, colour="black", aes(y=..density.., fill=..count..))
gs1 <- gs1 + scale_fill_gradient("Count", low="#DCDCDC", high="#7C7C7C")
gs1 <- gs1 + stat_function(fun=dnorm, color="red",args=list(mean=mean(df1$pG1, na.rm=TRUE), sd=sd(df1$pG1, na.rm=TRUE)))
gs1


#Create a qqplot
qqnorm(df1$pG1)
qqline(df1$pG1, col=2) #show a line on theplot


#Generate regular summary statistics - lots of packages offer mechanisms to do this
pastecs::stat.desc(df1$pG1, basic=F)


tpskew<-semTools::skew(df1$pG1)
tpkurt<-semTools::kurtosis(df1$pG1)

tpskew[1]/tpskew[2]

tpkurt[1]/tpkurt[2]

ztpstress1<- abs(scale(df1$pG1))

FSA::perc(as.numeric(ztpstress1), 1.96, "gt")

FSA::perc(as.numeric(ztpstress1), 3.29, "gt")

#############################################################
######         CORRELATION       ############################
#############################################################

#Simple scatterplot of feeling of control and perceived stress
#aes(x,y)
scatter <- ggplot(df1, aes(df1$pG2, df1$pG1))

#Add a regression line
scatter + geom_point() + geom_smooth(method = "lm", colour = "Red", se = F) + labs(x = "pG2", y = "pG1")


#Pearson Correlation
stats::cor.test(df1$pG2, df1$pG1, method='pearson')


##################################################################
##    Differences - Parametric Tests  ############################
##################################################################

#Get descriptive stastitics by group - output as a matrix
psych::describeBy(df1$pG2, df1$romantic.m , mat=TRUE)
#view(df1)
car::leveneTest(pG2 ~ romantic.m, data=df1)

stats::t.test(pG2 ~ romantic.m,var.equal=TRUE,data=df1)

res <- stats::t.test(pG2 ~ romantic.m,var.equal=TRUE,data=df1)

#Calculate Cohen's d
#artithmetically
effcd=round((2*res$statistic)/sqrt(res$parameter),2)

#Using function from effectsize package
effectsize::t_to_d(t = res$statistic, res$parameter)

effes=round((res$statistic*res$statistic)/((res$statistic*res$statistic)+(res$parameter)),3)
effes

######################
####   ANOVA  ########
######################

#Get descriptive stastitics by group - output as a matrix
psych::describeBy(df1$pG1, df1$Fjob, mat=TRUE)

stats::bartlett.test(pG1~ Fjob, data=df1)

userfriendlyscience::oneway(as.factor(df1$Fjob),y=df1$pG1,posthoc='Tukey')

res1<-userfriendlyscience::oneway(as.factor(df1$Fjob),y=df1$pG1,posthoc='Tukey')

#####  use the aov function - same as one way but makes it easier to access values for reporting
res2<-stats::aov(pG1~Fjob, data = df1)

#Get the F statistic into a variable to make reporting easier
fstat<-summary(res2)[[1]][["F value"]][[1]]
#Get the p value into a variable to make reporting easier
aovpvalue<-summary(res2)[[1]][["Pr(>F)"]][[1]]
#Calculate effect
aoveta<-sjstats::eta_sq(res2)[2]



###############################################################
#####################    LINEAR  REGRESSION  ##################
##############################################################

library(stats)
library(ggplot2)
library(foreign) #To work with SPSS data
library(lm.beta) #Will allow us to isolate the beta co-efficients
library(stargazer)#For formatting outputs/tables

model1<-lm(df1$pG2~df1$pG1)
anova(model1)

summary(model1)

lm.beta::lm.beta(model1)
stargazer(model1, type="text") #Tidy output of all the required stats


#############################################################################################
###########   Multiple Linear Regression with 2 variable and 1  target variable    ##########
#############################################################################################
model2<-lm(df1$pG2~df1$pG1+df1$paid.m)
anova(model2)

summary(model2)

stargazer(model2, type="text") #Tidy output of all the required stats
lm.beta(model2)

stargazer(model1, model2, type="text") #Quick model comparison

#############################################################################################
###########   Multiple Linear Regression with 3 variable and 1  target variable  ############
#############################################################################################

model2<-lm(df1$pG2~df1$pG1+df1$mG2+df1$paid.m)
anova(model2)

summary(model2)

stargazer(model2, type="text") #Tidy output of all the required stats
lm.beta(model2)

stargazer(model1, model2, type="text") #Quick model comparison


#################################################################
####  LOGISTIC REGRESSION uisng 2 variable and 1 prediction #####
#################################################################


library(Epi)#ROC Curve
library(DescTools)#Pseudo Rsquare statistics
library(stargazer)
library(foreign)#read SPSS file.
library(arm)#for invlogit calculating predicted probabilities
library(lmtest)#Simple calculation of Chi-square for model
library(car)#Needed to test for colinearity of predictors
library(generalhoslem)#Needed to test assumption of linearity
library("regclass")#For confusion matrix

## Building model as famsup as prediction 
logmodel1 <- glm(paid.m ~ famsup.m+schoolsup.m, data = df1, na.action = na.exclude, family = binomial(link=logit))
 
#Full summary of the model
summary(logmodel1)

lmtest::lrtest(logmodel1)

DescTools::PseudoR2(logmodel1, which="CoxSnell")


DescTools::PseudoR2(logmodel1, which="Nagelkerke")

### paid and famsup has character as data type, I am chaning its data type to factor for the ROC Plot  ########

class(df1$paid.m)
df1$paid.m=as.factor(df1$paid.m)
class(df1$famsup.m)
df1$famsup.m=as.factor(df1$famsup.m)

#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=df1$paid.m ~ df1$famsup.m+df1$schoolsup.m, plot="ROC")



#################################################################
####  LOGISTIC REGRESSION using 3 variable and 1 prediction #####
#################################################################

## Building model as famsup as prediction 

logmodel2 <- glm(paid.m ~ famsup.m+schoolsup.m+Mjob, data = df1, na.action = na.exclude, family = binomial(link=logit))

#Full summary of the model
summary(logmodel2)
class(logmodel2)

lmtest::lrtest(logmodel2)

DescTools::PseudoR2(logmodel2, which="CoxSnell")

DescTools::PseudoR2(logmodel2, which="Nagelkerke")

#Output the sensitivity, specificity, and ROC plot
Epi::ROC(form=df1$paid.m ~ df1$famsup.m+df1$schoolsup.m+df1$Mjob, plot="ROC")