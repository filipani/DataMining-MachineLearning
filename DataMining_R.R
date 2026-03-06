setwd("C:\\Users\\f.paninforni\\Desktop\\New folder\\archive")

install.packages("VIM")
library(VIM)
library(mice)
library(psych)
require(corrplot)
library(mctest)
library(MASS)
install.packages(c('colorRamps', 'proxy', 'ggpubr', 'mvtnorm', 'agricolae', 'forcats', 'formula.tools'))
install.packages("Y:/My Drive/Data mining/R e SAS/nuovi file  (re-inseriti nella cartella zippata dei codici R)-20251007/factorMerger_0.4.0.tar.gz", repos=NULL, type="source")
library(factorMerger)
library(dplyr)
library(gam)
library(car)
library(lmtest)
library(robustbase)
library(sandwich)
install.packages("gvlma")
library(gvlma)

data<-read.csv("Data Cycling.csv",  na.strings=c("NA","NaN", ""))
data$Date<-NULL
data$Activity.Type<-as.factor(data$Activity.Type)

summary(data)

for (var in names(data)){
  if (var!="Activity.Type" && var!="Date" && var!="Time"){
    data[[var]]<-as.numeric(data[[var]])
  }
}

data$Time<-sapply(data[["Time"]],function(t){
  gsub("\\.", ":", t)
})

data$Time<-sapply(data[["Time"]],function(t){
  s<-strsplit(t, ":")[[1]]
  if (length(s)==3){
    as.numeric(s[1])*3600+ as.numeric(s[2])*60+ as.numeric(s[3])}
  else if (length(s)==2){
    as.numeric(s[1])*60+ as.numeric(s[2])
  }
})

data[data==0]<-NA

sapply(data, function(x)(sum(is.na(x))/nrow(data)))


missingness<- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, cex.axis=.7,gap=3)

#rimuovo variabile con piu di 0.25 di missingness

missing<-colMeans(is.na(data))
data<-data[,missing<0.25]
colnames(data)
missingness<- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, cex.axis=.7,gap=3)

#MICE
covdata<-data
covdata$Time<-NULL
tempData <- mice(covdata, m=1, maxit=20, meth='pmm', seed=500)
data_imputed <- complete(tempData,1)
data=cbind(data_imputed, data$Time)
names(data)[12] <- "Time"
sapply(data, function(x)(sum(is.na(x))/nrow(data)))

summary(data)

#MULTICOLLINEARIT?

####CORRGRAM####
#correlation and multicol plot

data_numeric<-data[sapply(data, is.numeric)]
Corr<-round(cor(data_numeric),3)
Corr
pairs.panels(data_numeric, lm=T)
par(mfrow=c(1,1))
corrplot(Corr)

Formula <- paste(colnames(data), collapse="+")
Formula

par(mfrow=c(2,2))
lm1<-lm(Time~Distance+Calories+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Normalized.Power?...NP?..+Training.Stress.Score?.+Power+Max.Power+Activity.Type, data)
plot(lm1)
imcdiag(lm1)

lm2<-lm(Time~Distance+Calories+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Training.Stress.Score?.+Power+Max.Power+Activity.Type, data)
plot(lm2)
imcdiag(lm2)

lm3<-lm(Time~Distance+Calories+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Power+Max.Power+Activity.Type, data)
plot(lm3)
imcdiag(lm3)

lm4<-lm(Time~Distance+Calories+Avg.HR+Avg.Speed+Max.Speed+Power+Max.Power+Activity.Type, data)
plot(lm4)
imcdiag(lm4)

lm5<-lm(Time~Distance+Calories+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity.Type, data)
plot(lm5)
imcdiag(lm5)

#togliamo variabili con tol piu bassi <0.3 e VIF>5

cov=attr(terms(lm5), "term.labels")

for (var in cov){
  if (var != "Activity.Type"){
  ancova<-lm(data[[var]]~Activity.Type, data)
  print(anova(ancova))}
}
#sono tutte spiegate dal tipo di attivit? tranne distance e calories

par(mfrow=c(2,3))
for(var in cov) {
  if (var != "Activity.Type"){
  boxplot(data[[var]]~data$Activity.Type, main=var, title=var)}
  }
#i box plot confermano quanto prima


#modellizzazione
lm<-lm(Time~Distance+Calories+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity.Type, data)
summary(lm)
shapiro.test(lm$residuals)
par(mfrow=c(2,2))
plot(lm)
par(mfrow=c(1,2))
boxplot(lm$residuals)
qqnorm(lm$residuals)
qqline(lm$residuals)
#serious problems: no linearity, no normality, hetero, outliers

#####Box-Cox transformation####
par(mfrow=c(1,1))
boxcoxreg1<-boxcox(lm) 
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda
#valore di lambda molto vicino a zero, trasformazione log di Time

hist(data$Time)
hist(I(log(data$Time)))

loglin1<-lm(I(log(Time))~Distance+Calories+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity.Type, data)
summary(loglin1)
shapiro.test(loglin1$residuals)
par(mfrow=c(2,2))
plot(loglin1)
par(mfrow=c(1,2))
boxplot(loglin1$residuals)
qqnorm(loglin1$residuals)
qqline(loglin1$residuals)
#emerge ancora qualche problema, probabilmente causato da outliers

#optimal grouping
reduce_levels <- mergeFactors(response = data$Time, factor = data$Activity.Type)
par(mfrow=c(1,1))
plot(reduce_levels , panel = "GIC",title = "", panelGrid = FALSE )
og<-cutTree(reduce_levels)
data$optimal_group<-og
data$Activity_group <-as.factor(as.numeric(data$optimal_group))
loglin1_group<-lm(I(log(Time))~Distance+Calories+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity_group, data)
summary(loglin1_group)
par(mfrow=c(2,2))
plot(loglin1_group)
par(mfrow=c(1,2))
plot(data$Activity_group, log(data$Time))
plot(data$Activity.Type, log(data$Time))
anova(loglin1, loglin1_group)
extractAIC(loglin1)
extractAIC(loglin1_group)
bptest(loglin1)
bptest(loglin1_group)
#piccolo miglioramento ma nemmeno significativo, il raggruppamento non migliora nemmeno l'etero, teniamo activity.type (?), questo grouping pu? essere utile per la logistica


resettest(loglin1, power = 2, type = "fitted",  data = data) #mancano covariate di secondo grado, ma ricordiamo la presenza di outliers

par(mfrow=c(2,3))
hist(data$Distance) #suggerisce una trasformazione log
hist(I(log(data$Distance))
hist(data$Calories)
hist(data$Avg.HR)
hist(data$Avg.Speed)
hist(data$Max.Speed)
hist(data$Max.Power)

gam1 = gam(I(log(Time))~s(Distance)+s(Calories)+s(Avg.HR)+s(Avg.Speed)+s(Max.Speed)+s(Max.Power), data=data) 
summary(gam1)
par(mfrow=c(2,3))
plot(gam1, resid=T, pch=16)
anova(loglin1, gam1, test='F') #to test improvement significance
crPlots(loglin1) #These function constructs component+residual plots, also called partial-residual plots, for linear and generalized linear models.

#aggiungo calories quadrato e log di distance
#loglin2<-lm(I(log(Time))~I((Distance)^(1/2))+Calories+I((Calories)^2)+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity.Type, data)
loglin2<-lm(I(log(Time))~I(log(Distance))+Calories+I((Calories)^2)+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity.Type, data)
summary(loglin2)
shapiro.test(loglin2$residuals)
par(mfrow=c(2,2))
plot(loglin2)
par(mfrow=c(1,2))
boxplot(loglin2$residuals)
qqnorm(loglin2$residuals)
qqline(loglin2$residuals)
par(mfrow=c(3,3))
crPlots(loglin2)
resettest(loglin2, power = 2, type = "fitted",  data = data)#reset ok
resettest(loglin2, power = 3, type = "fitted",  data = data)
imcdiag(loglin2)

loglin3<-lm(I(log(Time))~I(log(Distance))+I((Calories)^2)+Avg.HR+Avg.Speed+I((Avg.Speed)^3)+Max.Speed+Max.Power+Activity.Type, data)
resettest(loglin3, power = 3, type = "fitted",  data = data)#reset ok
par(mfrow=c(4,3))
crPlots(loglin3)
imcdiag(loglin3)

loglin4<-lm(I(log(Time))~I(log(Distance))+I((Calories)^2)+Avg.HR+I((Avg.Speed)^3)+Max.Speed+Max.Power+Activity.Type, data)
resettest(loglin4, power = 3, type = "fitted",  data = data)
summary(loglin4)
shapiro.test(loglin2$residuals)
par(mfrow=c(2,2))
plot(loglin4)
par(mfrow=c(1,2))
boxplot(loglin4$residuals)
qqnorm(loglin4$residuals)
qqline(loglin4$residuals)
par(mfrow=c(4,3))
imcdiag(loglin4)


#outliers
influencePlot(loglin4,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# Cook's D are contained in the fitted model (object fit)
cooksd <- cooks.distance(loglin4)
cooksda=data.frame(cooksd)
summary(cooksd)
#cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!
n_used=length(loglin4$residuals)
nrow(data) #the one above, not this one, be careful!!! 
cutoff <- 4/(n_used-length(loglin4$coefficients)-2)
par(mfrow=c(1,1))
plot(loglin4, which=4, cook.levels=cutoff)
abline(h=cutoff)

data <- data[cooksd < cutoff, ]

par(mfrow=c(3,3))
hist(data$Distance) #suggerisce una trasformazione log
hist(I(log(data$Distance)))
hist(data$Calories)
hist(I(data$Calories)^2) #sembra non servire
hist(data$Avg.HR)
hist(data$Avg.Speed)
hist(I(data$Avg.Speed)^3) #rimuovo questa
hist(data$Max.Speed)
hist(data$Max.Power)

loglin4<-lm(I(log(Time))~I(log(Distance))+Calories+Avg.HR+Avg.Speed+Max.Speed+Max.Power+Activity.Type, data)
summary(loglin4)
shapiro.test(loglin4$residuals)
par(mfrow=c(2,2))
plot(loglin4)
par(mfrow=c(1,2))
boxplot(loglin4$residuals)
qqnorm(loglin4$residuals)
qqline(loglin4$residuals)


#model selection
step <- stepAIC(loglin4, direction="both")
step$anova # display results 

# model that remain
loglin5 = lm(I(log(Time)) ~ I(log(Distance)) + Calories + Avg.HR, data)

drop1(loglin5, .~., test="F")
summary(loglin5)
#diagnostic
par(mfrow=c(2,2)) 
plot(loglin5)
resettest(loglin5, power = 2, type = "fitted",  data = data) #causato da calories bimodale
resettest(loglin5, power = 3, type = "fitted",  data = data)


#eteroschedasticit?
par(mfrow=c(1,1))
plot(loglin5, which=3)
bptest(loglin5)#rifiuto, ho etero
summary(loglin5)
coeftest(loglin5, vcov=vcovHC(loglin5))
#i coefficienti sono significativi

loglin_rob<-lmrob(I(log(Time)) ~ I(log(Distance)) + Calories + Avg.HR, data)
summary(loglin_rob)
#da summary sembra un buon modello, utilizziamo questo

#bootstrap
library(boot)

boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- glm(I(log(Time)) ~I(log(Distance)) + I((Calories)^2) + Avg.HR, family = poisson, data = d)
  return(coef(model))
}
BOOT.MOD <- boot(data=data, statistic = boot_fun, R = 1500)
summary(BOOT.MOD, high.moments=TRUE)

# confint boot
Confint(BOOT.MOD, level=c(.95), type="perc")
hist(BOOT.MOD, legend="separate")

par(mfrow=c(2,2))
plot(lm)
plot(loglin6)


####LOGISTICO####
data<-read.csv("Data Cycling.csv",  na.strings=c("NA","NaN", ""))
data$Date<-NULL
data$Activity.Type<-as.factor(data$Activity.Type)

summary(data)

for (var in names(data)){
  if (var!="Activity.Type" && var!="Date" && var!="Time"){
    data[[var]]<-as.numeric(data[[var]])
  }
}

data$Time<-sapply(data[["Time"]],function(t){
  gsub("\\.", ":", t)
})

data$Time<-sapply(data[["Time"]],function(t){
  s<-strsplit(t, ":")[[1]]
  if (length(s)==3){
    as.numeric(s[1])*3600+ as.numeric(s[2])*60+ as.numeric(s[3])}
  else if (length(s)==2){
    as.numeric(s[1])*60+ as.numeric(s[2])
  }
})

data[data==0]<-NA

sapply(data, function(x)(sum(is.na(x))/nrow(data)))


missingness<- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, cex.axis=.7,gap=3)

#rimuovo variabile con piu di 0.25 di missingness

missing<-colMeans(is.na(data))
data<-data[,missing<0.25]
colnames(data)
missingness<- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, cex.axis=.7,gap=3)

#MICE
covdata<-data
covdata$Time<-NULL
tempData <- mice(covdata, m=1, maxit=20, meth='pmm', seed=500)
data_imputed <- complete(tempData,1)
data=cbind(data_imputed, data$Time)
names(data)[12] <- "Time"
sapply(data, function(x)(sum(is.na(x))/nrow(data)))
summary(data)

table(data$Activity.Type)

par(mfrow=c(3,4))
plot(data$Time, data$Activity.Type)
plot(data$Distance, data$Activity.Type)
plot(data$Calories, data$Activity.Type)
plot(data$Avg.HR, data$Activity.Type)
plot(data$Max.HR, data$Activity.Type)
plot(data$Avg.Speed, data$Activity.Type)
plot(data$Max.Speed, data$Activity.Type)
plot(data$Normalized.Power?...NP?.., data$Activity.Type)
plot(data$Training.Stress.Score?., data$Activity.Type)
plot(data$Power, data$Activity.Type)
plot(data$Max.Power, data$Activity.Type)
#no separation

# Calcolare la varianza per tutte le variabili numeriche nel dataset
for (var in colnames(data)){
  if (is.numeric(data[[var]])==TRUE){
    print(var(data[[var]]))
  }
}
#no zero variance


modello1 <- glm(
  Activity.Type ~ Time+Distance+Calories+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Normalized.Power?...NP?..+Training.Stress.Score?.+Power+Max.Power,
  data = data, family = binomial
)
vif(modello1)

#tolgo variabile Training.Stress.Score?.
modello2 <- glm(
  Activity.Type ~ Time+Distance+Calories+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Normalized.Power?...NP?..+Power+Max.Power,
  data = data, family = binomial
)
vif(modello2)

#tolgo variabile Normalized.Power?...NP?..
modello3 <- glm(
  Activity.Type ~ Time+Distance+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Power+Max.Power,
  data = data, family = binomial
)
vif(modello3)
summary(modello3)

#vif ok

#coeff
library(coefplot)
coefplot(modello3, intercept=FALSE)

#Odds Ratio
exp(modello3$coefficients)
exp(confint(modello3))
round(exp(cbind(OR=coef(modello3), confint(modello3))),2)
library(forestmodel)
print(forest_model(modello3),text_size = 5)


drop1(modello3, test="LRT") #brutto brutto
summary(modello3)
null = glm(Activity.Type ~ 1, data,family = binomial)
R2=1-(modello3$deviance/null$deviance)
R2

modello4 <- glm(
  Activity.Type ~ Time++Distance+I((Distance)^2)+Avg.HR+Max.HR+I((Max.HR)^2)+Avg.Speed+Max.Speed+Power+Max.Power+I((Max.Power)^2),
  data = data, family = binomial
)
vif(modello4)

modello5 <- glm(
  Activity.Type ~ Time++Distance+I((Distance)^2)+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Power+Max.Power+I((Max.Power)^2),
  data = data, family = binomial
)
vif(modello5)

modello6 <- glm(
  Activity.Type ~ Time+Distance+I((Distance)^2)+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Power+I((Max.Power)^2),
  data = data, family = binomial
)
vif(modello6)

modello7 <- glm(
  Activity.Type ~ Time+I((Distance)^2)+Avg.HR+Max.HR+Avg.Speed+Max.Speed+Power+I((Max.Power)^2),
  data = data, family = binomial
)
vif(modello7)
drop1(modello7, test="LRT") #brutto brutto
summary(modello4)
R2=1-(modello4$deviance/null$deviance)
R2 #migliorato


#ZERO VARIANCE (vista dal C logistic)
# zero variance and missing data (FATTA DAL PROF)
library(funModeling)
library(dplyr)
# SEE types of vars
status=data_status(data, print_results = F)
status


library(caret)
nzw <- nearZeroVar(data, saveMetrics = TRUE) 
nzw
#OUTPUT CON NESSUNA VARIABILE CON "TRUE" IN ENTRAMBE LE COLONNE, PER? LA TARGET
#? MOLTO SBILANCIATA.



#QUASI SEPARATION
# 4) select data for X in the list

fit = glm(yes ~ SEX + STACIV + TIPODIP + QUAL + SETT + IREG + CLETA + ACOM4C +
            STUDIO + ANNOEDU + area5c + PARENT + NCOMP + ANASC + NPERC + 
            ETALAV + ACONTRIB + PF + AR + AF + scolar
          , data=b,family = binomial)
# error



