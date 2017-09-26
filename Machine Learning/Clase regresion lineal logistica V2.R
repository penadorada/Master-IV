## -------------------------------------------------------------------------
## CLASE 10 Regresión Lineal y Regresión Logística.R
## ASIGNATURA: Regresión lineal y logística. Modelos lineales generalizados
## PROFESOR: Antonio Pita Lozano
## -------------------------------------------------------------------------


library(ggplot2)
library(effects)
library(plyr)
library(ROCR)

## -------------------------------------------------------------------------
##       PARTE 1: REGRESIÓN LINEAL
## -------------------------------------------------------------------------



##### Carga de datos

creditos=read.csv("creditos.csv",stringsAsFactors = FALSE)


##### INSPECCIÓN DATOS

str(creditos)
head(creditos)
summary(creditos)


##### VARIABLES

creditos$Gender=as.factor(creditos$Gender)
creditos$Mortgage=as.factor(creditos$Mortgage)
creditos$Married=as.factor(creditos$Married)
creditos$Ethnicity=as.factor(creditos$Ethnicity)

summary(creditos)



##### Test de diferencia de medias mediante regresión lineal 

t.test(Income ~ Gender, data = creditos)

# mediante un modelo lineal
modeloT=lm(Income ~ Gender, data = creditos)
summary(modeloT)


##### Regresión lineal Simple. Individual 

modeloInd1=lm(Income ~ Rating, data = creditos)
summary(modeloInd1)

modeloInd2=lm(Income ~ Products, data = creditos)
summary(modeloInd2)

modeloInd3=lm(Income ~ Age, data = creditos)
summary(modeloInd3)

modeloInd4=lm(Income ~ Education, data = creditos)
summary(modeloInd4)

modeloInd5=lm(Income ~ Gender, data = creditos)
summary(modeloInd5)

modeloInd6=lm(Income ~ Mortgage, data = creditos)
summary(modeloInd6)

modeloInd7=lm(Income ~ Married, data = creditos)
summary(modeloInd7)

modeloInd8=lm(Income ~ Ethnicity, data = creditos)
summary(modeloInd8)

modeloInd9=lm(Income ~ Balance, data = creditos)
summary(modeloInd9)


##### Regresión lineal multiple

modeloMul1=lm(Income ~ ., data = creditos)
summary(modeloMul1)



##### Comparación de modelos 

anova(modeloInd1,modeloMul1)



##### Ejercicios

## Variables a incluir en el modelo

modeloMul2=lm(Income ~                    , data = creditos)
summary(modeloMul2)


anova(modeloInd1,modeloMul2)
anova(modeloMul2,modeloMul1)

## Mediante Step, se comparan los distintos modelos y se selecciona aquel con menos AIC.
## Formas:backward y forward


##### Análisis del modelo 

modeloFinal=lm(Income ~ Rating+Mortgage+Balance, data = creditos)
summary(modeloFinal)
plot(modeloFinal$residuals)
hist(modeloFinal$residuals)
qqnorm(modeloFinal$residuals); qqline(modeloFinal$residuals,col=2)
confint(modeloFinal,level=0.95)

anova(modeloFinal,modeloMul1)

ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Rating, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Mortgage) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)

ggplot(creditos, aes(x = Balance, y = Income)) + geom_point() + facet_grid(~ Ethnicity) + 
  geom_smooth(method = "lm", se=TRUE, color="red", formula = y ~ x)


##### Análisis de interacciones 

modeloInter1=lm(Income ~ Balance+Rating*Mortgage+Balance:Mortgage, data = creditos)
summary(modeloInter1)

modeloInter2=lm(Income ~ Rating*Mortgage+Balance, data = creditos)
summary(modeloInter2)

modeloInter3=lm(Income ~ Rating:Mortgage+Balance, data = creditos)
summary(modeloInter3)

efecto1 <- effect("Rating*Mortgage", modeloInter1, xlevels = 10)
plot(efecto1)

efecto2 <- effect("Balance*Mortgage", modeloInter1, xlevels = 10)
plot(efecto2)

efecto3 <- effect("Rating*Mortgage", modeloInter2, xlevels = 10)
plot(efecto3)

efecto4 <- effect("Rating:Mortgage", modeloInter3, xlevels = 10)
plot(efecto4)

modeloInter5=lm(Income ~ Rating*Mortgage, data = creditos)
summary(modeloInter5)

efecto5 <- effect("Rating*Mortgage", modeloInter5, xlevels = 10)
plot(efecto5)



##### Análisis de la  variable Balance

modeloBalance=lm(Balance ~ ., data = creditos)
summary(modeloBalance)

## -------------------------------------------------------------------------
##       PARTE 2: MODELOS LINEALES GENERALIZADOS: REGRESIÖN LOGÍSTICA
## -------------------------------------------------------------------------

##### Carga de datos 

BANK=read.csv2("bank-full.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bank+Marketing



##### Inspección del dataset 

str(BANK)
head(BANK)
summary(BANK)


##### Formateo de variables

BANK$day=as.factor(BANK$day)
BANK$campaign=as.factor(BANK$campaign)
BANK$IND_PREVIO=as.factor(as.numeric(BANK$pdays!=-1))

str(BANK)
head(BANK)
summary(BANK)


##### Modelo de regresión logística 

model_logit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="logit"))
summary(model_logit1)
        
model_probit1=glm(y~job+marital+education+default+balance+housing+loan+contact+month+poutcome, data=BANK,family=binomial(link="probit"))
summary(model_probit1)

# Diferencia entre el logit y el probit
X=seq(from=-4,to=4,by=0.1)
sigmoide=1/(1+exp(-X))
cumulative<-pnorm(X, 0, 1)
plot(sigmoide,type="l",col="red")
lines(cumulative,col="blue")



##### Evaluación del modelo

BANK$prediccion=predict(model_logit1,type="response")
Pred_auxiliar= prediction(BANK$prediccion, BANK$y, label.ordering = NULL)
auc.tmp = performance(Pred_auxiliar, "auc");
auc_model_logit1_train = as.numeric(auc.tmp@y.values)
auc_model_logit1_train

CURVA_ROC_model_logit1_train <- performance(Pred_auxiliar,"tpr","fpr")
plot(CURVA_ROC_model_logit1_train,colorize=TRUE)
abline(a=0,b=1)

## Capacidad del Modelo
mean(as.numeric(BANK$y)-1)
aggregate(BANK$prediccion~BANK$y,FUN=mean)


##### Fijación del Threshold

ALPHA=0.5
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

ALPHA=0.2
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

ALPHA=0.8
Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
Accuracy= (sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y)
Precision=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA)
Cobertura=sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes")
Confusion
Accuracy
Precision
Cobertura

# Criterio maximizar F1-Score

ALPHAS=seq(0,1,0.1)
Accuracy=c()
Precision=c()
Cobertura=c()
F1_Score=c()
for (i in 1:length(ALPHAS)){
  ALPHA=ALPHAS[i]
  Confusion=table(BANK$y,BANK$prediccion>=ALPHA)
  Accuracy=c(Accuracy,(sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)+sum(BANK$y=="no" & BANK$prediccion<ALPHA))/length(BANK$y))
  Precision=c(Precision,sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$prediccion>=ALPHA))
  Cobertura=c(Cobertura,sum(BANK$y=="yes" & BANK$prediccion>=ALPHA)/sum(BANK$y=="yes"))
}
F1_Score=2*(Precision*Cobertura)/(Precision+Cobertura)
DF_F1=data.frame(ALPHAS,Accuracy,Precision,Cobertura,F1_Score)

DF_F1

## -------------------------------------------------------------------------
##       PARTE 3: MODELOS LINEALES GENERALIZADOS: REGRESION POISSON
## -------------------------------------------------------------------------

##### Carga de datos 

BICIS=read.csv("hour.csv")
##### datos extraidos de https://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset


##### Inspección del dataset 

str(BICIS)
head(BICIS)
summary(BICIS)


##### Modelos de regresión poisson 

hist(BICIS$cnt)
mean(BICIS$cnt)
sd(BICIS$cnt)

model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

BICIS$prediccion=predict(model_poisson,type="response")
SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

R2

##### Formateo de variables 

BICIS$season=as.factor(BICIS$season)
BICIS$yr=as.factor(BICIS$yr)
BICIS$mnth=as.factor(BICIS$mnth)
BICIS$hr=as.factor(BICIS$hr)
BICIS$holiday=as.factor(BICIS$holiday)
BICIS$weekday=as.factor(BICIS$weekday)
BICIS$workingday=as.factor(BICIS$workingday)
BICIS$weathersit=as.factor(BICIS$weathersit)

model_poisson=glm(cnt~.-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

model_poisson=glm(cnt~.-workingday-instant-dteday-casual-registered, family=poisson(link = "log"),data=BICIS)
summary(model_poisson)

BICIS$prediccion=predict(model_poisson,type="response")
SCE=sum((BICIS$cnt-BICIS$prediccion)^2)
STC=sum((BICIS$cnt-mean(BICIS$cnt))^2)
R2=1-sum((BICIS$cnt-BICIS$prediccion)^2)/sum((BICIS$cnt-mean(BICIS$cnt))^2)

R2

