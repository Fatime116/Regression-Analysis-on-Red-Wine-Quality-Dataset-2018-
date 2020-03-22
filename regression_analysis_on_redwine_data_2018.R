RedWineData_Stat6509Project =  read.csv("RedWineData_Stat6509Project.csv")
#Rename variables
colnames(RedWineData_Stat6509Project)=c("FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Denisty", "pH", "Sulphates", "Alcohol", "Quality")
#histogram for the distribution of quality ratings 
hist(RedWineData_Stat6509Project$Quality,main="Distribution of red wine quality ratings",xlim = c(2,10)  ,col = c("pink"))
#more informative scatterplot matrix
install.packages("psych")
library(psych)
pairs.panels(RedWineData_Stat6509Project[c("Quality","FixedAcidity", "VolatileAcidity", "CitricAcid", "ResidualSugar", "Chlorides", "FreeSulfurDioxide", "TotalSulfurDioxide", "Denisty", "pH", "Sulphates", "Alcohol")])
install.packages("psych")
# Run AIC to choose variables for model
null=lm(Quality~1, data=RedWineData_Stat6509Project)
summary(null)

#Additive models
full = lm(Quality~FixedAcidity+VolatileAcidity+CitricAcid+ResidualSugar+Chlorides+FreeSulfurDioxide+TotalSulfurDioxide+Denisty+pH+Sulphates+Alcohol, data=RedWineData_Stat6509Project)
summary(full)
#run forward step function to choose the best model 
step(null, scope=list(lower=null, upper=full),direction="forward")
#Step:  AIC=-1380.79 ,and we call this model 1
model1=lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + TotalSulfurDioxide + Chlorides + pH + FreeSulfurDioxide, 
          data = RedWineData_Stat6509Project)
##p-value < 0.05 ,R-squared = 0.359 
summary(model1)
#Diagnostics with Model 1
par(mfrow=c(2,2)) 
plot(model1, which=1:4)
#AIC Variable Selection for Interactive Model With Interactions , we call this interaction model as full2

full2 = lm(Quality~FixedAcidity*VolatileAcidity*CitricAcid*ResidualSugar*Chlorides*FreeSulfurDioxide*TotalSulfurDioxide*Denisty*pH*Sulphates*Alcohol, data=RedWineData_Stat6509Project)
summary(full2) 

##Step:  AIC=-1453.22 ,
step(null, scope=list(lower=null, upper=full2),
     direction="forward")
#choose the value with AIC=-1453.22 as model 2
model2=lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
            TotalSulfurDioxide + Chlorides + pH + CitricAcid + FreeSulfurDioxide + 
            Alcohol:Sulphates + Sulphates:TotalSulfurDioxide + VolatileAcidity:TotalSulfurDioxide + 
            Sulphates:CitricAcid + TotalSulfurDioxide:CitricAcid + Chlorides:FreeSulfurDioxide, 
          data = RedWineData_Stat6509Project)
summary(model2)
#Diagnostics with Model 2
par(mfrow=c(2,2)) 
plot(model2, which=1:4)
#Check if interaction needed ,model1 without interaction ,model 2 with interaction 
##9.201e-16 *** interaction is needed , so we choose the model with the interaction 
anova(model1,model2)

#model 3 interaction model removing insignificant variables
model3=lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
            TotalSulfurDioxide + Chlorides + pH + CitricAcid + FreeSulfurDioxide + 
            Alcohol:Sulphates + Sulphates:TotalSulfurDioxide + VolatileAcidity:TotalSulfurDioxide, 
          data = RedWineData_Stat6509Project)
summary(model3)


#Compare models , model 1 without interaction ,model 3 with interaction after removing some insignificant variables
anova(model1,model3) ##p value is .166e-15 *** , so we choose model 3 over model1

#Choose model 3 over model 1
#Diagnostics for model 3
par(mfrow=c(2,2)) 
plot(model3, which=1:4)

#Improve the model 3
#Check outliers
install.packages("car")

library(car)
par(mfrow=c(1,1)) 
qqPlot(model3, id.n=2)

outlierTest(model3)
#Outliers with high leverage
influencePlot(model3) #highest leverage point 363 and 1300
#removing highest leverage outliers FINAL MODEL
newdata1 <- RedWineData_Stat6509Project[c(-363,-1300),]
model3c = lm(formula = Quality ~ Alcohol + VolatileAcidity + Sulphates + 
               TotalSulfurDioxide + Chlorides + pH + CitricAcid + FreeSulfurDioxide + 
               Alcohol:Sulphates + Sulphates:TotalSulfurDioxide + VolatileAcidity:TotalSulfurDioxide, 
             data = newdata1)
summary(model3c)

#Check outlier distribution with the final model

influencePlot(model3c)

#Diagnostics for model 3

par(mfrow=c(2,2)) 
plot(model3c, which=1:4)
#Diagnostics Model 3c

par(mfrow=c(2,2))
plot(model3c, which=1:4)

#Normality --Shapiro Wilks P-value: 1.121e-08

shapiro.test(model3c$residuals)
#Constant Variance P-value ran with ncvTest command: 0.0001224764 
ncvTest(model3c)











