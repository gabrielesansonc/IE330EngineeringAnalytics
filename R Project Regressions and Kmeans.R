#Install packages and load them. 


library("mosaicData")


library("QuantPsyc")


library("ggplot2")


library ("dplyr")


#Save data into variable
projectdata<-project
ProjectDataNotStandarized<-project

#check data type
sapply(projectdata, class)

#correct data to binary
cols<-c("anaemia","diabetes","high_blood_pressure","sex","smoking","DEATH_EVENT") 
projectdata[cols] <- lapply(projectdata[cols], factor)
sapply(projectdata, class)

#Scale data
projectdata[,-c(1,2)]<- mutate_if(projectdata[,-c(1,2)],is.numeric, scale)
head(projectdata)

#Summary of the variables statistic
summary(projectdata)

#Storing new data with just dead patients
deaddata<-projectdatajustdead
DeadDataNotStandarized<-projectdatajustdead


#check data type
sapply(deaddata, class)

#correct data to binary
cols2<-c("anaemia","diabetes","high_blood_pressure","sex","smoking","DEATH_EVENT") 
deaddata[cols] <- lapply(deaddata[cols], factor)
sapply(deaddata, class)

#Scale data
deaddata[,-c(1,2)]<- mutate_if(deaddata[,-c(1,2)],is.numeric, scale)
head(deaddata)


#Linear regression of all variables for dead patients STANDARIZED
SurvivalTimeRegDead<-lm(SurvivalTime~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex, data=deaddata)
summary(SurvivalTimeRegDead)

#Linear regression with the top 5 significant variables STANDARIZED
SurvivalTimeRegDeadTop5<-lm(SurvivalTime~age+sex+ejection_fraction+serum_creatinine+high_blood_pressure, data=projectdatajustdead)
summary(SurvivalTimeRegDeadTop5)

#Simple linear regression with most signnificant factor (ejection fraction) of the top 5 variables. 
SurvivalTimeSLR<-lm(SurvivalTime~ejection_fraction, data = deaddata)
summary(SurvivalTimeSLR)

#Not standarized simple linear regression.
SurvivalTimeSLR<-lm(SurvivalTime~ejection_fraction, data = DeadDataNotStandarized)
summary(SurvivalTimeSLR)

#Simple linear regression and scatter plot graph
ggplot (data = DeadDataNotStandarized, mapping = aes(x = ejection_fraction, y = SurvivalTime)) + 
          geom_point(color = "#69b3a2", 
                     alpha = 0.6, 
                     size = 3) + 
          geom_smooth (method = "lm", color ="darkblue", fill = "#69b3a2", se = TRUE)

#Logistic regresion with the original data (including survivors) to predict Death Event
DeathEventLR<-glm(DEATH_EVENT~age+anaemia+creatinine_phosphokinase+diabetes+ejection_fraction+high_blood_pressure+platelets+serum_creatinine+serum_sodium+sex+smoking,family = binomial, data=projectdata)
summary(DeathEventLR, data=projectdata)

#Logistic regression with the top 5 significant variables: age, creatinine phosphokinase, ejection fraction, serum creatinine, serum sodium. Also scaled to find which are the two most signinficant values for part 4.
DeathEventtop5LR<-glm(DEATH_EVENT~age+creatinine_phosphokinase+ejection_fraction+serum_creatinine+serum_sodium,family = binomial, data=projectdata)
summary(DeathEventtop5LR, data=projectdata) 


#Doing the k means clustering of k = 4 with the two most influential factors in the logistic regression: Serium Creatine and ejection fraction
kmeandata<-kmeans

clustering <- kmeans(kmeans, centers = 4, iter.max = 10, nstart = 1)
clustering


#Scatter plot of the two variables used for the k means clustering to analyse the problem visually
plot (kmeandata, col = clustering$cluster)



