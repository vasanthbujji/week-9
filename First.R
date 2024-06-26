state_data <- as.data.frame(state.x77)
state_data
View(state_data)
pairs(state_data)
cor(state_data)

names(state_data)[names(state_data) == "Life exp"] <- "Life_Exp"
names(state_data)[names(state_data) == "HS Grad"] <- "HS_Grad"
state_data

library(e1071)
windows(20,12)
pairs(state_data, smooth=FALSE, scale=FALSE, density=TRUE,ellipses=FALSE, method="spearman",
      pch=21, lm=FALSE,cor=TRUE, jiggle =FALSE, factor=2, hist.col=4,stars=TRUE,ci =TRUE)
warnings()

windows(20.12)
par(mfrow=c(4,3))
scatter.smooth(x=state_data$Population, y=state_data$Murder,
               main="Correlation of population  ~Murder",
               xlab="Population(,000",
               ylab="Murder")
scatter.smooth(x=state_data$Income, y=state_data$Murder,
               main="Correlation of Income  ~Murder",
               xlab="Income",
               ylab="Murder%")
scatter.smooth(x=state_data$Illiteracy, y=state_data$Murder,
               main="Correlation of Illiteracy  ~Murder",
               xlab="Illiteracy",
               ylab="Murder%")
scatter.smooth(x=state_data$`Life Exp`, y=state_data$Murder,
               main="Correlation of Life_Exp  ~Murder",
               xlab="Life_Exp",
               ylab="Murder%")
scatter.smooth(x=state_data$HS_Grad, y=state_data$Murder,
               main="Correlation of HS_Grad  ~Murder",
               xlab="HS_Grad",
               ylab="Murder%")
scatter.smooth(x=state_data$Frost, y=state_data$Murder,
               main="Correlation of Frost  ~Murder",
               xlab="Frost",
               ylab="Murder%")
scatter.smooth(x=state_data$Area, y=state_data$Murder,
               main="Correlation of Area  ~Murder",
               xlab="Area",
               ylab="Murder%")
View(state_data)

cor(state_data)
paste("Correlation for murder and forst:",round(cor(state_data$Murder,state_data$Frost),2))
paste("Correlation for murder and population:",round(cor(state_data$Murder,state_data$Population),2))
paste("Correlation for murder and income:",round(cor(state_data$Murder,state_data$Income),2))
paste("Correlation for murder and illiteracy:",round(cor(state_data$Murder,state_data$Illiteracy),2))
paste("Correlation for murder and Life Exp:",round(cor(state_data$Murder,state_data$`Life Exp`),2))
paste("Correlation for murder and hs_grad:",round(cor(state_data$Murder,state_data$HS_Grad),2))
paste("Correlation for murder and area:",round(cor(state_data$Murder,state_data$Area),2))

windows(16,20)
par(mfrow = c(3, 3))
boxplot(state_data$Population, main = "Population")
boxplot(state_data$Income, main = "Income")
boxplot(state_data$Illiteracy, main = "Illiteracy")
boxplot(state_data$`Life Exp`, main = "Life Expectancy")
boxplot(state_data$Murder, main = "Murder")
boxplot(state_data$HS_Grad, main = "HS Graduation")
boxplot(state_data$Frost, main = "Frost")
boxplot(state_data$Area, main = "Area")

outlier_values <-boxplot.stats(state_data$Population)$out
paste0("Population outliers:", paste0(outlier_values, sep=", "))

states<-subset(state_data,
               state_data$Population!=9111 
               &state_data$Population!=10735)

outlier_income<- boxplot.stats(state_data$Income)$out

library(e1071)
windows(20,12)
par(mfrow=c(4,3))
 plot(density(state_data$Murder),
      main = "Density plot: Murder",
      ylab = "Frequency", xlab = "Murder",
      sub= paste0("Skewness:", round(e1071::skewness(state_data$Murder), 2)))
      polygon(density(state_data$Murder), col = "red")
      
      plot(density(state_data$Population),
           main = "Density plot: Population",
           ylab = "Frequency", xlab = "Population",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$Population), 2)))
      polygon(density(state_data$Population), col = "red")
      
      plot(density(state_data$Income),
           main = "Density plot: Income",
           ylab = "Frequency", xlab = "Income",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$Income), 2)))
      polygon(density(state_data$Income), col = "red")
      
      plot(density(state_data$Illiteracy),
           main = "Density plot: Illiteracy",
           ylab = "Frequency", xlab = "Illiteracy",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$Illiteracy), 2)))
      polygon(density(state_data$Illiteracy), col = "red")
      plot(density(state_data$`Life Exp`),
           main = "Density plot: Life Exp",
           ylab = "Frequency", xlab = "Life Exp",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$`Life Exp`), 2)))
      polygon(density(state_data$`Life Exp`), col = "red")
      plot(density(state_data$HS_Grad),
           main = "Density plot: HS_Grad",
           ylab = "Frequency", xlab = "HS_Grad",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$HS_Grad), 2)))
      polygon(density(state_data$HS_Grad), col = "red")
      
      plot(density(state_data$Frost),
           main = "Density plot: Frost",
           ylab = "Frequency", xlab = "Frost",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$Frost), 2)))
      polygon(density(state_data$Frost), col = "red")
      
      plot(density(state_data$Area),
           main = "Density plot: Area",
           ylab = "Frequency", xlab = "Area",
           sub= paste0("Skewness:", round(e1071::skewness(state_data$Area), 2)))
      polygon(density(state_data$Area), col = "blue")

#skewness of less than (<) -1 or greater than(>)1 = highly skewed
#-1 to -0.5 and 0.5 to 1 = moderately skewed
# skewness of -0.5 to 0.5 = approx symmetrical

paste("Skewness for Population :", round(e1071::skewness(state_data$Population),2))
paste("Skewness for Income :", round(e1071::skewness(state_data$Income),2))
paste("Skewness for Illiteracy :", round(e1071::skewness(state_data$Illiteracy),2))
paste("Skewness for Life exp :", round(e1071::skewness(state_data$`Life Exp`),2))
paste("Skewness for murder :", round(e1071::skewness(state_data$Murder),2))
paste("Skewness for HS_Grad :", round(e1071::skewness(state_data$HS_Grad),2))
paste("Skewness for Frost :", round(e1071::skewness(state_data$Frost),2))
paste("Skewness for Area :", round(e1071::skewness(state_data$Area),2))

shapiro.test(state_data$Population)
shapiro.test(state_data$Income)
shapiro.test(state_data$Illiteracy)
shapiro.test(state_data$`Life Exp`)
shapiro.test(state_data$Murder)
shapiro.test(state_data$HS_Grad)
shapiro.test(state_data$Frost)
shapiro.test(state_data$Area)

install.packages("MASS")
library(MASS)
attach(state_data)
windows(20,12)
box_cox_transform<- boxcox(Murder~Population)
box_cox_transform
lamda <- box_cox_transform$x[which.max(box_cox_transform$y)]
lamda
normalised_population<-(Murder^lamda-1)/lamda
normalised_population

hist(normalised_population)
shapiro.test(normalised_population)

state_data$Population_new <- normalised_population
shapiro.test(state_data$Population_new)
View(state_data)
state_data$Illiteracy_new <- normalised_population
shapiro.test(state_data$Illiteracy_new)
state_data$Income_new <- normalised_population
shapiro.test(state_data$Income_new)
state_data$Murder_new <- normalised_population
shapiro.test(state_data$Murder_new)
state_data$HS_Grad_new <- normalised_population
shapiro.test(state_data$HS_Grad_new)
state_data$Frost_new <- normalised_population
shapiro.test(state_data$Frost_new)
state_data$Area_new <- normalised_population
shapiro.test(state_data$Area_new)

str(state_data)
attach(state_data)
model_1<- lm(Murder~Population_new+
               Income+Illiteracy_new+
               `Life Exp`+HS_Grad_new+Area+Frost)
model_1

summary(model_1)






