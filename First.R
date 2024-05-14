state_data <- as.data.frame(state.x77)
state_data
View(state_data)
pairs(state_data)
cor(state_data)

names(state_data)[names(state_data) == "life_exp"] <- "Life_Exp"
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
scatter.smooth(x=state_data$Life_Exp, y=state_data$Murder,
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
cor(state_data)
paste("Correlation for murder and forst:",round(cor(state_data$Murder,state_data$Frost),2))
paste("Correlation for murder and population:",round(cor(state_data$Murder,state_data$Population),2))
paste("Correlation for murder and income:",round(cor(state_data$Murder,state_data$Income),2))
paste("Correlation for murder and illiteracy:",round(cor(state_data$Murder,state_data$Illiteracy),2))
paste("Correlation for murder and life_exp:",round(cor(state_data$Murder,state_data$Life_Exp),2))
paste("Correlation for murder and hs_grad:",round(cor(state_data$Murder,state_data$HS_Grad),2))
paste("Correlation for murder and area:",round(cor(state_data$Murder,state_data$Area),2))

