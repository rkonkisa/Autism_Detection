library(ggplot2)
library(plyr)
library(readr)
library(dplyr)
library(caret)
library(reshape2)
library(randomForest)
require(caTools)
library(randomForest)
library(mlbench)
library(caret)

data <- read.csv(file.choose())
head(data)

summary(data)

sum(is.na(data))

geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)


ggplot(data, aes(x=contry_of_res, y=austim)) + 
  geom_point()+
  geom_smooth(method=lm)


geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

ggplot(data, aes(x=contry_of_res, y=result)) + 
  geom_point()+
  geom_smooth(method=lm)


geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

ggplot(data, aes(x=contry_of_res, y=jundice)) + 
  geom_point()+
  geom_smooth(method=lm)



geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=contry_of_res, y=ethnicity)) + 
  geom_point()+
  geom_smooth(method=lm)


geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=contry_of_res, y=age)) + 
  geom_point()+
  geom_smooth(method=lm)


geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=contry_of_res, y=gender)) + 
  geom_point()+
  geom_smooth(method=lm)



geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=gender, y=Class.ASD )) + 
  geom_point()+
  geom_smooth(method=lm)


geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=age, y=Class.ASD )) + 
  geom_point()+
  geom_smooth(method=lm)



geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=ethnicity, y=Class.ASD )) + 
  geom_point()+
  geom_smooth(method=lm)



geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=contry_of_res, y=Class.ASD )) + 
  geom_point()+
  geom_smooth(method=lm)


geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Add the regression line
ggplot(data, aes(x=jundice, y=Class.ASD )) + 
  geom_point()+
  geom_smooth(method=lm)


cormat <- round(cor(data),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

# Melt the correlation matrix

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

hist(data$age)


index <-  createDataPartition(data$Class.ASD,p= .8, times=1, list=F)

train <-  data[index,]
test <- data[-index,]


logistic<- glm(Class.ASD~., data=train, family='binomial')

summary(logistic)

pred= predict(logistic, test, type='response')
pred


pred = ifelse(pred > 0.40, 1, 0)
pred= as.factor(pred)

matrix = confusionMatrix(pred,as.factor(test$Class.ASD))
matrix

dim(data)
dim(train)
dim(test)

summary(data)


data$Class.ASD =as.factor(data$Class.ASD)

dataset <- data
x <- data[,1:8]
y <- data["Class.ASD"]

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
#metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Class.ASD~., data=data, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_default)


# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Class.ASD~., data=dataset, method="rf", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)
























