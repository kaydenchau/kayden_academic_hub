# Read data and discover some basic information
rawdata <- read.csv("KidneyData.csv")
attach(rawdata)
head(rawdata)
names(rawdata)
dim(rawdata)
sapply(rawdata,class)
str(rawdata)
summary(rawdata)
table(rawdata$KidneyDisease)

# Declare libraries used in this script
library(ggplot2)
library(dplyr)
library(corrplot)
library(boot)
library(gridExtra)
library(caret)

# Step 1: Add new feature TdsEcRatio = TotalDissolvedSolids / ElectricConductivity
data.add <- cbind(rawdata, 
                  TdsEcRatio=rawdata$TotalDissolvedSolids / 
                    rawdata$ElectricConductivity)

# Plot boxplot
par(mfrow=c(4,4))
for ( f in 1:16) {
  boxplot(data.add[c(-19,-8,-3,-1)][f][data.add[19]==1],
          data.add[c(-19,-8,-3,-1)][f][data.add[19]==0],
          names = c("Yes", "No"),
          main=colnames(data.add[c(-19,-8,-3,-1)])[f])
}

# Plot correlation matrix
cor_matrix = cor(data.add[c(-8,-3,-1)])
par(mfrow=c(1,1))
corrplot(cor_matrix, method = 'color',
         addCoef.col = "#2C2C2C", # Add coefficient of correlation
         number.cex = 0.5,
         tl.col="black", 
         tl.srt=45,#Text label color and rotation
         order = 'AOE', tl.cex=0.6, cl.cex = 0.7)

data.add$KidneyDisease = factor(data.add$KidneyDisease)

# Plot histogram
theme_set(
  theme_classic()
)
p <- list()
i=0
for ( f in 1:17) {
  if (f == 16)
  { next }
  i = i + 1
  p[[i]] <- ggplot(data.add[c(-8,-3,-1)], 
                   aes_string(x = colnames(data.add[c(-8,-3,-1)])[f])) +
    aes(fill = KidneyDisease) +
    scale_x_continuous(guide = guide_axis(check.overlap = TRUE))+
    geom_histogram(color = "white", alpha=0.4,
                   position = "identity", bins=15) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    labs(y="") +
    theme(legend.position = "none",
          axis.title.x = element_text(size=8, face = "bold"),
          axis.text = element_text(size=7))
}
grid.arrange(grobs = lapply(p, "+", 
                            theme(plot.margin=margin(10,10,10,10))), cols = 2)

# Step 2: Remove column PatientID and encode categorical columns (Gender, SmokingStatus)
data.encode = data.add[c(-1)]
data.encode$Gender <-  as.integer(factor(data.encode$Gender))
data.encode$SmokingStatus <-  as.integer(factor(data.encode$SmokingStatus))

# Step 3: Remove column PatientID and encode categorical columns (Gender, SmokingStatus)
data.scaled = data.encode
data.scaled <- data.scaled %>% mutate(across(where(is.numeric), scale))

# Step 4: Split train set (80%) and test set (20%)
tr.id=sample(1:nrow(data.scaled),nrow(data.scaled)* 0.8)
train=data.scaled[tr.id,]
test=data.scaled[-tr.id,]
test.features = test[-18]
test.target = as.numeric(test[,18])-1
str(train)


# Logistic Regression Model

# Build model with all features
model=glm(KidneyDisease ~ ., data=train ,family=binomial)
summary(model)

# Define a function to evaluate model accuracy
evaluate <- function(model, test.features, test.target) {
  predicted <- as.numeric(predict (model, test.features, type="response")> .5)
  xtab <- table(predicted, test.target)
  cm <- caret::confusionMatrix(xtab)
  return (cm)
}

# Build model based on selected features which have significant coefficients 
model.select=glm(KidneyDisease ~ BloodPressure +
                   pH +
                   DissolvedOxygen +
                   Turbidity +
                   TotalDissolvedSolids +
                   NitriteLevel +
                   Humidity +
                   TdsEcRatio, data=train ,family=binomial)

model.select2=glm(KidneyDisease ~ BloodPressure +
                    pH +
                    DissolvedOxygen +
                    Turbidity +
                    TotalDissolvedSolids +
                    Humidity +
                    TdsEcRatio, data=train ,family=binomial)
model.select3=glm(KidneyDisease ~ BloodPressure +
                    pH +
                    DissolvedOxygen +
                    Turbidity +
                    TotalDissolvedSolids +
                    TdsEcRatio, data=train ,family=binomial)
model.select4=glm(KidneyDisease ~ BloodPressure +
                    ElectricConductivity +
                    pH +
                    DissolvedOxygen +
                    Turbidity +
                    TotalDissolvedSolids +
                    NitriteLevel +
                    Humidity +
                    TdsEcRatio, data=train ,family=binomial)
model.select5=glm(KidneyDisease ~ BloodPressure +
                    DissolvedOxygen +
                    Turbidity +
                    TotalDissolvedSolids +
                    TdsEcRatio, data=train ,family=binomial)

# Evaluate model accuracy
summary.glm(model)
evaluate(model, test.features, test.target)

summary(model.select)
evaluate(model.select, test.features, test.target)

summary(model.select2)
evaluate(model.select2, test.features, test.target)

summary(model.select3)
evaluate(model.select3, test.features, test.target)

summary(model.select4)
evaluate(model.select4, test.features, test.target)

summary(model.select5)
evaluate(model.select5, test.features, test.target)

# Apply cross-validation to model similar to the best one so far
train.cv = train
str(train.cv)
train.cv$KidneyDisease = as.numeric(train.cv$KidneyDisease)
train.cv$KidneyDisease[train.cv$KidneyDisease==1] = "No"
train.cv$KidneyDisease[train.cv$KidneyDisease==2] = "Yes"
test.target.cv = test.target
test.target.cv[test.target==0] = "No"
test.target.cv[test.target==1] = "Yes"

ctrlspecs <- trainControl(method="cv", 
                          number=10, 
                          savePredictions="all",
                          classProbs=TRUE)
set.seed(3)
model.cv <- train(KidneyDisease ~  ., 
                data=train.cv, 
                method="glm", 
                family=binomial, 
                trControl=ctrlspecs)

model.select.cv <- train(KidneyDisease ~ BloodPressure +
                           pH +
                           DissolvedOxygen +
                           Turbidity +
                           TotalDissolvedSolids +
                           NitriteLevel +
                           Humidity +
                           TdsEcRatio, 
                  data=train.cv, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)

model.select2.cv <- train(KidneyDisease ~ BloodPressure +
                            pH +
                            DissolvedOxygen +
                            Turbidity +
                            TotalDissolvedSolids +
                            Humidity +
                            TdsEcRatio, 
                  data=train.cv, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)

model.select3.cv <- train(KidneyDisease ~ BloodPressure +
                            pH +
                            DissolvedOxygen +
                            Turbidity +
                            TotalDissolvedSolids +
                            TdsEcRatio, 
                  data=train.cv, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)

model.select4.cv <- train(KidneyDisease ~ BloodPressure +
                            ElectricConductivity +
                            pH +
                            DissolvedOxygen +
                            Turbidity +
                            TotalDissolvedSolids +
                            NitriteLevel +
                            Humidity +
                            TdsEcRatio, 
                  data=train.cv, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)

model.select5.cv <- train(KidneyDisease ~ BloodPressure +
                            DissolvedOxygen +
                            Turbidity +
                            TotalDissolvedSolids +
                            TdsEcRatio, 
                  data=train.cv, 
                  method="glm", 
                  family=binomial, 
                  trControl=ctrlspecs)

predicted <- predict (model.cv, newdata=test.features)
confusionMatrix(data=predicted, factor(test.target.cv))
print(model.cv)

predicted <- predict (model.select.cv, newdata=test.features)
confusionMatrix(data=predicted, factor(test.target.cv))
print(model.select.cv)

predicted <- predict (model.select2.cv, newdata=test.features)
confusionMatrix(data=predicted, factor(test.target.cv))
print(model.select2.cv)

predicted <- predict (model.select3.cv, newdata=test.features)
confusionMatrix(data=predicted, factor(test.target.cv))
print(model.select3.cv)

predicted <- predict (model.select4.cv, newdata=test.features)
confusionMatrix(data=predicted, factor(test.target.cv))
print(model.select4.cv)


predicted <- predict (model.select5.cv, newdata=test.features)
confusionMatrix(data=predicted, factor(test.target.cv))
print(model.select5.cv)

summary(model.select5)

#================= PART B HERE=====================#


# Build model with all features
d=5
model.poly.all= glm(KidneyDisease ~ poly(Age,d)
               + poly(Gender,1)
               + poly(BloodPressure,2)
               + poly(BloodSugar,d)
               + poly(Cholesterol,1)
               + poly(BMI,4)
               + poly(SmokingStatus,2)
               + poly(ElectricConductivity,2)
               + poly(pH,d)
               + poly(DissolvedOxygen,3)
               + poly(TdsEcRatio,2)
               + poly(NitriteLevel,3)
               , data=train ,family=binomial)
summary.glm(model.poly.all)

# Build model with significant features and measure accuracy
model.poly.filter = glm(KidneyDisease ~ 
                 poly(BloodPressure,1,raw=TRUE)
               + poly(ElectricConductivity,2,raw=TRUE)
               + poly(pH,1,raw=TRUE)
               + poly(DissolvedOxygen,1,raw=TRUE)
               + poly(TdsEcRatio,1,raw=TRUE)
               , data=train ,family=binomial)

summary.glm(model.poly.filter)
evaluate(model.poly.filter, test.features, test.target)

# Reducing features and measure accuracy
model.poly.filter1 = glm(KidneyDisease ~ 
                          poly(BloodPressure,1,raw=TRUE)
                        + poly(ElectricConductivity,2,raw=TRUE)
                        + poly(DissolvedOxygen,1,raw=TRUE)
                        + poly(TdsEcRatio,1,raw=TRUE)
                        , data=train ,family=binomial)

summary.glm(model.poly.filter1)
evaluate(model.poly.filter1, test.features, test.target)

model.poly.filter2 = glm(KidneyDisease ~ 
                           poly(BloodPressure,1,raw=TRUE)
                         + poly(ElectricConductivity,2,raw=TRUE)
                         + poly(DissolvedOxygen,1,raw=TRUE)
                         , data=train ,family=binomial)

summary.glm(model.poly.filter2)
evaluate(model.poly.filter2, test.features, test.target)

model.poly.filter3 = glm(KidneyDisease ~ 
                           poly(BloodPressure,1,raw=TRUE)
                         + poly(ElectricConductivity,2,raw=TRUE)
                         , data=train ,family=binomial)

summary.glm(model.poly.filter3)
evaluate(model.poly.filter3, test.features, test.target)

model.poly.filter4 = glm(KidneyDisease ~ 
                           poly(ElectricConductivity,2,raw=TRUE)
                         + poly(DissolvedOxygen,1,raw=TRUE)
                         , data=train ,family=binomial)

summary.glm(model.poly.filter4)
evaluate(model.poly.filter4, test.features, test.target)

#---------------------DONE poly------

library(tree)
# Data pre-processing for decision tree model
data.tree = rawdata[c(-1)]
data.tree$KidneyDisease=ifelse(data.tree$KidneyDisease==0,'N','Y')
data.tree$KidneyDisease=as.factor(data.tree$KidneyDisease)
data.tree$Gender=as.factor(data.tree$Gender)
data.tree$SmokingStatus=as.factor(data.tree$SmokingStatus)
# Split train and test set
train.tree=data.tree[tr.id,]
test.tree=data.tree[-tr.id,]
test.tree.features = test.tree[-18]
test.tree.target = test.tree[,18]
# Check dim and type of data after pre-processing
sapply(data.tree,class)
dim(train.tree)
dim(test.tree)

# Build tree model with all features
tree_model = tree(KidneyDisease~.,train.tree)
summary(tree_model)

# Plot tree
plot(tree_model)
text(tree_model,pretty = 0, cex=0.5)

# Evaluate the model
tree_pred = predict(tree_model,test.tree.features,type="class")
table(tree_pred,test.tree.target)
tab1 <- table(tree_pred,test.tree.target)
MissclassificationRate <- (tab1[1,2]+tab1[2,1])/sum(tab1)
MissclassificationRate

# Find the best size of tree
cv.tree_model=cv.tree(tree_model,FUN=prune.misclass)
names(cv.tree_model)
plot(cv.tree_model$size, cv.tree_model$dev, type = "b")

# Prune tree model
prune.tree=prune.misclass(tree_model,best=5)
plot(prune.tree)
text(prune.tree,pretty=0, cex=0.5)

# Evaluate the pruned model
tree_pred = predict(prune.tree,test.tree.features,type="class")
table(tree_pred,test.tree.target)
tab1 <- table(tree_pred,test.tree.target)
MissclassificationRate <- (tab1[1,2]+tab1[2,1])/sum(tab1)
MissclassificationRate
caret::confusionMatrix(tab1)
print(prune.tree)


#-----------DONE tree--------------

# Implementing clustering
hh = hclust(dist(data.scaled[,-18]), method="ward.D2")
plot(hh, xlab=" ", sub = paste("Complete"," link cluster analysis"))
rect.hclust(hh, k=9)

# Extract membership
cut_avg <- cutree(hh, k=9)
# Merge cluster result with dataset
seeds_df_cl <- mutate(data.scaled, cluster = cut_avg)

# Pivot cluster and kidney disease class
df1 <- seeds_df_cl %>% count(cluster) %>% rename(n = n)
cluster.nkd <- seeds_df_cl %>% filter(KidneyDisease==0)
df2 <- cluster.nkd %>% count(cluster) %>% rename(WithoutKD = n)
cluster.kd <- seeds_df_cl %>% filter(KidneyDisease==1)
df3 <- cluster.kd %>% count(cluster) %>% rename(KD = n)

df_list <- list(df1, df2, df3)
df <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
df <- replace_na(df,list(n=0,WithoutKD=0,KD=0))
df$KD.rate <- df$KD / df$n
df

mean(df$KD.rate)






cluster1 <- seeds_df_cl %>% filter(cluster==9)
cluster1$Gender <- round(cluster1$Gender,0)
cluster1$SmokingStatus <- round(cluster1$SmokingStatus,0)

group.col <- 
  ifelse(cluster1$Gender == -1 & cluster1$KidneyDisease == 0, '#458B74'
         , ifelse(cluster1$Gender == 1 & cluster1$KidneyDisease == 0, '#7FFFD4'
                  , ifelse(cluster1$Gender == -1 & cluster1$KidneyDisease == 1, '#8B5A00'
                           , '#FFA500')))
group.pch <- as.factor(cluster1$SmokingStatus)
plot(cluster1$Age, cluster1$BloodPressure, pch = as.numeric(group.pch), col = group.col)

#------------

seeds_df_cl$Gender <- round(seeds_df_cl$Gender,0)
seeds_df_cl$SmokingStatus <- round(seeds_df_cl$SmokingStatus,0)

group.col <- 
  ifelse(seeds_df_cl$Gender == -1 & seeds_df_cl$KidneyDisease == 0 & seeds_df_cl$cluster==1, '#458B74'
         , ifelse(seeds_df_cl$Gender == 1 & seeds_df_cl$KidneyDisease == 0 & seeds_df_cl$cluster==1, '#7FFFD4'
                  , ifelse(seeds_df_cl$Gender == -1 & seeds_df_cl$KidneyDisease == 1 & seeds_df_cl$cluster==1, '#8B5A00'
                           , ifelse(seeds_df_cl$Gender == 1 & seeds_df_cl$KidneyDisease == 1 & seeds_df_cl$cluster==1,'#FFA500', '#000000'))))
group.pch <- as.factor(seeds_df_cl$SmokingStatus)
plot(seeds_df_cl$Age, seeds_df_cl$TdsEcRatio, pch = as.numeric(group.pch), col = group.col)

plot(seeds_df_cl$Age, seeds_df_cl$TdsEcRatio,pch=16,col=seeds_df_cl$cluster)
plot(seeds_df_cl[,1:4], pch=20, cex=1.5, col=seeds_df_cl$cluster)

print(cut_avg)

'#458B74'
'#7FFFD4'
'#FFA500'
'#8B5A00'

