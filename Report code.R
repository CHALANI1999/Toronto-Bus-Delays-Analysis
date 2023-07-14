
data=read.csv("D:/3RD YEAR/SEMESTER II/ST3082/Project I Materials//ttc-bus-delay-data-2022.csv")
View(data)
head(data)
summary(data)
str(data)
library(plyr)
library(dplyr)
library(stringr)
library(mgsub)
library(ggplot2)
library(corrplot)
library(psych)
library(packHV)
library(moments)
library(tidyverse)
library(Metrics)


################################   Pre- processing and Feature Engineering  #######################################

#Removing duplicates
sum(duplicated(data))
data=distinct(data)

#Replacing "" with NA
data = replace(data, data=='', NA)
colSums(is.na(data))

#Removing records with delay=0
data=data[data$Min.Delay != 0, ] 
nrow(data)

#Removing records with gap=0
data=data[data$Min.Gap != 0, ] 
nrow(data)

#Create Month column
data = data %>%
  mutate(Month = str_extract(Date, "[:alpha:]+")
  )%>%
  
  mutate(
    Month = ifelse( str_detect(Date, "Jan"),"January", Month)
  ) %>%
  
  mutate(
    Month = ifelse( str_detect(Date, "Feb"),"February", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "Mar"),"March", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "Apr"),"April", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "May"),"May", Month)
  ) %>%
  mutate(
    Month = ifelse( str_detect(Date, "Jun"),"June", Month)
  ) 


#Cleaning Direction column
table(data$Direction)
which(data$Direction=="/")
which(data$Direction=="2")
which(data$Direction=="6")
which(data$Direction=="D")
which(data$Direction=="I")
which(data$Direction=="J")
which(data$Direction=="Q")
data=data %>%  filter(!row_number() %in% c(76,19133,21555,6669,11507,152,4249,10326,13986))
nrow(data)

#Create Route_New column
x=c(table(data$Route))
which(data$Route=="RAD")
which(data$Route=="OTC")
data$Route[5619]=1000
data$Route[7346]=1000
data$Route[19679]=1000
data$Route[13053]=1001

Route_New=c()
for(i in 1:nrow(data)){
  if((!is.na(data$Route[i]))&& (as.numeric(substr(data$Route[i],1,4))>=7)&(as.numeric(substr(data$Route[i],1,4))<=189)){
    Route_New[i]="Regular and limited service routes"
    
  }else if((!is.na(data$Route[i]))&&(as.numeric(substr(data$Route[i],1,4))>299)&(as.numeric(substr(data$Route[i],1,4))<400)){
    Route_New[i]="Blue Night Routes"
    
  }else if((!is.na(data$Route[i]))&&(as.numeric(substr(data$Route[i],1,4))>399)&(as.numeric(substr(data$Route[i],1,4))<500)){
    Route_New[i]="Community Routes"
    
  }else if((!is.na(data$Route[i]))&&(as.numeric(substr(data$Route[i],1,4))>899)&(as.numeric(substr(data$Route[i],1,4))<1000)){
    Route_New[i]="Express Routes"
    
  }else if(!is.na(data$Route[i])){
    Route_New[i]="Others"
  }else{
    Route_New[i]=NA
  }
}
table(Route_New)
data=cbind(data,Route_New)



#Create Hour column
Hour=c()
for(i in 1:length(data$Time)){
  Hour[i]=as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))
}
data=cbind(data,Hour)

#Create Incident_New column
table(Incident_New)
Incident_New=c()
for(i in 1:length(data$Incident)){
  if((data$Incident[i]=="Cleaning - Disinfection")|(data$Incident[i]=="Held By")|
     (data$Incident[i]=="Late Entering Service")){
    Incident_New[i]="Others"
  }else if(data$Incident[i]=="Road Blocked - NON-TTC Collision"){
    Incident_New[i]="Road Blocked"
  }else{
    Incident_New[i]=data$Incident[i]
  }
  
}
data=cbind(data,Incident_New)

#Create is_Weekday column
is_Weekday=c()
for(i in 1:nrow(data)){
  if((data$Day[i]=="Saturday") | (data$Day[i]=="Sunday") ) {
    is_Weekday[i]="Weekend"
  }else{
    is_Weekday[i]="Weekday"
  }
}
data=cbind(data,is_Weekday)








#Cleaning Data I
y=c()

for(i in 1:nrow(data)){
  if((!is.na(data$Route_New[i]))&&( (data$Route_New[i]=="Regular and limited service routes")&
                                    (data$is_Weekday[i]=="Weekday")&((data$Hour[i]>=1)&(data$Hour[i]<6)))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&&( (data$Route_New[i]=="Regular and limited service routes")&
                                          (data$is_Weekday[i]=="Weekend")&((data$Hour[i]>=1)&(data$Hour[i]<8)))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Blue Night Routes")&
                                          (data$is_Weekday[i]=="Weekday")&(data$Hour[i]<1)&(data$Hour[i]>=6))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Blue Night Routes")&
                                          (data$is_Weekday[i]=="Weekend")&(data$Hour[i]<1)&(data$Hour[i]>=8))){
    y=append(y,i)
    
    
  }else{
    y=append(y,"")
  }
}
y
y=y[y != ""]
length(y)
data=data %>%  filter(!row_number() %in% as.numeric((y)))
nrow(data)
colSums(is.na(data))


########################################################
#Factoring
data$Route_New = factor(data$Route_New,level=c("Regular and limited service routes","Blue Night Routes",
                                               "Express Routes","Others"))
data$Day = factor(data$Day,level=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data$Month = factor(data$Month,level=c("January","February","March","April","May","June"))
data$Incident_New = factor(data$Incident_New,level=c("Cleaning - Unsanitary","Diversion","General Delay","Mechanical","Security","Vision",
                                                     "Collision - TTC","Emergency Services","Investigation","Operations - Operator",
                                                     "Road Blocked","Utilized Off Route","Others"))
data$Direction=factor(data$Direction,level=c("N","S","E","W","B"))
data$is_Weekday=factor(data$is_Weekday,level=c("Weekday","Weekend"))
data$Hour=as.factor(data$Hour)


#Removing Vehicle Number column
data=subset(data,select=-Vehicle)
str(data)
sum(table(data$Route_New))
#Splitting the data in to training and testing sets
set.seed(100)
indexes=sample(1:nrow(data),0.2*nrow(data))
testset=data[indexes,]
trainset=data[-indexes,]
View(trainset)
View(testset)
head(trainset)
head(testset)
nrow(trainset)
nrow(testset)

#Imputing missing values
colSums(is.na(trainset))
#Mode function.
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Calculate the mode of Route variable.
result1 = getmode(v=trainset$Route_New)
print(result1)
# Calculate the mode of Direction variable.
result2 = getmode(v=trainset$Direction)
print(result2)

table(trainset$Direction)


z1=which(is.na(trainset$Route_New))

h1=c()
w1=c()
for(i in 1:length(z1)){
  h1[i]=trainset$Hour[z1[i]]
  w1[i]=trainset$is_Weekday[z1[i]]
}

z2=which(is.na(testset$Route_New))


h2=c()
w2=c()
for(i in 1:length(z2)){
  h2[i]=testset$Hour[z2[i]]
  w2[i]=testset$is_Weekday[z2[i]]
}

trainset$Route_New[is.na(trainset$Route_New)] ="Regular and limited service routes"
trainset$Route_New[15943]="Blue Night Routes"
trainset$Route_New[17936]="Blue Night Routes"
testset$Route_New[is.na(testset$Route_New)] ="Regular and limited service routes"
trainset$Direction=factor(trainset$Direction,level=c("N","S","E","W","B","TBD"))
testset$Direction=factor(testset$Direction,level=c("N","S","E","W","B","TBD"))
trainset$Direction[is.na(trainset$Direction)] ="TBD"
testset$Direction[is.na(testset$Direction)] ="TBD"


trainset=subset(trainset,select=-c(Date,Route,Time,Location,Incident))
testset=subset(testset,select=-c(Date,Route,Time,Location,Incident))



#Histogram and Boxplot
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset$Min.Delay,main="Delay time distribution",col="#9494b8",xlab="Dealy time(In minutes)")
boxplot.stats(trainset$Min.Delay)$stats
x=which(trainset$Min.Delay %in% boxplot.stats(trainset$Min.Delay)$out)
length(x)

y=c()
for(i in 1:length(x)){
  if(trainset$Min.Delay[x[i]]>180){
    y=append(y,x[i])
    
  }else{
    
  }
  
}
y
length(y)
trainset_new=trainset %>%  filter(!row_number() %in% y)

#################################PLS###################################################

library(mdatools)
library(caret)
y=trainset_new$Min.Delay
dummy_coding= dummyVars(" ~ . ", data = trainset_new)
trainset_encoded=predict(dummy_coding, newdata =trainset_new)

View(trainset_encoded)

x=trainset_encoded[,-8]
x[,8]=scale(x[,8], center = TRUE, scale = TRUE)
y=as.matrix(trainset_encoded[,8])
set.seed(100)
ModelPLS = pls(x,y,cv=5, info = "Delay time prediction")
#view summary of model fitting
summary(ModelPLS)

#visualize CV plots
dev.off()
plot(ModelPLS)
plotXScores(ModelPLS,show.label = FALSE)
plotXYLoadings(ModelPLS,show.label = FALSE)
plotVIPScores(ModelPLS,ncomp=9, type = "h",show.label = FALSE)
summary(ModelPLS$coeffs)
plot(ModelPLS$coeffs,show.label = TRUE)
summary(ModelPLS$res$cal)
#Checking Outliers
Model0=setDistanceLimits(ModelPLS,lim.type = ModelPLS$lim.type,alpha=0.05)
plotXYResiduals(Model0,show.labels=FALSE,labels="indices")
# Identify outlier indices
# get row indices for outliers in calibration set
outliers = which(categorize(Model0, ModelPLS$res$cal) == "outlier")
length(outliers)
trainset_new=trainset_new %>%  filter(!row_number() %in% outliers)
str(trainset_new)
nrow(trainset_new)

###########################Cluster Analysis####################################################
library(cluster)
library(Rtsne)

#Computing gower distance

df=trainset_new
dummy_coding= dummyVars(" ~ . ", data = df)
df_encoded=predict(dummy_coding, newdata =df)
df_encoded=as.data.frame(df_encoded)
View(df_encoded)
df_encoded[,c(8,9)]=scale(df_encoded[,c(8,9)], center = TRUE, scale = TRUE)
df_encoded[,-c(8,9)] = lapply(df_encoded[,-c(8,9)], factor)
set.seed(100)
gower_sam1=sample(1:nrow(df),size=1000,replace=FALSE)
df1=df_encoded[gower_sam1,]
gd1=daisy(df1,metric="gower")
gm1=as.matrix(gd1)
View(df1)
df1[which(gm1==min(gm1[gm1!=min(gm1)]),arr.ind = TRUE)[1,],]
df1[which(gm1==max(gm1[gm1!=max(gm1)]),arr.ind = TRUE)[1,],]

#Graph to identify the number of clusters
sil_dist=c(NA)
for(i in 2:8){
  pam_fit1=pam(gd1,diss=TRUE,k=i)
  sil_dist[i]=pam_fit1$silinfo$avg.width
}
dev.off()
plot(1:8,sil_dist,
     xlab = "k",
     ylab="Average silhoutte distance",
     lines(1:8,sil_dist))


# Partitioning around medoids with 3 clusters
pam_fit = pam(gd1, diss = TRUE, k = 3)

# Visualizing the clusters
library(Rtsne)
tsne_obj= Rtsne::Rtsne(gd1, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))

#############################################################################################
#Creating the model matrix 
View(trainset_new)
data1=trainset_new


data1[,3]=scale(data1[,3], center = TRUE, scale = TRUE)
data1[,3]=as.numeric(data1[,3])
View(data1)

# Create model matrix with categorical variables as dummies and numerical variable as is
#model_matrix = model.matrix(formula, data = data1)
model_matrix = model.matrix(Min.Delay~., data = data1)[,-1]
#model_matrix = model.matrix(Min.Delay~., data = df)

# Separate the response variable from the predictors
y1 = data1$Min.Delay
#y1 = df$Min.Delay
is.matrix(model_matrix)
dim(model_matrix)
# Create a sequence of lambda values to test
lambda_seq = 10^seq(10, -2, length = 100)
library(glmnet)
# Use cv.glmnet to perform cross-validation and select the optimal lambda value
#cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq)
set.seed(100)
cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq,nfolds=10,standardize=FALSE)

# View the optimal lambda value
best_lambda2=cv_fit$lambda.1se
# Plot the cross-validation results
dev.off()
plot(cv_fit)

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 0, lambda = best_lambda)
model_cv = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda2,standardize = FALSE)


y_hat_ridge = predict(model_cv, model_matrix)
library(Metrics)
rmse_m = rmse(y1, y_hat_ridge)
rmse_m
actual=y1
pred1=c(y_hat_ridge)


coef(model_cv ,s=best_lambda2)
#For the test set
data2=testset


data2[,3]=scale(data2[,3], center = TRUE, scale = TRUE)
data2[,3]=as.numeric(data2[,3])
View(data2)
nrow(data2)
model_matrix1 = model.matrix(Min.Delay~., data = data2)[,-1]

# Separate the response variable from the predictors
#y1 = data1$Min.Delay
y2 = data2$Min.Delay

model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda2,standardize = FALSE)


y_hat_ridge2 = predict(model_cv2, model_matrix1)

mape_m = mape(y2, y_hat_ridge2)
rmse_m = rmse(y2, y_hat_ridge2)
mape_m
rmse_m


#Lasso Regression
# Perform 10-fold cross-validation to select lambda ---------------------------

set.seed(100)
lasso_cv = cv.glmnet(x = model_matrix, y=y1, alpha = 1, lambda = lambda_seq, nfolds = 10,standardize=FALSE)
# Plot cross-validation results
dev.off()
plot(lasso_cv)

# Best cross-validated lambda

lambda_cv2 = lasso_cv$lambda.1se

# Fit final model, get its sum of squared residuals and multiple R-squared

model_cv = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv2,standardize = FALSE)


y_hat_lasso = predict(model_cv, model_matrix)

rmse_m = rmse(y1, y_hat_lasso)
rmse_m


#rsq_lasso_cv = cor(y, y_hat_cv)^2

coef(model_cv2 ,s=lambda_cv2)


# See how increasing lambda shrinks the coefficients --------------------------
# Each line shows coefficients for one variables, for different lambdas.
# The higher the lambda, the more the coefficients are shrinked towards zero.
res = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq,standardize = FALSE)
plot(res, xvar = "lambda")



#For the test set


model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv2,standardize = FALSE)

y_hat_lasso2 = predict(model_cv2, model_matrix1)

mape_m = mape(y2, y_hat_lasso2)
rmse_m = rmse(y2, y_hat_lasso2)
mape_m
rmse_m

#Elastic Net Regression

set.seed(100)

train_control = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             search = "random",
                             verboseIter = TRUE)

elastic_net_model = train(Min.Delay ~ .,
                          data = data1,
                          method = "glmnet",
                          trControl = train_control)


# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data1[,-2])
actual=y1
pred=c(y_hat_enet)
mape_m = mape(y1, pred)
rmse_m = rmse(y1, pred)
mape_m
rmse_m



#For the test set
#elastic_net_model1 = train(log_delay ~ .,data = data2,method = "glmnet",trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data2[,-2])

actual=y2
pred=c(y_hat_enet )
mape_m = mape(y2, pred)
rmse_m = rmse(y2, pred)
mape_m
rmse_m
















#Random Forest

set.seed(100)
indexes=sample(1:nrow(data1),0.1*nrow(data1))
data_new=data1[indexes,]

# Define the tuning grid
tune_grid = expand.grid(
  mtry =seq(20,50,by=5),
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  Min.Delay ~ .,
  data = data_new,
  
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1[,-2])
actual=y1
pred=c(y_hat_enet)
mae_m = mae(y1, pred)
rmse_m = rmse(y1, pred)
mae_m
rmse_m


#For the test set
y_hat_enet = predict(rf_model, data2[,-2])
actual=y2
pred=c(y_hat_enet )
mape_m = mape(y2, pred)
rmse_m = rmse(y2, pred)
mape_m
rmse_m


# get variable importance
# Print the variable importance measures
var_imp=rf_model$finalModel$variable.importance
# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


var_imp = data.frame(Variables = names(rf_model$finalModel$variable.importance), 
                     Importance = rf_model$finalModel$variable.importance, 
                     row.names = NULL)

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

#########################################################################################
df=var_imp[order(-var_imp$Importance),]
View(df)
df[c(1:41),]

nrow(df)

# Calculate number of variables to be removed

vars_to_remove = df$Variables[42:56]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)

base_var=c("Route_NewRegular and limited service routes","DayMonday","MonthJanuary","Incident_NewCleaning - Unsanitary","DirectionN","is_WeekdayWeekday","Hour0" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("Min.Delay", "Min.Gap")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)
set.seed(100)
indexes=sample(1:nrow(data1_imp),0.1*nrow(data1_imp))
data_new=data1_imp[indexes,]
# Define the tuning grid
tune_grid = expand.grid(
  mtry =41,
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  Min.Delay ~ .,
  data = data_new,
  
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1_imp[,-7])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1, pred)
rmse_m




#For the test set
# Calculate number of variables to be removed
# Remove least important variables from data1

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)

data2_imp=as.data.frame(data2_imp)

# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

y_hat_enet = predict(rf_model, data2_imp[,-7])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m




########################################################################################################
#Xgboost
library(xgboost)
Xtrain = xgb.DMatrix(data = as.matrix(data1[,-2]),label=y1)
y_train = data1$sMin.Delay
Xtest = xgb.DMatrix(data = as.matrix(data2[,-2]),label=y2)
y_test = data2$Min.Delay

##############################################################################################

set.seed(100)
xgb_model = train(Xtrain,
                  y_train,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, Xtrain)
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1, pred)
rmse_m


#For the test set

y_hat_enet = predict(xgb_model, Xtest)

actual=y2
pred=c(y_hat_enet )
mape_m = mape(y2, pred)
rmse_m = rmse(y2, pred)
mape_m
rmse_m



# get variable importance
var_imp =varImp(xgb_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))





###############################################################################
# Calculate number of variables to be removed

vars_to_remove = df$Variables[23:56]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("Route_NewRegular and limited service routes","DayMonday","MonthJanuary","Incident_NewCleaning - Unsanitary","DirectionN","is_WeekdayWeekday","Hour0" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)

# Convert dummy variables to factors
num_cols = c("Min.Delay", "Min.Gap")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))


# Check the data types
str(data1_imp)
data_new=data1_imp[indexes,]
Xtrain = xgb.DMatrix(data = as.matrix(data1_imp[,-4]),label=y1)
y_train = data1$Min.Delay

set.seed(100)
xgb_model = train(Xtrain ~ .,
                  y_train,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, Xtrain)
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
rmse=sqrt(sum((actual -pred)^2)/nrow(data1))

#For the test set
# Remove least important variables from data2

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)
str(data2_imp)
data2_imp=as.data.frame(data2_imp)
# Convert dummy variables to factors
num_cols = c("Min.Delay", "Min.Gap")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))
Xtest = xgb.DMatrix(data = data2_imp[,-4],label=y2)
y_test = data2$Min.Delay

y_hat_enet = predict(xgb_model, Xtest)
actual=y_test
pred=c(y_hat_enet)
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2,pred)
mape_m




####################################################################################################
nrow(trainset_new)
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset_new$Min.Delay,main="Delay time distribution",col="#9494b8",xlab="Dealy time(In minutes)")
boxplot.stats(trainset_new$Min.Delay)$stats
x=which(trainset_new$Min.Delay %in% boxplot.stats(trainset_new$Min.Delay)$out)
length(x)

options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(log(trainset_new$Min.Delay),main="Log Delay time distribution",col="#9494b8",xlab="ln(delay time)")
boxplot.stats(log(trainset_new$Min.Delay))$stats
x=which(log(trainset_new$Min.Delay) %in% boxplot.stats(log(trainset_new$Min.Delay))$out)
length(x)
# Create a ggplot object
ggplot(trainset_new, aes(x = log(Min.Delay))) +
  
  # Add histogram layer
  geom_histogram(binwidth = 0.1, aes(y = ..density..), fill = "#9494b8", color = "black") +
  
  # Add density curve layer
  geom_density(color = "red", linetype = "solid", size = 1) +
  
  # Add x-axis label
  xlab("Log delay time ") +
  
  # Add y-axis label
  ylab("Density") +
  
  # Set theme
  theme_minimal()


ggplot(data=trainset_new,aes(x=Min.Gap,y=log(Min.Delay)))+
  geom_point()+
  
  theme_minimal()+
  
  labs(x="Min_Gap",y=" log_Delay")



#Skewness and kurtosis
skewness(trainset_new$Min.Delay)#>1 positively skewed
kurtosis(trainset_new$Min.Delay)
skewness(log(trainset_new$Min.Delay))#>1 positively skewed
kurtosis(log(trainset_new$Min.Delay))

log_delay=c(log(trainset_new$Min.Delay))
trainset_new=cbind(log_delay,trainset_new)
log_delay=c(log(testset$Min.Delay))
testset=cbind(log_delay,testset)
#############################################################################################
#Creating the model matrix 
View(trainset_new)
data1=trainset_new[,-3]
data1[,3]=scale(data1[,3], center = TRUE, scale = TRUE)
data1[,3]=as.numeric(data1[,3])
View(data1)
# Create model matrix with categorical variables as dummies and numerical variable as is
#model_matrix = model.matrix(formula, data = data1)
model_matrix = model.matrix(log_delay~., data = data1)[,-1]
#model_matrix2 = model.matrix(log_delay~., data = data1)[,-1]

#model_matrix = model.matrix(Min.Delay~., data = df)

# Separate the response variable from the predictors
y1 = data1$log_delay
#y1 = df$Min.Delay
is.matrix(model_matrix)
dim(model_matrix)
# Create a sequence of lambda values to test
lambda_seq = 10^seq(10, -2, length = 100)
library(glmnet)
# Use cv.glmnet to perform cross-validation and select the optimal lambda value
#cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq)
set.seed(100)
cv_fit = cv.glmnet(x = model_matrix, y= y1 , alpha = 0, lambda = lambda_seq,nfolds=10,standardize=FALSE)

# View the optimal lambda value

best_lambda2=cv_fit$lambda.1se
# Plot the cross-validation results
dev.off()
plot(cv_fit)

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 0, lambda = best_lambda)

model_cv2 = glmnet(x = model_matrix, y=y1, alpha = 0, lambda = best_lambda2,standardize = FALSE)


y_hat_ridge2 = predict(model_cv2, model_matrix)


actual=y1

pred2=c(y_hat_ridge2)

d_actual=trainset_new$Min.Delay
d_pred2=as.integer(exp(pred2))
rmse_m=rmse(d_actual,d_pred2)
rmse_m

coef(model_cv2 ,s=best_lambda2)


#For the test set
View(testset)
data2=testset[,-3]
data2[,3]=scale(data2[,3], center = TRUE, scale = TRUE)
data2[,3]=as.numeric(data2[,3])
View(data2)

#model_matrix1 = model.matrix(log_delay~., data = data2)
model_matrix1 = model.matrix(log_delay~., data = data2)[,-1]

# Separate the response variable from the predictors
#y1 = data1$Min.Delay
y2 = data2$log_delay

model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 0, lambda = best_lambda2,standardize = FALSE)


y_hat_ridge2 = predict(model_cv2, model_matrix1)

actual=y2
pred1=c(y_hat_ridge1)
pred2=c(y_hat_ridge2)
d_actual=testset$Min.Delay

d_pred2=as.integer(exp(pred2))
rmse_m=rmse(d_actual,d_pred2)
rmse_m
mape_m=mape(d_actual,d_pred2)
mape_m


#Lasso Regression
# Perform 10-fold cross-validation to select lambda ---------------------------
# Setting alpha = 1 implements lasso regression

#lasso_cv = cv.glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq, nfolds = 10)
set.seed(100)
lasso_cv = cv.glmnet(x = model_matrix, y=y1, alpha = 1, lambda = lambda_seq, nfolds = 10,standardize=FALSE)
# Plot cross-validation results
dev.off()
plot(lasso_cv)

# Best cross-validated lambda

lambda_cv2 = lasso_cv$lambda.1se

# Fit final model, get its sum of squared residuals and multiple R-squared
#model_cv = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv)

model_cv2 = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_cv2,standardize = FALSE)


y_hat_lasso2 = predict(model_cv2, model_matrix)
actual=y1

pred2=c(y_hat_lasso2)

d_actual=trainset_new$Min.Delay

d_pred2=as.integer(exp(pred2))
rmse_m=rmse(d_actual,d_pred2)
rmse_m

#rsq_lasso_cv = cor(y, y_hat_cv)^2

coef(model_cv2 ,s=lambda_cv2)
# See how increasing lambda shrinks the coefficients --------------------------
res = glmnet(x = model_matrix, y= y1, alpha = 1, lambda = lambda_seq,standardize = FALSE)
plot(res, xvar = "lambda",label=TRUE,lw=2)



#For the test set


model_cv2 = glmnet(x = model_matrix1, y=y2, alpha = 1, lambda = lambda_cv2,standardize = FALSE)


y_hat_lasso2 = predict(model_cv2, model_matrix1)

actual=y2

pred2=c(y_hat_lasso2)

d_actual=testset$Min.Delay

d_pred2=as.integer(exp(pred2))
rmse_m=rmse(d_actual,d_pred2)
rmse_m
mape_m=mape(d_actual,d_pred2)
mape_m

#Elastic Net Regression
# Set training control
set.seed(100)

train_control = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 5,
                             search = "random",
                             verboseIter = TRUE)
View(data1)
#str(data1)
# Train the model
#elastic_net_model = train(log_delay ~ .,data = data1,method = "glmnet",preProcess = c("center", "scale"),tuneLength = 25,trControl = train_control)
elastic_net_model = train(log_delay ~ .,
                          data = data1,
                          method = "glmnet",
                          trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)

d_actual=trainset_new$Min.Delay
d_pred=as.integer(exp(pred))
rmse_m=rmse(d_actual,d_pred)
rmse_m


#For the test set
#elastic_net_model1 = train(log_delay ~ .,data = data2,method = "glmnet",trControl = train_control)

# Check multiple R-squared
y_hat_enet = predict(elastic_net_model, data2[,-1])

actual=y2
pred=c(y_hat_enet )

d_actual=testset$Min.Delay
d_pred=as.integer(exp(pred))
rmse_m=rmse(d_actual,d_pred)
rmse_m
mape_m=mape(d_actual,d_pred)
mape_m


#Random Forest


set.seed(100)
indexes=sample(1:nrow(data1),0.1*nrow(data1))
data_new=data1[indexes,]

# Define the tuning grid
tune_grid = expand.grid(
  mtry =seq(20,50,by=5),
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  log_delay ~ .,
  data = data_new,
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1[,-1])
actual=y1
pred=c(y_hat_enet)
d_actual=trainset_new$Min.Delay
d_pred=as.integer(exp(pred))
rmse_m = rmse(d_actual, d_pred)
rmse_m

#For the test set

y_hat_enet = predict(rf_model, data2[,-1])
actual=y2
pred=c(y_hat_enet )
d_actual=testset$Min.Delay
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m




# get variable importance
# Print the variable importance measures
var_imp=rf_model$finalModel$variable.importance
# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


var_imp = data.frame(Variables = names(rf_model$finalModel$variable.importance), 
                     Importance = rf_model$finalModel$variable.importance, 
                     row.names = NULL)

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance)) +
  geom_bar(stat = "identity",fill="steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))+
  
  coord_flip()


var_imp = data.frame(Variables = names(rf_model$finalModel$variable.importance), 
                     Importance = rf_model$finalModel$variable.importance, 
                     row.names = NULL)

ggplot(data = var_imp, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradientn(colors = c("lightblue", "darkblue"), 
                       guide = "none", aesthetics = "fill") +
  coord_flip() +
  scale_y_log10()


##########################################################################################################

df=var_imp[order(-var_imp$Importance),]
View(df)


nrow(df)

# Calculate number of variables to be removed

vars_to_remove = df$Variables[36:56]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)

base_var=c("Route_NewRegular and limited service routes","DayMonday","MonthJanuary","Incident_NewCleaning - Unsanitary","DirectionN","is_WeekdayWeekday","Hour0" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)
# Convert dummy variables to factors
num_cols = c("Min.Delay", "Min.Gap")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))

# Check the data types
str(data1_imp)
set.seed(100)
indexes=sample(1:nrow(data1_imp),0.1*nrow(data1_imp))
data_new=data1_imp[indexes,]
# Define the tuning grid
tune_grid = expand.grid(
  mtry =31,
  splitrule = "variance",
  min.node.size = c(1, 5, 10,15)
  
)
# Tune the hyperparameters using the tune() function
library(ranger)


# Perform 5-fold cross-validation 
set.seed(100)
rf_model = train(
  Min.Delay ~ .,
  data = data_new,
  
  method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  tuneGrid=tune_grid,
  importance="impurity"
)
# Get best hyperparameters
best_params = rf_model$bestTune
plot(rf_model)
#For the training set
y_hat_enet = predict(rf_model, data1_imp[,-7])
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1, pred)
rmse_m




#For the test set
# Calculate number of variables to be removed
# Remove least important variables from data1

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)

data2_imp=as.data.frame(data2_imp)

# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))

y_hat_enet = predict(rf_model, data2_imp[,-7])

actual=y2
pred=c(y_hat_enet )
rmse_m = rmse(y2,pred)
rmse_m
mape_m = mape(y2, pred)
mape_m


#######################################################################################################
#Xgboost
library(xgboost)
Xtrain = xgb.DMatrix(data = as.matrix(data1[,-2]),label=y1)
y_train = data1$log_delay
Xtest = xgb.DMatrix(data = as.matrix(data2[,-2]),label=y2)
y_test = data2$log_delay
#Xgboost
set.seed(100)
xgb_model = train(Xtrain,
                  y_train,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, Xtrain)
actual=y1
pred=c(y_hat_enet)
d_actual=trainset_new$Min.Delay
d_pred=as.integer(exp(pred))

rmse_m = rmse(d_actual, d_pred)

rmse_m

#For the test set

y_hat_enet = predict(xgb_model, Xtest)

actual=y2
pred=c(y_hat_enet )
d_actual=testset$Min.Delay
d_pred=as.integer(exp(pred))
mape_m = mape(d_actual, d_pred)
rmse_m = rmse(d_actual, d_pred)
mape_m
rmse_m




# get variable importance
var_imp =varImp(xgb_model)

# print the variable importance
print(var_imp)

# load ggplot2 library
library(ggplot2)


# plot the variable importance
ggplot(data = var_imp, aes(x = reorder(rownames(var_imp), Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Variable Importance Plot", x = "Predictor Variables", y = "Importance") +
  theme(plot.title = element_text(hjust = 0.5))



###############################################################################
# Calculate number of variables to be removed

vars_to_remove = df$Variables[23:56]

# Remove least important variables from data1

dummy_data1= dummyVars(" ~ . ", data = data1,sep = "")
data1_encoded=predict(dummy_data1, newdata =data1)

View(data1_encoded)
data1_imp = data1_encoded[, !(colnames(data1_encoded) %in% vars_to_remove)]
View(data1_imp)
colnames(data1_imp)
base_var=c("Route_NewRegular and limited service routes","DayMonday","MonthJanuary","Incident_NewCleaning - Unsanitary","DirectionN","is_WeekdayWeekday","Hour0" )
data1_imp = data1_imp[, !(colnames(data1_imp) %in% base_var)]
View(data1_imp)
str(data1_imp)
data1_imp=as.data.frame(data1_imp)

# Convert dummy variables to factors
num_cols = c("log_delay", "Min.Gap")
# Convert non-numeric columns to factors
factor_cols = setdiff(names(data1_imp), num_cols)
data1_imp[factor_cols] = apply(data1_imp[factor_cols], 2, function(x) as.factor(x))


# Check the data types
str(data1_imp)
data_new=data1_imp[indexes,]
Xtrain = xgb.DMatrix(data = as.matrix(data1_imp[,-4]),label=y1)
y_train = data1$log_delay

set.seed(100)
xgb_model = train(Xtrain ~ .,
                  y_train,
                  method = "xgbTree",
                  objective = "reg:squarederror",
                  trControl = trainControl(method = "cv",
                                           number = 5,
                                           
                                           verboseIter = TRUE),
                  
                  
                  tuneGrid = expand.grid(nrounds = c(500,1000),
                                         eta=0.1,
                                         max_depth = c(2,4,6),
                                         colsample_bytree = c(0.5,0.6),
                                         subsample = c(0.5,0.6),
                                         gamma=0.1,
                                         min_child_weight = 1
                  ))

plot(xgb_model)
y_hat_enet = predict(xgb_model, Xtrain)
actual=y1
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m


#For the test set
# Remove least important variables from data2

dummy_data2= dummyVars(" ~ . ", data = data2,sep = "")
data2_encoded=predict(dummy_data2, newdata =data2)

View(data2_encoded)
data2_imp = data2_encoded[, !(colnames(data2_encoded) %in% vars_to_remove)]
View(data2_imp)
colnames(data2_imp)
data2_imp = data2_imp[, !(colnames(data2_imp) %in% base_var)]
View(data2_imp)
str(data2_imp)
data2_imp=as.data.frame(data2_imp)

# Convert non-numeric columns to factors
factor_cols = setdiff(names(data2_imp), num_cols)
data2_imp[factor_cols] = apply(data2_imp[factor_cols], 2, function(x) as.factor(x))
Xtest = xgb.DMatrix(data = data2_imp[,-4],label=y2)
y_test = data2$log_delay

y_hat_enet = predict(xgb_model, Xtest)
actual=y2
pred=c(y_hat_enet)
rmse_m = rmse(y1,pred)
rmse_m
mape_m = mape(y1,pred)
mape_m






