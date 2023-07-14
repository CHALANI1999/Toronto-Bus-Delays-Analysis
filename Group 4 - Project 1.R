data=read.csv("D:/3RD YEAR/SEMESTER II/ST3082/Project I Materials//ttc-bus-delay-data-2022.csv")
View(data)
head(data)
summary(data)
str(data)
library(dplyr)
library(stringr)
library(mgsub)
library(ggplot2)
library(corrplot)
library(psych)
library(packHV)
library(plyr)
library(moments)
library(tidyverse)



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
data=data %>%  filter(!row_number() %in% c(78,19277,21705,6757,11606,154,4310,10421,14092))
nrow(data)

#Create Route_New column
x=c(table(data$Route))
which(data$Route=="RAD")
which(data$Route=="OTC")
data$Route[5703]=1000
data$Route[7435]=1000
data$Route[19824]=1000
data$Route[13156]=1001
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
data=cbind(data,Route_New)



#Create Hour column
Hour=c()
for(i in 1:length(data$Time)){
    Hour[i]=as.numeric(substr(data$Time[i],1,regexpr(":",data$Time[i])-1))
}
data=cbind(data,Hour)

#Create Incident_New column
table(data$Incident)
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

#Create is_Sunday column
is_Sunday=c()
for(i in 1:nrow(data)){
  if(data$Day[i]=="Sunday" ) {
    is_Sunday[i]="Sunday"
  }else{
    is_Sunday[i]="Not Sunday"
  }
}
data=cbind(data,is_Sunday)







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
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Community Routes")&
                                          (data$is_Weekday[i]=="Weekday")& !(((data$Hour[i]>=6)&
                                                                              (data$Hour[i]<10))|((data$Hour[i]>=15)&(data$Hour[i]<19))))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Community Routes")&
                                          (data$is_Weekday[i]=="Weekend"))){
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
################################################################
#Cleaning Data II
y=c()

for(i in 1:nrow(data)){
  if((!is.na(data$Route_New[i]))&&( (data$Route_New[i]=="Regular and limited service routes")&
                                    (data$is_Sunday[i]=="Not Sunday")&((data$Hour[i]>=1)&(data$Hour[i]<6)))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&&( (data$Route_New[i]=="Regular and limited service routes")&
                                          (data$is_Sunday[i]=="Sunday")&((data$Hour[i]>=1)&(data$Hour[i]<8)))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Blue Night Routes")&
                                          (data$is_Sunday[i]=="Not Sunday")&(data$Hour[i]<1)&(data$Hour[i]>=6))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Blue Night Routes")&
                                          (data$is_Sunday[i]=="Sunday")&(data$Hour[i]<1)&(data$Hour[i]>=8))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Community Routes")&
                                          (data$is_Weekday[i]=="Weekday")& !(((data$Hour[i]>=6)&
                                                                              (data$Hour[i]<10))|((data$Hour[i]>=15)&(data$Hour[i]<19))))){
    y=append(y,i)
    
  }else if((!is.na(data$Route_New[i]))&& ((data$Route_New[i]=="Community Routes")&
                                          (data$is_Weekday[i]=="Weekend"))){
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

########################################################
#Factoring
data$Route_New = factor(data$Route_New,level=c("Regular and limited service routes","Blue Night Routes","Community Routes",
                                               "Express Routes","Others"))
data$Day = factor(data$Day,level=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
data$Month = factor(data$Month,level=c("January","February","March","April","May","June"))
data$Incident_New = factor(data$Incident_New,level=c("Cleaning - Unsanitary","Diversion","General Delay","Mechanical","Security","Vision",
                                                     "Collision - TTC","Emergency Services","Investigation","Operations - Operator",
                                                     "Road Blocked","Utilized Off Route","Others"))
data$Direction=factor(data$Direction,level=c("N","S","E","W","B"))
data$is_Weekday=factor(data$is_Weekday,level=c("Weekday","Weekend"))
#data$is_Sunday=factor(data$is_Sunday,level=c("Sunday","Not Sunday"))
#Removing Vehicle Number column
data=subset(data,select=-Vehicle)

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
trainset$Route_New[16002]="Blue Night Routes"
trainset$Route_New[17996]="Blue Night Routes"
#trainset$Route_New[16121]="Blue Night Routes"
#trainset$Route_New[18135]="Blue Night Routes"
testset$Route_New[is.na(testset$Route_New)] ="Regular and limited service routes"
trainset$Direction[is.na(trainset$Direction)] ="N"
testset$Direction[is.na(testset$Direction)] ="N"
#testset$Route_New[3737]="Blue Night Routes"
colSums(is.na(trainset))
colSums(is.na(testset))
str(trainset)
table(trainset$Direction)


#Descriptive Analysis

# 1) Distribution of Delay

#Histogram and Boxplot
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset$Min.Delay,main="Delay time distribution",col="#9494b8",xlab="Dealy time(In minutes)")
boxplot.stats(trainset$Min.Delay)$stats

options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(log(trainset$Min.Delay),main="Delay time distribution",col="#9494b8",xlab="Dealy time(In minutes)")
boxplot.stats(trainset$Min.Delay)$stats
new_response=log(trainset$Min.Delay)
#Numerical Summaries
#Mean of Delay time
mean(trainset$Min.Delay)

#Skewness and kurtosis
skewness(trainset$Min.Delay)#>1 positively skewed
kurtosis(trainset$Min.Delay)
x=which(trainset$Min.Delay %in% boxplot.stats(trainset$Min.Delay)$out)
length(x)

skewness(new_response)#>1 positively skewed
kurtosis(new_response)
x=which(new_response %in% boxplot.stats(new_response)$out)
length(x)
trainset=cbind(trainset,new_response)
trainset=trainset[-10]
###################################### MULTIPLE CORRESPONDANCE ANALYSIS #################

library(FactoMineR)
trainset2=subset(trainset,select=-c(Date,Time,Route,Location,Incident))
View(trainset2)
nrow(trainset2)
cats = apply(trainset2, 2, function(x) nlevels(as.factor(x)))
# apply MCA
trainset2.active =trainset2[,c(1,4:9)]
View(trainset2.active)
res=MCA(trainset2.active,graph=TRUE)
is.data.frame(trainset2.active)
trainset2=subset(trainset,select=-c(Date,Time,Route,Location,Incident))
View(trainset2)
cats = apply(trainset2, 2, function(x) nlevels(as.factor(x)))
# apply MCA

res=MCA(trainset2,quanti.sup =c(2,3))



res=MCA(trainset4,quanti.sup =c(2,3))

res$eig
res$var
summary(res,ncp=2,nbelements=Inf)
plot.MCA(res, invisible=c("var","quanti.sup"), cex=0.7)
plot.MCA(res, invisible=c("ind","quanti.sup"), cex=0.7)
plot.MCA(res, invisible=c("ind"))
plot.MCA(res, invisible=c("ind", "var"))



######################################PLS##########################################################

library(mdatools)
library(caret)
dummy= dummyVars(" ~ .", data = trainset2)
trainset2_encoded= data.frame(predict(dummy, newdata =trainset2 ))
View(trainset2_encoded)
trainset2_encoded=as.data.frame(trainset2_encoded)
xc = trainset2_encoded[-8]
yc = trainset2_encoded[8]
dimnames(trainset4)
str(trainset4)
ModelPLS = pls(xc, yc, scale = TRUE, cv=5, info = "Delay time prediction")
summary(ModelPLS)
plot(ModelPLS)
plotXScores(ModelPLS,show.label = TRUE)
plotXYLoadings(ModelPLS,show.label = TRUE)
plotVIPScores(ModelPLS,ncomp = 3, type = "h",show.label = TRUE)
summary(ModelPLS$coeffs)
plot(ModelPLS$coeffs,show.label = TRUE)
#################################################################################################









#Stacked bar chart with Direction Vs Number of Incidents
ggplot(trainset,aes(x=Route_New,fill=Direction))+
  geom_bar(stat='count',width=0.7,alpha=0.7)+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Route Type",y="Number of Incidents")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
#Stacked bar chart with Direction Vs Number of Incidents(Imputed)
ggplot(trainset4,aes(x=Route_New,fill=Direction))+
  geom_bar(stat='count',width=0.7,alpha=0.7)+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Route Type",y="Number of Incidents")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
#Stacked bar chart Direction by total delay time
tg1=ddply(trainset,c("Route_New","Direction"),summarise,delay=sum(Min.Delay))
ggplot(tg1,aes(x=Route_New,y=delay,fill=Direction,label=delay))+
  geom_bar(stat="identity",width=0.3,alpha=0.7)+
    scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Route Type",y="Total Delayed time(In minutes)")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )


aggregate(Min.Delay~ Route_New, data = trainset, sum) 


#Line plot of Mean delay time by hour and is_Weekday
tg2=ddply(trainset,c("Hour","is_Weekday"),summarise,delay=mean(Min.Delay))
ggplot(tg2,aes(x=Hour,y=delay,colour=is_Weekday,group=is_Weekday))+
  geom_point()+
  geom_line()+
  labs(x="Hour",y="Mean delay time(In minutes)")


#Dot plot of delay time by hour and is_Weekday
ggplot(data=trainset,aes(x=Hour,y=Min.Delay,fill=is_Weekday,color=is_Weekday))+
  geom_point()+
  theme_minimal()+
  labs(x="Hour",y="Delay time(In minutes)")

#Boxplot of delay times by Route and is_Weekday
ggplot(trainset, aes(x=factor(Route_New), y=Min.Delay, color = is_Weekday))+
  geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Route type",y="Delayed time(In minutes)")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )

#Multiple bar chart of Number of incidents by route and day
ggplot(trainset, aes(Route_New, fill = Day)) +
  geom_bar(stat="count",position = "dodge") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Route type",y="Number of Incidents")


#Boxplot of delay time by incidents
ggplot(trainset,aes(x=Incident_New,y=Min.Delay))+
  geom_boxplot(alpha=0.7,fill="blue")+
  coord_flip()+
  labs(x="Reason type",y="Delayed time(in minutes)")+
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    panel.background=element_blank(),axis.line=element_line(colour="black")
  )
#Multiple bar chart of Number of incidents by Incidents and is_Weekday
ggplot(trainset, aes(Incident_New, fill = is_Weekday)) +
  geom_bar(stat="count", position = "dodge") +
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_brewer(palette = "Set1")+
  labs(x="Reason type",y="Number of Incidents")


#Line plot of mean delay time by incident type and is_Weekday
tg3=ddply(trainset,c("Incident_New","is_Weekday"),summarise,delay=mean(Min.Delay))
ggplot(tg3,aes(x=Incident_New,y=delay,colour=is_Weekday,group=is_Weekday))+
  geom_point()+
  geom_line()+
  scale_x_discrete(guide = guide_axis(angle = 90))+
  labs(x="Reason type",y="Mean delay time(In minutes)")

#Summary plot
ggplot(trainset, aes(x= Hour,fill=Incident_New))+
  geom_bar(stat="count")+
  labs(x="Hour",y="Number of Incidents")+
  facet_wrap(~Incident_New)


#Multiple bar chart of Numbere of incidents by month
RBH=c()
for(i in 1:nrow(trainset)){
  if(trainset$Route_New[i]=="Regular and limited service routes"){
    RBH[i]="Regular and limited service Hours"
  }else{
    RBH[i]="Others"
  }

}
trainset=cbind(trainset,RBH)
trainset$RBH=factor(trainset$RBH,level=c("Regular and limited service Hours","Others"))
ggplot(trainset, aes(Month, fill = RBH)) +
  geom_bar(stat="count", position = "dodge") +
  
  scale_x_discrete(guide = guide_axis(angle = 90))+
  scale_fill_brewer(palette = "Set2")+
  labs(x="Month",y="Number of incidents")


#continuous variables
ggplot(trainset, aes(x=Min.Gap, y=Min.Delay)) +
  geom_point() +
  labs(x="Gap(In minutes)",y="Delay time")
#correlation of gap and the delay time variables
corr.test(trainset$Min.Delay,trainset$Min.Gap)



#Suggestions for Advanced Analysis 
#Examining the relationship of Route_New on Gap variable
#Checking the ANOVA assumptions
library(ggpubr)
library(rstatix)
#Time Gap distribution and Outlier detection
options(repr.plot.width=12,repr.plot.height=7)
hist_boxplot(trainset$Min.Gap,main="Time gap distribution",col="#9494b8",xlab="Time Gap(In minutes)")
x=which(trainset$Min.Gap %in% boxplot.stats(trainset$Min.Gap)$out)
length(x)
#Checking the normality assumption
ggqqplot(trainset, "Min.Gap", facet.by = "Route_New")
ggqqplot(trainset, "Min.Delay", facet.by = "Route_New")
ggqqplot(trainset, "Min.Delay", facet.by = "Direction")
ggqqplot(trainset, "Min.Delay", facet.by = "Hour")
ggqqplot(trainset, "Min.Delay", facet.by = "is_Weekday")
ggqqplot(trainset, "Min.Delay", facet.by = "Day")
ggqqplot(trainset, "Min.Delay", facet.by = "Incident_New")
ggqqplot(trainset, "Min.Delay", facet.by = "Month")

#Since ANOVA normality assumptions are violated we are using Kruskal wallis test
# Performing Kruskal-Wallis test
result = kruskal.test(Min.Gap ~ Route_New,
                      data = trainset)
print(result)
kruskal.test(Min.Delay ~ Route_New,
             data = trainset)
kruskal.test(Min.Delay ~ Direction,
             data = trainset)
kruskal.test(Min.Delay ~ Hour,
             data = trainset)
kruskal.test(Min.Delay ~ Incident_New,
             data = trainset)
kruskal.test(Min.Delay ~ is_Weekday,
             data = trainset)
kruskal.test(Min.Delay ~ Day,
             data = trainset)
kruskal.test(Min.Delay ~ Month,
             data = trainset)

#chisquared test between Route-New and Incident-New
chisq.test(trainset$Route_New, trainset$Incident_New, correct=FALSE)
chisq.test(trainset$Route_New, trainset$Hour, correct=FALSE)
 

###Normalizing
x=which(trainset$Min.Delay %in% boxplot.stats(trainset$Min.Delay)$out)

data1=trainset %>%  filter(!row_number() %in% c(x))
nrow(data1)
data1 = data1[,7]
length(data1)

library(AID)
out = boxcoxnc(data1, method = "mle", lambda = seq(-2,2,0.0001), verbose = F, plot = F)
out$lambda.hat
## [1] -0.0474

library(MASS)
out <- boxcox(data1, lambda = seq(-2,2,0.0001), plotit = F)
out$x[which.max(out$y)]
## [1] -0.0474

library(car)
out <- powerTransform(data1, family = "bcPower")
out$lambda

out <- boxcoxnc(data1, method = "sw")
out$lambda.hat


out <- boxcoxnc(data1, method = "ad")
out$lambda.hat


out <- boxcoxnc(data1, method = "cvm")
out$lambda.hat


out <- boxcoxnc(data1, method = "sf")
out$lambda.hat


out <- boxcoxnc(data1, method = "lt")
out$lambda.hat


out <- boxcoxnc(data1, method = "jb")#normal

out$lambda.hat


out <- boxcoxnc(data1, method = "pt")
out$lambda.hat

out$tf.data
skewness(out$tf.data)
kurtosis(out$tf.data)
confInt(out, level = 0.95)  

  
  