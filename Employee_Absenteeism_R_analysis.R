########################################
# Setup
########################################

#Clear Environment
rm(list=ls(all=T))

#Setting working directory
setwd("/home/gid/Documents/project_2")

#Load Libraries
x = c("ggplot2", "corrgram", "plyr", "rockchalk","factoextra","fastDummies", "caret", "C50", "nnet","randomForest", "xgboost","dummies", "e1071", "rpart", "gbm", 'DataCombine',"xlsx",'DMwR', 'pls','dplyr')

# Loading required libraries
lapply(x, require, character.only = TRUE)
#Remove list
rm(x)

########################################
# Function
########################################
#This function will segregate the different data types present in the dataframe.
dtype_separator <- function(df, col_names){
  fact_col = list()
  num_col =list()
  logic_col = list()
  int_col = list()
  char_col = list()
  unknown_col = list()
  for(i in 1:ncol(df)){
    if(class(df[,i])=='factor'){
      fact_col = c(fact_col,col_names[i])
    } else if(class(df[,i])=='numeric'){
      num_col = c(num_col,col_names[i])
    } else if(class(df[,i])=='integer'){
      int_col = c(int_col,col_names[i])
    } else if(class(df[,i])=='character'){
      char_col = c(char_col,col_names[i])
    } else if(class(df[,i])=='logical'){
      logic_col = c(logic_col,col_names[i])
    } else{
      unknown_col = c(unknown_col,col_names[i])
    }
  }
  list(fact_col,num_col,int_col,char_col,logic_col,unknown_col)
}

outlier_df<- function(df,num_dtype){
  k = 1
  len_val = c()
  ## Total number of outliers
  for(i in num_dtype){
    print(i)
    print(k)
    val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
    print(length(val))
    len_val[[k]] = length(val)
    k = k+1
  }
  outlier_analysis = data.frame(feature = num_dtype, total_outliers = len_val)
}

###################################################################################################
#Loading data
###################################################################################################

df = read.xlsx("Absenteeism_at_work_Project.xls", sheetName = "Absenteeism_at_work")


###########################################Explore the data##########################################
#Viewing dataframe
head(df)
#Display the Structure of dataframe
str(df)

df$Month.of.absence[df$Month.of.absence %in% 0] = NA
df$Work.load.Average.day. = df$Work.load.Average.day./1000

# Calling function to seperate by datatype
dtype = dtype_separator(df,colnames(df))
num_dtype = dtype[[2]]
num_dtype = unlist(num_dtype, recursive=FALSE)
int_dtype = dtype[[3]]
int_dtype = unlist(int_dtype, recursive=FALSE)

# Converted to corrected data type
####################################
# It was noticed that some features where stored as the wrong data type and hence are converted to the right data type.

convert_obj = c('ID', "Reason.for.absence", "Month.of.absence", "Day.of.the.week", "Seasons", "Disciplinary.failure", "Education",
                "Social.drinker", "Social.smoker",'Son','Pet')
for(i in 1:length(num_dtype)){
  if(num_dtype[i] %in% convert_obj){
    df[,num_dtype[i]] = factor(df[,num_dtype[i]])
  }
}

# Calling function to seperate by datatype
dtype = dtype_separator(df,colnames(df))
fact_dtype = dtype[[1]]
fact_dtype = unlist(fact_dtype, recursive=FALSE)
num_dtype = dtype[[2]]
num_dtype = unlist(num_dtype, recursive=FALSE)
int_dtype = dtype[[3]]
int_dtype = unlist(int_dtype, recursive=FALSE)

#Structure of dataframe
str(df)

########################################################################################################################
# Missing Values Analysis
########################################################################################################################
missing_val = sum(is.na(df))
print(missing_val)

##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(df)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)

ggplot(data = missing_val, aes(x=reorder(Columns, -Missing_percentage),y = Missing_percentage))+
  geom_bar(stat = "identity",fill = "white")+xlab("Parameter")+
  ggtitle("Missing data percentage (Train)") + 
  theme(panel.background = element_rect("cadetblue"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
x = df[-21]

# kNN Imputation
df = (knnImputation(df, k = 3,meth='median'))
sum(is.na(df))

#################################################################################################
#Outlier analysis
##################################################################################################
#Plot outliers present in continous data set
for (i in 1:length(num_dtype))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_dtype[i])), data = subset(df))+ geom_boxplot()+
           labs(y=num_dtype[i])+
           theme(panel.background = element_rect(fill = "palegreen"))+
           ggtitle(paste("Outlier analysis for ",num_dtype[i])))
}
## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)
## Plotting plots together
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,gn9,ncol=5)
gridExtra::grid.arrange(gn10,ncol=1)
outlier_analysis = outlier_df(df,num_dtype)

ggplot(data = outlier_analysis, aes(x = reorder(feature,-total_outliers), y = total_outliers))+
  geom_bar(stat = "identity",fill = "white")+
  labs(title = "Outliers in each feature", x = "Features")+ 
  theme(panel.background = element_rect("cadetblue"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

df_siz = dim(df)
outliers_percentage = (sum(outlier_analysis$total_outliers)/df_siz[1])*100
print(outliers_percentage)

#Replace all outliers with NA and impute
for(i in num_dtype){
  val = df[,i][df[,i] %in% boxplot.stats(df[,i])$out]
  #print(length(val))
  df[,i][df[,i] %in% val] = NA
}

df = knnImputation(df, k = 3,meth='median')

for (i in 1:length(num_dtype))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (num_dtype[i])), data = subset(df))+ geom_boxplot()+
           labs(y=num_dtype[i])+
           theme(panel.background = element_rect(fill = "palegreen"))+
           ggtitle(paste("Outlier analysis for ",num_dtype[i])))
}
## Plotting plots together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=4)
## Plotting plots together
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,gn9,ncol=5)
gridExtra::grid.arrange(gn10,ncol=1)
outlier_analysis = outlier_df(df,num_dtype)
################################################################################################
outlier_analysis = outlier_df(df,num_dtype)
df_siz = dim(df)
outliers_percentage = (sum(outlier_analysis$total_outliers)/df_siz[1])*100
print(outliers_percentage)

##################################Feature Engineering##############################################
###################################################################################################

table(df$Reason.for.absence)
df$Reason.for.absence = combineLevels(df$Reason.for.absence, c(1:21), newLabel = 1)
df$Reason.for.absence = combineLevels(df$Reason.for.absence, c(1, 2, 7), newLabel = 2)
df$Reason.for.absence = combineLevels(df$Reason.for.absence, c(1, 2), newLabel = 3)
table(df$Reason.for.absence)

df$Reason.for.absence = revalue(df$Reason.for.absence, c('26'='4', '27'='5'))

df_class = df
################################################################################################
################################################################################################
#EDA
################################################################################################

#Distribution of factor data using bar plot
bar_plt1 = ggplot(data = df, aes(x = ID)) +
  geom_bar(fill = "white") + ggtitle("Count of ID") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt2 = ggplot(data = df, aes(x = Reason.for.absence)) +
  geom_bar(fill = "white") + ggtitle("Count of Reason for absence") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt3 = ggplot(data = df, aes(x = Month.of.absence)) + 
  geom_bar(fill = "white") + ggtitle("Count of Month") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt4 = ggplot(data = df, aes(x = Disciplinary.failure)) + 
  geom_bar(fill = "white") + ggtitle("Count of Disciplinary failure") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt5 = ggplot(data = df, aes(x = Education)) + 
  geom_bar(fill = "white") + ggtitle("Count of Education") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt6 = ggplot(data = df, aes(x = Son)) + 
  geom_bar(fill = "white") + ggtitle("Count of Son") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt7 = ggplot(data = df, aes(x = Social.smoker)) + 
  geom_bar(fill = "white") +  ggtitle("Count of Social smoker") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt8 = ggplot(data = df, aes(x = Social.drinker)) + 
  geom_bar(fill = "white") + ggtitle("Count of Social.drinker") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt9 = ggplot(data = df, aes(x = Day.of.the.week)) + 
  geom_bar(fill = "white") + ggtitle("Count of Day.of.the.week") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt10 = ggplot(data = df, aes(x = Seasons)) + 
  geom_bar(fill = "white") + ggtitle("Count of Seasons") + 
  theme(panel.background = element_rect("cadetblue"))
bar_plt11 = ggplot(data = df, aes(x = Pet)) + 
  geom_bar(fill = "white") +  ggtitle("Count of Pet") + 
  theme(panel.background = element_rect("cadetblue"))
gridExtra::grid.arrange(bar_plt1,bar_plt2,bar_plt3,bar_plt4,ncol=2)
gridExtra::grid.arrange(bar_plt5,bar_plt6,bar_plt7,bar_plt8,ncol=2)
gridExtra::grid.arrange(bar_plt9,bar_plt10,bar_plt11,ncol=2)

#Check the distribution of numerical data using histogram
hist_plt1 = ggplot(data = df, aes(x =Transportation.expense)) + 
  ggtitle("Transportation.expense") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt2 = ggplot(data = df, aes(x =Distance.from.Residence.to.Work)) + 
  ggtitle("Distance.from.Residence.to.Work") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt3 = ggplot(data = df, aes(x =Service.time)) + 
  ggtitle("Service.time") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt4 = ggplot(data = df, aes(x =Age)) + 
  ggtitle("Age") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt5 = ggplot(data = df, aes(x =Work.load.Average.day.)) + 
  ggtitle("Work.load.Average.day.") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt6 = ggplot(data = df, aes(x =Hit.target)) + 
  ggtitle("Hit.target") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt7 = ggplot(data = df, aes(x =Weight)) + 
  ggtitle("Weight") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt8 = ggplot(data = df, aes(x =Height)) + 
  ggtitle("Distribution of Height") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))
hist_plt9 = ggplot(data = df, aes(x =Body.mass.index)) + 
  ggtitle("Distribution of Body.mass.index") + geom_histogram(bins = 25)+
  theme(panel.background = element_rect("cadetblue"))

gridExtra::grid.arrange(hist_plt1,hist_plt2,hist_plt3,hist_plt4,ncol=2)
gridExtra::grid.arrange(hist_plt5,hist_plt6,hist_plt7,hist_plt8,ncol=2)

##########################################################################

ggplot(df, aes(x = Absenteeism.time.in.hours))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours") +
  labs(x='Absenteeism time in hours', y="Count") 

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Reason.for.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours") +
  labs(x='Absenteeism time in hours', y="Count") 

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Reason.for.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours vs Reason for absence") +
  labs(x='Absenteeism time in hours', y="Count")+
  facet_wrap(~Reason.for.absence)

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Reason.for.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours aginst ID and Reason for absence") +
  labs(x='Absenteeism time in hours', y="Count")+
  facet_wrap(~ID)

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Month.of.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours") +
  labs(x='Absenteeism time in hours', y="Count") 

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Month.of.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours Vs Month of absence") +
  labs(x='Absenteeism time in hours', y="Count")+
  facet_wrap(~Month.of.absence)

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Reason.for.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours Vs Month of absence with respect to Reason for absence") +
  labs(x='Absenteeism time in hours', y="Count")+
  facet_wrap(~Month.of.absence)

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Month.of.absence))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours Vs Month of absence with respect to ID") +
  labs(x='Absenteeism time in hours', y="Count")+
  facet_wrap(~ID)

ggplot(df, aes(x = Absenteeism.time.in.hours, fill = Education))+
  geom_histogram(binwidth = 1)+ 
  labs(title="Histogram for Absenteeism time in hours Vs Education") +
  labs(x='Absenteeism time in hours', y="Count")+
  facet_wrap(~ID)

##################################Feature Selection################################################
###################################################################################################
## Correlation Plot 
#######################################
corrgram(df[,num_dtype], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
#Correlation plot and pair plot
library(GGally)
ggpairs(df[,num_dtype])


#######################################
## Anova Test
#######################################
anova = aov(Absenteeism.time.in.hours~ ID, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Reason.for.absence, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Month.of.absence, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Day.of.the.week, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Seasons, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Disciplinary.failure, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Education, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Son, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Social.drinker, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Social.smoker, data = df)
summary(anova)
anova = aov(Absenteeism.time.in.hours~ Pet, data = df)
summary(anova)

#Removal of less significiant features
df = subset(df, select = -c(Body.mass.index, Day.of.the.week, Seasons, Education,Social.drinker,Social.smoker))

# Calling function to seperate by datatype
dtype = dtype_separator(df,colnames(df))
fact_dtype = dtype[[1]]
fact_dtype = unlist(fact_dtype, recursive=FALSE)
num_dtype = dtype[[2]]
num_dtype = unlist(num_dtype, recursive=FALSE)
int_dtype = dtype[[3]]
int_dtype = unlist(int_dtype, recursive=FALSE)

#######################################
## Skewness Test
#######################################

for(i in num_dtype){
  skew = skewness(df[,i])
  print(i)
  print(skew)
}

#log transform
df$Absenteeism.time.in.hours = log1p(df$Absenteeism.time.in.hours)

#Standardisation
for(i in num_dtype){
  if(i !='Absenteeism.time.in.hours'){
    print(i)
    df[,i] = (df[,i] - mean(df[,i]))/sd(df[,i])
  }
}

for(i in num_dtype){
  skew = skewness(df[,i])
  print(i)
  print(skew)
}

##########################################################################################################
#Creation of dummy variables
##########################################################################################################
#If package not installed
#install.packages("fastDummies")

# Function Creates the dummy variables and drops the first dummy variable
df <- fastDummies::dummy_cols(df, remove_first_dummy = TRUE)

## Dropping the original catergorical variable
#df = subset(df, select = -c(Pet,Son, Social.drinker,Social.smoker, Month.of.absence, Reason.for.absence, ID))
df = subset(df, select = -c(Pet,Son, Month.of.absence, Reason.for.absence, ID,Disciplinary.failure))

###########################################################################################################
# Model Development
###########################################################################################################
#Clean the environment
rmExcept(keepers = c("df","df_class"))

#df = df[,c(1:10,12:87,11)]
df = df[,c(1:8,10:69,9)]

############################################################################################################
# Backward Elemination
############################################################################################################
model <- lm(Absenteeism.time.in.hours~., data = df)
summary(model)

#par(mfrow=c(2,2))
plot(model)

x = subset(df, select = -c(Son_0,Son_3,Son_4,Pet_0,Pet_4,Pet_2,Pet_5,Pet_8,ID_35))
x1 = x

model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)

x = subset(x, select = -c(ID_15))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x
x = subset(x, select = -c(Month.of.absence_10))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)

x = subset(x, select = -c(Month.of.absence_6))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Service.time))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Weight))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Month.of.absence_9))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x 

x = subset(x, select = -c(ID_18))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(ID_20))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Month.of.absence_2))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Hit.target))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Month.of.absence_4))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(ID_8))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Work.load.Average.day.))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Month.of.absence_11))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)
x1 = x

x = subset(x, select = -c(Month.of.absence_5))
model <- lm(Absenteeism.time.in.hours~., data = x)
summary(model)

#since there was a drop in Adj R2 from 0.4751 to 0.4748. We stop the backward elemination process and retain the features present in x1

df = x1
#######################################################################################################################################
###########################################################################################################
#Divide data into train and test data set
###########################################################################################################
seed = 1234
set.seed(seed)
train.index = createDataPartition(df$Absenteeism.time.in.hours, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]
###########################################################################################################
#Linear regression model
###########################################################################################################

model <- lm(Absenteeism.time.in.hours~., data = train)
summary(model)

plot(model)

# Predicting the Test data output
y_pred = predict(model, newdata = test[-46])

library(miscTools)
#Compute R^2
mlr_be_r2 <- rSquared(test[,46], test[,46] - y_pred)
print(mlr_be_r2)
#Compute MSE
mlr_be_mse <- mean((test[,46] - y_pred)^2)
print(mlr_be_mse)

#R2 = 0.46
#MSE = 0.24

###########################################################################################################
# Decision Tree
###########################################################################################################
set.seed(seed)
#Building model
fit = rpart(Absenteeism.time.in.hours~., data = train, method = "anova")
#Variable importance
fit$variable.importance

# Predicting the Test data output
predictions_DT = predict(fit, test[-46])

#Compute R^2
dt_be_r2 <- rSquared(test[,46], test[,46] - predictions_DT)
print(dt_be_r2)
#Compute MSE
dt_be_mse <- mean((test[,46] - predictions_DT)^2)
print(dt_be_mse)

#R2 = 0.51
#mse = 0.22
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
set.seed(seed)
maxdepth = c(1:100)
tunegrid = expand.grid(.maxdepth=maxdepth)
dt_random = caret::train(Absenteeism.time.in.hours ~.,data = train, method="rpart2", metric="RMSE", tuneLength =10, trControl=control)

#print out summary of the model
print(dt_random)

#Best fit parameters
view_dt_rand_para = dt_random$bestTune
print(view_dt_rand_para)
#Build model based on best fit 
rand_dt_model = rpart(Absenteeism.time.in.hours ~.,data = train, method = "anova", maxdepth = 9)
#print out summary of the model
print(rand_dt_model)

#Presdict test data using random forest model
rand_dt_Predictions = predict(rand_dt_model, test[-46])

#Compute R^2
rand_dt_be_r2 = rSquared(test[,46], test[,46] - rand_dt_Predictions)
print(rand_dt_be_r2)
#Compute MSE
rand_dt_be_mse = mean((test[,46] - rand_dt_Predictions)^2)
print(rand_dt_be_mse)
#r2 0.513
#mse 0.225
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################

# Create model based on paramters grid specificed with 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid = expand.grid(.maxdepth=c(6:18))
dt_grid = caret::train(Absenteeism.time.in.hours ~.,data = train, method="rpart2", metric="Rsquared", tuneGrid=tunegrid, trControl=control)

#print out summary of the model
print(dt_grid)
#Plot RMSE Vs mtry values
#From this plot we can see that when tree depth is 10 RMSE is at its lowest
plot(dt_grid)

#Best fit parameters
view_dt_grid_pram = dt_grid$bestTune
print(view_dt_grid_pram)

#Build model based on best fir value
grid_dt_model = rpart(Absenteeism.time.in.hours ~.,data = train, method = "anova",maxdepth = 8)
#print out summary of the model
print(grid_dt_model)

#Presdict test data using model
grid_dt_Predictions = predict(grid_dt_model, test[-46])

#Compute R^2
grid_dt_be_r2 = rSquared(test[,46], test[,46] - grid_dt_Predictions)
print(grid_dt_be_r2)
#Compute MSE
grid_dt_be_mse = mean((test[,46] - grid_dt_Predictions)^2)
print(grid_dt_be_mse)
# r2 = 0.5137
# mse 0.225
#############################################################################################################
#Random Forest
#############################################################################################################

#Building model
RF_model = randomForest(Absenteeism.time.in.hours ~ ., train, method = "anova",importance = TRUE)
#Prints out model information
print(RF_model)

#Predicting the Test data output
RF_Predictions = predict(RF_model, test[-46])

#Compute R^2
rf_be_r2 = rSquared(test[,46], test[,46] - RF_Predictions)
print(rf_be_r2)
#Compute MSE
rf_be_mse = mean((test[,46] - RF_Predictions)^2)
print(rf_be_mse)

#r2 = 0.50
#mse = 0.2
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
set.seed(seed)
rf_random = caret::train(Absenteeism.time.in.hours ~.,data = train, method="rf", metric="Rsquared", tuneLength =10, trControl=control)

#print out summary of the model
#here we see that when mtry is 5 RMSE is at its lowest
print(rf_random)

#Best fit parameters
view_rf_rand_para = rf_random$bestTune
print(view_rf_rand_para)

#Build model based on best fit 
rand_rf_model = randomForest(Absenteeism.time.in.hours ~.,data = train, method = "anova",importance = TRUE, mtry = 11,n_estimators =1000)
#print out summary of the model
#from the summary it is seen that this model explains 34.36%of the variance and the MSE of the model is 0.27
print(rand_rf_model)

#Presdict test data using random forest model
rand_rf_Predictions = predict(rand_rf_model, test[-46])

#Compute R^2
rand_rf_be_r2 <- rSquared(test[,46], test[,46] - rand_rf_Predictions)
print(rand_rf_be_r2)
#Compute MSE
rand_rf_be_mse <- mean((test[,46] - rand_rf_Predictions)^2)
print(rand_rf_be_mse)

# r2 0.48.5
# mse 0.23
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################

# Create model based on paramters grid specificed with 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid = expand.grid(.mtry=c(1:30))
rf_grid = caret::train(Absenteeism.time.in.hours ~.,data = train, method="rf", metric="Rsquared", tuneGrid=tunegrid, trControl=control)

#print out summary of the model
print(rf_grid)
#Plot RMSE Vs mtry values
#from the plot we can see that when the randomly selected predictor is 6 RMSE is at its lowest
plot(rf_grid)

#Best fit parameters
view_rf_grid_pram = rf_grid$bestTune
print(view_rf_grid_pram)

#Build model based on best fir value
grid_rf_model = randomForest(Absenteeism.time.in.hours ~.,data = train, method = "anova",importance = TRUE,mtry = 15)
#print out summary of the model
#from the summary we see that this model explains 33.52% of the variance and mse 0.27.
print(grid_rf_model)

#Presdict test data using model
grid_rf_Predictions = predict(grid_rf_model, test[-46])

#Compute R^2
grid_rf_be_r2 = rSquared(test[,46], test[,46] - grid_rf_Predictions)
print(grid_rf_be_r2)
#Compute MSE
grid_rf_be_mse = mean((test[,46] - grid_rf_Predictions)^2)
print(grid_rf_be_mse)
#r2 0.4949
#mse 0.2339 
####################################################################################################################################
####################################
###Gradient Boosting
####################################
#Building model
gbm_base = gbm(Absenteeism.time.in.hours~., data = train,distribution = "gaussian",n.trees = 10000,
               shrinkage = 0.01, interaction.depth = 4)
#Print out summary of the model
summary(gbm_base)

#Presdict test data using random forest model
gbm_Predictions = predict(gbm_base, test[-46],n.trees = 10000)

#Compute R^2
gbm_be_r2 = rSquared(test[,46], test[,46] - gbm_Predictions)
print(gbm_be_r2)
#Compute MSE
gbm_be_mse = mean((test[,46] - gbm_Predictions)^2)
print(gbm_be_mse)
#r2 = 0.456
#mse = 0.251
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with random paramters
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
set.seed(seed)
gbm_random = caret::train(Absenteeism.time.in.hours ~.,data = train, method="gbm", metric="Rsquared", tuneLength =10, trControl=control)
#print out summary of model
print(gbm_random)

#Best tune parameters
view_gbm_rand_param = gbm_random$bestTune
print(view_gbm_rand_param)

#Presdict test data using model
rand_gbm_Predictions = predict(gbm_random, test[-46],n.trees = 48, interaction.depth = 3, 
                               shrinkage = 0.3158928, n.minobsinnode = 9)

#Compute R^2
rand_gbm_be_r2 = r2 = rSquared(test[,46], test[,46] - rand_gbm_Predictions)
print(rand_gbm_be_r2)
#Compute MSE
rand_gbm_be_mse = mean((test[,46] - rand_gbm_Predictions)^2)
print(rand_gbm_be_mse)
# r2 0.464
#mse 0.247
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################
#Creat a model with 5 kfold and 1 repeat
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid <- expand.grid(n.trees = seq(44,50, by = 2),
                        interaction.depth = c(2:4), 
                        shrinkage = c(0.01,0.02,0.3),
                        n.minobsinnode = seq(5,13, by = 2))
gbm_grid <- caret::train(Absenteeism.time.in.hours ~.,data = train, method="gbm", metric="Rsquared", trControl=control, tuneGrid=tunegrid)
#Print out model summary
print(gbm_grid)
#PLot model 
#Boosting Iteration Vs RMSE for repeated cross validation is plotted for every combination
#of the grid parameter. It can be noted that the max Rsquared is achived when the shrinkage is 0.03
#interection depth 5 max tree depth.
plot(gbm_grid)

#Best tune parameters
view_gbm_grid_param = gbm_grid$bestTune
print(view_gbm_grid_param)

#Presdict test data using model
grid_gbm_Predictions = predict(gbm_grid, test[-46], n.trees = 50, interaction.depth = 2, shrinkage = 0.3,
                               n.minobsinnode = 5)
#Compute R^2
grid_gbm_be_r2 = rSquared(test[,46], test[,46] - grid_gbm_Predictions)
print(grid_gbm_be_r2)
#Compute MSE
grid_gbm_be_mse = mean((test[,46] - grid_gbm_Predictions)^2)
print(grid_gbm_be_mse)
#r2 0.524
#mse 0.2203
###################################################################################################################################
# PCA
###################################################################################################################################
train_pca = train[1:45]
test_pca = test[1:45]
set.seed(1234)
df.pca <- prcomp(train_pca, center = T) 

#install.packages("factoextra")

fviz_eig(df.pca)

# print method
print(df.pca)
# plot method
plot(df.pca, type = "l")
# summary method
summary(df.pca)
names(df.pca)
#compute standard deviation of each principal component
std = df.pca$sdev
#compute variance
pca_var <- std^2
#proportion of variance explained
exp_var = pca_var/sum(pca_var)

#scree plot
plot(exp_var, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

#cumulative scree plot
plot(cumsum(exp_var), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

#add a training set with principal components
pca.data <- data.frame(Absenteeism.time.in.hours = train$Absenteeism.time.in.hours, df.pca$x)
pca.train1 = pca.data[1:26]
##########################################################
# Miltivarient Linear Regression to the Training set
###########################################################
#Building model
set.seed(1234)
regressor = lm(formula = Absenteeism.time.in.hours ~.,data = pca.train1)
summary(regressor)

#transform test into PCA
test.data <- predict(df.pca, newdata = test_pca)
test.data <- as.data.frame(test.data)

#select the first 30 components
test.data <- test.data[,1:25]

# Predicting the Test data output
y_pred = predict(regressor, test.data)

#Compute R^2
mlr_pca_r2 <- rSquared(test[,46], test[,46] - y_pred)
print(mlr_pca_r2)
#Compute MSE
mlr_pca_mse <- mean((test[,46] - y_pred)^2)
print(mlr_pca_mse)
# r2 50
# mse 0.23
###################################################################################################################################
### Decission Tree Regression #####################################################################################################
###################################################################################################################################

set.seed(seed)
#Building model
fit = rpart(Absenteeism.time.in.hours ~.,data = pca.train1, method = "anova")
#Variable importance
fit$variable.importance

# Predicting the Test data output
predictions_DT = predict(fit, test.data)

#Compute R^2
dt_pca_r2 <- rSquared(test[,46], test[,46] - predictions_DT)
print(dt_pca_r2)
#Compute MSE
dt_pca_mse <- mean((test[,46] - predictions_DT)^2)
print(dt_pca_mse)
# r2 0.40
# nse 0.28
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
set.seed(seed)
maxdepth = c(1:100)
tunegrid = expand.grid(.maxdepth=maxdepth)
dt_random = caret::train(Absenteeism.time.in.hours ~.,data = pca.train1, method="rpart2", metric="RMSE", tuneLength =10, trControl=control)

#print out summary of the model
print(dt_random)

#Best fit parameters
view_dt_rand_para = dt_random$bestTune
print(view_dt_rand_para)
#Build model based on best fit 
rand_dt_model = rpart(Absenteeism.time.in.hours ~.,data = pca.train1, method = "anova", maxdepth = 13)
#print out summary of the model
print(rand_dt_model)

#Presdict test data using random forest model
rand_dt_Predictions = predict(rand_dt_model, test.data)

#Compute R^2
rand_dt_pca_r2 = rSquared(test[,46], test[,46] - rand_dt_Predictions)
print(rand_dt_pca_r2)
#Compute MSE
rand_dt_pca_mse = mean((test[,46] - rand_dt_Predictions)^2)
print(rand_dt_pca_mse)
#r2 0.40
#mse 0.28
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################

# Create model based on paramters grid specificed with 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid = expand.grid(.maxdepth=c(6:18))
dt_grid = caret::train(Absenteeism.time.in.hours ~.,data = pca.train1, method="rpart2", metric="Rsquared", tuneGrid=tunegrid, trControl=control)

#print out summary of the model
print(dt_grid)
#Plot RMSE Vs mtry values
#From this plot we can see that when tree depth is 10 RMSE is at its lowest
plot(dt_grid)

#Best fit parameters
view_dt_grid_pram = dt_grid$bestTune
print(view_dt_grid_pram)

#Build model based on best fir value
grid_dt_model = rpart(Absenteeism.time.in.hours ~.,data = pca.train1, method = "anova",maxdepth = 9)
#print out summary of the model
print(grid_dt_model)

#Presdict test data using model
grid_dt_Predictions = predict(grid_dt_model, test.data)

#Compute R^2
grid_dt_pca_r2 = rSquared(test[,46], test[,46] - grid_dt_Predictions)
print(grid_dt_pca_r2)
#Compute MSE
grid_dt_pca_mse = mean((test[,46] - grid_dt_Predictions)^2)
print(grid_dt_pca_mse)
# r2 = 0.40
# mse 0.28
####################################################################################################################################
####################################
###Random Forest Regression
####################################
#Building model
RF_model = randomForest(Absenteeism.time.in.hours ~.,data = pca.train1, method = "anova",importance = TRUE)
#Prints out model information
print(RF_model)

#Predicting the Test data output
RF_Predictions = predict(RF_model, test.data)

#Compute R^2
rf_pca_r2 = rSquared(test[,46], test[,46] - RF_Predictions)
print(rf_pca_r2)
#Compute MSE
rf_pca_mse = mean((test[,46] - RF_Predictions)^2)
print(rf_pca_mse)
#r2 0.51
#mse = 0.22
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
set.seed(seed)
rf_random = caret::train(Absenteeism.time.in.hours ~.,data = pca.train1, method="rf", metric="Rsquared", tuneLength =10, trControl=control)

#print out summary of the model
#here we see that when mtry is 5 RMSE is at its lowest
print(rf_random)

#Best fit parameters
view_rf_rand_para = rf_random$bestTune
print(view_rf_rand_para)

#Build model based on best fit 
rand_rf_model = randomForest(Absenteeism.time.in.hours ~.,data = pca.train1, method = "anova",importance = TRUE, mtry = 13,n_estimators =1000)
#print out summary of the model
#from the summary it is seen that this model explains 32.18%of the variance and the MSE of the model is 0.28
print(rand_rf_model)

#Presdict test data using random forest model
rand_rf_Predictions = predict(rand_rf_model, test.data)

#Compute R^2
rand_rf_pca_r2 <- rSquared(test[,46], test[,46] - rand_rf_Predictions)
print(rand_rf_pca_r2)
#Compute MSE
rand_rf_pca_mse <- mean((test[,46] - rand_rf_Predictions)^2)
print(rand_rf_pca_mse)

# r2 0.52
# mse 0.22
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################

# Create model based on paramters grid specificed with 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid = expand.grid(.mtry=c(1:30))
rf_grid = caret::train(Absenteeism.time.in.hours ~.,data = pca.train1, method="rf", metric="Rsquared", tuneGrid=tunegrid, trControl=control)

#print out summary of the model
print(rf_grid)
#Plot RMSE Vs mtry values
#from the plot we can see that when the randomly selected predictor is 6 RMSE is at its lowest
plot(rf_grid)

#Best fit parameters
view_rf_grid_pram = rf_grid$bestTune
print(view_rf_grid_pram)

#Build model based on best fir value
grid_rf_model = randomForest(Absenteeism.time.in.hours ~.,data = pca.train1, method = "anova",importance = TRUE,mtry = 11)
#print out summary of the model
#from the summary we see that this model explains 87.55% of the variance
print(grid_rf_model)

#Presdict test data using model
grid_rf_Predictions = predict(grid_rf_model, test.data)

#Compute R^2
grid_rf_pca_r2 = rSquared(test[,46], test[,46] - grid_rf_Predictions)
print(grid_rf_pca_r2)
#Compute MSE
grid_rf_pca_mse = mean((test[,46] - grid_rf_Predictions)^2)
print(grid_rf_pca_mse)
#r2 0.503
#mse 0.23 
####################################################################################################################################
####################################
###Gradient Boosting
####################################
#Building model
gbm_base = gbm(Absenteeism.time.in.hours ~.,data = pca.train1,distribution = "gaussian",n.trees = 10000,
               shrinkage = 0.01, interaction.depth = 4)
#Print out summary of the model
summary(gbm_base)

#Presdict test data using random forest model
gbm_Predictions = predict(gbm_base, test.data,n.trees = 10000)

#Compute R^2
gbm_pca_r2 = rSquared(test[,46], test[,46] - gbm_Predictions)
print(gbm_pca_r2)
#Compute MSE
gbm_pca_mse = mean((test[,46] - gbm_Predictions)^2)
print(gbm_pca_mse)
#r2 0.51
#mse 0.22
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with random paramters
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
set.seed(seed)
gbm_random = caret::train(Absenteeism.time.in.hours ~.,data = pca.train1, method="gbm", metric="Rsquared", tuneLength =10, trControl=control)
#print out summary of model
print(gbm_random)

#Best tune parameters
view_gbm_rand_param = gbm_random$bestTune
print(view_gbm_rand_param)

#Presdict test data using model
rand_gbm_Predictions = predict(gbm_random, test.data,n.trees = 48, interaction.depth = 3, 
                               shrinkage = 0.3158928, n.minobsinnode = 9)

#Compute R^2
rand_gbm_pca_r2 = r2 = rSquared(test[,46], test[,46] - rand_gbm_Predictions)
print(rand_gbm_pca_r2)
#Compute MSE
rand_gbm_pca_mse = mean((test[,46] - rand_gbm_Predictions)^2)
print(rand_gbm_pca_mse)
# r2 0.49
#mse 0.23
###################################################################################################################################
# Grid Search #####################################################################################################################
###################################################################################################################################
#Creat a model with 5 kfold and 1 repeat
control <- trainControl(method="repeatedcv", number=5, repeats=1, search="grid")
set.seed(seed)
tunegrid <- expand.grid(n.trees = seq(44,50, by = 2),
                        interaction.depth = c(2:4), 
                        shrinkage = c(0.01,0.02,0.3),
                        n.minobsinnode = seq(5,13, by = 2))
gbm_grid <- caret::train(Absenteeism.time.in.hours ~.,data = pca.train1, method="gbm", metric="Rsquared", trControl=control, tuneGrid=tunegrid)
#Print out model summary
print(gbm_grid)
#PLot model 
#Boosting Iteration Vs RMSE for repeated cross validation is plotted for every combination
#of the grid parameter. It can be noted that the least RMSE is achived when the shrinkage is 0.02
#n.minobsinnode is 13 max tree depth.
plot(gbm_grid)

#Best tune parameters
view_gbm_grid_param = gbm_grid$bestTune
print(view_gbm_grid_param)

#Presdict test data using model
grid_gbm_Predictions = predict(gbm_grid, test.data, n.trees = 50, interaction.depth = 4, shrinkage = 0.2,
                               n.minobsinnode = 13)
#Compute R^2
grid_gbm_pca_r2 = rSquared(test[,46], test[,46] - grid_gbm_Predictions)
print(grid_gbm_pca_r2)
#Compute MSE
grid_gbm_pca_mse = mean((test[,46] - grid_gbm_Predictions)^2)
print(grid_gbm_pca_mse)
#r2 0.37
#mse 0.28

####################################################################################################
#CLASSFICATION
#####################################################################################################
#####################################################################################################
df = df_class

##################################Feature Engineering##############################################
###################################################################################################
## Binning the target varible and converting it to a catergroical variable
df$Absenteeism.time.in.hours = factor(df$Absenteeism.time.in.hours)
levels(df$Absenteeism.time.in.hours)
df$Absenteeism.time.in.hours = combineLevels(df$Absenteeism.time.in.hours, c(3,4,5,6), newLabel = 2)
df$Absenteeism.time.in.hours = combineLevels(df$Absenteeism.time.in.hours, c(3,4,5), newLabel = 3)
table(df$Absenteeism.time.in.hours)

## Chi-squared Test of Independence
#conducting Chi Squared test on the catergorical variables and the new target
factor_index = sapply(df,is.factor)
factor_data = df[,factor_index]

for (i in 1:12)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Absenteeism.time.in.hours,factor_data[,i])))
}

## Dimension Reduction
df = subset(df, select = -c(Body.mass.index, Day.of.the.week,Social.drinker))
seed = 1234
set.seed(seed)
train.index = createDataPartition(df$Absenteeism.time.in.hours, p = .80, list = FALSE)
train = df[ train.index,]
test  = df[-train.index,]

##############################################################################################################################
#Randomforest
##############################################################################################################################
## Building model
model <- randomForest(Absenteeism.time.in.hours ~ ., data = train)
# Predictions
pred <- predict(model, newdata = test)
#Confusion matrix
cm=table(pred, test$Absenteeism.time.in.hours)

#Accuracy
rf = sum(diag(cm))/nrow(test)
print(rf)
# accuracy 0.66
###################################################################################################################################
## Random Search ##################################################################################################################
###################################################################################################################################

# Create model with Random paramters 5 fold CV with 1 repeats
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
set.seed(seed)
rf_random = caret::train(Absenteeism.time.in.hours ~.,data = train, method="rf", metric= "Accuracy", tuneLength =10, trControl=control)

#print out summary of the model
#here we see that when mtry is 5 RMSE is at its lowest
print(rf_random)

#Best fit parameters
view_rf_rand_para = rf_random$bestTune
print(view_rf_rand_para)

#Build model based on best fit 
rand_rf_model = randomForest(Absenteeism.time.in.hours ~.,data = train,importance = TRUE, mtry =65 ,n_estimators =1000)
#print out summary of the model
#from the summary it is seen that this model explains 86.88%of the variance
print(rand_rf_model)

pred <- predict(rand_rf_model, test)
cm=table(pred, test$Absenteeism.time.in.hours)

#Accuracy
rand_c_rf = sum(diag(cm))/nrow(test)
print(rand_c_rf)
# accuracy 0.64
##############################################################################################################################
#Multinomainal logestic regression
##############################################################################################################################

Y = subset(df, select =c(Absenteeism.time.in.hours))
X = subset(df, select =c(ID,Reason.for.absence,Month.of.absence,Transportation.expense, Distance.from.Residence.to.Work, 
                         Service.time ,Age,Work.load.Average.day.,Hit.target,Disciplinary.failure,Son,
                         Social.smoker ,Pet ,Weight,Height))

# Function Creates the dummy variables and drops the first dummy variable
X <- fastDummies::dummy_cols(X, remove_first_dummy = TRUE)

## Dropping the original catergorical variable
#df = subset(df, select = -c(Pet,Son, Social.drinker,Social.smoker, Month.of.absence, Reason.for.absence, ID))
X = subset(X, select = -c(Social.smoker,Pet,Son, Month.of.absence, Reason.for.absence, ID,Disciplinary.failure))

logit_df = data.frame(X, Y)
#logit_df$out = relevel(logit_df$Absenteeism.time.in.hours,ref='1') 
seed = 1234
set.seed(seed)
train.index = createDataPartition(logit_df$Absenteeism.time.in.hours, p = .80, list = FALSE)
logit_train = logit_df[ train.index,]
logit_test  = logit_df[-train.index,]

mod <- multinom(Absenteeism.time.in.hours~., data = logit_train)
summary(mod)

logit_predit =predict(mod,logit_test[1:70])

cm = table(logit_predit,logit_test$Absenteeism.time.in.hours)
#Accuracy
mlr = sum(diag(cm))/nrow(logit_test)
print(mlr)
# accuracy 0.66
#######################################################################################################################################
##Decision tree for classification
#######################################################################################################################################

#Develop Model on training data
C50_model = C5.0(Absenteeism.time.in.hours ~., train, trials = 100, rules = TRUE)

#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Lets predict for test cases
C50_Predictions = predict(C50_model, test[,-18], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(test$Absenteeism.time.in.hours, C50_Predictions)
confusionMatrix(ConfMatrix_C50)

#Accuracy
dt = sum(diag(ConfMatrix_C50))/nrow(logit_test)
print(dt)
# accuracy 0.64
###########################################################################################################################################
# Model results
###########################################################################################################################################

backward_elemination_results = data.frame('Model name'=c('Multivarient linear regression', 'Decision tree default', 'Decision tree Random Search CV', 
                                    'Decision tree Grid Search CV', 'Random Forest Default', 'Random Forest Random Search CV', 
                                    'Random Forest Grid Search CV', 'Gradient Boosting Default', 'Gradient Boosting Random Search CV',
                                    'Gradient Boosting Grid Search CV'), 'MSE'=c(mlr_be_mse, dt_be_mse, rand_dt_be_mse, grid_dt_be_mse, rf_be_mse,
                                                                                 rand_rf_be_mse, grid_rf_be_mse, gbm_be_mse, rand_gbm_be_mse, grid_gbm_be_mse),
                                    'R^2'=c(mlr_be_r2, dt_be_r2, rand_dt_be_r2, grid_dt_be_r2, rf_be_r2, rand_rf_be_r2, grid_rf_be_r2, gbm_be_r2, 
                                            rand_gbm_be_r2, grid_gbm_be_r2))

pca_results = data.frame('Model name'=c('Multivarient linear regression', 'Decision tree default', 'Decision tree Random Search CV', 
                                                         'Decision tree Grid Search CV', 'Random Forest Default', 'Random Forest Random Search CV', 
                                                         'Random Forest Grid Search CV', 'Gradient Boosting Default', 'Gradient Boosting Random Search CV',
                                                         'Gradient Boosting Grid Search CV'), 'MSE'=c(mlr_pca_mse, dt_pca_mse, rand_dt_pca_mse, grid_dt_pca_mse, rf_pca_mse,
                                                                                                      rand_rf_pca_mse, grid_rf_pca_mse, gbm_pca_mse, rand_gbm_pca_mse, grid_gbm_pca_mse),
                                          'R^2'=c(mlr_pca_r2, dt_pca_r2, rand_dt_pca_r2, grid_dt_pca_r2, rf_pca_r2, rand_rf_pca_r2, grid_rf_pca_r2, gbm_pca_r2, 
                                                  rand_gbm_pca_r2, grid_gbm_pca_r2))


pca_results = data.frame('Model name'=c('Multinominal logistic regression', 'Decision tree default', 'Random Forest Default', 'Random Forest Random Search CV'), 'Accuracy'=c(mlr, dt, rf, rand_c_rf))
######################################################################################################################################
# Predicting losses every month in 2011 if same trend of absenteeism continues
######################################################################################################################################

loss_df = df_class

for(i in 1:length(loss_df$ID)){
  op = ((loss_df$Work.load.Average.day.[i]/24)*loss_df$Absenteeism.time.in.hours[i])
  loss_df$work.loss.per.day[i] = op
} 

loss.per.month.in.hours = aggregate(loss_df$Absenteeism.time.in.hours, by=list(Category=loss_df$Month.of.absence), FUN=sum)

work.loss.per.month = aggregate(loss_df$work.loss.per.day, by=list(Category=loss_df$Month.of.absence), FUN=sum)
