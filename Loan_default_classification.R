# Load library----
library(dplyr) # Data manipulation and transformation
library(psych) # Descriptive analysis
library(Amelia) # Missing Data Map
library(ggplot2) # Visualization
library(naniar) # Missing Value Pattern checking
library(mice) # Missing value checking
library(gmodels)
library(randomForest) # Random Forest
library(Information)
library(ROSE) # Undersampling data
library(pROC) # Draw ROC
library(caret)
library(MLmetrics) # Check F1 Score
library(Matrix)
library(xgboost)
library(tidyverse)
library(gmodels)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(rpart)
library(rpart.plot)
library(readr)
library(caret)
library(MLmetrics)

# DATA PREPARATION ----
# Load data----
df <- read.csv(file.choose())

# Structure of data
str(df)

# Dimension reduction----
describe(df)

# ID investigation
table(duplicated(df$ID))
df$ID <- NULL # Drop ID column

# Drop year
df$year <- NULL # Drop year column

# Investigate construction type
prop.table(table(df$construction_type))
df$construction_type <- NULL # Drop construction type column

# Investigate Security 
prop.table(table(df$Secured_by))
df$Secured_by <- NULL # Drop the column

# Investigate Security
prop.table(table(df$Security_Type))
df$Security_Type <- NULL # Drop the column

# Remove by domain knowledge
df$Upfront_charges <- NULL # Drop the column
df$Neg_ammortization <- NULL # Drop the column

# Duplication ----
summary(duplicated(df))

# Data transformation----
str(df)
# Convert
# Factor some features
ranfac <- c("loan_limit","Gender","approv_in_adv","loan_type","loan_purpose","Credit_Worthiness","open_credit",
            "business_or_commercial","interest_only","lump_sum_payment","occupancy_type",
            "credit_type","co.applicant_credit_type","submission_of_application","Region","total_units")
df[ranfac] <- lapply(df[ranfac], factor)

# Convert total unit
summary(df$total_units)
df$total_units<-as.numeric(substr(df$total_units,1,1)) # Take left value then convert to number

# Missing values----
# Map for missing values
par(mar=c(4,4,4,8))
missmap(df, main = "Missing values in dataset")
# Detail
sapply(df,function(x) sum(is.na(x)))

# Missing value pattern detection
# Test pattern for missing values
MARC <- c("Status","approv_in_adv","Credit_Worthiness","rate_of_interest",
          "Interest_rate_spread", "term","interest_only","Credit_Score",
          "submission_of_application","LTV", "dtir1")
NA_result <- mcar_test(df[MARC])
NA_result
dev.off()
md.pattern(df[MARC],rotate.names=TRUE)

# Data manipulation ----
# Complete Case Analysis
df <- df[complete.cases(df[ ,"term"]),]
df <- df[complete.cases(df[ ,"submission_of_application"]),]

#Missing data check
sapply(df,function(x) sum(is.na(x)))

# Rate of interest and interest of spread
describe(df$rate_of_interest)

hist(df$rate_of_interest, 
     main ="Histogram of interest rate in 2019", 
     xlab = "interest rate")

boxplot(df$rate_of_interest,  
        main ="Boxplot of interest rate in 2019", 
        xlab = "interest rate")

quantile(df$rate_of_interest, na.rm=TRUE)

df$rate_of_interest[is.na(df$rate_of_interest)] <- 3.99 # Fill out by median
df$Interest_rate_spread <- NULL # Drop the column 

# Property value and LTV
describe(df$property_value)
hist(df$property_value, 
     main ="Histogram of property value", 
     xlab = "Property value")

# Property vs region
ggplot(df, aes(x = Region, y= property_value, color=Region))+
  geom_boxplot()+
  ylab("Property value") + 
  xlab("Region")+
  ggtitle("Affect of region on property value")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
df %>% group_by(Region) %>% 
  summarise(Mean_value=mean(property_value,na.rm=TRUE),
            Median_value=median(property_value,na.rm=TRUE))
# Property vs units
ggplot(df, aes(x = total_units, y= property_value, color=total_units, group=total_units))+
  geom_boxplot()+
  ylab("Property value") + 
  xlab("Total property unit")+
  ggtitle("Affect of Total property unit on property value")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
df %>% group_by(total_units) %>% 
  summarise(Mean_value=mean(property_value,na.rm=TRUE),
            Median_value=median(property_value,na.rm=TRUE))

# replace by median
df <- df %>% group_by(total_units) %>% 
             mutate(property_value = ifelse(is.na(property_value), 
                           median(property_value, na.rm = TRUE), 
                           property_value))
df$LTV <- NULL # Drop the column 

# Income
hist(df$income,
     breaks = 300, 
     main ="Histogram of income", 
     xlab = "income")

# Use MICE to impute data
to_income <- c("income","Gender","business_or_commercial","loan_amount","property_value",
            "occupancy_type","Credit_Score") # Choose most important variables to income
# Running MIC
tempData <- mice(df2[to_income],m=5,method = "pmm",seed=500) # impute for missing data
#Summary and visualization
summary(tempData)
densityplot(tempData)
# Fill data in
completedData <- complete(tempData,1)
# Replace in the original data frame
df$income <- completedData$income


df$dtir1 <- NULL # Drop the column 

# Outliers----
str(df)
summary(df)
describe(df)

# Income
plot(df$loan_amount,
     df$income, 
     main =" Scatter plot of Income and Loan Amount")

boxplot(df$income, 
        main ="Box plot of Income")

quantile(df$income)
quantile(df$income,0.99)

# Filter 1: Income
df <- df %>% filter(income<=26520)

# Property value
plot(df$income,
     df$property_value, 
     main =" Scatter plot of Property value and Income")

boxplot(df$property_value, 
        main ="Box plot of Property value")

quantile(df$property_value)
quantile(df$property_value,0.995)

# Filter 2: Property value
df <- df %>% filter(property_value<=2020350)

# Final check structure
df <-as.data.frame(df)
str(df)

# Fill empty
df <- df[complete.cases(df[ ,"approv_in_adv"]),]
df[df==""]<-NA
df$loan_limit <- as.character(df$loan_limit)
df$loan_limit[is.na(df$loan_limit)]<-"Missing"
df$loan_limit <- as.factor(df$loan_limit)
df$loan_purpose <- as.character(df$loan_purpose)
df$loan_purpose[is.na(df$loan_purpose)]<-"Missing"
df$loan_purpose <- as.factor(df$loan_purpose)
df$approv_in_adv <- as.character(df$approv_in_adv)
df$approv_in_adv[is.na(df$approv_in_adv)]<-"Missing"
df$approv_in_adv <- as.factor(df$approv_in_adv)
df$age <- as.factor(df$age)
# Final data
str(df)

# EDA----
# Target variable
ggplot(df, aes(x=as.character(Status)))+
  geom_bar(aes(y=..count..,fill=Status, color=Status)) +
  ylab("Density") + 
  xlab("Status")+ 
  theme(legend.position="none")
  ggtitle("Share of Loan status")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold"), 
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1)
        )
CrossTable(df$Status)
#visualization
# plot1 shows the status of applications and their percentages.
plot1 <- ggplot(df, aes(x = factor(Status))) + 
  geom_bar(fill = "#FF6666")+ 
  geom_text(stat = "count", 
            aes(label= sprintf("%.02f %%", ..count../sum(..count..)*100)), 
            size=4, 
            vjust= -0.5, 
            colour="black")+
  labs(x="status", y="Frequency") 
plot1

#male and female 
male <- sum(df$Gender == "Male", na.rm=TRUE)
female <- sum(df$Gender == "Female")
pie(c(male, female), c("Male", "Female"))

#box plot of income  with gender
d = df[, c( 'income', 'Gender')]
boxplot(income ~ Gender, 
        data = df, 
        xlab = "Gender", 
        ylab = "Income", 
        main = "")

#
plot(x = df$term,
     y = df$loan_amount,
     xlab = "Term",
     ylab = "LoanAmount",	 
     main = "Term vs LoanAmount",
)

#does interest rates influenced by the income?

#Summary
df %>%
  group_by(income) %>% 
  summarise(income_group =  mean(rate_of_interest))

#Data Visualization for Question 1
inc_rate <- df %>% 
  group_by(rate_of_interest) %>% 
  summarise(income_group =  mean(rate_of_interest))

ggplot(data = inc_rate, 
       mapping = aes(x = rate_of_interest, 
                     fill = income_group)
       ) +
  geom_histogram(aes(y=..density..), 
                 color = "green", 
                 bins = 20) +
  labs(title = "", x = "Interest rate", y = "Frequency")

###Histogram for Loan amount based on th purpose
ggplot(data = df, 
       mapping = aes(loan_amount, fill=factor(loan_type))) +
  geom_histogram(alpha = 0.6, position = 'identity')

###Histogram for Rate of interest based on credit type
ggplot(data = df, 
       mapping = aes(rate_of_interest, fill=factor(credit_type))) +
  geom_histogram(alpha = 0.6, position = 'identity')

###Boxplot for credit type and loan amount stating the status of the loan
box_plot<-ggplot(df,aes(x=credit_type,y=loan_amount,fill=Credit_Worthiness))+
  geom_boxplot()+
  labs(x='Credit Type',y='Loan Amount',title ='Loan Status based on Credit Type')
box_plot

###Approval of loan for business or commercial purpose and interest rate
scatter<-ggplot(df,aes(x=Credit_Score,y=rate_of_interest,color=factor(approv_in_adv)))+
  geom_point(size=2)+
  labs(x='Credit Score',y='Rate of Interest',title ='Loan Approval based on factors')
scatter

# Correlation Matrix

corContVar <-cor(df[sapply(df,is.numeric)],use = "complete.obs")

col <- colorRampPalette(c("darkgoldenrod4", "burlywood1",
                          "darkkhaki", "darkgreen"))

corrplot(corContVar,method = "number",col=col(200),
         order="hclust",
         type = "full",
         #addCoef.col = "black",
         tl.col="black", tl.srt=45, tl.cex=0.7, tl.offset = 0.5,
         number.cex=0.5,
         #diag = FALSE,
         number.digits = 2)

corrplot(corContVar, method="color", col=col(200),  
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, tl.offset = 0.5, #Text label color and rotation
)

mtext("Correlation Plot", family = "serif",
      col = "#0B3948", side = 2, at= 5, line= 2.50, cex=2)

# Statistical Association between Loan Type and Gender

prop.table(table(df$Gender,df$loan_type))
mosaicplot(table(df$loan_type,df$Gender),color = hcl(c(60,110)), shade = FALSE,
           main = "Types of Loans and Gender Classification",
           xlab = "Loan Type",
           ylab = "Gender",
           las = 1,
           border = "#5D2E46")

# Relationship between Property Value and Loan Amount sanctioned 
colnames(df)
ggplot(df[sapply(df,is.numeric)],aes(x = property_value, y = loan_amount )) +
  geom_point() +
  facet_wrap(.~ df$loan_type, scale ="free")

# creating subset for loan status
approved_df <- subset(df, Status == 1)

# loan approval in different region
table1 = as.table(table(approved_df$Region))
table1
approved_df %>%
  ggplot(aes(x = factor(Region))) +
  geom_bar() +
  xlab("Region") +
  ylab("No. of defaults")

# creating subset for Major and Minor region 
major_region_df <- subset(approved_df, Region %in% c("North", "south"))
minor_region_df <- subset(approved_df, Region %in% c("central", "North-East"))

# loan approval in different region group by age and gender
table2 = as.table(table(major_region_df$age, major_region_df$Gender))
table2
major_region_df %>%
  ggplot(aes(x = factor(age))) +
  geom_bar() +
  facet_wrap(~Gender) +
  xlab("Age Group") +
  ylab("No. of defaults") +
  ggtitle("Northern & Southern Region")

table3 = as.table(table(minor_region_df$age, minor_region_df$Gender))
table3
minor_region_df %>%
  ggplot(aes(x = factor(age))) +
  geom_bar() +
  facet_wrap(~Gender) +
  xlab("Age Group") +
  ylab("No. of defaults") +
  ggtitle("Central & North-East Region")

# creating subset for Major and Minor region and highest approval rate age group
major_region_35_65_df <- subset(major_region_df, age %in% c("35-44", "45-54", "55-64"))
minor_region_35_65_df <- subset(minor_region_df, age %in% c("35-44", "45-54", "55-64"))

# loan approval majority subsets of both group of region in business and 
# non business loan for each credit type
table4 = as.table(table(major_region_35_65_df$business_or_commercial, 
                        major_region_35_65_df$credit_type))
table4
major_region_35_65_df %>%
  ggplot(aes(x = factor(business_or_commercial))) +
  geom_bar() +
  facet_wrap(~credit_type) +
  xlab("business or non-business") +
  ylab("No. of defaults") +
  ggtitle("Central & North-East Region applicent with 35 to 65 age-group")

table5 = as.table(table(minor_region_35_65_df$business_or_commercial, 
                        minor_region_35_65_df$credit_type))
table5
minor_region_35_65_df %>%
  ggplot(aes(x = factor(business_or_commercial))) +
  geom_bar() +
  facet_wrap(~credit_type) +
  xlab("business or non-business") +
  ylab("No. of defaults") +
  ggtitle("Central & North-East Region applicent with 35 to 65 age-group")



# MODELING ----
# Split data
# Set seed of 567
set.seed(567)
# Store row numbers for training set: index_train
index_train <- sample(1:nrow(df), 0.8* nrow(df))
# Create training set: training_set
training_set <- df[index_train, ]
# Create test set: test_set
test_set <- df[-index_train,]

# Feature Selection for modeling
Train1 <- training_set
Test1 <- test_set

var_factor <- df %>% 
  select(where(is.character)) %>% 
  glimpse() # Extract all columns in character
name_factor <- colnames(var_factor) # Keep column name
Train1[name_factor] <- lapply(Train1[name_factor], as.numeric) # Factor all columns need
Test1[name_factor] <- lapply(Test1[name_factor], as.numeric) # Factor all columns need
str(df)

#Calculate the Information value table
IV <- create_infotables(data =Train1, valid=Test1, y="Status")

#Display result in order
result_IV <- as.data.frame(IV$Summary)
result_IV %>% arrange(desc(IV))


##LOGISTIC METHOD####################
#Full model
log.full <- glm(Status ~ ., 
                data=training_set, 
                family = binomial(link = logit))
summary(log.full)

#Backward elimination process
log.stepwise <- step(log.full, 
                     direction = "backward", 
                     trace = FALSE)
summary(log.stepwise)

# Compare two models
anova(log.full,log.stepwise,test="LRT")

# Prediction on test set
# Apply to test set
log.stepwise.prob.test <- predict(log.stepwise, 
                                  newdata = test_set, 
                                  type = "response")

# Make a binary predictions-vector using a cut-off of 50%
log.stepwise.pred.test <- ifelse(log.stepwise.prob.test > 0.5, 1, 0)
# Construct a confusion matrix
conf_matrix <- table(Actual = test_set$Status,
                     Pred = log.stepwise.pred.test)
conf_matrix
confusionMatrix(t(conf_matrix),
                positive = "1")


#WEIGHTED LOGISTIC
###### Full model with weight
# Create model weights 
table(training_set$Status)
n<- length(training_set$Status)
n0 <- table(training_set$Status)[1]
n1 <- table(training_set$Status)[2]
k<-2
model_weights<- ifelse(training_set$Status == 0,n/(k*n0),n/(k*n1))
table(model_weights)

# Run full model with weighted 
log.weighted <- glm(Status ~ loan_limit + Gender + approv_in_adv + loan_type + loan_purpose + 
                       Credit_Worthiness + open_credit + loan_amount + rate_of_interest + 
                       term + interest_only + lump_sum_payment + property_value + 
                       occupancy_type + total_units + income + credit_type + Credit_Score + 
                       co.applicant_credit_type + age + submission_of_application + 
                       Region, 
                    data=training_set,
                    weights = model_weights, 
                    family = binomial(link = logit))
summary(log.weighted)
#Make predictions on the test data using predict()
log.weighted.prob.test <- predict(log.weighted, newdata=test_set, type="response")
log.weighted.pred.test <- as.factor(ifelse(log.weighted.prob.test>=0.5, 1, 0))
#Model accuracy
conf_matrix2 <- table(Actual = test_set$Status,
                      Pred = log.weighted.pred.test)
conf_matrix2
confusionMatrix(t(conf_matrix2),
                positive = "1")

# Model comparison
# F1 Score
F1_Score(log.stepwise.pred.test, 
         as.factor(test_set$Status), 
         positive = "1")
F1_Score(log.weighted.pred.test, 
         as.factor(test_set$Status), 
         positive = "1")
# AUC
# Construct the objects containing ROC-information
ROC.log.stepwise <- roc(test_set$Status, log.stepwise.prob.test,levels = c(0, 1), direction = "<")
ROC.log.weighted <- roc(test_set$Status, log.weighted.prob.test,levels = c(0, 1), direction = "<")

# Draw all ROCs on one plot
plot(ROC.log.stepwise, col="red")
lines(ROC.log.weighted, col="blue")

# Compute the AUCs
auc(ROC.log.stepwise)
auc(ROC.log.weighted)

##DECISION TREES #################
# Unbalanced data
CrossTable(training_set$Status)

# UNDERSAMPLING METHOD
# Under sample method
result.tree <- ovun.sample(Status ~ .,
                      data = training_set,
                      method = "under",
                      p=0.5,
                      seed = 1234)
undersampled_training_set <- result.tree$data
table(undersampled_training_set$Status)

# Modeling fo decision tree
tree.undersample <- rpart(Status ~ ., method = "class",
                          data =  undersampled_training_set,
                          control = rpart.control(cp = 0.001))

# CHANGE PROBABILITY METHOD
tree.prob <- rpart(Status ~ ., method = "class",
                    data = training_set, 
                    parms = list(prior = c(0.75, 0.25)),
                    control = rpart.control(cp = 0.001))
# USING LOSS MATRIX METHOD
set.seed(345)
tree.lossmatrix <- rpart(Status ~ ., 
                         method = "class",
                         data =  training_set,
                         parms = list(loss = matrix(c(0, 3, 1, 0), ncol=2)),
                         control = rpart.control(cp = 0.001)
                         )

# USING WEIGHT
# Create Weight
case_weights_tree <- ifelse(training_set$Status== 1,3,1) # 3 for default, 1 for non-default
# run model
set.seed(345)
tree.weight <- rpart(Status ~ ., 
                     method = "class",
                     data = training_set,
                     weights=case_weights_tree,
                     control = rpart.control(cp = 0.001)
                     )

# Prune tree using optimal cp
# Cp table
printcp(tree.undersample)
printcp(tree.prob)
printcp(tree.lossmatrix)
printcp(tree.weight)

# Plot the cross-validated error rate as a function of the complexity parameter
plotcp(tree.undersample)
plotcp(tree.prob)
plotcp(tree.lossmatrix)
plotcp(tree.weight)
# Create an index for of the row with the minimum xerror
index_u <- which.min(tree.undersample$cptabl[ , "xerror"])
index_p <- which.min(tree.prob$cptabl[ , "xerror"])
index_l <- which.min(tree.lossmatrix$cptabl[ , "xerror"])
index_w <- which.min(tree.weight$cptabl[ , "xerror"])
# Create tree_min
tree_min_u <- tree.undersample$cptable[index_u, "CP"]
tree_min_u
tree_min_p <- tree.prob$cptable[index_p, "CP"]
tree_min_p
tree_min_l <- tree.lossmatrix$cptable[index_l, "CP"]
tree_min_l
tree_min_w <- tree.weight$cptable[index_w, "CP"]
tree_min_w
# Prune the tree using optimal cp
tree.undersample.prune <- prune(tree.undersample, cp = tree_min_u)
tree.prob.prune <- prune(tree.prob, cp = 0.0036)
tree.lossmatrix.prune <- prune(tree.lossmatrix, cp = tree_min_l)
tree.weight.prune <- prune(tree.weight, cp = tree_min_w)
# Use prp() and argument extra = 1 to plot the pruned tree
prp(tree.undersample.prune, extra = 1)
prp(tree.prob.prune, extra = 1)
prp(tree.lossmatrix.prune, extra = 1)
prp(tree.weight.prune, extra = 1)

rpart.plot(tree.undersample.prune, nn=TRUE, tweak=1.5)
rpart.plot(tree.prob.prune, nn=TRUE, tweak=1.7)
rpart.plot(tree.lossmatrix.prune, nn=TRUE, tweak=1.5)
rpart.plot(tree.weight.prune, nn=TRUE, tweak=1.5)

# Model comparison
# Make predictions for each of the pruned trees using the test set.
tree.undersample.prune.pred.test <- predict(tree.undersample.prune, 
                                            newdata = test_set,  
                                            type = "class")
tree.prob.prune.pred.test <- predict(tree.prob.prune, 
                                     newdata = test_set,  
                                     type = "class")
tree.lossmatrix.prune.pred.test <- predict(tree.lossmatrix.prune, 
                                           newdata = test_set,  
                                           type = "class")
tree.weight.prune.pred.test <- predict(tree.weight.prune, 
                                       newdata = test_set,  
                                       type = "class")

# construct confusion matrices using the predictions and confusion matrix
confmat_undersample <- table(Actual = test_set$Status,
                             Pred = tree.undersample.prune.pred.test)
confmat_undersample
confusionMatrix(t(confmat_undersample),
                positive = "1")

confmat_prob <- table(Actual = test_set$Status,
                      Pred = tree.prob.prune.pred.test)
confmat_prob
confusionMatrix(t(confmat_prob),
                positive = "1")

confmat_loss_matrix <- table(Actual = test_set$Status,
                             Pred = tree.lossmatrix.prune.pred.test)
confmat_loss_matrix
confusionMatrix(t(confmat_loss_matrix),
                positive = "1")

confmat_weights <- table(Actual = test_set$Status,
                         Pred = tree.weight.prune.pred.test)
confmat_weights
confusionMatrix(t(confmat_weights),
                positive = "1")

# F1 Score
F1_Score(tree.undersample.prune.pred.test, as.factor(test_set$Status), positive = "1")
F1_Score(tree.prob.prune.pred.test, as.factor(test_set$Status), positive = "1")
F1_Score(tree.lossmatrix.prune.pred.test, as.factor(test_set$Status), positive = "1")
F1_Score(tree.weight.prune.pred.test, as.factor(test_set$Status), positive = "1")


# AUC
# Construct the objects containing ROC-information
ROC.tree.undersample.prune <- roc(test_set$Status, factor(tree.undersample.prune.pred.test, ordered = TRUE),levels = c(0, 1), direction = "<")
ROC.tree.prob.prune <- roc(test_set$Status, factor(tree.prob.prune.pred.test, ordered = TRUE),levels = c(0, 1), direction = "<")
ROC.tree.lossmatrix.prune <- roc(test_set$Status, factor(tree.lossmatrix.prune.pred.test, ordered = TRUE),levels = c(0, 1), direction = "<")
ROC.tree.weight.prune <- roc(test_set$Status, factor(tree.weight.prune.pred.test, ordered = TRUE),levels = c(0, 1), direction = "<")

# Draw all ROCs on one plot
plot(ROC.tree.undersample.prune, col="red")
lines(ROC.tree.prob.prune, col="blue")
lines(ROC.tree.lossmatrix.prune, col="orange")
lines(ROC.tree.weight.prune, col="cyan4")
# Compute the AUCs
auc(ROC.tree.undersample.prune)
auc(ROC.tree.prob.prune)


## XGBOOST ######################
# Partition
set.seed(567)
ind <- sample(2,nrow(df),replace = T, prob = c(0.8,0.2))
trainset <- df[ind == 1,]
testset <- df[ind == 2,]
# Matrix One Hot Encoding 
trainm <- Matrix::sparse.model.matrix(Status ~ .-1, 
                                      data = trainset)
head(trainm)
train_label <- trainset[,'Status']
train_matrix <- xgb.DMatrix(data = as.matrix(trainm),
                            label = train_label)
testm <- Matrix::sparse.model.matrix(Status ~ .-1, 
                                     data = testset)
head(testm)
test_label <- testset[,'Status']
test_matrix <- xgb.DMatrix(data = as.matrix(testm),
                           label = test_label)
# learning task parameters
nc <- length(unique(train_label))

xgb_params <- list('objective' = 'multi:softprob',
                   'eval_metric' = 'mlogloss',
                   'num_class' = nc)

watchlist <- list(train = train_matrix,
                  test = test_matrix)

xgb.full <-
  xgb.train(
    data = train_matrix,
    params = xgb_params,
    nrounds = 66,
    watchlist = watchlist,
    eta = 0.025,
    max_depth = 15, 
    nround=25, 
    subsample = 0.5,
    colsample_bytree = 0.5,
    set.seed = 567,
    
  )

xgb.full

err <- data.frame(xgb.full$evaluation_log)
plot(err$iter, err$train_mlogloss, col = 'blue')
lines(err$iter,err$test_mlogloss, col = 'red')

min(err$test_mlogloss)
err[err$test_mlogloss == min(err$test_mlogloss),]

# Feature Importance
feat_imp <- xgb.importance(colnames(train_matrix),
                           model = xgb.full)

xgb.plot.importance(feat_imp)

# Prediction & confusion Matrix
y_pred <- predict(xgb.full, newdata = test_matrix)

pred <- matrix(y_pred, nrow = nc, ncol = length(y_pred)/2) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label, max_prob = max.col(., "last")-1)
table(Prediction = pred$max_prob, Actual = pred$label)

caret::confusionMatrix(factor(pred$max_prob),factor(pred$label),positive = '1')
# F1 Score
F1_Score(factor(pred$max_prob),factor(pred$label),positive = '1')

# --- Accuracy --- #
mean(pred$max_prob == pred$label)
accuracy <- sum(diag(table_mat)) / sum(table_mat)
accuracy

# --- Precision --- #
precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}

# --- Recall --- #
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]
  # false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}

prec <- precision(table_mat)
prec

rec <- recall(table_mat)
rec

# --- F1 Score --- #
f1 <- 2 * ((prec * rec) / (prec + rec))
f1
par(pty='s')
roc(
  pred$label,
  pred$max_prob,
  plot = TRUE,
  legacy.axes = TRUE,
  percent = TRUE,
  xlab = 'False Positive %' ,
  ylab = 'True Positive %',
  col = '#377eb8',
  lwd = 4,
  print.auc = TRUE
)
auc(pred$label, pred$max_prob)

## RANDOM FOREST METHOD
# Run Full model
training_set$Status <- as.factor(training_set$Status)
test_set$Status <- as.factor(test_set$Status)
set.seed(123)
rf.full <- randomForest(Status~., 
                        data = training_set)
rf.full
plot(rf.full,lwd=3)
# Evaluating model performance
rf.full.pred.test <- predict(rf.full, 
                             newdata=test_set)
# construct confusion matrices using the predictions.
rf.full.matrix <-  table(Actual = test_set$Status, 
                         Pred = rf.full.pred.test)
confusionMatrix(t(rf.full.matrix),positive = "1")


#### IMPROVEMENT 
# OVERSAMPLING METHOD
# Over sample method
oversamp <- ovun.sample(Status ~ .,
                      data = training_set,
                      method = "over",
                      p = 0.5,
                      seed = 1234)
oversampled_training_set <- oversamp$data
table(oversampled_training_set$Status)
# Run model
set.seed(300)
rf.Oversample = randomForest(Status~., 
                             data = oversampled_training_set, 
                             ntree = 500,
                             mtry = 5)
rf.Oversample
plot(rf.Oversample,lwd=3)
# Evaluating model performance
rf.Oversample.pred.test = predict(rf.Oversample, 
                                  newdata=test_set)
# construct confusion matrices using the predictions.
rf.Oversample.matrix <-  table(Actual = test_set$Status,
                               rf.Oversample.pred.test)
confusionMatrix(t(rf.Oversample.matrix),
                positive = "1")

#### FEATURE SELECTION METHOD + BALANCED FOREST###
# Run model
set.seed(300)
rf.feature = randomForest(Status~credit_type + rate_of_interest+property_value+
                            lump_sum_payment+income+co.applicant_credit_type+
                            submission_of_application+ loan_type +
                            business_or_commercial+Gender, 
                          data = training_set, 
                          ntree = 100,
                          mtry = 4,
                          strata = training_set$Status,
                          sampsize = rep(sum(training_set$Status==1),2))
rf.feature
plot(rf.feature, lwd=3)
# Evaluating model performance
rf.feature.pred.test = predict(rf.feature, 
                               newdata=test_set)
# construct confusion matrices using the predictions.
rf.feature.matrix <-  table(Actual = test_set$Status,
                            rf.feature.pred.test)
confusionMatrix(t(rf.feature.matrix),
                positive = "1")

# Model comparison
# F1 Score
F1_Score(rf.full.pred.test, as.factor(test_set$Status), positive = "1")
F1_Score(rf.Oversample.pred.test, as.factor(test_set$Status), positive = "1")
F1_Score(rf.feature.pred.test, as.factor(test_set$Status), positive = "1")
# AUC
# Construct the objects containing ROC-information
ROC.rf.full <- roc(test_set$Status, factor(rf.full.pred.test, ordered = TRUE),levels = c(0, 1), direction = "<")
ROC.rf.Oversample <- roc(test_set$Status, factor(rf.Oversample.pred.test,ordered = TRUE),levels = c(0, 1), direction = "<")
ROC.rf.feature <- roc(test_set$Status, factor(rf.feature.pred.test, ordered = TRUE),levels = c(0, 1), direction = "<")

# Draw all ROCs on one plot
plot(ROC.rf.full, col="red")
lines(ROC.rf.Oversample, col="blue")
lines(ROC.rf.feature, col="cyan4")

# Compute the AUCs
auc(ROC.rf.full)
auc(ROC.rf.Oversample)
auc(ROC.rf.feature)

## FINAL COMPARISON AND CONCLUSION
### BY METRICS
library(readxl)
Model_result <- read_excel("D:/OneDrive - DO NOT DELETE/OneDrive - Northeastern University/NEU/09. ALY 6040 - Data Mining/Group/Phase 4/Model.result.xlsx")
Model_result
#Visusalize
Model_result %>% gather(metrics, percentage,Accuracy, Sensitivity) %>% 
  ggplot(., aes(x= Model, y= percentage, group=metrics, color = metrics)) + 
  geom_line() +
  ylab("Percentage") + 
  xlab("Model")+ 
  ggtitle("Performance Metrics of Accuracy vs Sensitivity")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )+
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1)
        )

ggplot(Model_result, aes(x= reorder(Model,FN), y= FN)) + 
  #geom_bar(stat = "identity",aes(label =FN)) +
  geom_col(color="lightblue",fill="lightblue") +
  geom_text(aes(label =FN)) +
  ylab("Number of FN") + 
  xlab("Model")+ 
  ggtitle("Number of FN by models")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )+
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1)
        )


### BY BAD RATE
test_set$Status<- as.numeric(test_set$Status)
#Function
strategy_bank <- function(prob_of_def){
  cutoff=rep(NA, 21)
  bad_rate=rep(NA, 21)
  accept_rate=seq(1,0,by=-0.05)
  for (i in 1:21){
    cutoff[i]=quantile(prob_of_def,accept_rate[i])
    pred_i=ifelse(prob_of_def> cutoff[i], 1, 0)
    pred_as_good=test_set$Status[pred_i==0]
    bad_rate[i]=sum(pred_as_good)*0.1/length(pred_as_good)}
  table=cbind(accept_rate,cutoff=round(cutoff,4),bad_rate=round(bad_rate,4))
  return(list(table=table,bad_rate=bad_rate, accept_rate=accept_rate, cutoff=cutoff))
}

# Apply the function strategy_bank to all model
# Probability
log.stepwise.prob.test <- predict(log.stepwise, newdata = test_set, type = "response")
log.weighted.prob.test <- predict(log.weighted, newdata=test_set, type="response")
tree.prob.prune.prob.test <- predict(tree.prob.prune, newdata = test_set)[ ,2]
tree.lossmatrix.prob.pred.test <- predict(tree.lossmatrix.prune, newdata = test_set)[ ,2]
xgb.full.prob.test <- pred[,1]
rf.Oversample.prob.test = predict(rf.Oversample, newdata=test_set,type="prob")[ ,2]
rf.feature.prob.test = predict(rf.feature, newdata=test_set,type="prob")[ ,2]
rf.full.prob.test <- predict(rf.full, newdata=test_set,type="prob")[ ,2]

# Apply function
strategy.log.stepwise<- strategy_bank(log.stepwise.prob.test)
strategy.log.weighted <- strategy_bank(log.weighted.prob.test)
strategy.tree.prob.prune<- strategy_bank(tree.prob.prune.prob.test)
strategy.tree.lossmatrix.prune<- strategy_bank(tree.lossmatrix.prob.pred.test)
strategy.xgb.full <-strategy_bank(xgb.full.prob.test)
strategy.rf.Oversample<- strategy_bank(rf.Oversample.prob.test)
strategy.rf.feature <- strategy_bank(rf.feature.prob.test)
strategy.rf.full<- strategy_bank(rf.full.prob.test)

# Plot the strategy functions
par(mfrow = c(4,2))
plot(strategy.log.stepwise$accept_rate, strategy.log.stepwise$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = "Stepwise Logistic")
plot(strategy.log.weighted$accept_rate, strategy.log.weighted$bad_rate, 
     type = "l", xlab = "Acceptance rate", ylab = "Bad rate", 
     lwd = 2, main = " Weighted Logistic Regression")
plot(strategy.tree.prob.prune$accept_rate, strategy.tree.prob.prune$bad_rate, 
     type = "l", xlab = "Acceptance rate", 
     ylab = "Bad rate", lwd = 2, main = "Pruned Probability Tree")
plot(strategy.tree.lossmatrix.prune$accept_rate, strategy.tree.lossmatrix.prune$bad_rate, 
     type = "l", xlab = "Acceptance rate", 
     ylab = "Bad rate", lwd = 2, main = " Pruned Loss Matrix Tree")
# plot(strategy.xgb.full$accept_rate, strategy.xgb.full$bad_rate, 
#      type = "l", xlab = "Acceptance rate", 
#      ylab = "Bad rate", lwd = 2, main = "XGBoost")
plot(strategy.rf.full$accept_rate, strategy.rf.full$bad_rate, 
     type = "l", 
     xlab = "Acceptance rate", 
     ylab = "Bad rate", 
     lwd = 2, 
     main = "Default Random Forest"
     )
plot(strategy.rf.Oversample$accept_rate, strategy.rf.Oversample$bad_rate, 
     type = "l", 
     xlab = "Acceptance rate", 
     ylab = "Bad rate", 
     lwd = 2, 
     main = "Oversampled Random Forest")
plot(strategy.rf.feature$accept_rate, strategy.rf.feature$bad_rate, 
     type = "l", 
     xlab = "Acceptance rate", 
     ylab = "Bad rate", 
     lwd = 2, 
     main = "Balanced Random Forest")
# test case
Model_Selection <- function(rate){
  index = 2+ ((1-rate)/0.05)
result.table = rbind(strategy.log.stepwise$table[index,c(1,3)],
              strategy.log.weighted$table[index,c(1,3)],
              strategy.tree.prob.prune$table[index,c(1,3)],
              strategy.tree.lossmatrix.prune$table[index,c(1,3)],
              strategy.rf.Oversample$table[index,c(1,3)],
              strategy.rf.feature$table[index,c(1,3)],
              strategy.rf.full$table[index,c(1,3)])
result.table <- as.data.frame(result.table)
result.table <- result.table %>% 
  mutate(Model = c("log.stepwise","log.weighted","tree.prob.prune",
                    "tree.lossmatrix.prune",'rf.Oversample',"rf.feature","rf.full")) %>% 
  arrange(bad_rate)                           

return(result.table)}
  
# Test case
Model_Selection(0.9)

Model_Selection(0.8)

Model_Selection(0.5)


### SUGGESTIONS FOR BUSINESS ###
result_IV <- as.data.frame(IV$Summary)
result_IV <- result_IV %>% arrange(desc(IV)) 
result_IV$Top_5 <- ifelse(result_IV$IV>0.134,"Yes","No")
# Visualize top 5 by color
ggplot(result_IV[1:12,], aes(x= reorder(Variable,-IV), y= IV)) + 
  #geom_bar(stat = "identity",aes(label =FN)) +
  geom_col(aes(color=Top_5,fill=Top_5)) +
  geom_text(aes(label =round(IV,2)),vjust = -0.5) +
  ylab("Information value") + 
  xlab("Most powerful predictors")+ 
  ggtitle("Top 5 powerful predictors")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )+
  theme(axis.text.x = element_text(angle = 90, 
                                   vjust = 1, 
                                   hjust=1)
        )

# Analyse Credit type
CrossTable(df$credit_type,df$Status,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)

df %>% group_by(credit_type, Status) %>% 
  mutate (Count = n()) %>% 
  ggplot(.,aes(x= credit_type, y= Count, fill=as.character(Status))) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Count") + 
  xlab("Credit type")+ 
  ggtitle("Default possibility by Credit type")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )

# Analyse Rate of Interest
df %>% group_by(Status) %>% 
  summarize (Mean_Interest = mean(rate_of_interest,na.rm = T),
                                    Median_Interest=median(rate_of_interest,na.rm = T))
describe(df$rate_of_interest[df$Status==1])
describe(df$rate_of_interest[df$Status==0])

ggplot(df, aes(y= rate_of_interest,x=as.character(Status), colour = as.character(Status))) + 
  geom_jitter()+ 
  geom_boxplot(alpha=0.6, size= 1) + 
  ylab("Rate of interest") + xlab("Status type")+ 
  ggtitle("Default possibility by Rate of Interest")+
  theme(plot.title = element_text(hjust = 0.5, color = "cyan4", size = 12, 
                                  face = "bold"))

# Analyse Property value
df %>% group_by(Status) %>% summarize (Mean_Property_Value = mean(property_value,na.rm = T),
                                       Median_Property_Value= median(property_value,na.rm = T))
describe(df$property_value[df$Status==1])
describe(df$property_value[df$Status==0])

ggplot(df, aes(y= property_value,x=as.character(Status), colour = as.character(Status))) + 
  geom_jitter()+ 
  geom_boxplot(alpha=0.6, size= 1) + 
  ylab("Property value") + 
  xlab("Status type")+ 
  ggtitle("Default possibility by Property value")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold"))

# Analyse lump sum payment
CrossTable(df$lump_sum_payment,df$Status,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)

df %>% group_by(lump_sum_payment, Status) %>% 
  mutate (Count = n()) %>% 
  ggplot(.,aes(x= lump_sum_payment, y= Count, fill=as.character(Status))) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Count") + 
  xlab("Lump sum payment")+ 
  ggtitle("Default possibility by Lump sum payment")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )

# Analyse income
df %>% group_by(Status) %>% summarize (Mean_Income = mean(income),
                                       Median_Income= median(income))
describe(df$income[df$Status==1])
describe(df$income[df$Status==0])

ggplot(df, aes(y= income,x=as.character(Status), colour = as.character(Status))) + 
  geom_jitter()+ 
  geom_boxplot(alpha=0.6, size= 1) + 
  ylab("Income") + 
  xlab("Status type")+ 
  ggtitle("Default possibility by Income")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )

# Business or Commercial
CrossTable(df$business_or_commercial,df$Status,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)

df %>% group_by(business_or_commercial, Status) %>% 
  mutate (Count = n()) %>% 
  ggplot(.,aes(x= business_or_commercial, y= Count, fill=as.character(Status))) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Count") + 
  xlab("Business/ Commercial Loan")+ 
  ggtitle("Default possibility by Loan type")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )
# Gender
CrossTable(df$Gender,df$Status,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)

df %>% group_by(Gender, Status) %>% 
  mutate (Count = n()) %>% 
  ggplot(.,aes(x= Gender, y= Count, fill=as.character(Status))) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Count") + 
  xlab("Gender")+ 
  ggtitle("Default possibility by Gender")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )

# Type of submission
CrossTable(df$submission_of_application,df$Status,prop.r=TRUE,prop.c=FALSE,prop.t=FALSE,prop.chisq=FALSE)

df %>% group_by(submission_of_application, Status) %>% 
  mutate (Count = n()) %>% 
  ggplot(.,aes(x= submission_of_application, y= Count, fill=as.character(Status))) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Count") + 
  xlab("Type of Submission")+ 
  ggtitle("Default possibility by Submission")+
  theme(plot.title = element_text(hjust = 0.5, 
                                  color = "cyan4", 
                                  size = 12, 
                                  face = "bold")
        )