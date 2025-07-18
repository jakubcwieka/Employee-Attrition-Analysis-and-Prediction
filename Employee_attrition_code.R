

#----------------------- IMPORTING DATA, CLEANING, PREPARATION -----------------------------
  
library(tidyverse)
library(openxlsx)
library(skimr)
library(dplyr)

  setwd("set your location")

  df = read.xlsx("imput the data location")
  

#### Check for any missing values, though dataset should be clean and ready for analysis

  missing = is.na(df)
  sum(missing) 
  
  rm(missing) #no missing data, removing the unnecessary object
  
#### Check data structure
  
  df %>% head(20)
  
#### EmployeeCount, Over18 and StandardHours seem to be constant across all observations
  
  sapply(df, function(x) length(unique(x)))
  
# Constant variables, removing along with employee number 
  
  df = df %>% select(!c(EmployeeCount, Over18, StandardHours, EmployeeNumber))
  
#### Changing data types  
  
  str(df)

## Change to dychotomous numerics: Attrition, Gender, OverTime,   
  
  df$Attrition = ifelse(df$Attrition == "Yes", 1, 0) # 1 = "Yes"
  
  df$Gender = ifelse(df$Gender == "Male", 1, 0) # 1 = "Male"
  
  df$OverTime = ifelse(df$OverTime == "Yes", 1, 0) # 1 = "Yes"

  
## Change to ordered factors: BusinessTravel, Education, EnviromentSatisfaction, JobInvolvement, 
#  JobLevel, JobSatisfaction, PerformanceRating, ReelationshipSatisfaction, WorkLifeBalance, 
  
  df$BusinessTravel = factor(df$BusinessTravel, 
                             levels = c("Non-Travel", "Travel_Rarely", "Travel_Frequently"),
                             ordered = TRUE)
  
# Quick transformation for every variable with 1-4 scale 
  
  scale1_4 = c("1", "2", "3", "4")
  vars_scale1_4 = c("EnvironmentSatisfaction", "JobInvolvement", "JobSatisfaction", "PerformanceRating",
                    "RelationshipSatisfaction", "WorkLifeBalance")
  
  df = df %>% 
    mutate(across(all_of(vars_scale1_4),
                  ~ factor(., levels = scale1_4, ordered = TRUE)))
  
# The same for variables with 1-5 scale
  
  scale1_5 = c("1", "2", "3", "4", "5")
  vars_scale1_5 = c("Education", "JobLevel")
  
  df = df %>%
    mutate(across(all_of(vars_scale1_5),
                  ~ factor(., levels = scale1_5, ordered = TRUE)))

## Variables that need be changed as factor: Department, EducationField, JobRole, MaritalStatus  
  
  vars_to_factor = c("Department", "EducationField", "JobRole", "MaritalStatus")
  
  df[vars_to_factor] = lapply(df[vars_to_factor], as.factor)
   
  rm(scale1_4,scale1_5, vars_scale1_4, vars_scale1_5, vars_to_factor)
  
# Also look at PerformanceRating, it takes only 2 values out of possible 4, I don't think 
# it will be helpful as everyone gets positive score, so I will omit this variable 
  
  unique(df$PerformanceRating)
  
  df = df %>% select(!PerformanceRating)

# Also.. there are 4 variables related to income, all of which represent different values 
# and don't really calculate; let's check correlation
  
  income_indicator = c("DailyRate", "HourlyRate", "MonthlyIncome", "MonthlyRate")
  cor(df[, income_indicator])
  
# Very low correlations, to make analysis tidier I will keep only MonthlyIncome, as it makes
# the most sense 
  
  df = df %>% select(!c("DailyRate", "HourlyRate", "MonthlyRate"))
  rm(income_indicator)
  
##Final check
  
  str(df)
  skim(df)
  
#====================================== DATA EXPLORATION ==================================================
    
library(ggplot2)
library(polycor)
library(corrplot)
  
### Descriptive Statistics of selected variables
  
  vars = c("Age", "Gender", "MonthlyIncome", "YearsAtCompany", "YearsSinceLastPromotion")
  
  stats_table <- df %>%
    select(all_of(vars)) %>%
    summarise(across(
      everything(),
      list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        IQR = ~IQR(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ))
  
  print(stats_table)
  
#### Heterogenic Correlation Matrix computes Pearson, Polychoric, Polyserial or Tetrachoric 
#### correlations between suitable types of variables, this ensures that no correlation is biased
  
  het_cor = hetcor(df) 
  
# To visualize correlations with Attrition, I save results and store them as data frame
  
  corr_mat = het_cor$correlations 
  
  cor_attrition = data.frame(
    Variable = colnames(corr_mat),
    Correlation = corr_mat["Attrition", ]
  )
  
# dropping correlation of Attrition with itself   
  
  cor_attrition = cor_attrition[cor_attrition$Variable != "Attrition", ]
  
# dropping correlations between -0.1 and 0.1 for better visibility 
  
  cor_attrition = cor_attrition[abs(cor_attrition$Correlation) > 0.1, ]

# sorting correlation in descending order
  
  cor_attrition = cor_attrition[order(-cor_attrition$Correlation), ]  
  
# visualization of correlation using ggplot 
  
  ggplot(cor_attrition, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation > 0)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("#95B8D9", "#FED966"), guide = FALSE) +
    coord_flip() +
    labs(title = "Correlation with Attrition",
         x = " ",
         y = "Correlation") +
    theme_classic() +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=10)),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.text.y = element_text(size = 14, color = "black", hjust = 1),
      axis.text.x = element_text(size = 16, color = "black"),
      )
  
####check highest correlations: OverTime, MaritalStatus, BusinessTravel, JobRole, DistanceFromHome etc. 
  
#OverTime is a dichotomous variable indicating whether the employee works overtime or not 

 ggplot(df, aes(factor(Attrition), fill = factor(OverTime))) +
    geom_bar(position = "fill", color = "black") +
    ylab("Proportion") +
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(name = " ",
                      values = c("0" = "white", "1" = "#FED966"),
                      labels = c("Didn't work overtime", "Worked overtime"))+
    labs(x = "Attrition",
         y = "Proportion", 
         title = "Proportion of overtime in attrition")+
   scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
   scale_y_continuous(expand = expansion(0)) +
   theme_classic() +
   theme(
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 14, color = "black"),
     axis.text.y = element_text(size = 12, color = "black"),
     legend.text = element_text(size = 12, color = "black"),
     legend.margin = margin(t=5, l=5, r=5, b=5)
   )  
    
#Marital Status with Attrition 
 
 ggplot(df, aes(x = factor(Attrition), fill = factor(MaritalStatus))) +
   geom_bar(position = "fill", color = "black") +
   ylab("Proporcja") +
   scale_y_continuous(labels = scales::percent)+
   scale_fill_manual(name = " ",
                     values = c("Divorced" = "white",
                                "Married" = "#FED966", 
                                "Single" = "#95B8D9"),
                     labels = c("Divorced", "Married", "Single"))+
   labs(x = "Attrition",
        y = "Proportion", 
        title = "Proportion of marital statuses in attrition")+
   scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
   scale_y_continuous(expand = expansion(0)) +
   theme_classic() +
   theme(
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 18, color = "black"),
     axis.text.y = element_text(size = 18, color = "black"),
     legend.text = element_text(size = 18, color = "black"),
     legend.margin = margin(t=2, l=2, r=2, b=2)
   )
 
#BusinessTravel with Attrition
 
 ggplot(df, aes(x = factor(Attrition), fill = factor(BusinessTravel))) +
   geom_bar(position = "fill", color = "black") +
   ylab("Proporcja") +
   scale_y_continuous(labels = scales::percent)+
   scale_fill_manual(name = " ",
                     values = c("Non-Travel" = "white",
                                "Travel_Rarely" = "#FED966", 
                                "Travel_Frequently" = "#95B8D9"),
                     labels = c("Non Travel", "Travel Rarely", "Travel Frequently"))+
   labs(x = "Attrition",
        y = "Proportion", 
        title = "Proportion of business travels in attrition")+
   scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
   scale_y_continuous(expand = expansion(0)) +
   theme_classic() +
   theme(
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 18, color = "black"),
     axis.text.y = element_text(size = 18, color = "black"),
     legend.text = element_text(size = 18, color = "black"),
     legend.margin = margin(t=2, l=2, r=2, b=2)
   )
 
#DistanceFromHome with Attrition
 
 ggplot(df, aes(factor(Attrition), DistanceFromHome, fill = factor(Attrition))) +
   geom_boxplot(color = "black") +
   scale_fill_manual(values = c("0" = "white", "1" = "#FED966")) +
   labs(
     x = "Attrition",
     y = "Distance from home (km)",
     title = "Distribution of Distance from Home in Attrition"
   ) +
   scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
   theme_classic() +
   theme(
     legend.position = "none",
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 14, color = "black"),
     axis.text.y = element_text(size = 12, color = "black"),
     legend.text = element_text(size = 12, color = "black"),
     legend.margin = margin(t=5, l=5, r=5, b=5)
   )
 
# Age with Attrition
 
 ggplot(df, aes(factor(Attrition), Age, fill = factor(Attrition))) +
   geom_boxplot(color = "black") +
   scale_fill_manual(values = c("0" = "white", "1" = "#FED966")) +
   labs(
     x = "Attrition",
     y = "Age",
     title = "Distribution of Age in Attrition"
   ) +
   scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left")) +
   theme_classic() +
   theme(
     legend.position = "none",
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 22, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 18, color = "black"),
     axis.text.y = element_text(size = 18, color = "black")
   )
 
 
#Histogram of DistanceFromHome with kernel density function
 
 ggplot(df, aes(DistanceFromHome)) +
   geom_histogram(aes(y = ..density..), 
                  color = "black", 
                  fill = "#FED966", 
                  bins = 29) +
   geom_density(color = "#2C7BB6", size=1, adjust = 1.1, kernel = c("epanechnikov")) +
   labs(
     x = "Distance from home",
     y = "Density",
     title = "Histogram with kernel density function") +
   scale_y_continuous(expand = expansion(0)) +
   theme_classic() +
   theme(
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 14, color = "black"),
     axis.text.y = element_text(size = 12, color = "black")
   )
    
#Histogram of Age with kernel density 
 
 ggplot(df, aes(Age)) +
   geom_histogram(aes(y = ..density..), 
                  color = "black", 
                  fill = "#FED966", 
                  bins = 23) +
   geom_density(color = "#2C7BB6", size=1, adjust = 1.1, kernel = c("epanechnikov")) +
   labs(
     x = "Age",
     y = "Density",
     title = "Histogram with kernel density function") +
   scale_y_continuous(expand = expansion(0)) +
   theme_classic() +
   theme(
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.text = element_text(size = 14, color = "black"),
     axis.text.y = element_text(size = 12, color = "black")
   )
    
#Histogram of income 
 
 ggplot(df, aes(MonthlyIncome)) +
   geom_histogram(aes(y = ..density..), 
                  color = "black", 
                  fill = "#FED966", 
                  bins = 30) +
   geom_density(color = "#2C7BB6", size=1, adjust = 1.1, kernel = c("epanechnikov")) +
   labs(
     x = "Monthly Income",
     y = "Density",
     title = "Histogram of Monthly Income") +
   scale_y_continuous(expand = expansion(0)) +
   theme_classic() +
   theme(
     plot.margin = unit(c(1,1,1,1), "cm"),
     plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
     axis.line = element_line(color = "black"),
     axis.title = element_text(size = 20, color = "black", face = "bold"),
     axis.title.y = element_text(margin = margin(r = 12)),
     axis.title.x = element_text(margin = margin(t = 12)),
     axis.text = element_text(size = 14, color = "black"),
     axis.text.y = element_text(size = 12, color = "black")
   )
  
    
#Plot of dependent variable, Attrition
  
  ggplot(df, aes(factor(Attrition))) +
    geom_bar(fill = "#FED966", color = "black") +
    labs(x = "Attrition", y = "Frequency", title = "Frequency of Attrition") +
    scale_x_discrete(labels = c("0" = "Stayed", "1" = "Left the company")) +
    scale_y_continuous(expand = expansion(0)) +
    theme_classic() +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.title.y = element_text(size = 22, color = "black", face = "bold"),
      axis.line = element_line(color = "black")
    )  

# Cleaning objects
  
  rm(list = setdiff(ls(), "df"))

#======================================== CONDITIONAL RANDOM FOREST ===================================================
  
  library(party)
  require(flexplot)
  library(caret)
  library(varImp)
  library(pROC)
  

### Using CForest to further explore relationships in the data and construct hypotheses    

# New data frame with target variable marked as factor 
  
  d = df
  
  d$Attrition = factor(d$Attrition, levels = c("0", "1"))
  
  set.seed(123)
  
# Basic random forest, Attrition as dependent variable
  
  rf = cforest(Attrition ~., data=d)
  estimates_full = estimates(rf)
  estimates_full
  
# Variable importance suggest that strongest predictor is OverTime, and the importance drops quite
# drastically the further we go. Let's find unnecessary predictors and simplify the model. 
  
  flexplot(Attrition~YearsInCurrentRole, data = d) # replace YrsInCrrRole with different variables

# By evaluating flexplots, correlations and theory, I've decided to exclude following indicators:
 
  d_reduced = d %>% select(!c("YearsSinceLastPromotion", "Gender", "Education", 
                              "TrainingTimesLastYear", "RelationshipSatisfaction",
                              "NumCompaniesWorked", "PercentSalaryHike"))
  
# Checking reduced random forest
  
  rf_reduced = cforest(Attrition ~., data=d_reduced)
  estimates_reduced = estimates(rf_reduced)
  estimates_reduced  
  
# Improvement! Higher OOB and variable importances, which means that It could've been suppressed by noise. 
# Let's try to further simplify the model and reduce noise
  
  d_reduced_2 = d_reduced %>% select(!c("WorkLifeBalance", "YearsWithCurrManager", "JobInvolvement", 
                                      "EducationField", "EnvironmentSatisfaction", "YearsInCurrentRole"))
  
  
  rf_reduced_2 = cforest(Attrition ~., data=d_reduced_2)
  estimates_reduced_2 = estimates(rf_reduced_2)
  estimates_reduced_2

# Another improvement, Im going to evaluate prediction using confusion matrices;
# For this I need to save predicted categories from each model
  
  p_full <- predict(rf, newdata = d, type = "response")
  p_reduced <- predict(rf_reduced, newdata = d, type = "response")
  p_reduced_2 <- predict(rf_reduced_2, newdata = d, type = "response")
  
  confusionMatrix(p_full, d$Attrition, positive = "1")
  confusionMatrix(p_reduced, d$Attrition, positive = "1")
  confusionMatrix(p_reduced_2, d$Attrition, positive = "1")
  
# Classification improves with simplifying the model, I will try to continue this process
# Checking variable importance using varimpAUC which is more robust towards class imbalance
  
  varimpAUC(rf_reduced_2, conditional = TRUE)
  
# Most important variables: JobSatisfaction, OverTime
# Negligible or negative: Department, TotalWorkingYears

# Further Reducing:

  d_reduced_3 = d_reduced_2 %>% select(!c("Department", "TotalWorkingYears"))  
  
  rf_reduced_3 = cforest(Attrition ~., data = d_reduced_3)
  estimates_reduced_3 = estimates(rf_reduced_3)
  estimates_reduced_3
  
  p_reduced_3 <- predict(rf_reduced_3, newdata = d, type = "response")
  
  confusionMatrix(p_reduced_2, d$Attrition, positive = "1")
   
  varimp(rf_reduced_3, conditional = FALSE)
  
# Improvement in OOB, no differences in confusion matrix. I stay with this model.  
# As expected, model predicts too much negatives. We can find optimal threshold to maximise sensitivity:
  
  pred_class = predict(rf_reduced_3, newdata=d, type="prob")
  
  prob_positive <- sapply(pred_class, function(x) x[2])
  
  roc_obj <- roc(d$Attrition, prob_positive)
  auc(roc_obj)
  plot(roc_obj)
  
# AUC = 0.94, quite good. Finding best threshold, adding weights
  
  coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  
# best threshold = 0.159, printing new confusion matrix
  
  pred_class = ifelse(prob_positive > 0.159, 1, 0)
  table(Predictes = pred_class, Actual = d$Attrition)
  
# Best classification, model correctly predicts 96% of employees who left company on test data, though at the cost 
# of misclassifying workers who stayed. I am okay with that, as it's better in this particular situation. 
  
# Variable importance
  
  imp = varimpAUC(rf_reduced_3, conditional = TRUE) #MonthlyIncome with negative importance?

# Trying out model without MonthlyIncome
  
  d_reduced_4 = d_reduced_3 %>% select(!c("MonthlyIncome"))  
  
  rf_reduced_4 = cforest(Attrition ~., data = d_reduced_4)
  estimates_reduced_4 = estimates(rf_reduced_4)
  estimates_reduced_4
  
# Better OOB, checking the variable importance

  imp2 = varimpAUC(rf_reduced_4, conditional = TRUE) #No negative values, this would be the final model
  
  pred_class = predict(rf_reduced_4, newdata=d, type="prob")
  
  prob_positive <- sapply(pred_class, function(x) x[2])
  
  roc_obj <- roc(d$Attrition, prob_positive)
  auc(roc_obj)
  plot(roc_obj)
  
  # AUC = 0.937
  
  coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
  
  # best threshold = 0.15, printing new confusion matrix
  
  pred_class = ifelse(prob_positive > 0.1502298, 1, 0)
  pred_class <- factor(pred_class, levels = levels(d$Attrition))
  confusionMatrix(pred_class, d$Attrition, positive = "1")
  
# Plot of variable importance
  
  imp_df <- data.frame(
    Variable = names(imp2),
    Importance = as.numeric(imp2)
  )
  
  imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
  
  ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#FED966") +
    coord_flip() +
    labs(title = "Variable Conditional Importance",
         x = "",
         y = "Importance (ΔAUC)") +
    theme_classic() +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.title.y = element_text(size = 22, color = "black", face = "bold"),
      axis.line = element_line(color = "black")
    )  
  
# Plot of confusion matrix   
  
  cm_df <- as.data.frame(table(Prediction = pred_class, Reference = d$Attrition))

  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "#FED966") +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted") +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.title.y = element_text(size = 22, color = "black", face = "bold", margin = margin(r = 15)),
      axis.line = element_line(color = "black")
    )  
  
# Cleaning, creating dataset with variables selected from cforest
  
  rm(list = setdiff(ls(), "df"))
  
  df1 = df %>% select(!c("YearsSinceLastPromotion", "Gender", "Education", 
                       "TrainingTimesLastYear", "RelationshipSatisfaction",
                       "NumCompaniesWorked", "PercentSalaryHike",
                       "WorkLifeBalance", "YearsWithCurrManager", "JobInvolvement", 
                       "EducationField", "EnvironmentSatisfaction", "YearsInCurrentRole",
                       "Department", "TotalWorkingYears", "MonthlyIncome"))
  
#============================================= Extreme Gradient Boosting=======================================================================================
  

  
####XGBoost
  
library(xgboost)
library(caret)
library(MLmetrics)
  
#Data preparation
  
  
  ### One-hot encoding
  
  # Switching ordered factors to factors 
  
  vars_to_factor = c("BusinessTravel", "EnvironmentSatisfaction",
                     "JobInvolvement", "JobLevel", "JobSatisfaction", 
                      "WorkLifeBalance")
  
  df[vars_to_factor] = lapply(df[vars_to_factor], as.numeric)
  df[vars_to_factor] = lapply(df[vars_to_factor], as.factor)
  
  rm(vars_to_factor)
  
  df_onehot <- model.matrix(~ . -1, data = df)
  df_onehot <- as.data.frame(df_onehot)
  
# Training and testing dataset
  
  set.seed(123)

  train_index = createDataPartition(df_onehot$Attrition, p = 0.8, list = FALSE)
  
  train = df_onehot[train_index, ]
  test = df_onehot[-train_index, ] 
  
  rm(train_index)
  
  #Labels 
  
  train_label = train$Attrition
  test_label = test$Attrition
  
  #removing Attrition as it is dependent variable
  
  train$Attrition = NULL
  test$Attrition = NULL
  
  #Transforming to data matrix
  
  train = data.matrix(train)
  test = data.matrix(test)
  
  #transforming to DMatrix 
  
  dtrain = xgb.DMatrix(data = train, label = train_label)
  dtest = xgb.DMatrix(data = test, label = test_label)
  
##Training the model
# Setting early stopping rounds, so that after 10 rounds of no improvement the process will stop. 
# Also, AUC PR as evaluation metric will be the best since the data is imbalanced 
  
  model = xgboost(data = dtrain,
                  objective = "binary:logistic",
                  nrounds = 500,
                  early_stopping_rounds = 10,
                  params = list(eval_metric="aucpr"))
  
# Checking model quality 
  
  pred = predict(model, dtest)
  
  err = mean(as.numeric(pred > 0.5) !=test_label)
  print(paste("test error=", err))
  
# ~12% error rate, but let's check confusion matrix:
  
  table(Predicted = as.numeric(pred > 0.5), Actual = test_label)
  
# Again, missclassifying positive cases 
  
  roc(test_label, pred) %>% auc() 
  
# Variable importance
  
  xgb.importance(model = model)
  
# Different output compared to cforest, on top monthly income, age and overtime
  
####Model improvements
  
# Using caret::train() to find most optimal hyperparameters; because by default caret is using accuracy
# as evaluation metric, I change to AUCPR - recommended for imbalanced data. This requires some work with the data 

  grid = expand.grid(
    nrounds = c(50, 100, 200),            
    max_depth = c(3, 6, 9),
    eta = c(0.01, 0.1, 0.3),
    gamma = c(0, 0.5, 1),
    colsample_bytree = c(0.5, 0.7, 1),
    min_child_weight = c(1, 3, 5),
    subsample = 0.5                     # sampling 50% dataset for training 
  )
  
  df_fit = df_onehot
  
  df_fit$Attrition = factor(df_fit$Attrition, levels = c("0", "1"), labels = c("Neg", "Pos"))
  
  x = df_fit[, setdiff(names(df_fit), "Attrition")]
  y = df_fit$Attrition
  
  fit = train(
    x = x,
    y = y,
    method = "xgbTree",
    metric = "AUC",
    trControl = trainControl(method = "cv", number = 5,
                             classProbs = TRUE, summaryFunction = prSummary,
                             savePredictions = "final", verboseIter = TRUE),
    tuneGrid = grid
  )
  
  fit
  
# Results: The final values used for the model were nrounds = 200, max_depth = 3,
#  eta = 0.1, gamma = 0.5, colsample_bytree = 0.7, min_child_weight = 3 
  
# In final model, I will slightly increase lambda (1.5) and scale_pos_weight to prevent overfitting; 
# also nrounds were on the end of scale, so I increase value and add early stopping
  
# ---------------------- MODEL 2 --------------------------------------------------------------------------------------------------------- 
  
  params = list(
    objective = "binary:logistic", 
    eta = 0.1, 
    max_depth= 3,
    gamma = 0.5,
    colsample_bytree = 0.7,
    min_child_weight = 3,
    lambda = 1.5,
    scale_pos_weight = 5,
    eval_metric="aucpr")
  
  model_2 = xgboost(data = dtrain,
                    nround = 300,
                    early_stopping_rounds = 10,
                    params = params 
                    )
  
### Model evaluation 

## Model fit on test data 
  
  pred2 = predict(model_2, dtest)
  
  err2 = mean(as.numeric(pred2 > 0.5) !=test_label)
  print(paste("test error=", err2))
  
  table(Predicted = as.numeric(pred2 > 0.5), Actual = test_label)
  
  roc_obj <- roc(test_label, pred2)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  table(Predicted = as.numeric(pred2 > 0.1688462), Actual = test_label)
  
# correct positive prediction = 87%, test error = 15%, area under the curve = 85,2% 
  
##  Model fit on train data
  
  pred2 = predict(model_2, dtrain)
  
  err2 = mean(as.numeric(pred2 > 0.5) !=train_label)
  print(paste("test error=", err2))
  
  table(Predicted = as.numeric(pred2 > 0.5), Actual = train_label)
  
  roc_obj <- roc(train_label, pred2)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  table(Predicted = as.numeric(pred2 > 0.5601117), Actual = train_label)
  
# correct positive prediction = 100%, test error = 0.012, area under the curve = 99.9%
# Model overfitting
  
# What about variables importance?
  
  xgb.importance(model = model_2)
  
# Checking effects of top5 predictors on attrition
  
  flexplot(Attrition~MonthlyIncome, data = df) # Increased attrition in low income
  flexplot(Attrition~Age, data = df) # Increased attrition in young age
  flexplot(Attrition~OverTime, data = df) # Increased attrition in positive overtime group
  flexplot(Attrition~StockOptionLevel, data = df) # small effect
  flexplot(Attrition~YearsAtCompany, data = df) # Some bias on right end from outliers
  
# what about least important variables?
  
  flexplot(Attrition~EducationFieldOther, data = df_fit)
  flexplot(Attrition~JobRoleManager, data = df_fit)
  
# seems like there is still some effect even in lowest important variables
  
# ---------------------------------- MODEL 3 ---------------------------------------------------------------------------------------
  
  
  params = list(
    objective = "binary:logistic", 
    eta = 0.05, 
    max_depth= 3,
    gamma = 1,
    colsample_bytree = 0.5,
    min_child_weight = 5,
    lambda = 2,
    scale_pos_weight = 5,
    subsample = 0.7,
    eval_metric="aucpr")
  
  model_3 = xgboost(data = dtrain,
                    nround = 300,
                    early_stopping_rounds = 10,
                    params = params 
  )
  
### Evaluation
  
## Model fit on test data 
  
  pred3 = predict(model_3, dtest)
  
  roc_obj <- roc(test_label, pred3)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err3 = mean(as.numeric(pred3 > 0.5) !=test_label)
  print(paste("test error=", err3))
  
  table(Predicted = as.numeric(pred3 > 0.3097623), Actual = test_label)
  
# correct positive prediction = 83%, test error = 0.17, area under the curve = 85,3% 
  
##  Model fit on train data
  
  pred3 = predict(model_3, dtrain)
  
  roc_obj <- roc(train_label, pred3)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err3 = mean(as.numeric(pred3 > 0.5) !=train_label)
  print(paste("test error=", err3))
  
  table(Predicted = as.numeric(pred3 > 0.5156039), Actual = train_label)
  
# correct positive prediction = 95,7%, test error = 0.06, area under the curve = 98.7%

# My guess it to further increase gamma parameter, to make the algorithm more conservative;
# model seems to still over fit 
  
#--------------------------------- MODEL 4 --------------------------------------------------------------------------------------------
  
  params = list(
    objective = "binary:logistic", 
    eta = 0.05, 
    max_depth= 3,
    gamma = 1.5,
    colsample_bytree = 0.5,
    min_child_weight = 7,
    lambda = 3,
    scale_pos_weight = 5,
    subsample = 0.5,
    eval_metric="aucpr")
  
  model_4 = xgboost(data = dtrain,
                    nround = 300,
                    early_stopping_rounds = 10,
                    params = params 
  )

### Evaluation
  
## Model fit on test data 
  
  pred4 = predict(model_4, dtest)
  
  roc_obj <- roc(test_label, pred4)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err4 = mean(as.numeric(pred4 > 0.5) !=test_label)
  print(paste("test error=", err4))
  
  table(Predicted = as.numeric(pred4 > 0.397284), Actual = test_label)
  
# correct positive prediction = 79%, test error = 0.149, area under the curve = 88,6% 
  
##  Model fit on train data
  
  pred4 = predict(model_4, dtrain)
  
  roc_obj <- roc(train_label, pred4)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err4 = mean(as.numeric(pred4 > 0.5) !=train_label)
  print(paste("test error=", err4))
  
  table(Predicted = as.numeric(pred4 > 0.4992592), Actual = train_label)
  
# correct positive prediction = 92%, test error = 0.08, area under the curve = 97.7%
# still small overfit
  
# ---------------------------- MODEL 5 ---------------------------------------------------------------------------------------
  
params = list(
    objective = "binary:logistic", 
    eta = 0.02, 
    max_depth= 3,
    gamma = 2,
    colsample_bytree = 0.5,
    min_child_weight = 7,
    lambda = 3.5,
    scale_pos_weight = 5,
    subsample = 0.5,
    eval_metric="aucpr")
  
  model_5 = xgboost(data = dtrain,
                    nround = 300,
                    early_stopping_rounds = 10,
                    params = params 
  )

### Evaluation
  
## Model fit on test data 
  
  pred5 = predict(model_5, dtest)
  
  roc_obj <- roc(test_label, pred5)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err5 = mean(as.numeric(pred5 > 0.5) !=test_label)
  print(paste("test error=", err5))
  
  table(Predicted = as.numeric(pred5 > 0.3256494), Actual = test_label)
  
# correct positive prediction = 93,7%, test error = 0.19, area under the curve = 86,3% 
  
##  Model fit on train data
  
  pred5 = predict(model_5, dtrain)
  
  roc_obj <- roc(train_label, pred5)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err5 = mean(as.numeric(pred5 > 0.5) !=train_label)
  print(paste("test error=", err5))
  
  table(Predicted = as.numeric(pred5 > 0.4694551), Actual = train_label)
  
# correct positive prediction = 86%, test error = 0.132, area under the curve = 93.3%  
# I will stick with this model
  
# variable importance
  
  xgb.importance(model = model_5)
  
  flexplot(Attrition~MonthlyIncome | OverTime + Age, data = df) # Mostly young and overworked employees leave company 
  
# results are different from cforest and will need some thinking for logistic regression
  
# -------------------------- FINAL MODEL (5) FULL RESULTS --------------------------------------------------------------------------- 

# Data preparation  
  
  full_label = df_onehot$Attrition
  
  df_onehot$Attrition = NULL
  
  full = data.matrix(df_onehot)
  
  dfull = xgb.DMatrix(data = full, label = full_label)
  
# Model estimation
  
  model_final = xgboost(data = dfull,
                    nround = 300,
                    early_stopping_rounds = 10,
                    params = params 
  )
  
  
# Predictions  
  
  pred_final = predict(model_final, dfull)
  
  roc_obj <- roc(full_label, pred_final)
  coords(roc_obj, "best", ret = c("threshold", "specificity", "sensitivity"), transpose = FALSE)
  
  err_final = mean(as.numeric(pred_final > 0.5) !=full_label)
  print(paste("test error=", err_final))
  
  table(Predicted = as.numeric(pred_final > 0.4973955), Actual = full_label)
  
# Results: correct positive prediction = 83,5%, test error = 0.12, area under the curve = 93,2% 
  
# Specific confusion matrix and visualization: 
    
  pred_label = ifelse(pred_final  > 0.4973955, 1, 0)
  
  confusionMatrix(
    factor(pred_label, levels = c(0,1)),
    factor(full_label, levels = c(0,1)),
    positive = "1"
  )
  
  cm_df <- as.data.frame(table(Prediction = pred_label, Reference = df$Attrition))
  
  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "#FED966") +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted") +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.title.y = element_text(size = 22, color = "black", face = "bold", margin = margin(r = 15)),
      axis.line = element_line(color = "black")
    )
  
# Variable importance and visualization:
  
  importance = xgb.importance(model = model_final)
  importance = importance[order(importance$Gain, decreasing = TRUE), ]
  importance = importance[1:10, ]
  
  ggplot(importance, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "#FED966") +
    coord_flip() +
    labs(title = "Variable Importance",
         x = "",
         y = "Gain") +
    theme_classic() +
    theme(
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.title.y = element_text(size = 22, color = "black", face = "bold"),
      axis.line = element_line(color = "black")
    )
  
  
  rm(list = setdiff(ls(), "df"))
  
#================================================================================================================================================
  
##### Logistic Regression 
  
# From previous analysis we know that most of attrition comes from young, overworked employees
# Depvar: Attrition
# Indepvars: OverTime, MonthlyIncome, JobSatisfaction, MaritalStatus, Age, DistanceFromHome, TotalWorkingYears, BusinessTravel
  
  
  # Setting reference categories 
  
  df$MaritalStatus = relevel(df$MaritalStatus, ref = "Single")
  df$BusinessTravel = relevel(df$BusinessTravel, ref = 2)
  df$JobSatisfaction = relevel(df$JobSatisfaction, ref = 1)
  
  # Making sure indicators are well selected
  
  hetcor(df[, c("OverTime", "MonthlyIncome", "JobSatisfaction", "MaritalStatus", "Age", 
                "DistanceFromHome", "TotalWorkingYears", "BusinessTravel")])
  
  # Only Age with Income correlated , but that's interaction
  
  # Checking for possible interaction using flexplot
  # eg. 
  
  flexplot(Attrition~MonthlyIncome | Age, data=df, method="logistic")
  flexplot(Attrition~Age | OverTime + MonthlyIncome, data=df, method="logistic")
  flexplot(Attrition~Age | OverTime, data = df, method="logistic")
  flexplot(Attrition~Age, data=df, method="logistic")
  
  #models
  
  naive = lm(Attrition~OverTime + MonthlyIncome + Age, data = df)
  
  partial_residual_plot(Attrition~Age | MonthlyIncome + OverTime,
                        model = naive,
                        data = df,
                        method = "quadratic")
  
  full = lm(Attrition~OverTime + MonthlyIncome + Age + I(MonthlyIncome^2) + I(Age^2) + OverTime:I(MonthlyIncome^2) +
              I(MonthlyIncome^2):I(Age^2), data = df)
  
  partial_residual_plot(Attrition~Age | MonthlyIncome + OverTime,
                        model = full,
                        data = df,
                        method = "quadratic")
  
  # comparing
  
  compare.fits(Attrition~Age | MonthlyIncome + OverTime, data = df, full, naive)
  
  model.comparison(full, naive)  
  
  # Full model has better fit
  
  #Trying different one, with threeway interaction wihout quadratic effects
  
  full2 = lm(Attrition~OverTime*MonthlyIncome*Age, data = df)
  
  partial_residual_plot(Attrition~Age | MonthlyIncome + OverTime,
                        model = full2,
                        data = df,
                        method = "quadratic")
  
  compare.fits(Attrition~Age | MonthlyIncome + OverTime, data = df, full, full2)
  
  model.comparison(full, full2)  
  
  # Better fit
  
  full3 = lm(Attrition~OverTime*MonthlyIncome*Age + I(MonthlyIncome^2) + I(Age^2), data = df)
  
  p = partial_residual_plot(Attrition~Age | MonthlyIncome + OverTime,
                        model = full3,
                        data = df,
                        method = "quadratic")
  
  compare.fits(Attrition~Age | MonthlyIncome + OverTime, data = df,full2, full3)
  
  model.comparison(full2, full3)
  
  # Better fit, adding rest of variables
  
  full4 = lm(Attrition~OverTime*MonthlyIncome*Age + I(MonthlyIncome^2) + I(Age^2) +
               MaritalStatus + DistanceFromHome + JobSatisfaction + TotalWorkingYears
             , data = df)
  
  model.comparison(naive, full4)
  
  # Better fit
  
  #Final plots and results
  
  probs <- predict(full4, type = "response")
  true = df$Attrition
  
  library(cutpointr)
  
  results = cutpointr(df, probs, true, method = maximize_metric,
                      metric = youden)
  results$optimal_cutpoint
  
  #cutpoint 0.1944212 
  
  pred = ifelse(probs >= 0.1944212, 1, 0)
   
  confusionMatrix(
    factor(pred, levels = c(0,1)),
    factor(true, levels = c(0,1)),
    positive = "1"
  )
  
  cm_df <- as.data.frame(table(Prediction = pred, Reference = df$Attrition))
  
  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), vjust = 1, size = 8) +
    scale_fill_gradient(low = "white", high = "#FED966") +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted") +
    theme_classic() +
    theme(
      legend.position = "none",
      plot.margin = unit(c(1,1,1,1), "cm"),
      plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
      axis.text.x = element_text(size = 18, color = "black"),
      axis.text.y = element_text(size = 18, color = "black"),
      axis.title.x = element_text(size = 22, color = "black", face = "bold"),
      axis.title.y = element_text(size = 22, color = "black", face = "bold", margin = margin(r = 15)),
      axis.line = element_line(color = "black")
    )
  
  library(texreg)

  texreg(full4, booktabs = TRUE, dcolumn = TRUE, siunitx = TRUE, stars = c(0.01, 0.05, 0.1),
            file = "C:/Users/cwiek/Desktop/ANALIZY/Employee Attrition/model_table.tex")
  
  
  
  
  
  
  
  
  
  p + labs(title = "Logistic Regression Lines") +
         theme(
             legend.position = "none",
             plot.margin = unit(c(1,1,1,1), "cm"),
             plot.title = element_text(size=22, face ="bold", hjust = 0.5, margin = margin(b=15)),
             axis.text.x = element_text(size = 14, color = "black"),
             axis.text.y = element_text(size = 14, color = "black"),
             axis.title.x = element_text(size = 18, color = "black", face = "bold"),
             axis.title.y = element_text(size = 18, color = "black", face = "bold"),
             axis.line = element_line(color = "black")
           )
  
  
  
  
  
  
  
  
  
  