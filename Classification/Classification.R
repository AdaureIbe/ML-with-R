if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("caret","partykit","ROCR","lift","visdat","e1071","glmnet",
               "MASS","randomForest","xgboost", "keras", "dplyr", "Hmisc",
               "readxl","tidyverse","writexl","Hmisc","patchwork", "kknn",
               "ggplot2","ctv","skimr", "tidymodels", "mlbench","doParallel","pROC",
               "splines","rpart", "tune", "workflows","parsnip","GGally","vip","probably")

setwd("Documents/MMA/867 - Predictive Modelling/Assignment3/")

df01 <- read_excel("MMA867 A3 -- credit data.xlsx", 
                   sheet = "credit data", col_names = TRUE)
df02 <- read_excel("MMA867 A3 -- new applications.xlsx",
                   sheet = "KP3 - new applications - with d", col_names = TRUE)

colnames(df02)[25] <- "default_0"
df99$default_0 <- as.factor(df99$default_0)
df01$data <- "train"
df02$data <- "test"

df03 <- rbind(df01,df02)


df03$SEX_NEW <- if_else(df03$SEX == 2,0,1)
df03$EDUCATION_SUR <- if_else(df03$EDUCATION == 0, 1,0)
df03$EDUCATION_GRP <- if_else(df03$EDUCATION == 5 |df03$EDUCATION == 6|df03$EDUCATION == 0|df03$EDUCATION == 4,"Other",
                              if_else(df03$EDUCATION == 1, "Graduate", 
                                      if_else(df03$EDUCATION == 2, "Undergraduate", "HighSchool")))
df03$MARRIAGE_GRP <- if_else(df03$MARRIAGE == 1,"Married",if_else(df03$MARRIAGE == 2, "Single", if_else(df03$MARRIAGE == 3, "Divorce","Others")))
df03$AGE_GROUP <- if_else(df03$AGE < 25, "Under25", 
                          if_else(df03$AGE >= 25 & df03$AGE < 35, "25_34yrs",
                                  if_else(df03$AGE >= 35 & df03$AGE < 45, "35_44yrs",
                                          if_else(df03$AGE >=45 & df03$AGE < 55, "45_54yrs",
                                                  if_else(df03$AGE >=55 & df03$AGE < 65, "55_64yrs","64plus" )))))

df03$LIMIT_BAL_CAT <- if_else(df03$LIMIT_BAL <50000, "Lessthan50k",
                              if_else(df03$LIMIT_BAL  >=50000 & df03$LIMIT_BAL <100000, "50k_100k",
                                      if_else(df03$LIMIT_BAL  >=100000 & df03$LIMIT_BAL <250000, "100k_250k",
                                              if_else(df03$LIMIT_BAL  >=250000 & df03$LIMIT_BAL <500000, "250k_500k","500Kplus"))))


df03$PAY_NOCREDITL_LAST3 <- if_else(rowSums(df03[,7:9] == -2)>0,1,0)
df03$PAY_PAYFULL_LAST3 <- if_else(rowSums(df03[,7:9] == -1)>0,1,0)
df03$PAY_REVOLV_LAST3 <- if_else(rowSums(df03[,7:9] == 0)>0,1,0)
df03$PAY_DELAY1_LAST3 <- if_else(rowSums(df03[,7:9] == 1)>0,1,0)
df03$PAY_DELAY2_LAST3 <- if_else(rowSums(df03[,7:9] == 2)>0,1,0)
df03$PAY_DELAY3_LAST3 <- if_else(rowSums(df03[,7:9] == 3)>0,1,0)
df03$PAY_DELAY4Plus_LAST3 <- if_else(rowSums(df03[,7:9] >= 4)>0,1,0)



df03$PAY_1 <- df03$PAY_1+3
df03$PAY_2 <- df03$PAY_2+3
df03$PAY_3 <- df03$PAY_3+3
df03$PAY_4 <- df03$PAY_4+3
df03$PAY_5 <- df03$PAY_5+3
df03$PAY_6 <- df03$PAY_6+3

df03$PAY_MEAN <- floor(rowMeans(df03[,7:12],na.rm = TRUE))
df03$PAY_1_2MEAN <- floor(rowMeans(df03[,7:8],na.rm = TRUE))
df03$PAY_2_3MEAN <- floor(rowMeans(df03[,8:9],na.rm = TRUE))
df03$PAY_5_6MEAN <- floor(rowMeans(df03[,11:12],na.rm = TRUE))
df03$PAY_4_5MEAN <- floor(rowMeans(df03[,10:11],na.rm = TRUE))
df03$PAY_3_4MEAN <- floor(rowMeans(df03[,9:10],na.rm = TRUE))

df03$PAY_SUM <- apply(df03[,7:12],1,FUN = sum)
df03$PAY_MIN <- apply(df03[,7:12],1,FUN = min)
df03$PAY_MAX <- apply(df03[,7:12],1,FUN = max)
df03$PAY_SD <- floor(apply(df03[,7:12],1,FUN = sd))

df03$DIST_LIM1 <- round((df03$LIMIT_BAL - df03$BILL_AMT1)/df03$LIMIT_BAL,2)
df03$DIST_LIM2 <- round((df03$LIMIT_BAL - df03$BILL_AMT2)/df03$LIMIT_BAL,2)
df03$DIST_LIM3 <- round((df03$LIMIT_BAL - df03$BILL_AMT3)/df03$LIMIT_BAL,2)
df03$DIST_LIM4 <- round((df03$LIMIT_BAL - df03$BILL_AMT4)/df03$LIMIT_BAL,2)
df03$DIST_LIM5 <- round((df03$LIMIT_BAL - df03$BILL_AMT5)/df03$LIMIT_BAL,2)
df03$DIST_LIM6 <- round((df03$LIMIT_BAL - df03$BILL_AMT6)/df03$LIMIT_BAL,2)

df03$BILL_AMT_MEAN <- floor(rowMeans(df03[,13:18],na.rm = TRUE))
df03$BILL_AMT_SUM <- apply(df03[,13:18],1,FUN = sum)
df03$BILL_AMT_MIN <- apply(df03[,13:18],1,FUN = min)
df03$BILL_AMT_MAX <- apply(df03[,13:18],1,FUN = max)
df03$BILL_AMT_MEDIAN <- floor(apply(df03[,13:18],1,FUN = median))
df03$BILL_AMT_SD <- floor(apply(df03[,13:18],1,FUN = sd))

df03$PAY_AMT_MEAN <- floor(rowMeans(df03[,19:24],na.rm = TRUE))
df03$PAY_AMT_SUM <- apply(df03[,19:24],1,FUN = sum)
df03$PAY_AMT_MIN <- apply(df03[,19:24],1,FUN = min)
df03$PAY_AMT_MAX <- apply(df03[,19:24],1,FUN = max)
df03$PAY_AMT_MEDIAN <- floor(apply(df03[,19:24],1,FUN = median))
df03$PAY_AMT_SD <- floor(apply(df03[,19:24],1,FUN = sd))

df03[,7:12] <- lapply(df03[,7:12],factor) #PAY variables
df03[,"SEX_NEW"] <- lapply(df03[,"SEX_NEW"],factor)
df03[,"EDUCATION_SUR"] <- lapply(df03[,"EDUCATION_SUR"],factor)
df03[,"LIMIT_BAL_CAT"] <- lapply(df03[,"LIMIT_BAL_CAT"],factor)
df03[,"EDUCATION_GRP"] <- lapply(df03[,"EDUCATION"],factor)
df03[,"MARRIAGE_GRP"] <- lapply(df03[,"MARRIAGE"],factor)
df03[,"AGE_GROUP"] <- lapply(df03[,"AGE_GROUP"],factor)
#df03[,"PAY_CAT"] <- lapply(df03[,"PAY_CAT"],factor)
df03[,34:40] <- lapply(df03[,34:40],factor) #PAY_DELAY
df03$default_0 <- as.factor(df03$default_0)


dummy_var <- c("EDUCATION_GRP", "MARRIAGE_GRP", "AGE_GROUP","LIMIT_BAL_CAT",
               "PAY_1","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6")
nondummy_var <- setdiff(names(df03), dummy_var)
dummies <- dummyVars(~.,df03[dummy_var])
categorical_hot1 <- predict(dummies, df03[dummy_var])
df04 <- cbind(df03[nondummy_var], categorical_hot1)

df04 <- subset(df04, select = -c(AGE, MARRIAGE,LIMIT_BAL,EDUCATION,SEX))

train_df01 <- filter(df04,data == "train")
test_df01 <- filter(df04,data == "test")

train_df01 <- subset(train_df01, select = -data)
test_df01 <- subset(test_df01, select = -data)


####split traindata

set.seed(123)
data_split <- initial_split(train_df01, 
                            prop = 22001/24000,
                            strata = default_0)

training_df <- training(data_split)
testing_df <- testing(data_split)

set.seed(5)
test_split <- initial_split(testing_df, 
                            prop = 1001/2000,
                            strata = default_0)
validation_df <- training(test_split)
testing_df <- testing(test_split)

str(training_df)


###########################################
vis_dat(df03, warn_large_data = FALSE)

df03 %>% 
  ggplot(aes(x = MARRIAGE, y = LIMIT_BAL, 
             fill = default_0, color = MARRIAGE)) +
  geom_boxplot(alpha=0.4) 

df03 %>%
  ggplot(aes(default_0, AGE_GROUP)) +
  geom_bin2d() +
  scale_fill_continuous(type = "viridis")


###############################################

###recipe
credit_recipe2 <- 
  recipe(default_0~., data = training_df) %>%
  update_role(ID, new_role = "ID")

summary(credit_recipe2)  

#Cross validation
set.seed(100)
cv_folds <- vfold_cv(validation_df, v = 10, strata = default_0)

# Model specification
log_spec <- 
  logistic_reg() %>%
  set_engine(engine = "glm") %>%
  set_mode("classification")

rf_spec <- 
  rand_forest() %>%
  set_args(mtry = 6) %>%
  set_args(trees = 250) %>%
  set_args(min_n = 14) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

knn_spec <- 
  nearest_neighbor(neighbors = 4) %>%
  set_engine("kknn") %>% 
  set_mode("classification") 

decision_spec <-
  decision_tree() %>%
  set_mode("classification") %>% 
  set_engine("rpart")



floor(sqrt(ncol(training_df)))

######## Create Workflow

# Logistic Regression
log_wflow <- # new workflow object
  workflow() %>% # use workflow function
  add_recipe(credit_recipe2) %>%   # use the new recipe
  add_model(log_spec)   # add model spec

# Random Forest
rf_wflow <-
  workflow() %>%
  add_recipe(credit_recipe2) %>% 
  add_model(rf_spec)

# K-nearest neighbor
knn_wflow <-
  workflow() %>%
  add_recipe(credit_recipe2) %>% 
  add_model(knn_spec)

#Decision Tree
decision_wflow <- 
  workflow() %>%
  add_recipe(credit_recipe2) %>%
  add_model(decision_spec)

##Tune

rf_grid <- expand.grid(mtry = c(5:12), min_n = c(3:15)) # specify which values want to try

rf_tune_results <- rf_wflow %>% ## extract results
  tune_grid(resamples = cv_folds,
            grid = rf_grid,
            metrics = metric_set(accuracy,roc_auc))

# print results
rf_tune_results %>%
  collect_metrics()

credit_final <- rf_tune_results %>%
  select_best(metric = "accuracy")
credit_final

#Then we add this parameter to the workflow using the finalize_workflow() function.
rf_wflow <- rf_wflow %>%
  finalize_workflow(credit_final)

#####Evaluate Models
# Logistic Regression
get_model <- function(x) {
  pull_workflow_fit(x) %>% tidy()
}

log_res_2 <- 
  log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE,
      extract = get_model) # use extract and our new function
  ) 

#To get the results
log_res_2$.extracts[[1]][[1]]

#All of the results can be flattened and collected 
all_coef <- map_dfr(log_res_2$.extracts, ~ .x[[1]][[1]])

#Performance metrics
log_res_2 %>%  collect_metrics(summarize = TRUE)

#Collect predictions
log_pred <- 
  log_res_2 %>%
  collect_predictions()

#Confusion matrix
log_pred %>% 
  conf_mat(default_0, .pred_class) %>% 
  autoplot(type = "heatmap") #another type "heatmap"

#ROC-Curve
log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(default_0, .pred_0) %>% 
  autoplot()

#Plot predicted probability distribution
log_pred %>% 
  ggplot() +
  geom_density(aes(x = .pred_0, 
                   fill = default_0), 
               alpha = 0.5)


###RAndom Forest
rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

rf_res %>%  collect_metrics(summarize = TRUE)

##K nearest neighbor
knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

knn_res %>% collect_metrics(summarize = TRUE)

#Decision Tree
decision_res <- 
  decision_wflow %>%
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(
      recall, precision, f_meas,
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  )

decision_res %>% collect_metrics(summarize = TRUE)

###Compare Models
log_metrics <- 
  log_res_2 %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Logistic Regression") # add the name of the model to every row

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn")

decision_metrics <- 
  decision_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Decision Tree")

#create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  knn_metrics,
  decision_metrics
) 

# change data structure
model_comp <- 
  model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean F1-Score for every model
model_comp %>% 
  arrange(mean_f_meas) %>% 
  mutate(model = fct_reorder(model, mean_f_meas)) %>% # order results
  ggplot(aes(model, mean_f_meas, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_f_meas, 2), y = mean_f_meas + 0.08),
    vjust = 1
  )


# show mean area under the curve (auc) per model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>%
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(
    size = 3,
    aes(label = round(mean_roc_auc, 2), y = mean_roc_auc + 0.08),
    vjust = 1
  )


model_comp %>% slice_max(mean_f_meas)


#Last evaluation on test set

last_fit_rf <- last_fit(rf_wflow, 
                        split = test_split,
                        metrics = metric_set(
                          recall, precision, f_meas, 
                          accuracy, kap,
                          roc_auc, sens, spec)
)

last_fit_rf %>% 
  collect_metrics()

##very important variables
last_fit_rf %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10)

#confusion matrix:
last_fit_rf %>%
  collect_predictions() %>% 
  conf_mat(default_0, .pred_class) %>% 
  autoplot(type = "heatmap")

#ROC
last_fit_rf %>% 
  collect_predictions() %>% 
  roc_curve(default_0, .pred_0) %>% 
  autoplot()



#train final model on your full dataset and then use it to predict the response for new data
final_model <- fit(rf_wflow, train_df01)
final_model

saveRDS(final_model, file = "~/Documents/MMA/867 - Predictive Modelling/Assignment3/rf_model.rda")

final_prediction <- predict(final_model, new_data = test_df01, type = "prob")


###Finding predicitons: probabilities and classification
rf_classification<-rep("0",1000)
rf_classification[final_prediction[,2]>0.239]="1"
#rf_classification[final_prediction[,1]>0.75]="0" 
rf_classification<-as.factor(rf_classification)

#output
submit <- data.frame(rf_classification)
write_xlsx(submit,"newapp4.xlsx")



