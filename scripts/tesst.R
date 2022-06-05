train_test1 <- readRDS("dane_prob3.rds")
vek <- c("m","rooms_num","terrain_area","build_year")
train_test1[vek] <- sapply(train_test1[vek],as.numeric)
train_test <- subset(train_test1,train_test1$m<1500)
train_test$district_name <- as.factor(train_test$district_name)
train_test$city_name <- as.factor(train_test$city_name)
train_test$region_id <- as.factor(train_test$region_id)

vek <- c("m","rooms_num","terrain_area","build_year")
train_test[vek] <- sapply(train_test[vek],as.numeric)

data_subset <- train_test[ , c("m","is_business","city_name","district_name","market","building_type","construction_status","rooms_num","category","region_id")]
data_by_column <- train_test[complete.cases(data_subset), ]

pod_train <- data_by_column[1:000,]

library(stats)
library(tidymodels)

set.seed(9)

data_split <- initial_split(pod_train, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

rec <- recipe(price ~ category+is_business+m+rooms_num+market+building_type,
              data = train_data) %>% 
  step_normalize(all_numeric_predictors())
rf <- rand_forest(
  mtry = tune(),
  trees = tune(),
  min_n = tune()
) %>% 
  set_mode("regression") %>% 
  set_engine("ranger", importance = "impurity")

params <- extract_parameter_set_dials(rf)
params <- finalize(params, train_data)
rs <- vfold_cv(train_data, v = 5)
meas <- metric_set(rsq, rmse, mae)
grid <- grid_latin_hypercube(params, size = 5)
grid %>% 
  flextable::flextable()
rf_wf <- workflow() %>% 
  add_recipe(rec) %>% 
  add_model(rf)

#trenow
library(doParallel)
 registerDoParallel()
 rf_res <-
   rf_wf %>%
   tune_grid(
     resamples = rs,
     grid = grid,
     metrics = meas
 )
 rf_res %>% 
   collect_metrics() %>% 
   flextable::flextable() 
 
 rf_res %>% 
   show_best(metric = "rsq")

 rf_res %>% 
   show_best(metric = "rmse")

 rf_res %>% 
   show_best(metric = "mae")
 #rmse wsm
 
 #autoplot(rf_res)
 rf_best_param <- select_best(rf_res, metric = "rmse")
 rf_final <- 
   rf_wf %>% 
   finalize_workflow(rf_best_param) 
 rf_fit <- rf_final %>% 
   last_fit(data_split, metrics = meas)
 
 rf_fit %>% 
   collect_metrics()
 
 rf_fit %>% 
   collect_predictions() %>% 
   select(.pred, price) %>%
   head(n = 20) %>% 
   flextable::flextable()

 rf_fit %>% 
   collect_predictions() %>% 
   select(.pred, price) %>% 
   ggplot(aes(.pred, price)) +
   geom_jitter(width = 0, alpha = 0.05) +
   geom_smooth(method = lm, 
               se = F)
 
 library(vip)
 rf_fit %>% 
   extract_fit_parsnip() %>% 
   vip()

 predyk <- readRDS("dane_testowe.rds") 
 predyk$market <- as.factor(predyk$market)
 predyk$building_type <- as.factor(predyk$building_type)
 predyk$m <- as.numeric(predyk$m)
 predyk$rooms_num <- as.numeric(predyk$rooms_num)
 
 f <- predict(mod1, newdata = test_data)
 df <- as.data.frame(cbind(test_data$price,f)) %>% 
   cbind(.,test_data$price-f)
 
df[,3] <- abs(df[,3])
df[,3] %>% 
  hist()
 
 rf_fit %>% 
   collect_predictions() %>% 
   select(.pred, price) %>%
   head(n = 20) %>% 
   flextable::flextable()
 
 library(randomForest)

classifier <- randomForest(x=train_data[c("category", "is_business", "m", "rooms_num", "market"
                                                  , "building_type"),], 
                           y=train_data$price, ntree=500, random_state=0) 

rf <-randomForest(price~.,data=train_data, ntree=500) 

price <- predict(mod10, new_data = predyk)
perd_wyn <- cbind(predyk[1:20000,],price)
perd_wyn[,c("m","market","building_type","rooms_num","price")] %>% 
head() %>% 
  flextable::flextable()

relvspred <- as.data.frame(cbind(train_test$price[1:20000],perd_wyn$price[1:20000]))

library(kableExtra)
library(tidyverse)

options(knitr.kable.NA=0)
pods <- sub(".*:", "",summary(relvspred[,c("V1","V2")])) 
rownames(pods) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
colnames(pods) <- c("real","predicted")
pods%>% kable() %>% kable_styling(full_width = F, bootstrap_options  = "striped")

library(modelr)

predyk[,1:1000] %>% 
  add_predictions(rf_final)

xd <- train(price ~ ., data=na.omit(train_data), method="rf")
library(mlbench)
library(caret)

  
xd <- predict(rf_res)
