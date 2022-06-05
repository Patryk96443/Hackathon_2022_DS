library(tidyverse)
library(rio)
library(chron)
library(stringr)

train <- read_csv("data/train.csv")
cities <- read_csv("/home/patryk/Documents/DS/cities.csv")
districts <- read_csv("/home/patryk/Documents/DS/districts.csv")
test <- read_csv("/home/patryk/Documents/DS/test.csv")

train1 <- merge(train, districts, by.x=c("district_id"), by.y=c("id"))

str1 <- train$created_at_first
train <- train %>%
  mutate(created_at_first = as.Date(str_sub(str1, start=1, end=10)),
         created_at_first_hour = chron(times = str_sub(str1, start=11, end=19))
  )

saveRDS(train,file = "dane.rds")

skimr::skim(train)

subset(train$price,train$price<2000000) %>% 
  hist(breaks = 25)

subset(train$price,train$price>25000000&train$price<max(train$price)) %>% 
  hist(breaks = 25)

train$category <- as.factor(train$category)
train$is_business <- as.factor(train$is_business)

library(rlist)

paramsl <- train$params %>%
  str_extract_all(.,pattern="br>.*?<=") %>%
  rlist::list.ungroup() %>%
  substr(.,4,nchar(.)-2) %>%
  str_remove("<br>") %>%
  unique()

paramsl[1]

parametr <- paste0(paramsl[1],'<=>')

set.seed(2022)
train_test <- train[sample(1:length(train$id),100000,replace = F),]

paramsl <- paramsl[! paramsl %in% l]

for (i in 1:length(paramsl)){
  train_test <- cbind(train_test,NA)
  colnames(train_test)[which(colnames(train_test)=="NA")] <- as.character(paramsl[i])
    train_test[,ncol(train_test)] <- ifelse(str_detect(train_test$params,paste0(paramsl[i],'<=>')),
           str_extract(train_test$params,paste0('(?<=',paramsl[i],'<=>)[^<]+')),
           NA)
}

saveRDS(train_test,file = "dane_prob.rds")

colnames(train_test)[which(colnames(train_test)=="NA")] <- as.character(paramsl[i])

ifelse(str_detect(train[1:10,]$params,paste0(paramsl[3],'<=>')),
       str_extract(train[1:10,]$params,paste0('(?<=',paramsl[3],'<=>)[^<]+')),
       0)

summary(train_test)
l <- c('building_material','building_floors_num','roof_type',
       'equipment_types','roofing','recreational','access_types',
       'heating_types','fence_types','garret_type','building_ownership',
       'rent','vicinity_types','windows_type','rent[currency]', 'use_types', 
       'dimensions', 'fence', 'deposit', 'deposit[currency]', 'rent_to_students')

colnames(train_test)

vek <- c("m","rooms_num","terrain_area","build_year")
train_test[vek] <- sapply(train_test[vek],as.numeric)

train_test <- subset(train_test,train_test$m!=max(train_test$m))
train_test <- subset(train_test,train_test$m<1500)

boxplot(train_test$m)

corr <- rstatix::cor_mat(subset(train_test[c(vek,"price")]))
corr_pval <- rstatix::cor_pmat(subset(train_test[c(vek,"price")]))
ggcorrplot::ggcorrplot(corr,p.mat=corr_pval,lab=T)

train_test$market <- as.factor(train_test$market)
train_test$is_bungalow <- as.factor(train_test$is_bungalow)
train_test$heating <- as.factor(train_test$heating)
train_test$location <- as.factor(train_test$location)
train_test$building_type <- as.factor(train_test$building_type)
train_test$construction_status <- as.factor(train_test$construction_status)


mod_step <- step(lm(price~1, data = train_test), 
                 scope = ~m+rooms_num+is_business+city_id+market+is_bungalow+
                   location+building_type+construction_status,
                 direction = "forward", test = "F")

train_test <- test

paramsl <- c("m","market","building_type","rooms_num")

for (i in 1:length(paramsl)){
  train_test <- cbind(train_test,NA)
  colnames(train_test)[which(colnames(train_test)=="NA")] <- as.character(paramsl[i])
  train_test[,ncol(train_test)] <- ifelse(str_detect(train_test$params,paste0(paramsl[i],'<=>')),
                                          str_extract(train_test$params,paste0('(?<=',paramsl[i],'<=>)[^<]+')),
                                          NA)
}

saveRDS(train_test, file = "dane_testowe.rds")
