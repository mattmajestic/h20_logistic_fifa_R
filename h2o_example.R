
## Use h2o ####

library(h2o)
h2o.init()
fifa <- read.csv("C:/Users/Documents/fifa_19.csv")
fifa$Overall_binary <- ifelse(fifa$Overall > 75,1,0)

## Run Model for Gaussian of predicting overall player rating by attribute ratings ####

##### This function runs the binomial regression and gives a confusion matrix #######

logit_h2o_fun <- function(fifa_dat){

fifa_data <- as.h2o(fifa,destination_frame = "fifa.hex")
fifa.split = h2o.splitFrame(data = fifa_data, ratios = 0.8)
fifa.train = fifa.split[[1]]
fifa.test = fifa.split[[2]]
response_binary <- "Overall_binary"
feature.cols <- names(fifa)[15:48]

fit.logistic.fifa <- h2o.glm(x = feature.cols, 
                       y = response_binary, 
                       training_frame = fifa.train, 
                       family = "binomial",
                       model_id = "logistic")

pred.logistic.fifa <- h2o.predict(object = fit.logistic.fifa,
                            newdata = fifa.test)

logistic_confusion_matrix <- h2o.confusionMatrix(fit.logistic.fifa)
logistic_confusion_matrix_df <- data.frame(logistic_confusion_matrix)
return(logistic_confusion_matrix_df)

}

#### This is an lapply to run it across positions as different positions value attributes more ####

logit_group_h2o <- function(fifa){
  
  fifa_list <- split(fifa,fifa$Position)
  fifa_matrix_list <- lapply(fifa_list,logit_h2o_fun)
  fifa_confusion_df <- do.call(rbind.data.frame, fifa_matrix_list)
  fifa_confusion_df$ByPosition <- rownames(fifa_confusion_df)
  return(fifa_confusion_df)
}

#### This returns the confusion maatrix per position ####

all_positions_confusion_matrix <- logit_group_h2o(fifa)
View(all_positions_confusion_matrix)

## Voila you have models per player positions in the fifa data ####
















fifa_data <- as.h2o(fifa,destination_frame = "fifa.hex")
h2o.describe(fifa_data)
str(fifa)
## Prepare Data ####
fifa.split = h2o.splitFrame(data = fifa_data, ratios = 0.8)
fifa.train = fifa.split[[1]]
fifa.test = fifa.split[[2]]

response_gaussian <- "Overall"
response_binary <- "Overall_binary"

feature.cols <- names(fifa)[15:48]

## Run Model for Gaussian of predicting overall player rating by attribute ratings ####
fit.glm.fifa <- h2o.glm(x = feature.cols, 
                        y = response_gaussian, 
                        training_frame = fifa.train, 
                        family = "gaussian", 
                        model_id = "glm.fit")

pred.glm.fifa <- h2o.predict(object = fit.glm.fifa,
                             newdata = fifa.test)

## Deep Learning Models ####
fit.deep.fifa <- h2o.deeplearning(x = feature.cols, 
                                  y = response_binary,
                                  training_frame = fifa.train, 
                                  model_id = "deeplearning.fit")  

pred.deep.fifa <- h2o.predict(object = fit.deep.fifa,
                              newdata = fifa.test)




## DBPlyr use 

library(dplyr)

table_a <- data.frame('id' = c(1:3),
                      'fruit' = c('pineapple','apple','pear'))
table_b <- data.frame('id' = c(2:4),
                      'money' = c(9:11))

inner_table <- table_a %>% inner_join(table_b) %>% group_by(id) %>% mutate(money_2 = money/2)
inner_table

left_table <- table_a %>% left_join(table_b)
left_table

right_table <- table_a %>% right_join(table_b)
right_table

left_table %>% show_query()

translate_sql(table_a %>% inner_join(table_b) %>% filter(money > 5))


summary <- mtcars %>% 
  group_by(cyl) %>% 
  summarise(mpg = mean(mpg, na.rm = TRUE)) %>% 
  arrange(desc(mpg))

summary %>% show_query()
summary %>% collect()

library(sparklyr)
sc <- spark_connect(master = 'local')
