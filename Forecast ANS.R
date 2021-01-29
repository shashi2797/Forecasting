########## FOR COCACOLA DATA###################
library(forecast)
library(fpp)
library(smooth)
library(readxl)
Cocacola <- read_excel(file.choose())
View(Cocacola)
plot(Cocacola$Sales)
plot(Cocacola$Sales,type = "o")

Q1 <- ifelse(grepl("Q1",Cocacola$Quarter),"1","0")
Q2 <- ifelse(grepl("Q2",Cocacola$Quarter),"1","0")
Q3 <- ifelse(grepl("Q3",Cocacola$Quarter),"1","0")
Q4 <- ifelse(grepl("Q4",Cocacola$Quarter),"1","0")

CocacolaData <- cbind(Cocacola,Q1,Q2,Q3,Q4)
View(CocacolaData)
colnames(CocacolaData)

# input t
CocacolaData["t"] <- c(1:42)
View(CocacolaData)

CocacolaData["log_Sales"] <- log(CocacolaData["Sales"])
CocacolaData["t_square"] <- CocacolaData["t"]*CocacolaData["t"]
View(CocacolaData)
## Preprocesing completed

attach(CocacolaData)


train <- CocacolaData[1:36,]
test <- CocacolaData[37:40,]
test
train

########################### LINEAR MODEL #############################

linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata = test))
linear_pred
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################

expo_model <- lm(log_Sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad


######################### Additive Seasonality #########################

sea_add_model <- lm(Sales ~ Q1+Q2+Q3+Q4, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add


######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Sales ~ t+Q1+Q2+Q3+Q4, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad


######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Sales ~ Q1+Q2+Q3+Q4, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea


# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
View(table_rmse)
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

######Quadratic has least RMSE value

new_model <-lm(Sales~t+t_square+Q1+Q2+Q3+Q4,data = CocacolaData)
new_model_pred <- data.frame(predict(new_model,newdata = CocacolaData,interval = "predict"))
new_model_final <- new_model$fitted.values
View(new_model_final)

Quarter <- as.data.frame(CocacolaData$Quarter)
Final <-as.data.frame(cbind(Quarter,CocacolaData$Sales,new_model_final))
colnames(Final) <- c("Quarter","Sales","new_pred_value")

plot(Final$new_pred_value,main = "PredictedGraph", xlab = "Sales(Predicted)", ylab = "Quarter",
     col.axis="Green",type = "s")



###############FOR AIRLINE DATA############################
library(forecast)
library(fpp)
library(smooth)
library(readxl)
Airlines <- read_excel(file.choose())
View(Airlines)
plot(Airlines$Passengers)
plot(Airlines$Passengers,type = "o")

X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==")+0)
View(X)
colnames(X)<- month.abb
View(X)
AirlinesData <- cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)

AirlinesData["t"] <- c(1:96)
View(AirlinesData)

AirlinesData["log_Passengers"] <- log(AirlinesData["Passengers"])
AirlinesData["t_square"] <- AirlinesData["t"]*AirlinesData["t"]
View(AirlinesData)
## Preprocesing completed

attach(AirlinesData)


train <- AirlinesData[1:84,]
test <- AirlinesData[85:96,]
test
train

########################### LINEAR MODEL #############################

linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata = test))
linear_pred
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear



######################### Exponential #################################

expo_model <- lm(log_Passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################

Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad


######################### Additive Seasonality #########################

sea_add_model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add


######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model <- lm(Passengers ~ t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad


######################## Multiplicative Seasonality #########################

multi_sea_model <- lm(log_Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea


# Preparing table on model and it's RMSE values 

table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
View(table_rmse)
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)



#########Additive Seasonality with Quadratic has least value#######


new_model <-lm(Passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = AirlinesData)
new_model_pred <- data.frame(predict(new_model,newdata = AirlinesData,interval = "predict"))
new_model_final <- new_model$fitted.values
View(new_model_final)

Month <- as.data.frame(AirlinesData$Month)
Final <-as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_final))
colnames(Final) <- c("Month","Passengers","new_pred_value")

plot(Final$new_pred_value,main = "PredictedGraph", xlab = "Sales(Predicted)", ylab = "Quarter",
     col.axis="Green",type = "s")
