###### Adults albacore #######

# Random Forest models by month
# Fig 3
# organize data to export to Fig 5

library(tidyverse)
library(lubridate) 
library(glue)
library(rgdal)
library(ggplot2)
library(randomForest)
require(plotmo)
require (ModelMetrics)
   

dsn <- "/home/.../analises/diretório"
setwd(dsn)

captura <-read.csv("captura_juv_adul.csv") %>%  as_tibble()

# 1) Organize data #############################################################
# Models using adults kg 

captura_adul <- captura %>%  dplyr::select(Date_time, Cod_qdd,GeoStrata,Lat,Lon,Eff1,
                                           kg_adultos,SST_mean, Chlor_a)
captura_adul <- na.omit(captura_adul)
captura_adul <- captura_adul %>%  filter(kg_adultos > 0)

#  quarter information
captura_adul <- captura_adul %>% #  filter(GeoStrata=="5x5") %>% 
                mutate(quarter= case_when(month(Date_time)>=1 & month(Date_time)<4 ~"Quarter 1",
                                          month(Date_time)>=4 & month(Date_time)<7 ~"Quarter 2",
                                         month(Date_time)>=7 & month(Date_time)<10 ~"Quarter 3",
                                       month(Date_time)>=10 & month(Date_time)<=12 ~"Quarter 4"))


# separate by month

capt_mes_1 <- captura_adul %>% filter(month(Date_time)==1) 
capt_mes_2 <- captura_adul %>% filter(month(Date_time)==2) 
capt_mes_3 <- captura_adul %>% filter(month(Date_time)==3)
capt_mes_4 <- captura_adul %>% filter(month(Date_time)==4)
capt_mes_5 <- captura_adul %>% filter(month(Date_time)==5)
capt_mes_6 <- captura_adul %>% filter(month(Date_time)==6)
capt_mes_7 <- captura_adul %>% filter(month(Date_time)==7)
capt_mes_8 <- captura_adul %>% filter(month(Date_time)==8)
capt_mes_9 <- captura_adul %>% filter(month(Date_time)==9)
capt_mes_10 <- captura_adul %>% filter(month(Date_time)==10)
capt_mes_11 <- captura_adul %>% filter(month(Date_time)==11)
capt_mes_12 <- captura_adul %>% filter(month(Date_time)==12)


# 2) Random Forest  Model ######################################################


# check colinearity
require(faraway)
explicativas = capt_mes_12[,8:9]
pairs(explicativas)
cor(explicativas)
vif(explicativas)


# adults models (kg)
# 4 models are tested by month
# the selected model presented a combination of good residuals graphs and the high % Var explaination

# mes 1 
set.seed(11)
ind_1 <- sample(2, nrow(capt_mes_1), replace = TRUE, prob = c(0.7, 0.3))
train_1 <- capt_mes_1[ind_1==1,]
test_1 <- capt_mes_1[ind_1==2,]
# Testing if both bariables are important to the model
train_1<- as.data.frame(train_1)


rfkg_1_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_1)
print(rfkg_1_1)
plotres(rfkg_1_1)
resultadoRF <-predict( rfkg_1_1,test_1)

rmse(resultadoRF,test_1$kg_adultos) # 96248.37
mae(resultadoRF,test_1$kg_adultos)  # 36366.07

importance(rfkg_1_1)
varImpPlot(rfkg_1_1)

rfkg_1_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_1)
print(rfkg_1_2)
plotres(rfkg_1_2)
resultadoRF <-predict( rfkg_1_2,test_1)

rmse(resultadoRF,test_1$kg_adultos) # 96438.4
mae(resultadoRF,test_1$kg_adultos)  # 36347.05

rfkg_1_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_1,proximity=TRUE)
print(rfkg_1_3)
plotres(rfkg_1_3 , caption="Selected model adult January: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_1)")
resultadoRF <-predict( rfkg_1_3,test_1)

rmse(resultadoRF,test_1$kg_adultos) # 108569
mae(resultadoRF,test_1$kg_adultos)  # 34962.57

importance(rfkg_1_3)
varImpPlot(rfkg_1_3)

rfkg_1_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_1)
print(rfkg_1_4)
plotres(rfkg_1_4)
resultadoRF <-predict( rfkg_1_4,test_1)

rmse(resultadoRF,test_1$kg_adultos) # 108569
mae(resultadoRF,test_1$kg_adultos)  # 34962.57


# mes 2
set.seed(12)
ind_2 <- sample(2, nrow(capt_mes_2), replace = TRUE, prob = c(0.7, 0.3))
train_2 <- capt_mes_2[ind_2==1,]
test_2 <- capt_mes_2[ind_2==2,]

rfkg_2_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_2)
print(rfkg_2_1)
plotres(rfkg_2_1)
resultadoRF <-predict( rfkg_2_1,test_2)

rmse(resultadoRF,test_2$kg_adultos) # 51012.68
mae(resultadoRF,test_2$kg_adultos)  # 23420.95

importance(rfkg_2_1)
varImpPlot(rfkg_2_1)

rfkg_2_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_2)
print(rfkg_2_2)
plotres(rfkg_2_2)
resultadoRF <-predict( rfkg_2_2,test_2)

rmse(resultadoRF,test_2$kg_adultos) # 51401.33
mae(resultadoRF,test_2$kg_adultos)  # 23594.92


rfkg_2_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_2)
print(rfkg_2_3)
plotres(rfkg_2_3 , caption="Selected model adult February: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_2)")

resultadoRF <-predict( rfkg_2_3,test_2)

rmse(resultadoRF,test_2$kg_adultos) #  53223.87
mae(resultadoRF,test_2$kg_adultos)  # 21686.66

importance(rfkg_2_3)# chlor a + importante


rfkg_2_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_2)
print(rfkg_2_4)
plotres(rfkg_2_4)
resultadoRF <-predict( rfkg_2_4,test_2)

rmse(resultadoRF,test_2$kg_adultos) # 53223.87
mae(resultadoRF,test_2$kg_adultos)  # 21686.66

# mes 3
set.seed(13)
ind_3 <- sample(2, nrow(capt_mes_3), replace = TRUE, prob = c(0.7, 0.3))
train_3 <- capt_mes_3[ind_3==1,]
test_3 <- capt_mes_3[ind_3==2,]

rfkg_3_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_3)
print(rfkg_3_1)
plotres(rfkg_3_1)
resultadoRF <-predict( rfkg_3_1,test_3)


importance(rfkg_3_1)
varImpPlot(rfkg_3_1)

rfkg_3_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_3)
print(rfkg_3_2)
plotres(rfkg_3_2)
resultadoRF <-predict( rfkg_3_2,test_3)

rmse(resultadoRF,test_3$kg_adultos) # 31654.61
mae(resultadoRF,test_3$kg_adultos)  #  15583.98

rfkg_3_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_3)
print(rfkg_3_3)
plotres(rfkg_3_3 , caption="Selected model adult March: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_3)")

resultadoRF <-predict( rfkg_3_3,test_3)

rmse(resultadoRF,test_3$kg_adultos) # 35051.38
mae(resultadoRF,test_3$kg_adultos)  # 14788.74

importance(rfkg_3_3)


rfkg_3_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_3)
print(rfkg_3_4)
plotres(rfkg_3_4)
resultadoRF <-predict( rfkg_3_4,test_3)

rmse(resultadoRF,test_3$kg_adultos) # 35051.38
mae(resultadoRF,test_3$kg_adultos)  # 14788.74


# mes 4
set.seed(14)
ind_4 <- sample(2, nrow(capt_mes_4), replace = TRUE, prob = c(0.7, 0.3))
train_4 <- capt_mes_4[ind_4==1,]
test_4 <- capt_mes_4[ind_4==2,]

rfkg_4_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_4)
print(rfkg_4_1)
plotres(rfkg_4_1)
resultadoRF <-predict( rfkg_4_1,test_4)

rmse(resultadoRF,test_4$kg_adultos) #  25739.97
mae(resultadoRF,test_4$kg_adultos)  # 15392.85

importance(rfkg_4_1)
varImpPlot(rfkg_4_1)

rfkg_4_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_4)
print(rfkg_4_2)
plotres(rfkg_4_2)
resultadoRF <-predict( rfkg_4_2,test_4)

rmse(resultadoRF,test_4$kg_adultos) # 26150.04
mae(resultadoRF,test_4$kg_adultos)  #  15588.13

rfkg_4_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_4)
print(rfkg_4_3)
plotres(rfkg_4_3,  caption="Selected model adult April: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_4)")

resultadoRF <-predict( rfkg_4_3,test_4)

rmse(resultadoRF,test_4$kg_adultos) # 30084.81
mae(resultadoRF,test_4$kg_adultos)  # 13994.72


rfkg_4_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_4)
print(rfkg_4_4)
plotres(rfkg_4_4)
resultadoRF <-predict( rfkg_4_4,test_4)

rmse(resultadoRF,test_4$kg_adultos) # 30084.81
mae(resultadoRF,test_4$kg_adultos)  # 13994.72


# mes 5
set.seed(15)
ind_5 <- sample(2, nrow(capt_mes_5), replace = TRUE, prob = c(0.7, 0.3))
train_5 <- capt_mes_5[ind_5==1,]
test_5 <- capt_mes_5[ind_5==2,]

rfkg_5_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_5)
print(rfkg_5_1)
plotres(rfkg_5_1)
resultadoRF <-predict( rfkg_5_1,test_5)

rmse(resultadoRF,test_5$kg_adultos) # 49115.07
mae(resultadoRF,test_5$kg_adultos)  # 24182.42

importance(rfkg_5_1)
varImpPlot(rfkg_5_1)

rfkg_5_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_5)
print(rfkg_5_2)
plotres(rfkg_5_2)
resultadoRF <-predict( rfkg_5_2,test_5)

rmse(resultadoRF,test_5$kg_adultos) # 49200.01
mae(resultadoRF,test_5$kg_adultos)  # 23913.48

rfkg_5_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_5)
print(rfkg_5_3)
plotres(rfkg_5_3 ,  caption="Selected model adult May: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_5)")

resultadoRF <-predict( rfkg_5_3,test_5)
importance(rfkg_5_3)
rmse(resultadoRF,test_5$kg_adultos) #  58206.28
mae(resultadoRF,test_5$kg_adultos)  #  25158.88


rfkg_5_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_5)
print(rfkg_5_4)
plotres(rfkg_5_4)
resultadoRF <-predict( rfkg_5_4,test_5)
rmse(resultadoRF,test_5$kg_adultos) #  58206.28
mae(resultadoRF,test_5$kg_adultos)  #  25158.88



# mes 6
set.seed(16)
ind_6 <- sample(2, nrow(capt_mes_6), replace = TRUE, prob = c(0.7, 0.3))
train_6 <- capt_mes_6[ind_6==1,]
test_6 <- capt_mes_6[ind_6==2,]

rfkg_6_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_6)
print(rfkg_6_1)
plotres(rfkg_6_1)
resultadoRF <-predict( rfkg_6_1,test_6)

rmse(resultadoRF,test_6$kg_adultos) # 77716.31
mae(resultadoRF,test_6$kg_adultos)  #29053.78

importance(rfkg_6_1)
varImpPlot(rfkg_6_1)

rfkg_6_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_6)
print(rfkg_6_2)
plotres(rfkg_6_2)
resultadoRF <-predict( rfkg_6_2,test_6)

rmse(resultadoRF,test_6$kg_adultos) # 77253.66
mae(resultadoRF,test_6$kg_adultos)  # 28885.94

rfkg_6_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_6)
print(rfkg_6_3)
plotres(rfkg_6_3 ,  caption="Selected model adult June: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_6)")

resultadoRF <-predict( rfkg_6_3,test_6)
importance(rfkg_6_3)
rmse(resultadoRF,test_6$kg_adultos) #  83898.82
mae(resultadoRF,test_6$kg_adultos)  #  28152.3

rfkg_6_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_6)
print(rfkg_6_4)
plotres(rfkg_6_4)
resultadoRF <-predict( rfkg_6_4,test_6)
rmse(resultadoRF,test_6$kg_adultos) #  83898.82
mae(resultadoRF,test_6$kg_adultos)  #  28152.3


# mes 7
set.seed(17)
ind_7 <- sample(2, nrow(capt_mes_7), replace = TRUE, prob = c(0.7, 0.3))
train_7 <- capt_mes_7[ind_7==1,]
test_7 <- capt_mes_7[ind_7==2,]

rfkg_7_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_7)
print(rfkg_7_1)
plotres(rfkg_7_1)
resultadoRF <-predict( rfkg_7_1,test_7)

rmse(resultadoRF,test_7$kg_adultos) # 36925.46
mae(resultadoRF,test_7$kg_adultos)  # 18952.35

importance(rfkg_7_1)
varImpPlot(rfkg_7_1)

rfkg_7_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_7)
print(rfkg_7_2)
plotres(rfkg_7_2)
resultadoRF <-predict( rfkg_7_2,test_7)

rmse(resultadoRF,test_7$kg_adultos) # 36733.26
mae(resultadoRF,test_7$kg_adultos)  #  18867.26

rfkg_7_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_7)
print(rfkg_7_3)
plotres(rfkg_7_3 ,  caption="Selected model adult July: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_7)")

resultadoRF <-predict( rfkg_7_3,test_7)
importance(rfkg_7_3)
rmse(resultadoRF,test_7$kg_adultos) #  44450.11
mae(resultadoRF,test_7$kg_adultos)  #  19627.86

rfkg_7_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_7)
print(rfkg_7_4)
plotres(rfkg_7_4)
resultadoRF <-predict( rfkg_7_4,test_7)
rmse(resultadoRF,test_7$kg_adultos) #   44450.12
mae(resultadoRF,test_7$kg_adultos)  #   19627.89


# mes 8
set.seed(18)
ind_8 <- sample(2, nrow(capt_mes_8), replace = TRUE, prob = c(0.7, 0.3))
train_8 <- capt_mes_8[ind_8==1,]
test_8 <- capt_mes_8[ind_8==2,]

rfkg_8_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_8)
print(rfkg_8_1)
plotres(rfkg_8_1)
resultadoRF <-predict( rfkg_8_1,test_8)

rmse(resultadoRF,test_8$kg_adultos) # 32685.31
mae(resultadoRF,test_8$kg_adultos)  # 20088.21

importance(rfkg_8_1)
varImpPlot(rfkg_8_1)

rfkg_8_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_8)
print(rfkg_8_2)
plotres(rfkg_8_2)
resultadoRF <-predict( rfkg_8_2,test_8)

rmse(resultadoRF,test_8$kg_adultos) # 33045.72
mae(resultadoRF,test_8$kg_adultos)  # 20222.18

rfkg_8_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_8)
print(rfkg_8_3)
plotres(rfkg_8_3, caption="Selected model adult August: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_8)")

resultadoRF <-predict( rfkg_8_3,test_8)
importance(rfkg_8_3)
rmse(resultadoRF,test_8$kg_adultos) #  37976.34
mae(resultadoRF,test_8$kg_adultos)  #  19485.69

rfkg_8_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_8)
print(rfkg_8_4)
plotres(rfkg_8_4)
resultadoRF <-predict( rfkg_8_4,test_8)
rmse(resultadoRF,test_8$kg_adultos) #   37976.33
mae(resultadoRF,test_8$kg_adultos)  #   19485.69


# mes 9
set.seed(19)
ind_9 <- sample(2, nrow(capt_mes_9), replace = TRUE, prob = c(0.7, 0.3))
train_9 <- capt_mes_9[ind_9==1,]
test_9 <- capt_mes_9[ind_9==2,]

rfkg_9_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_9)
print(rfkg_9_1)
plotres(rfkg_9_1)
resultadoRF <-predict( rfkg_9_1,test_9)

rmse(resultadoRF,test_9$kg_adultos) # 37259.91
mae(resultadoRF,test_9$kg_adultos)  # 16662.27

importance(rfkg_9_1)
varImpPlot(rfkg_9_1)

rfkg_9_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_9)
print(rfkg_9_2)
plotres(rfkg_9_2)
resultadoRF <-predict( rfkg_9_2,test_9)

rmse(resultadoRF,test_9$kg_adultos) # 37392.97
mae(resultadoRF,test_9$kg_adultos)  # 16629.5

rfkg_9_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_9)
print(rfkg_9_3)
plotres(rfkg_9_3, caption="Selected model adult September: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_9)")

resultadoRF <-predict( rfkg_9_3,test_9)
importance(rfkg_9_3)
rmse(resultadoRF,test_9$kg_adultos) #  43025.04
mae(resultadoRF,test_9$kg_adultos)  #  16895.01

rfkg_9_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_9)
print(rfkg_9_4)
plotres(rfkg_9_4)
resultadoRF <-predict( rfkg_9_4,test_9)
rmse(resultadoRF,test_9$kg_adultos) #  43025.04
mae(resultadoRF,test_9$kg_adultos)  #  16895.01



# mes 10
set.seed(10)
ind_10 <- sample(2, nrow(capt_mes_10), replace = TRUE, prob = c(0.7, 0.3))
train_10 <- capt_mes_10[ind_10==1,]
test_10 <- capt_mes_10[ind_10==2,]

rfkg_10_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_10)
print(rfkg_10_1)
plotres(rfkg_10_1)
resultadoRF <-predict( rfkg_10_1,test_10)

rmse(resultadoRF,test_10$kg_adultos) #19638.34
mae(resultadoRF,test_10$kg_adultos)  # 11815.56

importance(rfkg_10_1)
varImpPlot(rfkg_10_1)

rfkg_10_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_10)
print(rfkg_10_2)
plotres(rfkg_10_2)
resultadoRF <-predict( rfkg_10_2,test_10)

rmse(resultadoRF,test_10$kg_adultos) # 19632.82
mae(resultadoRF,test_10$kg_adultos)  # 11813.6

rfkg_10_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_10)
print(rfkg_10_3)
plotres(rfkg_10_3, caption="Selected model adult October: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_10)")

resultadoRF <-predict( rfkg_10_3,test_10)
importance(rfkg_10_3)
rmse(resultadoRF,test_10$kg_adultos) #   23481.85
mae(resultadoRF,test_10$kg_adultos)  #  11907.3

rfkg_10_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_10)
print(rfkg_10_4)
plotres(rfkg_10_4)
resultadoRF <-predict( rfkg_10_4,test_10)
rmse(resultadoRF,test_10$kg_adultos) #   23481.85
mae(resultadoRF,test_10$kg_adultos)  #  11907.3


# mes 11
set.seed(011)
ind_11 <- sample(2, nrow(capt_mes_11), replace = TRUE, prob = c(0.7, 0.3))
train_11 <- capt_mes_11[ind_11==1,]
test_11 <- capt_mes_11[ind_11==2,]

rfkg_11_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_11)
print(rfkg_11_1)
plotres(rfkg_11_1)
resultadoRF <-predict( rfkg_11_1,test_11)

rmse(resultadoRF,test_11$kg_adultos) #  35971.18
mae(resultadoRF,test_11$kg_adultos)  #  19645.89

importance(rfkg_11_1)
varImpPlot(rfkg_11_1)

rfkg_11_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_11)
print(rfkg_11_2)
plotres(rfkg_11_2)
resultadoRF <-predict( rfkg_11_2,test_11)

rmse(resultadoRF,test_11$kg_adultos) #  19645.89
mae(resultadoRF,test_11$kg_adultos)  # 19398.11

rfkg_11_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_11)
print(rfkg_11_3)
plotres(rfkg_11_3, caption="Selected model adult November: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_11)")

resultadoRF <-predict( rfkg_11_3,test_11)
importance(rfkg_11_3)
rmse(resultadoRF,test_11$kg_adultos) #  39574.14
mae(resultadoRF,test_11$kg_adultos)  #  17082.55

rfkg_11_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_11)
print(rfkg_11_4)
plotres(rfkg_11_4)
resultadoRF <-predict( rfkg_11_4,test_11)
rmse(resultadoRF,test_11$kg_adultos) #  39574.13
mae(resultadoRF,test_11$kg_adultos)  #  17082.55


# mes 12
set.seed(012)
ind_12 <- sample(2, nrow(capt_mes_12), replace = TRUE, prob = c(0.7, 0.3))
train_12 <- capt_mes_12[ind_12==1,]
test_12 <- capt_mes_12[ind_12==2,]

rfkg_12_1 <-  randomForest(kg_adultos ~ SST_mean + Chlor_a, data=  train_12)
print(rfkg_12_1)
plotres(rfkg_12_1)
resultadoRF <-predict( rfkg_12_1,test_12)

rmse(resultadoRF,test_12$kg_adultos) #   60094.99
mae(resultadoRF,test_12$kg_adultos)  #  30297.89

importance(rfkg_12_1)
varImpPlot(rfkg_12_1)

rfkg_12_2 <-  randomForest(kg_adultos ~ SST_mean * Chlor_a, data=  train_12)
print(rfkg_12_2)
plotres(rfkg_12_2)
resultadoRF <-predict( rfkg_12_2,test_12)

rmse(resultadoRF,test_12$kg_adultos) # 60727.71
mae(resultadoRF,test_12$kg_adultos)  # 31113.2

rfkg_12_3 <-  randomForest(log(kg_adultos) ~ SST_mean + Chlor_a , data=  train_12)
print(rfkg_12_3)
plotres(rfkg_12_3, caption="Selected model adult December: \n randomForest(log(kg_adults) ~\n SST + Chlor_a , data= train_12)")

resultadoRF <-predict( rfkg_12_3,test_12)
importance(rfkg_12_3)
rmse(resultadoRF,test_12$kg_adultos) #  63182.17
mae(resultadoRF,test_12$kg_adultos)  #  23587.42

rfkg_12_4 <-  randomForest(log(kg_adultos) ~ SST_mean * Chlor_a, data=  train_12)
print(rfkg_12_4)
plotres(rfkg_12_4)
resultadoRF <-predict( rfkg_12_4,test_12)
rmse(resultadoRF,test_12$kg_adultos) #  63182.18
mae(resultadoRF,test_12$kg_adultos)  #  23587.42


# 3) Rasterization #############################################################

#Criar um data frame contendo Lat, Lon, SST média, Clorofila Média para cada mes. São 12 data frames ao todo.
#(essa parte vai ser feita no script "prepara rasterização.R")
#No QGIS: Importar o data frame como csv com Lat Lon e em seguida exportar como shapefile.
#De volta no R: carregar pacote “raster” e abrir cada shapefile com a função shapefile().

library(raster)
ext <- extent(c(-60, 20, -45, 10))
sst_clo_1 <- shapefile("/home/.../ssta_clor_mes_1.shp")
# rasterize points
r1 <- raster(sst_clo_1, ncols=80, nrows=55) 
raster_sst_clo_1 <- rasterize(sst_clo_1, r1, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_1, main=c('SST_mean mes 1', 'Chlor_a mes 1'))
s1 <- stack(raster_sst_clo_1)
names(s1) <- c('SST_mean','Chlor_a')
pg1 <- predict(s1, rfkg_1_3)  # o modelo que foi melhor no mes 1
plot(pg1, main='Random Forest')
# obtem os dados preditos em forma de data frame
pg1_values_jan <- as.data.frame(pg1@data@values) 
names(pg1_values_jan) <-"predicted"


sst_clo_2 <- shapefile("/home/.../ssta_clor_mes_2.shp")
# rasterize points
r2 <- raster(sst_clo_2, ncols=80, nrows=55)
raster_sst_clo_2 <- rasterize(sst_clo_2, r2, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_2, main=c('SST_mean mes 2', 'Chlor_a mes 2'))
s2 <- stack(raster_sst_clo_2)
names(s2) <- c('SST_mean','Chlor_a')
pg2 <- predict(s2, rfkg_2_3)  # o modelo que foi melhor no mes 2
plot(pg2, main='Random Forest')
pg2_values_fev <- as.data.frame(pg2@data@values) 
names(pg2_values_fev) <-"predicted"


sst_clo_3 <- shapefile("/home/.../ssta_clor_mes_3.shp")
# rasterize points
r3 <- raster(sst_clo_3, ncols=80, nrows=55)
raster_sst_clo_3 <- rasterize(sst_clo_3, r3, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_3, main=c('SST_mean mes 3', 'Chlor_a mes 3'))
s3 <- stack(raster_sst_clo_3)
names(s3) <- c('SST_mean','Chlor_a')
pg3 <- predict(s3, rfkg_3_3)  # o modelo que foi melhor no mes 3
plot(pg3, main='Random Forest')
pg3_values_mar <- as.data.frame(pg3@data@values) 
names(pg3_values_mar) <-"predicted"


sst_clo_4 <- shapefile("/home/.../ssta_clor_mes_4.shp")
# rasterize points
r4 <- raster(sst_clo_4, ncols=80, nrows=55)
raster_sst_clo_4 <- rasterize(sst_clo_4, r4, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_4, main=c('SST_mean mes 4', 'Chlor_a mes 4'))
s4 <- stack(raster_sst_clo_4)
names(s4) <- c('SST_mean','Chlor_a')
pg4 <- predict(s4, rfkg_4_3)  # o modelo que foi melhor no mes 4
plot(pg4, main='Random Forest')
pg4_values_abr <- as.data.frame(pg4@data@values) 
names(pg4_values_abr) <-"predicted"


sst_clo_5 <- shapefile("/home/.../ssta_clor_mes_5.shp")
# rasterize points
r5 <- raster(sst_clo_5, ncols=80, nrows=55)
raster_sst_clo_5 <- rasterize(sst_clo_5, r5, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_5, main=c('SST_mean mes 5', 'Chlor_a mes 5'))
s5 <- stack(raster_sst_clo_5)
names(s5) <- c('SST_mean','Chlor_a')
pg5 <- predict(s5, rfkg_5_3)  # o modelo que foi melhor no mes 5
plot(pg5, main='Random Forest')
pg5_values_mai <- as.data.frame(pg5@data@values) 
names(pg5_values_mai) <-"predicted"


sst_clo_6 <- shapefile("/home/.../ssta_clor_mes_6.shp")
# rasterize points
r6 <- raster(sst_clo_6, ncols=80, nrows=55)
raster_sst_clo_6 <- rasterize(sst_clo_6, r6, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_6, main=c('SST_mean mes 6', 'Chlor_a mes 6'))
s6 <- stack(raster_sst_clo_6)
names(s6) <- c('SST_mean','Chlor_a')
pg6 <- predict(s6, rfkg_6_3)  # o modelo que foi melhor no mes 6
plot(pg6, main='Random Forest')
pg6_values_jun <- as.data.frame(pg6@data@values) 
names(pg6_values_jun) <-"predicted"


sst_clo_7 <- shapefile("/home/.../ssta_clor_mes_7.shp")
# rasterize points
r7 <- raster(sst_clo_7, ncols=80, nrows=55)
raster_sst_clo_7 <- rasterize(sst_clo_7, r7, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_7, main=c('SST_mean mes 7', 'Chlor_a mes 7'))
s7 <- stack(raster_sst_clo_7)
names(s7) <- c('SST_mean','Chlor_a')
pg7 <- predict(s7, rfkg_7_3)  # o modelo que foi melhor no mes 7
plot(pg7, main='Random Forest')
pg7_values_jul <- as.data.frame(pg7@data@values) 
names(pg7_values_jul) <-"predicted"



sst_clo_8 <- shapefile("/home/.../ssta_clor_mes_8.shp")
# rasterize points
r8 <- raster(sst_clo_8, ncols=80, nrows=55)
raster_sst_clo_8 <- rasterize(sst_clo_8, r8, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_8, main=c('SST_mean mes 8', 'Chlor_a mes 8'))
s8 <- stack(raster_sst_clo_8)
names(s8) <- c('SST_mean','Chlor_a')
pg8 <- predict(s8, rfkg_8_3)  # o modelo que foi melhor no mes 8
plot(pg8, main='Random Forest')
pg8_values_ago <- as.data.frame(pg8@data@values) 
names(pg8_values_ago) <-"predicted"

sst_clo_9 <- shapefile("/home/.../ssta_clor_mes_9.shp")
# rasterize points
r9 <- raster(sst_clo_9, ncols=80, nrows=55)
raster_sst_clo_9 <- rasterize(sst_clo_9, r9, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_9, main=c('SST_mean mes 9', 'Chlor_a mes 9'))
s9 <- stack(raster_sst_clo_9)
names(s9) <- c('SST_mean','Chlor_a')
pg9 <- predict(s9, rfkg_9_3)  # o modelo que foi melhor no mes 9
plot(pg9, main='Random Forest')
pg9_values_set <- as.data.frame(pg9@data@values) 
names(pg9_values_set) <-"predicted"


sst_clo_10 <- shapefile("/home/.../ssta_clor_mes_10.shp")
# rasterize points
r10 <- raster(sst_clo_10, ncols=80, nrows=55)
raster_sst_clo_10 <- rasterize(sst_clo_10, r10, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_10, main=c('SST_mean mes 10', 'Chlor_a mes 10'))
s10 <- stack(raster_sst_clo_10)
names(s10) <- c('SST_mean','Chlor_a')
pg10 <- predict(s10, rfkg_10_3)  # o modelo que foi melhor no mes 10
plot(pg10, main='Random Forest')
pg10_values_out <- as.data.frame(pg10@data@values) 
names(pg10_values_out) <-"predicted"


sst_clo_11 <- shapefile("/home/.../ssta_clor_mes_11.shp")
# rasterize points
r11 <- raster(sst_clo_11, ncols=80, nrows=55)
raster_sst_clo_11 <- rasterize(sst_clo_11, r11, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_11, main=c('SST_mean mes 11', 'Chlor_a mes 11'))
s11 <- stack(raster_sst_clo_11)
names(s11) <- c('SST_mean','Chlor_a')
pg11 <- predict(s11, rfkg_11_3)  # o modelo que foi melhor no mes 11
plot(pg11, main='Random Forest')
pg11_values_nov <- as.data.frame(pg11@data@values) 
names(pg11_values_nov) <-"predicted"


sst_clo_12 <- shapefile("/home/.../ssta_clor_mes_12.shp")
# rasterize points
r12 <- raster(sst_clo_12, ncols=80, nrows=55)
raster_sst_clo_12 <- rasterize(sst_clo_12, r12, c('SST','clorofila'),  fun='first')
plot(raster_sst_clo_12, main=c('SST_mean mes 12', 'Chlor_a mes 12'))
s12 <- stack(raster_sst_clo_12)
names(s12) <- c('SST_mean','Chlor_a')
pg12 <- predict(s12, rfkg_12_3)  # o modelo que foi melhor no mes 12
plot(pg12, main='Random Forest')
pg12_values_dez <- as.data.frame(pg12@data@values) 
names(pg12_values_dez) <-"predicted"


# merging the graphics

rdf1 <- as.data.frame(pg1, xy=TRUE) %>% 
  mutate(mes= 1)
rdf2 <- as.data.frame(pg2, xy=TRUE)  %>% 
  mutate(mes= 2)
rdf3 <- as.data.frame(pg3, xy=TRUE)  %>% 
  mutate(mes= 3)
rdf4 <- as.data.frame(pg4, xy=TRUE)  %>% 
  mutate(mes= 4)
rdf5 <- as.data.frame(pg5, xy=TRUE) %>% 
  mutate(mes= 5)
rdf6 <- as.data.frame(pg6, xy=TRUE)  %>% 
  mutate(mes= 6)
rdf7 <- as.data.frame(pg7, xy=TRUE)  %>% 
  mutate(mes= 7)
rdf8 <- as.data.frame(pg8, xy=TRUE)  %>% 
  mutate(mes= 8)
rdf9 <- as.data.frame(pg9, xy=TRUE) %>% 
  mutate(mes= 9)
rdf10 <- as.data.frame(pg10, xy=TRUE)  %>% 
  mutate(mes= 10)
rdf11 <- as.data.frame(pg11, xy=TRUE)  %>% 
  mutate(mes= 11)
rdf12 <- as.data.frame(pg12, xy=TRUE)  %>% 
  mutate(mes= 12)

rdf<- rdf1

rdf <- bind_rows(rdf1, rdf2, rdf3, rdf4, rdf5, rdf6, rdf7, rdf8, rdf9, rdf10, rdf11, rdf12) %>% as_tibble()
rdf %>% as_tibble()
rdf <- rdf %>% mutate(mes_date= as.integer(mes))
rdf <- rdf %>% mutate(nomes_mes = reorder(month.name[mes], mes))

#write.csv(rdf, "randomforest_adultos.csv", row.names = FALSE)

# 4) Maps ######################################################################

library(maps)
library("rnaturalearth")
library("rnaturalearthdata")
# colours
library(pacman)
library(RColorBrewer)
library(khroma)


world <- ne_countries(scale = "small", returnclass = "sf")
class(world)

library(sf)
capturas_alb_adul <-st_read(dsn = "/home/.../shapefiles", layer= "maiores capturas ALB")
capturas_alb_adul <- capturas_alb_adul %>% 
                     mutate(nomes_mes=reorder(month.name[mes], mes))


smooth_rainbow <- colour("smooth rainbow")
discrete_rainbow <- colour("discrete rainbow")

plots_adults <-rdf %>%
  ggplot()+
  geom_raster(mapping=aes(x=x, y=y, fill=layer))+
  geom_sf(data=world, fill="grey40", colour="grey40")+
  geom_sf(data=capturas_alb_adul, fill="transparent", lwd=0.3)+
  coord_sf( xlim = c(-56.2, 17), ylim = c(-43, 8.2))+
  scale_fill_gradientn(colours =smooth_rainbow(256, range = c(0.32, 1)), name='partial') + 
  facet_wrap(~nomes_mes, ncol=3)+
  labs(x =NULL, y = NULL)+
  theme_bw() +
  theme(axis.text.x = element_text(size=10,angle=90, vjust =0.5),
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        strip.text.x = element_text(size=10))
plots_adults


# 5) Organize model data and combine to environmental data to Fig 5 ############

# creates data frame with only the 10% of highest predicted values 
pg1_1 <- rdf1 %>% slice_max(layer, prop=0.1) 
pg1_2 <- rdf2 %>% slice_max(layer, prop=0.1) 
pg1_3 <- rdf3 %>% slice_max(layer, prop=0.1) 
pg1_4 <- rdf4 %>% slice_max(layer, prop=0.1) 
pg1_5 <- rdf5 %>% slice_max(layer, prop=0.1) 
pg1_6 <- rdf6 %>% slice_max(layer, prop=0.1) 
pg1_7 <- rdf7 %>% slice_max(layer, prop=0.1) 
pg1_8 <- rdf8 %>% slice_max(layer, prop=0.1) 
pg1_9 <- rdf9 %>% slice_max(layer, prop=0.1) 
pg1_10 <- rdf10 %>% slice_max(layer, prop=0.1) 
pg1_11 <- rdf11 %>% slice_max(layer, prop=0.1) 
pg1_12 <- rdf12 %>% slice_max(layer, prop=0.1)

# save these  data to open in  QGIS

# write.csv(pg1_1, "rdf1.csv", row.names = FALSE)
# write.csv(pg1_2, "rdf2.csv", row.names = FALSE)
# write.csv(pg1_3, "rdf3.csv", row.names = FALSE)
# write.csv(pg1_4, "rdf4.csv", row.names = FALSE)
# write.csv(pg1_5, "rdf5.csv", row.names = FALSE)
# write.csv(pg1_6, "rdf6.csv", row.names = FALSE)
# write.csv(pg1_7, "rdf7.csv", row.names = FALSE)
# write.csv(pg1_8, "rdf8.csv", row.names = FALSE)
# write.csv(pg1_9, "rdf9.csv", row.names = FALSE)
# write.csv(pg1_10, "rdf10.csv", row.names = FALSE)
# write.csv(pg1_11, "rdf11.csv", row.names = FALSE)
# write.csv(pg1_12, "rdf12.csv", row.names = FALSE)



# retrieving the rasterized chlorophyll and TSM data, which are the same for all months
clorofila <- as.data.frame(s1@layers[[2]]@data@values)
names(clorofila) <-"Chlor"
SST <- as.data.frame(s1@layers[[1]]@data@values)
names(SST) <- "SST"

# join SST, Chlorophyll the predicted values of each month

tab_jan <- cbind(SST, clorofila, pg1_values_jan)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=1)
tab_fev <- cbind(SST, clorofila, pg2_values_fev)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=2)
tab_mar <- cbind(SST, clorofila, pg3_values_mar)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=3)
tab_abr <- cbind(SST, clorofila, pg4_values_abr)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=4)
tab_mai <- cbind(SST, clorofila, pg5_values_mai)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=5)
tab_jun <- cbind(SST, clorofila, pg6_values_jun)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=6)
tab_jul <- cbind(SST, clorofila, pg7_values_jul)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=7)
tab_ago <- cbind(SST, clorofila, pg8_values_ago)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=8)
tab_set <- cbind(SST, clorofila, pg9_values_set)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=9)
tab_out <- cbind(SST, clorofila, pg10_values_out)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=10)
tab_nov <- cbind(SST, clorofila, pg11_values_nov)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=11)
tab_dez <- cbind(SST, clorofila, pg12_values_dez)  %>% slice_max(predicted, prop=0.1) %>% mutate(mes=12)


tab_uni <- bind_rows(tab_jan, tab_fev, tab_mar, tab_abr, tab_mai, tab_jun, tab_jul,tab_ago,
                     tab_set, tab_out, tab_nov,tab_dez)
#write.csv(tab_uni, "randomforest_10%_adults.csv", row.names=FALSE)

