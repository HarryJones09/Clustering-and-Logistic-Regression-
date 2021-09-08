#install.packages("olsrr")
#install.packages("cluster")
#install.packages("ggfortify")
#install.packages("broom")


library(GGally)
library(olsrr)
library(tidyverse)
library(cluster)
library(ggfortify)
library(broom)
mydata = read_csv("decathlon_rio2016(Edit).csv")
#View(mydata)



colnames(mydata) <- c("Athelete","Overall Points", "100m", 
                      "Long Jump","Shot Put","High Jump",
                      "400m","110M Hurdles","Discuss Throw",
                      "Pole Vaulting", "Javelin Throw",
                      "1500m")


#Making tebble dataframe

Athelete = c(mydata$`Athelete`)
Overall_Points = c(mydata$`Overall Points`)
One_Hundred_m = c(mydata$`100m`)
Long_Jump = c(mydata$`Long Jump`)
Shot_Put = c(mydata$`Shot Put`)
High_Jump = c(mydata$`High Jump`)
Four_Hundred_m = c(mydata$`400m`)
One_Hundred_ten_Hurdles = c(mydata$`110M Hurdles`)
Discuss = c(mydata$`Discuss Throw`)
Pole_Vaulting = c(mydata$`Pole Vaulting`)
Javelin_Throw = c(mydata$`Javelin Throw`)

Fifteen_Hundred_m = c(mydata$`1500m`)


Decathlon = tibble(Athelete,Overall_Points,
                   One_Hundred_m,
                   Long_Jump, Shot_Put, High_Jump,
                   Four_Hundred_m, 
                   One_Hundred_ten_Hurdles,
                   Discuss,
                   Pole_Vaulting, Javelin_Throw,
                   Fifteen_Hundred_m)
Decathlon

#Scatter matrix 

De_sub = Decathlon %>%
  select(Athelete,Overall_Points, One_Hundred_m,
         Long_Jump, Shot_Put, High_Jump,
         Four_Hundred_m, 
         One_Hundred_ten_Hurdles,
         Discuss,
         Pole_Vaulting, Javelin_Throw,
         Fifteen_Hundred_m) 

Decathlon_matrix = select(De_sub, One_Hundred_m,
                          Long_Jump, Shot_Put, High_Jump,
                          Four_Hundred_m, 
                          One_Hundred_ten_Hurdles,
                          Discuss,
                          Pole_Vaulting, Javelin_Throw,
                          Fifteen_Hundred_m)
ggpairs(select(Decathlon, -Athelete))


#all predictions of overall points 
model <- lm(Overall_Points  ~ One_Hundred_m + Long_Jump +
              Shot_Put + High_Jump+
              Four_Hundred_m+
              One_Hundred_ten_Hurdles+
              Discuss+ Pole_Vaulting+
              Javelin_Throw+
              Fifteen_Hundred_m, data = Decathlon)

ols_step_all_possible(model)


#best predictions of overall points 


ols_step_best_subset(model)


#Task 2 question 3 
#Best day 1 events 


Day1Decathlon = tibble( One_Hundred_m,
                        Long_Jump, Shot_Put, High_Jump,
                        Four_Hundred_m)

Day1Decathlon

model1 <- lm(Overall_Points ~ One_Hundred_m + Long_Jump +
              Shot_Put + High_Jump+
              Four_Hundred_m
              , data = Day1Decathlon)

ols_step_best_subset(model1)

#2 different models in Q2
model4 <-lm(Overall_Points ~ Long_Jump-1)
summary(model4)

autoplot(model4)

ggplot(Decathlon, aes(x=Long_Jump, y=Overall_Points)) +
  geom_point() +
  geom_smooth(method4=lm, se=FALSE)


model5 <-lm(Overall_Points ~ (Long_Jump + Javelin_Throw)-1)
summary(model5)

autoplot(model5)

#is there an interaction effect present 
model8 <-lm(Overall_Points ~ (Long_Jump*Javelin_Throw)-1)
summary(model8)
#2 different models looked at in q3

#four predictor model
model2 <-lm(Overall_Points ~ (Long_Jump + Shot_Put + High_Jump +
                                Four_Hundred_m)-1)

summary(model2)

autoplot(model2)

#five predictor model 

model3 <-lm(Overall_Points ~ (Long_Jump + Shot_Put + High_Jump +
                                One_Hundred_ten_Hurdles + Discuss))

summary(model3)

autoplot(model3)

ggplot(Decathlon, aes(x=Long_Jump + Shot_Put + High_Jump +
                        One_Hundred_ten_Hurdles + Discuss, y=Overall_Points)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

#Task 2 question 3
#Plot to see individual athletes 
library(ggfortify)
autoplot(model)

#critically assess how good the model is 

model2 <-lm(Overall_Points ~ (Long_Jump + Shot_Put + High_Jump + 
                                Four_Hundred_m)-1)
            

summary(model2)

#--

#mess around with some ideas 

model3 = lm(Overall_Points~ Long_Jump + Shot_Put + High_Jump + One_Hundred_ten_Hurdles
               +Discuss, data=Decathlon)
summary(model3)

fitted = augment(model3)

fitted

ggplot(fitted, aes(x=.fitted,y=.resid)) +
  geom_point() +
  geom_hline(yintercept=0)
#-------------------------------------------------------------------------