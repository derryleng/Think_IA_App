rm(list = ls())

library(data.table)
library(RODBC)
library(dplyr)
library(ggplot2)
library(smooth)
library(modelr)
library(tidyverse)
library(lubridate)
library(grid)
library(gridExtra)

db <- fread("C:\\Users\\Michael Cowham\\Dropbox (Think Research)\\NATS Projects\\NATS NavCanada TBS\\Data Analysis\\Outputs\\ORD Output\\Adaptation V3 23_04_21\\Parameters_Aircraft_Type_Characteristics_2.csv")

db <- rename(db, Class = `Physical Class (Engine)`, Wingspan = `Wingspan, ft`, Length = `Length, ft`)
db <- mutate(db, Area = Wingspan * Length, Wing_Span_Weight = Wingspan / MTOW, Wing_Span_Sq_Weight = (Wingspan^2) / MTOW, Length_Weight = Length / MTOW, Wing_Span_Length_Weight = Wingspan * Length / MTOW, Wing_Span_Length_Sq_Weight = ((Wingspan * Length) ^ 2 )/ MTOW)

mod1 <- lm(a1 ~ Class + Wingspan + Length + MTOW, data = db)
summary(mod1)

mod2 <- lm(a1 ~ Wing_Span_Weight, data = db)
summary(mod2)

mod3 <- lm(a1 ~ Wing_Span_Weight, data = db)
summary(mod3)

mod4 <- lm(a1 ~ Wing_Span_Sq_Weight, data = db)
summary(mod4)

mod4 <- lm(vref ~ Wing_Span_Sq_Weight, data = db)
summary(mod4)



ggplot(data = )

pairs(~ a1 + Wingspan + Length + MTOW + Wing_Span_Weight + Wing_Span_Sq_Weight, data = db)

ggplot(data = db)+
  geom_point(aes(x = Length, y = a1, color = Class))+
  geom_label(aes(x = Length, y = a1, label = Follower_Aircraft_Type))

ggplot(data = db)+
  geom_point(aes(x = Wing_Span_Weight, y = a1, color = Class))+
  geom_label(aes(x = Wing_Span_Weight, y = a1, label = Follower_Aircraft_Type))

ggplot(data = db)+
  geom_point(aes(x = Wing_Span_Sq_Weight, y = a1, color = Class))+
  geom_label(aes(x = Wing_Span_Sq_Weight, y = a1, label = Follower_Aircraft_Type))


ggplot(data = db)+
  geom_point(aes(x = Wing_Span_Sq_Weight, y = vref, color = Class))+
  geom_label(aes(x = Wing_Span_Sq_Weight, y = vref, label = Follower_Aircraft_Type))


ggplot(data = filter(db, N >= 30))+
  geom_point(aes(x = Wing_Span_Sq_Weight, y = vref))+
  geom_label(aes(x = Wing_Span_Sq_Weight, y = vref, label = Follower_Aircraft_Type))




ggplot(data = db)+
  geom_point(aes(x = Wing_Span_Length_Sq_Weight, y = vref, color = Class))+
  geom_label(aes(x = Wing_Span_Length_Sq_Weight, y = vref, label = Follower_Aircraft_Type))
