#### Preamble ####
# Purpose: The purpose of this code is to clean-up the ces data 2019 
# from the Canadian Election Study. It needs to be put into a tidy format before it 
# can be analysed. This code does that.
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called ces_2019.csv.
# Authors: Michael Huang
# Contact: michaelmichael.huang@mail.utoronto.ca
# Date: 21 December 2020

library(cesR)
library(labelled)
library(tidyverse)
library(plyr)
library(dplyr)
library(aod)
library(lmtest)

get_ces("ces2019_web")
ces2019_web <- to_factor(ces2019_web)

ces<- ces2019_web %>%
  select(cps19_yob,
         cps19_gender,
         cps19_province,
         cps19_education,
         cps19_votechoice)

ces <-
  ces %>% 
  mutate(education = cps19_education)
ces$education <- 
  revalue(ces$education, c("No schooling"="Less than high school diploma or its equivalent"))
ces$education <- 
  revalue(ces$education, c("Some elementary school"="Less than high school diploma or its equivalent"))
ces$education <- 
  revalue(ces$education, c("Completed elementary school"="Less than high school diploma or its equivalent"))
ces$education <- 
  revalue(ces$education, c("Some secondary/ high school"="Less than high school diploma or its equivalent"))
ces$education <- 
  revalue(ces$education, c("Completed secondary/ high school"="High school diploma or a high school equivalency certificate"))
ces$education <- 
  revalue(ces$education, c("Completed technical, community college, CEGEP, College Classique"="College, CEGEP or other non-university certificate or di..."))
ces$education <- 
  revalue(ces$education, c("Some technical, community college, CEGEP, College Classique"="High school diploma or a high school equivalency certificate"))
ces$education <- 
  revalue(ces$education, c("Some university"="High school diploma or a high school equivalency certificate"))
ces$education <- 
  revalue(ces$education, c("Master's degree"="University certificate, diploma or degree above the bach..."))
ces$education <- 
  revalue(ces$education, c("Professional degree or doctorate"="University certificate, diploma or degree above the bach..."))
ces <-
  ces %>% 
  mutate(sex = cps19_gender)
ces$sex <- 
  revalue(ces$sex, c("A woman"="Female"))
ces$sex <- 
  revalue(ces$sex, c("A man"="Male"))

ces <- ces %>%
  filter(education != "Don't know/ Prefer not to answer")


ces <-
  ces %>% 
  mutate(province = cps19_province)

ces <-
  ces %>% 
  mutate(age = 2019- as.integer(ces$cps19_yob)-1919)
ces <- 
  ces%>%
  mutate(votechoice = cps19_votechoice)

ces <-
  ces %>%
  filter(votechoice != "ndp") %>%
  filter(votechoice != "Bloc Québécois")%>%
  filter(votechoice != "Green Party") %>%
  filter(votechoice != "People's Party ")%>%
  filter(votechoice != "Another party (please specify)") %>%
  filter(votechoice != "Don't know/ Prefer not to answer")%>%
  filter(sex != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)")%>%
  filter(province != "Yukon")
  
ces <- 
  ces%>%
  select(age,sex,province,education,votechoice)

#table(ces$education)
write_csv(ces, "ces.csv")
