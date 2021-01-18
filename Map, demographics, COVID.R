########################################################################
#                          1) MAP OF THE BORDER                        #
########################################################################

#rm(list=ls())
library(haven)
library(readxl)
library(tidyverse)
library(maps)
library(ggplot2)

setwd("/Users/christianmackeprangbruhn/Documents/Kort/")

###############################################
#              BORDER POINTS                  #
###############################################
points <- data.frame(latitude = c(50.0520733,50.0558373,50.0239089,49.9871623,49.930885,49.883920,49.833483,49.784090,49.701787,49.644301,
                                  49.544065,49.504552,49.463007,49.409166,49.360329,49.292794,49.273750,49.262534,49.224137,49.214257,
                                  49.203884,49.180399,49.157304,49.133990,49.085928,49.069976,49.018014,48.952706,48.901701,48.860780,
                                  48.7407024,48.738873,48.695479,48.616093,48.592374,48.514006,48.449350,48.453825,48.402005,48.361434,
                                  48.287031,48.335127,48.331394,48.299219,48.171791,48.131948,48.153624,48.158973,48.055411,47.937035,
                                  47.799598,47.592859,47.520159,47.437777,47.338866,47.279868,47.175527,47.123020,47.045963,46.960585,
                                  46.936576,46.899692,46.816017,46.759373,46.727178,46.680936,46.716675,46.699179,46.726044,46.631176,
                                  46.557875,46.523955,46.492218,46.467429,46.379652,46.327465,46.315392,46.251963,46.187108,46.194626,
                                  46.185396,46.189409,46.220398,46.237473,46.297219,46.270650,46.252291,46.176224,46.188781,46.240354,
                                  46.291631,46.339184,46.405275,46.448926,46.509978,46.499702,46.504983,46.495498,46.456187,46.436534,
                                  46.402849,46.382522,46.342590,46.345389,46.318848,46.271043,46.268401,46.293609,46.330550,46.310142,
                                  46.264699,46.266613,46.263506,46.294026,46.330457,46.353166,46.379970,46.396856,46.417933,48.163732,
                                  48.165365,48.154425,48.145072,48.135296,48.130688,48.122776,48.115964,48.116208,48.107626,48.099104,
                                  48.093664,48.064281,48.035561,48.019310,47.968939,47.929267,47.907208,47.895820,47.813278,48.295160,
                                  47.778849,47.769608,47.761396,47.743156,47.717357,47.716508,47.708353,47.696137,47.647874,48.256293,
                                  48.172106),
                     longitude = c(1.4281215,1.448014,1.4832451,1.5480692,1.642615,1.712284,1.726005,1.758169,1.740262,1.698410,
                                   1.724243,1.788900,1.757947,1.714045,1.761297,1.780650,1.802092,1.763623,1.697845,1.679536,
                                   1.676342,1.671585,1.660577,1.656821,1.623412,1.527642,1.474504,1.501052,1.545340,1.584120,
                                   1.6275228,1.622676,1.586528,1.672914,1.707234,1.776976,1.845487,1.923280,1.970411,1.984692,
                                   2.016857,2.194163,2.326677,2.420989,2.513742,2.590928,2.752448,2.886385,3.090255,3.081228,
                                   3.027763,2.946917,2.875486,2.931544,2.870431,2.956333,3.010053,3.029006,3.063927,3.079762,
                                   3.065221,3.059433,3.043940,3.049682,3.130529,3.197658,3.300585,3.431585,3.598488,3.718664,
                                   3.738222,3.846273,3.935273,3.996196,3.984462,4.004450,3.948902,3.907916,3.977629,4.084478,
                                   4.258474,4.372203,4.387544,4.391968,4.415075,4.536199,4.689159,4.747585,4.780386,4.809027,
                                   4.831204,4.852972,4.889356,4.896806,4.944886,5.097898,5.167048,5.204530,5.254566,5.317294,
                                   5.330212,5.378047,5.388515,5.417346,5.444463,5.469030,5.535088,5.575114,5.627897,5.692901,
                                   5.719681,5.783527,5.843947,5.916827,5.957942,5.978038,6.017454,6.039328,6.060127,2.936275,
                                   2.953040,2.980001,3.003899,3.021953,3.026487,3.020201,3.018268,3.035012,3.037276,3.038363,
                                   3.049315,3.064297,3.106651,3.100217,3.126927,3.064104,3.037992,3.007757,3.024132,2.060238,
                                   2.971902,2.914312,2.862934,2.855890,2.848548,2.871819,2.882269,2.903598,2.953554,2.467975,
                                   2.513067))
write.csv(points,file="border_points.csv")

##########################################
#              MAP DATA                  #
##########################################

map_data("france")
map <- map_data("france")

map <- map %>%
  mutate(hardlockdown = case_when(group == 5 ~ 1, #hard
                                  group == 7 ~ 1,
                                  group == 31 ~ 1,
                                  group == 43 ~ 1,
                                  group == 56 ~ 1,
                                  group == 15 ~ 1,
                                  group == 33 ~ 1,
                                  group == 11 ~ 1,
                                  group == 10 ~ 1,
                                  group == 12 ~ 1,
                                  group == 51 ~ 1,
                                  group == 1 ~ 1,
                                  group == 3 ~ 1,
                                  group == 8 ~ 1,
                                  group == 2 ~ 1,
                                  group == 19 ~ 1,
                                  group == 40 ~ 1,
                                  group == 44 ~ 1,
                                  group == 47 ~ 1,
                                  group == 52 ~ 1,
                                  group == 59 ~ 1,
                                  group == 24 ~ 2,
                                  group == 18 ~ 1,
                                  group == 17 ~ 1,
                                  group == 35 ~ 1,
                                  group == 38 ~ 1,
                                  group == 28 ~ 1,
                                  group == 22 ~ 2,
                                  group == 20 ~ 2,
                                  group == 26 ~ 2,
                                  group == 16 ~ 1),
         hardlockdown = replace(hardlockdown, is.na(hardlockdown), 0)) #soft

map <- map %>%
  mutate(border = case_when(group == 56 ~ 1, #red
                            group == 51 ~ 1,
                            group == 8 ~ 1,
                            group == 18 ~ 1,
                            group == 17 ~ 1,
                            group == 38 ~ 1,
                            group == 28 ~ 1,
                            group == 16 ~ 1,
                            group == 3 ~ 1,
                            group == 59 ~ 1,
                            group == 65 ~ 2, #green
                            group == 63 ~ 2,
                            group == 50 ~ 2,
                            group == 13 ~ 2,
                            group == 23 ~ 2,
                            group == 73 ~ 2,
                            group == 39 ~ 2,
                            group == 70 ~ 2,
                            group == 6 ~ 2,
                            group == 22 ~ 3, #paris
                            group == 20 ~ 3,
                            group == 26 ~ 3,
                            group == 24 ~ 3,),
         border = replace(border, is.na(border),0)) #everything else

#Remove Corsica
map_clean <- subset(map, region != "Haute-Corse" & region!="Corse du Sud")

############################################
#              PLOT MAP 1                  #
############################################
ggplot(map_clean, aes(x=long, y=lat, group=group, fill=as.factor(hardlockdown))) +
  geom_polygon(color="black", size=.5) +
  scale_fill_manual(name = "Lockdown areas",
                    labels = c("Lockdown until May 11", "Lockdown until June 2", "Lockdown until June 22"),
                    values=c("0"="darkgreen", "1"="darkred", "2"="yellow")) +
  coord_map() +
  theme_void() +
  ggsave("areas.png")

############################################
#              PLOT MAP 2                  #
############################################
ggplot(map_clean, aes(x=long, y=lat, group=group, fill=as.factor(hardlockdown))) +
  geom_polygon(color="black", size=.5) +
  scale_fill_manual(name = "Lockdown areas",
                    labels = c("Lockdown until May 11", "Lockdown until June 2", "Lockdown until June 22"),
                    values=c("0"="darkgreen", "1"="darkred", "2"="yellow")) +
  coord_map() +
  theme_void() +
  geom_point(data = points, aes(x = longitude, y = latitude), inherit.aes = FALSE, size = 1, col = "blue") +
  ggsave("areas_points.png")

############################################
#              PLOT MAP 3                  #
############################################
ggplot(map_clean, aes(x=long, y=lat, group=group, fill=as.factor(border))) +
  geom_polygon(color="black", size=.5) +
  scale_fill_manual(name = "Lockdown areas",
                    labels = c("Lockdown until May 11", "Lockdown until June 2", "Lockdown until June 22"),
                    values=c("0"="darkgreen", "1"="darkred", "2"="yellow")) +
  coord_map() +
  theme_void() +
  ggsave("border.png")

############################################
#              PLOT MAP 4                  #
############################################
ggplot(map_clean, aes(x=long, y=lat, group=group, fill=as.factor(border))) +
  geom_polygon(color="black", size=.5) +
  scale_fill_manual(name = "Lockdown areas",
                    labels = c("Non-border", "Lockdown until June 2", "Lockdown until May 11", "Lockdown until June 22"),
                    values=c("0"="grey95", "2"="darkgreen", "1"="darkred", "3"="yellow")) +
  coord_map() +
  theme_void() +
  geom_point(data = points, aes(x = longitude, y = latitude),
             inherit.aes = FALSE, size = 1, col = "white") +
  ggsave("border_points.png")

############################################################################
#                          2) DEMOGRAPHIC VARIABLES                        #
############################################################################
#rm(list=ls())
library(haven)
library(readxl)
library(tidyverse)
library(xtable)
library(tidyr)
library(dplyr)

#Data found at: https://statistiques-locales.insee.fr/#c=indicator&view=map2

controlvars <- read_xlsx("/Users/christianmackeprangbruhn/Documents/Control variables/controlvariables_dep.xlsx")
#############################################
#              ADD BORDERS                  #
#############################################
a <- controlvars %>% 
  mutate(near_border = 0,
         near_border = case_when(Code == 39 ~ 1, # Hard lockdown side
                                 Code == 58 ~ 1,
                                 Code == 60 ~ 1,
                                 Code == 71 ~ 1,
                                 Code == 77 ~ 1,
                                 Code == 78 ~ 1,
                                 Code == 80 ~ 1,
                                 Code == 89 ~ 1,
                                 Code == 91 ~ 1,
                                 Code == 95 ~ 1,
                                 Code == 75 ~ 1,
                                 Code == "01" ~ 2, # Soft lockdown side
                                 Code == "03" ~ 2,
                                 Code == 18 ~ 2,
                                 Code == 27 ~ 2,
                                 Code == 28 ~ 2,
                                 Code == 42 ~ 2,
                                 Code == 45 ~ 2,
                                 Code == 69 ~ 2,
                                 Code == 76 ~ 2,
                                 TRUE ~ as.numeric(near_border)))

b <- a %>% filter(near_border %in% c("1", "2"))

################################################
#              DATA WRANGLING                  #
################################################
b <- b %>% rename(population = "Population estimates 2020",
                  little_or_no_diploma = "Little or no diploma",
                  primary = "BEPC or patent (primary)",
                  pre_secondary = "CAP or BEP (pre-secondary)",
                  secondary = "BAC (secondary)",
                  higher_edu = "Diploma - higher education",
                  number_of_educated = "Number of people out of school aged 15 or over",
                  under_25years = "Under 25 years",
                  between25_64years = "Between 25 and 64 years",
                  over_65years = "65 years or more",
                  average_hourly_salary = "Average hourly net salary",
                  unemployment_rateQ2 = "Quarterly unemployment rate 2020-Q2",
                  annual_unemployment_rate2017 = "Average annual unemployment rate 2017",
                  standard_of_living2017 = "Median standard of living 2017",
                  poverty_rate2017 = "Poverty rate 2017")

c <- transform(b, little_or_no_diploma = as.numeric(little_or_no_diploma),
               primary = as.numeric(primary),
               pre_secondary = as.numeric(pre_secondary),
               secondary = as.numeric(secondary),
               higher_edu = as.numeric(higher_edu),
               number_of_educated = as.numeric(number_of_educated),
               under_25years = as.numeric(under_25years),
               between25_64years = as.numeric(between25_64years),
               over_65years = as.numeric(over_65years))

c <- c %>%
  mutate(little_or_no_diploma = little_or_no_diploma/number_of_educated,
         primary = primary/number_of_educated,
         pre_secondary = pre_secondary/number_of_educated,
         secondary = secondary/number_of_educated,
         higher_edu = higher_edu/number_of_educated,
         under_25years = under_25years/population,
         between25_64years = between25_64years/population,
         over_65years = over_65years/population)

c <- transform(c, Code = as.numeric(Code),
               average_hourly_salary = as.numeric(average_hourly_salary),
               near_border = as.numeric(near_border),
               unemployment_rateQ2 = as.numeric(unemployment_rateQ2),
               annual_unemployment_rate2017 = as.numeric(annual_unemployment_rate2017),
               standard_of_living2017 = as.numeric(standard_of_living2017),
               poverty_rate2017 = as.numeric(poverty_rate2017))

####################################################
#              CALCULATE AVERAGES                  #
####################################################
#Economic variables
economic <- c %>%
  group_by(near_border) %>%
  mutate("Average hourly salary" = sum(average_hourly_salary)/n(),
         "Unemployment rate 2020-Q2" = sum(unemployment_rateQ2)/n(),
         "Standard of living 2017" = sum(standard_of_living2017)/n(),
         "Poverty rate 2017" = sum(poverty_rate2017)/n())

econ_selected <- economic %>% select(near_border, "Average hourly salary", "Unemployment rate 2020-Q2", "Standard of living 2017", "Poverty rate 2017")
econ_selected <- econ_selected %>% distinct()
econ_selected <- econ_selected %>% arrange(near_border)
xtable(econ_selected)

#Demographic variables
demographic <- c %>%
  group_by(near_border) %>%
  mutate("<25 years" = sum(under_25years)/n(),
         "25-64 years" = sum(between25_64years)/n(),
         ">65 years" = sum(over_65years)/n())

demo_selected <- demographic %>% select(near_border, "<25 years", "25-64 years", ">65 years")
demo_selected <- demo_selected %>% distinct()
demo_selected <- demo_selected %>% arrange(near_border)
xtable(demo_selected)

#Education vairables
education <- c %>%
  group_by(near_border) %>%
  mutate("No diploma" = sum(little_or_no_diploma)/n(),
         Primary = sum(primary)/n(),
         Pre_secondary = sum(pre_secondary)/n(),
         Secondary = sum(secondary)/n(),
         "Higher education" = sum(higher_edu)/n())

edu_selected <- education %>% select(near_border, "No diploma", Primary, Pre_secondary, Secondary, "Higher education")
edu_selected <- edu_selected %>% distinct()
edu_selected <- edu_selected %>% arrange(near_border)
xtable(edu_selected)

###########################################################
#              ADD ALL VARIABLES TOGETHER                 #
###########################################################
#(X, Y) = Var1 i hård, Var2 i blød

all <- c %>%
  group_by(near_border) %>%
  mutate("Count" = n())

all <- all %>%
  group_by(near_border) %>%
  mutate(Average_hourly_salary = sum(average_hourly_salary)/n(),
         Unemployment_rate_2020_Q2 = sum(unemployment_rateQ2)/n(),
         Standard_of_living_2017 = sum(standard_of_living2017)/n(),
         Poverty_rate_2017 = sum(poverty_rate2017)/n(),
         Below_25 = sum(under_25years)/n(),
         Between_25_64 = sum(between25_64years)/n(),
         Over_65 = sum(over_65years)/n(),
         No_diploma = sum(little_or_no_diploma)/n(),
         Primary = sum(primary)/n(),
         Pre_secondary = sum(pre_secondary)/n(),
         Secondary = sum(secondary)/n(),
         Higher_education = sum(higher_edu)/n())

all_selected <- all %>% select(near_border, Average_hourly_salary, Unemployment_rate_2020_Q2, Standard_of_living_2017, Poverty_rate_2017,
                               near_border, Below_25, Between_25_64, Over_65,
                               near_border, No_diploma, Pre_secondary, Primary, Secondary, Higher_education)

all_selected <- all_selected %>% distinct()
all_selected <- all_selected %>% arrange(near_border)

############################################
#                  T-TESTS                 #
############################################
names(c)

t.test(average_hourly_salary ~ near_border, data=c)

t.test(unemployment_rateQ2 ~ near_border, data=c)

t.test(poverty_rate2017 ~ near_border, data=c)

t.test(little_or_no_diploma ~ near_border, data=c)

t.test(primary ~ near_border, data=c)

t.test(pre_secondary ~ near_border, data=c)

t.test(secondary ~ near_border, data=c)

t.test(higher_edu ~ near_border, data=c)

t.test(under_25years ~ near_border, data=c)

t.test(between25_64years ~ near_border, data=c)

t.test(over_65years ~ near_border, data=c)


############################################################################
#                          3) COVID IN FRANCE                              #
############################################################################
#rm(list=ls())
library(ggplot2)
library(tidyverse)
library(dplyr)

################################################
#              INFECTION DATA                  #
################################################
#COVID data for hypotheses section
#Data source: https://www.data.gouv.fr/fr/datasets/donnees-relatives-aux-resultats-des-tests-virologiques-covid-19/

smitte_dep <- read.csv("/Users/christianmackeprangbruhn/Documents/COVID data/sp-pos-quot-dep-2021-01-02-19h20.csv", sep=";")
smitte_dep$dep <- as.numeric(smitte_dep$dep)
smitte_dep <- smitte_dep %>% filter(jour >= "2020-06-21")
smitte_dep <- smitte_dep %>% filter(jour <= "2020-06-28")

smitte_dep <- smitte_dep %>% 
  mutate(near_border = 0,
         near_border = case_when(dep == 39 ~ 1, # Hard lockdown side
                                 dep == 58 ~ 1,
                                 dep == 60 ~ 1,
                                 dep == 71 ~ 1,
                                 dep == 77 ~ 1,
                                 dep == 78 ~ 1,
                                 dep == 80 ~ 1,
                                 dep == 89 ~ 1,
                                 dep == 91 ~ 1,
                                 dep == 95 ~ 1,
                                 dep == 1 ~ 2, # Soft lockdown side
                                 dep == 3 ~ 2,
                                 dep == 18 ~ 2,
                                 dep == 27 ~ 2,
                                 dep == 28 ~ 2,
                                 dep == 42 ~ 2,
                                 dep == 45 ~ 2,
                                 dep == 69 ~ 2,
                                 dep == 76 ~ 2,
                                 TRUE ~ as.numeric(near_border)))

smitte_dep <- smitte_dep %>% filter(near_border!="0", cl_age90=="0")
smitte_dep2 <- smitte_dep %>% filter(jour == "2020-06-28")

smitte_dep2 <- smitte_dep2 %>%
  group_by(near_border) %>%
  mutate("Population" = sum(pop))

smitte_dep2 <- smitte_dep2 %>% select(near_border, Population)
smitte_dep <- merge(smitte_dep, smitte_dep2, by="near_border")

smitte_dep <- smitte_dep %>% rename(Positive = "P", Test = "T")

A <- smitte_dep %>%
  group_by(near_border) %>%
  mutate("Positives" = sum(Positive),
         "Tests" = sum(Test),
         Percent_positive=Positives/Tests,
         Percent_positive=Percent_positive*100,
         Positive_per_pop=Positives/Population,
         Positive_per_pop=Positive_per_pop*100)

selected <- A %>% select(near_border, Positives, Tests, Percent_positive, Population, Positive_per_pop)
selected <- selected %>% distinct()
selected <- selected %>% arrange(near_border)