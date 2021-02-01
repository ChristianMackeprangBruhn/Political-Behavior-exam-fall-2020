############################################################################################
######################              PLACEBO TESTS                   ########################
############################################################################################
##############################################################################
###############                DATA TIDYING                  #################
##############################################################################
D14_PLACEBO <- D14 %>% 
  mutate(near_border = 0,
         near_border = case_when(depcode == 39 ~ 1, # Hard lockdown side
                                 depcode == 58 ~ 1,
                                 depcode == 60 ~ 1,
                                 depcode == 71 ~ 1,
                                 depcode == 77 ~ 1,
                                 depcode == 78 ~ 1,
                                 depcode == 80 ~ 1,
                                 depcode == 89 ~ 1,
                                 depcode == 91 ~ 1,
                                 depcode == 95 ~ 1,
                                 depcode == 1 ~ 2, # Soft lockdown side
                                 depcode == 3 ~ 2,
                                 depcode == 18 ~ 2,
                                 depcode == 27 ~ 2,
                                 depcode == 28 ~ 2,
                                 depcode == 42 ~ 2,
                                 depcode == 45 ~ 2,
                                 depcode == 69 ~ 2,
                                 depcode == 76 ~ 2,
                                 TRUE ~ as.numeric(near_border)),
         hardlockdown = 0,
         hardlockdown = case_when(depcode == 2 ~ 1,
                                  depcode == 8 ~ 1,
                                  depcode == 10 ~ 1,
                                  depcode == 21 ~ 1,
                                  depcode == 25 ~ 1,
                                  depcode == 39 ~ 1,
                                  depcode == 51 ~ 1,
                                  depcode == 52 ~ 1,
                                  depcode == 54 ~ 1,
                                  depcode == 55 ~ 1,
                                  depcode == 57 ~ 1,
                                  depcode == 58 ~ 1,
                                  depcode == 59 ~ 1,
                                  depcode == 60 ~ 1,
                                  depcode == 62 ~ 1,
                                  depcode == 67 ~ 1,
                                  depcode == 68 ~ 1,
                                  depcode == 70 ~ 1,
                                  depcode == 71 ~ 1,
                                  depcode == 75 ~ 1,
                                  depcode == 77 ~ 1,
                                  depcode == 78 ~ 1,
                                  depcode == 80 ~ 1,
                                  depcode == 88 ~ 1,
                                  depcode == 89 ~ 1,
                                  depcode == 91 ~ 1,
                                  depcode == 92 ~ 1,
                                  depcode == 93 ~ 1,
                                  depcode == 94 ~ 1,
                                  depcode == 95 ~ 1,
                                  TRUE ~ as.numeric(hardlockdown)))

#####################################
#            DATA FOR H1            #
#####################################
D14_PLACEBO_H1 <- D14_PLACEBO %>% 
  group_by(depcode, municode, pollstation) %>% 
  mutate(totalvotes_r1 = sum(personalvotes_r1, na.rm = TRUE),
         totalvotes_r2 = sum(personalvotes_r2, na.rm = TRUE),
         turnout_r1 = totalvotes_r1/registered_r1, na.rm = TRUE,
         turnout_r2 = totalvotes_r2/registered_r2, na.rm = TRUE)

D14_PLACEBO_H1 <- D14_PLACEBO_H1 %>% 
  select(depcode:muniname, pollstation, near_border, hardlockdown, partylist, name, registered_r1:voteshare_r1, turnout_r1, round1, registered_r2:voteshare_r2, turnout_r2, round2)

names(D14_PLACEBO_H1)

# Create D
D <- list()

#start for loop
start_col <- 9
for(i in 1:2) {
  
  cols <- c(1:8, start_col:(start_col+6))
  D[[i]] <- D14_PLACEBO_H1[, cols]
  
  names(D[[i]])[9:15] <- letters[1:7]
  
  start_col <- start_col + 7
}

D14_P_H1 <- do.call(rbind, D)

D14_P_H1 <- D14_P_H1 %>% 
  rename(registered14 = a,
         submitted14 = b,
         totalvotes14 = c,
         personalvotes14 = d,
         voteshare14 = e,
         turnout14 = f,
         round = g) %>% 
  arrange(municode, depcode)

D14_P_H1 <- D14_P_H1 %>% 
  filter(!is.na(personalvotes20))


#####################################
#          DATA FOR H2-H3b          #
#####################################
D14_PLACEBO_R <- D14_PLACEBO %>% 
  filter(!is.na(personalvotes_r2))

D14_PLACEBO_R <- D14_PLACEBO_R %>% 
  select(depcode:muniname, pollstation, near_border:hardlockdown, partylist:name, registered_r1:voteshare_r1, round1, registered_r2:voteshare_r2, round2)

names(D14_PLACEBO_R)

# Create D
D <- list()

#start for loop
start_col <- 9
for(i in 1:2) {
  
  cols <- c(1:8, start_col:(start_col+5))
  D[[i]] <- D14_PLACEBO_R[, cols]
  
  names(D[[i]])[9:14] <- letters[1:6]
  
  start_col <- start_col + 6
  
}

D14_P_R <- do.call(rbind, D)

D14_P_R <- D14_P_R %>% 
  rename(registered14 = a,
         submitted14 = b,
         totalvotes14 = c,
         personalvotes14 = d,
         voteshare14 = e,
         round = f) %>% 
  arrange(municode, depcode)

D14_P_R <- D14_P_R %>% 
  left_join(incumbent, by = c("depcode", "municode", "name")) %>% 
  mutate(incumbent = ifelse(is.na(incumbent), 0, incumbent))

##############################################################################
###############                 REGRESSIONS                  #################
##############################################################################
#####################################
#            H1 TURNOUT             #
#####################################
H1_P <- D14_P_H1 %>% 
  distinct(depcode, round, pollstation, municode, muniname, .keep_all = TRUE)  %>% 
  filter(near_border != 0)

lm1_P <- felm(turnout14 ~ round * hardlockdown
            |0 #no FE
            |0 #no IV
            |depcode, #clustered SE
            data = H1_P)

lm1_fe_P <- felm(turnout14 ~ round * hardlockdown
               |factor(municode) #FE
               |0 #no IV
               |depcode, #clustered SE
               data = H1_P)


summary(lm1_P)
summary(lm1_fe_P)

stargazer(lm1_P, lm1_fe_P,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for green parties"),
          dep.var.labels = c("Voteshare for green parties"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          omit = c("voteshare*", "Constant", "depname*"),
          add.lines = list(c("Municipal fixed effects",                          "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#####################################
#            H2 INCUMBENT           #
#####################################
H2_P <- D14_P_R %>%
  filter(near_border != 0,
         incumbent == 1)

lm2_P <- felm(voteshare14 ~ round * hardlockdown
            |0 #no FE
            |0 #no IV
            |municode, #clustered SE
            data = H2_P)

lm2_fe_P <- felm(voteshare14 ~ round * hardlockdown
               |factor(municode) #FE
               |0 #no IV
               |municode, #clustered SE
               data = H2_P)

summary(lm2_P)
summary(lm2_fe_P)

stargazer(lm2_P, lm2_fe_P,
          type = "latex", 
          model.names = FALSE,
          title= c("The effect of hard lockdown on vote share for green parties"),
          dep.var.labels = c("Voteshare for green parties"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          omit = c("voteshare*", "Constant", "depname*"),
          add.lines = list(c("Municipal fixed effects",                          "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#####################################
#          H3a Conservative         #
#####################################
H3a_P <- D14_P_R %>% 
  filter(partylist %in% c("LUD", "LLR", "LDVD", "LDLF"),
         near_border != 0)

lm3a_P <- felm(voteshare14 ~ round * hardlockdown
             |0 #no FE
             |0 #no IV
             |municode, #clustered SE
             data = H3a_P)

lm3a_fe_P <- felm(voteshare14 ~ round * hardlockdown
                |factor(municode) # FE
                |0 #no IV
                |municode, #clustered SE
                data = H3a_P)

summary(lm3a_P)
summary(lm3a_fe_P)

stargazer(lm3a_P, lm3a_fe_P,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for conservative parties"),
          dep.var.labels = c("Voteshare for conservative parties"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Municipal fixed effects",                                 "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


#####################################
#         H3b Green Parties         #
#####################################
H3b_P <- D14_P_R %>% 
  filter(partylist %in% c("LVEC", "LEGO"), 
         near_border != 0)

lm3b_P <- felm(voteshare14 ~ round * hardlockdown 
             |0 #no FE
             |0 #no IV
             |municode, #clustered SE
             data = H3b_P)

lm3b_fe_P <- felm(voteshare14 ~ round * hardlockdown
                |factor(municode) #no FE
                |0 #no IV
                |municode, #clustered SE
                data = H3b_P)

summary(lm3b_P)
summary(lm3b_fe_P)

stargazer(lm3b_P, lm3b_fe_P,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for green parties"),
          dep.var.labels = c("Voteshare for green parties"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          omit = c("voteshare*", "Constant", "depname*"),
          add.lines = list(c("Municipal fixed effects",                          "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


##################################################################################################
###############                 REGRESSION DISCONTINUITY DESIGN                  #################
##################################################################################################
library(haven)
library(foreign)
library(rdrobust)
library(tidyverse)
library(geosphere)
library(fields)
library(data.table)
library(rdd)

##########################################
###       SET POINTS ON BORDER         ###
##########################################
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

points$ID<-c(1:151)

################################################
###       DATA WRANGLING AND LOADING         ###
################################################
RED <- D20 %>%
  mutate(voteshare = personalvotes20_r2/totalvotes20_r2)

RED_geo <- RED %>% 
  select(id, latitude, longitude, hardlockdown)

combinations <- CJ(RED_geo$id, points$ID)
names(combinations) <- c("id", "ID")

geo <- combinations %>% 
  left_join(RED_geo, by = "id")

geo <- geo %>% 
  left_join(points, by = "ID") %>% 
  rename(RED_latitude = latitude.x,
         RED_longitude = longitude.x,
         points_latitude = latitude.y,
         points_longitude = longitude.y)

#########################################
###       CALCULATE DISTANCES         ###
#########################################
geo[, distance := distHaversine(matrix(c(RED_longitude, RED_latitude), ncol = 2),
                                matrix(c(points_longitude, points_latitude), ncol = 2))]

geodata <- geo %>% 
  group_by(id) %>% 
  summarize(distance = min(distance),
            hardlockdown = unique(hardlockdown))

geodata <- geodata %>% 
  mutate(distance = ifelse(hardlockdown == 0, distance * -1, distance))


RED_geo <- RED %>% 
  left_join(geodata, by = "id") %>%
  select(-hardlockdown.y) %>%
  rename(hardlockdown = hardlockdown.x)

#####################################
#          H1-H3 VOTESHARE          #
#####################################
## 2014 ELECTION
H123_14 <- D14 %>% 
  group_by(depcode, municode, partylist) %>% 
  mutate(totalvotes_r1 = sum(totalvotes_r1),
         totalvotes_r2 = sum(totalvotes_r2),
         personalvotes_r1 = sum(personalvotes_r1),
         personalvotes_r2 = sum(personalvotes_r2),
         voteshare14_r1 = personalvotes_r1 / totalvotes_r1,
         voteshare14_r2 = personalvotes_r2 / totalvotes_r2,
         voteshare14_r2 = ifelse(is.na(voteshare14_r2), voteshare14_r1, voteshare14_r2)) %>% 
  ungroup() %>% 
  select(depcode:municode, partylist, voteshare14_r1, voteshare14_r2, -depname) %>% 
  distinct(depcode, municode, partylist, .keep_all = TRUE)

## 2008 ELECTION
H123_08 <- D08 %>% 
  group_by(depcode, municode, partylist) %>% 
  mutate(totalvotes_r1 = sum(totalvotes_r1),
         totalvotes_r2 = sum(totalvotes_r2),
         personalvotes_r1 = sum(personalvotes_r1),
         personalvotes_r2 = sum(personalvotes_r2),
         voteshare08_r1 = personalvotes_r1 / totalvotes_r1,
         voteshare08_r2 = personalvotes_r2 / totalvotes_r2,
         voteshare08_r2 = ifelse(is.na(voteshare08_r2), voteshare08_r1, voteshare08_r2)) %>% 
  ungroup() %>% 
  select(depcode:municode, partylist, voteshare08_r1, voteshare08_r2, -depname) %>% 
  distinct(depcode, municode, partylist, .keep_all = TRUE)

#####################################
#            H4 TURNOUT             #
#####################################
## 2014 ELECTION
H4_14 <- D14 %>% 
  distinct(depcode, municode, .keep_all = TRUE) %>% 
  select(-depname, -muniname,-partylist:-list, -submitted_r1, -personalvotes_r1, -voteshare_r1, -submitted_r2, -personalvotes_r2:-seats_r2) %>%
  group_by(depcode, municode) %>% 
  mutate(totalvotes_r1 = sum(totalvotes_r1),
         totalvotes_r2 = sum(totalvotes_r2),
         registered_r1 = sum(registered_r1),
         registered_r2 = sum(registered_r2),
         turnout14_r1 = totalvotes_r1/registered_r1,
         turnout14_r2 = totalvotes_r2/registered_r2,
         turnout14_r2 = ifelse(is.na(turnout14_r2), turnout14_r1, turnout14_r2)) %>% 
  select(-registered_r1:-totalvotes_r2)

## 2008 ELECTION
H4_08 <- D08 %>% 
  distinct(depcode, municode, .keep_all = TRUE) %>% 
  select(-depname, -muniname,-partylist:-list, -submitted_r1, -personalvotes_r1, -voteshare_r1, -submitted_r2, -personalvotes_r2:-seats_r2) %>%
  mutate(totalvotes_r1 = sum(totalvotes_r1),
         totalvotes_r2 = sum(totalvotes_r2),
         registered_r1 = sum(registered_r1),
         registered_r2 = sum(registered_r2),
         turnout08_r1 = totalvotes_r1/registered_r1,
         turnout08_r2 = totalvotes_r2/registered_r2,
         turnout08_r2 = ifelse(is.na(turnout08_r2), turnout08_r1, turnout08_r2)) %>% 
  select(-registered_r1:-totalvotes_r2)

###########################################################
#            CREATE HYPOTHESIS VARIABLES: H1-H3           #
###########################################################
#H1
H1_RDD <- RED_geo %>% 
  filter(incumbent == 1)

H1_RDD <- H1_RDD %>% 
  left_join(H123_14, by = c("depcode", "municode", "partylist"))


#H2
H2_RDD <- RED_geo %>% 
  filter(partylist %in% c("LLR", "LUD", "LDVD", "LDLF"))

H2_14 <- H123_14 %>% 
  filter(partylist %in% c("LLR", "LUD", "LDVD", "LDLF"))

H2_08 <- H123_08 %>% 
  filter(partylist %in% c("LLR", "LUD", "LDVD", "LDLF"))

H2_RDD <- H2_RDD %>% 
  left_join(H2_14, by = c("depcode", "municode", "partylist")) 
H2_RDD <- H2_RDD %>% 
  left_join(H2_08, by = c("depcode", "municode", "partylist"))

#H3
H3_RDD <- RED_geo %>% 
  filter(partylist %in% c("LVEC", "LECO"))

H3_14 <- H123_14 %>% 
  filter(partylist %in% c("LVEC", "LECO"))

H3_08 <- H123_08 %>% 
  filter(partylist %in% c("LVEC", "LECO"))

H3_RDD <- H3_RDD %>% 
  left_join(H3_14, by = c("depcode", "municode", "partylist")) 
H3_RDD <- H3_RDD %>% 
  left_join(H3_08, by = c("depcode", "municode", "partylist"))

########################################################
#            CREATE HYPOTHESIS VARIABLES: H4           #
########################################################
D20_unrestricted <- read_csv("D20_unrestricted.csv")

UND_geo <- D20_unrestricted %>% 
  select(id, latitude, longitude, hardlockdown)

combinations <- CJ(UND_geo$id, points$ID)
names(combinations) <- c("id", "ID")

geo <- combinations %>% 
  left_join(UND_geo, by = "id")

geo <- geo %>% 
  left_join(points, by = "ID") %>% 
  rename(UND_latitude = latitude.x,
         UND_longitude = longitude.x,
         points_latitude = latitude.y,
         points_longitude = longitude.y)

geo[, distance := distHaversine(matrix(c(UND_longitude, UND_latitude), ncol = 2),
                                matrix(c(points_longitude, points_latitude), ncol = 2))]

geodata <- geo %>% 
  group_by(id) %>% 
  summarize(distance = min(distance),
            hardlockdown = unique(hardlockdown))

geodata <- geodata %>% 
  mutate(distance = ifelse(hardlockdown == 0, distance * -1, distance))

H4_RDD <- D20_unrestricted %>% 
  left_join(geodata, by = "id") %>%
  select(-hardlockdown.y) %>%
  rename(hardlockdown = hardlockdown.x)

#Only observations who move on to 2nd round
H4_RDD <- H4_RDD %>% filter(round2 == "1")

#Merge
H4_RDD <- H4_RDD %>%
  left_join(H4_14, by = c("depcode", "municode"))
H4_RDD <- H4_RDD %>%
  left_join(H4_08, by = c("depcode", "municode"))


######################################
###         RUN RDD: H1-H3         ###
######################################
rd_modelH1_1_p1 <- rdrobust(H1_RDD$voteshare, H1_RDD$distance, c = 0, p = 1, kernel = "triangular")
rd_modelH1_1_p2 <- rdrobust(H1_RDD$voteshare, H1_RDD$distance, c = 0, p = 2, kernel = "triangular")
rd_modelH1_1_p3 <- rdrobust(H1_RDD$voteshare, H1_RDD$distance, c = 0, p = 3, kernel = "triangular")
rd_modelH1_1_p4 <- rdrobust(H1_RDD$voteshare, H1_RDD$distance, c = 0, p = 4, kernel = "triangular")
summary(rd_modelH1_1_p1)
summary(rd_modelH1_1_p2)
summary(rd_modelH1_1_p3)
summary(rd_modelH1_1_p4)

rd_modelH2_1 <- rdrobust(H2_RDD$voteshare, H2_RDD$distance, c = 0, p = 1, kernel = "triangular")
rd_modelH2_2 <- rdrobust(H2_RDD$voteshare, H2_RDD$distance, c = 0, p = 2, kernel = "triangular")
rd_modelH2_3 <- rdrobust(H2_RDD$voteshare, H2_RDD$distance, c = 0, p = 3, kernel = "triangular")
rd_modelH2_4 <- rdrobust(H2_RDD$voteshare, H2_RDD$distance, c = 0, p = 4, kernel = "triangular")
summary(rd_modelH2_1)
summary(rd_modelH2_2)
summary(rd_modelH2_3)
summary(rd_modelH2_4)

rd_modelH3_1 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance, c = 0, p = 1, kernel = "triangular")
rd_modelH3_2 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance, c = 0, p = 2, kernel = "triangular")
rd_modelH3_3 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance, c = 0, p = 3, kernel = "triangular")
rd_modelH3_4 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance, c = 0, p = 4, kernel = "triangular")
summary(rd_modelH3_1)
summary(rd_modelH3_2)
summary(rd_modelH3_3)
summary(rd_modelH3_4)

###################################
###         RUN RDD: H4         ###
###################################
rd_modelH4_1 <- rdrobust(H4_RDD$turnout20_r2, H4_RDD$distance, c = 0, p = 1, kernel = "triangular")
rd_modelH4_2 <- rdrobust(H4_RDD$turnout20_r2, H4_RDD$distance, c = 0, p = 2, kernel = "triangular")
rd_modelH4_3 <- rdrobust(H4_RDD$turnout20_r2, H4_RDD$distance, c = 0, p = 3, kernel = "triangular")
rd_modelH4_4 <- rdrobust(H4_RDD$turnout20_r2, H4_RDD$distance, c = 0, p = 4, kernel = "triangular")
summary(rd_modelH4_1)
summary(rd_modelH4_2)
summary(rd_modelH4_3)
summary(rd_modelH4_4)

###########################################
###         COVARIATES INCLUDED         ###
###########################################
##H1
H1_RDDa <- H1_RDD %>%
  mutate(voteshare14 = ifelse(is.na(voteshare14_r2),voteshare14_r1,voteshare14_r2))

H1RDDc1 <- rdrobust(H1_RDDa$voteshare, H1_RDDa$distance,
                    covs=cbind(H1_RDDa$voteshare14),
                    c = 0, p = 1, kernel = "triangular")
H1RDDc2 <- rdrobust(H1_RDDa$voteshare, H1_RDDa$distance,
                    covs=cbind(H1_RDDa$voteshare14),
                    c = 0, p = 2, kernel = "triangular")
H1RDDc3 <- rdrobust(H1_RDDa$voteshare, H1_RDDa$distance,
                    covs=cbind(H1_RDDa$voteshare14),
                    c = 0, p = 3, kernel = "triangular")
H1RDDc4 <- rdrobust(H1_RDDa$voteshare, H1_RDDa$distance,
                    covs=cbind(H1_RDDa$voteshare14),
                    c = 0, p = 4, kernel = "triangular")

summary(H1RDDc1)
summary(H1RDDc2)
summary(H1RDDc3)
summary(H1RDDc4)

##H2
H2_RDDa <- H2_RDD %>%
  mutate(voteshare14 = ifelse(is.na(voteshare14_r2),voteshare14_r1,voteshare14_r2))

H2RDDc1 <- rdrobust(H2_RDDa$voteshare, H2_RDDa$distance,
                    covs=cbind(H2_RDDa$voteshare14),
                    c = 0, p = 1, kernel = "triangular")
H2RDDc2 <- rdrobust(H2_RDDa$voteshare, H2_RDDa$distance,
                    covs=cbind(H2_RDDa$voteshare14),
                    c = 0, p = 2, kernel = "triangular")
H2RDDc3 <- rdrobust(H2_RDDa$voteshare, H2_RDDa$distance,
                    covs=cbind(H2_RDDa$voteshare14),
                    c = 0, p = 3, kernel = "triangular")
H2RDDc4 <- rdrobust(H2_RDDa$voteshare, H2_RDDa$distance,
                    covs=cbind(H2_RDDa$voteshare14),
                    c = 0, p = 4, kernel = "triangular")

summary(H2RDDc1)
summary(H2RDDc2)
summary(H2RDDc3)
summary(H2RDDc4)

##H3
H3_RDDa <- H3_RDD %>%
  mutate(voteshare14 = ifelse(is.na(voteshare14_r2),voteshare14_r1,voteshare14_r2))

H3RDDc1 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance,
                    covs=cbind(H3_RDD$voteshare14_r2),
                    c = 0, p = 1, kernel = "triangular")
H3RDDc2 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance,
                    covs=cbind(H3_RDD$voteshare14_r2),
                    c = 0, p = 2, kernel = "triangular")
H3RDDc3 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance,
                    covs=cbind(H3_RDD$voteshare14_r2),
                    c = 0, p = 3, kernel = "triangular")
H3RDDc4 <- rdrobust(H3_RDD$voteshare, H3_RDD$distance,
                    covs=cbind(H3_RDD$voteshare14_r2),
                    c = 0, p = 4, kernel = "triangular")

summary(H3RDDc1)
summary(H3RDDc2)
summary(H3RDDc3)
summary(H3RDDc4)

##H4
H4_RDDa <- H4_RDD %>%
  mutate(turnout14 = ifelse(is.na(turnout14_r2),turnout14_r1,turnout14_r2))

H4RDDc1 <- rdrobust(H4_RDDa$turnout20_r2, H4_RDDa$distance,
                    covs=cbind(H4_RDDa$turnout14),
                    c = 0, p = 1, kernel = "triangular")
H4RDDc2 <- rdrobust(H4_RDDa$turnout20_r2, H4_RDDa$distance,
                    covs=cbind(H4_RDDa$turnout14),
                    c = 0, p = 2, kernel = "triangular")
H4RDDc3 <- rdrobust(H4_RDDa$turnout20_r2, H4_RDDa$distance,
                    covs=cbind(H4_RDDa$turnout14),
                    c = 0, p = 3, kernel = "triangular")
H4RDDc4 <- rdrobust(H4_RDDa$turnout20_r2, H4_RDDa$distance,
                    covs=cbind(H4_RDDa$turnout14),
                    c = 0, p = 4, kernel = "triangular")
summary(H4RDDc1)
summary(H4RDDc2)
summary(H4RDDc3)
summary(H4RDDc4)



