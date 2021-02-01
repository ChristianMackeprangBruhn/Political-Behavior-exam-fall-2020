#PACKAGES
library(xtable)
library(lmtest)
library(lfe)
library(stargazer)

####################################################################################################################
####################################################################################################################
##################################                    ANALYSIS                    ##################################
####################################################################################################################
####################################################################################################################
############################################################################################
######################               DIFF-IN-DIFFS                  ########################
############################################################################################
#####################################
#            H1 TURNOUT             #
#####################################
H1 <- D20_DID_U %>% 
  distinct(depcode, round, depname, pollstation, municode, muniname, .keep_all = TRUE)  %>% 
  filter(near_border != 0)

H1 <- H1 %>% 
  left_join(D14_turnout, by = c("depcode", "municode", "pollstation"))

test <- H1 %>% 
  filter(is.na(turnout14)) #Jeg ved ikke, hvorfor de er missing

H1 <- H1 %>% 
  filter(!is.na(turnout14),
         !is.na(round))

lm1 <- felm(turnout20 ~ round * hardlockdown
            |0 #no FE
            |0 #no IV
            |municode, #clustered SE
            data = H1)

lm1_c <- felm(turnout20 ~ round * hardlockdown + turnout14
            |0 #no FE
            |0 #no IV
            |municode, #clustered SE
            data = H1)

lm1_fe <- felm(turnout20 ~ round * hardlockdown
             |factor(municode) #FE
             |0 #no IV
             |municode, #clustered SE
             data = H1)

lm1_c_fe <- felm(turnout20 ~ round * hardlockdown + turnout14
               |factor(municode) #FE
               |0 #no IV
               |municode, #clustered SE
               data = H1) 


summary(lm1)
summary(lm1_c)
summary(lm1_fe)
summary(lm1_c_fe)

#full table
stargazer(lm1, lm1_c, lm1_fe, lm1_c_fe,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on turnout"),
          keep = c("round:hardlockdown"),
          dep.var.labels = c("Turnout"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          column.separate= c(2,2),
          omit = c("voteshare*", "Constant", "turnout14", "depname*"),
          add.lines = list(c("Control for turnout '14",                            "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#full table
stargazer(lm1, lm1_c, lm1_fe, lm1_c_fe,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on turnout"),
          dep.var.labels = c("Turnout"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          column.separate= c(2,2),
          omit = c("voteshare*", "turnout14", "depname*"),
          add.lines = list(c("Control for turnout '14",                            "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#####################################
#            H2 INCUMBENT           #
#####################################
H2 <- D20_DID %>%
  filter(near_border != 0,
         incumbent == 1)

H2 <- H2 %>% 
  left_join(D14_voteshare, by = c("depcode", "municode", "pollstation", "partylist"))

lm2 <- felm(voteshare20 ~ round * hardlockdown
            |0 #no FE
            |0 #no IV
            |municode, #clustered SE
            data = H2)

lm2_c <- felm(voteshare20 ~ round * hardlockdown + voteshare14
              |0 #no FE
              |0 #no IV
              |municode, #clustered SE
              data = H2)

lm2_fe <- felm(voteshare20 ~ round * hardlockdown
               |factor(municode) #FE
               |0 #no IV
               |municode, #clustered SE
               data = H2)

lm2_c_fe <- felm(voteshare20 ~ round * hardlockdown + voteshare14
               |factor(municode) #FE
               |0 #no IV
               |municode, #clustered SE
               data = H2)
summary(lm2)
summary(lm2_c)
summary(lm2_fe)
summary(lm2_c_fe)

stargazer(lm2, lm2_c, lm2_fe, lm2_c_fe,
          type = "text", 
          model.names = FALSE, 
          keep = c("round:hardlockdown"),
          title= c("The effect of hard lockdown on vote share for incumbent party"),
          dep.var.labels = c("Vote share for incumbent party"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Control for vote share for incumbent party '14",      "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                             "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#full table
stargazer(lm2, lm2_c, lm2_fe, lm2_c_fe,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for incumbent party"),
          dep.var.labels = c("Vote share for incumbent party"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          column.separate= c(2,2),
          omit = c("voteshare*", "turnout14", "depname*"),
          add.lines = list(c("Control for turnout '14",                            "No", "Yes", "No", "Yes"),
                 c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


#####################################
#          H3a Conservative         #
#####################################
H3a <- D20_DID %>% 
  filter(partylist %in% c("LUD", "LLR", "LDVD", "LDLF"),
         near_border != 0)

H3a <- H3a %>% 
  left_join(D14_voteshare, by = c("depcode", "municode", "pollstation", "partylist"))

lm3a <- felm(voteshare20 ~ round * hardlockdown
            |0 #no FE
            |0 #no IV
            |municode, #clustered SE
            data = H3a)

lm3a_c <- felm(voteshare20 ~ round * hardlockdown + voteshare14
              |0 #no FE
              |0 #no IV
              |municode, #clustered SE
              data = H3a)

lm3a_fe <- felm(voteshare20 ~ round * hardlockdown
               |factor(municode) # FE
               |0 #no IV
               |municode, #clustered SE
               data = H3a)

lm3a_c_fe <- felm(voteshare20 ~ round * hardlockdown + voteshare14
               |factor(municode) # FE
               |0 #no IV
               |municode, #clustered SE
               data = H3a)

summary(lm3a)
summary(lm3a_c)
summary(lm3a_fe)
summary(lm3a_c_fe)


stargazer(lm3a, lm3a_c, lm3a_fe, lm3a_c_fe,
          type = "latex", 
          model.names = FALSE, 
          keep = c("round:hardlockdown"),
          title= c("The effect of hard lockdown on vote share for conservative parties"),
          dep.var.labels = c("Voteshare for conservative parties"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Control for vote share for conservative parties '14",     "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                                 "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#full table
stargazer(lm3a, lm3a_c, lm3a_fe, lm3a_c_fe,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for conservative parties"),
          dep.var.labels = c("Voteshare for conservative parties"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          column.separate= c(2,2),
          omit = c("voteshare*", "turnout14", "depname*"),
          add.lines = list(c("Control for turnout '14",                            "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))



#####################################
#         H3b Green Parties         #
#####################################
H3b <- D20_DID %>% 
  filter(partylist %in% c("LVEC", "LEGO"), 
         near_border != 0)

H3b <- H3b %>% 
  left_join(D14_voteshare, by = c("depcode", "municode", "pollstation", "partylist"))

lm3b <- felm(voteshare20 ~ round * hardlockdown 
             |0 #no FE
             |0 #no IV
             |municode, #clustered SE
             data = H3b)

lm3b_c <- felm(voteshare20 ~ round * hardlockdown + voteshare14 
               |0 #no FE
               |0 #no IV
               |municode, #clustered SE
               data = H3b)

lm3b_fe <- felm(voteshare20 ~ round * hardlockdown
                |factor(municode) #no FE
                |0 #no IV
                |municode, #clustered SE
                data = H3b)

lm3b_c_fe <- felm(voteshare20 ~ round * hardlockdown + voteshare14
                  |factor(municode) #no FE
                  |0 #no IV
                  |municode, #clustered SE
                  data = H3b)

summary(lm3b)
summary(lm3b_c)
summary(lm3b_fe)
summary(lm3b_c_fe)

stargazer(lm3b, lm3b_c, lm3b_fe, lm3b_c_fe,
          type = "latex", 
          model.names = FALSE, 
          keep = c("round:hardlockdown"),
          title= c("The effect of hard lockdown on vote share for green parties"),
          dep.var.labels = c("Voteshare for green parties"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          add.lines = list(c("Control for vote share for green parties '14",     "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                          "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#full table
stargazer(lm3b, lm3b_c, lm3b_fe, lm3b_c_fe,
          type = "latex", 
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for green parties"),
          dep.var.labels = c("Voteshare for green parties"),
          covariate.labels = c("Post Lockdown", "Hard Lockdown", "Post lockdown $times$ Hard lockdown", "Intercept"),
          column.separate= c(2,2),
          omit = c("voteshare*", "turnout14", "depname*"),
          add.lines = list(c("Control for turnout '14",                            "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


############################################################################################
######################           NATURAL EXPERIMENT APPROACH        ########################
############################################################################################
#####################################
#            H1 TURNOUT             #
#####################################
H1_OLS <- D20_unrestricted %>% 
  mutate(turnout20_diff = turnout20_r2 - turnout20_r1) %>% 
  filter(near_border != 0)

H1_OLS <- H1_OLS %>% 
  left_join(D14_turnout, by = c("depcode", "municode", "pollstation"))

lm1_ols <- felm(turnout20_r2 ~ hardlockdown
                |0 #no FE
                |0 #no IV
                |municode, #clustered SE
                data = H1_OLS)

lm1_c_ols <- felm(turnout20_r2 ~ hardlockdown + turnout14
                  |0 #no FE
                  |0 #no IV
                  |municode, #clustered SE
                  data = H1_OLS)


lm1_fe_ols <- felm(turnout20_r2 ~ hardlockdown
                   |factor(municode) #no FE
                   |0 #no IV
                   |municode, #clustered SE
                   data = H1_OLS)

lm1_c_fe_ols <- felm(turnout20_r2 ~ hardlockdown + turnout14 
                     |factor(municode) #no FE
                     |0 #no IV
                     |municode, #clustered SE
                     data = H1_OLS)


summary(lm1_ols)
summary(lm1_c_ols)
summary(lm1_fe_ols)
summary(lm1_c_fe_ols)

stargazer(lm1_ols, lm1_c_ols, lm1_fe_ols, lm1_c_fe_ols,
          type = "latex",
          model.names = FALSE, 
          title= c("The effect of hard lockdown on turnout"),
          keep = c("hardlockdown"),
          dep.var.labels = c("Turnout"),
          covariate.labels = c("Hard lockdown"),
          column.separate= c(2,2),
          omit = c("voteshare*", "Constant", "turnout14", "depname*"),
          add.lines = list(c("Control for turnout '14",                            "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


#####################################
#            H2 INCUMBENT           #
#####################################
H2_OLS <- D20 %>% 
  filter(near_border != 0,
         incumbent == 1)

H2_OLS <- H2_OLS %>% 
  left_join(D14_voteshare, by = c("depcode", "municode", "pollstation", "partylist"))

lm2_ols <- felm(voteshare20_r2 ~ hardlockdown
                |0 #no FE
                |0 #no IV
                |municode, #clustered SE
                data = H2_OLS)

lm2_c_ols <- felm(voteshare20_r2 ~ hardlockdown + voteshare14
                  |0 #no FE
                  |0 #no IV
                  |municode, #clustered SE
                  data = H2_OLS)


lm2_fe_ols <- felm(voteshare20_r2 ~ hardlockdown
                   |factor(municode) # FE
                   |0 #no IV
                   |municode, #clustered SE
                   data = H2_OLS)

lm2_c_fe_ols <- felm(voteshare20_r2 ~ hardlockdown + voteshare14
                     |factor(municode) # FE
                     |0 #no IV
                     |municode, #clustered SE
                     data = H2_OLS)

summary(lm2_ols)
summary(lm2_c_ols)
summary(lm2_fe_ols)
summary(lm2_c_fe_ols)

stargazer(lm2_ols, lm2_c_ols, lm2_fe_ols, lm2_c_fe_ols,
          type = "latex",
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for incumbent party"),
          keep = c("hardlockdown"),
          dep.var.labels = c("Vote share for incumbent party"),
          covariate.labels = c("Hard lockdown"),
          column.separate= c(2,2),
          omit = c("voteshare*", "Constant", "turnout14", "depname*"),
          add.lines = list(c("Control for vote share for incumbent party '14",     "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                            "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


#####################################
#          H3a CONSERVATIVE         #
#####################################
H3a_OLS <- D20 %>% 
  filter(partylist %in% c("LUD", "LLR", "LDVD", "LDLF"),
         near_border != 0)


H3a_OLS <- H3a_OLS %>% 
  left_join(D14_voteshare, by = c("depcode", "municode", "pollstation", "partylist"))


lm3a_ols <- felm(voteshare20_r2 ~ hardlockdown
                 |0 #no FE
                 |0 #no IV
                 |municode, #clustered SE
                 data = H3a_OLS)

lm3a_c_ols <- felm(voteshare20_r2 ~ hardlockdown + voteshare14
                   |0 #no FE
                   |0 #no IV
                   |municode, #clustered SE
                   data = H3a_OLS)


lm3a_fe_ols <- felm(voteshare20_r2 ~ hardlockdown
                    |factor(municode) # FE
                    |0 #no IV
                    |municode, #clustered SE
                    data = H3a_OLS)

lm3a_c_fe_ols <- felm(voteshare20_r2 ~ hardlockdown + voteshare14
                      |factor(municode) # FE
                      |0 #no IV
                      |municode, #clustered SE
                      data = H3a_OLS)

summary(lm3a_ols)
summary(lm3a_c_ols)
summary(lm3a_fe_ols)
summary(lm3a_c_fe_ols)

stargazer(lm3a_ols, lm3a_c_ols, lm3a_fe_ols, lm3a_c_fe_ols,
          type = "latex",
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for conservative parties"),
          keep = c("hardlockdown"),
          dep.var.labels = c("Vote share for conservative parties"),
          covariate.labels = c("Hard lockdown"),
          column.separate= c(2,2),
          omit = c("voteshare*", "Constant", "turnout14", "depname*"),
          add.lines = list(c("Control for vote share for conservative parties '14",   "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                               "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#####################################
#          H3b GREEN PARTIES        #
#####################################
H3b_OLS <- D20 %>% 
  filter(partylist %in% c("LVEC", "LEGO"),
         near_border != 0)


H3b_OLS <- H3b_OLS %>% 
  left_join(D14_voteshare, by = c("depcode", "municode", "pollstation", "partylist"))


lm3b_ols <- felm(voteshare20_r2 ~ hardlockdown
                 |0 #no FE
                 |0 #no IV
                 |municode, #clustered SE
                 data = H3b_OLS)

lm3b_c_ols <- felm(voteshare20_r2 ~ hardlockdown + voteshare14
                   |0 #no FE
                   |0 #no IV
                   |municode, #clustered SE
                   data = H3b_OLS)


lm3b_fe_ols <- felm(voteshare20_r2 ~ hardlockdown + factor(municode) - 1
                    |0 # FE
                    |0 #no IV
                    |municode, #clustered SE
                    data = H3b_OLS)


lm3b_c_fe_ols <- felm(voteshare20_r2 ~ hardlockdown + voteshare14 + factor(municode) - 1
                      |0 # FE
                      |0 #no IV
                      |municode, #clustered SE
                      data = H3b_OLS)


summary(lm3b_ols)
summary(lm3b_c_ols)
summary(lm3b_fe_ols)
summary(lm3b_c_fe_ols)

stargazer(lm3b_ols, lm3b_c_ols, lm3b_fe_ols, lm3b_c_fe_ols,
          type = "latex",
          model.names = FALSE, 
          title= c("The effect of hard lockdown on vote share for green parties"),
          keep = c("hardlockdown"),
          dep.var.labels = c("Vote share for green parties"),
          covariate.labels = c("Hard lockdown"),
          column.separate= c(2,2),
          omit = c("voteshare*", "Constant", "turnout14", "depname*"),
          add.lines = list(c("Control for vote share for green parties '14",          "No", "Yes", "No", "Yes"),
                           c("Municipal fixed effects",                               "No", "No", "Yes", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


############################################################################################
######################             GENERALIZED DiD                  ########################
############################################################################################
names(D20_DID)
names(D14_P_R)

##############################################################################
###############                DATA TIDYING                  #################
##############################################################################
D14_merge <- D14_P_R %>% 
  select(depcode, municode, pollstation, hardlockdown, near_border, incumbent, partylist, name, registered14, totalvotes14, personalvotes14, voteshare14, round) %>% 
  rename(registered = registered14,
         totalvotes = totalvotes14,
         personalvotes = personalvotes14,
         voteshare = voteshare14) %>% 
  mutate(hardlockdown = 0,
         treated = 0,
         year = ifelse(round == 1, 2, 1))

D20_merge <- D20_DID %>% 
  select(depcode, municode, pollstation, hardlockdown, near_border, incumbent, partylist, name, registered20, totalvotes20, personalvotes20, voteshare20, round) %>%
  rename(registered = registered20,
         totalvotes = totalvotes20,
         personalvotes = personalvotes20,
         voteshare = voteshare20) %>% 
  mutate(treated = ifelse(round == 1 & hardlockdown == 1, 1, 0),
         year = ifelse(round == 1, 4, 3))

DID_merge <- rbind(D20_merge, D14_merge)

##############################################################################
###############                 REGRESSIONS                  #################
##############################################################################
#####################################
#            H1 TURNOUT             #
#####################################
m1 <- DID_merge %>%
  filter(near_border != 0,
         incumbent == 1) %>% 
  mutate(turnout = totalvotes/registered)

model1a <- felm(turnout ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m1)
summary(model2a)

model1b <- felm(turnout ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) + factor(municode):as.numeric(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m1)
summary(model1b)

stargazer(model1a, model1b,
          type = "latex", 
          model.names = FALSE,
          title= c("The effect of hard lockdown on vote share for incumbent party"),
          dep.var.labels = c("Vote share for incumbent party"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Two-way fixed effects",                               "No","Yes"),
                           c("Municipal trends",                                    "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))

#####################################
#            H2 INCUMBENT           #
#####################################
m2 <- DID_merge %>%
  filter(near_border != 0,
         incumbent == 1)

model2a <- felm(voteshare ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m2)
summary(model2a)

model2b <- felm(voteshare ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) + factor(municode):as.numeric(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m2)
summary(model2b)

stargazer(model2a, model2b,
          type = "latex", 
          model.names = FALSE,
          title= c("The effect of hard lockdown on vote share for incumbent party"),
          dep.var.labels = c("Vote share for incumbent party"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Two-way fixed effects",                               "No","Yes"),
                           c("Municipal trends",                                    "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


#####################################
#          H3a Conservative         #
#####################################
m3 <- DID_merge %>%
  filter(partylist %in% c("LUD", "LLR", "LDVD", "LDLF"),
         near_border != 0)

model3a <- felm(voteshare ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m3)
summary(model3a)

model3b <- felm(voteshare ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) + factor(municode):as.numeric(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m3)
summary(model3b)

stargazer(model3a, model3b,
          type = "latex", 
          model.names = FALSE,
          title= c("The effect of hard lockdown on vote share for conservative parties"),
          dep.var.labels = c("Vote share for conservative parties"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Two-way fixed effects",                               "No","Yes"),
                           c("Municipal trends",                                    "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


#####################################
#         H3b Green Parties         #
#####################################
m4 <- DID_merge %>%
  filter(partylist %in% c("LVEC", "LEGO"), 
         near_border != 0)

model4a <- felm(voteshare ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m4)
summary(model4a)

model4b <- felm(voteshare ~ treated   # Outcome regressed on treatment
                | factor(municode) + factor(year) + factor(municode):as.numeric(year) # Unit and time fixed effects
                | 0                           # (no instrumental variable)
                | municode,                       # SE clusted on the unit
                data = m4)
summary(model4b)

stargazer(model4a, model4b,
          type = "latex", 
          model.names = FALSE,
          title= c("The effect of hard lockdown on vote share for green parties"),
          dep.var.labels = c("Vote share for green parties"),
          covariate.labels = c("Post lockdown $times$ Hard lockdown"),
          omit = c("voteshare*", "Constant", "depname*"),
          column.separate= c(2,2),
          add.lines = list(c("Two-way fixed effects",                               "No","Yes"),
                           c("Municipal trends",                                    "No", "Yes")),
          digits = 2,
          style = "apsr",
          no.space = TRUE,
          table.layout ="-dc#-t-a-s-n",
          keep.stat = c("adj.rsq", "n"))


