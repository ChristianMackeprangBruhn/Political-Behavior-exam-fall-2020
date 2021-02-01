#rm(list=ls())
library(tidyverse)
library(readxl)
library(banR)
library(readxl)

setwd("/Users/jakobhanghoej/Desktop/Statskundskab/9. semester/Politisk Adfærd/01_exam/01_R")
####################################################################################################################
####################################################################################################################
##################################                  DATA TIDYING                  ##################################
####################################################################################################################
####################################################################################################################
############################################################################################
######################                LOADING DATA                  ########################
############################################################################################
## Round 1 2020 election data
D20r1_raw <- read_excel("Data/2020-05-18-resultats-par-niveau-burvot-t1-france-entiere.xlsx")

## Round 2 2020 election data
D20r2_raw <- read_excel("Data/resultats-par-niveau-burvot-t2-france-entiere.xlsx")

## Municipalities above 1,000 citizens
muni_r2 <- read_excel("2020-06-29-resultats-t2-communes-de-1000-hab-et-plus.xlsx")
## Municipalities above 1,000 citizens
muni_r1 <- read_excel("2020-05-18-resultats-communes-de-1000-et-plus.xlsx")

## Incumbent mayors
incumbent_raw <- read_excel("maires-17-06-2014.xlsx")

# 2014 Round 1
D14_raw <- read.csv("Data/MN14_Bvot_T1T2.txt", sep = ";")

############################################################################################
######################            2020: DATA TIDYING                ########################
############################################################################################
#####################################
#    Municipalities above 1,000     #
#####################################
#R2
muni_r2 <- muni_r2 %>% 
  rename(depcode = "Code du département",
         municode = "Code de la commune") %>% 
  select(depcode, municode) %>%
  mutate(above = 1,
         depcode = as.numeric(as.character(depcode))) %>%
  filter(!is.na(depcode),
         !is.na(municode))

muni_r2$municode <- str_replace(muni_r2$municode, 'SR', '')
muni_r2$municode <- as.numeric(as.character(muni_r2$municode))

muni_r2 <- muni_r2 %>% 
  mutate(municode = ifelse(depcode == 69 & municode == 12301 , 123, municode),
         municode = ifelse(depcode == 75, 56, municode),
         municode = ifelse(depcode == 13 & municode == 5501 , 55, municode)) %>% 
  distinct(depcode, municode, .keep_all = TRUE)


#R1
muni_r1 <- muni_r1 %>% 
  rename(depcode = "Code du département",
         municode = "Code de la commune") %>% 
  select(depcode, municode) %>%
  mutate(above = 1,
         depcode = as.numeric(as.character(depcode))) %>%
  filter(!is.na(depcode),
         !is.na(municode))

muni_r1$municode <- str_replace(muni_r1$municode, 'SR', '')
muni_r1$municode <- as.numeric(as.character(muni_r1$municode))

muni_r1 <- muni_r1 %>% 
  mutate(municode = ifelse(depcode == 69 & municode == 12301 , 123, municode),
         municode = ifelse(depcode == 75, 56, municode),
         municode = ifelse(depcode == 13 & municode == 5501 , 55, municode)) %>% 
  distinct(depcode, municode, .keep_all = TRUE)


#####################################
#           2020 Round 1            #
#####################################
names(D20r1_raw)

sum(D20r1_raw$Inscrits)
sum(D20r1_raw$Votants)
#20740205/46437411

D20r1_raw <- D20r1_raw %>%
  filter(`Code du département` != "ZA",
         `Code du département` != "ZB",
         `Code du département` != "ZD",
         `Code du département` != "ZM",
         `Code du département` != "ZN",
         `Code du département` != "ZP",
         `Code du département` != "2A", #Corsica South
         `Code du département` != "2B",) %>% #Corsica North
  mutate(`Code du département` = as.numeric(as.character(`Code du département`)))

D20r1_raw <- D20r1_raw %>%
  mutate(`Code du département` = as.numeric(as.character(`Code du département`))) %>%
  rename(muniname = "Libellé de la commune") %>% 
  filter(!is.na(`Code du département`))

#Convert municipality code
unique(D20r1_raw$"Code de la commune")

D20r1_raw$"Code de la commune" <- str_replace(D20r1_raw$"Code de la commune", 'SR', '')
D20r1_raw$"Code de la commune" <- as.numeric(as.character(D20r1_raw$"Code de la commune"))

## Rename secteurs to Arrondissements
D20r1_raw <- D20r1_raw %>% 
  mutate(muniname = case_when(muniname == "Marseille Secteur 1" ~ "Marseille 1er Arrondissement",
                              muniname == "Marseille Secteur 2" ~ "Marseille 2e Arrondissement",
                              muniname == "Marseille Secteur 3" ~ "Marseille 3e Arrondissement",
                              muniname == "Marseille Secteur 4" ~ "Marseille 4e Arrondissement",
                              muniname == "Marseille Secteur 5" ~ "Marseille 5e Arrondissement",
                              muniname == "Marseille Secteur 6" ~ "Marseille 6e Arrondissement",
                              muniname == "Marseille Secteur 7" ~ "Marseille 7e Arrondissement",
                              muniname == "Marseille Secteur 8" ~ "Marseille 8e Arrondissement",
                              muniname == "Lyon 1er secteur" ~ "Lyon 1er Arrondissement",
                              muniname == "Lyon 2eme secteur" ~ "Lyon 2e Arrondissement",
                              muniname == "Lyon 3eme secteur" ~ "Lyon 3e Arrondissement",
                              muniname == "Lyon 4eme secteur" ~ "Lyon 4e Arrondissement",
                              muniname == "Lyon 5eme secteur" ~ "Lyon 5e Arrondissement",
                              muniname == "Lyon 5eme secteur" ~ "Lyon 5e Arrondissement",
                              muniname == "Lyon 6eme secteur" ~ "Lyon 6e Arrondissement",
                              muniname == "Lyon 7eme secteur" ~ "Lyon 7e Arrondissement",
                              muniname == "Lyon 8eme secteur" ~ "Lyon 8e Arrondissement",
                              muniname == "Lyon 9eme secteur" ~ "Lyon 9e Arrondissement",
                              muniname == "Paris 1er secteur" ~ "Paris 1er Arrondissement",
                              muniname == "Paris 5eme secteur" ~ "Paris 5e Arrondissement",
                              muniname == "Paris 6eme secteur" ~ "Paris 6e Arrondissement",
                              muniname == "Paris 8eme secteur" ~ "Paris 8e Arrondissement",
                              muniname == "Paris 9eme secteur" ~ "Paris 9e Arrondissement",
                              muniname == "Paris 10eme secteur" ~ "Paris 10e Arrondissement",
                              muniname == "Paris 11eme secteur" ~ "Paris 11e Arrondissement",
                              muniname == "Paris 12eme secteur" ~ "Paris 12e Arrondissement",
                              muniname == "Paris 13eme secteur" ~ "Paris 13e Arrondissement",
                              muniname == "Paris 14eme secteur" ~ "Paris 14e Arrondissement",
                              muniname == "Paris 15eme secteur" ~ "Paris 15e Arrondissement",
                              muniname == "Paris 16eme secteur" ~ "Paris 16e Arrondissement",
                              muniname == "Paris 17eme secteur" ~ "Paris 17e Arrondissement",
                              muniname == "Paris 18eme secteur" ~ "Paris 18e Arrondissement",
                              muniname == "Paris 19eme secteur" ~ "Paris 19e Arrondissement",
                              muniname == "Paris 20eme secteur" ~ "Paris 20e Arrondissement",
                              TRUE ~ as.character((muniname))))


D20r1_raw <- D20r1_raw %>% 
  mutate(muniname = case_when(`Code de la commune` == 94 & `Code du département` == 41 ~ "Fresnes, Loir-et-Cher",
                              `Code de la commune` == 34 & `Code du département` == 94 ~ "Fresnes, Val-de-Marne",
                              `Code de la commune` == 447 & `Code du département` == 57 ~ "Marly, Moselle",
                              `Code de la commune` == 383 & `Code du département` == 59 ~ "Marly, Nord",
                              `Code de la commune` == 354 & `Code du département` == 89 ~ "Saint-Martin-du-Tertre, Yonne",
                              `Code de la commune` == 556 & `Code du département` == 95 ~ "Saint-Martin-du-Tertre, Val d'Oise",
                              TRUE ~ as.character(muniname)))
sum(D20r1_raw$Inscrits)
sum(D20r1_raw$Votants)
#19727091/44405869

names(D20r1_raw)

#include only muncipalities with more than 1,000 citizens
D20r1_raw <- D20r1_raw %>% 
  rename(depcode = "Code du département",
         municode = "Code de la commune")

D20r1_raw <- D20r1_raw %>% 
  left_join(muni_r1, by = c("depcode", "municode"))

D20r1_raw <- D20r1_raw %>% 
  filter(!is.na(above)) %>% 
  select(-above)

D20r1_raw <- D20r1_raw %>% 
  distinct(depcode, municode, "Code B.Vote", Inscrits, Votants, .keep_all = TRUE)


# Create D
D <- list()

#start for loop
start_col <- 20
for(i in 1:62) {
  
  cols <- c(1:19, start_col:(start_col+8))
  D[[i]] <- D20r1_raw[, cols]
  
  names(D[[i]])[20:28] <- letters[1:9]
  
  start_col <- start_col + 9
  
}

D20r1 <- do.call(rbind, D)

names(D20r1)

D20r1 <- D20r1 %>% 
  rename(depname = "Libellé du département",
         pollstation = "Code B.Vote",
         registered = "Inscrits",
         abstentions = "Abstentions",
         abs_pct = "% Abs/Ins",
         submitted = "Votants",
         sub_pct = "% Vot/Ins",
         blank = "Blancs",
         blank_pct_reg = "% Blancs/Ins",
         blank_pct_sub = "% Blancs/Vot",
         invalid = "Nuls",
         invalid_pct_reg = "% Nuls/Ins",
         invalid_pct_sub = "% Nuls/Vot",
         voters = "Exprimés",
         vote_pct_reg = "% Exp/Ins",
         vote_pct_sub = "% Exp/Vot",
         ballot_list = a,
         partylist = b,
         sex = c,
         lastname = d,
         firstname = e,
         list = f,
         personalvotes = g,
         personal_votes_pct_reg = h,
         personal_votes_pct_vot = i)


#removes candidates that doesn't exist
D20r1 <- subset(D20r1, !is.na(personalvotes))

#merge first name and last name
D20r1 <- D20r1 %>% 
  unite(name, firstname:lastname, sep = " ")

# removes some overseas municipalities and columns
D20r1 <- D20r1 %>%
  mutate(municode = as.numeric(as.character(municode))) %>% 
  filter(!is.na(municode)) %>% 
  select(-sub_pct, -abstentions, -abs_pct,-blank:-blank_pct_sub, -vote_pct_reg, -vote_pct_sub, -personal_votes_pct_reg, -personal_votes_pct_vot)

#####################################
#           2020 Round 2            #
#####################################
names(D20r2_raw)

sum(D20r2_raw$Inscrits)
sum(D20r2_raw$Votants)
#6870330/16411347

D20r2_raw <- D20r2_raw %>%
  filter(`Code du département` != "ZA",
         `Code du département` != "ZB",
         `Code du département` != "ZD",
         `Code du département` != "ZM",
         `Code du département` != "ZN",
         `Code du département` != "ZP",
         `Code du département` != "2A", #Corsica South
         `Code du département` != "2B",) %>% #Corsica North
  mutate(`Code du département` = as.numeric(as.character(`Code du département`)))

D20r2_raw <- D20r2_raw %>%
  mutate(`Code du département` = as.numeric(as.character(`Code du département`))) %>%
  rename(muniname = "Libellé de la commune") %>% 
  subset(!is.na(`Code du département`))

unique(D20r2_raw$"Code de la commune")
D20r2_raw$"Code de la commune" <- str_replace(D20r2_raw$"Code de la commune", 'SR', '')
D20r2_raw$"Code de la commune" <- as.numeric(as.character(D20r2_raw$"Code de la commune"))

## Rename secteurs to Arrondissements
D20r2_raw <- D20r2_raw %>% 
  mutate(muniname = case_when(muniname == "Marseille Secteur 1" ~ "Marseille 1er Arrondissement",
                              muniname == "Marseille Secteur 2" ~ "Marseille 2e Arrondissement",
                              muniname == "Marseille Secteur 3" ~ "Marseille 3e Arrondissement",
                              muniname == "Marseille Secteur 4" ~ "Marseille 4e Arrondissement",
                              muniname == "Marseille Secteur 5" ~ "Marseille 5e Arrondissement",
                              muniname == "Marseille Secteur 6" ~ "Marseille 6e Arrondissement",
                              muniname == "Marseille Secteur 7" ~ "Marseille 7e Arrondissement",
                              muniname == "Marseille Secteur 8" ~ "Marseille 8e Arrondissement",
                              muniname == "Lyon 1er secteur" ~ "Lyon 1er Arrondissement",
                              muniname == "Lyon 2eme secteur" ~ "Lyon 2e Arrondissement",
                              muniname == "Lyon 3eme secteur" ~ "Lyon 3e Arrondissement",
                              muniname == "Lyon 4eme secteur" ~ "Lyon 4e Arrondissement",
                              muniname == "Lyon 5eme secteur" ~ "Lyon 5e Arrondissement",
                              muniname == "Lyon 5eme secteur" ~ "Lyon 5e Arrondissement",
                              muniname == "Lyon 6eme secteur" ~ "Lyon 6e Arrondissement",
                              muniname == "Lyon 7eme secteur" ~ "Lyon 7e Arrondissement",
                              muniname == "Lyon 8eme secteur" ~ "Lyon 8e Arrondissement",
                              muniname == "Lyon 9eme secteur" ~ "Lyon 9e Arrondissement",
                              muniname == "Paris 1er secteur" ~ "Paris 1er Arrondissement",
                              muniname == "Paris 5eme secteur" ~ "Paris 5e Arrondissement",
                              muniname == "Paris 6eme secteur" ~ "Paris 6e Arrondissement",
                              muniname == "Paris 8eme secteur" ~ "Paris 8e Arrondissement",
                              muniname == "Paris 9eme secteur" ~ "Paris 9e Arrondissement",
                              muniname == "Paris 10eme secteur" ~ "Paris 10e Arrondissement",
                              muniname == "Paris 11eme secteur" ~ "Paris 11e Arrondissement",
                              muniname == "Paris 12eme secteur" ~ "Paris 12e Arrondissement",
                              muniname == "Paris 13eme secteur" ~ "Paris 13e Arrondissement",
                              muniname == "Paris 14eme secteur" ~ "Paris 14e Arrondissement",
                              muniname == "Paris 15eme secteur" ~ "Paris 15e Arrondissement",
                              muniname == "Paris 16eme secteur" ~ "Paris 16e Arrondissement",
                              muniname == "Paris 17eme secteur" ~ "Paris 17e Arrondissement",
                              muniname == "Paris 18eme secteur" ~ "Paris 18e Arrondissement",
                              muniname == "Paris 19eme secteur" ~ "Paris 19e Arrondissement",
                              muniname == "Paris 20eme secteur" ~ "Paris 20e Arrondissement",
                              TRUE ~ as.character((muniname))))

D20r2_raw <- D20r2_raw %>% 
  mutate(muniname = case_when("Code de la commune" == 94 & `Code du département` == 41 ~ "Fresnes, Loir-et-Cher",
                              "Code de la commune" == 34 & `Code du département` == 94 ~ "Fresnes, Val-de-Marne",
                              "Code de la commune" == 447 & `Code du département` == 57 ~ "Marly, Moselle",
                              "Code de la commune" == 383 & `Code du département` == 59 ~ "Marly, Nord",
                              "Code de la commune" == 354 & `Code du département` == 89 ~ "Saint-Martin-du-Tertre, Yonne",
                              "Code de la commune" == 556 & `Code du département` == 95 ~ "Saint-Martin-du-Tertre, Val d'Oise",
                              TRUE ~ as.character(muniname)))


names(D20r2_raw)

#include only muncipalities with more than 1,000 citizens
D20r2_raw <- D20r2_raw %>% 
  rename(depcode = "Code du département",
         municode = "Code de la commune")

D20r2_raw <- D20r2_raw %>% 
  left_join(muni_r2, by = c("depcode", "municode"))

D20r2_raw$`Code B.Vote` <- as.numeric(as.character(D20r2_raw$`Code B.Vote`))

D20r2_raw <- D20r2_raw %>% 
  filter(!is.na(above)) %>% 
  select(-above)

D20r2_raw <- D20r2_raw %>% 
  distinct(depcode, municode, "Code B.Vote", Inscrits, Votants, .keep_all = TRUE)

# Create D
D <- list()

#start for loop
start_col <- 20
for(i in 1:61) {
  
  cols <- c(1:19, start_col:(start_col+8))
  D[[i]] <- D20r2_raw[, cols]
  
  names(D[[i]])[20:28] <- letters[1:9]
  
  start_col <- start_col + 9
  
}

D20r2 <- do.call(rbind, D)

names(D20r2)

D20r2 <- D20r2 %>% 
  rename(depname = "Libellé du département",
         pollstation = "Code B.Vote",
         registered = "Inscrits",
         abstentions = "Abstentions",
         abs_pct = "% Abs/Ins",
         submitted = "Votants",
         sub_pct = "% Vot/Ins",
         blank = "Blancs",
         blank_pct_reg = "% Blancs/Ins",
         blank_pct_sub = "% Blancs/Vot",
         invalid = "Nuls",
         invalid_pct_reg = "% Nuls/Ins",
         invalid_pct_sub = "% Nuls/Vot",
         voters = "Exprimés",
         vote_pct_reg = "% Exp/Ins",
         vote_pct_sub = "% Exp/Vot",
         ballot_list = a,
         partylist = b,
         sex = c,
         lastname = d,
         firstname = e,
         list = f,
         personalvotes = g,
         personal_votes_pct_reg = h,
         personal_votes_pct_vot = i)


#removes candidates that doesn't exist
D20r2 <- subset(D20r2, !is.na(personalvotes))

#merge first name and last name
D20r2 <- D20r2 %>% 
  unite(name, firstname:lastname, sep = " ")

# removes some overseas municipalities and columns
D20r2 <- D20r2 %>%
  mutate(municode = as.numeric(as.character(municode)),
         pollstation = as.numeric(as.character(pollstation))) %>% 
  filter(!is.na(municode)) %>% 
  select(-sub_pct, -abstentions, -abs_pct,-blank:-blank_pct_sub, -vote_pct_reg, -vote_pct_sub, -personal_votes_pct_reg, -personal_votes_pct_vot)

#####################################
#               MERGE               #
#####################################

D20r2 <- D20r2 %>%
  rename(registered20_r2 = registered,
         submitted20_r2 = submitted,
         voters20_r2 = voters,
         personalvotes20_r2 = personalvotes) %>% 
  mutate(round2 = 1)

D20r1 <- D20r1 %>%
  rename(registered20_r1 = registered,
         submitted20_r1 = submitted,
         voters20_r1 = voters,
         personalvotes20_r1 = personalvotes) %>% 
  mutate(round1 = 0)

D20 <- D20r2 %>% 
  left_join(D20r1, by = c("depcode", "municode","pollstation", "name"))

D20 <- D20 %>% 
  group_by(depcode, municode, pollstation) %>% 
  mutate(totalvotes20_r1 = sum(personalvotes20_r1),
         totalvotes20_r2 = voters20_r2,
         voteshare20_r1 = personalvotes20_r1/totalvotes20_r1,
         voteshare20_r2 = personalvotes20_r2/totalvotes20_r2) %>% 
  ungroup()

names(D20)
D20 <- D20 %>% 
  select(-depname.y, -partylist.y, -sex.y, list.y) %>%
  rename(depname = depname.x,
         partylist = partylist.x,
         sex = sex.x,
         list = list.x,
         muniname = muniname.x)  %>% 
  select(depcode, depname, municode, muniname, pollstation, partylist, sex, name, list, registered20_r1, submitted20_r1, totalvotes20_r1, personalvotes20_r1, voteshare20_r1, round1, registered20_r2, submitted20_r2, totalvotes20_r2, personalvotes20_r2, voteshare20_r2, round2) %>% 
  arrange(municode, depcode)

D20 <- D20 %>% 
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
                                 TRUE ~ as.numeric(near_border)))

D20 <- D20 %>% 
  mutate(hardlockdown = 0,
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

## Incumbent mayor
incumbent <- incumbent_raw %>% 
  rename(depcode = "Code du département (Maire)",
         municode = "Code Insee de la commune",
         lastname = "Nom de l'élu",
         firstname = "Prénom de l'élu") %>% 
  unite(name, firstname:lastname, sep = " ") %>%
  select(depcode, municode, name)

incumbent$incumbent <- 1

D20 <- D20 %>% 
  left_join(incumbent, by = c("depcode", "municode", "name")) %>% 
  mutate(incumbent = ifelse(is.na(incumbent), 0, incumbent))

D20 <- D20 %>% 
  mutate(turnout20_r1 = totalvotes20_r1/registered20_r1,
         turnout20_r2 = totalvotes20_r2/registered20_r2,
         voteshare20_diff = voteshare20_r2 - voteshare20_r1)

write_csv(D20, file = "D20_restricted.csv")

############################################################################################
######################            UNRESTRICTED FOR H1               ########################
############################################################################################
D20_unrestricted <- D20r1 %>% 
  left_join(D20r2, by = c("depcode", "municode", "pollstation", "name"))

#test <- D20_unrestricted %>% 
# distinct(depcode, municode, .keep_all = TRUE)
#sum(test$voters20_r1, na.rm = TRUE)


D20_unrestricted <- D20_unrestricted %>% 
  group_by(depcode, municode, pollstation) %>% 
  mutate(totalvotes20_r1 = sum(personalvotes20_r1, na.rm = TRUE),
         totalvotes20_r2 = sum(personalvotes20_r2, na.rm = TRUE),
         voteshare20_r1 = personalvotes20_r1/totalvotes20_r1,
         voteshare20_r2 = personalvotes20_r2/totalvotes20_r2)

D20_unrestricted <- D20_unrestricted %>% 
  select(-depname.y, -muniname.y, -partylist.y, -sex.y, list.y) %>%
  rename(depname = depname.x,
         muniname = muniname.x,
         partylist = partylist.x,
         sex = sex.x,
         list = list.x) %>% 
  select(depcode, depname, municode, muniname, pollstation, partylist, sex, name, list, registered20_r1, submitted20_r1, totalvotes20_r1, personalvotes20_r1, voteshare20_r1, round1, registered20_r2, submitted20_r2, totalvotes20_r2, personalvotes20_r2, voteshare20_r2, round2) %>% 
  arrange(municode, depcode)

D20_unrestricted <- D20_unrestricted %>% 
  mutate(turnout20_r1 = totalvotes20_r1/registered20_r1,
         turnout20_r2 = totalvotes20_r2/registered20_r2,
         voteshare20_diff = voteshare20_r2 - voteshare20_r1)

D20_unrestricted <- D20_unrestricted %>% 
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
                                 TRUE ~ as.numeric(near_border)))

D20_unrestricted <- D20_unrestricted %>% 
  mutate(hardlockdown = 0,
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

D20_unrestricted <- D20_unrestricted %>% 
  distinct(depcode, depname, municode, muniname, pollstation, .keep_all = TRUE) %>%
  filter(!is.na(municode),
         !is.na(muniname)) 

write_csv(D20_unrestricted, file = "D20_unrestricted.csv")

############################################################################################
######################                DID RESTRICTED                ########################
############################################################################################
D20_DID <- D20 %>% 
  select(depcode:muniname, pollstation, near_border:incumbent, partylist:list, registered20_r1:voteshare20_r1, turnout20_r1, round1, registered20_r2:voteshare20_r2, turnout20_r2, round2)

names(D20_DID)

# Create D
D <- list()

#start for loop
start_col <- 13
for(i in 1:2) {
  
  cols <- c(1:12, start_col:(start_col+6))
  D[[i]] <- D20_DID[, cols]
  
  names(D[[i]])[13:19] <- letters[1:7]
  
  start_col <- start_col + 7
  
}

D20_DID <- do.call(rbind, D)

D20_DID <- D20_DID %>% 
  rename(registered20 = a,
         submitted20 = b,
         totalvotes20 = c,
         personalvotes20 = d,
         voteshare20 = e,
         turnout20 = f,
         round = g) %>% 
  arrange(municode, depcode)

write_csv(D20_DID, file = "D20_DID.csv")

############################################################################################
######################              DID UNRESTRICTED                ########################
############################################################################################
D20_DID_U <- D20_unrestricted %>% 
  select(depcode:muniname, pollstation, near_border, hardlockdown, partylist, sex, name, registered20_r1:voteshare20_r1, turnout20_r1, round1, registered20_r2:voteshare20_r2, turnout20_r2, round2)

names(D20_DID_U)

# Create D
D <- list()

#start for loop
start_col <- 11
for(i in 1:2) {
  
  cols <- c(1:10, start_col:(start_col+6))
  D[[i]] <- D20_DID_U[, cols]
  
  names(D[[i]])[11:17] <- letters[1:7]
  
  start_col <- start_col + 7
}

D20_DID_U <- do.call(rbind, D)

D20_DID_U <- D20_DID_U %>% 
  rename(registered20 = a,
         submitted20 = b,
         totalvotes20 = c,
         personalvotes20 = d,
         voteshare20 = e,
         turnout20 = f,
         round = g) %>% 
  arrange(municode, depcode)

write_csv(D20_DID_U, file = "D20_DID_U.csv")

test <- D20r2 %>% 
  filter(partylist == "LNC")


############################################################################################
######################             2014: DATA TIDYING               ########################
############################################################################################
names(D14_raw)

D14_raw <- D14_raw %>%
  rename(round = "Tour",
         municode = "Code.de.la.commune", 
         depcode = "Code.du.departement",
         muniname = "Libelle.de.la.commune",
         pollstation = "Code.B.Vote",
         registered = "Inscrits",
         submitted = "Votants",
         voters = "Exprimes",
         firstname = "Prenom",
         lastname = "Nom",
         partylist = "Code.Nuance",
         personalvotes = "Voix") %>% 
  select(-N.pan)


#Convert municipality code


unique(D14_raw$municode)

D14_raw <- D14_raw %>%
  mutate(municode = as.numeric(as.character(municode)),
         depcode = as.numeric(as.character(depcode)),
         registered = as.numeric(as.character(registered)),
         submitted = as.numeric(as.character(submitted)),
         personalvotes = as.numeric(as.character(personalvotes)))

D14_raw <- D14_raw %>% 
  mutate(muniname = case_when(muniname == "Marseille Secteur 1" ~ "Marseille 1er Arrondissement",
                              muniname == "Marseille Secteur 2" ~ "Marseille 2e Arrondissement",
                              muniname == "Marseille Secteur 3" ~ "Marseille 3e Arrondissement",
                              muniname == "Marseille Secteur 4" ~ "Marseille 4e Arrondissement",
                              muniname == "Marseille Secteur 5" ~ "Marseille 5e Arrondissement",
                              muniname == "Marseille Secteur 6" ~ "Marseille 6e Arrondissement",
                              muniname == "Marseille Secteur 7" ~ "Marseille 7e Arrondissement",
                              muniname == "Marseille Secteur 8" ~ "Marseille 8e Arrondissement",
                              muniname == "Lyon 1er secteur" ~ "Lyon 1er Arrondissement",
                              muniname == "Lyon 2eme secteur" ~ "Lyon 2e Arrondissement",
                              muniname == "Lyon 3eme secteur" ~ "Lyon 3e Arrondissement",
                              muniname == "Lyon 4eme secteur" ~ "Lyon 4e Arrondissement",
                              muniname == "Lyon 5eme secteur" ~ "Lyon 5e Arrondissement",
                              muniname == "Lyon 5eme secteur" ~ "Lyon 5e Arrondissement",
                              muniname == "Lyon 6eme secteur" ~ "Lyon 6e Arrondissement",
                              muniname == "Lyon 7eme secteur" ~ "Lyon 7e Arrondissement",
                              muniname == "Lyon 8eme secteur" ~ "Lyon 8e Arrondissement",
                              muniname == "Lyon 9eme secteur" ~ "Lyon 9e Arrondissement",
                              muniname == "Paris 1er secteur" ~ "Paris 1er Arrondissement",
                              muniname == "Paris 5eme secteur" ~ "Paris 5e Arrondissement",
                              muniname == "Paris 6eme secteur" ~ "Paris 6e Arrondissement",
                              muniname == "Paris 8eme secteur" ~ "Paris 8e Arrondissement",
                              muniname == "Paris 9eme secteur" ~ "Paris 9e Arrondissement",
                              muniname == "Paris 10eme secteur" ~ "Paris 10e Arrondissement",
                              muniname == "Paris 11eme secteur" ~ "Paris 11e Arrondissement",
                              muniname == "Paris 12eme secteur" ~ "Paris 12e Arrondissement",
                              muniname == "Paris 13eme secteur" ~ "Paris 13e Arrondissement",
                              muniname == "Paris 14eme secteur" ~ "Paris 14e Arrondissement",
                              muniname == "Paris 15eme secteur" ~ "Paris 15e Arrondissement",
                              muniname == "Paris 16eme secteur" ~ "Paris 16e Arrondissement",
                              muniname == "Paris 17eme secteur" ~ "Paris 17e Arrondissement",
                              muniname == "Paris 18eme secteur" ~ "Paris 18e Arrondissement",
                              muniname == "Paris 19eme secteur" ~ "Paris 19e Arrondissement",
                              muniname == "Paris 20eme secteur" ~ "Paris 20e Arrondissement",
                              TRUE ~ as.character((muniname))))

D14_raw <- D14_raw %>% 
  mutate(muniname = case_when(municode == 94 & depcode == 41 ~ "Fresnes, Loir-et-Cher",
                              municode == 34 & depcode == 94 ~ "Fresnes, Val-de-Marne",
                              municode == 447 & depcode == 57 ~ "Marly, Moselle",
                              municode == 383 & depcode == 59 ~ "Marly, Nord",
                              municode == 354 & depcode == 89 ~ "Saint-Martin-du-Tertre, Yonne",
                              municode == 556 & depcode == 95 ~ "Saint-Martin-du-Tertre, Val d'Oise",
                              TRUE ~ as.character(muniname)))

D14_raw <- D14_raw %>% 
  unite(name, firstname:lastname, sep = " ") %>% 
  mutate(personalvotes = as.numeric(personalvotes),
         municode = as.numeric(as.character(municode)),
         pollstation = as.numeric(as.character(pollstation))) %>% 
  filter(!is.na(municode))

#####################################
#     Create 2 round datasets       #
#####################################
D14r2_raw <- D14_raw %>% 
  filter(round == 2)

D14r1_raw <- D14_raw %>% 
  filter(round != 2)


#####################################
#               MERGE               #
#####################################
D14r2 <- D14r2_raw %>%
  rename(registered_r2 = registered,
         submitted_r2 = submitted,
         voters_r2 = voters,
         personalvotes_r2 = personalvotes)

D14r1 <- D14r1_raw %>%
  rename(registered_r1 = registered,
         submitted_r1 = submitted,
         voters_r1 = voters,
         personalvotes_r1 = personalvotes)

D14 <- D14r1 %>% 
  left_join(D14r2, by = c("depcode", "municode", "pollstation", "partylist", "name"))

D14 <- D14 %>% 
  group_by(depcode, municode, pollstation) %>% 
  mutate(totalvotes_r1 = sum(personalvotes_r1, na.rm = TRUE),
         totalvotes_r2 = sum(personalvotes_r2, na.rm = TRUE),
         voteshare_r1 = personalvotes_r1/totalvotes_r1,
         voteshare_r2 = personalvotes_r2/totalvotes_r2)

names(D14)
D14 <- D14 %>% 
  select(-muniname.y) %>%
  rename(round1 = round.x,
         round2 = round.y,
         muniname = muniname.x) %>% 
  select(depcode, municode, muniname, pollstation, partylist, name, round1, registered_r1, submitted_r1, totalvotes_r1, personalvotes_r1, voteshare_r1, round2, registered_r2, submitted_r2, totalvotes_r2, personalvotes_r2, voteshare_r2) %>% 
  arrange(municode, depcode)

D14_voteshare <- D14 %>% 
  mutate(voteshare14 = ifelse(is.na(voteshare_r2), voteshare_r1, voteshare_r2)) %>% 
  select(depcode, municode, pollstation, partylist, name, voteshare14)

D14_turnout <- D14 %>% 
  distinct(depcode, municode, pollstation, .keep_all = TRUE)  %>% 
  mutate(turnout_r1 = submitted_r1/registered_r1,
         turnout_r2 = submitted_r2/registered_r2,
         turnout14 = ifelse(is.na(turnout_r2), turnout_r1, turnout_r2)) %>% 
  select(depcode, municode, pollstation, turnout14)

D14_diffshare <- D14 %>% 
  mutate(voteshare14_diff = voteshare_r2 - voteshare_r1) %>% 
  select(depcode, municode, pollstation, partylist, name, voteshare14_diff) %>% 
  filter(is.na(voteshare14_diff))

#write.csv(D14, file = "D14.csv")






