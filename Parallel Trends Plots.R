library(tidyverse)
library(ggplot2)

vote2008 <- read_csv("D08ny.csv")
vote2008 <- vote2008 %>% 
  filter(!X1 == "5934") 
vote2014 <-read_csv("D14ny.csv")
vote2020 <- read_csv("D20_unrestricted.csv")

D08_r1 <- read_csv("D08_r1.csv") #from Datatidying file before merging
D08_r2 <- read_csv("D08_r2.csv") #from Datatidying file before merging
D14_r1 <- read_csv("D14_r1.csv") #from Datatidying file before merging
D14_r2 <- read_csv("D14_r2.csv") #from Datatidying file before merging
D2020 <- read_csv("D20_restricted.csv")

########################################
###     TURNOUT PARALLEL TRENDS     ####
########################################
#Create data for each round in each election year
vote2008_r1 <- vote2008 %>% select(registered_r1, totalvotes_r1, depcode, municode, partylist) %>% 
  mutate(turnout = totalvotes_r1/registered_r1) %>% 
  select(-totalvotes_r1, -registered_r1)

vote2008_r2 <- vote2008 %>% select(registered_r2, totalvotes_r2, depcode, municode, partylist) %>% 
  mutate(turnout = totalvotes_r2/registered_r2) %>% 
  select(-totalvotes_r2, -registered_r2)

vote2014_r1 <- vote2014 %>% select(registered_r1, totalvotes_r1, depcode, municode, partylist)%>% 
  mutate(turnout = totalvotes_r1/registered_r1) %>% 
  select(-totalvotes_r1, -registered_r1)

vote2014_r2 <- vote2014 %>% select(registered_r2, totalvotes_r2, depcode, municode, partylist)%>% 
  mutate(turnout = totalvotes_r2/registered_r2) %>% 
  select(-totalvotes_r2, -registered_r2)

vote2020 <- vote2020 %>% select(turnout20_r1, turnout20_r2, depcode, municode, partylist)

vote2020_r1 <- vote2020 %>% 
  rename(turnout = turnout20_r1) %>% 
  select(-turnout20_r2)

vote2020_r2 <- vote2020 %>% 
  rename(turnout = turnout20_r2) %>% 
  select(-turnout20_r1)

#Merge all rounds together and create year ID
turnout_trends <- bind_rows(vote2008_r1, vote2008_r2, vote2014_r1, vote2014_r2, vote2020_r1, vote2020_r2, .id = "year")
turnout_trends$year <- as.numeric(turnout_trends$year)
turnout_trends <- turnout_trends %>% 
  select(year, depcode, municode, turnout)
#year: 1 = 2008_r_1, 2= 2008_r_2, 3= 2014_r_1, 4 = 2014_r_2, 5= 2020_r_1, 6 = 2020_r_2

#Add near-border division
turnout_trends <- turnout_trends %>% 
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

#Filter for departments near border and create treatment variable
turnout_trends <- turnout_trends %>%
  group_by(depcode) %>%
  filter(!near_border == 0) %>% 
  mutate(treated_lockdown = ifelse(any(near_border == 1), "Hard Lockdown", "Soft Lockdown")) %>% 
  select(-near_border)

#Create mean voter turnout in the two groups near border and confidence intervals
turnout_trends_plot <- turnout_trends %>%
  group_by(treated_lockdown, year) %>%
  summarize(mean_voter_turnout = coef(lm(turnout ~ 1))[1],
            lower95 = confint(lm(turnout ~ 1))[1],
            upper95 = confint(lm(turnout ~ 1))[2])

##Plot
pdf("turnoutparallel.pdf", 6, 5)
ggplot(turnout_trends_plot, aes(x = year, y = mean_voter_turnout,
                     color = treated_lockdown, shape = treated_lockdown)) +
  labs(title = "",
       x = "", y = "Voter turnout", color = "", shape = "") +
  coord_cartesian(ylim = c(0.2, 0.7)) +
  scale_x_continuous(breaks = 1:6,
                     labels = c("2008\n(Round 1)", "2008\n(Round 2)",
                                "2014\n(Round 1)", "2014\n(Round 2)",
                                "2020\n(Round 1)", "2020\n(Round 2)")) +
  # position_dodge separates the points slightly left and right
  # so that they don't overlap
  geom_line(position = position_dodge(width = 0.15)) +
  geom_linerange(aes(ymax = upper95, ymin = lower95),
                 size = 2, position = position_dodge(width = 0.15)) +
  geom_point(fill = "white", size = 2, position = position_dodge(width = 0.15)) +
  geom_vline(size = 0.25, xintercept = 5.5, linetype = 5) + # Add a vertical line when refugees arrive
  scale_color_manual(values = c("Soft Lockdown" = "darkgreen", "Hard Lockdown" = "darkred")) +
  scale_shape_manual(values = c("Soft Lockdown" = 21, "Hard Lockdown" = 16)) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top",      # Put the legend at the top
        plot.title.position = "plot",
        axis.title.y = element_text(size =9)) # Fully left justify the title
dev.off()

########################################################
###     CONSERVATIVE VOTE SHARE PARALLEL TRENDS     ####
########################################################
#Merge so the data from 2008 and 2014 is left joined the 
#same way data D20_restricted is.
D08 <- D08r2 %>% 
  left_join(D08r1, by = c("depcode", "municode", "name"))

D08 <- D08 %>% 
  group_by(depcode, municode) %>% 
  mutate(totalvotes_r1 = sum(personalvotes_r1, na.rm = TRUE),
         totalvotes_r2 = sum(personalvotes_r2, na.rm = TRUE),
         voteshare_r1 = personalvotes_r1/totalvotes_r1,
         voteshare_r2 = personalvotes_r2/totalvotes_r2)

names(D08)
D08 <- D08 %>% 
  select(-depname.y, -muniname.y, -partylist.y, -sex.y, list.y) %>%
  rename(depname = depname.x,
         muniname = muniname.x,
         partylist = partylist.x,
         sex = sex.x,
         list = list.x) %>% 
  select(depcode, depname, municode, muniname, partylist, name, list, registered_r1, submitted_r1, totalvotes_r1, personalvotes_r1, voteshare_r1, registered_r2, submitted_r2, totalvotes_r2, personalvotes_r2, voteshare_r2, seats_r2) %>% 
  arrange(municode, depcode)

D14 <- D14r2 %>% 
  left_join(D14r1, by = c("depcode", "municode", "name"))

D14 <- D14 %>% 
  group_by(depcode, municode) %>% 
  mutate(totalvotes_r1 = sum(personalvotes_r1, na.rm = TRUE),
         totalvotes_r2 = sum(personalvotes_r2, na.rm = TRUE),
         voteshare_r1 = personalvotes_r1/totalvotes_r1,
         voteshare_r2 = personalvotes_r2/totalvotes_r2)

names(D14)
D14 <- D14 %>% 
  select(-depname.y, -muniname.y, -partylist.y, -sex.y, list.y) %>%
  rename(depname = depname.x,
         muniname = muniname.x,
         partylist = partylist.x,
         sex = sex.x,
         list = list.x) %>% 
  select(depcode, depname, municode, muniname, partylist, name, list, registered_r1, submitted_r1, totalvotes_r1, personalvotes_r1, voteshare_r1, registered_r2, submitted_r2, totalvotes_r2, personalvotes_r2, voteshare_r2, seats_r2) %>% 
  arrange(municode, depcode)

#Create data for each round in each election year
D08_r1_RED <- D08 %>% 
  select(voteshare_r1, depcode, municode, partylist) %>% 
  rename(voteshare = voteshare_r1) %>%  
  filter(!is.na(voteshare))

D08_r2_RED <- D08 %>% select(voteshare_r2, depcode, municode, partylist) %>% 
  rename(voteshare = voteshare_r2) %>%
  filter(!is.na(voteshare))

D14_r1_RED <- D14 %>% select(voteshare_r1, depcode, municode, partylist)%>% 
  rename(voteshare = voteshare_r1) %>% 
  filter(!is.na(voteshare))

D14_r2_RED <- D14 %>% select(voteshare_r2, depcode, municode, partylist)%>% 
  rename(voteshare = voteshare_r2) %>% 
  filter(!is.na(voteshare))

D2020 <- D2020 %>% select(voteshare20_r1, voteshare20_r2, depcode, municode, partylist)
D20_r1_RED  <- D2020 %>% 
  rename(voteshare = voteshare20_r1) %>% 
  filter(!is.na(voteshare)) %>% 
  select(-voteshare20_r2)

D20_r2_RED  <- D2020 %>% 
  rename(voteshare = voteshare20_r2) %>% 
  filter(!is.na(voteshare)) %>% 
  select(-voteshare20_r1)

#Merge all rounds together and create year ID
cons_trends <- bind_rows(D08_r1_RED, D08_r2_RED, D14_r1_RED, D14_r2_RED, D20_r1_RED, D20_r2_RED, .id = "year")
cons_trends$year <- as.numeric(cons_trends$year)
cons_trends <- cons_trends %>% 
  select(year, depcode, municode, voteshare, partylist)
#Year: 1 = 2008_r_1, 2= 2008_r_2, 3= 2014_r_1, 4 = 2014_r_2, 5= 2020_r_1, 6 = 2020_r_2

#Add near-border division
cons_trends <- cons_trends %>% 
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

#Filter for departments near border, conservative parties and create treatment variable
cons_trends <- cons_trends %>%
  group_by(depcode) %>%
  filter(!near_border == 0) %>% 
  mutate(treated_lockdown = ifelse(any(near_border == 1), "Hard Lockdown", "Soft Lockdown")) %>% 
  select(-near_border) %>% 
  ungroup() %>% 
  filter(partylist %in% c("LUD", "LLR","LDVD", "LDLF"))

#Create mean conservative vote share in the two groups near border and confidence intervals
cons_trends_plot <- cons_trends %>%
  group_by(treated_lockdown, year) %>%
  summarize(mean_vote_share = coef(lm(voteshare ~ 1))[1],
            lower95 = confint(lm(voteshare ~ 1))[1],
            upper95 = confint(lm(voteshare ~ 1))[2])

#Plot
pdf("parallelconservative.pdf", 6, 5)
ggplot(cons_trends_plot, aes(x = year, y = mean_vote_share,
                     color = treated_lockdown, shape = treated_lockdown)) +
  labs(title = "",
       x = "", y = "Vote share", color = "", shape = "") +
  coord_cartesian(ylim = c(0.15, 0.5)) +
  scale_x_continuous(breaks = 1:6,
                     labels = c("2008\n(Round 1)", "2008\n(Round 2)",
                                "2014\n(Round 1)", "2014\n(Round 2)",
                                "2020\n(Round 1)", "2020\n(Round 2)")) +
  # position_dodge separates the points slightly left and right
  # so that they don't overlap
  geom_line(position = position_dodge(width = 0.15)) +
  geom_linerange(aes(ymax = upper95, ymin = lower95),
                 size = 2, position = position_dodge(width = 0.15)) +
  geom_point(fill = "white", size = 2, position = position_dodge(width = 0.15)) +
  geom_vline(size = 0.25, xintercept = 5.5, linetype = 5) + # Add a vertical line when refugees arrive
  scale_color_manual(values = c("Soft Lockdown" = "darkgreen", "Hard Lockdown" = "darkred")) +
  scale_shape_manual(values = c("Soft Lockdown" = 21, "Hard Lockdown" = 16)) +
  theme_classic(base_size = 11) +
  theme(legend.position = "top",      # Put the legend at the top
        plot.title.position = "plot",
        axis.title.y = element_text(size = 9)) # Fully left justify the title
dev.off()
