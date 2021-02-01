#rm(list=ls())

library(xtable)
library(lmtest)
library(lfe)
library(stargazer)

############################################################################################
######################            DESCRIPTIVE MODELS                ########################
############################################################################################
#####################################
#            VOTE SHARES            #
#####################################
descmodel <- D20 %>%
  group_by(partylist) %>% 
  mutate(partyvotes20_r1 = sum(personalvotes20_r1, na.rm = TRUE),
         partyvotes20_r2 = sum(personalvotes20_r2, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(totalvotes_r1 = sum(personalvotes20_r1, na.rm = TRUE),
         totalvotes_r2 = sum(personalvotes20_r2, na.rm = TRUE),
         voteshare_r1 = (partyvotes20_r1/totalvotes_r1)*100,
         voteshare_r2 = (partyvotes20_r2/totalvotes_r2)*100,
         diff = voteshare_r2 - voteshare_r1) %>%
  distinct(partylist, voteshare_r1, voteshare_r2, .keep_all = TRUE) %>% 
  select(partylist, voteshare_r1, voteshare_r2, diff) %>% 
  mutate(group = 0,
         group = case_when(partylist == "LEXG" ~ 1,
                           partylist == "LCOM" ~ 2,
                           partylist == "LFI" ~ 2,
                           partylist == "LSOC" ~ 2,
                           partylist == "LRDG" ~ 2,
                           partylist == "LDVG" ~ 2,
                           partylist == "LUG" ~ 2,
                           partylist == "LVEC" ~ 3,
                           partylist == "LECO" ~ 3,
                           partylist == "LDIV" ~ 4,
                           partylist == "LREG" ~ 4,
                           partylist == "LGJ" ~ 4,
                           partylist == "LREM" ~ 5,
                           partylist == "LMDM" ~ 5,
                           partylist == "LUDI" ~ 5,
                           partylist == "LUC" ~ 5,
                           partylist == "LDVC" ~ 5,
                           partylist == "LLR" ~ 6,
                           partylist == "LUD" ~ 6,
                           partylist == "LDVD" ~ 6,
                           partylist == "LDLF" ~ 6,
                           partylist == "LRN" ~ 7,
                           partylist == "LEXD" ~ 7),
         partygroup = case_when(partylist == "LEXG" ~ "Far-left",
                           partylist == "LCOM" ~ "Left",
                           partylist == "LFI" ~ "Left",
                           partylist == "LSOC" ~ "Left",
                           partylist == "LRDG" ~ "Left",
                           partylist == "LDVG" ~ "Left",
                           partylist == "LUG" ~ "Left",
                           partylist == "LVEC" ~ "Left Ecologists",
                           partylist == "LECO" ~ "Left Ecologists",
                           partylist == "LDIV" ~ "Miscellaneous",
                           partylist == "LREG" ~ "Miscellaneous",
                           partylist == "LGJ" ~ "Miscellaneous",
                           partylist == "LREM" ~ "Centre",
                           partylist == "LMDM" ~ "Centre",
                           partylist == "LUDI" ~ "Centre",
                           partylist == "LUC" ~ "Centre",
                           partylist == "LDVC" ~ "Centre",
                           partylist == "LLR" ~ "Right",
                           partylist == "LUD" ~ "Right",
                           partylist == "LDVD" ~ "Right",
                           partylist == "LDLF" ~ "Right",
                           partylist == "LRN" ~ "Far-right",
                           partylist == "LEXD" ~ "Far-right")) %>% 
  rename(Partylist = partylist,
         "Voteshare in round 1" = voteshare_r1,
         "Voteshare in round 2" = voteshare_r2,
         "Difference" = diff) %>% 
  arrange(group) %>% 
  select(partygroup, Partylist:Difference)


print(xtable(descmodel), include.rownames=F)

#####################################
#              TURNOUT              #
#####################################
turnout_r1 <- D20r1_raw %>%
  summarize(totalvotes_r1 = sum(Votants, na.rm = TRUE),
            registered_r1 = sum(Inscrits, na.rm = TRUE),
            turnout_r1 = (totalvotes_r1/registered_r1)*100)

turnout_r2 <- D20r2_raw %>%
  summarize(totalvotes_r2 = sum(Votants, na.rm = TRUE),
            registered_r2 = sum(Inscrits, na.rm = TRUE),
            turnout_r2 = (totalvotes_r2/registered_r2)*100)

turnout <- cbind(turnout_r1, turnout_r2) 

turnout <- turnout %>% 
  select(-totalvotes_r1, -totalvotes_r2, -registered_r1, -registered_r2)

print(xtable(turnout), include.rownames=F)
  

#####################################
#              GRAPHS               #
#####################################
G <- D20_DID %>%
  filter(near_border != 0) %>% 
  group_by(partylist, round, near_border) %>% 
  mutate(partyvotes20 = sum(personalvotes20, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(totalvotes = sum(personalvotes20, na.rm = TRUE),
         voteshare = (partyvotes20/totalvotes)*100) %>%
  distinct(partylist, partyvotes20, totalvotes, voteshare, .keep_all = TRUE) %>% 
  select(partylist, voteshare, round, near_border) %>% 
  mutate(group = 0,
         group = case_when(partylist == "LEXG" ~ 1,
                           partylist == "LCOM" ~ 2,
                           partylist == "LFI" ~ 2,
                           partylist == "LSOC" ~ 2,
                           partylist == "LRDG" ~ 2,
                           partylist == "LDVG" ~ 2,
                           partylist == "LUG" ~ 2,
                           partylist == "LVEC" ~ 3,
                           partylist == "LECO" ~ 3,
                           partylist == "LDIV" ~ 4,
                           partylist == "LREG" ~ 4,
                           partylist == "LGJ" ~ 4,
                           partylist == "LREM" ~ 5,
                           partylist == "LMDM" ~ 5,
                           partylist == "LUDI" ~ 5,
                           partylist == "LUC" ~ 5,
                           partylist == "LDVC" ~ 5,
                           partylist == "LLR" ~ 6,
                           partylist == "LUD" ~ 6,
                           partylist == "LDVD" ~ 6,
                           partylist == "LDLF" ~ 6,
                           partylist == "LRN" ~ 7,
                           partylist == "LEXD" ~ 7))
  
  
  
G %>% 
  ggplot(aes(x=round, y = voteshare, group = as.factor(near_border), color = as.factor(near_border))) + 
  geom_line() + 
  geom_point() + 
  scale_x_continuous(breaks = c(0, 1), 
                     labels = c("Round 1", "Round 2")) + 
  ylim(c(0.48, 0.62)) + 
  scale_color_discrete(name = "Lockdown area", 
                       labels = c("Hard lockdown", "Soft lockdown")) + 
  theme_minimal() + 
  ylab("Voteshare") +
  xlab("")


############################################################################################
######################                  TIMELINE                    ########################
############################################################################################
dates <- c(NA,"March 15", "March 17", "May 11", "June 2", "June 28", NA)

timeline_data <- as.data.frame(dates)
timeline_data$points <- c(NA,0.5,NA,NA,NA,0.5,NA)
timeline_data$distdate <- c(0.8,1,1.3,2.3,3,3.5,3.7)
timeline_data$name <- c(NA,"Round 1",NA,NA,NA,"Round 2",NA)

pdf("Plots/timeline.pdf", 10, 5) #start print device
timeline_data %>%  
  ggplot(aes(x = distdate, 
             y = points, 
             label = name)) +
  geom_hline(yintercept = c(0.5), 
             linetype = 1,
             size = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label=name), 
            hjust = 0.45, 
            vjust = -2,
            size = 5) +
  scale_x_continuous(breaks = c(1,1.3,2.3,3,3.5),
                     labels = c("March 15", "March 17", "May 11", "June 2", "June 28")) +
  scale_y_continuous(breaks = c(0.494, 0.497, 0.503, 0.506),
                     labels = c("","Soft Lockdown\nArea", "Hard Lockdown\nArea", "")) +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, 
                                   hjust = 1, 
                                   vjust = 1,
                                   size = 16,
                                   color = "black"),
        axis.text.y = element_text(size = 18, 
                                   color = "black"),
        axis.ticks.length.x = unit(1, "mm")) +
  annotate(geom = "segment", x = c(1.3), xend = c(1.3),
           y = c(0.494), yend =c(0.506),
           linetype = "dashed", color = "black", size = 1) +
  annotate(geom = "segment", x = c(2.3), xend = c(2.3),
           y = c(0.494), yend =c(0.5),
           linetype = "dashed", color = "black", size = 1) +
  annotate(geom = "segment", x = c(3), xend = c(3),
           y = c(0.50), yend =c(0.506),
           linetype = "dashed", color = "black", size = 1) +
  annotate(geom = "segment", x = c(1), xend = c(1),
           y = c(0.494), yend =c(0.5),
           linetype = "dotted", color = "black", size = 0.5) +
  annotate(geom = "segment", x = c(3.5), xend = c(3.5),
           y = c(0.494), yend =c(0.5),
           linetype = "dotted", color = "black", size = 0.5) +
  annotate(geom = "text", x = c(1.8), y = c(0.497), 
           label = "Lockdown\nPeriod", size = 6, color = "black") +
  annotate(geom = "text", x = c(2.15), y = c(0.503), 
           label = "Lockdown\nPeriod", size = 6, color = "black") +
  theme(axis.ticks.y = element_blank(),
        rect = element_blank())
dev.off()
?unit


