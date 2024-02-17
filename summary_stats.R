###Read in Libraries and Data ------------------------------------------------------
#install packages
install.packages("rcompanion")
install.packages("dunn.test")
#libraries
library(dunn.test)
library(MASS)
library(multcomp)
library(statmod)
library(aod)
library(maps)
library(mapdata)
library(MuMIn)
library(logistf)
library(viridis)
library(ggplot2)
library(dplyr)
library(mgcv)
library(ordinal)
library(rcompanion)
library(tidyr)
#read in data frame
df <- read.csv("C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/dataframe.csv")
head(df)

#counts for paper
citations <- df %>%
  summarise(unique_count = n_distinct(Citation)) %>%
  pull(unique_count)
citations

#subset major taxa groups
birds <-  subset(df, Taxa == "Bird")
mammals <- subset(df, Taxa == "Mammal")
turtles <- subset(df, Taxa == "Turtle")

#counts for family 
bfamilies <- unique(birds$Family) #17
mfamilies <- unique(mammals$Family) #10
tfamilies <- unique(turtles$Family) #1

#counts for species
bspecies <- unique(birds$Species) #56
mspecies <- unique(mammals$Species) #31
tspecies <- unique(turtles$Species) #7

####Frequency of occurrence graph birds####
birds_ss <- birds %>%
  mutate(Soft = case_when(
    soft > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Hard = case_when(
    hard > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Rubber = case_when(
    rubber > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Thread = case_when(
    thread > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Foam = case_when(
    foam > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Cloth = case_when(
    cloth > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Other = case_when(
    other > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  mutate(Total = case_when(
    total > 0 ~ "Yes",
    TRUE ~ "No"
  ))

birds_ss <- birds_ss %>%
  select(Total, Hard, Soft, Thread, Rubber, Foam, Cloth)

summary_birdss <- birds_ss %>%
  pivot_longer(cols = everything(), names_to = "material_consumed") %>%
  group_by(material_consumed, value) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = value, values_from = count, names_prefix = "count_")
colnames(summary_birdss) <- c("material_consumed", "no", "yes")
summary_birdss <- summary_birdss %>% mutate(yes = ifelse(is.na(yes), 0, yes))
summary_birdss <- summary_birdss %>% mutate(total = yes + no)
summary_birdss <- summary_birdss %>% mutate(proportion = yes / total * 100)
summary_birdss <- summary_birdss %>% arrange(desc(material_consumed))

ggplot(summary_birdss, aes(x = material_consumed)) +
    geom_bar(aes(y = proportion), stat = "identity") + 
  labs(title = "Plastic ingestion by Birds",
       x = "Material Consumed",
       y = "% of individuals",
       fill = "Response")  # Customize fill colors if needed

birds_d <- birds %>%
  select(COD, harddeath, softdeath, threaddeath, rubberdeath, foamdeath, clothdeath)
summary_birdsd <- birds_d %>%
  pivot_longer(cols = everything(), names_to = "material_consumed") %>%
  group_by(material_consumed, value) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = value, values_from = count, names_prefix = "count_")
summary_birdsd <- summary_birdsd %>% mutate(count_Ind = ifelse(is.na(count_Ind), 0, count_Ind))
summary_birdsd <- summary_birdsd %>% mutate(count_KD = ifelse(is.na(count_KD), 0, count_KD))
summary_birdsd <- summary_birdsd %>% mutate(count_PD = ifelse(is.na(count_PD), 0, count_PD))
summary_birdsd <- summary_birdsd %>%
  mutate(material_consumed = recode(material_consumed, 
                              "COD" = "Total",
                              "clothdeath" = "Cloth",
                              "harddeath" = "Hard", 
                              "softdeath" = "Soft", 
                              "rubberdeath" = "Rubber", 
                              "foamdeath" = "Foam",
                              "threaddeath" = "Thread"))
summary_birdsd <- summary_birdsd %>% arrange(desc(material_consumed))
summary_birdsd$total <- summary_birdss$yes
summary_birdsd <- summary_birdsd %>% mutate(Ind = count_Ind / total * 100)
summary_birdsd <- summary_birdsd %>% mutate(KD = count_KD / total * 100)
summary_birdsd <- summary_birdsd %>% mutate(PD = count_PD / total * 100)
summary_birdsd <- summary_birdsd %>% mutate(ND = total - (count_PD + count_KD + count_Ind))
summary_birdsd_ss <- summary_birdsd %>% select(material_consumed, KD, PD)

  summary_birdsd_ss %>%
    gather(key = "Cause_of_Death", value = "Proportion", -material_consumed) %>%
    ggplot(aes(x = material_consumed, y = Proportion, fill = Cause_of_Death)) +
    geom_bar(stat = "identity") +
    labs(title = "Plastic COD in Birds",
         x = "Material Consumed",
         y = "Proportion",
         fill = "Cause of Death") +
    scale_fill_manual(values = c("KD" = "blue", "PD" = "pink")) +
    theme_minimal()
  
####Frequency of occurrence graph mammals####
mammals_ss <- mammals %>%
  mutate(Soft = case_when(
    soft > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Hard = case_when(
    hard > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Rubber = case_when(
    rubber > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Thread = case_when(
    thread > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Foam = case_when(
    foam > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Cloth = case_when(
    cloth > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Other = case_when(
    other > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  mutate(Total = case_when(
    total > 0 ~ "Yes",
    TRUE ~ "No"
  ))

mammals_ss <- mammals_ss %>%
  select(Total, Hard, Soft, Thread, Rubber, Foam, Cloth)

summary_mammalss <- mammals_ss %>%
  pivot_longer(cols = everything(), names_to = "material_consumed") %>%
  group_by(material_consumed, value) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = value, values_from = count, names_prefix = "count_")
colnames(summary_mammalss) <- c("material_consumed", "no", "yes")
summary_mammalss <- summary_mammalss %>% mutate(yes = ifelse(is.na(yes), 0, yes))
summary_mammalss <- summary_mammalss %>% mutate(total = yes + no)
summary_mammalss <- summary_mammalss %>% mutate(proportion = yes / total * 100)
summary_mammalss <- summary_mammalss %>% arrange(desc(material_consumed))

ggplot(summary_mammalss, aes(x = material_consumed)) +
  geom_bar(aes(y = proportion), stat = "identity") +
  labs(title = "Plastic ingestion by mammals",
       x = "Material Consumed",
       y = "Individuals")  # Customize fill colors if needed

mammals_d <- mammals %>%
  select(COD, harddeath, softdeath, threaddeath, rubberdeath, foamdeath, clothdeath)
summary_mammalsd <- mammals_d %>%
  pivot_longer(cols = everything(), names_to = "material_consumed") %>%
  group_by(material_consumed, value) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = value, values_from = count, names_prefix = "count_")
summary_mammalsd <- summary_mammalsd %>% mutate(count_KD = ifelse(is.na(count_KD), 0, count_KD))
summary_mammalsd <- summary_mammalsd %>% mutate(count_PD = ifelse(is.na(count_PD), 0, count_PD))
summary_mammalsd <- summary_mammalsd %>%
  mutate(material_consumed = recode(material_consumed, 
                                    "COD" = "Total",
                                    "clothdeath" = "Cloth",
                                    "harddeath" = "Hard", 
                                    "softdeath" = "Soft", 
                                    "rubberdeath" = "Rubber", 
                                    "foamdeath" = "Foam",
                                    "threaddeath" = "Thread"))
summary_mammalsd <- summary_mammalsd %>% arrange(desc(material_consumed))
summary_mammalsd$total <- summary_mammalss$yes
summary_mammalsd <- summary_mammalsd %>% mutate(KD = count_KD / total * 100)
summary_mammalsd <- summary_mammalsd %>% mutate(PD = count_PD / total * 100)
summary_mammalsd <- summary_mammalsd %>% mutate(ND = total - (count_KD + count_PD))
summary_mammalsd_ss <- summary_mammalsd %>% select(material_consumed, KD, PD)

summary_mammalsd_ss %>%
  gather(key = "Cause_of_Death", value = "Proportion", -material_consumed) %>%
  ggplot(aes(x = material_consumed, y = Proportion, fill = Cause_of_Death)) +
  geom_bar(stat = "identity") +
  labs(title = "Plastic COD in mammals",
       x = "Material Consumed",
       y = "Proportion",
       fill = "Cause of Death") +
  scale_fill_manual(values = c("KD" = "blue", "PD" = "pink")) +
  theme_minimal()

####Frequency of occurrence graph turtles####
turtles_ss <- turtles %>%
  mutate(Soft = case_when(
    soft > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Hard = case_when(
    hard > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Rubber = case_when(
    rubber > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Thread = case_when(
    thread > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Foam = case_when(
    foam > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Cloth = case_when(
    cloth > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Other = case_when(
    other > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  mutate(Total = case_when(
    total > 0 ~ "Yes",
    TRUE ~ "No"
  ))

turtles_ss <- turtles_ss %>%
  select(Total, Hard, Soft, Thread, Rubber, Foam, Cloth)

summary_turtless <- turtles_ss %>%
  pivot_longer(cols = everything(), names_to = "material_consumed") %>%
  group_by(material_consumed, value) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = value, values_from = count, names_prefix = "count_")
colnames(summary_turtless) <- c("material_consumed", "no", "yes")
summary_turtless <- summary_turtless %>% mutate(yes = ifelse(is.na(yes), 0, yes))
summary_turtless <- summary_turtless %>% mutate(total = yes + no)
summary_turtless <- summary_turtless %>% mutate (proportion = yes / total *100)
summary_turtless <- summary_turtless %>% arrange(desc(material_consumed))

ggplot(summary_turtless, aes(x = material_consumed)) +
    geom_bar(aes(y = proportion), stat = "identity") +
  labs(title = "Plastic ingestion by turtles",
       x = "Material Consumed",
       y = "Individuals")  # Customize fill colors if needed

turtles_d <- turtles %>%
  select(COD, harddeath, softdeath, threaddeath, rubberdeath, foamdeath, clothdeath)
summary_turtlesd <- turtles_d %>%
  pivot_longer(cols = everything(), names_to = "material_consumed") %>%
  group_by(material_consumed, value) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = value, values_from = count, names_prefix = "count_")
summary_turtlesd <- summary_turtlesd %>% mutate(count_KD = ifelse(is.na(count_KD), 0, count_KD))
summary_turtlesd <- summary_turtlesd %>% mutate(count_PD = ifelse(is.na(count_PD), 0, count_PD))
summary_turtlesd <- summary_turtlesd %>% mutate(count_Ind = ifelse(is.na(count_Ind), 0, count_Ind))
summary_turtlesd <- summary_turtlesd %>%
  mutate(material_consumed = recode(material_consumed, 
                                    "COD" = "Total",
                                    "clothdeath" = "Cloth",
                                    "harddeath" = "Hard", 
                                    "softdeath" = "Soft", 
                                    "rubberdeath" = "Rubber", 
                                    "foamdeath" = "Foam",
                                    "threaddeath" = "Thread"))
summary_turtlesd <- summary_turtlesd %>% arrange(desc(material_consumed))
summary_turtlesd$total <- summary_turtless$yes
summary_turtlesd <- summary_turtlesd %>% mutate(KD = count_KD / total * 100)
summary_turtlesd <- summary_turtlesd %>% mutate(PD = count_PD / total * 100)
summary_turtlesd <- summary_turtlesd %>% mutate(ND = total - (count_KD + count_PD + count_Ind))
summary_turtlesd_ss <- summary_turtlesd %>% select(material_consumed, KD, PD)

summary_turtlesd_ss %>%
  gather(key = "Cause_of_Death", value = "Proportion", -material_consumed) %>%
  ggplot(aes(x = material_consumed, y = Proportion, fill = Cause_of_Death)) +
  geom_bar(stat = "identity") +
  labs(title = "Plastic COD in turtles",
       x = "Material Consumed",
       y = "Proportion",
       fill = "Cause of Death") +
  scale_fill_manual(values = c("KD" = "blue", "PD" = "pink")) +
  theme_minimal()

#### Statistical analyses#####

####birds#####
###ingestion analyses
analysis_bird <- birds %>%
  select(hard, soft, thread, rubber, foam, cloth, other)
analysis_bird <- analysis_bird %>%
  gather(key = "Group", value = "Value")
kw_bird <- kruskal.test(Value ~ Group, data = analysis_bird, na.action = "na.exclude")
print(kw_bird) #P = 2e-16
kw_bird_dunn <- dunn.test(analysis_bird$Value, g = analysis_bird$Group, method = "bonferroni")
#statistically significant P hard significant for all < 1e-4. soft P significant from cloth (p = 0.0061)

#mortality rate analyses
birdcs <- summary_birdsd %>% select(count_Ind, count_KD, ND, count_PD)
birdcs <- subset(birdcs, material_consumed != "Total")
ftbird <- fisher.test(birdcs[,2:5], simulate.p.value = TRUE)
print(ftbird) 
#statistically significant p = 0.0005

#####mammals#####
#ingestion analysis
analysis_mammal <- mammals %>%
  select(hard, soft, thread, rubber, foam, cloth, other)
analysis_mammal <- analysis_mammal %>%
  gather(key = "Group", value = "Value")
kw_mammal <- kruskal.test(Value ~ Group, data = analysis_mammal, na.action = "na.exclude")
print(kw_mammal) #P = 2.2e-16
kw_mammal_dunn <- dunn.test(analysis_mammal$Value, g = analysis_mammal$Group, method = "bonferroni")
#statistically significant thread P <1e-4 for all. soft P <.0004 for cloth, foam, hard, thread. 
#other is significantly different from all besides rubber P <.0001. cloth, foam, hard, rubber are all same

#mortality rate analyses
mammalcs <- summary_mammalsd %>% select(count_KD, count_PD, ND)
mammalcs <- subset(mammalcs, material_consumed != "Total")
ftmammal <- fisher.test(mammalcs[,2:4], simulate.p.value = TRUE)
print(ftmammal)
#statistically significant P= 0.007

####Turtles#####
#ingestion rate analysis
analysis_turtle <- turtles %>%
  select(hard, soft, thread, rubber, foam, cloth, other)
analysis_turtle <- analysis_turtle %>%
  gather(key = "Group", value = "Value")
kw_turtle <- kruskal.test(Value ~ Group, data = analysis_turtle, na.action = "na.exclude")
print(kw_turtle) #P = 2.2e-16
kw_turtle_dunn <- dunn.test(analysis_turtle$Value, g = analysis_turtle$Group, method = "bonferroni")
#foam, cloth are same. hard is SD from all P <.02, other is different from all but foam P <.02. 
#rubber is different from hard, other, soft, thread P < 0.02. soft is different from all P <1e-4. 
#thread from all P <0.02

#mortality rate analyses#
turtlecs <- summary_turtlesd %>% select(count_Ind, count_KD, ND, count_PD)
turtlecs <- subset(turtlecs, material_consumed != "Total")
ftturtle <- fisher.test(turtlecs[,2:5], simulate.p.value = TRUE)
print(ftturtle)
#statistically significant. p = .5

