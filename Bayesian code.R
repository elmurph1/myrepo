# Load the brms package
install.packages("brms")
install.packages("rstan")
library(brms)
library(ggplot2)
library(dplyr)
#read in data frame
df <- read.csv("C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/dataframe.csv")
head(df)

#subset major taxa groups and clean data
birds <-  subset(df, Taxa == "Bird")
mammals <- subset(df, Taxa == "Mammal")
mammals_noman <- subset(df, Taxa == "Mammal" & Citation != "Chris Robbins Data")
turtles <- subset(df, Taxa == "Turtle")
turtles_infant <- subset(turtles, Age == "Infant")
turtles_juvenile <- subset(turtles, Age == "Juvenile")
turtles_young <- rbind(turtles_infant, turtles_juvenile)

#Have a total of 18 models before increasing the complexity. 8 mammal models, 6 turtle models, & 4 bird models
###Mammals model####
#1 mammal soft with manatee
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), "KD", softdeath))
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("KD"), 1, softdeath))
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("PDO", "KDO"), NA, softdeath))

mammal_model_soft <- brm(softdeath ~ soft, 
                    data = mammals, 
                    family = bernoulli(logit))

summary(mammal_model_soft)
msp <- conditional_effects(mammal_model_soft)
msp <- as.data.frame(msp[1])
x <- (log(19, base = exp(1)) + 7.53)/1.4

ggplot(msp, aes(x = soft.soft, y = soft.estimate__)) +
  geom_line() +
  geom_line(aes(y = soft.lower__), color = "blue") +
  geom_line(aes(y = soft.upper__), color = "blue") +
  geom_ribbon(aes(ymin = soft.lower__, ymax = soft.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Mammal Soft Pieces",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20)

#manatee soft volume 
mammal_model_softv <- brm(softdeath ~ soft_volume + Size_avg, 
                               data = mammals, 
                               family = bernoulli(logit))

summary(mammal_model_softv)
mvs <- conditional_effects(mammal_model_softv)
mvs <- as.data.frame(mvs[1])
x <- (log(19, base = exp(1)) + 7.02)/.01

ggplot(mvs, aes(x = soft_volume.soft_volume, y = soft_volume.estimate__)) +
  geom_line() +
  geom_line(aes(y = soft_volume.lower__), color = "blue") +
  geom_line(aes(y = soft_volume.upper__), color = "blue") +
  labs(title = "Cumulative Density Function",
       x = "Soft",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 4000)


#2 manatee soft pieces without manatees
mammals_noman <- mammals_noman %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), "KD", softdeath))
mammals_noman <- mammals_noman %>% mutate(softdeath = ifelse(softdeath %in% c("KD"), 1, softdeath))
mammals_noman <- mammals_noman %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))
mammalnoman_model_soft <- brm(softdeath ~ soft + Size_avg + Age, 
                         data = mammals_noman, 
                         family = bernoulli())

summary(mammalnoman_model_soft)
mpsnmce <- conditional_effects(mammalnoman_model_soft)
mpsnmce <- as.data.frame(mpsnmce[1])
x <- (log(19, base = exp(1)) + 6.31)/.35

ggplot(mpsnmce, aes(x = soft.soft, y = soft.estimate__)) +
  geom_line() +
  geom_line(aes(y = soft.lower__), color = "blue") +
  geom_line(aes(y = soft.upper__), color = "blue") +
  geom_ribbon(aes(ymin = soft.lower__, ymax = soft.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Cumulative Density Function",
       x = "Soft",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 20)

#manatee soft volume without manatees
mammalnoman_model_softv <- brm(softdeath ~ soft_volume, 
                              data = mammals_noman, 
                              family = bernoulli())

summary(mammalnoman_model_softv)
mvsnmce <- conditional_effects(mammalnoman_model_softv)
mvsnmce <- as.data.frame(mvsnmce[1])

ggplot(mvsnmce, aes(x = soft_volume.soft_volume, y = soft_volume.estimate__)) +
  geom_line() +
  geom_line(aes(y = soft_volume.lower__), color = "blue") +
  geom_line(aes(y = soft_volume.upper__), color = "blue") +
  geom_ribbon(aes(ymin = soft_volume.lower__, ymax = soft_volume.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Cumulative Density Function",
       x = "Soft",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 10000)

#mammal thread volume.
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), "KD", threaddeath))
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("KD"), 1, threaddeath))
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("KND"), 0, threaddeath))

mammal_model_threadv <- brm(threaddeath ~ thread_volume, 
                    data = mammals, 
                    control = list(adapt_delta = 0.9),
                    family = bernoulli())


summary(mammal_model_threadv)
mvt <- conditional_effects(mammal_model_threadv)
mvt <- as.data.frame(mvt[1])
x <- (log(19, base = exp(1)) + 4.86)/.3

ggplot(mpt, aes(x = thread.thread, y = thread.estimate__)) +
  geom_line() +
  geom_line(aes(y = thread.lower__), color = "blue") +
  geom_line(aes(y = thread.upper__), color = "blue") +
  geom_ribbon(aes(ymin = thread.lower__, ymax = thread.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Mammal thread pieces",
       x = "Pieces",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 50) +
  ylim(0,1)
mpt <- as.data.frame(mpt[1])
x <- (log(19, base = exp(1)) + 4.86)/.3

ggplot(mpt, aes(x = thread.thread, y = thread.estimate__)) +
  geom_line() +
  geom_line(aes(y = thread.lower__), color = "blue") +
  geom_line(aes(y = thread.upper__), color = "blue") +
  geom_ribbon(aes(ymin = thread.lower__, ymax = thread.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Mammal thread pieces",
       x = "Pieces",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 50) +
  ylim(0,1)

#mammal no man thread
mammals_noman <- mammals_noman %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), "KD", threaddeath))
mammals_noman <- mammals_noman %>% mutate(threaddeath = ifelse(threaddeath %in% c("KD"), 1, threaddeath))
mammals_noman <- mammals_noman %>% mutate(threaddeath = ifelse(threaddeath %in% c("KND"), 0, threaddeath))

mammals_noman_model_thread <- brm(threaddeath ~ thread + Age + Size_avg, 
                           data = mammals_noman, 
                           family = bernoulli())

summary(mammals_noman_model_thread)
mntsce <- conditional_effects(mammals_noman_model_thread)
mntsce <- as.data.frame(mntsce[1])
x <- (log(19, base = exp(1)) + 4.78)/.02

ggplot(mntsce, aes(x = thread.thread, y = thread.estimate__)) +
  geom_line() +
  geom_line(aes(y = thread.lower__), color = "blue") +
  geom_line(aes(y = thread.upper__), color = "blue") +
  geom_ribbon(aes(ymin = thread.lower__, ymax = thread.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Mammal thread pieces",
       x = "Thread",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 70) +
  ylim(0,1)

####Bird models####
#hard death pieces
birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("PD", "KD"), 1, harddeath))
birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("KND"), 0, harddeath))
birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("Ind"), NA, harddeath))

#for (i in 1:50) {
# set.seed(123 + i)  # Set a different seed for each simulation
#  na_indices <- which(is.na(birds$harddeath))
#  birds$harddeath[na_indices] <- runif(length(na_indices))

bird_model_hard <- brm(harddeath ~ hard, 
                         data = birds,  control = list(adapt_delta = 0.9),
                         family = bernoulli())

summary(bird_model_hard)
bph <- conditional_effects(bird_model_hard)
bph <- as.data.frame(bph[1])
x <- (log(19, base = exp(1)) + 4.41)/.49

ggplot(bph, aes(x = hard.hard, y = hard.estimate__)) +
  geom_line() +
  geom_line(aes(y = hard.lower__), color = "blue") +
  geom_line(aes(y = hard.upper__), color = "blue") +
  geom_ribbon(aes(ymin = hard.lower__, ymax = hard.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Bird Mortality",
       x = "hard pieces",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 30)

#hard death volume

bird_model_hardv <- brm(harddeath ~ hard_volume, 
                       data = birds, 
                       family = bernoulli())

summary(bird_model_hardv)
bhvce <- conditional_effects(bird_model_hardv)
bhvce <- as.data.frame(bhvce[1])
x <- (log(19, base = exp(1)) + 5.21)/.01

ggplot(bhvce, aes(x = hard_volume.hard_volume, y = hard_volume.estimate__)) +
  geom_line() +
  geom_line(aes(y = hard_volume.lower__), color = "blue") +
  geom_line(aes(y = hard_volume.upper__), color = "blue") +
  geom_ribbon(aes(ymin = hard_volume.lower__, ymax = hard_volume.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Bird Mortality",
       x = "hard pieces",
       y = "Cumulative Probability") +
  theme_minimal() +
  xlim(0, 1000)

#rubber death pieces
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("PD", "KD"), 1, rubberdeath))
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("KND"), 0, rubberdeath))
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("Ind"), NA, rubberdeath))

bird_model_rubber <- brm(rubberdeath ~ rubber, 
                         data = birds,
                         family = bernoulli())

summ_df <- as.data.frame(summary(bird_model_rubber)$fixed)
bpr <- conditional_effects(bird_model_rubber)
bpr <- as.data.frame(bpr[1])
x <- (log(19, base = exp(1)) + 4.97)/1.91

brprange <- seq(0, 15, by = .0001)
brExp <- exp(summ_df[1,1] + summ_df[2,1] * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb <- brExp/(1+brExp)

brExp_U <- exp(summ_df[1,4] + summ_df[2,4] * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb_U <- brExp_U/(1+brExp_U)

brExp_L <- exp(summ_df[1,3] + summ_df[2,3] * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb_L <- brExp_L/(1+brExp_L)

#bird_rubber_pieces <- plot(brProb ~ brprange, type = "l", xlab = "Bird rubber Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

br_df <- data.frame(brProb, brExp, brProb_U, brExp_U, brProb_L, brExp_L, brprange)

ggplot(br_df, aes(x = brprange, y = brProb)) +
  geom_line() +
  geom_line(aes(y = brProb_L), color = "blue") +
  geom_line(aes(y = brProb_U), color = "blue") +
  geom_ribbon(aes(ymin = brProb_L, ymax = brProb_U), fill = "gray", alpha = 0.3) +
  labs(title = "Bird rubber Mortality",
       x = " pieces",
       y = "Cumulative Mortality Probability") +
  theme_minimal() +
  xlim(0, 15)

ggplot(bpr, aes(x = rubber.rubber, y = rubber.estimate__)) +
  geom_line() +
  geom_line(aes(y = rubber.lower__), color = "blue") +
  geom_line(aes(y = rubber.upper__), color = "blue") +
  geom_ribbon(aes(ymin = rubber.lower__, ymax = rubber.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Bird rubber Mortality",
       x = " pieces",
       y = "Cumulative Mortality Probability") +
  theme_minimal() +
  xlim(0, 15)


#rubber death volume
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("Ind"), NA, rubberdeath))
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("PD", "KD"), 1, rubberdeath))
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("KND"), 0, rubberdeath))

bird_model_rubberv <- brm(rubberdeath ~ rubber_volume, 
                         data = birds, 
                         family = bernoulli())

summary(bird_model_rubberv)
brvce <- conditional_effects(bird_model_rubberv)
brvce <- as.data.frame(brvce[1])
x <- (log(19, base = exp(1)) + 4.98)/.04

ggplot(brvce, aes(x = rubber_volume.rubber_volume, y = rubber_volume.estimate__)) +
  geom_line() +
  geom_line(aes(y = rubber_volume.lower__), color = "blue") +
  geom_line(aes(y = rubber_volume.upper__), color = "blue") +
  geom_ribbon(aes(ymin = rubber_volume.lower__, ymax = rubber_volume.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Bird Mortality",
       x = "rubber volume",
       y = "Probability Mortality") +
  theme_minimal() +
  xlim(0, 1500)

#####Turtle models###
#turtle soft pieces #excluded NA here
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("Ind"), NA, softdeath))
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), 1, softdeath))
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))

turtle_model_soft <- brm(softdeath ~ soft + Size_avg, 
                         data = turtles, 
                         family = bernoulli(logit))

summary(turtle_model_soft)
tps <- conditional_effects(turtle_model_soft)
tps <- as.data.frame(tps[1])
x <- (log(19, base = exp(1)) + 1.49)/.2

ggplot(tps, aes(x = soft.soft, y = soft.estimate__)) +
  geom_line() +
  geom_line(aes(y = soft.lower__), color = "blue") +
  geom_line(aes(y = soft.upper__), color = "blue") +
  geom_ribbon(aes(ymin = soft.lower__, ymax = soft.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Turtle soft pieces Mortality ",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 300)

#turtle soft volume <- need to switch it to volume
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("Ind"), NA, softdeath))
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), 1, softdeath))
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))

turtle_model_softv <- brm(softdeath ~ soft_volume, 
                         data = turtles, 
                         family = bernoulli())

summary(turtle_model_softv)
tsv <- conditional_effects(turtle_model_softv)
tsv <- as.data.frame(tsv[1])
x <- (log(19, base = exp(1)) + 5.34)/.00

ggplot(tsv, aes(x = soft_volume.soft_volume, y = soft_volume.estimate__)) +
  geom_line() +
  geom_line(aes(y = soft_volume.lower__), color = "blue") +
  geom_line(aes(y = soft_volume.upper__), color = "blue") +
  geom_ribbon(aes(ymin = soft_volume.lower__, ymax = soft_volume.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Turtle soft volume Mortality ",
       x = "volume",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)

#turtle hard pieces
turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("Ind"), NA, harddeath))
turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("PD", "KD"), 1, harddeath))
turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("KND"), 0, harddeath))

turtle_model_hard <- brm(harddeath ~ hard + Size_avg, 
                         data = turtles, 
                         family = bernoulli(logit))

summary(turtle_model_hard)
thce <- conditional_effects(turtle_model_hard)
thce <- as.data.frame(thce[1])
x <- (log(19, base = exp(1)) + 5.77)/.06

ggplot(thce, aes(x = hard.hard, y = hard.estimate__)) +
  geom_line() +
  geom_line(aes(y = hard.lower__), color = "blue") +
  geom_line(aes(y = hard.upper__), color = "blue") +
  geom_ribbon(aes(ymin = hard.lower__, ymax = hard.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Turtle hard pieces Mortality ",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 350)

#turtle thread pieces
turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("Ind"), 1, threaddeath))
turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), 1, threaddeath))
turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("KND"), 0, threaddeath))

turtle_model_thread <- brm(threaddeath ~ thread + S, 
                         data = turtles, 
                         family = bernoulli(logit))

summary(turtle_model_thread)
ttce <- conditional_effects(turtle_model_thread)
ttce <- as.data.frame(ttce[1])

ggplot(ttce, aes(x = thread.thread, y = thread.estimate__)) +
  geom_line() +
  geom_line(aes(y = thread.lower__), color = "blue") +
  geom_line(aes(y = thread.upper__), color = "blue") +
  geom_ribbon(aes(ymin = thread.lower__, ymax = thread.upper__), fill = "gray", alpha = 0.3) +
  labs(title = "Turtle Mortality ",
       x = "Thread",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 250)