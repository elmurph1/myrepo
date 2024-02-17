####packages and libraries#####
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
turtles <- subset(df, Taxa == "Turtle")
turtles_infant <- subset(turtles, Age == "Infant")
turtles_juvenile <- subset(turtles, Age == "Juvenile")
turtles_young <- rbind(turtles_infant, turtles_juvenile)
mammals_noman <- subset(df, Taxa == "Mammal" & Citation != "Chris Robbins Data")

####Data cleaning for all models#####
#Turn cause of death categories into binary categories of 1 or 0 for all plastic deaths
#Exclude indeterminate deaths
birds <- birds %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), 1, COD))
birds <- birds %>% mutate(COD = ifelse(COD %in% c("KND"), 0, COD))
birds <- birds %>% mutate(COD = ifelse(COD %in% c("Ind", "KDO", "PDO"), NA, COD))

mammals <- mammals %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), 1, COD))
mammals <- mammals %>% mutate(COD = ifelse(COD %in% c("KND"), 0, COD))
mammals <- mammals %>% mutate(COD = ifelse(COD %in% c("Ind", "KDO", "PDO"), NA, COD))

mammals_noman <- mammals_noman %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), 1, COD))
mammals_noman <- mammals_noman %>% mutate(COD = ifelse(COD %in% c("KND"), 0, COD))
mammals_noman <- mammals_noman %>% mutate(COD = ifelse(COD %in% c("Ind", "KDO", "PDO"), NA, COD))

turtles <- turtles %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), 1, COD))
turtles <- turtles %>% mutate(COD = ifelse(COD %in% c("KND"), 0, COD))
turtles <- turtles %>% mutate(COD = ifelse(COD %in% c("Ind", "KDO", "PDO"), NA, COD))

turtles_young <- turtles_young %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), 1, COD))
turtles_young <- turtles_young %>% mutate(COD = ifelse(COD %in% c("KND"), 0, COD))
turtles_young <- turtles_young %>% mutate(COD = ifelse(COD %in% c("Ind", "KDO", "PDO"), NA, COD))

###... for hard plastic deaths
birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("PD", "KD"), 1, harddeath))
birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("KND"), 0, harddeath))
birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("Ind", "KDO", "PDO"), NA, harddeath))

turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("Ind", "KDO", "PDO"), NA, harddeath))
turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("PD", "KD"), 1, harddeath))
turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("KND"), 0, harddeath))

###... for rubber deaths
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("PD", "KD"), 1, rubberdeath))
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("KND"), 0, rubberdeath))
birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("Ind", "KDO", "PDO"), NA, rubberdeath))

###.. for soft deaths
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), "KD", softdeath))
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("KD"), 1, softdeath))
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))
mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("KDO", "PDO"), NA, softdeath))

mammals_noman <- mammals_noman %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), 1, softdeath))
mammals_noman <- mammals_noman %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))
mammals_noman <- mammals_noman %>% mutate(softdeath = ifelse(softdeath %in% c("Ind", "KDO", "PDO"), NA, softdeath))

turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("Ind", "KDO", "PDO"), NA, softdeath))
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), 1, softdeath))
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("KND"), 0, softdeath))

#... for thread deaths
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), "KD", threaddeath))
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("KD"), 1, threaddeath))
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("KND"), 0, threaddeath))
mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("KDO", "PDO"), NA, threaddeath))

mammals_noman <- mammals_noman %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), 1, threaddeath))
mammals_noman <- mammals_noman %>% mutate(threaddeath = ifelse(threaddeath %in% c("KND"), 0, threaddeath))
mammals_noman <- mammals_noman %>% mutate(threaddeath = ifelse(threaddeath %in% c("Ind", "KDO", "PDO"), NA, threaddeath))

turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("Ind", "KDO", "PDO"), NA, threaddeath))
turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), 1, threaddeath))
turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("KND"), 0, threaddeath))

#birds age proportions
binf <- length(which(birds$Age == "Infant"))
bjuv <- length(which(birds$Age == "Juvenile"))
bsa <- length(which(birds$Age == "Subadult"))
bad <- length(which(birds$Age == "Adult"))

pbinf <- binf/ (binf + bjuv + bsa + bad)
pbjuv <- bjuv/ (binf + bjuv + bsa + bad)
pbsa <- bsa/ (binf + bjuv + bsa + bad)
pbad <- bad/ (binf + bjuv + bsa + bad)

#mammals age proportions
minf <- length(which(mammals$Age == "Infant"))
mjuv <- length(which(mammals$Age == "Juvenile"))
msa <- length(which(mammals$Age == "Subadult"))
mad <- length(which(mammals$Age == "Adult"))

pminf <- minf/ (minf + mjuv + msa + mad)
pmjuv <- mjuv/ (minf + mjuv + msa + mad)
pmsa <- msa/ (minf + mjuv + msa + mad)
pmad <- mad/ (minf + mjuv + msa + mad)

#turtle age proportions
tinf <- length(which(turtles$Age == "Infant"))
tjuv <- length(which(turtles$Age == "Juvenile"))
tsa <- length(which(turtles$Age == "Subadult"))
tad <- length(which(turtles$Age == "Adult"))

ptinf <- tinf/ (tinf + tjuv + tsa + tad)
ptjuv <- tjuv/ (tinf + tjuv + tsa + tad)
ptsa <- tsa/ (tinf + tjuv + tsa + tad)
ptad <- tad/ (tinf + tjuv + tsa + tad)

#model prior
priors <- c(prior(normal(0,1), class = Intercept))

###Calculating mean piece size####
#...birds total pieces
birds_total_mean <- subset(birds, !is.na(birds$total))
birds_total_mean <- subset(birds_total_mean, !is.na(birds_total_mean$volume_estimate))
sum(birds_total_mean$volume_estimate)/sum(birds_total_mean$total)

#...birds hard pieces
birds_hard_mean <- subset(birds, !is.na(birds$hard))
birds_hard_mean <- subset(birds_hard_mean, !is.na(birds_hard_mean$hard_volume))
sum(birds_hard_mean$hard_volume)/sum(birds_hard_mean$hard)

#...birds rubber pieces
birds_rubber_mean <- subset(birds, !is.na(birds$rubber))
birds_rubber_mean <- subset(birds_rubber_mean, !is.na(birds_rubber_mean$rubber_volume))
sum(birds_rubber_mean$rubber_volume)/sum(birds_rubber_mean$rubber)

#...mammals total pieces
mammals_total_mean <- subset(mammals, !is.na(mammals$total))
mammals_total_mean <- subset(mammals_total_mean, !is.na(mammals_total_mean$volume_estimate))
sum(mammals_total_mean$volume_estimate)/sum(mammals_total_mean$total)

#...mammals soft pieces
mammals_soft_mean <- subset(mammals, !is.na(mammals$soft))
mammals_soft_mean <- subset(mammals_soft_mean, !is.na(mammals_soft_mean$soft_volume))
sum(mammals_soft_mean$soft_volume)/sum(mammals_soft_mean$soft)

#...mammals thread pieces
mammals_thread_mean <- subset(mammals, !is.na(mammals$thread))
mammals_thread_mean <- subset(mammals_thread_mean, !is.na(mammals_thread_mean$line_length))
sum(mammals_thread_mean$line_length)/sum(mammals_thread_mean$thread)

#...turtles total pieces
turtles_total_mean <- subset(turtles, !is.na(turtles$total))
turtles_total_mean <- subset(turtles_total_mean, !is.na(turtles_total_mean$volume_estimate))
sum(turtles_total_mean$volume_estimate)/sum(turtles_total_mean$total)

#...turtles hard pieces
turtles_hard_mean <- subset(turtles, !is.na(turtles$hard))
turtles_hard_mean <- subset(turtles_hard_mean, !is.na(turtles_hard_mean$hard_volume))
sum(turtles_hard_mean$hard_volume)/sum(turtles_hard_mean$hard)

###Bird total pieces####
bird_model_all <- brm(COD ~ total, prior = priors, data = birds, 
                      control = list(adapt_delta = 0.9),  
                                     family = bernoulli()) 

loo_bird_model_all <- loo(bird_model_all) #168.9 pareto k < 0.5

#B is for the birds model with no additional factors
summ_b <- as.data.frame(summary(bird_model_all)$fixed)
brange <- seq(0, 45, by = .0001)
bExp <- exp(summ_b[1,1] + summ_b[2,1] * brange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
bProb <- bExp/(1+bExp)

bExp_U <- exp(summ_b[1,4] + summ_b[2,4] * brange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
bProb_U <- bExp_U/(1+bExp_U)

bExp_L <- exp(summ_b[1,3] + summ_b[2,3] * brange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
bProb_L <- bExp_L/(1+bExp_L)
b_df <- data.frame(bProb, bExp, bProb_U, bExp_U, bProb_L, bExp_L, brange)

ggplot(b_df, aes(x = brange, y = bProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bProb_L, ymax = bProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 45)

#Calculate x for 95% mortality
birdtotal95 <- (log(19, base = exp(1)) - summ_b[1,1])/summ_b[2,1]
birdtotal95_U <- (log(19, base = exp(1)) - summ_b[1,4])/summ_b[2,4]
birdtotal95_L <- (log(19, base = exp(1)) - summ_b[1,3])/summ_b[2,3]
birdtotal50 <- (log(1, base = exp(1)) - summ_b[1,1])/summ_b[2,1]
birdtotal50_U <- (log(1, base = exp(1)) - summ_b[1,4])/summ_b[2,4]
birdtotal50_L <- (log(1, base = exp(1)) - summ_b[1,3])/summ_b[2,3]

###Bird total pieces + Age####
bird_model_all_ba <- brm(COD ~ total + Age, prior = priors, data = birds, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli()) 

loo_bird_model_all_ba <- loo(bird_model_all_ba) #162.4 900 pareto k were good and 1 was very bad. 

#Ba is for the birds model with Age
summ_ba <- as.data.frame(summary(bird_model_all_ba)$fixed)
barange <- seq(0, 45, by = .0001)

baExp <- exp(summ_ba[1,1] + summ_ba[2,1] * barange + summ_ba[3,1] * pbinf + summ_ba[4,1] * pbjuv + summ_ba[5,1] *pbsa)
baProb <- baExp/(1+baExp)

baExp_U <- exp(summ_ba[1,4] + summ_ba[2,4] * barange + summ_ba[3,4] * pbinf + summ_ba[4,4] * pbjuv + summ_ba[5,4] *pbsa) 
baProb_U <- baExp_U/(1+baExp_U)

baExp_L <- exp(summ_ba[1,3] + summ_ba[2,3] * barange + summ_ba[3,3] * pbinf + summ_ba[4,3] * pbjuv + summ_ba[5,3] *pbsa)  
baProb_L <- baExp_L/(1+baExp_L)
ba_df <- data.frame(baProb, baExp, baProb_U, baExp_U, baProb_L, baExp_L, barange)

ggplot(ba_df, aes(x = barange, y = baProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = baProb_L, ymax = baProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 45)

#Calculate x for 95% mortality
birdtotalage95 <- (log(19, base = exp(1)) - summ_ba[1,1])/summ_ba[2,1]
birdtotalage50 <- (log(1, base = exp(1)) - summ_ba[1,1])/summ_ba[2,1]

###Bird total pieces + Species_size####
bird_model_all_ss <- brm(COD ~ total + Size_avg, prior = priors, data = birds, 
                      control = list(adapt_delta = 0.9),  
                      family = bernoulli()) 

loo_bird_model_all_ss <- loo(bird_model_all_ss) #170.6 all pareto k <0.5

#Bs is for the birds model with species size
summ_bs <- as.data.frame(summary(bird_model_all_ss)$fixed)
bsrange <- seq(0, 45, by = .0001)
bsExp <- exp(summ_bs[1,1] + summ_bs[2,1] * bsrange + summ_bs[3,1] * mean(birds$Size_avg, na.rm=TRUE))
bsProb <- bsExp/(1+bsExp)

bsExp_U <- exp(summ_bs[1,4] + summ_bs[2,4] * bsrange+ summ_bs[3,4] * mean(birds$Size_avg, na.rm=TRUE)) 
bsProb_U <- bsExp_U/(1+bsExp_U)

bsExp_L <- exp(summ_bs[1,3] + summ_bs[2,3] * bsrange+ summ_bs[3,3] * mean(birds$Size_avg, na.rm=TRUE))  
bsProb_L <- bsExp_L/(1+bsExp_L)
bs_df <- data.frame(bsProb, bsExp, bsProb_U, bsExp_U, bsProb_L, bsExp_L, bsrange)

ggplot(bs_df, aes(x = bsrange, y = bsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bsProb_L, ymax = bsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 45)

#Calculate x for 95% mortality
birdtotalsize95 <- (log(19, base = exp(1)) - summ_bs[1,1])/summ_bs[2,1]
birdtotalsize50 <- (log(1, base = exp(1)) - summ_bs[1,1])/summ_bs[2,1]

###Bird total volume####
bird_model_allv <- brm(COD ~ volume_estimate, prior = priors, data = birds, 
                      control = list(adapt_delta = 0.9),  
                      family = bernoulli())

loo_bird_model_allv <- loo(bird_model_allv) #163.1 pareto k = 1011 good, 1 okay, 4 bad, 3 very bad

#bv birds volume model with no additional factors
summ_bv <- as.data.frame(summary(bird_model_allv)$fixed)
bvrange <- seq(0, 7000, by = .01)
bvExp <- exp(summ_bv[1,1] + summ_bv[2,1] * bvrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
bvProb <- bvExp/(1+bvExp)

bvExp_U <- exp(summ_bv[1,4] + summ_bv[2,4] * bvrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
bvProb_U <- bvExp_U/(1+bvExp_U)

bvExp_L <- exp(summ_bv[1,3] + summ_bv[2,3] * bvrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
bvProb_L <- bvExp_L/(1+bvExp_L)
bv_df <- data.frame(bvProb, bvExp, bvProb_U, bvExp_U, bvProb_L, bvExp_L, bvrange)

ggplot(bv_df, aes(x = bvrange, y = bvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bvProb_L, ymax = bvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

#Calculate x for 95% mortality
birdtotalv95 <- (log(19, base = exp(1)) - summ_bv[1,1])/summ_bv[2,1]
birdtotalv95_U <- (log(19, base = exp(1)) - summ_bv[1,4])/summ_bv[2,4]
birdtotalv95_L <- (log(19, base = exp(1)) - summ_bv[1,3])/summ_bv[2,3]
birdtotalv50 <- (log(1, base = exp(1)) - summ_bv[1,1])/summ_bv[2,1]
birdtotalv50_U <- (log(1, base = exp(1)) - summ_bv[1,4])/summ_bv[2,4]
birdtotalv50_L <- (log(1, base = exp(1)) - summ_bv[1,3])/summ_bv[2,3]

###Bird total volume + age ####
bird_model_allva <- brm(COD ~ volume_estimate + Age, prior = priors, data = birds, 
                       control = list(adapt_delta = 0.9),  
                       family = bernoulli())

loo_bird_model_allv <- loo(bird_model_allva)  #157.4 pareto k = 865 good, 4 okay, 2 bad, 2 very bad

#BVA is for birds volume model with age
summ_bva <- as.data.frame(summary(bird_model_allva)$fixed)
bvarange <- seq(0, 10000, by = .01)

bvaExp <- exp(summ_bva[1,1] + summ_bva[2,1] * bvarange + summ_bva[3,1] * pbinf + summ_bva[4,1] * pbjuv + summ_bva[5,1] *pbsa)
bvaProb <- bvaExp/(1+bvaExp)

bvaExp_U <- exp(summ_bva[1,4] + summ_bva[2,4] * bvarange + summ_bva[3,4] * pbinf + summ_bva[4,4] * pbjuv + summ_bva[5,4] *pbsa) 
bvaProb_U <- bvaExp_U/(1+bvaExp_U)

bvaExp_L <- exp(summ_bva[1,3] + summ_bva[2,3] * bvarange + summ_bva[3,3] * pbinf + summ_bva[4,3] * pbjuv + summ_bva[5,3] *pbsa)  
bvaProb_L <- bvaExp_L/(1+bvaExp_L)
bva_df <- data.frame(bvaProb, bvaExp, bvaProb_U, bvaExp_U, bvaProb_L, bvaExp_L, bvarange)

ggplot(bva_df, aes(x = bvarange, y = bvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bvaProb_L, ymax = bvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds w/age",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

#Calculate x for 95% mortality
birdtotalva95 <- (log(19, base = exp(1)) - summ_bva[1,1])/summ_bva[2,1]
birdtotalva50 <- (log(1, base = exp(1)) - summ_bva[1,1])/summ_bva[2,1]

###Bird total volume + Species_size####
bird_model_allv_ss <- brm(COD ~ volume_estimate + Size_avg, 
                         prior = priors, data = birds, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli()) 

loo_bird_model_allv_ss <- loo(bird_model_allv_ss)  #164.0 pareto k = 1001 good, 3 okay, 3 bad, 2 very bad

summ_bvs <- as.data.frame(summary(bird_model_allv_ss)$fixed)
bvsrange <- seq(0, 8000, by = .01)
bvsExp <- exp(summ_bvs[1,1] + summ_bvs[2,1] * bvsrange + summ_bvs[3,1] * mean(birds$Size_avg, na.rm=TRUE))
bvsProb <- bvsExp/(1+bvsExp)

bvsExp_U <- exp(summ_bvs[1,4] + summ_bvs[2,4] * bvsrange+ summ_bvs[3,4] * mean(birds$Size_avg, na.rm=TRUE)) 
bvsProb_U <- bvsExp_U/(1+bvsExp_U)

bvsExp_L <- exp(summ_bvs[1,3] + summ_bvs[2,3] * bvsrange+ summ_bvs[3,3] * mean(birds$Size_avg, na.rm=TRUE))  
bvsProb_L <- bvsExp_L/(1+bvsExp_L)
bvs_df <- data.frame(bvsProb, bvsExp, bvsProb_U, bvsExp_U, bvsProb_L, bvsExp_L, bvsrange)

ggplot(bvs_df, aes(x = bvsrange, y = bvsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bvsProb_L, ymax = bvsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds w/ species size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

#Calculate x for 95% mortality
birdtotalvs95 <- (log(19, base = exp(1)) - summ_bvs[1,1])/summ_bvs[2,1]
birdtotalvs50 <- (log(1, base = exp(1)) - summ_bvs[1,1])/summ_bvs[2,1]

###Bird Hard Pieces####
bird_model_hard <- brm(harddeath ~ hard, prior = priors,
                       data = birds,  control = list(adapt_delta = 0.9),
                       family = bernoulli())

loo_bird_model_hard <- loo(bird_model_hard)  #110 pareto k =  all good

#BH is birds hard pieces model with no additional factors
summ_bh <- as.data.frame(summary(bird_model_hard)$fixed)
bhrange <- seq(0, 50, by = .0001)
bhExp <- exp(summ_bh[1,1] + summ_bh[2,1] * bhrange) 
bhProb <- bhExp/(1+bhExp)

bhExp_U <- exp(summ_bh[1,4] + summ_bh[2,4] * bhrange) 
bhProb_U <- bhExp_U/(1+bhExp_U)

bhExp_L <- exp(summ_bh[1,3] + summ_bh[2,3] * bhrange)
bhProb_L <- bhExp_L/(1+bhExp_L)
bh_df <- data.frame(bhProb, bhExp, bhProb_U, bhExp_U, bhProb_L, bhExp_L, bhrange)

ggplot(bh_df, aes(x = bhrange, y = bhProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bhProb_L, ymax = bhProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by birds",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 50)

#Calculate x for 95% mortality
birdhard95 <- (log(19, base = exp(1)) - summ_bh[1,1])/summ_bh[2,1]
birdhard95_U <- (log(19, base = exp(1)) - summ_bh[1,4])/summ_bh[2,4]
birdhard95_L <- (log(19, base = exp(1)) - summ_bh[1,3])/summ_bh[2,3]
birdhard50 <- (log(1, base = exp(1)) - summ_bh[1,1])/summ_bh[2,1]
birdhard50_U <- (log(1, base = exp(1)) - summ_bh[1,4])/summ_bh[2,4]
birdhard50_L <- (log(1, base = exp(1)) - summ_bh[1,3])/summ_bh[2,3]

###Birds Hard Pieces + Age#####
bird_model_harda <- brm(harddeath ~ hard + Age, prior = priors,
                       data = birds,  control = list(adapt_delta = 0.9),
                       family = bernoulli())

loo_bird_model_harda <- loo(bird_model_harda)  #109.7 pareto K = 954 is good, 2 is okay, 1 very bad

summ_bha <- as.data.frame(summary(bird_model_harda)$fixed)
bharange <- seq(0, 60, by = .0001)

bhaExp <- exp(summ_bha[1,1] + summ_bha[2,1] * bharange + summ_bha[3,1] * pbinf + summ_bha[4,1] * pbjuv + summ_bha[5,1] *pbsa)
bhaProb <- bhaExp/(1+bhaExp)

bhaExp_U <- exp(summ_bha[1,4] + summ_bha[2,4] * bharange + summ_bha[3,4] * pbinf + summ_bha[4,4] * pbjuv + summ_bha[5,4] *pbsa) 
bhaProb_U <- bhaExp_U/(1+bhaExp_U)

bhaExp_L <- exp(summ_bha[1,3] + summ_bha[2,3] * bharange + summ_bha[3,3] * pbinf + summ_bha[4,3] * pbjuv + summ_bha[5,3] *pbsa)  
bhaProb_L <- bhaExp_L/(1+bhaExp_L)
bha_df <- data.frame(bhaProb, bhaExp, bhaProb_U, bhaExp_U, bhaProb_L, bhaExp_L, bharange)

ggplot(bha_df, aes(x = bharange, y = bhaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bhaProb_L, ymax = bhaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard Plastic ingestion by birds w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 60)

#Calculate x for 95% mortality
birdharda95 <- (log(19, base = exp(1)) - summ_bha[1,1])/summ_bha[2,1]
birdharda50 <- (log(1, base = exp(1)) - summ_bha[1,1])/summ_bha[2,1]

###Birds Hard Pieces + Species Size#####
bird_model_hards <- brm(harddeath ~ hard + Size_avg, prior = priors,
                        data = birds,  control = list(adapt_delta = 0.9),
                        family = bernoulli())

loo_bird_model_hards <- loo(bird_model_hards)  #111.7 pareto K < 0.5

#BHS is for birds hard species size
summ_bhs <- as.data.frame(summary(bird_model_hards)$fixed)
bhsrange <- seq(0, 60, by = .0001)
bhsExp <- exp(summ_bhs[1,1] + summ_bhs[2,1] * bhsrange + summ_bhs[3,1] * mean(birds$Size_avg, na.rm=TRUE))
bhsProb <- bhsExp/(1+bhsExp)

bhsExp_U <- exp(summ_bhs[1,4] + summ_bhs[2,4] * bhsrange+ summ_bhs[3,4] * mean(birds$Size_avg, na.rm=TRUE)) 
bhsProb_U <- bhsExp_U/(1+bhsExp_U)

bhsExp_L <- exp(summ_bhs[1,3] + summ_bhs[2,3] * bhsrange+ summ_bhs[3,3] * mean(birds$Size_avg, na.rm=TRUE))  
bhsProb_L <- bhsExp_L/(1+bhsExp_L)
bhs_df <- data.frame(bhsProb, bhsExp, bhsProb_U, bhsExp_U, bhsProb_L, bhsExp_L, bhsrange)

ggplot(bhs_df, aes(x = bhsrange, y = bhsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bhsProb_L, ymax = bhsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by birds w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 50)

#Calculate x for 95% mortality
birdhardsize95 <- (log(19, base = exp(1)) - summ_bhs[1,1])/summ_bhs[2,1]
birdhardsize50 <- (log(1, base = exp(1)) - summ_bhs[1,1])/summ_bhs[2,1]

###Bird Hard Volume####
bird_model_hardv <- brm(harddeath ~ hard_volume,  prior = priors,
                       data = birds, 
                       family = bernoulli())

loo_bird_model_hardv <- loo(bird_model_hardv)  #120.8 pareto K = 1050 good, 2 okay

#BHV bird hard volume model 
summ_bhv <- as.data.frame(summary(bird_model_hardv)$fixed)
bhvrange <- seq(0, 2000, by = .01)
bhvExp <- exp(summ_bhv[1,1] + summ_bhv[2,1] * bhvrange)
bhvProb <- bhvExp/(1+bhvExp)

bhvExp_U <- exp(summ_bhv[1,4] + summ_bhv[2,4] * bhvrange)
bhvProb_U <- bhvExp_U/(1+bhvExp_U)

bhvExp_L <- exp(summ_bhv[1,3] + summ_bhv[2,3] * bhvrange)
bhvProb_L <- bhvExp_L/(1+bhvExp_L)
bhv_df <- data.frame(bhvProb, bhvExp, bhvProb_U, bhvExp_U, bhvProb_L, bhvExp_L, bhvrange)

ggplot(bhv_df, aes(x = bhvrange, y = bhvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bhvProb_L, ymax = bhvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by birds",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1500)

#Calculate x for 95% mortality
birdhardv95 <- (log(19, base = exp(1)) - summ_bhv[1,1])/summ_bhv[2,1]
birdhardv95_U <- (log(19, base = exp(1)) - summ_bhv[1,4])/summ_bhv[2,4]
birdhardv95_L <- (log(19, base = exp(1)) - summ_bhv[1,3])/summ_bhv[2,3]
birdhardv50 <- (log(1, base = exp(1)) - summ_bhv[1,1])/summ_bhv[2,1]
birdhardv50_U <- (log(1, base = exp(1)) - summ_bhv[1,4])/summ_bhv[2,4]
birdhardv50_L <- (log(1, base = exp(1)) - summ_bhv[1,3])/summ_bhv[2,3]

###Bird Hard Volume with age#### 
#Sample size is too low. cannot include
bird_model_hardva <- brm(harddeath ~ hard_volume + Age,  prior = priors,
                        data = birds, 
                        family = bernoulli())

loo_bird_model_hardva <- loo(bird_model_hardva)  #111.5 pareto K = 1094 good, 1 okay

summ_bhva <- as.data.frame(summary(bird_model_allva)$fixed)
bhvarange <- seq(0, 9000, by = .01)

bhvaExp <- exp(summ_bhva[1,1] + summ_bhva[2,1] * bhvarange + summ_bhva[3,1] * pbinf + summ_bhva[4,1] * pbjuv + summ_bhva[5,1] *pbsa)
bhvaProb <- bhvaExp/(1+bhvaExp)

bhvaExp_U <- exp(summ_bhva[1,4] + summ_bhva[2,4] * bhvarange + summ_bhva[3,4] * pbinf + summ_bhva[4,4] * pbjuv + summ_bhva[5,4] *pbsa) 
bhvaProb_U <- bhvaExp_U/(1+bhvaExp_U)

bhvaExp_L <- exp(summ_bhva[1,3] + summ_bhva[2,3] * bhvarange + summ_bhva[3,3] * pbinf + summ_bhva[4,3] * pbjuv + summ_bhva[5,3] *pbsa)  
bhvaProb_L <- bhvaExp_L/(1+bhvaExp_L)
bhva_df <- data.frame(bhvaProb, bhvaExp, bhvaProb_U, bhvaExp_U, bhvaProb_L, bhvaExp_L, bhvarange)

ggplot(bhva_df, aes(x = bhvarange, y = bhvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bhvaProb_L, ymax = bhvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 9000)

#Calculate x for 95% mortality
birdhardvage95 <- (log(19, base = exp(1)) - summ_bhva[1,1])/summ_bhva[2,1]
birdhardvage50 <- (log(1, base = exp(1)) - summ_bhva[1,1])/summ_bhva[2,1]

###Bird Hard Volume with species size####
bird_model_hardvs <- brm(harddeath ~ hard_volume + Size_avg,  prior = priors,
                        data = birds, 
                        family = bernoulli()) 

loo_bird_model_hardvs <- loo(bird_model_hardvs)  #122.6 pareto K < 0.5

summ_bhvs <- as.data.frame(summary(bird_model_hardvs)$fixed)
bhvsrange <- seq(0, 2500, by = .01)
bhvsExp <- exp(summ_bhvs[1,1] + summ_bhvs[2,1] * bhvsrange + summ_bhvs[3,1] * mean(birds$Size_avg, na.rm=TRUE))
bhvsProb <- bhvsExp/(1+bhvsExp)

bhvsExp_U <- exp(summ_bhvs[1,4] + summ_bhvs[2,4] * bhvsrange+ summ_bhvs[3,4] * mean(birds$Size_avg, na.rm=TRUE)) 
bhvsProb_U <- bhvsExp_U/(1+bhvsExp_U)

bhvsExp_L <- exp(summ_bhvs[1,3] + summ_bhvs[2,3] * bhvsrange+ summ_bhvs[3,3] * mean(birds$Size_avg, na.rm=TRUE))  
bhvsProb_L <- bhvsExp_L/(1+bhvsExp_L)
bhvs_df <- data.frame(bhvsProb, bhvsExp, bhvsProb_U, bhvsExp_U, bhvsProb_L, bhvsExp_L, bhvsrange)

ggplot(bhvs_df, aes(x = bhvsrange, y = bhvsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = bhvsProb_L, ymax = bhvsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by birds w/ species size",
       x = "volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1500)

#Calculate x for 95% mortality
birdhardvsize95 <- (log(19, base = exp(1)) - summ_bhvs[1,1])/summ_bhvs[2,1]
birdhardvsize50 <- (log(1, base = exp(1)) - summ_bhvs[1,1])/summ_bhvs[2,1]

###Bird Rubber Pieces####
bird_model_rubber <- brm(rubberdeath ~ rubber, prior = priors,
                         data = birds,
                         family = bernoulli())

loo_bird_model_rubber <- loo(bird_model_rubber)  #66.7 pareto K = 1404 good, 2 okay

#BR birds rubber pieces model with no additional factors
summ_br <- as.data.frame(summary(bird_model_rubber)$fixed)
brprange <- seq(0, 20, by = .0001)
brExp <- exp(summ_br[1,1] + summ_br[2,1] * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb <- brExp/(1+brExp)

brExp_U <- exp(summ_br[1,4] + summ_br[2,4] * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb_U <- brExp_U/(1+brExp_U)

brExp_L <- exp(summ_br[1,3] + summ_br[2,3] * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb_L <- brExp_L/(1+brExp_L)
br_df <- data.frame(brProb, brExp, brProb_U, brExp_U, brProb_L, brExp_L, brprange)

ggplot(br_df, aes(x = brprange, y = brProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = brProb_L, ymax = brProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Rubber ingestion by birds",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20)

#Calculate x for 95% mortality
birdrubber95 <- (log(19, base = exp(1)) - summ_br[1,1])/summ_br[2,1]
birdrubber95_U <- (log(19, base = exp(1)) - summ_br[1,4])/summ_br[2,4]
birdrubber95_L <- (log(19, base = exp(1)) - summ_br[1,3])/summ_br[2,3]
birdrubber50 <- (log(1, base = exp(1)) - summ_br[1,1])/summ_br[2,1]
birdrubber50_U <- (log(1, base = exp(1)) - summ_br[1,4])/summ_br[2,4]
birdrubber50_L <- (log(1, base = exp(1)) - summ_br[1,3])/summ_br[2,3]

###Bird Rubber Pieces + Age####
#sample size too small
bird_model_rubbera <- brm(rubberdeath ~ rubber + Age, prior = priors,
                         data = birds,
                         family = bernoulli())

loo_bird_model_rubbera <- loo(bird_model_rubbera)  #63.1 pareto K = 1254 good, 2 okay, 1 bad, 1 very bad
 
binf <- length(which(birds$Age == "Infant"))
bjuv <- length(which(birds$Age == "Juvenile"))
bsa <- length(which(birds$Age == "Subadult"))
bad <- length(which(birds$Age == "Adult"))

pbinf <- binf/ (binf + bjuv + bsa + bad)
pbjuv <- bjuv/ (binf + bjuv + bsa + bad)
pbsa <- bsa/ (binf + bjuv + bsa + bad)
pbad <- bad/ (binf + bjuv + bsa + bad)

summ_bra <- as.data.frame(summary(bird_model_rubbera)$fixed)
brarange <- seq(0, 45, by = .0001)

braExp <- exp(summ_bra[1,1] + summ_bra[2,1] * brarange + summ_bra[3,1] * pbinf + summ_bra[4,1] * pbjuv + summ_bra[5,1] *pbsa)
braProb <- braExp/(1+braExp)

braExp_U <- exp(summ_bra[1,4] + summ_bra[2,4] * brarange + summ_bra[3,4] * pbinf + summ_bra[4,4] * pbjuv + summ_bra[5,4] *pbsa) 
braProb_U <- braExp_U/(1+braExp_U)

braExp_L <- exp(summ_bra[1,3] + summ_bra[2,3] * brarange + summ_bra[3,3] * pbinf + summ_bra[4,3] * pbjuv + summ_bra[5,3] *pbsa)  
braProb_L <- braExp_L/(1+braExp_L)
bra_df <- data.frame(braProb, braExp, braProb_U, braExp_U, braProb_L, braExp_L, brarange)

ggplot(bra_df, aes(x = brarange, y = braProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = braProb_L, ymax = braProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Rubber ingestion by birds w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 40)

#Calculate x for 95% mortality
birdrubberage95 <- (log(19, base = exp(1)) - summ_bra[1,1])/summ_bra[2,1]
birdrubberage50 <- (log(1, base = exp(1)) - summ_bra[1,1])/summ_bra[2,1]

###Bird Rubber Pieces + Species size####
bird_model_rubbers <- brm(rubberdeath ~ rubber + Size_avg, prior = priors,
                         data = birds,
                         family = bernoulli())

loo_bird_model_rubbers <- loo(bird_model_rubbers)  ##68.1 pareto K = 1394 good, 1 okay, 1 bad

#BRS is Birds Rubber species size model
summ_brs <- as.data.frame(summary(bird_model_rubbers)$fixed)
brsrange <- seq(0, 45, by = .0001)
brsExp <- exp(summ_brs[1,1] + summ_brs[2,1] * brsrange + summ_brs[3,1] * mean(birds$Size_avg, na.rm=TRUE))
brsProb <- brsExp/(1+brsExp)

brsExp_U <- exp(summ_brs[1,4] + summ_brs[2,4] * brsrange+ summ_brs[3,4] * mean(birds$Size_avg, na.rm=TRUE)) 
brsProb_U <- brsExp_U/(1+brsExp_U)

brsExp_L <- exp(summ_brs[1,3] + summ_brs[2,3] * brsrange+ summ_brs[3,3] * mean(birds$Size_avg, na.rm=TRUE))  
brsProb_L <- brsExp_L/(1+brsExp_L)
brs_df <- data.frame(brsProb, brsExp, brsProb_U, brsExp_U, brsProb_L, brsExp_L, brsrange)

ggplot(brs_df, aes(x = brsrange, y = brsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = brsProb_L, ymax = brsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Rubber ingestion by birds w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20)

#Calculate x for 95% mortality
birdrubbersize95 <- (log(19, base = exp(1)) - summ_brs[1,1])/summ_brs[2,1]

###Bird Rubber Volume#####
bird_model_rv <- brm(rubberdeath ~ rubber_volume, 
                     prior = priors, data = birds, 
                     family = bernoulli())

loo_bird_model_rv <- loo(bird_model_rv)  ##58.3 pareto K = 1395 good, 1 okay, 1 bad, 1 very bad

#BRV is the birds rubber volume model
summ_brv <- as.data.frame(summary(bird_model_rv)$fixed)
brvrange <- seq(0, 2500, by = 1)
brvExp <- exp(summ_brv[1,1] + summ_brv[2,1] * brvrange)
brvProb <- brvExp/(1+brvExp)

brvExp_U <- exp(summ_brv[1,4] + summ_brv[2,4] * brvrange)
brvProb_U <- brvExp_U/(1+brvExp_U)

brvExp_L <- exp(summ_brv[1,3] + summ_brv[2,3] * brvrange)
brvProb_L <- brvExp_L/(1+brvExp_L)
brv_df <- data.frame(brvProb, brvExp, brvProb_U, brvExp_U, brvProb_L, brvExp_L, brvrange)

ggplot(brv_df, aes(x = brvrange, y = brvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = brvProb_L, ymax = brvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Rubber ingestion by birds",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2500)

#Calculate x for 95% mortality
birdrubberv95 <- (log(19, base = exp(1)) - summ_brv[1,1])/summ_brv[2,1]
birdrubberv95_U <- (log(19, base = exp(1)) - summ_brv[1,4])/summ_brv[2,4]
birdrubberv95_L <- (log(19, base = exp(1)) - summ_brv[1,3])/summ_brv[2,3]
birdrubberv50 <- (log(1, base = exp(1)) - summ_brv[1,1])/summ_brv[2,1]
birdrubberv50_U <- (log(1, base = exp(1)) - summ_brv[1,4])/summ_brv[2,4]
birdrubberv50_L <- (log(1, base = exp(1)) - summ_brv[1,3])/summ_brv[2,3]

###Bird Rubber Volume + Age#####
bird_model_rva <- brm(rubberdeath ~ rubber_volume + Age, 
                     prior = priors, data = birds, 
                     control = list(adapt_delta = 0.9), 
                     family = bernoulli())

loo_bird_model_rva <- loo(bird_model_rva)  ##55.2 pareto K = 1247 good, 1 bad, 2 very bad

#BRVA model is Birds rubber volume with Age
summ_brva <- as.data.frame(summary(bird_model_rva)$fixed)
brvarange <- seq(0, 4000, by = .01)

brvaExp <- exp(summ_brva[1,1] + summ_brva[2,1] * brvarange + summ_brva[3,1] * pbinf + summ_brva[4,1] * pbjuv + summ_brva[5,1] *pbsa)
brvaProb <- brvaExp/(1+brvaExp)

brvaExp_U <- exp(summ_brva[1,4] + summ_brva[2,4] * brvarange + summ_brva[3,4] * pbinf + summ_brva[4,4] * pbjuv + summ_brva[5,4] *pbsa) 
brvaProb_U <- brvaExp_U/(1+brvaExp_U)

brvaExp_L <- exp(summ_brva[1,3] + summ_brva[2,3] * brvarange + summ_brva[3,3] * pbinf + summ_brva[4,3] * pbjuv + summ_brva[5,3] *pbsa)  
brvaProb_L <- brvaExp_L/(1+brvaExp_L)
brva_df <- data.frame(brvaProb, brvaExp, brvaProb_U, brvaExp_U, brvaProb_L, brvaExp_L, brvarange)

ggplot(brva_df, aes(x = brvarange, y = brvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = brvaProb_L, ymax = brvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Rubber ingestion by birds w/age",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 3000)

#Calculate x for 95% mortality
birdrubbervage95 <- (log(19, base = exp(1)) - summ_brva[1,1])/summ_brva[2,1]

###Bird Rubber Volume + Species Size#####
bird_model_rvs <- brm(rubberdeath ~ rubber_volume + Size_avg, 
                     prior = priors, data = birds, 
                     family = bernoulli())

loo_bird_model_rvs <- loo(bird_model_rvs)  ##59.1 pareto K = 1386 good, 1 okay, 1 very bad

summ_brvs <- as.data.frame(summary(bird_model_rvs)$fixed)
brvsrange <- seq(0, 3000, by = 1)
brvsExp <- exp(summ_brvs[1,1] + summ_brvs[2,1] * brvsrange + summ_brvs[3,1] * mean(birds$Size_avg, na.rm=TRUE))
brvsProb <- brvsExp/(1+brvsExp)

brvsExp_U <- exp(summ_brvs[1,4] + summ_brvs[2,4] * brvsrange + summ_brvs[3,4] * mean(birds$Size_avg, na.rm=TRUE))
brvsProb_U <- brvsExp_U/(1+brvsExp_U)

brvsExp_L <- exp(summ_brvs[1,3] + summ_brvs[2,3] * brvsrange + summ_brvs[3,3] * mean(birds$Size_avg, na.rm=TRUE)) 
brvsProb_L <- brvsExp_L/(1+brvsExp_L)
brvs_df <- data.frame(brvsProb, brvsExp, brvsProb_U, brvsExp_U, brvsProb_L, brvsExp_L, brvsrange)

ggplot(brvs_df, aes(x = brvsrange, y = brvsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = brvsProb_L, ymax = brvsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Rubber ingestion by birds w/Species size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2500)

#Calculate x for 95% mortality
birdrubbervsize95 <- (log(19, base = exp(1)) - summ_brvs[1,1])/summ_brvs[2,1]

###Mammal Models####
###Mammal total pieces####
mammal_model_all <- brm(COD ~ total, prior = priors, data = mammals, 
                      control = list(adapt_delta = 0.9),  
                      family = bernoulli()) 

loo_mammal_model_all <- loo(mammal_model_all) #557.5 pareto k = 6721 good, 1 okay, 1 very bad

#M is for mammal pieces with no addition
summ_m <- as.data.frame(summary(mammal_model_all)$fixed)
mrange <- seq(0, 450, by = .001)
mExp <- exp(summ_m[1,1] + summ_m[2,1] * mrange) 
mProb <- mExp/(1+mExp)

mExp_U <- exp(summ_m[1,4] + summ_m[2,4] * mrange)
mProb_U <- mExp_U/(1+mExp_U)

mExp_L <- exp(summ_m[1,3] + summ_m[2,3] * mrange)
mProb_L <- mExp_L/(1+mExp_L)
m_df <- data.frame(mProb, mExp, mProb_U, mExp_U, mProb_L, mExp_L, mrange)

ggplot(m_df, aes(x = mrange, y = mProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mProb_L, ymax = mProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 250)

#Calculate x for 95% mortality
mammaltotal95 <- (log(19, base = exp(1)) - summ_m[1,1])/summ_m[2,1]
mammaltotal95_U <- (log(19, base = exp(1)) - summ_m[1,4])/summ_m[2,4]
mammaltotal95_L <- (log(19, base = exp(1)) - summ_m[1,3])/summ_m[2,3]
mammaltotal50 <- (log(1, base = exp(1)) - summ_m[1,1])/summ_m[2,1]
mammaltotal50_U <- (log(1, base = exp(1)) - summ_m[1,4])/summ_m[2,4]
mammaltotal50_L <- (log(1, base = exp(1)) - summ_m[1,3])/summ_m[2,3]

###Mammal total pieces + Age####
mammal_model_all_a <- brm(COD ~ total + Age, prior = priors, data = mammals, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli()) 

loo_mammal_model_all_a <- loo(mammal_model_all_a) #451.0 6221 pareto k were good, 1 was okay, 1 was bad and 3 were very bad. 

#MAA is mammals all pieces with age factor
summ_maa <- as.data.frame(summary(mammal_model_all_a)$fixed)
maarange <- seq(0, 45, by = .001)

maaExp <- exp(summ_maa[1,1] + summ_maa[2,1] * maarange + summ_maa[3,1] * pminf + summ_maa[4,1] * pmjuv + summ_maa[5,1] *pmsa)
maaProb <- maaExp/(1+maaExp)

maaExp_U <- exp(summ_maa[1,4] + summ_maa[2,4] * maarange + summ_maa[3,4] * pminf + summ_maa[4,4] * pmjuv + summ_maa[5,4] *pmsa) 
maaProb_U <- maaExp_U/(1+maaExp_U)

maaExp_L <- exp(summ_maa[1,3] + summ_maa[2,3] * maarange + summ_maa[3,3] * pminf + summ_maa[4,3] * pmjuv + summ_maa[5,3] *pmsa)  
maaProb_L <- maaExp_L/(1+maaExp_L)
maa_df <- data.frame(maaProb, maaExp, maaProb_U, maaExp_U, maaProb_L, maaExp_L, maarange)

ggplot(maa_df, aes(x = maarange, y = maaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = maaProb_L, ymax = maaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 45)

#Calculate x for 95% mortality
mammaltotalage95 <- (log(19, base = exp(1)) - summ_maa[1,1])/summ_maa[2,1]

###Mammal total pieces + Species_size####
mammal_model_all_ss <- brm(COD ~ total + Size_avg, prior = priors, data = mammals, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli()) 

loo_mammal_model_all_ss <- loo(mammal_model_all_ss) #550.9 pareto k = 6702, 1 is bad, 1 is very bad

#MASS is mammal all pieces with species size
summ_mass <- as.data.frame(summary(mammal_model_all_ss)$fixed)
massrange <- seq(0, 450, by = .001)
massExp <- exp(summ_mass[1,1] + summ_mass[2,1] * massrange + summ_mass[3,1] * mean(mammals$Size_avg, na.rm=TRUE))
massProb <- massExp/(1+massExp)

massExp_U <- exp(summ_mass[1,4] + summ_mass[2,4] * massrange+ summ_mass[3,4] * mean(mammals$Size_avg, na.rm=TRUE)) 
massProb_U <- massExp_U/(1+massExp_U)

massExp_L <- exp(summ_mass[1,3] + summ_mass[2,3] * massrange+ summ_mass[3,3] * mean(mammals$Size_avg, na.rm=TRUE))  
massProb_L <- massExp_L/(1+massExp_L)
mass_df <- data.frame(massProb, massExp, massProb_U, massExp_U, massProb_L, massExp_L, massrange)

ggplot(mass_df, aes(x = massrange, y = massProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = massProb_L, ymax = massProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 450)

#Calculate x for 95% mortality
mammaltotalspeciessize95 <- (log(19, base = exp(1)) - summ_mass[1,1])/summ_mass[2,1]

###Mammal total pieces + size####
mammal_model_all_sa <- brm(COD ~ total + Size_age, prior = priors, data = mammals, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_mammal_model_all_sa <- loo(mammal_model_all_sa) #546.7 pareto k 6609, 1 okay, 1 very bad

summ_mas <- as.data.frame(summary(mammal_model_all_sa)$fixed)
masrange <- seq(0, 450, by = .001)
masExp <- exp(summ_mas[1,1] + summ_mas[2,1] * masrange + summ_mas[3,1] * mean(mammals$Size_age, na.rm=TRUE))
masProb <- masExp/(1+masExp)

masExp_U <- exp(summ_mas[1,4] + summ_mas[2,4] * masrange+ summ_mas[3,4] * mean(mammals$Size_age, na.rm=TRUE)) 
masProb_U <- masExp_U/(1+masExp_U)

masExp_L <- exp(summ_mas[1,3] + summ_mas[2,3] * masrange+ summ_mas[3,3] * mean(mammals$Size_age, na.rm=TRUE))  
masProb_L <- masExp_L/(1+masExp_L)
mas_df <- data.frame(masProb, masExp, masProb_U, masExp_U, masProb_L, masExp_L, masrange)

ggplot(mas_df, aes(x = masrange, y = masProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = masProb_L, ymax = masProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 450)

#Calculate x for 95% mortality
mammaltotalsize95 <- (log(19, base = exp(1)) - summ_mas[1,1])/summ_mas[2,1]

###Mammal total volume####
## too small sample size
mammal_model_allv <- brm(COD ~ volume_estimate, prior = priors, data = mammals_noman, 
                       family = bernoulli())

loo_mammal_model_allv <- loo(mammal_model_allv) #162.6 pareto k = 1019 good, 2 okay, 3 bad, 2 very bad

summ_mva <- as.data.frame(summary(mammal_model_allv)$fixed)
mvarange <- seq(0, 7000, by = .01)
mvaExp <- exp(summ_mva[1,1] + summ_mva[2,1] * mvarange) 
mvaProb <- mvaExp/(1+mvaExp)

mvaExp_U <- exp(summ_mva[1,4] + summ_mva[2,4] * mvarange)
mvaProb_U <- mvaExp_U/(1+mvaExp_U)

mvaExp_L <- exp(summ_mva[1,3] + summ_mva[2,3] * mvarange)
mvaProb_L <- mvaExp_L/(1+mvaExp_L)
mva_df <- data.frame(mvaProb, mvaExp, mvaProb_U, mvaExp_U, mvaProb_L, mvaExp_L, mvarange)

ggplot(mva_df, aes(x = mvarange, y = mvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mvaProb_L, ymax = mvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Mammal total volume + age ####
mammal_model_allva <- brm(COD ~ volume_estimate + Age, prior = priors, data = mammals_noman, 
                        control = list(adapt_delta = 0.9),  
                        family = bernoulli())

loo_mammal_model_allva <- loo(mammal_model_allva)  #mostly bad

#MVA Mammals volume model with age
summ_mva <- as.data.frame(summary(mammal_model_allva)$fixed)
mvarange <- seq(0, 20000, by = .01)

mvaExp <- exp(summ_mva[1,1] + summ_mva[2,1] * mvarange + summ_mva[3,1] * pminf + summ_mva[4,1] * pmjuv + summ_mva[5,1] *pmsa)
mvaProb <- mvaExp/(1+mvaExp)

mvaExp_U <- exp(summ_mva[1,4] + summ_mva[2,4] * mvarange + summ_mva[3,4] * pminf + summ_mva[4,4] * pmjuv + summ_mva[5,4] *pmsa) 
mvaProb_U <- mvaExp_U/(1+mvaExp_U)

mvaExp_L <- exp(summ_mva[1,3] + summ_mva[2,3] * mvarange + summ_mva[3,3] * pminf + summ_mva[4,3] * pmjuv + summ_mva[5,3] *pmsa)  
mvaProb_L <- mvaExp_L/(1+mvaExp_L)
mva_df <- data.frame(mvaProb, mvaExp, mvaProb_U, mvaExp_U, mvaProb_L, mvaExp_L, mvarange)

ggplot(mva_df, aes(x = mvarange, y = mvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mvaProb_L, ymax = mvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/age",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20000)

###Mammal total volume + species size####
#sample size too low
mammal_model_all_vss <- brm(COD ~ volume_estimate + Size_avg, prior = priors, data = mammals_noman, 
                            control = list(adapt_delta = 0.9),  
                            family = bernoulli()) 

loo_mammal_model_allv_ss <- loo(mammal_model_allv_ss)  #164.7 pareto k = 1009 good, 2 okay, 3 bad, 2 very bad

summ_mvs <- as.data.frame(summary(mammal_model_allv_ss)$fixed)
mvsrange <- seq(0, 8000, by = .01)
mvsExp <- exp(summ_mvs[1,1] + summ_mvs[2,1] * mvsrange + summ_mvs[3,1] * mean(mammals$Size_avg, na.rm=TRUE))
mvsProb <- mvsExp/(1+mvsExp)

mvsExp_U <- exp(summ_mvs[1,4] + summ_mvs[2,4] * mvsrange+ summ_mvs[3,4] * mean(mammals$Size_avg, na.rm=TRUE)) 
mvsProb_U <- mvsExp_U/(1+mvsExp_U)

mvsExp_L <- exp(summ_mvs[1,3] + summ_mvs[2,3] * mvsrange+ summ_mvs[3,3] * mean(mammals$Size_avg, na.rm=TRUE))  
mvsProb_L <- mvsExp_L/(1+mvsExp_L)
mvs_df <- data.frame(mvsProb, mvsExp, mvsProb_U, mvsExp_U, mvsProb_L, mvsExp_L, mvsrange)

ggplot(mvs_df, aes(x = mvsrange, y = mvsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mvsProb_L, ymax = mvsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/ species size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Mammal total volume + size####
#sample size too low
mammal_model_all_vsa <- brm(COD ~ volume_estimate + Size_age, prior = priors, data = mammals_noman, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_mammal_model_all_vsa <- loo(mammal_model_all_vsa) #10.5 all pareto k <0.5

summ_mvsa <- as.data.frame(summary(mammal_model_all_vsa)$fixed)
mvsarange <- seq(0, 5000, by = .01)
mvsaExp <- exp(summ_mvsa[1,1] + summ_mvsa[2,1] * mvsarange + summ_mvsa[3,1] * mean(mammals$Size_age, na.rm=TRUE))
mvsaProb <- msExp/(1+msExp)

mvsaExp_U <- exp(summ_mvsa[1,4] + summ_mvsa[2,4] * mvsarange+ summ_mvsa[3,4] * mean(mammals$Size_age, na.rm=TRUE)) 
mvsaProb_U <- mvsaExp_U/(1+mvsaExp_U)

mvsaExp_L <- exp(summ_mvsa[1,3] + summ_mvsa[2,3] * mvsarange+ summ_mvsa[3,3] * mean(mammals$Size_age, na.rm=TRUE))  
mvsaProb_L <- mvsaExp_L/(1+mvsaExp_L)
mvsa_df <- data.frame(mvsaProb, mvsaExp, mvsaProb_U, mvsaExp_U, mvsaProb_L, mvsaExp_L, mvsarange)

ggplot(mvsa_df, aes(x = mvsarange, y = mvsaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mvsaProb_L, ymax = mvsaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 5000)

###Mammal Soft Pieces####
mammal_model_soft <- brm(softdeath ~ soft, prior = priors,
                         data = mammals, 
                         family = bernoulli(logit))

loo_mammal_model_soft <- loo(mammal_model_soft) #79.3 pareto k = 6720 good, 2 bad, 2 very bad

#MS is mammal soft model with no additions
summ_ms <- as.data.frame(summary(mammal_model_soft)$fixed)
msrange <- seq(0, 20, by = .0001)
msExp <- exp(summ_ms[1,1] + summ_ms[2,1] * msrange)
msProb <- msExp/(1+msExp)

msExp_U <- exp(summ_ms[1,4] + summ_ms[2,4] * msrange) 
msProb_U <- msExp_U/(1+msExp_U)

msExp_L <- exp(summ_ms[1,3] + summ_ms[2,3] * msrange)
msProb_L <- msExp_L/(1+msExp_L)
ms_df <- data.frame(msProb, msExp, msProb_U, msExp_U, msProb_L, msExp_L, msrange)

ggplot(ms_df, aes(x = msrange, y = msProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = msProb_L, ymax = msProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by mammals",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20)

#Calculate x for 95% mortality
mammalsoft95 <- (log(19, base = exp(1)) - summ_ms[1,1])/summ_ms[2,1]
mammalsoft95_U <- (log(19, base = exp(1)) - summ_ms[1,4])/summ_ms[2,4]
mammalsoft95_L <- (log(19, base = exp(1)) - summ_ms[1,3])/summ_ms[2,3]
mammalsoft50 <- (log(1, base = exp(1)) - summ_ms[1,1])/summ_ms[2,1]
mammalsoft50_U <- (log(1, base = exp(1)) - summ_ms[1,4])/summ_ms[2,4]
mammalsoft50_L <- (log(1, base = exp(1)) - summ_ms[1,3])/summ_ms[2,3]

###Mammal soft pieces + Age####
mammal_model_soft_a <- brm(COD ~ soft + Age, prior = priors, data = mammals, 
                          control = list(adapt_delta = 0.9),  
                          family = bernoulli()) 

loo_mammal_model_soft_a <- loo(mammal_model_soft_a) #472.9 6220 pareto k were good, 1 was okay and 4 were very bad. 

#MSA mammal soft pieces model with age
summ_msa <- as.data.frame(summary(mammal_model_soft_a)$fixed)
msarange <- seq(0, 45, by = .001)

msaExp <- exp(summ_msa[1,1] + summ_msa[2,1] * msarange + summ_msa[3,1] * pminf + summ_msa[4,1] * pmjuv + summ_msa[5,1] *pmsa)
msaProb <- msaExp/(1+msaExp)

msaExp_U <- exp(summ_msa[1,4] + summ_msa[2,4] * msarange + summ_msa[3,4] * pminf + summ_msa[4,4] * pmjuv + summ_msa[5,4] *pmsa) 
msaProb_U <- msaExp_U/(1+msaExp_U)

msaExp_L <- exp(summ_msa[1,3] + summ_msa[2,3] * msarange + summ_msa[3,3] * pminf + summ_msa[4,3] * pmjuv + summ_msa[5,3] *pmsa)  
msaProb_L <- msaExp_L/(1+msaExp_L)
msa_df <- data.frame(msaProb, msaExp, msaProb_U, msaExp_U, msaProb_L, msaExp_L, msarange)

ggplot(msa_df, aes(x = msarange, y = msaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = msaProb_L, ymax = msaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by mammals w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 25)

#Calculate x for 95% mortality
mammalsoftage95 <- (log(19, base = exp(1)) - summ_msa[1,1])/summ_msa[2,1]

###Mammal soft pieces + species size####
#554 transitions after warmup exceeded maximum treedepth
mammal_model_soft_ss <- brm(COD ~ soft + Size_avg, prior = priors, data = mammals, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_mammal_model_soft_ss <- loo(mammal_model_soft_ss) #483.8 6698 pareto k were good, 3 were okay, 2 were very bad. 

#MSSS mammals soft pieces model with species size
summ_msss <- as.data.frame(summary(mammal_model_soft_ss)$fixed)
msssrange <- seq(0, 450, by = .001)
msssExp <- exp(summ_msss[1,1] + summ_msss[2,1] * msssrange + summ_msss[3,1] * mean(mammals$Size_avg, na.rm=TRUE))
msssProb <- msssExp/(1+msssExp)

msssExp_U <- exp(summ_msss[1,4] + summ_msss[2,4] * msssrange+ summ_msss[3,4] * mean(mammals$Size_avg, na.rm=TRUE)) 
msssProb_U <- msssExp_U/(1+msssExp_U)

msssExp_L <- exp(summ_msss[1,3] + summ_msss[2,3] * msssrange+ summ_msss[3,3] * mean(mammals$Size_avg, na.rm=TRUE))  
msssProb_L <- msssExp_L/(1+msssExp_L)
msss_df <- data.frame(msssProb, msssExp, msssProb_U, msssExp_U, msssProb_L, msssExp_L, msssrange)

ggplot(msss_df, aes(x = msssrange, y = msssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = msssProb_L, ymax = msssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by mammals w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 25)

#Calculate x for 95% mortality
mammalsoftspeciessize95 <- (log(19, base = exp(1)) - summ_msss[1,1])/summ_msss[2,1]

###Mammal soft pieces + size####
mammal_model_soft_s <- brm(COD ~ soft + Size_age, prior = priors, data = mammals, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_mammal_model_soft_s <- loo(mammal_model_soft_s) #478.3 6687 pareto k were good, 3 were okay and 2 were very bad. 

summ_mssa <- as.data.frame(summary(mammal_model_soft_s)$fixed)
mssarange <- seq(0, 450, by = .001)
mssaExp <- exp(summ_mssa[1,1] + summ_mssa[2,1] * mssarange + summ_mssa[3,1] * mean(mammals$Size_age, na.rm=TRUE))
mssaProb <- mssaExp/(1+mssaExp)

mssaExp_U <- exp(summ_mssa[1,4] + summ_mssa[2,4] * mssarange+ summ_mssa[3,4] * mean(mammals$Size_age, na.rm=TRUE)) 
mssaProb_U <- mssaExp_U/(1+mssaExp_U)

mssaExp_L <- exp(summ_mssa[1,3] + summ_mssa[2,3] * mssarange+ summ_mssa[3,3] * mean(mammals$Size_age, na.rm=TRUE))  
mssaProb_L <- mssaExp_L/(1+mssaExp_L)
mssa_df <- data.frame(mssaProb, mssaExp, mssaProb_U, mssaExp_U, mssaProb_L, mssaExp_L, mssarange)

ggplot(mssa_df, aes(x = mssarange, y = mssaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mssaProb_L, ymax = mssaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by mammals w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 25)

#Calculate x for 95% mortality
mammalsoftsize95 <- (log(19, base = exp(1)) - summ_mssa[1,1])/summ_mssa[2,1]

###Mammal Soft Volume####
mammal_model_softv <- brm(softdeath ~ soft_volume, prior = priors,
                          data = mammals_noman, 
                          family = bernoulli())

loo_mammal_model_softv <- loo(mammal_model_softv) #44.5 1011 pareto k were good, 3 were okay, 1 was bad, and 4 were very bad. 

#MSV is mammal soft volume with no additional factors
summ_msv <- as.data.frame(summary(mammal_model_softv)$fixed)
msvrange <- seq(0, 2500, by = .01)
msvExp <- exp(summ_msv[1,1] + summ_msv[2,1] * msvrange)
msvProb <- msvExp/(1+msvExp)

msvExp_U <- exp(summ_msv[1,4] + summ_msv[2,4] * msvrange)
msvProb_U <- msvExp_U/(1+msvExp_U)

msvExp_L <- exp(summ_msv[1,3] + summ_msv[2,3] * msvrange)
msvProb_L <- msvExp_L/(1+msvExp_L)
msv_df <- data.frame(msvProb, msvExp, msvProb_U, msvExp_U, msvProb_L, msvExp_L, msvrange)

ggplot(msv_df, aes(x = msvrange, y = msvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = msvProb_L, ymax = msvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by mammals",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2500)

#Calculate x for 95% mortality
mammalsoftv95 <- (log(19, base = exp(1)) - summ_msv[1,1])/summ_msv[2,1]
mammalsoftv95_U <- (log(19, base = exp(1)) - summ_msv[1,4])/summ_msv[2,4]
mammalsoftv95_L <- (log(19, base = exp(1)) - summ_msv[1,3])/summ_msv[2,3]
mammalsoftv50 <- (log(1, base = exp(1)) - summ_msv[1,1])/summ_msv[2,1]
mammalsoftv50_U <- (log(1, base = exp(1)) - summ_msv[1,4])/summ_msv[2,4]
mammalsoftv50_L <- (log(1, base = exp(1)) - summ_msv[1,3])/summ_msv[2,3]

###Mammal Soft Volume + Age####
#sample size too small
mammal_model_softva <- brm(softdeath ~ soft_volume + Age, prior = priors,
                          data = mammals_noman, 
                          family = bernoulli())

###Mammal Soft Volume + Species Size####
#sample size too small
mammal_model_softvss <- brm(softdeath ~ soft_volume + Size_avg, prior = priors,
                           data = mammals_noman, 
                           family = bernoulli())

###Mammal Soft Volume + Size####
mammal_model_softvsa <- brm(softdeath ~ soft_volume + Size_age, prior = priors,
                           data = mammals_noman, 
                           family = bernoulli())

loo_mammal_model_softvsa <- loo(mammal_model_softvsa) #55 945 pareto k were good, 36 were okay and 6 were very bad. 

#MVSSA is for mammals soft volume of plastic by individual Combines size and age
summ_mvssa <- as.data.frame(summary(mammal_model_softvsa)$fixed)
mvssarange <- seq(0, 3000, by = .01)
mvssaExp <- exp(summ_mvssa[1,1] + summ_mvssa[2,1] * mvssarange + summ_mvssa[3,1] * mean(mammals$Size_age, na.rm=TRUE))
mvssaProb <- mvssaExp/(1+mvssaExp)

mvssaExp_U <- exp(summ_mvssa[1,4] + summ_mvssa[2,4] * mvssarange+ summ_mvssa[3,4] * mean(mammals$Size_age, na.rm=TRUE)) 
mvssaProb_U <- mvssaExp_U/(1+mvssaExp_U)

mvssaExp_L <- exp(summ_mvssa[1,3] + summ_mvssa[2,3] * mvssarange+ summ_mvssa[3,3] * mean(mammals$Size_age, na.rm=TRUE))  
mvssaProb_L <- mvssaExp_L/(1+mvssaExp_L)
mvssa_df <- data.frame(mvssaProb, mvssaExp, mvssaProb_U, mvssaExp_U, mvssaProb_L, mvssaExp_L, mvssarange)

ggplot(mvssa_df, aes(x = mvssarange, y = mvssaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mvssaProb_L, ymax = mvssaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft Plastic ingestion by mammals w/ size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)

###Mammal Thread Pieces#####
mammal_model_thread <- brm(threaddeath ~ thread, prior = priors,
                           data = mammals, 
                           control = list(adapt_delta = 0.9),
                           family = bernoulli(logit))

loo_mammal_model_thread <- loo(mammal_model_thread) #392.7 pareto k = 6704 good, 1 very bad

#MT is mammal thread pieces with no additional factors
summ_mt <- as.data.frame(summary(mammal_model_thread)$fixed)
mtrange <- seq(0, 1200, by = .001)
mtExp <- exp(summ_mt[1,1] + summ_mt[2,1] * mtrange)
mtProb <- mtExp/(1+mtExp)

mtExp_U <- exp(summ_mt[1,4] + summ_mt[2,4] * mtrange) 
mtProb_U <- mtExp_U/(1+mtExp_U)

mtExp_L <- exp(summ_mt[1,3] + summ_mt[2,3] * mtrange)
mtProb_L <- mtExp_L/(1+mtExp_L)
mt_df <- data.frame(mtProb, mtExp, mtProb_U, mtExp_U, mtProb_L, mtExp_L, mtrange)

ggplot(mt_df, aes(x = mtrange, y = mtProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtProb_L, ymax = mtProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1200)

#Calculate x for 95% mortality
mammalthread95 <- (log(19, base = exp(1)) - summ_mt[1,1])/summ_mt[2,1]
mammalthread95_U <- (log(19, base = exp(1)) - summ_mt[1,4])/summ_mt[2,4]
mammalthread95_L <- (log(19, base = exp(1)) - summ_mt[1,3])/summ_mt[2,3]
mammalthread50 <- (log(1, base = exp(1)) - summ_mt[1,1])/summ_mt[2,1]
mammalthread50_U <- (log(1, base = exp(1)) - summ_mt[1,4])/summ_mt[2,4]
mammalthread50_L <- (log(1, base = exp(1)) - summ_mt[1,3])/summ_mt[2,3]

###Mammal Thread Pieces + Age#####
mammal_model_threada <- brm(threaddeath ~ thread + Age, 
                            data = mammals, 
                            control = list(adapt_delta = 0.9),
                            family = bernoulli(logit))

loo_mammal_model_threada <- loo(mammal_model_threada) #357.4 pareto k = 6203 good, 3 okay, 2 bad, 1 very bad

#MTA is mammal thread model with Age
summ_mta <- as.data.frame(summary(mammal_model_threada)$fixed)
mtarange <- seq(0, 80, by = .01)
mtaExp <- exp(summ_mta[1,1] + summ_mta[2,1] * mtarange + summ_mta[3,1] * pminf + summ_mta[4,1] * pmjuv + summ_mta[5,1] * pmsa)
mtaProb <- mtaExp/(1+mtaExp)

mtaExp_U <- exp(summ_mta[1,4] + summ_mta[2,4] * mtarange + summ_mta[3,4] * pminf + summ_mta[4,4] * pmjuv + summ_mta[5,4] * pmsa)
mtaProb_U <- mtaExp_U/(1+mtaExp_U)

mtaExp_L <- exp(summ_mta[1,3] + summ_mta[2,3] * mtarange + summ_mta[3,3] * pminf + summ_mta[4,3] * pmjuv + summ_mta[5,3] * pmsa)
mtaProb_L <- mtaExp_L/(1+mtaExp_L)
mta_df <- data.frame(mtaProb, mtaExp, mtaProb_U, mtaExp_U, mtaProb_L, mtaExp_L, mtarange)

ggplot(mta_df, aes(x = mtarange, y = mtaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtaProb_L, ymax = mtaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals w/age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 80)

#Calculate x for 95% mortality
mammalthreadage95 <- (log(19, base = exp(1)) - summ_mta[1,1])/summ_mta[2,1]

###Mammal Thread Pieces + Species size#####
#211 exceed tree depth
mammal_model_threadss <- brm(threaddeath ~ thread + Size_avg, 
                            data = mammals, 
                            control = list(adapt_delta = 0.9),
                            family = bernoulli(logit))

loo_mammal_model_threadss <- loo(mammal_model_threadss) #385.7 pareto k = 66825 good, 2 okay, 1 very bad

#MTSS for mammal thread model with species size
summ_mtss <- as.data.frame(summary(mammal_model_threadss)$fixed)
mtssrange <- seq(0, 1000, by = .01)
mtssExp <- exp(summ_mtss[1,1] + summ_mtss[2,1] * mtssrange + summ_mtss[3,1] * mean(mammals$Size_avg, na.rm=TRUE))
mtssProb <- mtssExp/(1+mtssExp)

mtssExp_U <- exp(summ_mtss[1,4] + summ_mtss[2,4] * mtssrange+ summ_mtss[3,4] * mean(mammals$Size_avg, na.rm=TRUE)) 
mtssProb_U <- mtssExp_U/(1+mtssExp_U)

mtssExp_L <- exp(summ_mtss[1,3] + summ_mtss[2,3] * mtssrange+ summ_mtss[3,3] * mean(mammals$Size_avg, na.rm=TRUE))  
mtssProb_L <- mtssExp_L/(1+mtssExp_L)
mtss_df <- data.frame(mtssProb, mtssExp, mtssProb_U, mtssExp_U, mtssProb_L, mtssExp_L, mtssrange)

ggplot(mtss_df, aes(x = mtssrange, y = mtssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtssProb_L, ymax = mtssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 500)

#Calculate x for 95% mortality
mammalthreadspeciessize95 <- (log(19, base = exp(1)) - summ_mtss[1,1])/summ_mtss[2,1]

###Mammal Thread Pieces + Size#####
mammal_model_threadsa <- brm(threaddeath ~ thread + Size_age, 
                            data = mammals, 
                            control = list(adapt_delta = 0.9),
                            family = bernoulli(logit))

loo_mammal_model_threadsa <- loo(mammal_model_threadsa) #380.7 pareto k = 6672 good, 1 bad, 1 very bad

#MTSA is mammal thread pieces with Size_age combination (individual size)
summ_mtsa <- as.data.frame(summary(mammal_model_threadsa)$fixed)
mtsarange <- seq(0, 1000, by = .01)
mtsaExp <- exp(summ_mtsa[1,1] + summ_mtsa[2,1] * mtsarange + summ_mtsa[3,1] * mean(mammals$Size_age, na.rm=TRUE))
mtsaProb <- mtsaExp/(1+mtsaExp)

mtsaExp_U <- exp(summ_mtsa[1,4] + summ_mtsa[2,4] * mtsarange+ summ_mtsa[3,4] * mean(mammals$Size_age, na.rm=TRUE)) 
mtsaProb_U <- mtsaExp_U/(1+mtsaExp_U)

mtsaExp_L <- exp(summ_mtsa[1,3] + summ_mtsa[2,3] * mtsarange+ summ_mtsa[3,3] * mean(mammals$Size_age, na.rm=TRUE))  
mtsaProb_L <- mtsaExp_L/(1+mtsaExp_L)
mtsa_df <- data.frame(mtsaProb, mtsaExp, mtsaProb_U, mtsaExp_U, mtsaProb_L, mtsaExp_L, mtsarange)

ggplot(mtsa_df, aes(x = mtsarange, y = mtsaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtsaProb_L, ymax = mtsaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

#Calculate x for 95% mortality
mammalthreadsize95 <- (log(19, base = exp(1)) - summ_mtsa[1,1])/summ_mtsa[2,1]

###Mammal Thread Length#########
mammal_model_threadv <- brm(threaddeath ~ line_length, 
                            data = mammals_noman, 
                            control = list(adapt_delta = 0.9),
                            family = bernoulli(logit))

loo_mammal_model_threadv <- loo(mammal_model_threadv) #46.2 pareto k = 1006 good, 1 okay, 1 bad

#MTV is mammal thread length model with no additional factors
summ_mtv <- as.data.frame(summary(mammal_model_threadv)$fixed)
mtvrange <- seq(0, 25000, by = .1)
mtvExp <- exp(summ_mtv[1,1] + summ_mtv[2,1] * mtvrange)
mtvProb <- mtvExp/(1+mtvExp)

mtvExp_U <- exp(summ_mtv[1,4] + summ_mtv[2,4] * mtvrange)
mtvProb_U <- mtvExp_U/(1+mtvExp_U)

mtvExp_L <- exp(summ_mtv[1,3] + summ_mtv[2,3] * mtvrange)
mtvProb_L <- mtvExp_L/(1+mtvExp_L)
mtv_df <- data.frame(mtvProb, mtvExp, mtvProb_U, mtvExp_U, mtvProb_L, mtvExp_L, mtvrange)

ggplot(mtv_df, aes(x = mtvrange, y = mtvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtvProb_L, ymax = mtvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Fishing debris in mammals",
       x = "Length (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 22000)

###Mammal Thread Length + Age#####
mammal_model_threadva <- brm(threaddeath ~ line_length + Age, prior = priors,
                           data = mammals_noman, 
                           family = bernoulli())

loo_mammal_model_threadva <- loo(mammal_model_threadva) #24.6 pareto k = 508 good, 1 okay, 2 bad, 1 very bad

#MTLA is mammal thread model with Age
summ_mtla <- as.data.frame(summary(mammal_model_threadva)$fixed)
mtlarange <- seq(0, 8000, by = .01)
mtlaExp <- exp(summ_mtla[1,1] + summ_mtla[2,1] * mtlarange + summ_mtla[3,1] * pminf + summ_mtla[4,1] * pmjuv + summ_mtla[5,1] * pmsa)
mtlaProb <- mtlaExp/(1+mtlaExp)

mtlaExp_U <- exp(summ_mtla[1,4] + summ_mtla[2,4] * mtlarange + summ_mtla[3,4] * pminf + summ_mtla[4,4] * pmjuv + summ_mtla[5,4] * pmsa)
mtlaProb_U <- mtlaExp_U/(1+mtlaExp_U)

mtlaExp_L <- exp(summ_mtla[1,3] + summ_mtla[2,3] * mtlarange + summ_mtla[3,3] * pminf + summ_mtla[4,3] * pmjuv + summ_mtla[5,3] * pmsa)
mtlaProb_L <- mtlaExp_L/(1+mtlaExp_L)
mtla_df <- data.frame(mtlaProb, mtlaExp, mtlaProb_U, mtlaExp_U, mtlaProb_L, mtlaExp_L, mtlarange)

ggplot(mtla_df, aes(x = mtlarange, y = mtlaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtlaProb_L, ymax = mtlaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals w/age",
       x = "Length (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 3000)

#Calculate x for 95% mortality
mammalthreadlengthage95 <- (log(19, base = exp(1)) - (summ_mtla[1,1] + summ_mtla[3,1] * pminf + summ_mtla[4,1] * pmjuv + summ_mtla[5,1] * pmsa))/summ_mtla[2,1]
mammalthreadlengthage95_U <- (log(19, base = exp(1)) - (summ_mtla[1,4] + summ_mtla[3,4] * pminf + summ_mtla[4,4] * pmjuv + summ_mtla[5,4] * pmsa))/summ_mtla[2,4]
mammalthreadlengthage95_L <- (log(19, base = exp(1)) - (summ_mtla[1,1] + summ_mtla[3,3] * pminf + summ_mtla[4,3] * pmjuv + summ_mtla[5,3] * pmsa))/summ_mtla[2,1]
mammalthreadlengthage50 <- (log(1, base = exp(1)) - (summ_mtla[1,1] + summ_mtla[3,1] * pminf + summ_mtla[4,1] * pmjuv + summ_mtla[5,1] * pmsa))/summ_mtla[2,1]
mammalthreadlengthage50_U <- (log(1, base = exp(1)) - (summ_mtla[1,4] + summ_mtla[3,4] * pminf + summ_mtla[4,4] * pmjuv + summ_mtla[5,4] * pmsa))/summ_mtla[2,4]
mammalthreadlengthage5_L <- (log(1, base = exp(1)) - (summ_mtla[1,1] + summ_mtla[3,3] * pminf + summ_mtla[4,3] * pmjuv + summ_mtla[5,3] * pmsa))/summ_mtla[2,1]

###Mammal Thread Length + Species Size####
mammal_model_threadvss <- brm(threaddeath ~ line_length + Size_avg, 
                             data = mammals_noman, 
                             control = list(adapt_delta = 0.9),
                             family = bernoulli(logit))

loo_mammal_model_threadvss <- loo(mammal_model_threadvss) #52.3 pareto k 986 good, 1 okay, 1 bad, 2 very bad

#MTVSS for mammal thread volume model with species size
summ_mtvss <- as.data.frame(summary(mammal_model_threadss)$fixed)
mtvssrange <- seq(0, 2000, by = .01)
mtvssExp <- exp(summ_mtvss[1,1] + summ_mtvss[2,1] * mtvssrange + summ_mtvss[3,1] * mean(mammals$Size_avg, na.rm=TRUE))
mtvssProb <- mtvssExp/(1+mtvssExp)

mtvssExp_U <- exp(summ_mtvss[1,4] + summ_mtvss[2,4] * mtvssrange+ summ_mtvss[3,4] * mean(mammals$Size_avg, na.rm=TRUE)) 
mtvssProb_U <- mtvssExp_U/(1+mtvssExp_U)

mtvssExp_L <- exp(summ_mtvss[1,3] + summ_mtvss[2,3] * mtvssrange+ summ_mtvss[3,3] * mean(mammals$Size_avg, na.rm=TRUE))  
mtvssProb_L <- mtvssExp_L/(1+mtvssExp_L)
mtvss_df <- data.frame(mtvssProb, mtvssExp, mtvssProb_U, mtvssExp_U, mtvssProb_L, mtvssExp_L, mtvssrange)

ggplot(mtvss_df, aes(x = mtvssrange, y = mtvssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtvssProb_L, ymax = mtvssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals w/ species size",
       x = "Length (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)

###Mammal Thread Length + Size####
mammal_model_threadvsa <- brm(threaddeath ~ line_length + Size_age, 
                              data = mammals_noman, 
                              control = list(adapt_delta = 0.9),
                              family = bernoulli(logit))

loo_mammal_model_threadvsa <- loo(mammal_model_threadvsa) #48.1 975 good, 1 bad, 1 very bad

#MTLSA is mammal thread length with Size_age combination (individual size)
summ_mtlsa <- as.data.frame(summary(mammal_model_threadsa)$fixed)
mtlsarange <- seq(0, 10000, by = .1)
mtlsaExp <- exp(summ_mtlsa[1,1] + summ_mtlsa[2,1] * mtlsarange + summ_mtlsa[3,1] * mean(mammals$Size_age, na.rm=TRUE))
mtlsaProb <- mtlsaExp/(1+mtlsaExp)

mtlsaExp_U <- exp(summ_mtlsa[1,4] + summ_mtlsa[2,4] * mtlsarange+ summ_mtlsa[3,4] * mean(mammals$Size_age, na.rm=TRUE)) 
mtlsaProb_U <- mtlsaExp_U/(1+mtlsaExp_U)

mtlsaExp_L <- exp(summ_mtlsa[1,3] + summ_mtlsa[2,3] * mtlsarange+ summ_mtlsa[3,3] * mean(mammals$Size_age, na.rm=TRUE))  
mtlsaProb_L <- mtlsaExp_L/(1+mtlsaExp_L)
mtlsa_df <- data.frame(mtlsaProb, mtlsaExp, mtlsaProb_U, mtlsaExp_U, mtlsaProb_L, mtlsaExp_L, mtlsarange)

ggplot(mtlsa_df, aes(x = mtlsarange, y = mtlsaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = mtlsaProb_L, ymax = mtlsaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by mammals w/ size",
       x = "Length (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2500)

###Turtle Models####
###Turtle all##### 
turtle_model_all <- brm(COD ~ total, prior = priors, data = turtles, 
                              control = list(adapt_delta = 0.9),  
                              family = bernoulli()) 

loo_turtle_model_all <- loo(turtle_model_all) #429.4 pareto k < 0.5

#TA stands for Turtle All
summ_ta <- as.data.frame(summary(turtle_model_all)$fixed)
tarange <- seq(0, 1000, by = .01)
taExp <- exp(summ_ta[1,1] + summ_ta[2,1] * tarange)
taProb <- taExp/(1+taExp)

taExp_U <- exp(summ_ta[1,4] + summ_ta[2,4] * tarange) 
taProb_U <- taExp_U/(1+taExp_U)

taExp_L <- exp(summ_ta[1,3] + summ_ta[2,3] * tarange)
taProb_L <- taExp_L/(1+taExp_L)
ta_df <- data.frame(taProb, taExp, taProb_U, taExp_U, taProb_L, taExp_L, tarange)

ggplot(ta_df, aes(x = tarange, y = taProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = taProb_L, ymax = taProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle young Total pieces ####
turtle_young_model_all <- brm(COD ~ total, prior = priors, data = turtles_young, 
                      control = list(adapt_delta = 0.9),  
                      family = bernoulli()) 

loo_turtle_young_model_all <- loo(turtle_young_model_all) #156.2 pareto k < 0.7

#TYA stands for Turtle Young All
summ_tya <- as.data.frame(summary(turtle_young_model_all)$fixed)
tyarange <- seq(0, 2000, by = .01)
tyaExp <- exp(summ_tya[1,1] + summ_tya[2,1] * tyarange)
tyaProb <- tyaExp/(1+tyaExp)

tyaExp_U <- exp(summ_tya[1,4] + summ_tya[2,4] * tyarange) 
tyaProb_U <- tyaExp_U/(1+tyaExp_U)

tyaExp_L <- exp(summ_tya[1,3] + summ_tya[2,3] * tyarange)
tyaProb_L <- tyaExp_L/(1+tyaExp_L)
tya_df <- data.frame(tyaProb, tyaExp, tyaProb_U, tyaExp_U, tyaProb_L, tyaExp_L, tyarange)

ggplot(tya_df, aes(x = tyarange, y = tyaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tyaProb_L, ymax = tyaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Total pieces + Age####
#Too small sample size
#TAA for Turtle All Age
turtle_model_all_taa <- brm(COD ~ total + Age, prior = priors, data = turtles, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli()) 

loo_turtle_model_all_taa <- loo(turtle_model_all_taa) #160.8 758 pareto k were good and 1  bad. 

summ_taa <- as.data.frame(summary(turtle_model_all_taa)$fixed)
taarange <- seq(0, 2000, by = .01)

taaExp <- exp(summ_taa[1,1] + summ_taa[2,1] * taarange + summ_taa[3,1] * ptinf + summ_taa[4,1] * ptjuv + summ_taa[5,1] *ptsa)
taaProb <- taaExp/(1+taaExp)

taaExp_U <- exp(summ_taa[1,4] + summ_taa[2,4] * taarange + summ_taa[3,4] * ptinf + summ_taa[4,4] * ptjuv + summ_taa[5,4] *ptsa) 
taaProb_U <- taaExp_U/(1+taaExp_U)

taaExp_L <- exp(summ_taa[1,3] + summ_taa[2,3] * taarange + summ_taa[3,3] * ptinf + summ_taa[4,3] * ptjuv + summ_taa[5,3] *ptsa)  
taaProb_L <- taaExp_L/(1+taaExp_L)
taa_df <- data.frame(taaProb, taaExp, taaProb_U, taaExp_U, taaProb_L, taaExp_L, taarange)

ggplot(taa_df, aes(x = taarange, y = taaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = taaProb_L, ymax = taaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)

###Turtle Total pieces + Species_size####
turtle_model_all_ss <- brm(COD ~ total + Size_avg, prior = priors, data = turtles, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli()) 

loo_turtle_model_all_ss <- loo(turtle_model_all_ss) #414.3 all pareto k <0.5

#TASS is Turtle All Species Size
summ_tass <- as.data.frame(summary(turtle_model_all_ss)$fixed)
tassrange <- seq(0, 1000, by = .01)
tassExp <- exp(summ_tass[1,1] + summ_tass[2,1] * tassrange + summ_tass[3,1] * mean(turtles$Size_avg, na.rm=TRUE))
tassProb <- tassExp/(1+tassExp)

tassExp_U <- exp(summ_tass[1,4] + summ_tass[2,4] * tassrange+ summ_tass[3,4] * mean(turtles$Size_avg, na.rm=TRUE)) 
tassProb_U <- tassExp_U/(1+tassExp_U)

tassExp_L <- exp(summ_tass[1,3] + summ_tass[2,3] * tassrange+ summ_tass[3,3] * mean(turtles$Size_avg, na.rm=TRUE))  
tassProb_L <- tassExp_L/(1+tassExp_L)
tass_df <- data.frame(tassProb, tassExp, tassProb_U, tassExp_U, tassProb_L, tassExp_L, tassrange)

ggplot(tass_df, aes(x = tassrange, y = tassProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tassProb_L, ymax = tassProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Total Pieces + Size####
turtle_model_all_sa <- brm(COD ~ total + Size_age, prior = priors, data = turtles, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_turtle_model_all_sa <- loo(turtle_model_all_sa) #403.8 pareto k <0.7

#TAS is for turtle all pieces model with size
summ_tas <- as.data.frame(summary(turtle_model_all_sa)$fixed)
tasrange <- seq(0, 1000, by = .01)
tasExp <- exp(summ_tas[1,1] + summ_tas[2,1] * tasrange + summ_tas[3,1] * mean(turtles$Size_age, na.rm=TRUE))
tasProb <- tasExp/(1+tasExp)

tasExp_U <- exp(summ_tas[1,4] + summ_tas[2,4] * tasrange+ summ_tas[3,4] * mean(turtles$Size_age, na.rm=TRUE)) 
tasProb_U <- tasExp_U/(1+tasExp_U)

tasExp_L <- exp(summ_tas[1,3] + summ_tas[2,3] * tasrange+ summ_tas[3,3] * mean(turtles$Size_age, na.rm=TRUE))  
tasProb_L <- tasExp_L/(1+tasExp_L)
tas_df <- data.frame(tasProb, tasExp, tasProb_U, tasExp_U, tasProb_L, tasExp_L, tasrange)

ggplot(tas_df, aes(x = tasrange, y = tasProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tasProb_L, ymax = tasProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

#Calculate x for 95% mortality
turtlesize95 <- (log(19, base = exp(1)) - (summ_tas[1,1] + summ_tas[3,1] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tas[2,1]
turtlesize95_U <- (log(19, base = exp(1)) - (summ_tas[1,4] + summ_tas[3,4] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tas[2,4]
turtlesize95_L <- (log(19, base = exp(1)) - (summ_tas[1,3] + summ_tas[3,3] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tas[2,3]
turtlesize50 <- (log(1, base = exp(1)) - (summ_tas[1,1] + summ_tas[3,1] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tas[2,1]
turtlesize50_U <- (log(1, base = exp(1)) - (summ_tas[1,4] + summ_tas[3,4] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tas[2,4]
turtlesize50_L <- (log(1, base = exp(1)) - (summ_tas[1,3] + summ_tas[3,3] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tas[2,3]


###Turtle Total Volume####
turtle_model_allv <- brm(COD ~ volume_estimate, prior = priors, data = turtles, 
                       control = list(adapt_delta = 0.9),  
                       family = bernoulli())

loo_turtle_model_allv <- loo(turtle_model_allv) #324.7 pareto k = 867 good, 1 okay, 1 very bad

#TAV is for turtle all volume
summ_tav <- as.data.frame(summary(turtle_model_allv)$fixed)
tavrange <- seq(0, 17500, by = .1)
tavExp <- exp(summ_tav[1,1] + summ_tav[2,1] * tavrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
tavProb <- tavExp/(1+tavExp)

tavExp_U <- exp(summ_tav[1,4] + summ_tav[2,4] * tavrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
tavProb_U <- tavExp_U/(1+tavExp_U)

tavExp_L <- exp(summ_tav[1,3] + summ_tav[2,3] * tavrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
tavProb_L <- tavExp_L/(1+tavExp_L)
tav_df <- data.frame(tavProb, tavExp, tavProb_U, tavExp_U, tavProb_L, tavExp_L, tavrange)

ggplot(tav_df, aes(x = tavrange, y = tavProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tavProb_L, ymax = tavProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 17500)

###Turtle Total Volume + age ####
turtle_model_allva <- brm(COD ~ volume_estimate + Age, prior = priors, data = turtles, 
                        control = list(adapt_delta = 0.9),  
                        family = bernoulli())

loo_turtle_model_allva <- loo(turtle_model_allva)  #165.8 pareto k = 698 good, 1 okay, 1 bad, 1 very bad#

#TAVA is turtle all plastics, volume model with age
summ_tava <- as.data.frame(summary(turtle_model_allva)$fixed)
tavarange <- seq(0, 20000, by = .1)

tavaExp <- exp(summ_tava[1,1] + summ_tava[2,1] * tavarange + summ_tava[3,1] * ptinf + summ_tava[4,1] * ptjuv + summ_tava[5,1] *ptsa)
tavaProb <- tavaExp/(1+tavaExp)

tavaExp_U <- exp(summ_tava[1,4] + summ_tava[2,4] * tavarange + summ_tava[3,4] * ptinf + summ_tava[4,4] * ptjuv + summ_tava[5,4] *ptsa) 
tavaProb_U <- tavaExp_U/(1+tavaExp_U)

tavaExp_L <- exp(summ_tava[1,3] + summ_tava[2,3] * tavarange + summ_tava[3,3] * ptinf + summ_tava[4,3] * ptjuv + summ_tava[5,3] *ptsa)  
tavaProb_L <- tavaExp_L/(1+tavaExp_L)
tava_df <- data.frame(tavaProb, tavaExp, tavaProb_U, tavaExp_U, tavaProb_L, tavaExp_L, tavarange)

ggplot(tava_df, aes(x = tavarange, y = tavaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tavaProb_L, ymax = tavaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/age",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20000)

###Turtle Total Volume + Species_size####
turtle_model_allv_ss <- brm(COD ~ volume_estimate + Size_avg, 
                          prior = priors, data = turtles, 
                          control = list(adapt_delta = 0.9),  
                          family = bernoulli()) 

loo_turtle_model_allv_ss <- loo(turtle_model_allv_ss)  #419.7 pareto k = 941 good, 1 bad

#TAVSS is turtle all plastics volume model with species size
summ_tavss <- as.data.frame(summary(turtle_model_allv_ss)$fixed)
tavssrange <- seq(0, 20000, by = .1)
tavssExp <- exp(summ_tavss[1,1] + summ_tavss[2,1] * tavssrange + summ_tavss[3,1] * mean(turtles$Size_avg, na.rm=TRUE))
tavssProb <- tavssExp/(1+tavssExp)

tavssExp_U <- exp(summ_tavss[1,4] + summ_tavss[2,4] * tavssrange+ summ_tavss[3,4] * mean(turtles$Size_avg, na.rm=TRUE)) 
tavssProb_U <- tavssExp_U/(1+tavssExp_U)

tavssExp_L <- exp(summ_tavss[1,3] + summ_tavss[2,3] * tavssrange+ summ_tavss[3,3] * mean(turtles$Size_avg, na.rm=TRUE))  
tavssProb_L <- tavssExp_L/(1+tavssExp_L)
tavss_df <- data.frame(tavssProb, tavssExp, tavssProb_U, tavssExp_U, tavssProb_L, tavssExp_L, tavssrange)

ggplot(tavss_df, aes(x = tavssrange, y = tavssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tavssProb_L, ymax = tavssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/ species size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 5000)

###Turtle Total Volume + Size####
turtle_model_allv_s <- brm(COD ~ volume_estimate + Size_age, 
                            prior = priors, data = turtles, 
                            control = list(adapt_delta = 0.9),  
                            family = bernoulli()) 

loo_turtle_model_allv_s <- loo(turtle_model_allv_s)  #281.4 pareto k = 861 good, 2 bad

#TAVS is turtle all plastics volume model with size
summ_tavs <- as.data.frame(summary(turtle_model_allv_s)$fixed)
tavsrange <- seq(0, 20000, by = .1)
tavsExp <- exp(summ_tavs[1,1] + summ_tavs[2,1] * tavsrange + summ_tavs[3,1] * mean(turtles$Size_age, na.rm=TRUE))
tavsProb <- tavsExp/(1+tavsExp)

tavsExp_U <- exp(summ_tavs[1,4] + summ_tavs[2,4] * tavsrange+ summ_tavs[3,4] * mean(turtles$Size_age, na.rm=TRUE)) 
tavsProb_U <- tavsExp_U/(1+tavsExp_U)

tavsExp_L <- exp(summ_tavs[1,3] + summ_tavs[2,3] * tavsrange+ summ_tavs[3,3] * mean(turtles$Size_age, na.rm=TRUE))  
tavsProb_L <- tavsExp_L/(1+tavsExp_L)
tavs_df <- data.frame(tavsProb, tavsExp, tavsProb_U, tavsExp_U, tavsProb_L, tavsExp_L, tavsrange)

ggplot(tavs_df, aes(x = tavsrange, y = tavsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tavsProb_L, ymax = tavsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/ size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 5000)

#Calculate x for 95% mortality
turtlevsize95 <- (log(19, base = exp(1)) - (summ_tavs[1,1] + summ_tavs[3,1] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tavs[2,1]
turtlevsize95_U <- (log(19, base = exp(1)) - (summ_tavs[1,4] + summ_tavs[3,4] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tavs[2,4]
turtlevsize95_L <- (log(19, base = exp(1)) - (summ_tavs[1,3] + summ_tavs[3,3] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tavs[2,3]
turtlevsize50 <- (log(1, base = exp(1)) - (summ_tavs[1,1] + summ_tavs[3,1] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tavs[2,1]
turtlevsize50_U <- (log(1, base = exp(1)) - (summ_tavs[1,4] + summ_tavs[3,4] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tavs[2,4]
turtlevsize50_L <- (log(1, base = exp(1)) - (summ_tavs[1,3] + summ_tavs[3,3] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tavs[2,3]

###Turtle Hard Pieces#####
turtle_model_hard <- brm(harddeath ~ hard, prior = priors,
                         data = turtles, 
                         family = bernoulli())

loo_turtle_model_hard <- loo(turtle_model_hard)  #149.6 pareto k = 1261 good, 2 okay

#th stands for Turtle Hard model with no additional factors
summ_th <- as.data.frame(summary(turtle_model_hard)$fixed)
thrange <- seq(0, 2000, by = .01)
thExp <- exp(summ_th[1,1] + summ_th[2,1] * thrange)
thProb <- thExp/(1+thExp)

thExp_U <- exp(summ_th[1,4] + summ_th[2,4] * thrange) 
thProb_U <- thExp_U/(1+thExp_U)

thExp_L <- exp(summ_th[1,3] + summ_th[2,3] * thrange)
thProb_L <- thExp_L/(1+thExp_L)
th_df <- data.frame(thProb, thExp, thProb_U, thExp_U, thProb_L, thExp_L, thrange)

ggplot(th_df, aes(x = thrange, y = thProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thProb_L, ymax = thProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by turtles",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 750)

###Turtle Hard pieces + Age####
turtle_model_hard_a <- brm(harddeath ~ hard + Age, prior = priors, data = turtles, 
                            control = list(adapt_delta = 0.9),  
                            family = bernoulli()) 

loo_turtle_model_hard_a <- loo(turtle_model_hard_a) #56.9 738 pareto k were good and 1  okay 

#THA for Turtle All Age
summ_tha <- as.data.frame(summary(turtle_model_hard_a)$fixed)
tharange <- seq(0, 2000, by = .01)

thaExp <- exp(summ_tha[1,1] + summ_tha[2,1] * tharange + summ_tha[3,1] * ptinf + summ_tha[4,1] * ptjuv + summ_tha[5,1] *ptsa)
thaProb <- thaExp/(1+thaExp)

thaExp_U <- exp(summ_tha[1,4] + summ_tha[2,4] * tharange + summ_tha[3,4] * ptinf + summ_tha[4,4] * ptjuv + summ_tha[5,4] *ptsa) 
thaProb_U <- thaExp_U/(1+thaExp_U)

thaExp_L <- exp(summ_tha[1,3] + summ_tha[2,3] * tharange + summ_tha[3,3] * ptinf + summ_tha[4,3] * ptjuv + summ_tha[5,3] *ptsa)  
thaProb_L <- thaExp_L/(1+thaExp_L)
tha_df <- data.frame(thaProb, thaExp, thaProb_U, thaExp_U, thaProb_L, thaExp_L, tharange)

ggplot(tha_df, aes(x = tharange, y = thaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thaProb_L, ymax = thaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by turtles w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 500)

###Turtle Hard pieces + Species_size####
turtle_model_hard_ss <- brm(harddeath ~ hard + Size_avg, prior = priors, data = turtles, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_turtle_model_hard_ss <- loo(turtle_model_hard_ss) #125.8 1259 good, 1 okay, 1 bad.

#THSS is for turtle hard model with species size
summ_thss <- as.data.frame(summary(turtle_model_hard_ss)$fixed)
thssrange <- seq(0, 1000, by = .01)
thssExp <- exp(summ_thss[1,1] + summ_thss[2,1] * thssrange + summ_thss[3,1] * mean(turtles$Size_avg, na.rm=TRUE))# + summ_mt[4,1] * pjuv + summ_mt[5,1] * psa)
thssProb <- thssExp/(1+thssExp)

thssExp_U <- exp(summ_thss[1,4] + summ_thss[2,4] * thssrange + summ_thss[3,4] * mean(turtles$Size_avg, na.rm=TRUE))#+ summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
thssProb_U <- thssExp_U/(1+thssExp_U)

thssExp_L <- exp(summ_thss[1,3] + summ_thss[2,3] * thssrange + summ_thss[3,3] * mean(turtles$Size_avg, na.rm=TRUE))# + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
thssProb_L <- thssExp_L/(1+thssExp_L)
thss_df <- data.frame(thssProb, thssExp, thssProb_U, thssExp_U, thssProb_L, thssExp_L, thssrange)

ggplot(thss_df, aes(x = thssrange, y = thssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thssProb_L, ymax = thssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by turtles w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 750)

#Calculate x for 95% mortality
turtlehardspeciessize95 <- (log(19, base = exp(1)) - (summ_thss[1,1] + summ_thss[3,1] * mean(turtles$Size_avg, na.rm=TRUE)))/summ_thss[2,1]
turtlehardspeciessize95_U <- (log(19, base = exp(1)) - (summ_thss[1,4] + summ_thss[3,4] * mean(turtles$Size_avg, na.rm=TRUE)))/summ_thss[2,4]
turtlehardspeciessize95_L <- (log(19, base = exp(1)) - (summ_thss[1,3] + summ_thss[3,3] * mean(turtles$Size_avg, na.rm=TRUE)))/summ_thss[2,3]
turtlehardspeciessize50 <- (log(1, base = exp(1)) - (summ_thss[1,1] + summ_thss[3,1] * mean(turtles$Size_avg, na.rm=TRUE)))/summ_thss[2,1]
turtlehardspeciessize50_U <- (log(1, base = exp(1)) - (summ_thss[1,4] + summ_thss[3,4] * mean(turtles$Size_avg, na.rm=TRUE)))/summ_thss[2,4]
turtlehardspeciessize50_L <- (log(1, base = exp(1)) - (summ_thss[1,3] + summ_thss[3,3] * mean(turtles$Size_avg, na.rm=TRUE)))/summ_thss[2,3]

###Turtle Hard Pieces + Size####
turtle_model_hard_s <- brm(harddeath ~ hard + Size_age, prior = priors,
                         data = turtles, 
                         family = bernoulli())

loo_turtle_model_hard_s <- loo(turtle_model_hard_s) #107.1 1254 pareto k were good and 4 okay 

#THS is for turtle hard model with size
summ_ths <- as.data.frame(summary(turtle_model_hard_s)$fixed)
thsrange <- seq(0, 1000, by = .01)
thsExp <- exp(summ_ths[1,1] + summ_ths[2,1] * thsrange + summ_ths[3,1] * mean(turtles$Size_age, na.rm=TRUE))# + summ_mt[4,1] * pjuv + summ_mt[5,1] * psa)
thsProb <- thsExp/(1+thsExp)

thsExp_U <- exp(summ_ths[1,4] + summ_ths[2,4] * thsrange + summ_ths[3,4] * mean(turtles$Size_age, na.rm=TRUE))#+ summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
thsProb_U <- thsExp_U/(1+thsExp_U)

thsExp_L <- exp(summ_ths[1,3] + summ_ths[2,3] * thsrange + summ_ths[3,3] * mean(turtles$Size_age, na.rm=TRUE))# + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
thsProb_L <- thsExp_L/(1+thsExp_L)
ths_df <- data.frame(thsProb, thsExp, thsProb_U, thsExp_U, thsProb_L, thsExp_L, thsrange)

ggplot(ths_df, aes(x = thsrange, y = thsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thsProb_L, ymax = thsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by turtles w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Hard Volume####
turtle_model_hardv <- brm(harddeath ~ hard_volume, prior = priors, data = turtles, 
                         control = list(adapt_delta = 0.9),  
                         family = bernoulli())

loo_turtle_model_hardv <- loo(turtle_model_hardv) #168.6 pareto k = 1187 good, 1 okay, 1 very bad

#thv is for turtle all volume
summ_thv <- as.data.frame(summary(turtle_model_hardv)$fixed)
thvrange <- seq(0, 17500, by = .1)
thvExp <- exp(summ_thv[1,1] + summ_thv[2,1] * thvrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
thvProb <- thvExp/(1+thvExp)

thvExp_U <- exp(summ_thv[1,4] + summ_thv[2,4] * thvrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
thvProb_U <- thvExp_U/(1+thvExp_U)

thvExp_L <- exp(summ_thv[1,3] + summ_thv[2,3] * thvrange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
thvProb_L <- thvExp_L/(1+thvExp_L)
thv_df <- data.frame(thvProb, thvExp, thvProb_U, thvExp_U, thvProb_L, thvExp_L, thvrange)

ggplot(thv_df, aes(x = thvrange, y = thvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thvProb_L, ymax = thvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 5000)

###Turtle Hard Volume + Age####
##Sample size too small
turtle_model_hardva <- brm(harddeath ~ hard_volume + Age, prior = priors, data = turtles, 
                          control = list(adapt_delta = 0.9),  
                          family = bernoulli())

loo_turtle_model_hardva <- loo(turtle_model_hardva)  #165.8 pareto k = 698 good, 1 okay, 1 bad, 1 very bad#

#THVA is turtle all plastics, volume model with age
summ_thva <- as.data.frame(summary(turtle_model_hardva)$fixed)
thvarange <- seq(0, 20000, by = .1)

thvaExp <- exp(summ_thva[1,1] + summ_thva[2,1] * thvarange + summ_thva[3,1] * ptinf + summ_thva[4,1] * ptjuv + summ_thva[5,1] *ptsa)
thvaProb <- thvaExp/(1+thvaExp)

thvaExp_U <- exp(summ_thva[1,4] + summ_thva[2,4] * thvarange + summ_thva[3,4] * ptinf + summ_thva[4,4] * ptjuv + summ_thva[5,4] *ptsa) 
thvaProb_U <- thvaExp_U/(1+thvaExp_U)

thvaExp_L <- exp(summ_thva[1,3] + summ_thva[2,3] * thvarange + summ_thva[3,3] * ptinf + summ_thva[4,3] * ptjuv + summ_thva[5,3] *ptsa)  
thvaProb_L <- thvaExp_L/(1+thvaExp_L)
thva_df <- data.frame(thvaProb, thvaExp, thvaProb_U, thvaExp_U, thvaProb_L, thvaExp_L, thvarange)

ggplot(thva_df, aes(x = thvarange, y = thvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thvaProb_L, ymax = thvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Plastic ingestion by turtles w/age",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 20000)

###Turtle Hard Volume + Species Size#####
turtle_model_hardvss <- brm(harddeath ~ hard_volume + Size_avg, prior = priors,
                           data = turtles, 
                           family = bernoulli(logit))

loo_turtle_model_hardvss <- loo(turtle_model_hardvss) #144.7 1186 good, 1 very bad.

#THVSS for Turtle Hard Volume w/ Species Size
summ_thvss <- as.data.frame(summary(turtle_model_hardvss)$fixed)
thvssrange <- seq(0, 10000, by = .1)
thvssExp <- exp(summ_thvss[1,1] + summ_thvss[2,1] * thvssrange + summ_thvss[3,1] * mean(turtles$Size_avg, na.rm=TRUE))
thvssProb <- thvssExp/(1+thvssExp)

thvssExp_U <- exp(summ_thvss[1,4] + summ_thvss[2,4] * thvssrange + summ_thvss[3,4] * mean(turtles$Size_avg, na.rm=TRUE)) #+ summ_th[3,4] * pinf + summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
thvssProb_U <- thvssExp_U/(1+thvssExp_U)

thvssExp_L <- exp(summ_thvss[1,3] + summ_thvss[2,3] * thvssrange + summ_thvss[3,3] * mean(turtles$Size_avg, na.rm=TRUE)) #+ summ_mt[3,3] * pinf + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
thvssProb_L <- thvssExp_L/(1+thvssExp_L)
thvss_df <- data.frame(thvssProb, thvssExp, thvssProb_U, thvssExp_U, thvssProb_L, thvssExp_L, thvssrange)

ggplot(thvss_df, aes(x = thvssrange, y = thvssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thvssProb_L, ymax = thvssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by turtles w/ species size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Hard Volume + Size ######
turtle_model_hardvs <- brm(harddeath ~ hard_volume + Size_age, prior = priors,
                         data = turtles, 
                         family = bernoulli(logit))

loo_turtle_model_hardvs <- loo(turtle_model_hardvs) #118.4 1183 good, 1 okay, 1 very bad.

summ_thvs <- as.data.frame(summary(turtle_model_hardvs)$fixed)
thvsrange <- seq(0, 10000, by = .1)
thvsExp <- exp(summ_thvs[1,1] + summ_thvs[2,1] * thvsrange + summ_thvs[3,1] * mean(turtles$Size_age, na.rm=TRUE))
thvsProb <- thvsExp/(1+thvsExp)

thvsExp_U <- exp(summ_thvs[1,4] + summ_thvs[2,4] * thvsrange + summ_thvs[3,4] * mean(turtles$Size_age, na.rm=TRUE)) #+ summ_th[3,4] * pinf + summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
thvsProb_U <- thvsExp_U/(1+thvsExp_U)

thvsExp_L <- exp(summ_thvs[1,3] + summ_thvs[2,3] * thvsrange + summ_thvs[3,3] * mean(turtles$Size_age, na.rm=TRUE)) #+ summ_mt[3,3] * pinf + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
thvsProb_L <- thvsExp_L/(1+thvsExp_L)
thvs_df <- data.frame(thvsProb, thvsExp, thvsProb_U, thvsExp_U, thvsProb_L, thvsExp_L, thvsrange)

ggplot(thvs_df, aes(x = thvsrange, y = thvsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = thvsProb_L, ymax = thvsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Hard plastic ingestion by turtles w/ size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Soft Pieces#####
turtle_model_soft <- brm(softdeath ~ soft, prior = priors,
                         data = turtles, 
                         family = bernoulli())

loo_turtle_model_soft <- loo(turtle_model_soft)  #223.5 pareto k = 1269 good, 1 bad

#ts stands for Turtle soft model with no additional factors
summ_ts <- as.data.frame(summary(turtle_model_soft)$fixed)
tsrange <- seq(0, 2000, by = .01)
tsExp <- exp(summ_ts[1,1] + summ_ts[2,1] * tsrange)
tsProb <- tsExp/(1+tsExp)

tsExp_U <- exp(summ_ts[1,4] + summ_ts[2,4] * tsrange) 
tsProb_U <- tsExp_U/(1+tsExp_U)

tsExp_L <- exp(summ_ts[1,3] + summ_ts[2,3] * tsrange)
tsProb_L <- tsExp_L/(1+tsExp_L)
ts_df <- data.frame(tsProb, tsExp, tsProb_U, tsExp_U, tsProb_L, tsExp_L, tsrange)

ggplot(ts_df, aes(x = tsrange, y = tsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsProb_L, ymax = tsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by turtles",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)

###Turtle Soft pieces + Age####
turtle_model_soft_a <- brm(softdeath ~ soft + Age, prior = priors, data = turtles, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_turtle_model_soft_a <- loo(turtle_model_soft_a) #118.8 743 pareto k were good and 2 okay 

#TSA for Turtle All Age
summ_tsa <- as.data.frame(summary(turtle_model_soft_a)$fixed)
tsarange <- seq(0, 2000, by = .01)

tsaExp <- exp(summ_tsa[1,1] + summ_tsa[2,1] * tsarange + summ_tsa[3,1] * ptinf + summ_tsa[4,1] * ptjuv + summ_tsa[5,1] *ptsa)
tsaProb <- tsaExp/(1+tsaExp)

tsaExp_U <- exp(summ_tsa[1,4] + summ_tsa[2,4] * tsarange + summ_tsa[3,4] * ptinf + summ_tsa[4,4] * ptjuv + summ_tsa[5,4] *ptsa) 
tsaProb_U <- tsaExp_U/(1+tsaExp_U)

tsaExp_L <- exp(summ_tsa[1,3] + summ_tsa[2,3] * tsarange + summ_tsa[3,3] * ptinf + summ_tsa[4,3] * ptjuv + summ_tsa[5,3] *ptsa)  
tsaProb_L <- tsaExp_L/(1+tsaExp_L)
tsa_df <- data.frame(tsaProb, tsaExp, tsaProb_U, tsaExp_U, tsaProb_L, tsaExp_L, tsarange)

ggplot(tsa_df, aes(x = tsarange, y = tsaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsaProb_L, ymax = tsaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "soft plastic ingestion by turtles w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 750)

###Turtle Soft pieces + Species_size####
turtle_model_soft_ss <- brm(softdeath ~ soft + Size_avg, prior = priors, data = turtles, 
                            control = list(adapt_delta = 0.9),  
                            family = bernoulli()) 

loo_turtle_model_soft_ss <- loo(turtle_model_soft_ss) #225.2 1267 good, 1 bad.

#THSS is for turtle soft model with species size
summ_tsss <- as.data.frame(summary(turtle_model_soft_ss)$fixed)
tsssrange <- seq(0, 1000, by = .01)
tsssExp <- exp(summ_tsss[1,1] + summ_tsss[2,1] * tsssrange + summ_tsss[3,1] * mean(turtles$Size_avg, na.rm=TRUE))# + summ_mt[4,1] * pjuv + summ_mt[5,1] * psa)
tsssProb <- tsssExp/(1+tsssExp)

tsssExp_U <- exp(summ_tsss[1,4] + summ_tsss[2,4] * tsssrange + summ_tsss[3,4] * mean(turtles$Size_avg, na.rm=TRUE))#+ summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
tsssProb_U <- tsssExp_U/(1+tsssExp_U)

tsssExp_L <- exp(summ_tsss[1,3] + summ_tsss[2,3] * tsssrange + summ_tsss[3,3] * mean(turtles$Size_avg, na.rm=TRUE))# + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
tsssProb_L <- tsssExp_L/(1+tsssExp_L)
tsss_df <- data.frame(tsssProb, tsssExp, tsssProb_U, tsssExp_U, tsssProb_L, tsssExp_L, tsssrange)

ggplot(tsss_df, aes(x = tsssrange, y = tsssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsssProb_L, ymax = tsssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "soft plastic ingestion by turtles w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 750)

###Turtle Soft Pieces + Size#####
turtle_model_softs <- brm(softdeath ~ soft + Size_age, prior = priors, 
                         data = turtles, 
                         family = bernoulli())

loo_turtle_model_softs <- loo(turtle_model_softs) #215.4 1263 good, 1 okay

#TSS is turtle soft death with size
summ_tss <- as.data.frame(summary(turtle_model_softs)$fixed)
tssrange <- seq(0, 4000, by = .1)
tssExp <- exp(summ_tss[1,1] + summ_tss[2,1] * tssrange + summ_tss[3,1] * mean(turtles$Size_age, na.rm=TRUE))
tssProb <- tssExp/(1+tssExp)

tssExp_U <- exp(summ_tss[1,4] + summ_tss[2,4] * tssrange + summ_tss[3,4] * mean(turtles$Size_age, na.rm=TRUE))
tssProb_U <- tssExp_U/(1+tssExp_U)

tssExp_L <- exp(summ_tss[1,3] + summ_tss[2,3] * tssrange + summ_tss[3,3] * mean(turtles$Size_age, na.rm=TRUE))
tssProb_L <- tssExp_L/(1+tssExp_L)
tss_df <- data.frame(tssProb, tssExp, tssProb_U, tssExp_U, tssProb_L, tssExp_L, tssrange)

ggplot(tss_df, aes(x = tssrange, y = tssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tssProb_L, ymax = tssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by turtles w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 4000)

###Turtle Soft volume####
turtle_model_softv <- brm(softdeath ~ soft_volume, prior = priors, data = turtles, 
                          control = list(adapt_delta = 0.9),  
                          family = bernoulli())

loo_turtle_model_softv <- loo(turtle_model_softv) #104.9 pareto k = 1143 good, 2 very bad

#tsv is for turtle all volume
summ_tsv <- as.data.frame(summary(turtle_model_softv)$fixed)
tsvrange <- seq(0, 17500, by = .1)
tsvExp <- exp(summ_tsv[1,1] + summ_tsv[2,1] * tsvrange)
tsvProb <- tsvExp/(1+tsvExp)

tsvExp_U <- exp(summ_tsv[1,4] + summ_tsv[2,4] * tsvrange) 
tsvProb_U <- tsvExp_U/(1+tsvExp_U)

tsvExp_L <- exp(summ_tsv[1,3] + summ_tsv[2,3] * tsvrange)
tsvProb_L <- tsvExp_L/(1+tsvExp_L)
tsv_df <- data.frame(tsvProb, tsvExp, tsvProb_U, tsvExp_U, tsvProb_L, tsvExp_L, tsvrange)

ggplot(tsv_df, aes(x = tsvrange, y = tsvProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsvProb_L, ymax = tsvProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft Plastic ingestion by turtles",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 6000)

###Turtle Soft volume + Age####
turtle_model_softva <- brm(softdeath ~ soft_volume + Age, prior = priors, data = turtles, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli())

loo_turtle_model_softva <- loo(turtle_model_softva)  #95.3 pareto k = 1137 good, 1 bad, 1 very bad

#tsva is turtle all plastics, volume model with age
summ_tsva <- as.data.frame(summary(turtle_model_softva)$fixed)
tsvarange <- seq(0, 20000, by = .1)

tsvaExp <- exp(summ_tsva[1,1] + summ_tsva[2,1] * tsvarange + summ_tsva[3,1] * ptinf + summ_tsva[4,1] * ptjuv + summ_tsva[5,1] *ptsa)
tsvaProb <- tsvaExp/(1+tsvaExp)

tsvaExp_U <- exp(summ_tsva[1,4] + summ_tsva[2,4] * tsvarange + summ_tsva[3,4] * ptinf + summ_tsva[4,4] * ptjuv + summ_tsva[5,4] *ptsa) 
tsvaProb_U <- tsvaExp_U/(1+tsvaExp_U)

tsvaExp_L <- exp(summ_tsva[1,3] + summ_tsva[2,3] * tsvarange + summ_tsva[3,3] * ptinf + summ_tsva[4,3] * ptjuv + summ_tsva[5,3] *ptsa)  
tsvaProb_L <- tsvaExp_L/(1+tsvaExp_L)
tsva_df <- data.frame(tsvaProb, tsvaExp, tsvaProb_U, tsvaExp_U, tsvaProb_L, tsvaExp_L, tsvarange)

ggplot(tsva_df, aes(x = tsvarange, y = tsvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsvaProb_L, ymax = tsvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by turtles w/age",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 10000)

###Turtle Soft Volume + Species Size#####
turtle_model_softvss <- brm(softdeath ~ soft_volume + Size_avg, prior = priors,
                            data = turtles, 
                            family = bernoulli(logit))

loo_turtle_model_softvss <- loo(turtle_model_softvss) #101.6 1141 good, 2 bad.

#TSVSS for Turtle soft Volume w/ Species Size
summ_tsvss <- as.data.frame(summary(turtle_model_softvss)$fixed)
tsvssrange <- seq(0, 10000, by = .1)
tsvssExp <- exp(summ_tsvss[1,1] + summ_tsvss[2,1] * tsvssrange + summ_tsvss[3,1] * mean(turtles$Size_avg, na.rm=TRUE))
tsvssProb <- tsvssExp/(1+tsvssExp)

tsvssExp_U <- exp(summ_tsvss[1,4] + summ_tsvss[2,4] * tsvssrange + summ_tsvss[3,4] * mean(turtles$Size_avg, na.rm=TRUE)) #+ summ_th[3,4] * pinf + summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
tsvssProb_U <- tsvssExp_U/(1+tsvssExp_U)

tsvssExp_L <- exp(summ_tsvss[1,3] + summ_tsvss[2,3] * tsvssrange + summ_tsvss[3,3] * mean(turtles$Size_avg, na.rm=TRUE)) #+ summ_mt[3,3] * pinf + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
tsvssProb_L <- tsvssExp_L/(1+tsvssExp_L)
tsvss_df <- data.frame(tsvssProb, tsvssExp, tsvssProb_U, tsvssExp_U, tsvssProb_L, tsvssExp_L, tsvssrange)

ggplot(tsvss_df, aes(x = tsvssrange, y = tsvssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsvssProb_L, ymax = tsvssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "soft plastic ingestion by turtles w/ species size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 10000)

###Turtle Soft Volume + Size####
turtle_model_softvs <- brm(softdeath ~ soft_volume + Size_age, prior = priors,
                          data = turtles, 
                          family = bernoulli(logit))

loo_turtle_model_softvs <- loo(turtle_model_softvs) #90.0 1136 good, 1 okay, 2 very bad

#TSVS is for Turtle Soft Volume with Size
summ_tsvs <- as.data.frame(summary(turtle_model_softvs)$fixed)
tsvsrange <- seq(0, 6000, by = .1)
tsvsExp <- exp(summ_tsvs[1,1] + summ_tsvs[2,1] * tsvsrange + summ_tsvs[3,1] * mean(turtles$Size_age, na.rm=TRUE))
tsvsProb <- tsvsExp/(1+tsvsExp)

tsvsExp_U <- exp(summ_tsvs[1,4] + summ_tsvs[2,4] * tsvsrange + summ_tsvs[3,4] * mean(turtles$Size_age, na.rm=TRUE))
tsvsProb_U <- tsvsExp_U/(1+tsvsExp_U)

tsvsExp_L <- exp(summ_tsvs[1,3] + summ_tsvs[2,3] * tsvsrange + summ_tsvs[3,3] * mean(turtles$Size_age, na.rm=TRUE))
tsvsProb_L <- tsvsExp_L/(1+tsvsExp_L)
tsvs_df <- data.frame(tsvsProb, tsvsExp, tsvsProb_U, tsvsExp_U, tsvsProb_L, tsvsExp_L, tsvsrange)

ggplot(tsvs_df, aes(x = tsvsrange, y = tsvsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = tsvsProb_L, ymax = tsvsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Soft plastic ingestion by turtles w/Size",
       x = "Volume (cm3)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 5000)

#Calculate x for 95% mortality
turtlesoftvsize95 <- (log(19, base = exp(1)) - (summ_tsvs[1,1] + summ_tsvs[3,1] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tsvs[2,1]
turtlesoftvsize95_U <- (log(19, base = exp(1)) - (summ_tsvs[1,4] + summ_tsvs[3,4] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tsvs[2,4]
turtlesoftvsize95_L <- (log(19, base = exp(1)) - (summ_tsvs[1,3] + summ_tsvs[3,3] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tsvs[2,3]
turtlesoftvsize50 <- (log(1, base = exp(1)) - (summ_tsvs[1,1] + summ_tsvs[3,1] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tsvs[2,1]
turtlesoftvsize50_U <- (log(1, base = exp(1)) - (summ_tsvs[1,4] + summ_tsvs[3,4] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tsvs[2,4]
turtlesoftvsize50_L <- (log(1, base = exp(1)) - (summ_tsvs[1,3] + summ_tsvs[3,3] * mean(turtles$Size_age, na.rm=TRUE)))/summ_tsvs[2,3]

###Turtle Thread Pieces####
turtle_model_thread <- brm(threaddeath ~ thread, prior = priors,
                         data = turtles, 
                         family = bernoulli(logit))

loo_turtle_model_thread <- loo(turtle_model_thread)  #80.5 pareto k = 1250 good, 2 ok

#TT is for turtle thread model with no additional factors
summ_tt <- as.data.frame(summary(turtle_model_thread)$fixed)
ttrange <- seq(0, 1000, by = .01)
ttExp <- exp(summ_tt[1,1] + summ_tt[2,1] * ttrange)
ttProb <- ttExp/(1+ttExp)

ttExp_U <- exp(summ_tt[1,4] + summ_tt[2,4] * ttrange)
ttProb_U <- ttExp_U/(1+ttExp_U)

ttExp_L <- exp(summ_tt[1,3] + summ_tt[2,3] * ttrange)
ttProb_L <- ttExp_L/(1+ttExp_L)
tt_df <- data.frame(ttProb, ttExp, ttProb_U, ttExp_U, ttProb_L, ttExp_L, ttrange)

ggplot(tt_df, aes(x = ttrange, y = ttProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttProb_L, ymax = ttProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "thread plastic ingestion by turtles",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Thread pieces + Age####
turtle_model_thread_a <- brm(threaddeath ~ thread + Age, prior = priors, data = turtles, 
                           control = list(adapt_delta = 0.9),  
                           family = bernoulli()) 

loo_turtle_model_thread_a <- loo(turtle_model_thread_a) #81.7 1244 pareto k were good and 2 bad 

#tta for Turtle All Age
summ_tta <- as.data.frame(summary(turtle_model_soft_a)$fixed)
ttarange <- seq(0, 2000, by = .01)

ttaExp <- exp(summ_tta[1,1] + summ_tta[2,1] * ttarange + summ_tta[3,1] * ptinf + summ_tta[4,1] * ptjuv + summ_tta[5,1] *ptsa)
ttaProb <- ttaExp/(1+ttaExp)

ttaExp_U <- exp(summ_tta[1,4] + summ_tta[2,4] * ttarange + summ_tta[3,4] * ptinf + summ_tta[4,4] * ptjuv + summ_tta[5,4] *ptsa) 
ttaProb_U <- ttaExp_U/(1+ttaExp_U)

ttaExp_L <- exp(summ_tta[1,3] + summ_tta[2,3] * ttarange + summ_tta[3,3] * ptinf + summ_tta[4,3] * ptjuv + summ_tta[5,3] *ptsa)  
ttaProb_L <- ttaExp_L/(1+ttaExp_L)
tta_df <- data.frame(ttaProb, ttaExp, ttaProb_U, ttaExp_U, ttaProb_L, ttaExp_L, ttarange)

ggplot(tta_df, aes(x = ttarange, y = ttaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttaProb_L, ymax = ttaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "soft plastic ingestion by turtles w/ age",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 600)

###Turtle Thread pieces + Species_size####
turtle_model_thread_ss <- brm(threaddeath ~ thread + Size_avg, prior = priors, data = turtles, 
                              control = list(adapt_delta = 0.9),  
                              family = bernoulli()) 

loo_turtle_model_thread_ss <- loo(turtle_model_thread_ss) #81.9 1248 good, 1 okay, 1 bad.

#TTSS is for turtle thread model with species size
summ_ttss <- as.data.frame(summary(turtle_model_thread_ss)$fixed)
ttssrange <- seq(0, 1000, by = .01)
ttssExp <- exp(summ_ttss[1,1] + summ_ttss[2,1] * ttssrange + summ_ttss[3,1] * mean(turtles$Size_avg, na.rm=TRUE))# + summ_mt[4,1] * pjuv + summ_mt[5,1] * psa)
ttssProb <- ttssExp/(1+ttssExp)

ttssExp_U <- exp(summ_ttss[1,4] + summ_ttss[2,4] * ttssrange + summ_ttss[3,4] * mean(turtles$Size_avg, na.rm=TRUE))#+ summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
ttssProb_U <- ttssExp_U/(1+ttssExp_U)

ttssExp_L <- exp(summ_ttss[1,3] + summ_ttss[2,3] * ttssrange + summ_ttss[3,3] * mean(turtles$Size_avg, na.rm=TRUE))# + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
ttssProb_L <- ttssExp_L/(1+ttssExp_L)
ttss_df <- data.frame(ttssProb, ttssExp, ttssProb_U, ttssExp_U, ttssProb_L, ttssExp_L, ttssrange)

ggplot(ttss_df, aes(x = ttssrange, y = ttssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttssProb_L, ymax = ttssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "thread plastic ingestion by turtles w/ species size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 1000)

###Turtle Thread Pieces + Size#####
turtle_model_threads <- brm(threaddeath ~ thread + Size_age, prior = priors, 
                            data = turtles, 
                            family = bernoulli())

loo_turtle_model_threads <- loo(turtle_model_threads) #81.7 1245 good, 1 okay, 1 bad

#TTS is turtle thread death with size
summ_tts <- as.data.frame(summary(turtle_model_threads)$fixed)
ttsrange <- seq(0, 4000, by = .1)
ttsExp <- exp(summ_tts[1,1] + summ_tts[2,1] * ttsrange + summ_tts[3,1] * mean(turtles$Size_age, na.rm=TRUE))
ttsProb <- ttsExp/(1+ttsExp)

ttsExp_U <- exp(summ_tts[1,4] + summ_tts[2,4] * ttsrange + summ_tts[3,4] * mean(turtles$Size_age, na.rm=TRUE))
ttsProb_U <- ttsExp_U/(1+ttsExp_U)

ttsExp_L <- exp(summ_tts[1,3] + summ_tts[2,3] * ttsrange + summ_tts[3,3] * mean(turtles$Size_age, na.rm=TRUE))
ttsProb_L <- ttsExp_L/(1+ttsExp_L)
tts_df <- data.frame(ttsProb, ttsExp, ttsProb_U, ttsExp_U, ttsProb_L, ttsExp_L, ttsrange)

ggplot(tts_df, aes(x = ttsrange, y = ttsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttsProb_L, ymax = ttsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "thread plastic ingestion by turtles w/ size",
       x = "Pieces",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)

###Turtle Thread Length####
turtle_model_threadv <- brm(threaddeath ~ line_length, prior = priors,
                          data = turtles, 
                          family = bernoulli(logit))

loo_turtle_model_threadv <- loo(turtle_model_threadv)  #40 pareto k = 1233 good, 1 ok, 1 bad

#TTL is Turtle thread length
summ_ttl <- as.data.frame(summary(turtle_model_threadv)$fixed)
ttlrange <- seq(0, 5000, by = .01)
ttlExp <- exp(summ_ttl[1,1] + summ_ttl[2,1] * ttlrange)
ttlProb <- ttlExp/(1+ttlExp)

ttlExp_U <- exp(summ_ttl[1,4] + summ_ttl[2,4] * ttlrange)
ttlProb_U <- ttlExp_U/(1+ttlExp_U)

ttlExp_L <- exp(summ_ttl[1,3] + summ_ttl[2,3] * ttlrange)
ttlProb_L <- ttlExp_L/(1+ttlExp_L)

ttl_df <- data.frame(ttlProb, ttlExp, ttlProb_U, ttlExp_U, ttlProb_L, ttlExp_L, ttlrange)

ggplot(ttl_df, aes(x = ttlrange, y = ttlProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttlProb_L, ymax = ttlProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "thread plastic ingestion by turtles",
       x = "Line (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 4000)

###Turtle Thread Length + Age####
turtle_model_threadva <- brm(threaddeath ~ line_length + Age, prior = priors,
                            data = turtles, 
                            family = bernoulli(logit))

loo_turtle_model_threadva <- loo(turtle_model_threadva)  #44.4 pareto k = 1233 good, 1 bad, 1 very bad

#ttva is turtle all plastics, volume model with age
summ_ttva <- as.data.frame(summary(turtle_model_threadva)$fixed)
ttvarange <- seq(0, 20000, by = .1)

ttvaExp <- exp(summ_ttva[1,1] + summ_ttva[2,1] * ttvarange + summ_ttva[3,1] * ptinf + summ_ttva[4,1] * ptjuv + summ_ttva[5,1] *ptsa)
ttvaProb <- ttvaExp/(1+ttvaExp)

ttvaExp_U <- exp(summ_ttva[1,4] + summ_ttva[2,4] * ttvarange + summ_ttva[3,4] * ptinf + summ_ttva[4,4] * ptjuv + summ_ttva[5,4] *ptsa) 
ttvaProb_U <- ttvaExp_U/(1+ttvaExp_U)

ttvaExp_L <- exp(summ_ttva[1,3] + summ_ttva[2,3] * ttvarange + summ_ttva[3,3] * ptinf + summ_ttva[4,3] * ptjuv + summ_ttva[5,3] *ptsa)  
ttvaProb_L <- ttvaExp_L/(1+ttvaExp_L)
ttva_df <- data.frame(ttvaProb, ttvaExp, ttvaProb_U, ttvaExp_U, ttvaProb_L, ttvaExp_L, ttvarange)

ggplot(ttva_df, aes(x = ttvarange, y = ttvaProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttvaProb_L, ymax = ttvaProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "Thread plastic ingestion by turtles w/age",
       x = "Line (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 5000)

###Turtle Thread Length + Species Size####
turtle_model_threadvss <- brm(threaddeath ~ line_length + Size_avg, prior = priors,
                             data = turtles, 
                             family = bernoulli())

loo_turtle_model_threadvss <- loo(turtle_model_threadvss)  #44.2 pareto k = 1231 good, 1 bad, 1 very bad

#TTLSS is Turtle thread length with Size
summ_ttlss <- as.data.frame(summary(turtle_model_threadvs)$fixed)
ttlssrange <- seq(0, 5000, by = .01)
ttlssExp <- exp(summ_ttlss[1,1] + summ_ttlss[2,1] * ttlssrange + summ_ttlss[3,1] * mean(turtles$Size_avg, na.rm=TRUE)) # + summ_mt[3,1] * pinf + summ_mt[4,1] * pjuv + summ_mt[5,1] * psa)
ttlssProb <- ttlssExp/(1+ttlssExp)

ttlssExp_U <- exp(summ_ttlss[1,4] + summ_ttlss[2,4] * ttlssrange + summ_ttlss[3,4] * mean(turtles$Size_avg, na.rm=TRUE)) #+ summ_th[3,4] * pinf + summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
ttlssProb_U <- ttlssExp_U/(1+ttlssExp_U)

ttlssExp_L <- exp(summ_ttlss[1,3] + summ_ttlss[2,3] * ttlssrange + summ_thv[3,3] * mean(turtles$Size_avg, na.rm=TRUE)) #+ summ_mt[3,3] * pinf + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
ttlssProb_L <- ttlssExp_L/(1+ttlssExp_L)
ttlss_df <- data.frame(ttlssProb, ttlssExp, ttlssProb_U, ttlssExp_U, ttlssProb_L, ttlssExp_L, ttlssrange)

ggplot(ttlss_df, aes(x = ttlssrange, y = ttlssProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttlssProb_L, ymax = ttlssProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "thread plastic ingestion by turtles w/ species size",
       x = "Line (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 4000)

###Turtle Thread Length + Size####
turtle_model_threadvs <- brm(threaddeath ~ line_length + Size_age, prior = priors,
                            data = turtles, 
                            family = bernoulli())

loo_turtle_model_threadvs <- loo(turtle_model_threadvs)  #43.6 pareto k = 1228 good, 1 bad, 1 very bad

#TTLS is Turtle thread length with Size
summ_ttls <- as.data.frame(summary(turtle_model_threadvs)$fixed)
ttlsrange <- seq(0, 5000, by = .01)
ttlsExp <- exp(summ_ttls[1,1] + summ_ttls[2,1] * ttlsrange + summ_ttls[3,1] * mean(turtles$Size_age, na.rm=TRUE)) # + summ_mt[3,1] * pinf + summ_mt[4,1] * pjuv + summ_mt[5,1] * psa)
ttlsProb <- ttlsExp/(1+ttlsExp)

ttlsExp_U <- exp(summ_ttls[1,4] + summ_ttls[2,4] * ttlsrange + summ_ttls[3,4] * mean(turtles$Size_age, na.rm=TRUE)) #+ summ_th[3,4] * pinf + summ_mt[4,4] * pjuv + summ_mt[5,4] * psa)
ttlsProb_U <- ttlsExp_U/(1+ttlsExp_U)

ttlsExp_L <- exp(summ_ttls[1,3] + summ_ttls[2,3] * ttlsrange + summ_thv[3,3] * mean(turtles$Size_age, na.rm=TRUE)) #+ summ_mt[3,3] * pinf + summ_mt[4,3] * pjuv + summ_mt[5,3] * psa)
ttlsProb_L <- ttlsExp_L/(1+ttlsExp_L)
ttls_df <- data.frame(ttlsProb, ttlsExp, ttlsProb_U, ttlsExp_U, ttlsProb_L, ttlsExp_L, ttlsrange)

ggplot(ttls_df, aes(x = ttlsrange, y = ttlsProb)) +
  geom_line() +
  geom_ribbon(aes(ymin = ttlsProb_L, ymax = ttlsProb_U), fill = "gray", alpha = 0.3) +
  scale_y_continuous(breaks = seq(0, 1, by=.1)) +
  labs(title = "thread plastic ingestion by turtles w/size",
       x = "Line (cm)",
       y = "Probability of Mortality") +
  theme_minimal() +
  xlim(0, 2000)
