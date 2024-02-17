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

#read in data frame
df <- read.csv("C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/dataframe.csv")
head(df)

#subset major taxa groups
birds <-  subset(df, Taxa == "Bird")
mammals <- subset(df, Taxa == "Mammal")
turtles <- subset(df, Taxa == "Turtle")

#
###Initial Plots to see differences between COD groups#######
# In these analyses I use non-binomial GLMs to check that the amount of plastic,
# by volume and pieces, varies significantly between individuals based on their 
# cause of death: known not debris deaths (KND), Probably debris death (PD), 
# indeterminate death (Ind), and Known Debris Deaths (KD)

#Plots to compare the amount of pieces and volume consumed by COD groups  
#Plots are broken up by taxa and material type
###Bird Plots#####
#birds <- birds %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), "KD", COD))
#If I want to combine PD and KD again remove the PD label and change the vector to c(1,3)
birds$COD <- factor(birds$COD, levels = c("KND", "Ind", "PD", "KD"))
par(mfrow = c(1,4))

#total pieces
ggplot(birds, aes(x = COD, y = total, fill = COD)) +
  geom_boxplot() +
  labs(title = "Birds",
       x = "COD",
       y = "All Pieces") +
  scale_fill_brewer(palette = "Set1")

#total volume
ggplot(birds, aes(x = COD, y = volume_estimate, fill = COD)) +
  geom_boxplot() +
  labs(title = "Birds",
       x = "COD",
       y = "All Volume") +
  scale_fill_brewer(palette = "Set1")

#hard pieces
#birds <- birds %>% mutate(harddeath = ifelse(harddeath %in% c("PD", "KD"), "KD", harddeath))
birds$harddeath <- factor(birds$harddeath, levels = c("KND", "Ind", "PD", "KD"))
par(mfrow = c(1,4))
ggplot(birds, aes(x = harddeath, y = hard, fill = harddeath)) +
  geom_boxplot() +
  labs(title = "Birds",
       x = "COD",
       y = "Hard pieces") +
  scale_fill_brewer(palette = "Set1") 

#hard volume
ggplot(birds, aes(x = harddeath, y = hard_volume, fill = harddeath)) +
  geom_boxplot() +
  labs(title = "Birds",
       x = "COD",
       y = "Hard Volume") +
  scale_fill_brewer(palette = "Set1") 

#rubber pieces
#birds <- birds %>% mutate(rubberdeath = ifelse(rubberdeath %in% c("PD", "KD"), "KD", rubberdeath))
birds$rubberdeath <- factor(birds$rubberdeath, levels = c("KND", "Ind", "PD", "KD"))
ggplot(birds, aes(x = rubberdeath, y = rubber, fill = rubberdeath)) +
  geom_boxplot() +
  labs(title = "Birds",
       x = "COD",
       y = "Rubber Pieces") +
  scale_fill_brewer(palette = "Set1") 

#rubber volume
birds$rubber_volume <- as.numeric(birds$rubber_volume)
ggplot(birds, aes(x = rubberdeath, y = rubber_volume, fill = rubberdeath)) +
  geom_boxplot() +
  labs(title = "Birds",
       x = "COD",
       y = "Rubber Volume") +
  scale_fill_brewer(palette = "Set1") 


###Mammal plots###########
#mammals <- mammals %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), "KD", COD))
mammals$COD <- factor(mammals$COD, levels = c("KND", "Ind", "PD", "KD"))
par(mfrow = c(1,4))

#total pieces
ggplot(mammals, aes(x = COD, y = total, fill = COD)) +
  geom_boxplot() +
  labs(title = "Mammals",
       x = "COD",
       y = "All Pieces") +
  scale_fill_brewer(palette = "Set1")

#total volume
ggplot(mammals, aes(x = COD, y = volume_estimate, fill = COD)) +
  geom_boxplot() +
  labs(title = "Mammals",
       x = "COD",
       y = "All Volume") +
  scale_fill_brewer(palette = "Set1")

#soft pieces
#mammals <- mammals %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), "KD", softdeath))
mammals$softdeath <- factor(mammals$softdeath, levels = c("KND", "PD", "KD"))
ggplot(mammals, aes(x = softdeath, y = soft, fill = softdeath)) +
  geom_boxplot() +
  labs(title = "Mammals",
       x = "COD",
       y = "Soft Pieces") +
  scale_fill_brewer(palette = "Set1") 

#soft volume
mammals$soft_volume <- as.numeric(mammals$soft_volume)
ggplot(mammals, aes(x = softdeath, y = soft_volume, fill = softdeath)) +
  geom_boxplot() +
  labs(title = "Mammals",
       x = "COD",
       y = "Soft Volume") +
  scale_fill_brewer(palette = "Set1")  

#thread pieces
#mammals <- mammals %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), "KD", threaddeath))
mammals$threaddeath <- factor(mammals$threaddeath, levels = c("KND", "Ind", "PD", "KD"))
ggplot(mammals, aes(x = threaddeath, y = thread, fill = threaddeath)) +
  geom_boxplot() +
  labs(title = "Mammals",
       x = "COD",
       y = "Thread Pieces") +
  scale_fill_brewer(palette = "Set1") 

#thread volume
mammals$thread_volume <- as.numeric(mammals$thread_volume)
ggplot(mammals, aes(x = threaddeath, y = thread_volume, fill = threaddeath)) +
  geom_boxplot() +
  labs(title = "Mammals",
       x = "COD",
       y = "Thread Volume") +
  scale_fill_brewer(palette = "Set1")  

###Turtle plots#########
#turtles <- turtles %>% mutate(COD = ifelse(COD %in% c("PD", "KD"), "KD", COD))
turtles$COD <- factor(turtles$COD, levels = c("KND", "Ind", "PD", "KD"))
par(mfrow = c(1,4))

#total pieces
ggplot(turtles, aes(x = COD, y = total, fill = COD)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "All Pieces") +
  scale_fill_brewer(palette = "Set1")

#total volume
ggplot(turtles, aes(x = COD, y = volume_estimate, fill = COD)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "All Volume") +
  scale_fill_brewer(palette = "Set1")

#hard pieces
#turtles <- turtles %>% mutate(harddeath = ifelse(harddeath %in% c("PD", "KD"), "KD", harddeath))
turtles$harddeath <- factor(turtles$harddeath, levels = c("KND", "Ind", "PD", "KD"))
par(mfrow = c(1,4))
ggplot(turtles, aes(x = harddeath, y = hard, fill = harddeath)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "Hard pieces") +
  scale_fill_brewer(palette = "Set1") 

#hard volume
turtles$hard_volume <- as.numeric(turtles$hard_volume)
ggplot(turtles, aes(x = harddeath, y = hard_volume, fill = harddeath)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "Hard Volume") +
  scale_fill_brewer(palette = "Set1") 

#soft pieces
turtles <- turtles %>% mutate(softdeath = ifelse(softdeath %in% c("PD", "KD"), "KD", softdeath))
turtles$softdeath <- factor(turtles$softdeath, levels = c("KND", "Ind", "PD", "KD"))
ggplot(turtles, aes(x = softdeath, y = soft, fill = softdeath)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "Soft Pieces") +
  scale_fill_brewer(palette = "Set1") 

#soft volume
turtles$soft_volume <- as.numeric(turtles$soft_volume)
ggplot(turtles, aes(x = softdeath, y = soft_volume, fill = softdeath)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "Soft Volume") +
  scale_fill_brewer(palette = "Set1")  

#thread pieces
#turtles <- turtles %>% mutate(threaddeath = ifelse(threaddeath %in% c("PD", "KD"), "KD", threaddeath))
turtles$threaddeath <- factor(turtles$threaddeath, levels = c("KND", "Ind", "PD", "KD"))
ggplot(turtles, aes(x = threaddeath, y = thread, fill = threaddeath)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "Thread Pieces") +
  scale_fill_brewer(palette = "Set1") 

#thread volume
turtles$thread_volume <- as.numeric(turtles$thread_volume)
ggplot(turtles, aes(x = threaddeath, y = thread_volume, fill = threaddeath)) +
  geom_boxplot() +
  labs(title = "Turtles",
       x = "COD",
       y = "Thread Volume") +
  scale_fill_brewer(palette = "Set1")  
#
###Figures to compare number of pieces to volume which provides residuals#####
#### Bird Mortality analysis 
birds$volume_estimate <- as.numeric(birds$volume_estimate)
birds$total <- as.numeric(birds$total)
birds <- birds %>%
  filter(!is.na(COD) | is.na(total) | is.na(volume_estimate))
plot(birds$total,birds$volume_estimate)
birdNumberVolRelation <- lm(volume_estimate ~ total - 1, data = birds, na.action = "na.exclude")
summary(birdNumberVolRelation)
birds$NVResiduals <- residuals(birdNumberVolRelation)

#### Mammal Mortality analysis 
mammals$volume_estimate <- as.numeric(mammals$volume_estimate)
mammals$total <- as.numeric(mammals$total)
mammals <- mammals %>%
  filter(!is.na(COD) | is.na(total) | is.na(volume_estimate))
plot(mammals$total,mammals$volume_estimate)
mammalNumberVolRelation <- lm(volume_estimate ~ total - 1, data = mammals, na.action = "na.exclude")
summary(mammalNumberVolRelation)
mammals$NVResiduals <- residuals(mammalNumberVolRelation)

#### Turtle Mortality analysis 
turtles$volume_estimate <- as.numeric(turtles$volume_estimate)
turtles$total <- as.numeric(turtles$total)
turtles <- turtles %>%
  filter(!is.na(COD) | is.na(total) | is.na(volume_estimate))
plot(turtles$total,turtles$volume_estimate)
turtleNumberVolRelation <- lm(volume_estimate ~ total - 1, data = turtles,  na.action = "na.exclude")
summary(turtleNumberVolRelation)
turtles$NVResiduals <- residuals(turtleNumberVolRelation)
### only did this for total materials so far, but could also do it for each material type



#
####Regression analysis of differences in amounts of debris by COD####
#If plastic causes death, then the order of the coefficients in terms of 
#plastic in the gut should be KND < Ind < PD < KD. 
#should be significantly different then each other (at least the KND and KD)

###Birds pieces#######
#PD was very likely KD so comfortable clumping together
birds$harddeath <- factor(birds$harddeath, levels = c("KND", "Ind", "KD"))
class(birds$harddeath)
birds$softdeath <- factor(birds$softdeath, levels = c("KND", "Ind", "KD"))
birds$rubberdeath <- factor(birds$rubberdeath, levels = c("KND", "Ind", "KD"))
###Birds hard pieces#######
#First PD and KD were classified together so there were only 3 groups to compare

#used non-binomial comparison here
#hard pieces
bhM.n.0 <- glm.nb(hard ~ 1, data = birds, na.action = "na.exclude")
bhM.n.C <- glm.nb(hard ~ harddeath, data = birds, na.action = "na.exclude")
bhM.n.CA <- glm.nb(hard ~ harddeath + Age, data = birds, na.action = "na.exclude")
#bhM.n.CAS <- glm.nb(hard ~ harddeath + Age + Species, data = birds, na.action = "na.exclude")
#bhM.n.CS <- glm.nb(hard ~ harddeath + Species, data = birds, na.action = "na.exclude") #removed for now cause getting an error
bhM.n.CAF <- glm.nb(hard ~ harddeath + Age + Family, data = birds, na.action = "na.exclude")
bhM.n.CF <- glm.nb(hard ~ harddeath + Family, data = birds, na.action = "na.exclude")

bhAICs <- round(c(bhM.n.0$aic, bhM.n.C$aic, bhM.n.CA$aic, bhM.n.CAF$aic, bhM.n.CF$aic),1) #removed M.n.CAxS$aic
bhModels <- c("bhM.n.0", "bhM.n.C", "bhM.n.CA", "bhM.n.CAF", "bhM.n.CF") #removed "M.n.CAxS"
birdhardAICTable.n <- cbind(bhModels, bhAICs)[order(bhAICs),]
#bhCAF model is the best fit

write.table(birdhardAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/birdhardAICTable.n.csv", sep = ",") 

#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1 - pchisq(summary(bhM.n.C)$deviance, summary(bhM.n.C)$df.residual)

#if they are ordered as we suspect
bhCoeffTest <- summary(glht(bhM.n.C, mcp(harddeath="Tukey")))
write.table(data.frame(names(bhCoeffTest$test$tstat),signif(bhCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/bird-soft-tukey.csv", sep = ",")
#coefficient vcov in the table shows the p-values. Here all 3 are significantly different. 
#Did AIC but I didn't use it because the data is limited and we really just want to prove these groups are different

#used kruskal wallis as well
#bhaov <- aov(hard_volume ~ harddeath, data = birds, na.action = "na.exclude")
bhkw <- kruskal.test(hard ~ harddeath, data = birds, na.action = "na.exclude")
#summary(bhaov)
print(bhkw)
bhposthoc_dunn <- dunn.test(birds$hard, g = birds$harddeath, method = "bonferroni")
#significant differences bteween KND and KD and KND and Ind

#Use Kruskal Wallis
bhkw <- kruskal.test(hard ~ harddeath, data = birds, na.action = "na.exclude")
print(bhkw)
bhposthoc_dunn <- dunn.test(birds$hard, g = birds$harddeath, method = "bonferroni")

###Bird rubber Pieces ------------------------------------------------------
brM.n.0 <- glm.nb(rubber ~ 1, data = birds, na.action = "na.exclude")
brM.n.C <- glm.nb(rubber ~ rubberdeath, data = birds, na.action = "na.exclude")
brM.n.CA <- glm.nb(rubber ~ rubberdeath + Age, data = birds, na.action = "na.exclude")
#brM.n.CAS <- glm.nb(rubber ~ rubberdeath + Age + Species, data = birds, na.action = "na.exclude")
#brM.n.CS <- glm.nb(rubber ~ rubberdeath + Species, data = birds, na.action = "na.exclude")
brM.n.CAF <- glm.nb(rubber ~ rubberdeath + Age + Family, data = birds, na.action = "na.exclude")
brM.n.CF <- glm.nb(rubber ~ rubberdeath + Family, data = birds, na.action = "na.exclude")
brAICs <- round(c(brM.n.0$aic, brM.n.C$aic, brM.n.CA$aic, brM.n.CAF$aic, brM.n.CF$aic),1) #removed M.n.CAxS$aic
brModels <- c("brM.n.0","brM.n.C","brM.n.CA", "brM.n.CAF", "brM.n.CF") #removed "M.n.CAxS"
birdrubberAICTable.n <- cbind(brModels, brAICs)[order(brAICs),]

#brM.n.CA is the best fit
write.table(birdrubberAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/birdrubberAICTable.n.csv", sep = ",") 
#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1- pchisq(summary(brM.n.C)$deviance, summary(brM.n.C)$df.residual)

#if they are ordered as we suspect.
brCoeffTest <- summary(glht(brM.n.C, mcp(rubberdeath="Tukey")))
write.table(data.frame(names(brCoeffTest$test$tstat),signif(brCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/bird-rubber-tukey.csv", sep = ",")
#KND is significantly different than IND and KD
brBarPlotValues <- signif(summary(brM.n.CA)$coefficients,2)[c(4,3:1),1]  ###Birds- Need to remove AgeAdult as its M.n.CS now. I also have 3 levels of COD not 4. I changed [c(4,1:3),1] for turtles to [c(3,2:1),1] for birds (ordered from debris->indeterminate->not debris)
brErrorBarValues <- signif(summary(brM.n.CA)$coefficients,2)[c(4,3:1),1] + c(-1, -1, -1, 1) * signif(summary(bsM.n.CA)$coefficients,2)[c(4,3:1),1]
barplot(brBarPlotValues, names.arg = c("KND","Ind","PD", "KD"), ylim = c(min(brErrorBarValues)-2,max(brErrorBarValues)+2), ylab = "Coefficient Value", xlab = "Cause of Death")
arrows(c(.75,1.9,3.1,4.3),BarPlotValues,c(.75,1.9,3.1,4.3),ErrorBarValues, angle = 90) ##need fixing
text(x = c(.75,1.9,3.1,4.3), y = c(10,10,10,10), labels = c("a","b","ab","b")) ##need fixing
###check rownames in fig match these
rownames(summary(brM.n.CA)$coefficients,2)[1:4]

#Use Kruskal Wallis here
brkw <- kruskal.test(rubber ~ rubberdeath, data = birds, na.action = "na.exclude")
print(brkw)
brposthoc_dunn <- dunn.test(birds$rubber, g = birds$rubberdeath, method = "bonferroni")

####Birds volume
###Bird Hard Volume #####
# I just used an anova and Kruskal wallis and then a dunn test. 
# I left the code from Laurens below but I dont see how that could be better
bvhaov <- aov(hard_volume ~ harddeath, data = birds, na.action = "na.exclude")
bvhkw <- kruskal.test(hard_volume ~ harddeath, data = birds, na.action = "na.exclude")
summary(bvhaov)
print(bvhkw)
bvhposthoc_dunn <- dunn.test(birds$hard_volume, g = birds$harddeath, method = "bonferroni")

###Bird rubber volume #####
# I just used an anova and Kruskal wallis and then a dunn test. 
# I left the code from Laurens below but I dont see how that could be better
bvraov <- aov(rubber_volume ~ rubberdeath, data = birds, na.action = "na.exclude")
bvrkw <- kruskal.test(rubber_volume ~ rubberdeath, data = birds, na.action = "na.exclude")
summary(bvraov)
print(bvrkw)
bvrposthoc_dunn <- dunn.test(birds$rubber_volume, g = birds$rubberdeath, method = "bonferroni")

###Mammals pieces####
mammals$harddeath <- factor(mammals$harddeath, levels = c("KND", "KD"))
class(mammals$harddeath)
mammals$softdeath <- factor(mammals$softdeath, levels = c("KND", "PD", "KD"))
mammals$threaddeath <- factor(mammals$threaddeath, levels = c("KND", "PD", "KD"))

###Mammals soft pieces####
msM.n.0 <- glm.nb(soft ~ 1, data = mammals, na.action = "na.exclude")
msM.n.C <- glm.nb(soft ~ softdeath, data = mammals, na.action = "na.exclude")
msM.n.CA <- glm.nb(soft ~ softdeath + Age, data = mammals, na.action = "na.exclude")
#msM.n.CAS <- glm.nb(soft ~ softdeath + Age + Species, data = mammals, na.action = "na.exclude")
msM.n.CS <- glm.nb(soft ~ softdeath + Species, data = mammals, na.action = "na.exclude")
#msM.n.CAF <- glm.nb(soft ~ softdeath + Age + Family, data = mammals, na.action = "na.exclude")
msM.n.CF <- glm.nb(soft ~ softdeath + Family, data = mammals, na.action = "na.exclude")
msAICs <- round(c(msM.n.0$aic, msM.n.C$aic, msM.n.CA$aic, msM.n.CS$aic, msM.n.CF$aic),1) #removed M.n.CAxS$aic
msModels <- c("msM.n.0","msM.n.C","msM.n.CA","msM.n.CS", "msM.n.CF") #removed "M.n.CAxS"
mammalsoftAICTable.n <- cbind(msModels, msAICs)[order(msAICs),]

#mhM.n.CAF is best fit
write.table(mammalsoftAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/mammalsoftAICTable.n.csv", sep = ",") 
#the best fit was the CAF but I used
#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1 - pchisq(summary(msM.n.CF)$deviance, summary(msM.n.CF)$df.residual)

#if they are ordered as we suspect.
msCoeffTest <- summary(glht(msM.n.CF, mcp(softdeath="Tukey")))
write.table(data.frame(names(msCoeffTest$test$tstat),signif(msCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/mammal-soft-tukey.csv", sep = ",")

#Use Kruskal Wallis here
mskw <- kruskal.test(soft ~ softdeath, data = mammals, na.action = "na.exclude")
print(mskw)
msposthoc_dunn <- dunn.test(mammals$soft, g = mammals$softdeath, method = "bonferroni")

###Mammals thread pieces#####
mtM.n.0 <- glm.nb(thread ~ 1, data = mammals, na.action = "na.exclude")
mtM.n.C <- glm.nb(thread ~ threaddeath, data = mammals, na.action = "na.exclude")
mtM.n.CA <- glm.nb(thread ~ threaddeath + Age, data = mammals, na.action = "na.exclude")
mtM.n.CAS <- glm.nb(thread ~ threaddeath + Age + Species, data = mammals, na.action = "na.exclude")
mtM.n.CS <- glm.nb(thread ~ threaddeath + Species, data = mammals, na.action = "na.exclude")
mtM.n.CAF <- glm.nb(thread ~ threaddeath + Age + Family, data = mammals, na.action = "na.exclude")
mtM.n.CF <- glm.nb(thread ~ threaddeath + Family, data = mammals, na.action = "na.exclude")
mtAICs <- round(c(mtM.n.0$aic, mtM.n.C$aic, mtM.n.CA$aic, mtM.n.CAS$aic, mtM.n.CS$aic, mtM.n.CAF$aic, mtM.n.CF$aic),1) #removed M.n.CAxS$aic
mtModels <- c("mtM.n.0","mtM.n.C","mtM.n.CA","mtM.n.CAS","mtM.n.CS","mtM.n.CAF", "mtM.n.CF") #removed "M.n.CAxS"
mammalthreadAICTable.n <- cbind(mtModels, msAICs)[order(msAICs),]

#mtM.n.CAF is best fit
write.table(mammalthreadAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/mammalthreadAICTable.n.csv", sep = ",") 
#the best fit was the CAF but I used
#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1 - pchisq(summary(mtM.n.CAF)$deviance, summary(mtM.n.CAF)$df.residual)

#if they are ordered as we suspect.
mtCoeffTest <- summary(glht(mtM.n.CAF, mcp(threaddeath="Tukey")))
write.table(data.frame(names(mtCoeffTest$test$tstat),signif(mtCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/mammal-thread-tukey.csv", sep = ",")
#all significantly different

#using a Kruskal Wallis now
mtkw <- kruskal.test(thread ~ threaddeath, data = mammals, na.action = "na.exclude")
print(mtkw)
mtposthoc_dunn <- dunn.test(mammals$thread, g = mammals$threaddeath, method = "bonferroni")

###Mammals soft volume#############
mammals$soft_volume <- as.numeric(mammals$soft_volume) 
mvsaov <- aov(soft_volume ~ softdeath, data = mammals, na.action = "na.exclude")
mvskw <- kruskal.test(soft_volume ~ softdeath, data = mammals, na.action = "na.exclude")
summary(mvsaov)
print(mvskw)
mvsposthoc_dunn <- dunn.test(mammals$soft_volume, g = mammals$softdeath, method = "bonferroni")

###Mammals thread volume#############
mammals$thread_volume <- as.numeric(mammals$thread_volume) 
mvtaov <- aov(thread_volume ~ threaddeath, data = mammals, na.action = "na.exclude")
mvtkw <- kruskal.test(thread_volume ~ threaddeath, data = mammals, na.action = "na.exclude")
summary(mvtaov)
print(mvtkw)
mvtposthoc_dunn <- dunn.test(mammals$thread_volume, g = mammals$threaddeath, method = "bonferroni")

###Turtles pieces####
turtles$harddeath <- factor(turtles$harddeath, levels = c("KND", "Ind", "KD"))
class(turtles$harddeath)
turtles$softdeath <- factor(turtles$softdeath, levels = c("KND", "Ind", "KD"))
turtles$threaddeath <- factor(turtles$threaddeath, levels = c("KND", "Ind", "KD"))

###Turtles hard pieces#####
thM.n.0 <- glm.nb(hard ~ 1, data = turtles, na.action = "na.exclude")
thM.n.C <- glm.nb(hard ~ harddeath, data = turtles, na.action = "na.exclude")
thM.n.CA <- glm.nb(hard ~ harddeath + Age, data = turtles, na.action = "na.exclude")
#thM.n.CAS <- glm.nb(hard ~ harddeath + Age + Species, data = turtles, na.action = "na.exclude")
thM.n.CS <- glm.nb(hard ~ harddeath + Species, data = turtles, na.action = "na.exclude")
#thM.n.CAF <- glm.nb(hard ~ harddeath + Age + Family, data = turtles, na.action = "na.exclude")
thM.n.CF <- glm.nb(hard ~ harddeath + Family, data = turtles, na.action = "na.exclude")
thAICs <- round(c(thM.n.0$aic, thM.n.C$aic, thM.n.CA$aic, thM.n.CS$aic, thM.n.CF$aic),1) #removed M.n.CAxS$aic
thModels <- c("thM.n.0","thM.n.C","thM.n.CA","thM.n.CS", "thM.n.CF") #removed "M.n.CAxS"
turtlehardAICTable.n <- cbind(thModels, thAICs)[order(thAICs),]

#thM.n.CAF is best fit
write.table(turtlehardAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/turtlehardAICTable.n.csv", sep = ",") 
#the best fit was the CAF but I used
#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1 - pchisq(summary(thM.n.CF)$deviance, summary(thM.n.CF)$df.residual)

#if they are ordered as we suspect.
thCoeffTest <- summary(glht(thM.n.CF, mcp(harddeath="Tukey")))
write.table(data.frame(names(thCoeffTest$test$tstat),signif(thCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/turtle-hard-tukey.csv", sep = ",")
#all significantly different

#Using Kruskal Wallis
thkw <- kruskal.test(hard ~ harddeath, data = turtles, na.action = "na.exclude")
print(thkw)
thposthoc_dunn <- dunn.test(turtles$hard, g = turtles$harddeath, method = "bonferroni")

###Turtles soft pieces####
tsM.n.0 <- glm.nb(soft ~ 1, data = turtles, na.action = "na.exclude")
tsM.n.C <- glm.nb(soft ~ softdeath, data = turtles, na.action = "na.exclude")
tsM.n.CA <- glm.nb(soft ~ softdeath + Age, data = turtles, na.action = "na.exclude")
#tsM.n.CAS <- glm.nb(soft ~ softdeath + Age + Species, data = turtles, na.action = "na.exclude")
tsM.n.CS <- glm.nb(soft ~ softdeath + Species, data = turtles, na.action = "na.exclude")
#tsM.n.CAF <- glm.nb(soft ~ softdeath + Age + Family, data = turtles, na.action = "na.exclude")
tsM.n.CF <- glm.nb(soft ~ softdeath + Family, data = turtles, na.action = "na.exclude")
tsAICs <- round(c(tsM.n.0$aic, tsM.n.C$aic, tsM.n.CA$aic, tsM.n.CS$aic, tsM.n.CF$aic),1) #removed M.n.CAxS$aic
tsModels <- c("tsM.n.0","tsM.n.C","tsM.n.CA","tsM.n.CS", "tsM.n.CF") #removed "M.n.CAxS"
turtlesoftAICTable.n <- cbind(tsModels, tsAICs)[order(tsAICs),]

#tsM.n.CAF is best fit
write.table(turtlesoftAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/turtlesoftAICTable.n.csv", sep = ",") 
#the best fit was the CAF but I used
#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1 - pchisq(summary(tsM.n.CF)$deviance, summary(tsM.n.CF)$df.residual)

#if they are ordered as we suspect.
tsCoeffTest <- summary(glht(tsM.n.CF, mcp(softdeath="Tukey")))
write.table(data.frame(names(tsCoeffTest$test$tstat),signif(tsCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/turtle-soft-tukey.csv", sep = ",")

#Using Kruskal Wallis
tskw <- kruskal.test(soft ~ softdeath, data = turtles, na.action = "na.exclude")
print(tskw)
tsposthoc_dunn <- dunn.test(turtles$soft, g = turtles$softdeath, method = "bonferroni")

###Turtles thread pieces#####
ttM.n.0 <- glm.nb(thread ~ 1, data = turtles, na.action = "na.exclude")
ttM.n.C <- glm.nb(thread ~ threaddeath, data = turtles, na.action = "na.exclude")
ttM.n.CA <- glm.nb(thread ~ threaddeath + Age, data = turtles, na.action = "na.exclude")
ttM.n.CAS <- glm.nb(thread ~ threaddeath + Age + Species, data = turtles, na.action = "na.exclude")
ttM.n.CS <- glm.nb(thread ~ threaddeath + Species, data = turtles, na.action = "na.exclude")
ttM.n.CAF <- glm.nb(thread ~ threaddeath + Age + Family, data = turtles, na.action = "na.exclude")
ttM.n.CF <- glm.nb(thread ~ threaddeath + Family, data = turtles, na.action = "na.exclude")
ttAICs <- round(c(ttM.n.0$aic, ttM.n.C$aic, ttM.n.CA$aic, ttM.n.CAS$aic, ttM.n.CS$aic, ttM.n.CAF$aic, ttM.n.CF$aic),1) #removed M.n.CAxS$aic
ttModels <- c("ttM.n.0","ttM.n.C","ttM.n.CA","ttM.n.CAS","ttM.n.CS","ttM.n.CAF", "ttM.n.CF") #removed "M.n.CAxS"
turtlethreadAICTable.n <- cbind(ttModels, ttAICs)[order(ttAICs),]

#ttM.n.CAF is best fit
write.table(turtlethreadAICTable.n, file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/turtlethreadAICTable.n.csv", sep = ",") 
#the best fit was the CAF but I used
#test for goodness of fit - if the p value returned is <0.05 then the model does not fit
1 - pchisq(summary(ttM.n.CAF)$deviance, summary(ttM.n.CAF)$df.residual)

#if they are ordered as we suspect.
ttCoeffTest <- summary(glht(ttM.n.CAF, mcp(threaddeath="Tukey")))
write.table(data.frame(names(ttCoeffTest$test$tstat),signif(ttCoeffTest$test$pvalues,2)), file = "C:/Users/erinm/OneDrive/Documents/Ocean Conservancy Post-Doc/Mortality modeling project/Mortality model/data/turtle-thread-tukey.csv", sep = ",")
#all significantly different

#Using Kruskal Wallis
ttkw <- kruskal.test(thread ~ threaddeath, data = turtles, na.action = "na.exclude")
print(ttkw)
ttposthoc_dunn <- dunn.test(turtles$thread, g = turtles$threaddeath, method = "bonferroni")

###Turtles hard volume#############
turtles$hard_volume <- as.numeric(turtles$hard_volume) 
tvhaov <- aov(hard_volume ~ harddeath, data = turtles, na.action = "na.exclude")
tvhkw <- kruskal.test(hard_volume ~ harddeath, data = turtles, na.action = "na.exclude")
summary(tvhaov)
print(tvhkw)
tvhposthoc_dunn <- dunn.test(turtles$hard_volume, g = turtles$harddeath, method = "bonferroni")

###Turtle soft volume#############
turtles$soft_volume <- as.numeric(turtles$soft_volume) 
tvsaov <- aov(soft_volume ~ softdeath, data = turtles, na.action = "na.exclude")
tvskw <- kruskal.test(soft_volume ~ softdeath, data = turtles, na.action = "na.exclude")
summary(tvsaov)
print(tvskw)
tvsposthoc_dunn <- dunn.test(turtles$soft_volume, g = turtles$softdeath, method = "bonferroni")

###Turtle thread volume#############
turtles$thread_volume <- as.numeric(turtles$thread_volume) 
tvtaov <- aov(thread_volume ~ threaddeath, data = turtles, na.action = "na.exclude")
tvtkw <- kruskal.test(thread_volume ~ threaddeath, data = turtles, na.action = "na.exclude")
summary(tvtaov)
print(tvtkw)
tvtposthoc_dunn <- dunn.test(turtles$thread_volume, g = turtles$threaddeath, method = "bonferroni")


######### Predictive models: Mammals Soft Plastic Pieces ###############
####First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

msChanceKD <- matrix(rep(case_when(mammals$softdeath == 'KD' ~ 1, 
                                   mammals$softdeath == 'KND' ~ 0, 
                                   mammals$softdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


msChanceKD <- ifelse(is.na(msChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     msChanceKD)

# run NumReps logit regressions and store the model results
ms_glms  <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft, family = "binomial"))
ms_glms2 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft + mammals$Age, family = "binomial"))
ms_glms3 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft + mammals$Family , family = "binomial"))
ms_glms4 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft + mammals$Age + mammals$Family, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
mssummaries <- lapply(ms_glms, summary)
mssummaries2 <- lapply(ms_glms2, summary)
mssummaries3 <- lapply(ms_glms3, summary)
mssummaries4 <- lapply(ms_glms4, summary)
# ...AICs
msaics <- unlist(sapply(mssummaries, function(x) c(aic = x$aic)))
msaics2 <- unlist(sapply(mssummaries2, function(x) c(aic = x$aic)))
msaics3 <- unlist(sapply(mssummaries3, function(x) c(aic = x$aic)))
msaics4 <- unlist(sapply(mssummaries4, function(x) c(aic = x$aic)))

mean(msaics)
mean(msaics2)
mean(msaics3)
mean(msaics4)
# AIC for model 4 is lowest by mean, but limited data makes it difficult to include family so using 2

# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 100
set.seed(1234)

msChanceKD <- matrix(rep(case_when(mammals$softdeath == 'KD' ~ 1, 
                                   mammals$softdeath == 'KND' ~ 0, 
                                   mammals$softdeath == 'PD' ~ 1), NumReps), ncol = NumReps, byrow = FALSE)

msChanceKD <- ifelse(is.na(msChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     msChanceKD)

# run NumReps logit regressions and store the model results
msglms2 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft + mammals$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
mssummaries2 <- lapply(msglms2, summary)

# ...coefficents with standard errors:
mscoefs2 <- unlist(lapply(mssummaries2, function(x) x$coefficients[2, 1]))
msses2 <- unlist(lapply(mssummaries2, function(x) x$coefficients[2, 2]))
msaics2 <- unlist(sapply(mssummaries2, function(x) c(aic = x$aic)))


msbeta0 <- median(unlist((lapply(mssummaries2, function(x) x$coefficients[1, 1])))) #intercept
msbeta1 <- median(unlist((lapply(mssummaries2, function(x) x$coefficients[2, 1])))) #soft
msbeta2 <- median(unlist((lapply(mssummaries2, function(x) x$coefficients[3, 1])))) #age:Infant
msbeta3 <- median(unlist((lapply(mssummaries2, function(x) x$coefficients[4, 1])))) #age:Juvenile
msbeta4 <- median(unlist((lapply(mssummaries2, function(x) x$coefficients[5, 1])))) #age:Subadult

msse0 <- median(unlist((lapply(mssummaries2, function(x) x$coefficients[1, 2])))) #intercept

msx2 <- mean(mammals$Age == "Infant", na.rm = TRUE)
msx3 <- mean(mammals$Age == "Juvenile", na.rm = TRUE)
msx4 <- mean(mammals$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
msprange <- seq(0, 20)
msExp <- exp(msbeta0 + msbeta1 * msprange + msbeta2 * msx2 + msbeta3 * msx3 + msbeta4 * msx4)#+ beta5 * x5 + beta6 * x6 + beta7 * x7 + beta8 * x8 + beta9 * x9 + beta10 * x10) 
msProb <- msExp/(1+msExp)
mammal_soft_prediction <- plot(msProb ~ msprange, type = "l", xlab = "Mammal Soft Plastic", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

######################################################
######################################################
#Predictive models: Mammals Soft Plastic Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

msChanceKD <- matrix(rep(case_when(mammals$softdeath == 'KD' ~ 1, 
                                   mammals$softdeath == 'KND' ~ 0, 
                                   mammals$softdeath == 'PD' ~ 1), NumReps), ncol = NumReps, byrow = FALSE)


msChanceKD <- ifelse(is.na(msChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     msChanceKD)0


# run NumReps logit regressions and store the model results
msv_glms  <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft_volume, family = "binomial"))
msv_glms2 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft_volume + mammals$Age, family = "binomial"))
msv_glms3 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft_volume + mammals$Family , family = "binomial"))
msv_glms4 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft_volume + mammals$Age + mammals$Family, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
msvsummaries <- lapply(msv_glms, summary)
msvsummaries2 <- lapply(msv_glms2, summary)
msvsummaries3 <- lapply(msv_glms3, summary)
msvsummaries4 <- lapply(msv_glms4, summary)
# ...AICs
msvaics <- unlist(sapply(msvsummaries, function(x) c(aic = x$aic)))
msvaics2 <- unlist(sapply(msvsummaries2, function(x) c(aic = x$aic)))
msvaics3 <- unlist(sapply(msvsummaries3, function(x) c(aic = x$aic)))
msvaics4 <- unlist(sapply(msvsummaries4, function(x) c(aic = x$aic)))

mean(msvaics)
mean(msvaics2)
mean(msvaics3)
mean(msvaics4)
# AIC for model 4 is lowest by mean (and median) but there are few samples by family so using 2 which had close mean


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 100
set.seed(1234)

msChanceKD <- matrix(rep(case_when(mammals$softdeath == 'KD' ~ 1, 
                                   mammals$softdeath == 'KND' ~ 0, 
                                   mammals$softdeath == 'PD' ~ 1), NumReps), ncol = NumReps, byrow = FALSE)


msChanceKD <- ifelse(is.na(msChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     msChanceKD)


# run NumReps logit regressions and store the model results
msvglms2 <- lapply(1:NumReps, function(x) glm(msChanceKD[,x] ~ mammals$soft_volume + mammals$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
msvsummaries2 <- lapply(msvglms2, summary)

# ...coefficents with standard errors:
msvcoefs <- unlist(lapply(msvsummaries2, function(x) x$coefficients[2, 1]))
msvses <- unlist(lapply(msvsummaries2, function(x) x$coefficients[2, 2]))
msvaics <- unlist(sapply(msvsummaries2, function(x) c(aic = x$aic)))

msvbeta0 <- median(unlist((lapply(msvsummaries2, function(x) x$coefficients[1, 1])))) #intercept
msvbeta1 <- median(unlist((lapply(msvsummaries2, function(x) x$coefficients[2, 1])))) #soft
msvbeta2 <- median(unlist((lapply(msvsummaries2, function(x) x$coefficients[3, 1])))) #age:Infant
msvbeta3 <- median(unlist((lapply(msvsummaries2, function(x) x$coefficients[4, 1])))) #age:Juvenile
msvbeta4 <- median(unlist((lapply(msvsummaries2, function(x) x$coefficients[5, 1])))) #age:Subadult

msvse0 <- median(unlist((lapply(msvsummaries2, function(x) x$coefficients[1, 2])))) #intercept

msvx2 <- mean(mammals$Age == "Infant", na.rm = TRUE)
msvx3 <- mean(mammals$Age == "Juvenile", na.rm = TRUE)
msvx4 <- mean(mammals$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
msvprange <- seq(200, 500)
msvExp <- exp(msvbeta0 + msvbeta1 * msvprange + msvbeta2 * msvx2 + msvbeta3 * msvx3 + msvbeta4 * msvx4) 
msvProb <- msvExp/(1+msvExp)
mammal_softvolume_plastic <- plot(msvProb ~ msvprange, type = "l", xlab = "Mammal Soft Volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

##########################################################################
##########################################################################
#Predictive models: Mammals Thread Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

mtChanceKD <- matrix(rep(case_when(mammals$threaddeath == 'KD' ~ 1, 
                                   mammals$threaddeath == 'KND' ~ 0, 
                                   mammals$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


mtChanceKD <- ifelse(is.na(mtChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     mtChanceKD)


# run NumReps logit regressions and store the model results
mt_glms  <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread, family = "binomial"))
mt_glms2 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread + mammals$Age, family = "binomial"))
mt_glms3 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread + mammals$Family , family = "binomial"))
mt_glms4 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread + mammals$Age + mammals$Family, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
mtsummaries <- lapply(mt_glms, summary)
mtsummaries2 <- lapply(mt_glms2, summary)
mtsummaries3 <- lapply(mt_glms3, summary)
mtsummaries4 <- lapply(mt_glms4, summary)
# ...AICs
mtaics <- unlist(sapply(mtsummaries, function(x) c(aic = x$aic)))
mtaics2 <- unlist(sapply(mtsummaries2, function(x) c(aic = x$aic)))
mtaics3 <- unlist(sapply(mtsummaries3, function(x) c(aic = x$aic)))
mtaics4 <- unlist(sapply(mtsummaries4, function(x) c(aic = x$aic)))

mean(mtaics)
mean(mtaics2)
mean(mtaics3)
mean(mtaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one

# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

mtChanceKD <- matrix(rep(case_when(mammals$threaddeath == 'KD' ~ 1, 
                                   mammals$threaddeath == 'KND' ~ 0, 
                                   mammals$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


mtChanceKD <- ifelse(is.na(mtChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     mtChanceKD)


# run NumReps logit regressions and store the model results
mtglms2 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread + mammals$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
mtsummaries2 <- lapply(mtglms2, summary)

# ...coefficents with standard errors:
mtcoefs <- unlist(lapply(mtsummaries2, function(x) x$coefficients[2, 1]))
mtses <- unlist(lapply(mtsummaries2, function(x) x$coefficients[2, 2]))
mtaics <- unlist(sapply(mtsummaries2, function(x) c(aic = x$aic)))


mtbeta0 <- median(unlist((lapply(mtsummaries2, function(x) x$coefficients[1, 1])))) #intercept
mtbeta1 <- median(unlist((lapply(mtsummaries2, function(x) x$coefficients[2, 1])))) #soft
mtbeta2 <- median(unlist((lapply(mtsummaries2, function(x) x$coefficients[3, 1])))) #age:Infant
mtbeta3 <- median(unlist((lapply(mtsummaries2, function(x) x$coefficients[4, 1])))) #age:Juvenile
mtbeta4 <- median(unlist((lapply(mtsummaries2, function(x) x$coefficients[5, 1])))) #age:Subadult

mtse0 <- median(unlist((lapply(mtsummaries2, function(x) x$coefficients[1, 2])))) #intercept

mtx2 <- mean(mammals$Age == "Infant", na.rm = TRUE)
mtx3 <- mean(mammals$Age == "Juvenile", na.rm = TRUE)
mtx4 <- mean(mammals$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
mtprange <- seq(0, 50)
mtExp <- exp(mtbeta0 + mtbeta1 * mtprange + mtbeta2 * mtx2 + mtbeta3 * mtx3 + mtbeta4 * mtx4) 
mtProb <- mtExp/(1+mtExp)
mammal_threads_pieces <- plot(mtProb ~ mtprange, type = "l", xlab = "Mammal Thread Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

##############################################
###############################################
#Predictive models: Mammals Thread Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

mtChanceKD <- matrix(rep(case_when(mammals$threaddeath == 'KD' ~ 1, 
                                   mammals$threaddeath == 'KND' ~ 0, 
                                   mammals$threaddeath == 'PD' ~ 1), NumReps), ncol = NumReps, byrow = FALSE)


mtChanceKD <- ifelse(is.na(mtChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     mtChanceKD)


# run NumReps logit regressions and store the model results
mtv_glms  <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread_volume, family = "binomial"))
mtv_glms2 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread_volume + mammals$Age, family = "binomial"))
mtv_glms3 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread_volume + mammals$Family , family = "binomial"))
mtv_glms4 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread_volume + mammals$Age + mammals$Family, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
mtvsummaries <- lapply(mtv_glms, summary)
mtvsummaries2 <- lapply(mtv_glms2, summary)
mtvsummaries3 <- lapply(mtv_glms3, summary)
mtvsummaries4 <- lapply(mtv_glms4, summary)

# ...AICs
mtvaics <- unlist(sapply(mtvsummaries, function(x) c(aic = x$aic)))
mtvaics2 <- unlist(sapply(mtvsummaries2, function(x) c(aic = x$aic)))
mtvaics3 <- unlist(sapply(mtvsummaries3, function(x) c(aic = x$aic)))
mtvaics4 <- unlist(sapply(mtvsummaries4, function(x) c(aic = x$aic)))

mean(mtvaics)
mean(mtvaics2)
mean(mtvaics3)
mean(mtvaics4)
# AIC for model 4 is lowest by mean (and median) but using 2 b/c of limited number per family


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

mtChanceKD <- matrix(rep(case_when(mammals$threaddeath == 'KD' ~ 1, 
                                   mammals$threaddeath == 'KND' ~ 0, 
                                   mammals$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


mtChanceKD <- ifelse(is.na(mtChanceKD), 
                     matrix(round(runif(nrow(mammals)*NumReps)), nrow = nrow(mammals), ncol = NumReps), 
                     mtChanceKD)


# run NumReps logit regressions and store the model results
mtvglms2 <- lapply(1:NumReps, function(x) glm(mtChanceKD[,x] ~ mammals$thread_volume + mammals$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
mtvsummariesX <- lapply(mtvglms2, summary)

# ...coefficents with standard errors:
mtvcoefs <- unlist(lapply(mtvsummariesX, function(x) x$coefficients[2, 1]))
mtvses <- unlist(lapply(mtvsummariesX, function(x) x$coefficients[2, 2]))
mtvaics <- unlist(sapply(mtvsummariesX, function(x) c(aic = x$aic)))


mtvbeta0 <- median(unlist((lapply(mtvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
mtvbeta1 <- median(unlist((lapply(mtvsummariesX, function(x) x$coefficients[2, 1])))) #soft
mtvbeta2 <- median(unlist((lapply(mtvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
mtvbeta3 <- median(unlist((lapply(mtvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
mtvbeta4 <- median(unlist((lapply(mtvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

mtvse0 <- median(unlist((lapply(mtvsummariesX, function(x) x$coefficients[1, 2])))) #intercept

mtvx2 <- mean(mammals$Age == "Infant", na.rm = TRUE)
mtvx3 <- mean(mammals$Age == "Juvenile", na.rm = TRUE)
mtvx4 <- mean(mammals$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
mtvprange <- seq(0, 80)
mtvExp <- exp(mtvbeta0 + mtvbeta1 * mtvprange + mtvbeta2 * mtvx2 + mtvbeta3 * mtvx3 + mtvbeta4 * mtvx4)
mtvProb <- mtvExp/(1+mtvExp)
mammal_thread_volume <- plot(mtvProb ~ mtvprange, type = "l", xlab = "Mammal Thread volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

####################################################################################################################################################
####################################################################################################################################################
#Predictive models: Bird Hard Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

bhChanceKD <- matrix(rep(case_when(birds$harddeath == 'KD' ~ 1, 
                                   birds$harddeath == 'KND' ~ 0, 
                                   birds$harddeath == 'Ind' ~ NA,
                                   birds$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


bhChanceKD <- ifelse(is.na(bhChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     bhChanceKD)


# run NumReps logit regressions and store the model results
bh_glms  <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard, family = "binomial"))
bh_glms2 <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard + birds$Age, family = "binomial"))
bh_glms3 <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard + birds$Group , family = "binomial"))
bh_glms4 <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard + birds$Age + birds$Group, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
bhsummaries <- lapply(bh_glms, summary)
bhsummaries2 <- lapply(bh_glms2, summary)
bhsummaries3 <- lapply(bh_glms3, summary)
bhsummaries4 <- lapply(bh_glms4, summary)
# ...AICs
bhaics <- unlist(sapply(bhsummaries, function(x) c(aic = x$aic)))
bhaics2 <- unlist(sapply(bhsummaries2, function(x) c(aic = x$aic)))
bhaics3 <- unlist(sapply(bhsummaries3, function(x) c(aic = x$aic)))
bhaics4 <- unlist(sapply(bhsummaries4, function(x) c(aic = x$aic)))

mean(bhaics)
mean(bhaics2)
mean(bhaics3)
mean(bhaics4)
# AIC for model 4 is lowest by mean (and median) but the SE is bad so I just did the plain model


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

bhChanceKD <- matrix(rep(case_when(birds$harddeath == 'KD' ~ 1, 
                                   birds$harddeath == 'KND' ~ 0,
                                   birds$harddeath == 'Ind' ~ NA,
                                   birds$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


bhChanceKD <- ifelse(is.na(bhChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     bhChanceKD)


# run NumReps logit regressions and store the model results
bhglms <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
bhsummariesX <- lapply(bhglms, summary)

# ...coefficents with standard errors:
bhcoefs <- unlist(lapply(bhsummariesX, function(x) x$coefficients[2, 1]))
bhses <- unlist(lapply(bhsummariesX, function(x) x$coefficients[2, 2]))
bhaics <- unlist(sapply(bhsummariesX, function(x) c(aic = x$aic)))


bhbeta0 <- median(unlist((lapply(bhsummariesX, function(x) x$coefficients[1, 1])))) #intercept
bhbeta1 <- median(unlist((lapply(bhsummariesX, function(x) x$coefficients[2, 1])))) #soft
#bhbeta2 <- median(unlist((lapply(bhsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
#bhbeta3 <- median(unlist((lapply(bhsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
#bhbeta4 <- median(unlist((lapply(bhsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

bhse0 <- median(unlist((lapply(bhsummariesX, function(x) x$coefficients[1, 2])))) #intercept

#bhx2 <- mean(birds$Age == "Infant", na.rm = TRUE)
#bhx3 <- mean(birds$Age == "Juvenile", na.rm = TRUE)
#bhx4 <- mean(birds$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
bhprange <- seq(0, 40)
bhExp <- exp(bhbeta0 + bhbeta1 * bhprange)
bhProb <- bhExp/(1+bhExp)
bird_hard_pieces <- plot(bhProb ~ bhprange, type = "l", xlab = "Bird Hard", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

########################################################################################################################################
#########################################################################################################################################
#Predictive models: Bird Hard Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

bhChanceKD <- matrix(rep(case_when(birds$harddeath == 'KD' ~ 1, 
                                   birds$harddeath == 'KND' ~ 0,
                                   birds$harddeath == 'Ind' ~ NA,
                                   birds$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


bhChanceKD <- ifelse(is.na(bhChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     bhChanceKD)


# run NumReps logit regressions and store the model results
bhv_glms  <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard_volume, family = "binomial"))
bhv_glms2 <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard_volume + birds$Age, family = "binomial"))
bhv_glms3 <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard_volume + birds$Group , family = "binomial"))
bhv_glms4 <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard_volume + birds$Age + birds$Group, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
bhvsummaries <- lapply(bhv_glms, summary)
bhvsummaries2 <- lapply(bhv_glms2, summary)
bhvsummaries3 <- lapply(bhv_glms3, summary)
bhvsummaries4 <- lapply(bhv_glms4, summary)
# ...AICs
bhvaics <- unlist(sapply(bhvsummaries, function(x) c(aic = x$aic)))
bhvaics2 <- unlist(sapply(bhvsummaries2, function(x) c(aic = x$aic)))
bhvaics3 <- unlist(sapply(bhvsummaries3, function(x) c(aic = x$aic)))
bhvaics4 <- unlist(sapply(bhvsummaries4, function(x) c(aic = x$aic)))

mean(bhvaics)
mean(bhvaics2)
mean(bhvaics3)
mean(bhvaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one

# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

bhChanceKD <- matrix(rep(case_when(birds$harddeath == 'KD' ~ 1, 
                                   birds$harddeath == 'KND' ~ 0, 
                                   birds$harddeath == 'Ind' ~ NA,
                                   birds$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)

bhChanceKD <- ifelse(is.na(bhChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     bhChanceKD)

# run NumReps logit regressions and store the model results
bhvglmsX <- lapply(1:NumReps, function(x) glm(bhChanceKD[,x] ~ birds$hard_volume, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
bhvsummariesX <- lapply(bhvglmsX, summary)

# ...coefficents with standard errors:
bhvcoefs <- unlist(lapply(bhvsummariesX, function(x) x$coefficients[2, 1]))
bhvses <- unlist(lapply(bhvsummariesX, function(x) x$coefficients[2, 2]))
bhvaics <- unlist(sapply(bhvsummariesX, function(x) c(aic = x$aic)))


bhvbeta0 <- median(unlist((lapply(bhvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
bhvbeta1 <- median(unlist((lapply(bhvsummariesX, function(x) x$coefficients[2, 1])))) #soft
bhvbeta2 <- median(unlist((lapply(bhvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
bhvbeta3 <- median(unlist((lapply(bhvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
bhvbeta4 <- median(unlist((lapply(bhvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

bhvse0 <- median(unlist((lapply(bhvsummariesX, function(x) x$coefficients[1, 2])))) #intercept

bhvx2 <- mean(birds$Age == "Infant", na.rm = TRUE)
bhvx3 <- mean(birds$Age == "Juvenile", na.rm = TRUE)
bhvx4 <- mean(birds$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
bhvprange <- seq(0, 1500)
bhvExp <- exp(bhvbeta0 + bhvbeta1 * bhvprange) # + bhvbeta2 * bhvx2 + bhvbeta3 * bhvx3 + bhvbeta4 * bhvx4)
bhvProb <- bhvExp/(1+bhvExp)
bird_hard_volume <- plot(bhvProb ~ bhvprange, type = "l", xlab = "Bird hard volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

#############################################################################################################################
#############################################################################################################################
#Predictive models: Bird thread Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

btChanceKD <- matrix(rep(case_when(birds$threaddeath == 'KD' ~ 1, 
                                   birds$threaddeath == 'KND' ~ 0, 
                                   birds$threaddeath == 'Ind' ~ NA,
                                   birds$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


btChanceKD <- ifelse(is.na(btChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     btChanceKD)


# run NumReps logit regressions and store the model results
bt_glms  <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread, family = "binomial"))
bt_glms2 <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread + birds$Age, family = "binomial"))
bt_glms3 <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread + birds$Group , family = "binomial"))
bt_glms4 <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread + birds$Age + birds$Group, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
btsummaries <- lapply(bt_glms, summary)
btsummaries2 <- lapply(bt_glms2, summary)
btsummaries3 <- lapply(bt_glms3, summary)
btsummaries4 <- lapply(bt_glms4, summary)
# ...AICs
btaics <- unlist(sapply(btsummaries, function(x) c(aic = x$aic)))
btaics2 <- unlist(sapply(btsummaries2, function(x) c(aic = x$aic)))
btaics3 <- unlist(sapply(btsummaries3, function(x) c(aic = x$aic)))
btaics4 <- unlist(sapply(btsummaries4, function(x) c(aic = x$aic)))

mean(btaics)
mean(btaics2)
mean(btaics3)
mean(btaics4)
# AIC for model 2 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

btChanceKD <- matrix(rep(case_when(birds$threaddeath == 'KD' ~ 1, 
                                   birds$threaddeath == 'KND' ~ 0, 
                                   birds$threaddeath == 'Ind' ~ NA,
                                   birds$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


btChanceKD <- ifelse(is.na(btChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     btChanceKD)


# run NumReps logit regressions and store the model results
btglmsX <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread + birds$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
btsummariesX <- lapply(btglmsX, summary)

# ...coefficents with standard errors:
btcoefs <- unlist(lapply(btsummariesX, function(x) x$coefficients[2, 1]))
btses <- unlist(lapply(btsummariesX, function(x) x$coefficients[2, 2]))
btaics <- unlist(sapply(btsummariesX, function(x) c(aic = x$aic)))


btbeta0 <- median(unlist((lapply(btsummariesX, function(x) x$coefficients[1, 1])))) #intercept
btbeta1 <- median(unlist((lapply(btsummariesX, function(x) x$coefficients[2, 1])))) #soft
btbeta2 <- median(unlist((lapply(btsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
btbeta3 <- median(unlist((lapply(btsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
btbeta4 <- median(unlist((lapply(btsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

btse0 <- median(unlist((lapply(btsummariesX, function(x) x$coefficients[1, 2])))) #intercept

btx2 <- mean(birds$Age == "Infant", na.rm = TRUE)
btx3 <- mean(birds$Age == "Juvenile", na.rm = TRUE)
btx4 <- mean(birds$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
btprange <- seq(0, 30)
btExp <- exp(btbeta0 + btbeta1 * btprange + btbeta2 * btx2 + btbeta3 * btx3 + btbeta4 * btx4)
btProb <- btExp/(1+btExp)
bird_thread_pieces <- plot(btProb ~ btprange, type = "l", xlab = "Bird Thread Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

##############################################
###############################################
#Predictive models: Bird Thread Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

btChanceKD <- matrix(rep(case_when(birds$threaddeath == 'KD' ~ 1, 
                                   birds$threaddeath == 'KND' ~ 0, 
                                   birds$threaddeath == 'Ind' ~ NA,
                                   birds$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


btChanceKD <- ifelse(is.na(btChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     btChanceKD)


# run NumReps logit regressions and store the model results
btv_glms  <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread_volume, family = "binomial"))
btv_glms2 <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread_volume + birds$Age, family = "binomial"))
btv_glms3 <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread_volume + birds$Group , family = "binomial"))
btv_glms4 <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$thread_volume + birds$Age + birds$Group, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
btvsummaries <- lapply(btv_glms, summary)
btvsummaries2 <- lapply(btv_glms2, summary)
btvsummaries3 <- lapply(btv_glms3, summary)
btvsummaries4 <- lapply(btv_glms4, summary)
# ...AICs
btvaics <- unlist(sapply(btvsummaries, function(x) c(aic = x$aic)))
btvaics2 <- unlist(sapply(btvsummaries2, function(x) c(aic = x$aic)))
btvaics3 <- unlist(sapply(btvsummaries3, function(x) c(aic = x$aic)))
btvaics4 <- unlist(sapply(btvsummaries4, function(x) c(aic = x$aic)))

mean(btvaics)
mean(btvaics2)
mean(btvaics3)
mean(btvaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

btChanceKD <- matrix(rep(case_when(birds$threaddeath == 'KD' ~ 1, 
                                   birds$threaddeath == 'KND' ~ 0, 
                                   birds$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


btChanceKD <- ifelse(is.na(btChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     btChanceKD)

# run NumReps logit regressions and store the model results
btvglmsX <- lapply(1:NumReps, function(x) glm(btChanceKD[,x] ~ birds$threads_volume, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
btvsummariesX <- lapply(btvglmsX, summary)

# ...coefficents with standard errors:
btvcoefs <- unlist(lapply(btvsummariesX, function(x) x$coefficients[2, 1]))
btvses <- unlist(lapply(btvsummariesX, function(x) x$coefficients[2, 2]))
btvaics <- unlist(sapply(btvsummariesX, function(x) c(aic = x$aic)))


btvbeta0 <- median(unlist((lapply(btvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
btvbeta1 <- median(unlist((lapply(btvsummariesX, function(x) x$coefficients[2, 1])))) #soft
btvbeta2 <- median(unlist((lapply(btvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
btvbeta3 <- median(unlist((lapply(btvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
btvbeta4 <- median(unlist((lapply(btvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

btvse0 <- median(unlist((lapply(btvsummariesX, function(x) x$coefficients[1, 2])))) #intercept

btvx2 <- mean(birds$Age == "Infant", na.rm = TRUE)
btvx3 <- mean(birds$Age == "Juvenile", na.rm = TRUE)
btvx4 <- mean(birds$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
btvprange <- seq(0, 50)
btvExp <- exp(btvbeta0 + btvbeta1 * btvprange + btvbeta2 * btvx2 + btvbeta3 * btvx3 + btvbeta4 * btvx4)
btvProb <- btvExp/(1+btvExp)
bird_thread_volume <- plot(btvProb ~ btvprange, type = "l", xlab = "Bird thread volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

#############################################################################################################################
#############################################################################################################################
#Predictive models: Bird Rubber Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

brChanceKD <- matrix(rep(case_when(birds$rubberdeath == 'KD' ~ 1, 
                                   birds$rubberdeath == 'KND' ~ 0, 
                                   birds$rubberdeath == 'Ind' ~ NA,
                                   birds$rubberdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


brChanceKD <- ifelse(is.na(brChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     brChanceKD)


# run NumReps logit regressions and store the model results
br_glms  <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber, family = "binomial"))
br_glms2 <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber + birds$Age, family = "binomial"))
br_glms3 <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber + birds$Family , family = "binomial"))
br_glms4 <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber + birds$Age + birds$Family, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
brsummaries <- lapply(br_glms, summary)
brsummaries2 <- lapply(br_glms2, summary)
brsummaries3 <- lapply(br_glms3, summary)
brsummaries4 <- lapply(br_glms4, summary)
# ...AICs
braics <- unlist(sapply(brsummaries, function(x) c(aic = x$aic)))
braics2 <- unlist(sapply(brsummaries2, function(x) c(aic = x$aic)))
braics3 <- unlist(sapply(brsummaries3, function(x) c(aic = x$aic)))
braics4 <- unlist(sapply(brsummaries4, function(x) c(aic = x$aic)))

mean(braics)
mean(braics2)
mean(braics3)
mean(braics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

brChanceKD <- matrix(rep(case_when(birds$rubberdeath == 'KD' ~ 1, 
                                   birds$rubberdeath == 'KND' ~ 0, 
                                   birds$rubberdeath == 'Ind' ~ NA,
                                   birds$rubberdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


brChanceKD <- ifelse(is.na(brChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     brChanceKD)


# run NumReps logit regressions and store the model results
brglmsX <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
brsummariesX <- lapply(brglmsX, summary)

# ...coefficents with standard errors:
brcoefs <- unlist(lapply(brsummariesX, function(x) x$coefficients[2, 1]))
brses <- unlist(lapply(brsummariesX, function(x) x$coefficients[2, 2]))
braics <- unlist(sapply(brsummariesX, function(x) c(aic = x$aic)))


brbeta0 <- median(unlist((lapply(brsummariesX, function(x) x$coefficients[1, 1])))) #intercept
brbeta1 <- median(unlist((lapply(brsummariesX, function(x) x$coefficients[2, 1])))) #soft
brbeta2 <- median(unlist((lapply(brsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
brbeta3 <- median(unlist((lapply(brsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
brbeta4 <- median(unlist((lapply(brsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

brse0 <- median(unlist((lapply(brsummariesX, function(x) x$coefficients[1, 2])))) #intercept

brx2 <- mean(birds$Age == "Infant", na.rm = TRUE)
brx3 <- mean(birds$Age == "Juvenile", na.rm = TRUE)
brx4 <- mean(birds$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
brprange <- seq(0, 30)
brExp <- exp(brbeta0 + brbeta1 * brprange) # + brbeta2 * brx2 + brbeta3 * brx3 + brbeta4 * brx4)
brProb <- brExp/(1+brExp)
bird_rubber_pieces <- plot(brProb ~ brprange, type = "l", xlab = "Bird rubber Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

##############################################
###############################################
#Predictive models: Bird rubber Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

brChanceKD <- matrix(rep(case_when(birds$rubberdeath == 'KD' ~ 1, 
                                   birds$rubberdeath == 'KND' ~ 0, 
                                   birds$rubberdeath == 'Ind' ~ NA,
                                   birds$rubberdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


brChanceKD <- ifelse(is.na(brChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     brChanceKD)


# run NumReps logit regressions and store the model results
brv_glms  <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber_volume, family = "binomial"))
brv_glms2 <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber_volume + birds$Age, family = "binomial"))
brv_glms3 <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber_volume + birds$Group , family = "binomial"))
brv_glms4 <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber_volume + birds$Age + birds$Group, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
brvsummaries <- lapply(brv_glms, summary)
brvsummaries2 <- lapply(brv_glms2, summary)
brvsummaries3 <- lapply(brv_glms3, summary)
brvsummaries4 <- lapply(brv_glms4, summary)
# ...AICs
brvaics <- unlist(sapply(brvsummaries, function(x) c(aic = x$aic)))
brvaics2 <- unlist(sapply(brvsummaries2, function(x) c(aic = x$aic)))
brvaics3 <- unlist(sapply(brvsummaries3, function(x) c(aic = x$aic)))
brvaics4 <- unlist(sapply(brvsummaries4, function(x) c(aic = x$aic)))

mean(brvaics)
mean(brvaics2)
mean(brvaics3)
mean(brvaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

brChanceKD <- matrix(rep(case_when(birds$rubberdeath == 'KD' ~ 1, 
                                   birds$rubberdeath == 'KND' ~ 0, 
                                   birds$rubberdeath == 'Ind' ~ NA,
                                   birds$rubberdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


brChanceKD <- ifelse(is.na(brChanceKD), 
                     matrix(round(runif(nrow(birds)*NumReps)), nrow = nrow(birds), ncol = NumReps), 
                     brChanceKD)

# run NumReps logit regressions and store the model results
brvglmsX <- lapply(1:NumReps, function(x) glm(brChanceKD[,x] ~ birds$rubber_volume, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
brvsummariesX <- lapply(brvglmsX, summary)

# ...coefficents with standard errors:
brvcoefs <- unlist(lapply(brvsummariesX, function(x) x$coefficients[2, 1]))
brvses <- unlist(lapply(brvsummariesX, function(x) x$coefficients[2, 2]))
brvaics <- unlist(sapply(brvsummariesX, function(x) c(aic = x$aic)))


brvbeta0 <- median(unlist((lapply(brvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
brvbeta1 <- median(unlist((lapply(brvsummariesX, function(x) x$coefficients[2, 1])))) #soft
brvbeta2 <- median(unlist((lapply(brvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
brvbeta3 <- median(unlist((lapply(brvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
brvbeta4 <- median(unlist((lapply(brvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

brvse0 <- median(unlist((lapply(brvsummariesX, function(x) x$coefficients[1, 2])))) #intercept

brvx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
brvx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
brvx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
brvprange <- seq(0, 200)
brvExp <- exp(brvbeta0 + brvbeta1 * brvprange) # + brvbeta2 * brvx2 + brvbeta3 * brvx3 + brvbeta4 * brvx4)
brvProb <- brvExp/(1+brvExp)
bird_rubber_volume <- plot(brvProb ~ brvprange, type = "l", xlab = "Bird rubber volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

#############################################################################################################################
#############################################################################################################################
#Predictive models: Turtle Soft Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

tsChanceKD <- matrix(rep(case_when(turtles$softdeath == 'KD' ~ 1, 
                                   turtles$softdeath == 'KND' ~ 0, 
                                   turtles$softdeath == 'Ind' ~ NA,
                                   turtles$softdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


tsChanceKD <- ifelse(is.na(tsChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     tsChanceKD)


# run NumReps logit regressions and store the model results
ts_glms  <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft, family = "binomial"))
ts_glms2 <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft + turtles$Age, family = "binomial"))
ts_glms3 <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft + turtles$Species, family = "binomial"))
#ts_glms4 <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft + turtles$Age + turtles$Family, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
tssummaries <- lapply(ts_glms, summary)
tssummaries2 <- lapply(ts_glms2, summary)
tssummaries3 <- lapply(ts_glms3, summary)
#tssummaries4 <- lapply(ts_glms4, summary)
# ...AICs
tsaics <- unlist(sapply(tssummaries, function(x) c(aic = x$aic)))
tsaics2 <- unlist(sapply(tssummaries2, function(x) c(aic = x$aic)))
tsaics3 <- unlist(sapply(tssummaries3, function(x) c(aic = x$aic)))
#tsaics4 <- unlist(sapply(tssummaries4, function(x) c(aic = x$aic)))

mean(tsaics)
mean(tsaics2)
mean(tsaics3)
#mean(tsaics4)
# AIC for model 2 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 100
set.seed(1234)

tsChanceKD <- matrix(rep(case_when(turtles$softdeath == 'KD' ~ 1, 
                                   turtles$softdeath == 'KND' ~ 0, 
                                   turtles$softdeath == 'Ind' ~ NA,
                                   turtles$softdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


tsChanceKD <- ifelse(is.na(tsChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     tsChanceKD)


# run NumReps logit regressions and store the model results
tsglmsX <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft + turtles$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
tssummariesX <- lapply(tsglmsX, summary)

# ...coefficents with standard errors:
tscoefs <- unlist(lapply(tssummariesX, function(x) x$coefficients[2, 1]))
tsses <- unlist(lapply(tssummariesX, function(x) x$coefficients[2, 2]))
tsaics <- unlist(sapply(tssummariesX, function(x) c(aic = x$aic)))


tsbeta0 <- median(unlist((lapply(tssummariesX, function(x) x$coefficients[1, 1])))) #intercept
tsbeta1 <- median(unlist((lapply(tssummariesX, function(x) x$coefficients[2, 1])))) #soft
tsbeta2 <- median(unlist((lapply(tssummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
tsbeta3 <- median(unlist((lapply(tssummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
tsbeta4 <- median(unlist((lapply(tssummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

tsse0 <- median(unlist((lapply(tssummariesX, function(x) x$coefficients[1, 2])))) #intercept

tsx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
tsx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
tsx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
tsprange <- seq(0, 100)
tsExp <- exp(tsbeta0 + tsbeta1 * tsprange) # + tsbeta2 * tsx2 + tsbeta3 * tsx3 + tsbeta4 * tsx4)
tsProb <- tsExp/(1+tsExp)
turtle_soft_pieces <- plot(tsProb ~ tsprange, type = "l", xlab = "Turtle soft Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

##############################################
###############################################
#Predictive models: Turtle Soft Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

tsChanceKD <- matrix(rep(case_when(turtles$softdeath == 'KD' ~ 1, 
                                   turtles$softdeath == 'KND' ~ 0, 
                                   turtles$softdeath == 'Ind' ~ NA,
                                   turtles$softdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


tsChanceKD <- ifelse(is.na(tsChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     tsChanceKD)


# run NumReps logit regressions and store the model results
tsv_glms  <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft_volume, family = "binomial"))
tsv_glms2 <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft_volume + turtles$Age, family = "binomial"))
tsv_glms3 <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft_volume + turtles$Species , family = "binomial"))
tsv_glms4 <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft_volume + turtles$Age + turtles$Species, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
tsvsummaries <- lapply(tsv_glms, summary)
tsvsummaries2 <- lapply(tsv_glms2, summary)
tsvsummaries3 <- lapply(tsv_glms3, summary)
tsvsummaries4 <- lapply(tsv_glms4, summary)
# ...AICs
tsvaics <- unlist(sapply(tsvsummaries, function(x) c(aic = x$aic)))
tsvaics2 <- unlist(sapply(tsvsummaries2, function(x) c(aic = x$aic)))
tsvaics3 <- unlist(sapply(tsvsummaries3, function(x) c(aic = x$aic)))
tsvaics4 <- unlist(sapply(tsvsummaries4, function(x) c(aic = x$aic)))

mean(tsvaics)
mean(tsvaics2)
mean(tsvaics3)
mean(tsvaics4)
# AIC for model 1 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 100
set.seed(1234)

tsChanceKD <- matrix(rep(case_when(turtles$softdeath == 'KD' ~ 1, 
                                   turtles$softdeath == 'KND' ~ 0,
                                   turtles$softdeath == 'Ind' ~ NA,
                                   turtles$softdeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


tsChanceKD <- ifelse(is.na(tsChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     tsChanceKD)

# run NumReps logit regressions and store the model results
tsvglmsX <- lapply(1:NumReps, function(x) glm(tsChanceKD[,x] ~ turtles$soft, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
tsvsummariesX <- lapply(tsvglmsX, summary)

# ...coefficents with standard errors:
tsvcoefs <- unlist(lapply(tsvsummariesX, function(x) x$coefficients[2, 1]))
tsvses <- unlist(lapply(tsvsummariesX, function(x) x$coefficients[2, 2]))
tsvaics <- unlist(sapply(tsvsummariesX, function(x) c(aic = x$aic)))


tsvbeta0 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
tsvbeta1 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[2, 1])))) #soft
tsvbeta2 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
tsvbeta3 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
tsvbeta4 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

tsvse0 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[1, 2])))) #intercept
tsvse1 <- median(unlist((lapply(tsvsummariesX, function(x) x$coefficients[2, 2])))) #error

tsvx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
tsvx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
tsvx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by soft plastic with median coefficient estimates from MC and mean values for variables
tsvprange <- seq(0, 800)
tsvExp <- exp(tsvbeta0 + tsvbeta1 * tsvprange) # + tsvbeta2 * tsvx2 + tsvbeta3 * tsvx3 + tsvbeta4 * tsvx4)
tsvExp1 <-exp(tsvbeta0 + (tsvbeta1 + tsvse1)*thprange + thbeta2 * thx2 + thbeta3 * thx3 + thbeta4 * thx4)
tsvExp2 <- exp(tsvbeta0 + (tsvbeta1 - tsvse1)*thprange + thbeta2 * thx2 + thbeta3 * thx3 + thbeta4 * thx4)
tsvProb <- tsvExp/(1+tsvExp)
tsvProb1 <-tsvExp1/(1+tsvExp1)
tsvProb2 <-tsvExp2/(1+tsvExp2)
turtles_soft_volume <- plot(tsvProb ~ tsvprange, type = "l", xlab = "Turtles Soft volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

#############################################################################################################################
#############################################################################################################################
#Predictive models: Turtle Thread Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

ttChanceKD <- matrix(rep(case_when(turtles$threaddeath == 'KD' ~ 1, 
                                   turtles$threaddeath == 'KND' ~ 0, 
                                   turtles$threaddeath == 'Ind' ~ NA,
                                   turtles$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


ttChanceKD <- ifelse(is.na(ttChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     ttChanceKD)


# run NumReps logit regressions and store the model results
tt_glms  <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread, family = "binomial"))
tt_glms2 <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread + turtles$Age, family = "binomial"))
tt_glms3 <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread + turtles$Species , family = "binomial"))
tt_glms4 <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread + turtles$Age + turtles$Species, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
ttsummaries <- lapply(tt_glms, summary)
ttsummaries2 <- lapply(tt_glms2, summary)
ttsummaries3 <- lapply(tt_glms3, summary)
ttsummaries4 <- lapply(tt_glms4, summary)
# ...AICs
ttaics <- unlist(sapply(ttsummaries, function(x) c(aic = x$aic)))
ttaics2 <- unlist(sapply(ttsummaries2, function(x) c(aic = x$aic)))
ttaics3 <- unlist(sapply(ttsummaries3, function(x) c(aic = x$aic)))
ttaics4 <- unlist(sapply(ttsummaries4, function(x) c(aic = x$aic)))

mean(ttaics)
mean(ttaics2)
mean(ttaics3)
mean(ttaics4)
# AIC for model 2 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

ttChanceKD <- matrix(rep(case_when(turtles$threaddeath == 'KD' ~ 1, 
                                   turtles$threaddeath == 'KND' ~ 0, 
                                   turtles$threaddeath == 'Ind' ~ NA,
                                   turtles$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


ttChanceKD <- ifelse(is.na(ttChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     ttChanceKD)


# run NumReps logit regressions and store the model results
ttglmsX <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread + turtles$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
ttsummariesX <- lapply(ttglmsX, summary)

# ...coefficents with standard errors:
ttcoefs <- unlist(lapply(ttsummariesX, function(x) x$coefficients[2, 1]))
ttses <- unlist(lapply(ttsummariesX, function(x) x$coefficients[2, 2]))
ttaics <- unlist(sapply(ttsummariesX, function(x) c(aic = x$aic)))


ttbeta0 <- median(unlist((lapply(ttsummariesX, function(x) x$coefficients[1, 1])))) #intercept
ttbeta1 <- median(unlist((lapply(ttsummariesX, function(x) x$coefficients[2, 1])))) #thread
ttbeta2 <- median(unlist((lapply(ttsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
ttbeta3 <- median(unlist((lapply(ttsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
ttbeta4 <- median(unlist((lapply(ttsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

ttse0 <- median(unlist((lapply(ttsummariesX, function(x) x$coefficients[1, 2])))) #intercept

ttx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
ttx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
ttx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by thread plastic with median coefficient estimates from MC and mean values for variables
ttprange <- seq(0, 500)
ttExp <- exp(ttbeta0 + ttbeta1 * ttprange + ttbeta2 * ttx2 + ttbeta3 * ttx3 + ttbeta4 * ttx4)
ttProb <- ttExp/(1+ttExp)
Turtle_thread_pieces <- plot(ttProb ~ ttprange, type = "l", xlab = "Turtle thread Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

##########################################################################################################################################
###########################################################################################################################################
#Predictive models: Turtle thread Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

ttChanceKD <- matrix(rep(case_when(turtles$threaddeath == 'KD' ~ 1, 
                                   turtles$threaddeath == 'KND' ~ 0, 
                                   turtles$threaddeath == 'Ind' ~ NA,
                                   turtles$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


ttChanceKD <- ifelse(is.na(ttChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     ttChanceKD)


# run NumReps logit regressions and store the model results
ttv_glms  <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread_volume, family = "binomial"))
ttv_glms2 <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread_volume + turtles$Age, family = "binomial"))
ttv_glms3 <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread_volume + turtles$Species , family = "binomial"))
ttv_glms4 <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread_volume + turtles$Age + turtles$Species, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
ttvsummaries <- lapply(ttv_glms, summary)
ttvsummaries2 <- lapply(ttv_glms2, summary)
ttvsummaries3 <- lapply(ttv_glms3, summary)
ttvsummaries4 <- lapply(ttv_glms4, summary)
# ...AICs
ttvaics <- unlist(sapply(ttvsummaries, function(x) c(aic = x$aic)))
ttvaics2 <- unlist(sapply(ttvsummaries2, function(x) c(aic = x$aic)))
ttvaics3 <- unlist(sapply(ttvsummaries3, function(x) c(aic = x$aic)))
ttvaics4 <- unlist(sapply(ttvsummaries4, function(x) c(aic = x$aic)))

mean(ttvaics)
mean(ttvaics2)
mean(ttvaics3)
mean(ttvaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

ttChanceKD <- matrix(rep(case_when(turtles$threaddeath == 'KD' ~ 1, 
                                   turtles$threaddeath == 'KND' ~ 0,
                                   turtles$threaddeath == 'Ind' ~ NA,
                                   turtles$threaddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


ttChanceKD <- ifelse(is.na(ttChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     ttChanceKD)

# run NumReps logit regressions and store the model results
ttvglmsX <- lapply(1:NumReps, function(x) glm(ttChanceKD[,x] ~ turtles$thread_volume + turtles$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
ttvsummariesX <- lapply(ttvglmsX, summary)

# ...coefficents with standard errors:
ttvcoefs <- unlist(lapply(ttvsummariesX, function(x) x$coefficients[2, 1]))
ttvses <- unlist(lapply(ttvsummariesX, function(x) x$coefficients[2, 2]))
ttvaics <- unlist(sapply(ttvsummariesX, function(x) c(aic = x$aic)))

ttvbeta0 <- median(unlist((lapply(ttvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
ttvbeta1 <- median(unlist((lapply(ttvsummariesX, function(x) x$coefficients[2, 1])))) #thread
ttvbeta2 <- median(unlist((lapply(ttvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
ttvbeta3 <- median(unlist((lapply(ttvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
ttvbeta4 <- median(unlist((lapply(ttvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

ttvse0 <- median(unlist((lapply(ttvsummariesX, function(x) x$coefficients[1, 2])))) #intercept

ttvx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
ttvx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
ttvx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by thread plastic with median coefficient estimates from MC and mean values for variables
ttvprange <- seq(0, 5000)
ttvExp <- exp(ttvbeta0 + ttvbeta1 * ttvprange + ttvbeta2 * ttvx2 + ttvbeta3 * ttvx3 + ttvbeta4 * ttvx4)
ttvProb <- ttvExp/(1+ttvExp)
turtles_thread_volume <- plot(ttvProb ~ ttvprange, type = "l", xlab = "Turtles thread volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")

#############################################################################################################################
#############################################################################################################################
#Predictive models: Turtle Hard Pieces
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

thChanceKD <- matrix(rep(case_when(turtles$harddeath == 'KD' ~ 1, 
                                   turtles$harddeath == 'KND' ~ 0, 
                                   turtles$harddeath == 'Ind' ~ NA,
                                   turtles$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


thChanceKD <- ifelse(is.na(thChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     thChanceKD)


# run NumReps logit regressions and store the model results
th_glms  <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard, family = "binomial"))
th_glms2 <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard + turtles$Age, family = "binomial"))
th_glms3 <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard + turtles$Species , family = "binomial"))
th_glms4 <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard + turtles$Age + turtles$Species, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
thsummaries <- lapply(th_glms, summary)
thsummaries2 <- lapply(th_glms2, summary)
thsummaries3 <- lapply(th_glms3, summary)
thsummaries4 <- lapply(th_glms4, summary)
# ...AICs
thaics <- unlist(sapply(thsummaries, function(x) c(aic = x$aic)))
thaics2 <- unlist(sapply(thsummaries2, function(x) c(aic = x$aic)))
thaics3 <- unlist(sapply(thsummaries3, function(x) c(aic = x$aic)))
thaics4 <- unlist(sapply(thsummaries4, function(x) c(aic = x$aic)))

mean(thaics)
mean(thaics2)
mean(thaics3)
mean(thaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

thChanceKD <- matrix(rep(case_when(turtles$harddeath == 'KD' ~ 1, 
                                   turtles$harddeath == 'KND' ~ 0, 
                                   turtles$harddeath == 'Ind' ~ NA,
                                   turtles$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


thChanceKD <- ifelse(is.na(thChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     thChanceKD)


# run NumReps logit regressions and store the model results
thglmsX <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard + turtles$Age, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
thsummariesX <- lapply(thglmsX, summary)

# ...coefficents with standard errors:
thcoefs <- unlist(lapply(thsummariesX, function(x) x$coefficients[2, 1]))
thses <- unlist(lapply(thsummariesX, function(x) x$coefficients[2, 2]))
thaics <- unlist(sapply(thsummariesX, function(x) c(aic = x$aic)))


thbeta0 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[1, 1])))) #intercept
thbeta1 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[2, 1])))) #thread
thbeta2 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
thbeta3 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
thbeta4 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

thse0 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[1, 2])))) #intercept
thse1 <- median(unlist((lapply(thsummariesX, function(x) x$coefficients[2, 2]))))

thx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
thx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
thx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by thread plastic with median coefficient estimates from MC and mean values for variables
thprange <- seq(0, 250)
thExp <- exp(thbeta0 + thbeta1 * thprange + thbeta2 * thx2 + thbeta3 * thx3 + thbeta4 * thx4)
#thExp <- exp(thbeta0 + thbeta1 * thvprange) # + thvbeta2 * thvx2 + thvbeta3 * thvx3 + thvbeta4 * thvx4)
thExp1 <-exp(thbeta0 + (thbeta1 + thse1)*thprange + thbeta2 * thx2 + thbeta3 * thx3 + thbeta4 * thx4)
thExp2 <- exp(thbeta0 + (thbeta1 - thse1)*thprange + thbeta2 * thx2 + thbeta3 * thx3 + thbeta4 * thx4)
thProb <- thExp/(1+thExp)
thProb1 <-thExp1/(1+thExp1)
thProb2 <-thExp2/(1+thExp2)
turtles_hard_pieces <- plot(thProb ~ thprange, type = "l", xlab = "Turtle Hard Pieces", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")
lines(thProb1 ~ thprange, type = "l", lwd = 2, col = "red")
lines(thProb2 ~ thprange, type = "l", lwd = 2, col = "red")

##########################################################################################################################################
###########################################################################################################################################
#Predictive models: Turtle hard Volume
# First let's test which model fits best...using only 50 reps for speed
NumReps <- 50
set.seed(1234)

thChanceKD <- matrix(rep(case_when(turtles$harddeath == 'KD' ~ 1, 
                                   turtles$harddeath == 'KND' ~ 0, 
                                   turtles$harddeath == 'Ind' ~ NA,
                                   turtles$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)


thChanceKD <- ifelse(is.na(thChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     thChanceKD)


# run NumReps logit regressions and store the model results
thv_glms  <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard_volume, family = "binomial"))
thv_glms2 <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard_volume + turtles$Age, family = "binomial"))
thv_glms3 <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard_volume + turtles$Species , family = "binomial"))
thv_glms4 <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard_volume + turtles$Age + turtles$Species, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
thvsummaries <- lapply(thv_glms, summary)
thvsummaries2 <- lapply(thv_glms2, summary)
thvsummaries3 <- lapply(thv_glms3, summary)
thvsummaries4 <- lapply(thv_glms4, summary)
# ...AICs
thvaics <- unlist(sapply(thvsummaries, function(x) c(aic = x$aic)))
thvaics2 <- unlist(sapply(thvsummaries2, function(x) c(aic = x$aic)))
thvaics3 <- unlist(sapply(thvsummaries3, function(x) c(aic = x$aic)))
thvaics4 <- unlist(sapply(thvsummaries4, function(x) c(aic = x$aic)))

mean(thvaics)
mean(thvaics2)
mean(thvaics3)
mean(thvaics4)
# AIC for model 4 is lowest by mean (and median) so let's use that one


# Now let's do the monte carlo for 1000 reps, but limited to best fitting model
NumReps <- 1000
set.seed(1234)

thChanceKD <- matrix(rep(case_when(turtles$harddeath == 'KD' ~ 1, 
                                   turtles$harddeath == 'KND' ~ 0,
                                   turtles$harddeath == 'Ind' ~ NA,
                                   turtles$harddeath == 'PD' ~ NA), NumReps), ncol = NumReps, byrow = FALSE)

thChanceKD <- ifelse(is.na(thChanceKD), 
                     matrix(round(runif(nrow(turtles)*NumReps)), nrow = nrow(turtles), ncol = NumReps), 
                     thChanceKD)

# run NumReps logit regressions and store the model results
thvglmsX <- lapply(1:NumReps, function(x) glm(thChanceKD[,x] ~ turtles$hard_volume, family = "binomial"))

# if you need more info, get full summary call. now you can get whatever, like:
thvsummariesX <- lapply(thvglmsX, summary)

# ...coefficents with standard errors:
thvcoefs <- unlist(lapply(thvsummariesX, function(x) x$coefficients[2, 1]))
thvses <- unlist(lapply(thvsummariesX, function(x) x$coefficients[2, 2]))
thvaics <- unlist(sapply(thvsummariesX, function(x) c(aic = x$aic)))

thvbeta0 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[1, 1])))) #intercept
thvbeta1 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[2, 1])))) #thread
#thvbeta2 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[3, 1])))) #age:Infant
#thvbeta3 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[4, 1])))) #age:Juvenile
#thvbeta4 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult
#thvbeta5 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult
#thvbeta6 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult
#thvbeta7 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[5, 1])))) #age:Subadult

thvse0 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[1, 2])))) #intercept
thvse1 <- median(unlist((lapply(thvsummariesX, function(x) x$coefficients[2, 2])))) #SE of coefficient

#thvx2 <- mean(turtles$Age == "Infant", na.rm = TRUE)
#thvx3 <- mean(turtles$Age == "Juvenile", na.rm = TRUE)
#thvx4 <- mean(turtles$Age == "Subadult", na.rm = TRUE)

# Predict probability of death by thread plastic with median coefficient estimates from MC and mean values for variables
thvprange <- seq(0, 1000)
thvExp <- exp(thvbeta0 + thvbeta1 * thvprange) # + thvbeta2 * thvx2 + thvbeta3 * thvx3 + thvbeta4 * thvx4)
thvExp1 <-exp(thvbeta0 + (thvbeta1 + .5 * thvse1)*thvprange)
thvExp2 <- exp(thvbeta0 + (thvbeta1 - .5 * thvse1)*thvprange)
thvProb <- thvExp/(1+thvExp)
thvProb1 <-thvExp1/(1+thvExp1)
thvProb2 <-thvExp2/(1+thvExp2)
turtles_hard_volume <- plot(thvProb ~ thvprange, type = "l", xlab = "Turtles hard volume", ylab = "Probability of Death", ylim = c(0, 1), lwd = 2, col = "blue")
lines(thvProb1 ~ thvprange, type = "l", lwd = 2, col = "red")
lines(thvProb2 ~ thvprange, type = "l", lwd = 2, col = "red")

