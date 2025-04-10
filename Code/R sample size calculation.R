#To simulate sample size we need to find distribution of our predictors, confidence and AOTS, which we will use from control condition in study 1

df <- read.csv('./Data/Study 1.2 Final Data.csv')

library(tidyr)
library(dplyr)

#only include full data
df<-subset(df,Progress==100)
nrow(df)# 3 participants excluded for not fully finishing the study

#only include participant's who spend 15minutes or more
removed<-subset(df, Duration<15)
df<-subset(df,Duration>=15)
nrow(removed)# 71 participants excluded for spending less than 15minutes
nrow(removed[removed$Feedback==5,])# 12 excluded from new feedback condition

#format dataset
df$Feedback<-as.factor(df$Feedback)
df$Participant <- as.factor(df$Participant)

#let's create average score for the AOTS 
df$AOTS_avg <- rowMeans(df[, grepl("^AOTS", colnames(df))], na.rm = TRUE)
df<-df%>% relocate (AOTS_avg,.after=AOTS13) #move column so it is just after the other AOTS columns

#Calculate average confidence
df$Confidence <- rowMeans(df[, grepl("^CV", colnames(df))], na.rm = TRUE)

Control<-subset(df,Feedback==1)#select only control group as we are not interested in feedback conditions

#generate Prior Confidence scores
#find variation of confidence within video
clean_data <- Control[, c("Participant", 
                          grep("^CV", names(Control), value = TRUE), 
                          grep("^V", names(Control), value = TRUE),"AOTS_avg")]

#convert data to long
long_data<-reshape(clean_data, direction="long", 
                 varying=list(c(paste0("V",1:20)), c(paste0("CV",1:20))), 
                 v.names=c("Answer","Confidence"),timevar="Video", idvar="Participant")

# View transformed data
head(long_data, 20)

long_data$Video <- as.factor(long_data$Video)

hist(long_data$Confidence, main = "Distribution of Confidence", xlab = "Confidence")
hist(clean_data$AOTS_avg, main = "Distribution of AOTS_avg", xlab = "AOTS_avg")



#generate data for videos and participant
artificial_data <- expand.grid(Video= (1:20), Participant = (1:20))

#generate data for our main predictor variable, source, which has 4 levels, using contrasting coding to compare control with the 3 others
artificial_data["Source"] <- c(rep(-1, 100), rep(1/3, 100), rep(1/3, 100), rep (1/3, 100))
artificial_data$Source <- as.factor(artificial_data$Source)

#also generate the data for contrast coding between advice groups
artificial_data["Source2"] <- c(rep(1, 100), rep(2, 100), rep(3, 100), rep (4, 100))
artificial_data$Source2 <- as.factor(artificial_data$Source2)

#generate AOTS scores
mean(clean_data$AOTS_avg)
sd(clean_data$AOTS_avg)
AOTS_scores <- rnorm(20, mean=4.647059, sd=0.633987) #no value outside of 1-6 possible range
artificial_data["AOTS_avg"] <- AOTS_scores[artificial_data$Participant]  # Assign scores by Participant
artificial_data$CenteredAOTS <- scale(artificial_data$AOTS_avg, scale= F)
artificial_data$CenteredAOTS <- as.numeric(artificial_data$CenteredAOTS)

library(lme4)

#Fit to non_normal distribution of confidence
library(stats)

n_samples=400

# Resample directly from the original Confidence data (with replacement)
simulated_confidence <- sample(long_data$Confidence, size = n_samples, replace = TRUE)

# Check the results
summary(simulated_confidence)
hist(simulated_confidence, main = "Bootstrapped Confidence", xlab = "Confidence")

#As the effect size used for the effect of confidence is based on Chong et al. 2022, who used a 1-5 scale, we first need to rescale our confidence variable
rescaled_simulated_confidence <- 1 + (simulated_confidence - 50) * (4 / 50)  # Rescale 50-100 to 1-5


artificial_data["Prior_Confidence"] <- rescaled_simulated_confidence
artificial_data <- artificial_data %>%
  group_by(Participant) %>%
  mutate(CenteredConfidence = Prior_Confidence - mean(Prior_Confidence, na.rm = TRUE)) %>%
  ungroup()

artificial_data$GrandMeanCenteredConfidence <- scale(artificial_data$Prior_Confidence, scale= F)
artificial_data$GrandMeanCenteredConfidence <- as.numeric(artificial_data$GrandMeanCenteredConfidence)

#For random variance components 
# Fit a model with random intercepts for Participant and Video
model1 <- glmer(Answer ~ 1 + (1 | Participant) + (1 | Video), family="binomial", data = long_data)

#Since video random effect has low variance, let's simplify the model by removing it
model2 <- glmer(Answer ~ 1 + (1 | Participant), family="binomial", data = long_data)

# Show variance estimates
summary(model1)
variance_components <- as.data.frame(VarCorr(model1))
variance_components_model2 <- as.data.frame(VarCorr(model2))
variance_CIs <- confint(model1, method = "profile") #calculates CI's for random effects SD's
participant_variance_upper_CI <- (0.4317107)^2 #0.1863741
video_variance_upper_CI <- (0.3905155)^2 #0.1525024

anova(model1, model2) #difference in models is statistically significant, indicating variance explained by video random effect is significant
AIC(model1, model2) #model 1 has lower AIC indicating better model

#For baseline, since we have no data to base this, we will assume low rate of belief updating, so let's say 5%, which corresponds to -2.94
Intercept <- -2.94

#Using findings from Groh et al. (2022) for effect of source
#12% changed answer following AI feedback
Source0.3 <- log(0.12/(1-0.12)) - Intercept #-1.99243 - 2.94

#As there is no analagous effect size for the relationship between belief updating and AOTS, and due to monetary constraints
#we will specify medium effect size for the interaction between AOTS and Source Type and the fixed effect for AOTS
AOTS <-  log(1.6) #60% increase in belief updating per 1 point increase in AOTS
Source0.3_AOTS <- log(1.6) #60% increase in belief updating per 1 point increase in AOTS

#Based on results from Chong et al. 2022 we will use effect size of self-confidence on probability to accept the AI suggestion for the effect size of self-confidence
#effect size is log odds of -1, which is based on 1-5 confidence scale used
Confidence <- 0 #We do not expect a main effect for Confidence as we don't expect relationship between the control group and Confidence
Source0.3_Confidence <- -1

#We do not have predictions regarding a three-way interaction or an interaction between AOTS and confidence
Confidence_AOTS <- 0
Source0.3_Confidence_AOTS <- 0

#set value for intercept, fixed effects and interaction
fixed_effects <- c(Intercept, Source0.3, Confidence, AOTS, Source0.3_Confidence, Source0.3_AOTS, Confidence_AOTS, Source0.3_Confidence_AOTS)

#set random variances for participant and stimuli based on model1
random_variance <- list(Participant = c(0.08286332), Video = c(0.05354703))
random_variance_upper_CI <- list(Participant = c(participant_variance_upper_CI),Video = c(video_variance_upper_CI))

library(simr)

formula <- Y ~ Source*CenteredConfidence*CenteredAOTS + (1|Participant) +(1|Video)

#create artificial GLMM
artificial_glmer <- makeGlmer(formula, fixef = fixed_effects,
                              VarCorr = random_variance,
                              family = "binomial",
                              data= artificial_data)
summary(artificial_glmer)

fixed_effects <- fixef(artificial_glmer)

set.seed(123)  # Setting seed
# analysis with simr for fixed effect "Source"
model_N_200 <- extend(artificial_glmer, along ="Participant", n = 200)
source_N_200<- powerSim(model_N_200,  test = fixed("Source"), nsim = 1000, progress =TRUE) #99.7% power
confidence_N_200<- powerSim(model_N_200,  test = fixed("Source0.333333333333333:CenteredConfidence"), nsim = 1000, progress =TRUE) #100% power

model_N_400 <- extend(artificial_glmer, along ="Participant", n = 400)
AOTS_N_400<- powerSim(model_N_400,  test = fixed("Source0.333333333333333:CenteredAOTS"), nsim = 1000, progress =TRUE) #70.80%

model_N_500 <- extend(artificial_glmer, along ="Participant", n = 500)
AOTS_N_500<- powerSim(model_N_500,  test = fixed("Source0.333333333333333:CenteredAOTS"), nsim = 1000, progress =TRUE) #57% power

model_N_600 <- extend(artificial_glmer, along ="Participant", n = 600)
AOTS_N_600<- powerSim(model_N_600,  test = fixed("Source0.333333333333333:CenteredAOTS"), nsim = 1000, progress =TRUE) #96.60%% power


#create artificial GLMM
artificial_glmer_upper_CI <- makeGlmer(formula, fixef = fixed_effects,
                              VarCorr = random_variance_upper_CI,
                              family = "binomial",
                              data= artificial_data)
summary(artificial_glmer)

model_N_600_upper_CI <- extend(artificial_glmer, along ="Participant", n = 600)
AOTS_N_600_upper_CI<- powerSim(model_N_600_upper_CI,  test = fixed("Source0.333333333333333:CenteredAOTS"), nsim = 1000, progress =TRUE) #50.80% power

#Let's now look at detection accuracy

#We first need to re-construct dataset so that it can account for trial phase which will included as fixed effect

#Add Phase 1 to the current dataset
acc_artificial_data <- artificial_data %>%
  mutate(Phase = 1)  # Add Phase column with value 1 for existing trials

#Create Phase 2 dataset by duplicating and modifying
acc_artificial_data_phase2 <- acc_artificial_data %>%
  mutate(Phase = 2,  # Change Phase to 2
         Video = Video + 20)  # Offset video numbers by 20 to distinguish them

#Combine both phases into a new dataset
acc_artificial_data <- bind_rows(acc_artificial_data, acc_artificial_data_phase2)

acc_artificial_data$Phase <- as.factor(acc_artificial_data$Phase)#change to factor

#Rearrange columns
acc_artificial_data <- acc_artificial_data %>%
  select(Video, Participant, Source,Source2, Phase, everything())


#Since we do not want pseudoreplication for Prior Confidence we'll use the aggregated prior confidence as a fixed effect
acc_artificial_data <- acc_artificial_data %>%
  group_by(Participant) %>%  # Group by participant
  mutate(mean_prior_confidence = mean(Prior_Confidence, na.rm = TRUE)) %>%  # Compute mean
  ungroup()  

acc_artificial_data$mean_centered_prior_confidence <- scale(acc_artificial_data$mean_prior_confidence)
acc_artificial_data$mean_centered_prior_confidence <- as.numeric(acc_artificial_data$mean_centered_prior_confidence)

# View new dataset
View(acc_artificial_data)


#For baseline, from our first study indicates an accuracy of 58%
acc_intercept <- log(0.58/(1-0.58))

#Let's first specify our main fixed effects, which we only predict AOTS to have significant relationship
acc_Source0.3 <- 0 
acc_Phase2 <- 0 
acc_Confidence <- 0
acc_AOTS <- log(1.6) #Use same effect size as for belief updating

#Let's now look at all the 2-way interactions
#Using findings from Groh et al. (2022) for increase in accuracy following AI advice
#7% increase in detection accuracy following AI feedback 
#we will use this as fixed effect for interaction between Source and Phase, as the expected change in detection accuracy in the advice conditions in phase 2
acc_Source0.3_Phase2 <- log(0.65/(1-0.65)) - acc_intercept #0.6190392- 0.3227734
acc_Source0.3_Confidence <- 0
acc_Source0.3_AOTS <- 0
acc_Phase2_Confidence <- 0
acc_Phase2_AOTS <- 0

#We expect significant three-way interaction between Source_Phase and AOTS and Confidence
acc_Source0.3_Phase2_Confidence <- 1 #use same effect size as belief updating as we don't have better estimate
acc_Source0.3_Phase2_AOTS <- log(1.6) #use same effect size as belief updating



#We will use the same effect sizes for AOTS and (Prior/First Round) Confidence 
acc_fixed_effects<-c(acc_intercept, 
                     acc_Source0.3, 
                     acc_Phase2, 
                     acc_Confidence, 
                     acc_AOTS,
                     acc_Source0.3_Phase2,
                     acc_Source0.3_Confidence, 
                     acc_Source0.3_AOTS,
                     acc_Phase2_Confidence,
                     acc_Phase2_AOTS,
                     acc_Source0.3_Phase2_Confidence, 
                     acc_Source0.3_Phase2_AOTS)

acc_formula <- Y ~ Source*Phase*mean_centered_prior_confidence + Source*Phase*CenteredAOTS +
  (1|Participant) +(1|Video)

#create artificial GLMM
acc_artificial_glmer <- makeGlmer(acc_formula, fixef = acc_fixed_effects,
                              VarCorr = random_variance,
                              family = "binomial",
                              data= acc_artificial_data)
summary(acc_artificial_glmer)

acc_model_N_600 <- extend(acc_artificial_glmer, along ="Participant", n = 600)
acc_source_N_600<- powerSim(acc_model_N_600,  test = fixed("Source0.333333333333333:Phase2"), nsim = 50, progress =TRUE) #84% power
acc_confidence_N_600<- powerSim(acc_model_N_600,  test = fixed("Source0.333333333333333:Phase2:mean_centered_prior_confidence"), nsim = 50, progress = TRUE) #44% power, underpowered
acc_AOTS_N_600 <- powerSim(acc_model_N_600,  test = fixed("Source0.333333333333333:Phase2:CenteredAOTS"), nsim = 50, progress = TRUE)

#These power analyses indicate there's not enough power to detect effect sizes for three-way interactions
#To increase power models might need to be simplified. 

#We can do this by removing Phase and only look at final round accuracy.
#This technique still provides meaningful results, whilst we can do a comparison between control in Phase 1 and Phase 2 as a check that people don't meaningfully improve without advice
#This technique also allows us to use individual trial Prior confidence, rather than an aggregated measure

#Since we are only looking at trial correctness for the second set of trials we can use the original artificial_data dataframe

#Let's re-designate our fixed effects
acc2_intercept <- log(0.58/(1-0.58)) #base-rate from control condition in study 1 
acc2_Source0.3 <- log(0.65/(1-0.65)) - acc2_intercept #0.6190392- 0.3227734
acc2_Confidence <- 0
acc2_AOTS <- log(1.6) #Use same effect size as for belief updating

#Let's now look at all the 2-way interactions
acc2_Source0.3_Confidence <- 1
acc2_Source0.3_AOTS <- log(1.6)
acc2_Confidence_AOTS <- 0

#3-way interaction
acc2_Source0.3_Confidence_AOTS <- 0

#We will use the same effect sizes for AOTS and (Prior/First Round) Confidence 
acc2_fixed_effects<-c(acc2_intercept, 
                     acc2_Source0.3, 
                     acc2_Confidence, 
                     acc2_AOTS,
                     acc2_Source0.3_Confidence, 
                     acc2_Source0.3_AOTS,
                     acc2_Confidence_AOTS,
                     acc2_Source0.3_Confidence_AOTS)

acc2_formula <- Y ~ Source*CenteredConfidence*CenteredAOTS +
  (1|Participant) +(1|Video)

acc2_artificial_glmer <- makeGlmer(acc2_formula, fixef = acc2_fixed_effects,
                                   VarCorr = random_variance,
                                   family = "binomial",
                                   data= artificial_data)
summary(acc2_artificial_glmer)

acc2_model_N_500 <- extend(acc2_artificial_glmer, along ="Participant", n = 500)
acc2_source_N_500<- powerSim(acc2_model_N_500,  test = fixed("Source0.333333333333333"), nsim = 1000, progress =TRUE) #100% power
acc2_confidence_N_500<- powerSim(acc2_model_N_500,  test = fixed("Source0.333333333333333:CenteredConfidence"), nsim = 1000, progress = TRUE) #
acc2_AOTS_N_500 <- powerSim(acc2_model_N_500,  test = fixed("Source0.333333333333333:CenteredAOTS"), nsim = 1000, progress = TRUE) #

acc2_model_N_600 <- extend(acc2_artificial_glmer, along ="Participant", n = 600)
acc2_source_N_600<- powerSim(acc2_model_N_600,  test = fixed("Source0.333333333333333"), nsim = 1000, progress =TRUE) #92.70% power
acc2_confidence_N_600<- powerSim(acc2_model_N_600,  test = fixed("Source0.333333333333333:CenteredConfidence"), nsim = 1000, progress = TRUE) #100%power
acc2_AOTS_N_600 <- powerSim(acc2_model_N_600,  test = fixed("CenteredAOTS"), nsim = 1000, progress = TRUE) #100% power
acc2_AOTS_int_N_600 <- powerSim(acc2_model_N_600,  test = fixed("Source0.333333333333333:CenteredAOTS"), nsim = 1000, progress = TRUE) #100% power

