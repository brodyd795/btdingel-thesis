library(tidyverse)
options("scipen"=10, "digits"=2)
library(lme4)
library(nlme)
library(optimx)
library(lmerTest)
library(lsmeans)
library(effects)
library(rstanarm)

setwd('/Users/myUsername/data')

######################################
# paired t-tests for initial overview of quiz scores
######################################

dat_all = read.table("../expanded_data/quiz-level/quiz-level-results-outlier-removed-all.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
dat_102 = read.table("../expanded_data/quiz-level/quiz-level-results-outlier-removed-102.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
dat_201 = read.table("../expanded_data/quiz-level/quiz-level-results-outlier-removed-201.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
dat_202 = read.table("../expanded_data/quiz-level/quiz-level-results-outlier-removed-202.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# change line below for easy running of t-tests below with different portions of dataset
dat_ttest = dat_all

# check normality and outlier assumptions with hist and boxplot
hist(dat_ttest$NR.R, breaks=8, main="Differences between R and NR quiz scores", col="lightblue", xlab="Difference")
boxplot(dat_ttest$NR.R, main="Differences between R and NR quiz scores", col="lightblue")
t.test(dat_ttest$NR, dat_ttest$R, paired=TRUE)

######################################
# logistic MER models
######################################

dat_original = read.table("../expanded_data/item-level/expanded-IDs/csv/expanded_data_original_outlier_removed.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# 'strict' logit

val = factor(ifelse(dat_original$Score==2,1,0))

logit_m0 <- glmer(val ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + 1, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_m1 <- glmer(val ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_m2 <- glmer(val ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType + Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_m3 <- glmer(val ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType*Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

logit_m4 <- glmer(val ~ (1|Subj) + (1|Section) + (1|Instructor) + ListType*Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_m5 <- glmer(val ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType*Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

anova(logit_m0, logit_m1)
summary(logit_m1)
anova(logit_m1, logit_m2)
summary(logit_m2)
anova(logit_m2, logit_m3)
summary(logit_m3)
anova(logit_m4, logit_m5)
summary(logit_m5)


# 'sensitive' logit

val_sensitive = factor(ifelse(dat_original$Score==0,0,1))

logit_sensitive_m0 <- glmer(val_sensitive ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + 1, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_sensitive_m1 <- glmer(val_sensitive ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_sensitive_m2 <- glmer(val_sensitive ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType + Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_sensitive_m3 <- glmer(val_sensitive ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType*Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

logit_sensitive_m4 <- glmer(val_sensitive ~ (1|Subj) + (1|Section) + (1|Instructor) + ListType*Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
logit_sensitive_m5 <- glmer(val_sensitive ~ (1|Subj) + (1|Section) + (1|Instructor) + (1|Item) + ListType*Course, data = dat_original, family = binomial, nAGQ=1, control = glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

anova(logit_sensitive_m0, logit_sensitive_m1)
summary(logit_sensitive_m1)
anova(logit_sensitive_m1, logit_sensitive_m2)
summary(logit_sensitive_m2)
anova(logit_sensitive_m2, logit_sensitive_m3)
summary(logit_sensitive_m3)
anova(logit_sensitive_m4, logit_sensitive_m5)
summary(logit_sensitive_m5)
