#load libraries
library(readr)
library(ggpubr)
library(mice)
library(ggplot2)
library(psych)
library(plyr)
library(dplyr)
library(tidyr)
library(blandr)
library(car)

#load in dataset
df <- read.csv("Desktop/Chapter 7/submission/df.csv")

#check structure of dataframe
df$ID = as.numeric(df$ID)
df$age = as.numeric(df$age)
df$gender = as.factor(df$gender)
df$exercise = as.numeric(df$exercise)
df$sleep = as.numeric(df$sleep)
df$smoker = as.factor(df$smoker)
df$shai14_total = as.numeric(df$shai14_total)
convert <- c("sf1", "sf2", "sf3", "sf4", "sf5", "sf6", "sf7", "sf8", "sf9", "sf10","sf11", "sf12")
df <- df %>%
  mutate_at(vars(convert), as.numeric)
str(df)


#scatterplot of single item and shai scores
plot1 <- ggscatter(df, x = "shai14_total", y = "oneitem", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "SHAI", ylab = "oneitem")
plot1

#correlation test
cor.test(df$shai14_total, df$oneitem)

#generating a bland-altman plot (comparing the single item to the SHAI)
bland <- subset(df, select = c("shai14_total", "oneitem", "ID"))

#create new column for average measurement
bland$shai14_total = as.numeric(bland$shai14_total)
bland$oneitem = as.numeric(bland$oneitem)
bland$avg <- rowMeans(bland) 

#create new column for difference in measurements
bland$diff <- bland$shai14_total - bland$oneitem

#view first six rows of data
head(bland)

#find average difference
mean_diff <- mean(bland$diff)
mean_diff

lower <- mean_diff - 1.96*sd(bland$diff)
lower

upper <- mean_diff + 1.96*sd(bland$diff)
upper

x <- ggplot(bland, aes(x = avg, y = diff)) +
  geom_point(size=1) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ylab("Difference Between Measurements") +
  xlab("Average Measurement") +
  theme_bw()
plot(x)


#simple linear regression
lmba <- lm(diff ~ avg, data = bland)
summary(lmba)

#one samples t test
res <- t.test(bland$diff)
res  


#imputation of missing data
unimputed = df

init = mice(unimputed, maxit=0) 
print(init$method)
meth = init$method
predM = init$predictorMatrix

predM[, c("ID")]=0 #remove id variable from the imputation, not needed to predict other values

meth[c("oneitem", "pcs12", "mcs12", "shai14_total", "exercise", "smoker", "sleep", "age", "gender")]=""

meth[c("age")]="pmm" 
meth[c("gender")]="pmm" 

set.seed(103)
dfunimputed = mice(unimputed, method=meth, predictorMatrix=predM, m=6)

df2 <- complete(dfunimputed)

sapply(df2, function(x) sum(is.na(x)))



#relevel predictor variables
levels(df2$gender) <- c(1,2,1,1)
df2$exercise = as.numeric(df2$exercise)
df2$sleep = as.numeric(df2$sleep)
df2$smoker = as.factor(df2$smoker)
df2$age = as.numeric(df2$age)
df2$shai14_total = as.numeric(df2$shai14_total)
str(df2)


#hierarchical multiple regression models
#pcs component of sf-12 scores
#step1
lm1 = lm(pcs12 ~ age + gender + exercise + smoker + sleep, data = df2)
summary(lm1)
confint(lm1)
vif(lm1)

#step2
lm2 = lm(pcs12 ~ age + gender + exercise + smoker + sleep + oneitem, data = df2)
summary(lm2)
confint(lm2)
vif(lm2)

#step3
lm3 = lm(pcs12 ~ age + gender + exercise + smoker + sleep + oneitem + shai14_total, data = df2)
summary(lm3)
confint(lm3)
vif(lm3)


#mcs component of sf-12 scores
#step1
lm4 = lm(mcs12 ~ age + gender + exercise + smoker + sleep, data = df2)
summary(lm4)
confint(lm4)
vif(lm4)

#step2
lm5 = lm(mcs12 ~ age + gender + exercise + smoker + sleep + oneitem, data = df2)
summary(lm5)
confint(lm5)
vif(lm5)

#step3
lm6 = lm(mcs12 ~ age + gender + exercise + smoker + sleep + oneitem + shai14_total, data = df2)
summary(lm6)
confint(lm6)
vif(lm6)


#sensitivity analysis
lms1 = lm(pcs12 ~ age + gender + exercise + smoker + sleep + shai14_total, data = df2)
summary(lms1)
lms2 = lm(pcs12 ~ age + gender + exercise + smoker + sleep + shai14_total + oneitem, data = df2)
summary(lms2)
lms3 = lm(mcs12 ~ age + gender + exercise + smoker + sleep + shai14_total, data = df2)
summary(lms3)
lms4 = lm(mcs12 ~ age + gender + exercise + smoker + sleep + shai14_total + oneitem, data = df2)
summary(lms4)

write.csv(df, "Desktop/Chapter 7/issda/df.csv", row.names = F)
write_dta(df, "Desktop/Chapter 7/issda/df.dta")
write_sav(df, "Desktop/Chapter 7/issda/df.sav")
