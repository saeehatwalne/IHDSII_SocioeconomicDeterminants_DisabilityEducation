library(haven)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(car)
library(RColorBrewer)
# install.packages("ROSE")
# install.packages("smotefamily")
# install.packages("Hmisc")
library(Hmisc)
library(stargazer)
library(ROSE)
library(smotefamily)
library(lmtest)
library(gridExtra)
rm(list=ls())
setwd("/Users/saeehatwalne/Desktop/GIPE 2020-23/Thesis /November 2023")
mydata <- read_excel("/Users/saeehatwalne/Desktop/GIPE 2020-23/Thesis /November 2023/ThesisFinalDataset.xlsx")
#keeping only required columns
mydata <- mydata %>%
  select(Enrollment, Disability, Sex_RO3,Rural_Urban, Religion_ID11, Caste_ID13,
         Main_incomesourceHH_ID14, Activity_Father_RO7, worker_status_Father_WS13, Education_Father_ED6,
         Activity_Mother_RO7, worker_status_Mother_WS13, Education_Mother_ED6, Distancetoschool_CS5)

#RECODING THE DATA
main_income_list <- read_excel("Main_incomesourceHH_ID14.xlsx")
activity_status_list <- read_excel("activitystatus_codes.xlsx")
worker_status_list <- read_excel("workerstatus_code.xlsx")
years_education_list <- read_excel("years_education_codes.xlsx")
caste_list <- read_excel("caste_codes.xlsx")
religion_list <- read_excel("religion_codes.xlsx")
mydata$Main_incomesourceHH_ID14 <- factor(mydata$Main_incomesourceHH_ID14,
                              levels = main_income_list$code,
                              labels = main_income_list$incomesource)
mydata$Activity_Father_RO7 <- factor(mydata$Activity_Father_RO7,
                                          levels = activity_status_list$code,
                                          labels = activity_status_list$activitystatus)
mydata$Activity_Mother_RO7 <- factor(mydata$Activity_Mother_RO7,
                                     levels = activity_status_list$code,
                                     labels = activity_status_list$activitystatus)
mydata$worker_status_Father_WS13 <- factor(mydata$worker_status_Father_WS13,
                                     levels = worker_status_list$code,
                                     labels = worker_status_list$workerstatus)
mydata$worker_status_Mother_WS13 <- factor(mydata$worker_status_Mother_WS13,
                                           levels = worker_status_list$code,
                                           labels = worker_status_list$workerstatus)
mydata$Education_Father_ED6 <- factor(mydata$Education_Father_ED6,
                                           levels = years_education_list$code,
                                           labels = years_education_list$years_education)
mydata$Education_Mother_ED6 <- factor(mydata$Education_Mother_ED6,
                                      levels = years_education_list$code,
                                      labels = years_education_list$years_education)
mydata$Caste_ID13 <- factor(mydata$Caste_ID13,
                                      levels = caste_list$codes,
                                      labels = caste_list$caste)
mydata$Religion_ID11 <- factor(mydata$Religion_ID11,
                            levels = religion_list$code,
                            labels = religion_list$religion)
mydata$Rural_Urban[mydata$Rural_Urban == "0"] <- "rural"
mydata$Rural_Urban[mydata$Rural_Urban == "1"] <- "urban"
mydata$Sex_RO3[mydata$Sex_RO3 == "0"] <- "male"
mydata$Sex_RO3[mydata$Sex_RO3 == "1"] <- "female"
mydata$Disability[mydata$Disability== "0"] <- "not_disabled"
mydata$Disability[mydata$Disability== "1"] <- "disabled"
mydata$Enrollment[mydata$Enrollment== "0"] <- "not_enrolled"
mydata$Enrollment[mydata$Enrollment== "1"] <- "enrolled"

str(mydata)

mydata$Enrollment <- as.factor(mydata$Enrollment)
mydata$Disability <- as.factor(mydata$Disability)
mydata$Sex_RO3 <- as.factor(mydata$Sex_RO3)
mydata$Rural_Urban <- as.factor(mydata$Rural_Urban)

#MODEL 1: ORIGINAL PLAN: NOT DISABLED/DISABLED

#univariate tables
#enrollment
describe(mydata$Enrollment)
enrollment_counts <- table(mydata$Enrollment)
enrollment_df <- as.data.frame(enrollment_counts)
colnames(enrollment_df) <- c("Enrollment", "Count")
enrollment_df$Proportion <- enrollment_df$Count / sum(enrollment_df$Count)

# ggplot(enrollment_df, aes(x = Enrollment, y = Proportion, fill = Enrollment)) +
#   geom_bar(stat = "identity") +
#   labs(title = "Proportion of Enrolled and \n Not Enrolled Children",
#        y = "Proportion") +
#   scale_fill_manual(values = c("enrolled" = "skyblue", "not_enrolled" = "salmon")) +
#   theme_minimal()

pie1 <- ggplot(enrollment_df, aes(x = "", y = Proportion, fill = Enrollment)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Enrollment", fill = "Enrollment") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(Proportion), y = Proportion/2), position = position_stack(vjust = 1)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "GnBu")

#disability
describe(mydata$Disability)
disability_counts <- table(mydata$Disability)
disability_df <- as.data.frame(disability_counts)
colnames(disability_df) <- c("Disability", "Count")
disability_df$Proportion <- disability_df$Count / sum(disability_df$Count)
pie2 <- ggplot(disability_df, aes(x = "", y = Proportion, fill = Disability)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Disability", fill = "Disability") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(Proportion), y = Proportion/2), position = position_stack(vjust = 0.5)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_brewer(palette = "GnBu")


#sex
describe(mydata$Sex_RO3)
sex_counts <- table(mydata$Sex_RO3)
sex_df <- as.data.frame(sex_counts)
colnames(sex_df) <- c("Sex", "Count")
sex_df$Proportion <- sex_df$Count / sum(sex_df$Count)
pie3 <- ggplot(sex_df, aes(x = "", y = Proportion, fill = Sex)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Sex", fill = "Sex") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(Proportion), y = Proportion/2), position = position_stack(vjust = 1.5)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())+
  scale_fill_brewer(palette = "GnBu")

#Region of residence
describe(mydata$Rural_Urban)
describe(mydata$Religion_ID11)
describe(mydata$Caste_ID13)
describe(mydata$Main_incomesourceHH_ID14)
incomesource_counts <- table(mydata$Main_incomesourceHH_ID14)
incomesource_df <- as.data.frame(incomesource_counts)
colnames(incomesource_df) <- c("Main income source", "Count")
incomesource_df$Proportion <- incomesource_df$Count / sum(incomesource_df$Count)
pie4 <- ggplot(incomesource_df, aes(x = "", y = Proportion, fill = `Main income source`)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "`Main income source`", fill = "`Main income source`") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(Proportion), y = Proportion/2), position = position_stack(vjust = 2.5)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) + 
  scale_fill_brewer(palette = "GnBu")
pie4

#Religion

describe(mydata$Religion_ID11)

religion_counts <- table(mydata$Religion_ID11)
religion_df <- as.data.frame(religion_counts)
colnames(religion_df) <- c("Religion", "Count")
religion_df$Proportion <- religion_df$Count / sum(religion_df$Count)
pie5 <- ggplot(religion_df, aes(x = "", y = Proportion, fill = Religion)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Religion", fill = "Religion") +
  theme_minimal() +
  geom_text(aes(label = scales::percent(Proportion), y = Proportion/2), position = position_stack(vjust = 2.5)) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank()) + 
  scale_fill_brewer(palette = "GnBu")
pie5



#arranging
par(bg = "#f2f4f5")

grid.arrange(pie1, pie2, pie3, pie4, ncol = 2, nrow = 2)
describe(mydata$Activity_Father_RO7)





describe(mydata$worker_status_Father_WS13)
describe(mydata$Education_Father_ED6)
describe(mydata$Activity_Mother_RO7)
describe(mydata$worker_status_Mother_WS13)
describe(mydata$Education_Mother_ED6)

bar1 <- ggplot(mydata, aes(x = factor(Education_Mother_ED6))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = brewer.pal(9, "BuGn")[5]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Mother's Education Level",
       x = "Mother's Education Level",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bar2 <- ggplot(mydata, aes(x = factor(Activity_Mother_RO7))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = brewer.pal(9, "BuGn")[6]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Mother's Activity",
       x = "Mother's Activity",
       y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bar3 <- ggplot(mydata, aes(x = factor(worker_status_Mother_WS13))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = brewer.pal(9, "BuGn")[7]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Mother's Worker Status",
       x = "Mother's Worker Status",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bar4 <- ggplot(mydata, aes(x = factor(Education_Father_ED6))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = brewer.pal(9, "PuBuGn")[6]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Father's Education Level",
       x = "Father's Education Level",
       y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bar5 <- ggplot(mydata, aes(x = factor(Activity_Father_RO7))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = brewer.pal(9, "PuBuGn")[7]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Father's Activity",
       x = "Father's Activity",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
bar6 <- ggplot(mydata, aes(x = factor(worker_status_Father_WS13))) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = brewer.pal(9, "PuBuGn")[8]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(title = "Father's Worker Status",
       x = "Father's Worker Status",
       y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(bar1, bar2, bar3, bar4,bar5, bar6, ncol = 3, nrow = 2)


#bivariate tables
#bivariate table: enrollment and disability
bivariate_table1 <- xtabs(~ Enrollment + Disability, data = mydata)
bivariate_table1
bivariate_table1 <- prop.table(bivariate_table1, margin = 1)*100
bivariate_table1<- as.data.frame(bivariate_table1)
plot1<- ggplot(bivariate_table1, aes(x = Enrollment, y = Freq, fill = Disability)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Enrollment and Disability", x = "Enrollment Status", y = "Relative Proportion (%)") +
  scale_fill_brewer(palette = "BuPu")

bivariate_table3 <- xtabs(~ Enrollment + Rural_Urban, data = mydata)
bivariate_table3 <- prop.table(bivariate_table3, margin = 1)*100
bivariate_table3<- as.data.frame(bivariate_table3)
plot2 <- ggplot(bivariate_table3, aes(x = Enrollment, y = Freq, fill = Rural_Urban)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Enrollment and Region", x = "Enrollment Status", y = "Relative Proportion (%)")+
  scale_fill_brewer(palette = "BuPu")

bivariate_table4 <- xtabs(~ Enrollment + Religion_ID11, data = mydata)
bivariate_table4 <- prop.table(bivariate_table4, margin = 1)*100
bivariate_table4<- as.data.frame(bivariate_table4)
plot3 <- ggplot(bivariate_table4, aes(x = Enrollment, y = Freq, fill = Religion_ID11)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Enrollment and Religion", x = "Enrollment Status", y = "Relative Proportion (%)")+
  scale_fill_brewer(palette = "BuPu")

bivariate_table5 <- xtabs(~ Enrollment + Main_incomesourceHH_ID14, data = mydata)
bivariate_table5 <- prop.table(bivariate_table5, margin = 1)*100
bivariate_table5<- as.data.frame(bivariate_table5)
plot4 <- ggplot(bivariate_table5, aes(x = Enrollment, y = Freq, fill = Main_incomesourceHH_ID14)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Enrollment and Main income source of household", x = "Enrollment Status", y = "Relative Proportion (%)")+
  scale_fill_brewer(palette = "BuPu")

grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
# 
# bivariate_table6 <- xtabs(~ Education_Mother_ED6, data = mydata)
# 
# bivariate_table6 <- prop.table(bivariate_table6, margin = 1)*100
# bivariate_table6<- as.data.frame(bivariate_table6)



# other bivariate tables
# bivariate_table3 <- xtabs(~ Education_Mother_ED6, data = mydata)
# bivariate_table3
# bivariate_table3 <- round(prop.table(bivariate_table3), digits =4)
# bivariate_table3

#Chi square test of independence
chi1 <- chisq.test(mydata$Enrollment, mydata$Disability)
chi1
chi2 <- chisq.test( mydata$Distancetoschool_CS5, mydata$Main_incomesourceHH_ID14)
chi2
chisq.test(mydata$Enrollment, mydata$Sex_RO3)
chisq.test(mydata$Enrollment, mydata$Rural_Urban)
chisq.test(mydata$Enrollment, mydata$Religion_ID11)
chisq.test(mydata$Enrollment, mydata$Caste_ID13)
chisq.test(mydata$Enrollment, mydata$Main_incomesourceHH_ID14)
chisq.test(mydata$Enrollment, mydata$Education_Father_ED6)
chisq.test(mydata$Enrollment, mydata$Education_Mother_ED6)


#there is association

mydata_disabled <- mydata %>%
  filter(Disability == "disabled") %>%
  filter(Enrollment == 1)
mydata_notdisabled <- mydata %>%
  filter(Disability == "not_disabled") %>%
  filter(Enrollment == 1)

t.test(0.74,0.88)


#MODEL 1: ORIGINAL PLAN REGRESSION
#changing the reference categories
mydata$Enrollment <- relevel(mydata$Enrollment, ref = "not_enrolled")
mydata$Disability <- relevel(mydata$Disability, ref = "not_disabled")
mydata$Sex_RO3 <- relevel(mydata$Sex_RO3, ref = "male")
mydata$Rural_Urban <- relevel(mydata$Rural_Urban, ref = "rural")
mydata$Religion_ID11 <- relevel(mydata$Religion_ID11, ref = "hindu")
mydata$Caste_ID13 <- relevel(mydata$Caste_ID13, ref = "general")
mydata$Main_incomesourceHH_ID14 <- relevel(mydata$Main_incomesourceHH_ID14, ref = "agriculture")
mydata$Activity_Father_RO7 <- relevel(mydata$Activity_Father_RO7, ref = "agriculture")
mydata$worker_status_Father_WS13 <- relevel(mydata$worker_status_Father_WS13, ref = "casual")
mydata$Education_Father_ED6 <- relevel(mydata$Education_Father_ED6, ref = "none")
mydata$Activity_Mother_RO7 <- relevel(mydata$Activity_Mother_RO7, ref = "homemaker")
mydata$worker_status_Mother_WS13 <- relevel(mydata$worker_status_Mother_WS13, ref = "casual")
mydata$Education_Mother_ED6 <- relevel(mydata$Education_Mother_ED6, ref = "none")

#regression:
# logit1 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13, data = mydata, family = "binomial")
# summary(logit1)
# exp(coef(logit1))
# logit2 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14, data = mydata, family = "binomial")
# summary(logit2)
#exp(coef(logit2))
# logit3 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14 + Education_Father_ED6 + Activity_Father_RO7 + Education_Mother_ED6 + Activity_Mother_RO7, data = mydata, family = "binomial")
# summary(logit3)
#exp(coef(logit3))
# tablefit <- stargazer (logit1, type="html", apply.coef = exp)
# summary(tablefit)
# print(tablefit)
# writeLines(tablefit, "regressiontrial1.html")

#getting output without the *** errors
logit1 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13, data = mydata,family=binomial(link="logit"))
logit2 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14, data = mydata, family = "binomial")
logit3 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14 + Education_Father_ED6 + Activity_Father_RO7 + worker_status_Father_WS13 + Education_Mother_ED6 + Activity_Mother_RO7 + worker_status_Mother_WS13, data = mydata, family = "binomial")
model <- list(logit1, logit2, logit3)
stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 4])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}
tablefit <- stargazer2(model, odd.ratio = T, type = "html") 
summary(tablefit)
print(tablefit)
writeLines(tablefit, "regressiontrial11dec.html")

#interaction models
#disability and region
interaction11 = glm(Enrollment ~ Disability*Rural_Urban + Disability + Rural_Urban +  Sex_RO3 + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14 + Education_Father_ED6 + Activity_Father_RO7 + worker_status_Father_WS13 + Education_Mother_ED6 + Activity_Mother_RO7 + worker_status_Mother_WS13, data = mydata, family = "binomial")
model <- list(interaction11)
stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 4])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}
tablefit <- stargazer2(model, odd.ratio = T, type="html")
summary(tablefit)
print(tablefit)
writeLines(tablefit, "interaction1.html")

#disability and sex
interaction2 = glm(Enrollment ~ Disability*Sex_RO3 + Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14 + Education_Father_ED6 + Activity_Father_RO7 + worker_status_Father_WS13 + Education_Mother_ED6 + Activity_Mother_RO7 + worker_status_Mother_WS13, data = mydata, family = "binomial")

model <- list(interaction2)
stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 4])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}
tablefit <- stargazer2(model, odd.ratio = T, type="html")
summary(tablefit)
print(tablefit)
writeLines(tablefit, "interaction2.html")


#interaction trial
interactiontrial = glm(Enrollment ~ Disability + Rural_Urban + Disability*Rural_Urban, data = mydata, family = "binomial")
model <- list(interactiontrial)
stargazer2 <- function(model, odd.ratio = F, ...) {
  if(!("list" %in% class(model))) model <- list(model)
  
  if (odd.ratio) {
    coefOR2 <- lapply(model, function(x) exp(coef(x)))
    seOR2 <- lapply(model, function(x) exp(coef(x)) * summary(x)$coef[, 4])
    p2 <- lapply(model, function(x) summary(x)$coefficients[, 4])
    stargazer(model, coef = coefOR2, se = seOR2, p = p2, ...)
    
  } else {
    stargazer(model, ...)
  }
}
tablefit <- stargazer2(model, odd.ratio = T, type="html")
summary(tablefit)
print(tablefit)
writeLines(tablefit, "interactiontrial.html")


#bootstrapped standard errors
boot_reg <- function(data, idx){
  m3 <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + prop_hh_internet + cr_rate_women + prop_hh_scooter + prop_hindu + prop_muslim + prop_sc, data = df5[idx,])
  coef(m3)
}
b <- boot::boot(df5, boot_reg, 1000)
boot::boot.ci(b, type = "perc")
boot::boot.ci(b, index = 2, type = "perc")
summary(m3)
summary(b)

rm(logit1, logit2, logit3)
#LR test
#simplest model
logit1 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13, data = na.omit(mydata[ , all.vars(formula(logit3))]),family= "binomial")
#intermediate model
logit2 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14, data = na.omit(mydata[ , all.vars(formula(logit3))]), family = "binomial")
#full model
logit3 = glm(Enrollment ~ Disability + Sex_RO3 + Rural_Urban + Religion_ID11 + Caste_ID13 + Main_incomesourceHH_ID14 + Education_Father_ED6 + Activity_Father_RO7 + worker_status_Father_WS13 + Education_Mother_ED6 + Activity_Mother_RO7 + worker_status_Mother_WS13, data = mydata, family = binomial())
#LR test between logit1 and logit2
lrtest(logit3, logit2) #p value less than 0.05 - prefer the full model
lrtest(logit3, logit1) #p value less than 0.05 - prefer the full model
lrtest(logit2,logit1) #p value less than 0.05 - prefer the full model




