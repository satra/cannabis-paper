### Load Libraries ###

library("lmerTest")
library("ggplot2")
library("stats")
library("sjPlot")
library("ordinal")




### Data Preparation ###

#get data
cannabis <- read.csv('cannabis_data_for_modeling.csv', header = TRUE, stringsAsFactors = TRUE)

head(cannabis)

#check types are correct
str(cannabis)

#flip the session
cannabis$session <- factor(cannabis$session, levels = c("baseline", "1year"))

#check types are correct
str(cannabis)

#convert factor to character to be able to appropriately encode missing values as NA and then reconvert to factor
cannabis$Positive.urine.THC <- as.character(cannabis$Positive.urine.THC)
cannabis$Positive.urine.THC[cannabis$Positive.urine.THC == ""] <- NA
cannabis$Positive.urine.THC <- factor(cannabis$Positive.urine.THC, levels = c("False", "True", NA))

#check the structure of the data frame after modification
str(cannabis)

#add numeric binary for urine THC
cannabis$Positive.urine.THC_numeric <- ifelse(cannabis$Positive.urine.THC == "True", 1, 0)

#check the structure of the data frame after modification
str(cannabis)

#convert frequency to ordered variable
cannabis$THC.frequency.per.month_ordered <- factor(cannabis$THC.frequency.per.month, levels = c("Less than once a month", "Less than once every two weeks", "Less than once a week", "1-2 days a week", "3-4 days a week", "5-6 days a week", "Once or more per day"), ordered = TRUE)

#check the structure of the data frame after modification again
str(cannabis)

#add numeric binary for CUD diagnosis
cannabis$CUD.diagnosis_numeric <- ifelse(cannabis$CUD.diagnosis == "mild CUD diagnosis", 1, 0)

#check the structure of the data frame after modification again
str(cannabis)

#add a ordinal for CUDIT summed score
cannabis$CUDIT.summed.score_remainder = 32 - cannabis$CUDIT.summed.score

#check the structure of the data frame after modification again
str(cannabis)

#make Sex into a numeric to standardize
cannabis$Sex_numeric <- ifelse(cannabis$Sex == "Male", 1, 0)

#check the structure of the data frame after modification again
str(cannabis)

#add standardized variables
cannabis$Age_standardized <- scale(cannabis$Age, center = TRUE, scale = TRUE)[,1]
cannabis$Sex_numeric_standardized <- scale(as.numeric(cannabis$Sex_numeric), center = TRUE, scale = TRUE)[,1]

#check the structure of the data frame after modification again
str(cannabis)




### Modeling ###


## Proportions: CUD diagnosis

#get proportions for reporting
contingency_table_CUD_dx = table(cannabis$session, cannabis$CUD.diagnosis)
contingency_table_CUD_dx
prop.table(contingency_table_CUD_dx, margin = 1)

#no need to do repeated measures logistic regression since everyone by definition did not have CUD at baseline 



## Comparison across time: THC urine

#quick EDA                                                      
contingency_table_THC_urine = table(cannabis$session, cannabis$Positive.urine.THC)
contingency_table_THC_urine
prop.table(contingency_table_THC_urine, margin = 1)

#logistic regression model
fit_THC_urine <- glmer(Positive.urine.THC_numeric ~ session + (1|subs), data = cannabis, family = binomial)

#summary results
summary(fit_THC_urine)
confint(fit_THC_urine,method = "Wald")

#effects plots
plot_model(fit_THC_urine, type = "pred", terms = "session[all]")

#model with covariates
fit_THC_urine_cov <- glmer(Positive.urine.THC_numeric 
                           ~ session + Age_standardized + Sex_numeric_standardized + (1|subs), data = cannabis, family = binomial)
#summary results
summary(fit_THC_urine_cov)
confint(fit_THC_urine_cov,method = "Wald")

#effects plots
plot_model(fit_THC_urine_cov, type = "pred", terms = "session[all]")



## Comparison across time: THC frequency

#quick EDA
contingency_table_THC_freq = table(cannabis$session, cannabis$THC.frequency.per.month_ordered)
contingency_table_THC_freq
prop.table(contingency_table_THC_freq, margin = 1)

spineplot(cannabis$THC.frequency.per.month_ordered ~ cannabis$session, xlab = "session", ylab = "THC frequency")

#ordinal regression model
fit_THC_freq <- clmm(THC.frequency.per.month_ordered ~ session + (1|subs), data = cannabis)       ## from the ordinal package

#summary results
summary(fit_THC_freq)  
confint(fit_THC_freq)

#effects plots
plot_model(fit_THC_freq, type = "pred", terms = "session[all]")

#model with covariates
fit_THC_freq_cov <- clmm(THC.frequency.per.month_ordered ~ session + Age_standardized + Sex_numeric_standardized + (1|subs), data = cannabis)       ## from the ordinal package

#summary results
summary(fit_THC_freq_cov)
confint(fit_THC_freq_cov)

#effects plots
plot_model(fit_THC_freq_cov, type = "pred", terms = "session[all]")



## Comparison across time: CUDIT score
#note this is a summed score with 8 questions, scores ranging from 0-32

#quick EDA
summary(cannabis$CUDIT.summed.score[cannabis$session == 'baseline'])
summary(cannabis$CUDIT.summed.score[cannabis$session == '1year'])

ggplot(cannabis, aes(x = CUDIT.summed.score, fill = factor(session))) +
  geom_histogram(position = "identity", alpha = 0.7, bins = 15) +
  facet_wrap(~session, scales = "free") +
  theme(legend.position = "none") + 
  labs(title = "CUDIT-R summed score by session", x = "CUDIT-R summed score", y = "Frequency")

#binomial regression model
fit_CUDIT_score <- glmer(cbind(CUDIT.summed.score, CUDIT.summed.score_remainder) 
                          ~ session + (1|subs), data = cannabis, family = binomial)

#summary results
summary(fit_CUDIT_score)
confint(fit_CUDIT_score)

#effects plots
plot_model(fit_CUDIT_score, type = "pred", terms = "session[all]")

#model with covariates
fit_CUDIT_score_cov <- glmer(cbind(CUDIT.summed.score, CUDIT.summed.score_remainder) 
                          ~ session + Age_standardized + Sex_numeric_standardized + (1|subs), data = cannabis, family = binomial)

#summary results
summary(fit_CUDIT_score_cov)
confint(fit_CUDIT_score_cov)

#effects plots
plot_model(fit_CUDIT_score_cov, type = "pred", terms = "session[all]")




### Benjamini-Hochberg Correction ###

#function to pull out p-values
extract_pvalue <- function(model, predictor) {
  coef_table <- coef(summary(model))
  p_value <- coef_table[predictor, "Pr(>|z|)"]
  return(p_value)
}

#list to store the models
models <- list(fit_THC_urine_cov, fit_THC_freq_cov, fit_CUDIT_score_cov)

#pull out p-values for the predictor session1year
p_values <- lapply(models, extract_pvalue, predictor = "session1year")

#apply Benjamini-Hochberg correction
adjusted_p_values <- p.adjust(p_values, method = "BH")

#print the adjusted p-values
print(adjusted_p_values)

