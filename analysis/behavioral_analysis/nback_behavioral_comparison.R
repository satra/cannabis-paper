### Load Libraries ###

library("lmerTest")
library("ggplot2")
library("stats")
library("sjPlot")




### Data Preparation ###

nback_baseline <- read.csv('nback_data_for_modeling_HC_MM_baseline_comparison.csv', header = TRUE, stringsAsFactors = TRUE)
nback_paired <- read.csv('nback_data_for_modeling_MM_paired_comparison.csv', header = TRUE, stringsAsFactors = TRUE)

head(nback_baseline)
head(nback_paired)

#check types are correct
str(nback_baseline)
str(nback_paired)

#flip the session
nback_paired$session <- factor(nback_paired$session, levels = c("baseline", "1year"))

#check types are correct
str(nback_paired)

#make Sex into a numeric to standardize
nback_baseline$Sex_numeric <- ifelse(nback_baseline$Sex == "Male", 1, 0)
nback_paired$Sex_numeric <- ifelse(nback_paired$Sex == "Male", 1, 0)

#check the structure of the data frame after modification again
str(nback_baseline)
str(nback_paired)

#add standardized variables
nback_baseline$Age_standardized <- scale(nback_baseline$Age, center = TRUE, scale = TRUE)[,1]
nback_paired$Age_standardized <- scale(nback_paired$Age, center = TRUE, scale = TRUE)[,1]
nback_baseline$Sex_numeric_standardized <- scale(as.numeric(nback_baseline$Sex_numeric), center = TRUE, scale = TRUE)[,1]
nback_paired$Sex_numeric_standardized <- scale(as.numeric(nback_paired$Sex_numeric), center = TRUE, scale = TRUE)[,1]

#check the structure of the data frame after modification again
str(nback_baseline)
str(nback_paired)

#add a group column for the baseline comparison
nback_baseline$subs_char <- as.character(nback_baseline$subs)
nback_baseline$group <- as.factor(ifelse(startsWith(nback_baseline$subs_char, 'MM'), 'MM', 'HC'))

#check the structure of the data frame after modification again
str(nback_baseline)




### Modeling ###

#make list of nback behavioral measures
nback_measures <- c("ACC_all", "ACC_0b", "ACC_2b", "RT_all_cor", "RT_0b_cor", "RT_2b_cor")


## Baseline Comparison

#create empty list to store the regression summaries and p values for the final model with covariates
baseline_comparison_regression_summaries <- list()
baseline_comparison_p_values <- list()

#loop over each nback behavioral measure
for (measure in nback_measures) {
  
  #quick EDA
  cat("Summary of", measure, "for group HC:\n")
  print(summary(nback_baseline[[measure]][nback_baseline$group == 'HC']))
  
  cat("Summary of", measure, "for group MM:\n")
  print(summary(nback_baseline[[measure]][nback_baseline$group == 'MM']))
  
  measure_range <- range(nback_baseline[[measure]], na.rm = TRUE)
  
  p <- ggplot(nback_baseline, aes_string(x = measure, fill = "factor(group)")) +
    geom_histogram(position = "identity", alpha = 0.7, bins = 15) +
    facet_wrap(~group, scales = "free") +
    theme(legend.position = "none") + 
    labs(title = paste(measure, "by group"), y = "Frequency") +
    xlim(measure_range)
  
  print(p)
  
  #linear regression model
  formula <- as.formula(paste(measure, "~ group"))
  fit <- lm(formula, data = nback_baseline)
  
  #summary results
  cat("Summary of the linear model for", measure, ":\n")
  print(summary(fit))
  cat("Confidence intervals for the linear model for", measure, ":\n")
  print(confint(fit))
  
  #effects plot
  plot_model(fit, type = "pred", terms = "group[all]")
  
  #model with covariates
  formula_cov <- as.formula(paste(measure, "~ group + Age_standardized + Sex_numeric_standardized"))
  fit_cov <- lm(formula_cov, data = nback_baseline)
  
  #save model output
  baseline_comparison_regression_summaries[[measure]] <- summary(fit_cov)
  
  #summary results
  cat("Summary of the linear model for", measure, " with covariates:\n")
  print(summary(fit_cov))
  cat("Confidence intervals for the linear model for", measure, " with covariates:\n")
  print(confint(fit_cov))
  
  #effects plots
  plot_model(fit_cov, type = "pred", terms = "group[all]")
  
  #extract p-value for 'session' from the model with covariates
  p_value <- coef(summary(fit_cov))["groupMM", "Pr(>|t|)"]
  baseline_comparison_p_values[[measure]] <- p_value
}




## Paired Comparison

#create empty list to store the regression summaries and p values for the final model with covariates
paired_MM_comparison_regression_summaries <- list()
paired_MM_comparison_p_values <- list()

#loop over each nback behavioral measure
for (measure in nback_measures) {
  
  #quick EDA
  cat("Summary of", measure, "for session baseline:\n")
  print(summary(nback_paired[[measure]][nback_paired$session == 'baseline']))
  
  cat("Summary of", measure, "for session 1year:\n")
  print(summary(nback_paired[[measure]][nback_paired$session == '1year']))
  
  measure_range <- range(nback_paired[[measure]], na.rm = TRUE)
  
  p <- ggplot(nback_paired, aes_string(x = measure, fill = "factor(session)")) +
    geom_histogram(position = "identity", alpha = 0.7, bins = 15) +
    facet_wrap(~session, scales = "free") +
    theme(legend.position = "none") + 
    labs(title = paste(measure, "by session"), y = "Frequency") +
    xlim(measure_range)
  
  print(p)
  
  #linear mixed-effects model
  formula <- as.formula(paste(measure, "~ session + (1|subs)"))
  fit <- lmer(formula, data = nback_paired)

  #summary results
  cat("Summary of the linear mixed model for", measure, ":\n")
  print(summary(fit))
  cat("Confidence intervals for the linear mixed model for", measure, ":\n")
  print(confint(fit))
  
  #effects plot
  plot_model(fit, type = "pred", terms = "session[all]")
  
  #model with covariates
  formula_cov <- as.formula(paste(measure, "~ session + Age_standardized + Sex_numeric_standardized + (1|subs)"))
  fit_cov <- lmer(formula_cov, data = nback_paired)
  
  #save model output
  paired_MM_comparison_regression_summaries[[measure]] <- summary(fit_cov)
  
  #summary results
  cat("Summary of the linear mixed model with covariates for", measure, ":\n")
  print(summary(fit_cov))
  cat("Confidence intervals for the linear mixed model with covariates for", measure, ":\n")
  print(confint(fit_cov))
  
  #effects plot
  plot_model(fit_cov, type = "pred", terms = "session[all]")
  
  #extract p-value for 'session' from the model with covariates
  p_value <- coef(summary(fit_cov))["session1year", "Pr(>|t|)"]
  paired_MM_comparison_p_values[[measure]] <- p_value
}




### Benjamini-Hochberg Correction ###

## Baseline Comparison

#apply Benjamini-Hochberg correction
baseline_comparison_adjusted_p_values <- p.adjust(baseline_comparison_p_values, method = "BH")

#print the adjusted p-values
print(baseline_comparison_adjusted_p_values)


## Paired Comparison

#apply Benjamini-Hochberg correction
paired_MM_adjusted_p_values <- p.adjust(paired_MM_comparison_p_values, method = "BH")

#print the adjusted p-values
print(paired_MM_adjusted_p_values)

