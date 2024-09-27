#Fisher exact tests for table 1 entries where the Chi-squared test wasn't appropriate due to less than 5 observation in a category



### Fisher Exact Tests ###


## race ##

#contingency table
race_contingency_table <- matrix(c(27, 3, 2, 19, 6, 4, 37, 2, 2), nrow = 3, byrow = TRUE)

# Perform Fisher's exact test
race_fisher_result <- fisher.test(race_contingency_table)

# Print results
print(race_fisher_result)



## ethnicity ##

#contingency table
ethnicity_contingency_table <- matrix(c(29, 3, 29, 0, 40, 1), nrow = 3, byrow = TRUE)

# Perform Fisher's exact test
ethnicity_fisher_result <- fisher.test(ethnicity_contingency_table)

# Print results
print(ethnicity_fisher_result)



## handedness ##

#contingency table
handedness_contingency_table <- matrix(c(29, 3, 25, 4, 35, 6), nrow = 3, byrow = TRUE)

# Perform Fisher's exact test
handedness_fisher_result <- fisher.test(handedness_contingency_table)

# Print results
print(handedness_fisher_result)

