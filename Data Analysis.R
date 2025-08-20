# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(broom)
library(openxlsx)

# Load the dataset
df <- read_csv("/Users/.../Dataset.csv")

# Display the first few rows and summary of the dataset
head(df)
str(df)

# List of Likert scale columns that need conversion based on dataset inspection
likert_columns <- c(
  'I feel worried about diversion or misuse of buprenorphine by patients for non-medical purposes.',
  'The process to become qualified to prescribe buprenorphine is time-consuming.',
  'I feel patients have limited access to OUDa services.',
  'Administration of health services and facilities associated with OUDa treatment is properly managed.',
  'I have concerns about interference from the DEAb in my practice.',
  'The required training for prescribing buprenorphine through California Medical Association (CMA) seems challenging.',
  'I have adequate staffing to properly manage patients on buprenorphine.',
  'The reporting requirements to regulatory agencies prevent me from prescribing buprenorphine.',
  'Stigma does NOT prevent me from treating OUD.a',
  'I feel supported by the medical community and physician peers to treat OUD.a',
  'I feel the reimbursement for prescribing buprenorphine is appropriate.',
  'I feel adequately trained to manage patients with OUD.a',
  'In my practice, the demand for buprenorphine treatment is lacking.',
  'In general, OUDa access (e.g., naloxone) in overdose cases are sufficient.',
  'I have adequate space to properly manage patients on buprenorphine.'
)

# Convert Likert scale responses to numeric values
likert_mapping <- c(
  "Strongly disagree" = 1,
  "Disagree" = 2,
  "Neutral" = 3,
  "Agree" = 4,
  "Strongly agree" = 5
)

df_clean <- df %>%
  mutate(across(all_of(likert_columns), ~ as.numeric(factor(.x, levels = names(likert_mapping), labels = likert_mapping))))

# Check for any remaining missing values in the Likert columns after conversion
missing_data_summary <- colSums(is.na(df_clean[likert_columns]))

# Check for how many males and females are in the dataset
gender_count <- table(df_clean$Gender)

missing_data_summary
gender_count

# Filter the dataset to only include male and female respondents
df_filtered <- df_clean %>%
  filter(Gender %in% c('Male', 'Female')) %>%
  drop_na(all_of(likert_columns))

# Confirm that df_filtered was created successfully
if(nrow(df_filtered) == 0) {
  stop("The filtered dataset is empty. Please check your filtering criteria.")
}

# Perform Shapiro-Wilk test for normality
normality_results <- sapply(likert_columns, function(col) {
  male_data <- df_filtered %>% filter(Gender == 'Male') %>% pull(col)
  female_data <- df_filtered %>% filter(Gender == 'Female') %>% pull(col)
  
  # Check if there are enough data points for the test
  if(length(male_data) < 3 || length(female_data) < 3) {
    return(c(Male_Normality_p = NA, Female_Normality_p = NA))
  }
  
  male_normality_p <- shapiro.test(male_data)$p.value
  female_normality_p <- shapiro.test(female_data)$p.value
  
  c(Male_Normality_p = round(male_normality_p, 4), Female_Normality_p = round(female_normality_p, 4))
}, simplify = FALSE)

# Convert the results to a DataFrame for display
normality_df <- as.data.frame(do.call(rbind, normality_results))
row.names(normality_df) <- likert_columns

# Display the normality test results
print(normality_df)
View(normality_df)

#t-test or Wilcoxon test:
# Initialize a list to store test results
test_results <- list()

# Perform t-test or Wilcoxon test based on the normality results for each Likert item
for (col in likert_columns) {
  male_data <- df_filtered %>% filter(Gender == 'Male') %>% pull(col)
  female_data <- df_filtered %>% filter(Gender == 'Female') %>% pull(col)
  
  # Check normality for both groups
  male_normality_p <- normality_df[col, 'Male_Normality_p']
  female_normality_p <- normality_df[col, 'Female_Normality_p']
  
  # Calculate means and standard deviations
  mean_male <- round(mean(male_data, na.rm = TRUE), 2)
  mean_female <- round(mean(female_data, na.rm = TRUE), 2)
  sd_male <- round(sd(male_data, na.rm = TRUE), 2)
  sd_female <- round(sd(female_data, na.rm = TRUE), 2)
  
  # Combine mean and standard deviation in the desired format
  mean_sd_male <- paste(mean_male, "±", sd_male)
  mean_sd_female <- paste(mean_female, "±", sd_female)
  
  # If both groups pass the normality test, perform t-test
  if (male_normality_p > 0.05 && female_normality_p > 0.05) {
    test_result <- t.test(male_data, female_data, var.equal = TRUE) # Assuming equal variances
    test_type <- "t-test"
  } else {
    # If either group fails the normality test, perform Wilcoxon rank-sum test
    test_result <- wilcox.test(male_data, female_data, exact = FALSE) # approximate p-value
    test_type <- "Wilcoxon"
  }
  
  # Store the test results
  test_results[[col]] <- list(
    Test_Type = test_type,
    Test_Statistic = round(test_result$statistic, 4),
    p_value = round(test_result$p.value, 4),
    Mean_Male = mean_sd_male,
    Mean_Female = mean_sd_female
  )
}

# Convert the results to a DataFrame for display
test_results_df <- bind_rows(test_results, .id = "Likert_Item")

# Display the test results
print(test_results_df)



#Statistical Tests Among different Demographic Groups:

# 'Racial group', 'Ethnicity', 'Primary specialty', 'Type of community':
library(stats)
install.packages('FSA')  # for aov, kruskal.test, TukeyHSD
library(FSA)             # for Dunn test

# List of demographic columns to analyze
demographic_columns <- c('Racial group', 'Ethnicity', 'Primary specialty', 'Type of community')

# Perform normality test (Shapiro-Wilk) for each Likert item across each demographic group
normality_results_demographic <- list()

for (col in likert_columns) {
  for (demo_col in demographic_columns) {
    # Prepare the data by removing rows with missing values in the Likert or demographic column
    df_demo_filtered <- df_clean %>% select(all_of(c(col, demo_col))) %>% drop_na()
    
    # Group the data by the demographic column
    groups <- unique(df_demo_filtered[[demo_col]])
    
    # Initialize flags to store normality checks
    normality_check <- list()
    
    # Perform Shapiro-Wilk normality test for each group
    for (group in groups) {
      group_data <- df_demo_filtered %>% filter(!!sym(demo_col) == group) %>% pull(col)
      
      if (length(group_data) >= 3) {  # Ensure at least 3 data points for normality testing
        if (length(unique(group_data)) > 1) {  # Check for variability in data
          normality_p_value <- shapiro.test(group_data)$p.value
          normality_check[[group]] <- normality_p_value > 0.05  # TRUE if normal, FALSE if not
        } else {
          normality_check[[group]] <- FALSE  # No variability in data
        }
      } else {
        normality_check[[group]] <- FALSE  # Too few data points
      }
    }
    
    # If all groups are normally distributed, flag ANOVA; else, flag Kruskal-Wallis
    use_anova <- all(unlist(normality_check))
    
    normality_results_demographic[[paste(col, demo_col, sep = " - ")]] <- list(
      Use_ANOVA = use_anova,
      Normality_Check = normality_check
    )
  }
}

# Proceed with the correct test based on normality (ANOVA or Kruskal-Wallis)
anova_or_kruskal_results <- list()
post_hoc_results <- list()

for (col in likert_columns) {
  for (demo_col in demographic_columns) {
    df_demo_filtered <- df_clean %>% select(all_of(c(col, demo_col))) %>% drop_na()
    
    # Ensure that the demographic column is treated as a factor
    df_demo_filtered[[demo_col]] <- as.factor(df_demo_filtered[[demo_col]])
    
    groups <- unique(df_demo_filtered[[demo_col]])
    
    # Initialize lists to store means and standard deviations
    group_means_sd <- list()
    
    for (group in groups) {
      group_data <- df_demo_filtered %>% filter(!!sym(demo_col) == group) %>% pull(col)
      group_mean <- round(mean(group_data, na.rm = TRUE), 2)
      group_sd <- round(sd(group_data, na.rm = TRUE), 2)
      group_means_sd[[group]] <- paste(group, ": ", group_mean, "±", group_sd)  # Label each group with mean ± SD
    }
    
    if (length(groups) > 1) {
      if (normality_results_demographic[[paste(col, demo_col, sep = " - ")]]$Use_ANOVA) {
        # Perform ANOVA
        anova_result <- aov(as.formula(paste(col, "~", demo_col)), data = df_demo_filtered)
        f_stat <- summary(anova_result)[[1]]$`F value`[1]
        p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
        test_type <- "ANOVA"
        
        # If p-value is significant, perform Tukey's HSD post-hoc test
        if (p_value < 0.05) {
          tukey_result <- TukeyHSD(anova_result)
          post_hoc_results[[paste(col, demo_col, sep = " - ")]] <- tukey_result[[demo_col]]
        }
        
      } else {
        # Perform Kruskal-Wallis
        kruskal_result <- kruskal.test(df_demo_filtered[[col]], df_demo_filtered[[demo_col]])
        f_stat <- kruskal_result$statistic
        p_value <- kruskal_result$p.value
        test_type <- "Kruskal-Wallis"
        
        # If p-value is significant, perform Dunn's post-hoc test
        if (p_value < 0.05) {
          dunn_result <- dunnTest(df_demo_filtered[[col]], df_demo_filtered[[demo_col]], method = "bonferroni")
          post_hoc_results[[paste(col, demo_col, sep = " - ")]] <- dunn_result$res
        }
      }
      
      anova_or_kruskal_results[[paste(col, demo_col, sep = " - ")]] <- list(
        Test_Type = test_type,
        Test_Statistic = round(f_stat, 4),
        p_value = round(p_value, 4),
        Group_Means_SD = paste(group_means_sd, collapse = "; ")  # Join means and SD for each group, labeled
      )
    }
  }
}

# Convert results to DataFrame for display
anova_or_kruskal_df <- bind_rows(anova_or_kruskal_results, .id = "Demographic_Comparison")

# Display the results
print(anova_or_kruskal_df)

# If any post-hoc tests were performed, display the results
if (length(post_hoc_results) > 0) {
  post_hoc_df <- bind_rows(post_hoc_results, .id = "Demographic_Comparison")
  print(post_hoc_df)
  View(post_hoc_df)
  write.xlsx(post_hoc_df, file.path("/Users/eshtiaghi/Desktop/Khashi/Chapman/Research/My works/X-Waiver/Results/Final results/Sep 13", "post_hoc_df.xlsx"), rowNames = FALSE)
}

# Create separate tables for each demographic category based on the updated results
racial_group_df_corrected <- anova_or_kruskal_df %>% filter(grepl('Racial group', Demographic_Comparison))
ethnicity_df_corrected <- anova_or_kruskal_df %>% filter(grepl('Ethnicity', Demographic_Comparison))
primary_specialty_df_corrected <- anova_or_kruskal_df %>% filter(grepl('Primary specialty', Demographic_Comparison))
community_type_df_corrected <- anova_or_kruskal_df %>% filter(grepl('Type of community', Demographic_Comparison))

# Display each table to the user
print(racial_group_df_corrected)
View(racial_group_df_corrected)
print(ethnicity_df_corrected)
print(primary_specialty_df_corrected)
print(community_type_df_corrected)



# Statistical Test between Physicians with different DATA-Waiver status:
# Filter the dataset for "Yes" and "No" responses in the DATA-Waiver column
df_data_waiver_filtered <- df_clean %>%
  filter(`DATA-Waiver` %in% c('Yes', 'No'))

# Initialize a list to store the results
data_waiver_results <- list()

# Perform normality test and appropriate comparison for each Likert item
for (col in likert_columns) {
  yes_data <- df_data_waiver_filtered %>% 
    filter(`DATA-Waiver` == 'Yes') %>% 
    pull(col) %>% 
    na.omit()  # Remove NA values
  
  no_data <- df_data_waiver_filtered %>% 
    filter(`DATA-Waiver` == 'No') %>% 
    pull(col) %>% 
    na.omit()  # Remove NA values
  
  # Check if both groups have enough non-missing observations for the test
  if (length(yes_data) >= 3 && length(no_data) >= 3) {
    
    # Initialize p-values for normality test
    yes_normality_p <- NA
    no_normality_p <- NA
    
    # Perform Shapiro-Wilk normality test if sample size is within the valid range
    if (length(yes_data) <= 5000) {
      yes_normality_p <- tryCatch({
        shapiro.test(yes_data)$p.value
      }, error = function(e) NA)  # Handle errors in Shapiro-Wilk test
    }
    if (length(no_data) <= 5000) {
      no_normality_p <- tryCatch({
        shapiro.test(no_data)$p.value
      }, error = function(e) NA)  # Handle errors in Shapiro-Wilk test
    }
    
    # Determine if we should use t-test or Wilcoxon based on normality
    if (!is.na(yes_normality_p) && !is.na(no_normality_p) &&
        yes_normality_p > 0.05 && no_normality_p > 0.05) {
      # Both groups pass the normality test -> Perform t-test
      test_result <- t.test(yes_data, no_data, na.rm = TRUE)
      test_type <- "t-test"
      test_stat <- test_result$statistic
      p_value <- test_result$p.value
    } else {
      # At least one group fails the normality test or sample size too small -> Perform Wilcoxon rank-sum test
      test_result <- wilcox.test(yes_data, no_data, exact = FALSE)
      test_type <- "Wilcoxon"
      test_stat <- test_result$statistic
      p_value <- test_result$p.value
    }
    
    # Calculate means and standard deviations
    mean_yes <- round(mean(yes_data, na.rm = TRUE), 2)
    mean_no <- round(mean(no_data, na.rm = TRUE), 2)
    sd_yes <- round(sd(yes_data, na.rm = TRUE), 2)
    sd_no <- round(sd(no_data, na.rm = TRUE), 2)
    
    # Store the results with mean ± SD
    data_waiver_results[[col]] <- list(
      Test_Type = test_type,
      Test_Statistic = round(test_stat, 4),
      p_value = round(p_value, 4),
      Mean_Yes = paste(mean_yes, "±", sd_yes),
      Mean_No = paste(mean_no, "±", sd_no)
    )
  } else {
    # If there are not enough valid observations, store NA
    mean_yes <- round(mean(yes_data, na.rm = TRUE), 2)
    mean_no <- round(mean(no_data, na.rm = TRUE), 2)
    sd_yes <- round(sd(yes_data, na.rm = TRUE), 2)
    sd_no <- round(sd(no_data, na.rm = TRUE), 2)
    
    data_waiver_results[[col]] <- list(
      Test_Type = "Not enough data",
      Test_Statistic = NA,
      p_value = NA,
      Mean_Yes = paste(mean_yes, "±", sd_yes),
      Mean_No = paste(mean_no, "±", sd_no)
    )
  }
}

# Convert the results to a DataFrame for display
data_waiver_results_df <- do.call(rbind, lapply(data_waiver_results, as.data.frame))
rownames(data_waiver_results_df) <- names(data_waiver_results)

# Display the results
print(data_waiver_results_df)
View(data_waiver_results_df)

