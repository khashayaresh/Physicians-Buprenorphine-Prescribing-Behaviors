df <- read.csv("/Users/eshtiaghi/Desktop/Khashi/Chapman/Research/My works/X-Waiver/Results/Final results/df_cleaned.csv")
# Load necessary libraries
library(dplyr)

colnames(df)
# Assuming 'df' is your dataset and it has been properly loaded

# Specify the columns with Likert scale data
likert_columns <- c(
  'I.feel.worried.about.diversion.or.misuse.of.buprenorphine.by.patients.for.non.medical.purposes.',
  'The.process.to.become.qualified.to.prescribe.buprenorphine.is.time.consuming.',
  'I.feel.patients.have.limited.access.to.OUDa.services.',
  'Administration.of.health.services.and.facilities.associated.with.OUDa.treatment.is.properly.managed.',
  'I.have.concerns.about.interference.from.the.DEAb.in.my.practice.',
  'The.required.training.for.prescribing.buprenorphine.through.California.Medical.Association..CMA..seems.challenging.',
  'I.have.adequate.staffing.to.properly.manage.patients.on.buprenorphine.',
  'The.reporting.requirements.to.regulatory.agencies.prevent.me.from.prescribing.buprenorphine.',
  'Stigma.does.NOT.prevent.me.from.treating.OUD.a',
  'I.feel.supported.by.the.medical.community.and.physician.peers.to.treat.OUD.a',
  'I.feel.the.reimbursement.for.prescribing.buprenorphine.is.appropriate.',
  'I.feel.adequately.trained.to.manage.patients.with.OUD.a',
  'In.my.practice..the.demand.for.buprenorphine.treatment.is.lacking.',
  'In.general..OUDa.access..e.g...naloxone..in.overdose.cases.are.sufficient.',
  'I.have.adequate.space.to.properly.manage.patients.on.buprenorphine.'
)

# Likert scale mapping to numeric values
likert_mapping <- c("Strongly disagree" = 1, "Disagree" = 2, "Neutral" = 3, "Agree" = 4, "Strongly agree" = 5)

# Convert the Likert scale columns to numeric based on the mapping
df_clean <- df %>%
  filter(Gender %in% c("Male", "Female")) %>%
  mutate(across(all_of(likert_columns), ~ as.numeric(factor(., levels = names(likert_mapping), labels = likert_mapping))))

# Initialize a list to store the normality test results
normality_results <- list()

# Perform normality tests (Shapiro-Wilk test) for each Likert item
for (col in likert_columns) {
  
  # Get the data for male and female groups
  male_data <- df_clean[[col]][df_clean$Gender == "Male"]
  female_data <- df_clean[[col]][df_clean$Gender == "Female"]
  
  # Check if there are valid non-NA values in both groups (at least 3 for the test to work)
  if (length(na.omit(male_data)) < 3 | length(na.omit(female_data)) < 3) {
    next  # Skip this item if there are too few data points for either group
  }
  
  # Perform Shapiro-Wilk test for both groups
  male_normality <- shapiro.test(male_data)$p.value
  female_normality <- shapiro.test(female_data)$p.value
  
  # Store the result
  normality_results[[col]] <- data.frame(
    Action = col,
    Male_Normality_p = round(male_normality, 4),
    Female_Normality_p = round(female_normality, 4)
  )
}

# Combine all results into a single data frame
normality_df <- do.call(rbind, normality_results)

# Print the results
print(normality_df)
