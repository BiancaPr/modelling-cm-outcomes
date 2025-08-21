
library(nhanesA)

# creating a vector of all the needed tables from 2017-2018
var_list = c('DEMO_J', 'BMX_J', 'BPX_J', 'SMQ_J','ALQ_J',
             'PAQ_J', 'HDL_J', 'GLU_J', 'DIQ_J', 'TCHOL_J')

# storing selected nhanes table into a new list
nhanes_tables <- lapply(var_list, nhanes)

# merging all into a single wide data frame
all_data <- Reduce(function(x, y) merge(x, y, by = 'SEQN', all.x = TRUE), nhanes_tables)
View(all_data)



range(all_data$RIDAGEYR, na.rm = TRUE)

# loading dplyr library
library(dplyr)

# table(all_data$RIAGENDR)



# filtering and selecting necessary raw variable to create a prepped data frame 
prepped_data <- all_data %>%
  # filtering for study population
  filter(RIAGENDR == 'Female', RIDAGEYR >= 20) %>%

  
  # selecting all the raw columns needed
  select(
    SEQN, #resp id 
    RIAGENDR, 
    RIDAGEYR,
    RIDRETH3, # race/ethnicity
    DMDEDUC2, # education
    DMDMARTL, # marital status
    INDHHIN2, # household family income
    INDFMPIR, # ratio of family income to poverty 
    SDMVPSU,  # survey design psu
    SDMVSTRA, # survey design stratum 
    WTMEC2YR, # survey weight
    
    # clinical measures for creatin predictors and targets
    DIQ010, # diabetes diagnosis
    LBDTCSI, # total cholesterol
    LBDHDDSI, # HDL cholesterol
    LBDGLUSI, # fasting glucose - mmol/L
    BPXSY2, # systolic blood pressure
    BPXDI2, # diastolic plood pressure
    BMXBMI, # body mass index 
    BMXWAIST, # waist circumference in cm
    
    # Questionnaire variables
    starts_with('SMQ'),
    starts_with('SMD'),
    starts_with('ALQ'),
    starts_with('PAQ'),
    starts_with('PAD')
  )
head(prepped_data$SEQN)
count(prepped_data)
# n = 2867


                          # --- EDA ---

# calculating the percentage of missing values for each column
missing_vals <- colMeans(is.na(prepped_data)) * 100


# sort the results to view the columns with the most missing data first
sort(missing_vals, decreasing = TRUE)
  # a significant number of variables for SMQ, ALQ ana PAQ show high percentage of missingness
  # 80-100% though not a concern and is expected due to the survey design/skip logic
  # key clinical variable for fasting glucose (LBDGLUSI) missing ~ 56% but this
  # is also consistent with survey design and sampling. The core clinical predictors have
  # lower missingness, between 6.5% - 12%


# converting missing vals to a df and exporting as a csv
missing_df <- data.frame(
  variable = names(missing_vals),
  missing_pct = as.numeric(missing_vals)
)
    # updated to include marital status and changed HHI measure
library(readr)

#write.csv(missing_df, 'missingness_results.csv', row.names = FALSE)



    # summary stats for key continuous variables
summary(prepped_data$BMXBMI)
summary(prepped_data$BMXWAIST)
summary(prepped_data$BPXDI2)
summary(prepped_data$BPXSY2)
summary(prepped_data$LBDTCSI)
summary(prepped_data$LBDHDDSI)
summary(prepped_data$LBDGLUSI)
    # Mean vs median: mean and median values were reasonably close, suggesting no skewness distributions
    # Min/Max: the ranges are generally plausible with only diastolic blood pressure (BPXDI2)
        # minimum value 0 being the exception. This is like a code for an unobtainable reading
# NA's: The level of missingness is consistent with the initial check


# visualising distributions 
library(GGally)

eda_vars <- prepped_data %>%
  select(BMXBMI, BMXWAIST, BPXSY2, BPXDI2, LBDTCSI, LBDHDDSI, LBDGLUSI)
ggpairs(eda_vars)
  # note added to word doc

# summarising key categorical variables
prepped_data %>%
  select(RIDRETH3,DMDEDUC2, DMDMARTL, INDHHIN2) %>%
  lapply(table)
  # the distribution of the categorical variables appears reasonable for analysis
  # the values for don't know or refused is sparse so will handle as missing data in pre-processing

# visualising relations by reshaping data and create a faceted box plot for all continuous variables

library(tidyr)
library(ggplot2)

# clinical measures by education level
prepped_data %>% 
  select(
    DMDEDUC2, 
    BMXBMI, BMXWAIST,
    BPXSY2, BPXDI2,
    LBDTCSI, LBDHDDSI, LBDGLUSI
  ) %>%
  
  pivot_longer(
    cols = -DMDEDUC2,
    names_to = 'clinical_measure',
    values_to = 'value'
  ) %>%
  
  ggplot(aes(x = DMDEDUC2, y = value)) +
    geom_boxplot() +
    facet_wrap(~ clinical_measure, scales = 'free_y') +
    labs(
      title = 'Disribution of Clinical Measures by Education Level', 
      x = 'Education Level', 
      y = 'Value'
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

  # higher levels of education were associated with - 
  # lower median Body Mass Index BMXBMI
  # lower median Systolic Blood Pressure BPXSY2
  # higher median HDL (good) Cholesterol LBDHDDSI



# clinical measures by race/ethnicity
prepped_data %>% 
  select(
    RIDRETH3, 
    BMXBMI, BMXWAIST,
    BPXSY2, BPXDI2,
    LBDTCSI, LBDHDDSI, LBDGLUSI
  ) %>%
  
  pivot_longer(
    cols = -RIDRETH3,
    names_to = 'clinical_measure',
    values_to = 'value'
  ) %>%
  
  ggplot(aes(x = RIDRETH3, y = value)) +
  geom_boxplot() +
  facet_wrap(~ clinical_measure, scales = 'free_y') +
  labs(
    title = 'Disribution of Clinical Measures by Race/Ethnicity', 
    x = 'Race/Ethnicity', 
    y = 'Value'
  ) +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))
    # non-hispanic black women show a higher median bmi and blood pressure compared to other groups
    # non-hispanic asian women show the lowest median bmi and waist circumference
    # other measures like fasting glucose showed less variation across groups


# clinical measures by marital status
prepped_data %>% 
  select(
    DMDMARTL, 
    BMXBMI, BMXWAIST,
    BPXSY2, BPXDI2,
    LBDTCSI, LBDHDDSI, LBDGLUSI
  ) %>%
  
  pivot_longer(
    cols = -DMDMARTL,
    names_to = 'clinical_measure',
    values_to = 'value'
  ) %>%
  
  ggplot(aes(x = DMDMARTL, y = value)) +
  geom_boxplot() +
  facet_wrap(~ clinical_measure, scales = 'free_y') +
  labs(
    title = 'Disribution of Clinical Measures by Marital Status', 
    x = 'Marital Status', 
    y = 'Value'
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
    # variations less pronounced between marital status groups than between 
    # education and race'ethnicity
    # some trends were noted, widowed group show a higher median systolic bp
    # likely due to older age and never married group show a slightly lower median
    # bmi


# exploring lifestyle variables

# frequency count for smoking screening question 
table(prepped_data$SMQ020)
    # mostly non-smokers - about a third with a history of significant smoking
    #  Yes   No 
    #  904 1963 

# frequency count for alcohol screening question
table(prepped_data$ALQ130)
    # the counts for higher number of drink are very small
    # will combine into on category of 7 or more at pre-processing


# frequency count for moderate/vigorous physical activity
table(prepped_data$PAQ605) # vigorous work
    # Yes         No      Don't know 
    # 474       2390          3 

table(prepped_data$PAQ620) # moderate work
    # Yes         No      Don't know 
    #  1055       1808          4 

table(prepped_data$PAQ650) # vigorous recreation
    # Yes   No 
    # 545 2322 

table(prepped_data$PAQ665) # moderate recreation
    # Yes   No 
    # 1087 1780 



# initial bivariate analysis with potential targets (just to see)

# bmi vs diabetes diagnosis
prepped_data %>%
  filter(DIQ010 %in% c('Yes', 'No')) %>%
  ggplot(aes(x = DIQ010, y = BMXBMI)) +
  geom_boxplot() +
  labs(title = 'BMI Distribution by Diabetes Diagnosis',
       x = 'Doctor told you have diabetes',
       y = 'Body Mass Index (BMI)')


# age vs systolic blood pressure
ggplot(prepped_data, aes(x = RIDAGEYR, y = BPXSY2)) +
  geom_point(alpha = 0.2) + # alpha for transparency to see dense areas
  geom_smooth(method = 'loess') + # add smoothed trend line
  labs(title = 'Systolic Blood Pressure by Age',
       x = 'Age (Years)',
       y = 'Systolic Blood Pressure (mm Hg)')



# exploring relationship between lifestyle and clinical measures

#smoking history
prepped_data %>% 
  select(
    SMQ020,
    BMXBMI, BMXWAIST,
    BPXSY2, BPXDI2,
    LBDTCSI, LBDHDDSI, LBDGLUSI
  ) %>%
  
  pivot_longer(
    cols = -SMQ020, 
    names_to = 'clinical_measure',
    values_to = 'value'
  ) %>%
  
  ggplot(aes(x =SMQ020, y = value)) +
    geom_boxplot() +
    facet_wrap (~ clinical_measure, scales = 'free_y') +
  labs(
    title = 'Distribution of Clinical Measures by Smoking History',
    x = 'Smoked at least 100 cigarettes in life', 
    y = 'Value'
  )
  
    # notes typed in word doc



# drinking habits

prepped_data %>%
  mutate(
    drinker_status = case_when(
      !is.na(ALQ130) ~ 'Current Drinker', # if NOT NA they are a drinker
      is.na(ALQ130) ~ 'Non-drinker' # if IS NA they are not a drinker 
    )
  ) %>%
  
  select(
    drinker_status,
    BMXBMI, BMXWAIST,
    BPXSY2, BPXDI2,
    LBDTCSI, LBDHDDSI, LBDGLUSI
  ) %>%
  
  pivot_longer(
    cols = -drinker_status, 
    names_to = 'clinical_measure',
    values_to = 'value'
  ) %>%
  
  ggplot(aes(x = drinker_status, y = value)) +
  geom_boxplot() +
  facet_wrap (~ clinical_measure, scales = 'free_y') +
  labs(
    title = 'Distribution of Clinical Measures by Alcohol Usage',
    x = 'Alcohol Usage Status (Past Year)', 
    y = 'Value'
  )

    # notes typed in word doc


# physical activity - moderate recreational

prepped_data %>%
  select(
    PAQ665,
    BMXBMI, BMXWAIST,
    BPXSY2, BPXDI2,
    LBDTCSI, LBDHDDSI, LBDGLUSI
  ) %>%
  
  pivot_longer(
    cols = -PAQ665, 
    names_to = 'clinical_measure',
    values_to = 'value'
  ) %>%
  
  ggplot(aes(x = PAQ665, y = value)) +
  geom_boxplot() +
  facet_wrap (~ clinical_measure, scales = 'free_y') +
  labs(
    title = 'Distribution of Clinical Measures by Moderate Recreational Activity',
    x = 'Moderate Recreational Activity', 
    y = 'Value'
  )
    

# pre-processing 

# creating binarised target variables

# target 1 - high cholesterol - 1 = yes, 0 = no
women_cmr_data <- prepped_data %>%
  mutate(
    tgt_high_chol = case_when(
      LBDTCSI >= 6.2 ~ 1,
      LBDTCSI < 6.2 ~ 0,
      TRUE ~ NA_real_
    ),
    
# target 2 - diabetes - 1 = yes, 0 = no 
    tgt_diabetes = case_when(
      DIQ010 == 'Yes' ~ 1,
      DIQ010 == 'No' ~ 0,
      TRUE ~ NA_real_
    ),

#target 3 - high blood sugar
    tgt_high_glucose = case_when(
      LBDGLUSI >= 7.0 ~ 1,
      LBDGLUSI <7.0 ~ 0, 
      TRUE ~ NA_real_
    ),

# target 4 - high blood pressure 
    tgt_high_bp = case_when(
      BPXSY2 >= 140 | BPXDI2 >= 90 ~ 1,
      BPXSY2 < 140 & BPXDI2 < 90 ~ 0,
      TRUE ~ NA_real_
    )
  )
   # note on process added to word doc  


View(women_cmr_data)

# creating csv of women's data 
#write.csv(women_cmr_data, 'womens_cmr_v1.csv', row.names = FALSE)
#View(women_cmr_data)
  

# checking target variable balance and co-occurrence

# frequency counts for all target variables 
women_cmr_data %>%
  select(starts_with('tgt_')) %>%
  lapply(table)

    #notes added to word doc 

# check for co-occurrence

# counting the number of risk factors for each person
co_occ_summary <- women_cmr_data %>%
  mutate(risk_factor_cnt = rowSums(select(., starts_with('tgt')), na.rm = TRUE))

# frequency table of the risk factor counts
table(co_occ_summary$risk_factor_cnt)

# applying survey weights

# installing the survey package
install.packages('survey')

# load the survey library
library(survey)

# filtering out rows where weight is missing
final_data_for_weighting <- filter(women_cmr_data, !is.na(WTMEC2YR))

# creating the survey design logic
nhanes_svy_dsn <- svydesign(
  id = ~SDMVPSU,         # primary sampling unit 
  strata = ~SDMVSTRA,    # strata
  weights = ~WTMEC2YR,   # MEC exam weight
  nest = TRUE,           # strata are nest within each other
  data = final_data_for_weighting
)

print(nhanes_svy_dsn)

# advanced eda with weighted data 

# calculating the weighted prevalence for all target variables 
prevalence_table <- svymean(
  ~tgt_high_chol + tgt_diabetes + tgt_high_glucose + tgt_high_bp,
  design = nhanes_svy_dsn,
  na.rm = TRUE
)

print(prevalence_table)

# multicollinearity check 
library(corrplot)

num_predictors <- women_cmr_data %>%
  select(
    RIDAGEYR, INDFMPIR, BMXBMI, BMXWAIST, BPXSY2,
    BPXDI2, LBDTCSI, LBDHDDSI, LBDGLUSI
  )
# calculating the correlation matrix
corr_matrix <- cor(num_predictors, use = 'pairwise.complete.obs')

# correlation matrix heatmap
corrplot(corr_matrix, method = 'circle', type = 'upper', tl.col = 'black', tl.srt = 45)



# calculating the prevalence of all targets stratified by race/ethnicity
prevalence_by_race <- svyby(
  ~tgt_high_chol + tgt_diabetes + tgt_high_glucose + tgt_high_bp, 
  by = ~RIDRETH3,                                                     
  design = nhanes_svy_dsn,                                            
  FUN = svymean,                                                      
  na.rm = TRUE
)

# print the table
print(prevalence_by_race)


# calculating the prevalence of all targets stratified by education
prevalence_by_education <- svyby(
  ~tgt_high_chol + tgt_diabetes + tgt_high_glucose + tgt_high_bp,
  by = ~DMDEDUC2,
  design = nhanes_svy_dsn,
  FUN = svymean,
  na.rm = TRUE
)

# print the table
print(prevalence_by_education)



# creating the final imputed data frame for machine learning
cmr_ml_ready_data <- women_cmr_data %>%
  # imputing numeric predictors with the median
  mutate(across(
    .cols = c(INDFMPIR, BMXBMI, BMXWAIST, BPXSY2, BPXDI2, LBDTCSI, LBDHDDSI, LBDGLUSI),
    .fns = ~ifelse(is.na(.), median(., na.rm = TRUE), .)
  )) %>%
  
  # imputing categorical predictors with the mode 
  # and converting "Refused" and "Don't Know" categories to NA so they can be imputed along with other NAs.
  mutate(
    DMDEDUC2 = na_if(as.character(DMDEDUC2), 'Refused'),
    DMDEDUC2 = na_if(DMDEDUC2, "Don't Know"),  # Already character from previous line
    DMDMARTL = na_if(as.character(DMDMARTL), 'Refused')
  ) %>%
  # imputing with the mode
  mutate(across(
    .cols = c(DMDEDUC2, DMDMARTL),
    .fns = ~ifelse(is.na(.), names(which.max(table(., useNA = "no"))), .)
  ))

# checking there are no more missing values in the predictors
colSums(is.na(cmr_ml_ready_data))


# Final check on imputed data

#  selecting only the numeric predictor variables the imputed data
numeric_predictors_imputed <- cmr_ml_ready_data %>%
  select(
    RIDAGEYR, INDFMPIR, BMXBMI, BMXWAIST, BPXSY2, 
    BPXDI2, LBDTCSI, LBDHDDSI, LBDGLUSI
  )

# calculating the correlation matrix on the complete imputed data
cor_matrix_imputed <- cor(numeric_predictors_imputed)

# visualising the new correlation matrix with a heatmap
corrplot(cor_matrix_imputed, method = 'circle', type = 'upper', tl.col = 'black', tl.srt = 45)

# creating the final data frame for export
final_cmr_data <- cmr_ml_ready_data %>%
  mutate(
    drinker_status = case_when(
      !is.na(ALQ130) ~ 'Current Drinker',
      is.na(ALQ130) ~ 'Non-Drinker'
    )
  ) %>%
  
  select(
    starts_with('tgt_'),
    
    RIDAGEYR, INDFMPIR, BMXBMI, BMXWAIST, BPXSY2,
    BPXDI2, LBDTCSI, LBDHDDSI, LBDGLUSI,
    
    DMDEDUC2, DMDMARTL, RIDRETH3,
    SMQ020, drinker_status, PAQ665
  )

# saving final data as csv ready for modelling 
#write.csv(final_cmr_data,'final_cmr_data.csv')

