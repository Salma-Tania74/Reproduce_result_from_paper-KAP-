#Load package
library(tidyverse)
library(readxl)
library(openxlsx)
library(gtsummary)
library(gt)
library(easystats)
library(naniar)
library(dplyr)
library(MASS)
library(car)

#Input data
AMR_Kap_clean <- read_excel("Copy of AMR_KAP_Data_F(1).xlsx")

strings2factors(AMR_Kap_clean)


#Table 1.Demographic characteristics of the study population(N=704)
AMR_Kap_clean |>
  select(1:10) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean}±{sd}"
  ) |>
  bold_labels() |>
  as_gt() |>
  gtsave("Assignment 2/Tables/Table1.docx")



#Table 2. Major sources of information about antibiotic parents (N=704).

AMR_kap_recoded |>
  select(72:74) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean}±{sd}"
  ) |>
  bold_labels() |>
  as_gt() |> 
  gtsave("Assignment 2/Tables/tables22.docx")


#recode data using cut off method for K/A/P

AMR_Kap_clean$Knowladge_percentage <- cut(AMR_Kap_clean$Knowladge_percentage,
                                          breaks = c(-Inf, 51, 79, Inf),
                                          labels = c("Poor","Moderate","Good"))
print(AMR_Kap)
str(AMR_Kap)

AMR_Kap_clean$Attitude_percentage <- cut(AMR_Kap_clean$Attitude_percentage,
                                         breaks = c(-Inf, 51, 79, Inf),
                                         labels = c("Negative","Uncertain","Positive"))
print(AMR_Kap)
str(AMR_Kap)

AMR_Kap_clean$Prectice_Percentagle <- cut(AMR_Kap_clean$Prectice_Percentagle,
                                          breaks = c(-Inf, 79, Inf),
                                          labels = c("Inappropiate","Appropiate"))
str(AMR_Kap)

View(AMR_Kap_clean)

names(AMR_Kap_clean)


#Table 3. Major sources of information about antibiotic parents (N = 704).
AMR_Kap_clean |>
  select(41:49) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean}±{sd}"
  ) |>
  bold_labels() |>
  as_gt() |> 
  gtsave("Assignment 2/Tables/Tables2.docx")


sum(is.na(AMR_Kap))
AMR_Kap_clean <- na.omit(AMR_Kap)
gg_miss_var(AMR_Kap_clean)


#LOad package
library(dplyr)
library(MASS)

#Fit model Knowladge
MODEL_11 <- polr(Knowladge_percentage ~ Parents_age_years
                 +Parents_sex
                 +Parents_education_level
                 +Employment_status
                 +Family_type
                 +Your_average_household_income_per_month
                 +Childs_sex
                 +Childs_age_years
                 +Number_of_children,
                 data = AMR_Kap_clean2,
                 method ="logistic", control = list(maxit = 1000))




AMR_Kap_clean2 <- AMR_Kap_clean[complete.cases(AMR_Kap_clean), ]


#TAble for MODEL 11

reg_table <- MODEL_11 |>
  tbl_regression(exponentiate = TRUE) |>
  add_global_p() |>  # Ensure p-values are added
  bold_p(t = 0.05) |>  # Bold significant p-values
  bold_labels() |>  # Bold variable labels
  as_gt() |> 
  gtsave("Assignment 2/Tables/table5.docx")


names(AMR_Kap_clean2)

#MODEL FIT FOR ATTITUDE 

MODEL_21 <- polr(Attitude_percentage ~ Parents_age_years
                 +Parents_sex
                 +Parents_education_level
                 +Employment_status
                 +Family_type
                 +Your_average_household_income_per_month
                 +Childs_sex
                 +Childs_age_years
                 +Number_of_children,
                 data = AMR_Kap_clean2,
                 method ="logistic", control = list(maxit = 1000))


MODEL_21 |>
  tbl_regression(exponentiate = TRUE) |>
  add_global_p()|>  # Ensure p-values are added
  bold_p(t = 0.05) |>  # Bold significant p-values
  bold_labels() |>  # Bold variable labels
  as_gt() |> 
  gtsave("Assignment 2/Tables/table555.docx")


#Nodel for Prectice

MODEL_31 <- glm(Prectice_Percentagle ~ Parents_age_years
                +Parents_sex
                +Parents_education_level
                +Employment_status
                +Family_type
                +Your_average_household_income_per_month
                +Childs_sex
                +Childs_age_years
                +Number_of_children
                +Knowladge_percentage
                +Attitude_percentage,
                data = AMR_Kap_clean,
                family ="binomial")
#table for attitude
MODEL_31 |>
  tbl_regression(exponentiate = TRUE) |>
  add_global_p()|>  # Ensure p-values are added
  bold_p(t = 0.05) |>  # Bold significant p-values
  bold_labels() |>  # Bold variable labels
  as_gt() |> 
  gtsave("Assignment 2/Tables/table6.docx")
