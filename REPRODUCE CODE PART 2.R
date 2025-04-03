# load required packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(gtsummary)
library(gt)
library(easystats)
library(naniar)
library(dplyr)
library(MASS)
# import the data
AMR_Kap <- read_excel("C:/Users/COMPUTER LAB/Desktop/tables/AMR_KAP_Data F.xlsx")

#Check missing value
sum(is.na(AMR_Kap))
na.omit(AMR_Kap)
str(AMR_Kap)


#demographic Data labeing process

AMR_Kap$Parents_sex <- factor(AMR_Kap$Parents_sex, levels = c("Female", "Male"), ordered = TRUE)
AMR_Kap$Parents_sex<- as.numeric(AMR_Kap$Parents_sex)
head(AMR_Kap)

AMR_Kap$Parents_education_level <- factor(AMR_Kap$Parents_education_level,
                                          levels = c("Postgraduate", "Primary","Secondary","Undergraduate"), ordered = TRUE)
AMR_Kap$Parents_education_level<- as.numeric(AMR_Kap$Parents_education_level)
head(AMR_Kap)

AMR_Kap$Employment_status <- factor(AMR_Kap$Employment_status,
                                          levels = c("Employed", "Not employed","Self employed"), ordered = TRUE)
AMR_Kap$Employment_status<- as.numeric(AMR_Kap$Employment_status)
head(AMR_Kap)

AMR_Kap$Family_type <- factor(AMR_Kap$Family_type,
                                    levels = c("Extended family", "Nuclear family","Single parent family"), ordered = TRUE)
AMR_Kap$Family_type<- as.numeric(AMR_Kap$Family_type)
head(AMR_Kap)

AMR_Kap$Your_average_household_income_per_month_BDT <- factor(AMR_Kap$Your_average_household_income_per_month_BDT,
                              levels = c("High (greater than 50000 BDT)", "Low (less than 30000 BDT)","Middle (less than 50000 BDT)"), ordered = TRUE)
AMR_Kap$Your_average_household_income_per_month_BDT<- as.numeric(AMR_Kap$Your_average_household_income_per_month_BDT)
head(AMR_Kap)

AMR_Kap$Childs_sex <- factor(AMR_Kap$Childs_sex,
                              levels = c("Female", "Male"), ordered = TRUE)
AMR_Kap$Childs_sex<- as.numeric(AMR_Kap$Childs_sex)
head(AMR_Kap)
str(AMR_Kap)



#Table 1.Demographic characteristics of the study population(N=704)

AMR_Kap |>
  select(1:10) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean}±{sd}"
  ) |>
  bold_labels() |>
  as_gt() |>
  gtsave("C:/Users/COMPUTER LAB/Desktop/tables/Table 1.docx")

#Table 2. Major sources of information about antibiotic parents (N=704)

AMR_kap|>
  select(72:74) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean}±{sd}"
  ) |>
  bold_labels() |>
  as_gt() |> 
  gtsave("Assignment 2/Tables/tables22.docx")


#Table 3. Major sources of information about antibiotic parents (N = 704).
AMR_Kap |>
  select(41:49) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean}±{sd}"
  ) |>
  bold_labels() |>
  as_gt() |> 
  gtsave(""C:/Users/COMPUTER LAB/Desktop/tables/TABLE 2.docx")


AMR_kap






#ORDINAL LOGISTOC REGRESSION

$Knowledge_percentage_recoded <- factor(AMR_Kap$Knowledge_percentage_recoded,
                                               levels = c("Poor","Moderate","Good"),
                                               ordered = TRUE)



AMR_Kap$Parents_sex<- as.factor(AMR_Kap$Parents_sex)
AMR_Kap$Parents_education_level<- as.factor(AMR_Kap$Parents_education_level)
AMR_Kap$Employment_status<- as.factor(AMR_Kap$Employment_status)
AMR_Kap$Family_type<- as.factor(AMR_Kap$Family_type)

AMR_Kap$Childs_sex<- as.factor(AMR_Kap$Childs_sex)






#model fit 

model_1 <- polr(knowledge_percentage_recoded ~ 
                Parents_age_years + Parents_sex + 
                Parents_education_level + Employment_status + 
                Family_type + Your_average_household_income_per_month_BDT + 
                Childs_sex + Childs_age_years + 
                Number_of_children, 
                data = , 
                Hess = TRUE)



























