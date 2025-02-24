############### Load Data ##############
library(dplyr)

# Load the data from the RDA file
load("DTx.rda")  # Assuming the data frame is named 'DTx'

# Select the first 25 columns for analysis
ana <- DTx[, c(1:25)]

# Check the number of missing values in each column
colSums(is.na(ana))

# Summary statistics for the selected data
summary(ana)


############### Data Transformation ##############
# Categorize enrollment numbers into groups
ana$no.Enrollment <- NA  # Initialize the column
ana$no.Enrollment[ana$Enrollment < 50] <- '<50'
ana$no.Enrollment[ana$Enrollment >= 50 & ana$Enrollment <= 100] <- '50-100'
ana$no.Enrollment[ana$Enrollment > 100 & ana$Enrollment <= 150] <- '101-150'
ana$no.Enrollment[ana$Enrollment > 150] <- '>150'
ana$no.Enrollment <- factor(ana$no.Enrollment, 
                            levels = c('<50', '50-100', '101-150', '>150'))

# Convert Allocation to a factor
ana$Allocation <- factor(ana$Allocation, 
                         levels = c('RANDOMIZED', 'NON_RANDOMIZED', 'OBSERVATIONAL'))

# Convert Intervention Model to a factor
ana$Intervention_Model <- factor(ana$Intervention_Model, 
                                 levels = c('PARALLEL', 'SINGLE_GROUP', 'OTHERS', 'OBSERVATIONAL'))

# Convert Masking to a factor
ana$Masking <- factor(ana$Masking, 
                      levels = c('QUADRUPLE', 'TRIPLE', 'DOUBLE', 'SINGLE', 'NONE', 'OBSERVATIONAL'))

# Convert Region to a factor
ana$Region <- factor(ana$Region, 
                     levels = c('Asia', 'Europe', 'North America', 
                                'South America', 'Africa', 'Oceania'))


############### Create Summary Tables ##############
library(table1)
library(boot)

# Custom function to calculate p-values
pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(g ~ y)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c('', sub('<', '&lt;', format.pval(p, digits = 3, eps = 0.001)))
}

# Custom function to calculate test statistics
stats <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    s <- t.test(y ~ g)$statistic
  } else {
    s <- chisq.test(table(y, g))$statistic
  }
  c("", sprintf("%.4f", s))
}

# Table 1: Summary by 'Shangshi' variable
table1(~ DTx_type + Study_Type + Start_Phases + Region + category
       + Allocation + Intervention_Model + Endpoint + Endpoint_new
       + Funder_Type + Mask + no.Enrollment 
       | Shangshi, data = ana,
       extra.col = list('P-value' = pvalue, `statistic` = stats))

# Table 2: Summary by 'Shangshi' variable (alternative variables)
table1(~ DTx_type + Study_Type + Start_Phases + Region
       + Allocation + Intervention_Model + Endpoint + Endpoint_new
       + Funder_Type + no.Enrollment + Mask
       | Shangshi, data = ana,
       extra.col = list('P-value' = pvalue, `statistic` = stats))

# Table 3: Summary by 'Primary_Purpose' variable
table1(~ Study_Type + Allocation + Intervention_Model + Masking
       + no.Enrollment + Region
       | Primary_Purpose, data = ana,
       extra.col = list('P-value' = pvalue, `statistic` = stats))
