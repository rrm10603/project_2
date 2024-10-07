library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)
library(VIM)
library(naniar)
library(readr)
library(kableExtra)
library(finalfit)
library(broom)
library(sjPlot)
library(StepReg)
library(reshape2)
library(Amelia)
getwd()

#####
Implications
Longitudinal Analysis: You will likely want to conduct analyses that account for the repeated measures. 
Techniques such as mixed-effects models or generalized estimating equations (GEEs) can be used to 
handle the correlations between measurements taken from the same subject.

Restructuring Data: Depending on your analysis, you might consider reshaping your
data (e.g., from long format to wide format) #if you're interested in specific time points.
This can be done using functions like pivot_wider() from the tidyverse package.

Grouping Data: If you want to compare treatment responses at specific time points (e.g., year 2), you can filter or group your dataset accordingly.
#######


read in data
hiv<- read_csv("/home/robmcneil/Documents/advanced_data/hiv_dataset.csv")
check data types
str(hiv)

Question of Interest: We are interested in understanding how treatment response
2 years after initiating HAART differs between subjects who report using hard
drugs, such as heroin and cocaine, and other subjects, who did not report hard
drug use. We would like to compare subjects who never report using hard drugs to
subjects that report using hard drugs at year 2 (current hard drug users), as well as to
subjects that did not use hard drugs at the year 2 visit but reported hard drug use in the
past (previous hard drug use, reported at either or both of the year 0 and 1 visits). We
would also like to understand if differences in treatment response between the
drug use groups can be explained by differences in adherence to the HAART
treatment regimen.

Information about variables: We have 4 measures of treatment response. The first two
are laboratory measures, viral load (VLOAD), which is the number of HIV copies in a mL
of blood, and the second is CD4+ T cell count (LEU3N), a measure of immunologic
health. In untreated HIV infection, viral load increases over time and CD4+ T cell
counts decline as the immune system is attacked by the virus. Once treatment is
initiated, we expect viral load to decrease rapidly and CD4 counts to recover. Our last
two measures are quality of life measures from the SF-36. The first is the aggregate
physical quality of life score (AGG_PHYS) and the second is the aggregate mental
quality of life score (AGG_MENT). These scores range from 0 to 100, with higher




# Histograms for treatment response measures
ggplot(hiv, aes(x = AGG_MENT)) + 
  geom_histogram(binwidth = 5, fill = "blue", alpha = 0.7) + 
  labs(title = "Distribution of Mental Quality of Life Score", x = "AGG_MENT", y = "Frequency")

ggplot(hiv, aes(x = AGG_PHYS)) + 
  geom_histogram(binwidth = 5, fill = "green", alpha = 0.7) + 
  labs(title = "Distribution of Physical Quality of Life Score", x = "AGG_PHYS", y = "Frequency")



#correlation 

# Select continuous variables for correlation
continuous_vars <- hiv[, c("AGG_MENT", "AGG_PHYS", "LEU3N", "VLOAD", "income", "BMI", "TCHOL", "TRIG", "LDL")]

# Calculate correlation matrix
cor_matrix <- cor(continuous_vars, use = "complete.obs")



# Melt correlation matrix for ggplot
melted_cor <- melt(cor_matrix)

# Create heatmap for visualization
ggplot(melted_cor, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "red", high = "green", mid = "white", limit = c(-1, 1), space = "Lab", name="Correlation") + 
  theme_minimal() + 
  labs(title = "Correlation Heatmap", x = "", y = "")


#missing data: need to insect, use Amelia package to do multiple imputation 

# Summary to check for missing data
summary(hiv)


# Plot missing data to visualize
missmap(hiv, main = "Missing Data Map", col = c("yellow", "black"), legend = TRUE)



