# ROH data analysis

# open libraries
library(tidyverse)
library(apaTables)


###-------------------------###

# input the Froh genome wide data set
Froh.genome.wide = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_genome_wide.csv")

#inspect the file
Froh.genome.wide

#anova on Froh_genome_wide by breed
anova.Froh.genome.wide <- aov (Froh_genome ~ group, data = Froh.genome.wide)
summary (anova.Froh.genome.wide)

# ouput the anova table in apa format
apa.aov.table (anova.Froh.genome.wide)


###-------------------------###

# Froh file separated by class of roh length
Froh.class = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_class.csv")

# Pivot the data from multiple columns to a 'tidy' format with one observation/row
# Also using data piping (%>%) from dplyr

# First get the data into 2 long columns, type of data, frequency class bins
int_class_data <- Froh.class %>%
  pivot_longer(
  cols = Sum_Class_0:Froh_Class_16,
  names_to = c("type", "class"),
  names_pattern = "(.*)_.*_(.*)",
  values_to = "values"
)

# Now widen the table so it is one 'class' observation/row with two values Sum and Froh
class_data <- int_class_data %>%
  pivot_wider(
    names_from = "type",
    values_from = "values"
)

# Get data means and sd for group*class
class_data %>%
  group_by(group,class) %>%
  summarise(Sum_mean = mean(Sum, na.rm = TRUE),
            Sum_sd = sd(Sum, na.rm = TRUE),
            Froh_mean = mean(Froh, na.rm = TRUE),
            Froh_sd = sd(Froh, na.rm = TRUE))

# Plot means
means %>%
  ggplot(aes(x=factor(class, levels = c(0,2,4,8,16)),y=Froh_mean, fill=group)) +
  geom_col(position = "dodge")

#anova on Froh_genome_wide by breed
anova_group <- aov (Sum ~ group, data = class_data)
summary (anova_group)

# ouput the anova table in apa format
apa.aov.table (anova_group)


# 2 factors
anova_group2 <- aov (Sum ~ group + class, data = class_data)
summary (anova_group2)

# ouput the anova table in apa format
apa.aov.table (anova_group2)


# 2 factors with interaction
anova_group3 <- aov (Sum ~ group + class + group*class, data = class_data)
summary (anova_group3)

# ouput the anova table in apa format
apa.aov.table (anova_group3)



