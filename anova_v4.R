# ROH data analysis

# open libraries
library(tidyverse)
library(apaTables)
library(agricolae)


###-------------------------###

# input the Froh genome wide data set
Froh_genome_wide = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_genome_wide.csv")

#inspect the file
head(Froh_genome_wide)

# plot the data
Froh_genome_wide %>%
  ggplot(aes(x=group,y=Froh_genome)) +
  geom_violin(fill="black")

# The Froh is a function of the sum length / total length and therefore the plot of the 
#   sum lengths should not be different than Froh.  So, we plot it here to confirm that.
Froh_genome_wide %>%
  ggplot(aes(x=group,y=sum)) +
  geom_violin(fill="black")

# Get data means and sd for group*class
froh_summary <- Froh_genome_wide %>%
  group_by(group) %>%
  summarise(Froh_mean = mean(Froh_genome, na.rm = TRUE),
            Froh_sd = sd(Froh_genome, na.rm = TRUE),
            Froh_se = Froh_mean/(sqrt(n())))

# Plot means
froh_summary %>%
  ggplot(aes(x=group,y=Froh_mean)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymax = Froh_mean+Froh_se, ymin = Froh_mean-Froh_se), width=0.2)

#anova on Froh_genome_wide by breed
anova_froh_group <- aov (Froh_genome ~ group, data = Froh_genome_wide)
summary (anova_froh_group)

# Mean separation by Tukey's HSD
tukey_froh_mean_sep_v1 <- TukeyHSD(anova_froh_group)
tukey_froh_mean_sep_v1

# Alternative display of Tukey's HSD data
tukey_froh_mean_sep_v2 <- HSD.test(anova_froh_group, trt='group')
tukey_froh_mean_sep_v2

# ouput the anova table in apa format
apa.aov.table (anova_froh_group)


###-------------------------###

# Froh file separated by class of roh length
Froh_class = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_class.csv")

# Pivot the data from multiple columns to a 'tidy' format with one observation/row
# Also using data piping (%>%) from dplyr

# First get the data into 2 long columns, type of data, frequency class bins
int_class_data <- Froh_class %>%
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

# plot the data
# Plot by group(breed)
class_data %>%
  ggplot(aes(x=group,y=Froh)) +
  geom_violin(fill="black")

# Plot by length class
class_data %>%
  ggplot(aes(x=factor(class, levels = c(0,2,4,8,16)),y=Froh)) +
  geom_violin(fill="black")

# Plot by interactions of class and group
class_data %>%
  group_by(class,group) %>%
  ggplot(aes(x=factor(class, levels = c(0,2,4,8,16)),y=Froh)) +
  geom_violin(aes(fill=factor(class, levels = c(0,2,4,8,16)))) +
  facet_grid(col=vars(group))+
  xlab("Class")+
  scale_fill_discrete(name="Class")

# Get data means and sd for group*class
means_group <- class_data %>%
  group_by(group) %>%
  summarise(Sum_mean = mean(Sum, na.rm = TRUE),
            Sum_sd = sd(Sum, na.rm = TRUE),
            Froh_mean = mean(Froh, na.rm = TRUE),
            Froh_sd = sd(Froh, na.rm = TRUE),
            Froh_se = Froh_mean/(sqrt(n())))

means_class <- class_data %>%
  group_by(class) %>%
  summarise(Sum_mean = mean(Sum, na.rm = TRUE),
            Sum_sd = sd(Sum, na.rm = TRUE),
            Froh_mean = mean(Froh, na.rm = TRUE),
            Froh_sd = sd(Froh, na.rm = TRUE),
            Froh_se = Froh_mean/(sqrt(n())))

means2way <- class_data %>%
  group_by(group,class) %>%
  summarise(Sum_mean = mean(Sum, na.rm = TRUE),
            Sum_sd = sd(Sum, na.rm = TRUE),
            Froh_mean = mean(Froh, na.rm = TRUE),
            Froh_sd = sd(Froh, na.rm = TRUE),
            Froh_se = Froh_mean/(sqrt(n())))

#froh_mean <- means_group$
# Plot means
means_group %>%
  ggplot(aes(x=group,y=Froh_mean)) +
  geom_col() +
  geom_errorbar(aes(ymax = Froh_mean+Froh_se, ymin = Froh_mean-Froh_se), width=0.2)

means_class %>%
  ggplot(aes(x=factor(class, levels = c(0,2,4,8,16)),y=Froh_mean)) +
  geom_col() +
  geom_errorbar(aes(ymax = Froh_mean+Froh_se, ymin = Froh_mean-Froh_se), width=0.2)

means2way %>%
  ggplot(aes(x=factor(class, levels = c(0,2,4,8,16)),y=Froh_mean, fill=group)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymax = Froh_mean+Froh_se, ymin = Froh_mean-Froh_se), width=0.2, position = position_dodge(.9))

# 2 factors with interaction
anova_all_froh <- aov (Froh ~ group + class + group*class, data = class_data)
summary (anova_all_froh)

# ouput the anova table in apa format
apa.aov.table (anova_all_froh)

# Mean separation by Tukey's HSD
tukey_froh_group_sep_v1 <- TukeyHSD(anova_all_froh)
tukey_froh_group_sep_v1

# Alternative display of group separations using Tukey's HSD data
tukey_froh_group_sep_v2 <- HSD.test(anova_all_froh, trt='group')
tukey_froh_group_sep_v2

# Alternative display of class separations using Tukey's HSD data
tukey_froh_class_sep_v2 <- HSD.test(anova_all_froh, trt='class')
tukey_froh_class_sep_v2

# Have not figured out how to get the group*class output yet.


