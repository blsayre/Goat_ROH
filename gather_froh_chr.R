# tidy the froh_chromosome data

# open libraries
library(tidyverse)

# input froh_chromsome data set
Froh.chr = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_chromosome_wide.csv")

# Pivot the data from multiple columns to a 'tidy' format with one observation/row
# Also using data piping (%>%) from dplyr

int_chr_data <- Froh.chr %>%
  pivot_longer(
    cols = Chr_1:Chr_29,
    names_to = c("chromosome"),
    values_to = "values"
  )
write.csv (int_chr_data, "~/Desktop/map-ped/summaryList/froh_chr_data.csv")

anova_froh_chr <- aov (values ~ Breed + chromosome, data = int_chr_data)
summary (anova_froh_chr)

anova_froh_chr2 <- aov (values ~ Breed + chromosome + Breed*chromosome, data = int_chr_data)
summary (anova_froh_chr2)
