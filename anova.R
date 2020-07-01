setwd ("~/Desktop/map-ped/summaryList")

Froh.genome.wide = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_genome_wide.csv")

Froh.genome.wide

anova <- aov (Froh_genome ~ Breed, data = Froh.genome.wide)

summary (anova)
