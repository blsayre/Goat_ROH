setwd ("~/Desktop/map-ped/summaryList")

Froh.genome.wide = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_genome_wide.csv")

#inspect the file
Froh.genome.wide

#anova on Froh_genome_wide by breed
anova.Froh.genome.wide <- aov (Froh_genome ~ group, data = Froh.genome.wide)

summary (anova.Froh.genome.wide)

# trying to get an apa table output (not working)
library(apaTables)
apa.aov.table (anova.Froh.genome.wide)

#trying a different way to get apa style tables for summary of anova
#not working
library (knitr)
kable (anova.Froh.genome.wide)

Froh.chr.wide = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_chromosome_wide.csv")
Froh.chr.wide

anova.Froh.chr1 <- aov ( Chr_1 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr1)

anova.Froh.chr2 <- aov ( Chr_2 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr2)

anova.Froh.chr3 <- aov ( Chr_3 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr3)

anova.Froh.chr4 <- aov ( Chr_4 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr4)

anova.Froh.chr5 <- aov ( Chr_5 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr5)

anova.Froh.chr6 <- aov ( Chr_6 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr6)

anova.Froh.chr7 <- aov ( Chr_7 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr7)

anova.Froh.chr8 <- aov ( Chr_8 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr8)


anova.Froh.chr9 <- aov ( Chr_9 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr9)

anova.Froh.chr10 <- aov ( Chr_10 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr10)

anova.Froh.chr11 <- aov ( Chr_11 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr11)

anova.Froh.chr12 <- aov ( Chr_12 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr12)

anova.Froh.chr13 <- aov ( Chr_13 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr13)

anova.Froh.chr14 <- aov ( Chr_14 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr14)

anova.Froh.chr15 <- aov ( Chr_15 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr15)

anova.Froh.chr16 <- aov ( Chr_16 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr16)

anova.Froh.chr17 <- aov ( Chr_17 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr17)

anova.Froh.chr18 <- aov ( Chr_18 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr18)

anova.Froh.chr19 <- aov ( Chr_19 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr19)

anova.Froh.chr20 <- aov ( Chr_20 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr20)

anova.Froh.chr21 <- aov ( Chr_21 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr21)

anova.Froh.chr22 <- aov ( Chr_22 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr22)

anova.Froh.chr23 <- aov ( Chr_23 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr23)

anova.Froh.chr24 <- aov ( Chr_24 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr24)

anova.Froh.chr25 <- aov ( Chr_25 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr25)

anova.Froh.chr26 <- aov ( Chr_26 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr26)

anova.Froh.chr27 <- aov ( Chr_27 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr27)

anova.Froh.chr28 <- aov ( Chr_28 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr28)

anova.Froh.chr29 <- aov ( Chr_29 ~ group, data = Froh.chr.wide)
summary (anova.Froh.chr29)


# install apa tables package 
#install.packages("apaTables",dep=T)

# trying to get an apa table output (not working)
library(apaTables)
apa.aov.table (anova.Froh.chr29)

#trying a different way to get apa style tables for summary of anova
#not working
#library (knitr)
#kable (anova.Froh.chr29)

# Froh file separated by class of roh length
Froh.class = read.csv ("~/Desktop/map-ped/summaryList/result_Froh_class.csv")

library(tidyr)
Froh.class.tidy <- gather(Froh.class, "Sum_Class_*", "Froh_Class_*", key="class", values="Sum","Froh")

# anova for Froh class 0 by breed
anova.Froh.class.0 <- aov (Froh_Class_0 ~ group, data = Froh.class)

summary (anova.Froh.class.0)

# anova for Froh class 2 by breed
anova.Froh.class2 <- aov (Froh_Class_2 ~ group, data = Froh.class)
summary (anova.Froh.class2)

# anova for Froh class 4 by breed
anova.Froh.class4 <- aov (Froh_Class_4 ~ group, data = Froh.class)
summary (anova.Froh.class4)

# anova for Froh class 8 by breed
anova.Froh.class8 <- aov (Froh_Class_8 ~ group, data = Froh.class)
summary (anova.Froh.class8)

# anova for Froh class 16 by breed
anova.Froh.class16 <- aov (Froh_Class_16 ~ group, data = Froh.class)
summary (anova.Froh.class16)



# anova of ROH sum by class regardless of breed
roh.count = read.csv ("~/Desktop/map-ped/summaryList/summary_ROH_count.csv")
anova.roh.count <- aov (rohs_sum ~ class , data= roh.count)
summary (anova.roh.count)

# anova of ROH count by breed regardless of class, didn't work
anova.roh.count.breed <- aov (total_roh_count ~ Breed, data = roh.count)
summary (anova.roh.count.breed)

# trying to see if there is class_1 differences between breeds, not working
roh.mean.class1 = read.csv ("~/Desktop/map-ped/summaryList/summary_ROH_mean_class.csv")
anova.roh.mean.class1 <- aov (class_1 ~ breed, data = roh.mean.class1)
summary (anova.roh.mean.class1)
