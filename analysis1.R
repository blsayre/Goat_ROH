setwd ("~/Desktop/map-ped/")
library(detectRUNS)



# getting map and ped paths
genotypeFile <- read.table( "VSUGoatped.ped", sep=" ", header=F) 
mapFile <- read.table( "VSUGoatmap.map", sep=" ", header=F)


slidingRuns <- slidingRUNS.run (
  genotypeFile =  "~/Desktop/map-ped/VSUGoatped.ped", 
  mapFile = "~/Desktop/map-ped/VSUGoatmap.map", 
  windowSize = 15, 
  threshold = 0.05,
  minSNP = 20, 
  ROHet = FALSE, 
  maxOppWindow = 1, 
  maxMissWindow = 1,
  maxGap = 10^6, 
  minLengthBps = 250000, 
  minDensity = 1/10^3, # SNP/kbps
  maxOppRun = NULL,
  maxMissRun = NULL
) 


froh <- Froh_inbreeding(runs = slidingRuns, mapFile = "~/Desktop/map-ped/VSUGoatmap.map")
write.csv(froh, "Result_FROH_output.csv")

summaryList <- summaryRuns(
  runs = slidingRuns, mapFile = "~/Desktop/map-ped/VSUGoatmap.map", genotypeFile = "~/Desktop/map-ped/VSUGoatped.ped", snpInRuns = TRUE)

plot_InbreedingChr(runs = slidingRuns, mapFile = "~/Desktop/map-ped/VSUGoatmap.map", style='All')


plot_InbreedingChr(slidingRuns, "~/Desktop/map-ped/VSUGoatmap.map" , groupSplit = TRUE,
                   style = c("ChrBarPlot", "ChrBoxPlot", "FrohBoxPlot", "All"),
                   outputName = NULL, plotTitle = NULL, savePlots = FALSE)

plot_Runs(runs = slidingRuns)


plot_StackedRuns(runs = slidingRuns, separatePlots = FALSE)

plot_SnpsInRuns(runs = slidingRuns[slidingRuns$chrom==6,], genotypeFile = "~/Desktop/map-ped/VSUGoatped.ped", 
                mapFile = "~/Desktop/map-ped/VSUGoatmap.map")

plot_manhattanRuns(
  runs = slidingRuns[slidingRuns$group=="BO",], 
  genotypeFile = "~/Desktop/map-ped/VSUGoatped.ped", 
  mapFile = "~/Desktop/map-ped/VSUGoatmap.map")

write.csv(summaryList$summary_ROH_mean_chr, "summary_ROH_mean_chr.csv") 

summary(summaryList$result_Froh_genome_wide)
summaryList$summary_ROH_count
