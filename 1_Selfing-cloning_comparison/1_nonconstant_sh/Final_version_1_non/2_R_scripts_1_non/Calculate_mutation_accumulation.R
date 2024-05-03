library(vcfR, quietly=TRUE)
library(dplyr, quietly=TRUE)

setwd("./Simulation_outputs")


Uniparenting_rates = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1.00)
N_uniparenting_rates = length(Uniparenting_rates)

N_replicates = 20

Segregating_mutations_list <-list.files(pattern="X_Sampled_individuals*")

Segregating_mutations_list_selfing <- Segregating_mutations_list[grepl("selfing", Segregating_mutations_list)]

Segregating_mutations_list_cloning <- Segregating_mutations_list[grepl("cloning", Segregating_mutations_list)]

### Extracting the number of segregeting mutations from files to vectors

N_seg_homo_genotypes_selfing <- rep(c(0), each=N_replicates*N_uniparenting_rates)
N_segregating_mutations_selfing <- rep(c(0), each=N_replicates*N_uniparenting_rates)

N_seg_homo_genotypes_cloning <- rep(c(0), each=N_replicates*N_uniparenting_rates)
N_segregating_mutations_cloning <- rep(c(0), each=N_replicates*N_uniparenting_rates)

j_s = 0
for (y in Segregating_mutations_list_selfing){ 
	j_s = j_s + 1
	file_s <- read.vcfR(y, verbose = FALSE) 
	genotype <- extract_gt_tidy(file_s, verbose = FALSE)$gt_GT 
	
	genotype_number <- rep(c(0),times=length(genotype)) 
	
	k=0
	for (z in genotype){ 
		k = k + 1
		allele_1 <- as.numeric(substring(z,1,1))
		allele_2 <- as.numeric(substring(z,3,3))
		genotype_number[k] = allele_1 + allele_2}
	
	N_seg_homo_genotypes_selfing[j_s] <- length(genotype_number[genotype_number == 2]) ## recessive load
	N_segregating_mutations_selfing[j_s] <- sum(genotype_number) ## additive load
}

j_c = 0
for (y in Segregating_mutations_list_cloning){ 
	j_c = j_c + 1
	file_s <- read.vcfR(y, verbose = FALSE) 
	genotype <- extract_gt_tidy(file_s, verbose = FALSE)$gt_GT 
	
	genotype_number <- rep(c(0),times=length(genotype)) 
	
	k=0
	for (z in genotype){ 
		k = k + 1
		allele_1 <- as.numeric(substring(z,1,1))
		allele_2 <- as.numeric(substring(z,3,3))
		genotype_number[k] = allele_1 + allele_2}
	
	N_seg_homo_genotypes_cloning[j_c] <- length(genotype_number[genotype_number == 2]) ## recessive load
	N_segregating_mutations_cloning[j_c] <- sum(genotype_number) ## additive load
}

# print(Segregating_mutations_list_selfing)
# following the naming convetion of the SLiM output file

Uniparenting_rate <- rep(rep(Uniparenting_rates, each=N_replicates))
Replicate <- rep(c(1:N_replicates), times = N_uniparenting_rates)
 
Recessive_load_selfing <- data.frame(Uniparenting_rate, Replicate, N_seg_homo_genotypes_selfing)
Additive_load_selfing <- data.frame(Uniparenting_rate, Replicate, N_segregating_mutations_selfing)

Recessive_load_cloning <- data.frame(Uniparenting_rate, Replicate, N_seg_homo_genotypes_cloning)
Additive_load_cloning <- data.frame(Uniparenting_rate, Replicate, N_segregating_mutations_cloning)


### Summarize and save data into files
## Selfing


Recessive_load_stat_selfing <- Recessive_load_selfing %>%
	group_by(Uniparenting_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_selfing), SD_recessive_load=sd(N_seg_homo_genotypes_selfing), .groups = "rowwise")

Additive_load_stat_selfing <- Additive_load_selfing %>%
	group_by(Uniparenting_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_selfing), SD_additive_load=sd(N_segregating_mutations_selfing), .groups = "rowwise")


## Cloning


Recessive_load_stat_cloning <- Recessive_load_cloning %>%
	group_by(Uniparenting_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_cloning), SD_recessive_load=sd(N_seg_homo_genotypes_cloning), .groups = "rowwise")

Additive_load_stat_cloning <- Additive_load_cloning %>%
	group_by(Uniparenting_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_cloning), SD_additive_load=sd(N_segregating_mutations_cloning), .groups = "rowwise")

setwd("../")

write.csv(Additive_load_stat_selfing, file="Data_selfing_additive_load.csv")
write.csv(Recessive_load_stat_selfing, file="Data_selfing_recessive_load.csv")

write.csv(Additive_load_stat_cloning, file="Data_cloning_additive_load.csv")
write.csv(Recessive_load_stat_cloning, file="Data_cloning_recessive_load.csv")
