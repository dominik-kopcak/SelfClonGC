library(vcfR, quietly=TRUE)
library(dplyr, quietly=TRUE)

setwd("./Simulation_output")


Uniparenting_rates = c(0, 0.2, 0.4, 0.6, 0.8, 0.95, 0.99, 1.00)
N_uniparenting_rates = length(Uniparenting_rates)

h_values = c(0.2)
N_h_values = length(h_values)

s_values = c(0.01)
N_s_values = length(s_values)

N_replicates = 100

Segregating_mutations_list <-list.files(pattern="X_Sampled_individuals*")

Segregating_mutations_list_selfing <- Segregating_mutations_list[grepl("selfing", Segregating_mutations_list)]

Segregating_mutations_list_cloning <- Segregating_mutations_list[grepl("cloning", Segregating_mutations_list)]

### Extracting the number of segregeting mutations from files to vectors

N_seg_homo_genotypes_selfing <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_uniparenting_rates)
N_segregating_mutations_selfing <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_uniparenting_rates)
Relative_homo_selfing <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_uniparenting_rates)

N_seg_homo_genotypes_cloning <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_uniparenting_rates)
N_segregating_mutations_cloning <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_uniparenting_rates)
Relative_homo_cloning <- rep(c(0), each=N_replicates*N_h_values*N_s_values*N_uniparenting_rates)

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
	Relative_homo_selfing[j_s] <- 2*N_seg_homo_genotypes_selfing[j_s]/N_segregating_mutations_selfing[j_s] ## fraction of mutations that are in homozygous state
	
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
	Relative_homo_cloning[j_c] <- 2*N_seg_homo_genotypes_cloning[j_c]/N_segregating_mutations_cloning[j_c] ## fraction of mutations that are in homozygous state
}

# print(Segregating_mutations_list_selfing)
# following the naming convetion of the SLiM output file

Selection_coefficient <- rep(s_values, each = N_h_values*N_uniparenting_rates*N_replicates)
Dominance_coefficient <- rep(rep(h_values, each = N_uniparenting_rates*N_replicates), times = N_s_values)
Uniparenting_rate <- rep(rep(Uniparenting_rates, each=N_replicates), times = N_s_values*N_h_values)
Replicate <- rep(c(1:N_replicates), times = N_uniparenting_rates*N_s_values*N_h_values)

Recessive_load_selfing <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, N_seg_homo_genotypes_selfing)
Additive_load_selfing <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, N_segregating_mutations_selfing)
Relative_homozygosity_selfing <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, Relative_homo_selfing)

Recessive_load_cloning <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, N_seg_homo_genotypes_cloning)
Additive_load_cloning <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, N_segregating_mutations_cloning)
Relative_homozygosity_cloning <- data.frame(Selection_coefficient, Dominance_coefficient, Uniparenting_rate, Replicate, Relative_homo_cloning)

### Summarize and save data into files
## Selfing

Recessive_load_stat_selfing <- Recessive_load_selfing %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_selfing), SD_recessive_load=sd(N_seg_homo_genotypes_selfing), .groups = "rowwise")

Additive_load_stat_selfing <- Additive_load_selfing %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_selfing), SD_additive_load=sd(N_segregating_mutations_selfing), .groups = "rowwise")

Relative_homozygosity_stat_selfing <- Relative_homozygosity_selfing %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate) %>%
	summarise(Expected_relative_homozygosity=mean(Relative_homo_selfing), SD_relative_homozygosity=sd(Relative_homo_selfing), .groups = "rowwise")
	
## Cloning

Recessive_load_stat_cloning <- Recessive_load_cloning %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_cloning), SD_recessive_load=sd(N_seg_homo_genotypes_cloning), .groups = "rowwise")

Additive_load_stat_cloning <- Additive_load_cloning %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_cloning), SD_additive_load=sd(N_segregating_mutations_cloning), .groups = "rowwise")

Relative_homozygosity_stat_cloning <- Relative_homozygosity_cloning %>%
	group_by(Selection_coefficient, Dominance_coefficient, Uniparenting_rate) %>%
	summarise(Expected_relative_homozygosity=mean(Relative_homo_cloning), SD_relative_homozygosity=sd(Relative_homo_cloning), .groups = "rowwise")

setwd("../")

write.csv(Additive_load_stat_selfing, file="Data_selfing_additive_load_full_60000.csv")
write.csv(Recessive_load_stat_selfing, file="Data_selfing_recessive_load_full_60000.csv")
write.csv(Relative_homozygosity_stat_selfing, file="Data_selfing_relative_homozygosity_full_60000.csv")

write.csv(Additive_load_stat_cloning, file="Data_cloning_additive_load_full_60000.csv")
write.csv(Recessive_load_stat_cloning, file="Data_cloning_recessive_load_full_60000.csv")
write.csv(Relative_homozygosity_stat_cloning, file="Data_cloning_relative_homozygosity_full_60000.csv")

## Also save raw data

write.csv(Additive_load_selfing, file="Data_selfing_additive_load_full_60000_raw.csv")
write.csv(Recessive_load_selfing, file="Data_selfing_recessive_load_full_60000_raw.csv")
write.csv(Relative_homozygosity_selfing, file="Data_selfing_relative_homozygosity_full_60000_raw.csv")

write.csv(Additive_load_cloning, file="Data_cloning_additive_load_full_60000_raw.csv")
write.csv(Recessive_load_cloning, file="Data_cloning_recessive_load_full_60000_raw.csv")
write.csv(Relative_homozygosity_cloning, file="Data_cloning_relative_homozygosity_full_60000_raw.csv")
