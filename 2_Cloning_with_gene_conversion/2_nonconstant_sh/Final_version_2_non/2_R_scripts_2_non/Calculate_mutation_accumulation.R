library(vcfR, quietly=TRUE)
library(dplyr, quietly=TRUE)

setwd("./Simulation_outputs")


gamma_values = c(10**(seq(-5, -11, -1)))
N_gamma_values = length(gamma_values)

N_replicates = 10

Segregating_mutations_list_cloning <-list.files(pattern="X_Sampled_individuals*")

print(gamma_values)

	
### Extracting the number of segregeting mutations from files to vectors

N_seg_homo_genotypes_cloning <- rep(c(0), each=N_replicates*N_gamma_values)
N_segregating_mutations_cloning <- rep(c(0), each=N_replicates*N_gamma_values)
Relative_homo_cloning <- rep(c(0), each=N_replicates*N_gamma_values)

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

GC_rate <- rep(rep(gamma_values, each=N_replicates))
Replicate <- rep(c(1:N_replicates), times = N_gamma_values)

Recessive_load_cloning <- data.frame(GC_rate, Replicate, N_seg_homo_genotypes_cloning)
Additive_load_cloning <- data.frame(GC_rate, Replicate, N_segregating_mutations_cloning)
Relative_homozygosity_cloning <- data.frame(GC_rate, Replicate, Relative_homo_cloning)

## Cloning

Recessive_load_stat_cloning <- Recessive_load_cloning %>%
	group_by(GC_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_cloning), SD_recessive_load=sd(N_seg_homo_genotypes_cloning), .groups = "rowwise")

Additive_load_stat_cloning <- Additive_load_cloning %>%
	group_by(GC_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_cloning), SD_additive_load=sd(N_segregating_mutations_cloning), .groups = "rowwise")

Relative_homozygosity_stat_cloning <- Relative_homozygosity_cloning %>%
	group_by(GC_rate) %>%
	summarise(Expected_relative_homozygosity=mean(Relative_homo_cloning), SD_relative_homozygosity=sd(Relative_homo_cloning), .groups = "rowwise")

setwd("../")

write.csv(Additive_load_cloning, file="Data_GC_additive_load_nonconstant_sh_raw.csv")
write.csv(Recessive_load_cloning, file="Data_GC_recessive_load_nonconstant_sh_raw.csv")
write.csv(Relative_homozygosity_cloning, file="Data_GC_relative_homozygosity_nonconstant_sh_raw.csv")

write.csv(Additive_load_stat_cloning, file="Data_GC_additive_load_nonconstant_sh.csv")
write.csv(Recessive_load_stat_cloning, file="Data_GC_recessive_load_nonconstant_sh.csv")
write.csv(Relative_homozygosity_stat_cloning, file="Data_GC_relative_homozygosity_nonconstant_sh.csv")
