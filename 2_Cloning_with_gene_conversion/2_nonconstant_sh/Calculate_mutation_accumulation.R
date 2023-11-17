library(vcfR, quietly=TRUE)
library(dplyr, quietly=TRUE)

setwd("./Simulation_outputs")


gamma_values = c(10**(seq(-5, -7, -1)))
N_gamma_values = length(gamma_values)

N_replicates = 5

Fixed_mutations_list_cloning <- list.files(pattern="X_Fixed_mutations*")
Segregating_mutations_list_cloning <-list.files(pattern="X_Sampled_individuals*")

print(gamma_values)
print(Fixed_mutations_list_cloning)

### Extracting the number of fixed mutations from files to vectors
N_fixed_mutations_cloning <- rep(c(0), each=N_replicates*N_gamma_values)

i_c=0
for (x in Fixed_mutations_list_cloning){
	i_c = i_c + 1
	file_c <- read.csv(x, skip=1) 
	N_fixed_mutations_cloning[i_c] <- nrow(file_c)}
	
### Extracting the number of segregeting mutations from files to vectors

N_seg_homo_genotypes_cloning <- rep(c(0), each=N_replicates*N_gamma_values)
N_segregating_mutations_cloning <- rep(c(0), each=N_replicates*N_gamma_values)

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

GC_rate <- rep(rep(gamma_values, each=N_replicates))
Replicate <- rep(c(1:N_replicates), times = N_gamma_values)

Fixed_mutations_cloning <- data.frame(GC_rate, Replicate, N_fixed_mutations_cloning)

Recessive_load_cloning <- data.frame(GC_rate, Replicate, N_seg_homo_genotypes_cloning)
Additive_load_cloning <- data.frame(GC_rate, Replicate, N_segregating_mutations_cloning)


## Cloning

Fixed_mutations_stat_cloning <- Fixed_mutations_cloning %>%
	group_by(GC_rate) %>%
	summarise(Expected_N_fixed_mutations=mean(N_fixed_mutations_cloning), SD_N_fixed_mutations=sd(N_fixed_mutations_cloning), .groups = "rowwise")

Recessive_load_stat_cloning <- Recessive_load_cloning %>%
	group_by(GC_rate) %>%
	summarise(Expected_recessive_load=mean(N_seg_homo_genotypes_cloning), SD_recessive_load=sd(N_seg_homo_genotypes_cloning), .groups = "rowwise")

Additive_load_stat_cloning <- Additive_load_cloning %>%
	group_by(GC_rate) %>%
	summarise(Expected_additive_load=mean(N_segregating_mutations_cloning), SD_additive_load=sd(N_segregating_mutations_cloning), .groups = "rowwise")

setwd("../")

write.csv(Fixed_mutations_stat_cloning, file="Data_GC_fixed_mutations.csv")
write.csv(Additive_load_stat_cloning, file="Data_GC_additive_load.csv")
write.csv(Recessive_load_stat_cloning, file="Data_GC_recessive_load.csv")
